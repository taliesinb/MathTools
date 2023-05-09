PrivateFunction[katexAliasRiffled, katexAlias]

katexAliasRiffled[fn_] := riffled @ toAlias @ fn;
katexAlias[fn_] := Construct[Function, toAlias @ fn];

toAlias[fn_] /; StringStartsQ[fn, " "] := fn;
toAlias[fn_] := StringJoin["\\", fn, " "];

(**************************************************************************************************)

PrivateFunction[katexNary]

katexNary[op_][a_] := op[a];
katexNary[op_][a_, b_] := op[a][b];
katexNary[op_][a_, b_, c_] := op[a][b][c];
katexNary[op_][a_, b_, c_, d_] := op[a][b][c][d];
katexNary[op_][a_, b_, c_, d_, e_] := op[a][b][c][d][e];

(**************************************************************************************************)

PrivateFunction[declareAppliedFormatting]

declareAppliedFormatting[sym_Symbol] :=
  declareBoxFormatting[s
    (f_sym)[args___] :> MakeBoxes @ AppliedForm[f, args]
  ];

(**************************************************************************************************)

PrivateFunction[declareSymbolForm, declareSymbolFormExplicit]

declareSymbolForm::badsym = "Name of symbol `` should end in SymbolForm."
declareSymbolForm::notsym = "First arg `` is not a symbol."

declareSymbolForm[head_, type_:RawSymbolForm] := iDeclareSymbolForm[head, type, False];
declareSymbolFormExplicit[head_, type_:RawSymbolForm] := iDeclareSymbolForm[head, type, True];

iDeclareSymbolForm[head_, type_, fullName_] := Scope[
  If[Head[head] =!= Symbol, ReturnFailed[declareSymbolForm::notsym, head]];
  name = SymbolName @ head;
  Which[
    StringEndsQ[name, "SymbolForm"], Null,
    StringEndsQ[name, "Symbol"], name = name <> "Form",
    True, ReturnFailed[declareSymbolForm::badsym, head]
  ];
  With[{name2 = name},
    declareBoxFormatting[head[s_] :> makeTypedTemplateBox[s -> type, name2]];
  ];
  katexName = ReplaceNone[$customKatex,
    LowerCaseFirst @ If[fullName,
      StringTrim[name, "Form"],
      StringTrim[name, "SymbolForm"]
    ]
  ];
  $TemplateKatexFunction[name] = katexName;
];

(**************************************************************************************************)

PrivateFunction[declareUnaryWrapperForm]

$unaryWrapperFormName = <||>;

declareUnaryWrapperForm::badsym = "Name of symbol `` should end in Form."
declareUnaryWrapperForm[head_Symbol, katex_:Automatic] := With[
  {name = SymbolName @ head},
  If[!StringEndsQ[name, "Form"], ReturnFailed["badsym", head]];
  declareBoxFormatting[head[s_] :> TemplateBox[List @ MakeQGBoxes @ s, name]];
  $unaryWrapperFormName[head] = name;
  $TemplateKatexFunction[name] = If[katex === Automatic, LowerCaseFirst @ StringDrop[name, -4], katex];
];

(**************************************************************************************************)

PrivateVariable[$rawSymbolP, $literalSymbolsP]

$rawSymbolP = _Symbol | _String | _Subscript | _Superscript | _Subsuperscript | EllipsisSymbol;
$literalSymbolsP = Alternatives[Aligner, EllipsisSymbol];

(**************************************************************************************************)

PrivateFunction[usingCustomKatex]

$customKatex = None;
usingCustomKatex[katex_String] := Function[body, Block[{$customKatex = katex}, body], {HoldAllComplete}];

(**************************************************************************************************)

PrivateFunction[declareInfixSymbol]

declareInfixSymbol[form_] := declareInfixSymbol[form, None, False];

declareInfixSymbol[form_, hint_] := declareInfixSymbol[form, hint, False];

declareInfixSymbol[forms_List, hint_, wrapped_] := Scan[declareInfixSymbol[#, hint, wrapped]&, forms];

declareInfixSymbol[form_, hint_, wrapped_] := With[
  {formName = SymbolName[form]},
  {baseName = StringTrim[formName, "Form"]},
  {symbolName = baseName <> "Symbol"},
  {katexName = ReplaceNone[LowerCaseFirst @ symbolName] @ $customKatex},
  declareBoxFormatting[
    form[args__] :> makeNaryHintedTemplateBox[hint, args, formName],
    form[] :> SBox[symbolName]
  ];
  If[wrapped,
    declareWrappedInfixKatexAlias[baseName, katexName],
    declareInfixKatexAlias[baseName, katexName]
  ]
];

(**************************************************************************************************)

PrivateFunction[declareInfixKatexAlias, declareWrappedInfixKatexAlias]

declareInfixKatexAlias[baseName_, katexName_] := (
  $TemplateKatexFunction[baseName <> "Form"] = katexAliasRiffled[katexName];
  $TemplateKatexFunction[baseName <> "Symbol"] = katexAlias[katexName];
);

declareWrappedInfixKatexAlias[baseName_, katexName_] := (
  $TemplateKatexFunction[baseName <> "Form"] = katexAliasRiffled[katexName] /* LowerCaseFirst[baseName];
  $TemplateKatexFunction[baseName <> "Symbol"] = katexAlias[katexName];
);

(**************************************************************************************************)

PrivateFunction[declareConstantSymbol]

declareConstantSymbol[forms_List] := Scan[declareConstantSymbol, forms];

declareConstantSymbol[symbol_Symbol] := With[
  {symbolName = SymbolName @ symbol},
  {baseName = StringTrim[symbolName, "Symbol"]},
  {katexName = ReplaceNone[LowerCaseFirst @ baseName] @ $customKatex},
  AppendTo[$literalSymbolsP, symbol];
  declareBoxFormatting[
    symbol :> SBox[symbolName]
  ];
  $TemplateKatexFunction[symbolName] = katexAlias[katexName];
];

(**************************************************************************************************)

PrivateFunction[declareUnaryForm, declareBinaryForm, declareUnaryBinaryForm, declareNAryForm]

declareUnaryForm[symbol_Symbol, hint_:None] :=
  declareUnaryBinaryForm[symbol, hint, False];

declareBinaryForm[symbol_Symbol, hint_:None] :=
  declareUnaryBinaryForm[symbol, hint, True];

declareUnaryBinaryForm[symbol_, hint_, isBinary_] := With[
  {formName = SymbolName[symbol]},
  {baseName = StringTrim[formName, "Form"]},
  {symbolName = baseName <> "Symbol"},
  {katexName = ReplaceNone[LowerCaseFirst @ baseName] @ $customKatex},
  If[isBinary,
    declareBoxFormatting[symbol[a_, b_] :> makeNaryHintedTemplateBox[hint, a, b, formName]],
    declareBoxFormatting[symbol[arg_] :> makeNaryHintedTemplateBox[hint, arg, formName]]
  ];
  $TemplateKatexFunction[formName] = katexName;
  
  declareBoxFormatting[symbol[] :> SBox[symbolName]];
  $TemplateKatexFunction[symbolName] = katexAlias[LowerCaseFirst @ symbolName];
];

declareNAryForm[symbol_Symbol, empty_:None, sep_:" "] := With[
  {symbolName = SymbolName @ symbol},
  {katexName = ReplaceNone[LowerCaseFirst @ symbolName] @ $customKatex},
  declareBoxFormatting[
    symbol[args__] :> makeTemplateBox[args, symbolName]
  ];
  If[empty =!= None,
    declareBoxFormatting[symbol[] :> MakeBoxes[empty]];
  ];
  $TemplateKatexFunction[symbolName] = applyRiffled[katexName, sep];
];


(**************************************************************************************************)

PrivateVariable[$symbolFormsP]

$symbolFormsP = Alternatives[
  _SymbolForm,
  _PathQuotientSymbol, _PathGroupoidSymbol, _GroupoidSymbol,
  _PathQuiverSymbol, _QuiverSymbol, _PathSymbol,
  _WordForm,
  _VertexSymbol,
  _PathMapSymbol, _ChartSymbol,
  EllipsisSymbol, BlankSymbol, PlaceholderSquareSymbol
];

(**************************************************************************************************)

PrivateFunction[declareSumLikeFormatting]

declareSumLikeFormatting[form_Symbol, katexName_String] := With[
  {formName = SymbolName @ form},
  {symbolName = StringTrim[formName, "Form"] <> "Symbol"},
  declareBoxFormatting[
    form[] :> SBox[symbolName],
    form[a_] :>
      MakeBoxes @ form[a, Null, Null],
    form[a_, b_] :>
      MakeBoxes @ form[a, b, Null],
    form[a_, b_, c_] :>
      TemplateBox[{MakeQGBoxes @ a, makeSubSupBoxes @ b, makeSubSupBoxes @ c}, formName],
    form[a_, b_, c_, d_] :>
      TemplateBox[{MakeBoxes @ SuchThatForm[a, d], makeSubSupBoxes @ b, makeSubSupBoxes @ c}, formName]
  ];
  $TemplateKatexFunction[formName] = katexName;
  $TemplateKatexFunction[symbolName] = katexName <> "Symbol";
];

SetHoldAllComplete[makeSubSupBoxes]
makeSubSupBoxes = Case[
  list_List := MakeBoxes @ SubstackForm @ list;
  e_        := MakeQGBoxes @ e;
];

(**************************************************************************************************)

PrivateSymbol[symbolBoxes]

symbolBoxes = Case[
  s:($symbolFormsP)   := MakeBoxes @ s;
  InvertedForm[n_]    := % @ Inverted @ n;
  (* Inverted[n_]        := InvertedBoxForm @  % @ n; *)
  f_PlainTextForm     := MakeBoxes @ f;
  other_              := rawSymbolBoxes @ other;
];

(**************************************************************************************************)

PrivateSymbol[recurseWrapperBoxes, unaryWrappedQ]

SetHoldAllComplete[recurseWrapperBoxes, unaryWrapperBoxes, rdb, unaryWrapperQ, unaryWrappedQ];

unaryWrapperQ[s_Symbol] := KeyExistsQ[$unaryWrapperFormName, Unevaluated @ s];
unaryWrapperQ[_] := False;

unaryWrappedQ[(head_Symbol ? unaryWrapperQ)[_]] := True;
unaryWrappedQ[_Subscript | _Superscript | _Subsuperscript] := True;
unaryWrappedQ[_] := False;

recurseWrapperBoxes[e_, f_] := Block[{$decf = f}, rdb @ e];

rdb = Case[
  e:((_Symbol ? unaryWrapperQ)[_])   := unaryWrapperBoxes[e, %];
  Subscript[s_, i_]                  := SubscriptBox[% @ s, MakeQGBoxes @ i];
  Superscript[s_, i_]                := SuperscriptBox[% @ s, MakeQGBoxes @ i];
  Subsuperscript[s_, i_, j_]         := SubsuperscriptBox[% @ s, MakeQGBoxes @ i, MakeQGBoxes @ j];
  (Inverted|InvertedForm)[e_]        := TemplateBox[List @ % @ e, "InvertedForm"];
  e_                                 := $decf @ e;
];

unaryWrapperBoxes[head_Symbol[e_], f_] :=
  TemplateBox[List @ f @ e, $unaryWrapperFormName @ head];

unaryWrapperBoxes[e_, f_] := f @ e;

(**************************************************************************************************)

PrivateFunction[makeTemplateBox, makeTypedTemplateBox]

SetHoldAllComplete[makeTemplateBox];
makeTemplateBox[args___, tag_] :=
  TemplateBox[
    MapUnevaluated[MakeQGBoxes, {args}],
    tag
  ];

SetHoldAllComplete[makeTypedTemplateBox, toTypedSymbol];
makeTypedTemplateBox[args___, tag_] :=
  TemplateBox[
    MapUnevaluated[toTypedSymbol, {args}],
    tag
  ];

(**************************************************************************************************)

PrivateVariable[$colorFormP, $colorFormAssoc]

$colorFormP = Alternatives[
  LightRedForm, LightGreenForm, LightBlueForm,
  RedForm, BlueForm, GreenForm,
  PinkForm, TealForm, OrangeForm, PurpleForm,

  DarkRedForm, DarkBlueForm, DarkGreenForm,
  DarkPinkForm, DarkTealForm, DarkOrangeForm, DarkPurpleForm,

  DarkGrayForm, MediumGrayForm, LightGrayForm
];

$colorFormAssoc = <|
  LightRedForm -> $LightRed, LightGreenForm -> $LightGreen, LightBlueForm -> $LightBlue,
  RedForm -> $Red, BlueForm -> $Blue, GreenForm -> $Green,
  DarkRedForm -> $DarkRed, DarkBlueForm -> $DarkBlue, DarkGreenForm -> $DarkGreen,
  PinkForm -> $Pink, TealForm -> $Teal, OrangeForm -> $Orange, PurpleForm -> $Purple,
  DarkGrayForm -> $DarkGray, MediumGrayForm -> $Gray, LightGrayForm -> $LightGray
|>;

toTypedSymbol = Case[
  Rule[Form[e_], _]                 := MakeQGBoxes @ e;
  Rule[e_, None]                    := e;
  Rule[e_, Automatic]               := MakeQGBoxes @ e;
  Rule[BlankSymbol, _]              := MakeBoxes @ BlankSymbol;
  Rule[e_ ? unaryWrappedQ, type_]   := recurseWrapperBoxes[e, toTypedSymbol[# -> type]&];
  Rule[arg:((type_)[___]), type_]   := MakeBoxes @ arg;
  Rule[None|Null, _]                := "";
  Rule[arg_, type_]                 := MakeBoxes @ type @ arg;
  arg_                              := MakeQGBoxes @ arg
]


(* TODO: USE InputAliases!!! *)

(**************************************************************************************************)

PublicForm[ClassTaggedForm]

declareBoxFormatting[
  ClassTaggedForm[form_, tag_] :> TagBox[ToBoxes @ form, "ClassTaggedForm"[tag]]
];

(**************************************************************************************************)

PublicForm[RasterizedForm]

(* TODO: this isn't picked up by the markdown code at all! *)
declareBoxFormatting[
  RasterizedForm[form_] :> TagBox[ToBoxes @ form, "RasterizationOptions"[]],
  RasterizedForm[form_, opts__] :> TagBox[ToBoxes @ form, "RasterizationOptions"[opts]]
];

(**************************************************************************************************)

PublicForm[RawSymbolForm]

declareBoxFormatting[
  RawSymbolForm[p_] :>
    rawSymbolBoxes @ p
];

(**************************************************************************************************)

PublicForm[PreformattedCodeForm]

declareBoxFormatting[
  PreformattedCodeForm[s_String] :> preformattedCodeBoxes[s]
];

(*
─
-─―‒-–—⸻⸺
─
*)

PrivateFunction[preformattedCodeBoxes, fixExtensibleChars]

preformattedCodeBoxes[str_String] := Scope[
  boxes = fixExtensibleChars @ ToString[str, InputForm];
  TemplateBox[{Construct[InterpretationBox, boxes, str], str}, "StringBlockForm"]
];

fixExtensibleChars[str_String] := StringReplace[str, {"\[VerticalLine]" -> "⎥", "\[HorizontalLine]" -> "—"}];