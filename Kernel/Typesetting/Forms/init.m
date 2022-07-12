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
  declareBoxFormatting[
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
  declareBoxFormatting[head[s_] :> TemplateBox[List @ makeQGBoxes @ s, name]];
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

PrivateFunction[declareCommaRiffledForm, applyStyledRiffled, declareStyledCommaRiffledForm]

declareCommaRiffledForm[symbol_, katex_] := With[
  {formName = SymbolName[symbol]},
  declareBoxFormatting[
    symbol[args___] :> makeTemplateBox[args, formName]
  ];
  $TemplateKatexFunction[formName] = applyRiffled[katex, ","];
];

getStyleHead[style_] := Scope[
  fn = Lookup[$TemplateKatexFunction, style, $templateToKatexFunction @ style];
  Replace[fn, {
    (s_String | Function[s_[#]]) :> PrefixSlash[s]
  }]
];

applyStyledRiffled[katex_, sep_][style_, args___] :=
  katex[getStyleHead[style], riffled[sep][args]];

declareStyledCommaRiffledForm[symbol_, katex_] := With[
  {formName = SymbolName[symbol]},
  declareBoxFormatting[
    symbol[style_][args___] :> MapAt[Prepend[SymbolName @ style], 1] @ makeTemplateBox[args, formName]
  ];
  $TemplateKatexFunction[formName] = applyStyledRiffled[katex, ","]
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
      TemplateBox[{makeQGBoxes @ a, makeSubSupBoxes @ b, makeSubSupBoxes @ c}, formName],
    form[a_, b_, c_, d_] :>
      TemplateBox[{MakeBoxes @ SuchThatForm[a, d], makeSubSupBoxes @ b, makeSubSupBoxes @ c}, formName]
  ];
  $TemplateKatexFunction[formName] = katexName;
  $TemplateKatexFunction[symbolName] = katexName <> "Symbol";
];

SetHoldAllComplete[makeSubSupBoxes]
makeSubSupBoxes = Case[
  list_List := MakeBoxes @ SubstackForm @ list;
  e_        := makeQGBoxes @ e;
];

(**************************************************************************************************)

PrivateFunction[declareAlgebraicSymbol]

declareAlgebraicSymbol[sym_Symbol, aliases_] := With[
  {symName = SymbolName @ sym},
  {formName = symName <> "Form"},

  declareBoxFormatting[

    sym[s_String /; KeyExistsQ[aliases, s]] :>
      TemplateBox[List @ TemplateBox[{}, Lookup[aliases, s]], formName],

    sym[s_Symbol /; MemberQ[aliases, HoldSymbolName @ s]] :>
      ToBoxes @ sym @ First @ IndexOf[aliases, SymbolName @ s],

    sym[e_, power_] :> With[
      {inner = MakeBoxes @ sym[e]},
      TemplateBox[List @ TemplateBox[{inner, makeQGBoxes @ power}, "PowerForm"], formName]
    ],

    sym[n_] :>
      TemplateBox[List @ rawSymbolBoxes @ n, formName]

  ];

  $TemplateKatexFunction[formName] = ToLowerCase @ StringTrim[symName, "Symbol"];
];

(**************************************************************************************************)

PrivateFunction[declareLieGroupOrAlgebraForm]

declareLieGroupOrAlgebraForm[list_List] :=
  Scan[declareLieGroupOrAlgebraForm, list];

declareLieGroupOrAlgebraForm[symbol_Symbol] := With[
  {name = SymbolName @ symbol},
  {katex = LowerCaseFirst @ StringTrim[name, "Form"]},
  declareBoxFormatting[
    symbol[n_] :> MakeBoxes @ symbol[n, Reals],
    symbol[n_, f_] :> TemplateBox[{rawSymbolBoxes @ n, fieldOrRingBoxes @ f}, name]
  ];
  $TemplateKatexFunction[name] = katex;
];

fieldOrRingBoxes = Case[
  f:fieldsP      := MakeBoxes @ FieldSymbol @ f;
  r:ringsP       := MakeBoxes @ RingSymbol @ r;
  sr:semiringsP  := MakeBoxes @ SemiringSymbol @ sr;
  n_Integer      := MakeBoxes @ FiniteFieldSymbol[n];
  other_         := makeQGBoxes @ other,
  {
    fieldsP     -> Alternatives[Reals, Complexes, Rationals, "R", "C", "Q", "K"],
    ringsP      -> Alternatives[Integers, "Z"],
    semiringsP  -> Alternatives[Naturals, "N"]
  }
]

(**************************************************************************************************)

PrivateSymbol[symbolBoxes]

symbolBoxes = Case[
  s:($symbolFormsP)   := MakeBoxes @ s;
  InvertedForm[n_]     := % @ Inverted @ n;
  Inverted[n_]         := InvertedBoxForm @ RawBoxes @ % @ n;
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
  Subscript[s_, i_]                  := SubscriptBox[% @ s, makeQGBoxes @ i];
  Superscript[s_, i_]                := SuperscriptBox[% @ s, makeQGBoxes @ i];
  Subsuperscript[s_, i_, j_]         := SubsuperscriptBox[% @ s, makeQGBoxes @ i, makeQGBoxes @ j];
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
    MapUnevaluated[makeQGBoxes, {args}],
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
  Rule[Form[e_], _]                 := makeQGBoxes @ e;
  Rule[e_, None]                    := e;
  Rule[e_, Automatic]               := makeQGBoxes @ e;
  Rule[BlankSymbol, _]              := MakeBoxes @ BlankSymbol;
  Rule[e_ ? unaryWrappedQ, type_]   := recurseWrapperBoxes[e, toTypedSymbol[# -> type]&];
  Rule[arg:((type_)[___]), type_]   := MakeBoxes @ arg;
  Rule[None|Null, _]                := "";
  Rule[arg_, type_]                 := MakeBoxes @ type @ arg;
  arg_                              := makeQGBoxes @ arg
]

(**************************************************************************************************)

PublicForm[ClassTaggedForm]

declareBoxFormatting[
  ClassTaggedForm[form_, tag_] :> TagBox[ToBoxes @ form, "ClassTaggedForm"[tag]]
];

(**************************************************************************************************)

PublicForm[RasterizedForm]

declareBoxFormatting[
  RasterizedForm[form_] :> TagBox[ToBoxes @ form, "Rasterized"]
];

(**************************************************************************************************)

PublicForm[RawSymbolForm]

declareBoxFormatting[
  RawSymbolForm[p_] :>
    rawSymbolBoxes @ p
];

(**************************************************************************************************)

PublicForm[SymbolForm]

declareBoxFormatting[
  SymbolForm[p_] :>
    TemplateBox[List @ symbolBoxes @ p, "SymbolForm"]
];

$TemplateKatexFunction["SymbolForm"] = "sym";

