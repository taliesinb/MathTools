(********************************************)

TBox[form_][args___] := TemplateBox[{args}, form];
SBox[form_] := TemplateBox[{}, form];
RBox[args___] := RowBox[{args}];

(**************************************************************************************************)

PackageExport["RasterizedForm"]

declareBoxFormatting[
  RasterizedForm[form_] :> TagBox[ToBoxes @ form, "Rasterized"]
];

(**************************************************************************************************)

PackageExport["Naturals"]
PackageExport["PositiveNaturals"]

SetUsage @ "Naturals represents the natural numbers."
SetUsage @ "PositiveNaturals represents the positive natural numbers."

MakeBoxes[Naturals, StandardForm] := SBox["Naturals"];
MakeBoxes[Naturals, TraditionalForm] := SBox["Naturals"];

MakeBoxes[PositiveNaturals, StandardForm] := SBox["PositiveNaturals"];
MakeBoxes[PositiveNaturals, TraditionalForm] := SBox["PositiveNaturals"];

MakeBoxes[PositiveReals, StandardForm] := SBox["PositiveReals"];
MakeBoxes[PositiveReals, TraditionalForm] := SBox["PositiveReals"];

MakeBoxes[Primes, StandardForm] := SBox["Primes"];

applyRiffled[f_, op_][args___] := f[riffled[op][args]];

GBox[entries_, alignments_, rowSpacings_, colSpacings_] :=
  GridBox[
    entries,
    GridBoxAlignment -> {"Columns" -> alignments},
    GridBoxSpacings -> {"Rows" -> prepend0 @ rowSpacings, "Columns" -> prepend0 @ colSpacings}
  ];

prepend0[list_List] := Prepend[list, 0];
prepend0[e_] := e;

(********************************************)

PackageScope["riffled"]

riffled[op_][] := "";
riffled[op_][a_] := a;
riffled[op_][a_, b_] := {a, op, b}
riffled[op_][a_, b_, c__] := Riffle[{a, b, c}, op];

(********************************************)

toAlias[fn_] /; StringStartsQ[fn, " "] := fn;
toAlias[fn_] := StringJoin["\\", fn, " "];

katexAliasRiffled[fn_] := riffled @ toAlias @ fn;
katexAlias[fn_] := Construct[Function, toAlias @ fn];

(********************************************)

katexNary[op_][a_] := op[a];
katexNary[op_][a_, b_] := op[a][b];
katexNary[op_][a_, b_, c_] := op[a][b][c];
katexNary[op_][a_, b_, c_, d_] := op[a][b][c][d];
katexNary[op_][a_, b_, c_, d_, e_] := op[a][b][c][d][e];

(********************************************)

(* katexComma[op_][a_] := op[a];
katexComma[op_][a_, b_] := op[a, " , ", b]
katexComma[op_][a_, b_, c__] := op @@ Riffle[{a, b, c}, " , "];

 *)(********************************************)

PackageExport["RawSymbolForm"]

declareBoxFormatting[
  RawSymbolForm[p_] :>
    rawSymbolBoxes @ p
];

(**************************************************************************************************)

declareAppliedFormatting[sym_Symbol] :=
  declareBoxFormatting[
    (f_sym)[args___] :> MakeBoxes @ AppliedForm[f, args]
  ];

(**************************************************************************************************)

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

(********************************************)

$unaryWrapperFormName = <||>;

declareUnaryWrapperForm::badsym = "Name of symbol `` should end in Form."
declareUnaryWrapperForm[head_Symbol, katex_:Automatic] := With[
  {name = SymbolName @ head},
  If[!StringEndsQ[name, "Form"], ReturnFailed["badsym", head]];
  declareBoxFormatting[head[s_] :> TemplateBox[List @ makeQGBoxes @ s, name]];
  $unaryWrapperFormName[head] = name;
  $TemplateKatexFunction[name] = If[katex === Automatic, LowerCaseFirst @ StringDrop[name, -4], katex];
];

(********************************************)

$rawSymbolP = _Symbol | _String | _Subscript | _Superscript | _Subsuperscript | EllipsisSymbol;

$literalSymbolsP = Alternatives[Aligner];

(********************************************)

$customKatex = None;
usingCustomKatex[katex_String] := Function[body, Block[{$customKatex = katex}, body], {HoldAllComplete}];

(********************************************)

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

declareInfixKatexAlias[baseName_, katexName_] := (
  $TemplateKatexFunction[baseName <> "Form"] = katexAliasRiffled[katexName];
  $TemplateKatexFunction[baseName <> "Symbol"] = katexAlias[katexName];
);

declareWrappedInfixKatexAlias[baseName_, katexName_] := (
  $TemplateKatexFunction[baseName <> "Form"] = katexAliasRiffled[katexName] /* LowerCaseFirst[baseName];
  $TemplateKatexFunction[baseName <> "Symbol"] = katexAlias[katexName];
);

(********************************************)

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

(********************************************)

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

(********************************************)

declareUnaryForm[symbol_Symbol, hint_:None] :=
  declareUnaryBinaryForm[symbol, hint, False];

(********************************************)

declareBinaryForm[symbol_Symbol, hint_:None] :=
  declareUnaryBinaryForm[symbol, hint, True];

(********************************************)

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

(********************************************)

declareCommaRiffledForm[symbol_, katex_] := With[
  {formName = SymbolName[symbol]},
  declareBoxFormatting[
    symbol[args___] :> makeTemplateBox[args, formName]
  ];
  $TemplateKatexFunction[formName] = applyRiffled[katex, ","];
];

(********************************************)

declareStyledCommaRiffledForm[symbol_, katex_] := With[
  {formName = SymbolName[symbol]},
  declareBoxFormatting[
    symbol[style_][args___] :> MapAt[Prepend[SymbolName @ style], 1] @ makeTemplateBox[args, formName]
  ];
  $TemplateKatexFunction[formName] = applyStyledRiffled[katex, ","]
];

applyStyledRiffled[katex_, sep_][style_, args___] :=
  katex[StringJoin["\\", $TemplateKatexFunction @ style], riffled[sep][args]];

(********************************************)

PackageExport["SymbolForm"]

declareBoxFormatting[
  SymbolForm[p_] :>
    TemplateBox[List @ symbolBoxes @ p, "SymbolForm"]
];

$TemplateKatexFunction["SymbolForm"] = "sym";

(********************************************)

PackageExport["PiSymbol"]
PackageExport["TauSymbol"]

declareConstantSymbol[{PiSymbol, TauSymbol}]

(********************************************)

PackageExport["NotApplicableSymbol"]
PackageExport["UnknownSymbol"]
PackageExport["EmptySetSymbol"]

declareConstantSymbol[{NotApplicableSymbol, UnknownSymbol, EmptySetSymbol}];

(********************************************)

PackageExport["PlainTextForm"]

declareBoxFormatting[
  PlainTextForm[p_] :>
    TemplateBox[List @ MakeBoxes @ p, "PlainTextForm"]
];

$TemplateKatexFunction["PlainTextForm"] = "textrm";

(**************************************************************************************************)

$symbolFormsP = Alternatives[
  _SymbolForm,
  _PathQuotientSymbol, _PathGroupoidSymbol, _GroupoidSymbol,
  _PathQuiverSymbol, _QuiverSymbol, _PathSymbol,
  _WordForm,
  _VertexSymbol,
  _PathMapSymbol, _ChartSymbol,
  EllipsisSymbol, BlankSymbol, PlaceholderSquareSymbol
];

(********************************************)

symbolBoxes = Case[
  s:($symbolFormsP)   := MakeBoxes @ s;
  InvertedForm[n_]     := % @ Inverted @ n;
  Inverted[n_]         := InvertedBoxForm @ RawBoxes @ % @ n;
  f_PlainTextForm     := MakeBoxes @ f;
  other_              := rawSymbolBoxes @ other;
];

(**************************************************************************************************)

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
  (Inverted|InvertedForm)[e_]          := TemplateBox[List @ % @ e, "InvertedForm"];
  e_                                 := $decf @ e;
];

unaryWrapperBoxes[head_Symbol[e_], f_] :=
  TemplateBox[List @ f @ e, $unaryWrapperFormName @ head];

unaryWrapperBoxes[e_, f_] := f @ e;

(**************************************************************************************************)

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

PackageScope["$colorFormP"]
PackageScope["$colorFormAssoc"]

$colorFormP = Alternatives[
  LightRedForm, LightGreenForm, LightBlueForm,
  RedForm, BlueForm, GreenForm,
  RedBlueForm, GreenBlueForm, RedGreenForm, PurpleForm,

  DarkRedForm, DarkBlueForm, DarkGreenForm,
  DarkRedBlueForm, DarkGreenBlueForm, DarkRedGreenForm, DarkPurpleForm,

  DarkGrayForm, MediumGrayForm, LightGrayForm
];

$colorFormAssoc = <|
  LightRedForm -> $LightRed, LightGreenForm -> $LightGreen, LightBlueForm -> $LightBlue,
  RedForm -> $Red, BlueForm -> $Blue, GreenForm -> $Green,
  DarkRedForm -> $DarkRed, DarkBlueForm -> $DarkBlue, DarkGreenForm -> $DarkGreen,
  RedBlueForm -> $Pink, GreenBlueForm -> $Teal, RedGreenForm -> $Orange, PurpleForm -> $Purple,
  DarkGrayForm -> $DarkGray, MediumGrayForm -> $Gray, LightGrayForm -> $LightGray
|>;

InvertedForm[(c:$colorFormP)[e_]] := c[InvertedForm[e]];

toTypedSymbol = Case[
  Rule[Form[e_], _] := makeQGBoxes @ e;
  Rule[e_, None] := e;
  Rule[e_, Automatic] := makeQGBoxes @ e;
  Rule[BlankSymbol, _] :=
    MakeBoxes @ BlankSymbol;
  Rule[(c:colorsP)[arg_], type_] :=
    TemplateBox[List @ toTypedSymbol[arg -> type], SymbolName @ c];
  Rule[arg:((type_)[___]), type_] :=
    MakeBoxes @ arg;
  Rule[None|Null, _] := "";
  Rule[arg_, type_] :=
    MakeBoxes @ type @ arg;
  arg_ :=
    makeQGBoxes @ arg,
  {colorsP -> $colorFormP}
]

(**************************************************************************************************)

PackageExport["StripColorForms"]

StripColorForms[expr_] := expr //. $colorFormP[z_] :> z;

(**************************************************************************************************)

PackageExport["BlankSymbol"]

declareBoxFormatting[
  BlankSymbol :> SBox["BlankSymbol"]
]

$TemplateKatexFunction["BlankSymbol"] = Function["\\blank"];

(**************************************************************************************************)

PackageExport["PlaceholderSquareSymbol"]

declareBoxFormatting[
  PlaceholderSquareSymbol :> SBox["PlaceholderSquareSymbol"]
]

$TemplateKatexFunction["PlaceholderSquareSymbol"] = Function["□"];

(**************************************************************************************************)

PackageExport["PathQuotientSymbol"]

SetUsage @ "
PathQuotientSymbol[q$, mu$] represents the path quiver on quiver q$.
"

declareBoxFormatting[
  PathQuotientSymbol[q_, u_] :> makeTypedTemplateBox[q -> QuiverSymbol, u -> PathMapSymbol, "PathQuotientSymbolForm"]
];

(**************************************************************************************************)

PackageExport["ElementOfForm"]
PackageExport["NotElementOfForm"]
PackageExport["VertexOfForm"]
PackageExport["EdgeOfForm"]
PackageExport["PathOfForm"]
PackageExport["VertexCountOfForm"]

declareUnaryForm[VertexCountOfForm, QuiverSymbol];

declareBoxFormatting[
  ElementOfForm[a__, b_] :> makeTemplateBox[CommaRowForm[a], b, "ElementOfForm"],
  ElementOfForm[a_, b_] :> makeTemplateBox[a, b, "ElementOfForm"],
  NotElementOfForm[a_, b_] :> makeTemplateBox[a, b, "NotElementOfForm"],
  VertexOfForm[a_, b_] :> makeHintedTemplateBox[a -> VertexSymbol, b -> QuiverSymbol, "VertexOfForm"],
  EdgeOfForm[a_, b_] :> makeHintedTemplateBox[a -> EdgeSymbol, b -> QuiverSymbol, "EdgeOfForm"],
  PathOfForm[a_, b_] :> makeHintedTemplateBox[a -> PathSymbol, b -> QuiverSymbol, "PathOfForm"]
];

$TemplateKatexFunction["ElementOfForm"] = "elemOf";
$TemplateKatexFunction["NotElementOfForm"] = "notElemOf";
$TemplateKatexFunction["VertexOfForm"] = "vertOf";
$TemplateKatexFunction["EdgeOfForm"] = "edgeOf";
$TemplateKatexFunction["PathOfForm"] = "pathOf";

(**************************************************************************************************)

PackageExport["EdgesSymbol"]
PackageExport["VerticesSymbol"]

declareBoxFormatting[
  EdgesSymbol[q_] :> makeHintedTemplateBox[q -> QuiverSymbol, "EdgesSymbolForm"],
  EdgesSymbol[] :> SBox["EdgesSymbol"],
  VerticesSymbol[q_] :> makeHintedTemplateBox[q -> QuiverSymbol, "VerticesSymbolForm"],
  VerticesSymbol[] :> SBox["VerticesSymbol"]
]

$TemplateKatexFunction["EdgesSymbolForm"] = "edges";
$TemplateKatexFunction["VerticesSymbolForm"] = "vertices";

$TemplateKatexFunction["EdgesSymbol"] = "edges";
$TemplateKatexFunction["VerticesSymbol"] = "vertices";

(**************************************************************************************************)

PackageExport["VertexFieldSymbol"]
PackageExport["EdgeFieldSymbol"]

declareSymbolForm[VertexFieldSymbol];
declareSymbolForm[EdgeFieldSymbol];

PackageExport["PathVectorSpaceSymbol"]
PackageExport["PathVectorSymbol"]
PackageExport["PathWeightSymbol"]

PathVectorSpaceSymbol[] := PathVectorSpaceSymbol["P"];

declareSymbolForm[PathVectorSpaceSymbol];
declareSymbolForm[PathVectorSymbol];
declareSymbolForm[PathWeightSymbol];

(**************************************************************************************************)

PackageExport["BasisPathVectorSymbol"]
PackageExport["BasisPathWeightSymbol"]

BasisPathVectorSymbol[sub_] := BasisPathVectorSymbol["p", sub];
BasisPathWeightSymbol[sub_] := BasisPathWeightSymbol["p", sub];

declareBoxFormatting[
  BasisPathVectorSymbol[p_, sub_] :> makeTemplateBox[p, sub, "BasisPathVectorSymbolForm"],
  BasisPathWeightSymbol[p_, sub_] :> makeTemplateBox[p, sub, "BasisPathWeightSymbolForm"]
];

$TemplateKatexFunction["BasisPathVectorSymbolForm"] = "basisPath";
$TemplateKatexFunction["BasisPathWeightSymbolForm"] = "basisPathWeight";

(**************************************************************************************************)

PackageExport["BaseFieldSymbol"]

BaseFieldSymbol[] := BaseFieldSymbol["K"];

declareBoxFormatting[
  BaseFieldSymbol[s_] :> makeTemplateBox[s, "BaseFieldSymbolForm"]
];

$TemplateKatexFunction["BaseFieldSymbolForm"] = "baseField";

(**************************************************************************************************)

PackageExport["EdgeFieldsSymbol"]
PackageExport["VertexFieldsSymbol"]

declareBoxFormatting[
  EdgeFieldsSymbol[k_] :> MakeBoxes @ FunctionSpaceForm[EdgesSymbol[], k],
  EdgeFieldsSymbol[k_, q_] :> MakeBoxes @ FunctionSpaceForm[EdgesSymbol[q], k],
  VertexFieldsSymbol[k_] :> MakeBoxes @ FunctionSpaceForm[VerticesSymbol[], k],
  VertexFieldsSymbol[k_, q_] :> MakeBoxes @ FunctionSpaceForm[VerticesSymbol[q], k]
];

(**************************************************************************************************)

PackageExport["PiecewiseForm"]

declareBoxFormatting[
  PiecewiseForm[cases__Rule] :> makePiecewiseBoxes[{cases}],
  OtherwiseSymbol :> SBox["OtherwiseSymbol"]
];

SetHoldAllComplete[makePiecewiseBoxes, makePiecewiseRow]

makePiecewiseBoxes[rules_List] := Scope[
  entries = MapUnevaluated[makePiecewiseRow, rules];
  grid = GridBox[
    {{"\[Piecewise]", GridBox[
      entries,
      ColumnAlignments -> {Left}, ColumnSpacings -> 1.2, ColumnWidths -> Automatic
    ]}},
    ColumnAlignments -> {Left}, ColumnSpacings -> 0.5, ColumnWidths -> Automatic
  ];
  TemplateBox[List @ grid, "PiecewiseForm"]
]

$TemplateKatexFunction["PiecewiseForm"] = katexPiecewise;
$TemplateKatexFunction["OtherwiseSymbol"] = "\\text{otherwise}"&;

katexPiecewise[GridBox[{{_, GridBox[entries_, ___]}}, ___]] := {
  "\\begin{cases}\n",
  katexPiecewiseRow @@@ entries,
  "\\end{cases}\n"
};

katexPiecewiseRow[case_, value_] :=
  {case, " &\\text{if } ", value, "\\\\\n"};

katexPiecewiseRow[case_, o:SBox["OtherwiseSymbol"]] :=
  {case, " &", o, "\n"};

makePiecewiseRow[All -> value_] :=
  {makeQGBoxes @ value, MakeBoxes @ OtherwiseSymbol}

makePiecewiseRow[case_ -> value_] :=
  {makeQGBoxes @ value, makeQGBoxes @ case};

(**************************************************************************************************)

PackageExport["FunctionGraphForm"]

declareUnaryForm[FunctionGraphForm];

(**************************************************************************************************)

PackageExport["FunctionSpaceForm"]
PackageExport["FiniteTotalFunctionSpaceForm"]

declareBoxFormatting[
  FunctionSpaceForm[from_, to_] :> makeHintedTemplateBox[from, to -> BaseFieldSymbol, "FunctionSpaceForm"],
  FiniteTotalFunctionSpaceForm[from_, to_] :> makeHintedTemplateBox[from, to -> BaseFieldSymbol, "FiniteTotalFunctionSpaceForm"]
];

$TemplateKatexFunction["FunctionSpaceForm"] = "functionSpace"
$TemplateKatexFunction["FiniteTotalFunctionSpaceForm"] = "finiteTotalFunctionSpace"

(**************************************************************************************************)

PackageExport["FunctionTypeForm"]

declareBinaryForm[FunctionTypeForm]

(**************************************************************************************************)

PackageExport["FiniteFieldSymbol"]

declareBoxFormatting[
  FiniteFieldSymbol[n_] :> makeTemplateBox[n, "FiniteField"]
];

$TemplateKatexFunction["FiniteField"] = "finiteField";

(**************************************************************************************************)

PackageExport["FunctionSymbol"]

$namedFunctions = {
  VertexListFunction,
  AndFunction,
  OrFunction,
  NotFunction,
  EdgeListFunction,
  CardinalListFunction,
  SignedCardinalListFunction,
  SignedLengthFunction,
  LengthFunction,
  WordFunction,
  PathListFunction,
  HeadVertexFunction, TailVertexFunction,
  AutomorphismsFunction, EndomorphismsFunction,
  BasisFunction,
  SupportFunction,
  SplitFunction,
  LCMFunction,
  GradeFunction,
  ModFunction,
  MinimalContractionsFunction,
  MinimalContractionSetsFunction,
  CoefficientFunction,
  MaxFunction,
  MinFunction,
  SinFunction,
  CosFunction,
  TanFunction,
  ArcTanFunction,
  TorusFunction,
  MobiusFunction,
  ClipFunction,
  SignFunction,
  StepFunction,
  DomainFunction,
  CodomainFunction,
  ProjectionFunction,
  LiftFunction,
  IdentityFunction,
  TotalFunction,
  
  StateJoinFunction,
  StateMeetFunction,
  StateExtentFunction,
  StateIntentFunction,
  StateComposeFunction,
  StateDecomposeFunction
};

$functionHeads = {
  FunctionSymbol, PathMapSymbol,
  GroupoidHomomorphismSymbol, GroupHomomorphismSymbol,
  GroupoidFunctionSymbol, GroupFunctionSymbol,
  PathHomomorphismSymbol, GraphHomomorphismSymbol,
  InverseForm,
  VertexFieldSymbol, EdgeFieldSymbol,
  TransportMapSymbol,
  VertexSymbol, QuiverSymbol
}

setupGrabbingRule[sym_] := (
  sym /: Subscript[sym[inner_], rest__] := sym[Subscript[inner, rest]];
  sym /: Superscript[sym[inner_], rest__] := sym[Superscript[inner, rest]];
  sym /: Subsuperscript[sym[inner_], rest__] := sym[Subsuperscript[inner, rest]];
  f_sym[args__] := AppliedForm[f, args];
);

Scan[setupGrabbingRule, $functionHeads];


$functionFormP = Alternatives @@ Join[
  Blank /@ $functionHeads,
  $namedFunctions
];

SetHoldAllComplete[symOrStringMatchingQ, isMuQ];

symOrStringMatchingQ[s_String, patt_] := StringMatchQ[s, patt];
symOrStringMatchingQ[s_Symbol, patt_] := StringMatchQ[SymbolName[Unevaluated @ s], patt];
symOrStringMatchingQ[_, _] := False;

isMuQ[s_] := symOrStringMatchingQ[s, "\[Mu]" | "mu"];

declareBoxFormatting[

  FunctionSymbol[f:$functionFormP] :>
    MakeBoxes @ f,

  FunctionSymbol[f_ ? isMuQ] :>
    MakeBoxes @ PathMapSymbol["\[Mu]"],

  FunctionSymbol[f_] :>
    makeTemplateBox[f, "FunctionSymbolForm"]
];

$TemplateKatexFunction["FunctionSymbolForm"] = "function";

(**************************************************************************************************)

PackageExport["WhiteCircleModifierForm"]
PackageExport["BlackCircleModifierForm"]

declareUnaryForm[WhiteCircleModifierForm];
declareUnaryForm[BlackCircleModifierForm];

(**************************************************************************************************)

PackageExport["ImageModifierForm"]
PackageExport["PreimageModifierForm"]
PackageExport["MultiImageModifierForm"]
PackageExport["MultiPreimageModifierForm"]

PackageExport["MultiImageColorModifierForm"]
PackageExport["MultiPreimageColorModifierForm"]

declareUnaryForm[ImageModifierForm];
declareUnaryForm[PreimageModifierForm];
declareUnaryForm[MultiImageModifierForm];
declareUnaryForm[MultiPreimageModifierForm];
declareUnaryForm[MultiImageColorModifierForm];
declareUnaryForm[MultiPreimageColorModifierForm];

declareAppliedFormatting[ImageModifierForm];
declareAppliedFormatting[PreimageModifierForm];
declareAppliedFormatting[MultiImageModifierForm];
declareAppliedFormatting[MultiPreimageModifierForm];
declareAppliedFormatting[MultiImageColorModifierForm];
declareAppliedFormatting[MultiPreimageColorModifierForm];

declareBoxFormatting[
  f_ImageModifierForm[args___] :> MakeBoxes @ AppliedForm[f, args],
  f_PreimageModifierForm[args___] :> MakeBoxes @ AppliedForm[f, args],
  f_MultiImageModifierForm[args___] :> MakeBoxes @ AppliedForm[f, args],
  f_MultiPreimageModifierForm[args___] :> MakeBoxes @ AppliedForm[f, args],
  f_MultiImageColorModifierForm[args___] :> MakeBoxes @ AppliedForm[f, args],
  f_MultiPreimageColorModifierForm[args___] :> MakeBoxes @ AppliedForm[f, args]
];

(**************************************************************************************************)

PackageExport["FunctionCompositionForm"]

declareInfixSymbol[FunctionCompositionForm, FunctionSymbol, True];

declareBoxFormatting[
  f_FunctionCompositionForm[args___] :> MakeBoxes[AppliedForm[ParenthesesForm[f], args]]
]

(**************************************************************************************************)

PackageExport["AppliedForm"]

declareBoxFormatting[
  AppliedForm[f_, args__] :> makeTypedTemplateBox[f -> FunctionSymbol, args, "AppliedForm"]
];

$TemplateKatexFunction["AppliedForm"] = appliedKatex;

appliedKatex[f_, args___] := {f, "(", Riffle[{args}, ","], ")"};

(**************************************************************************************************)

PackageExport["OperatorAppliedForm"]

declareBoxFormatting[
  OperatorAppliedForm[f_, g_] :> makeTemplateBox[f, g, "OperatorAppliedForm"]
];

$TemplateKatexFunction["OperatorAppliedForm"] = operatorAppliedKatex;

operatorAppliedKatex[f_, g_] := {f, "\,", g};

(**************************************************************************************************)

PackageExport["PathMapSymbol"]

PathMapSymbol[] := PathMapSymbol["\[Mu]"];

declareBoxFormatting[
  PathMapSymbol[mu_ ? isMuQ] :>
    makeTemplateBox["\[Mu]", "PathMapSymbolForm"],

  PathMapSymbol[mu_] :>
    makeTemplateBox[mu, "PathMapSymbolForm"]
]

$TemplateKatexFunction["PathMapSymbolForm"] = "pathMap";

(**************************************************************************************************)

PackageExport["FunctionSignatureForm"]

FunctionSignatureForm[f_, a_List, b_] :=
  FunctionSignatureForm[f, TupleForm @@ a, b];

FunctionSignatureForm[f_, a_, b_List] :=
  FunctionSignatureForm[f, a, TupleForm @@ b];

declareBoxFormatting[
  FunctionSignatureForm[f_, a_, b_] :>
    makeTypedTemplateBox[f -> FunctionSymbol, a, b, "FunctionSignatureForm"]
]

$TemplateKatexFunction["FunctionSignatureForm"] = "functionSignature";

(**************************************************************************************************)

PackageExport["PartialFunctionSignatureForm"]

PartialFunctionSignatureForm[f_, a_List, b_] :=
  PartialFunctionSignatureForm[f, TupleForm @@ a, b];

PartialFunctionSignatureForm[f_, a_, b_List] :=
  PartialFunctionSignatureForm[f, a, TupleForm @@ b];

declareBoxFormatting[
  PartialFunctionSignatureForm[f_, a_, b_] :>
    makeTypedTemplateBox[f -> FunctionSymbol, a, b, "PartialFunctionSignatureForm"]
]

$TemplateKatexFunction["PartialFunctionSignatureForm"] = "partialFunctionSignature";

(**************************************************************************************************)

PackageExport["GraphHomomorphismSymbol"]

GraphHomomorphismSymbol[] := GraphHomomorphismSymbol["\[Pi]"]

declareBoxFormatting[
  
  GraphHomomorphismSymbol[f_] :>
    TemplateBox[List @ rawSymbolBoxes @ f, "GraphHomomorphismSymbolForm"]

]

$TemplateKatexFunction["GraphHomomorphismSymbolForm"] = "graphHomomorphism";

(**************************************************************************************************)

PackageExport["PathHomomorphismSymbol"]

PathHomomorphismSymbol[] := PathHomomorphismSymbol["\[Rho]"]

declareSymbolForm[PathHomomorphismSymbol];

(**************************************************************************************************)

PackageExport["GroupoidFunctionSymbol"]
PackageExport["GroupoidHomomorphismSymbol"]

GroupoidFunctionSymbol[] := GroupoidFunctionSymbol["\[Mu]"]

declareSymbolForm[GroupoidFunctionSymbol];
declareSymbolForm[GroupoidHomomorphismSymbol];

(**************************************************************************************************)

PackageExport["ToroidalModifierForm"]

declareUnaryWrapperForm[ToroidalModifierForm];

declareBoxFormatting[
  t_ToroidalModifierForm[args___] :> MakeBoxes @ CardinalSizeBindingForm[t, args]
];

(**************************************************************************************************)

PackageExport["AffineModifierForm"]

declareUnaryWrapperForm[AffineModifierForm];

(**************************************************************************************************)

PackageExport["ModuloForm"]

declareUnaryWrapperForm[ModuloForm]

(**************************************************************************************************)

PackageExport["GroupFunctionSymbol"]
PackageExport["GroupHomomorphismSymbol"]

GroupFunctionSymbol[] := GroupoidFunctionSymbol["\[Pi]"]

declareSymbolForm[GroupFunctionSymbol];
declareSymbolForm[GroupHomomorphismSymbol];

(**************************************************************************************************)

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

PackageExport["GroupoidSymbol"]

$groupoidAliases = <|
  "N" -> "Naturals",
  "C" -> "Complexes",
  "R" -> "Reals",
  "Z" -> "Integers",
  "Q" -> "Rationals"
|>

GroupoidSymbol["\[Gamma]"] := PathGroupoidSymbol["Q"];

declareAlgebraicSymbol[GroupoidSymbol, $groupoidAliases];

(********************************************)

PackageExport["ActionGroupoidSymbol"]

declareSymbolForm[ActionGroupoidSymbol, ActionSymbol];

(********************************************)

PackageExport["ActionSymbol"]
PackageExport["SelfActionSymbol"]

declareSymbolForm[ActionSymbol];
declareSymbolForm[SelfActionSymbol, GroupSymbol];

(**************************************************************************************************)

PackageExport["GroupSymbol"]

GroupSymbol[] := GroupSymbol["G"];

declareAlgebraicSymbol[GroupSymbol, $groupoidAliases];

(**************************************************************************************************)

PackageExport["FreeGroupForm"]

declareUnaryForm[FreeGroupForm];

(**************************************************************************************************)

PackageExport["CyclicGroupForm"]

declareBoxFormatting[
  CyclicGroupForm[n_] :> makeTemplateBox[n, "CyclicGroupForm"]
]

$TemplateKatexFunction["CyclicGroupForm"] = "cyclicGroup";

(**************************************************************************************************)

PackageExport["GroupDirectProductForm"]

declareInfixSymbol[GroupDirectProductForm, GroupSymbol, True];

(**************************************************************************************************)

PackageExport["GroupPresentationSymbol"]

declareSymbolForm[GroupPresentationSymbol] // usingCustomKatex["presentation"];


PackageExport["GroupPresentationForm"]
PackageExport["GroupRelationForm"]
PackageExport["GroupGeneratorSymbol"]
PackageExport["GroupRelatorSymbol"]

declareBoxFormatting[
  GroupPresentationForm[lhs_, rhs_] :>
    TemplateBox[
      {groupGeneratorBoxes @ lhs,
       groupRelationSetBoxes @ rhs},
      "GroupPresentationForm"
    ]
];

SetHoldAllComplete[groupGeneratorBoxes, groupRelationSetBoxes, groupRelationBoxes];

groupRelationSetBoxes = Case[
  {}         := MakeBoxes @ EmptySetSymbol;
  list_List  := TemplateBox[MapUnevaluated[groupRelationBoxes, list], "CommaRowForm"];
  relation_  := groupRelationBoxes @ relation;
]

groupRelationBoxes = Case[
  EqualForm[a_, b_]     := MakeBoxes @ GroupRelationForm[a, b];
  gr_GroupRelationForm  := MakeBoxes @ gr;
  r_GroupRelatorSymbol  := MakeBoxes @ r;
  a_                    := groupRelationTermBoxes @ a;
];

$TemplateKatexFunction["GroupPresentationForm"] = "groupPresentation"

groupGeneratorBoxes = Case[
  list_List               := TemplateBox[MapUnevaluated[%, list], "CommaRowForm"];
  s:(symP | _Integer)     := MakeBoxes @ GroupGeneratorSymbol @ s;
  CardinalSymbol[s_]      := MakeBoxes @ GroupGeneratorSymbol @ s;
  e_ ? unaryWrappedQ      := recurseWrapperBoxes[e, %];
  gr_GroupGeneratorSymbol := MakeBoxes @ gr;
,
  symP -> $rawSymbolP
]

declareSymbolForm[GroupGeneratorSymbol];
declareSymbolForm[GroupRelatorSymbol];

declareBoxFormatting[
  GroupRelationForm[a_, b_] :>
    TemplateBox[
      MapUnevaluated[groupRelationTermBoxes, {a, b}],
      "GroupRelationForm"
    ],
  GroupRelationForm[a_] :>
    TemplateBox[List @ groupRelationTermBoxes @ a, "GroupRelationForm"],
  GroupRelationForm[] :>
    SBox["GroupRelationSymbol"]
]

SetHoldAllComplete[groupRelationTermBoxes];

groupRelationTermBoxes = Case[
  list_List                   := TemplateBox[MapUnevaluated[%, list], "ImplicitGroupMultiplicationForm"];
  (Power|PowerForm|GroupPowerForm)[g_, e_]  := TemplateBox[{% @ g, makeQGBoxes @ e}, "GroupPowerForm"];
  1                           := MakeBoxes @ GroupElementSymbol["e"];
  s:symP                      := MakeBoxes @ GroupElementSymbol @ s;
  GroupInverseForm[e_]        := TemplateBox[List @ % @ e, "GroupInverseForm"];
  CardinalSymbol[s_]          := MakeBoxes @ GroupElementSymbol @ s;
  e_ ? unaryWrappedQ          := recurseWrapperBoxes[e, %] /. "InvertedForm" -> "GroupInverseForm";
  ge_GroupElementSymbol       := MakeBoxes @ ge;
  ge_GroupIdentitySymbol      := MakeBoxes @ ge;
  gg_GroupGeneratorSymbol     := MakeBoxes @ gg;
  gm_GroupMultiplicationForm  := MakeBoxes @ gm;
  gm_ImplicitGroupMultiplicationForm  := MakeBoxes @ gm;
  GroupCommutatorForm[a_, b_] := TemplateBox[{% @ a, % @ b}, "GroupCommutatorForm"];
,
  symP -> $rawSymbolP
];

declareInfixKatexAlias["GroupRelation", "groupRelationIso"];

(**************************************************************************************************)

$TemplateKatexFunction["IdentityElementForm"] = "identityElement"

(**************************************************************************************************)

PackageExport["GroupCommutatorForm"]

declareBinaryForm[GroupCommutatorForm];

(**************************************************************************************************)

PackageExport["GroupPowerForm"]

declareBinaryForm[GroupPowerForm];

(**************************************************************************************************)

PackageExport["FieldSymbol"]

FieldSymbol[] := FieldSymbol["K"];

$fieldAliases = <|
  "C" -> "Complexes",
  "R" -> "Reals",
  "Q" -> "Rationals"
|>

declareAlgebraicSymbol[FieldSymbol, $fieldAliases];

(**************************************************************************************************)

PackageExport["RingSymbol"]

RingSymbol[] := RingSymbol["R"];

$ringAliases = <|
  "Z" -> "Integers"
|>

declareAlgebraicSymbol[RingSymbol, $ringAliases];

(**************************************************************************************************)

PackageExport["SemiringSymbol"]

$semiringAliases = <|
  "N" -> "Naturals"
|>

declareAlgebraicSymbol[SemiringSymbol, $semiringAliases];

(********************************************)

declareRingForm[list_List] :=
  Scan[declareLieGroupOrAlgebraForm, list];

(**************************************************************************************************)

restCommaRiffled[name_][first_, rest__] := name[first, riffled[","][rest]]

declareDerivedRingForm[symbol_Symbol, type_:Automatic] := With[
  {name = SymbolName @ symbol},
  {katex = LowerCaseFirst @ StringTrim[name, "Form"]},
  declareBoxFormatting[
    symbol[ring_, args___] :> TemplateBox[
      Prepend[toHintedSymbol[ring -> RingSymbol]] @
        MapUnevaluated[toHintedSymbol[# -> type]&, {args}],
      name
    ]
  ];
  $TemplateKatexFunction[name] = restCommaRiffled[katex];
];

(**************************************************************************************************)

PackageExport["MatrixRingForm"]

declareDerivedRingForm[MatrixRingForm];

(**************************************************************************************************)

PackageExport["PolynomialSymbol"]

declareSymbolForm[PolynomialSymbol];

(**************************************************************************************************)

PackageExport["MultisetSemiringForm"]
PackageExport["SignedMultisetRingForm"]

(* why not declareDerivedRingForm ? *)
declareBinaryForm[MultisetSemiringForm];
declareBinaryForm[SignedMultisetRingForm];

PackageExport["MultisetSemiringSymbolForm"]
PackageExport["SignedMultisetRingSymbolForm"]

declareSymbolFormExplicit[MultisetSemiringSymbolForm];
declareSymbolFormExplicit[SignedMultisetRingSymbolForm];

PackageExport["MultisetSemiringProductForm"]
PackageExport["MultisetSemiringSumForm"]

declareInfixSymbol[MultisetSemiringProductForm] // usingCustomKatex["msrdot"];
declareInfixSymbol[MultisetSemiringSumForm] // usingCustomKatex["msrplus"];

PackageExport["SignedMultisetRingProductForm"]
PackageExport["SignedMultisetRingSumForm"]

declareInfixSymbol[SignedMultisetRingProductForm] // usingCustomKatex["smrdot"];
declareInfixSymbol[SignedMultisetRingSumForm] // usingCustomKatex["smrplus"];

(**************************************************************************************************)

PackageExport["PolynomialRingForm"]
PackageExport["IndeterminateSymbol"]

declareDerivedRingForm[PolynomialRingForm, IndeterminateSymbol];

declareSymbolForm[IndeterminateSymbol];

(********************************************)

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

PackageExport["TopologicalSpaceSymbolForm"]
PackageExport["BundleSectionSymbolForm"]
PackageExport["BundleProjectionSymbolForm"]

declareSymbolForm[TopologicalSpaceSymbolForm]
declareSymbolForm[BundleSectionSymbolForm]
declareSymbolForm[BundleProjectionSymbolForm]

(**************************************************************************************************)

PackageExport["BundleFunctionStyleForm"]
PackageExport["BundleGraphStyleForm"]
PackageExport["BundleProjectionStyleForm"]
PackageExport["BundleSectionStyleForm"]

f_BundleFunctionStyleForm[arg__] := AppliedForm[f, arg];
f_BundleGraphStyleForm[arg__] := AppliedForm[f, arg];
f_BundleProjectionStyleForm[arg__] := AppliedForm[f, arg];
f_BundleSectionStyleForm[arg__] := AppliedForm[f, arg];

declareUnaryForm[BundleFunctionStyleForm]
declareUnaryForm[BundleGraphStyleForm]
declareUnaryForm[BundleProjectionStyleForm]
declareUnaryForm[BundleSectionStyleForm]

PackageExport["TotalSpaceStyleForm"]
PackageExport["BaseSpaceStyleForm"]
PackageExport["FiberSpaceStyleForm"]

declareUnaryForm[TotalSpaceStyleForm]
declareUnaryForm[BaseSpaceStyleForm]
declareUnaryForm[FiberSpaceStyleForm]

PackageExport["BaseSpaceElementStyleForm"]
PackageExport["FiberSpaceElementStyleForm"]
PackageExport["TotalSpaceElementStyleForm"]

declareUnaryForm[BaseSpaceElementStyleForm]
declareUnaryForm[FiberSpaceElementStyleForm]
declareUnaryForm[TotalSpaceElementStyleForm]

(**************************************************************************************************)

PackageExport["TopologicalQuotientSpaceForm"]

declareBinaryForm[TopologicalQuotientSpaceForm]

(**************************************************************************************************)

PackageExport["CircleSpaceForm"]

declareUnaryForm[CircleSpaceForm];

declareBoxFormatting[
  CircleSpaceForm[] :> TemplateBox[{}, "CircleSpaceSymbol"]
]

(********************************************)

PackageExport["RealVectorSpaceForm"]
PackageExport["ComplexVectorSpaceForm"]

declareUnaryForm[RealVectorSpaceForm]
declareUnaryForm[ComplexVectorSpaceForm]

(********************************************)

PackageExport["GeneralLinearAlgebraForm"]
PackageExport["GeneralLinearGroupForm"]

declareLieGroupOrAlgebraForm[{GeneralLinearAlgebraForm, GeneralLinearGroupForm}];

PackageExport["SpecialLinearAlgebraForm"]
PackageExport["SpecialLinearGroupForm"]

declareLieGroupOrAlgebraForm[{SpecialLinearAlgebraForm, SpecialLinearGroupForm}];

PackageExport["ProjectiveGeneralLinearAlgebraForm"]
PackageExport["ProjectiveGeneralLinearGroupForm"]

declareLieGroupOrAlgebraForm[{ProjectiveGeneralLinearAlgebraForm, ProjectiveGeneralLinearGroupForm}];

PackageExport["ProjectiveSpecialLinearAlgebraForm"]
PackageExport["ProjectiveSpecialLinearGroupForm"]

declareLieGroupOrAlgebraForm[{ProjectiveSpecialLinearAlgebraForm, ProjectiveSpecialLinearGroupForm}];

PackageExport["OrthogonalAlgebraForm"]
PackageExport["OrthogonalGroupForm"]

declareLieGroupOrAlgebraForm[{OrthogonalAlgebraForm, OrthogonalGroupForm}];

PackageExport["SpecialOrthogonalAlgebraForm"]
PackageExport["SpecialOrthogonalGroupForm"]

declareLieGroupOrAlgebraForm[{SpecialOrthogonalAlgebraForm, SpecialOrthogonalGroupForm}];

PackageExport["UnitaryAlgebraForm"]
PackageExport["UnitaryGroupForm"]

declareLieGroupOrAlgebraForm[{UnitaryAlgebraForm, UnitaryGroupForm}];

PackageExport["SpecialUnitaryAlgebraForm"]
PackageExport["SpecialUnitaryGroupForm"]

declareLieGroupOrAlgebraForm[{SpecialUnitaryAlgebraForm, SpecialUnitaryGroupForm}];

PackageExport["SpinAlgebraForm"]
PackageExport["SpinGroupForm"]

declareLieGroupOrAlgebraForm[{SpinAlgebraForm, SpinGroupForm}];

PackageExport["PinAlgebraForm"]
PackageExport["PinGroupForm"]

declareLieGroupOrAlgebraForm[{PinAlgebraForm, PinGroupForm}];

(********************************************)

PackageExport["SymmetricGroupForm"]

declareUnaryForm[SymmetricGroupForm];

(**************************************************************************************************)

PackageExport["ContractionLatticeSymbol"]

SetUsage @ "
ContractionLatticeSymbol[q$] represents the lattice of contractions of a quiver q$.
"

declareSymbolForm[ContractionLatticeSymbol, QuiverSymbol];

(**************************************************************************************************)

PackageExport["ContractionProductForm"]
PackageExport["ContractionSumForm"]

declareInfixSymbol[{ContractionProductForm, ContractionSumForm}, None, True];

(**************************************************************************************************)

PackageExport["ContractionSetForm"]

ContractionSetForm[{RepeatedNull[{_}]}] := "";

ContractionSetForm[e_List] :=
  ContractionSumForm @@ (ContractionProductForm @@@ DeleteCases[e, {_}])

(**************************************************************************************************)

PackageExport["OrderedContractionSetForm"]

OrderedContractionSetForm[index_][set_] :=
  ContractionSetForm @ SortContractionSet[DeleteCases[set, {_}], index]

(**************************************************************************************************)

PackageExport["WordGroupSymbol"]
PackageExport["WordRingSymbol"]
PackageExport["PlanRingSymbol"]

(* TODO: factor this into declare01SymbolForm *)
declareSymbolForm[WordGroupSymbol, QuiverSymbol]
declareSymbolForm[WordRingSymbol, QuiverSymbol]
declareSymbolForm[PlanRingSymbol, QuiverSymbol]

declareBoxFormatting[
  WordGroupSymbol[] :> SBox["WordGroupSymbol"],
  WordRingSymbol[] :> SBox["WordRingSymbol"],
  PlanRingSymbol[] :> SBox["PlanRingSymbol"]
];

$TemplateKatexFunction["WordGroupSymbol"] = katexAlias["wordGroupSymbol"];
$TemplateKatexFunction["WordRingSymbol"] = katexAlias["wordRingSymbol"];
$TemplateKatexFunction["PlanRingSymbol"] = katexAlias["planRingSymbol"];

(**************************************************************************************************)

PackageExport["FromToForm"]

declareInfixSymbol[FromToForm, VertexSymbol, True];

(**************************************************************************************************)

PackageExport["RouteSymbol"]
PackageExport["MultirouteSymbol"]
PackageExport["PlanSymbol"]
PackageExport["MultiwordSymbol"]

declareSymbolFormExplicit[RouteSymbol];
declareSymbolFormExplicit[MultirouteSymbol];
declareSymbolFormExplicit[PlanSymbol];
declareSymbolFormExplicit[MultiwordSymbol];

(**************************************************************************************************)

PackageExport["RouteForm"]
PackageExport["MultirouteForm"]

RouteForm[a_, b_String, c_] := RouteForm[a, ToPathWord @ b, c];

declareBoxFormatting[
  RouteForm[t_, w_, h_] :>
    makeTypedTemplateBox[t -> generalizedVertexSymbol, w -> WordForm, h -> generalizedVertexSymbol, "RouteForm"],
  MultirouteForm[t_, w_, h_] :>
    makeTypedTemplateBox[t -> generalizedVertexSymbol, w, h -> generalizedVertexSymbol, "MultirouteForm"]
];

$TemplateKatexFunction["RouteForm"] = "route";
$TemplateKatexFunction["MultirouteForm"] = "multiroute";

(**************************************************************************************************)

PackageExport["SetTypeForm"]
PackageExport["SignedSetTypeForm"]
PackageExport["OrderedSetTypeForm"]
PackageExport["ListTypeForm"]
PackageExport["CyclicListTypeForm"]
PackageExport["TupleTypeForm"]
PackageExport["MultisetTypeForm"]
PackageExport["SignedMultisetTypeForm"]

PackageExport["PathTypeForm"]
PackageExport["VertexTypeForm"]
PackageExport["EdgeTypeForm"]

declareTypeForm::badsym = "Name of symbol `` should end in Form."
declareTypeForm[head_Symbol, katex_:Automatic] := With[
  {name = SymbolName @ head},
  If[!StringEndsQ[name, "Form"], ReturnFailed["badsym", head]];
  declareBoxFormatting[head[s_] :> TemplateBox[List @ makeQGBoxes @ s, name]];
  $unaryWrapperFormName[head] = name;
  $TemplateKatexFunction[name] = If[katex === Automatic, LowerCaseFirst @ StringDrop[name, -4], katex];
];

declareTypeForm[PathTypeForm]
declareTypeForm[VertexTypeForm]
declareTypeForm[EdgeTypeForm]

(* TODO: These aren't used *)
declareTypeForm[SetTypeForm]
declareTypeForm[SignedSetTypeForm]
declareTypeForm[OrderedSetTypeForm]
declareTypeForm[ListTypeForm]
declareTypeForm[CyclicListTypeForm]
declareTypeForm[TupleTypeForm]
declareTypeForm[MultisetTypeForm]
declareTypeForm[SignedMultisetTypeForm]

(**************************************************************************************************)

PackageExport["GroupElementSymbol"]
PackageExport["GroupoidElementSymbol"]
PackageExport["RingElementSymbol"]

GroupElementSymbol[] := GroupElementSymbol["g"];
GroupoidElementSymbol[] := GroupoidElementSymbol["g"];
RingElementSymbol[] := RingElementSymbol["r"];

declareSymbolForm[GroupElementSymbol];
declareSymbolForm[GroupoidElementSymbol];
declareSymbolForm[RingElementSymbol];

(**************************************************************************************************)

PackageExport["GroupIdentitySymbol"]
PackageExport["GroupoidIdentitySymbol"]
PackageExport["RingIdentitySymbol"]

GroupIdentitySymbol[] := GroupIdentitySymbol["e"];
GroupoidIdentitySymbol[] := GroupoidIdentitySymbol["e"];
RingIdentitySymbol[] := RingIdentitySymbol["e"];

declareSymbolForm[GroupIdentitySymbol];
declareSymbolForm[GroupoidIdentitySymbol];
declareSymbolForm[RingIdentitySymbol];

(**************************************************************************************************)

PackageExport["LinearCombinationCoefficientSymbol"]

declareSymbolForm[LinearCombinationCoefficientSymbol];

(**************************************************************************************************)

PackageExport["GroupInverseForm"]
PackageExport["GroupoidInverseForm"]

declareUnaryForm[GroupInverseForm, maybeParen[GroupElementSymbol|GroupGeneratorSymbol]];
declareUnaryForm[GroupoidInverseForm, maybeParen[GroupoidElementSymbol]];

(**************************************************************************************************)

PackageExport["MatrixPartForm"]
PackageExport["SubMatrixPartForm"]

declareBoxFormatting[
  MatrixPartForm[m_, i_, j_] :> makeHintedTemplateBox[m -> maybeParen[MatrixSymbol], i -> MatrixRowPartForm, j -> MatrixColumnPartForm, "MatrixPartForm"],
  SubMatrixPartForm[m_, i_, j_] :> makeHintedTemplateBox[m -> maybeParen[MatrixSymbol], i -> MatrixRowPartForm, j -> MatrixColumnPartForm, "SubMatrixPartForm"]
]

$TemplateKatexFunction["MatrixPartForm"] = "matrixPart";
$TemplateKatexFunction["SubMatrixPartForm"] = "subMatrixPart";

(**************************************************************************************************)

PackageExport["MatrixRowPartForm"]
PackageExport["MatrixColumnPartForm"]

declareUnaryWrapperForm[MatrixRowPartForm];
declareUnaryWrapperForm[MatrixColumnPartForm];

(**************************************************************************************************)

PackageExport["MatrixDotForm"]
PackageExport["MatrixPlusForm"]

declareInfixSymbol[MatrixDotForm, maybeParen[MatrixSymbol|TranslationVectorForm]]
declareInfixSymbol[MatrixPlusForm, maybeParen[MatrixSymbol|TranslationVectorForm]]

(**************************************************************************************************)

PackageExport["GroupMultiplicationForm"]
PackageExport["ImplicitGroupMultiplicationForm"]
PackageExport["GroupoidMultiplicationForm"]

$grouplikeTerms = Alternatives[
  GroupElementSymbol, GroupElementSymbol, GroupIdentitySymbol, GroupGeneratorSymbol, GroupPowerForm, GroupInverseForm, PathSymbol, TupleForm
];

$groupoidlikeTerms = Alternatives[
  GroupoidElementSymbol, GroupoidIdentitySymbol, GroupIdentitySymbol, GroupElementSymbol, GroupGeneratorSymbol, GroupPowerForm, GroupInverseForm, PathSymbol, TupleForm
];

declareInfixSymbol[GroupMultiplicationForm, maybeParen @ $grouplikeTerms] // usingCustomKatex["Gmult"];
declareInfixSymbol[ImplicitGroupMultiplicationForm, maybeParen @ $grouplikeTerms] // usingCustomKatex["iGmult"];
declareInfixSymbol[GroupoidMultiplicationForm, maybeParen @ $groupoidlikeTerms] // usingCustomKatex["gmult"];

(**************************************************************************************************)

PackageExport["MonoidProductForm"]

declareInfixSymbol[MonoidProductForm] // usingCustomKatex["mdot"];

(**************************************************************************************************)

PackageExport["SemigroupProductForm"]

declareInfixSymbol[SemigroupProductForm] // usingCustomKatex["sgdot"];

(**************************************************************************************************)

PackageExport["SemiringProductForm"]
PackageExport["SemiringSumForm"]

declareInfixSymbol[SemiringProductForm] // usingCustomKatex["srdot"];
declareInfixSymbol[SemiringSumForm] // usingCustomKatex["srplus"];

PackageExport["StyledSemiringProductForm"]
PackageExport["StyledSemiringSumForm"]


(**************************************************************************************************)

PackageExport["InfixForm"]

declareBoxFormatting[
  InfixForm[symbol_String][args___] :> makeTemplateBox[symbol, args, "InfixForm"],
  InfixForm[symbol_][args___] :> makeTemplateBox[symbol[], args, "InfixForm"],
  InfixForm[symbol_String] :> symbol,
  InfixForm[symbol_] :> MakeBoxes @ symbol[]
];

$TemplateKatexFunction["InfixForm"] = katexInfix;

katexInfix[op_, rest___] := Riffle[{rest}, RBox["\\,", op, "\\,"]];

(**************************************************************************************************)

PackageExport["StyledInfixForm"]

declareBoxFormatting[
  StyledInfixForm[style_, symbol_][args__] :> makeTemplateBox[style @ symbol[], args, "InfixForm"],
  StyledInfixForm[style_, symbol_] :> MakeBoxes @ style @ symbol[]
];

(**************************************************************************************************)

PackageExport["GroupSmallDotForm"]
PackageExport["GroupLargeDotForm"]

declareInfixSymbol[GroupSmallDotForm, maybeParen @ $grouplikeTerms] // usingCustomKatex["gdot"];
declareInfixSymbol[GroupLargeDotForm, maybeParen @ $grouplikeTerms] // usingCustomKatex["gDot"];

(**************************************************************************************************)

PackageExport["PathGroupoidSymbol"]

PathGroupoidSymbol[] := PathGroupoidSymbol["Q"];

declareBoxFormatting[
  PathGroupoidSymbol[q_] :>
    TemplateBox[List @ graphOrQuiverBoxes @ q, "PathGroupoidSymbolForm"]
]

graphOrQuiverBoxes = Case[
  g_GraphSymbol | g_QuiverSymbol := MakeBoxes @ g;
  c:(colorsP[_])                 := MakeBoxes @ c;
  s:symsP                        := MakeBoxes @ QuiverSymbol @ s;
  other_                         := makeQGBoxes @ other;
,
  {colorsP -> $colorFormP, symsP -> $rawSymbolP}
]


$TemplateKatexFunction["PathGroupoidSymbolForm"] = "pathGroupoid";

(**************************************************************************************************)

PackageExport["PathQuiverSymbol"]

SetUsage @ "
PathQuiverSymbol[q$] represents the path quiver on quiver q$.
"

declareSymbolForm[PathQuiverSymbol, QuiverSymbol];

(**************************************************************************************************)

PackageExport["ForwardPathQuiverSymbol"]

SetUsage @ "
ForwardPathQuiverSymbol[q$] represents the path quiver on quiver q$.
"

ForwardPathQuiverSymbol[] := ForwardPathQuiverSymbol["Q", "v"];
ForwardPathQuiverSymbol[q_] := ForwardPathQuiverSymbol[q, "v"];

declareBoxFormatting[
  HoldPattern[ForwardPathQuiverSymbol[q_, v_]] :>
    makeHintedTemplateBox[q -> QuiverSymbol, v -> VertexSymbol, "ForwardPathQuiverSymbolForm"]
];

$TemplateKatexFunction["ForwardPathQuiverSymbolForm"] = "forwardPathQuiver";


(**************************************************************************************************)

PackageExport["BackwardPathQuiverSymbol"]

SetUsage @ "
BackwardPathQuiverSymbol[q$] represents the path quiver on quiver q$.
"

BackwardPathQuiverSymbol[] := BackwardPathQuiverSymbol["Q", "v"];
BackwardPathQuiverSymbol[q_] := BackwardPathQuiverSymbol[q, "v"];

declareBoxFormatting[
  HoldPattern[BackwardPathQuiverSymbol[q_, v_]] :>
    makeTypedTemplateBox[q -> QuiverSymbol, v -> VertexSymbol, "BackwardPathQuiverSymbolForm"]
];

$TemplateKatexFunction["BackwardPathQuiverSymbolForm"] = "backwardPathQuiver";


(**************************************************************************************************)

PackageExport["LatticeQuiverForm"]

declareBoxFormatting[
  LatticeQuiverForm[fq_, v_, cv_, d_] :>
    makeHintedTemplateBox[fq -> QuiverSymbol, v -> VertexSymbol, cv -> FunctionSymbol, d -> SymbolForm, "LatticeQuiverForm"]
];

$TemplateKatexFunction["LatticeQuiverForm"] = applyRiffled["latticeBFS", ","];

(********************************************)

PackageExport["QuiverSymbol"]

QuiverSymbol[] := QuiverSymbol["Q"];

declareSymbolForm[QuiverSymbol];

declareBoxFormatting[
  QuiverSymbol[a_TransportAtlasSymbolForm] :> MakeBoxes @ a
]

(********************************************)

declareNamedRewritingSystem[symbol_] := With[
  {symbolName = SymbolName[symbol]},
  declareBoxFormatting[
    symbol[] :> SBox[symbolName],
    symbol[args__] :> TemplateBox[
      Prepend[SBox[symbolName]] @
      MapUnevaluated[rewritingRuleBoxes, {args}],
      "RewritingSystemRuleBindingForm"
    ]
  ];
  $TemplateKatexFunction[symbolName] = LowerCaseFirst @ StringTrim[symbolName, "Symbol"];
]

$TemplateKatexFunction["RewritingSystemRuleBindingForm"] = Function["rewritingRuleBinding"[#1, riffled[","][##2]]];

SetHoldAllComplete[rewritingRuleBoxes];

rewritingRuleBoxes = Case[
  a_ -> b_ := MakeBoxes @ RewritingRuleForm[a, b];
  other_   := makeQGBoxes @ other;
];

(********************************************)

PackageExport["GroupWordRewritingForm"]

declareBoxFormatting[
  GroupWordRewritingForm[args__] :>
    TemplateBox[
      MapUnevaluated[groupWordRewritingRuleBox, {args}],
      "GroupWordRewritingForm"
  ]
];

SetHoldAllComplete[groupWordRewritingRuleBox];
groupWordRewritingRuleBox = Case[
  a_ -> b_ := MakeBoxes @ RewritingRuleForm[a, b];
  other_   := makeQGBoxes @ other;
];

$TemplateKatexFunction["GroupWordRewritingForm"] = applyRiffled["groupWordRewriting", ","];

(********************************************)

PackageExport["PathWordRewritingForm"]

declareBoxFormatting[
  PathWordRewritingForm[args__] :>
    TemplateBox[
      MapUnevaluated[pathWordRewritingRuleBox, {args}],
      "GroupWordRewritingForm"
  ]
];

SetHoldAllComplete[pathWordRewritingRuleBox];
pathWordRewritingRuleBox = Case[
  a_ -> b_ := TemplateBox[{wordBoxes @ a, wordBoxes @ b}, "RewritingRuleForm"];
  other_   := makeQGBoxes @ other;
];

(********************************************)

PackageExport["VerticalModifierForm"]

declareBoxFormatting[
  VerticalModifierForm[inner_, args___] :> rewriteToVerticalOuter[makeQGBoxes[inner], args]
];

$verticalFormData = <|
  "GroupWordRewritingForm" -> {"\[LeftAngleBracket]", "\[RightAngleBracket]"},
  "ListForm" -> {SBox["LeftBrace"], SBox["RightBrace"]},
  "TupleForm" -> {"(", ")"},
  "SetForm" -> {SBox["LeftBrace"], SBox["RightBrace"]},
  "MultisetForm" -> {SBox["LeftMultisetBracket"], SBox["RightMultisetBracket"]},
  "AssociativeArrayForm" -> {"\[LeftAngleBracket]", "\[RightAngleBracket]"}
|>;

$TemplateKatexFunction["LeftBrace"] = katexAlias["lbrace"];
$TemplateKatexFunction["RightBrace"] = katexAlias["rbrace"];

$TemplateKatexFunction["LeftMultisetBracket"] = katexAlias["openMultiset"];
$TemplateKatexFunction["RightMultisetBracket"] = katexAlias["closeMultiset"];

makeFixedSpanning[char_] :=
  StyleBox[char, SpanMinSize -> 1.5, SpanMaxSize -> 1.5];

rewriteToVerticalOuter[arg_, opts:OptionsPattern[]] :=
  rewriteToVerticalOuter[arg, 1, opts];

Options[rewriteToVerticalOuter] = Options[VerticalModifierForm] = {
  Alignment -> Automatic
}

rewriteToVerticalOuter[arg_, n_Integer, opts:OptionsPattern[]] :=
  rewriteToVertical[arg, n, OptionValue[Alignment]]

rewriteToVertical[TemplateBox[args_, "AssociativeArrayForm"] /; Length[args] >= 2, n_, align_] := Scope[
  {l, r} = makeFixedSpanning /@ $verticalFormData["AssociativeArrayForm"];
  args = splice[{#1, SBox["MapsToSymbol"], #2}]& @@@ Part[args, All, 1];
  rows = Partition[args, n]; nrows = Length[rows];
  rows = PrependColumn[rows, Prepend[l] @ Table["", nrows-1]];
  rows = AppendColumn[rows, Append[r] @ Table["", nrows-1]];
  rows = rows /. splice -> Splice;
  rows = MapAt[addComma, rows, {All, 4 ;; -3 ;; 3}];
  rows = MapAt[addComma, rows, {;;-2, -2}];
  colSpacings = Flatten[{1, Table[{1, 1, 2}, n], 0} / 4];
  colSpacings[[-2]] = 0;
  TBox["GridForm"] @ createGridBox[rows, {Right, {Right, ReplaceAutomatic[align, Center], Left}, Left}, Automatic, colSpacings]
];

addComma[""] := "";
addComma[a_] := RBox[a, ","];

rewriteToVertical[TemplateBox[args_, form_String] /; KeyExistsQ[$verticalFormData, form], 1, align_] /; Length[args] >= 2 := Scope[
  {l, r} = makeFixedSpanning /@ $verticalFormData[form]; gap = spacerBox[2];
  l = RBox[l, gap]; r = RBox[gap, r];
  first = First[args]; last = Last[args]; middle = Part[args, 2 ;; -2];
  spacer = TemplateBox[{l}, "InvisibleForm"];
  rows = Map[List, Flatten @ {
    RBox[l, first, ","],
    RBox[spacer, #, ","]& /@ middle,
    RBox[spacer, last, r]
  }];
  TBox["GridForm"] @ createGridBox[rows, {ReplaceAutomatic[align, Left]}, Automatic, 0]
];

rewriteToVertical[TemplateBox[{style_, rest___}, form_ /; StringStartsQ[form, "Styled"]], args___] := Scope[
  res = rewriteToVertical[TemplateBox[{rest}, StringDrop[form, 6]], args];
  res /. GridBox[g_, opts___] :> GridBox[
      g // MapAt[styleOpenItem[style], {1, 1}] // MapAt[styleCloseItem[style], {-1, -1}],
      opts
  ]
];

styleOpenItem[s_][e_] := TBox[s][e];
styleOpenItem[s_][r_RowBox] := MapAt[styleOpenItem[s], r, {1, 1}];

styleCloseItem[s_][e_] := TBox[s][e];
styleCloseItem[s_][r_RowBox] := MapAt[styleCloseItem[s], r, {1, -1}];

rewriteToVertical[TemplateBox[args_, form_String] /; KeyExistsQ[$verticalFormData, form], n_, align_] /; Length[args] >= 2 := Scope[
  {l, r} = makeFixedSpanning /@ $verticalFormData[form];
  rows = Partition[args, n];
  spacer = TemplateBox[{l}, "InvisibleForm"];
  rows = {
    middleRow[l] @ First @ rows,
    Splice[middleRow[spacer] /@ Part[rows, 2 ;; -2]],
    lastRow[spacer, r] @ Last @ rows
  };
  TBox["GridForm"] @ createGridBox[rows, {Right, {ReplaceAutomatic[align, Center]}, Left}, Automatic, 0]
];

rewriteToVertical[other_, _, _] := other;

middleRow[l_][{most__}] := Join[{l}, commaRowBox /@ {most}, {""}];
lastRow[l_, r_][{most__, last_}] := Join[{l}, commaRowBox /@ {most}, {last, r}];

commaRowBox[""] := "";
commaRowBox[e_] := RowBox[TemplateBox[List @ ",", "InvisibleForm"], e, ","];

(********************************************)

(* TODO:
this combination of LocalStateSymbolForm, LocalState, and their corresponding katex matching things
should be factorized
*)

PackageExport["GlobalStateSymbol"]
PackageExport["RegionalStateSymbol"]
PackageExport["LocalStateSymbol"]

declareSymbolFormExplicit[LocalStateSymbol];
declareSymbolFormExplicit[RegionalStateSymbol];
declareSymbolFormExplicit[GlobalStateSymbol];

PackageExport["KeySubStateSymbol"]
PackageExport["ValueSubStateSymbol"]
declareSymbolFormExplicit[KeySubStateSymbol];
declareSymbolFormExplicit[ValueSubStateSymbol];

(********************************************)

PackageExport["RegionalSubstateForm"]
PackageExport["RegionalSuperstateForm"]
PackageExport["IncomparableRegionalStatesForm"]
PackageExport["ComparableRegionalStatesForm"]

declareInfixSymbol[RegionalSubstateForm];
declareInfixSymbol[RegionalSuperstateForm];
declareInfixSymbol[IncomparableRegionalStatesForm];
declareInfixSymbol[ComparableRegionalStatesForm];

(********************************************)

PackageExport["LHSStateForm"]
PackageExport["RHSStateForm"]

declareUnaryForm[LHSStateForm] // usingCustomKatex["lhsState"];
declareUnaryForm[RHSStateForm] // usingCustomKatex["rhsState"];

(********************************************)

PackageExport["RewriteLHSRegionalStateForm"]
PackageExport["RewriteRHSRegionalStateForm"]

declareUnaryForm[RewriteLHSRegionalStateForm]
declareUnaryForm[RewriteRHSRegionalStateForm]

(********************************************)

PackageExport["LocalStateForm"]
PackageExport["RegionalStateForm"]

declareBinaryForm[LocalStateForm]
declareNAryForm[RegionalStateForm, EmptyRegionalState];

PackageExport["StringRegionalStateForm"]

StringRegionalStateForm[str_, {i_, j_}] /; j < i :=
  makeStrRegState[str, Join[Range[i, i + StringLength[str] - j - 1], Range[1, j]]];

StringRegionalStateForm[str_, {i_, j_}] :=
  makeStrRegState[str, Range[i, j]];

makeStrRegState[chars_, indices_] := makeRegState[LiteralCharacterForm /@ Characters @ chars, indices];
makeRegState[states_, keys_] := RegionalStateForm @@ MapThread[LocalStateForm, {keys, states}];

(********************************************)

PackageExport["InvalidRegionalState"]
PackageExport["EmptyRegionalState"]

declareConstantSymbol[{InvalidRegionalState, EmptyRegionalState}];

(********************************************)

PackageExport["LocalStatesForm"]
PackageExport["GlobalStatesForm"]
PackageExport["RegionalStatesForm"]
declareUnaryForm[LocalStatesForm, RewritingSystemSymbol];
declareUnaryForm[GlobalStatesForm, RewritingSystemSymbol];
declareUnaryForm[RegionalStatesForm, RewritingSystemSymbol];

PackageExport["KeySubStatesForm"]
PackageExport["ValueSubStatesForm"]
declareUnaryForm[KeySubStatesForm, RewritingSystemSymbol];
declareUnaryForm[ValueSubStatesForm, RewritingSystemSymbol];

(********************************************)

PackageExport["InfixStateComposeForm"]
PackageExport["InfixStateMeetForm"]
PackageExport["InfixStateJoinForm"]

declareInfixSymbol[InfixStateComposeForm];
declareInfixSymbol[InfixStateMeetForm];
declareInfixSymbol[InfixStateJoinForm];

(********************************************)

PackageExport["GenericRewritingSystemSymbol"]
PackageExport["StringRewritingSystemSymbol"]
PackageExport["CircularStringRewritingSystemSymbol"]
PackageExport["TuringMachineRewritingSystemSymbol"]
PackageExport["GraphRewritingSystemSymbol"]
PackageExport["HypergraphRewritingSystemSymbol"]
PackageExport["CellularAutomatonRewritingSystemSymbol"]
PackageExport["PetriNetRewritingSystemSymbol"]

declareNamedRewritingSystem[GenericRewritingSystemSymbol];
declareNamedRewritingSystem[StringRewritingSystemSymbol];
declareNamedRewritingSystem[CircularStringRewritingSystemSymbol];
declareNamedRewritingSystem[TuringMachineRewritingSystemSymbol];
declareNamedRewritingSystem[GraphRewritingSystemSymbol];
declareNamedRewritingSystem[HypergraphRewritingSystemSymbol];
declareNamedRewritingSystem[CellularAutomatonRewritingSystemSymbol];
declareNamedRewritingSystem[PetriNetRewritingSystemSymbol];

(********************************************)

PackageExport["RewritingSystemStateBindingForm"]

declareBoxFormatting[
  RewritingSystemStateBindingForm[sys_, state_] :>
    makeHintedTemplateBox[sys -> RewritingSystemSymbol, state, "RewritingSystemStateBindingForm"]
]

$TemplateKatexFunction["RewritingSystemStateBindingForm"] = "rewritingStateBinding";

(********************************************)

PackageExport["RewritingSystemSymbol"]

RewritingSystemSymbol[] := RewritingSystemSymbol["R"];

declareSymbolForm[RewritingSystemSymbol];

(**************************************************************************************************)

PackageExport["MultiwayGraphForm"]

declareBoxFormatting[
  MultiwayGraphForm[rs_, init_, d_] :>
    makeHintedTemplateBox[rs -> RewritingSystemSymbol, init, d -> SymbolForm, "MultiwayGraphForm"],
  MultiwayGraphForm[rs_, d_] :>
    makeHintedTemplateBox[rs -> RewritingSystemSymbol, d -> SymbolForm, "MultiwayGraphForm"]
];

$TemplateKatexFunction["MultiwayGraphForm"] = applyRiffled["multiwayBFS", ","];

(********************************************)

PackageExport["RewriteForm"]

declareBinaryForm[RewriteForm]

declareBoxFormatting[
  RewriteForm[a_, b_, i_] :> MakeBoxes[RewriteForm[a, b]]
];

(********************************************)

PackageExport["RewritingRuleForm"]

declareBinaryForm[RewritingRuleForm]

(********************************************)

PackageExport["QuiverSizeSymbol"]

declareBoxFormatting[
  QuiverSizeSymbol[Null] :> "",
  QuiverSizeSymbol[n_Integer] :> MakeBoxes @ n,
  QuiverSizeSymbol[Infinity] :> "\[Infinity]",
  QuiverSizeSymbol[Modulo[n_]] :> MakeBoxes @ ModuloForm @ n,
  QuiverSizeSymbol[other_] :> makeQGBoxes @ other
]

(********************************************)

declareNamedBindingSymbol[symbol_] := With[
  {symbolName = SymbolName[symbol]},
  {formName = symbolName <> "Form"},
  declareBoxFormatting[
    symbol[dim_] :> makeHintedTemplateBox[dim -> rawSymbolBoxes, formName],
    q_symbol[cards__] :> MakeBoxes @ CardinalSizeBindingForm[q, cards]
  ];
  $TemplateKatexFunction[formName] = LowerCaseFirst @ StringTrim[symbolName, "Symbol"];
]

(********************************************)

PackageExport["StarModifierForm"]

declareUnaryWrapperForm[StarModifierForm];

(********************************************)

PackageExport["TranslationPathValuationSymbol"]
PackageExport["StarTranslationPathValuationSymbol"]

declareNamedBindingSymbol[TranslationPathValuationSymbol];
declareNamedBindingSymbol[StarTranslationPathValuationSymbol];

(********************************************)

PackageExport["TranslationWordHomomorphismSymbol"]
PackageExport["StarTranslationWordHomomorphismSymbol"]

declareNamedBindingSymbol[TranslationWordHomomorphismSymbol];
declareNamedBindingSymbol[StarTranslationWordHomomorphismSymbol];


(********************************************)

PackageExport["TranslationPresentationSymbol"]
PackageExport["StarTranslationPresentationSymbol"]

declareNamedBindingSymbol[TranslationPresentationSymbol];
declareNamedBindingSymbol[StarTranslationPresentationSymbol];

(********************************************)

declareNamedQuiverSymbol[symbol_] := With[
  {symbolName = SymbolName[symbol]},
  {katexName = LowerCaseFirst @ StringTrim[symbolName, "Symbol"]},
  AppendTo[$literalSymbolsP, symbol];
  declareBoxFormatting[
    symbol :> SBox[symbolName],
    symbol[] :> MakeBoxes @ symbol @ Infinity,
    symbol[size_] :> MakeBoxes @ SubSizeBindingForm[symbol, size],
    symbol[size_Rule] :> MakeBoxes @ CardinalSizeBindingForm[symbol, size],
    symbol[size__] | symbol[TupleForm[size__]] :> MakeBoxes @ CardinalSizeBindingForm[symbol, size],
    q_symbol[cards__] :> MakeBoxes @ CardinalSizeBindingForm[q, cards]
  ];
  $TemplateKatexFunction[symbolName] = katexAlias @ katexName;
]

declareTwoParameterNamedQuiverSymbol[symbol_] := With[
  {symbolName = SymbolName[symbol]},
  {formName = symbolName <> "Form"},
  {compactFormName = "Compact" <> symbolName <> "Form"},
  {katexName = LowerCaseFirst @ StringTrim[symbolName, "Symbol"]},
  declareBoxFormatting[
    symbol[k_] :> makeTemplateBox[k, formName],
    symbol[k_, size_] :> makeHintedTemplateBox[k, size -> QuiverSizeSymbol, compactFormName],
    symbol[k_, size__] | symbol[k_, TupleForm[size__]] :> MakeBoxes @ CardinalSizeBindingForm[symbol[k], size],
    q_symbol[cards__] :> MakeBoxes @ CardinalSizeBindingForm[q, cards]
  ];
  $TemplateKatexFunction[compactFormName] = "subSize"[katexName[#1], #2]&;
  $TemplateKatexFunction[formName] = katexName;
]

(********************************************)

PackageExport["BouquetQuiverSymbol"]
PackageExport["GridQuiverSymbol"]
PackageExport["TreeQuiverSymbol"]

declareTwoParameterNamedQuiverSymbol[BouquetQuiverSymbol];
declareTwoParameterNamedQuiverSymbol[GridQuiverSymbol];
declareTwoParameterNamedQuiverSymbol[TreeQuiverSymbol];

PackageExport["LineQuiverSymbol"]
PackageExport["CycleQuiverSymbol"]
PackageExport["SquareQuiverSymbol"]
PackageExport["CubicQuiverSymbol"]
PackageExport["TriangularQuiverSymbol"]
PackageExport["HexagonalQuiverSymbol"]
PackageExport["RhombilleQuiverSymbol"]

declareNamedQuiverSymbol[LineQuiverSymbol];
declareNamedQuiverSymbol[CycleQuiverSymbol];
declareNamedQuiverSymbol[SquareQuiverSymbol];
declareNamedQuiverSymbol[TriangularQuiverSymbol];
declareNamedQuiverSymbol[HexagonalQuiverSymbol];
declareNamedQuiverSymbol[RhombilleQuiverSymbol];
declareNamedQuiverSymbol[CubicQuiverSymbol];

(********************************************)

PackageExport["DividesForm"]

declareInfixSymbol[DividesForm];

(********************************************)

PackageExport["CoversForm"]
PackageExport["CoveredByForm"]
PackageExport["StrictlyCoversForm"]
PackageExport["StrictlyCoveredByForm"]

declareInfixSymbol[{CoversForm, CoveredByForm, StrictlyCoversForm, StrictlyCoveredByForm}, GraphSymbol];

(********************************************)

PackageExport["GraphIndexedCoveringForm"]
PackageExport["QuiverIndexedCoveringForm"]

declareBoxFormatting[
  GraphIndexedCoveringForm[pi_, g_, h_] :> makeHintedTemplateBox[pi -> GraphHomomorphismSymbol, g -> GraphSymbol, h -> GraphSymbol, "GraphIndexedCoveringForm"],
  QuiverIndexedCoveringForm[pi_, g_, h_] :> makeHintedTemplateBox[pi -> GraphHomomorphismSymbol, g -> QuiverSymbol, h -> QuiverSymbol, "QuiverIndexedCoveringForm"]
];

$TemplateKatexFunction["IndexedCoveringForm"] = "graphCovering";
$TemplateKatexFunction["GraphIndexedCoveringForm"] = "graphCovering";
$TemplateKatexFunction["QuiverIndexedCoveringForm"] = "quiverCovering";

(********************************************)

PackageExport["GraphSymbol"]

GraphSymbol[] := GraphSymbol["G"];

declareSymbolForm[GraphSymbol];

(********************************************)

PackageExport["MatrixSymbol"]

MatrixSymbol[] := MatrixSymbol["M"];

declareSymbolForm[MatrixSymbol];

(**************************************************************************************************)

PackageExport["NullPath"]
PackageExport["NullElement"]

declareConstantSymbol[{NullPath, NullElement}];

(********************************************)

PackageExport["PathSymbol"]

PathSymbol[] := PathSymbol["P"];

declareSymbolForm[PathSymbol];

(**************************************************************************************************)

PackageExport["EdgeSymbol"]

declareSymbolForm[EdgeSymbol];

(**************************************************************************************************)

PackageExport["VertexSymbol"]
PackageExport["HeadVertexForm"]
PackageExport["TailVertexForm"]

declareSymbolForm[VertexSymbol];
declareUnaryForm[TailVertexForm];
declareUnaryForm[HeadVertexForm];

declareBoxFormatting[
  VertexSymbol[e:(_[___])] :> TemplateBox[List @ MakeBoxes @ e, "VertexSymbolForm"]
];

$TemplateKatexFunction["HeadVertexForm"] = "hvert";
$TemplateKatexFunction["TailVertexForm"] = "tvert";
$TemplateKatexFunction["VertexSymbolForm"] = "vert";

(* for legacy notebooks: *)
$TemplateKatexFunction["HeadVertexSymbolForm"] = "hvert";
$TemplateKatexFunction["TailVertexSymbolForm"] = "tvert";

(**************************************************************************************************)

PackageExport["CardinalSymbol"]

declareSymbolForm[CardinalSymbol];

$TemplateKatexFunction["CardinalSymbolForm"] = "card";
$TemplateKatexFunction["InvertedCardinalSymbolForm"] = "ncard";
$TemplateKatexFunction["MirrorCardinalSymbolForm"] = "mcard";
$TemplateKatexFunction["InvertedMirrorCardinalSymbolForm"] = "nmcard";

(* for legacy notebooks: *)
$TemplateKatexFunction["NegatedMirrorCardinalSymbolForm"] = "nmcard";
$TemplateKatexFunction["NegatedCardinalSymbolForm"] = "ncard";

(**************************************************************************************************)

PackageExport["MirrorForm"]

declareUnaryWrapperForm[MirrorForm]

declareBoxFormatting[
  m:MirrorForm[_CardinalSymbol | _InvertedForm] :> cardinalBox @ m
]



(**************************************************************************************************)

PackageExport["ParenthesesLabeledForm"]
PackageExport["ParenthesesRepeatedForm"]
PackageExport["UnderbraceLabeledForm"]
PackageExport["UnderbraceRepeatedForm"]
PackageExport["OverbraceLabeledForm"]
PackageExport["OverbraceRepeatedForm"]

declareBoxFormatting[
  ParenthesesLabeledForm[a_, l_] :> makeTemplateBox[a, l, "ParenthesesLabeledForm"],
  ParenthesesRepeatedForm[a_, n_] :> makeTemplateBox[a, n, "ParenthesesRepeatedForm"],
  UnderbraceLabeledForm[a_, l_] :> makeTemplateBox[a, l, "UnderbraceLabeledForm"],
  UnderbraceRepeatedForm[a_, n_] :> makeTemplateBox[a, n, "UnderbraceRepeatedForm"],
  OverbraceLabeledForm[a_, l_] :> makeTemplateBox[a, l, "OverbraceLabeledForm"],
  OverbraceRepeatedForm[a_, n_] :> makeTemplateBox[a, n, "OverbraceRepeatedForm"]
];

$TemplateKatexFunction["ParenthesesLabeledForm"] = "parenLabeled";
$TemplateKatexFunction["ParenthesesRepeatedForm"] = "parenRepeated";
$TemplateKatexFunction["UnderbraceLabeledForm"] = "underLabeled";
$TemplateKatexFunction["UnderbraceRepeatedForm"] = "underRepeated";
$TemplateKatexFunction["OverbraceLabeledForm"] = "overLabeled";
$TemplateKatexFunction["OverbraceRepeatedForm"] = "overRepeated";

(**************************************************************************************************)

PackageExport["ModulusLabeledForm"]

declareBoxFormatting[
  ModulusLabeledForm[a_, m_] :> makeTemplateBox[a, m, "ModulusLabeledForm"]
];

$TemplateKatexFunction["ModulusLabeledForm"] = "modLabeled";

(**************************************************************************************************)

PackageExport["MathTextForm"]

declareBoxFormatting[
  MathTextForm[l_] :> TemplateBox[List @ TextString @ l, "MathTextForm"]
];

$TemplateKatexFunction["MathTextForm"] = "textrm";

(**************************************************************************************************)

PackageExport["DirectedEdgeForm"]

declareBoxFormatting[
  DirectedEdgeForm[a_, b_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, "DirectedEdgeForm"],
  DirectedEdgeForm[a_, b_, c_] :>
    toLongEdgeForm @ makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, c -> CardinalSymbol, "TaggedDirectedEdgeForm"],
  DirectedEdgeForm[a_, b_, c_, d_] :>
    toLongEdgeForm @ makeTemplateBox[a, b, c, d, "MultiTaggedDirectedEdgeForm"]

]

longTagQ[e_] := Count[e, "CardinalSymbolForm", {0, Infinity}] > 1;
longTagQ[TemplateBox[_, "CardinalProductForm"]] := True;

toLongEdgeForm[TemplateBox[{a_, b_, tag_}, form_String]] /; longTagQ[tag] :=
  TemplateBox[{a, b, tag}, "Long" <> form];

toLongEdgeForm[other_] := other;

$TemplateKatexFunction["DirectedEdgeForm"] = "de";
$TemplateKatexFunction["TaggedDirectedEdgeForm"] = "tde";
$TemplateKatexFunction["MultiTaggedDirectedEdgeForm"] = "mtde";
$TemplateKatexFunction["LongTaggedDirectedEdgeForm"] = "tde";

(**************************************************************************************************)

PackageExport["SerialCardinal"]
PackageExport["ParallelCardinal"]

declareBoxFormatting[
  SerialCardinal[args__] :>
    naryCardinalForm[{args}, "SerialCardinalForm"],
  ParallelCardinal[args__] :>
    naryCardinalForm[{args}, "ParallelCardinalForm"]
]

SetHoldAllComplete[naryCardinalForm];

naryCardinalForm[args_, form_] :=
  TemplateBox[MapUnevaluated[maybeParen[CardinalSymbol|InvertedForm|Inverted], args], form];

$TemplateKatexFunction["SerialCardinalForm"] = katexAliasRiffled["serialCardSymbol"];
$TemplateKatexFunction["ParallelCardinalForm"] = katexAliasRiffled["parallelCardSymbol"];

(**************************************************************************************************)

PackageExport["ComponentSuperQuiverOfForm"]

declareInfixSymbol[ComponentSuperQuiverOfForm]

(**************************************************************************************************)

PackageExport["UndirectedEdgeForm"]

declareBoxFormatting[
  UndirectedEdgeForm[a_, b_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, "UndirectedEdgeForm"],
  UndirectedEdgeForm[a_, b_, c_] :>
    toLongEdgeForm @ makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, c -> CardinalSymbol, "TaggedUndirectedEdgeForm"]
]

$TemplateKatexFunction["UndirectedEdgeForm"] = "de";
$TemplateKatexFunction["TaggedUndirectedEdgeForm"] = "tue";
$TemplateKatexFunction["LongTaggedUndirectedEdgeForm"] = "tue";

(**************************************************************************************************)

PackageExport["QuiverProductAppliedForm"]
PackageExport["CompactQuiverProductAppliedForm"]

declareBoxFormatting[
  QuiverProductAppliedForm[poly_, graphs__] :> makeTemplateBox[poly, graphs, "QuiverProductAppliedForm"],
  CompactQuiverProductAppliedForm[poly_, graphs__] :> makeTemplateBox[poly, graphs, "CompactQuiverProductAppliedForm"]
]

$TemplateKatexFunction["QuiverProductAppliedForm"] = quiverProductAppliedKatex;
$TemplateKatexFunction["CompactQuiverProductAppliedForm"] = compactQuiverProductAppliedKatex;

quiverProductAppliedKatex[f_, args___] := {"\\frac{", f, "} {", Riffle[{args}, ","], "}"};

compactQuiverProductAppliedKatex[f_, arg_] := {"{", f, "} / {", arg, "}"};
compactQuiverProductAppliedKatex[f_, args__] := {"{", f, "} / {(", Riffle[{args}, ","], ")}"};

(**************************************************************************************************)

PackageExport["IsContractedForm"]
PackageExport["IsNotContractedForm"]

declareBoxFormatting[
  IsContractedForm[a_, b_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, "IsContractedForm"],
  IsContractedForm[a_, b_, q_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, q -> QuiverSymbol, "IsContractedInForm"],
  IsNotContractedForm[a_, b_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, "IsNotContractedForm"],
  IsNotContractedForm[a_, b_, q_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, q -> QuiverSymbol, "IsNotContractedInForm"]
];

$TemplateKatexFunction["IsContractedForm"] = "isContracted"
$TemplateKatexFunction["IsContractedInForm"] = "isContractedIn"
$TemplateKatexFunction["IsNotContractedForm"] = "isNotContracted"
$TemplateKatexFunction["IsNotContractedInForm"] = "isNotContractedIn"

(**************************************************************************************************)

PackageExport["ContractedRelationForm"]

declareUnaryForm[ContractedRelationForm];

(**************************************************************************************************)

PackageExport["AppliedRelationForm"]

declareBoxFormatting[
  AppliedRelationForm[a_, b_, c_] :> makeTemplateBox[a, b, c, "AppliedRelationForm"]
];

$TemplateKatexFunction["AppliedRelationForm"] = applyRiffled["appliedRelation", " "];

(**************************************************************************************************)

PackageExport["LatticeSymbol"]
PackageExport["MeetSemilatticeSymbol"]
PackageExport["JoinSemilatticeSymbol"]
PackageExport["PosetSymbol"]

declareSymbolFormExplicit[LatticeSymbol]
declareSymbolFormExplicit[MeetSemilatticeSymbol]
declareSymbolFormExplicit[JoinSemilatticeSymbol]
declareSymbolFormExplicit[PosetSymbol]

(**************************************************************************************************)

PackageExport["LatticeTopSymbol"]
PackageExport["LatticeBottomSymbol"]
PackageExport["SemilatticeTopSymbol"]
PackageExport["SemilatticeBottomSymbol"]

declareConstantSymbol[{LatticeTopSymbol, LatticeBottomSymbol, SemilatticeTopSymbol, SemilatticeBottomSymbol}];

(**************************************************************************************************)

PackageExport["LatticeElementSymbol"]
PackageExport["MeetSemilatticeElementSymbol"]
PackageExport["JoinSemilatticeElementSymbol"]
PackageExport["PosetElementSymbol"]

declareSymbolFormExplicit[LatticeElementSymbol];
declareSymbolFormExplicit[MeetSemilatticeElementSymbol];
declareSymbolFormExplicit[JoinSemilatticeElementSymbol];
declareSymbolFormExplicit[PosetElementSymbol];

(**************************************************************************************************)

PackageExport["LatticeMeetForm"]
PackageExport["LatticeJoinForm"]

declareInfixSymbol[{LatticeMeetForm, LatticeJoinForm}, LatticeElementSymbol];

(**************************************************************************************************)

PackageExport["SemilatticeMeetForm"]
PackageExport["SemilatticeJoinForm"]

declareInfixSymbol[SemilatticeMeetForm, MeetSemilatticeElementSymbol];
declareInfixSymbol[SemilatticeJoinForm, JoinSemilatticeElementSymbol];

(**************************************************************************************************)

PackageExport["SemilatticeSemimeetForm"]
PackageExport["SemilatticeSemijoinForm"]

declareInfixSymbol[SemilatticeSemimeetForm, JoinSemilatticeElementSymbol];
declareInfixSymbol[SemilatticeSemijoinForm, MeetSemilatticeElementSymbol];

(**************************************************************************************************)

PackageExport["PosetGreaterForm"]
PackageExport["PosetGreaterEqualForm"]
PackageExport["PosetLessForm"]
PackageExport["PosetLessEqualForm"]
PackageExport["PosetCoversForm"]
PackageExport["PosetCoveredByForm"]

declareInfixSymbol[PosetGreaterForm, PosetElementSymbol];
declareInfixSymbol[PosetGreaterEqualForm, PosetElementSymbol];
declareInfixSymbol[PosetLessForm, PosetElementSymbol];
declareInfixSymbol[PosetLessEqualForm, PosetElementSymbol];
declareInfixSymbol[PosetCoversForm, PosetElementSymbol];
declareInfixSymbol[PosetCoveredByForm, PosetElementSymbol];

(**************************************************************************************************)

PackageExport["LatticeGreaterForm"]
PackageExport["LatticeGreaterEqualForm"]
PackageExport["LatticeLessForm"]
PackageExport["LatticeLessEqualForm"]

declareInfixSymbol[LatticeGreaterForm, LatticeElementSymbol];
declareInfixSymbol[LatticeGreaterEqualForm, LatticeElementSymbol];
declareInfixSymbol[LatticeLessForm, LatticeElementSymbol];
declareInfixSymbol[LatticeLessEqualForm, LatticeElementSymbol];

(**************************************************************************************************)

PackageExport["GraphProductForm"]
PackageExport["GraphUnionForm"]

PackageExport["DependentQuiverProductForm"]

PackageExport["LockedQuiverProductForm"]
PackageExport["LeftFreeQuiverProductForm"]
PackageExport["RightFreeQuiverProductForm"]

PackageExport["StrongIndependentQuiverProductForm"]
PackageExport["StrongQuiverProductForm"]

PackageExport["CartesianQuiverProductForm"]

(* PackageExport["LeftStrongQuiverProductForm"]
PackageExport["RightStrongQuiverProductForm"]

PackageExport["LeftFiberQuiverProductForm"]
PackageExport["RightFiberQuiverProductForm"]
 *)
declareInfixSymbol[
  {GraphUnionForm, GraphProductForm, DependentQuiverProductForm,
    LockedQuiverProductForm,
    LeftFreeQuiverProductForm, RightFreeQuiverProductForm,
    StrongIndependentQuiverProductForm,
    CartesianQuiverProductForm,
    StrongQuiverProductForm
    (* , LeftFiberQuiverProductForm, RightFiberQuiverProductForm,
    LeftStrongQuiverProductForm, RightStrongQuiverProductForm,  *)},
  QuiverSymbol
];

(**************************************************************************************************)

PackageExport["VerticalVertexProductForm"]
PackageExport["VerticalCardinalProductForm"]

PackageExport["VertexProductForm"]
PackageExport["EdgeProductForm"]
PackageExport["CardinalProductForm"]

declareBinaryForm[VerticalVertexProductForm];
declareBinaryForm[VerticalCardinalProductForm];

declareCommaRiffledForm[VertexProductForm, "vertexProduct"];
declareCommaRiffledForm[EdgeProductForm, "edgeProduct"];
declareCommaRiffledForm[CardinalProductForm, "cardinalProduct"];

(**************************************************************************************************)

PackageExport["CardinalSequenceForm"]

declareInfixSymbol[CardinalSequenceForm, CardinalSymbol];

(**************************************************************************************************)

PackageExport["PlaceholderVertexSymbol"]
PackageExport["HeadVertexSymbol"]
PackageExport["TailVertexSymbol"]

declareConstantSymbol[PlaceholderVertexSymbol] // usingCustomKatex["placeholderVertexSymbol"];
declareConstantSymbol[HeadVertexSymbol] // usingCustomKatex["headVertexSymbol"];
declareConstantSymbol[TailVertexSymbol] // usingCustomKatex["tailVertexSymbol"];

(**************************************************************************************************)

PackageExport["InverseArrowheadSymbol"]
PackageExport["ArrowheadSymbol"]
PackageExport["UpArrowheadSymbol"]
PackageExport["DownArrowheadSymbol"]
PackageExport["LeftArrowheadSymbol"]
PackageExport["RightArrowheadSymbol"]

declareConstantSymbol[{ArrowheadSymbol, InverseArrowheadSymbol, UpArrowheadSymbol, DownArrowheadSymbol, LeftArrowheadSymbol, RightArrowheadSymbol}];

(**************************************************************************************************)

PackageExport["ForwardFactorSymbol"]
PackageExport["BackwardFactorSymbol"]
PackageExport["NeutralFactorSymbol"]
PackageExport["ForwardBackwardFactorSymbol"]
PackageExport["ForwardBackwardNeutralFactorSymbol"]

declareConstantSymbol[
  {ForwardFactorSymbol, BackwardFactorSymbol, NeutralFactorSymbol,
  ForwardBackwardFactorSymbol, ForwardBackwardNeutralFactorSymbol}
];

(**************************************************************************************************)

PackageExport["SetCardinalityForm"]
PackageExport["MultisetCardinalityForm"]

declareUnaryForm[SetCardinalityForm];
declareUnaryForm[MultisetCardinalityForm];

(**************************************************************************************************)

PackageExport["ClosedIntervalForm"]
PackageExport["OpenIntervalForm"]
PackageExport["ClosedOpenIntervalForm"]
PackageExport["OpenClosedIntervalForm"]

declareBinaryForm[ClosedIntervalForm];
declareBinaryForm[OpenIntervalForm];
declareBinaryForm[ClosedOpenIntervalForm];
declareBinaryForm[OpenClosedIntervalForm];

(**************************************************************************************************)

PackageExport["IntegerRangeForm"]

declareBoxFormatting[
  IntegerRangeForm[1, n_] :> makeTemplateBox[n, "OneToNForm"],
  IntegerRangeForm[0, n_] :> makeTemplateBox[n, "ZeroToNForm"]
]

$TemplateKatexFunction["OneToNForm"] = "oneTo"
$TemplateKatexFunction["ZeroToNForm"] = "zeroTo"

(**************************************************************************************************)

PackageExport["LimitForm"]

declareBinaryForm[LimitForm];

(**************************************************************************************************)

declareSumLikeFormatting[form_Symbol, katexName_String] := With[
  {formName = SymbolName @ form},
  {symbolName = StringTrim[formName, "Form"] <> "Symbol"},
  declareBoxFormatting[
    form[] :> SBox[symbolName],
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

PackageExport["StyledOperatorForm"]

declareBoxFormatting[
  StyledOperatorForm[f_][e_] :>
    TemplateBox[{makeQGBoxes @ f @ "|", makeQGBoxes @ e}, "StyledOperatorForm"]
];

$TemplateKatexFunction["StyledOperatorForm"] = styledOperatorKatex;

styledOperatorKatex[dummy_, inner_] := Scope[
  katexStr = boxesToKatexString @ inner;
  styleOp = First @ StringCases[boxesToKatexString @ dummy, "\\" ~~ LetterCharacter.., 1];
  StringReplace[katexStr, "\\" ~~ w:LetterCharacter.. :>
    If[StringEndsQ[w, "Symbol"],
      StringJoin[styleOp, "\\", w],
      StringJoin["\\styled", UpperCaseFirst @ w, "{", styleOp, "}"]
    ],
    1
  ]
]

(**************************************************************************************************)

PackageExport["SubstackForm"]

declareBoxFormatting[
  SubstackForm[list_List] :> TemplateBox[
    List @ GridBox[MapUnevaluated[List @ makeQGBoxesOrComma @ #&, list], RowSpacings -> 0],
    "SubstackForm"
  ]
]

SetHoldAllComplete[makeQGBoxesOrComma]
makeQGBoxesOrComma = Case[
  {elems__} := MakeBoxes @ SpacedCommaRowForm @ elems;
  other_    := makeQGBoxes @ other
];

$TemplateKatexFunction["SubstackForm"] = substackKatex

substackKatex[GridBox[entries_, ___]] :=
  "substack" @ Riffle[Catenate @ entries, "\\\\"];

(**************************************************************************************************)

PackageExport["IndexedMaxForm"]
PackageExport["IndexedMinForm"]

declareSumLikeFormatting[IndexedMaxForm, "indexMax"];
declareSumLikeFormatting[IndexedMinForm, "indexMin"];

(**************************************************************************************************)

PackageExport["IndexedUnionForm"]
PackageExport["IndexedIntersectionForm"]

declareSumLikeFormatting[IndexedUnionForm, "indexUnion"];
declareSumLikeFormatting[IndexedIntersectionForm, "indexIntersection"];

(**************************************************************************************************)

PackageExport["IndexedGraphUnionForm"]
PackageExport["IndexedGraphDisjointUnionForm"]

declareSumLikeFormatting[IndexedGraphUnionForm, "indexGraphUnion"];
declareSumLikeFormatting[IndexedGraphDisjointUnionForm, "indexGraphDisjointUnion"];

(**************************************************************************************************)

PackageExport["SumForm"]
PackageExport["ProductForm"]

declareSumLikeFormatting[SumForm, "indexSum"];
declareSumLikeFormatting[ProductForm, "indexProd"];

(**************************************************************************************************)

PackageExport["PlusForm"]

declareInfixSymbol[PlusForm] // usingCustomKatex[" + "];

(**************************************************************************************************)

PackageExport["SubtractForm"]

 declareInfixSymbol[SubtractForm] // usingCustomKatex[" - "];

(**************************************************************************************************)

PackageExport["MinusForm"]

declareUnaryForm[MinusForm];

(**************************************************************************************************)

PackageExport["TimesForm"]

declareInfixSymbol[TimesForm] // usingCustomKatex["times"];

(**************************************************************************************************)

PackageExport["CartesianProductForm"]

declareInfixSymbol[CartesianProductForm];

(**************************************************************************************************)

PackageExport["SetUnionForm"]
PackageExport["SetIntersectionForm"]
PackageExport["SetRelativeComplementForm"]

declareInfixSymbol[{SetUnionForm, SetIntersectionForm, SetRelativeComplementForm}];

(**************************************************************************************************)

PackageExport["MultisetUnionForm"]
PackageExport["MultisetIntersectionForm"]
PackageExport["MultisetRelativeComplementForm"]
PackageExport["MultisetSumForm"]

declareInfixSymbol[{MultisetUnionForm, MultisetIntersectionForm, MultisetRelativeComplementForm, MultisetSumForm}];

(**************************************************************************************************)

PackageExport["PowerSetForm"]

declareUnaryForm[PowerSetForm];

(**************************************************************************************************)

PackageExport["SetSymbolForm"]
PackageExport["SignedSetSymbolForm"]
PackageExport["SetElementSymbolForm"]
PackageExport["SignedSetElementSymbolForm"]
PackageExport["MultisetSymbolForm"]
PackageExport["MultisetElementSymbolForm"]
PackageExport["SignedMultisetSymbolForm"]
PackageExport["SignedMultisetElementSymbolForm"]

declareSymbolFormExplicit[SetSymbolForm];
declareSymbolFormExplicit[MultisetSymbolForm];
declareSymbolFormExplicit[SignedSetSymbolForm];
declareSymbolFormExplicit[SignedMultisetSymbolForm];
declareSymbolFormExplicit[SetElementSymbolForm];
declareSymbolFormExplicit[SignedSetElementSymbolForm];
declareSymbolFormExplicit[MultisetElementSymbolForm];
declareSymbolFormExplicit[SignedMultisetElementSymbolForm];

(**************************************************************************************************)

PackageExport["SubsetsForm"]
PackageExport["MultisetsForm"]

PackageExport["SignedSubsetsForm"]
PackageExport["SignedMultisetsForm"]

declareUnaryForm[SubsetsForm];
declareUnaryForm[MultisetsForm];

declareUnaryForm[SignedSubsetsForm];
declareUnaryForm[SignedMultisetsForm];

(**************************************************************************************************)

PackageExport["MultisetMultiplicityForm"]
PackageExport["SignedMultisetMultiplicityForm"]

declareInfixSymbol[MultisetMultiplicityForm];
declareInfixSymbol[SignedMultisetMultiplicityForm];

(**************************************************************************************************)

PackageExport["BoundMultiplicityFunctionForm"]
PackageExport["BoundSignedMultiplicityFunctionForm"]

declareUnaryForm[BoundMultiplicityFunctionForm]
declareUnaryForm[BoundSignedMultiplicityFunctionForm]

(**************************************************************************************************)

PackageExport["DivideForm"]
PackageExport["InlineDivideForm"]

declareBinaryForm[DivideForm] // usingCustomKatex["frac"];
declareInfixSymbol[InlineDivideForm] // usingCustomKatex[" / "];

(**************************************************************************************************)

PackageExport["FactorialForm"]
PackageExport["PowerForm"]
PackageExport["RepeatedPowerForm"]

declareUnaryForm[FactorialForm];
declareBinaryForm[PowerForm];
declareBinaryForm[RepeatedPowerForm];

(**************************************************************************************************)

PackageExport["PathRelationForm"]

declareBoxFormatting[
  PathRelationForm[args__] :>
    TemplateBox[MapUnevaluated[wordBoxes, {args}], "PathRelationForm"],
  PathRelationForm[] :>
    SBox["PathRelationSymbol"]
]

$TemplateKatexFunction["PathRelationForm"] = katexAliasRiffled["pathIso"]
$TemplateKatexFunction["PathRelationSymbol"] = katexAlias["pathIso"];

(**************************************************************************************************)

PackageExport["TailEqualForm"]
PackageExport["HeadEqualForm"]
PackageExport["ApproxEqualForm"]
PackageExport["IsomorphicForm"]
PackageExport["HomeomorphicForm"]
PackageExport["CongruentForm"]
PackageExport["IdenticallyEqualForm"]
PackageExport["HomotopicForm"]
PackageExport["DefEqualForm"]
PackageExport["SyntaxEqualForm"]
PackageExport["DotEqualForm"]
PackageExport["ColonEqualForm"]

declareInfixSymbol[{ApproxEqualForm, IsomorphicForm, HomeomorphicForm, HomotopicForm, DefEqualForm, SyntaxEqualForm,
  DotEqualForm, ColonEqualForm, TailEqualForm, HeadEqualForm, CongruentForm, IdenticallyEqualForm}];

(**************************************************************************************************)

PackageExport["EqualForm"]
PackageExport["NotEqualForm"]

declareInfixSymbol[EqualForm] // usingCustomKatex[" = "];
declareInfixSymbol[NotEqualForm] // usingCustomKatex[" \\neq "];

(**************************************************************************************************)

PackageExport["SetToMultisetForm"]
PackageExport["MultisetToSetForm"]

declareUnaryForm[SetToMultisetForm]
declareUnaryForm[MultisetToSetForm]

(**************************************************************************************************)


PackageExport["SetConstructorForm"]
PackageExport["MultisetConstructorForm"]
PackageExport["CardinalityConstructorForm"]

declareBoxFormatting[
  SetConstructorForm[lhs_, rhs_] :>
    TemplateBox[{toColGrid @ lhs, toColGrid @ rhs}, "SetConstructorForm"],

  MultisetConstructorForm[lhs_, rhs_] :>
    TemplateBox[{toColGrid @ lhs, toColGrid @ rhs}, "MultisetConstructorForm"],

  CardinalityConstructorForm[lhs_, rhs_] :>
    TemplateBox[{toColGrid @ lhs, toColGrid @ rhs}, "CardinalityConstructorForm"]
];

SetHoldAllComplete[toColGrid]

toColGrid = Case[
  {a_}   := makeQGBoxesOrNull @ a;
  {a__}  := MakeBoxes @ CommaRowForm[a];
  s_SingleColumnForm := makeQGBoxes @ s;
  a_     := makeQGBoxesOrNull @ a
];

$TemplateKatexFunction["SetConstructorForm"] = "setConstructor"
$TemplateKatexFunction["MultisetConstructorForm"] = "multisetConstructor"
$TemplateKatexFunction["CardinalityConstructorForm"] = "cardinalityConstructor"

(**************************************************************************************************)

PackageExport["ConstructorForm"]

declareBoxFormatting[
  ConstructorForm[lhs_, rhs_] :>
    TemplateBox[{toColGrid @ lhs, toColGrid @ rhs}, "ConstructorForm"]
];

$TemplateKatexFunction["ConstructorForm"] = "constructor"

(**************************************************************************************************)

PackageExport["ItemForm"]
PackageExport["GridSpacings"]
PackageExport["SpillLength"]

(**************************************************************************************************)

PackageExport["MultiGridForm"]

Options[MultiGridForm] = {
  Alignment -> Automatic,
  ItemForm -> None,
  ColumnSpacings -> 1,
  RowSpacings -> Automatic,
  GridSpacings -> 2
};

declareBoxFormatting[
  MultiGridForm[grids__List, opts___Rule] :> makeMultiGridBoxes[{grids}, opts]
];

(**************************************************************************************************)

PackageExport["GridForm"]

OptionExport["RowLabels"]
OptionExport["ColumnLabels"]

Options[GridForm] = {
  Alignment -> None,
  ItemForm -> None,
  ColumnSpacings -> 1,
  RowSpacings -> Automatic,
  SpillLength -> Infinity,
  GridSpacings -> 2,
  RowLabels -> None,
  ColumnLabels -> None,
  LabelSpacing -> {2, 1},
  LabelFunction -> Bold
};

declareBoxFormatting[
  GridForm[rows_List, opts___Rule] :> makeSingleGridBoxes[rows, opts]
];

(**************************************************************************************************)

PackageExport["GridRowsForm"]

Options[GridRowsForm] = Options[GridForm];

declareBoxFormatting[
  GridRowsForm[rows__, opts___Rule] :> makeSingleGridBoxes[{rows}, opts]
];

(**************************************************************************************************)

PackageExport["GridColumnsForm"]

Options[GridColumnsForm] = Options[multiColumnBoxes] = {
  ItemForm -> None,
  ColumnSpacings -> 3,
  RowSpacings -> Automatic,
  Alignment -> Center
}

declareBoxFormatting[
  GridColumnsForm[cols__, opts___Rule] :> multiColumnBoxes[{cols}, opts]
];

multiColumnBoxes[cols_List, opts:OptionsPattern[]] := Scope[
  UnpackOptions[rowSpacings, columnSpacings, itemForm, alignment];
  grids = Flatten[{#}]& /@ cols;
  makeMultiGridBoxes[
    grids,
    RowSpacings -> rowSpacings,
    GridSpacings -> columnSpacings,
    ItemForm -> itemForm,
    Alignment -> alignment
  ]
]

(**************************************************************************************************)

PackageExport["SingleColumnForm"]

declareBoxFormatting[

  SingleColumnForm[args__, opts___Rule] :> singleColumnBoxes[{args}, opts]

]

Options[SingleColumnForm] = Options[singleColumnBoxes] = {
  ItemForm -> None,
  ColumnSpacings -> 3,
  RowSpacings -> Automatic,
  SpillLength -> Infinity,
  Alignment -> Center
}

singleColumnBoxes[items_List, opts:OptionsPattern[]] := Scope[
  UnpackOptions[rowSpacings, columnSpacings, itemForm, spillLength, alignment];
  items = List /@ Flatten @ items;
  makeSingleGridBoxes[
    items,
    RowSpacings -> rowSpacings,
    GridSpacings -> columnSpacings,
    ItemForm -> itemForm,
    Alignment -> alignment,
    SpillLength -> spillLength
  ]
]

(**************************************************************************************************)

Options[makeSingleGridBoxes] = Options[GridForm];

makeSingleGridBoxes[grid_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[itemForm, alignment, rowSpacings, columnSpacings, spillLength, rowLabels, columnLabels, labelFunction, labelSpacing];
  If[Length[Unevaluated @ grid] > spillLength,
    items = Partition[grid, UpTo @ spillLength];
    Return @ makeMultiGridBoxes[items, opts];
  ];
  $itemForm = itemForm;
  TBox["GridForm"] @ createGridBox[MapUnevaluated[makeGridRowBoxes, grid], alignment, rowSpacings, columnSpacings, rowLabels, columnLabels, labelFunction, labelSpacing]
];

(**************************************************************************************************)

Options[makeMultiGridBoxes] = JoinOptions[
  MultiGridForm,
  SpillLength -> Infinity (* to prevent messages, doesn't do anything *)
];

makeMultiGridBoxes[gridList_, OptionsPattern[]] := Scope[
  
  UnpackOptions[alignment, columnSpacings, rowSpacings, gridSpacings, itemForm, rowLabels, columnLabels];

  $itemForm = itemForm;
  {gridListBoxes, gridListAlignments, gridListRowSpacings, gridListColSpacings} =
    Transpose @ MapUnevaluated[
      createGridBoxData[MapUnevaluated[makeGridRowBoxes, #], alignment, rowSpacings, columnSpacings]&,
      gridList
    ];
  
  gridListHeights = Length /@ gridListBoxes;
  maxHeight = Max[gridListHeights];
  
  gridListBoxes //= Map[PadColumns[#, maxHeight, ""]&];
  gridListBoxes = ArrayFlatten @ List @ gridListBoxes;

  rowSpacings = StandardizeRowColumnSpec[rowSpacings, maxHeight - 1];

  TBox["GridForm"] @ GBox[gridListBoxes, Flatten @ gridListAlignments, rowSpacings, Flatten @ Riffle[gridListColSpacings, gridSpacings]]
];

(**************************************************************************************************)

createGridBox[args___] := GBox @@ createGridBoxData[args];

(**************************************************************************************************)

createGridBoxData[rows_, alignments_, rowSpacings_, colSpacings_, rowLabels_:None, colLabels_:None, labelFn_:Identity, labelSpacing_:{1, 1}] := Scope[

  entries = padArray @ rows;
  {numRows, numCols} = Dimensions[entries, 2];
  
  SetAutomatic[alignments, autoAlignmentSpec @ numCols];
  alignments = StandardizeRowColumnSpec[alignments, numCols];

  colSpacings = StandardizeRowColumnSpec[colSpacings, numCols - 1];
  rowSpacings = StandardizeRowColumnSpec[rowSpacings, numRows - 1];

  hasColLabels = MatchQ[colLabels, _List | Placed[_List, _]];

  rowLabelSpacing = First[labelSpacing, labelSpacing];
  colLabelSpacing = Last[labelSpacing, labelSpacing];
  If[hasColLabels,
    If[!ListQ[rowSpacings], rowSpacings = Ones[numRows - 1] / 2];
    isBelow = MatchQ[colLabels, Placed[_, Below]];
    rowSpacings = Insert[rowSpacings, colLabelSpacing, If[isBelow, -1, 1]];
    colLabelBoxes = labelBoxes[labelFn, Replace[colLabels, Placed[l_, _] :> l]];
    If[isBelow, AppendTo, PrependTo][entries, PadRight[colLabelBoxes, numCols, ""]];
  ];

  If[ListQ[rowLabels],
    If[!ListQ[colSpacings], colSpacings = Ones[numCols - 1]];
    colSpacings = Insert[colSpacings, rowLabelSpacing, 1];
    rowLabelBoxes = labelBoxes[labelFn, rowLabels];
    rowLabelBoxes = If[hasColLabels,
      PadRight[If[isBelow, Append, Prepend][rowLabelBoxes, ""], numRows + 1, ""],
      PadRight[rowLabelBoxes, numRows, ""]
    ];
    entries = PrependColumn[entries, rowLabelBoxes];
  ];

  {entries, alignments, rowSpacings, colSpacings}
];

autoAlignmentSpec = Case[
  1 := {Center};
  2 := {Right, Left};
  _ := {Right, {Center}, Left};
]

labelBoxes[None, labels_] := labelBoxes[Identity, labels];
labelBoxes[s:Bold|Italic, labels_] := labelBoxes[Style[#, s]&, labels];
labelBoxes[labelFn_, labels_] := ToBoxes[labelFn[#]]& /@ labels;

PackageScope["$infixFormCharacters"]

$infixFormCharacters = Association[
  EqualForm             -> "=",                 NotEqualForm            -> "\[NotEqual]",
  DefEqualForm          -> "≝",
  ColonEqualForm        -> "≔",
  DotEqualForm          -> "≐",
  IdenticallyEqualForm  -> "\[Congruent]",

  ElementOfForm         -> "\[Element]",
  Subset                -> "\[Subset]",         SubsetEqual             -> "\[SubsetEqual]",
  Superset              -> "\[Superset]",       SupersetEqual           -> "\[SupersetEqual]",
  CartesianProductForm  -> "\[Times]",

  Less                  -> "<",                 Greater                 -> ">",
  LessEqual             -> "\[LessEqual]",      GreaterEqual            -> "\[GreaterEqual]",

  AndForm               -> "\[And]",            OrForm                  -> "\[Or]",
  
  ImpliesForm           -> "\[Implies]",
  EquivalentForm        -> "\[Equivalent]",
  IsomorphicForm        -> "\[TildeEqual]",
  HomeomorphicForm      -> "\[TildeFullEqual]",
  BijectiveForm         -> "\[TildeTilde]",

  PlusForm              -> "+",                 TimesForm               -> "\[Times]",

  GraphUnionForm        -> "\[SquareUnion]",
  SetUnionForm          -> "\[Union]",          SetIntersectionForm     -> "\[Intersection]",
  SetRelativeComplementForm -> "\[Backslash]",

  RewriteForm           -> "\[Rule]"
];
  
makeGridRowBoxes = Case[
  
  item_List            := MapUnevaluated[makeGridEntryBox, item];

  a_ -> b_             := % @ {a, b};

  e:((head_Symbol)[___]) /; KeyExistsQ[$infixFormCharacters, head] := riffledGridRowBox[$infixFormCharacters @ head, e];

  e_SyntaxEqualForm    := riffledGridRowBox["≑", e];

  e_                   := List @ makeGridEntryBox @ e;

];

riffledGridRowBox[div_, _[]] := {div};

riffledGridRowBox[div_, _[args__]] :=
  Riffle[MapUnevaluated[makeGridEntryBox, {args}], div];

makeGridEntryBox = Case[
  None | Null                       := "";
  Form[item_] /; $itemForm =!= None := makeQGBoxes @ item;
  item_ /; $itemForm =!= None       := With[{if = $itemForm}, makeQGBoxes @ if @ item];
  item_                             := makeQGBoxes @ item
];

(**************************************************************************************************)

$TemplateKatexFunction["GridForm"] = katexGrid;

katexGrid[g_GridBox] :=
  katexGridBoxDispatch @@ getGridData[g];

katexGridBoxDispatch[entries_, alignments_, rowSpacings_, {0..}] :=
  {"\\begin{nsarray}{",
    toKatexAlignmentSpec[alignments, Length @ First @ entries], "}\n",
    createKatexGridBody[entries, rowSpacings],
    "\n\\end{nsarray}\n"
  };

katexGridBoxDispatch[entries_, alignments_, rowSpacings_, Automatic | {0, (1 | Automatic)..}] :=
  {"\\begin{array}{",
    toKatexAlignmentSpec[alignments, Length @ First @ entries], "}\n",
    createKatexGridBody[entries, rowSpacings],
    "\n\\end{array}\n"
  };

katexGridBoxDispatch[entries_, alignments_, rowSpacings_, colSpacings_] :=
  {"\\begin{csarray}{",
    toKatexAlignmentSpec[alignments, Length @ First @ entries], "}{",
    toKatexCSArraySpacingSpec[colSpacings],
    "}\n",
    createKatexGridBody[entries, rowSpacings],
    "\n\\end{csarray}\n"
  };

createKatexGridBody[entries_, rowSpacings_] :=
  assembleKatexGridRows[
    Riffle[#, " & "]& /@ entries,
    rowSpacings
  ];

assembleKatexGridRows[lines_, Automatic | {Automatic..}] :=
  Riffle[lines, "\\\\\n"];

assembleKatexGridRows[lines_, rowSpacings_] :=
  Riffle[lines,
    Map[
      If[# === Automatic, "\\\\\n", "\\\\[" <> TextString[#] <> "em]\n"]&,
      Rest @ rowSpacings
    ]
  ];

$spacingAlphabet = Characters["abcdefghijklmnopqrstuvwxyz"];

toKatexCSArraySpacingSpec[spacings_] :=
  StringJoin @ Part[$spacingAlphabet, Round[spacings * 4] + 1]

toKatexAlignmentSpec[Automatic, n_] :=
  toKatexAlignmentSpec[Table[Center, n], n];

toKatexAlignmentSpec[aligns_List, n_] :=
  StringJoin[
    toAlignmentLetter /@ If[Length[aligns] >= n, aligns, PadRight[aligns, n, aligns]]
  ];

toAlignmentLetter = Case[
  Left    := "l";
  Center  := "c";
  Right   := "r";
  None    := "c";
];

(**************************************************************************************************)

PackageExport["Aligner"]

declareBoxFormatting[
  Aligner :> SBox["Aligner"]
];

$TemplateKatexFunction["Aligner"] = "&"&;

(**************************************************************************************************)

PackageExport["FlowAlignedGridForm"]
PackageExport["AlignmentSet"]

Options[FlowAlignedGridForm] = {
  AlignmentSet -> {EqualForm, NotEqualForm},
  ColumnSpacings -> 1/2,
  RowSpacings -> Automatic,
  Alignment -> Center
}

declareBoxFormatting[
  FlowAlignedGridForm[args__, opts___Rule] :> makeFlowAlignedGridForm[{args}, opts]
]

SetHoldAllComplete[makeFlowAlignedGridForm, flowAlignedRow]

makeFlowAlignedGridForm[rawRows_List, OptionsPattern[FlowAlignedGridForm]] := Scope[
  UnpackOptions[alignmentSet, columnSpacings, rowSpacings, alignment];
  rows = MapUnevaluated[flowAlignedRow, rawRows];
  rows = Flatten[{#}]& /@ rows;
  patt = Alternatives @@ Flatten[toFlowAlignmentSplits /@ alignmentSet];
  JoinTo[patt, "" | TemplateBox[_, "Spacer1"]];
  rows = Map[toRowBox, SequenceSplit[#, e:{patt} :> e]]& /@ rows;
  rows = PadRows[rows, ""];
  numColumns = Length @ First @ rows;
  TBox["GridForm"] @ createGridBox[rows, alignment, rowSpacings, columnSpacings]
]

toFlowAlignmentSplits[s_] := Scope[
  If[KeyExistsQ[$flowAlignmentTable, s],
    Return @ $flowAlignmentTable @ s];
  {left, boxFn, right} = Lookup[$flowAlignmentContainerTable, s,
    Print["BAD ALIGNSET: ", alignmentSet]; Return @ Nothing];
  res = {left, ", ", right};
  If[s === AssociativeArrayForm, JoinTo[res, {"", SBox["MapsToSymbol"]}]];
  res
]

$flowAlignmentTable = <|
  FunctionSignatureForm -> ":"|"\[RightArrow]",
  InfixStateJoinForm -> "\[SquareUnion]",
  InfixStateMeetForm -> "\[SquareIntersection]",
  EqualForm -> "=", NotEqualForm -> "\[NotEqual]",
  DefEqualForm -> "≝", ColonEqualForm -> "≔", DotEqualForm -> "≐", SyntaxEqualForm -> "≑",
  Subset -> "\[Subset]", SubsetEqual -> "\[SubsetEqual]", Superset -> "\[Superset]", SupersetEqual -> "\[SupersetEqual]",
  ElementOfForm -> "\[Element]",
  AndForm -> "\[And]", OrForm -> "\[Or]", ImpliesForm -> "\[Implies]", EquivalentForm -> "\[Equivalent]",
  IsomorphicForm -> "\[TildeEqual]", HomeomorphicForm -> "\[TildeFullEqual]",
  "" -> "",
  SetUnionForm -> "\[Union]", SetIntersectionForm -> "\[Intersection]", SetRelativeComplementForm -> "\[Backslash]"
|>;


$flowAlignmentContainerTable = <|
  SetForm -> {SBox["LeftBrace"], makeQGBoxesList, SBox["RightBrace"]},
  ListForm -> {SBox["LeftBrace"], makeQGBoxesList, SBox["RightBrace"]},
  TupleForm -> {"(", makeQGBoxesList, ")"},
  MultisetForm -> {SBox["LeftMultisetBracket"], makeQGBoxesList, SBox["RightMultisetBracket"]},
  AssociativeArrayForm -> {"\[LeftAngleBracket]", makeRuleBoxes, "\[RightAngleBracket]"}
|>;

makeQGBoxesList = makeQGBoxes /* List;
makeRuleBoxes[a_ -> b_] := {makeQGBoxes @ a, " ", SBox["MapsToSymbol"], " ", makeQGBoxes @ b};

flowAlignedRow = Case[

  e_List := MapUnevaluated[%, e];

  FunctionSignatureForm[f_, d_, c_] /; KeyExistsQ[$flowAlignmentTable, FunctionSignatureForm] :=
    {makeQGBoxes @ f, ":", makeQGBoxes @ d, "\[RightArrow]", makeQGBoxes @ c};

  AppliedForm[f_, h_Symbol[a_, b_]] /; KeyExistsQ[$flowAlignmentTable, h] :=
    {RowBox[makeQGBoxes @ f, "(", makeQGBoxes @ a], $flowAlignmentTable @ h, RowBox[makeQGBoxes @ b, ")"]};

  Form[e_] := makeQGBoxes @ e;

  e:((h_Symbol)[_, __]) /; KeyExistsQ[$flowAlignmentTable, h] :=
    riffledFlowAlignedGridRow[$flowAlignmentTable @ h, e];

  (h_Symbol)[args___] /; KeyExistsQ[$flowAlignmentContainerTable, h] :=
    makeContainerFlow[h, args];

  e_ := makeQGBoxes @ e;

];

makeContainerFlow[h_, args___] := Scope[
  {pre, boxFn, post} = $flowAlignmentContainerTable @ h;
  Join[{pre}, MapMost[attachComma, Map[boxFn, {args}]], {post}]
]

attachComma[{a_, " ", s:SBox["MapsToSymbol"], " ", b_}] := {a, " ", s, " ", addComma[b], ""}
attachComma[e_] := {e, ", "};

riffledFlowAlignedGridRow[div_, _[args__]] :=
  Riffle[MapUnevaluated[flowAlignedRow, {args}], div];

toRowBox = Case[
  {e_} := e;
  e_List := RowBox[Riffle[e, " "]];
];

(**************************************************************************************************)

PackageExport["EquationCascadeForm"]

SetHoldAllComplete[equationCascadeFormBoxes]
declareBoxFormatting[
  EquationCascadeForm[first_, second_, rest__] :>
    equationCascadeFormBoxes[first, second, rest]
]

equationCascadeFormBoxes[first_, second_, rest__] :=
  TBox["EquationGridForm"] @ createGridBox[
    Prepend[equationGridRow[EqualForm[first, second]]] @
          MapUnevaluated[equationGridRow[{None, "=", #}]&, {rest}],
    {Right, Center, Left}, Automatic, Automatic
  ];

(**************************************************************************************************)

PackageExport["EquationGridForm"]
PackageExport["Aligned"]
PackageExport["Divider"]

SetUsage @ "
Divider is used in EquationGridForm.
"

SetHoldAllComplete[makeQGBoxesOrNull, equationGridRow, riffledEqGridRow];
SetHoldFirst[equationGridFormBoxes];

Options[EquationGridForm] = {
  RowSpacings -> Automatic,
  ColumnSpacings -> Automatic,
  Alignment -> {Right, Center, Left}
}

declareBoxFormatting[
  EquationGridForm[args__, opts:OptionsPattern[]] :>
    equationGridFormBoxes[{args}, OptionValue[EquationGridForm, {opts}, {Alignment, RowSpacings, ColumnSpacings}]]
];

equationGridFormBoxes[rows_, {alignment_, rowSpacings_, colSpacings_}] :=
  TBox["EquationGridForm"] @ createGridBox[
    MapUnevaluated[equationGridRow, rows],
    alignment, rowSpacings, colSpacings
  ];

equationGridRow = Case[
  Divider          := {"---"};
  e_List           := MapUnevaluated[makeQGBoxesOrNull, e];
  Aligned[e_]      := % @ List @ Aligned @ e;
  e_EqualForm      := riffledEqGridRow["=", e];
  e_DefEqualForm   := riffledEqGridRow["≝", e];
  e_DotEqualForm := riffledEqGridRow["≐", e];
  e_ColonEqualForm := riffledEqGridRow["≔", e];
  e_SyntaxEqualForm := riffledEqGridRow["≑", e];
  e_IdenticallyEqualForm := riffledEqGridRow["\[Congruent]", e];
  e_Less           := riffledEqGridRow["<", e];
  e_Greater        := riffledEqGridRow[">", e];
  e_LessEqual      := riffledEqGridRow["\[LessEqual]", e];
  e_GreaterEqual   := riffledEqGridRow["\[GreaterEqual]", e];
  e_Subset         := riffledEqGridRow["\[Subset]", e];
  e_SubsetEqual    := riffledEqGridRow["\[SubsetEqual]", e];
  e_Superset       := riffledEqGridRow["\[Superset]", e];
  e_SupersetEqual  := riffledEqGridRow["\[SupersetEqual]", e];
  e_ElementOfForm  := riffledEqGridRow["\[Element]", e];
  e_AndForm        := riffledEqGridRow["\[And]", e];
  e_OrForm         := riffledEqGridRow["\[Or]", e];
  e_ImpliesForm    := riffledEqGridRow["\[Implies]", e];
  e_EquivalentForm := riffledEqGridRow["\[Equivalent]", e];
  e_IsomorphicForm := riffledEqGridRow["\[TildeEqual]", e];
  e_HomeomorphicForm := riffledEqGridRow["\[TildeFullEqual]", e];
  e_BijectiveForm  := riffledEqGridRow["\[TildeTilde]", e];
  e_RewriteForm    := riffledEqGridRow["\[Rule]", e];
];

riffledEqGridRow[div_, _[args__]] :=
  Riffle[MapUnevaluated[makeQGBoxesOrNull, {args}], div];

makeQGBoxesOrNull[Aligned[e_]] := Splice @ MapUnevaluated[makeQGBoxes, {Aligner, e, Aligner}];
makeQGBoxesOrNull[Null|None] := "";
makeQGBoxesOrNull[other_] := makeQGBoxes[other]

padArray[rows_] := Scope[
  maxLen = Max[Length /@ rows];
  PadRight[#, maxLen, ""]& /@ rows
];

$TemplateKatexFunction["EquationGridForm"] = katexEquationGrid;

$equationSymbolRules = {
  "="                                             -> "&= ",
  "+"                                             -> "&+ ",
  "\[Element]"                                    -> "&\\in ",
  "≔" | SBox["ColonEqualSymbol"]                  -> "&\\coloneqq ",
  "≐" | SBox["DotEqualSymbol"]                    -> "&\\doteq ",
  "≑" | SBox["SyntaxEqualSymbol"]                 -> "&\\syntaxEqualSymbol ",
  ":="                                            -> "&\\defEqualSymbol ", (* LEGACY *)
  "≝" | SBox["DefEqualSymbol"]                    -> "&\\defEqualSymbol ",
  "---"                                           -> "\\hline",
  "\[Subset]"                                     -> "&\\subset ",
  "\[SubsetEqual]"                                -> "&\\subseteq ",
  "\[Superset]"                                   -> "&\\supset ",
  "\[SupersetEqual]"                              -> "&\\supseteq ",
  "where"                                         -> "&\\text{where} ",
  "\[TildeEqual]" | SBox["IsomorphicSymbol"]      -> "&\\isomorphicSymbol",
  "\[TildeFullEqual]" | SBox["HomeomorphicSymbol"]  -> "&\\homeomorphicSymbol",
  "\[TildeTilde]"                                 -> "&\[TildeTilde] ",
  "=>"                                            -> "&\[Implies] ",
  "\[Implies]"                                    -> "&\[Implies] ",
  "\[And]"                                        -> "&\[And] ",
  "\[Or]"                                         -> "&\[Or] ",
  "\[Congruent]"                                  -> "&\[Congruent]",
  "\[Rule]"                                       -> "&\[Rule]",
  "<"                                             -> "&\\lt",
  ">"                                             -> "&\\gt",
  "<="                                            -> "&\\le",
  ">="                                            -> "&\\ge",
  " "                                             -> "\, ",
  "\t"                                            -> "&\\quad "
}

$equationSplitP = Alternatives @@ Values[$equationSymbolRules];

katexEquationGrid[g_GridBox] := Scope[
  {entries, alignments, rowSpacings, columnSpacings} = getGridData[g];
  lines = assembleKatexGridRows[entries, rowSpacings];
  lines = MatrixReplace[lines, $equationSymbolRules];
  lines = VectorReplace[lines, {"\\hline", h___} :> {"\\hline \\\\[-2ex]"}];
  {"\\begin{aligned}\n", lines, "\\end{aligned}\n"}
]

getGridData[GridBox[entries_, opts:OptionsPattern[GridBox]]] := Scope[
  {gridBoxAlignment, gridBoxSpacings} = OptionValue[GridBox, {opts}, {GridBoxAlignment, GridBoxSpacings}];
  Join[
    {entries, Lookup[gridBoxAlignment, "Columns", Automatic]},
    Lookup[gridBoxSpacings, {"Rows", "Columns"}, Automatic]
  ]
]

(**************************************************************************************************)

PackageExport["BinaryRelationForm"]

declareBoxFormatting[
  BinaryRelationForm[relation_String][args__] :>
    makeTypedTemplateBox[relation -> None, args, "BinaryRelationForm"]
]

$TemplateKatexFunction["BinaryRelationForm"] = Function[riffled[#1][##2]];

(**************************************************************************************************)

PackageExport["ImplicitTimesForm"]
PackageExport["SpacedImplicitTimesForm"]

declareInfixSymbol[ImplicitTimesForm] // usingCustomKatex[" \\, "];
declareInfixSymbol[SpacedImplicitTimesForm] // usingCustomKatex[" \\; "];

(**************************************************************************************************)

PackageExport["PolyForm"]

declareBoxFormatting[
  PolyForm[args__] :>
    generalPolyBoxes[
      PolyForm, "PolynomialForm", "PowerForm", "PlusForm", "ImplicitTimesForm", makeQGBoxes,
      args
    ]
];

$TemplateKatexFunction["PolynomialForm"] = "poly";

(**************************************************************************************************)

PackageExport["QuiverProductPolyForm"]

declareBoxFormatting[
  QuiverProductPolyForm[args__] :>
    generalPolyBoxes[
      QuiverProductPolyForm,
      "QuiverProductPolynomialForm", "QuiverProductPowerForm", "QuiverProductPlusForm", "QuiverProductTimesForm", graphOrQuiverBoxes,
      args
    ]
];

$TemplateKatexFunction["QuiverProductPowerForm"] = "quiverProdPower";
$TemplateKatexFunction["QuiverProductPolynomialForm"] = "quiverProdPoly";
$TemplateKatexFunction["QuiverProductPlusForm"] = riffled["+"];
$TemplateKatexFunction["QuiverProductTimesForm"] = riffled["\,"];

(**************************************************************************************************)

SetHoldAllComplete[generalPolyBoxes, polyTermForm, makeInnerPolyParamQGBoxes, innerPolyBoxes, longQ]

generalPolyBoxes[polyHead_, polyForm_, powerForm_, plusForm_, timesForm_, scalarForm_, args___] := Scope[
  $polyHead = polyHead;
  $polyPlusForm = plusForm;
  $polyPowerForm = powerForm;
  $polyTimesForm = timesForm;
  $scalarBoxes = scalarForm;
  TemplateBox[
    List @ innerPolyBoxes @ args,
    polyForm
  ]
];

innerPolyBoxes[a_, n_Integer ? Negative] :=
  TemplateBox[{polyTermForm @ a, IntegerString @ Abs @ n}, "SubtractForm"];

innerPolyBoxes[args___] :=
  TemplateBox[MapUnevaluated[polyTermForm, {args}], $polyPlusForm];
    
polyTermForm = Case[
  HoldForm[a_]                := % @ a;
  Style[e_, s_]               := StyleBox[% @ e, s];
  p_Plus | p_PlusForm         := Apply[innerPolyBoxes, Unevaluated @ p];
  a_Times | a_TimesForm       := Construct[%, Apply[List, Unevaluated @ a]];
  Power[a_, b_]               := TemplateBox[{% @ a, makeQGBoxes @ b}, $polyPowerForm];
  (Inverted|InvertedForm)[n_]   := TemplateBox[List @ % @ n, "InvertedForm"];
  a_ ? longPolyQ              := TemplateBox[List @ Apply[innerPolyBoxes, Unevaluated @ a], "SpacedParenthesesForm"];
  a_List                      := TemplateBox[MapUnevaluated[makeInnerPolyParamQGBoxes, a], $polyTimesForm];
  a_                          := $scalarBoxes @ a;
];

makeInnerPolyParamQGBoxes = Case[
  Style[e_, s_]               := StyleBox[% @ e, s];
  a_List | a_Times | a_TimesForm | ParenthesesForm[a_] := TemplateBox[{polyTermForm @ a}, "SpacedParenthesesForm"];
  Power[a_, b_]               := TemplateBox[{% @ a, makeQGBoxes @ b}, $polyPowerForm];
  a_ ? longPolyQ              := TemplateBox[List @ Apply[innerPolyBoxes, Unevaluated @ a], "SpacedParenthesesForm"];
  (Inverted|InvertedForm)[n_]   := TemplateBox[List @ % @ n, "InvertedForm"];
  a_                          := $scalarBoxes @ a;
];

longPolyQ[e_PolyForm] := Length[Unevaluated @ e] > 1;
longPolyQ[e_] := Head[Unevaluated @ e] === $polyHead && Length[Unevaluated @ e] > 1;

(**************************************************************************************************)

PackageExport["InverseForm"]

declareUnaryForm[InverseForm];

(**************************************************************************************************)

PackageExport["LookupStyleColor"]

LookupStyleColor[styleName_] :=
  FirstCase[CurrentValue[{StyleDefinitions, styleName}], rgb_RGBColor :> rgb, None, {0, Infinity}];

(**************************************************************************************************)

PackageExport["BoldForm"]
PackageExport["LightRedForm"]
PackageExport["LightGreenForm"]
PackageExport["LightBlueForm"]
PackageExport["LightRedGreenForm"]
PackageExport["LightGreenBlueForm"]
PackageExport["LightRedBlueForm"]
PackageExport["LightPurpleForm"]
PackageExport["RedForm"]
PackageExport["GreenForm"]
PackageExport["BlueForm"]
PackageExport["DarkRedForm"]
PackageExport["DarkGreenForm"]
PackageExport["DarkBlueForm"]
PackageExport["RedBlueForm"]
PackageExport["GreenBlueForm"]
PackageExport["RedGreenForm"]
PackageExport["DarkRedBlueForm"]
PackageExport["DarkGreenBlueForm"]
PackageExport["DarkRedGreenForm"]
PackageExport["PurpleForm"]
PackageExport["DarkPurpleForm"]
PackageExport["DarkGrayForm"]
PackageExport["MediumGrayForm"]
PackageExport["LightGrayForm"]

declareUnaryWrapperForm[BoldForm, "boldForm"]
declareUnaryWrapperForm[LightRedForm, "lrform"];
declareUnaryWrapperForm[LightGreenForm, "lgform"];
declareUnaryWrapperForm[LightBlueForm, "lbform"];
declareUnaryWrapperForm[LightRedGreenForm, "lrgform"];
declareUnaryWrapperForm[LightGreenBlueForm, "lgbform"];
declareUnaryWrapperForm[LightRedBlueForm, "lrbform"];
declareUnaryWrapperForm[LightPurpleForm, "lpform"];
declareUnaryWrapperForm[RedForm, "rform"];
declareUnaryWrapperForm[GreenForm, "gform"];
declareUnaryWrapperForm[BlueForm, "bform"];
declareUnaryWrapperForm[DarkRedForm, "drform"];
declareUnaryWrapperForm[DarkGreenForm, "dgform"];
declareUnaryWrapperForm[DarkBlueForm, "dbform"];
declareUnaryWrapperForm[DarkRedGreenForm, "drgform"];
declareUnaryWrapperForm[DarkGreenBlueForm, "dgbform"];
declareUnaryWrapperForm[DarkRedBlueForm, "drbform"];
declareUnaryWrapperForm[RedBlueForm, "rbform"];
declareUnaryWrapperForm[PurpleForm, "pform"];
declareUnaryWrapperForm[DarkPurpleForm, "dpform"];
declareUnaryWrapperForm[RedGreenForm, "rgform"];
declareUnaryWrapperForm[GreenBlueForm, "gbform"];
declareUnaryWrapperForm[DarkGrayForm, "waform"];
declareUnaryWrapperForm[MediumGrayForm, "wbform"];
declareUnaryWrapperForm[LightGrayForm, "wcform"];

(**************************************************************************************************)

PackageExport["TickSymbol"]

declareConstantSymbol[TickSymbol];

(**************************************************************************************************)

PackageExport["UnitInterval"]

declareConstantSymbol[UnitInterval];

(**************************************************************************************************)

PackageExport["BarTokenSymbol"]
PackageExport["FilledTokenSymbol"]
PackageExport["FilledSquareTokenSymbol"]
PackageExport["FilledRectangleTokenSymbol"]
PackageExport["EmptyTokenSymbol"]
PackageExport["EmptySquareTokenSymbol"]
PackageExport["EmptyRectangleTokenSymbol"]

declareConstantSymbol[
  {BarTokenSymbol,
   FilledTokenSymbol, FilledSquareTokenSymbol, FilledRectangleTokenSymbol,
   EmptyTokenSymbol, EmptySquareTokenSymbol, EmptyRectangleTokenSymbol
}];

(**************************************************************************************************)

PackageExport["InvertedBoxForm"]

SetHoldFirst[InvertedBoxForm];
InvertedBoxForm[e_] := makeTemplateBox[e, "InvertedForm"]

$TemplateKatexFunction["InvertedForm"] = "inverted";

(********************************************)

PackageExport["NegatedForm"]

declareUnaryWrapperForm[NegatedForm]

(********************************************)

PackageExport["UnitVertexFieldSymbol"]

declareConstantSymbol[UnitVertexFieldSymbol];

(********************************************)

PackageExport["WordVectorForm"]

WordVectorForm[p_String, args___] := WordVectorForm[ToPathWord @ p, args]

declareBoxFormatting[
  WordVectorForm[p_, dir_:"Forward"] :>
    TemplateBox[{wordBoxes @ p, directionBoxes @ dir}, "WordVectorForm"]
];

directionBoxes = Case[
  "Forward"         := SBox["ForwardSymbol"];
  "Backward"        := SBox["BackwardSymbol"];
  "Symmetric"       := SBox["SymmetricSymbol"];
  "Antisymmetric"   := SBox["AntisymmetricSymbol"];
];

$TemplateKatexFunction["WordVectorForm"] = "wordVector";
$TemplateKatexFunction["ForwardSymbol"] = katexAlias["forwardSymbol"];
$TemplateKatexFunction["BackwardSymbol"] = katexAlias["backwardSymbol"];
$TemplateKatexFunction["SymmetricSymbol"] = katexAlias["symmetricSymbol"];
$TemplateKatexFunction["AntisymmetricSymbol"] = katexAlias["antisymmetricSymbol"];

(********************************************)

$appliedCompositionP = (_FunctionCompositionForm);

makePathBoxTemplate[left_, rest___, tag_] :=
  TemplateBox[
    Join[
      List @ maybeParen[PathSymbol|RouteSymbol|ParenthesesForm|PathHeadForm|PathTailForm|PathReverseForm|CardinalSymbol|$colorFormP|$functionFormP|$appliedCompositionP|EdgeFieldSymbol|PathVectorSymbol] @ left,
      MapUnevaluated[
        maybeParen[PathSymbol|RouteSymbol|ParenthesesForm|PathHeadForm|PathTailForm|PathReverseForm|CardinalSymbol|EdgeFieldSymbol|VertexFieldSymbol|PathVectorSymbol|PathTranslateForm|PathBackwardTranslateForm|$colorFormP|$appliedCompositionP],
        {rest}
      ]
    ],
    tag
  ];

makePathBoxTemplate[left_, tag_] :=
  TemplateBox[
    List @ maybeParen[PathSymbol|PathReverseForm|ParenthesesForm|EdgeFieldSymbol|VertexFieldSymbol|PathVectorSymbol|PathTranslateForm|PathBackwardTranslateForm|$colorFormP|$appliedCompositionP] @ left,
    tag
  ];

(********************************************)

PackageExport["$PathComposeSymbol"]

$PathComposeSymbol = "\[Proportion]";

PackageExport["PathComposeForm"]

declareBoxFormatting[
  PathComposeForm[a_, b__] :>
    makePathBoxTemplate[a, b, "PathComposeForm"],

  PathComposeForm[] :>
    SBox["PathComposeSymbol"]
];

$TemplateKatexFunction["PathComposeForm"] = "pathCompose";
$TemplateKatexFunction["PathComposeSymbol"] = "pathComposeSymbol";

(********************************************)

PackageExport["PathIntegralForm"]

declareBoxFormatting[
  PathIntegralForm[a_, b_] :>
    makePathBoxTemplate[a, b, "PathIntegralForm"],

  PathIntegralForm[] :>
    SBox["PathIntegralSymbol"]
];

$TemplateKatexFunction["PathIntegralForm"] = "pathIntegral";
$TemplateKatexFunction["PathIntegralSymbol"] = "pathIntegralSymbol";

(********************************************)

PackageExport["PathDotForm"]

declareBoxFormatting[
  PathDotForm[a_, b_] :>
    makePathBoxTemplate[a, b, "PathDotForm"],

  PathDotForm[] :>
    SBox["PathDotSymbol"]
];

$TemplateKatexFunction["PathDotForm"] = "pathDot";
$TemplateKatexFunction["PathDotSymbol"] = "pathDotSymbol";

(********************************************)

PackageExport["PathTranslateForm"]
PackageExport["PathLeftTranslateForm"]

declareBoxFormatting[
  PathTranslateForm[] :>
    SBox["PathTranslateSymbol"],
  PathTranslateForm[a_, b__] :>
    makePathBoxTemplate[a, b, "PathTranslateForm"],
  PathTranslateForm[a_] :>
    makePathBoxTemplate[a, "PathAnonymousTranslateForm"],
  PathLeftTranslateForm[a_] :>
    makePathBoxTemplate[a, "PathLeftTranslateForm"]
];

$TemplateKatexFunction["PathTranslateForm"] = "pathTranslate";
$TemplateKatexFunction["PathAnonymousTranslateForm"] = "pathTranslate{}";
$TemplateKatexFunction["PathLeftTranslateForm"] = "pathLeftTranslate";
$TemplateKatexFunction["PathTranslateSymbol"] = katexAlias["translateSymbol"];

(********************************************)

PackageExport["PathBackwardTranslateForm"]
PackageExport["PathLeftBackwardTranslateForm"]

declareBoxFormatting[
  PathBackwardTranslateForm[] :>
    SBox["PathBackwardTranslateSymbol"],
  PathBackwardTranslateForm[a_, b__] :>
    makePathBoxTemplate[a, b, "PathBackwardTranslateForm"],
  PathBackwardTranslateForm[a_] :>
    makePathBoxTemplate[a, "PathAnonymousBackwardTranslateForm"],
  PathLeftBackwardTranslateForm[a_] :>
    makePathBoxTemplate[a, "PathLeftBackwardTranslateForm"]
];

$TemplateKatexFunction["PathBackwardTranslateForm"] = "pathBackwardTranslate";
$TemplateKatexFunction["PathAnonymousBackwardTranslateForm"] = "pathBackwardTranslate{}";
$TemplateKatexFunction["PathLeftBackwardTranslateForm"] = "pathLeftBackwardTranslate";
$TemplateKatexFunction["PathBackwardTranslateSymbol"] = katexAlias["backwardTranslateSymbol"];

(********************************************)

PackageExport["PathHeadVectorForm"]

declareBoxFormatting[
  PathHeadVectorForm[a_] :>
    makePathBoxTemplate[a, "PathHeadVectorForm"]
];

$TemplateKatexFunction["PathHeadVectorForm"] = "pathHeadVector";

(********************************************)

PackageExport["PathTailVectorForm"]

declareBoxFormatting[
  PathTailVectorForm[a_] :>
    makePathBoxTemplate[a, "PathTailVectorForm"]
];

$TemplateKatexFunction["PathTailVectorForm"] = "pathTailVector";

(********************************************)

PackageExport["PathTailVertexForm"]
PackageExport["PathHeadVertexForm"]
PackageExport["PathTailForm"]
PackageExport["PathHeadForm"]

declareUnaryForm[PathTailVertexForm, PathSymbol];
declareUnaryForm[PathHeadVertexForm, PathSymbol];
declareUnaryForm[PathTailForm, PathSymbol];
declareUnaryForm[PathHeadForm, PathSymbol];

(********************************************)

PackageExport["PathSplitForm"]

declareBoxFormatting[
  PathSplitForm[a_] :>
    MakeBoxes @ AppliedForm[SplitFunction, a]
];

(********************************************)

PackageExport["PathReverseForm"]

declareBoxFormatting[
  PathReverseForm[a_] :>
    makePathBoxTemplate[a, "PathReverseForm"],
  PathReverseForm[] :>
    SBox["PathReverseSymbol"]
];

$TemplateKatexFunction["PathReverseForm"] = "pathReverse";
$TemplateKatexFunction["PathReverseSymbol"] = "pathReverse";

(********************************************)

PackageExport["GradientForm"]

declareBoxFormatting[
  GradientForm[q_] :> makeHintedTemplateBox[q -> VertexFieldSymbol, "GradientForm"],
  GradientForm[] :> SBox["GradientSymbol"]
];

$TemplateKatexFunction["GradientForm"] = "gradOf";
$TemplateKatexFunction["GradientSymbol"] = katexAlias["grad"];

(********************************************)

PackageExport["DivergenceForm"]

declareBoxFormatting[
  DivergenceForm[q_] :> makeHintedTemplateBox[q -> EdgeFieldSymbol, "DivergenceForm"],
  DivergenceForm[] :> SBox["DivergenceSymbol"]
];

$TemplateKatexFunction["DivergenceForm"] = "divOf";
$TemplateKatexFunction["DivergenceSymbol"] = katexAlias["div"];

(********************************************)

PackageExport["LaplacianForm"]

declareBoxFormatting[
  LaplacianForm[q_] :> makeHintedTemplateBox[q -> EdgeFieldSymbol, "LaplacianForm"],
  LaplacianForm[] :> SBox["LaplacianSymbol"]
];

$TemplateKatexFunction["LaplacianForm"] = "laplacianOf";
$TemplateKatexFunction["LaplacianSymbol"] = katexAlias["laplacian"];

(********************************************)

PackageExport["SuchThatForm"]

declareBoxFormatting[
  SuchThatForm[a_, b_] :> makeTemplateBox[a, b, "SuchThatForm"]
];

$TemplateKatexFunction["SuchThatForm"] = "suchThat";

(********************************************)

PackageExport["CompactBasisSymbolForm"]

declareSymbolForm[CompactBasisSymbolForm, PathVectorSpaceSymbol];

(********************************************)

PackageExport["CompactPathCovariantDifferenceForm"]
PackageExport["PathCovariantDifferenceForm"]

declareBinaryForm[CompactPathCovariantDifferenceForm, PathVectorSymbol];
declareBinaryForm[PathCovariantDifferenceForm, PathVectorSymbol];
(*
declareBoxFormatting[
  CompactPathCovariantDifferenceForm[a_, b_] :>

    TemplateBox[MapUnevaluated[pathOrWordBoxes, {a, b}], "CompactPathCovariantDifferenceForm"],
  PathCovariantDifferenceForm[a_, b_] :>
    TemplateBox[MapUnevaluated[pathOrWordBoxes, {a, b}], "PathCovariantDifferenceForm"]
];

$TemplateKatexFunction["CompactPathCovariantDifferenceForm"] = "compactCovariantDifference";
$TemplateKatexFunction["PathCovariantDifferenceForm"] = "covariantDifference";
 *)
(********************************************)

PackageExport["PathForwardDifferenceForm"]

PathForwardDifferenceForm[w_String] :=
  PathForwardDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathForwardDifferenceForm[w__] :>
    TemplateBox[Map[pathOrWordBoxes, {w}], "PathForwardDifferenceForm"],
  PathForwardDifferenceForm[] :>
    SBox["PathForwardDifferenceSymbol"],
  (op_PathForwardDifferenceForm)[arg_] :>
    MakeBoxes @ OperatorAppliedForm[op, arg]
];

$TemplateKatexFunction["PathForwardDifferenceForm"] = applyRiffled["pathForwardDifference",","];
$TemplateKatexFunction["PathForwardDifferenceSymbol"] = "forwardDifference";

(********************************************)

PackageExport["PathBackwardDifferenceForm"]

PathBackwardDifferenceForm[w_String] :=
  PathBackwardDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathBackwardDifferenceForm[w__] :>
    TemplateBox[Map[pathOrWordBoxes, {w}], "PathBackwardDifferenceForm"],
  PathBackwardDifferenceForm[] :>
    SBox["PathBackwardDifferenceSymbol"],
  (op_PathBackwardDifferenceForm)[arg_] :>
    MakeBoxes @ OperatorAppliedForm[op, arg]
];

$TemplateKatexFunction["PathBackwardDifferenceForm"] = applyRiffled["pathBackwardDifference",","];
$TemplateKatexFunction["PathBackwardDifferenceSymbol"] = "backwardDifference";

(********************************************)

PackageExport["LieBracketForm"]

declareBinaryForm[LieBracketForm];

(********************************************)

PackageExport["PathCentralDifferenceForm"]

PathCentralDifferenceForm[w_String] :=
  PathCentralDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathCentralDifferenceForm[w__] :>
    TemplateBox[Map[pathOrWordBoxes, {w}], "PathCentralDifferenceForm"],
  PathCentralDifferenceForm[] :>
    SBox["PathCentralDifferenceSymbol"],
  (op_PathCentralDifferenceForm)[arg_] :>
    MakeBoxes @ OperatorAppliedForm[op, arg]
];

$TemplateKatexFunction["PathCentralDifferenceForm"] = applyRiffled["pathCentralDifference",","];
$TemplateKatexFunction["PathCentralDifferenceSymbol"] = "centralDifference";

(********************************************)

toPathWordOrSymbol = Case[
  s:("P" | "Q" | "R" | "T")           := PathSymbol @ s;
  c:"x" | "y" | "z" | "a" | "b" | "c" := CardinalSymbol @ c;
  w_                                  := ToPathWord[w];
];

pathOrWordBoxes = Case[
  s:("P" | "Q" | "R" | "T") := MakeBoxes @ PathSymbol @ s;
  s:symsP                   := MakeBoxes @ PathSymbol @ s;
  s_List                    := wordBoxes @ s;
  c:(colsP[_])              := MakeBoxes @ c;
  p:(_PathSymbol | _VertexFieldSymbol | _EdgeFieldSymbol | _WordForm | _CardinalSymbol) := MakeBoxes @ p,
  {symsP -> $rawSymbolP, colsP -> $colorFormP}
]

(**************************************************************************************************)

PackageExport["AssociativeArrayForm"]

declareBoxFormatting[
  AssociativeArrayForm[rules__] :>
    TemplateBox[
      MapUnevaluated[assocRuleBox, {rules}],
      "AssociativeArrayForm"
    ]
]

SetHoldAllComplete[assocRuleBox];
assocRuleBox = Case[
  a_ -> b_ := MakeBoxes @ MapsToForm[a, b];
  other_   := makeQGBoxes @ other;
];

$TemplateKatexFunction["AssociativeArrayForm"] = applyRiffled["assocArray", ","];

(**************************************************************************************************)

PackageExport["HomomorphismMappingForm"]

declareBoxFormatting[
  HomomorphismMappingForm[rules__] :>
    TemplateBox[
      MapUnevaluated[assocRuleBox, {rules}],
      "HomomorphismMappingForm"
    ]
]

SetHoldAllComplete[assocRuleBox];
assocRuleBox = Case[
  a_ -> b_ := MakeBoxes @ MapsToForm[a, b];
  other_   := makeQGBoxes @ other;
];

$TemplateKatexFunction["HomomorphismMappingForm"] = applyRiffled["homomorphismMapping", ","];

(********************************************)

PackageExport["SetForm"]
PackageExport["StyledSetForm"]

declareCommaRiffledForm[SetForm, "set"];
declareStyledCommaRiffledForm[StyledSetForm, "styledSet"];

(********************************************)

PackageExport["SignedSetForm"]

declareCommaRiffledForm[SignedSetForm, "signedSet"];

(********************************************)

PackageExport["ListForm"]
PackageExport["StyledListForm"]

declareCommaRiffledForm[ListForm, "list"];
declareStyledCommaRiffledForm[StyledListForm, "styledList"];

(********************************************)

PackageExport["MultisetForm"]
PackageExport["StyledMultisetForm"]
PackageExport["SignedMultisetForm"]
PackageExport["StyledSignedMultisetForm"]

declareCommaRiffledForm[MultisetForm, "multiset"];
declareCommaRiffledForm[SignedMultisetForm, "signedMultiset"];
declareStyledCommaRiffledForm[StyledMultisetForm, "styledMultiset"];
declareStyledCommaRiffledForm[StyledSignedMultisetForm, "styledSignedMultiset"];

(********************************************)

PackageExport["RepeatedMultisetForm"]

declareBinaryForm[RepeatedMultisetForm];

(**************************************************************************************************)

PackageExport["TranspositionForm"]

declareBinaryForm[TranspositionForm];

(**************************************************************************************************)

PackageExport["PermutationCycleForm"]

declareBoxFormatting[
  PermutationCycleForm[a_, b_] :> MakeBoxes @ TranspositionForm[a, b],

  PermutationCycleForm[args__] :> TemplateBox[
    With[{list = MapUnevaluated[makeQGBoxes, {args}]},
      Append[list, First @ list]
    ],
    "PermutationCycleForm"
  ]
];

$TemplateKatexFunction["PermutationCycleForm"] =
  applyRiffled["permutationCycle", "\\permutationCycleSymbol "];

(**************************************************************************************************)

PackageExport["StandardPermutationForm"]

declareBoxFormatting[
  StandardPermutationForm[Cycles[{}]] :>
    TemplateBox[
      List @ standardCycleBoxes @ {},
      "StandardPermutationForm"
    ],
  StandardPermutationForm[Cycles[list:{___List}]] :>
    TemplateBox[
      standardCycleBoxes /@ list,
      "StandardPermutationForm"
    ]
];

(**************************************************************************************************)

PackageExport["StandardPermutationCycleForm"]

declareBoxFormatting[
  StandardPermutationCycleForm[list_List] :>
    standardCycleBoxes @ list
];

standardCycleBoxes[list_] := TemplateBox[makeQGBoxes /@ list, "StandardPermutationCycleForm"]

(**************************************************************************************************)

PackageExport["PermutationForm"]

declareBoxFormatting[

  PermutationForm[Cycles[list:{___List}] | list:{___List}] :>
    TemplateBox[
      ToBoxes /@ (PermutationCycleForm @@@ list),
      "PermutationSetForm"
    ],

  PermutationForm[Cycles[{list_List}]] :>
    ToBoxes @ (PermutationCycleForm @@ list),

  PermutationForm[Cycles[{}]] :>
    MakeBoxes @ GroupElementSymbol @ "e"
];

$TemplateKatexFunction["PermutationSetForm"] =
  applyRiffled["permutationSet", "\\permutationSetSymbol "]

(********************************************)

PackageExport["TupleForm"]

declareCommaRiffledForm[TupleForm, "tuple"];

(********************************************)

PackageExport["PrimedForm"]

declareUnaryWrapperForm[PrimedForm];

(********************************************)

PackageExport["ParenthesesForm"]
PackageExport["SpacedParenthesesForm"]

declareCommaRiffledForm[ParenthesesForm, "paren"];
declareCommaRiffledForm[SpacedParenthesesForm, "paren"];

(********************************************)

PackageExport["NoParenthesesForm"]

declareBoxFormatting[
  NoParenthesesForm[e_] :> makeQGBoxes @ e
];

(********************************************)

PackageExport["CeilingForm"]
PackageExport["FloorForm"]

declareUnaryForm[CeilingForm]
declareUnaryForm[FloorForm]

(********************************************)

PackageExport["TranslationVectorForm"]

declareCommaRiffledForm[TranslationVectorForm, "translationVector"];

(********************************************)

PackageExport["SmallMatrixForm"]
PackageExport["NormalMatrixForm"]

declareBoxFormatting[
  SmallMatrixForm[array_] :>
    TemplateBox[MapUnevaluated[matrixRowBoxes, array], "SmallMatrixForm"],

  NormalMatrixForm[array_] :>
    TemplateBox[MapUnevaluated[matrixRowBoxes, array], "NormalMatrixForm"]
];

SetHoldAllComplete[matrixRowBoxes]

matrixRowBoxes[row_List] :=
  TemplateBox[MapUnevaluated[makeQGBoxes, row], "MatrixRowForm"];

$TemplateKatexFunction["SmallMatrixForm"] = smallMatrixKatex;
$TemplateKatexFunction["NormalMatrixForm"] = normalMatrixKatex;
$TemplateKatexFunction["MatrixRowForm"] = matrixRowKatex;

smallMatrixKatex[rows___] := {"{\\begin{smallmatrix}", Riffle[{rows}, "\\\\"], "\\end{smallmatrix}}"};
normalMatrixKatex[rows___] := {"{\\begin{pmatrix}", Riffle[{rows}, "\\\\"], "\\end{pmatrix}}"};

matrixRowKatex[cols___] := Riffle[{cols}, "&"];

(********************************************)

PackageExport["ConcatenationForm"]

declareNAryForm[ConcatenationForm];

$TemplateKatexFunction["ConcatenationForm"] = applyRiffled["concat", " "];

(********************************************)

PackageExport["SpacedConcatenationForm"]

declareNAryForm[SpacedConcatenationForm];

$TemplateKatexFunction["SpacedConcatenationForm"] = applyRiffled["concat", "\,"];

(********************************************)

(* unfortunate we have to do this *)
Unprotect[GeneralUtilities`CommaForm];
GeneralUtilities`CommaForm[arg:Except[_List], rest___] :=
  CommaRowForm[arg, rest];

PackageExport["SpacedCommaForm"]
PackageExport["SpacedForm"]

declareBoxFormatting[
(*   CommaForm[args___] :> CommaRowForm[args], *)
  SpacedCommaForm[args___] :> MakeBoxes @ SpacedCommaRowForm[args],
  SpacedForm[args___] :> MakeBoxes @ SpacedRowForm[args]
];

(* TODO: migrate away from the old ones, rename the form names *)

(********************************************)

PackageExport["CommaAndForm"]

declareBoxFormatting[
  CommaAndForm[a_, b_] :> MakeBoxes @ TextAndForm[a, b],
  CommaAndForm[most__, a_, b_] :> MakeBoxes @ CommaRowForm[most, TextAndForm[a, b]]
]

(********************************************)

PackageExport["TextAndForm"]

declareInfixSymbol[TextAndForm] // usingCustomKatex["textAnd"];

(********************************************)

PackageExport["CommaRowForm"]

declareNAryForm[CommaRowForm];

$TemplateKatexFunction["CommaRowForm"] = riffled[","];

(********************************************)

PackageExport["SpacedCommaRowForm"]

declareNAryForm[SpacedCommaRowForm];

$TemplateKatexFunction["SpacedCommaRowForm"] = riffled[",\;"];

(********************************************)

$TemplateKatexFunction["Spacer1"] = katexPtSpacer;

katexEmSpacer[0] := "";
katexEmSpacer[spacing_] := katexEmSpacer[spacing] = StringJoin["\\kern{", TextString @ spacing, "em}"];

katexPtSpacer[0] := "";
katexPtSpacer[spacing_] := katexPtSpacer[spacing] = StringJoin["\\kern{", TextString @ spacing, "pt}"];

spacerBox[n_] := TemplateBox[{n}, "Spacer1"];

(********************************************)

PackageExport["SpacedRowForm"]

declareNAryForm[SpacedRowForm];

$TemplateKatexFunction["SpacedRowForm"] = katexAliasRiffled["quad"];

(********************************************)

PackageExport["ThinSpacedForm"]

declareNAryForm[ThinSpacedForm];

$TemplateKatexFunction["ThinSpacedForm"] = katexAliasRiffled["enspace"];

(********************************************)

PackageExport["ExistsForm"]
PackageExport["ForAllForm"]

declareBinaryForm[ExistsForm] // usingCustomKatex["existsForm"];
declareBinaryForm[ForAllForm] // usingCustomKatex["forAllForm"];

(********************************************)

PackageExport["AndForm"]
PackageExport["OrForm"]

declareInfixSymbol[{AndForm, OrForm}];

(********************************************)

PackageExport["NotForm"]

declareUnaryForm[NotForm, maybeParen[SymbolForm]] // usingCustomKatex["notted"];

(********************************************)

PackageExport["ImpliesForm"]

declareInfixSymbol[ImpliesForm] // usingCustomKatex["implies"];

(********************************************)

PackageExport["EquivalentForm"]

declareInfixSymbol[EquivalentForm] // usingCustomKatex["iff"];

(********************************************)

PackageExport["BijectiveForm"]

declareInfixSymbol[BijectiveForm];

(********************************************)

PackageExport["MapsToForm"]

declareBinaryForm[MapsToForm] // usingCustomKatex["mto"];
$TemplateKatexFunction["MapsToSymbol"] = katexAlias["mtoSymbol"];

(**************************************************************************************************)

PackageExport["EllipsisSymbol"]
PackageExport["VerticalEllipsisSymbol"]

declareConstantSymbol[EllipsisSymbol];
declareConstantSymbol[VerticalEllipsisSymbol];

(**************************************************************************************************)

PackageExport["PartialDifferentialOfForm"]

declareBoxFormatting[
  PartialDifferentialOfForm[x_] :>
    makeTemplateBox[x, "PartialDifferentialOfForm"],
  PartialDifferentialOfForm[] :>
    SBox["PartialDifferentialSymbol"]
];

$TemplateKatexFunction["PartialDifferentialOfForm"] = "partialdof";
$TemplateKatexFunction["PartialDifferentialSymbol"] = katexAlias["partial"];

(**************************************************************************************************)

PackageExport["AndFunction"]
PackageExport["OrFunction"]
PackageExport["NotFunction"]

PackageExport["VertexListFunction"]
PackageExport["MinimalContractionsFunction"]
PackageExport["MinimalContractionSetsFunction"]
PackageExport["EdgeListFunction"]
PackageExport["PathListFunction"]
PackageExport["CardinalListFunction"]
PackageExport["SignedCardinalListFunction"]
PackageExport["SignedLengthFunction"]
PackageExport["LengthFunction"]
PackageExport["LCMFunction"]
PackageExport["HeadVertexFunction"]
PackageExport["TailVertexFunction"]
PackageExport["SplitFunction"]
PackageExport["WordFunction"]
PackageExport["AutomorphismsFunction"]
PackageExport["EndomorphismsFunction"]
PackageExport["BasisFunction"]
PackageExport["SupportFunction"]
PackageExport["GradeFunction"]
PackageExport["ModFunction"]
PackageExport["CoefficientFunction"]
PackageExport["MaxFunction"]
PackageExport["MinFunction"]
PackageExport["SinFunction"]
PackageExport["CosFunction"]
PackageExport["TanFunction"]
PackageExport["ArcTanFunction"]
PackageExport["TorusFunction"]
PackageExport["MobiusFunction"]

PackageExport["ClipFunction"]
PackageExport["SignFunction"]
PackageExport["StepFunction"]
PackageExport["DomainFunction"]
PackageExport["CodomainFunction"]
PackageExport["ProjectionFunction"]
PackageExport["LiftFunction"]
PackageExport["IdentityFunction"]
PackageExport["TotalFunction"]

PackageExport["StateMeetFunction"]
PackageExport["StateJoinFunction"]
PackageExport["StateExtentFunction"]
PackageExport["StateIntentFunction"]

PackageExport["StateComposeFunction"]
PackageExport["StateDecomposeFunction"]

declareFunctionFormatting[sym_] := With[
  {name = StringDelete[SymbolName[sym], "Function"]},
  declareBoxFormatting[
    sym :> TemplateBox[{name}, "NamedFunctionSymbolForm"],
    sym[args___] :> makeTypedTemplateBox[sym, args, "AppliedForm"]
  ]
];

Scan[declareFunctionFormatting, $namedFunctions];

$TemplateKatexFunction["NamedFunctionSymbolForm"] = namedFuncKatex;

(* to avoid conflict with built-in Katex: *)
namedFuncKatex["And"] := "\\andFn";
namedFuncKatex["Or"] := "\\orFn";
namedFuncKatex["Not"] := "\\Not";

namedFuncKatex["Word"] := "\\wordOf"; (* because 'word' already menas wordForm *)
namedFuncKatex["LCM"] := "\\lcm"; (* lowercase *)
namedFuncKatex["Mod"] := "\\modFunction"; (* becuase mod already defined *)
namedFuncKatex[s_String] := namedFuncKatex[s] = StringJoin["\\", LowerCaseFirst @ s];

(**************************************************************************************************)

PackageExport["QuotientForm"]
PackageExport["CompactQuotientForm"]
PackageExport["MultilineQuotientForm"]

declareBoxFormatting[
  QuotientForm[a_, b_] :>
    makeTemplateBox[a, b, "QuotientForm"],
  MultilineQuotientForm[a_, b_] :>
    makeTemplateBox[a, b, "MultilineQuotientForm"],
  CompactQuotientForm[f_, x_, v_] :>
    makeHintedTemplateBox[f -> QuiverSymbol, x -> VertexSymbol, v -> FunctionSymbol, "CompactQuotientForm"]
];

$TemplateKatexFunction["QuotientForm"] = "quotient";
$TemplateKatexFunction["MultilineQuotientForm"] = "multilineQuotient";
$TemplateKatexFunction["CompactQuotientForm"] = "compactQuotient";

(********************************************)

PackageExport["RowForm"]
PackageExport["TermRowForm"]

declareNAryForm[RowForm];
declareNAryForm[TermRowForm];

$TemplateKatexFunction["RowForm"] = riffled[" "];
$TemplateKatexFunction["TermRowForm"] = riffled[" "];

(********************************************)

PackageExport["CardinalRewriteForm"]

declareBoxFormatting[
  CardinalRewriteForm[a_, b_] :>
    TemplateBox[MapUnevaluated[cardinalBox, {a, b}], "CardinalRewriteForm"]
]

$TemplateKatexFunction["CardinalRewriteForm"] = "cardinalRewrite"

(********************************************)

PackageExport["ChartSymbolForm"]

declareBoxFormatting[
  ChartSymbolForm[elem_] :>
    makeTemplateBox[elem, "ChartSymbolForm"],
  ChartSymbolForm[elem_List] :>
    makeTypedBoxTemplate[elem -> ConcatenationForm, "ChartSymbolForm"],
  ChartSymbolForm[] :>
    SBox["ChartSymbol"]
];

$TemplateKatexFunction["ChartSymbolForm"] = "chart"
$TemplateKatexFunction["ChartSymbol"] = katexAlias["chartSymbol"];

(********************************************)

PackageExport["PositiveSignedPartForm"]
PackageExport["NegativeSignedPartForm"]

declareUnaryWrapperForm[PositiveSignedPartForm];
declareUnaryWrapperForm[NegativeSignedPartForm];

(********************************************)

PackageExport["SignedForm"]

declareUnaryWrapperForm[SignedForm]

declareAppliedFormatting[SignedForm];

(********************************************)

PackageExport["TransportMapSymbol"]

declareBoxFormatting[
  TransportMapSymbol[p_] :> makeTypedTemplateBox[p -> PathSymbol, "TransportMapSymbolForm"],
  TransportMapSymbol[] :> SBox["TransportMapSymbol"]
];

$TemplateKatexFunction["TransportMapSymbolForm"] = "transportMap"
$TemplateKatexFunction["TransportMapSymbol"] = "transportMapSymbol"

(********************************************)

(* how is this different from wordgroup? *)
PackageExport["CardinalGroupSymbolForm"]

declareSymbolForm[CardinalGroupSymbolForm, SymbolForm];

(********************************************)

(* this doesn't have any katex, and what is it for? shouldn't it be a word group? *)
(* PackageExport["CardinalGroupoidSymbolForm"]

declareSymbolForm[CardinalGroupoidSymbolForm, SymbolForm];
 *)
(********************************************)

PackageExport["TransportAtlasSymbolForm"]

declareBoxFormatting[
  TransportAtlasSymbolForm[q_] :> makeTypedTemplateBox[q -> QuiverSymbol, "TransportAtlasSymbolForm"],
  TransportAtlasSymbolForm[] :> SBox["TransportAtlasSymbol"]
];

$TemplateKatexFunction["TransportAtlasSymbolForm"] = "transportAtlas"
$TemplateKatexFunction["TransportAtlasSymbol"] = "\\transportAtlas{}"&

(********************************************)

PackageExport["GraphRegionIntersectionForm"]
PackageExport["GraphRegionUnionForm"]

declareInfixSymbol[{GraphRegionIntersectionForm, GraphRegionUnionForm}]

(********************************************)

declareBindingForm[form_, katexName_, argBoxFn_] := With[
  {formName = SymbolName @ form},
  declareBoxFormatting[
    form[first_, args__] :>
      TemplateBox[
        Prepend[
          MapUnevaluated[argBoxFn, {args}],
          makeQGBoxes @ first
        ],
        formName
      ]
  ];
  $TemplateKatexFunction[formName] = katexName[#1, Riffle[{##2}, ","]]&;
];

(********************************************)

PackageExport["BindingRuleForm"]

declareInfixSymbol[BindingRuleForm];

makeSizeBindingRuleBoxes = Case[
  s:(_SymbolForm | EllipsisSymbol | _Modulo) := MakeBoxes @ s;
  sz_QuiverSizeSymbol := MakeBoxes @ sz;
  sz_                 := MakeBoxes @ QuiverSizeSymbol @ sz;
];

makeCardinalSizeBindingRuleBoxes = Case[
  s:(_SymbolForm | EllipsisSymbol | _Modulo | _Integer) := MakeBoxes @ s;
  c_ -> sz_           := makeHintedTemplateBox[c -> CardinalSymbol, sz -> QuiverSizeSymbol @ sz, "CompactBindingRuleForm"];
  g_GroupGeneratorSymbol := MakeBoxes @ g;
  c_                  := cardinalBox @ c;
  Form[f_]            := makeQGBoxes @ f;
];


(********************************************)

PackageExport["CompactBindingRuleForm"]

declareInfixSymbol[CompactBindingRuleForm];

makeCardinalBindingRuleBoxes = Case[
  s:(_SymbolForm | EllipsisSymbol | _Modulo) := MakeBoxes @ s;
  c_                  := cardinalBox @ c;
];

(********************************************)

PackageExport["CayleyQuiverSymbolForm"]

declareUnaryForm[CayleyQuiverSymbolForm, GroupPresentationSymbol];

declareBoxFormatting[
  c_CayleyQuiverSymbolForm[args__] :> MakeBoxes @ CardinalSizeBindingForm[c, args]
];

(********************************************)

PackageExport["CayleyQuiverBindingForm"]
PackageExport["ActionQuiverBindingForm"]

declareBindingForm[CayleyQuiverBindingForm, "bindCayleyQuiver", makeGeneratorBindingRuleBoxes];
declareBindingForm[ActionQuiverBindingForm, "bindActionQuiver", makeGeneratorBindingRuleBoxes];

makeGeneratorBindingRuleBoxes = Case[
  s:(_SymbolForm | EllipsisSymbol | _SetSymbolForm) := MakeBoxes @ s;
  t_TupleForm             := MakeBoxes @ t;
  c_ -> g_                := makeHintedTemplateBox[c -> CardinalSymbol, g -> GroupGeneratorSymbol, "BindingRuleForm"];
  g_GroupGeneratorSymbol  := MakeBoxes @ g;
  g_GroupElementSymbol  := MakeBoxes @ g;
  g_                      := MakeBoxes @ GroupGeneratorSymbol @ g;
]

(********************************************)

PackageExport["SubSizeBindingForm"]

declareBindingForm[SubSizeBindingForm, "subSize", makeSizeBindingRuleBoxes];

(********************************************)

PackageExport["SizeBindingForm"]

declareBindingForm[SizeBindingForm, "bindSize", makeSizeBindingRuleBoxes];

(********************************************)

PackageExport["CardinalBindingForm"]

declareBindingForm[CardinalBindingForm, "bindCards", makeCardinalBindingRuleBoxes];

(********************************************)

PackageExport["CardinalSizeBindingForm"]

declareBindingForm[CardinalSizeBindingForm, "bindCardSize", makeCardinalSizeBindingRuleBoxes];

(********************************************)

PackageExport["ParenPathWordForm"]
PackageExport["ParenEmptyPathWordForm"]

declareBoxFormatting[
  ParenPathWordForm[a_, b_, c_] :> ReplacePart[MakeBoxes @ PathWordForm[a, b, c], 2 -> "ParenPathWordForm"],
  ParenEmptyPathWordForm[v_] :> ReplacePart[MakeBoxes @ EmptyPathWordForm[v], 2 -> "ParenPathWordForm"]
];

$TemplateKatexFunction["ParenPathWordForm"] = "parenPathWord";

(********************************************)

PackageExport["EmptyPathWordForm"]

declareBoxFormatting[
  EmptyPathWordForm[v_] :>
    MakeBoxes @ PathWordForm[v, {}, v]
];

(********************************************)

PackageExport["PathWordForm"]

PathWordForm[a_, b_String, c_] := PathWordForm[a, ToPathWord @ b, c];

declareBoxFormatting[
  PathWordForm[t_, w_, h_] :>
    makeTypedTemplateBox[t -> generalizedVertexSymbol, w -> WordForm, h -> generalizedVertexSymbol, "PathWordForm"]
];

SetHoldAllComplete[generalizedVertexSymbol, generalizedVertexBoxes];

declareBoxFormatting[
  generalizedVertexSymbol[e_] :> recurseWrapperBoxes[e, generalizedVertexBoxes]
];

generalizedVertexBoxes = Case[
  s_Symbol | s_String := MakeBoxes @ VertexSymbol @ s;
  e_ := MakeBoxes[e];
];

$TemplateKatexFunction["PathWordForm"] = "pathWord";

(********************************************)

PackageExport["$AutoColorCardinals"]

$AutoColorCardinals = True;

(********************************************)

PackageExport["TupleSequence"]
PackageExport["ListSequence"]
PackageExport["MultisetSequence"]
PackageExport["PlusSequence"]
PackageExport["CommaSequence"]

ListSequence[args___] := ListForm @ EllipsisSequence[args];
MultisetSequence[args___] := MultisetForm @ EllipsisSequence[args];
PlusSequence[args___] := PlusForm @ EllipsisSequence[args];
TupleSequence[args___] := TupleForm @ EllipsisSequence[args];
CommaSequence[args___] := CommaForm @ EllipsisSequence[args];

PackageExport["EllipsisSequence"]

toSeqF = Case[
  f_Function                     := f;
  f_ /; ContainsQ[f, \[FormalI]] := Construct[Function, \[FormalI], f];
  f_                             := f
];

EllipsisSequence[f_] :=
  EllipsisSequence[f, SymbolForm["n"]];

EllipsisSequence[f_, n_, k_Integer:2] := With[
  {f2 = toSeqF @ f,
   n2 = Switch[n, None, None, Automatic | Null, SymbolForm["n"], _Symbol | _String, SymbolForm @ n, _, n]},
  Sequence @@ Flatten[{f2 /@ Range[1, k], {EllipsisSymbol, If[n2 === None, Nothing, f2 @ n2]}}]
];

EllipsisSequence[f_, n_, k_:1, "Reversed" -> True] :=
  Apply[Sequence, Reverse @ List @ EllipsisSequence[f, n, k]]

(********************************************)

PackageExport["MakeSequence"]

MakeSequence[f_, n_] := With[
  {f2 = toSeqF @ f},
  f2 /@ Range[1, n]
];

(********************************************)

PackageExport["CreateSequenceVars"]

CreateSequenceVars[baseVar_, f_, n_] := With[
  {f2 = toSeqF @ f, symName = If[StringQ[baseVar], baseVar, SymbolName[baseVar]]},
  Sequence @@ Map[
    With[{s = Symbol[symName <> IntegerString[#]]}, Set[s, f2[#]]]&,
    Range[1, n]
  ]
];

(********************************************)

PackageExport["WordSymbol"]

declareBoxFormatting[
  WordSymbol[s_] :> TemplateBox[List @ rawSymbolBoxes @ s, "WordSymbolForm"]
];

$TemplateKatexFunction["WordSymbolForm"] = "wordSymbol";

(********************************************)

PackageExport["WordForm"]

WordForm[s_String] := WordForm @ ToPathWord @ s;

declareBoxFormatting[
  WordForm[e_] :> wordBoxes[e]
];

$TemplateKatexFunction["EmptyWordForm"] := "emptyWord"
$TemplateKatexFunction["WordForm"] = "word";

$cardP = _CardinalSymbol | InvertedForm[_CardinalSymbol] | MirrorForm[_CardinalSymbol] ;
$maybeColoredCardP = $colorFormP[$cardP] | $cardP;

SetHoldAllComplete[wordBoxes, cardinalBox];
wordBoxes = Case[
  {}|""                               := SBox["EmptyWordForm"];
  1                                   := TemplateBox[{"1"}, "WordForm"];
  word_String                         := Construct[%, ToPathWord @ word];
  Style[word_String, assoc_Association] := applyCardColoring[Construct[%, ToPathWord @ word], assoc];
  (Times|ConcatenationForm)[args__]   := TemplateBox[MapUnevaluated[%, {args}], "ConcatenationForm"];
  RepeatedPowerForm[a_, b_]           := TemplateBox[{% @ a, makeQGBoxes @ b}, "RepeatedPowerForm"];
  c:cardP                             := TemplateBox[List @ MakeBoxes @ c, "WordForm"];
  list:{cardP..}                      := TemplateBox[MapUnevaluated[MakeBoxes, list], "WordForm"];
  list_List                           := TemplateBox[tryColorCardinals @ cardinalBoxes @ list, "WordForm"];
  (Inverted|InvertedForm)[e_]         := TemplateBox[List @ wordBoxes @ e, "InvertedForm"];
  MirrorForm[e_]                      := TemplateBox[List @ wordBoxes @ e, "MirrorForm"];
  Form[f_]                            := makeQGBoxes @ f;
  cs_CardinalSequenceForm             := MakeBoxes @ cs;
  sc_SerialCardinal                   := MakeBoxes @ sc;
  pc_ParallelCardinal                 := MakeBoxes @ pc;
  w_WordForm                          := MakeBoxes @ w;
  w_WordSymbol                        := MakeBoxes @ w;
  w_SymbolForm                        := MakeBoxes @ s; (* placeholder *)
  s:symsP                             := TemplateBox[List @ rawSymbolBoxes @ s, "WordSymbolForm"],
  {symsP -> $rawSymbolP, cardP -> $maybeColoredCardP}
];

applyCardColoring[list_, colors_] := ReplaceAll[list,
  TemplateBox[{c_String}, form_] /; KeyExistsQ[colors, c] :>
    TemplateBox[{TemplateBox[{c}, form]}, SymbolName @ colors @ c]];

cardinalBoxes[list_List] := Map[cardinalBox, list];

cardSymBox[e_] := TemplateBox[{e}, "CardinalSymbolForm"];

toNegCard[TemplateBox[{e_}, "CardinalSymbolForm"]] :=
  TemplateBox[{e}, "InvertedCardinalSymbolForm"];

toNegCard[TemplateBox[{e_}, "MirrorCardinalSymbolForm"]] :=
  TemplateBox[{e}, "InvertedMirrorCardinalSymbolForm"];

toMirrorCard[TemplateBox[{e_}, "CardinalSymbolForm"]] :=
  TemplateBox[{e}, "MirrorCardinalSymbolForm"];

toMirrorCard[TemplateBox[{e_}, "InvertedCardinalSymbolForm"]] :=
  TemplateBox[{e}, "MirrorInvertedCardinalSymbolForm"];

cardinalBox = Case[
  c_CardinalSymbol                    := MakeBoxes @ c;
  c:InvertedForm[_CardinalSymbol]     := MakeBoxes @ c;
  s_String                            := cardSymBox @ s;
  (col:colsP)[e_]                     := TemplateBox[List @ MakeBoxes @ e, SymbolName @ col];
  i_Integer                           := cardSymBox @ TextString @ i;
  s:symsP                             := cardSymBox @ rawSymbolBoxes @ s;
  cs_CardinalSequenceForm             := MakeBoxes @ cs;
  sc_SerialCardinal                   := MakeBoxes @ sc;
  pc_ParallelCardinal                 := MakeBoxes @ pc;
  MirrorForm[s_]                      := toMirrorCard @ % @ s;
  Inverted[s_]                        := toNegCard @ % @ s;
  p_CardinalProductForm               := MakeBoxes @ p;
  p_VerticalCardinalProductForm       := MakeBoxes @ p,
  {symsP -> $rawSymbolP, colsP -> $colorFormP}
]

(* only colors if it is all-or-nothing *)
tryColorCardinals[list_] /; $AutoColorCardinals :=
  If[SubsetQ[{"r", "g", "b"}, list /. TemplateBox[{a_}, _] :> a],
    list /. $colorCardinalRules,
    list
  ];

tryColorCardinals[list_] := list;

$cardFormP = "CardinalSymbolForm" | "InvertedCardinalSymbolForm";

$colorCardinalRules = {
  b:TemplateBox[{"r"}, $cardFormP] -> TemplateBox[List @ b, "RedForm"],
  b:TemplateBox[{"g"}, $cardFormP] -> TemplateBox[List @ b, "GreenForm"],
  b:TemplateBox[{"b"}, $cardFormP] -> TemplateBox[List @ b, "BlueForm"]
};


(********************************************)

PackageExport["PlainWordForm"]

PlainWordForm[s_String] := PlainWordForm @ ToPathWord @ s;

declareBoxFormatting[
  PlainWordForm[e_] :> Block[{$AutoColorCardinals = False}, wordBoxes[e]]
];


(********************************************)

PackageExport["DoubleQuotedStringForm"]
PackageExport["QuotedCharacterForm"]
PackageExport["WildcardStringForm"]

PackageExport["LiteralStringForm"]
PackageExport["StringSymbolForm"]

PackageExport["LiteralCharacterForm"]
PackageExport["CharacterSymbolForm"]

declareBoxFormatting[
  DoubleQuotedStringForm[args___] :>
    TemplateBox[MapUnevaluated[stringElementBoxes, {args}], "DoubleQuotedStringForm"],

  WildcardStringForm[args___] :>
    TemplateBox[MapUnevaluated[stringElementBoxes, {args}], "WildcardStringForm"],

  QuotedCharacterForm[a_] :>
    TemplateBox[List @ quotedCharBoxes @ a, "QuotedCharacterForm"],

  LiteralStringForm[str_String] :> TemplateBox[List @ StringJoin["\"", str, "\""], "LiteralStringForm"],
  LiteralCharacterForm[str_String] :> TemplateBox[List @ StringJoin["\"", str, "\""], "LiteralCharacterForm"]
];

$TemplateKatexFunction["WildcardStringForm"] = applyRiffled["wstring", " "];

$TemplateKatexFunction["DoubleQuotedStringForm"] = applyRiffled["qstring", " "];
$TemplateKatexFunction["QuotedCharacterForm"] = "qchar";

$TemplateKatexFunction["LiteralStringForm"] = katexWrapText["lstr"];
$TemplateKatexFunction["LiteralCharacterForm"] = katexWrapText["lchar"];

$TemplateKatexFunction["StringSymbolForm"] = "strsym";
$TemplateKatexFunction["CharacterSymbolForm"] = "charsym";

katexWrapText[op_][s_String] := op @ "texttt" @ StringTrim[StringReplace[s, "-" -> "\\textendash "]; s, "\""];

declareSymbolForm[CharacterSymbolForm]
declareSymbolForm[StringSymbolForm]

SetHoldAllComplete[stringElementBoxes, stringSymbolBoxes, quotedCharBoxes]

stringElementBoxes = Case[
  s_String /; StringLength[s] === 1 := If[UpperCaseQ[s],
    MakeBoxes @ StringSymbolForm @ s,
    MakeBoxes @ LiteralCharacterForm @ s
  ];
  str_String := MakeBoxes @ LiteralStringForm @ str;
  s:lsymsP := MakeBoxes @ s;
  e_ ? unaryWrappedQ := recurseWrapperBoxes[e, stringSymbolBoxes];
  other := MakeBoxes @ other;
,
  {lsymsP -> $literalSymbolsP}
]

stringSymbolBoxes = Case[
  s_String := If[UpperCaseQ[s],
    MakeBoxes @ StringSymbolForm @ s,
    MakeBoxes @ CharacterSymbolForm @ s
  ];
  s_StringSymbolForm | s_CharacterSymbolForm := MakeBoxes @ s;
]

(* this should only be a literal or a CharacterSymbol *)
quotedCharBoxes = Case[
  s_String /; StringLength[s] === 1 := MakeBoxes @ LiteralCharacterForm @ s;
  s_CharacterSymbolForm := MakeBoxes @ s;
  e_ ? unaryWrappedQ := recurseWrapperBoxes[e, %];
]

(**************************************************************************************************)

PackageExport["Form"]

declareBoxFormatting[
  Form[e_] :> makeQGBoxes @ e
]

SetHoldAllComplete[makeQGBoxes];

$binaryRelationMapping = <|
  Equal              -> "=",
  Plus               -> "+",
  Unequal            -> "\[NotEqual]",
  Subset             -> "\[Subset]",
  SubsetEqual        -> "\[SubsetEqual]",
  Superset           -> "\[Superset]",
  SupersetEqual      -> "\[SupersetEqual]",
  NotSubset          -> "\[NotSubset]",
  NotSubsetEqual     -> "\[NotSubsetEqual]",
  NotSuperset        -> "\[NotSuperset]",
  NotSupersetEqual   -> "\[NotSupersetEqual]",
  TildeEqual         -> "\[TildeEqual]",
  TildeFullEqual     -> "\[TildeFullEqual]",
  TildeTilde         -> "\[TildeTilde]",
  LessEqual          -> "\[LessEqual]",
  Less               -> "<",
  GreaterEqual       -> "\[GreaterEqual]",
  Greater            -> ">"
|>;

$binaryRelationHeads = Alternatives @@ Keys[$binaryRelationMapping];

$domainsP = Alternatives[Integers, Reals, Rationals, Complexes, Naturals, PositiveNaturals, PositiveReals, UnitInterval, Primes];

(* this is the general dispatch mechanism for a form of unknown type *)
makeQGBoxes = Case[
  None | Null               := "";
  Pi                        := MakeBoxes @ PiSymbol;
  Tau                       := MakeBoxes @ TauSymbol;
  l:lsymsP                  := MakeBoxes @ l;
  s:namedFnP                := MakeBoxes @ s;
  d:domainsP                := MakeBoxes @ d;
  e:symP                    := symbolBoxes @ e;
  e_Subtract                := algebraBoxes[e, "SubtractForm"];
  Plus[a_, Times[-1, b_]]   := MakeBoxes @ SubtractForm[a, b];
  Plus[a_, Times[n_Integer ? Negative, b_]] := With[{n2 = Abs @ n}, MakeBoxes @ SubtractForm[a, ImplicitTimesForm[n2, b]]];
  Times[n_Integer, e_]      := MakeBoxes @ ImplicitTimesForm[n, e];
  e_Times                   := algebraBoxes[e, "ImplicitTimesForm"];
  e_Plus                    := algebraBoxes[e, "PlusForm"];
  Equal[a_, b_]             := MakeBoxes @ EqualForm[a, b];
  Unequal[a_, b_]           := MakeBoxes @ UnequalForm[a, b];
  h:binHeads                := Lookup[$binaryRelationMapping, h];
  (h:binHeads)[args__]      := With[{str = Lookup[$binaryRelationMapping, h]}, MakeBoxes @ BinaryRelationForm[str][args]];
  (i_InverseForm)[args__]   := MakeBoxes @ AppliedForm[i, args];
  Minus[e_]                 := makeTemplateBox[e, "MinusForm"];
  Power[e_, -1]             := makeTemplateBox[e, "InverseForm"];
  Times[-1, e_]             := makeTemplateBox[e, "MinusForm"];
  Inverted[e_]               := makeTemplateBox[e, "InvertedForm"];
  DirectedEdge[args__]      := MakeBoxes @ DirectedEdgeForm[args];
  UndirectedEdge[args__]    := MakeBoxes @ UndirectedEdgeForm[args];
  Labeled[a_, l_]           := MakeBoxes @ ParenthesesLabeledForm[a, l];
  Text[t_]                  := MakeBoxes @ MathTextForm[t];
  Row[{r__}]                := MakeBoxes @ RowForm[r];
  Modulo[n_]                := MakeBoxes @ ModuloForm[n];
  Invisible[n_]             := TemplateBox[List @ MakeBoxes @ n, "InvisibleForm"];
  {a_, b__}                 := MakeBoxes @ TupleForm[a, b];
  other_                    := MakeBoxes @ other,
  {lsymsP -> $literalSymbolsP, symP -> $rawSymbolP, namedFnP -> Alternatives @@ $namedFunctions,
    binHeads -> $binaryRelationHeads, domainsP -> $domainsP}
];

$TemplateKatexFunction["InvisibleForm"] := "phantom";

algebraBoxes[_[args__], tag_] := makeTemplateBox[args, tag];

(**************************************************************************************************)

SetHoldAllComplete[rawSymbolBoxes, toSymbolName];

rawSymbolBoxes = Case[
  l:lsymsP                    := MakeBoxes @ l;
  (c:colorP)[e_]              := TemplateBox[List @ % @ e, SymbolName @ c];
  s_Symbol                    := toSymbolName[s];
  str_String                  := str;
  i_Integer                   := TextString @ i;
  s_SymbolForm                := MakeBoxes @ s;
  PrimedForm[x_]              := TemplateBox[List @ % @ x, "PrimedForm"];
  Subscript[a_, b_]           := SubscriptBox[makeQGBoxes @ a, makeQGBoxes @ b];
  Subscript[a_, b_, c_]       := SubscriptBox[makeQGBoxes @ a, RBox[makeQGBoxes @ b, ",", makeQGBoxes @ c]];
  Superscript[a_, b_]         := SuperscriptBox[makeQGBoxes @ a, makeQGBoxes @ b];
  Subsuperscript[a_, b_, c_]  := SubsuperscriptBox[% @ a, makeQGBoxes @ b, makeQGBoxes @ c];
  m_WhiteCircleModifierForm   := MakeBoxes @ m;
  m_BlackCircleModifierForm   := MakeBoxes @ m;
,
  {lsymsP -> $literalSymbolsP, colorP -> $colorFormP}
]

(* todo: support formal symbols, rewriting them as necessary *)

toSymbolName[e_] := SymbolName[Unevaluated @ e];

(**************************************************************************************************)

SetHoldAllComplete[makeNaryHintedTemplateBox, makeHintedTemplateBox, toHintedSymbol];

makeNaryHintedTemplateBox[None, args___, tag_] :=
  makeTemplateBox[args, tag];

makeNaryHintedTemplateBox[hint_, args___, tag_] :=
  TemplateBox[
    MapUnevaluated[toHintedSymbol[# -> hint]&, {args}],
    tag
  ];

makeHintedTemplateBox[args___, tag_] :=
  TemplateBox[
    MapUnevaluated[toHintedSymbol, {args}],
    tag
  ];

toHintedSymbol = Case[
  Rule[None, _] :=
    "";
  Rule[Form[e_], _] :=
    makeQGBoxes @ e;
  Rule[e_, None] :=
    e;
  Rule[e_, Automatic] :=
    makeQGBoxes @ e;
  Rule[e_, m_maybeParen] :=
    m @ e;
  Rule[l:literalP, _] :=
    MakeBoxes @ l;
  Rule[BlankSymbol, _] :=
    MakeBoxes @ BlankSymbol;
  Rule[(c:colorsP)[arg_], hint_] :=
    TemplateBox[List @ toHintedSymbol[arg -> hint], SymbolName @ c];
  Rule[arg:symsP, hint_] :=
    MakeBoxes @ hint @ arg;
  Rule[arg_, _] :=
    makeQGBoxes @ arg;
  arg_ :=
    makeQGBoxes @ arg,
  {symsP -> $rawSymbolP, colorsP -> $colorFormP, literalP -> $literalSymbolsP}
]

(********************************************)

SetHoldAllComplete[maybeParen, maybeParenBoxes];

maybeParen[h_][b_] := Block[{$eh = h, $ehc = If[Head[h] === Alternatives, First @ h, h]}, maybeParenBoxes @  b];

maybeParenBoxes = Case[
  NoParenthesesForm[e_]                 := MakeBoxes @ e;
  l:lsymsP                              := MakeBoxes @ l;
  s:symsP                               := With[{head = $ehc}, MakeBoxes @ head @ s];
  e:_InverseForm | _GroupInverseForm | _GroupoidInverseForm | _AppliedForm := MakeBoxes @ e;
  (e:(head_[___])) /; MatchQ[head, $eh] := MakeBoxes @ e;
  e:(colsP[_])                          := MakeBoxes @ e;
  other_                                := MakeBoxes @ ParenthesesForm @ other,
  {symsP -> $rawSymbolP, colsP -> $colorFormP, lsymsP -> $literalSymbolsP}
];