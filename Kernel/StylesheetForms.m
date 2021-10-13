(**************************************************************************************************)

PackageExport["Naturals"]

SetUsage @ "Naturals represents the natural numbers."

MakeBoxes[Naturals, StandardForm] := TemplateBox[{}, "Naturals"];
MakeBoxes[Naturals, TraditionalForm] := TemplateBox[{}, "Naturals"];

applyRiffled[f_, op_][args___] := f[riffled[op][args]];

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

declareSymbolForm::badsym = "Name of symbol `` should end in SymbolForm."
declareSymbolForm[head_Symbol] := Scope[
  name = SymbolName @ head;
  Which[
    StringEndsQ[name, "SymbolForm"], Null,
    StringEndsQ[name, "Symbol"], name = name <> "Form",
    True, ReturnFailed["badsym", head]
  ];
  With[{name2 = name},
    declareBoxFormatting[head[s_] :> makeTypedTemplateBox[s -> RawSymbolForm, name2]];
  ];
  $TemplateKatexFunction[name] = LowerCaseFirst @ StringDrop[name, -10];
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
    form[] :> TemplateBox[{}, symbolName]
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
    symbol :> TemplateBox[{}, symbolName]
  ];
  $TemplateKatexFunction[symbolName] = katexAlias[katexName];
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
  
  declareBoxFormatting[symbol[] :> TemplateBox[{}, symbolName]];
  $TemplateKatexFunction[symbolName] = LowerCaseFirst @ symbolName;
];

(********************************************)

declareCommaRiffledForm[symbol_, katex_] := With[
  {formName = SymbolName[symbol]},
  declareBoxFormatting[
    symbol[args__] :> makeTemplateBox[args, formName]
  ];
  $TemplateKatexFunction[formName] = applyRiffled[katex, ","];
];

(********************************************)

PackageExport["SymbolForm"]

declareBoxFormatting[
  SymbolForm[p_] :>
    TemplateBox[List @ symbolBoxes @ p, "SymbolForm"]
];

$TemplateKatexFunction["SymbolForm"] = "sym";

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
  _PathMapSymbol, _MapSymbol, _ChartSymbol,
  EllipsisSymbol, BlankSymbol
];

(********************************************)

symbolBoxes = Case[
  s:($symbolFormsP)   := MakeBoxes @ s;
  NegatedForm[n_]     := % @ Negated @ n;
  Negated[n_]         := NegatedBoxForm @ RawBoxes @ % @ n;
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
  (Negated|NegatedForm)[e_]          := TemplateBox[List @ % @ e, "NegatedForm"];
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

$colorFormP = Alternatives[
  RedForm, BlueForm, GreenForm,
  RedBlueForm, GreenBlueForm, RedGreenForm,
  DarkGrayForm, MediumGrayForm, LightGrayForm
];

NegatedForm[(c:$colorFormP)[e_]] := c[NegatedForm[e]];

toTypedSymbol = Case[
  Rule[e_, None] := e;
  Rule[BlankSymbol, _] :=
    MakeBoxes @ BlankSymbol;
  Rule[(c:colorsP)[arg_], type_] :=
    TemplateBox[List @ toTypedSymbol[arg -> type], SymbolName @ c];
  Rule[arg:((type_)[___]), type_] :=
    MakeBoxes @ arg;
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
  BlankSymbol :> TemplateBox[{}, "BlankSymbol"]
]

$TemplateKatexFunction["BlankSymbol"] = Function["\\blank"];

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
PackageExport["VertexOfForm"]
PackageExport["EdgeOfForm"]
PackageExport["PathOfForm"]

declareBoxFormatting[
  ElementOfForm[a__, b_] :> makeTemplateBox[CommaRowForm[a], b, "ElementOfForm"],
  ElementOfForm[a_, b_] :> makeTemplateBox[a, b, "ElementOfForm"],
  VertexOfForm[a_, b_] :> makeHintedTemplateBox[a -> VertexSymbol, b -> QuiverSymbol, "VertexOfForm"],
  EdgeOfForm[a_, b_] :> makeHintedTemplateBox[a -> EdgeSymbol, b -> QuiverSymbol, "EdgeOfForm"],
  PathOfForm[a_, b_] :> makeHintedTemplateBox[a -> PathSymbol, b -> QuiverSymbol, "PathOfForm"]
];

$TemplateKatexFunction["ElementOfForm"] = "elemOf";
$TemplateKatexFunction["VertexOfForm"] = "vertOf";
$TemplateKatexFunction["EdgeOfForm"] = "edgeOf";
$TemplateKatexFunction["PathOfForm"] = "pathOf";

(**************************************************************************************************)

PackageExport["EdgesSymbol"]
PackageExport["VerticesSymbol"]

declareBoxFormatting[
  EdgesSymbol[q_] :> makeHintedTemplateBox[q -> QuiverSymbol, "EdgesSymbolForm"],
  EdgesSymbol[] :> TemplateBox[{}, "EdgesSymbol"],
  VerticesSymbol[q_] :> makeHintedTemplateBox[q -> QuiverSymbol, "VerticesSymbolForm"],
  VerticesSymbol[] :> TemplateBox[{}, "VerticesSymbol"]
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
  OtherwiseSymbol :> TemplateBox[{}, "OtherwiseSymbol"]
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

katexPiecewiseRow[case_, o:TemplateBox[{}, "OtherwiseSymbol"]] :=
  {case, " &", o, "\n"};

makePiecewiseRow[All -> value_] :=
  {makeQGBoxes @ value, MakeBoxes @ OtherwiseSymbol}

makePiecewiseRow[case_ -> value_] :=
  {makeQGBoxes @ value, makeQGBoxes @ case};

(**************************************************************************************************)

PackageExport["FunctionSpaceForm"]

declareBoxFormatting[
  FunctionSpaceForm[from_, to_] :> makeHintedTemplateBox[from, to -> BaseFieldSymbol, "FunctionSpaceForm"]
];

$TemplateKatexFunction["FunctionSpaceForm"] = "functionSpace"

(**************************************************************************************************)

PackageExport["FiniteFieldSymbol"]

declareBoxFormatting[
  FiniteFieldSymbol[n_] :> makeTemplateBox[n, "FiniteField"]
];

$TemplateKatexFunction["FiniteField"] = "finiteField";

(**************************************************************************************************)

(* PackageExport["SignedForm"]

SignedForm[s_String] := FunctionSymbol @ SignedForm @ s;
(* SignedForm[(h:FunctionSymbol|PathMapSymbol)[arg_]] := h[SignedForm @ arg]; *)

declareBoxFormatting[
  SignedForm[(h:FunctionSymbol|PathMapSymbol)[arg_]] :>
    MakeBoxes[head @ SignedForm @ arg],

  SignedForm[e_] :>
    makeTemplateBox[e, "SignedForm"]
]; *)

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
  AutomorphismsFunction,
  BasisFunction,
  SupportFunction,
  SplitFunction,
  LCMFunction,
  GradeFunction
};

$functionHeads = {
  FunctionSymbol, PathMapSymbol,
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

PackageExport["GroupoidSymbol"]

$groupoidAliases = <|
  "N" -> "Naturals",
  "C" -> "Complexes",
  "R" -> "Reals",
  "Z" -> "Integers",
  "Q" -> "Rationals"
|>

declareBoxFormatting[
  GroupoidSymbol[s_String /; KeyExistsQ[$groupoidAliases, s]] :>
    TemplateBox[List @ TemplateBox[{}, Lookup[$groupoidAliases, s]], "GroupoidSymbolForm"],

  GroupoidSymbol["\[Gamma]"] :>
    MakeBoxes @ PathGroupoidSymbol["Q"],
  
  GroupoidSymbol[n_] :>
    TemplateBox[List @ rawSymbolBoxes @ n, "GroupoidSymbolForm"]

]

$TemplateKatexFunction["GroupoidSymbolForm"] = "groupoid";

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

GroupoidFunctionSymbol[] := GroupoidFunctionSymbol["\[Mu]"]

declareSymbolForm[GroupoidFunctionSymbol];

(**************************************************************************************************)

PackageExport["AffineModifierForm"]

declareUnaryWrapperForm[AffineModifierForm]

(**************************************************************************************************)

PackageExport["ModuloForm"]

declareUnaryWrapperForm[ModuloForm]

(**************************************************************************************************)

PackageExport["GroupFunctionSymbol"]

GroupFunctionSymbol[] := GroupoidFunctionSymbol["\[Pi]"]

declareSymbolForm[GroupFunctionSymbol];

(**************************************************************************************************)

declareAlgebraicSymbol[sym_Symbol, aliases_] := With[
  {symName = SymbolName @ sym},
  {formName = symName <> "Form"},

  declareBoxFormatting[

    sym[s_String /; KeyExistsQ[aliases, s]] :>
      TemplateBox[List @ TemplateBox[{}, Lookup[aliases, s]], formName],

    sym[s_Symbol /; MemberQ[aliases, SymbolName @ s]] :>
      TemplateBox[List @ TemplateBox[{}, SymbolName @ s], formName],

    sym[n_] :>
      TemplateBox[List @ rawSymbolBoxes @ n, formName]

  ];

  $TemplateKatexFunction[formName] = ToLowerCase @ StringTrim[symName, "Symbol"];
];

(**************************************************************************************************)

PackageExport["GroupSymbol"]

GroupSymbol[] := GroupSymbol["G"];

declareAlgebraicSymbol[GroupSymbol, $groupoidAliases];

(**************************************************************************************************)

PackageExport["CyclicGroupForm"]

declareBoxFormatting[
  CyclicGroupForm[n_] :> makeTemplateBox[n, "CyclicGroupForm"]
]

$TemplateKatexFunction["CyclicGroupForm"] = "cyclicGroup";

(**************************************************************************************************)

PackageExport["GroupPresentationForm"]
PackageExport["GroupRelationForm"]
PackageExport["GroupGeneratorSymbol"]

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
  relation_  := MakeBoxes @ groupRelationBoxes @ relation;
]

groupRelationBoxes = Case[
  EqualForm[a_, b_]     := MakeBoxes @ GroupRelationForm[a, b];
  gr_GroupRelationForm  := MakeBoxes @ gr;
  a_                    := groupRelationTermBoxes @ a;
];

$TemplateKatexFunction["GroupPresentationForm"] = "groupPresentation"

groupGeneratorBoxes = Case[
  list_List               := TemplateBox[MapUnevaluated[%, list], "CommaRowForm"];
  s:symP                  := MakeBoxes @ GroupGeneratorSymbol @ s;
  CardinalSymbol[s_]      := MakeBoxes @ GroupGeneratorSymbol @ s;
  e_ ? unaryWrappedQ      := recurseWrapperBoxes[e, %];
  gr_GroupGeneratorSymbol := MakeBoxes @ gr;
,
  symP -> $rawSymbolP
]

declareSymbolForm[GroupGeneratorSymbol];

declareBoxFormatting[
  GroupRelationForm[a_, b_] :>
    TemplateBox[
      MapUnevaluated[groupRelationTermBoxes, {a, b}],
      "GroupRelationForm"
    ],
  GroupRelationForm[a_] :>
    TemplateBox[List @ groupRelationTermBoxes @ a, "GroupRelationForm"],
  GroupRelationForm[] :>
    TemplateBox[{}, "GroupRelationSymbol"]
]

SetHoldAllComplete[groupRelationTermBoxes];

groupRelationTermBoxes = Case[
  list_List                   := TemplateBox[MapUnevaluated[%, list], "ImplicitGroupMultiplicationForm"];
  (Power|GroupPower)[g_, e_]  := TemplateBox[{% @ g, makeQGBoxes @ e}, "GroupPowerForm"];
  1                           := MakeBoxes @ GroupElementSymbol["e"];
  s:symP                      := MakeBoxes @ GroupElementSymbol @ s;
  GroupInverseForm[e_]        := TemplateBox[List @ % @ e, "GroupInverseForm"];
  CardinalSymbol[s_]          := MakeBoxes @ GroupElementSymbol @ s;
  e_ ? unaryWrappedQ          := recurseWrapperBoxes[e, %] /. "NegatedForm" -> "GroupInverseForm";
  ge_GroupElement             := MakeBoxes @ ge;
  gm_GroupMultiplicationForm  := MakeBoxes @ gm;
  GroupCommutatorForm[a_, b_] := TemplateBox[{% @ a, % @ b}, "GroupCommutatorForm"];
,
  symP -> $rawSymbolP
];

declareInfixKatexAlias["GroupRelation", "groupRelationIso"];

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

(**************************************************************************************************)

PackageExport["GeneralLinearGroup"]

declareBoxFormatting[
  GeneralLinearGroup[n_] :>
    MakeBoxes @ GeneralLinearGroup[n, Reals],
  GeneralLinearGroup[n_, f_] :>
    TemplateBox[{rawSymbolBoxes @ n, fieldOrRingBoxes @ f}, "GeneralLinearGroupForm"]
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


$TemplateKatexFunction["GeneralLinearGroupForm"] = "gl";

(**************************************************************************************************)

PackageExport["ContractionLatticeSymbol"]

SetUsage @ "
ContractionLatticeSymbol[q$] represents the lattice of contractions of a quiver q$.
"

declareBoxFormatting[
  HoldPattern[ContractionLatticeSymbol[q_]] :>
    makeTypedTemplateBox[q -> QuiverSymbol, "ContractionLatticeSymbolForm"]
];

$TemplateKatexFunction["ContractionLatticeSymbolForm"] = "contractionLattice";

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

declareBoxFormatting[
  
  (* this looks suspicious *)
  WordGroupSymbol[] :>
    TemplateBox[{}, "WordGroupSymbolForm"],

  WordGroupSymbol[s_] :>
    makeTypedTemplateBox[s -> QuiverSymbol, "WordGroupSymbolForm"]

];

$TemplateKatexFunction["WordGroupSymbolForm"] = "wordGroup";

(**************************************************************************************************)

PackageExport["GroupElementSymbol"]

GroupElementSymbol[] := GroupElementSymbol["g"];

declareSymbolForm[GroupElementSymbol];

(**************************************************************************************************)

PackageExport["GroupoidElementSymbol"]

GroupoidElementSymbol[] := GroupoidElementSymbol["g"];

declareSymbolForm[GroupoidElementSymbol];

(**************************************************************************************************)

PackageExport["GroupInverseForm"]
PackageExport["GroupoidInverseForm"]

declareUnaryForm[GroupInverseForm, maybeParen[GroupElementSymbol]];
declareUnaryForm[GroupoidInverseForm, maybeParen[GroupoidElementSymbol]];

(**************************************************************************************************)

PackageExport["MatrixDotForm"]

declareInfixSymbol[MatrixDotForm, maybeParen[MatrixSymbol|TranslationVectorForm]]

(**************************************************************************************************)

PackageExport["GroupMultiplicationForm"]
PackageExport["ImplicitGroupMultiplicationForm"]
PackageExport["GroupoidMultiplicationForm"]

declareInfixSymbol[GroupMultiplicationForm, maybeParen[GroupElementSymbol]] // usingCustomKatex["Gmult"];
declareInfixSymbol[GroupoidMultiplicationForm, maybeParen[GroupoidElementSymbol|PathSymbol|IdentityElementForm]] // usingCustomKatex["gmult"];
declareInfixSymbol[ImplicitGroupMultiplicationForm, maybeParen[GroupElementSymbol]] // usingCustomKatex[" \\, "];

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

PathQuiverSymbol[] := PathQuiverSymbol["Q"];

declareBoxFormatting[
  HoldPattern[PathQuiverSymbol[q_]] :>
    makeTypedTemplateBox[q -> QuiverSymbol, "PathQuiverSymbolForm"]
];

$TemplateKatexFunction["PathQuiverSymbolForm"] = "pathQuiver";

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
    symbol[] :> TemplateBox[{}, symbolName],
    symbol[args__] :> TemplateBox[
      Prepend[TemplateBox[{}, symbolName]] @
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

PackageExport["GenericRewritingSystemSymbol"]
PackageExport["StringRewritingSystemSymbol"]
PackageExport["TuringMachineRewritingSystemSymbol"]
PackageExport["GraphRewritingSystemSymbol"]
PackageExport["HypergraphRewritingSystemSymbol"]
PackageExport["CellularAutomatonRewritingSystemSymbol"]
PackageExport["PetriNetRewritingSystemSymbol"]

declareNamedRewritingSystem[GenericRewritingSystemSymbol];
declareNamedRewritingSystem[StringRewritingSystemSymbol];
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
    makeHintedTemplateBox[rs -> RewritingSystemSymbol, init, d -> SymbolForm, "MultiwayGraphForm"]
];

$TemplateKatexFunction["MultiwayGraphForm"] = applyRiffled["multiwayBFS", ","];

(********************************************)

PackageExport["RewriteForm"]

declareBinaryForm[RewriteForm]

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

declareNamedCardinalValuationSymbol[symbol_] := With[
  {symbolName = SymbolName[symbol]},
  {formName = symbolName <> "Form"},
  declareBoxFormatting[
    symbol[dim_] :> makeHintedTemplateBox[dim -> rawSymbolBoxes, formName],
    q_symbol[cards__] :> MakeBoxes @ CardinalBindingForm[q, cards]
  ];
  $TemplateKatexFunction[formName] = LowerCaseFirst @ StringTrim[symbolName, "Symbol"];
]

(********************************************)

PackageExport["TranslationCardinalValuationSymbol"]
PackageExport["StarTranslationCardinalValuationSymbol"]

declareNamedCardinalValuationSymbol[TranslationCardinalValuationSymbol];
declareNamedCardinalValuationSymbol[StarTranslationCardinalValuationSymbol];

(********************************************)

declareNamedQuiverSymbol[symbol_] := With[
  {symbolName = SymbolName[symbol]},
  {katexName = LowerCaseFirst @ StringTrim[symbolName, "Symbol"]},
  AppendTo[$literalSymbolsP, symbol];
  declareBoxFormatting[
    symbol :> TemplateBox[{}, symbolName],
    symbol[] :> MakeBoxes @ symbol @ Infinity,
    symbol[size_] :> MakeBoxes @ SubSizeBindingForm[symbol, size],
    symbol[size__] | symbol[TupleForm[size__]] :> MakeBoxes @ SizeBindingForm[symbol, size],
    q_symbol[cards__] :> MakeBoxes @ CardinalBindingForm[q, cards]
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
    symbol[k_, size__] | symbol[k_, TupleForm[size__]] :> MakeBoxes @ SizeBindingForm[symbol[k], size],
    q_symbol[cards__] :> MakeBoxes @ CardinalBindingForm[q, cards]
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

PackageExport["CoversForm"]
PackageExport["StrictlyCoversForm"]

declareInfixSymbol[{CoversForm, StrictlyCoversForm}, GraphSymbol];

(********************************************)

PackageExport["IndexedCoveringForm"]

declareBoxFormatting[
  IndexedCoveringForm[pi_, g_, h_] :>
    makeHintedTemplateBox[pi -> GraphHomomorphismSymbol, g -> GraphSymbol, h -> GraphSymbol, "IndexedCoveringForm"]
];

$TemplateKatexFunction["IndexedCoveringForm"] = "covering";


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
PackageExport["TailVertexSymbol"]
PackageExport["HeadVertexSymbol"]

declareSymbolForm[VertexSymbol];
declareSymbolForm[TailVertexSymbol];
declareSymbolForm[HeadVertexSymbol];

declareBoxFormatting[
  VertexSymbol[e:(_ChartSymbol | _AppliedForm | _TailVertexSymbol | _HeadVertexSymbol)] :> MakeBoxes @ e
];

$TemplateKatexFunction["HeadVertexSymbolForm"] = "hvert";
$TemplateKatexFunction["TailVertexSymbolForm"] = "tvert";
$TemplateKatexFunction["VertexSymbolForm"] = "vert";

(**************************************************************************************************)

PackageExport["CardinalSymbol"]

declareSymbolForm[CardinalSymbol];

$TemplateKatexFunction["CardinalSymbolForm"] = "card";
$TemplateKatexFunction["NegatedCardinalSymbolForm"] = "ncard";
$TemplateKatexFunction["MirrorCardinalSymbolForm"] = "mcard";
$TemplateKatexFunction["NegatedMirrorCardinalSymbolForm"] = "nmcard";

(**************************************************************************************************)

PackageExport["MirrorForm"]

declareUnaryWrapperForm[MirrorForm]

declareBoxFormatting[
  m:MirrorForm[_CardinalSymbol | _NegatedForm] :> cardinalBox @ m
]

(**************************************************************************************************)

PackageExport["ParenthesesLabeledForm"]

declareBoxFormatting[
  ParenthesesLabeledForm[a_, l_] :> makeTemplateBox[a, l, "ParenthesesLabeledForm"]
];

$TemplateKatexFunction["ParenthesesLabeledForm"] = "parenLabeled";

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
    toLongEdgeForm @ makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, c -> CardinalSymbol, "TaggedDirectedEdgeForm"]
]

longTagQ[e_] := Count[e, "CardinalSymbolForm", {0, Infinity}] > 1;
longTagQ[TemplateBox[_, "CardinalProductForm"]] := True;

toLongEdgeForm[TemplateBox[{a_, b_, tag_}, form_String]] /; longTagQ[tag] :=
  TemplateBox[{a, b, tag}, "Long" <> form];
toLongEdgeForm[other_] := other;

$TemplateKatexFunction["DirectedEdgeForm"] = "de";
$TemplateKatexFunction["TaggedDirectedEdgeForm"] = "tde";
$TemplateKatexFunction["LongTaggedDirectedEdgeForm"] = "ltde";

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
  TemplateBox[MapUnevaluated[maybeParen[CardinalSymbol|NegatedForm|Negated], args], form];

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
$TemplateKatexFunction["LongTaggedUndirectedEdgeForm"] = "ltue";

(**************************************************************************************************)

PackageExport["QuiverProductAppliedForm"]

declareBoxFormatting[
  QuiverProductAppliedForm[poly_, graphs__] :>
    makeTemplateBox[poly, graphs, "QuiverProductAppliedForm"]
]

$TemplateKatexFunction["QuiverProductAppliedForm"] = quiverProductAppliedKatex;

quiverProductAppliedKatex[f_, args___] := {"\\frac{", f, "} {", Riffle[{args}, ","], "}"};

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

PackageExport["AppliedRelationForm"]

declareBoxFormatting[
  AppliedRelationForm[a_, b_, c_] :> makeTemplateBox[a, b, c, "AppliedRelationForm"]
];

$TemplateKatexFunction["AppliedRelationForm"] = applyRiffled["appliedRelation", " "];

(**************************************************************************************************)

PackageExport["LatticeTopSymbol"]
PackageExport["LatticeBottomSymbol"]

declareConstantSymbol[{LatticeTopSymbol, LatticeBottomSymbol}];

(**************************************************************************************************)

PackageExport["LatticeElementSymbol"]

declareSymbolForm[LatticeElementSymbol];

(**************************************************************************************************)

PackageExport["LatticeMeetForm"]
PackageExport["LatticeJoinForm"]

declareInfixSymbol[{LatticeMeetForm, LatticeJoinForm}, LatticeElementSymbol];

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
PackageExport["IndependentQuiverProductForm"]

declareInfixSymbol[
  {GraphUnionForm, GraphProductForm, DependentQuiverProductForm, IndependentQuiverProductForm},
  QuiverSymbol
];

(**************************************************************************************************)

PackageExport["VertexProductForm"]
PackageExport["EdgeProductForm"]

declareInfixSymbol[VertexProductForm, VertexSymbol];
declareInfixSymbol[EdgeProductForm, EdgeSymbol];

(**************************************************************************************************)

PackageExport["CardinalProductForm"]
PackageExport["CardinalSequenceForm"]

declareInfixSymbol[{CardinalProductForm, CardinalSequenceForm}, CardinalSymbol];

(**************************************************************************************************)

PackageExport["ArrowheadSymbol"]

declareConstantSymbol[ArrowheadSymbol];

(**************************************************************************************************)

PackageExport["ForwardFactorSymbol"]
PackageExport["BackwardFactorSymbol"]
PackageExport["NeutralFactorSymbol"]

declareConstantSymbol[{ForwardFactorSymbol, BackwardFactorSymbol, NeutralFactorSymbol}];

(**************************************************************************************************)

PackageExport["SetCardinalityForm"]

declareUnaryForm[SetCardinalityForm];

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
  declareBoxFormatting[
    form[a_, b_] :>
      MakeBoxes @ form[a, b, Null],
    form[a_, b_, c_] :>
      makeTemplateBox[a, b, c, formName],
    form[a_, b_, c_, d_] :>
      makeTemplateBox[SuchThatForm[a, d], b, c, formName]
  ];
  $TemplateKatexFunction[formName] = katexName;
];

(**************************************************************************************************)

PackageExport["IndexedMaxForm"]
PackageExport["IndexedMinForm"]

declareSumLikeFormatting[IndexedMaxForm, "indexMax"];
declareSumLikeFormatting[IndexedMinForm, "indexMin"];

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

declareInfixSymbol[{SetUnionForm, SetIntersectionForm}];

(**************************************************************************************************)

PackageExport["PowerSetForm"]

declareUnaryForm[PowerSetForm];

(**************************************************************************************************)

PackageExport["DivideForm"]
PackageExport["InlineDivideForm"]

declareBinaryForm[DivideForm] // usingCustomKatex["frac"];
declareInfixSymbol[InlineDivideForm] // usingCustomKatex[" / "];

(**************************************************************************************************)

PackageExport["PowerForm"]
PackageExport["RepeatedPowerForm"]

declareBinaryForm[PowerForm];
declareBinaryForm[RepeatedPowerForm];

(**************************************************************************************************)

PackageExport["PathRelationForm"]

declareBoxFormatting[
  PathRelationForm[args__] :>
    TemplateBox[MapUnevaluated[wordBoxes, {args}], "PathRelationForm"],
  PathRelationForm[] :>
    TemplateBox[{}, "PathRelationSymbol"]
]

$TemplateKatexFunction["PathRelationForm"] = katexAliasRiffled["pathIso"]
$TemplateKatexFunction["PathRelationSymbol"] = katexAlias["pathIso"];

(**************************************************************************************************)

PackageExport["ApproxEqualForm"]
PackageExport["IsomorphicForm"]
PackageExport["HomotopicForm"]
PackageExport["DefEqualForm"]

declareInfixSymbol[{ApproxEqualForm, IsomorphicForm, HomotopicForm, DefEqualForm}];

(**************************************************************************************************)

PackageExport["EqualForm"]
PackageExport["NotEqualForm"]

declareInfixSymbol[EqualForm] // usingCustomKatex[" = "];
declareInfixSymbol[NotEqualForm] // usingCustomKatex[" \\neq "];

(**************************************************************************************************)

PackageExport["SetConstructorForm"]

declareBoxFormatting[
  SetConstructorForm[lhs_, rhs_] :>
    TemplateBox[{toColGrid @ lhs, toColGrid @ rhs}, "SetConstructorForm"]
];

SetHoldAllComplete[toColGrid]

toColGrid = Case[
  {a__}  := MakeBoxes @ ColumnGridForm[a];
  a_     := makeQGBoxesOrNull @ a
];

$TemplateKatexFunction["SetConstructorForm"] = "setConstructor"

(**************************************************************************************************)

PackageExport["ConstructorForm"]

declareBoxFormatting[
  ConstructorForm[lhs_, rhs_] :>
    TemplateBox[{toColGrid @ lhs, toColGrid @ rhs}, "ConstructorForm"]
];

$TemplateKatexFunction["ConstructorForm"] = "constructor"

(**************************************************************************************************)

PackageExport["ColumnGridForm"]

declareBoxFormatting[

  ColumnGridForm[args__, Alignment -> alignment_] :>
    TemplateBox[
      List @ GridBox[
        Map[List, MapUnevaluated[makeQGBoxesOrNull, {args}]],
        GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}},
        GridBoxAlignment -> {"Columns" -> {ToList @ alignment}}
      ],
      "ColumnGridForm"
    ],

  ColumnGridForm[args__] :>
    TemplateBox[
      List @ GridBox[
        Map[List, MapUnevaluated[makeQGBoxesOrNull, {args}]],
        GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}}
      ],
      "ColumnGridForm"
    ]
]

$TemplateKatexFunction["ColumnGridForm"] = katexColumnGrid;

katexColumnGrid[GridBox[entries_, ___, GridBoxAlignment -> {"Columns" -> {{alignment_}}}]] :=
  {"\\begin{matrix*}[", toAlignmentLetter @ alignment, "]\n", Riffle[entries, "\\\\"], "\\end{matrix*}\n"}

katexColumnGrid[GridBox[entries_, ___]] :=
  {"\\begin{aligned}\n", Riffle[entries, "\\\\"], "\\end{aligned}\n"}

toAlignmentLetter = <|Left -> "l", Center -> "c", Right -> "r"|>;

(**************************************************************************************************)

PackageExport["Aligner"]

declareBoxFormatting[
  Aligner :> TemplateBox[{}, "Aligner"]
];

$TemplateKatexFunction["Aligner"] = "&"&;

(**************************************************************************************************)

PackageExport["EquationGridForm"]
PackageExport["Aligned"]
PackageExport["Divider"]

SetUsage @ "
Divider is used in EquationGridForm.
"

SetHoldAllComplete[makeQGBoxesOrNull, equationGridRow, riffledEqGridRow];

declareBoxFormatting[
  EquationGridForm[args__] :>
    TemplateBox[
      {GridBox @ padArray @ MapUnevaluated[equationGridRow, {args}]},
      "EquationGridForm"
    ]
]

equationGridRow = Case[
  Divider          := {"---"};
  e_List           := MapUnevaluated[makeQGBoxesOrNull, e];
  Aligned[e_]      := % @ List @ Aligned @ e;
  e_EqualForm      := riffledEqGridRow["=", e];
  e_DefEqualForm   := riffledEqGridRow[":=", e];
  e_Subset         := riffledEqGridRow["\[Subset]", e];
  e_SubsetEqual    := riffledEqGridRow["\[SubsetEqual]", e];
  e_ElementOfForm  := riffledEqGridRow["\[Element]", e];
  e_AndForm        := riffledEqGridRow["\[And]", e];
  e_OrForm         := riffledEqGridRow["\[Or]", e];
  e_ImpliesForm    := riffledEqGridRow["\[Implies]", e];
  e_EquivalentForm := riffledEqGridRow["\[Equivalent]", e];
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
  "="                   -> "&= ",
  "\[Element]"          -> "&\\in ",
  ":="                  -> "&\\defEqualSymbol ",
  "---"                 -> "\\hline",
  "\[Subset]"           -> "&\\subset ",
  "\[SubsetEqual]"      -> "&\\subseteq ",
  "where"               -> "&\\text{where} ",
  "\[TildeTilde]"       -> "&\[TildeTilde] ",
  "\[Implies]"          -> "&\[Implies] ",
  "\[And]"              -> "&\[And] ",
  "\[Or]"               -> "&\[Or] "
}

$equationSplitP = Alternatives @@ Values[$equationSymbolRules];

katexEquationGrid[GridBox[rows_, ___]] := Scope[
  rows = Replace[katexEquationGridRow /@ rows, $equationSymbolRules, {2}];
  rows = Replace[rows, {"\\hline", h___} :> {"\\hline \\\\[-2ex]"}, {1}];
  numCols = Max[Count[#, $equationSplitP]& /@ rows];
  {"\\begin{aligned}\n", rows, "\\end{aligned}\n"}
]

katexEquationGridRow[row_List] := Append[Riffle[row, " "], "\\\\\n"];

(**************************************************************************************************)

PackageExport["BinaryRelationForm"]

declareBoxFormatting[
  BinaryRelationForm[relation_String][args__] :>
    makeTypedTemplateBox[relation -> None, args, "BinaryRelationForm"]
]

$TemplateKatexFunction["BinaryRelationForm"] = Function[riffled[#1][##2]];

(**************************************************************************************************)

PackageExport["ImplicitTimesForm"]

declareInfixSymbol[ImplicitTimesForm] // usingCustomKatex[" \\, "];

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

innerPolyBoxes[args___] :=
  TemplateBox[MapUnevaluated[polyTermForm, {args}], $polyPlusForm];
    
polyTermForm = Case[
  a_Times                     := Construct[%, Apply[List, a]];
  Power[a_, b_]               := TemplateBox[{% @ a, makeQGBoxes @ b}, $polyPowerForm];
  (Negated|NegatedForm)[n_]   := TemplateBox[List @ % @ n, "NegatedForm"];
  a_ ? longPolyQ              := TemplateBox[List @ Apply[innerPolyBoxes, Unevaluated @ a], "ParenthesesForm"];
  a_List                      := TemplateBox[MapUnevaluated[makeInnerPolyParamQGBoxes, a], $polyTimesForm];
  a_                          := $scalarBoxes @ a;
];

makeInnerPolyParamQGBoxes = Case[
  a_List | a_Times            := TemplateBox[{polyTermForm @ a}, "ParenthesesForm"];
  Power[a_, b_]               := TemplateBox[{% @ a, makeQGBoxes @ b}, $polyPowerForm];
  a_ ? longPolyQ              := TemplateBox[List @ Apply[innerPolyBoxes, Unevaluated @ a], "ParenthesesForm"];
  (Negated|NegatedForm)[n_]   := TemplateBox[List @ % @ n, "NegatedForm"];
  a_                          := $scalarBoxes @ a;
];

longPolyQ[e_PolyForm] := Length[Unevaluated @ e] > 1;
longPolyQ[e_] := Head[Unevaluated @ e] === $polyHead && Length[Unevaluated @ e] > 1;

(**************************************************************************************************)

PackageExport["InverseForm"]

declareUnaryForm[InverseForm];

(**************************************************************************************************)

PackageExport["BoldForm"]
PackageExport["RedForm"]
PackageExport["GreenForm"]
PackageExport["BlueForm"]
PackageExport["RedBlueForm"]
PackageExport["GreenBlueForm"]
PackageExport["RedGreenForm"]
PackageExport["DarkGrayForm"]
PackageExport["MediumGrayForm"]
PackageExport["LightGrayForm"]

declareUnaryWrapperForm[BoldForm, "boldForm"]
declareUnaryWrapperForm[RedForm, "rform"];
declareUnaryWrapperForm[GreenForm, "gform"];
declareUnaryWrapperForm[BlueForm, "bform"];
declareUnaryWrapperForm[RedBlueForm, "rbform"];
declareUnaryWrapperForm[RedGreenForm, "rgform"];
declareUnaryWrapperForm[GreenBlueForm, "gbform"];
declareUnaryWrapperForm[DarkGrayForm, "waform"];
declareUnaryWrapperForm[MediumGrayForm, "wbform"];
declareUnaryWrapperForm[LightGrayForm, "wcform"];

(**************************************************************************************************)

PackageExport["BarTokenSymbol"]
PackageExport["FilledTokenSymbol"]
PackageExport["FilledRectangleTokenSymbol"]
PackageExport["EmptyTokenSymbol"]
PackageExport["EmptyRectangleTokenSymbol"]

declareConstantSymbol[{BarTokenSymbol, FilledTokenSymbol, FilledRectangleTokenSymbol, EmptyTokenSymbol, EmptyRectangleTokenSymbol}];

(**************************************************************************************************)

PackageExport["NegatedBoxForm"]

SetHoldFirst[NegatedBoxForm];
NegatedBoxForm[e_] := makeTemplateBox[e, "NegatedForm"]

$TemplateKatexFunction["NegatedForm"] = "negated";

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
  "Forward"         := TemplateBox[{}, "ForwardSymbol"];
  "Backward"        := TemplateBox[{}, "BackwardSymbol"];
  "Symmetric"       := TemplateBox[{}, "SymmetricSymbol"];
  "Antisymmetric"   := TemplateBox[{}, "AntisymmetricSymbol"];
];

$TemplateKatexFunction["WordVectorForm"] = "wordVector";
$TemplateKatexFunction["ForwardSymbol"] = katexAlias["forwardSymbol"];
$TemplateKatexFunction["BackwardSymbol"] = katexAlias["backwardSymbol"];
$TemplateKatexFunction["SymmetricSymbol"] = katexAlias["symmetricSymbol"];
$TemplateKatexFunction["AntisymmetricSymbol"] = katexAlias["antisymmetricSymbol"];

(********************************************)

makePathBoxTemplate[left_, rest___, tag_] :=
  TemplateBox[
    Join[
      List @ maybeParen[PathSymbol|CardinalSymbol|$colorFormP|$functionFormP|EdgeFieldSymbol|PathVectorSymbol] @ left,
      MapUnevaluated[
        maybeParen[PathSymbol|CardinalSymbol|EdgeFieldSymbol|VertexFieldSymbol|PathVectorSymbol|PathTranslateForm|PathBackwardTranslateForm|$colorFormP],
        {rest}
      ]
    ],
    tag
  ];

makePathBoxTemplate[left_, tag_] :=
  TemplateBox[
    List @ maybeParen[PathSymbol|EdgeFieldSymbol|VertexFieldSymbol|PathVectorSymbol|PathTranslateForm|PathBackwardTranslateForm|$colorFormP] @ left,
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
    TemplateBox[{}, "PathComposeSymbol"]
];

$TemplateKatexFunction["PathComposeForm"] = "pathCompose";
$TemplateKatexFunction["PathComposeSymbol"] = "pathComposeSymbol";

(********************************************)

PackageExport["PathIntegralForm"]

declareBoxFormatting[
  PathIntegralForm[a_, b_] :>
    makePathBoxTemplate[a, b, "PathIntegralForm"],

  PathIntegralForm[] :>
    TemplateBox[{}, "PathIntegralSymbol"]
];

$TemplateKatexFunction["PathIntegralForm"] = "pathIntegral";
$TemplateKatexFunction["PathIntegralSymbol"] = "pathIntegralSymbol";

(********************************************)

PackageExport["PathDotForm"]

declareBoxFormatting[
  PathDotForm[a_, b_] :>
    makePathBoxTemplate[a, b, "PathDotForm"],

  PathDotForm[] :>
    TemplateBox[{}, "PathDotSymbol"]
];

$TemplateKatexFunction["PathDotForm"] = "pathDot";
$TemplateKatexFunction["PathDotSymbol"] = "pathDotSymbol";

(********************************************)

PackageExport["PathTranslateForm"]
PackageExport["PathLeftTranslateForm"]

declareBoxFormatting[
  PathTranslateForm[] :>
    TemplateBox[{}, "PathTranslateSymbol"],
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
    TemplateBox[{}, "PathBackwardTranslateSymbol"],
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
    TemplateBox[{}, "PathReverseSymbol"]
];

$TemplateKatexFunction["PathReverseForm"] = "pathReverse";
$TemplateKatexFunction["PathReverseSymbol"] = "pathReverse";

(********************************************)

PackageExport["GradientForm"]

declareBoxFormatting[
  GradientForm[q_] :> makeHintedTemplateBox[q -> VertexFieldSymbol, "GradientForm"],
  GradientForm[] :> TemplateBox[{}, "GradientSymbol"]
];

$TemplateKatexFunction["GradientForm"] = "gradOf";
$TemplateKatexFunction["GradientSymbol"] = katexAlias["grad"];

(********************************************)

PackageExport["DivergenceForm"]

declareBoxFormatting[
  DivergenceForm[q_] :> makeHintedTemplateBox[q -> EdgeFieldSymbol, "DivergenceForm"],
  DivergenceForm[] :> TemplateBox[{}, "DivergenceSymbol"]
];

$TemplateKatexFunction["DivergenceForm"] = "divOf";
$TemplateKatexFunction["DivergenceSymbol"] = katexAlias["div"];

(********************************************)

PackageExport["LaplacianForm"]

declareBoxFormatting[
  LaplacianForm[q_] :> makeHintedTemplateBox[q -> EdgeFieldSymbol, "LaplacianForm"],
  LaplacianForm[] :> TemplateBox[{}, "LaplacianSymbol"]
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

declareBoxFormatting[
  CompactBasisSymbolForm[p_] :> makeTypedTemplateBox[p -> PathVectorSpaceSymbol, "CompactBasisSymbolForm"]
];

$TemplateKatexFunction["CompactBasisSymbolForm"] = "compactBasis";

(********************************************)

PackageExport["PathForwardDifferenceForm"]

PathForwardDifferenceForm[w_String] :=
  PathForwardDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathForwardDifferenceForm[w__] :>
    TemplateBox[Map[pathOrWordBoxes, {w}], "PathForwardDifferenceForm"],
  PathForwardDifferenceForm[] :>
    TemplateBox[{}, "PathForwardDifferenceSymbol"],
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
    TemplateBox[{}, "PathBackwardDifferenceSymbol"],
  (op_PathBackwardDifferenceForm)[arg_] :>
    MakeBoxes @ OperatorAppliedForm[op, arg]
];

$TemplateKatexFunction["PathBackwardDifferenceForm"] = applyRiffled["pathBackwardDifference",","];
$TemplateKatexFunction["PathBackwardDifferenceSymbol"] = "backwardDifference";

(********************************************)

PackageExport["PathCentralDifferenceForm"]

PathCentralDifferenceForm[w_String] :=
  PathCentralDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathCentralDifferenceForm[w__] :>
    TemplateBox[Map[pathOrWordBoxes, {w}], "PathCentralDifferenceForm"],
  PathCentralDifferenceForm[] :>
    TemplateBox[{}, "PathCentralDifferenceSymbol"],
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

(********************************************)

PackageExport["ListForm"]

declareCommaRiffledForm[ListForm, "list"];

(********************************************)

PackageExport["TupleForm"]

declareCommaRiffledForm[TupleForm, "tuple"];

(********************************************)

PackageExport["PrimedForm"]

declareUnaryWrapperForm[PrimedForm];

(********************************************)

PackageExport["ParenthesesForm"]

declareCommaRiffledForm[ParenthesesForm, "paren"];

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

declareBoxFormatting[
  ConcatenationForm[args__] :>
    makeTemplateBox[args, "ConcatenationForm"]
]

$TemplateKatexFunction["ConcatenationForm"] = applyRiffled["concat", " "];

(********************************************)

PackageExport["CommaRowForm"]

declareBoxFormatting[
  CommaRowForm[args__] :>
    makeTemplateBox[args, "CommaRowForm"]
]

$TemplateKatexFunction["CommaRowForm"] = riffled[","];

(********************************************)

PackageExport["SpacedRowForm"]

declareBoxFormatting[
  SpacedRowForm[args__] :>
    makeTemplateBox[args, "SpacedRowForm"]
]

$TemplateKatexFunction["SpacedRowForm"] = katexAliasRiffled["quad"];

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

PackageExport["MapsToForm"]

declareBinaryForm[MapsToForm] // usingCustomKatex["mto"];

(**************************************************************************************************)

PackageExport["EllipsisSymbol"]

declareConstantSymbol[EllipsisSymbol];

(**************************************************************************************************)

PackageExport["PartialDifferentialOfForm"]

declareBoxFormatting[
  PartialDifferentialOfForm[x_] :>
    makeTemplateBox[x, "PartialDifferentialOfForm"],
  PartialDifferentialOfForm[] :>
    TemplateBox[{}, "PartialDifferentialSymbol"]
];

$TemplateKatexFunction["PartialDifferentialOfForm"] = "partialdof";
$TemplateKatexFunction["PartialDifferentialSymbol"] = katexAlias["partial"];

(**************************************************************************************************)

PackageExport["AndFunction"]
PackageExport["OrFunction"]
PackageExport["NotFunction"]

PackageExport["VertexListFunction"]
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
PackageExport["BasisFunction"]
PackageExport["SupportFunction"]
PackageExport["GradeFunction"]

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

declareBoxFormatting[
  RowForm[args__] :>
    makeTemplateBox[args, "RowForm"],
  TermRowForm[args__] :>
    makeTemplateBox[args, "TermRowForm"]
];

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
    TemplateBox[{}, "ChartSymbol"]
];

$TemplateKatexFunction["ChartSymbolForm"] = "chart"
$TemplateKatexFunction["ChartSymbol"] = katexAlias["chartSymbol"];

(********************************************)

PackageExport["SignedForm"]

declareUnaryWrapperForm[SignedForm]

(********************************************)

PackageExport["TransportMapSymbol"]

declareBoxFormatting[
  TransportMapSymbol[p_] :> makeTypedTemplateBox[p -> PathSymbol, "TransportMapSymbolForm"],
  TransportMapSymbol[] :> TemplateBox[{}, "TransportMapSymbol"]
];

$TemplateKatexFunction["TransportMapSymbolForm"] = "transportMap"
$TemplateKatexFunction["TransportMapSymbol"] = "transportMapSymbol"

(********************************************)

PackageExport["CardinalGroupSymbolForm"]

declareBoxFormatting[
  CardinalGroupSymbolForm[q_] :> makeTypedTemplateBox[q -> SymbolForm, "CardinalGroupSymbolForm"]
];

$TemplateKatexFunction["CardinalGroupSymbolForm"] = "cardinalGroup"

(********************************************)

PackageExport["TransportAtlasSymbolForm"]

declareBoxFormatting[
  TransportAtlasSymbolForm[q_] :> makeTypedTemplateBox[q -> QuiverSymbol, "TransportAtlasSymbolForm"],
  TransportAtlasSymbolForm[] :> TemplateBox[{}, "TransportAtlasSymbol"]
];

$TemplateKatexFunction["TransportAtlasSymbolForm"] = "transportAtlas"
$TemplateKatexFunction["TransportAtlasSymbol"] = "\\transportAtlas{}"&

(********************************************)

PackageExport["GraphRegionIntersectionForm"]
PackageExport["GraphRegionUnionForm"]

declareInfixSymbol[{GraphRegionIntersectionForm, GraphRegionUnionForm}]

(********************************************)

PackageExport["IdentityElementForm"]

declareBoxFormatting[
  IdentityElementForm[args___] :>
    makeTemplateBox[args, "IdentityElementForm"]
];

$TemplateKatexFunction["IdentityElementForm"] = "idElem";

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

PackageExport["SubSizeBindingForm"]

declareBindingForm[SubSizeBindingForm, "subSize", size |-> MakeBoxes[QuiverSizeSymbol[size]]];

(********************************************)

PackageExport["SizeBindingForm"]

declareBindingForm[SizeBindingForm, "bindSize", size |-> MakeBoxes[QuiverSizeSymbol[size]]];

(********************************************)

PackageExport["CardinalBindingForm"]

declareBindingForm[CardinalBindingForm, "bindCards", cardinalBox];

(********************************************)

PackageExport["ParenPathWordForm"]

ParenPathWordForm[args__] := ParenthesesForm @ PathWordForm @ args;

(********************************************)

PackageExport["ParenEmptyPathWordForm"]

ParenEmptyPathWordForm[v_] := ParenthesesForm @ EmptyPathWordForm @ v;

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

PackageExport["EllipsisSequenceForm"]

EllipsisSequenceForm[f_] := EllipsisSequenceForm[f, SymbolForm["n"]];

EllipsisSequenceForm[f_, n_] := Sequence[f[1], f[2], EllipsisSymbol, f @ n];

EllipsisSequenceForm[f_, n_, 1] := Sequence[f[1], EllipsisSymbol, f @ n];

(********************************************)

PackageExport["ReverseEllipsisSequenceForm"]

ReverseEllipsisSequenceForm[f_] := ReverseEllipsisSequenceForm[f, SymbolForm["n"]];

ReverseEllipsisSequenceForm[f_, n_] := Sequence[f @ n, EllipsisSymbol, f[2], f[1]];

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

$cardP = _CardinalSymbol | NegatedForm[_CardinalSymbol] | MirrorForm[_CardinalSymbol] ;
$maybeColoredCardP = $colorFormP[$cardP] | $cardP;

SetHoldAllComplete[wordBoxes, cardinalBox];
wordBoxes = Case[
  {}|""                               := TemplateBox[{}, "EmptyWordForm"];
  1                                   := TemplateBox[{"1"}, "WordForm"];
  word_String                         := Construct[%, ToPathWord @ word];
  (Times|ConcatenationForm)[args__]   := TemplateBox[MapUnevaluated[%, {args}], "ConcatenationForm"];
  RepeatedPowerForm[a_, b_]           := TemplateBox[{% @ a, makeQGBoxes @ b}, "RepeatedPowerForm"];
  c:cardP                             := TemplateBox[List @ MakeBoxes @ c, "WordForm"];
  list:{cardP..}                      := TemplateBox[MapUnevaluated[MakeBoxes, list], "WordForm"];
  list_List                           := TemplateBox[tryColorCardinals @ cardinalBoxes @ list, "WordForm"];
  (Negated|NegatedForm)[e_]           := TemplateBox[List @ wordBoxes @ e, "NegatedForm"];
  MirrorForm[e_]                      := TemplateBox[List @ wordBoxes @ e, "MirrorForm"];
  cs_CardinalSequenceForm             := MakeBoxes @ cs;
  sc_SerialCardinal                   := MakeBoxes @ sc;
  pc_ParallelCardinal                 := MakeBoxes @ pc;
  w_WordForm                          := MakeBoxes @ w;
  w_WordSymbol                        := MakeBoxes @ w;
  w_SymbolForm                        := MakeBoxes @ s; (* placeholder *)
  s:symsP                             := TemplateBox[List @ rawSymbolBoxes @ s, "WordSymbolForm"],
  {symsP -> $rawSymbolP, cardP -> $maybeColoredCardP}
];

cardinalBoxes[list_List] := Map[cardinalBox, list];

cardSymBox[e_] := TemplateBox[{e}, "CardinalSymbolForm"];

toNegCard[TemplateBox[{e_}, "CardinalSymbolForm"]] :=
  TemplateBox[{e}, "NegatedCardinalSymbolForm"];

toNegCard[TemplateBox[{e_}, "MirrorCardinalSymbolForm"]] :=
  TemplateBox[{e}, "NegatedMirrorCardinalSymbolForm"];

toMirrorCard[TemplateBox[{e_}, "CardinalSymbolForm"]] :=
  TemplateBox[{e}, "MirrorCardinalSymbolForm"];

toMirrorCard[TemplateBox[{e_}, "NegatedCardinalSymbolForm"]] :=
  TemplateBox[{e}, "MirrorNegatedCardinalSymbolForm"];

cardinalBox = Case[
  c_CardinalSymbol                    := MakeBoxes @ c;
  c:NegatedForm[_CardinalSymbol]      := MakeBoxes @ c;
  s_String                            := cardSymBox @ s;
  (col:colsP)[e_]                     := TemplateBox[List @ MakeBoxes @ e, SymbolName @ col];
  i_Integer                           := cardSymBox @ TextString @ i;
  s:symsP                             := cardSymBox @ rawSymbolBoxes @ s;
  cs_CardinalSequenceForm             := MakeBoxes @ cs;
  sc_SerialCardinal                   := MakeBoxes @ sc;
  pc_ParallelCardinal                 := MakeBoxes @ pc;
  MirrorForm[s_]                      := toMirrorCard @ % @ s;
  Negated[s_]                         := toNegCard @ % @ s,
  {symsP -> $rawSymbolP, colsP -> $colorFormP}
]

(* only colors if it is all-or-nothing *)
tryColorCardinals[list_] /; $AutoColorCardinals :=
  If[SubsetQ[{"r", "g", "b"}, list /. TemplateBox[{a_}, _] :> a],
    list /. $colorCardinalRules,
    list
  ];

tryColorCardinals[list_] := list;

$cardFormP = "CardinalSymbolForm" | "NegatedCardinalSymbolForm";

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

PackageExport["LiteralStringForm"]
PackageExport["StringSymbolForm"]

PackageExport["LiteralCharacterForm"]
PackageExport["CharacterSymbolForm"]

declareBoxFormatting[
  DoubleQuotedStringForm[args___] :>
    TemplateBox[MapUnevaluated[stringElementBoxes, {args}], "DoubleQuotedStringForm"],

  QuotedCharacterForm[a_] :>
    TemplateBox[List @ quotedCharBoxes @ a, "QuotedCharacterForm"],

  LiteralStringForm[str_String] :> TemplateBox[List @ str, "LiteralStringForm"],
  LiteralCharacterForm[str_String] :> TemplateBox[List @ str, "LiteralCharacterForm"]
];

$TemplateKatexFunction["DoubleQuotedStringForm"] = applyRiffled["qstring", " "];
$TemplateKatexFunction["QuotedCharacterForm"] = "qchar";

$TemplateKatexFunction["LiteralStringForm"] = "lstr";
$TemplateKatexFunction["LiteralCharacterForm"] = "lchar";

$TemplateKatexFunction["StringSymbolForm"] = "strsym";
$TemplateKatexFunction["CharacterSymbolForm"] = "charsym";

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

$domainsP = Alternatives[Integers, Reals, Rationals, Complexes, Naturals];

(* this is the general dispatch mechanism for a form of unknown type *)
makeQGBoxes = Case[
  None | Null               := "";
  l:lsymsP                  := MakeBoxes @ l;
  s:namedFnP                := MakeBoxes @ s;
  d:domainsP                := MakeBoxes @ d;
  e:symP                    := symbolBoxes @ e;
  e_Subtract                := algebraBoxes[e, "SubtractForm"];
  e_Times                   := algebraBoxes[e, "TimesForm"];
  e_Plus                    := algebraBoxes[e, "PlusForm"];
  Equal[a_, b_]             := MakeBoxes @ EqualForm[a, b];
  Unequal[a_, b_]           := MakeBoxes @ UnequalForm[a, b];
  (h:binHeads)[args__]      := With[{str = Lookup[$binaryRelationMapping, h]}, MakeBoxes @ BinaryRelationForm[str][args]];
  (i_InverseForm)[args__]   := MakeBoxes @ AppliedForm[i, args];
  Minus[e_]                 := makeTemplateBox[e, "MinusForm"];
  Power[e_, -1]             := makeTemplateBox[e, "InverseForm"];
  Times[-1, e_]             := makeTemplateBox[e, "MinusForm"];
  Negated[e_]               := makeTemplateBox[e, "NegatedForm"];
  DirectedEdge[args__]      := MakeBoxes @ DirectedEdgeForm[args];
  UndirectedEdge[args__]    := MakeBoxes @ UndirectedEdgeForm[args];
  Labeled[a_, l_]           := MakeBoxes @ ParenthesesLabeledForm[a, l];
  Text[t_]                  := MakeBoxes @ MathTextForm[t];
  Row[{r__}]                := MakeBoxes @ RowForm[r];
  Modulo[n_]                := MakeBoxes @ ModuloForm[n];
  other_                    := MakeBoxes @ other,
  {lsymsP -> $literalSymbolsP, symP -> $rawSymbolP, namedFnP -> Alternatives @@ $namedFunctions,
    binHeads -> $binaryRelationHeads, domainsP -> $domainsP}
];

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
  Subscript[a_, b_, c_]       := SubscriptBox[makeQGBoxes @ a, RowBox[{makeQGBoxes @ b, ",", makeQGBoxes @ c}]];
  Superscript[a_, b_]         := SuperscriptBox[makeQGBoxes @ a, makeQGBoxes @ b];
  Subsuperscript[a_, b_, c_]  := SubsuperscriptBox[% @ a, makeQGBoxes @ b, makeQGBoxes @ c]
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
  l:lsymsP                              := MakeBoxes @ l;
  s:symsP                               := With[{head = $ehc}, MakeBoxes @ head @ s];
  e:_InverseForm | _GroupInverseForm | _GroupoidInverseForm | _AppliedForm := MakeBoxes @ e;
  (e:(head_[___])) /; MatchQ[head, $eh] := MakeBoxes @ e;
  e:(colsP[_])                          := MakeBoxes @ e;
  other_                                := MakeBoxes @ ParenthesesForm @ other,
  {symsP -> $rawSymbolP, colsP -> $colorFormP, lsymsP -> $literalSymbolsP}
];
