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

$literalSymbolsP = Alternatives[
  EllipsisSymbol,
  FilledTokenSymbol, EmptyTokenSymbol,
  FilledRectangleTokenSymbol, EmptyRectangleTokenSymbol,
  BarTokenSymbol,
  ForwardFactorSymbol, BackwardFactorSymbol, NeutralFactorSymbol,
  ArrowheadSymbol, Aligner
];

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
  Subsuperscript[a_, b_, c_]  := SubsuperscriptBox[% @ a, makeQGBoxes @ b, makeQGBoxes @ c],
  {lsymsP -> $literalSymbolsP, colorP -> $colorFormP}
]

(* todo: support formal symbols, rewriting them as necessary *)

toSymbolName[e_] := SymbolName[Unevaluated @ e];

(********************************************)

PackageExport["SymbolForm"]

declareBoxFormatting[
  SymbolForm[p_] :>
    TemplateBox[List @ symbolBoxes @ p, "SymbolForm"]
];

$TemplateKatexFunction["SymbolForm"] = "sym";

(********************************************)

PackageExport["NotApplicableSymbol"]

declareBoxFormatting[
  NotApplicableSymbol :> TemplateBox[{}, "NotApplicableSymbol"]
]

$TemplateKatexFunction["NotApplicableSymbol"] = "\\wbform{\\text{---}}"&;

(********************************************)

PackageExport["UnknownSymbol"]

declareBoxFormatting[
  UnknownSymbol :> TemplateBox[{}, "UnknownSymbol"]
]

$TemplateKatexFunction["UnknownSymbol"] = "\\wbform{\\text{?}}"&;

(********************************************)

PackageExport["EmptySetSymbol"]

declareBoxFormatting[
  EmptySetSymbol :> TemplateBox[{}, "EmptySetSymbol"]
]

$TemplateKatexFunction["EmptySetSymbol"] = "\\empty"&;

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

SetHoldAllComplete[makeStandardBoxTemplate];
makeStandardBoxTemplate[args___, tag_] :=
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

SetHoldAllComplete[makeNaryHintedTemplateBox, makeHintedTemplateBox, toHintedSymbol];

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
  ElementOfForm[a__, b_] :> makeStandardBoxTemplate[CommaRowForm[a], b, "ElementOfForm"],
  ElementOfForm[a_, b_] :> makeStandardBoxTemplate[a, b, "ElementOfForm"],
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
  BasisPathVectorSymbol[p_, sub_] :> makeStandardBoxTemplate[p, sub, "BasisPathVectorSymbolForm"],
  BasisPathWeightSymbol[p_, sub_] :> makeStandardBoxTemplate[p, sub, "BasisPathWeightSymbolForm"]
];

$TemplateKatexFunction["BasisPathVectorSymbolForm"] = "basisPath";
$TemplateKatexFunction["BasisPathWeightSymbolForm"] = "basisPathWeight";

(**************************************************************************************************)

PackageExport["BaseFieldSymbol"]

BaseFieldSymbol[] := BaseFieldSymbol["K"];

declareBoxFormatting[
  BaseFieldSymbol[s_] :> makeStandardBoxTemplate[s, "BaseFieldSymbolForm"]
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
  FiniteFieldSymbol[n_] :> makeStandardBoxTemplate[n, "FiniteField"]
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
    makeStandardBoxTemplate[e, "SignedForm"]
]; *)

(**************************************************************************************************)

PackageExport["FunctionSymbol"]

$namedFunctions = {
  VertexListFunction,
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
  TransportMapSymbol
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
    makeStandardBoxTemplate[f, "FunctionSymbolForm"]
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
  OperatorAppliedForm[f_, g_] :> makeStandardBoxTemplate[f, g, "OperatorAppliedForm"]
];

$TemplateKatexFunction["OperatorAppliedForm"] = operatorAppliedKatex;

operatorAppliedKatex[f_, g_] := {f, "\,", g};

(**************************************************************************************************)

PackageExport["PathMapSymbol"]

PathMapSymbol[] := PathMapSymbol["\[Mu]"];

declareBoxFormatting[
  PathMapSymbol[mu_ ? isMuQ] :>
    makeStandardBoxTemplate["\[Mu]", "PathMapSymbolForm"],

  PathMapSymbol[mu_] :>
    makeStandardBoxTemplate[mu, "PathMapSymbolForm"]
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
  CyclicGroupForm[n_] :> makeStandardBoxTemplate[n, "CyclicGroupForm"]
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

$TemplateKatexFunction["GroupRelationForm"] = riffled[" \\groupRelationIso "]
$TemplateKatexFunction["GroupRelationSymbol"] = "\\groupRelationIso"&;

(**************************************************************************************************)

PackageExport["GroupCommutatorForm"]

declareBoxFormatting[
  GroupCommutatorForm[a_, b_] :> makeStandardBoxTemplate[a, b, "GroupCommutatorForm"]
]

$TemplateKatexFunction["GroupCommutatorForm"] = "groupCommutator";

(**************************************************************************************************)

PackageExport["GroupPowerForm"]

declareBoxFormatting[
  GroupPowerForm[g_, n_] :> makeStandardBoxTemplate[g, n, "GroupPowerForm"]
]

$TemplateKatexFunction["GroupPowerForm"] = "groupPow";


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

PackageExport["ContractionProductForm"]
PackageExport["ContractionSumForm"]

declareBoxFormatting[
  ContractionProductForm[args__] :> makeStandardBoxTemplate[args, "ContractionProductForm"],
  ContractionProductForm :> TemplateBox[{}, "ContractionTimesSymbol"],

  ContractionSumForm[args__] :> makeStandardBoxTemplate[args, "ContractionSumForm"],
  ContractionSumForm :> TemplateBox[{}, "ContractionPlusSymbol"]

]

$TemplateKatexFunction["ContractionProductForm"] = applyRiffled["contractProduct", "\\contractTimes "];
$TemplateKatexFunction["ContractionTimesSymbol"] = "\\contractTimes "&;

$TemplateKatexFunction["ContractionSumForm"] = applyRiffled["contractSum", "\\contractPlus "];
$TemplateKatexFunction["ContractionPlusSymbol"] = "\\contractPlus "&;

(**************************************************************************************************)

PackageExport["WordGroupSymbol"]

declareBoxFormatting[
  
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

declareBoxFormatting[
  GroupInverseForm[g_] :>
    TemplateBox[List @ maybeParen[GroupElementSymbol] @ g, "GroupInverseForm"],

  GroupoidInverseForm[g_] :>
    TemplateBox[List @ maybeParen[GroupoidElementSymbol] @ g, "GroupoidInverseForm"]
];

$TemplateKatexFunction["GroupInverseForm"] = "groupInv";
$TemplateKatexFunction["GroupoidInverseForm"] = "groupoidInv";

(**************************************************************************************************)

PackageExport["MatrixDotForm"]

declareBoxFormatting[
  MatrixDotForm[args__] :>
    TemplateBox[
      MapUnevaluated[maybeParen[MatrixSymbol|TranslationVectorForm], {args}],
      "MatrixDotForm"
    ],

  MatrixDotForm[] :>
    makeStandardBoxTemplate[args, "MatrixDotSymbol"]
];

$TemplateKatexFunction["MatrixDotForm"] = riffled[" \\cdot "];
$TemplateKatexFunction["MatrixDotSymbol"] = " \\cdot "&;


(**************************************************************************************************)

PackageExport["GroupMultiplicationForm"]

declareBoxFormatting[
  GroupMultiplicationForm[args__] :>
    TemplateBox[
      MapUnevaluated[maybeParen[GroupElementSymbol], {args}],
      "GroupMultiplicationForm"
    ],

  GroupMultiplicationForm[] :>
    makeStandardBoxTemplate[args, "GroupMultiplicationSymbol"]
];

$TemplateKatexFunction["GroupMultiplicationForm"] = riffled[" \\Gmult "];
$TemplateKatexFunction["GroupMultiplicationSymbol"] = " \\Gmult "&;

(**************************************************************************************************)

PackageExport["ImplicitGroupMultiplicationForm"]

declareBoxFormatting[
  ImplicitGroupMultiplicationForm[args__] :>
    TemplateBox[
      MapUnevaluated[maybeParen[GroupElementSymbol], {args}],
      "ImplicitGroupMultiplicationForm"
    ]
];

$TemplateKatexFunction["ImplicitGroupMultiplicationForm"] = riffled["\\,"];

(**************************************************************************************************)

PackageExport["GroupoidMultiplicationForm"]

declareBoxFormatting[
  GroupoidMultiplicationForm[args__] :>
    TemplateBox[
      MapUnevaluated[
        maybeParen[GroupoidElementSymbol|PathSymbol|IdentityElementForm],
        {args}
      ],
      "GroupoidMultiplicationForm"
    ],

  GroupoidMultiplicationForm[] :>
    makeStandardBoxTemplate[args, "GroupoidMultiplicationSymbol"]
];

$TemplateKatexFunction["GroupoidMultiplicationForm"] = riffled[" \\gmult "];
$TemplateKatexFunction["GroupoidMultiplicationSymbol"] = " \\gmult "&;

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
  e_                             := MakeBoxes @ QuiverSymbol @ e,
  {colorsP -> $colorFormP}
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

declareBoxFormatting[
  RewriteForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "RewriteForm"]
];

$TemplateKatexFunction["RewriteForm"] = "rewrite";

(********************************************)

PackageExport["RewritingRuleForm"]

declareBoxFormatting[
  RewritingRuleForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "RewritingRuleForm"]
];

$TemplateKatexFunction["RewritingRuleForm"] = "rewritingRule";

(********************************************)

PackageExport["QuiverSizeSymbol"]

declareBoxFormatting[
  QuiverSizeSymbol[n_Integer] :> MakeBoxes @ n,
  QuiverSizeSymbol[Infinity] :> "\[Infinity]",
  QuiverSizeSymbol[other_] :> rawSymbolBoxes @ other
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
  {formName = symbolName <> "Form"},
  declareBoxFormatting[
    symbol[] :> ToBoxes @ symbol[Infinity],
    symbol[size_] :> makeHintedTemplateBox[size -> QuiverSizeSymbol, formName],
    q_symbol[cards__] :> MakeBoxes @ CardinalBindingForm[q, cards]
  ];
  $TemplateKatexFunction[formName] = LowerCaseFirst @ StringTrim[symbolName, "Symbol"];
]

declareTwoParameterNamedQuiverSymbol[symbol_] := With[
  {symbolName = SymbolName[symbol]},
  {formName = symbolName <> "Form"},
  declareBoxFormatting[
    symbol[k_] :> ToBoxes @ symbol[k, Infinity],
    symbol[k_, size_] :> makeHintedTemplateBox[k, size -> QuiverSizeSymbol, formName],
    q_symbol[cards__] :> MakeBoxes @ CardinalBindingForm[q, cards]
  ];
  $TemplateKatexFunction[formName] = LowerCaseFirst @ StringTrim[symbolName, "Symbol"];
]

(********************************************)

PackageExport["BouquetQuiverSymbol"]
PackageExport["GridQuiverSymbol"]
PackageExport["TreeQuiverSymbol"]

declareNamedQuiverSymbol[BouquetQuiverSymbol];
declareTwoParameterNamedQuiverSymbol[GridQuiverSymbol];
declareTwoParameterNamedQuiverSymbol[TreeQuiverSymbol];

PackageExport["LineQuiverSymbol"]
PackageExport["CycleQuiverSymbol"]
PackageExport["SquareQuiverSymbol"]
PackageExport["CubicQuiverSymbol"]
PackageExport["TriangularQuiverSymbol"]
PackageExport["HexagonalQuiverSymbol"]

declareNamedQuiverSymbol[LineQuiverSymbol];
declareNamedQuiverSymbol[CycleQuiverSymbol];
declareNamedQuiverSymbol[SquareQuiverSymbol];
declareNamedQuiverSymbol[TriangularQuiverSymbol];
declareNamedQuiverSymbol[HexagonalQuiverSymbol];
declareNamedQuiverSymbol[CubicQuiverSymbol];

(********************************************)

PackageExport["CoversForm"]

declareBoxFormatting[
  CoversForm[g_, h_] :>
    makeHintedTemplateBox[g -> GraphSymbol, h -> GraphSymbol, "CoversForm"],
  CoversForm[g_, h_, i_] :>
    makeHintedTemplateBox[g -> GraphSymbol, h -> GraphSymbol, i -> GraphSymbol, "CoversForm"],
  CoversForm[] :>
    TemplateBox[{}, "CoversSymbol"]
]

$TemplateKatexFunction["CoversForm"] = riffled["\\covers"];
$TemplateKatexFunction["CoversSymbol"] = "\\covers";

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

declareBoxFormatting[
  NullPath :> TemplateBox[{}, "NullPathSymbol"],
  NullElement :> TemplateBox[{}, "NullPathSymbol"]
];

$TemplateKatexFunction["NullPathSymbol"] = "nullpath";

(********************************************)

PackageExport["PathSymbol"]

PathSymbol[] := PathSymbol["P"];

declareBoxFormatting[
  PathSymbol[e_] :>
    makeStandardBoxTemplate[e, "PathSymbolForm"]
]

$TemplateKatexFunction["PathSymbolForm"] = "path";

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
  None                      := "";
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
  Minus[e_]                 := makeStandardBoxTemplate[e, "MinusForm"];
  Power[e_, -1]             := makeStandardBoxTemplate[e, "InverseForm"];
  Times[-1, e_]             := makeStandardBoxTemplate[e, "MinusForm"];
  Negated[e_]               := makeStandardBoxTemplate[e, "NegatedForm"];
  DirectedEdge[args__]      := MakeBoxes @ DirectedEdgeForm[args];
  UndirectedEdge[args__]    := MakeBoxes @ UndirectedEdgeForm[args];
  Labeled[a_, l_]           := MakeBoxes @ ParenthesesLabeledForm[a, l];
  Text[t_]                  := MakeBoxes @ MathTextForm[t];
  Row[{r__}]                := MakeBoxes @ RowForm[r];
  other_                    := MakeBoxes @ other,
  {lsymsP -> $literalSymbolsP, symP -> $rawSymbolP, namedFnP -> Alternatives @@ $namedFunctions,
    binHeads -> $binaryRelationHeads, domainsP -> $domainsP}
];

algebraBoxes[_[args__], tag_] := makeStandardBoxTemplate[args, tag];

(**************************************************************************************************)

PackageExport["ParenthesesLabeledForm"]

declareBoxFormatting[
  ParenthesesLabeledForm[a_, l_] :> makeStandardBoxTemplate[a, l, "ParenthesesLabeledForm"]
];

$TemplateKatexFunction["ParenthesesLabeledForm"] = "parenLabeled";

(**************************************************************************************************)

PackageExport["ModulusLabeledForm"]

declareBoxFormatting[
  ModulusLabeledForm[a_, m_] :> makeStandardBoxTemplate[a, m, "ModulusLabeledForm"]
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

$TemplateKatexFunction["SerialCardinalForm"] = riffled[" \\serialCardSymbol "];
$TemplateKatexFunction["ParallelCardinalForm"] = riffled[" \\parallelCardSymbol "];

(**************************************************************************************************)

PackageExport["ComponentSuperQuiverOf"]

declareBoxFormatting[
  ComponentSuperQuiverOf[a_, b_] :> makeStandardBoxTemplate[a, b, "ComponentSuperQuiverOfForm"],
  ComponentSuperQuiverOf[] :> TemplateBox[{}, "ComponentSuperQuiverOfSymbol"]
]

$TemplateKatexFunction["ComponentSuperQuiverOfForm"] = riffled[" \\compSuperQuiverOf "];
$TemplateKatexFunction["ComponentSuperQuiverOfSymbol"] = " \\compSuperQuiverOf "&;

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
    makeStandardBoxTemplate[poly, graphs, "QuiverProductAppliedForm"]
]

$TemplateKatexFunction["QuiverProductAppliedForm"] = quiverProductAppliedKatex;

quiverProductAppliedKatex[f_, args___] := {"\\frac{", f, "} {", Riffle[{args}, ","], "}"};

(**************************************************************************************************)

PackageExport["GraphProductForm"]

PackageExport["DependentQuiverProductForm"]
PackageExport["IndependentQuiverProductForm"]

PackageExport["GraphUnionForm"]
PackageExport["VertexProductForm"]
PackageExport["EdgeProductForm"]
PackageExport["CardinalProductForm"]
PackageExport["CardinalSequenceForm"]

declareBoxFormatting[
  GraphUnionForm[g__] :>
    makeNaryHintedTemplateBox[QuiverSymbol, g, "GraphUnionForm"],
  GraphUnionForm[] :>
    TemplateBox[{}, "GraphUnionSymbol"],

  GraphProductForm[g__] :>
    makeNaryHintedTemplateBox[QuiverSymbol, g, "GraphProductForm"],
  GraphProductForm[] :>
    TemplateBox[{}, "GraphProductSymbol"],

  DependentQuiverProductForm[g1_, g2_] :>
    makeHintedTemplateBox[g1 -> QuiverSymbol, g2 -> QuiverSymbol, "DependentQuiverProductForm"],
  DependentQuiverProductForm[] :>
    TemplateBox[{}, "DependentQuiverProductSymbol"],

  IndependentQuiverProductForm[g1_, g2_] :>
    makeHintedTemplateBox[g1 -> QuiverSymbol, g2 -> QuiverSymbol, "IndependentQuiverProductForm"],
  IndependentQuiverProductForm[] :>
    TemplateBox[{}, "IndependentQuiverProductSymbol"],

  AlternatingGraphProductForm[g1_, g2_] :>
    makeHintedTemplateBox[g1 -> QuiverSymbol, g2 -> QuiverSymbol, "AlternatingGraphProductForm"],
  AlternatingGraphProductForm[] :>
    TemplateBox[{}, "AlternatingGraphProductSymbol"],

  VertexProductForm[v__] :>
    makeNaryHintedTemplateBox[VertexSymbol, v, "VertexProductForm"],
  EdgeProductForm[e__] :>
    makeNaryHintedTemplateBox[EdgeSymbol, e, "EdgeProductForm"],

  CardinalProductForm[c__] :>
    makeNaryHintedTemplateBox[CardinalSymbol, c, "CardinalProductForm"],
  CardinalSequenceForm[c__] :>
    makeNaryHintedTemplateBox[CardinalSymbol, c, "CardinalSequenceForm"]
]

$TemplateKatexFunction["GraphUnionForm"] = riffled[" \\graphUnionSymbol "];
$TemplateKatexFunction["GraphUnionSymbol"] = " \\graphUnionSymbol "&;

$TemplateKatexFunction["DependentQuiverProductForm"] = riffled[" \\depQuiverProdSymbol "];
$TemplateKatexFunction["DependentQuiverProductSymbol"] = " \\depQuiverProdSymbol "&;

$TemplateKatexFunction["IndependentQuiverProductForm"] = riffled[" \\indepQuiverProdSymbol "];
$TemplateKatexFunction["IndependentQuiverProductSymbol"] = " \\indepQuiverProdSymbol "&;

$TemplateKatexFunction["GraphProductForm"] = riffled[" \\graphProdSymbol "];
$TemplateKatexFunction["GraphProductSymbol"] = " \\graphProdSymbol "&;

$TemplateKatexFunction["VertexProductForm"] = riffled[" \\vertexProdSymbol "];
$TemplateKatexFunction["EdgeProductForm"] = riffled[" \\edgeProdSymbol "];
$TemplateKatexFunction["CardinalProductForm"] = riffled[" \\cardProdSymbol "];
$TemplateKatexFunction["CardinalSequenceForm"] = riffled[" \\cardSeqSymbol "];

(**************************************************************************************************)

PackageExport["ArrowheadSymbol"]

declareBoxFormatting[
  ArrowheadSymbol :> TemplateBox[{}, "ArrowheadSymbol"]
]

$TemplateKatexFunction["ArrowheadSymbol"] = "\\arrowhead"&;

(**************************************************************************************************)

PackageExport["ForwardFactorSymbol"]
PackageExport["BackwardFactorSymbol"]
PackageExport["NeutralFactorSymbol"]

declareBoxFormatting[
  ForwardFactorSymbol :> TemplateBox[{}, "ForwardFactorSymbol"],
  BackwardFactorSymbol :> TemplateBox[{}, "BackwardFactorSymbol"],
  NeutralFactorSymbol :> TemplateBox[{}, "NeutralFactorSymbol"]
]

$TemplateKatexFunction["ForwardFactorSymbol"] = "\\forwardFactor"&;
$TemplateKatexFunction["BackwardFactorSymbol"] = "\\backwardFactor"&;
$TemplateKatexFunction["NeutralFactorSymbol"] = "\\neutralFactor"&;

(**************************************************************************************************)

PackageExport["EdgeProductForm"]

declareBoxFormatting[
  EdgeProductForm[e1_, e2_] :>
    makeHintedTemplateBox[e1 -> EdgeSymbol, e2 -> EdgeSymbol, "EdgeProductForm"],
  EdgeProductForm[] :>
    TemplateBox[{}, "EdgeProductSymbol"]
]

$TemplateKatexFunction["EdgeProductForm"] = riffled["\\edgeProdSymbol"];
$TemplateKatexFunction["EdgeProductSymbol"] = "\\edgeProdSymbol"&;

(**************************************************************************************************)

PackageExport["SetCardinalityForm"]

declareBoxFormatting[
  SetCardinalityForm[s_] :>
    makeStandardBoxTemplate[s, "SetCardinalityForm"]
]

$TemplateKatexFunction["SetCardinalityForm"] = "cardinality"

(**************************************************************************************************)

PackageExport["IntegerRangeForm"]

declareBoxFormatting[
  IntegerRangeForm[1, n_] :> makeStandardBoxTemplate[n, "OneToNForm"],
  IntegerRangeForm[0, n_] :> makeStandardBoxTemplate[n, "ZeroToNForm"]
]

$TemplateKatexFunction["OneToNForm"] = "oneTo"
$TemplateKatexFunction["ZeroToNForm"] = "zeroTo"

(**************************************************************************************************)

PackageExport["SumForm"]

declareBoxFormatting[
  SumForm[a_, b_] :>
    MakeBoxes @ SumForm[a, b, ""],
  SumForm[a_, b_, c_] :>
    makeStandardBoxTemplate[a, b, c, "SumForm"],
  SumForm[a_, b_, c_, d_] :>
    makeStandardBoxTemplate[SuchThatForm[a, d], b, c, "SumForm"]
]

$TemplateKatexFunction["SumForm"] = "indexSum";

(**************************************************************************************************)

PackageExport["ProductForm"]

declareBoxFormatting[
  ProductForm[a_, b_] :>
    MakeBoxes @ ProductForm[a, b, ""],
  ProductForm[a_, b_, c_] :>
    makeStandardBoxTemplate[a, b, c, "ProductForm"],
  ProductForm[a_, b_, c_, d_] :>
    makeStandardBoxTemplate[SuchThatForm[a, d], b, c, "ProductForm"]
]

$TemplateKatexFunction["ProductForm"] = "indexProd";

(**************************************************************************************************)

PackageExport["PlusForm"]

declareBoxFormatting[
  PlusForm[args__] :>
    makeStandardBoxTemplate[args, "PlusForm"],
  PlusForm[] :> "+"
]

$TemplateKatexFunction["PlusForm"] = riffled[" + "];

(**************************************************************************************************)

PackageExport["SubtractForm"]

declareBoxFormatting[
  SubtractForm[args__] :>
    makeStandardBoxTemplate[args, "SubtractForm"],
  SubtractForm[] :> "-"
]

$TemplateKatexFunction["SubtractForm"] = riffled[" - "];

(**************************************************************************************************)

PackageExport["MinusForm"]

declareBoxFormatting[
  MinusForm[args__] :>
    makeStandardBoxTemplate[args, "MinusForm"],
  MinusForm[] :> "-"
]

$TemplateKatexFunction["MinusForm"] = {" -", #1}&;

(**************************************************************************************************)

PackageExport["TimesForm"]

declareBoxFormatting[
  TimesForm[args__] :>
    makeStandardBoxTemplate[args, "TimesForm"]
]

$TemplateKatexFunction["TimesForm"] = riffled[" \\times "];

(**************************************************************************************************)

PackageExport["CartesianProductForm"]

declareBoxFormatting[
  CartesianProductForm[args__] :>
    makeStandardBoxTemplate[args, "CartesianProductForm"]
]

$TemplateKatexFunction["CartesianProductForm"] = riffled[" \\cprod "];

(**************************************************************************************************)

PackageExport["SetUnionForm"]
PackageExport["SetIntersectionForm"]

declareBoxFormatting[
  SetUnionForm[args__] :>
    makeStandardBoxTemplate[args, "SetUnionForm"],
  SetUnionForm[] :>
    makeStandardBoxTemplate[args, "SetUnionSymbol"],
  SetIntersectionForm[args__] :>
    makeStandardBoxTemplate[args, "SetIntersectionForm"],
  SetIntersectionForm[] :>
    makeStandardBoxTemplate[args, "SetIntersectionSymbol"]
]

$TemplateKatexFunction["SetUnionForm"] = riffled[" \\cup "];
$TemplateKatexFunction["SetUnionSymbol"] = " \\cup "&;
$TemplateKatexFunction["SetIntersectionForm"] = riffled[" \\cap "];
$TemplateKatexFunction["SetIntersectionSymbol"] = " \\cap "&;

(**************************************************************************************************)

PackageExport["DivideForm"]
PackageExport["InlineDivideForm"]

declareBoxFormatting[
  DivideForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "DivideForm"],
  InlineDivideForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "InlineDivideForm"]
]

$TemplateKatexFunction["DivideForm"] = "frac";
$TemplateKatexFunction["InlineDivideForm"] = riffled[" / "];

(**************************************************************************************************)

PackageExport["PowerForm"]
PackageExport["RepeatedPowerForm"]

declareBoxFormatting[
  PowerForm[a_, b_] :> makeStandardBoxTemplate[a, b, "PowerForm"],
  RepeatedPowerForm[a_, b_] :> makeStandardBoxTemplate[a, b, "RepeatedPowerForm"]
]

$TemplateKatexFunction["PowerForm"] = "pow";
$TemplateKatexFunction["RepeatedPowerForm"] = "rep";

(**************************************************************************************************)

PackageExport["PathRelationForm"]

declareBoxFormatting[
  PathRelationForm[args__] :>
    TemplateBox[MapUnevaluated[wordBoxes, {args}], "PathRelationForm"],
  PathRelationForm[] :>
    TemplateBox[{}, "PathRelationSymbol"]
]

$TemplateKatexFunction["PathRelationForm"] = riffled[" \\pathIso "]
$TemplateKatexFunction["PathRelationSymbol"] = "\\pathIso"&;

(**************************************************************************************************)

PackageExport["ApproxEqualForm"]

declareBoxFormatting[
  ApproxEqualForm[args__] :>
    makeStandardBoxTemplate[args, "ApproxEqualForm"]
]

$TemplateKatexFunction["ApproxEqualForm"] = riffled[" \\approx "];

(**************************************************************************************************)

PackageExport["IsomorphicForm"]

declareBoxFormatting[
  IsomorphicForm[args__] :>
    makeStandardBoxTemplate[args, "IsomorphicForm"]
]

$TemplateKatexFunction["IsomorphicForm"] = riffled[" \\iso "];

(**************************************************************************************************)

PackageExport["HomotopicForm"]

declareBoxFormatting[
  HomotopicForm[args__] :>
    makeStandardBoxTemplate[args, "HomotopicForm"],

  HomotopicForm[] :> TemplateBox[{}, "HomotopicSymbol"]

]

$TemplateKatexFunction["HomotopicForm"] = riffled[" \\homotopic "];
$TemplateKatexFunction["HomotopicSymbol"] = " \\homotopic{}{}"

(**************************************************************************************************)

PackageExport["DefEqualForm"]

declareBoxFormatting[
  DefEqualForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "DefEqualForm"]
]

$TemplateKatexFunction["DefEqualForm"] = riffled[" \\defeq "];

(**************************************************************************************************)

PackageExport["EqualForm"]

declareBoxFormatting[
  EqualForm[args__] :>
    makeStandardBoxTemplate[args, "EqualForm"]
]

$TemplateKatexFunction["EqualForm"] = riffled[" = "]

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
  ":="                  -> "&\\defeq ",
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

PackageExport["Form"]

declareBoxFormatting[
  BinaryRelationForm[relation_String][args__] :>
    makeTypedTemplateBox[relation -> None, args, "BinaryRelationForm"]
]

$TemplateKatexFunction["BinaryRelationForm"] = Function[riffled[#1][##2]];

(**************************************************************************************************)

PackageExport["NotEqualForm"]

declareBoxFormatting[
  NotEqualForm[args__] :>
    makeStandardBoxTemplate[args, "NotEqualForm"]
]

$TemplateKatexFunction["NotEqualForm"] = riffled[" \\neq "]

(**************************************************************************************************)

PackageExport["ImplicitTimesForm"]

declareBoxFormatting[
  ImplicitTimesForm[args__] :>
    makeStandardBoxTemplate[args, "ImplicitTimesForm"]
]

$TemplateKatexFunction["ImplicitTimesForm"] = riffled["\,"]


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

declareBoxFormatting[
  InverseForm[e_] :>
    makeStandardBoxTemplate[e, "InverseForm"]
];

$TemplateKatexFunction["InverseForm"] = "inv";

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

declareBoxFormatting[
  BarTokenSymbol :> TemplateBox[{}, "BarTokenSymbol"]
]

$TemplateKatexFunction["BarTokenSymbol"] = "\\barToken"&;

(**************************************************************************************************)

PackageExport["FilledTokenSymbol"]
PackageExport["FilledRectangleTokenSymbol"]

declareBoxFormatting[
  FilledTokenSymbol :> TemplateBox[{}, "FilledTokenSymbol"],
  FilledRectangleTokenSymbol :> TemplateBox[{}, "FilledRectangleTokenSymbol"]
];

$TemplateKatexFunction["FilledTokenSymbol"] = "\\filledToken"&;
$TemplateKatexFunction["FilledRectangleTokenSymbol"] = "\\filledRectToken"&;

(**************************************************************************************************)

PackageExport["EmptyTokenSymbol"]
PackageExport["EmptyRectangleTokenSymbol"]

declareBoxFormatting[
  EmptyTokenSymbol :> TemplateBox[{}, "EmptyTokenSymbol"],
  EmptyRectangleTokenSymbol :> TemplateBox[{}, "EmptyRectangleTokenSymbol"]
];

$TemplateKatexFunction["EmptyTokenSymbol"] = "\\emptyToken"&;
$TemplateKatexFunction["EmptyRectangleTokenSymbol"] = "\\emptyRectToken"&;

(**************************************************************************************************)

PackageExport["NegatedBoxForm"]

SetHoldFirst[NegatedBoxForm];
NegatedBoxForm[e_] := makeStandardBoxTemplate[e, "NegatedForm"]

$TemplateKatexFunction["NegatedForm"] = "negated";

(********************************************)

PackageExport["UnitVertexFieldSymbol"]

declareBoxFormatting[
  UnitVertexFieldSymbol[] :> TemplateBox[{}, "UnitVertexFieldSymbol"]
]

$TemplateKatexFunction["UnitVertexFieldSymbol"] = "unitVertexField"

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
$TemplateKatexFunction["ForwardSymbol"] = "\\forwardSymbol"&;
$TemplateKatexFunction["BackwardSymbol"] = "\\backwardSymbol"&;
$TemplateKatexFunction["SymmetricSymbol"] = "\\symmetricSymbol"&;
$TemplateKatexFunction["AntisymmetricSymbol"] = "\\antisymmetricSymbol"&;

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
$TemplateKatexFunction["PathTranslateSymbol"] = "\\translateSymbol"&;

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
$TemplateKatexFunction["PathBackwardTranslateSymbol"] = "\\backwardTranslateSymbol"&;

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
$TemplateKatexFunction["GradientSymbol"] = "\\grad"&;

(********************************************)

PackageExport["DivergenceForm"]

declareBoxFormatting[
  DivergenceForm[q_] :> makeHintedTemplateBox[q -> EdgeFieldSymbol, "DivergenceForm"],
  DivergenceForm[] :> TemplateBox[{}, "DivergenceSymbol"]
];

$TemplateKatexFunction["DivergenceForm"] = "divOf";
$TemplateKatexFunction["DivergenceSymbol"] = "\\div"&;

(********************************************)

PackageExport["LaplacianForm"]

declareBoxFormatting[
  LaplacianForm[q_] :> makeHintedTemplateBox[q -> EdgeFieldSymbol, "LaplacianForm"],
  LaplacianForm[] :> TemplateBox[{}, "LaplacianSymbol"]
];

$TemplateKatexFunction["LaplacianForm"] = "laplacianOf";
$TemplateKatexFunction["LaplacianSymbol"] = "\\laplacian"&;

(********************************************)

PackageExport["SuchThatForm"]

declareBoxFormatting[
  SuchThatForm[a_, b_] :> makeStandardBoxTemplate[a, b, "SuchThatForm"]
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

(********************************************)

SetHoldAllComplete[maybeParen, maybeParenBoxes];

maybeParen[h_][b_] := Block[{$eh = h, $ehc = If[Head[h] === Alternatives, First @ h, h]}, maybeParenBoxes @  b];

maybeParenBoxes = Case[
  l:lsymsP                              := MakeBoxes @ l;
  s:syms                                := With[{head = $ehc}, MakeBoxes @ head @ s];
  e:_InverseForm | _GroupInverseForm | _GroupoidInverseForm | _AppliedForm := MakeBoxes @ e;
  (e:(head_[___])) /; MatchQ[head, $eh] := MakeBoxes @ e;
  e:(colsP[_])                          := MakeBoxes @ e;
  other_                                := MakeBoxes @ ParenthesesForm @ other,
  {symsP -> $rawSymbolP, colsP -> $colorFormP, lsymsP -> $literalSymbolsP}
];

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

declareBoxFormatting[
  ListForm[args__] :>
    makeStandardBoxTemplate[args, "ListForm"]
]

$TemplateKatexFunction["ListForm"] = applyRiffled["list", ","];

(********************************************)

PackageExport["PrimedForm"]

declareUnaryWrapperForm[PrimedForm];

(********************************************)

PackageExport["ParenthesesForm"]

declareBoxFormatting[
  ParenthesesForm[args__] :>
    makeStandardBoxTemplate[args, "ParenthesesForm"]
]

$TemplateKatexFunction["ParenthesesForm"] = applyRiffled["paren", ","];

(********************************************)

PackageExport["CeilingForm"]
PackageExport["FloorForm"]

declareBoxFormatting[
  CeilingForm[n_] :> makeStandardBoxTemplate[n, "Ceiling"],
  FloorForm[n_] :> makeStandardBoxTemplate[n, "Floor"]
]

$TemplateKatexFunction["Ceiling"] = "ceil";
$TemplateKatexFunction["Floor"] = "floor";

(********************************************)

PackageExport["TupleForm"]

declareBoxFormatting[
  TupleForm[args__] :>
    makeStandardBoxTemplate[args, "TupleForm"]
]

$TemplateKatexFunction["TupleForm"] = applyRiffled["tuple", ","];

(********************************************)

PackageExport["TranslationVectorForm"]

declareBoxFormatting[
  TranslationVectorForm[entries__] :>
    makeStandardBoxTemplate[entries, "TranslationVectorForm"]
];

$TemplateKatexFunction["TranslationVectorForm"] = applyRiffled["tvector", ","];

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
    makeStandardBoxTemplate[args, "ConcatenationForm"]
]

$TemplateKatexFunction["ConcatenationForm"] = applyRiffled["concat", " "];

(********************************************)

PackageExport["CommaRowForm"]

declareBoxFormatting[
  CommaRowForm[args__] :>
    makeStandardBoxTemplate[args, "CommaRowForm"]
]

$TemplateKatexFunction["CommaRowForm"] = riffled[","];

(********************************************)

PackageExport["SpacedRowForm"]

declareBoxFormatting[
  SpacedRowForm[args__] :>
    makeStandardBoxTemplate[args, "SpacedRowForm"]
]

$TemplateKatexFunction["SpacedRowForm"] = riffled[" \\quad "];

(********************************************)

PackageExport["AndForm"]

declareBoxFormatting[
  AndForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "AndForm"]
];

$TemplateKatexFunction["AndForm"] = riffled["\\land"];

(********************************************)

PackageExport["OrForm"]

declareBoxFormatting[
  OrForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "OrForm"]
];

$TemplateKatexFunction["OrForm"] = riffled["\\lor"];

(********************************************)

PackageExport["NotForm"]

declareBoxFormatting[
  NotForm[a_] :>
    TemplateBox[List @ maybeParen[SymbolForm] @ a, "NotForm"]
];

$TemplateKatexFunction["NotForm"] = riffled["\\lnot"];

(********************************************)

PackageExport["ImpliesForm"]

declareBoxFormatting[
  ImpliesForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "ImpliesForm"]
];

$TemplateKatexFunction["ImpliesForm"] = riffled["\\implies"];

(********************************************)

PackageExport["EquivalentForm"]

declareBoxFormatting[
  EquivalentForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "EquivalentForm"]
];

$TemplateKatexFunction["EquivalentForm"] = riffled["\\iff"];

(********************************************)

PackageExport["MapsToForm"]

declareBoxFormatting[
  MapsToForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "MapsToForm"]
];

$TemplateKatexFunction["MapsToForm"] = "mto";

(**************************************************************************************************)

PackageExport["EllipsisSymbol"]

declareBoxFormatting[
  EllipsisSymbol :> TemplateBox[{}, "EllipsisSymbol"]
]

$TemplateKatexFunction["EllipsisSymbol"] = "\\dots"&;

(**************************************************************************************************)

PackageExport["PartialDifferentialOfForm"]

declareBoxFormatting[
  PartialDifferentialOfForm[x_] :>
    makeStandardBoxTemplate[x, "PartialDifferentialOfForm"],
  PartialDifferentialOfForm[] :>
    TemplateBox[{}, "PartialDifferentialSymbol"]
];

$TemplateKatexFunction["PartialDifferentialOfForm"] = "partialdof";
$TemplateKatexFunction["PartialDifferentialSymbol"] = "\\partial"&;

(**************************************************************************************************)

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

namedFuncKatex["Word"] := "\\wordOf"; (* because 'word' already menas wordForm *)
namedFuncKatex["LCM"] := "\\lcm"; (* lowercase *)
namedFuncKatex[s_String] := namedFuncKatex[s] = StringJoin["\\", LowerCaseFirst @ s];

(**************************************************************************************************)

PackageExport["QuotientForm"]
PackageExport["CompactQuotientForm"]
PackageExport["MultilineQuotientForm"]

declareBoxFormatting[
  QuotientForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "QuotientForm"],
  MultilineQuotientForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "MultilineQuotientForm"],
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
    makeStandardBoxTemplate[args, "RowForm"],
  TermRowForm[args__] :>
    makeStandardBoxTemplate[args, "TermRowForm"]
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
    makeStandardBoxTemplate[elem, "ChartSymbolForm"],
  ChartSymbolForm[elem_List] :>
    makeTypedBoxTemplate[elem -> ConcatenationForm, "ChartSymbolForm"],
  ChartSymbolForm[] :>
    TemplateBox[{}, "ChartSymbol"]
];

$TemplateKatexFunction["ChartSymbolForm"] = "chart"
$TemplateKatexFunction["ChartSymbol"] = "\\chartSymbol"&;

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

declareBoxFormatting[
  GraphRegionIntersectionForm[args__] :>
    makeStandardBoxTemplate[args, "GraphRegionIntersectionForm"],
  GraphRegionIntersectionForm[] :>
    TemplateBox[{}, "GraphRegionIntersectionSymbol"],
  GraphRegionUnionForm[args__] :>
    makeStandardBoxTemplate[args, "GraphRegionUnionForm"],
  GraphRegionUnionForm[] :>
    TemplateBox[{}, "GraphRegionUnionSymbol"]
];

$TemplateKatexFunction["GraphRegionIntersectionForm"] = riffled[" \\regionIntersection "]
$TemplateKatexFunction["GraphRegionIntersectionSymbol"] = "\\regionIntersection"&;
$TemplateKatexFunction["GraphRegionUnionForm"] = riffled[" \\regionUnion "]
$TemplateKatexFunction["GraphRegionUnionSymbol"] = "\\regionUnion"&;

(********************************************)

PackageExport["IdentityElementForm"]

declareBoxFormatting[
  IdentityElementForm[args___] :>
    makeStandardBoxTemplate[args, "IdentityElementForm"]
];

$TemplateKatexFunction["IdentityElementForm"] = "idElem";

(********************************************)

PackageExport["CardinalBindingForm"]

declareBoxFormatting[
  CardinalBindingForm[q_, cards__] :>
    TemplateBox[
      Prepend[
        MapUnevaluated[cardinalBox, {cards}],
        makeQGBoxes @ q
      ],
      "CardinalBindingForm"
    ]
];

$TemplateKatexFunction["CardinalBindingForm"] = "bindCards"[#1, Riffle[{##2}, ","]]&;


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