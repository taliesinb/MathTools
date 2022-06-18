PublicForm[GraphProductForm, GraphUnionForm]

PublicForm[DependentQuiverProductForm]

PublicForm[LockedQuiverProductForm, LeftFreeQuiverProductForm, RightFreeQuiverProductForm]

PublicForm[StrongIndependentQuiverProductForm, StrongQuiverProductForm]

PublicForm[CartesianQuiverProductForm]

(* PublicForm[LeftStrongQuiverProductForm, RightStrongQuiverProductForm]

PublicForm[LeftFiberQuiverProductForm, RightFiberQuiverProductForm]
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

PublicForm[VerticalVertexProductForm, VerticalCardinalProductForm]

PublicForm[VertexProductForm, EdgeProductForm, CardinalProductForm]

declareBinaryForm[VerticalVertexProductForm];
declareBinaryForm[VerticalCardinalProductForm];

declareCommaRiffledForm[VertexProductForm, "vertexProduct"];
declareCommaRiffledForm[EdgeProductForm, "edgeProduct"];
declareCommaRiffledForm[CardinalProductForm, "cardinalProduct"];

(**************************************************************************************************)

PublicForm[ComponentSuperQuiverOfForm]

declareInfixSymbol[ComponentSuperQuiverOfForm]

(**************************************************************************************************)

PublicForm[QuiverProductAppliedForm, CompactQuiverProductAppliedForm]

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

PublicSymbol[ForwardFactorSymbol, BackwardFactorSymbol, NeutralFactorSymbol, ForwardBackwardFactorSymbol, ForwardBackwardNeutralFactorSymbol]

declareConstantSymbol[
  {ForwardFactorSymbol, BackwardFactorSymbol, NeutralFactorSymbol,
  ForwardBackwardFactorSymbol, ForwardBackwardNeutralFactorSymbol}
];