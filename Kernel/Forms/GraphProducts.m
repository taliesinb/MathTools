PublicTypesettingForm[GraphUnionForm, GraphProductForm]

PublicTypesettingForm[DependentQuiverProductForm]

PublicTypesettingForm[LockedQuiverProductForm, LeftFreeQuiverProductForm, RightFreeQuiverProductForm]

PublicTypesettingForm[StrongIndependentQuiverProductForm, StrongQuiverProductForm]

PublicTypesettingForm[CartesianQuiverProductForm]

DefineInfixForm[GraphUnionForm,             WideOpBox @ "\[SquareUnion]"]
DefineInfixForm[GraphProductForm,           WideOpBox @ "\[Times]"]
DefineInfixForm[CartesianQuiverProductForm, WideOpBox @ "\[EmptySquare]"]

DefineInfixBinaryForm[DependentQuiverProductForm,         WideOpBox @ "\[Times]"]
DefineInfixBinaryForm[LockedQuiverProductForm,            WideOpBox @ "\[LowerRightArrow]"]
DefineInfixBinaryForm[LeftFreeQuiverProductForm,          WideOpBox @ "\:2aab"]
DefineInfixBinaryForm[RightFreeQuiverProductForm,         WideOpBox @ "\:2aaa"]
DefineInfixBinaryForm[StrongIndependentQuiverProductForm, WideOpBox @ "\:2a1d"]
DefineInfixBinaryForm[StrongQuiverProductForm,            WideOpBox @ "\:29d1"]

(**************************************************************************************************)

PublicTypesettingForm[VerticalVertexProductForm, VerticalCardinalProductForm]

PublicTypesettingForm[VertexProductForm, EdgeProductForm, CardinalProductForm]

DefineBinaryForm[VerticalVertexProductForm, FractionBox[$1, $2]]
DefineBinaryForm[VerticalCardinalProductForm, FractionBox[$1, $2]]

DefineStandardTraditionalForm[{
  VertexProductForm[a_, b_] :> MakeBoxes[TupleForm[a, b]],
  EdgeProductForm[a_, b_]     :> MakeBoxes[TupleForm[a, b]],
  CardinalProductForm[a_, b_] :> MakeBoxes[TupleForm[a, b]]
}];

(**************************************************************************************************)

PublicTypesettingForm[ComponentSuperQuiverOfForm]

DefineInfixForm[ComponentSuperQuiverOfForm, WideOpBox @ "\[Succeeds]"]

(**************************************************************************************************)

PublicTypesettingForm[QuiverProductAppliedForm, CompactQuiverProductAppliedForm]

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

factorSymbolBox[wl_, k_] := KBox[
  StyleBox[AdjustmentBox[wl, BoxBaselineShift -> -0.2], SpanMinSize -> 1.15, SpanMaxSize -> 1.15, FontWeight -> "Bold", FontFamily ->   "KaTeX_WLBypass"],
  KOrd @ k
];

overlay[args___] := OverlayBox[{args}, BaselinePosition -> Baseline];

DefineSymbolForm[{
  ForwardFactorSymbol                 -> factorSymbolBox["\:3191", "\\uparrow"],
  BackwardFactorSymbol                -> factorSymbolBox["\:3193", "\\downarrow"],
  NeutralFactorSymbol                 -> factorSymbolBox["\:21af", "\\updownarrow"],
  ForwardBackwardFactorSymbol         -> factorSymbolBox[overlay["\:3191", "\:3193"], "\\shornarrow"],
  ForwardBackwardNeutralFactorSymbol  -> factorSymbolBox[overlay["\:3191", "\:3193", "\:3212"], """\mathrlap{\downarrow}{\mathrlap{\uparrow}{\endash}}"""]
}];
