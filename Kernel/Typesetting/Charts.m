PublicFunction[ChartColorForm]

ChartColorForm[expr_, colors_] := Scope[
  colors = Which[
    GraphQ[colors], LookupCardinalColors @ colors,
    AssociationQ[colors], colors,
    Automatic, Automatic,
    True, Return @ expr
  ];
  ReplaceAll[
    expr,
    ChartSymbol[sym_String] :> formatChartSymbol[sym, colors]
  ]
];

ChartColorForm[graph_][expr_] := ChartColorForm[expr, graph];

(**************************************************************************************************)

PublicFunction[CardinalTransition]

SetUsage @ "
CardinalTransition[a$ -> b$] represents a transition from cardinal a$ to cardinal b$.
CardinalTransition[{rule$1, rule$2, $$}] represents multiple simultaneous transitions.
"

$anyRuleP = _Rule | _TwoWayRule;
declareFormatting[
  ca:CardinalTransition[$anyRuleP | {$anyRuleP..}] :> formatCardinalTransition[ca]
]

PrivateFunction[formatCardinalTransition]

formatCardinalTransition = Case[
  CardinalTransition[{}] :=
    "";
  CardinalTransition[r_Rule | r_TwoWayRule] :=
    fmtCardinalArrow @ r;
  CardinalTransition[list:{$anyRuleP..}] :=
    Column[fmtCardinalArrow /@ list, Spacings -> -0.1, ItemSize -> {All, 1}];
  _ := "?"
];

fmtCardinalArrow[a_ -> a_] := Nothing;

fmtCardinalArrow[a_ -> b_] :=
  Row[formatCardinal /@ {a, b}, Style["\[RightArrow]", Gray]]

fmtCardinalArrow[TwoWayRule[a_, b_]] :=
  Row[formatCardinal /@ {a, b}, Style["\[LeftRightArrow]", Gray]]

formatCardinal[c_] := If[!GraphQ[$Graph], c,
  Style[c, LookupCardinalColors[$Graph, StripInverted @ c]]
];
