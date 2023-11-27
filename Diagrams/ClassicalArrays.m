$insetCubeOpts = Sequence[
  FontSize -> 40, FontColor -> GrayLevel[0,1], CubeGap->0.2,
  CubeOpacity -> 0.75, FlipAxes -> {1,2,3},
  Displacement -> "Screen"
];
$peopleVector = {3, 0, 4};
$peopleArr = {{1,2,2},{3,0,4}};
$people3Arr = {{{1,2}, {2, 1}, {2, 0}},{{3,2}, {0, 3}, {4, 1}}};

NamedDiagram["ClassicalArrays/ExampleCubeArrays"] :=
SpacedColumn[
  NeutralGraphics3D[
    AssembleGraphics3D @ XYStackR[
      InsetCubeGrid[List @ List @ List @ 3, CubeStyle -> EdgeForm[None], $insetCubeOpts],
      InsetCubeGrid[List @ Map[List] @ $peopleVector, $insetCubeOpts],
      InsetCubeGrid[ArrayReshape[$peopleArr, {2, 3, 1}], $insetCubeOpts],
      InsetCubeGrid[$people3Arr, $insetCubeOpts], Text[""],
      Spacing -> 2
    ],
    ImageSize -> 520
  ],
  SpacedRow["scalar  ", "vector       ", "   matrix         ", "  3-array", BaseStyle -> {FontSize -> 16, FontColor -> $DarkGray, FontFamily -> "Avenir"}, Spacings -> 60],
  SpacedRow["   \[LeftAngleBracket]\[RightAngleBracket]", "    \[LeftAngleBracket]3\[RightAngleBracket]  ", "        \[LeftAngleBracket]3,2\[RightAngleBracket]  ", "       \[LeftAngleBracket]3,2,2\[RightAngleBracket]", BaseStyle -> {FontSize -> 16, FontColor -> $DarkGray, FontFamily -> "Avenir"}, Spacings -> 80],
  Alignment -> Left
];

NamedDiagram["ClassicalArrays/SVM"] :=
ArrayCircuitGraphics @ NodeRow[{
  ClassicalArrayNode["S", {}, 1],
  ClassicalArrayNode["V", 1, 1.25],
  ClassicalArrayNode["M", 2, 1.5]
}];

NamedDiagram["ClassicalArrays/MapFM"] :=
ArrayCircuitGraphics @
DerivedArrayNode[2,
  NodeColumn[{ClassicalArrayNode["M", 2, 1.5], ScalarFunctionNode["f", 1, {1.5, 1.2}]}],
  {KeyWire[1,"M"->1], KeyWire[2,"M"->2],
   ValueWire["M", "f"], ValueWire["f", 1]}
];

$fShape = RotateLeft /@ {{0,1,1,1}, {0,1,0,0}, {0,1,1,0}, {0,1,0,0}, {0,1,0,0}};
$colonShape = {{0,0,0,0}, {0,0,0,0}, {0,0,0,1}, {0,0,0,0}, {0,0,0,1}};
$fData = Transpose[{$fShape, 2 * $colonShape, 3 * $colonShape}, {3, 1, 2}];

NamedDiagram["ClassicalArrays/FColon2D"] :=
MeshImage[N @ $fData, 30];

$imgArrayOpts = Sequence[
  CubeGap -> 0.15,
  FrameStyle -> Directive[AbsoluteThickness[3], GrayLevel[0.2,.5]], CubeStyle -> EdgeForm @ AbsoluteThickness[1.5],
  ColorRules -> {1 -> $Red, 2 -> $Green, 3 -> $Blue, 4 -> $Red},
  FlipAxes -> {1, 2, 3},
  Ticks -> {Style[Right, $Pink, Bold], Style[TopRight, $DarkTeal, Bold], Style[TopLeft, $DarkPurple, Bold]},
  TickSpacing -> .3, FlipTicks -> {1,2,3}, TicksStyle -> 25
];

NamedDiagram["ClassicalArrays/FColon3D"] :=
NeutralGraphics3D @ ColoredCubeGrid[$fData,  $imgArrayOpts];

$wvecField = Table[({y-1.5,1.5-x})2,{x,0,3},{y,0,3}];
$windVecOpts = Sequence[
  ArrowheadShape -> "FilledTriangle",
  ArrowColor -> $Gray,
  ArrowheadPosition -> Offset[0.05, 1],
  ArrowThickness -> 2,
  ArrowheadLength -> 0.1
];

vecFieldElem[sz_][vec_, pos_] := {ExtendedArrow[VectorCurve[pos, sz * vec]], Disk[pos, .04]};
vecFieldPlot[array_, sz_] := Graphics[
  Style[
    MapIndexed[vecFieldElem[sz], array, {2}],
    $windVecOpts
  ],
  Frame -> True,
  FrameTicks -> {{1,2,3,4}, {{1, "4"}, {2, "3"}, {3, "2"}, {4, "1"}}}
];

NamedDiagram["ClassicalArrays/WindVectorField"] :=
vecFieldPlot[$wvecField, 0.15];


$wvlabelTextStyle = Sequence[BaseStyle -> {FontFamily -> "Fira Code", FontWeight -> "Medium", FontSize -> 11}];
signStr[0] := " 0";
signStr[n_?Positive] := "+" <> IntegerString[n];
signStr[n_] := n;

NamedDiagram["ClassicalArrays/WindVectorExample"] :=
Graphics[
  Style[{
    vecFieldElem[1][{3,3}, {0,0}], Text["[+3 -3]", {3,3}, {0.2, -1.1}, $wvlabelTextStyle],
    vecFieldElem[1][{1,-3}, {0,0}], Text["[+1 +3]", {1,-3}, {0, 1.1}, $wvlabelTextStyle],
    (*vecFieldElem[1][{-1,-1}, {0,0}], Text["[-1 +1]", {-1.5,-1.6}, $labelTextStyle],*)
    Disk[{0, 0}, .2]},
    $windVecOpts, ArrowheadLength -> .4],
  PlotRange -> {{-3, 3}, {-3, 3}},
  ImageSize -> Small, GridLines -> {Range[-3,3], Range[-3,3]}, GridLinesStyle-> $LightGray,
  PlotRangeClipping-> True, Frame -> True, PlotRangePadding -> 1,
  FrameTicks -> {{None, Map[{-#, signStr @ #}&, Range[-3,3]]}, {Map[{#, signStr[#]}&, Range[-3,3]], None}}
];

NamedDiagram["ClassicalArrays/ExampleGraph"] :=
  ExtendedGraph[
    {"a" -> "b", "b" -> "c", "b" -> "c", "b" -> "d", "d" -> "e", "e" -> "d"},
    VertexLabels -> Automatic, ArrowheadSize -> 20, ImageSize -> 250, EdgeThickness -> 2,
    VertexLabelStyle -> {FontSize ->15, FontFamily -> "Fira Code", Background -> None},
    PlotRangePadding -> .15
  ];

NamedDiagram["ClassicalArrays/ExampleHypergraph"] := Module[{p1, p2, p3, p4, dx},
  {p1, p2, p3} = CirclePoints[3]; p4 = p1 + p1 - p3; dx = {0.1, 0};
  Graphics[{
    ArrowheadLength -> .15, ArrowheadThickness -> 0, ArrowheadColor -> $Gray, ArrowPathSetback -> 0.12,
    ArrowThickness -> 2, ArrowheadPosition -> Offset[.03, 1], ArrowColor -> $Gray,
    ArrowheadShape -> "Arrow",
    ExtendedArrow[RollingCurve[{p1, 0.98 p2, p3}, ArrowPathSetback -> {0.1, 0}]],
    ExtendedArrow[RollingCurve[{0.9 p2 + dx/2, p3 + dx, p1}], ArrowPathSetback -> 0.1],
    ExtendedArrow[RollingCurve[{p4, p1+ dx*1.5, p2 + dx*1.3}], ArrowPathSetback -> 0.1],
    Disk[#, .03]& /@ {p1, p2, p3, p4},
    Text[#1, #2, #3, BaseStyle -> {FontFamily -> "Fira Code", FontSize -> 15}]& @@@ {
      {"3", p1, {0, 1.5}},
      {"1", p2, {0, -1.5}},
      {"2", p3, {0, 1.5}},
      {"4", p4, {0, 1.5}}
    }
  }, ImageSize -> 250]
];

NamedDiagram["ClassicalArrays/FMapABC"] :=
DerivedArraySignatureForm[Superscript["f", "\[UpArrow]"]["A", "B", "C"]] @
ArrayCircuitGraphics @
DerivedArrayNode[1,
  NodeColumn[
    {
      NodeRow[{ClassicalArrayNode["A", 1, 1], ClassicalArrayNode["B", 1, 1], ClassicalArrayNode["C", 1, 1]}, Spacings -> 0.25],
      ScalarFunctionNode["f", 3, {3.5, 1}, PortSpacing -> 1.25]
    },
    Spacings -> 0.75, Epilog -> {
    ValueWire[$["ABC"] -> 1, "f" -> $[1,2,3]]}
  ],
  {KeyWire[1, {"A", "B", "C"}],
   ValueWire["f", 1]},
  FrameMargins -> {All -> .75, Top -> 1.25}
]

Clear[plotCube]; SetHoldFirst[plotCube];
plotCube[body_, {m_, n_, p_}, col_, sz_, opts___Rule] := NeutralGraphics3D[
  {ColoredCubeGrid[Boole @ Table[body,{r,m},{b,n},{g,p}], ColorRules -> {1 -> col}, opts,
  Ticks -> {Style[Right, $Green], Style[TopRight, $Blue], Style[TopLeft, $Red]},
  FrameStyle -> Directive[AbsoluteThickness[2], GrayLevel[0.2,.5]],
  MeshStyle -> Directive[AbsoluteThickness[2], GrayLevel[0.5,0.1]], $imgArrayOpts], Transparent, Point[{-1,0,0}]},
  ImageSize -> sz * 0.9];

NamedDiagram["ClassicalArrays/CubeAggregations1,2,3"] :=
SpacedColumn[
  SpacedRow[
    plotCube[g == b == 1, {2, 2, 4}, $Red, 150],
    Pane @ Labeled[MakeArrow[50, 10, 2, $Gray], Subscript["sum", Color1Form @ 1]],
    plotCube[g == b == 1, {1, 2, 4}, $Red, 115, Ticks -> {Style[Right, $Green], Style[TopRight, $Blue], None}],
    Spacings -> 30
  ],
  SpacedRow[
    plotCube[r == g == 1, {2, 2, 4}, $Blue, 150],
    Pane @ Labeled[MakeArrow[50, 10, 2, $Gray], Subscript["sum", Color2Form @ 2]],
    plotCube[r == g == 1, {2, 1, 4}, $Blue, 115, Ticks -> {Style[Right, $Green], None, Style[TopLeft, $Red]}],
    Spacings -> 30
  ],
  SpacedRow[
    plotCube[r == b == 1, {2, 2, 4}, $Green, 150],
    Pane @ Labeled[MakeArrow[50, 10, 2, $Gray], Subscript["sum", Color3Form @ 3]],
    plotCube[r == b == 1, {2, 2, 1}, $Green, 140, Ticks -> {None, Style[TopRight, $Blue], Style[TopLeft, $Red]}],
    Spacings -> 30
  ],
  Spacings -> 10, Alignment -> Left
];

NamedDiagram["ClassicalArrays/CubeAggregations23"] :=
SpacedRow[
  plotCube[r == 1, {2, 2, 4}, $Teal, 150],
  Pane @ Labeled[MakeArrow[50, 10, 2, $Gray], Subscript["sum", Row[{Color2Form @ 2, ",", Color3Form @ 3}]]],
  plotCube[r == 1, {2, 1, 1}, $Teal, 115, Ticks -> {None, None, Style[TopLeft, $Red]}],
  Spacings -> 30
];

NamedDiagram["ClassicalArrays/CubeAggregations123"] :=
SpacedRow[
  plotCube[True, {2, 2, 4}, $Gray, 150],
  Pane @ Labeled[MakeArrow[50, 10, 2, $Gray], Subscript["sum", Row[{Color1Form @ 1, ",", Color2Form @ 2, ",", Color3Form @ 3}]]],
  NeutralGraphics3D[InsetCubeGrid[List @ List @ List @ "", CubeStyle -> EdgeForm[None], $insetCubeOpts], ImageSize -> 50],
  Spacings -> 30
];

NamedDiagram["ClassicalArrays/MatrixAggregationFull"] :=
DerivedArraySignatureForm[Subscript["sum", 2]["M"]] @
ArrayCircuitGraphics @
DerivedArrayNode[1,
  NodeColumn[
    {
      KeyPortSkeleton[1, 1, NodeAlias -> "S"],
      MultisetNode["MS", 1,
        ClassicalArrayNode["M", 2, 2],
        {KeyWire[1, "M" -> 2], ValueWire["M", 1]}
      ],
      SumNode[]
    },
    Spacings -> .8
  ],
  {KeyWire[1, "M" -> 1],
   KeyWire["S" -> 1, "MS" -> 1, LineDashing -> True], ValueWire["MS", "sum", LineDashing -> True],
   ValueWire["sum", 1]
  }
];

NamedDiagram["ClassicalArrays/PortDashedWire"] :=
ArrayCircuitGraphics @
ManualNodeLayout[{
  {1, 0} -> KeyPortSkeleton[1, 1, NodeAlias -> "S"]},
  {1, 1},
  Epilog -> {KeyWire[{1, 0}, {1, -1.2}, LineDashing -> True],
    Style[FadingStripe[AbsoluteOffset[{0, -.7}] @ NodeSide["S", Bottom], {0, -.5}, 4], ZOrder -> 10]
  }
];

NamedDiagram["ClassicalArrays/AggregationInterior"] :=
ArrayCircuitGraphics @
NodeColumn[
  {
    KeyPortSkeleton[1, 1, NodeAlias -> "S", PortSize -> 0, PortColor -> Transparent],
    MultisetNode["MS", 1,
      ClassicalArrayNode["", 1, 1, NodeAlias -> "V"],
      {KeyWire[1, "V" -> 1], ValueWire["V", 1]}
    ],
    KeyPortSkeleton[1, 1, NodeAlias -> "T", PortSize -> 0, PortColor -> Transparent]
  },
  Epilog -> {
    KeyWire["S", "MS", LineDashing -> True],
    ValueWire["MS", NodeOutPort["T",1], LineDashing -> True],
    Style[{
      FadingStripe[AbsoluteOffset[{0, .5}] @ NodeSide["MS", Top],    {0, .5}, 4],
      FadingStripe[AbsoluteOffset[{0, -.5}] @ NodeSide["MS", Bottom], {0, -.5}, 4]},
      ZOrder -> 10
    ]
  },
  Spacings -> 1
];

NamedDiagram["ClassicalArrays/AggregationPartialSugar"] :=
ArrayCircuitGraphics @
DerivedArrayNode[1,
  AggregationNode["agg", 1 -> "sum",
    ClassicalArrayNode["A", 2, 1.5],
    {KeyWire[1, "A" -> 2], ValueWire["A", 1]}
  ],
  {KeyWire[1, "A" -> 1], ValueWire["agg", 1]}
];

NamedDiagram["ClassicalArrays/AggregationFullSugar"] :=
ArrayCircuitGraphics @
DerivedArrayNode[1,
  ClassicalArrayNode["A", {1, Style[2, "Disk"]} -> {Style[1, Labeled["OuterDiamond", Style["sum", Bold], Above]]}, 1.5],
  {KeyWire[1, "A" -> 1], ValueWire["A", 1]}
];

NamedDiagram["ClassicalArrays/AggregationSyntacticSugarRow"] :=
SpacedRow[
  Part[NamedDiagram["ClassicalArrays/MatrixAggregationFull"], 1],
  LargeSymbolForm["\[Congruent]"],
  NamedDiagram["ClassicalArrays/AggregationPartialSugar"],
  LargeSymbolForm["\[Congruent]"],
  NamedDiagram["ClassicalArrays/AggregationFullSugar"]
];

NamedDiagram["ClassicalArrays/Merge"] :=
DerivedArraySignatureForm[Subsuperscript["sum", 1, "R"]["V"]] @
ArrayCircuitGraphics @
DerivedArrayNode[1,
  NodeColumn[
    {
      ClassicalRelationNode["R", 2, {1.2, 1.25}],
      MultisetNode["MS", 1,
        ClassicalArrayNode["V", 1, {1.5, 1.25}],
        {KeyWire[1, "V"], ValueWire["V", 1]}
      ],
      SumNode[]
    },
    Spacings -> .75
  ],
  {
    KeyWire[1, NodePort["R", 1]],
    KeyWire[NodePort["R", 2], "MS", LineDashing -> True], ValueWire["MS", "sum", LineDashing -> True],
    ValueWire["sum", 1]
  }
];

NamedDiagram["ClassicalArrays/BroadcastV12"] :=
SpacedRow[
  DerivedArraySignatureForm[Superscript["V", IntegerString[3-#] <> "\[Rule]"]] @
  ArrayCircuitGraphics @ DerivedArrayNode[2,
    ClassicalArrayNode["V", 1, {1.25, 1.25}],
    {KeyWire[#, "V", BendRadius -> Infinity], ValueWire["V", 1]},
    FrameMargins -> {Top -> 1, All -> 0.75}, PortPositions -> Automatic
  ]& /@ {1, 2}
];

NamedDiagram["ClassicalArrays/BroadcastM13"] :=
DerivedArraySignatureForm[Superscript["M", "2\[RightArrow]"]] @
ArrayCircuitGraphics @
DerivedArrayNode[3,
  ClassicalArrayNode["M", 2, 1.5],
  {
    KeyWire[NodeInPort[$[1, 3]], "M" -> $[1, 2], BendRadius -> Infinity],
    ValueWire["M", 1]
  },
  FrameMargins -> {Top -> 1, All -> 0.75}, PortPositions -> Automatic
];

NamedDiagram["ClassicalArrays/UVDot"] :=
DerivedArraySignatureForm["U ⋅ V"] @
ArrayCircuitGraphics @
DerivedArrayNode[{},
  AggregationNode["agg", 1 -> "sum",
    NodeGrid[
      {
        {1,1} -> ClassicalArrayNode["U", 1, 1],
        {1,2} -> ClassicalArrayNode["V", 1, 1],
        {2,1;;2} -> ScalarTimesNode[]
      },
      FrameMargins -> Top -> .4
    ],
    {KeyWire[1, {"U", "V"}],
     ValueWire[$["UV"] -> 1, NodeSide["*", $[Left, Right]], JoinStyle -> Below],
     ValueWire[NodeSide["*", Bottom], 1]}
  ],
  {ValueWire["agg", 1]},
  FrameMargins -> {Bottom -> .85, All -> 0.75}
];

NamedDiagram["ClassicalArrays/UVTensor"] :=
DerivedArraySignatureForm["M \[CircleTimes] N"] @
ArrayCircuitGraphics @
DerivedArrayNode[2,
  NodeGrid[
    {
      {1,1} -> ClassicalArrayNode["U", 1, 1],
      {1,2} -> ClassicalArrayNode["V", 1, 1],
      {2,1;;2} -> ScalarTimesNode[]
    }
  ],
  {KeyWire[1, "U" -> 1], KeyWire[2, "V"],
   ValueWire[NodeOutPort[$ @ "UV", 1], NodeSide["*", $[Left, Right]], JoinStyle -> Below],
   ValueWire[NodeSide["*", Bottom], 1]},
  FrameMargins -> {Bottom -> .6, All -> 0.75}
];

NamedDiagram["ClassicalArrays/MNMatrixMultiplication"] :=
DerivedArraySignatureForm["M ⋅ N"] @
ArrayCircuitGraphics @
DerivedArrayNode[
  {1, 3},
  AggregationNode["agg", 1 -> "sum",
    NodeGrid[
      {
        {1,1} -> ClassicalArrayNode["M", 2, 1.5],
        {1,2} -> ClassicalArrayNode["N", 2, 1.5],
        {2,1;;2} -> ScalarTimesNode[]
      },
      FrameMargins -> Top -> .4
    ],
    {KeyWire[1, {"M" -> 2, "N" -> 1}],
     ValueWire[$["MN"] -> 1, NodeSide["*", $[Left, Right]], JoinStyle -> Below],
     ValueWire[NodeSide["*", Bottom], 1]}
  ],
  {KeyWire[1, "M"->1], KeyWire[3, "N" -> 2], ValueWire["agg", 1]},
  FrameMargins -> {Bottom -> .85, All -> 0.75}
];

NamedDiagram["ClassicalArrays/MNMatrixMultiplicationExpanded"] :=
ArrayCircuitGraphics @
DerivedArrayNode[2,
  NodeColumn[
    {
      KeyPortSkeleton[1, 1, NodeAlias -> "S"],
      MultisetNode["MS", 1,
        NodeGrid[
          {
            {1,1} -> ClassicalArrayNode["M", 2, 1.5],
            {1,2} -> ClassicalArrayNode["N", 2, 1.5],
            {2,1;;2} -> ScalarTimesNode[]
          },
          FrameMargins -> {Top -> .4, Bottom -> -0.2}
        ],
        {
          KeyWire[1, {"M" -> 2, "N" -> 1}],
          ValueWire[$["MN"] -> 1, NodeSide["*", $[Left, Right]], JoinStyle -> Below],
          ValueWire["*", 1]
        }
      ],
      SumNode[]
    },
    Spacings -> .8
  ],
  {KeyWire[NodeInPort @ 1, "M" -> 1],
   KeyWire[NodeInPort @ 2, "N" -> 2],
   KeyWire["S", "MS", LineDashing -> True],
   ValueWire["MS", "sum", LineDashing -> True],
   ValueWire["sum", 1]}
];

NamedDiagram["ClassicalArrays/MTranspose"] :=
DerivedArraySignatureForm[Superscript["M", "(1,2)"]] @
ArrayCircuitGraphics @
DerivedArrayNode[2,
  ClassicalArrayNode["M", 2, {2, 1.5}],
  {
    KeyWire[NodeInPort[$[1,2]], "M" -> $[2,1], ShortcutRadius -> .3],
    ValueWire["M", 1]
  },
  FrameMargins -> {Top -> 1, All -> 0.75}, PortPositions -> Automatic, PortSpacing -> .8
];

NamedDiagram["ClassicalArrays/ITranspose"] :=
DerivedArraySignatureForm[Superscript["I", "(1,2,3)"]] @
ArrayCircuitGraphics @
DerivedArrayNode[3,
  ClassicalArrayNode["I", 3, {2.5, 2}],
  {
    KeyWire[NodeInPort[$[1,2,3]], "I" -> $[2,3,1], ShortcutRadius -> .3],
    ValueWire["I", 1]
  },
  FrameMargins -> {Top -> 1, All -> 0.75}, PortPositions -> Automatic, PortSpacing -> .8
];

NamedDiagram["ClassicalArrays/GenericTranspose"] :=
DerivedArraySignatureForm[Superscript["A", "\[Sigma]"]] @
ArrayCircuitGraphics @
DerivedArrayNode[1,
  NodeColumn[
    {
      KeyFunctionNode[Style["\[Sigma]", FontSize -> 20], 1, {1.5, 1.5}, NodeAlias -> "s"],
      ClassicalArrayNode["A", 1, {1.5, 1.5}]
    },
    Spacings -> .75
  ],
  {
    KeyWireBundle[1, "s"],
    KeyWireBundle["s", "A"],
    ValueWire["A", 1]
  }
];

NamedDiagram["ClassicalArrays/Nesting"] :=
DerivedArraySignatureForm[Superscript["M", "2\[Succeeds]"]] @
ArrayCircuitGraphics @
DerivedBubbleOutFunctionNode[{1} -> {1},
  BubbleValueNode["B", {2},
    ClassicalArrayNode["M", 2, 1.5],
    {KeyWire[NodeInPort[2], NodeInPort["M", 2]],
     ValueWire["M", 1]},
    PortPositions -> "MatchInterior"
  ],
  {KeyWire[NodeInPort[1], NodeInPort["M", 1]],
   OffsetBubbleWire[-0.5, "B", NodeOutPort[1]]},
  FrameMargins -> {Top -> .75, All -> 0.75, Bottom -> 0.9}
];

NamedDiagram["ClassicalArrays/Unnesting"] :=
DerivedArraySignatureForm[Superscript["V", "2\[Precedes]"]] @
ArrayCircuitGraphics @
DerivedArrayNode[2,
  NodeColumn[
    {
      NodeRow[{ClassicalArrayNode["V", 1 -> {Style[1, $BubbleWireColor]}, {1.25, 1.25}], Spacer[{1,0}]}],
      NodeRow[{Spacer[{1,0}], BubbleValueNode["B", 1, {1.25, 1.25}, RoundingRadius -> .4]}]
    },
    Spacings -> .5
  ],
  {
    KeyWire[1, "V"],
    KeyWire[2, "B"],
    BubbleWire["V", NodeSide["B", Left], JoinStyle -> Below],
    ValueWire["B", 1]
  },
  PortSpacing -> 1.5
];

$innerBubble = BubbleValueNode[
  "B", 1 -> {1, Style[2, $BubblePortColor]},
  ClassicalArrayNode["M", 2, 1.5],
  {KeyWire[1, "M" -> 2], ValueWire["M", 1]},
  PortPositions -> {In -> "MatchInterior", Out -> {0, .8}}
];

NamedDiagram["ClassicalArrays/NestingUnnestingSubstitution"] :=
DerivedArraySignatureForm[Row[{"(", Superscript["M", "2\[Succeeds]"], Superscript[")", "2\[Precedes]"]}]] @
ArrayCircuitGraphics @
DerivedArrayNode[2,
  NodeColumn[
    {
      NodeRow[{$innerBubble, Spacer[{1,0}]}],
      NodeRow[{Spacer[{2.5,0}], BubbleValueNode["B2", 1, {1.5, 1.5}, RoundingRadius -> .5]}]
    },
    Epilog -> BubbleWire[NodeOutPort["B", 2], NodeSide["B2", Left], JoinStyle -> Below],
    Spacings -> .5, FrameMargins -> 0
  ],
  {KeyWire[1, "M" -> 1], KeyWire[2, "B2" -> 1], ValueWire["B2", 1]},
  FrameMargins -> {Horizontal -> 0.5, Vertical -> 0.75}
];

NamedDiagram["ClassicalArrays/NestingUnnestingEvaluation"] :=
DerivedArraySignatureForm[Row[{"(", Superscript["M", "2\[Succeeds]"], Superscript[")", "2\[Precedes]"]}]] @
ArrayCircuitGraphics @
DerivedArrayNode[2,
  BubbleValueNode[
    "B", 1,
    ClassicalArrayNode["M", 2, 1.5],
    {KeyWire[1,"M" -> 2], ValueWire["M", 1]},
    PortPositions -> "MatchInterior"
  ],
  {KeyWire[1, "M" -> 1], KeyWire[2, "B" -> 1], ValueWire["B", 1]},
  FrameMargins -> {Horizontal -> 0.5, Vertical -> 0.75}
];

NamedDiagram["ClassicalArrays/NestingUnnestingResult"] :=
DerivedArraySignatureForm[Row[{"(", Superscript["M", "2\[Succeeds]"], Superscript[")", "2\[Precedes]"]}]] @
ArrayCircuitGraphics @
DerivedArrayNode[2,
  ClassicalArrayNode["M", 2, 1.5],
  {
    KeyWire[1, "M" -> 1],
    KeyWire[2, "M" -> 2],
    ValueWire["M", 1]
  },
  FrameMargins -> {Horizontal -> 0.75, Vertical -> 0.75}
];

NamedDiagram["ClassicalArrays/MDiagonal"] :=
DerivedArraySignatureForm[Superscript["M", "1:(1,2)"]] @
ArrayCircuitGraphics @
DerivedArrayNode[1,
  ClassicalArrayNode["M", 2, 1.5],
  {KeyWire[1, {"M" -> 1, "M" -> 2}, SegmentPosition -> 0.45], ValueWire["M", 1]},
  FrameMargins -> {All -> 0.65, Top -> .9}
]

$diagWireOpts = Sequence[ShortcutRadius -> 0.5, BendRadius -> Infinity, LineThickness -> 4, LineEdging -> False];
$rainbowDiagWireOpts = Sequence[SegmentPosition -> 0.4, LineThickness -> 4, LineEdging -> False, Setback -> {0, .05}];

$imgArrayOpts = Sequence[
  CubeGap -> 0.15,
  FrameStyle -> Directive[AbsoluteThickness[3], GrayLevel[0.2,.5]], CubeStyle -> EdgeForm @ AbsoluteThickness[1.5],
  ColorRules -> {1 -> $Red, 2 -> $Green, 3 -> $Blue, 4 -> $Red},
  FlipAxes -> {1, 2, 3},
  Ticks -> {Style[Right, $Pink, Bold], Style[TopRight, $DarkTeal, Bold], Style[TopLeft, $DarkPurple, Bold]},
  TickSpacing -> .3, FlipTicks -> {1,2,3}, TicksStyle -> 25
];

plotCube[body_, n_, col_, opts___Rule] := NeutralGraphics3D[
  ColoredCubeGrid[Boole @ Table[body,{r,n},{b,n},{g,n}], ColorRules -> {1 -> col},
  Ticks -> {Style[Right, $Green], Style[TopRight, $Blue], Style[TopLeft, $Red]},
  FrameStyle -> Directive[AbsoluteThickness[2], GrayLevel[0.2,.5]],
  MeshStyle -> Directive[AbsoluteThickness[2], GrayLevel[0.5,0.1]], $imgArrayOpts], opts, ImageSize -> 200];


NamedDiagram["ClassicalArrays/ADiagonal123"] :=
DerivedArraySignatureForm[Superscript["A", RawBoxes @ RBox[GrayBox @ 1, ":", RBox["(", RedBox @ 1, ",", BlueBox @ 2, ",", GreenBox @ 3, ")"]]]] @
ArrayCircuitGraphics @ DerivedRainbowArrayNode[{7},
  RainbowArrayNode["A", {1, 2, 3}, {2.5, 1.5}, PortSpacing -> 0.75],
  {RainbowWire[7, {"A" -> 1,  "A" -> 2, "A" -> 3}, $rainbowDiagWireOpts], ValueWire["A", 1]},
  PortPositions -> Automatic, FrameMargins -> {All -> 0.65, Top -> .8}
];

NamedDiagram["ClassicalArrays/ADiagonal123Cubes"] :=
  plotCube[r == g == b, 3, $Gray];


NamedDiagram["ClassicalArrays/ADiagonal23"] :=
DerivedArraySignatureForm[Superscript["A", RawBoxes @ RBox[TealBox @ 2, ":", RBox["(", BlueBox @ 2, ",", GreenBox @ 3, ")"]]]] @
ArrayCircuitGraphics @ DerivedRainbowArrayNode[{1, 6},
  RainbowArrayNode["A", {1, 2, 3}, {2.5, 1.5}, PortSpacing -> 0.75],
  {RainbowWire[1, "A" -> 1, $diagWireOpts],
   RainbowWire[6, {"A" -> 2, "A" -> 3}, $rainbowDiagWireOpts],
   ValueWire["A", 1]},
  PortPositions -> Automatic, FrameMargins -> {All -> 0.65, Top -> .8}
];

NamedDiagram["ClassicalArrays/ADiagonal23Cubes"] :=
  plotCube[g == b, 3, $Teal];


NamedDiagram["ClassicalArrays/ADiagonal13"] :=
DerivedArraySignatureForm[Superscript["A", RawBoxes @ RBox[OrangeBox @ 2, ":", RBox["(", RedBox @ 1, ",", GreenBox @ 3, ")"]]]] @
ArrayCircuitGraphics @ DerivedRainbowArrayNode[{2, 4},
  RainbowArrayNode["A", {1, 2, 3}, {2.5, 1.5}, PortSpacing -> 0.75],
  {RainbowWire[4, {"A" -> 1, "A" -> 3}, 4, $rainbowDiagWireOpts],
   RainbowWire[2, "A" -> 2, $diagWireOpts],
   ValueWire["A", 1]},
  PortPositions -> Automatic, FrameMargins -> {All -> 0.65, Top -> .8}
];

NamedDiagram["ClassicalArrays/ADiagonal13Cubes"] :=
  plotCube[r == g, 3, $Orange]


NamedDiagram["ClassicalArrays/ADiagonal12"] :=
DerivedArraySignatureForm[Superscript["A", RawBoxes @ RBox[PurpleBox @ 1, ":", RBox["(", RedBox @ 1, ",", BlueBox @ 2, ")"]]]] @
ArrayCircuitGraphics @ DerivedRainbowArrayNode[{5, 3},
  RainbowArrayNode["A", {1, 2, 3}, {2.5, 1.5}, PortSpacing -> 0.75],
  {RainbowWire[3, "A" -> 3, $diagWireOpts],
   RainbowWire[5, {"A" -> 1, "A" -> 2}, $rainbowDiagWireOpts],
   ValueWire["A", 1]},
  PortPositions -> Automatic, FrameMargins -> {All -> 0.65, Top -> .8}
];

NamedDiagram["ClassicalArrays/ADiagonal12Cubes"] :=
  plotCube[r == b, 3, $Purple];


NamedDiagram["ClassicalArrays/Pick"] :=
DerivedArraySignatureForm["A[P]"] @
ArrayCircuitGraphics @
DerivedArrayNode[1,
  NodeColumn[
    {
      KeyFunctionNode["P", 1, 1.5],
      ClassicalArrayNode["A", 1, 1.5]
    },
    Spacings -> .75
  ],
  {
    KeyWireBundle[1, "P"],
    KeyWireBundle["P", "A"],
    ValueWire["A", 1]
  },
  FrameMargins -> .75
]