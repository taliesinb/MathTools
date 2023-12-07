$timesNodeGridOpts = Sequence[RowSpacings -> .5, FrameMargins -> {Top -> .4, Bottom -> -0.2}, RowAlignments -> Top];

NamedDiagram["RainbowArrays/ClassicalMPlusV"] :=
ArrayCircuitGraphics @ DerivedArrayNode[2,
  NodeGrid[{
    {1,1} -> ClassicalArrayNode["M", 2, 1.5],
    {1,2} -> ClassicalArrayNode["V", 1, 1],
    {2,1;;2} -> ScalarPlusNode[]
  }, $timesNodeGridOpts],
  {
    KeyWire[1, "M" -> 1],
    KeyWire[2, {"M" -> 2, "V" -> 1}],
    ValueWire[$["MV"] -> 1, NodeSide["+", $[Left, Right]], JoinStyle -> Below],
    ValueWire[NodeSide["+", Bottom], 1]
  }
];

NamedDiagram["RainbowArrays/MPlusV"] :=
ArrayCircuitGraphics @
DerivedRainbowArrayNode[{1, 2},
  NodeGrid[
    {{1,1} -> RainbowArrayNode["M", {1,2}, 1.5],
     {1,2} -> RainbowArrayNode["V", {2}, 1],
     {2,1;;2} -> ScalarPlusNode[]},
    $timesNodeGridOpts
  ],
  {RainbowWire[1, "M" -> 1],
   RainbowWire[2, {"M" -> 2, "V" -> 2}],
   ValueWire[$["MV"] -> 1, NodeSide["+", $[Left, Right]], JoinStyle -> Below],
   ValueWire[NodeSide["+", Bottom], 1]}
];

NamedDiagram["RainbowArrays/ASum"] :=
DerivedArraySignatureForm["\!\(\*SubscriptBox[\"sum\", StyleBox[\"\[FilledDiamond]\",FontColor:>CurrentValue[{StyleDefinitions, \"Color2\", FontColor}]]]\)", {"A" -> {"\[Ellipsis]",2}}, "\[Ellipsis]"] @
ArrayCircuitGraphics @
DerivedRainbowArrayNode[{0},
  RainbowAggregationNode["sum", {2}->"sum",
    RainbowArrayNode["A", {0, 2}, 1.5],
    {RainbowWire[2, "A" -> 2], ValueWire["A", 1]}
  ],
  {RainbowWireBundle[0, "A" -> 0], ValueWire["sum", 1]}
];

NamedDiagram["RainbowArrays/STimesV"] :=
DerivedArraySignatureForm["\!\(\*SuperscriptBox[\(*\), \(\[UpArrow]\)]\)", {"S" -> {}, "V" -> {1}}, {1}] @
ArrayCircuitGraphics @
DerivedRainbowArrayNode[1,
  NodeGrid[
    {{1,1} -> RainbowArrayNode["S", {}, .75],
     {1,2} -> RainbowArrayNode["V", {1}, 1],
     {2,1;;2} -> ScalarTimesNode[]},
    FrameMargins -> {Vertical -> -0.33, Horizontal -> -0.2}, $timesNodeGridOpts
  ],
  {RainbowWire[1, "V" -> 1, 1],
   ValueWire[NodeOutPort[$ @ "SV", 1], NodeSide["*", $[Left, Right]], JoinStyle -> Below],
   ValueWire[NodeSide["*", Bottom], NodeOutPort[1]]},
  PortSpacing -> 1.25,
  FrameMargins -> {All -> .75, Top -> 1}
]

NamedDiagram["RainbowArrays/STimesM"] :=
DerivedArraySignatureForm["\!\(\*SuperscriptBox[\(*\), \(\[UpArrow]\)]\)", {"S" -> {}, "M" -> {1, 2}}, {1, 2}] @
ArrayCircuitGraphics @
DerivedRainbowArrayNode[{1,2},
  NodeGrid[
    {{1,1} -> RainbowArrayNode["S", {}, .75],
     {1,2} -> RainbowArrayNode["M", {1,2}, 1.5],
     {2,1;;2} -> ScalarTimesNode[]},
    FrameMargins -> {Vertical -> -0.33, Horizontal -> -0.2}, $timesNodeGridOpts],
  {RainbowWire[1, "M" -> 1], RainbowWire[2, "M" -> 2],
   ValueWire[NodeOutPort[$ @ "SM", 1], NodeSide["*", $[Left, Right]], JoinStyle -> Below],
   ValueWire[NodeSide["*", Bottom], NodeOutPort[1]]},
  FrameMargins -> {All -> .75, Top -> 1}
];

NamedDiagram["RainbowArrays/MTimesN2Shared"] :=
DerivedArraySignatureForm["\!\(\*SuperscriptBox[\(*\), \(\[UpArrow]\)]\)", {"M" -> {1, 2}, "N" -> {1, 2}}, {1, 2}] @
ArrayCircuitGraphics @
DerivedRainbowArrayNode[{1,2},
  NodeGrid[
    {{1,1} -> RainbowArrayNode["M", {1, 2}, 1.5],
     {1,2} -> RainbowArrayNode["N", {1, 2}, 1.5],
     {2,1;;2} -> ScalarTimesNode[]},
    FrameMargins -> {Bottom -> -0.33, Horizontal -> -0.1, Top -> 0.4}, $timesNodeGridOpts
  ],
  {{RainbowWire[1, "M" -> 1], RainbowWire[2, "M" -> 2],
    RainbowWire[1, "N" -> 1], RainbowWire[2, "N" -> 2]} // ApplyWireOptions[ShortcutRadius -> .4],
   ValueWire[NodeOutPort[$ @ "MN", 1], NodeSide["*", $[Left, Right]], JoinStyle -> Below],
   ValueWire[NodeSide["*", Bottom], NodeOutPort[1]]},
  FrameMargins -> {All -> .75, Top -> 1}
];

NamedDiagram["RainbowArrays/MTimesN1Shared"] :=
DerivedArraySignatureForm["\!\(\*SuperscriptBox[\(*\), \(\[UpArrow]\)]\)", {"M" -> {1,2}, "N" -> {2, 3}}, {1, 2, 3}] @
ArrayCircuitGraphics @
DerivedRainbowArrayNode[{1,2,3},
  NodeGrid[
    {{1,1} -> RainbowArrayNode["M", {1, 2}, 1.5],
     {1,2} -> RainbowArrayNode["N", {2, 3}, 1.5],
     {2,1;;2} -> ScalarTimesNode[]},
    FrameMargins -> {Bottom -> -0.33, Horizontal -> -0.1, Top -> 0.4}, $timesNodeGridOpts
  ],
  {RainbowWire[1, "M" -> 1], RainbowWire[2, {"M" -> 2, "N" -> 2}], RainbowWire[3, "N" -> 3],
   ValueWire[$["MN"] -> 1, NodeSide["*", $[Left, Right]], JoinStyle -> Below],
   ValueWire[NodeSide["*", Bottom], 1]},
  FrameMargins -> {All -> .75, Top -> 1}
];

NamedDiagram["RainbowArrays/MTimesN0Shared"] :=
DerivedArraySignatureForm["\!\(\*SuperscriptBox[\(*\), \(\[UpArrow]\)]\)", {"M" -> {1, 2}, "N" -> {3, 4}}, {1, 2, 3, 4}] @
ArrayCircuitGraphics @
DerivedRainbowArrayNode[{1,2,3,4},
  NodeGrid[
    {{1,1} -> RainbowArrayNode["M", {1, 2}, 1.5],
     {1,2} -> RainbowArrayNode["N", {3, 4}, 1.5],
     {2,1;;2} -> ScalarTimesNode[]},
    FrameMargins -> {Bottom -> -0.33, Horizontal -> -0.1}, $timesNodeGridOpts
  ],
  {RainbowWire[1, "M" -> 1], RainbowWire[2, "M" -> 2],
   RainbowWire[3, "N" -> 3], RainbowWire[4, "N" -> 4],
   ValueWire[NodeOutPort[$ @ "MN", 1], NodeSide["*", $[Left, Right]], JoinStyle -> Below],
   ValueWire[NodeSide["*", Bottom], NodeOutPort[1]]},
  FrameMargins -> {All -> .75, Top -> 1}
];

NamedDiagram["RainbowArrays/MatMul"] :=
DerivedArraySignatureForm["matmul", {"M" -> {1,2}, "N" -> {2,3}}, {1,3}] @
ArrayCircuitGraphics @
DerivedRainbowArrayNode[{1, 3},
  RainbowAggregationNode["sum", {2} -> "sum",
    NodeGrid[
      {
        {1,1} -> RainbowArrayNode["M", {1, 2}, 1.5],
        {1,2} -> RainbowArrayNode["N", {2, 3}, 1.5],
        {2,1;;2} -> ScalarTimesNode[]
      },
      FrameMargins -> {Top -> .4}
    ],
    {RainbowWire[2, {"M" -> 2, "N" -> 2}],
     ValueWire[NodeOutPort[$ @ "MN", 1], NodeSide["*", $[Left, Right]], JoinStyle -> Below],
     ValueWire[NodeSide["*", Bottom], NodeOutPort[1]]}
  ],
  {ValueWire["sum", 1],
   RainbowWire[1, "M" -> 1],
   RainbowWire[3, "N" -> 3]}
];

NamedDiagram["RainbowArrays/MatMulTensor"] :=
DerivedArraySignatureForm["matmul", {"M" -> {1,2}, "N" -> {2,3}}, {1,3}] @
ArrayCircuitGraphics @ DerivedRainbowTensorNode[{1,3},
  NodeGrid[{
    {1,1} -> RainbowTensorNode["M", {Top -> 1, Right -> 2}, 1.5],
    {1,2} -> RainbowTensorNode["N", {Top -> 3, Left -> 2}, 1.5]
  }],
  {RainbowWire[1, NodePort["M", 1]],
   RainbowWire[3, NodePort["N", 3]],
   RainbowWire[NodePort["M", 2], NodePort["N", 2]]}
];

NamedDiagram["RainbowArrays/VecDot"] :=
DerivedArraySignatureForm["vecdot", {"U" -> {1}, "V" -> {1}}, {}] @
ArrayCircuitGraphics[
  DerivedRainbowArrayNode[{},
    RainbowAggregationNode["sum", {1} -> "sum",
      NodeGrid[
        {
          {1,1} -> RainbowArrayNode["U", {1}, 1],
          {1,2} -> RainbowArrayNode["V", {1}, 1],
          {2,1;;2} -> ScalarTimesNode[]
        },
        FrameMargins -> {Top -> .4}
      ],
      {RainbowWire[1, {"U" -> 1, "V" -> 1}],
       ValueWire[$["UV"] -> 1, NodeSide["*", $[Left, Right]], JoinStyle -> Below],
       ValueWire[NodeSide["*", Bottom], 1]}
    ],
    {ValueWire["sum", 1]}
  ]
];

NamedDiagram["RainbowArrays/VecDotTensor"] :=
DerivedArraySignatureForm["vecdot", {"U" -> {1}, "V" -> {1}}, {}] @
ArrayCircuitGraphics @ DerivedRainbowTensorNode[{},
  NodeGrid[{
    {1,1} -> RainbowTensorNode["U", {Right -> 1}, 1.1],
    {1,2} -> RainbowTensorNode["V", {Left -> 1}, 1.1]
  }],
  {RainbowWire[NodePort["U", 1], NodePort["V", 1]]}
];

NamedDiagram["RainbowArrays/VADot"] :=
DerivedArraySignatureForm["dot", {"V" -> {1}, "A" -> {1,2,3}}, {2,3}] @
ArrayCircuitGraphics @
DerivedRainbowArrayNode[{2, 3},
  RainbowAggregationNode["sum", 1 -> "sum",
    NodeGrid[
      {
        {1,1} -> RainbowArrayNode["V", {1}, 1],
        {1,2} -> RainbowArrayNode["A", {1, 2, 3}, 2, PortSpacing -> 0.66],
        {2,1;;2} -> ScalarTimesNode[]
      },
      FrameMargins -> {Top -> .4}, RowAlignments -> Top
    ],
    {RainbowWire[1, {"V" -> 1, "A" -> 1}],
     ValueWire[$["VA"] ->  1, NodeSide["*", $[Left, Right]], JoinStyle -> Below],
     ValueWire[NodeSide["*", Bottom], 1]}
  ],
  {RainbowWire[2, "A" -> 2], RainbowWire[3, "A" -> 3],
   ValueWire["sum", 1]}
];

NamedDiagram["RainbowArrays/VADotTensor"] :=
ArrayCircuitGraphics @
DerivedRainbowTensorNode[{2,3},
  NodeGrid[
    {{2,1} -> RainbowTensorNode["V", {Top -> 1}, 1, PortSpacing -> 0.15],
     {1,1} -> RainbowTensorNode["A", {-0.08 -> 2, 0.08 -> 3, Bottom -> 1}, 1.5]},
    FrameMargins -> {Top -> 0.5}
  ],
  {RainbowWire[NodeInPort @ $[2, 3], NodePort["A", $[2,3]], BendRadius -> Infinity, JoinStyle -> {Vertical, $[{-1/2,1}, {1/2, 1}]}],
   RainbowWire[NodePort["V", 1], NodePort["A", 1]]},
  PortSpacing -> 1.2, PortPositions -> Automatic
];

NamedDiagram["RainbowArrays/GeneralizedDotTensor"] :=
DerivedArraySignatureForm["dot", {"A" -> {All -> 5, All -> 8}, "B\[VeryThinSpace]" -> {All -> 8,All->6}}, {All->5, All -> 6}] @
ArrayCircuitGraphics @
DerivedRainbowTensorNode[{"A","B"},
  NodeGrid[
    {{1,1} -> RainbowTensorNode["A", {Top -> "A", Right -> "AB"}, 1.5],
     {1,2} -> RainbowTensorNode["B", {Top -> "B", Left -> "AB"}, 1.5]}
  ],
  {RainbowWireBundle[NodeInPort["A"], NodePort["A", "A"], "A"],
   RainbowWireBundle[NodeInPort["B"], NodePort["B", "B"], "B"],
   RainbowWireBundle[NodePort["A", "AB"], NodePort["B", "AB"], "AB"]},
  NodePalette -> {"A" -> 5, "B" -> 6, "AB" -> 8}
];

NamedDiagram["RainbowArrays/ContractionTensor"] :=
DerivedArraySignatureForm[ColorNForm[8] @ "\[CircleDot]", {"A" -> {All -> 8}, "B\[VeryThinSpace]" -> {All -> 8}}, {}] @
ArrayCircuitGraphics @
DerivedRainbowTensorNode[{},
  NodeGrid[
    {{1,1} -> RainbowTensorNode["A", {Right -> "AB"}, 1],
     {1,2} -> RainbowTensorNode["B", {Left -> "AB"}, 1]}
  ],
  {RainbowWire[NodePort["A", "AB"], NodePort["B", "AB"], "AB"]},
  NodePalette -> {"A" -> 5, "B" -> 6, "AB" -> 8}
];

NamedDiagram["RainbowArrays/GenericContractionTensor"] :=
DerivedArraySignatureForm[ColorNForm[8] @ "\[CircleDot]", {"A" -> {All -> 5, All -> 8}, "B\[VeryThinSpace]" -> {All -> 8, All->6}}, {All->5, All -> 6}] @
ArrayCircuitGraphics @
DerivedRainbowTensorNode[{"A","B"},
  NodeGrid[
    {{1,1} -> RainbowTensorNode["A", {Top -> "A", Right -> "AB"}, 1.5],
     {1,2} -> RainbowTensorNode["B", {Top -> "B", Left -> "AB"}, 1.5]}
  ],
  {RainbowWireBundle[NodeInPort["A"], NodePort["A", "A"], "A"],
   RainbowWireBundle[NodeInPort["B"], NodePort["B", "B"], "B"],
   RainbowWire[NodePort["A", "AB"], NodePort["B", "AB"], "AB"]},
  NodePalette -> {"A" -> 5, "B" -> 6, "AB" -> 8}
];

NamedDiagram["RainbowArrays/MapF"] :=
DerivedArraySignatureForm["\!\(\*SuperscriptBox[\(f\), \(\[UpArrow]\)]\)", {"A" -> "\!\(\*SubscriptBox[\(K\), \(1\)]\)"->1, "B" -> "\!\(\*SubscriptBox[\(K\), \(2\)]\)"-> 2, "C" -> "\!\(\*SubscriptBox[\(K\), \(3\)]\)"->3}, {"\!\(\*SubscriptBox[\(K\), \(1\)]\)"->1,"\!\(\*SubscriptBox[\(K\), \(2\)]\)"-> 2,"\!\(\*SubscriptBox[\(K\), \(3\)]\)"->3}] @
ArrayCircuitGraphics @
DerivedRainbowArrayNode[3,
  NodeGrid[
    {{1,1} -> RainbowArrayNode["A", {1}],
     {1,2} -> RainbowArrayNode["B", {2}],
     {1,3} -> RainbowArrayNode["C", {3}],
     {2,1;;3} -> ScalarFunctionNode["f", 3, PortSpacing->1.5]}
  ],
  {RainbowWire[NodeInPort[$[3]], NodeInPort[$["ABC"], $[3]]],
   ValueWire[$["ABC"] -> 1, "f" -> $[3]],
   ValueWire["f" -> 1, 1]},
  FrameMargins -> {All -> .75, Top -> 1}
];

NamedDiagram["RainbowArrays/MapFColored"] :=
ArrayCircuitGraphics @
DerivedRainbowArrayNode[3,
  NodeColumn[
    {NodeRow[
      {RainbowArrayNode["A", {1} -> {Style[1,1]}, 1, NodeLabelColor -> 1],
       RainbowArrayNode["B", {2} -> {Style[2,2]}, 1, NodeLabelColor -> 2],
       RainbowArrayNode["C", {3} -> {Style[3,3]}, 1, NodeLabelColor -> 3]},
      Spacings -> 0.25],
     RainbowScalarFunctionNode["f", {1,2,3}, {3.5, 1}, PortSpacing->1.25]},
    Spacings -> 0.75],
  {RainbowWire[NodeOutPort[$["A","B","C"], $[1, 2, 3]], NodeInPort["f", $[1,2,3]]],
   RainbowWire[NodeInPort[$[1,2,3]], $["A","B","C"] -> $[1,2,3]],
   ValueWire["f" -> 1, 1]},
  PortSpacing -> 1.25,
  FrameMargins -> {All -> .75, Top -> 1}
];

NamedDiagram["RainbowArrays/Unnest"] :=
ArrayCiruitLabeled[Style[Superscript["M", BlueForm @ "\[Succeeds]"], 20]] @
ArrayCircuitGraphics @
DerivedBubbleOutFunctionNode["A" -> {1},
  BubbleValueNode["B", {"B"},
    RainbowArrayNode["M", {"A", "B"}, 1.5],
    {RainbowWire[NodeInPort["B"], NodeInPort["M", "B"]],
     RainbowWire[NodeInPort["M", "A"], AbsoluteOffset[{0,1.25}] @ NodeInPort["M", "A"]],
     ValueWire["M", 1]},
    PortPositions -> "MatchInterior"
  ],
  {RainbowWire[NodeInPort["A"], NodeInPort["M", "A"]],
   OffsetBubbleWire[-0.5, "B", NodeOutPort[1]]},
  NodePalette -> {"A" -> 1, "B" -> 2}
];

NamedDiagram["RainbowArrays/UnnestExterior"] :=
ArrayCiruitLabeled[Style[Superscript["M", BlueForm @ "\[Succeeds]"], 20]] @
ArrayCircuitGraphics @
DerivedBubbleOutFunctionNode["A" -> 1,
  {3, 3},
  {Text[Style["\[VerticalEllipsis]", 20], {1.5,-1.5}]},
  NodePalette -> {"A" -> 1, "B" -> 2}
];

NamedDiagram["RainbowArrays/UnnestInterior"] :=
ArrayCircuitGraphics @
BubbleValueNode["B", {"B"},
  RainbowArrayNode["M", {"A", "B"}, 1.5],
  {RainbowWire[NodeInPort["B"], NodeInPort["M", "B"]],
   RainbowWire[NodeInPort["M", "A"], AbsoluteOffset[{0,1.25}] @ NodeInPort["M", "A"]],
   ValueWire["M", 1], Style[FadingStripe[NodeSide["B",Top],{0,.44},1], ZOrder -> 12]},
  PortPositions -> "MatchInterior",
  NodePalette -> {"A" -> 1, "B" -> 2}
];

NamedDiagram["RainbowArrays/Perceptron"] :=
DerivedArraySignatureForm["SLP", {"I" -> {1}, "W" -> {1, 2}, "B" -> {2}}, {2}] @
ArrayCircuitGraphics @
DerivedRainbowArrayNode[{"b"} -> {1},
  NodeGrid[
    {
      {1, 1} ->
        DerivedRainbowTensorNode[{},
          NodeGrid[
            {{1,1} -> RainbowTensorNode["I", {Right -> "r"}, 1.1],
             {1,2} -> RainbowTensorNode["W", {Left -> "r", Top -> "b"}, 1.1]}
          ],
          {RainbowWire[NodePort["I", "r"], NodePort["W", "r"]]},
          NodePalette -> {"r" -> 1, "b" -> 2}, NodeAlias -> "IW",
          PortPositions -> {In -> {0}, Out -> {0.75}},
          FrameMargins -> .5
        ],
      {1, 2} ->
        Padded[RainbowArrayNode["B", {"b"}, 1], Left -> 0.1],
      {2, 1} ->
        Padded[ScalarPlusNode[], Left -> 1.5]
    },
    FrameMargins -> {Top -> 0.65, Bottom -> -0.23}
  ],
  {RainbowWire[NodeInPort["b"], NodePort["W", "b"]],
   RainbowWire[NeatCurve[{{3.4, -0.7}, AbsoluteOffset[{0, -.1}] @ NodePort["B", "b"]}, JoinStyle -> Above, SegmentPosition -> 0], LineColor -> $Blue],
   ValueWire[NodeOutPort["IW", 1], NodeSide["+", Top]],
   ValueWire[NeatCurve[{NodeSide["+", Right], NodeOutPort["B", 1]}, JoinStyle -> Below, SegmentPosition -> 0, Setback -> 0]],
   ValueWire[NodeSide["+", Bottom], NodeOutPort[1]]},
  PortPositions -> {In -> {0}, Out -> {-0.05}},
  NodePalette -> {"r" -> 1, "b" -> 2}
];

NamedDiagram["RainbowArrays/PerceptronFunction"] :=
DerivedArraySignatureForm["SLP", {"W" -> {1, 2}, "B" -> {2}}, ArrayShapeForm["I" -> {1}], ArrayShapeForm["O" -> {2}]] @
ArrayCircuitGraphics @
DerivedBubbleFunctionNode["I" -> {Style[1, $BubbleWireStyle]},
  BubbleValueNode["O", {"b"} -> {1},
    NodeGrid[
      {
        {1, 1} ->
          DerivedRainbowTensorNode[{},
            NodeGrid[
              {{1,1} -> RainbowTensorNode["I", {Right -> "r"}, 1.1],
               {1,2} -> RainbowTensorNode["W", {Left -> "r", Top -> "b"}, 1.1]}
            ],
            {RainbowWire[NodePort["I", "r"], NodePort["W", "r"]]},
            NodePalette -> {"r" -> 1, "b" -> 2}, NodeAlias -> "IW",
            PortPositions -> {In -> {0}, Out -> {0.75}},
            FrameMargins -> .5
          ],
        {1, 2} ->
          Padded[RainbowArrayNode["B", {"b"}, 1], Left -> 0.1],
        {2, 1} ->
          Padded[ScalarPlusNode[], Left -> 1.5]
      },
      FrameMargins -> {Top -> 0.65, Bottom -> -0.23}
    ],
    {RainbowWire[NodeInPort["b"], NodePort["W", "b"]],
     RainbowWire[NeatCurve[{{4.1, -1.45}, AbsoluteOffset[{0, -.1}] @ NodePort["B", "b"]}, JoinStyle -> Above, SegmentPosition -> 0, Setback -> 0.05], LineColor -> $Blue],
     ValueWire[NodeOutPort["IW", 1], NodeSide["+", Top]],
     ValueWire[NeatCurve[{NodeSide["+", Right], NodeOutPort["B", 1]}, JoinStyle -> Below, SegmentPosition -> 0, Setback -> 0]],
     ValueWire[NodeSide["+", Bottom], NodeOutPort[1]]},
    PortPositions -> {In -> {0}, Out -> {-0.05}}
  ],
  {BubbleWire[NodeInPort["I"], NodeSide["I", Top], {1}, WireTypeSlugPosition -> 0.275],
   BubbleWire[AbsoluteOffset[{-1.6, 0}] @ NodeSide["O", Bottom], NodeOutPort[1], {2}]},
  NodePalette -> {"r" -> 1, "b" -> 2}, FrameColor -> $DarkGray, FrameThickness -> 5,
  PortPositions -> {In -> "MatchOut", Out -> {-1.6}}
];

NamedDiagram["RainbowArrays/MLPFunction"] :=
DerivedArraySignatureForm["mlp", {Subscript["\:03f4", "1"] -> None, Subscript["\:03f4",2] -> None, "\[Ellipsis]"}, {ArrayShapeForm["I" -> {"N" -> 1}]}, ArrayShapeForm["O" -> {"M" -> 4}]] @
ArrayCircuitGraphics @
DerivedBubbleFunctionNode[{"t"} -> {"t"},
  NodeColumn[sz = {4, 1.25}; sz2 = {2, 1};
    {BubbleFunctionNode["SLP"[Subscript["\:03f4", 1]], "t" -> "t", sz, NodeAlias -> "a1"],
     BubbleFunctionNode["ReLU", "t" -> "t", sz2, NodeAlias -> "r1"],
     BubbleFunctionNode["SLP"[Subscript["\:03f4", 2]], "t" -> "t", sz, NodeAlias -> "a2"],
     BubbleFunctionNode["ReLU", "t" -> "t", sz2, NodeAlias -> "r2"],
     BubbleFunctionNode["\[VerticalEllipsis]", "t" -> "t", sz2, NodeAlias -> "a3", FrameColor -> None, PortShape -> None],
     BubbleFunctionNode["SLP"[Subscript["\:03f4", "n"]], "t" -> "t", sz, NodeAlias -> "an"]}
  ],
  {BubbleWire[NodeInPort["t"], NodeInPort["a1", "t"]],
   BubbleWire[NodeOutPort["a1", "t"], NodeInPort["r1", "t"]], BubbleWire[NodeOutPort["r1", "t"], NodeInPort["a2", "t"]] // ApplyWireOptions[SetbackDistance -> {0.2,0}],
   BubbleWire[NodeOutPort["a2", "t"], NodeInPort["r2", "t"]], BubbleWire[NodeOutPort["r2", "t"], NodeInPort["a3", "t"]] // ApplyWireOptions[SetbackDistance -> {0.2,0}],
   BubbleWire[NodeOutPort["a3", "t"], NodeInPort["an", "t"]], BubbleWire[NodeOutPort["an", "t"], NodeOutPort["t"]]}
];

NamedDiagram["RainbowArrays/RNNInformationFlow"] := Module[
  {n, xs, circ, yt, ym, yb, arr},
  n = 4;
  xs = Range[1, n];
  circ[p_] := Disk[p, .1];
  {yt, ym, yb} = {2, 1, 0};
  arr[a_, b_] := ExtendedArrow[{a, b}, Setback -> 0.15, ArrowheadLength -> .1,
    ArrowheadShape -> "Triangle", ArrowThickness -> 2, ArrowColor -> $Gray];
  FixedGraphics[
    {
      FaceEdgeForm[$Green], circ[{#, yt}]& /@ xs,
      arr[{#, yt}, {#, ym}]& /@ xs,
      ApplyWindowed[arr[{#1, ym}, {#2, ym}]&, xs],
      FaceEdgeForm[$Orange], circ[{#, ym}]& /@ Most[xs],
      FaceEdgeForm[$Red], circ[{#, ym}]& /@ Take[xs, -1]
    },
    GraphicsScale -> 80
  ]
];

NamedDiagram["RainbowArrays/TransformerInformationFlow"] := Module[
  {n, xs, circ, yt, ym, yb, yb2, arr},
  n = 4;
  xs = Range[1, n];
  circ[p_] := Disk[p, .1];
  {yt, ym, yb, yb2} = {2, 1, 0, -1};
  arr[a_, b_] := ExtendedArrow[{a, b}, Setback -> 0.15, ArrowheadLength -> .1,
    ArrowheadShape -> "Triangle", ArrowThickness -> 2, ArrowColor -> $Gray];
  FixedGraphics[
    {
      FaceEdgeForm[$Green], circ[{#, yt}]& /@ xs,
      ApplyTuples[If[#2 >= #1, arr[{#1, yt}, {#2, ym}], Nothing]&, xs, 2],
      FaceEdgeForm[$Orange], circ[{#, ym}]& /@ xs,
      ApplyTuples[If[#2 >= #1, arr[{#1, ym}, {#2, yb}], Nothing]&, xs, 2],
      FaceEdgeForm[$Orange], circ[{#, yb}]& /@ xs,
      ApplyTuples[If[#2 >= #1, arr[{#1, yb}, {#2, yb2}], Nothing]&, xs, 2],
      FaceEdgeForm[$Red], circ[{#, yb2}]& /@ xs
    },
    GraphicsScale -> 80
  ]
];

NamedDiagram["RainbowArrays/ScalarScoreTensor"] :=
DerivedArraySignatureForm["score", {"Q" -> {"K"->3}, "K" -> {"K"->3}}, {}] @
ArrayCircuitGraphics @
DerivedRainbowTensorNode[{},
  NodeGrid[
    {{1,1} -> RainbowTensorNode["Q", {Right -> "K"}, 1.5, PortColor -> 3],
     {1,2} -> RainbowTensorNode["K", {Left -> "K"}, 1.5, PortColor -> 3]}
  ],
  {RainbowWire[NodePort["Q", "K"], NodePort["K", "K"]]},
  NodePalette -> {"M" -> 1, "N" -> 4, "K" -> 3, "V" -> 2}
];


NamedDiagram["RainbowArrays/MultiKeyScoreTensor"] :=
DerivedArraySignatureForm["score", {"Q" -> {"K"->3}, "K" -> {"K"->3, "M"->1}}, {"M" -> 1}] @
ArrayCircuitGraphics @
DerivedRainbowTensorNode[{"M"},
  NodeGrid[
    {{1,1} -> RainbowTensorNode["Q", {Right -> "K"}, 1.5],
     {1,2} -> RainbowTensorNode["K", {Top -> "M", Left -> "K"}, 1.5]}
  ],
  {RainbowWire[NodePort["Q", "K"], NodePort["K", "K"]],
   RainbowWire[NodeInPort["M"], NodePort["K", "M"]]},
  NodePalette -> {"M" -> 1, "N" -> 4, "K" -> 3, "V" -> 2}
];

NamedDiagram["RainbowArrays/MultiQueryKeyScoreTensor"] :=
DerivedArraySignatureForm["score", {"Q" -> {"K"->3,"N"->4}, "K" -> {"M" -> 1, "K"->3}}, {"M" -> 1, "N" -> 4}] @
ArrayCircuitGraphics @
DerivedRainbowTensorNode[{"N", "M"},
  NodeGrid[
    {{1,1} -> RainbowTensorNode["Q", {Top -> "N", Right -> "K"}, 1.5],
     {1,2} -> RainbowTensorNode["K", {Top -> "M", Left -> "K"}, 1.5]}
  ],
  {RainbowWire[NodePort["Q", "K"], NodePort["K", "K"]],
   RainbowWire[NodeInPort["N"], NodePort["Q", "N"]],
   RainbowWire[NodeInPort["M"], NodePort["K", "M"]]},
  NodePalette -> {"M" -> 1, "N" -> 4, "K" -> 3, "V" -> 2}
];

$aSumNode = RainbowArrayNode["A",
  {Style[1, "Disk", PortColor -> 1]} -> {Style[1, Labeled["OuterDiamond", Style["sum", Bold], Above]]}, 1.5,
  NodeAlias -> "Asum"
];

$aSumGenericNode = RainbowArrayNode["A",
  {Style[1, "Disk", PortColor -> 1], 0} -> {Style[1, Labeled["OuterDiamond", Style["sum", Bold], Above]]}, 1.5,
  NodeAlias -> "Asum"
];

NamedDiagram["RainbowArrays/Normalize"] :=
DerivedArraySignatureForm["\!\(\*SubscriptBox[\(normalize\),
StyleBox[\"\[FilledDiamond]\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color1\", FontColor}]]]\)", {"A" -> {"\[FilledSquare]" -> 1}}, {"\[FilledSquare]" -> 1}] @
ArrayCircuitGraphics @
DerivedRainbowArrayNode[{1},
  NodeGrid[
    {
      {1, 1} -> RainbowArrayNode["A", {1}, 1.25],
      {1, 2} -> $aSumNode,
      {2, 1;;2} -> ScalarDivideNode[]
    },
    FrameMargins -> {Top -> 0.65, Bottom -> -0.23}
  ],
  {RainbowWire[1, "A" -> 1],
   ValueWire[NodeOutPort[$["A", "Asum"], 1], NodeSide["/", $[Left, Right]], JoinStyle -> Below],
   ValueWire[NodeSide["/", Bottom], NodeOutPort[1]]}
]

NamedDiagram["RainbowArrays/GenericNormalize"] :=
DerivedArraySignatureForm["\!\(\*SubscriptBox[\(normalize\),StyleBox[\"\[FilledDiamond]\",\nFontColor:>CurrentValue[{StyleDefinitions, \"Color1\", FontColor}]]]\)", {"A" -> {"\[FilledSquare]" -> 1,"\[Ellipsis]"}}, {"\[FilledSquare]" -> 1,"\[Ellipsis]"}] @
ArrayCircuitGraphics @
DerivedRainbowArrayNode[{0, 1},
  NodeGrid[
    {
      {1, 1} -> RainbowArrayNode["A", {1, 0}, 1.5],
      {1, 2} -> $aSumGenericNode,
      {2, 1;;2} -> ScalarDivideNode[]
    },
    FrameMargins -> {Top -> 0.65, Bottom -> -0.23}
  ],
  {RainbowWireBundle[0, {"A" -> 0, "Asum" -> 0}],
   RainbowWire[1, "A" -> 1],
   ValueWire[NodeOutPort[$["A", "Asum"], 1], NodeSide["/", $[Left, Right]], JoinStyle -> Below],
   ValueWire[NodeSide["/", Bottom], NodeOutPort[1]]}
];

NamedDiagram["RainbowArrays/GenericNormalizeEquation"] :=
SpacedRow[
  ArrayCircuitGraphics @
  DerivedRainbowArrayNode[{Style[1, PortShape -> "Disk", PortColor -> 1], 0},
    RainbowArrayNode["nlz(A)", {1, 0}, {2, 1.5}, NodeAlias -> "A", NodeLabelStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Fira Code", PrivateFontOptions -> {"OperatorSubstitution" -> False}, BaseStyle -> "PreformattedCode"}],
    {RainbowWireBundle[0, "A" -> 0], RainbowWire[1, "A" -> 1], ValueWire["A", 1]},
    AggregationLabelProlog["sum"]
  ],
  LargeSymbolForm["="],
  ArrayCircuitGraphics @ RainbowArrayNode["1", {1, 0}, 1.75]
];

NamedDiagram["RainbowArrays/NormalizeAbbreviation"] :=
SpacedRow[
  ArrayCircuitGraphics @
  DerivedRainbowArrayNode[{0, 1},
    RainbowArrayNode["A", {1, 0}, 1.5, Epilog -> Text["nlz", NodeSide[BottomRight], {1.5,-1.3}, BaseStyle -> {$Red, Bold}]],
    {RainbowWireBundle[0, "A" -> 0], RainbowWire[1, "A" -> 1], ValueWire["A", 1]}
  ],
  LargeSymbolForm["\[Congruent]"],
  ArrayCircuitGraphics @
  DerivedRainbowArrayNode[{0, 1},
    NodeGrid[
      {
        {1, 1} -> RainbowArrayNode["A", {1, 0}, 1.5],
        {1, 2} -> $aSumGenericNode,
        {2, 1;;2} -> ScalarDivideNode[]
      },
      FrameMargins -> {Top -> 0.65, Bottom -> -0.23}
    ],
    {RainbowWireBundle[0, {"A" -> 0, "Asum" -> 0}],
     RainbowWire[1, "A" -> 1],
     ValueWire[NodeOutPort[$["A", "Asum"], 1], NodeSide["/", $[Left, Right]], JoinStyle -> Below],
     ValueWire[NodeSide["/", Bottom], NodeOutPort[1]]}
  ]
];

NamedDiagram["RainbowArrays/SoftmaxDefinition"] :=
SpacedRow[
  ArrayCircuitGraphics @
  RainbowAliasedFunction[1 -> "softmax", {0, 1},
    RainbowArrayNode["A", {1, 0}, 1.5],
    {RainbowWireBundle[0, "A" -> 0], RainbowWire[1, "A" -> 1], ValueWire["A", 1]}
  ],
  LargeSymbolForm["\[Congruent]"],
  ArrayCircuitGraphics @
  DerivedRainbowArrayNode[{1, 0},
    NodeColumn[{
      RainbowArrayNode["A", {1, 0}, 1.5],
      ScalarFunctionNode["exp", 1, {1.5, 1}]
    }],
    {RainbowWireBundle[0, "A" -> 0], RainbowWire[1, "A" -> 1],
     ValueWire["A", "exp"],
     ValueWire["exp", 1]},
    Prolog -> Text["nlz", NodeSide[BottomRight], {1.5,-1.3}, BaseStyle -> {$Red, Bold}]
  ]
];

NamedDiagram["RainbowArrays/WeightsTensor"] :=
DerivedArraySignatureForm["weights", {"Q" -> {"K"->3}, "K" -> {"K"->3,"N"->1}}, {"N" -> 1}] @
ArrayCircuitGraphics @
DerivedRainbowTensorNode[{Style["N", 1]},
  NodeGrid[{
    {1,1} -> RainbowTensorNode["Q", {Right -> "K"}, 1.5, PortColor -> {3}],
    {1,2} -> RainbowTensorNode["K", {Top -> "N", Left -> "K"}, 1.5, PortColor -> {1, 3}]
  }],
  {RainbowWire[NodePort["Q", "K"], NodePort["K", "K"], 3],
   RainbowWire[NodePort["N"], NodePort["K", "N"], 1]},
  Prolog -> Text["softmax", NodeSide[BottomRight], {1.5,-1.3}, BaseStyle -> {$Red, Bold}]
];

NamedDiagram["RainbowArrays/WeightedSumTensor"] :=
ArrayCircuitGraphics @
DerivedRainbowTensorNode[{"V"},
  NodeGrid[{
    {1, 1} -> RainbowTensorNode["W", {Right -> "N"}, 1.5],
    {1, 2} -> RainbowTensorNode["V", {Left -> "N", Top -> "V"}, 1.5]
  }],
  {RainbowWire[NodePort["W", "N"], NodePort["V", "N"]],
   RainbowWire[NodePort["V"], NodePort["V", "V"]]},
  NodePalette -> {"N" -> 1, "K" -> 3, "V" -> 2}
];

NamedDiagram["RainbowArrays/AttendTensor"] :=
DerivedArraySignatureForm["attend", {"Q" -> {"K"->3}, "K" -> {"K"->3,"N"->1}, "V" -> {"N" -> 1, "V" -> 2}}, {"V" -> 2}] @
ArrayCircuitGraphics @
DerivedRainbowTensorNode[{"V"},
  NodeGrid[{
    {1, 1} ->
      DerivedRainbowTensorNode[None,
        NodeGrid[{
          {1,1} -> RainbowTensorNode["Q", {Right -> "K"}, 1.5],
          {1,2} -> RainbowTensorNode["K", {Right -> "N", Left -> "K"}, 1.5]
        }],
        {RainbowWire[NodePort["Q", "K"], NodePort["K", "K"], 3],
         RainbowWire[NodePort["N"], NodePort["K", "N"], 1]},
        Prolog -> Text["softmax", NodeSide[Bottom], {0,-1.3}, BaseStyle -> {$Red, Bold}],
        NodePorts -> {Right -> {"N"}, Bottom -> {}},
        NodeAlias -> "smax"
      ],
    {1, 2} ->
      RainbowTensorNode["V", {Left -> "N", Top -> "V"}, 1.5]
  }],
  {RainbowWire[NodePort["smax", "N"], NodePort["V", "N"]],
   RainbowWire[NodePort["V"], NodePort["V", "V"]]},
  NodePalette -> {"N" -> 1, "K" -> 3, "V" -> 2}
];

NamedDiagram["RainbowArrays/AttendSubcircuit"] = BubbleValueNode["RV", {"V"},
  NodeGrid[{
    {1, 1} ->
      DerivedRainbowTensorNode[None,
        NodeGrid[{
          {1,1} -> RainbowBubbleTensorNode["Q", {Right -> "K"}, 1.2],
          {1,2} -> RainbowTensorNode["K", {Right -> "N", Left -> "K"}, 1.2]
        }],
        {RainbowWire[NodePort["Q", "K"], NodePort["K", "K"], 3],
         RainbowWire[NodePort["N"], NodePort["K", "N"], 1]},
         Prolog -> Text["softmax", NodeSide[Bottom], {0,-1.3}, BaseStyle -> {$Red, Bold}],
         NodePorts -> {Right -> {"N"}, Bottom -> {}},
         NodeAlias -> "smax"
      ],
    {1, 2} ->
      RainbowTensorNode["V", {Left -> "N", Top -> "V"}, 1.2]
  }],
  {RainbowWire[NodePort["smax", "N"], NodePort["V", "N"]],
   RainbowWire[NodePort["V"], NodePort["V", "V"]]},
  PortPositions -> "MatchInterior"
];

NamedDiagram["RainbowArrays/QueryAttendFunctionExplicit"] :=
DerivedArraySignatureForm["attend", {"K" -> {"K"->3,"N"->1}, "V" -> {"N" -> 1, "V" -> 2}}, ArrayShapeForm["Q" -> {"K"->3}], ArrayShapeForm["R" -> {"V"->2}]] @
ArrayCircuitGraphics @
NodeGrid[
  {
    {1, 1} ->
      Padded[RainbowBubbleTensorNode["Q", {Right -> "K"}, 1.1, NodeAlias -> "Qext"], Left -> 2.3],
    {2, 1} ->
      DerivedBubbleFunctionNode[{"QV"} -> {"RV"},
        NamedDiagram["RainbowArrays/AttendSubcircuit"],
        {BubbleWire[NodePort["QV"], NodeSide["Q",Top]],
         BubbleWire[AbsoluteOffset[{-1.7, 0}] @ NodeSide["RV", Bottom], NodePort["RV"]]},
        PortPositions -> "MatchInterior", $HigherOrderFunctionOptions,
        NodeAlias -> "toR"
      ],
    {3, 1} ->
      Padded[RainbowBubbleTensorNode["R", {Right -> "V"}, 1.1, NodeAlias -> "Rext"], Left -> 2.3]
  },
  Epilog -> {
    BubbleWire[NodeSide["Qext", Bottom], NodePort["toR", "QV"]],
    BubbleWire[NodePort["toR", "RV"], NodeSide["Rext", Top]]
  },
  ColumnAlignments -> Left,
  NodePalette -> {"N" -> 1, "K" -> 3, "V" -> 2}
];

NamedDiagram["RainbowArrays/QueryAttendFunction"] :=
DerivedArraySignatureForm["attend", {"K" -> {"K"->3,"N"->1}, "V" -> {"N" -> 1, "V" -> 2}}, ArrayShapeForm["Q" -> {"K"->3}], ArrayShapeForm["R" -> {"V"->2}]] @
ArrayCircuitGraphics @
DerivedBubbleFunctionNode[{"QV"} -> {"RV"},
  NamedDiagram["RainbowArrays/AttendSubcircuit"],
  {BubbleWire[NodePort["QV"], NodeSide["Q",Top], {3}],
   BubbleWire[AbsoluteOffset[{-1.7, 0}] @ NodeSide["RV", Bottom], NodePort["RV"], {2}]},
  PortPositions -> "MatchInterior",
  NodeAlias -> "toR",NodePalette -> {"N" -> 1, "K" -> 3, "V" -> 2}
];


NamedDiagram["RainbowArrays/AttendFunction"] :=
DerivedArraySignatureForm["attend", {}, {ArrayShapeForm["Q" -> {"K"->3}], ArrayShapeForm["K" -> {"K"->3,"N"->1}], ArrayShapeForm["V" -> {"V"->2,"N"->1}]}, ArrayShapeForm["R" -> {"V"->2}]] @
ArrayCircuitGraphics @
DerivedBubbleFunctionNode[{"QV", "KV", "VV"} -> {"RV"},
  BubbleValueNode["RV", {},
    NodeGrid[{
      {1, 1} ->
          DerivedRainbowTensorNode[None,
            NodeGrid[{
              {1,1} -> RainbowBubbleTensorNode["Q", {Right -> "K"}, 1.2],
              {1,2} -> RainbowBubbleTensorNode["K", {Right -> "N", Left -> "K"}, 1.2]
            }],
            {RainbowWire[NodePort["Q", "K"], NodePort["K", "K"], 3],
             RainbowWire[NodePort["N"], NodePort["K", "N"], 1]},
            Prolog -> Text["softmax", NodeSide[Bottom], {0,-1.3}, BaseStyle -> {$Red, Bold}],
            NodePorts -> {Right -> {"N"}, Bottom -> {}},
            NodeAlias -> "smax"
          ],
      {1, 2} ->
          RainbowBubbleTensorNode["V", {Left -> "N", Top -> "V"}, 1.2]
    }],
    Epilog -> {RainbowWire[NodePort["smax", "N"], NodePort["V", "N"]]},
    PortPositions -> "MatchInterior"
  ],
  {BubbleWire[NodePort["QV"], NodeSide["Q",Top], {3}],
   BubbleWire[NodeInPort["KV"], NodeSide["K", Top], {3, 1}],
   BubbleWire[NodeInPort["VV"], NodeSide["V", Top], {1, 2}],
   BubbleWire[AbsoluteOffset[{-1.7, 0}] @ NodeSide["RV", Bottom], NodePort["RV"], {2}]},
  PortPositions -> "MatchInterior",
  NodeAlias -> "toR",
  NodePalette -> {"N" -> 1, "K" -> 3, "V" -> 2}];

NamedDiagram["RainbowArrays/FromTokenFunction"] :=
DerivedArraySignatureForm["from_token", {Subscript["\:03f4","qkv"] -> None}, {ArrayShapeForm["T" -> {"T"->6}]}, Row @ {ArrayShapeForm["Q" -> {"K"->3}], ArrayShapeForm["K" -> {"K"->3}], ArrayShapeForm["V" -> {"V"->2}]}] @
ArrayCircuitGraphics @
NodeColumn[
  {
    RainbowBubbleTensorNode["T", {Right -> "T"}, 1.1],
    DerivedBubbleFunctionNode[{1} -> {1,2,3},
      NodeRow[{
        BubbleFunctionNode["SLP"[Subscript["\:03f4","q"]], {1}, {2.5,1}, NodeAlias -> "sq"],
        BubbleFunctionNode["SLP"[Subscript["\:03f4","k"]], {1}, {2.5,1}, NodeAlias -> "sk"],
        BubbleFunctionNode["SLP"[Subscript["\:03f4","v"]], {1}, {2.5,1}, NodeAlias -> "sv"]
      }],
      {BubbleWire[NodeInPort["fromT", 1], {"sq", "sk", "sv"}],
       BubbleWire[NodeOutPort[$["sq", "sk", "sv"], 1], NodeOutPort["fromT", $[3]]]},
      NodeAlias -> "fromT", FrameMargins -> {Top -> 1.25, All -> 0.75},
      PortPositions -> "MatchInterior"
    ],
    NodeRow[{
      RainbowBubbleTensorNode["Q", {Right -> "K"}, 1.1, NodeLabel -> "Q"],
      RainbowBubbleTensorNode["K", {Right -> "K"}, 1.1, NodeLabel -> "K"],
      RainbowBubbleTensorNode["V", {Right -> "V"}, 1.1, NodeLabel -> "V"]},
      Spacings -> 1.9
    ]
  },
  Spacings -> 0.5,
  Epilog -> {
    BubbleWire[NodeSide["T",Bottom], "fromT" -> 1],
    BubbleWire[NodeOutPort["fromT", 1], NodeSide["Q",Top]],
    BubbleWire[NodeOutPort["fromT", 2], NodeSide["K",Top]],
    BubbleWire[NodeOutPort["fromT", 3], NodeSide["V",Top]]
  },
  NodePalette -> {"N" -> 1, "K" -> 3, "V" -> 2, "T" -> 6}
];

NamedDiagram["RainbowArrays/ToTokenFunction"] :=
DerivedArraySignatureForm["to_token", {Subscript["\:03f4", "t"] -> None}, {ArrayShapeForm["R" -> {"K"->2}]}, ArrayShapeForm["T" -> {"T"->6}]] @
ArrayCircuitGraphics @
NodeColumn[
  {
    RainbowBubbleTensorNode["R", {Right -> "V"}, 1.1],
    DerivedBubbleFunctionNode[{"r"} -> {"t"},
      BubbleFunctionNode["SLP"[Subscript["\:03f4","t"]], {1}, {2.5,1}, NodeAlias -> "slp"],
      {BubbleWire[NodeInPort["toT", "r"], NodeInPort["slp", 1]],
       BubbleWire[NodeOutPort["slp", 1], NodeOutPort["toT", "t"]]},
      NodeAlias -> "toT", PortPositions -> "MatchExterior"
    ],
    RainbowBubbleTensorNode["T", {Right -> "T"}, 1.1]
  },
  Epilog -> {
    BubbleWire[NodeSide["R",Bottom], NodeInPort["toT", "r"]],
    BubbleWire[NodeOutPort["toT", "t"], NodeSide["T",Top]] // ApplyWireOptions[SplitPosition -> 0, SetbackDistance -> 0.05]
  },
  Spacings -> 0.5, NodePalette -> {"N" -> 1, "K" -> 3, "V" -> 2, "T" -> 6}
];

NamedDiagram["RainbowArrays/FromTokenFunctionMapped"] :=
DerivedArraySignatureForm["from_token", {Subscript["\:03f4", "qkv"] -> None}, {ArrayShapeForm["T" -> {"N" -> 1, "T"->6}]}, Row @ {ArrayShapeForm["Q" -> {"N" -> 1, "K"->3}], ArrayShapeForm["K" -> {"N" -> 1, "K"->3}], ArrayShapeForm["V" -> {"N" -> 1, "V"->2}]}] @
ArrayCircuitGraphics @
NodeColumn[
  {
    RainbowBubbleTensorNode["T", {Left -> "N", Right -> "T"}, 1.1],
    BubbleFunctionNode["\[Ellipsis]", 1 -> {1,2,3}, {5, 1.5}, NodeAlias -> "fromT", PortSpacing -> 1.8],
    NodeRow[
      {RainbowBubbleTensorNode["Q", {Left -> "N", Right -> "K"}, 1.1, NodeLabel -> "Q"],
       RainbowBubbleTensorNode["K", {Left -> "N", Right -> "K"}, 1.1, NodeLabel -> "K"],
       RainbowBubbleTensorNode["V", {Left -> "N", Right -> "V"}, 1.1, NodeLabel -> "V"]},
      Spacings -> 0.7
    ]
  },
  Epilog -> {
    BubbleWire[NodeSide["T",Bottom], "fromT" -> 1],
    BubbleWire[NodeOutPort["fromT", $[3]], NodeSide[$["QKV"],Top]]
  },
  Spacings -> 0.5, NodePalette -> {"N" -> 1, "K" -> 3, "V" -> 2, "T" -> 6}
];

$r = 1.2;
NamedDiagram["RainbowArrays/SelfAttentionFunction"] :=
DerivedArraySignatureForm["self_attention", {Subscript["\:03f4", "qkvt"] -> None}, {ArrayShapeForm["T" -> {"N" -> 1, "T"->6}]}, ArrayShapeForm["T" -> {"N" -> 1, "T"->6}]] @
ArrayCircuitGraphics @ DerivedBubbleFunctionNode[{"t"} -> {"t"},
  NodeColumn[
    {
      BubbleFunctionNode["from_token"[Subscript["\:03f4", "qkv"]], {"t"} -> {"q", "k", "v"}, {5, 1.25}, NodeAlias -> "fromT"],
      BubbleFunctionNode["attend", {"q", "k", "v"} -> {"r"}, {5, 1.25}],
      BubbleFunctionNode["to_token"[Subscript["\:03f4", "t"]], "r" -> "t", {5, 1.25}, NodeAlias -> "toT"]
    },
    FrameMargins -> {Top -> 0.1, Bottom -> 0.1}, Spacings -> 0.75
  ],
  {BubbleWire[NodeInPort["t"],     "fromT" -> "t", {1, 6}],
   BubbleWire["fromT" -> $["qkv"], "attend" -> $["qkv"], {1, $[3,3,2]}],
   BubbleWire["attend" -> "r",     "toT" -> "r", {1, 2}],
   BubbleWire["toT" -> "t",        NodeOutPort["t"], {1, 6}]},
  NodePalette -> {"N" -> 1, "K" -> 3, "V" -> 2, "T" -> 4},
  NodeAlias -> "transform"
];

NamedDiagram["RainbowArrays/SelfAttentionFunctionExpanded"] :=
DerivedArraySignatureForm["self_attention", Subscript["\:03f4", "qkvt"] -> None, {ArrayShapeForm["T" -> {"N" -> 1, "T"->6}]}, ArrayShapeForm["T" -> {"N" -> 1, "T"->6}]] @
ArrayCircuitGraphics @ DerivedBubbleFunctionNode["t" -> "t",
  NodeGrid[
    {
      {1, 1} ->
        NodeRow[slpw = 2;
          {
            BubbleFunctionNode["SLP"[Subscript["\:03f4","q"]], {1}, {slpw,1}, NodeAlias -> "slpq", PortPositions -> {0}],
            BubbleFunctionNode["SLP"[Subscript["\:03f4","k"]], {1}, {slpw,1}, NodeAlias -> "slpk", PortPositions -> {0}],
            BubbleFunctionNode["SLP"[Subscript["\:03f4","v"]], {1}, {slpw,1}, NodeAlias -> "slpv", PortPositions -> {0}]
          },
          Spacings -> 0.45
        ],
      {2, 1} ->
        BubbleValueNode["RV", {Right -> {"V"}, Bottom -> {}, Left -> {"N"}},
          NodeGrid[{
            {1, 1} -> DerivedRainbowTensorNode[None,
                NodeGrid[{
                  {1,1} -> RainbowBubbleTensorNode["Q", {Right -> "K", Left -> "N"}, 1.2],
                  {1,2} -> RainbowBubbleTensorNode["K", {Right -> "N", Left -> "K"}, 1.2]
                }],
                {RainbowWire[NodePort["Q", "K"], NodePort["K", "K"], 3],
                 RainbowWire[NodePort["N"], NodePort["K", "N"], 1]},
                Prolog -> Text["softmax", NodeSide[Bottom], {0,-1.3}, BaseStyle -> {$Red, Bold}],
                NodePorts -> {Right -> {"N"}}, NodeAlias -> "smax"
              ],
            {1, 2} -> RainbowBubbleTensorNode["V", {Left -> "N", Right -> "V"}, 1.2]
          }],
          {RainbowWire[NodePort["smax", "N"], NodePort["V", "N"]],
           RainbowWire[NodePort["N"], NodePort["Q", "N"]],
           RainbowWire[NodePort["V"], NodePort["V", "V"]]}
        ],
      {3, 1} -> BubbleFunctionNode["SLP"[Subscript["\:03f4","t"]], {1}, {slpw,1}, NodeAlias -> "slpt"]
    },
    RowSpacings -> {1, 0.7}, FrameMargins -> {Top -> 0.4, Bottom -> 0.1}
  ],
  {BubbleWire[NodeInPort["t"], {"slpq" -> 1, "slpv" -> 1, "slpk" -> 1}, {1,6}, WireTypeSlugPosition -> 0.3],
   BubbleWire["slpq" -> 1, NodeSide["Q",Top], {1,3}, BendRadius -> Infinity],
   BubbleWire["slpk" -> 1, NodeSide["K",Top], {3,1}],
   BubbleWire["slpv" -> 1, NodeSide["V",Top], {1,2}],
   BubbleWire[NodeSide["RV", Bottom], "slpt" -> 1, {1, 2}],
   BubbleWire["slpt" -> 1, NodeOutPort["t"], {1,6}]},
  NodePalette -> {"N" -> 1, "K" -> 3, "V" -> 2},
  NodeAlias -> "transform"
];

NamedDiagram["RainbowArrays/MultiheadFromTokenFunction"] :=
DerivedArraySignatureForm["from_token", {Subscript["\:03f4", "qkv"] -> None}, {ArrayShapeForm["T" -> {"T"->6}]}, Row @ {ArrayShapeForm["Q" -> {"H" -> 4, "K"->3}], ArrayShapeForm["K" -> {"K"->3}], ArrayShapeForm["V" -> {"V"->2}]}] @
ArrayCircuitGraphics @
NodeColumn[
  {
    RainbowBubbleTensorNode["T", {Right -> "T"}, 1.1],
    BubbleFunctionNode["\[Ellipsis]", 1 -> {1,2,3}, {5, 1.25}, NodeAlias -> "fromT", PortSpacing -> 1.8],
    NodeRow[
      {
        RainbowBubbleTensorNode["Q", {Right -> "K", Left -> "H"}, 1.1, NodeLabel -> "Q"],
        RainbowBubbleTensorNode["K", {Right -> "K"}, 1.1, NodeLabel -> "K"],
        RainbowBubbleTensorNode["V", {Right -> "V"}, 1.1, NodeLabel -> "V"]
      },
      Spacings -> 0.7
    ]
  },
  Epilog -> {
    BubbleWire[NodeSide["T",Bottom], "fromT" -> 1],
    BubbleWire["fromT" -> $[3], NodeSide[$["QKV"],Top]]
  },
  Spacings -> 0.5, NodePalette -> {"N" -> 1, "K" -> 3, "V" -> 2, "T" -> 6, "H" -> 4}
];

NamedDiagram["RainbowArrays/MultiheadToTokenFunction"] :=
DerivedArraySignatureForm["to_token", {Subscript["\:03f4", "t"] -> None}, {ArrayShapeForm["R" -> {"H" -> 4, "N"->2}]}, ArrayShapeForm["T" -> {"T"->6}]] @
ArrayCircuitGraphics @ NodeColumn[
  {
    RainbowBubbleTensorNode["R", {Left -> "H", Right -> "V"}, 1.1],
    BubbleFunctionNode["\[Ellipsis]", "r" -> "t", {3, 1.25}, NodeAlias -> "to_token"],
    RainbowBubbleTensorNode["T", {Right -> "T"}, 1.1]
  },
  Epilog -> {
    BubbleWire[NodeSide["R",Bottom], NodeInPort["to_token", "r"]],
    BubbleWire[NodeOutPort["to_token", "t"], NodeSide["T",Top]] // ApplyWireOptions[SplitPosition -> 0, SetbackDistance -> 0.05]
  },
  Spacings -> 0.5, NodePalette -> {"N" -> 1, "K" -> 3, "V" -> 2, "T" -> 6, "H" -> 4}
];

NamedDiagram["RainbowArrays/MultiheadSelfAttentionFunction"] :=
DerivedArraySignatureForm["self_attention", Subscript["\:03f4", "qkvt"] -> None, {ArrayShapeForm["T" -> {"N" -> 1, "T"->6}]}, ArrayShapeForm["T" -> {"N" -> 1, "T"->6}]] @
ArrayCircuitGraphics @
DerivedBubbleFunctionNode["t" -> "t",
  NodeGrid[
    {
      {1, 1} ->
        NodeRow[slpw = 2;
          {
            BubbleFunctionNode["SLP"[Subscript["\:03f4","q"]], {1}, {slpw,1}, NodeAlias -> "slpq", PortPositions -> {0}],
            BubbleFunctionNode["SLP"[Subscript["\:03f4","k"]], {1}, {slpw,1}, NodeAlias -> "slpk", PortPositions -> {0}],
            BubbleFunctionNode["SLP"[Subscript["\:03f4","v"]], {1}, {slpw,1}, NodeAlias -> "slpv", PortPositions -> {0}]
          },
          Spacings -> 0.45
        ],
      {2, 1} ->
        BubbleValueNode["RV", {Right -> {"V"}, Bottom -> {}, Left -> {"N", "H"}},
          NodeGrid[
            {
              {1, 1} ->
                DerivedRainbowTensorNode[{Right -> {"N"}},
                  NodeGrid[
                    {{1,1} -> RainbowBubbleTensorNode["Q", {BottomLeft -> "H", Right -> "K", TopLeft -> "N"}, 1.2],
                     {1,2} -> RainbowBubbleTensorNode["K", {Right -> "N", Left -> "K"}, 1.2]}],
                  {RainbowWire[NodePort["Q", "K"], NodePort["K", "K"], 3],
                   RainbowWire[NodePort["N"], NodePort["K", "N"], 1]},
                  Prolog -> Text["softmax", NodeSide[Bottom], {0,-1.3}, BaseStyle -> {$Red, Bold}],
                  NodeAlias -> "smax"
                ],
              {1, 2} ->
                RainbowBubbleTensorNode["V", {Left -> "N", Right -> "V"}, 1.2]
            }
          ],
          {RainbowWire[NodePort["smax", "N"], NodePort["V", "N"]],
           RainbowWire[NodePort["N"], NodePort["Q", "N"], BendRadius -> Infinity, JoinStyle -> {Horizontal, {-1, 1}}],
           RainbowWire[NodePort["H"], NodePort["Q", "H"], BendRadius -> Infinity, JoinStyle -> {Horizontal, {-1, -1}}],
           RainbowWire[NodePort["V"], NodePort["V", "V"]]},
          PortSpacing -> 1.5
        ],
      {3, 1} ->
        BubbleFunctionNode["SLP"[Subscript["\:03f4","t"]], {1}, {slpw,1}, NodeAlias -> "slpt"]
    },
    RowSpacings -> {1, 0.7}, FrameMargins -> {Top -> 0.4, Bottom -> 0.1}
  ],
  {BubbleWire[NodeInPort["t"], {"slpq" -> 1, "slpv" -> 1, "slpk" -> 1}, {1,6}, WireTypeSlugPosition -> 0.3],
   BubbleWire["slpq" -> 1, NodeSide["Q",Top], {1,4,3}, BendRadius -> Infinity],
   BubbleWire["slpk" -> 1, NodeSide["K",Top], {3,1}],
   BubbleWire["slpv" -> 1, NodeSide["V",Top], {1, 2}],
   BubbleWire[NodeSide["RV", Bottom], "slpt" -> 1, {1, 2}],
   BubbleWire["slpt" -> 1, NodeOutPort["t"], {1,6}]},
  NodePalette -> {"N" -> 1, "K" -> 3, "V" -> 2, "H" -> 4},
  NodeAlias -> "transform"
];

NamedDiagram["RainbowArrays/RoundFunction"] :=
DerivedArraySignatureForm["round", {"\:03f4" -> None}, {ArrayShapeForm["T" -> {"N" -> 1, "T"->6}]}, ArrayShapeForm["T" -> {"N" -> 1, "T"->6}]] @
ArrayCircuitGraphics @
DerivedBubbleFunctionNode[{"t"} -> {"t"},
  NodeColumn[{
    BubbleFunctionNode["self_attention"["\:03f4"], "t" -> "t", {5, 1.25}, NodeAlias -> "satt"],
    Padded[ArrayPlusNode[], {Vertical -> -0.2}],
    BubbleFunctionNode["normalization", "t" -> "t", {5, 1.25}]
  }],
  {BubbleWire[NodeInPort["t"], NodeSide["satt", Top], Setback -> {0, -0.1}],
   BubbleWire[NodeOutPort["satt", "t"], NodeSide["+", Top]],
   {BubbleWire[RollingCurve[CompassCurve[{AbsoluteOffset[{0, -.5}] @ NodeInPort["t"], NodeSide["+", Left]}, "WWW;V;"], BendRadius -> .3]]},
   BubbleWire[NodeSide["+", Bottom], NodeInPort["normalization", "t"]],
   BubbleWire[NodeOutPort["normalization", "t"], NodeOutPort["t"]]},
  PortPositions -> {0}, FrameMargins -> {Horizontal -> 1, Top -> 1, Bottom -> 0.5}
];

NamedDiagram["RainbowArrays/TransformerFunction"] :=
DerivedArraySignatureForm["transformer", {Subscript["\:03f4", "1"] -> None, Subscript["\:03f4",2] -> None, "\[Ellipsis]"}, {ArrayShapeForm["T" -> {"N" -> 1, "T"->6}]}, ArrayShapeForm["T" -> {"N" -> 1, "T"->6}]] @
ArrayCircuitGraphics @
DerivedBubbleFunctionNode[{"t"} -> {"t"},
  NodeColumn[{
    BubbleFunctionNode["round"[Subscript["\:03f4", "1"]], "t" -> "t", {4, 1.25}, NodeAlias -> "a1"],
    BubbleFunctionNode["round"[Subscript["\:03f4", "2"]], "t" -> "t", {4, 1.25}, NodeAlias -> "a2"],
    BubbleFunctionNode["\[VerticalEllipsis]", "t" -> "t", {4, 1.25}, NodeAlias -> "a3", FrameColor -> None, PortShape -> None],
    BubbleFunctionNode["round"[Subscript["\:03f4", "n"]], "t" -> "t", {4, 1.25}, NodeAlias -> "an"]
  }],
  {BubbleWire[NodeInPort["t"], NodeInPort["a1", "t"], {1, 6}],
   BubbleWire[NodeOutPort["a1", "t"], NodeInPort["a2", "t"]],
   BubbleWire[NodeOutPort["a2", "t"], NodeInPort["a3", "t"]],
   BubbleWire[NodeOutPort["a3", "t"], NodeInPort["an", "t"]],
   BubbleWire[NodeOutPort["an", "t"], NodeOutPort["t"], {1, 6}]}
]