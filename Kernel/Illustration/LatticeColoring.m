PublicFunction[LatticeColoringPlot]

$customColors = <|"a" -> $Purple, "b" -> $Pink, "c" -> $Teal, "x" -> $Pink, "y" -> $Teal|>;

$plcOrientation = "Horizontal";

LatticeColoringPlot[quiver_, args___, "Orientation" -> o_] := Block[
  {$plcOrientation = o},
  LatticeColoringPlot[quiver, args]
];

$plcIconSize = "AverageEdge" -> {65, 100};
LatticeColoringPlot[quiver_, args___, "IconSize" -> iconSize_] := Block[
  {$plcIconSize = iconSize},
  LatticeColoringPlot[quiver, args]
];

$plcSLR = 0.5;
LatticeColoringPlot[quiver_, args___, SelfLoopRadius -> r_] := Block[
  {$plcSLR = r},
  LatticeColoringPlot[quiver, args]
];

$vCount = Automatic;
LatticeColoringPlot[quiver_, args___, "VertexCount" -> c_] := Block[
  {$vCount = c},
  LatticeColoringPlot[quiver, args]
];


LatticeColoringPlot[quiver_, args___] := Scope[
  quiver = Quiver[quiver];
  vCount = VertexCount[quiver];
  notb = vCount > 1;
  icon = Quiver[
    quiver,
    GraphTheme -> "FundamentalColoringQuiver",
    SelfLoopRadius -> $plcSLR, ImageSize -> $plcIconSize,
    VertexCoordinates -> If[notb, Take[CirclePoints @ ReplaceAutomatic[$vCount, vCount], vCount], {{0, 0}}]
  ];
  If[notb, icon //= CombineMultiedges];
  graph = LatticeGraph[quiver, FilterOptions @ args,
    VertexColorFunction -> "GeneratingVertex",
    VertexSize -> 1.2, ImageSize -> 150, GraphLegend -> None
  ];
  If[$plcOrientation === "Horizontal",
    Row[{graph, icon}, Spacer[15]],
    Labeled[graph, icon]
  ]
]

(**************************************************************************************************)

DefineGraphTheme["FundamentalColoringQuiver",
  ArrowheadSize -> MediumSmall,
  GraphLegend -> None,
  ArrowheadShape -> {"Arrow", TwoWayStyle -> "OutClose"},
  ArrowheadStyle -> $LightGray,
  LabelCardinals -> Below, VertexSize -> Huge,
  ImagePadding -> {{15, 15}, {20, 20}},
  VertexColorFunction -> "Name"
];

(**************************************************************************************************)

PublicFunction[LatticeColoringRow]

$lcrMW = 3;

LatticeColoringRow[args___, MaxWidth -> m_] := Block[{$lcrMW = m}, LatticeColoringRow[args]];

LatticeColoringRow[list_List, args___] :=
  SpacedRow[
    LatticeColoringPlot[#, args, "Orientation" -> "Vertical"]& /@ list,
    MaxWidth -> $lcrMW
  ];

(**************************************************************************************************)

PublicFunction[LatticeColoringGrid]

makeColoringGridEntry[label:(_String | _Integer | _Subscript), ___] :=
  {Item[LabelForm[label, 15, Bold], ItemSize -> {Full, 2}, Alignment -> Center], SpanFromLeft};

makeColoringGridEntry[None, ___] :=
  {" ", SpanFromLeft};

makeColoringGridEntry[quiver_List, args___] :=
  Map[makeColoringGridEntry[#, args]&, quiver];

makeColoringGridEntry[quiver_, args___] :=
  First @ LatticeColoringPlot[quiver, args];

LatticeColoringGrid[items_, args___] := Scope[
  entries = Map[Flatten[List[makeColoringGridEntry[#, args]]]&, items];
  entries = PadRight[entries, Automatic, ""];
  entries = VectorReplace[entries, row:{_, SpanFromLeft, Repeated[""]} :> VectorReplace[row, "" -> SpanFromLeft]];
  Grid[
    entries,
    Spacings -> {0, 0}, ItemSize -> {All, 0},
    Alignment -> Center
  ]
];
