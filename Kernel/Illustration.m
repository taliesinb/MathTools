PackageExport["IllustatePathsOnFundamentalQuiver"]

IllustatePathsOnFundamentalQuiver[quiver_, pathSpecs_, opts___] := Scope[
  quivers = Labeled[DrawFundamentalQuiverPath[quiver, #1, #2, #3], FormatCardinalWord @ #1]& @@@ pathSpecs;
  regionSpec = Style[Arrow @ Path[1, #1], #2, "Foreground", HighlightRadius->0.3, EdgeSetback -> 2.5]& @@@ pathSpecs;
  lattice = LatticeGraph[
    quiver, opts, GraphRegionHighlight -> regionSpec, ArrowheadSize -> Scaled[0.8],
    LabelCardinals -> True, DirectedEdges -> True, ImagePadding -> {{0, 20}, {20, 0}},
    ArrowheadShape -> "Line"
  ];
  SpacedRow[lattice, quivers, Spacings -> 0]
]


(**************************************************************************************************)

PackageExport["DrawFundamentalQuiverPath"]

DrawFundamentalQuiverPath[quiver_, path_, color_, adjustments_] := Scope[
  regionSpec = Style[
    Arrow[Path[1, path, PathAdjustments -> adjustments]], color, "Foreground",
    HighlightRadius -> 0.3, EdgeSetback -> 3, PathOutline -> False
  ];
  ExtendedGraph[quiver, GraphRegionHighlight -> regionSpec, ArrowheadStyle -> $Gray, ArrowheadSize -> MediumSmall,
    ImagePadding -> 15, EdgeStyle -> LightGray, LabelCardinals -> True,
    GraphLegend -> None, ArrowheadShape -> "Line"]
];


(**************************************************************************************************)

PackageExport["PathBuilder"]

PathBuilder[vertex_, path_String, adjustments_:{}] :=
  GraphRegionHighlight -> Style[Arrow[Path[vertex, path, PathAdjustments -> adjustments]], $Purple,
    "Foreground", HighlightRadius->0.3, EdgeSetback -> 2];

PathBuilder[vertex_, {path1_String, path2_String}] :=
  GraphRegionHighlight -> Style[
      {Style[Arrow[Path[vertex, path1]], $Purple],
       Style[Arrow[Path[vertex, path2]], $Teal]},
      "Foreground", HighlightRadius->0.3, EdgeSetback -> 2];

(**************************************************************************************************)

PackageExport["UnitGraphics"]

UnitGraphics[g_, n_:1] := Graphics[g,
  ImageSize -> Medium, PlotRange -> {{-n, n}, {-n, n}}, PlotRangePadding -> Scaled[0.1],
  Frame -> True, FrameTicks -> None, GridLines -> {Range[-n, n, n / 5], Range[-n, n, n / 5]}
];

(**************************************************************************************************)

PackageExport["LabeledEdgeGraph"]

LabeledEdgeGraph[g_, opts___] := ExtendedGraph[g, opts,
  VertexSize -> Large, ArrowheadSize -> Medium,
  LabelCardinals->True, ImagePadding -> {{20,10},{20,10}},
  VertexLabels -> "Name",
  ImageSize -> ("ShortestEdge" -> 60)
];


(**************************************************************************************************)

PackageExport["LatticeColoringPlot"]

$customColors = <|"a" -> $Purple, "b" -> $Pink, "c" -> $Teal, "x" -> $Pink, "y" -> $Teal|>;

$plcOrientation = "Horizontal";

LatticeColoringPlot[quiver_, args___, "Orientation" -> o_] := Block[
  {$plcOrientation = o},
  LatticeColoringPlot[quiver, args]
];

LatticeColoringPlot[quiver_, args___] := Scope[
  quiver = Quiver[quiver];
  notb = VertexCount[quiver] > 1;
  icon = Quiver[quiver,
    GraphLegend -> None,
    ArrowheadSize -> MediumSmall,
    ArrowheadShape -> {"Arrow", TwoWayStyle -> "OutClose"},
    ArrowheadStyle -> $LightGray,
    LabelCardinals -> True, VertexSize -> Huge,
    ImagePadding -> {{12, 12},{20, 20}}, ImageSize -> "ShortestEdge" -> 45,
    VertexColorFunction -> "Index",
    VertexCoordinates -> If[notb, CirclePoints @ VertexCount[quiver], {{0, 0}}]
  ];
  If[notb, icon //= CombineMultiedges];
  graph = LatticeGraph[quiver, args,
    VertexColorFunction -> "GeneratingVertex",
    VertexSize -> 1.2, ImageSize -> 120, GraphLegend -> None
  ];
  If[$plcOrientation === "Horizontal",
    Row[{graph, icon}, Spacer[15]],
    Labeled[graph, icon]
  ]
]

(**************************************************************************************************)

PackageExport["LatticeColoringRow"]

LatticeColoringRow[list_List, args___] :=
  MapSpacedRow[LatticeColoringPlot[#, args, "Orientation" -> "Vertical"]&, list];

(**************************************************************************************************)

PackageExport["LatticeColoringGrid"]

makeColoringGridEntry[label:(_String | _Integer | _Subscript), ___] :=
  {Item[Style[label, $LegendLabelStyle, 15, Bold], ItemSize -> {Full, 2}, Alignment -> Center], SpanFromLeft};

makeColoringGridEntry[None, ___] :=
  {" ", SpanFromLeft};

makeColoringGridEntry[quiver_List, args___] :=
  Map[makeColoringGridEntry[#, args]&, quiver];

makeColoringGridEntry[quiver_, args___] :=
  First @ LatticeColoringPlot[quiver, args];

LatticeColoringGrid[items_, args___] := Scope[
  entries = Map[Flatten[List[makeColoringGridEntry[#, args]]]&, items];
  entries = PadRight[entries, Automatic, ""];
  entries = Replace[entries, row:{_, SpanFromLeft, Repeated[""]} :> Replace[row, "" -> SpanFromLeft, {1}], {1}];
  Grid[
    entries,
    Spacings -> {0, 0}, ItemSize -> {All, 0},
    Alignment -> Center
  ]
];

