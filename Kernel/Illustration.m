PackageExport["IllustatePathsOnFundamentalQuiver"]

IllustatePathsOnFundamentalQuiver[quiver_, pathSpecs_, opts___] := Scope[
  fquiver = If[QuiverRepresentationObjectQ[quiver], quiver["Quiver"], quiver];
  quivers = Labeled[DrawFundamentalQuiverPath[fquiver, #1, #2, #3], WordForm @ #1]& @@@ pathSpecs;
  regionSpec = Style[Arrow @ Path[1, #1], #2, "Foreground", "SemitransparentArrowheads", PathRadius->3, EdgeSetback -> 3]& @@@ pathSpecs;
  lattice = LatticeQuiver[
    quiver, opts, GraphRegionHighlight -> regionSpec, ArrowheadSize -> 20,
    ImagePadding -> {{0, 20}, {20, 0}},
    ArrowheadShape -> "Line", GraphLegend -> None
  ];
  Row[Flatten[{lattice, quivers}]]
]


(**************************************************************************************************)

PackageExport["DrawFundamentalQuiverPath"]

DrawFundamentalQuiverPath[quiver_, path_, color_, adjustments_] := Scope[
  regionSpec = Style[
    Arrow[Path[GraphOrigin, path, PathAdjustments -> adjustments]], color,
    "Foreground", "SemitransparentArrowheads", PathStyle -> "DiskArrow",
    PathRadius -> 3, EdgeSetback -> 4, PathOutline -> False
  ];
  ExtendedGraph[quiver, GraphRegionHighlight -> regionSpec, ArrowheadSize -> Medium,
    ImagePadding -> 15, EdgeStyle -> LightGray,
    GraphLegend -> None, ArrowheadShape -> {"Line", EdgeThickness -> 2}]
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
      "Foreground", "SemitransparentArrowheads", PathRadius -> 3, EdgeSetback -> 2];

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
  ArrowheadShape -> {"Line", EdgeThickness -> 2},
  LabelCardinals->True, ImagePadding -> {{20,10},{20,10}},
  VertexLabels -> "Name",
  ImageSize -> ("ShortestEdge" -> 60)
];

(**************************************************************************************************)

PackageExport["ColoredGraph"]

$colorRules = <|
  "r" -> $Red, "b" -> $Blue, "g" -> $Green,
  "R" -> $LightRed, "B" -> $LightBlue, "G" -> $LightGreen,
  "o" -> $Orange, "t" -> $Teal, "p" -> $Pink,
  "O" -> $LightOrange, "T" -> $LightTeal, "P" -> $LightPink,
  "0" -> Black, "1" -> $DarkGray, "2" -> $Gray, "3" -> $LightGray, "4" -> White,
  "w" -> White
|>;

makeColor[str_String] := HumanBlend @ Lookup[$colorRules, Characters @ str, Nothing];

ColoredGraph[edges_List, opts___Rule] :=
  ColoredGraph[AllUniqueVertices @ edges, edges, opts];

ColoredGraph[vertices_List, edges_List, opts___Rule] := Scope[
  vertexColors = AssociationMap[makeColor, vertices];
  cardinalColors = If[Length[First @ edges] =!= 3, None,
    AssociationMap[makeColor, DeleteDuplicates @ Part[edges, All, 3]]
  ];
  ExtendedGraph[
    vertices, edges, opts,
    VertexColorFunction -> vertexColors,
    If[cardinalColors === None, ArrowheadShape -> "Line", CardinalColors -> cardinalColors],
    GraphTheme -> "ColoredGraph"
  ]
];

(**************************************************************************************************)

$coloredGraphThemeRules = {
    VertexSize -> 10,
    ArrowheadSize -> MediumSmall,
    ImageSize -> {50, 50},
    ImagePadding -> 10,
    SelfLoopRadius -> 0.4, MultiEdgeDistance -> 0.4,
    AspectRatioClipping -> False, (* Frame -> True, *)
    EdgeThickness -> 1, EdgeStyle -> Directive[{AbsoluteThickness[0], GrayLevel[0.7, 1]}],
    ArrowheadShape -> {"FlatArrow", BorderStyle -> Function[{Darker[#, .3], AbsoluteThickness[0]}]}
};

$GraphThemeData["ColoredGraph"] := $coloredGraphThemeRules;

(**************************************************************************************************)

PackageExport["PartialOrderGraph"]

PartialOrderGraph[vertices_, edges_, opts___Rule] := Scope[
  If[MatchQ[vertices, {__Graph} -> _List],
    {graphs, opts} = FirstLast @ vertices;
    plots = ExtendedGraphPlot[#, opts]& /@ graphs;
    vertices = Range @ Length @ vertices;
    shapes = AssociationThread[vertices, plots];
    vsize = 100;
  ,
    shapes = Automatic;
    vsize = 6;
  ];
  ExtendedGraph[vertices, edges,
    opts,
    VertexShapeFunction -> shapes,
    GraphLayout -> "LayeredDigraph", GraphOrigin -> First @ vertices,
    ArrowheadShape -> None, ArrowheadSize -> Huge,
    ImageSize -> 250, VertexSize -> vsize, EdgeThickness -> 3,
    CoordinateTransformFunction -> "CenterTree"
  ]
];

(**************************************************************************************************)

PackageExport["LatticeColoringPlot"]

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

LatticeColoringPlot[quiver_, args___] := Scope[
  quiver = Quiver[quiver];
  notb = VertexCount[quiver] > 1;
  icon = Quiver[quiver,
    GraphTheme -> "FundamentalColoringQuiver",
    SelfLoopRadius -> $plcSLR, ImageSize -> $plcIconSize,
    VertexCoordinates -> If[notb, CirclePoints @ VertexCount[quiver], {{0, 0}}]
  ];
  If[notb, icon //= CombineMultiedges];
  graph = LatticeGraph[quiver, args,
    VertexColorFunction -> "GeneratingVertex",
    VertexSize -> 1.2, ImageSize -> 150, GraphLegend -> None
  ];
  If[$plcOrientation === "Horizontal",
    Row[{graph, icon}, Spacer[15]],
    Labeled[graph, icon]
  ]
]

(**************************************************************************************************)

$fundamentalColoringQuiverThemeOpts = {
  ArrowheadSize -> MediumSmall,
  GraphLegend -> None,
  ArrowheadShape -> {"Arrow", TwoWayStyle -> "OutClose"},
  ArrowheadStyle -> $LightGray,
  LabelCardinals -> Below, VertexSize -> Huge,
  ImagePadding -> {{15, 15}, {20, 20}},
  VertexColorFunction -> "Index"
};

$GraphThemeData["FundamentalColoringQuiver"] := $fundamentalColoringQuiverThemeOpts;

(**************************************************************************************************)

PackageExport["LatticeColoringRow"]

$lcrMW = 3;

LatticeColoringRow[args___, MaxWidth -> m_] := Block[{$lcrMW = m}, LatticeColoringRow[args]];

LatticeColoringRow[list_List, args___] :=
  SpacedRow[
    LatticeColoringPlot[#, args, "Orientation" -> "Vertical"]& /@ list,
    MaxWidth -> $lcrMW
  ];

(**************************************************************************************************)

PackageExport["LatticeColoringGrid"]

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
  entries = Replace[entries, row:{_, SpanFromLeft, Repeated[""]} :> Replace[row, "" -> SpanFromLeft, {1}], {1}];
  Grid[
    entries,
    Spacings -> {0, 0}, ItemSize -> {All, 0},
    Alignment -> Center
  ]
];

(**************************************************************************************************)

PackageExport["PathPlot"]

PathPlot[graph_Graph, p_Path -> color_] :=
  PathPlot[graph, p, color];

PathPlot[graph_Graph, path_Path, color_:$Teal] :=
  HighlightGraphRegion[graph, path,
    {color, PathStyle -> "DiskArrow", PathRadius -> 3, DiskRadius -> 4, "Foreground", "SemitransparentArrowheads"},
    GraphLegend -> None
  ];

(**************************************************************************************************)

PackageExport["PathWordPlot"]

$pwpStyle = GrayLevel[0.25];
$pwpLabel = Word;

PathWordPlot[graph_Graph, p:Path[_, _String, ___, PathCancellation -> False, ___]] := Block[
  {$pathCancellation = False},
  PathWordPlot[graph, MapAt[ToPathWord, p, 2]]
];

PathWordPlot[graph_Graph, path:Path[start_, word_, ___]] :=
  Labeled[
    PathPlot[graph, path, $pwpStyle],
    $pwpLabel /. Word :> PathWordForm[start, ToPathWord @ word, pathEndVertex[graph, path]]
  ]

PathWordPlot[graph_Graph, Labeled[path_, label_]] := Scope[
  $pwpLabel = label; PathWordPlot[graph, path]
];

PathWordPlot[graph_Graph, Style[path_, color_]] := Scope[
  $pwpStyle = color; PathWordPlot[graph, path]
];

PathWordPlot[graph_Graph, None] :=
  inlineSymbol["\[UpTee]", 30];

pathEndVertex[graph_, path_] := Scope[
  end = Part[GraphRegion[graph, Take[path, 2]], 1, 1, -1];
  Part[VertexList @ graph, end]
];

(**************************************************************************************************)

PackageExport["PathConcatPlot"]

inlineSymbol[s_, args___] := Style[s, 20, args];

PathConcatPlot[args___, PathStyle -> style_] := Block[{$pwpStyle = style},
  PathConcatPlot[args]
];

PathConcatPlot[graph_, p1_, p2_, p3_] :=
  SpacedRow[
    PathWordPlot[graph, p1],
    inlineSymbol @ "\[Star]",
    PathWordPlot[graph, p2],
    inlineSymbol @ "=",
    PathWordPlot[graph, p3]
  ]

(**************************************************************************************************)

PackageExport["LargeSymbolForm"]

(**************************************************************************************************)

PackageExport["LargeSymbolForm"]

LargeSymbolForm[e_, opts___Rule] := inlineSymbol[e, opts];

(**************************************************************************************************)

PackageExport["PathComposePlot"]

PathComposePlot[args___, PathStyle -> style_] := Block[{$pwpStyle = style},
  PathComposePlot[args]
];

PathComposePlot[graph_, p1_, p2_, p3_] :=
  SpacedRow[
    PathWordPlot[graph, p1],
    inlineSymbol @ $PathComposeSymbol,
    PathWordPlot[graph, p2],
    inlineSymbol @ "=",
    PathWordPlot[graph, p3]
  ]

PathComposePlot[graph_, p1_, p2_, p3_, p4_] :=
  SpacedRow[
    PathWordPlot[graph, p1],
    inlineSymbol @ $PathComposeSymbol,
    PathWordPlot[graph, p2],
    inlineSymbol @ "=",
    PathWordPlot[graph, p3],
    inlineSymbol @ "=",
    PathWordPlot[graph, p4]
  ]
(**************************************************************************************************)

PackageExport["HighlightChartRegion"]

Options[HighlightChartRegion] = {
  "Color" -> Automatic,
  "Arrowheads" -> "Cardinals",
  "PreserveColors" -> True,
  "Lighter" -> 0,
  "Label" -> True
}

HighlightChartRegion[graph_, chart_, OptionsPattern[]] := Scope[
  UnpackOptions[color, arrowheads, preserveColors, lighter, label];
  cardinals = ChartSymbolCardinals @ chart;
  SetAutomatic[color, HumanBlend @ DeleteCases[$DarkGray] @ LookupCardinalColors[graph, cardinals]];
  If[lighter != 0, color = ColorConvert[MapAt[# - lighter&, ColorConvert[color, Hue], 2], RGBColor]];
  result = HighlightGraphRegion[
    graph,
    chart, {color,
      If[preserveColors, "ReplaceEdges", "Replace"],
      If[arrowheads === None, "HideArrowheads", Nothing],
      If[arrowheads === All, "FadeEdges", "FadeGraph"],
      Cardinals -> cardinals
    },
    If[arrowheads === None, ArrowheadShape -> None, Sequence @@ {}],
    EdgeThickness -> VeryThick,
    VisibleCardinals -> If[arrowheads === "Cardinals", cardinals, All],
    GraphLegend -> None
  ];
  If[label === True, result = Labeled[result, chart]];
  result
];

(**************************************************************************************************)

PackageExport["FadePathPlot"]

Options[FadePathPlot] = {
  "Labels" -> None,
  "HideArrowheads" -> True
};

FadePathPlot[g_, line_, OptionsPattern[]] := Scope[
  UnpackOptions[labels, hideArrowheads];
  If[labels =!= None,
    Return @ FadePathPlotWithLabels[g, line, labels, hideArrowheads]];
  initialVertices = Map[pathInitialVertex, ToList @ line];
  HighlightGraphRegion[g,
    {line, Point /@ initialVertices},
    {$Teal, RegionStyle -> "Highlight", "Replace", "FadeGraph", If[hideArrowheads, "HideArrowheads", Nothing], PathRadius -> 2, PointSize -> 8},
    GraphLegend -> None, VertexSize -> Map[# -> 8&, initialVertices]
  ]
];

pathInitialVertex[Line[{v1_, ___}, ___]] := v1;

FadePathPlotWithLabels[g_, Line[{v1_, v2_}, dir_], c_List, hideArrowheads_] := Scope[
  mainPath = Line[{v1, v2}, dir];
  cLast = Last @ c;
  If[ListQ[cLast], cLast //= First];
  If[RuleQ[cLast], cLast //= First];
  transportPath = Line @ {Offset[v2, Negated @ cLast], Offset[v2, cLast]};
  mainPathVertices = Part[GraphRegion[g, mainPath], 1, 1];
  If[Length[c] =!= Length[mainPathVertices], ReturnFailed[]];
  colors = LookupCardinalColors @ g;
  c = ReplaceAll[c, s_String /; KeyExistsQ[colors, s] :> Style[s, Italic, colors @ s]];
  color = colors @ StripNegated @ cLast;
  c = MapAt[Row[{"      ", #}]&, c, -1];
  HighlightGraphRegion[g,
    {Style[transportPath, color, PathStyle -> "DiskArrow", EdgeSetback -> 0, ArrowheadSize -> 3], mainPath},
    {"Replace", "FadeGraph", $Teal, PathRadius -> 2, If[hideArrowheads, "HideArrowheads", Nothing]},
    GraphLegend -> None, VertexSize -> {v1 -> 8},
    VertexLabels -> AssociationThread[IndexedVertex /@ mainPathVertices, c],
    VertexLabelStyle -> {LabelPosition -> Offset[{0, 3}], BaseStyle -> {FontColor -> $Gray, FontWeight -> Bold}}
  ]
];

toCardinalEdgePattern[v2_, c_] := EdgePattern[IndexedVertex @ v2, _, c];
toCardinalEdgePattern[v2_, Negated[c_]] := EdgePattern[_, IndexedVertex @ v2, c];

(**************************************************************************************************)

PackageExport["CompassPathPlot"]

CompassPathPlot[compass_, path_, color_:$Red] :=
  HighlightGraphRegion[compass,
    {Arrow[path]},
    {color, "Foreground", PathStyle -> "DiskArrow", PathRadius -> 3},
    Epilog -> GraphicsValue[{"CardinalPrimitives", All, _}]
  ];

(**************************************************************************************************)

PackageExport["MobiusStrip"]

MobiusStrip[n_, is3D_:False] := Scope[
  $n = n; tauN = Tau / n; $isA = Table[i <= n/2, {i, 0, n-1}];
  $isC = RotateLeft[$isA, Ceiling[n/3]];
  $isB = RotateRight[$isA, Ceiling[n/3]];
  edges = mobiousPatch /@ Range[0, n-1];
  vertices = Flatten @ Table[LatticeVertex[{x, y}], {x, 0, n-1}, {y, -1, 1}];
  coords = If[is3D,
    Catenate @ Table[TorusVector[{n, y}, {phi, phi/2}], {phi, 0, Tau - tauN, tauN}, {y, -1, 1}],
    First /@ vertices
  ];
  Quiver[vertices, edges,
    VertexCoordinates -> coords,
    EdgeShapeFunction -> If[is3D, Automatic, drawMobiusEdge],
    ImagePadding -> {{20, 20}, {20, 0}}, Cardinals -> {"x", "r", "g", "b"},
    ArrowheadPosition -> <|"r" -> 0.33, "g" -> 0.66, "b" -> {0.4, .6}, "x" -> 0.5|>,
    GraphOrigin -> LatticeVertex[{Floor[n/2], 0}], ArrowheadShape -> {"Line", EdgeThickness -> 2}
  ]
];

mlv[x_, y_] := LatticeVertex[{Mod[x, $n], If[x == $n || x == -1, -y, y]}];

mobiousPatch[x_] := <|
  "x" -> Table[mlv[x, y] -> mlv[x + 1, y], {y, -1, 1}],
  toCards[x] -> {DirectedPath[mlv[x, -1], mlv[x, 0], mlv[x, 1]]}
|>;
toCardinalSet[{e_}] := e;
toCardinalSet[e_] := CardinalSet[e];
toCards[n_] := toCardinalSet @ Pick[{"r", "g", If[n < $n/2, Negated @ "b", "b"]}, {Part[$isA, n+1], Part[$isB, n+1], Part[$isC, n+1]}];

drawMobiusEdge[assoc_] := Scope[
  UnpackAssociation[assoc, coordinates, arrowheads, shape, edgeIndex];
  {a, b} = {{ax, ay}, {bx, by}} = FirstLast[coordinates];
  lines = If[EuclideanDistance[a, b] > 1,
    ab = Normalize[b - a];
    ab *= If[Abs[First[ab]] > Abs[Last[ab]], {1, 0}, {0, 1}] * 0.8;
    {l, r} = {{b + ab, b}, {a, a - ab}};
    counter = assoc["Counter"];
    {shape /@ {l, r}, {Opacity[1, $DarkGray], Text[counter, Mean @ #, {0, 1.8}]& /@ {l, r}}}
  ,
    shape @ {a, b}
  ];
  Style[lines, arrowheads]
];

(**************************************************************************************************)

PackageExport["SimpleLabeledGraph"]

SimpleLabeledGraph[args___] := ExtendedGraph[args, GraphTheme -> "SimpleLabeledGraph"]

(**************************************************************************************************)

$simpleLabeledGraphOpts = {
  CardinalColors -> None,
  VertexLabels -> "Name",
  VertexLabelPosition -> Automatic,
  VertexLabelBaseStyle -> $MathLabelStyle,
  EdgeLabels -> "Cardinal",
  EdgeLabelBaseStyle -> $CardinalLabelStyle,
  EdgeLabelSpacing -> -0.3,
  ArrowheadShape -> {"Line", EdgeThickness -> 2},
  ImagePadding -> {{0,0}, {0, 25}},
  ExtendedGraphLayout -> "Linear",
  MultiEdgeDistance -> 0.3, ArrowheadPosition -> 0.525,
  ArrowheadSize -> Medium, ArrowheadStyle -> $Gray,
  ImageSize -> "ShortestEdge" -> 90
};

$GraphThemeData["SimpleLabeledGraph"] := $simpleLabeledGraphOpts;

(**************************************************************************************************)

PackageExport["SimpleLabeledQuiver"]

$rgbList = {"r", "g", "b"};
$abcList = {"a", "b", "c"};

SimpleLabeledQuiver[args___] := Scope[
  res = Quiver[args];
  cards = DeleteDuplicates @ EdgeTags[res];
  Which[
    SubsetQ[$rgbList, cards], cards = Select[$rgbList, MemberQ[cards, #]&],
    SubsetQ[$abcList, cards], cards = Select[$abcList, MemberQ[cards, #]&],
    True, Null
  ];
  ExtendedGraph[res, Cardinals -> cards, GraphTheme -> "SimpleLabeledQuiver"]
];

(**************************************************************************************************)

$simpleLabeledQuiverOpts = {
  VertexLabels -> "Name",
  VertexLabelPosition -> Automatic,
  VertexLabelBaseStyle -> $MathLabelStyle,
  ExtendedGraphLayout -> "Linear",
  MultiEdgeDistance -> 0.1,
  ArrowheadShape -> {"Line", EdgeThickness -> 2},
  ArrowheadPosition -> 0.59,
  ArrowheadSize -> Medium,
  ImageSize -> "ShortestEdge" -> 90
};

$GraphThemeData["SimpleLabeledQuiver"] := $simpleLabeledQuiverOpts;

(**************************************************************************************************)

PackageExport["PathQuiverPlot"]

PathQuiverPlot[fq_, paths_, v0_, v0Label_, cardinalDirs_, pathOpts_List, opts___Rule] := Scope[
  If[!QuiverQ[fq], ReturnFailed[]];
  $fq = AnnotationDelete[fq, {ArrowheadShape, VertexLabels, EdgeLabels}]; (* <- so that the theme will take effect *)
  $fq = DeleteOptions[$fq, ImageSize];
  $v0 = v0;
  $scaling = 1.0;
  cardinals = CardinalList @ fq;
  paths = parsePath /@ DeleteDuplicates[paths];
  If[ContainsQ[paths, $Failed], ReturnFailed[]];
  pathWords = extractWord /@ paths;
  UnpackStringOptions[{opts}, Automatic, additionalEdges, direction];
  {doForward, doReverse} = Switch[direction,
    "Forward" | Automatic, {True, False},
    "Backward", {False, True},
    "Both", {True, True},
    _, ReturnFailed[]
  ];
  Which[
    cardinalDirs === Inherited,
      coords = AssociationThread[
        Map[LatticeVertex @ ToPathWord @ #&, VertexList @ fq],
        First @ ExtractGraphPrimitiveCoordinates @ fq
      ];
    ,
    cardinalDirs =!= None,
      If[cardinalDirs === "Linear",
        cardinalDirs = ConstantArray[{1, 0}, Length @ cardinals];
        $scaling = 0.0
      ];
      If[Length[cardinalDirs] =!= Length[cardinals], ReturnFailed[]];
      $cardinalDirs = AssociationThread[cardinals, cardinalDirs];
      $cardinalDirs = Join[$cardinalDirs, Map[Minus, KeyMap[Negated, $cardinalDirs]]];
      coords = Map[wordToCoords, pathWords];
    ,
    cardinalDirs === None,
      coords = Automatic
  ];
  vertices = Map[LatticeVertex, pathWords];
  pathWords2 = DeepCases[#, Path[_, word_, ___] :> word]& /@ paths;
  pathWordIndex = PositionIndex[pathWords2, 2];
  pathKeys = DeleteCases[{}] @ Keys @ pathWordIndex;
  edges = DeleteDuplicates @ Flatten @ {
    If[doForward, Map[makeExtensionEdges[Most, Identity], pathKeys], Nothing],
    If[doReverse, Map[makeExtensionEdges[Rest, MirrorForm], pathKeys], Nothing]
  };
  If[additionalEdges =!= Automatic,
    edges = Join[edges, Map[parseAdditionalEdge, additionalEdges]];
  ];
  $setback = 3;
  pathOpts = DeleteCases[pathOpts, EdgeSetback -> v_ /; ($setback = v; True)];
  labels = AssociationThread[vertices, FQPVertexIcon[pathOpts] /@ paths];
  plot = Quiver[
    vertices, edges, FilterOptions @ opts,
    VertexCoordinates -> coords,
    VertexShapeFunction -> labels,
    Cardinals -> LookupExtendedOption[fq, Cardinals],
    GraphTheme -> "PathQuiver"
  ] // CombineMultiedges;
  LargeLabeled[plot, ForwardPathQuiverSymbol["Q", v0Label]]
];

makeExtensionEdges[wordFn_, mirrFn_][word_] := DirectedEdge[
  Part[vertices, removeSingleton @ pathWordIndex @ wordFn @ word],
  Part[vertices, removeSingleton @ pathWordIndex @ word],
  mirrFn @ Last @ word
]

parseAdditionalEdge[DirectedEdge[a_, b_, c_]] :=
  DirectedEdge[parseAdditionalEdgeVertex @ a, parseAdditionalEdgeVertex @ b, c];

parseAdditionalEdge[other_] := (Print[other]; $Failed);

parseAdditionalEdgeVertex[vertex_] := LatticeVertex @ extractWord @ parsePath @ vertex;

(**************************************************************************************************)

$fundamentalQuiverOpts = {
  ImageSize -> "ShortestEdge" -> 60, VertexLabels -> "Name",
  GraphLayout -> "Linear",
  Cardinals -> {"r", "b"},
  ImagePadding -> {10, 15},
  ArrowheadSize -> MediumLarge,
  VertexLabelBaseStyle -> $MathLabelStyle
}

$GraphThemeData["FundamentalQuiver"] := $fundamentalQuiverOpts;

(**************************************************************************************************)

$pathQuiverOpts = {
  GraphOrigin -> LatticeVertex[{}], BaselinePosition -> Center,
  VertexSize -> Inherited,
  ArrowheadShape -> {"Line", EdgeThickness -> 2}, ArrowheadSize -> Medium, EdgeStyle -> LightGray,
  EdgeThickness -> Thick,
  ImageSize -> 400, ImagePadding -> 5, AspectRatioClipping -> False,
  GraphLegend -> None
}

$GraphThemeData["PathQuiver"] := $pathQuiverOpts;

(**************************************************************************************************)

wordToCoords = MatchValues[
  {}          := {0, 0};
  path_List   := Total @ MapIndexed[$cardinalDirs[#1] / ($scaling * First[#2]+0.33)&, path];
];

extractWord = MatchValues[
  Path[_, word_, ___] := word;
  list_List           := % @ First @ list;
  Labeled[spec_, _]   := % @ spec;
];

parsePath = MatchValues[
  path_String                   := Path[$v0, ToPathWord @ path];
  paths_List                    := Map[parsePath, paths];
  Labeled[spec_, label_]        := Labeled[parsePath @ spec, label];
  Rule[paths_List, adj_List]    := Splice[parsePath[# -> adj]& /@ paths];
  Rule[path_String, adj_List]   := Path[$v0, ToPathWord @ path, PathAdjustments -> adj];
  _ := $Failed;
];

(**************************************************************************************************)

$pathQuiverIconOpts = {
  VertexLabels -> None,
  Frame -> True, FrameStyle -> {LightGray, SlightlyThin}, PlotRangeClipping -> False,
  GraphLegend -> None, ImageSize -> "ShortestEdge" -> 20, ArrowheadShape -> None,
  VertexSize -> Medium, VertexStyle -> $LightGray
};

$GraphThemeData["PathQuiverIcon"] := $pathQuiverIconOpts;

(**************************************************************************************************)

FQPVertexIcon[opts_][path_] := Scope[
  hasClassLabel = False;
  If[MatchQ[path, Labeled[_, _]], hasClassLabel = True; {path, classLabel} = FirstLast @ path];
  hasPathLabel = !MatchQ[path, Path[_, {}, ___]];
  highlighted = HighlightGraphRegion[
    $fq, path, {$Teal, PathRadius -> 2, PathStyle -> "DiskArrowDisk", EdgeSetback -> $setback, "Foreground"}, Sequence @@ opts,
    GraphTheme -> {"PathQuiverIcon", "FundamentalQuiver"},
    FrameLabel -> {
      Bottom -> If[!hasPathLabel, None, fmtPaths @ path],
      Top -> If[!hasClassLabel, None, Style[classLabel, FontColor -> Black, FontSize -> 12]]
    }
  ];
  ExtendedGraphPlot @ highlighted
];

fmtPaths = MatchValues[
  Path[_, word_, ___] := Style[WordForm @ word, FontColor -> Gray, FontSize -> 12];
  list_List           := Row[fmtPaths /@ list, Style[",", Gray], BaseStyle -> {FontTracking -> "Narrow"}];
];

(**************************************************************************************************)

PackageExport["PathQuiverComparisonPlot"]

PathQuiverComparisonPlot[pq_, q_, baseVertex_:0, quotient_:False] := SpacedRow[
  If[quotient, PathQuotientSymbol, ForwardPathQuiverSymbol]["Q", baseVertex] -> ExtendedGraph[pq, GraphLegend -> None],
  "" -> ArrowheadLegend[LookupCardinalColors[q], ArrowheadShape -> "Line"],
  QuiverSymbol["Q"] -> ExtendedGraph[q, GraphLegend -> None],
  LabelStyle -> {FontSize -> 16},
  Spacings -> 30
];

(**************************************************************************************************)

PackageExport["ExportNotebookOutputs"]

ExportNotebookOutputs[destination_String, prefix_String:"", sz_:3] := Scope[
  EnsureDirectory[destination];
  If[FileType[destination] =!= Directory, ReturnFailed[]];
  outputCells = NotebookImport[EvaluationNotebook[], "Output" -> "Cell"];
  Print["# of cells: ", Length @ outputCells];
  $i = 1;
  Scan[cell |-> (
    path = FileNameJoin[{destination, prefix <> IntegerString[$i++, 10, 3] <> ".png"}];
    image = Rasterize[cell, ImageFormattingWidth -> Infinity, ImageResolution -> Ceiling[144 * sz]];
    Print["Rasterizing ", ImageDimensions[image], " to ", path];
    Export[path, image])
  ,
    outputCells
  ];
];

(**************************************************************************************************)

PackageExport["PairwiseTable"]

PairwiseTable[f_, list_] :=
  TableForm[Outer[f, list, list, 1]];

(**************************************************************************************************)

PackageExport["VertexField1DPlot"]

VertexField1DPlot[vals_] := ListLinePlot[vals,
  Joined -> False, Filling -> {1 -> Axis},
  FillingStyle -> Directive[Opacity[.5],CapForm[None], AbsoluteThickness[2]], PlotMarkers->{Automatic, 0.12},
  Frame -> True, FrameStyle -> $LightGray, FrameTicks -> None, Axes -> None, GridLines -> {{}, {-0.025}},
  PlotStyle -> $DarkGray,
  PlotRange -> {{1, All}, {-1, 1}}, PlotRangePadding -> 0.4, ImageSize -> 125, AspectRatio -> 1/2.2
]

(**************************************************************************************************)

PackageExport["PathHomomorphimsGrid"]

Options[PathHomomorphimsGrid] = {
  "HighlightColor" -> $DarkGreen
}

PathHomomorphimsGrid[graphsAndPaths_:{Repeated[_-> _]}, OptionsPattern[]] := Scope[
  UnpackOptions[highlightColor];
  {graphs, paths} = KeysValues @ graphsAndPaths;
  If[!MatrixQ[paths], ReturnFailed[]];
  {graphs, labelRow} = Transpose @ Map[toPHGColumnLabel, graphs];
  entries = MapThread[
    MapThread[pathHomomorphismDiagram, {graphs, {##}}]&,
    paths
  ];
  alignment = {{Center}};
  If[First[graphs] === "Paths", PrependTo[alignment, Right]];
  If[Last[graphs] === "Paths", PrependTo[alignment, Left]];
  Grid[
    Prepend[labelRow] @ entries,
    Spacings -> {{0, {2}, 0}, {10., 0.5, {0}}}, Alignment -> {alignment, Baseline}
  ]
]

toPHGColumnLabel = Case[
  Labeled[g_Graph, label_]     := {g, Style[label, $LabelStyle, Bold]};
  g_Graph                      := {g, ""};
  s:(_String | _Form | _Style) := {"Path", s};
];

pathHomomorphismDiagram["Path", path_] :=
  path;

pathHomomorphismDiagram[graph_Graph, path_] := Scope[
  HighlightGraphRegion[graph,
    Style[path, highlightColor, PathRadius->2, DiskRadius -> 4,  ArrowheadSize -> 3.5, "Opaque", "SemitransparentArrowheads", PathStyle -> "DiskArrow"],
    VertexLabels->None,
    ArrowheadShape -> "Line", VertexStyle -> $LightGray, EdgeStyle -> $LightGray,
    ImagePadding -> {{15,15},{10,10}},
    ArrowheadPosition -> 0.55
  ]
];

(**************************************************************************************************)

PackageExport["GraphProductTable"]

Options[GraphProductTable] = {
  "Labels" -> {None, None},
  "UseCardinalSet" -> True,
  ArrowheadPosition -> 0.525
};

GraphProductTable[prodFn_, aList_, bList_, OptionsPattern[]] := Scope[
  UnpackOptions[labels, useCardinalSet, arrowheadPosition];
  aList = RotateGraph /@ aList;
  entries = Outer[
    prodFn[#1, #2,
      PackingSpacing -> 1, "UseCardinalSet" -> useCardinalSet,
      If[!useCardinalSet, ArrowheadShape -> None, Sequence @@ {}], MultiEdgeDistance -> 0.1,
      ArrowheadPosition -> arrowheadPosition, EdgeSetback -> .1
    ]&,
    aList,
    bList,
    1
  ];
  {aLabels, bLabels} = labels;
  table = PrependColumn[entries, aList];
  If[aLabels =!= None,
    blank = Splice[{"", ""}];
    table = PrependColumn[table, aLabels];
  ,
    blank = ""];
  PrependTo[table, Prepend[blank] @ bList];
  If[bLabels =!= None, PrependTo[table, Prepend[blank] @ bLabels]];
  Grid[table]
];

(**************************************************************************************************)

PackageExport["GraphProductUnionSpacedRow"]

productMeanPos[vertices_] := N @ Mean[List @@@ vertices];
GraphProductUnionSpacedRow[full_, items_, opts___Rule] := Scope[
  items = SortBy[items, ApplyThrough[{VertexList /* productMeanPos, VertexCount}]];
  items = ShowFaded[full, #, .85]& /@ items;
  SpacedRow[items, RiffleItem -> "\[Union]", opts, MaxWidth -> 6, RowSpacings -> 15]
];

(**************************************************************************************************)

PackageExport["ConnectedComponentProductDecomposition"]

Options[ConnectedComponentProductDecomposition] = JoinOptions[
  {MaxWidth -> 4, Spacings -> 15, Transposed -> False},
  ExtendedGraph
];

ConnectedComponentProductDecomposition[graphs_, terms_, userOpts:OptionsPattern[]] := Scope[
  If[graphs ~~~ l_Labeled,
    displayForm = toQuiverProductColumn @ Last @ graphs;
    graphs = First @ graphs;
  ,
    displayForm = {Automatic};
  ];
  UnpackOptions[maxWidth, spacings, transposed];
  opts = Sequence @@ DeleteOptions[{userOpts}, {MaxWidth, Spacings, Transposed}];
  base = GeneralQuiverProduct[graphs, terms, Automatic, opts,
    ImageSize -> 120, VertexSize -> 4, ArrowheadShape -> None,
    ExtendedGraphLayout ->{"NudgeDistance"->0}];
  products = GeneralQuiverProduct[graphs, terms, All, opts,
    ImageSize -> 120, VertexSize -> 4, ArrowheadSize -> 12];
  imgSize = First @ LookupImageSize[base];
  dislayForm = Replace[displayForm, {Automatic -> base, g_Graph :> ReplaceOptions[g, ImageSize -> imgSize]}, {1}];
  SpacedColumn[
    Sequence @@ dislayForm,
    LargeSymbolForm["="],
    GraphProductUnionSpacedRow[base, products,
      MaxWidth -> maxWidth, Spacings -> spacings, Transposed -> transposed],
    Spacings -> 45
  ]
];

toQuiverProductColumn = Case[
  IndependentQuiverProductForm[a_, b_] :=
    {toSimpleQuiver @ a, LargeSymbolForm @ IndependentQuiverProductForm[], toSimpleQuiver @ b};
  DependentQuiverProductForm[a_, b_] :=
    {toSimpleQuiver @ a, LargeSymbolForm @ DependentQuiverProductForm[], toSimpleQuiver @ b};
  other_ := {OnFailed[toSimpleQuiver @ other, other]};
];

(**************************************************************************************************)

PackageExport["QuiverProductTable"]

QuiverProductTable[quivers_, termsLists_, args___] := Scope[
  makePlot = Labeled[
    GeneralQuiverProduct[quivers, #2, args, ImageSize -> {100, 100}, ArrowheadStyle -> $Gray],
    Row[{Style[#1, Bold], Invisible @ "X"}]
  ]&;
  SpacedColumn[
    SpacedRow[makePlot @@@ #, Spacings -> 50, LabelSpacing -> 15]& /@ termsLists,
    Spacings -> 50
  ]
];