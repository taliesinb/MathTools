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
  ArrowheadShape -> {"Line", EdgeThickness -> 2},
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

$pwpStyle = $Teal;
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

inlineSymbol[s_, args___] := Labeled[Style[s, 20, args], ""]

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

LargeSymbolForm[e_] := inlineSymbol[e];

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
  cardinals = chartCardinals @ chart;
  SetAutomatic[color, HumanBlend @ LookupCardinalColors[graph, cardinals]];
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

PackageExport["CompassDiagram"]

Options[CompassDiagram] = JoinOptions[
  "ImpliedRelations" -> True, "Directed" -> False, "TransitionStyle" -> "Square", CombineMultiedges -> False,
  ExtendedGraph
];

CompassDiagram[compasses_, equivSets_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[impliedRelations, directed, transitionStyle, combineMultiedges];
  (* don't use pairing! *)
  cardinals = DeleteDuplicates[Join @@ Values @ compasses];
  If[MatchQ[equivSets, {__DirectedEdge}],
    manualEdges = equivSets;
    equivSets = {};
  ,
    manualEdges = {};
  ];
  If[impliedRelations,
    equivSets = DeleteDuplicates @ Join[equivSets, Map[Negated, equivSets, {2}], List /@ cardinals];
    equivIndex = Map[Union @@ Part[equivSets, #]&, PositionIndex[equivSets, 2]];
    compassIndex = PositionIndex[compasses, 2] /. Key[k_] :> k;
    CollectTo[{$edges}, Do[
      createCDEdges[card, equivCard],
      {card, cardinals},
      {equivCard, equivIndex[card]}
    ]];
  ,
    $edges = {}
  ];
  compasses = Keys @ compasses;
  coords = {-#1, #2}& @@@ CirclePoints[Length @ compasses];
  graph = ExtendedGraph[
    compasses, Join[$edges, manualEdges],
    VertexCoordinates -> coords,
    FilterOptions @ opts,
    GraphLayout -> {"MultiEdgeDistance" -> 0.13},
    GraphLegend -> Automatic, Cardinals -> cardinals,
    ArrowheadShape -> {
      If[transitionStyle === "Label", "Line", transitionStyle],
      PairedDistance -> 0, NegationStyle -> "OverBar",
      CardinalTransitionStyle -> transitionStyle
    },
    CardinalColors -> ChooseCardinalColors[cardinals],
    VertexLabelStyle -> {LabelPosition -> Automatic, BaseStyle -> {FontSize -> 12}},
    BaselinePosition -> Center, EdgeSetback -> .1,
    VertexLabels -> "Name", ArrowheadSize -> Medium
  ];
  If[combineMultiedges,
    graph = CombineMultiedges @ graph;
    If[transitionStyle === "Label",
      tags = EdgeTags @ graph;
      graph = ExtendedSubgraph[graph, All, Take[EdgeList @ graph, All, 2]];
      graph = ExtendedGraph[graph,
        EdgeLabels -> tags,
        EdgeLabelStyle -> {LabelPosition -> Scaled[0.5]},
        ArrowheadShape -> "Line", ArrowheadSize -> Small, ArrowheadPosition -> 0.9,
        GraphLegend -> None
      ]
    ];
  ];
  graph
];

createCDEdges[card1_, card2_] := Outer[
  {comp1, comp2} |-> If[Order[comp1, comp2] == 1,
    Internal`StuffBag[$edges, DirectedEdge[comp1, comp2, CardinalTransition[card1 -> card2]]]
  ],
  compassIndex @ card1, compassIndex @ StripNegated @ card2, 1
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
    Flatten[Table[TorusVector[{n, y}, {phi, phi/2}], {phi, 0, Tau - tauN, tauN}, {y, -1, 1}], 1],
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

SimpleLabeledGraph[args___] := ExtendedGraph[args, $simpleLabeledGraphOpts];

$simpleLabeledGraphOpts = Sequence[
  CardinalColors -> None, VertexLabels -> "Name", VertexLabelStyle -> {LabelPosition -> Automatic},
  EdgeLabels -> "Cardinal", ArrowheadShape -> {"Line", EdgeThickness -> 2},
  ImagePadding -> {{0,0}, {0, 25}}, EdgeLabelStyle -> {Spacings -> -0.3},
  GraphLayout -> {"Linear", "MultiEdgeDistance" -> 0.6}, ArrowheadPosition -> 0.59,
  ImageSize -> "ShortestEdge" -> 55, ArrowheadSize -> Medium, ArrowheadStyle -> $Gray
];

(**************************************************************************************************)

PackageExport["SimpleLabeledQuiver"]

$rgbList = {"r", "g", "b"};
$abcList = {"a", "b", "c"};

SimpleLabeledQuiver[args___] := Scope[
  res = Quiver[args, $simpleLabeledQuiverOpts];
  cards = DeleteDuplicates @ EdgeTags[res];
  Which[
    SubsetQ[$rgbList, cards], cards = Select[$rgbList, MemberQ[cards, #]&],
    SubsetQ[$abcList, cards], cards = Select[$abcList, MemberQ[cards, #]&],
    True, Null
  ];
  ExtendedGraph[res, Cardinals -> cards]
];

$simpleLabeledQuiverOpts = Sequence[
  VertexLabels -> "Name", VertexLabelStyle -> {LabelPosition -> Automatic},
  GraphLayout -> {"Linear", "MultiEdgeDistance" -> 0.2}, ArrowheadShape -> {"Line", EdgeThickness -> 2},
  ArrowheadPosition -> 0.59, ImageSize -> "ShortestEdge" -> 80, ArrowheadSize -> Medium
];

(**************************************************************************************************)

PackageExport["PathQuiverPlot"]

PathQuiverPlot[fq_, paths_, v0_, cardinalDirs_, pathOpts_List, opts___Rule] := Scope[
  If[!QuiverQ[fq], ReturnFailed[]];
  $v0 = v0; $fq = fq; $scaling = 1.0;
  cardinals = CardinalList @ fq;
  paths = parsePath /@ DeleteDuplicates[paths];
  If[ContainsQ[paths, $Failed], ReturnFailed[]];
  pathWords = extractWord /@ paths;
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
  edges = DeleteDuplicates @ Map[
    word |-> DirectedEdge[
      Part[vertices, removeSingleton @ pathWordIndex @ Most @ word],
      Part[vertices, removeSingleton @ pathWordIndex @ word],
      Last @ word
    ],
    DeleteCases[{}] @ Keys @ pathWordIndex
  ];
  $setback = 3;
  pathOpts = DeleteCases[pathOpts, EdgeSetback -> v_ /; ($setback = v; True)];
  labels = AssociationThread[vertices, FQPVertexIcon[pathOpts] /@ paths];
  Quiver[
    vertices, edges, opts,
    VertexCoordinates -> coords,
    GraphOrigin -> LatticeVertex[{}], BaselinePosition -> Center,
    VertexShapeFunction -> labels, VertexSize -> Inherited,
    ArrowheadShape -> {"Line", EdgeThickness -> 2}, ArrowheadSize -> Medium, EdgeStyle -> LightGray,
    EdgeThickness -> Thick, Cardinals -> LookupExtendedGraphAnnotations[fq, Cardinals],
    ImageSize -> 400, ImagePadding -> 5, AspectRatioClipping -> False
  ] // CombineMultiedges
];

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

FQPVertexIcon[opts_][path_] := Scope[
  hasClassLabel = False;
  If[MatchQ[path, Labeled[_, _]], hasClassLabel = True; {path, classLabel} = FirstLast @ path];
  hasPathLabel = !MatchQ[path, Path[_, {}, ___]];
  highlighted = HighlightGraphRegion[
    $fq, path, {$Teal, PathRadius->2, PathStyle -> "DiskArrow", EdgeSetback -> $setback, "Foreground"}, Sequence @@ opts,
    ImagePadding -> {10, 8}, VertexLabels -> None,
    Frame -> True, FrameStyle -> {LightGray, Thin}, PlotRangeClipping -> False,
    GraphLegend -> None, ImageSize -> "ShortestEdge" -> 25, ArrowheadShape -> None, VertexSize -> Small,
    FrameLabel -> {
      Bottom -> If[!hasPathLabel, None, fmtPaths @ path],
      Top -> If[!hasClassLabel, None, Style[classLabel, FontColor -> Black, FontSize -> 10]]
    }
  ];
  ExtendedGraphPlot @ highlighted
];

fmtPaths = MatchValues[
  Path[_, word_, ___] := Style[WordForm @ word, FontColor -> Gray, FontSize -> 10];
  list_List           := Row[fmtPaths /@ list, Style[",", Gray]];
];

(**************************************************************************************************)

PackageExport["PathQuiverComparisonPlot"]

PathQuiverComparisonPlot[pq_, q_, baseVertex_:None, quotient_:False] := SpacedRow[
  If[quotient, PathQuotientSymbol, ForwardPathQuiverSymbol]["Q", baseVertex] -> ExtendedGraph[pq, GraphLegend -> None],
  "" -> ArrowheadLegend[LookupCardinalColors[q], "Line"],
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
  PlotRange -> {{1, All}, {-1, 1}}, PlotRangePadding -> 0.25, ImageSize -> 90, AspectRatio -> 1/2.2
]