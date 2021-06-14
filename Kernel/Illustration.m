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

(**************************************************************************************************)

PackageExport["HighlightCompassDomain"]

HighlightCompassDomain[graph_, cardinals_, color_] := Scope[
  region = ConnectedSubgraph[EdgePattern[_, _, Alternatives @@ cardinals]];
  arrowheadStyle = Append[All -> Transparent] @ KeyTake[LookupCardinalColors @ graph, cardinals];
  ExtendedGraph[graph, ArrowheadStyle -> arrowheadStyle, EdgeStyle -> VeryThick,
    ColorRules -> {region -> Opacity[1,color], All -> LightGray}, GraphLegend -> None]
];

(**************************************************************************************************)

PackageExport["CompassDiagram"]

CompassDiagram[compasses_, equivSets_, opts___] := Scope[
  cardinals = DeleteDuplicates[Join @@ Values @ compasses];
  equivSets = DeleteDuplicates @ Join[equivSets, Map[Negated, equivSets, {2}], List /@ cardinals];
  equivIndex = Map[Union @@ Part[equivSets, #]&, PositionIndex[equivSets, 2]];
  compassIndex = PositionIndex[compasses, 2] /. Key[k_] :> k;
  CollectTo[{$edges}, Do[
    createEdges[card, equivCard],
    {card, cardinals},
    {equivCard, equivIndex[card]}
  ]];
  compasses = Keys @ compasses;
  coords = {-#1, #2}& @@@ CirclePoints[Length @ compasses];
  ExtendedGraph[compasses, $edges, VertexCoordinates -> coords,
    opts,
    GraphLayout -> {"MultiEdgeDistance" -> 0.13},
    GraphLegend -> Automatic, Cardinals -> cardinals,
    ArrowheadShape -> {"PairedSquare", PairedDistance -> 0, NegationStyle -> "OverBar"},
    VertexLabelStyle -> {LabelPosition -> Automatic},
    BaselinePosition -> Center,
    VertexLabels -> "Name", ArrowheadSize -> Large
  ]
];

createEdges[card1_, card2_] := Outer[
  {comp1, comp2} |-> If[Order[comp1, comp2] == 1,
    Internal`StuffBag[$edges, DirectedEdge[comp1, comp2, CardinalSet[{card1, card2}]]]
  ],
  compassIndex @ card1, compassIndex @ StripNegated @ card2, 1
];

(**************************************************************************************************)

PackageExport["FadePathPlot"]

FadePathPlot[g_, v1_, v2_, dir_, c_:None, c2_:None] := Scope[
  mainPath = Line[{v1, v2}, dir];
  transportPath = If[c =!= None, Line @ {Offset[v2, Negated @ c], Offset[v2, c]}, Nothing];
  mainPathVertices = GraphRegion[g, mainPath];
  HighlightGraphRegion[g,
    {Style[transportPath, $Gray, PathStyle -> "DiskArrow", EdgeSetback -> 0, ArrowheadSize -> 3], mainPath},
    {"Replace", "FadeGraph", $Teal, PathRadius -> 2},
    GraphLegend -> None, VertexSize -> {v1 -> 8},
    Epilog -> If[c === None, None, FadeProtected @ {
      GraphicsValue[{"CardinalPrimitives", Disk[v2, 1], c}],
      Switch[c2,
        None, Nothing,
        _List,
        GraphicsValue[{"CardinalPrimitives", Disk[v2, 1], c2}, SetColorLightness[0.7]]]
    }]
  ]
];

FadePathPlot[g_, v1_, v2_, dir_, c_List] := Scope[
  mainPath = Line[{v1, v2}, dir];
  cLast = Last @ c;
  If[ListQ[cLast], cLast //= First];
  transportPath = Line @ {Offset[v2, Negated @ cLast], Offset[v2, cLast]};
  mainPathVertices = Part[GraphRegion[g, mainPath], 1, 1];
  If[Length[c] =!= Length[mainPathVertices], ReturnFailed[]];
  epilog = MapThread[
    {vertex, card} |-> GraphicsValue[
      c = If[ListQ[card], Alternatives @@ card, card];
      {"CardinalPrimitives", toCardinalEdgePattern[vertex, c], c}
    ],
    {mainPathVertices, c}
  ];
  HighlightGraphRegion[g,
    {Style[transportPath, $Gray, PathStyle -> "DiskArrow", EdgeSetback -> 0, ArrowheadSize -> 3], mainPath},
    {"Replace", "FadeGraph", $Teal, PathRadius -> 2},
    GraphLegend -> None, VertexSize -> {v1 -> 8},
    Epilog -> FadeProtected[epilog]
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
    ImagePadding -> {{20, 20}, {20, 0}}, Cardinals -> {"x", "a", "b", "c"},
    GraphOrigin -> LatticeVertex[{Floor[n/2], 0}]
  ]
];

mlv[x_, y_] := LatticeVertex[{Mod[x, $n], If[x == $n || x == -1, -y, y]}];

mobiousPatch[x_] := <|
  "x" -> Table[mlv[x, y] -> mlv[x + 1, y], {y, -1, 1}],
  toCards[x] -> {DirectedPath[mlv[x, -1], mlv[x, 0], mlv[x, 1]]}
|>;
toCardinalSet[{e_}] := e;
toCardinalSet[e_] := CardinalSet[e];
toCards[n_] := toCardinalSet @ Pick[{"a", "b", If[n < $n/2, Negated @ "c", "c"]}, {Part[$isA, n+1], Part[$isB, n+1], Part[$isC, n+1]}];

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
  CardinalColors -> None, VertexLabels -> Automatic, EdgeLabels -> "Cardinal",
  VertexCoordinates -> {{-1, 0}, {0, 0}, {1, 0}}, ImagePadding -> {{0,0}, {0, 25}}, EdgeLabelStyle -> {Spacings -> -0.3},
  GraphLayout -> {"MultiEdgeDistance" -> 0.6}, ArrowheadPosition -> 0.59, ImageSize -> "ShortestEdge" -> 55, ArrowheadSize -> Medium
];

(**************************************************************************************************)

PackageExport["SimpleLabeledQuiver"]

SimpleLabeledQuiver[args___] := Quiver[args, $simpleLabeledQuiverOpts];

$simpleLabeledQuiverOpts = Sequence[
  VertexLabels -> "Name",
  VertexCoordinates -> RotateLeft[CirclePoints[3],2],
  GraphLayout -> {"MultiEdgeDistance" -> 0.2}, ArrowheadPosition -> 0.59, ImageSize -> "ShortestEdge" -> 80, ArrowheadSize -> Medium
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
        Map[LatticeVertex @ ParseCardinalWord @ #&, VertexList @ fq],
        GraphVertexCoordinates @ fq
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
    VertexShapeFunction -> labels, VertexSize -> Inherited,
    VertexLabelStyle -> {LabelPosition -> Center, Background -> None, ZOrder -> 1},
    ArrowheadShape -> {"Line", EdgeThickness -> 2}, ArrowheadSize -> Medium, EdgeStyle -> {Thick, LightGray},
    ImageSize -> 400, ImagePadding -> 30
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
  path_String                   := Path[$v0, ParseCardinalWord @ path];
  paths_List                    := Map[parsePath, paths];
  Labeled[spec_, label_]        := Labeled[parsePath @ spec, label];
  Rule[paths_List, adj_List]    := Splice[parsePath[# -> adj]& /@ paths];
  Rule[path_String, adj_List]   := Path[$v0, ParseCardinalWord @ path, PathAdjustments -> adj];
  _ := $Failed;
];

FQPVertexIcon[opts_][path_] := Scope[
  hasClassLabel = False;
  If[MatchQ[path, Labeled[_, _]], hasClassLabel = True; {path, classLabel} = FirstLast @ path];
  hasPathLabel = !MatchQ[path, Path[_, {}, ___]];
  highlighted = HighlightGraphRegion[
    $fq, path, {$Teal, PathRadius->2, PathStyle -> "DiskArrow", EdgeSetback -> $setback, "Foreground"}, Sequence @@ opts,
    ImagePadding -> {10, 10},
    Frame -> True, FrameStyle -> {LightGray, Thin},
    GraphLegend -> None, ImageSize -> "ShortestEdge" -> 25, ArrowheadShape -> None, VertexSize -> Small,
    FrameLabel -> {
      Bottom -> If[!hasPathLabel, None, fmtPaths[path] /. Negated -> UnderNegatedForm],
      Top -> If[!hasClassLabel, None, Style[classLabel, FontColor -> Black, FontSize -> 10]]
    }
  ];
  ExtendedGraphPlot @ highlighted
];

fmtPaths = MatchValues[
  Path[_, word_, ___] := FormatCardinalWord[word, FontColor -> Gray, FontSize -> 10];
  list_List           := Row[fmtPaths /@ list, Style[",", Gray]];
];
