PublicFunction[IllustatePathsOnFundamentalQuiver]

IllustatePathsOnFundamentalQuiver[quiver_, pathSpecs_, opts___] := Scope[
  fquiver = If[PathRepresentationObjectQ[quiver], quiver["Quiver"], quiver];
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

PublicFunction[DrawFundamentalQuiverPath]

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

PublicFunction[PathQuiverPlot]

PathQuiverPlot[fq_, paths_, v0_, v0Label_, cardinalDirs_, pathOpts_List, opts___Rule] := Scope[
  If[!QuiverQ[fq], ReturnFailed[]];
  $fq = AnnotationDelete[fq, {ArrowheadShape, VertexLabels, EdgeLabels}]; (* <- so that the theme will take effect *)
  $fq = DropOptions[$fq, ImageSize];
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
        cardinalDirs = Repeat[{1, 0}, Length @ cardinals];
        $scaling = 0.0
      ];
      If[Length[cardinalDirs] =!= Length[cardinals], ReturnFailed[]];
      $cardinalDirs = AssociationThread[cardinals, cardinalDirs];
      $cardinalDirs = Join[$cardinalDirs, Map[Minus, KeyMap[Inverted, $cardinalDirs]]];
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

DefineGraphTheme["FundamentalQuiver",
  ImageSize -> "ShortestEdge" -> 60, VertexLabels -> "Name",
  GraphLayout -> LinearLayout[],
  Cardinals -> {"r", "b"},
  ImagePadding -> {10, 15},
  ArrowheadSize -> MediumLarge,
  VertexLabelBaseStyle -> $MathLabelStyle
];

(**************************************************************************************************)

DefineGraphTheme["PathQuiver",
  GraphOrigin -> LatticeVertex[{}], BaselinePosition -> Center,
  VertexSize -> Inherited,
  ArrowheadShape -> {"Line", EdgeThickness -> 2}, ArrowheadSize -> Medium, EdgeStyle -> LightGray,
  EdgeThickness -> Thick,
  ImageSize -> 400, ImagePadding -> 5, AspectRatioClipping -> False,
  GraphLegend -> None
];

(**************************************************************************************************)

wordToCoords = Case[
  {}          := {0, 0};
  path_List   := Total @ MapIndex1[$cardinalDirs[#1] / ($scaling * #2+0.33)&, path];
];

extractWord = Case[
  Path[_, word_, ___] := word;
  list_List           := % @ First @ list;
  Labeled[spec_, _]   := % @ spec;
];

parsePath = Case[
  path_String                   := Path[$v0, ToPathWord @ path];
  paths_List                    := Map[parsePath, paths];
  Labeled[spec_, label_]        := Labeled[parsePath @ spec, label];
  Rule[paths_List, adj_List]    := Splice[parsePath[# -> adj]& /@ paths];
  Rule[path_String, adj_List]   := Path[$v0, ToPathWord @ path, PathAdjustments -> adj];
  _ := $Failed;
];

(**************************************************************************************************)

DefineGraphTheme["PathQuiverIcon",
  VertexLabels -> None,
  Frame -> True, FrameStyle -> {LightGray, SlightlyThin}, PlotRangeClipping -> False,
  GraphLegend -> None, ImageSize -> "ShortestEdge" -> 20, ArrowheadShape -> None,
  VertexSize -> Medium, VertexStyle -> $LightGray
];

(**************************************************************************************************)

FQPVertexIcon[opts_][path_] := Scope[
  hasClassLabel = False;
  If[MatchQ[path, Labeled[_, _]], hasClassLabel = True; {path, classLabel} = FirstLast @ path];
  hasPathLabel = !MatchQ[path, Path[_, {}, ___]];
  highlighted = HighlightGraphRegion[
    $fq, path, {$Teal, PathRadius -> 2, PathStyle -> "DiskArrowDisk", EdgeSetback -> $setback, "Foreground"}, Sequence @@ opts,
    GraphTheme -> {"PathQuiverIcon", "FundamentalQuiver"},
    FrameLabel -> {
      Bottom -> fmtPaths @ path,
      Top -> If[!hasClassLabel, None, Style[classLabel, FontColor -> Black, FontSize -> 12]]
    }
  ];
  ExtendedGraphPlot @ highlighted
];

fmtPaths = Case[
  Path[_, {}|"", ___] := Style[CardinalSymbol["1"], FontColor -> Gray, FontSize -> 12];
  Path[_, word_, ___] := Style[WordForm @ word, FontColor -> Gray, FontSize -> 12];
  list_List           := Row[fmtPaths /@ list, Style[",", Gray], BaseStyle -> {}];
];

(**************************************************************************************************)

PublicFunction[PathQuiverComparisonPlot]

PathQuiverComparisonPlot[pq_, q_, baseVertex_:0, quotient_:False] := SpacedRow[
  If[quotient, PathQuotientSymbol, ForwardPathQuiverSymbol]["Q", baseVertex] -> ExtendedGraph[pq, GraphLegend -> None],
  "" -> ArrowheadLegend[LookupCardinalColors[q], ArrowheadShape -> "Line"],
  QuiverSymbol["Q"] -> ExtendedGraph[q, GraphLegend -> None],
  LabelStyle -> {FontSize -> 16},
  Spacings -> 30
];
