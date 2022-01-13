(**************************************************************************************************)

PackageExport["LookupVertexCoordinates"]

LookupVertexCoordinates[graph_Graph, vertexList_:All] := Scope[
  UnpackExtendedThemedOptions[graph,
    coordinateTransformFunction,
    vertexCoordinateRules, vertexCoordinateFunction
  ];

  $Graph = graph;
  SetAll[vertexList, VertexList @ graph];
  If[!ListQ[vertexList], ReturnFailed[]];

  coords = Which[
    coordinateTransformFunction =!= None || (vertexCoordinateRules === None && vertexCoordinateFunction === None),
      First @ ExtractGraphPrimitiveCoordinates @ graph
    ,
    RuleListQ @ vertexCoordinateRules,
      AppendTo[vertexCoordinateRules, _ -> None];
      VectorReplace[vertexList, vertexCoordinateRules]
    ,
    vertexCoordinateFunction =!= None,
      Map[vertexCoordinateFunction, vertexList]
    ,
    True,
      ReturnFailed[];
  ];

  AssociationThread[vertexList, coords]
]

(**************************************************************************************************)

PackageExport["ExtractGraphPrimitiveCoordinates"]

SetUsage @ "
ExtractGraphPrimitiveCoordinates[graph$] returns the pair {vcoords$, ecoords$}, where \
vcoords$ is a list of coordinate tuples in the same order as VertexList[graph$], and \
ecoords$ is a list of coordinate matrices in the same order as EdgeList[graph$].
"

ExtractGraphPrimitiveCoordinates::badvcoordrules = "VertexCoordinateRules is not a list of rules.";
ExtractGraphPrimitiveCoordinates::badvcoordfunc = "VertexCoordinateFunction did not return a valid result.";
ExtractGraphPrimitiveCoordinates::badvcoords = "Initial setting of VertexCoordinates is not a matrix of coordinates.";
ExtractGraphPrimitiveCoordinates::glayoutfail = "Failed to layout graph, using circle.";
ExtractGraphPrimitiveCoordinates::badctrans = "CoordinateTransformFunction produced invalid values, using circle.";

ExtractGraphPrimitiveCoordinates[graph_] := (*GraphCachedScope[graph, *) Scope[

  If[!GraphQ[graph], ReturnFailed[]];

  UnpackExtendedThemedOptions[graph, vertexLayout];
  If[vertexLayout =!= None, Return @ ExtractGraphPrimitiveCoordinatesNew[graph]];

  igraph = IndexEdgeTaggedGraph @ ToIndexGraph @ graph;
  If[!GraphQ[igraph], ReturnFailed[]];

  $Graph = graph;

  If[EdgeCount[$Graph] == VertexCount[$Graph] == 0,
    Return[{{}, {}}];
  ];

  UnpackAnonymousThemedOptions[graph, Automatic,
    graphLayout, vertexCoordinates, plotRange
  ];
  initialVertexCoordinates = vertexCoordinates;

  UnpackExtendedThemedOptions[graph,
    layoutDimension, extendedGraphLayout, viewOptions, coordinateTransformFunction, coordinateRotation,
    vertexCoordinateRules, vertexCoordinateFunction, selfLoopRadius, multiEdgeDistance, packingSpacing
  ];
    
  actualDimension = Which[
    ContainsQ[graphLayout, "Dimension" -> 3] || CoordinateMatrixQ[vertexCoordinates, 3], 3,
    ContainsQ[graphLayout, "Dimension" -> 2] || CoordinateMatrixQ[vertexCoordinates, 2], 2,
    True, Automatic
  ];
  graphLayout //= DeleteCases["Dimension" -> _];
  Which[
    actualDimension === layoutDimension === Automatic,
      actualDimension = 2,
    actualDimension === Automatic,
      actualDimension = layoutDimension,
    True,
      Null
  ];

  If[MatchQ[graphLayout, {___, "VertexLayout" -> _, ___}], graphLayout = Lookup[graphLayout, "VertexLayout"]];
  If[extendedGraphLayout =!= Automatic, graphLayout = extendedGraphLayout];
  SetAutomatic[graphLayout, {}];

  If[MemberQ[graphLayout, "NudgeDistance" -> _],
    nudgeDistance = FirstCase[graphLayout, ("NudgeDistance" -> d_) :> d];
    graphLayout //= DeleteOptions["NudgeDistance"];
  ,
    nudgeDistance = Automatic;
  ];

  vertexList = VertexList @ graph;
  vertexCount = Length @ vertexList;
  vertexCoordinates = ConstantArray[0., {vertexCount, actualDimension}];

  edgeList = EdgeList @ igraph;
  edgeCount = Length @ edgeList;
  edgeCoordinateLists = ConstantArray[{}, edgeCount];

  If[UndirectedGraphQ[igraph] || MixedGraphQ[igraph],
    edgeList //= CanonicalizeEdges];

  method = Match[graphLayout, s_String | {s_String, ___} :> s, Automatic];
  autoLayout = Match[graphLayout, {s_String, opts___} :> {opts}, {___String, opts___} :> opts, Automatic];

  If[method === "Linear", method = If[AcyclicGraphQ[UndirectedGraph @ graph], "Line", "Circle"]];
  Switch[method,
    "Line",
      graphLayout = autoLayout;
      SetAutomatic[initialVertexCoordinates, N[{# - 1, 0}& /@ Range[vertexCount]]],
    "Circle",
      graphLayout = autoLayout;
      SetAutomatic[initialVertexCoordinates, N @ RotateRight[CirclePoints @ vertexCount, 1]],
    "LayeredDigraphEmbedding",
      graphLayout //= ReplaceAll[
        Rule["RootVertex", v_] :> Rule["RootVertex", IndexOf[vertexList, v]]
      ],
    "Random",
      graphLayout = autoLayout;
      SetAutomatic[initialVertexCoordinates, RandomReal[{-1, 1}, {vertexCount, actualDimension}]],
    "Tree" | "CenteredTree" | "HorizontalTree" | "HorizontalCenteredTree" | "InvertedCenteredTree",
      graphLayout = {"LayeredDigraphEmbedding"};
      root = LookupExtendedOption[graph, GraphOrigin];
      If[root =!= None,
        rootIndex = IndexOf[vertexList, root];
        If[IntegerQ[rootIndex], AppendTo[graphLayout, "RootVertex" -> rootIndex]];
      ];
      If[StringContainsQ[method, "Horizontal"], AppendTo[graphLayout, "Orientation" -> Left]];
      If[StringContainsQ[method, "CenteredTree"], coordinateTransformFunction = "CenterHorizontal"];
      If[method === "HorizontalCenteredTree", coordinateTransformFunction = "CenterVertical"];
      If[StringContainsQ[method, "Inverted"], coordinateTransformFunction = {coordinateTransformFunction, "ReflectVertical"}];
    ,
    s_String /; !StringEndsQ[s, "Embedding"],
      graphLayout //= ReplaceAll[method -> (method <> "Embedding")],
    True,
      Null
  ];

  extraGraphOptions = {};
  If[(MultigraphQ[igraph] || !DuplicateFreeQ[Sort /@ Take[edgeList, All, 2]]),
    SetAutomatic[multiEdgeDistance, 0.2];
    AppendTo[extraGraphOptions, "MultiEdgeDistance" -> 2*multiEdgeDistance];
  ];

  If[packingSpacing =!= Automatic,
    AppendTo[extraGraphOptions, "PackingLayout" -> {"LayeredTop", "Padding" -> packingSpacing}];
  ];

  If[graphLayout === {}, graphLayout = Automatic];
  graphLayout = Prepend[extraGraphOptions, "VertexLayout" -> graphLayout];
  
  Which[
    vertexCoordinateFunction =!= None,
      initialVertexCoordinates = Map[vertexCoordinateFunction, vertexList];
      If[!CoordinateMatrixQ[initialVertexCoordinates, _],
        Message[ExtractGraphPrimitiveCoordinates::badvcoordfunc];
        initialVertexCoordinates = None;
      ,
      Switch[
        Last @ Dimensions @ initialVertexCoordinates,
        1,
          initialVertexCoordinates = AppendConstantColumn[initialVertexCoordinates, 0];
        2|3,
          Null,
        _,
          initialVertexCoordinates = DimensionReduce[initialVertexCoordinates, ReplaceAutomatic[layoutDimension, 3]];
        ];
      ];
    ,
    vertexCoordinateRules === None,
      Null
    ,
    RuleListQ @ vertexCoordinateRules,
      AppendTo[vertexCoordinateRules, _ -> None];
      initialVertexCoordinates = VectorReplace[vertexList, vertexCoordinateRules];
    ,
    True,
      Message[ExtractGraphPrimitiveCoordinates::badvcoordrules];
  ];

  If[ListQ @ initialVertexCoordinates,
    If[!CoordinateMatrixQ[initialVertexCoordinates],
      Message[ExtractGraphPrimitiveCoordinates::badvcoords];
      initialVertexCoordinates = Automatic;
    ,
      If[nudgeDistance =!= 0,
        initialVertexCoordinates = nudgeOverlappingVertices[initialVertexCoordinates, nudgeDistance, plotRange]];
    ];
  ];

  newGraph = If[actualDimension == 3, Graph3D, Graph][
    Range @ vertexCount, edgeList,
    VertexShapeFunction -> captureVertexCoordinates,
    EdgeShapeFunction -> captureEdgeCoordinates,
    GraphLayout -> graphLayout,
    VertexCoordinates -> initialVertexCoordinates
  ];

  gdResult = Check[GraphComputation`GraphDrawing @ newGraph, $Failed];

  If[FailureQ[gdResult] || !MatrixQ[vertexCoordinates] || !VectorQ[edgeCoordinateLists, MatrixQ],
    Message[ExtractGraphPrimitiveCoordinates::glayoutfail];
    Print["GraphLayout -> ", graphLayout];
    useFallbackLayout[];
    Goto[end];
  ];

  vertexCoordinates = ToPackedReal @ vertexCoordinates;
  correctSelfLoops[];

  If[UndirectedGraphQ[graph],
    edgeCoordinateLists = MapThread[orientEdgeCoords, {edgeCoordinateLists, edgeList}];
  ];

  applyCoordinateTransform[coordinateTransformFunction];
  If[!CoordinateMatrixQ[vertexCoordinates],
    Message[ExtractGraphPrimitiveCoordinates::badctrans];
    useFallbackLayout[];
    Goto[end];
  ];

  If[NumericQ[coordinateRotation],
    If[CoordinateMatrixQ[vertexCoordinates, 3],
      vertexCoordinates //= SphericalRotateVector[coordinateRotation];
      edgeCoordinateLists //= SphericalRotateVector[coordinateRotation];
    ,
      vertexCoordinates //= RotateVector[coordinateRotation];
      edgeCoordinateLists //= RotateVector[coordinateRotation];
    ];
  ];

  If[CoordinateMatrixQ[vertexCoordinates, 3] && layoutDimension == 2,
    SetAutomatic[viewOptions, $automaticViewOptions];
    viewOptions = Association[PlotRange -> CoordinateBounds[vertexCoordinates], viewOptions];
    viewTransform = ConstructGraphicsViewTransform[viewOptions];
    vertexCoordinates //= viewTransform;
    edgeCoordinateLists //= Map[viewTransform];
  ];

  Label[end];
  {ToPackedReal @ vertexCoordinates, ToPackedRealArrays @ edgeCoordinateLists}
];

orientEdgeCoords[coords_, _DirectedEdge] := coords;
orientEdgeCoords[coords_, UndirectedEdge[a_, b_, tag_]] := If[
  EuclideanDistance[
    First @ coords,
    Part[vertexCoordinates, Part[edgeList, tag, 1]]
  ] < 0.001,
  coords, Reverse @ coords
];

captureVertexCoordinates[coords_, vertex_, _] :=
  Part[vertexCoordinates, vertex] = coords;

captureEdgeCoordinates[coords_, edge_] :=
  Part[edgeCoordinateLists, Last @ edge] = coords;

useFallbackLayout[] := (
  vertexCoordinates = CirclePoints @ vertexCount;
  If[actualDimension === 3, vertexCoordinates //= AppendColumn @ Zeros @ vertexCount];
  edgeCoordinateLists = Part[vertexCoordinates, #]& /@ EdgePairs @ igraph;
)

correctSelfLoops[] := Scope[
  selfLoopIndices = SelectIndices[edgeCoordinateLists, selfLoopQ];
  If[selfLoopIndices === {}, Return[]];
  edgeCoordinateLists ^= MapIndices[fixSelfLoop, selfLoopIndices, edgeCoordinateLists];
];

selfLoopQ[coords_] := First[coords] == Last[coords];

(* fixSelfLoop[coords_] := Scope[
  terminus = First @ coords;
  mean = Mean @ coords;
  If[selfLoopRadius === Automatic,
    selfLoopRadius ^= EdgeLengthScale[edgeCoordinateLists, .5] / 4.0];
  radialVector = selfLoopRadius * Normalize[mean - terminus];
  center = terminus + radialVector;
  circlePoints = CirclePoints[center, {selfLoopRadius, ArcTan @@ (-radialVector)}, 16];
  AppendTo[circlePoints, First @ circlePoints];
  ToPackedReal @ Reverse @ circlePoints
]
 *)
fixSelfLoop[coords_] := Scope[
  terminus = First @ coords;
  If[selfLoopRadius === Automatic,
    selfLoopRadius ^= EdgeLengthScale[edgeCoordinateLists, .5] / 4.0];
  radialVector = selfLoopRadius * Normalize[mean - terminus];
  centeredCoords = PlusVector[coords, -terminus];
  scale = Norm @ Part[centeredCoords, Ceiling[Length[centeredCoords] / 2]];
  centeredCoords *= selfLoopRadius / (scale / 2);
  ToPackedReal @ PlusVector[centeredCoords, terminus]
]

(**************************************************************************************************)

PackageExport["ExtractGraphPrimitiveCoordinatesNew"]

(* swap this out when it is completely working *)

ExtractGraphPrimitiveCoordinates::badvcoordrules = "VertexCoordinateRules is not a list of rules.";
ExtractGraphPrimitiveCoordinates::badvcoordlen = "VertexCoordinates has length ``, but should have length ``.";
ExtractGraphPrimitiveCoordinates::badvcoords = "Initial setting of VertexCoordinates is not a matrix of coordinates.";
ExtractGraphPrimitiveCoordinates::glayoutfail = "Failed to layout graph, using circle.";
ExtractGraphPrimitiveCoordinates::badctrans = "CoordinateTransformFunction produced invalid values, using circle.";
ExtractGraphPrimitiveCoordinates::layoutobjres = "Layout object failed to returned a valid result.";

ExtractGraphPrimitiveCoordinatesNew[graph_] := Scope[

  igraph = IndexEdgeTaggedGraph @ ToIndexGraph @ graph;
  If[!GraphQ[igraph], ReturnFailed[]];

  $Graph = graph;
  If[EdgeCount[$Graph] == VertexCount[$Graph] == 0,
    Return[{{}, {}}];
  ];

  vertexList = VertexList @ graph;
  vertexCount = Length @ vertexList;
  vertexCoordinates = ConstantArray[0., vertexCount];
  edgeList = EdgeList @ igraph;
  edgeCount = Length @ edgeList;

  (* forgot why we do this? *)
  If[UndirectedGraphQ[igraph] || MixedGraphQ[igraph],
    edgeList //= CanonicalizeEdges];

  (* unpack options and extended options *)
  UnpackAnonymousThemedOptions[graph, Automatic,
    vertexCoordinates, plotRange
  ];

  UnpackExtendedThemedOptions[graph,
    vertexLayout, layoutDimension, viewOptions, coordinateTransformFunction, coordinateRotation, vertexOverlapResolution,
    vertexCoordinateRules, vertexCoordinateFunction, selfLoopRadius, multiEdgeDistance, packingSpacing
  ];
    
  (* process VertexCoordinateFunction, VertexCoordinateRules, and VertexCoordinates to produce a single
  VertexCoordinates element, which is either a list or Automatic. *)
  Which[
    vertexCoordinates =!= Automatic,
      If[ListQ[vertexCoordinates] && Length[vertexCoordinates] =!= vertexCount,
        Message[ExtractGraphPrimitiveCoordinates::badvcoordlen, Length[vertexCoordinates], vertexCount];
        vertexCoordinates = Automatic;
      ];
    ,
    vertexCoordinateFunction =!= None,
      vertexCoordinates = Map[vertexCoordinateFunction, vertexList];
      If[!CoordinateMatrixQ[vertexCoordinates, _],
        Message[ExtractGraphPrimitiveCoordinates::badvcoordfunc];
        vertexCoordinates = Automatic;
      ];
    ,
    vertexCoordinateRules === None,
      vertexCoordinates = Automatic;
    ,
    RuleListQ @ vertexCoordinateRules,
      AppendTo[vertexCoordinateRules, _ -> None];
      vertexCoordinates = VectorReplace[vertexList, vertexCoordinateRules];
    ,
    True,
      Message[ExtractGraphPrimitiveCoordinates::badvcoordrules];
      vertexCoordinates = Automatic;
  ];

  If[vertexCoordinates =!= Automatic,
    If[!CoordinateMatrixQ[vertexCoordinates, _],
      Message[ExtractGraphPrimitiveCoordinates::badvcoords];
      vertexCoordinates = Automatic;
    ,
      Switch[
        Last @ Dimensions @ vertexCoordinates,
        1,     vertexCoordinates = AppendConstantColumn[vertexCoordinates, 0],
        2|3,   Null,
        _,     vertexCoordinates = DimensionReduce[vertexCoordinates, ReplaceAutomatic[layoutDimension, 3], Method -> "LatentSemanticAnalysis"]
      ];
    ];
  ];

  SetAutomatic[vertexLayout, $defaultVertexLayout];
  SetAutomatic[multiEdgeDistance, 0.3];
  SetAutomatic[selfLoopRadius, 0.5];

  data = Association[
    "Graph" -> $Graph,
    "IndexGraph" -> igraph,
    "VertexCount" -> vertexCount,
    "EdgeCount" -> edgeCount,
    "IndexVertices" -> Range @ vertexCount,
    "IndexEdges" -> edgeList,
    "VertexCoordinates" -> vertexCoordinates,
    "LayoutDimension" -> layoutDimension,
    "SelfLoopRadius" -> selfLoopRadius,
    "MultiEdgeDistance" -> multiEdgeDistance,
    "VertexOverlapResolution" -> vertexOverlapResolution
  ];

  result = vertexLayout @ data;
  If[MatchQ[result, {_ ? CoordinateMatrixQ, _ ? CoordinateMatricesQ}],
    {$vertexCoordinates, $edgeCoordinateLists} = result;
  ,
    Message[ExtractGraphPrimitiveCoordinates::layoutobjres];
    $vertexCoordinates = CirclePoints @ vertexCount;
    $edgeCoordinateLists = Part[$vertexCoordinates, {#1, #2}]& @@@ edgeList;
  ];

  If[coordinateTransformFunction ~!~ None | {},
    applyCoordinateTransformNew[coordinateTransformFunction];
  ];

  If[NumericQ[coordinateRotation],
    If[CoordinateMatrixQ[vertexCoordinates, 3],
      $vertexCoordinates //= SphericalRotateVector[coordinateRotation];
      $edgeCoordinateLists //= SphericalRotateVector[coordinateRotation];
    ,
      $vertexCoordinates //= RotateVector[coordinateRotation];
      $edgeCoordinateLists //= RotateVector[coordinateRotation];
    ];
  ];

  If[CoordinateMatrixQ[vertexCoordinates, 3] && layoutDimension == 2,
    SetAutomatic[viewOptions, $automaticViewOptions];
    viewOptions = Association[PlotRange -> CoordinateBounds[vertexCoordinates], viewOptions];
    viewTransform = ConstructGraphicsViewTransform[viewOptions];
    $vertexCoordinates //= viewTransform;
    $edgeCoordinateLists //= Map[viewTransform];
  ];

  {ToPackedReal @ $vertexCoordinates, ToPackedRealArrays @ $edgeCoordinateLists}
]

$defaultVertexLayout := $defaultVertexLayout = SpringElectricalLayout[];

(* we'll remove this when we've fully transitioned to the new code *)
applyCoordinateTransformNew[trans_] := Block[{vertexCoordinates, edgeCoordinateLists},
  {vertexCoordinates, edgeCoordinateLists} = {$vertexCoordinates, $edgeCoordinateLists};
  applyCoordinateTransform[trans];
  {$vertexCoordinates, $edgeCoordinateLists} = {vertexCoordinates, edgeCoordinateLists};
];

(* TODO: moveVertex, which will update the vertex, find all edges that start or end there,
and shear them to match *)

(**************************************************************************************************)

PackageExport["LatticeLayout"]
PackageExport["BasisVectors"]
PackageExport["ScaleFactor"]

Options[LatticeLayout] = {BasisVectors -> Automatic, ScaleFactor -> 1};

LatticeLayout[opts:OptionsPattern[]][data_] := Scope[
  
  UnpackOptions[basisVectors, scaleFactor];

  UnpackAssociation[data, indexGraph, graph];

  {vertexCoordinates, visitedEdges} = LatticeQuiverCoordinates[graph, basisVectors];

  vertexCoordinates *= scaleFactor;

  edgePairs = EdgePairs @ indexGraph;

  wasVisited = ConstantArray[True, EdgeCount @ indexGraph];
  Part[wasVisited, visitedEdges] = True;

  edgeCoordinateLists = MapThread[makeLatticeEdge, {edgePairs, wasVisited}];

  {vertexCoordinates, edgeCoordinateLists}
]

makeLatticeEdge[pair_, True] := Part[vertexCoordinates, pair];
makeLatticeEdge[pair_, False] := Scope[
  {a, b} = Part[vertexCoordinates, pair];
  d = 0.25 * Normalize[b - a];
  e = rot90 @ d;
  ae = a + e;
  be = b + e;
  corn = 4;
  DiscretizeCurve[{a, ae - e/corn, ae, ae + d/corn, be - d/corn, be, be + e/corn, b}]
];

rot90[{x_, y_}] := {y, -x};
rot270[{x_, y_}] := {-y, x};

(**************************************************************************************************)

PackageExport["SmartLayout"]

$tetraGraph := $tetraGraph = UndirectedGraph[
  {1 -> 2, 2 -> 3, 3 -> 4, 4 -> 1, 1 -> 3, 2 -> 4},
  VertexCoordinates -> Append[CirclePoints[3], {0, 0}]
];

SmartLayout[][data_] := Scope[
  UnpackAssociation[data, vertexCount, edgeCount, indexGraph];
  ugraph = UndirectedGraph @ RemoveEdgeTags @ indexGraph;
  coords = Which[
    IsomorphicGraphQ[ugraph, $tetraGraph],
      getIsomorphicCoordinates[ugraph, $tetraGraph],
    vertexCount == (edgeCount + 1) && PathGraphQ[ugraph],
      getIsomorphicCoordinates[ugraph, PathGraph[vertexCount]],
    True,
      $Failed
  ];
  If[!FailureQ[coords],
    data["VertexCoordinates"] = coords;
    VertexEdgeCoordinateData[data, Automatic]
  ,
    VertexEdgeCoordinateData[data, {"SpringElectricalEmbedding"}]
  ]
]

getIsomorphicCoordinates[source_, target_] := Scope[
  iso = FindGraphIsomorphism[source, target];
  If[iso === {}, ReturnFailed[]];
  targetVertices = Lookup[First @ iso, VertexList @ source];
  LookupVertexCoordinates[target, targetVertices]
]

(**************************************************************************************************)

PackageExport["SpringLayout"]
PackageExport["Orientation"]

Options[SpringLayout] = {
  "EnergyControl" -> Automatic,
  "StepControl" -> Automatic,
  "StepLength" -> Automatic,
  "Tolerance" -> Automatic
};

SpringLayout[opts:OptionsPattern[]][data_] :=
  VertexEdgeCoordinateData[data, {"SpringEmbedding", opts}]

(**************************************************************************************************)

PackageExport["SpringElectricalLayout"]

Options[SpringElectricalLayout] = JoinOptions[
  "SpringConstant" -> 1,
  "RepulsiveForcePower" -> Automatic,
  SpringLayout
];

SpringElectricalLayout[opts:OptionsPattern[]][data_] :=
  VertexEdgeCoordinateData[data, {"SpringElectricalEmbedding", opts}]

(**************************************************************************************************)

PackageExport["TreeVertexLayout"]
PackageExport["Orientation"]
PackageExport["RootVertex"]

Options[TreeVertexLayout] = {Alignment -> None, Orientation -> Top, RootVertex -> Automatic, "Bubble" -> False};

TreeVertexLayout[OptionsPattern[]][data_] := Scope[
  UnpackAssociation[data, graph, indexGraph];
  UnpackOptions[alignment, orientation, rootVertex, bubble];

  rootIndex = Switch[rootVertex,
    None,       None,
    Automatic,  Automatic,
    "Source",   First[GraphSources @ SimpleGraph @ ExpandCardinalSetEdges @ indexGraph, None],
    _,          VertexIndex[graph, rootVertex]
  ];
  vertexLayout = {"LayeredDigraphEmbedding", "Orientation" -> orientation, "RootVertex" -> rootIndex};
  
  VertexEdgeCoordinateData[data, vertexLayout]
];

(**************************************************************************************************)

PackageExport["LinearLayout"]

Options[LinearLayout] = {
  Method -> Automatic,
  Orientation -> Automatic
};

$threePoints = CirclePoints[3];

LinearLayout[opts:OptionsPattern[]][data_] := Scope[
  UnpackOptions[method, orientation];
  UnpackAssociation[data, vertexCount];
  SetAutomatic[method, If[AcyclicGraphQ[UndirectedGraph @ data["Graph"]], "Line", "Circle"]];
  SetAutomatic[orientation, If[method === "Line", Left, If[vertexCount === 3, Top, Left]]];
  data["VertexCoordinates"] = orientationTransform[orientation] @ Switch[method,
    "Line",
      N[{# - 1, 0}& /@ Range[vertexCount]],
    "Circle",
      points = MapColumn[Minus, 2, CirclePoints[{1, Pi}, vertexCount]];
      If[vertexCount === 3, points //= TranslationTransform[{0.15, 0}];];
      points
    ,
    True,
      ReturnFailed[]
  ];
  VertexEdgeCoordinateData[data, Automatic]
];

orientationTransform = Case[
  Left        := Identity;
  Right       := RotationTransform[Pi];
  Top         := RotationTransform[-Pi/2];
  TopLeft     := RotationTransform[-Pi/4];
  TopRight    := RotationTransform[-3/4 Pi];
  Bottom      := RotationTransform[Pi/2];
  BottomLeft  := RotationTransform[Pi/4];
  BottomRight := RotationTransform[3/4 Pi];
];

(**************************************************************************************************)

PackageExport["VertexEdgeCoordinateData"]

(* this handles capturing the vertex and edge coordinates from a given graph, which must be an index graph.
this will be called by layout objects as necessary.
it expects to be given a VertexLayout, and will lay out both vertices and edges at once.
if VertexCoordinates is not Automatic, it will supersede the layout algorithm and only edges will be laid out.
self-loops will be manually corrected to have the correct scale
*)


VertexEdgeCoordinateData[data_Association, vertexLayout_] := Scope[

  UnpackAssociation[data,
    indexVertices, indexEdges, vertexCoordinates,
    selfLoopRadius, multiEdgeDistance, vertexOverlapResolution,
    layoutDimension
  ];

  $vertexCoordinates = ConstantArray[None, vertexCount];
  $edgeCoordinateLists = ConstantArray[{}, Length @ indexEdges];

  graphLayout = {
    "VertexLayout" -> vertexLayout,
    "SelfLoopRadius" -> selfLoopRadius,
    "MultiEdgeDistance" -> multiEdgeDistance,
    If[IntegerQ[layoutDimension], "Dimension" -> layoutDimension, Nothing]
  };

  newGraph = Graph[
    indexVertices, indexEdges,
    VertexShapeFunction -> captureVertexCoordinatesNew,
    EdgeShapeFunction -> captureEdgeCoordinatesNew,
    GraphLayout -> graphLayout,
    VertexCoordinates -> vertexCoordinates
  ];

  gdResult = Check[GraphComputation`GraphDrawing @ newGraph, $Failed];

  If[FailureQ[gdResult],
    Message[ExtractGraphPrimitiveCoordinates::glayoutfail];
    Print["GraphLayout -> ", graphLayout];
    Return @ $Failed;
  ];

  $vertexCoordinates //= ToPackedReal;
  correctSelfLoopsNew[selfLoopRadius];

  If[UndirectedGraphQ[graph],
    $edgeCoordinateLists = MapThread[orientEdgeCoordsNew, {$edgeCoordinateLists, indexEdges}];
  ];

  If[vertexOverlapResolution ~!~ None | 0 | 0.,
    $vertexCoordinates = nudgeOverlappingVertices[$vertexCoordinates, vertexOverlapResolution, plotRange];
  ];

  {$vertexCoordinates, $edgeCoordinateLists}
];

captureVertexCoordinatesNew[coords_, vertex_, _] :=
  Part[$vertexCoordinates, vertex] = coords;

captureEdgeCoordinatesNew[coords_, edge_] :=
  Part[$edgeCoordinateLists, Last @ edge] = coords;

orientEdgeCoordsNew[coords_, _DirectedEdge] := coords;
orientEdgeCoordsNew[coords_, UndirectedEdge[a_, b_, tag_]] := If[
  EuclideanDistance[
    First @ coords,
    Part[$vertexCoordinates, Part[edgeList, tag, 1]]
  ] < 0.001,
  coords, Reverse @ coords
];

correctSelfLoopsNew[selfLoopRadius_] := Scope[
  selfLoopIndices = SelectIndices[$edgeCoordinateLists, selfLoopQ];
  If[selfLoopIndices === {}, Return[]];
  SetAutomatic[selfLoopRadius, EdgeLengthScale[$edgeCoordinateLists, .5] / 4.0];
  $edgeCoordinateLists ^= MapIndices[fixSelfLoopNew[selfLoopRadius], selfLoopIndices, $edgeCoordinateLists];
];

selfLoopQ[coords_] := First[coords] == Last[coords];

fixSelfLoopNew[selfLoopRadius_][coords_] := Scope[
  If[Length[coords] === 2, Return @ coords];
  terminus = First @ coords;
  radialVector = selfLoopRadius * Normalize[Mean[coords] - terminus];
  centeredCoords = PlusVector[coords, -terminus];
  scale = Norm @ Part[centeredCoords, Ceiling[Length[centeredCoords] / 2]];
  centeredCoords *= selfLoopRadius / (scale / 2);
  ToPackedReal @ PlusVector[centeredCoords, terminus]
]

(**************************************************************************************************)

nudgeOverlappingVertices[coords_, nudgeDistance_, plotRange_] := Scope[
  nudgedCoords = coords; num = Length[coords];
  nudgeScale = nudgeDistance;
  If[nudgeScale === Automatic,
    nudgeScale = ChessboardDistance @@ If[
      CoordinateMatrixQ @ plotRange,
      Transpose @ plotRange,
      CoordinateBoundingBox @ coords
    ];
    nudgeScale = If[nudgeScale === 0, 1, Max[nudgeScale, 0.1]];
  ];
  dupPos = Select[Length[#] > 1&] @ PositionIndex[Round[coords, nudgeScale / 40]];
  is3D = CoordinateMatrixQ[coords, 3];
  If[Length[dupPos] === 0, Return @ coords];
  If[Length[dupPos] === 1 && num == 1,
    points = If[CoordinateMatrixQ[coords, 3],
      SpherePoints[num] * nudgeScale/5,
      CirclePoints[{nudgeScale/5, Tau * .25/num}, num]
    ];
    Return @ PlusVector[points, Mean @ coords]
  ];
  Scan[nudge, Values @ dupPos];
  nudgedCoords
];

nudge[{_}] := Null;
nudge[indices_] := nudge2 @ SortBy[indices, Part[vertexList, #]&];
nudge2[indices_] :=
  Part[nudgedCoords, indices] = Plus[
    Part[nudgedCoords, indices],
    nudgeScale/5 * If[is3D, SpherePoints, CirclePoints][Length @ indices]
  ];

(**************************************************************************************************)

ExtendedGraphPlot::badwrappedshape = "CoordinateTransformFunction -> ProjectionOnto[...] contains an invalid shape.";
ExtendedGraphPlot::badcoordtrans = "CoordinateTransformFunction -> `` issued messages on application.";
ExtendedGraphPlot::badcoordtransname = "CoordinateTransformFunction -> `` is not one of ``."

applyCoordinateTransform[Automatic|None] :=
  Null

applyCoordinateTransform[list_List] :=
  Scan[applyCoordinateTransform, list];

applyCoordinateTransform[f_] := Block[{res},
  res = Check[
    vertexCoordinates //= Map[f];
    edgeCoordinateLists = MatrixMap[f, edgeCoordinateLists];
  ,
    $Failed
  ];
  If[FailureQ[res], Message[ExtendedGraphPlot::badcoordtrans, f]];
];

applyRigidCoordinateTransform[f_] := Block[{res},
  res = Check[
    vertexCoordinates //= Map[f];
    edgeCoordinateLists //= Map[transformEdgePoints @ f];
  ,
    $Failed
  ];
  If[FailureQ[res], Message[ExtendedGraphPlot::badcoordtrans, f]];
];

transformEdgePoints[f_][{a_, b_}] := {f @ a, f @ b};
transformEdgePoints[f_][points_List] := Scope[
  {a, b} = FirstLast @ points;
  t = FindRigidTransform[N @ {a, b}, N @ {f @ a, f @ b}];
  points
]

applyCoordinateTransform["CenterMean"] := Scope[
  center = Mean @ vertexCoordinates;
  applyCoordinateTransform[TranslationTransform[-center]];
];

applyCoordinateTransform["CenterBounds"] := Scope[
  center = Mean @ CoordinateBoundingBox @ {vertexCoordinates, edgeCoordinateLists};
  applyCoordinateTransform[TranslationTransform[-center]];
];

applyCoordinateTransform["Snap"] :=
  applyCoordinateTransform[{"Snap", 10}];

applyCoordinateTransform[{"Snap", m_, nudge_:0.1}] := Scope[
  applyCoordinateTransform["CenterMean"];
  bounds = CoordinateBounds[edgeCoordinateLists];
  step = (EuclideanDistance @@@ bounds) / m;
  grid = Catenate @ CoordinateBoundsArray[bounds, step];
  applyNearest[grid];
  duplicateIndices = DuplicateIndices @ vertexCoordinates;
  newVertexCoordinates = vertexCoordinates;
  adjacencyTable = VertexAdjacencyTable @ $Graph;
  $nudge = nudge;
  Scan[
    index |-> (
      center = Mean @ Part[vertexCoordinates, Part[adjacencyTable, index]];
      Part[newVertexCoordinates, index] //= nudgeDuplicate[center]
    ),
    duplicateIndices, {2}
  ];
  vertexCoordinates ^= newVertexCoordinates;
  (* TODO: rigid transfrom the nudged edges *)
];

applyNearest[points_] := Scope[
  nearest = Nearest @ grid;
  applyRigidCoordinateTransform @ Function[p, First @ nearest[p, 1]];
];

nudgeDuplicate[z_][p_] := p + Normalize[Cross[z - p]] * Im[$nudge] + Normalize[z - p] * Re[$nudge];

DuplicateIndices[list_] :=
  Select[Length[#] > 1&] @ Values @ PositionIndex @ vertexCoordinates;

applyCoordinateTransform["RaiseHorizontalTreeNodes"] := raiseTreeNodes[True];
applyCoordinateTransform["RaiseVerticalTreeNodes"] := raiseTreeNodes[False];

(* tree does not put first layer of vertices in first row for some reason, this horrible piece of code corrects that! *)
raiseTreeNodes[horizontal_] := Scope[
  {fn1, fn2} = If[horizontal, {Last, First}, {First, Last}];

  rootIndex = If[horizontal, MaximumIndexBy[vertexCoordinates, Last], MinimumIndexBy[vertexCoordinates, First]];
  pointEdgeIntersect = If[horizontal, findPointEdgeIntersectionH, findPointEdgeIntersectionV];

  rootCoords = Part[vertexCoordinates, rootIndex];
  gen1Indices = VertexIndex[$Graph, #]& /@ VertexOutComponent[$Graph, Part[vertexList, rootIndex], {1}];
  gen1Coords = Part[vertexCoordinates, gen1Indices];
  edgePairs = EdgePairs @ $Graph;
  edgePairRange = AssociationRange @ edgePairs;
  layer1Pos = fn1 @ rootCoords + If[horizontal, -1, 1];
  ScanThread[{vertIndex, coord} |->
    If[fn1[coord] != layer1Pos,
      edgeIndex = edgePairRange[{rootIndex, vertIndex}];
      edgeSegment = Part[edgeCoordinateLists, edgeIndex];
      newCoord = First[pointEdgeIntersect[layer1Pos, #]& /@ SortBy[fn1] /@ Partition[edgeSegment, 2, 1]];
      Part[edgeCoordinateLists, edgeIndex] = {rootCoords, newCoord};
      Part[vertexCoordinates, vertIndex] = newCoord;
      outgoingIndices = Flatten @ Position[edgePairs, {vertIndex, _}, {1}];
      Part[edgeCoordinateLists, outgoingIndices] //= Map[{newCoord, Last @ #}&];
    ],
    {gen1Indices, gen1Coords}
  ];
]

applyCoordinateTransform["CenterHorizontal"|"CenterTree"] := centerTree[True];
applyCoordinateTransform["CenterVertical"] := centerTree[False];

centerTree[horizontal_] := Scope[
  {fn1, fn2} = If[horizontal, {Last, First}, {First, Last}];
  pointEdgeIntersect = If[horizontal, findPointEdgeIntersectionH, findPointEdgeIntersectionV];
  
  groups = GroupBy[vertexCoordinates, Round[fn1 @ #,.01]& -> fn2];
  keys = Keys @ groups;
  pseudoEdgeCoordinates = Catenate @ Outer[
    pointEdgeIntersect,
    keys, SortBy[fn1] /@ toEdgeListSegments /@ edgeCoordinateLists,
    1
  ];
  Graphics[{Red, Point @ pseudoEdgeCoordinates, Blue, Point @ vertexCoordinates}];
  groups = GroupBy[Join[vertexCoordinates, pseudoEdgeCoordinates], Round[fn1 @ #,.01]& -> fn2];
  min = Min @ groups;
  offsets = KeyValueMap[{#1, Mean[MinMax[#2]] - min}&, groups];
  offsetFn = Interpolation[offsets, InterpolationOrder -> 1];
  applyCoordinateTransform[{If[horizontal, "HorizontalWarp", "VerticalWarp"], offsetFn}];
]

toEdgeListSegments[e_] := If[Length[e] == 2, e, Splice @ Partition[e, 2, 1]];

findPointEdgeIntersectionH[y_, {{x1_, y1_}, {x2_, y2_}}] :=
  If[y1 <= y <= y2, {Lerp[x1, x2, (y - y1) / (y2 - y1)], y}, Nothing];

findPointEdgeIntersectionV[x_, {{x1_, y1_}, {x2_, y2_}}] :=
  If[x1 <= x <= x2, {x, Lerp[y1, y2, (x - x1) / (x2 - x1)]}, Nothing];

applyCoordinateTransform[{"HorizontalWarp", offsetFn_}] := (
  transFn = {x, y} |-> {x - offsetFn[y], y};
  vertexCoordinates = VectorApply[transFn, vertexCoordinates];
  edgeCoordinateLists = MatrixApply[transFn, edgeCoordinateLists];
);

applyCoordinateTransform[{"VerticalWarp", offsetFn_}] := (
  transFn = {x, y} |-> {x, y - offsetFn[x]};
  vertexCoordinates = VectorApply[transFn, vertexCoordinates];
  edgeCoordinateLists = MatrixApply[transFn, edgeCoordinateLists];
);

applyCoordinateTransform["Reflect"] := (
  applyCoordinateTransform[ScalingTransform[{-1, -1}]];
);

applyCoordinateTransform[{"Rotate", n_}] := (
  applyCoordinateTransform["CenterMean"];
  applyCoordinateTransform[RotationTransform[n * Degree]];
);

applyCoordinateTransform[{"Rotate", n_, p_}] :=
  applyCoordinateTransform[RotationTransform[n * Degree, p]];

applyCoordinateTransform[{"Radial", f_}] := Scope[
  applyCoordinateTransform["CenterMean"];
  applyCoordinateTransform[Normalize[#] * f[Norm[#]]&];
];

applyCoordinateTransform["PolarProjection"] :=
  applyCoordinateTransform[{"PolarProjection", 1}];

applyCoordinateTransform[{"PolarProjection", h_}] := Scope[
  applyCoordinateTransform["CenterMean"];
  applyCoordinateTransform[Apply[{x, y, z} |-> {x / (h-z), y/(h-z)}]];
];

$namedTransforms = <|
  "Rotate0" -> Identity,
  "Rotate45" -> RotationTransform[45 * Degree],
  "Rotate60" -> RotationTransform[60 * Degree],
  "Rotate120" -> RotationTransform[120 * Degree],
  "Rotate240" -> RotationTransform[240 * Degree],
  "Rotate300" -> RotationTransform[300 * Degree],
  "Rotate90" -> RotationTransform[90 * Degree],
  "Rotate180" -> RotationTransform[180 * Degree],
  "Rotate270" -> RotationTransform[270 * Degree],
  "ReflectDiagonal" -> ReflectionTransform[{1, 1}],
  "ReflectHorizontal" -> ReflectionTransform[{1, 0}],
  "ReflectVertical" -> ReflectionTransform[{0, 1}],
  "ShrinkHorizontal" -> ScalingTransform[{0.75, 1}],
  "ShrinkVertical" -> ScalingTransform[{1, 0.75}],
  "ExpandHorizontal" -> ScalingTransform[{1.25, 1}],
  "ExpandVertical" -> ScalingTransform[{1, 1.25}]
|>;

applyCoordinateTransform["BendVertical"] :=
  edgeCoordinateLists //= Map[bendVertical];

bendVertical[{a:{ax_, ay_}, b:{bx_, by_}}] := Scope[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  c = {bx, ay};
  ca = along[c, a, .25];
  cb = along[c, b, .25];
  Join[{a}, DiscretizeCurve[{ca, c, cb}], {b}]
];

bendVertical[line_] := line;

applyCoordinateTransform["SquareSelfLoops"] :=
  edgeCoordinateLists //= Map[squareSelfLoop];

squareSelfLoop[list:{a_, Repeated[_, {3, Infinity}], b_}] /; EuclideanDistance[a, b] < 0.01 := Scope[
  c = Mean @ list;
  {p1, p3} = {{xl, yl}, {xh, yh}} = CoordinateBoundingBox @ list;
  p2 = {xh, yl}; p4 = {xl, yh};
  ang = ArcTan @@ (a - c); ang *= 2/Pi;
  (* p4 p3
     p1 p2 *)
  {u, v, w, x} = Which[
    -0.5 <= ang < +0.5, (* E *) {p3, p4, p1, p2},
    +0.5 <= ang < +1.5, (* N *) {p4, p1, p2, p3},
    -1.5 <= ang < -0.5, (* S *) {p2, p3, p4, p1},
    True,               (* W *) {p1, p2, p3, p4}
  ];
  trunc = 0.5;
  v = (v * trunc + u * (1 - trunc));
  w = (w * trunc + x * (1 - trunc));
  DiscretizeCurve[{corner[a, u, v], corner[u, v, w], corner[v, w, x], corner[w, x, a], a}, BSplineCurve]
];

$cr = .1;
corner[a_, b_, c_] :=
  Splice @ {a, along[b, a, $cr], along[b, a, 0.8*$cr], b, along[b, c, 0.8*$cr], along[b, c, $cr]};

along[a_, b_, d_] := PointAlongLine[{a, b}, d];

squareSelfLoop[line_] := line;

applyCoordinateTransform[name_String] := Scope[
  trans = Lookup[$namedTransforms, name,
    Message[ExtendedGraphPlot::badcoordtransname, name, commaString @ Keys @ $namedTransforms];
    $Failed
  ];
  If[FailureQ[trans], ReturnFailed[]];
  applyCoordinateTransform @ trans
];

applyCoordinateTransform[ProjectionOnto[shape_]] := Block[{$rnf},
  $rnf = BoundaryProjection @ shape;
  If[FailureQ[$rnf], Message[ExtendedGraphPlot::badwrappedshape]; Return @ $Failed];
  vertexCoordinates //= $rnf;
  edgeCoordinateLists //= Map[projectLineOntoRNF];
];

projectLineOntoRNF = Case[
  {a_, b_} ? CoordinateMatrixQ /; (Head[$rnf] === RegionNearestFunction) :=
    $rnf @ Interpolated[a, b, 6];
  points_List ? CoordinateMatrixQ :=
    $rnf @ points;
  points_List := Print[points]; (* projectLineOntoRNF /@ points; *)
];

