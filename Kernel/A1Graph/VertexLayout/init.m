PublicFunction[LookupVertexCoordinates]

LookupVertexCoordinates[graph_Graph, vertexList_:All] := Scope[
  UnpackExtendedThemedOptions[graph,
    coordinateTransformFunction,
    vertexCoordinates, vertexCoordinateRules, vertexCoordinateFunction
  ];

  $Graph = graph;
  SetAll[vertexList, VertexList @ graph];
  If[!ListQ[vertexList], ReturnFailed[]];

  coords = Which[
    coordinateTransformFunction === None && MatrixQ[vertexCoordinates] && vertexCoordinateFunction === None,
      vertexCoordinates,
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

PublicSymbol[$LargeGraphVertexCutoff]

$LargeGraphVertexCutoff = 500;

(**************************************************************************************************)

PrivateFunction[ExtractGraphPrimitiveCoordinates]

SetUsage @ "
ExtractGraphPrimitiveCoordinates[graph$] returns the pair {vcoords$, ecoords$}, where \
vcoords$ is a list of coordinate tuples in the same order as VertexList[graph$], and \
ecoords$ is a list of coordinate matrices in the same order as EdgeList[graph$].
"

ExtractGraphPrimitiveCoordinates::badvcoordrules = "VertexCoordinateRules is not a list of rules.";
ExtractGraphPrimitiveCoordinates::badvcoordlen = "VertexCoordinates has length ``, but should have length ``.";
ExtractGraphPrimitiveCoordinates::badvcoords = "Initial setting of VertexCoordinates is not a matrix of coordinates.";
ExtractGraphPrimitiveCoordinates::glayoutfail = "Failed to layout graph, using circle.";
ExtractGraphPrimitiveCoordinates::badctrans = "CoordinateTransformFunction produced invalid values, using circle.";
ExtractGraphPrimitiveCoordinates::layoutobjres = "Layout object `` failed to returned a valid result.";

ExtractGraphPrimitiveCoordinates[graph_] := Scope[

  If[!GraphQ[graph], ReturnFailed[]];
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
    vertexCoordinates, plotRange, graphLayout
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

  SetAutomatic[vertexLayout,
    If[vertexCoordinates =!= Automatic,
      ManualVertexLayout[],
      $defaultVertexLayout
    ]
  ];
  SetAutomatic[multiEdgeDistance, 0.3];
  SetAutomatic[selfLoopRadius, 0.5];

  data = Association[
    "Graph" -> $Graph,
    "IndexGraph" -> igraph,
    "VertexCount" -> vertexCount,
    "EdgeCount" -> edgeCount,
    "IndexVertices" -> Range @ vertexCount,
    "IndexEdges" -> edgeList,
    "LargeGraphQ" -> (vertexCount > $LargeGraphVertexCutoff),
    "VertexCoordinates" -> vertexCoordinates,
    "LayoutDimension" -> layoutDimension,
    "SelfLoopRadius" -> selfLoopRadius,
    "MultiEdgeDistance" -> multiEdgeDistance,
    "VertexOverlapResolution" -> vertexOverlapResolution
  ];

  (* TODO: fully support old graphLayout specs *)
  Switch[graphLayout,
    _String,
      Return @ VertexEdgeCoordinateData[data, graphLayout],
    {_String, ___},
      Return @ VertexEdgeCoordinateData[data, First @ graphLayout],
    _,
      Null
  ];

  result = vertexLayout @ data;
  If[MatchQ[result, {_ ? CoordinateMatrixQ, _ ? CoordinateMatricesQ}],
    {$vertexCoordinates, $edgeCoordinateLists} = result;
  ,
    Message[ExtractGraphPrimitiveCoordinates::layoutobjres, Shallow @ vertexLayout];
    $vertexCoordinates = CirclePoints @ vertexCount;
    $edgeCoordinateLists = Part[$vertexCoordinates, {#1, #2}]& @@@ edgeList;
  ];

  If[coordinateTransformFunction ~!~ None | {},
    applyCoordinateTransform[coordinateTransformFunction];
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

(* TODO: moveVertex, which will update the vertex, find all edges that start or end there,
and shear them to match *)

(**************************************************************************************************)

PrivateFunction[VertexEdgeCoordinateData]

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
    VertexShapeFunction -> captureVertexCoordinates,
    EdgeShapeFunction -> captureEdgeCoordinates,
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
  correctSelfLoops[selfLoopRadius];

  If[vertexCoordinates =!= Automatic && MatchQ[Dimensions @ $edgeCoordinateLists, {_, 2, 2}],
    (* find simplistic edges that are likely to overlap vertices *)
    $edgeCoordinateLists = fixEdgeVertexIntersections[$vertexCoordinates, $edgeCoordinateLists]];

  If[UndirectedGraphQ[graph],
    $edgeCoordinateLists = MapThread[orientEdgeCoords, {$edgeCoordinateLists, indexEdges}];
  ];

  If[vertexOverlapResolution ~!~ None | 0 | 0.,
    $vertexCoordinates = nudgeOverlappingVertices[$vertexCoordinates, vertexOverlapResolution, plotRange];
  ];

  {$vertexCoordinates, $edgeCoordinateLists}
];

captureVertexCoordinates[coords_, vertex_, _] :=
  Part[$vertexCoordinates, vertex] = coords;

captureEdgeCoordinates[coords_, edge_] :=
  Part[$edgeCoordinateLists, Last @ edge] = coords;

orientEdgeCoords[coords_, _DirectedEdge] := coords;
orientEdgeCoords[coords_, UndirectedEdge[a_, b_, tag_]] := If[
  EuclideanDistance[
    First @ coords,
    Part[$vertexCoordinates, Part[edgeList, tag, 1]]
  ] < 0.001,
  coords, Reverse @ coords
];

correctSelfLoops[selfLoopRadius_] := Scope[
  selfLoopIndices = SelectIndices[$edgeCoordinateLists, selfLoopQ];
  If[selfLoopIndices === {}, Return[]];
  SetAutomatic[selfLoopRadius, EdgeLengthScale[$edgeCoordinateLists, .5] / 4.0];
  $edgeCoordinateLists ^= MapIndices[fixSelfLoop[selfLoopRadius], selfLoopIndices, $edgeCoordinateLists];
];

selfLoopQ[coords_] := First[coords] == Last[coords];

fixSelfLoop[selfLoopRadius_][coords_] := Scope[
  If[Length[coords] === 2, Return @ coords];
  terminus = First @ coords;
  radialVector = selfLoopRadius * Normalize[Mean[coords] - terminus];
  centeredCoords = PlusVector[coords, -terminus];
  scale = Norm @ Part[centeredCoords, Ceiling[Length[centeredCoords] / 2]];
  centeredCoords *= selfLoopRadius / (scale / 2);
  ToPackedReal @ PlusVector[centeredCoords, terminus]
]

(**************************************************************************************************)

fixEdgeVertexIntersections[vertexCoordinates_, edgeCoordinateLists_] := Scope[
  edgeLengths = Map[If[First[#] === Last[#], 0, LineLength[#]]&, edgeCoordinateLists];
  scale = Quantile[DeleteCases[edgeLengths, 0], .15];
  longIndices = SelectIndices[edgeLengths, GreaterEqualThan[scale * 2]];
  If[longIndices === {}, Return @ edgeCoordinateLists];
  vertexNearest = Nearest[vertexCoordinates];
  MapIndices[displaceVertexIntersectingEdges, longIndices, edgeCoordinateLists]
];

displaceVertexIntersectingEdges[{a_, b_}] := Scope[
  If[Min[EuclideanDistance[First @ a, First @ b], EuclideanDistance[Last @ a, Last @ b]] > scale/5, Return @ {a, b}];
  points = Take[Lerp[a, b, Into[30]], {2, 19}];
  nearest = Union @@ vertexNearest[points, {All, scale / 15}];
  nearest = DeleteCases[nearest, a | b];
  If[nearest === {}, Return @ {a, b}];
  dy = Normalize[b - a] / 4;
  dx = VectorRotate90[dy];
  {a, Splice @ halfBend[a - dy/2, dx, -dy], Splice @ Reverse @ halfBend[b + dy/2, dx, dy], b}
];


$bendMatrix = {{0.,0.},{0.04,0.19},{0.15,0.35},{0.31,0.46},{0.5,0.5},{0.69,0.46},{0.85,0.35},{0.96,0.19},{1.,0.}};
halfBend[p_, x_, y_] := Threaded[p] + Dot[$bendMatrix, ToPacked @ {x, y}];

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
    $vertexCoordinates //= Map[f];
    $edgeCoordinateLists = MatrixMap[f, $edgeCoordinateLists];
  ,
    $Failed
  ];
  If[FailureQ[res], Message[ExtendedGraphPlot::badcoordtrans, f]];
];

applyRigidCoordinateTransform[f_] := Block[{res},
  res = Check[
    $vertexCoordinates //= Map[f];
    $edgeCoordinateLists //= Map[transformEdgePoints @ f];
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
  center = Mean @ $vertexCoordinates;
  applyCoordinateTransform[TranslationTransform[-center]];
];

ExtendedGraph::noorigin = "Graph does not have a GraphOrigin set.";

applyCoordinateTransform["CenterOrigin"] := Scope[
  origin = LookupExtendedOption[$Graph, GraphOrigin];
  If[origin === None,
    Message[ExtendedGraph::noorigin]
  ,
    center = Part[$vertexCoordinates, VertexIndex[$Graph, origin]];
    applyCoordinateTransform[TranslationTransform[-center]]
  ];
];

applyCoordinateTransform["CenterBounds"] := Scope[
  center = Mean @ CoordinateBoundingBox @ {$vertexCoordinates, $edgeCoordinateLists};
  applyCoordinateTransform[TranslationTransform[-center]];
];

applyCoordinateTransform["Snap"] :=
  applyCoordinateTransform[{"Snap", 10}];

applyCoordinateTransform[{"Snap", m_, nudge_:0.1}] := Scope[
  applyCoordinateTransform["CenterMean"];
  bounds = CoordinateBounds[$edgeCoordinateLists];
  step = (EuclideanDistance @@@ bounds) / m;
  grid = Catenate @ CoordinateBoundsArray[bounds, step];
  applyNearest[grid];
  duplicateIndices = DuplicateIndices @ $vertexCoordinates;
  newVertexCoordinates = $vertexCoordinates;
  adjacencyTable = VertexAdjacencyTable @ $Graph;
  $nudge2 = nudge;
  Scan[
    index |-> (
      center = Mean @ Part[$vertexCoordinates, Part[adjacencyTable, index]];
      Part[newVertexCoordinates, index] //= nudgeDuplicate[center]
    ),
    duplicateIndices, {2}
  ];
  $vertexCoordinates ^= newVertexCoordinates;
  (* TODO: rigid transfrom the nudged edges *)
];

applyNearest[points_] := Scope[
  nearest = Nearest @ grid;
  applyRigidCoordinateTransform @ Function[p, First @ nearest[p, 1]];
];

nudgeDuplicate[z_][p_] := p + Normalize[Cross[z - p]] * Im[$nudge2] + Normalize[z - p] * Re[$nudge2];

DuplicateIndices[list_] :=
  Select[Length[#] > 1&] @ Values @ PositionIndex @ $vertexCoordinates;

applyCoordinateTransform["RaiseHorizontalTreeNodes"] := raiseTreeNodes[True];
applyCoordinateTransform["RaiseVerticalTreeNodes"] := raiseTreeNodes[False];

(* tree does not put first layer of vertices in first row for some reason, this horrible piece of code corrects that! *)
raiseTreeNodes[horizontal_] := Scope[
  {fn1, fn2} = If[horizontal, {Last, First}, {First, Last}];

  rootIndex = If[horizontal, MaximumIndexBy[$vertexCoordinates, Last], MinimumIndexBy[$vertexCoordinates, First]];
  pointEdgeIntersect = If[horizontal, findPointEdgeIntersectionH, findPointEdgeIntersectionV];

  rootCoords = Part[$vertexCoordinates, rootIndex];
  gen1Indices = VertexIndex[$Graph, #]& /@ VertexOutComponent[$Graph, Part[vertexList, rootIndex], {1}];
  gen1Coords = Part[$vertexCoordinates, gen1Indices];
  edgePairs = EdgePairs @ $Graph;
  edgePairRange = AssociationRange @ edgePairs;
  layer1Pos = fn1 @ rootCoords + If[horizontal, -1, 1];
  ScanThread[{vertIndex, coord} |->
    If[fn1[coord] != layer1Pos,
      edgeIndex = edgePairRange[{rootIndex, vertIndex}];
      edgeSegment = Part[$edgeCoordinateLists, edgeIndex];
      newCoord = First[pointEdgeIntersect[layer1Pos, #]& /@ SortBy[fn1] /@ Partition[edgeSegment, 2, 1]];
      Part[$edgeCoordinateLists, edgeIndex] = {rootCoords, newCoord};
      Part[$vertexCoordinates, vertIndex] = newCoord;
      outgoingIndices = Flatten @ Position[edgePairs, {vertIndex, _}, {1}];
      Part[$edgeCoordinateLists, outgoingIndices] //= Map[{newCoord, Last @ #}&];
    ],
    {gen1Indices, gen1Coords}
  ];
]

applyCoordinateTransform["CenterHorizontal"|"CenterTree"] := centerTree[True];
applyCoordinateTransform["CenterVertical"] := centerTree[False];

centerTree[horizontal_] := Scope[
  {fn1, fn2} = If[horizontal, {Last, First}, {First, Last}];
  pointEdgeIntersect = If[horizontal, findPointEdgeIntersectionH, findPointEdgeIntersectionV];
  
  groups = GroupBy[$vertexCoordinates, Round[fn1 @ #,.01]& -> fn2];
  keys = Keys @ groups;
  pseudoEdgeCoordinates = Catenate @ Outer[
    pointEdgeIntersect,
    keys, SortBy[fn1] /@ toEdgeListSegments /@ $edgeCoordinateLists,
    1
  ];
  Graphics[{Red, Point @ pseudoEdgeCoordinates, Blue, Point @ $vertexCoordinates}];
  groups = GroupBy[Join[$vertexCoordinates, pseudoEdgeCoordinates], Round[fn1 @ #,.01]& -> fn2];
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
  $vertexCoordinates = VectorApply[transFn, $vertexCoordinates];
  $edgeCoordinateLists = MatrixApply[transFn, $edgeCoordinateLists];
);

applyCoordinateTransform[{"VerticalWarp", offsetFn_}] := (
  transFn = {x, y} |-> {x, y - offsetFn[x]};
  $vertexCoordinates = VectorApply[transFn, $vertexCoordinates];
  $edgeCoordinateLists = MatrixApply[transFn, $edgeCoordinateLists];
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
  $edgeCoordinateLists //= Map[bendVertical];

bendVertical[{a:{ax_, ay_}, b:{bx_, by_}}] := Scope[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  c = {bx, ay};
  ca = ptAlong[c, a, .25];
  cb = ptAlong[c, b, .25];
  Join[{a}, DiscretizeCurve[{ca, c, cb}], {b}]
];

bendVertical[line_] := line;

applyCoordinateTransform["SquareSelfLoops"] :=
  $edgeCoordinateLists //= Map[squareSelfLoop];

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
  Splice @ {a, ptAlong[b, a, $cr], ptAlong[b, a, 0.8*$cr], b, ptAlong[b, c, 0.8*$cr], ptAlong[b, c, $cr]};

PrivateFunction[ptAlong]

ptAlong[a_, b_, d_] := Which[d <= 0, a, d >= EuclideanDistance[a, b], b, True, PointAlongLine[{a, b}, d]];

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
  $vertexCoordinates //= $rnf;
  $edgeCoordinateLists //= Map[projectLineOntoRNF];
];

projectLineOntoRNF = Case[
  {a_, b_} ? CoordinateMatrixQ /; (Head[$rnf] === RegionNearestFunction) :=
    $rnf @ Interpolated[a, b, 6];
  points_List ? CoordinateMatrixQ :=
    $rnf @ points;
  points_List := Print[points]; (* projectLineOntoRNF /@ points; *)
];

(**************************************************************************************************)

PublicFunction[GraphEmbeddingGallery]

$layouts = {
  "GravityEmbedding", "HighDimensionalEmbedding", "PlanarEmbedding",
  "SpectralEmbedding", "SpringElectricalEmbedding", "SpringEmbedding", "TutteEmbedding"
};

GraphEmbeddingGallery[g_Graph] := Table[Graph[g, GraphLayout -> layout, PlotLabel -> layout], {layout, $layouts}]
