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
      F @ ExtractGraphPrimitiveCoordinates @ graph
    ,
    RuleListQ @ vertexCoordinateRules,
      AppTo[vertexCoordinateRules, _ -> None];
      VectorReplace[vertexList, vertexCoordinateRules]
    ,
    vertexCoordinateFunction =!= None,
      Map[vertexCoordinateFunction, vertexList]
    ,
    True,
      ReturnFailed[];
  ];

  AssocThread[vertexList, coords]
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
ExtractGraphPrimitiveCoordinates::badvcoordfunc = "Setting of VertexCoordinateFunction -> `` is not valid.";
ExtractGraphPrimitiveCoordinates::glayoutfail = "Failed to layout graph, using circle.";
ExtractGraphPrimitiveCoordinates::badctrans = "CoordinateTransformFunction produced invalid values, using circle.";
ExtractGraphPrimitiveCoordinates::badvertexlayout = "Invalid result returned by VertexLayout object ``.";
ExtractGraphPrimitiveCoordinates::badedgelayout = "Invalid result returned by EdgeLayout object ``.";

ExtractGraphPrimitiveCoordinates[graph_] := Scope[

  If[!GraphQ[graph], ReturnFailed[]];
  igraph = IndexEdgeTaggedGraph @ ToIndexGraph @ graph;
  If[!GraphQ[igraph], ReturnFailed[]];

  $Graph = graph;
  If[EdgeCount[$Graph] == VertexCount[$Graph] == 0,
    Return[{{}, {}}];
  ];

  vertexList = VertexList @ graph;
  vertexCount = Len @ vertexList;
  vertexCoordinates = Repeat[0., vertexCount];
  edgeList = EdgeList @ igraph;
  edgeCount = Len @ edgeList;

  (* forgot why we do this? *)
  If[UndirectedGraphQ[igraph] || MixedGraphQ[igraph],
    edgeList //= CanonicalizeEdges];

  (* unpack options and extended options *)
  UnpackAnonymousThemedOptions[graph, Auto,
    vertexCoordinates, plotRange, graphLayout
  ];

  UnpackExtendedThemedOptions[graph,
    vertexLayout, layoutDimension, viewOptions, coordinateTransformFunction, coordinateRotation, vertexOverlapResolution,
    vertexCoordinateRules, vertexCoordinateFunction, selfLoopRadius, multiEdgeDistance, packingSpacing,
    edgeLayout, additionalVertexLayoutOptions
  ];

  (* process VertexCoordinateFunction, VertexCoordinateRules, and VertexCoordinates to produce a single
  VertexCoordinates element, which is either a list or Automatic. *)
  Which[
    vertexCoordinates =!= Auto,
      If[ListQ[vertexCoordinates] && Len[vertexCoordinates] =!= vertexCount,
        Message[ExtractGraphPrimitiveCoordinates::badvcoordlen, Len[vertexCoordinates], vertexCount];
        vertexCoordinates = Auto;
      ];
    ,
    vertexCoordinateFunction =!= None,
      vertexCoordinates = Map[vertexCoordinateFunction, vertexList];
      If[!CoordinateMatrixQ[vertexCoordinates, _],
        Message[ExtractGraphPrimitiveCoordinates::badvcoordfunc, MsgExpr @ vertexCoordinateFunction];
        vertexCoordinates = Auto;
      ];
    ,
    vertexCoordinateRules === None,
      vertexCoordinates = Auto;
    ,
    RuleListQ @ vertexCoordinateRules,
      AppTo[vertexCoordinateRules, _ -> None];
      vertexCoordinates = VectorReplace[vertexList, vertexCoordinateRules];
    ,
    True,
      Message[ExtractGraphPrimitiveCoordinates::badvcoordrules];
      vertexCoordinates = Auto;
  ];

  If[vertexCoordinates =!= Auto,
    If[!CoordinateMatrixQ[vertexCoordinates, _],
      Message[ExtractGraphPrimitiveCoordinates::badvcoords];
      vertexCoordinates = Auto;
    ,
      Switch[
        L @ Dims @ vertexCoordinates,
        1,     vertexCoordinates = AppendConstantColumn[vertexCoordinates, 0],
        2|3,   Null,
        _,     vertexCoordinates = DimensionReduce[vertexCoordinates, SubAuto[layoutDimension, 3], Method -> "LatentSemanticAnalysis"]
      ];
    ];
  ];

  SetAuto[vertexLayout,
    If[vertexCoordinates =!= Auto,
      ManualVertexLayout[],
      $defaultVertexLayout
    ]
  ];
  SetAuto[multiEdgeDistance, 0.3];
  SetAuto[selfLoopRadius, 0.5];

  data = Assoc[
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
    _Str,
      Return @ VertexEdgeCoordinateData[data, graphLayout],
    {_Str, ___},
      Return @ VertexEdgeCoordinateData[data, F @ graphLayout],
    _,
      Null
  ];

  If[additionalVertexLayoutOptions =!= {},
    With[{seq = Sequence @@ additionalVertexLayoutOptions},
      vertexLayout = Insert[vertexLayout, Uneval @ seq, FirstIndex[vertexLayout, _Rule, -1]]
    ];
  ];

  result = vertexLayout @ data;
  If[MatchQ[result, {_ ? CoordinateMatrixQ, _ ? CoordinateMatricesQ}],
    {$vertexCoordinates, $edgeCoordinateLists} = result;
  ,
    Message[ExtractGraphPrimitiveCoordinates::badvertexlayout, MsgExpr @ vertexLayout];
    If[ListQ[result], PrintPF /@ result, PrintPF[result]];
    $vertexCoordinates = CirclePoints @ vertexCount;
    $edgeCoordinateLists = Part[$vertexCoordinates, {#1, #2}]& @@@ edgeList;
  ];

  If[!MatchQ[edgeLayout, None | Auto],
    data["VertexCoordinates"] = $vertexCoordinates;
    data["EdgeCoordinateLists"] = $edgeCoordinateLists;
    result = edgeLayout @ data;
    If[!CoordinateMatricesQ[result],
      Message[ExtractGraphPrimitiveCoordinates::badedgelayout, MsgExpr @ edgeLayout],
      $edgeCoordinateLists = ToPackedReal @ result;
    ];
  ];

  If[coordinateTransformFunction ~!~ None | {},
    applyCoordinateTransform[coordinateTransformFunction];
  ];

  If[NumericQ[coordinateRotation],
    If[CoordinateMatrix3DQ[vertexCoordinates],
      $vertexCoordinates //= SphericalRotateVector[coordinateRotation];
      $edgeCoordinateLists //= SphericalRotateVector[coordinateRotation];
    ,
      $vertexCoordinates //= RotateVector[coordinateRotation];
      $edgeCoordinateLists //= RotateVector[coordinateRotation];
    ];
  ];

  If[CoordinateMatrix3DQ[vertexCoordinates] && layoutDimension == 2,
    SetAuto[viewOptions, $automaticViewOptions];
    viewOptions = Assoc[PlotRange -> CoordinateBounds[vertexCoordinates], viewOptions];
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

VertexEdgeCoordinateData[data_Assoc, vertexLayout_] := Scope[

  UnpackAssociation[data,
    indexVertices, indexEdges, vertexCoordinates,
    selfLoopRadius, multiEdgeDistance, vertexOverlapResolution,
    layoutDimension
  ];

  $vertexCoordinates = Repeat[None, vertexCount];
  $edgeCoordinateLists = Repeat[{}, Len @ indexEdges];

  graphLayout = {
    "VertexLayout" -> vertexLayout,
    "SelfLoopRadius" -> selfLoopRadius,
    "MultiEdgeDistance" -> multiEdgeDistance,
    If[IntQ[layoutDimension], "Dimension" -> layoutDimension, Nothing]
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

  If[vertexCoordinates =!= Auto && MatchQ[Dims @ $edgeCoordinateLists, {_, 2, 2}],
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
  Part[$edgeCoordinateLists, L @ edge] = coords;

orientEdgeCoords[coords_, _DirectedEdge] := coords;
orientEdgeCoords[coords_, UndirectedEdge[a_, b_, tag_]] := If[
  Dist[
    F @ coords,
    Part[$vertexCoordinates, Part[edgeList, tag, 1]]
  ] < 0.001,
  coords, Rev @ coords
];

correctSelfLoops[selfLoopRadius_] := Scope[
  selfLoopIndices = SelectIndices[$edgeCoordinateLists, selfLoopQ];
  If[selfLoopIndices === {}, Return[]];
  SetAuto[selfLoopRadius, EdgeLengthScale[$edgeCoordinateLists, .5] / 4.0];
  $edgeCoordinateLists ^= MapIndices[fixSelfLoop[selfLoopRadius], selfLoopIndices, $edgeCoordinateLists];
];

selfLoopQ[coords_] := F[coords] == L[coords];

fixSelfLoop[selfLoopRadius_][coords_] := Scope[
  If[Len[coords] === 2, Return @ coords];
  terminus = F @ coords;
  radialVector = selfLoopRadius * Normalize[Mean[coords] - terminus];
  centeredCoords = PlusVector[coords, -terminus];
  scale = Norm @ Part[centeredCoords, Ceiling[Len[centeredCoords] / 2]];
  centeredCoords *= selfLoopRadius / (scale / 2);
  ToPackedReal @ PlusVector[centeredCoords, terminus]
]

(**************************************************************************************************)

PrivateFunction[fixEdgeVertexIntersections]

fixEdgeVertexIntersections[_, {}] := {};

fixEdgeVertexIntersections[vertexCoordinates_, edgeCoordinateLists_] := Scope[
  edgeLengths = Map[If[F[#] === L[#], 0, LineLength[#]]&, edgeCoordinateLists];
  scale = Quantile[Decases[edgeLengths, 0], .15];
  longIndices = SelectIndices[edgeLengths, GreaterEqualThan[scale * 2]];
  If[longIndices === {}, Return @ edgeCoordinateLists];
  vertexNearest = Nearest[vertexCoordinates];
  MapIndices[displaceVertexIntersectingEdges, longIndices, edgeCoordinateLists]
];

displaceVertexIntersectingEdges[{a_, b_}] := Scope[
  If[Min[Dist[F @ a, F @ b], Dist[L @ a, L @ b]] > scale/5, Return @ {a, b}];
  points = Take[Lerp[a, b, Into[30]], {2, 19}];
  nearest = Union @@ vertexNearest[points, {All, scale / 15}];
  nearest = Decases[nearest, a | b];
  If[nearest === {}, Return @ {a, b}];
  dy = Normalize[b - a] / 4;
  dx = VectorRotate90[dy];
  {a, Splice @ halfBend[a - dy/2, dx, -dy], Splice @ Rev @ halfBend[b + dy/2, dx, dy], b}
];

displaceVertexIntersectingEdges[other_] := other;

$bendMatrix = {{0.,0.},{0.04,0.19},{0.15,0.35},{0.31,0.46},{0.5,0.5},{0.69,0.46},{0.85,0.35},{0.96,0.19},{1.,0.}};
halfBend[p_, x_, y_] := Threaded[p] + Dot[$bendMatrix, ToPacked @ {x, y}];

(**************************************************************************************************)

nudgeOverlappingVertices[coords_, nudgeDistance_, plotRange_] := Scope[
  nudgedCoords = coords; num = Len[coords];
  nudgeScale = nudgeDistance;
  If[nudgeScale === Auto,
    nudgeScale = ChessboardDistance @@ If[
      CoordinateMatrixQ @ plotRange,
      Transpose @ plotRange,
      CoordinateBoundingBox @ coords
    ];
    nudgeScale = If[nudgeScale === 0, 1, Max[nudgeScale, 0.1]];
  ];
  dupPos = Select[Len[#] > 1&] @ PositionIndex[Round[coords, nudgeScale / 40]];
  is3D = CoordinateMatrix3DQ[coords];
  If[Len[dupPos] === 0, Return @ coords];
  If[Len[dupPos] === 1 && num == 1,
    points = If[CoordinateMatrix3DQ[coords],
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
    nudgeScale/5 * If[is3D, SpherePoints, CirclePoints][Len @ indices]
  ];

(**************************************************************************************************)

ExtendedGraphPlot::badwrappedshape = "CoordinateTransformFunction -> ProjectionOnto[...] contains an invalid shape.";
ExtendedGraphPlot::badcoordtrans = "CoordinateTransformFunction -> `` issued messages on application.";
ExtendedGraphPlot::badcoordtransname = "CoordinateTransformFunction -> `` is not one of ``."

applyCoordinateTransform[Auto|None] :=
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
  step = (Dist @@@ bounds) / m;
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
  applyRigidCoordinateTransform @ Fn[p, F @ nearest[p, 1]];
];

nudgeDuplicate[z_][p_] := p + Normalize[Cross[z - p]] * Im[$nudge2] + Normalize[z - p] * Re[$nudge2];

applyCoordinateTransform["RaiseHorizontalTreeNodes"] := raiseTreeNodes[True];
applyCoordinateTransform["RaiseVerticalTreeNodes"] := raiseTreeNodes[False];

(* tree does not put first layer of vertices in first row for some reason, this horrible piece of code corrects that! *)
raiseTreeNodes[horizontal_] := Scope[
  {fn1, fn2} = If[horizontal, {L, F}, {F, L}];

  rootIndex = If[horizontal, MaximumIndexBy[$vertexCoordinates, L], MinimumIndexBy[$vertexCoordinates, F]];
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
      newCoord = F[pointEdgeIntersect[layer1Pos, #]& /@ SortBy[fn1] /@ Partition[edgeSegment, 2, 1]];
      Part[$edgeCoordinateLists, edgeIndex] = {rootCoords, newCoord};
      Part[$vertexCoordinates, vertIndex] = newCoord;
      outgoingIndices = Flatten @ Position[edgePairs, {vertIndex, _}, {1}];
      Part[$edgeCoordinateLists, outgoingIndices] //= Map[{newCoord, L @ #}&];
    ],
    {gen1Indices, gen1Coords}
  ];
]

applyCoordinateTransform["CenterHorizontal"|"CenterTree"] := centerTree[True];
applyCoordinateTransform["CenterVertical"] := centerTree[False];

centerTree[horizontal_] := Scope[
  {fn1, fn2} = If[horizontal, {L, F}, {F, L}];
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
  offsets = KVMap[{#1, Mean[MinMax[#2]] - min}&, groups];
  offsetFn = Interpolation[offsets, InterpolationOrder -> 1];
  applyCoordinateTransform[{If[horizontal, "HorizontalWarp", "VerticalWarp"], offsetFn}];
]

toEdgeListSegments[e_] := If[Len[e] == 2, e, Splice @ Partition[e, 2, 1]];

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

$namedTransforms := $namedTransforms = <|
  "Rotate0" -> Id,
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

squareSelfLoop[list:{a_, Repeated[_, {3, Inf}], b_}] /; Dist[a, b] < 0.01 := Scope[
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

ptAlong[a_, b_, d_] := Which[d <= 0, a, d >= Dist[a, b], b, True, PointAlongLine[{a, b}, d]];

squareSelfLoop[line_] := line;

applyCoordinateTransform[name_Str] := Scope[
  trans = LookupOrMessageKeys[$namedTransforms, name, $Failed];
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
  {a_, b_} ? CoordinateMatrixQ /; (H[$rnf] === RegionNearestFunction) :=
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