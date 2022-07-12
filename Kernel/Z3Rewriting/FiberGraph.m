PublicFunction[FiberGraphRewritingSystem]

FiberGraphRewritingSystem::arg1 = "Base graph should be a directed graph."
FiberGraphRewritingSystem::arg2 = "Fiber graph should be a directed graph."
FiberGraphRewritingSystem::args = "Two arguments expected: a base graph and fiber graph."

FiberGraphRewritingSystem[baseGraph_, fiberGraph_] := Scope[
  If[!DirectedGraphQ[baseGraph], ReturnFailed["arg1"]];
  If[!DirectedGraphQ[fiberGraph], ReturnFailed["arg1"]];
  props = <|"BaseGraph" -> baseGraph, "FiberGraph" -> fiberGraph|>;
  constructRewritingSystem["FiberGraph", None, "CustomProperties" -> props]
]

_FiberGraphRewritingSystem := (Message[FiberGraphRewritingSystem::args]; $Failed);

declareRewritingSystemDispatch["FiberGraph", FiberGraphRewritingSystemProperty]

FiberGraphRewritingSystemProperty[data_, "CayleyFunction", opts___Rule] := Scope[
  UnpackAssociation[data, customProperties];
  UnpackAssociation[customProperties, baseGraph, fiberGraph];
  baseAdj = VertexAdjacentVertexEdgeAssociation /@ {baseGraph, fiberGraph};
  
];

PublicFunction[FiberSection]

PublicFunction[FiberCayleyFunction]

FiberCayleyFunction[baseAdj_, fiberAdj_][FiberVertex[b_, f_]] := Scope[
  foo

];



$FiberIconGraphThemeRules = {
  AspectRatioClipping -> False,
  ArrowheadSize -> 0,
  VertexSize -> 5,
  ImageSize -> {100, 100}
};

$GraphThemeData["FiberIconGraph"] := $FiberIconGraphThemeRules;

PublicFunction[FiberGraph]
PublicOption[FiberScale, FiberCoordinateRotation]

Options[FiberGraph] = JoinOptions[
  FiberScale -> 1,
  FiberCoordinateRotation -> Pi/2,
  LayoutDimension -> Automatic,
  $ExtendedGraphOptions
];

FiberGraph[baseGraph_, fiberGraph_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[fiberScale, fiberCoordinateRotation, layoutDimension];
  graphs = {baseGraph, fiberGraph};
  {baseCoords, fiberCoords} = LookupVertexCoordinates /@ graphs;
  If[fiberCoordinateRotation =!= 0,
    fiberCoords //= RotateVector[fiberCoordinateRotation]];
  {baseVertices, fiberVertices} = VertexList /@ graphs;
  {baseEdges, fiberEdges} = EdgeList /@ graphs;
  {baseAdj, fiberAdj} = VertexAdjacentVertexEdgeAssociation /@ graphs;
  vertexTuples = ApplyTuples[FiberVertex, {baseVertices, fiberVertices}];
  vertices = FiberVertex @@@ vertexTuples;
  edges = Flatten @ {
    MapIndexed[fiberEdge1, Outer[List, baseEdges, fiberVertices], {2}],
    MapIndexed[fiberEdge2, Outer[List, baseEdges, fiberEdges], {2}]
  };
  If[layoutDimension === 3,
    If[Length[First[baseCoords]] == 2, baseCoords //= Map[Append[0]]];
    If[Length[First[fiberCoords]] == 2, fiberCoords //= Map[Prepend[0]]];
  ];
  ExtendedGraph[vertices, edges,
    FilterOptions[opts],
    ArrowheadPosition -> 0.75, ArrowheadSize -> 0,
    VertexCoordinateFunction -> FiberVertexCoordinateFunction[baseCoords, fiberCoords, fiberScale]
  ]
];

fiberEdge1[{DirectedEdge[bv1_, bv2_], fv_}, {i_, _}] :=
  DirectedEdge[FiberVertex[bv1, fv], FiberVertex[bv2, fv], FiberCardinal[i, None]];

fiberEdge1[{DirectedEdge[bv1_, bv2_, bt_], fv_}, _] :=
  DirectedEdge[FiberVertex[bv1, fv], FiberVertex[bv2, fv], FiberCardinal[bt, None]];

fiberEdge2[{DirectedEdge[bv1_, bv2_], DirectedEdge[fv1_, fv2_]}, {i_, j_}] := {
  DirectedEdge[FiberVertex[bv1, fv1], FiberVertex[bv2, fv2], FiberCardinal[i, j]],
  DirectedEdge[FiberVertex[bv1, fv2], FiberVertex[bv2, fv1], FiberCardinal[i, Inverted @ j]]
};

fiberEdge2[{DirectedEdge[bv1_, bv2_, bt_], DirectedEdge[fv1_, fv2_, ft_]}, _] := {
  DirectedEdge[FiberVertex[bv1, fv1], FiberVertex[bv2, fv2], FiberCardinal[bt, ft]],
  DirectedEdge[FiberVertex[bv1, fv2], FiberVertex[bv2, fv1], FiberCardinal[bt, Inverted @ ft]]
};

PublicFunction[FiberVertexCoordinateFunction]

FiberVertexCoordinateFunction[bc_, fc_, scale_][FiberVertex[b_, f_]] :=
  Lookup[bc, b] + (scale * Lookup[fc, f])
  

