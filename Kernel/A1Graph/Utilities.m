PublicFunction[PlainGraph]

Options[PlainGraph] = $ExtendedGraphOptions;

PlainGraph[graph_Graph, opts:OptionsPattern[]] := Scope[
  If[OptionValue[VertexCoordinates] === Inherited,
    opts //= SeqReplaceOptions[VertexCoordinates -> Values[LookupVertexCoordinates @ graph]];
  ];
  ExtendedGraph[VertexList @ graph, EdgeList @ graph, opts]
];

(**************************************************************************************************)

PublicFunction[IndexedExtendedGraph]

SetUsage @ "
IndexedExtendedGraph[args$$] is like ExtendedGraph but accepts edges in the form of indexices into \
the vertex list.
"

Options[IndexedExtendedGraph] = $ExtendedGraphOptions;
IndexedExtendedGraph[vertices_, edges_, opts___] := Scope[
  range = Range @ Len @ vertices;
  ExtendedGraph[
    VertexReplace[
      Graph[range, edges],
      RuleThread[range, vertices]
    ],
    opts
  ]
];
  
(**************************************************************************************************)

PublicFunction[ToIndexGraph]

ToIndexGraph[graph_ ? IndexGraphQ] := graph;
ToIndexGraph[graph_] := IndexGraph @ graph;

(**************************************************************************************************)

PublicFunction[EdgeListQ]

SetUsage @ "
EdgeListQ[e$] returns True if e$ is a list of edges (%UndirectedEdge or %DirectedEdge).
"

EdgeListQ = Case[
  {RepeatedNull[_DirectedEdge | _UndirectedEdge]} := True;
  _ := False
];

(**************************************************************************************************)

PublicFunction[VertexRange]

SetUsage @ "
VertexRange[graph$] returns {1, 2, $$, n$} where n$ is the number of vertices in graph.
"

VertexRange[graph_] := Range @ VertexCount @ graph;

(**************************************************************************************************)

PublicFunction[EdgePairsToAdjacencyMatrix]

EdgePairsToAdjacencyMatrix[pairs_, n_] :=
  ExtendedSparseArray[pairs, {n, n}]

(**************************************************************************************************)

PublicFunction[GraphCorners]

GraphCorners[graph_] := Scope[
  degree = DegreeCentrality[graph];
  vertices = Pick[VertexList[graph], degree, Min[degree]];
  SortBy[vertices, LatticeVertexAngle]
];

(**************************************************************************************************)

PublicFunction[GraphVertexCoordinates]

GraphVertexCoordinates[graph_Graph] :=
  GraphEmbedding[graph];

(**************************************************************************************************)

PrivateFunction[integersToVertices]

integersToVertices[graph_Graph, expr_] :=
  integersToVertices[VertexList[graph], expr];

integersToVertices[vertices_List, expr_] :=
  expr /. {i:{__Int} :> Part[vertices, i], i_Int :> Part[vertices, i]};

(**************************************************************************************************)

PublicFunction[ToGraph]

SetUsage @ "
ToGraph[obj$] attempts to convert obj$ to a Graph[$$] object.
* If obj$ is already a Graph, it is returned unchanged.
* If obj$ is a list of rules, it is converted to a Graph object.
* Otherwise, $Failed is returned.
"

$edgeP = _DirectedEdge | _UndirectedEdge | _Rule | _TwoWayRule;

ToGraph = Case[
  g_Graph                 := g;
  list:{Repeated[$edgeP]} := Graph[list];
  _                       := $Failed
];

(**************************************************************************************************)

PublicFunction[IndexGraphQ]

IndexGraphQ[g_Graph ? GraphQ] :=
  RangeQ @ VertexList @ g;

IndexGraphQ[_] := False;

(**************************************************************************************************)

PublicFunction[DropComponents]

DropComponents[graph_Graph, spec_] := Scope[
  components = WeaklyConnectedComponents @ graph;
  components = Rev @ Sort @ components;
  vertices = Quiet @ Check[
    If[IntegerQ[spec], Part[components, spec], Union @@ Part[components, spec]],
    $Failed
  ];
  If[!VectorQ[vertices], ReturnFailed[]];
  ExtendedSubgraph[graph, Complement[VertexList @ graph, vertices], Automatic]
];

DropComponents[n_][graph_] := DropComponents[graph, n];

(**************************************************************************************************)

PublicFunction[ComponentGraphs]

ComponentGraphs[graph_] := ComponentGraphs[graph, All];

ComponentGraphs[graph_, u_UpTo] := ComponentGraphs[graph, 1 ;; u];

ComponentGraphs::compob = "Component `` was requested but only `` are available.";
ComponentGraphs[graph_, spec:(_Int|All|_Span)] := Scope[
  components = WeaklyConnectedGraphComponents @ graph;
  components = Rev @ SortBy[components, ApplyThrough[{VertexCount, VertexList}]];
  If[IntegerQ[spec] && Abs[spec] > Len[components],
    ReturnFailed["compob", spec, Len[components]]];
  Part[components, spec]
];

ComponentGraphs::badvertex = "Graph does not contain a vertex matching ``."
ComponentGraphs[graph_, VertexPattern[p_]] :=
  First[WeaklyConnectedGraphComponents[graph, p], Message["badvertex", p]; $Failed];

ComponentGraphs::badcomp = "`` is not a valid component specification."
ComponentGraphs[_, spec_] := (Message[ComponentGraphs::badcomp, spec]; $Failed);

(**************************************************************************************************)

PublicFunction[CanonicalizeEdges]

CanonicalizeEdges[edges_] := Map[sortUE, edges];
sortUE[UndirectedEdge[a_, b_, tag___]] /; Order[a, b] === 1 := UndirectedEdge[b, a, tag];
sortUE[other_] := other;

(**************************************************************************************************)

PublicFunction[ToSymmetricGraph]

ToSymmetricGraph[graph_ ? DirectedGraphQ] :=
  Graph[VertexList @ graph, EdgeList[graph] /. DirectedEdge -> UndirectedEdge];

ToSymmetricGraph[graph_] := graph;

(**************************************************************************************************)

PublicFunction[FindAllUndirectedSpanningEdgeSets]

(* this is O(n!) in the number of vertices, so not practical for all but small graphs,
but at least we don't have to rewrite FindSpanningTree *)

FindAllUndirectedSpanningEdgeSets[graph_] := Scope[
  {vertices, edges} = VertexEdgeList @ graph;
  vertices = VertexList @ graph;
  igraph = ToUndirectedEdgeIndexGraph @ graph;
  {ivertices, iedges} = VertexEdgeList @ igraph;
  perms = Permutations @ ivertices;
  spanningEdgeSets = DeleteDuplicates @ Map[
    Sort[findSpanningEdgeTags[#, iedges]]&,
    perms
  ]
];

(*
  Graph[vertices, Part[edges, #], opts]& /@ skeletonIndices
]
*)
ToUndirectedEdgeIndexGraph[graph_] := Scope[
  {vertices, edges} = VertexEdgeList @ IndexGraph[graph];
  i = 1; Graph[vertices, UndirectedEdge[#1, #2, i++]& @@@ edges]
];

findSpanningEdgeTags[vertices_, edgeList_] :=
  Part[EdgeList[FindSpanningTree[Graph[vertices, edgeList]]], All, 3];

(**************************************************************************************************)

FindAllDirectedTrees[graph_] := Scope[
  {vertices, edges} = VertexEdgeList @ graph;
  $Unreachable
];

(**************************************************************************************************)

PublicFunction[RotateGraph]

RotateGraph[graph_, opts___Rule] :=
  RotateGraph[graph, 90, opts];

RotateGraph[graph_, degrees_Int, opts___Rule] := Scope[
  coords = Lookup[LookupVertexCoordinates @ graph, VertexList @ graph];
  coords = RotationTransform[degrees Degree] @ coords;
  imageSize = Rev @ LookupImageSize @ graph;
  ExtendedGraph[
    graph,
    opts,
    VertexCoordinates -> coords, ImageSize -> imageSize
  ]
]

(**************************************************************************************************)

PublicFunction[RangePartitionGraph]

RangePartitionGraph[n_] := Scope[
  init = Array[List, n];
  MultiwaySystem[rangePartitionSuccessors, {init}]
];
  
rangePartitionSuccessors[part_] := Join @@ Table[
  Sort @ Append[
    Delete[part, {{i}, {j}}],
    Sort[Join @@ Part[part, {i, j}]]
  ],
  {i, Len @ part}, {j, i+1, Len @ part}
];
