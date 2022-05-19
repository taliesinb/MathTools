(**************************************************************************************************)

PackageExport["PlainGraph"]

Options[PlainGraph] = $simpleGraphOptionRules;

PlainGraph[graph_Graph, opts:OptionsPattern[]] := Scope[
  If[OptionValue[VertexCoordinates] === Inherited,
    opts = Sequence @ ReplaceOptions[{opts}, VertexCoordinates -> Values[LookupVertexCoordinates @ graph]]
  ];
  ExtendedGraph[VertexList @ graph, EdgeList @ graph, opts]
];

(**************************************************************************************************)

PackageExport["IndexedExtendedGraph"]

SetUsage @ "
IndexedExtendedGraph[args$$] is like ExtendedGraph but accepts edges in the form of indexices into \
the vertex list.
"

Options[IndexedExtendedGraph] = $simpleGraphOptionRules;
IndexedExtendedGraph[vertices_, edges_, opts___] := Scope[
  range = Range @ Length @ vertices;
  ExtendedGraph[
    VertexReplace[
      Graph[range, edges],
      RuleThread[range, vertices]
    ],
    opts
  ]
];
  
(**************************************************************************************************)

PackageExport["ToIndexGraph"]

ToIndexGraph[graph_ ? IndexGraphQ] := graph;
ToIndexGraph[graph_] := IndexGraph @ graph;

(**************************************************************************************************)

PackageExport["EdgeListQ"]

SetUsage @ "
EdgeListQ[e$] returns True if e$ is a list of edges (%UndirectedEdge or %DirectedEdge).
"

EdgeListQ = Case[
  {RepeatedNull[_DirectedEdge | UndirectedEdge]} := True;
  _ := False
];

(**************************************************************************************************)

PackageExport["VertexRange"]

SetUsage @ "
VertexRange[graph$] returns {1, 2, $$, n$} where n$ is the number of vertices in graph.
"

VertexRange[graph_] := Range @ VertexCount @ graph;

(**************************************************************************************************)

PackageExport["EdgePairsToAdjacencyMatrix"]

EdgePairsToAdjacencyMatrix[pairs_, n_] :=
  ExtendedSparseArray[pairs, {n, n}]

(**************************************************************************************************)

PackageExport["ExtendedSparseArray"]

ExtendedSparseArray[{} | <||>, sz_] := SparseArray[{}, sz];

ExtendedSparseArray[assoc_Association, sz_] := SparseArray[Normal @ assoc, sz];

ExtendedSparseArray[list:{___List} ? DuplicateFreeQ, sz_] := SparseArray[Thread[list -> 1], sz];

ExtendedSparseArray[list:{___List}, sz_] := SparseArray[Normal @ Counts @ list, sz];

ExtendedSparseArray[list:{___Rule}, sz_] := SparseArray[Normal @ Merge[list, Total], sz];

(**************************************************************************************************)

PackageExport["GraphCorners"]

GraphCorners[graph_] := Scope[
  degree = DegreeCentrality[graph];
  vertices = Pick[VertexList[graph], degree, Min[degree]];
  SortBy[vertices, LatticeVertexAngle]
];

(**************************************************************************************************)

PackageExport["GraphVertexCoordinates"]

GraphVertexCoordinates[graph_Graph] :=
  GraphEmbedding[graph];

(**************************************************************************************************)

PackageScope["integersToVertices"]

integersToVertices[graph_Graph, expr_] :=
  integersToVertices[VertexList[graph], expr];

integersToVertices[vertices_List, expr_] :=
  expr /. {i:{__Integer} :> Part[vertices, i], i_Integer :> Part[vertices, i]};

(**************************************************************************************************)

PackageExport["ToGraph"]

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

PackageExport["IndexGraphQ"]

IndexGraphQ[g_Graph ? GraphQ] :=
  RangeQ @ VertexList @ g;

IndexGraphQ[_] := False;

(**************************************************************************************************)

PackageExport["ComponentGraphs"]

ComponentGraphs[graph_] := ComponentGraphs[graph, All];

ComponentGraphs[graph_, u_UpTo] := ComponentGraphs[graph, 1 ;; u];

ComponentGraphs::compob = "Component `` was requested but only `` are available.";
ComponentGraphs[graph_, spec:(_Integer|All|_Span)] := Scope[
  components = WeaklyConnectedGraphComponents @ graph;
  components = Reverse @ SortBy[components, ApplyThrough[{VertexCount, VertexList}]];
  If[IntegerQ[spec] && Abs[spec] > Length[components],
    ReturnFailed["compob", spec, Length[components]]];
  Part[components, spec]
];

ComponentGraphs::badvertex = "Graph does not contain a vertex matching ``."
ComponentGraphs[graph_, VertexPattern[p_]] :=
  First[WeaklyConnectedGraphComponents[graph, p], Message["badvertex", p]; $Failed];

ComponentGraphs::badcomp = "`` is not a valid component specification."
ComponentGraphs[_, spec_] := (Message[ComponentGraphs::badcomp, spec]; $Failed);

(**************************************************************************************************)

PackageExport["CanonicalizeEdges"]

CanonicalizeEdges[edges_] := Map[sortUE, edges];
sortUE[UndirectedEdge[a_, b_, tag___]] /; Order[a, b] === 1 := UndirectedEdge[b, a, tag];
sortUE[other_] := other;

(**************************************************************************************************)

PackageExport["ToSymmetricGraph"]

ToSymmetricGraph[graph_ ? DirectedGraphQ] :=
  Graph[VertexList @ graph, EdgeList[graph] /. DirectedEdge -> UndirectedEdge];

ToSymmetricGraph[graph_] := graph;

(**************************************************************************************************)

PackageExport["FindAllUndirectedSpanningEdgeSets"]

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

PackageExport["ExtractExtendedGraphOptions"]

ExtractExtendedGraphOptions[graph_Graph] := Scope[
  opts = Options @ graph;
  annoRules = Lookup[opts, AnnotationRules, {}];
  graphProps = Lookup[annoRules, "GraphProperties", {}];
  Join[DeleteOptions[opts, AnnotationRules], graphProps]
]

(**************************************************************************************************)

PackageExport["RotateGraph"]

RotateGraph[graph_, opts___Rule] :=
  RotateGraph[graph, 90, opts];

RotateGraph[graph_, degrees_Integer, opts___Rule] := Scope[
  coords = Lookup[LookupVertexCoordinates @ graph, VertexList @ graph];
  coords = RotationTransform[degrees Degree] @ coords;
  imageSize = Reverse @ LookupImageSize @ graph;
  ExtendedGraph[
    graph,
    opts,
    VertexCoordinates -> coords, ImageSize -> imageSize
  ]
]