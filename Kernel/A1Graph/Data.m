PublicOption[Signed]

SetUsage @ "
Signed is an option to various graph utility functions.
"

(**************************************************************************************************)

PublicFunction[AdjacentVerticesPredicate]

AdjacentVerticesPredicate[graph_] := Scope[
  edges = {#1, #2}& @@@ ToEdges[graph];
  ConstantUAssociation[Join[edges, Rev[edges, 2]], True] /* TrueQ
];

(**************************************************************************************************)

PublicFunction[AdjacentPairs]

SetUsage @ "
AdjacentPairs[graph$] gives the list of {{u$1, v$1}, {u$2, v$2}, $$} such that \
vertex with index u$i is adjacent to vertex with index v$i.
* Note that AdjacentPairs is not given in the same order as %EdgeList[graph$], and \
in general might have fewer values when there are multiple edges between the same \
pair of vertices. Use EdgePairs if this matters.
* The relation is undirected, so that a$ => b$ generates both {a$, b$} and {b$, a$}.
* Use AdjacentPairs[graph, 'Directed'] to obtain the directed form.
"

AdjacentPairs[graph_] := AdjacencyMatrix[graph]["NonzeroPositions"];

AdjacentPairs[graph_ ? DirectedGraphQ] := Scope[
  adj = AdjacencyMatrix[graph];
  (adj + Transpose[adj])["NonzeroPositions"]
];

AdjacentPairs[graph_, "Undirected"] := AdjacentPairs[graph];
AdjacentPairs[graph_, "Directed"] := AdjacencyMatrix[graph]["NonzeroPositions"];

(**************************************************************************************************)

PublicFunction[EdgePairs]

SetUsage @ "
EdgePairs[graph$] gives the list of {{u$1, v$1}, {u$2, v$2}, $$}} such that \
these is an vertex with index u$i is connected to vertex with index v$i.
* EdgePairs[graph$] has the same length and order as EdgeList[graph$].
* If the correspondence with EdgeList does not matter, consider using AdjacentPairs,
which is faster.
"

(* todo: find a better way of obtaining these than via indexgraph! it seems like
vertex renaming might be expensive, and there is all the option processing that goes along with it.
unfortunately i can't find a way of extracting the list of edges in indexed form directly. *)
EdgePairs[graph_ ? EdgeTaggedGraphQ] := {#1, #2}& @@@ EdgeList @ ToIndexGraph @ graph;
EdgePairs[graph_] := List @@@ EdgeList @ ToIndexGraph @ graph;
EdgePairs[graph_, "Undirected"] := Join[#, ReverseEdges[#]]& @ EdgePairs[graph];
EdgePairs[graph_, "Directed"] := EdgePairs[graph];

(**************************************************************************************************)

PublicFunction[VertexOutTable, VertexInTable]

SetUsage @ "
VertexOutTable[graph$] returns a list of lists {out$1, out$2, $$} where out$i is a list of the \
indices of the vertices that are have a connection from vertex v$i.
"

SetUsage @ "
VertexInTable[graph$] returns a list of lists {in$1, in$2, $$} where in$i consists of the \
indices of the vertices that are have a connection to vertex v$i.
"

VertexOutTable[graph_] := AdjacencyMatrix[graph]["AdjacencyLists"];
VertexInTable[graph_] := Transpose[AdjacencyMatrix[graph]]["AdjacencyLists"];

(**************************************************************************************************)

PublicFunction[VertexInOutTable]

SetUsage @ "
VertexInOutTable[graph$] returns a list of pairs of lists {{in$1, out$1}, {in$2, out$2}, $$} where in$i \
is the list of indices of vertices that are have an edge to vertex v$i, and out$i is the \
list of indices of vertices that have a edge from vertex v$i.
"

VertexInOutTable[graph_] := Scope[
  adj = AdjacencyMatrix[graph];
  Trans[adj["AdjacencyLists"], Transpose[adj]["AdjacencyLists"]]
];

(**************************************************************************************************)

PublicFunction[VertexAdjacencyTable]

SetUsage @ "
VertexAdjacencyTable[graph$] returns a list of lists {adj$1, adj$2, $$} where adj$i \
is the list of indices of vertices that are have a connection to vertex v$i.
"

VertexAdjacencyTable[graph_] := Scope[
  adj = AdjacencyMatrix[graph];
  MapThread[Union, {adj["AdjacencyLists"], Transpose[adj]["AdjacencyLists"]}]
];

(**************************************************************************************************)

PublicFunction[VertexAdjacencyAssociation]

SetUsage @ "
VertexAdjacencyAssociation[graph$] returns an association from vertices to lists of their neighbors.
"

VertexAdjacencyAssociation[graph_] :=
  GroupPairs @ ExtractIndices[VertexList @ graph, AdjacentPairs @ graph];

(**************************************************************************************************)

PublicFunction[VertexOutEdgeTable, VertexInEdgeTable]

SetUsage @ "
VertexOutEdgeTable[graph$] returns a list of lists {out$1, out$2, $$} where out$i is a list of the \
indices of edges whose origin is the vertex v$i.
"

SetUsage @ "
VertexInEdgeTable[graph$] returns a list of lists {in$1, in$2, $$} where in$i is a list of the \
indices of edges whose destination is the vertex v$i.
"

VertexOutEdgeTable[graph_] :=
  Lookup[PositionIndex @ Col1 @ EdgePairs @ graph, VertexRange @ graph, {}];

VertexInEdgeTable[graph_] :=
  Lookup[PositionIndex @ Col2 @ EdgePairs @ graph, VertexRange @ graph, {}];

(**************************************************************************************************)

PublicFunction[VertexInOutEdgeTable]

SetUsage @ "
VertexInOutEdgeTable[graph$] returns a list of lists {{in$1, out$1}, {in$2, out$2}, $$}  where in$i \
is a list of the indices of edges whose destination is the vertex v$i, and out$i is a list of the \
indices of edges whose origin is the vertex v$i.
"

VertexInOutEdgeTable[graph_] := Scope[
  pairs = EdgePairs @ graph;
  vertices = VertexRange @ graph;
  Trans[
    Lookup[PositionIndex @ Col2 @ pairs, vertices, {}],
    Lookup[PositionIndex @ Col1 @ pairs, vertices, {}]
  ]
];

(**************************************************************************************************)

PublicFunction[VertexAdjacentEdgeTable]

SetUsage @ "
VertexAdjacentEdgeTable[graph$] returns a list of lists {adj$1, adj$2, $$}  where adj$i \
is a list of the indices of edges which begin or end at vertex v$i.
* If the option %Signed -> True is provided, edges will be wrapped in Inverted if they are traversed in the \
reversed direction.
"

Options[VertexAdjacentEdgeTable] = {Signed -> False};

VertexAdjacentEdgeTable[graph_, OptionsPattern[]] := Scope[
  pairs = EdgePairs @ graph;
  vertices = VertexRange @ graph;
  negator = If[OptionValue[Signed], MatrixMap[Inverted, #]&, Id];
  MapThread[Union, {
    Lookup[PositionIndex @ Col1 @ pairs, vertices, {}],
    Lookup[negator @ PositionIndex @ Col2 @ pairs, vertices, {}]
  }]
];

(**************************************************************************************************)

PublicFunction[VertexAdjacentVertexEdgeTable, VertexAdjacentVertexEdgeAssociation]

SetUsage @ "
VertexAdjacentVertexEdgeTable[graph$] returns a list of lists {adj$1, adj$2, $$}  where adj$i \
is a list of the pairs {e$, v$}, where e$ is an edge index and v$ is a vertex index connected to \
vertex $i by edge $e.
"

VertexAdjacentVertexEdgeTable[graph_] := vertexAdjacentEdgeVertex[graph, False];

SetUsage @ "
VertexAdjacentVertexEdgeTable[graph$] returns an association from each vertex u$ to the  \
list of the pairs {e$, v$}, where e$ is an edge index and v$ is a vertex index connected to \
vertex $u by edge $e.
"

VertexAdjacentVertexEdgeAssociation[graph_] := vertexAdjacentEdgeVertex[graph, True];

vertexAdjacentEdgeVertex[graph_, returnAssoc_] := Scope[
  pairs = EdgePairs @ graph;
  vertices = VertexRange @ graph;
  edgeIndices = PositionIndex @ Col1 @ pairs;
  outVertices = Col2 @ pairs;
  outEdges = Lookup[edgeIndices, vertices, {}];
  res = Map[edgeIndices |-> Trans[edgeIndices, Part[outVertices, edgeIndices]], outEdges];
  If[returnAssoc, AssocThread[vertices, res], res]
];

(**************************************************************************************************)

PublicFunction[VertexIndexAssociation]

VertexIndexAssociation[graph_] := AssociationRange @ VertexList @ graph;

(**************************************************************************************************)

PublicFunction[EdgeIndexAssociation]

EdgeIndexAssociation[graph_] := AssociationRange @ ToUntaggedEdges @ graph;

EdgeIndexAssociation[graph_, Signed -> False] := EdgeIndexAssociation @ graph;

EdgeIndexAssociation[graph_, Signed -> True] := UAssoc @ MapIndex1[toUEdgeIndex, ToUntaggedEdges @ graph];

toUEdgeIndex[head_[a_, b_, ___], index_] := {head[a, b] -> i, head[b, a] -> Inverted[i]};

(**************************************************************************************************)

PublicFunction[ToEdges, ToUntaggedEdges]

ToEdges = Case[
  e_Graph := EdgeList[e];
  e_List  := e;
];

ToUntaggedEdges = Case[
  e_Graph := % @ EdgeList[e];
  e_List := Take[e, All, 2];
];

(**************************************************************************************************)

PublicFunction[EdgePairIndexAssociation]

EdgePairIndexAssociation[graph_] := AssociationRange @ EdgePairs @ graph;

(**************************************************************************************************)

PublicFunction[VertexOrientedOutTable]

SetUsage @ "
VertexOrientedOutTable[graph$] returns a list of pairs of lists {{dout$1, uout$1}, {dout$2, uout$2}, $$} \
where dout$i is the list of indices of vertices that are have a directed edge from vertex i$, and uout$i is \
the list of indices of vertices that have a undirected edge from vertex i$.
"

toOutTable[count_, edges_] := Lookup[GroupPairs @ edges, Range[count], {}];

VertexOrientedOutTable[graph_] := Scope[
  edges = EdgeList @ IndexGraph @ graph; count = VertexCount[graph];
  dir = Cases[edges, _DirectedEdge];
  undir = Cases[edges, _UndirectedEdge];
  Trans[
    toOutTable[count, dir],
    toOutTable[count, Join[undir, Rev[undir, 2]], 1]
  ]
];

(**************************************************************************************************)

PublicFunction[VertexOutAssociation, VertexInAssociation]

SetUsage @ "
VertexOutAssociation[graph$] returns an association of lists <|v$1 -> out$1, v$2 -> out$2, $$|> \
where out$i is a list of the vertices that have a connection from v$i.
"

SetUsage @ "
VertexInAssociation[graph$] returns an association of lists <|v$1 -> in$1, v$2 -> in$2, $$|> \
where in$i is a list of the vertices that have a connection to v$i.
"

tableToAssoc[vertices_, table_] := Assoc @ MapIndexed[
  Part[vertices, F @ #2] -> Part[vertices, #1]&,
  table
];

VertexOutAssociation[graph_] :=
  tableToAssoc[VertexList @ graph, VertexOutTable @ graph];

VertexInAssociation[graph_] :=
  tableToAssoc[VertexList @ graph, VertexInTable @ graph];

(**************************************************************************************************)

PublicFunction[VertexInOutAssociation]

SetUsage @ "
VertexInOutAssociation[graph$] returns an association of lists <|v$1 -> {in$1, out$1}, v$2 -> {in$2, out$2}, $$|> \
where in$i is the list of indices of vertices that are have a connection to vertex i$, and out$i is the \
list of indices of vertices that have a connection from vertex i$.
"

VertexInOutAssociation[graph_] := Scope[
  vertices = VertexList[graph];
  Assoc @ MapIndexed[
    Part[vertices, F @ #2] -> {Part[vertices, F[#1]], Part[vertices, L[#1]]}&,
    VertexInOutTable[graph]
  ]
];

(**************************************************************************************************)

PublicFunction[InVertices, OutVertices, AllVertices, AllUniqueVertices]

InVertices[edges_] := Col1 @ edges;
OutVertices[edges_] := Col2 @ edges;
AllVertices[edges_] := Join[InVertices @ edges, OutVertices @ edges];
AllUniqueVertices[edges_] := Dedup @ AllVertices[edges];

(**************************************************************************************************)

PublicFunction[VertexEdgeList]

SetUsage @ "
VertexEdgeList[graph$] returns {vertices$, edges$}.
"

VertexEdgeList[graph_] := {VertexList @ graph, EdgeList @ graph};