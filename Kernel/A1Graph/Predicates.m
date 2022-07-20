PublicFunction[CycleGraphQ]

CycleGraphQ[g_] := And[
  GraphQ[g], VertexCount[g] == EdgeCount[g],
  IsomorphicGraphQ[g, CycleGraph[VertexCount @ g, DirectedEdges -> DirectedGraphQ[g]]]
];