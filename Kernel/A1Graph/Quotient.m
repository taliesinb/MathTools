PublicFunction[GraphVertexQuotient]

GraphVertexQuotient[graph_, equivFn_, userOpts___Rule] := Scope[
  setupGraphVertexData[graph];
  {vertexList, edgeList} = VertexEdgeList @ graph;
  opts = ExtractExtendedGraphOptions[graph];
  quotientVertexIndices = EquivalenceClassIndices[vertexList, equivFn];
  quotientVertexLabels = EquivalenceClassLabels[quotientVertexIndices];
  quotientVertexCounts = Length /@ quotientVertexIndices;
  edgePairs = EdgePairs @ graph;
  quotientEdgePairs = edgePairs /. i_Integer :> Part[quotientVertexLabels, i];
  quotientEdgesIndex = PositionIndex[quotientEdgePairs];
  {quotientEdges, quotientEdgesIndices} = KeysValues @ quotientEdgesIndex;
  quotientEdgesCounts = Length /@ quotientEdgesIndices;
  quotientVertices = Range @ Length @ quotientVertexIndices;
  vertexAnnos = <|
    "EquivalenceClassIndices" -> quotientVertexIndices,
    "EquivalenceClassSizes" -> quotientVertexCounts
  |>;
  edgeAnnos = <|
    "EquivalenceClassIndices" -> quotientEdgesIndices,
    "EquivalenceClassSizes" -> quotientEdgesCounts
  |>;
  opts //= ReplaceOptions[{VertexAnnotations -> vertexAnnos, EdgeAnnotations -> edgeAnnos}];
  ExtendedGraph[
    quotientVertices,
    DirectedEdge @@@ quotientEdges,
    Sequence @@ userOpts,
    Sequence @@ opts
  ]
];