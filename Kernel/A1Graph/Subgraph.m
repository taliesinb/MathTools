PublicFunction[ExtendedSubgraph]

ExtendedSubgraph[oldGraph_, newVertices_, newEdges_] := Scope[
  options = Options[oldGraph];
  annotations = ExtendedGraphAnnotations[oldGraph];
  vertexCoords = Lookup[options, VertexCoordinates, Auto];
  oldVertices = VertexList[oldGraph];
  If[newVertices === All,
    newVertexIndices = Range @ Len @ oldVertices;
    newVertices = oldVertices;
  ,
    oldVertexIndex = AssociationRange @ oldVertices;
    newVertexIndices = Lookup[oldVertexIndex, newVertices];
    newVertexOrdering = Ordering[newVertexIndices];
    newVertices = Part[newVertices, newVertexOrdering];
  ];
  {graphOrigin, vertexAnnotations} = LookupAnnotation[oldGraph, {GraphOrigin, VertexAnnotations}, None];
  isNewVertex = UAssoc @ ConstantAssociation[newVertices, True];
  If[!MemberQ[newVertices, graphOrigin],
    annotations //= ReplaceOptions[GraphOrigin -> None]];
  sortedNewVertexIndices = Sort @ newVertexIndices;
  If[ListQ[vertexCoords],
    vertexCoords = Part[vertexCoords, sortedNewVertexIndices];
    options //= ReplaceOptions[VertexCoordinates -> vertexCoords];
  ];
  If[AssocQ[vertexAnnotations],
    vertexAnnotations //= Map[Part[#, sortedNewVertexIndices]&];
    annotations //= ReplaceOptions[VertexAnnotations -> vertexAnnotations];
  ];
  If[newEdges === Auto,
    newEdges = Select[EdgeList @ oldGraph, isNewVertex[P1[#]] && isNewVertex[P2[#]]&]
  ];
  graph = Graph[newVertices, newEdges, options];
  Annotate[graph, annotations]
];