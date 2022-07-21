PublicFunction[ExtendedSubgraph]

ExtendedSubgraph[oldGraph_, newVertices_, newEdges_] := Scope[
  options = Options[oldGraph];
  annotations = ExtendedGraphAnnotations[oldGraph];
  vertexCoords = Lookup[options, VertexCoordinates, Automatic];
  oldVertices = VertexList[oldGraph];
  If[newVertices === All,
    newVertexIndices = Range @ Length @ oldVertices;
    newVertices = oldVertices;
  ,
    oldVertexIndex = AssociationRange @ oldVertices;
    newVertexIndices = Lookup[oldVertexIndex, newVertices];
    newVertexOrdering = Ordering[newVertexIndices];
    newVertices = Part[newVertices, newVertexOrdering];
  ];
  {graphOrigin, vertexAnnotations} = LookupAnnotation[oldGraph, {GraphOrigin, VertexAnnotations}, None];
  isNewVertex = UAssociation @ ConstantAssociation[newVertices, True];
  If[!MemberQ[newVertices, graphOrigin],
    annotations //= ReplaceOptions[GraphOrigin -> None]];
  sortedNewVertexIndices = Sort @ newVertexIndices;
  If[ListQ[vertexCoords],
    vertexCoords = Part[vertexCoords, sortedNewVertexIndices];
    options //= ReplaceOptions[VertexCoordinates -> vertexCoords];
  ];
  If[AssociationQ[vertexAnnotations],
    vertexAnnotations //= Map[Part[#, sortedNewVertexIndices]&];
    annotations //= ReplaceOptions[VertexAnnotations -> vertexAnnotations];
  ];
  If[newEdges === Automatic,
    newEdges = Select[EdgeList @ oldGraph, isNewVertex[Part[#, 1]] && isNewVertex[Part[#, 2]]&]
  ];
  graph = Graph[newVertices, newEdges, options];
  Annotate[graph, annotations]
];
