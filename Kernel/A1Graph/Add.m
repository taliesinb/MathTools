PublicFunction[GraphAdd]

SetUsage @ "
GraphAdd[graph$, vertices$, edges$] adds additional vertices and edges to graph$.
* Additional vertices will use vertex coordinates that are given by the mean of their neighbors.
"

GraphAdd[graph_, newVertices_, newEdges_] := Scope[
  CheckIsGraph[1];
  options = Options @ graph;
  {vertices, edges} = VertexEdgeList @ graph;
  newVertices //= ToList;
  newEdges //= ToList;
  newEdges //= Map[toProperEdges];
  If[ContainsQ[newEdges, $Failed], ReturnFailed[]];
  If[IntersectingQ[vertices, newVertices],
    renamedVertices = AdditionalVertex /@ newVertices;
    renamingRules = RuleThread[newVertices, renamedVertices];
    newVertices = renamedVertices;
    newEdges //= ReplaceAll[renamingRules];
  ];
  oldVertexCount = Length @ vertices;
  newVertexCount = Length @ newVertices;
  newGraph = Graph[
    Join[vertices, newVertices],
    Join[edges, newEdges],
    Sequence @@ DropOptions[options, VertexCoordinates]
  ];
  If[!GraphQ[newGraph], ReturnFailed[]];
  newGraph //= DeleteVertexAnnotations;
  vertexCoordinates = Lookup[options, VertexCoordinates];
  If[CoordinateMatrixQ[vertexCoordinates],
    vertexCoordinates = Join[vertexCoordinates, ConstantArray[0., {newVertexCount, InnerDimension @ vertexCoordinates}]];
    newVertexIndices = Range[newVertexCount] + oldVertexCount;
    adjTable = Drop[VertexAdjacencyTable @ newGraph, oldVertexCount];
    Do[
      Part[vertexCoordinates, newVertexIndices] = Map[Mean @ Part[vertexCoordinates, #]&, adjTable];
    ,
      {10}
    ];
    newGraph = Graph[newGraph, VertexCoordinates -> vertexCoordinates];
  ];
  newGraph
];

GraphAdd::badedge = "`` is not a valid edge."

toProperEdges = Case[
  a_ -> b_ := DirectedEdge[a, b];
  a_ <-> b_ := UndirectedEdge[a, b];
  d_DirectedEdge := d;
  u_UndirectedEdge := u;
  other_ := (Message[GraphAdd::badedge, other]; $Failed)
];


PublicHead[AdditionalVertex]
