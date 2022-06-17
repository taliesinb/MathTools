PublicObject[SmartLayout]

$tetraGraph := $tetraGraph = UndirectedGraph[
  {1 -> 2, 2 -> 3, 3 -> 4, 4 -> 1, 1 -> 3, 2 -> 4},
  VertexCoordinates -> Append[CirclePoints[3], {0, 0}]
];

SmartLayout[][data_] := Scope[
  UnpackAssociation[data, vertexCount, edgeCount, indexGraph];
  ugraph = UndirectedGraph @ RemoveEdgeTags @ indexGraph;
  coords = Which[
    IsomorphicGraphQ[ugraph, $tetraGraph],
      getIsomorphicCoordinates[ugraph, $tetraGraph],
    vertexCount == (edgeCount + 1) && PathGraphQ[ugraph],
      getIsomorphicCoordinates[ugraph, PathGraph[vertexCount]],
    True,
      $Failed
  ];
  If[!FailureQ[coords],
    data["VertexCoordinates"] = coords;
    VertexEdgeCoordinateData[data, Automatic]
  ,
    VertexEdgeCoordinateData[data, {"SpringElectricalEmbedding"}]
  ]
]

getIsomorphicCoordinates[source_, target_] := Scope[
  iso = FindGraphIsomorphism[source, target];
  If[iso === {}, ReturnFailed[]];
  targetVertices = Lookup[First @ iso, VertexList @ source];
  LookupVertexCoordinates[target, targetVertices]
]
