PublicObject[SmartLayout]

$tetraGraph := $tetraGraph = UndirectedGraph[
  {1 -> 2, 2 -> 3, 3 -> 4, 4 -> 1, 1 -> 3, 2 -> 4},
  VertexCoordinates -> App[CirclePoints[3], {0, 0}]
];

SmartLayout[][data_] := Scope[
  UnpackAssociation[data, vertexCount, edgeCount, indexGraph];
  ugraph = UndirectedGraph @ RemoveEdgeTags @ indexGraph;
  isDir = DirectedGraphQ[indexGraph];
  coords = Which[
    IsomorphicGraphQ[ugraph, $tetraGraph],
      getIsomorphicCoordinates[ugraph, $tetraGraph],
    vertexCount == (edgeCount + 1) && PathGraphQ[ugraph],
      coords = getIsomorphicCoordinates[ugraph, PathGraph[vertexCount]];
      If[Part[coords, 1, 1] < Part[coords, -1, 1], coords, Threaded[{-1, 1}] * coords], (* ensure first vertex in list order is on the left *)
    vertexCount == edgeCount && CycleGraphQ[ugraph],
      getIsomorphicCoordinates[ugraph, CycleGraph[vertexCount]],
    TreeGraphQ[ugraph],
      degree = VertexDegree @ ugraph; nonUnitDegree = Decases[degree, 1];
      If[nonUnitDegree =!= {} && AllSameQ[nonUnitDegree],
        Return @ VertexEdgeCoordinateData[data, {"BalloonEmbedding"}],
        Return @ TreeVertexLayout[Balanced -> True][data]
      ],
    And[isDir, (2 * edgeCount) / vertexCount === 3, !FailureQ[
      dicoords = getIsomorphicCoordinates[
        ugraph, RemoveEdgeTags @ LatticeGraph[{"Dihedral", vertexCount / 2}]
      ]
    ]],
      dicoords,
    True,
      $Failed
  ];
  If[!FailureQ[coords],
    data["VertexCoordinates"] = coords;
    VertexEdgeCoordinateData[data, Auto]
  ,
    VertexEdgeCoordinateData[data, {"SpringElectricalEmbedding"}]
  ]
]

getIsomorphicCoordinates[source_, target_] := Scope[
  iso = FindGraphIsomorphism[source, target];
  iso = F[iso, ReturnFailed[]];
  Lookup[LookupVertexCoordinates[target, All], Lookup[iso, VertexList @ source]]
]
