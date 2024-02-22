PublicObject[ManualVertexLayout]

ManualVertexLayout::vnotspec = "Vertices must be specified to use ManualVertexLayout."

ManualVertexLayout[][data_] := Scope[

  UnpackAssociation[data, indexGraph, largeGraphQ, vertexCoordinates];

  If[!CoordinateMatrixQ[vertexCoordinates], ReturnFailed[ManualVertexLayout::vnotspec]];

  If[largeGraphQ || (SimpleGraphQ[indexGraph] && DuplicateFreeQ[Sort /@ EdgePairs[indexGraph]]),
    edgeCoordinateLists = Map[
      pair |-> ToPacked[Part[vertexCoordinates, pair]],
      EdgePairs @ indexGraph
    ];
    {vertexCoordinates, edgeCoordinateLists}
  ,
    VertexEdgeCoordinateData[data, Auto]
  ]
  
];