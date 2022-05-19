PackageExport["AGraph"]

pathGraph[n_Integer, opts___] := ExtendedGraph[PathGraph @ Range @ n, opts, VertexLabels -> "Name"];
lineCoords[n_] := Thread[{Range[n], 0}];

AGraph[n_] := AGraph[n] =
  pathGraph[n, VertexCoordinates -> lineCoords[n],
    ImageSize -> chooseWidth[n]
  ];

PackageExport["DGraph"]

DGraph[n_] := DGraph[n] = makeDGraph[n];
(* makeDGraph[n_] := EdgeAdd[
  pathGraph[n-2], {(n-2) <-> (n-1), (n-2) <-> n},
  VertexCoordinates -> Join[lineCoords[n-2], {{n-1, 1/2}, {n-1, -1/2}}],
  ImageSize -> chooseWidth[n - 1]
];
 *)
makeDGraph[n_] := EdgeAdd[
  pathGraph[n-1], {2 <-> n},
  VertexCoordinates -> Join[lineCoords[n-1], {{2, +1}}],
  ImageSize -> chooseWidth[n - 1]
];

PackageExport["EGraph"]

EGraph[n_] /; 6 <= n <= 8 := EGraph[n] = makeEGraph[n-1];
EGraph[_] := None;
EGraph[All] := EGraph /@ {6, 7, 8};

makeEGraph[n_] := EdgeAdd[
  pathGraph[n], (n+1) <-> 3,
  VertexCoordinates -> Append[lineCoords[n], {3, 1}],
  ImageSize -> chooseWidth[n - 1]
]

chooseWidth[n_] := (30 + (n - 1) * 30)


PackageExport["ATildeGraph"]

ATildeGraph[n_] := ATildeGraph[n] = addAffineVertex[AGraph[n], {0, 1}];


PackageExport["DTildeGraph"]

DTildeGraph[n_] := DTildeGraph[n] = addAffineVertex[DGraph[n], {0, 1}];


PackageExport["ETildeGraph"]

E6Tilde[] := E6Tilde[] = addAffineVertex[E6Graph[], {0, 1}];
E7Tilde[] := E7Tilde[] = addAffineVertex[E7Graph[], {-1, 0}];
E8Tilde[] := E8Tilde[] = addAffineVertex[E8Graph[], {1, 0}];
D4Tilde[] := D4Tilde[] = addAffineVertex[DGraph[3], {2, 2}];

ETildeGraphs[] := {E6Tilde[], E7Tilde[], E8Tilde[]};


addAffineVertex[graph_, offset_] := Scope[
  vcoords = GraphVertexCoordinates[graph];
  newVertex = VertexCount[graph] + 1;
  graph = EdgeAdd[graph, newVertex <-> #& /@ VertexList[graph]];
  meanX = Mean @ FirstColumn @ vcoords;
  maxY = Min @ LastColumn @ vcoords;
  AppendTo[vcoords, {meanX, maxY - 0.8}];
  ExtendedGraph[graph, VertexCoordinates -> vcoords]
];
