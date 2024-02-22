PublicFunction[ADEAGraph]

pathGraph[n_Int, opts___] := ExtendedGraph[PathGraph @ Range @ n, opts, VertexLabels -> "Name"];
lineCoords[n_] := Thread[{Range[n], 0}];

ADEAGraph[n_] := ADEAGraph[n] =
  pathGraph[n, VertexCoordinates -> lineCoords[n],
    ImageSize -> chooseWidth[n]
  ];

PublicFunction[ADEDGraph]

ADEDGraph[n_] := ADEDGraph[n] = makeDGraph[n];
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

PublicFunction[ADEEGraph]

ADEEGraph[n_] /; 6 <= n <= 8 := ADEEGraph[n] = makeEGraph[n-1];
ADEEGraph[_] := None;
ADEEGraph[All] := EGraph /@ {6, 7, 8};

makeEGraph[n_] := EdgeAdd[
  pathGraph[n], (n+1) <-> 3,
  VertexCoordinates -> App[lineCoords[n], {3, 1}],
  ImageSize -> chooseWidth[n - 1]
]

chooseWidth[n_] := (30 + (n - 1) * 30)


PublicFunction[ADEATildeGraph]

ADEATildeGraph[n_] := ADEATildeGraph[n] = addAffineVertex[AGraph[n], {0, 1}];


PublicFunction[ADEDTildeGraph]

ADEDTildeGraph[n_] := ADEDTildeGraph[n] = addAffineVertex[DGraph[n], {0, 1}];


PublicFunction[ADEETildeGraph]

ADEE6Tilde[] := ADEE6Tilde[] = addAffineVertex[E6Graph[], {0, 1}];
ADEE7Tilde[] := ADEE7Tilde[] = addAffineVertex[E7Graph[], {-1, 0}];
ADEE8Tilde[] := ADEE8Tilde[] = addAffineVertex[E8Graph[], {1, 0}];
ADED4Tilde[] := ADED4Tilde[] = addAffineVertex[DGraph[3], {2, 2}];

ADEETildeGraphs[] := {ADEE6Tilde[], ADEE7Tilde[], ADEE8Tilde[]};


addAffineVertex[graph_, offset_] := Scope[
  vcoords = GraphVertexCoordinates[graph];
  newVertex = VertexCount[graph] + 1;
  graph = EdgeAdd[graph, newVertex <-> #& /@ VertexList[graph]];
  meanX = Mean @ FirstColumn @ vcoords;
  maxY = Min @ LastColumn @ vcoords;
  AppTo[vcoords, {meanX, maxY - 0.8}];
  ExtendedGraph[graph, VertexCoordinates -> vcoords]
];
