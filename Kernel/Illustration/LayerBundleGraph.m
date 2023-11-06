PublicFunction[LayerBundleGraph]

Options[LayerBundleGraph] = JoinOptions[
  $ExtendedGraphOptions,
  LayerDistance -> 0.1, LayerScales -> Automatic, ImageSize -> 400
];

LayerBundleGraph[list_List, opts:OptionsPattern[]] := Scope[
  UnpackOptions[layerDistance, layerScales, imageSize];
  If[!ListQ[layerScales], layerScales = Repeat[layerScales, Len @ list]];
  $vertices = $edges = $coords = {};
  $dy = 0;
  ScanIndex1[procBundleLayer, list];
  ExtendedGraph[
    Catenate @ $vertices, Catenate @ $edges, VertexCoordinates -> Catenate[$coords],
    FilterOptions @ opts
  ]
];

procBundleLayer[g_Graph, i_Int] := Scope[
  g2 = MapVertices[SumVertex[i], g];
  AppendTo[$vertices, VertexList @ g2];
  AppendTo[$edges, EdgeList @ g2];
  coords = Values @ LookupVertexCoordinates @ g;
  {{l, b}, {r, t}} = CoordinateBoundingBox @ coords;
  w = r - l;
  scale = Part[layerScales, 1];
  SetAutomatic[scale,
    imageWidth = P1 @ LookupImageSize @ ExtendedGraphPlot[g];
    imageWidth / w
  ];
  SetNone[scale, 1];
  AppendTo[$coords, scale * (Threaded[{-w/2 - l, -b + $dy}] + coords)];
  $dy += (t - b) + layerDistance;
]
