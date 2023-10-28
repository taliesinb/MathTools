PublicObject[OrderedTreeVertexLayout]

PublicOption[LayerDepths, ParentCentering]

Options[OrderedTreeVertexLayout] = {
  Orientation             -> Top,
  RootVertex              -> Automatic,
  RootOrientation         -> "Source",
  GlobalCentering         -> False,
  FanoutStyle             -> Automatic,
  BendRadius              -> Automatic,
  LayerDepths             -> Automatic
};

SetUsage @ "
OrderedTreeVertexLayout[$$] is a layout engine for ExtendedGraph that supports the following options:
| %Orientation | one of Top or Left, Bottom, or Right |
| %RootOrientation | 'Sink' if root is a sink, 'Source' if it is a source |
| %FanoutStyle | where to put the fan-out between a parent and its children |
| %BendRadius | bend radius of parent-to-child edges |
| %GlobalCentering | whether to center parent on all children or just immediate children |
| %LayerDepths | gives a list of depths for each layer, excluding first |
* %FanoutStyle can be one of Top, Center, Bottom, or 'Circuit'.
* Leaf vertices are visited in depth-first order.
* Leaf vertices occupy successive horizontal positions.
* Parent vertices use the center over the interval spanned by their descendents.
* Vertices are placed at the depth that they live from the root.
"

OrderedTreeVertexLayout[OptionsPattern[]][data_] := Scope[

  UnpackOptions[orientation, rootVertex, rootOrientation, fanoutStyle, bendRadius, layerDepths, globalCentering];

  UnpackAssociation[data, graph, indexGraph, vertexCount, edgeCount, layoutDimension];
  If[layoutDimension === 3, ReturnFailed[]];

  If[rootOrientation === "Sink", indexGraph //= ReverseGraph];
  rootIndex = Switch[rootVertex,
    IndexedVertex[_Integer],    First @ rootVertex,
    GraphOrigin,                VertexIndex[graph, LookupExtendedOption[graph, GraphOrigin]],
    Automatic|"Source",         First[GraphSources @ SimpleGraph @ indexGraph, None],
    _,                          VertexIndex[graph, rootVertex]
  ];
  If[!IntegerQ[rootIndex], ReturnFailed[]];

  vertexCoordinates = OrderedTreeLayoutCoordinates[indexGraph, rootIndex, layerDepths];

  edgeCoordinateLists = ExtractIndices[vertexCoordinates, EdgePairs[indexGraph]];
  edgeCoordinateLists = applyTreeEdgeCoordinateFanoutStyle[edgeCoordinateLists, bendRadius, fanoutStyle];

  fn = Switch[orientation,
    Top,    Identity,
    Bottom, VectorReflectVertical,
    Left,   VectorReflectVertical /* VectorTranspose,
    Right,  VectorReflectHorizontal /* VectorTranspose,
    _,      ReturnFailed[]
  ];

  If[fn =!= Identity,
    vertexCoordinates //= fn;
    edgeCoordinateLists //= Map[fn];
  ];

  {vertexCoordinates, ToPackedReal /@ edgeCoordinateLists}
]

(**************************************************************************************************)

PublicFunction[OrderedTreeLayoutCoordinates]

OrderedTreeLayoutCoordinates[indexGraph_, root_, layerDepths_:Automatic] := Scope[

  vertexCount = VertexCount @ indexGraph;

  depths = ReplaceAutomatic[layerDepths, {}];
  If[NumericQ[depths], depths //= ToList];

  $x = 1.;
  $d = Repeat[0, vertexCount];
  $ccount = Repeat[0, vertexCount];
  $cindex = Repeat[0, vertexCount];
  $parent = Repeat[0, vertexCount];
  $xs = Repeat[0., vertexCount];
  $ys = Repeat[0., vertexCount];
  $bounds = Repeat[{}, vertexCount];
  $isLast = $isFirst = Repeat[False, vertexCount];
  $outDeg = VertexOutDegree @ indexGraph;
  Part[$ccount, rootIndex] = -1;

  If[EdgeCount[indexGraph] == 0, $xs = Range @ vertexCount; Goto[Done]];
  DepthFirstScan[indexGraph, root, {
    "DiscoverVertex"  -> discoverVertex,
    "PrevisitVertex"  -> previsitVertex,
    "PostvisitVertex" -> postvisitVertex
  }];

  mids = Pick[Range @ vertexCount, MapThread[1 < #1 < #2&, {$cindex, Part[$ccount, $parent]}]];
  Part[$xs, mids] += $firstLastDelta;

  Label[Done];
  $maxD = Max[$d] + 1;
  If[depths =!= {},
    numDepths = Length @ depths;
    PrependTo[depths, 0];
    If[$maxD >= numDepths,
      depthDelta = Subtract @@ Part[depths, {-1, -2}];
      newDepths = Last[depths, 1] + Range[$maxD - numDepths] * depthDelta;
      JoinTo[depths, newDepths]
    ];
    $ys = N @ Part[depths, $ys + 1];
  ];

  ToPackedReal @ Transpose[{$xs, $maxD - $ys}]
]

discoverVertex[t_, s_, d_] := (
  Part[$ccount, s]++;
  Part[$cindex, t] = Part[$ccount, s];
  Part[$parent, t] = s;
  Part[$d, t] = d;
);

previsitVertex[v_] := (
  Part[$ys, v] = Part[$d, v];
  If[Part[$outDeg, v] > 0,
    If[globalCentering && Part[$xs, v] == 0., Part[$xs, v] = $x],
    Part[$xs, v] = $x++;
  ];
);

postvisitVertex[v_] := (
  If[Part[$outDeg, v] > 0,
    Part[$xs, v] = If[globalCentering,
      Avg[Part[$xs, v], ($x-1)],
      Mean @ Part[$bounds, v]
    ];
  ];
  If[!globalCentering, Part[$bounds, Part[$parent, v]] //= MinMax[{#, Part[$xs, v]}]&];
)

PrivateVariable[$firstLastDelta]

$firstLastDelta = 0.00125;

