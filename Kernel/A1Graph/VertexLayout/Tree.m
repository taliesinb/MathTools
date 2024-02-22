PublicObject[TreeVertexLayout]
PublicOption[Orientation, RootVertex, RootOrientation, Balanced, FanoutStyle, BendRadius, StretchFactor, PreserveBranchOrder]

Options[TreeVertexLayout] = {
  Orientation             -> Top,
  RootVertex              -> Auto,
  Balanced                -> False,
  RootOrientation         -> "Source",
  FanoutStyle             -> Auto,
  PreserveBranchOrder     -> False,
  BendRadius              -> Auto,
  LayerDepthInterpolation -> None,
  StretchFactor           -> 1
};

SetUsage @ "
TreeVertexLayout[$$] is a layout engine for ExtendedGraph based on the kernel 'LayeredEmbedding' that supports the following options:
| %Orientation | one of Top or Left |
| %RootVertex | which vertex is chosen as the root |
| %Balanced | whether to use postprocess layout with electrical gravitational layout |
| %RootOrientation | 'Sink' if root is a sink, 'Source' if it is a source |
| %FanoutStyle | where to put the fan-out between a parent and its children |
| %PreserveBranchOrder | whether to preserve order of edges in branches |
| %BendRadius | bend radius of parent-to-child edges |
| %StretchFactor | amount to deform major axis of layout |
* %RootVertex can be one of vertex$, None, Automatic, %IndexedVertex[n$], GraphOrigin 'Source'.
* %Balanced can be one of True, False, steps$, {steps$, delta$}.
* %FanoutStyle can be one of Top, Center, Bottom, or 'Circuit'.
* use %OrderedTreeVertexLayout for a more predictable layout that doesn't do smart packing of subtrees.
"

TreeVertexLayout::dim3 = "TreeVertexLayout[] does not work with LayoutDimension -> 3, using SpringElectricalLayout[] instead.";
TreeVertexLayout::badopt = "Option setting `` is invalid."
TreeVertexLayout[OptionsPattern[]][data_] := Scope[
  UnpackAssociation[data, graph, indexGraph, vertexCount, layoutDimension];

  If[layoutDimension === 3,
    Message[TreeVertexLayout::dim3];
    Return @ VertexEdgeCoordinateData[data, {"SpringElectricalEmbedding"}]
  ];

  UnpackOptions[orientation, rootVertex, balanced, rootOrientation, fanoutStyle, bendRadius, preserveBranchOrder, stretchFactor];

  graphOrigin = LookupExtendedOption[graph, GraphOrigin];
  rootIndex = Switch[rootVertex,
    None,                       None,
    Auto,                  Auto,
    IndexedVertex[_Int],    F @ rootVertex,
    GraphOrigin,                If[graphOrigin === None, Auto, VertexIndex[graph, graphOrigin]],
    "Source",                   F[GraphSources @ SimpleGraph @ ExpandCardinalSetEdges @ indexGraph, None],
    _,                          VertexIndex[graph, rootVertex]
  ];
  baseMethod = If[preserveBranchOrder, "LayeredEmbedding", "LayeredDigraphEmbedding"];
  vertexLayout = {baseMethod, "Orientation" -> orientation, "RootVertex" -> rootIndex};
  
  If[rootOrientation === "Sink", data = MapAt[ReverseEdges, data, "IndexEdges"]; indexGraph //= InvertGraph];
  {vertexCoordinates, edgeCoordinateLists} = VertexEdgeCoordinateData[data, vertexLayout];

  transposed = orientation === Left;
  rever = Switch[orientation, Top, Id, Left, Rev, _, $NotImplemented];

  {balanceSteps, balanceDelta} = Rep[balanced, {
    True           -> {100, 0.1},
    _Int       -> {balanced, 0.1},
    {_Int, _}  -> balanced,
    _              -> {0, 0}
  }];

  obtainNewEdges = False;
  If[balanceSteps > 0,
    {coordsX, coordsY} = rever @ Transpose @ vertexCoordinates; ocoordsX = coordsX; widthTarget = Max[coordsX] - Min[coordsX];

    coordsX = ElectricalGravitationalBalanceX[0.95 * coordsX + 0.05 * ocoordsX, coordsY, indexGraph, balanceSteps, balanceDelta];
    (* coordsX = (Standardize[coordsX] * widthTarget) + Mean[coordsX]; *)
    vertexCoordinates = Transpose @ rever @ {coordsX, coordsY};
    obtainNewEdges = True;
  ];

  If[obtainNewEdges,
    edgeCoordinateLists = ExtractIndices[vertexCoordinates, If[rootOrientation === "Sink", ReverseEdges, Id] @ EdgePairs[indexGraph]];
  ];

  If[stretchFactor =!= 1,
    stretchX = stretchY = 1;
    If[transposed, stretchX = stretchFactor, stretchY = stretchFactor];
    {vertexCoordinates, edgeCoordinateLists} = {vertexCoordinates, edgeCoordinateLists} /. {x_Real, y_Real} :> {x * stretchX, y * stretchY}
  ];

  edgeCoordinateLists = applyTreeEdgeCoordinateFanoutStyle[edgeCoordinateLists, bendRadius, fanoutStyle];

  edgeCoordinateLists = fixEdgeVertexIntersections[vertexCoordinates, edgeCoordinateLists];

  {vertexCoordinates, edgeCoordinateLists}
];

(**************************************************************************************************)

PrivateFunction[applyTreeEdgeCoordinateFanoutStyle]

applyTreeEdgeCoordinateFanoutStyle[edgeCoordinateLists_, bendRadius_, fanoutStyle_] := Scope[
  $r = bendRadius;
  SetAutomatic[$r, 0.333 * MinimumDistance[Join[
    Part[edgeCoordinateLists, All, 1],
    Part[edgeCoordinateLists, All, -1]]]
  ];
  Switch[fanoutStyle,
    "Top" | Top,
      Map[bendTop, edgeCoordinateLists],
    "Circuit",
      Map[bendCenter, edgeCoordinateLists],
    "Center" | Center,
      Map[bendCenterFraction[#, 0.5]&, edgeCoordinateLists],
    Center -> _,
      Map[bendCenterFraction[#, L @ fanoutStyle]&, edgeCoordinateLists],
    "Bottom" | Bottom,
      Map[bendBottom, edgeCoordinateLists],
    Auto | None,
      edgeCoordinateLists,
    _,
      Message[TreeVertexLayout::badopt, fanoutStyle -> fanoutStyle];
      edgeCoordinateLists
  ]
]

bendCenter[{a:{ax_, ay_}, b:{bx_, by_}}] := Scope[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  aby = (ay + by) / 2;
  abx = (ax + bx) / 2;
  c = {ax, aby}; d = {bx, aby};
  ca = ptAlong[c, a, $r];
  cd = ptAlong[c, d, $r]; Part[cd, 1] //= ClipOperator[Sort @ {ax, abx}];
  dc = ptAlong[d, c, $r]; Part[dc, 1] //= ClipOperator[Sort @ {bx, abx}];
  db = ptAlong[d, b, $r];
  Join[{a}, DiscretizeCurve[{ca, c, cd}], DiscretizeCurve[{dc, d, db}], {b}]
];

bendCenterFraction[{a:{ax_, ay_}, b:{bx_, by_}}, f_] := Scope[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  aby = Lerp[ay, by, f];
  If[isMidX[bx], Return @ {{bx, aby}, {bx, by}}];
  c = {ax, aby}; d = {bx, aby};
  ca = ptAlong[c, a, $r];
  cd = ptAlong[c, d, $r];
  dc = ptAlong[d, c, $r];
  db = ptAlong[d, b, $r];
  Join[{a, c}, DiscretizeCurve[{dc, d, db}], {b}]
];

isMidX[x_] := Norm[FractionalPart[x] - $firstLastDelta] < $firstLastDelta/128;

bendTop[{a:{ax_, ay_}, b:{bx_, by_}}] := Scope[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  If[isMidX[bx], Return @ {{bx, ay}, {bx, by}}];
  c = {bx, ay};
  ca = ptAlong[c, a, $r];
  cb = ptAlong[c, b, $r];
  Join[{a}, DiscretizeCurve[{ca, c, cb}], {b}]
];

bendBottom[{a:{ax_, ay_}, b:{bx_, by_}}] := Scope[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  c = {ax, by};
  ca = ptAlong[c, a, $r];
  cb = ptAlong[c, b, $r];
  Join[{a}, DiscretizeCurve[{ca, c, cb}], {b}]
];

bendBottom[line_] := line;
bendTop[line_] := line;
bendCenter[line_] := line;
bendCenterFraction[line_, _] := line;