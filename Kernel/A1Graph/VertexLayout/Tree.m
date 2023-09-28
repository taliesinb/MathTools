PublicObject[TreeVertexLayout]
PublicOption[Orientation, RootVertex, RootOrientation, Balanced, BendStyle, BendRadius, StretchFactor, PreserveBranchOrder, ManualOffsetInsertions, LayerDepthInterpolation]

Options[TreeVertexLayout] = {
  Orientation             -> Top,
  RootVertex              -> Automatic,
  Balanced                -> False,
  RootOrientation         -> "Source",
  BendStyle               -> Automatic,
  PreserveBranchOrder     -> False,
  BendRadius              -> 0.25,
  LayerDepthInterpolation -> None,
  StretchFactor           -> 1,
  ManualOffsetInsertions  -> {}
};

SetUsage @ "
TreeVertexLayout[$$] is a layout engine for ExtendedGraph that supports the following options:
| %Orientation | one of Top or Left |
| %RootVertex | which vertex is chosen as the root |
| %Balanced | whether to use postprocess layout with electrical graviational layout |
| %RootOrientation | 'Sink' if root is a sink, 'Source' if it is a source |
| %BendStyle | where to put the fan-out between a parent and its children |
| %PreserveBranchOrder | whether to preserve order of edges in branches |
| %BendRadius | bend radius of parent-to-child edges |
| %StretchFactor | amount to deform major axis of layout |
| %ManualOffsetInsertions | list of rules specifying offsets to introduce at particular vertex indices |
* %RootVertex can be one of vertex$, None, Automatic, %IndexedVertex[n$], GraphOrigin 'Source'.
* %Balanced can be one of True, False, steps$, {steps$, delta$}.
* %BendStyle can be one of Top, Center, Bottom, or 'MidCenter'.
"

TreeVertexLayout::dim3 = "TreeVertexLayout[] does not work with LayoutDimension -> 3, using SpringElectricalLayout[] instead.";
TreeVertexLayout::badopt = "Option setting `` is invalid."
TreeVertexLayout[OptionsPattern[]][data_] := Scope[
  UnpackAssociation[data, graph, indexGraph, vertexCount, layoutDimension];

  If[layoutDimension === 3,
    Message[TreeVertexLayout::dim3];
    Return @ VertexEdgeCoordinateData[data, {"SpringElectricalEmbedding"}]
  ];

  UnpackOptions[orientation, rootVertex, balanced, rootOrientation, bendStyle, bendRadius, preserveBranchOrder, stretchFactor, manualOffsetInsertions, layerDepthInterpolation];

  graphOrigin = LookupExtendedOption[graph, GraphOrigin];
  rootIndex = Switch[rootVertex,
    None,                       None,
    Automatic,                  Automatic,
    IndexedVertex[_Integer],    First @ rootVertex,
    GraphOrigin,                If[graphOrigin === None, Automatic, VertexIndex[graph, graphOrigin]],
    "Source",                   First[GraphSources @ SimpleGraph @ ExpandCardinalSetEdges @ indexGraph, None],
    _,                          VertexIndex[graph, rootVertex]
  ];
  baseMethod = If[preserveBranchOrder, "LayeredEmbedding", "LayeredDigraphEmbedding"];
  vertexLayout = {baseMethod, "Orientation" -> orientation, "RootVertex" -> rootIndex};
  
  If[rootOrientation === "Sink", data = MapAt[ReverseEdges, data, "IndexEdges"]; indexGraph //= InvertGraph];
  {vertexCoordinates, edgeCoordinateLists} = VertexEdgeCoordinateData[data, vertexLayout];

  transposed = orientation === Left;
  rever = Switch[orientation, Top, Identity, Left, Reverse, _, $NotImplemented];

  {balanceSteps, balanceDelta} = Replace[balanced, {
    True           -> {100, 0.1},
    _Integer       -> {balanced, 0.1},
    {_Integer, _}  -> balanced,
    _              -> {0, 0}
  }];

  obtainNewEdges = False;
  If[manualOffsetInsertions =!= {},
    off = {0, 0};
    vertexCoordinates //= MapIndex1[{pos, ind} |-> (
      off += Lookup[manualOffsetInsertions, ind, {0, 0}];
      pos + off
    )];
    obtainNewEdges = True;
  ];

  If[balanceSteps > 0,
    {coordsX, coordsY} = rever @ Transpose @ vertexCoordinates; ocoordsX = coordsX; widthTarget = Max[coordsX] - Min[coordsX];

    coordsX = ElectricalGravitationalBalanceX[0.95 * coordsX + 0.05 * ocoordsX, coordsY, indexGraph, balanceSteps, balanceDelta];
    (* coordsX = (Standardize[coordsX] * widthTarget) + Mean[coordsX]; *)
    vertexCoordinates = Transpose @ rever @ {coordsX, coordsY};
    obtainNewEdges = True;
  ];

  If[ListQ[layerDepthInterpolation],
    {coordsX, coordsY} = rever @ Transpose @ vertexCoordinates;
    coordsY *= -1;
    coordsY //= RescaleTo[{0, 1}];
    interp = Interpolation[Transpose[{Lerp[0, 1, Into @ Length @ layerDepthInterpolation], -layerDepthInterpolation}], InterpolationOrder -> 1];
    coordsY = Map[interp, coordsY];
    vertexCoordinates = Transpose @ rever @ {coordsX, coordsY};
    obtainNewEdges = True;
  ];

  If[obtainNewEdges,
    edgeCoordinateLists = ExtractIndices[vertexCoordinates, If[rootOrientation === "Sink", ReverseEdges, Identity] @ EdgePairs[graph]];
  ];

  If[stretchFactor =!= 1,
    stretchX = stretchY = 1;
    If[transposed, stretchX = stretchFactor, stretchY = stretchFactor];
    {vertexCoordinates, edgeCoordinateLists} = {vertexCoordinates, edgeCoordinateLists} /. {x_Real, y_Real} :> {x * stretchX, y * stretchY}
  ];

  Switch[bendStyle,
    "Top" | Top,
      edgeCoordinateLists //= Map[bendTop],
    "Center" | Center,
      edgeCoordinateLists //= Map[bendCenter],
    "HalfCenter" | "MidCenter",
      edgeCoordinateLists //= Map[bendCenterHalf],
    "Bottom",
      edgeCoordinateLists //= Map[bendBottom],
    _,
      Message[TreeVertexLayout::badopt, BendStyle -> bendStyle];
  ];
  edgeCoordinateLists = fixEdgeVertexIntersections[vertexCoordinates, edgeCoordinateLists];
  {vertexCoordinates, edgeCoordinateLists}
];

bendCenter[{a:{ax_, ay_}, b:{bx_, by_}}] := Scope[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  aby = (ay + by) / 2;
  abx = (ax + bx) / 2;
  c = {ax, aby}; d = {bx, aby};
  ca = ptAlong[c, a, bendRadius];
  cd = ptAlong[c, d, bendRadius]; Part[cd, 1] //= ClipOperator[Sort @ {ax, abx}];
  dc = ptAlong[d, c, bendRadius]; Part[dc, 1] //= ClipOperator[Sort @ {bx, abx}];
  db = ptAlong[d, b, bendRadius];
  Join[{a}, DiscretizeCurve[{ca, c, cd}], DiscretizeCurve[{dc, d, db}], {b}]
];

bendCenterHalf[{a:{ax_, ay_}, b:{bx_, by_}}] := Scope[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  aby = (ay + by) / 2;
  c = {ax, aby}; d = {bx, aby};
  ca = ptAlong[c, a, bendRadius];
  cd = ptAlong[c, d, bendRadius];
  dc = ptAlong[d, c, bendRadius];
  db = ptAlong[d, b, bendRadius];
  Join[{a, c}, DiscretizeCurve[{dc, d, db}], {b}]
];

bendTop[{a:{ax_, ay_}, b:{bx_, by_}}] := Scope[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  c = {bx, ay};
  ca = ptAlong[c, a, bendRadius];
  cb = ptAlong[c, b, bendRadius];
  Join[{a}, DiscretizeCurve[{ca, c, cb}], {b}]
];

bendBottom[{a:{ax_, ay_}, b:{bx_, by_}}] := Scope[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  c = {ax, by};
  ca = ptAlong[c, a, bendRadius];
  cb = ptAlong[c, b, bendRadius];
  Join[{a}, DiscretizeCurve[{ca, c, cb}], {b}]
];

bendBottom[line_] := line;
bendTop[line_] := line;
bendCenter[line_] := line;
bendCenterHalf[line_] := line;