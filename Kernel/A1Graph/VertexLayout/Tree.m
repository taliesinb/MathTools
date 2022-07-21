PublicObject[TreeVertexLayout]
PublicOption[Orientation, RootVertex, RootOrientation, Balanced, BendStyle, BendRadius, StretchFactor, PreserveBranchOrder]

Options[TreeVertexLayout] = {
  Alignment -> None, Orientation -> Top, RootVertex -> Automatic, "Bubble" -> False,
  Balanced -> False, RootOrientation -> "Source", BendStyle -> Automatic,
  PreserveBranchOrder -> False,
  BendRadius -> 0.25, StretchFactor -> 1
};

TreeVertexLayout::dim3 = "TreeVertexLayout[] does not work with LayoutDimension -> 3, using SpringElectricalLayout[] instead.";

TreeVertexLayout[OptionsPattern[]][data_] := Scope[
  UnpackAssociation[data, graph, indexGraph, vertexCount, layoutDimension];

  If[layoutDimension === 3,
    Message[TreeVertexLayout::dim3];
    Return @ VertexEdgeCoordinateData[data, {"SpringElectricalEmbedding"}]
  ];

  UnpackOptions[alignment, orientation, rootVertex, bubble, balanced, rootOrientation, bendStyle, bendRadius, preserveBranchOrder, stretchFactor];

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

  If[balanceSteps > 0,
    {coordsX, coordsY} = rever @ Transpose @ vertexCoordinates; ocoordsX = coordsX; widthTarget = Max[coordsX] - Min[coordsX];

    coordsX = ElectricalGravitationalBalanceX[0.95 * coordsX + 0.05 * ocoordsX, coordsY, indexGraph, balanceSteps, balanceDelta];
    (* coordsX = (Standardize[coordsX] * widthTarget) + Mean[coordsX]; *)
    vertexCoordinates = Transpose @ rever @ {coordsX, coordsY};
    edgeCoordinateLists = ExtractIndices[vertexCoordinates, EdgePairs[graph]];
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
    "HalfCenter",
      edgeCoordinateLists //= Map[bendCenterHalf]
  ];

  edgeCoordinateLists = fixEdgeVertexIntersections[vertexCoordinates, edgeCoordinateLists];
  {vertexCoordinates, edgeCoordinateLists}
];

bendCenter[{a:{ax_, ay_}, b:{bx_, by_}}] := Scope[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  aby = (ay + by) / 2;
  c = {ax, aby}; d = {bx, aby};
  ca = ptAlong[c, a, bendRadius];
  cd = ptAlong[c, d, bendRadius];
  dc = ptAlong[d, c, bendRadius];
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

(* bendCenterHalf[{a:{ax_, ay_}, b:{bx_, by_}}] := Scope[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  aby = (ay + by) / 2;
  c = {ax, aby}; d = {bx, aby};
  ca = ptAlong[c, a, bendRadius];
  cd = ptAlong[c, d, bendRadius];
  dc = ptAlong[d, c, bendRadius];
  db = ptAlong[d, b, bendRadius];
  Join[{c}, DiscretizeCurve[{dc, d, db}], {b}]
]; *)

bendTop[{a:{ax_, ay_}, b:{bx_, by_}}] := Scope[
  If[Min[Abs[ax - bx], Abs[ay - by]] < 0.001, Return @ {a, b}];
  c = {bx, ay};
  ca = ptAlong[c, a, bendRadius];
  cb = ptAlong[c, b, bendRadius];
  Join[{a}, DiscretizeCurve[{ca, c, cb}], {b}]
];

bendTop[line_] := line;
bendCenter[line_] := line;
bendCenterHalf[line_] := line;