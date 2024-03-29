PublicObject[LinearLayout]

Options[LinearLayout] = {
  Method -> Auto,
  Orientation -> Auto
};

SetCached[$threePoints, CirclePoints[3]];

LinearLayout[opts:OptionsPattern[]][data_] := Scope[
  UnpackOptions[method, orientation];
  UnpackAssociation[data, vertexCount, layoutDimension];
  SetAuto[method, If[AcyclicGraphQ[UndirectedGraph @ data["Graph"]], "Line", "Circle"]];
  If[layoutDimension === 1, method = "Line"];
  SetAuto[orientation, If[method === "Line", Left, If[vertexCount === 3, Top, Left]]];
  data["VertexCoordinates"] = orientationTransform[orientation] @ Switch[method,
    "Line",
      N @ Array[{# - 1, 0}&, vertexCount],
    "Circle",
      points = MapColumn[Minus, 2, CirclePoints[{1, Pi}, vertexCount]];
      If[vertexCount === 3, points //= TranslationTransform[{0.15, 0}];];
      points
    ,
    True,
      ReturnFailed[]
  ];
  VertexEdgeCoordinateData[data, Auto]
];

orientationTransform = Case[
  Left        := Id;
  Right       := RotationTransform[Pi];
  Top         := RotationTransform[-Pi/2];
  TopLeft     := RotationTransform[-Pi/4];
  TopRight    := RotationTransform[-3/4 Pi];
  Bottom      := RotationTransform[Pi/2];
  BottomLeft  := RotationTransform[Pi/4];
  BottomRight := RotationTransform[3/4 Pi];
];