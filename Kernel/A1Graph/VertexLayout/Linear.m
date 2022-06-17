PublicObject[LinearLayout]

Options[LinearLayout] = {
  Method -> Automatic,
  Orientation -> Automatic
};

$threePoints = CirclePoints[3];

LinearLayout[opts:OptionsPattern[]][data_] := Scope[
  UnpackOptions[method, orientation];
  UnpackAssociation[data, vertexCount];
  SetAutomatic[method, If[AcyclicGraphQ[UndirectedGraph @ data["Graph"]], "Line", "Circle"]];
  SetAutomatic[orientation, If[method === "Line", Left, If[vertexCount === 3, Top, Left]]];
  data["VertexCoordinates"] = orientationTransform[orientation] @ Switch[method,
    "Line",
      N[{# - 1, 0}& /@ Range[vertexCount]],
    "Circle",
      points = MapColumn[Minus, 2, CirclePoints[{1, Pi}, vertexCount]];
      If[vertexCount === 3, points //= TranslationTransform[{0.15, 0}];];
      points
    ,
    True,
      ReturnFailed[]
  ];
  VertexEdgeCoordinateData[data, Automatic]
];

orientationTransform = Case[
  Left        := Identity;
  Right       := RotationTransform[Pi];
  Top         := RotationTransform[-Pi/2];
  TopLeft     := RotationTransform[-Pi/4];
  TopRight    := RotationTransform[-3/4 Pi];
  Bottom      := RotationTransform[Pi/2];
  BottomLeft  := RotationTransform[Pi/4];
  BottomRight := RotationTransform[3/4 Pi];
];