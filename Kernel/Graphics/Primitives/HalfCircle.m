PublicHead[HalfCircle, HalfDisk]

DeclareGraphicsPrimitive[HalfCircle, "Vector,Delta", halfCircleBoxes]
DeclareGraphicsPrimitive[HalfDisk, "Vector,Delta", halfDiskBoxes]

halfCircleBoxes[HalfCircle[p:$CoordP, d:$CoordP]] := Scope[
  ang = PairAngle[d];
  p1 = p + VectorRotate90[d];
  p2 = p + VectorRotate90CW[d];
  List[
    Construct[CircleBox, p, Norm @ d, {ang - Pi/2, ang + Pi/2}],
    Construct[LineBox, {p1, p2}]
  ]
];

halfDiskBoxes[HalfDisk[p:$CoordP, d:$CoordP]] := Scope[
  ang = PairAngle[d];
  Construct[DiskBox, p, Norm @ d, {ang - Pi/2, ang + Pi/2}]
];
