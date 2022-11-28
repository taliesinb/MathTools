PublicHead[CenteredCuboid, CenteredRectangle]

CenteredCuboid[pos_] := CenteredCuboid[pos, 1];
CenteredRectangle[pos_] := CenteredRectangle[pos, 1];

declareGraphicsFormatting[
  CenteredCuboid[pos:$Coord3P, sz:$Coord3P|$NumberP] :> Construct[CuboidBox, pos - sz/2, pos + sz/2],
  Graphics3D
]

declareGraphicsFormatting[
  CenteredRectangle[pos:$Coord2P, sz:$Coord2P|$NumberP] :> Construct[RectangleBox, pos - sz/2, pos + sz/2],
  Graphics
];
