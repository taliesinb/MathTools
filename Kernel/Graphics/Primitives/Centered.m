PublicHead[CenteredCuboid, CenteredRectangle]

Options[CenteredRectangle] = {RoundingRadius -> 0};

CenteredRectangle[pos_] := CenteredRectangle[pos, 1];
CenteredCuboid[pos_] := CenteredCuboid[pos, 1];

DeclareGraphicsPrimitive[CenteredRectangle, "Vector,Radius", centeredRectangleBoxes, {2}];
DeclareGraphicsPrimitive[CenteredCuboid, "Vector,Radius", centeredCuboidBoxes, {3}];

centeredRectangleBoxes[CenteredRectangle[pos:$Coord2P, sz:$Coord2P|$NumberP, opts___Rule]] :=
  Construct[RectangleBox, pos - sz/2, pos + sz/2, opts];

centeredCuboidBoxes[CenteredCuboid[pos:$Coord3P, sz:$Coord3P|$NumberP]] :=
  Construct[CuboidBox, pos - sz/2, pos + sz/2];
