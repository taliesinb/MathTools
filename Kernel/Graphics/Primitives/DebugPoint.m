PublicForm[DebugPoints]

SetUsage @ "
DebugPoints[points$] displays as points that are labeled with their indices.
"

DeclareGraphicsPrimitive[DebugPoints, "Matrices", debugPointBoxes];

debugPointBoxes[DebugPoints[matrix:$CoordMatP]] := Scope[
  points = PointBox @ matrix;
  labels = MapIndex1[Construct[InsetBox, IntegerString @ #2, Offset[{0, 7}, #1]]&, matrix];
  {Red, FontSize -> 8, AbsolutePointSize[5], points, labels}
]

