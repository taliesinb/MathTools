PublicForm[DebugPoints]

SetUsage @ "
DebugPoints[points$] displays as points that are labeled with their indices.
"

declareGraphicsFormatting[DebugPoints[points_List] :> debugPointBoxes[points], Graphics];

debugPointBoxes[matrix_] := Scope[
  points = PointBox @ matrix;
  labels = MapIndex1[Construct[InsetBox, IntegerString @ #2, Offset[{0, 7}, #1]]&, matrix];
  {Red, FontSize -> 8, AbsolutePointSize[5], points, labels}
]

