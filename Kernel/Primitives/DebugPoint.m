PublicGraphicsPrimitive[DebugPoints]

SetUsage @ "
DebugPoints[points$] displays as points that are labeled with their indices.
"

DeclareGraphicsPrimitive[DebugPoints, "Matrices", debugPointBoxes];

debugPointBoxes[DebugPoints[matrix:$CoordMatP, opts___Rule]] := Scope[
  n = Len @ matrix;
  color = Lookup[{opts}, PlotStyle, None];
  If[color === None,
    colors = Hue[#, 1, 1 - #]& /@ Lerp[0.3, 0, Into[n]],
    colors = Repeat[color, n]
  ];
  points = MapThread[StyleBox[PointBox[#1], #2]&, {matrix, colors}];
  labels = If[n > 8,
    {AbsolutePointSize[8], Opacity[0], MapIndex1[makeStatusPoint, matrix]},
    MapIndex1[makeLabeledPoint, matrix]
  ];
  {Red, FontSize -> 8, AbsolutePointSize[5], points, labels}
]

makeLabeledPoint[p_, i_] := Construct[InsetBox, IntStr @ i, Offset[{0, 7}, p]];
makeStatusPoint[p_, i_] := TagBox[PointBox @ p, #!&, TagBoxNote -> IntStr[i]];