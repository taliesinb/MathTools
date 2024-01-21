PublicGraphicsPrimitive[BoundingRectangle]

DeclareGraphicsPrimitive[BoundingRectangle, "Pair", boundingRectangleBoxes]

boundingRectangleBoxes[BoundingRectangle[{{xmin:$NumberP, xmax:$NumberP}, {ymin:$NumberP, ymax:$NumberP}}]] :=
  RectangleBox[{xmin, ymin}, {xmax, ymax}];

boundingRectangleBoxes[BoundingRectangle[{{xmin:$NumberP, xmax:$NumberP}, {ymin:$NumberP, ymax:$NumberP}}, RoundingRadius -> r_]] :=
  RectangleBox[{xmin, ymin}, {xmax, ymax}, RoundingRadius -> r];
