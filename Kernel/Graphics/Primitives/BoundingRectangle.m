PublicHead[BoundingRectangle]

DeclareGraphicsPrimitive[BoundingRectangle, "Pair", boundingRectangleBoxes]

boundingRectangleBoxes[BoundingRectangle[{{xmin:$NumberP, xmax:$NumberP}, {ymin:$NumberP, ymax:$NumberP}}]] :=
  RectangleBox[{xmin, ymin}, {xmax, ymax}];
