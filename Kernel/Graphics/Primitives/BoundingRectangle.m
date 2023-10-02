PublicHead[BoundingRectangle]

declareGraphicsFormatting[c:BoundingRectangle[$CoordMat2P] :> boundingRectangleBoxes[c], Graphics];

boundingRectangleBoxes[BoundingRectangle[{{xmin_, xmax_}, {ymin_, ymax_}}]] :=
  RectangleBox[{xmin, ymin}, {xmax, ymax}];
