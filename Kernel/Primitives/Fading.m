PublicGraphicsPrimitive[FadingStripe]

DeclareGraphicsPrimitive[FadingStripe, "Vector,Radius", fadingStripeBoxes]

fadingStripeBoxes[FadingStripe[p:$CoordP, d:$CoordP, w:$NumberP]] := Scope[
  d2 = Normalize[VectorRotate90[d]] * w/2;
  points = ToPackedReal @ {p + d2, p + d2 + d, p - d2 + d, p - d2};
  white = GrayLevel[1, 1]; trans = GrayLevel[1, 0];
  TagBox[
    Construct[PolygonBox, points, VertexColors -> {trans, white, white, trans}],
    "NoBounds"
  ]
];
