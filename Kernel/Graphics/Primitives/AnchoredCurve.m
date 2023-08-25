PublicHead[AnchoredCurve]

declareGraphicsFormatting[ac:AnchoredCurve[_, $Coord2P | ($NumberP -> $Coord2P)] :> Construct[LineBox, anchoredCurvePoints @ ac], Graphics];

PrivateFunction[anchoredCurvePoints]

anchoredCurvePoints[AnchoredCurve[curve_, anchor_]] := Scope[
  points = toCurvePoints @ curve;
  {anchorPos, anchorTarget} = Switch[anchor,
    _Rule, FirstLast @ anchor,
    _,     {0.5, anchor}
  ];
  anchorPos = PointAlongLine[points, Scaled @ anchorPos];
  delta = anchorTarget - anchorPos;
  points + Threaded[delta]
]