PublicGraphicsPrimitive[AnchoredCurve]

(* todo: replace AnchoredCurve with just Anchored, which works on any graphics primitive *)

DeclareCurvePrimitive[AnchoredCurve, anchoredCurvePoints];

SignPrimitive["Curve,Vector", AnchoredCurve];

(**************************************************************************************************)

anchoredCurvePoints[AnchoredCurve[points:$CoordMat2P, anchor:$Coord2P | ($NumberP -> $Coord2P)]] := Scope[
  {anchorPos, anchorTarget} = Switch[anchor,
    _Rule, FirstLast @ anchor,
    _,     {0.5, anchor}
  ];
  anchorPos = PointAlongLine[points, Scaled @ anchorPos];
  delta = anchorTarget - anchorPos;
  points + Threaded[delta]
]