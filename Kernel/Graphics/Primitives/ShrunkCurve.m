PublicHead[ShrunkCurve]

DeclareCurvePrimitive[ShrunkCurve, shrunkCurvePoints];

SignPrimitive["Curve,Radius", ShrunkCurve];

(**************************************************************************************************)

shrunkCurvePoints[ShrunkCurve[points:$CoordMat2P, amount:$NumberP]] :=
  ShrinkPolygon[points, amount]

