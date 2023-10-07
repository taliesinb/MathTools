PublicHead[ShrunkCurve]

DeclareCurvePrimitive[ShrunkCurve, shrunkCurvePoints];

SignPrimitive["Curve,Radius", shrunkCurvePoints];

(**************************************************************************************************)

shrunkCurvePoints[ShrunkCurve[points:$CoordMat2P, amount:$NumberP]] :=
  ShrinkPolygon[points, amount]

