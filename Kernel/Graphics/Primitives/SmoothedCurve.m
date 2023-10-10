PublicHead[SmoothedCurve]

DeclareCurvePrimitive[SmoothedCurve, smoothedCurvePoints];

SignPrimitive["Curve", SmoothedCurve];

smoothedCurvePoints[SmoothedCurve[points_]] :=
  DiscretizeCurve[BezierCurve[points]];

