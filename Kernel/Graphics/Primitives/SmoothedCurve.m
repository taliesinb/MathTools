PublicHead[SmoothedCurve]

DeclareCurvePrimitive[SmoothedCurve, smoothedCurvePoints];

smoothedCurvePoints[SmoothedCurve[points_]] :=
  DiscretizeCurve[BezierCurve[points]];

