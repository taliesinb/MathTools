PublicGraphicsPrimitive[SetbackCurve]

DeclareCurvePrimitive[SetbackCurve, setbackCurveCoordinates];

SignPrimitive["Curve,Radius", SetbackCurve];

(**************************************************************************************************)

setbackCurveCoordinates[SetbackCurve[points:$CoordMatP, spec_]] :=
  SetbackCoordinates[points, spec];