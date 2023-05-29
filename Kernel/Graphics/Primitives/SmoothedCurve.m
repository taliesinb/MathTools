PublicHead[SmoothedCurve]

declareGraphicsFormatting[s:SmoothedCurve[($CoordMat2P | $GArrowIntP), ___] :> smoothedCurveBoxes[s], Graphics];

smoothedCurveBoxes[curve_] := Construct[LineBox, ToPackedReal @ smoothedCurvePoints @ curve];

(**************************************************************************************************)

PrivateFunction[smoothedCurvePoints]

smoothedCurvePoints[SmoothedCurve[curve_, amount_:Automatic]] := Scope[
  points = toCurvePoints @ curve;
  If[Length[points] == 2, Return @ points];
  DiscretizeCurve[BezierCurve[points]]
];
