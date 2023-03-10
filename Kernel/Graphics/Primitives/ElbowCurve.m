PublicForm[ElbowCurve]

declareGraphicsFormatting[ElbowCurve[a:$Coord2P, b:$Coord2P, amount_:Automatic] :> elbowCurveBoxes[a, b, amount], Graphics];

elbowCurveBoxes[a_, b_, amount_] := Construct[BezierCurveBox, ToPackedReal @ elbowBezierCurvePoints[a, b, amount]];

(**************************************************************************************************)

PrivateFunction[elbowBezierCurvePoints]

elbowBezierCurvePoints[a_, b_, amount_] := Scope[
  mid = Avg[a, b];
  delta = Normalize @ VectorRotate90[b - a];
  SetAutomatic[amount, Scaled[0.333]];
  SetScaledFactor[amount, EuclideanDistance[a, b]];
  delta *= amount;
  {a, mid + delta, b}
];
