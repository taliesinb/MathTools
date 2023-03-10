PublicHead[SetbackCurve]

declareGraphicsFormatting[c:SetbackCurve[$CoordMat3P | $GCurveIntP, ___Rule] :> Construct[Line3DBox, setbackCurvePoints @ c], Graphics3D];
declareGraphicsFormatting[c:SetbackCurve[$CoordMat2P | $GCurveIntP, ___Rule] :> Construct[LineBox,   setbackCurvePoints @ c], Graphics];

(**************************************************************************************************)

PrivateFunction[setbackCurvePoints]

setbackCurvePoints[SetbackCurve[curve_, d_]] := Scope[
  points = toCurvePoints @ curve;
  SetbackCoordinates[points, d]
]
