PublicForm[LoopCurve]

declareGraphicsFormatting[c:LoopCurve[$Coord2P, $Coord2P|$NumberP] :> Construct[LineBox, loopCurvePoints @ c], Graphics];

(**************************************************************************************************)

PrivateFunction[loopCurvePoints]

$loopPoints := $loopPoints = DiscretizeCurve @ BezierCurve[ToPackedReal @ Part[CirclePoints[{1, 0}, {1, -1/2 Tau}, 24], {1,5,8,10,16,18,21,1}], SplineDegree -> 7];

loopCurvePoints[LoopCurve[pos_, dir_]] := Scope[
  If[NumberQ[dir], dir = AngleVector[dir * Tau]];
  Threaded[pos] + Dot[$loopPoints, Transpose @ {dir, VectorRotate90CW @ dir}]
];
