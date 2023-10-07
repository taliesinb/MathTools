PublicForm[LoopCurve]

DeclareAtomicCurvePrimitive[LoopCurve, loopCurvePoints];

SignPrimitive["Vector,Delta | Vector", LoopCurve];

(**************************************************************************************************)

$loopPoints := $loopPoints = DiscretizeCurve @
  BezierCurve[ToPackedReal @ Part[CirclePoints[{1, 0}, {1, -1/2 Tau}, 24]*Sqrt[1/2.], {1,5,8,10,16,18,21,1}], SplineDegree -> 7];

loopCurvePoints[LoopCurve[pos:$Coord2P, dir:$SidePattern | $Coord2P]] :=
  AffineOperator[RotateToMatrix[dir /. $SideToCoords], pos] @ $loopPoints;

