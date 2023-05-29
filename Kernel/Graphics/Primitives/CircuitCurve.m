PublicForm[CircuitCurve]

declareGraphicsFormatting[c:CircuitCurve[{$Coord2P, $Coord2P}] :> circuitCurveBoxes[c], Graphics];

circuitCurveBoxes[c_] := Construct[LineBox, ToPackedReal @ circuitCurvePoints[c]];

(**************************************************************************************************)

PrivateFunction[circuitCurvePoints]

circuitCurvePoints[CircuitCurve[c:{{x_, _}, {x_, _}}]] := c;
circuitCurvePoints[CircuitCurve[c:{{_, y_}, {_, y_}}]] := c;
circuitCurvePoints[CircuitCurve[{a:{ax_, ay_}, b:{bx_, by_}}]] := Scope[
  my = Avg[ay, by];
  {a, {ax, my}, {bx, my}, b}
];
