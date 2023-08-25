PublicForm[HorizontalCurve, VerticalCurve]

declareGraphicsFormatting[c:(HorizontalCurve[$Coord2P, $NumberP] | HorizontalCurve[$NumberP]) :> Construct[LineBox, horizontalCurvePoints @ c], Graphics];
declareGraphicsFormatting[c:(VerticalCurve[$Coord2P, $NumberP] | VerticalCurve[$NumberP])     :> Construct[LineBox, verticalCurvePoints @ c], Graphics];

(**************************************************************************************************)

PrivateFunction[horizontalCurvePoints, verticalCurvePoints]

horizontalCurvePoints[HorizontalCurve[pos_, p_]] := {pos, pos + {p, 0}};
horizontalCurvePoints[HorizontalCurve[p_]] := {{0,0}, {p, 0}};

verticalCurvePoints[VerticalCurve[pos_, p_]] := {pos, pos + {0, p}};
verticalCurvePoints[VerticalCurve[p_]] := {{0,0}, {0, p}};
