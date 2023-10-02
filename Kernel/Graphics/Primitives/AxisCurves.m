PublicForm[HorizontalCurve, VerticalCurve]

declareGraphicsFormatting[{
  c:HorizontalCurve[$Coord2P, $NumberP|$Coord2P] :> Construct[LineBox, horizontalCurvePoints @ c],
  c:VerticalCurve[$Coord2P, $NumberP|$Coord2P] :> Construct[LineBox, verticalCurvePoints @ c],
  c:HorizontalCurve[$NumberP|$Coord2P] :> Construct[LineBox, horizontalCurvePoints @ c],
  c:VerticalCurve[$NumberP|$Coord2P]     :> Construct[LineBox, verticalCurvePoints @ c]
}, Graphics];

(**************************************************************************************************)

PrivateFunction[horizontalCurvePoints, verticalCurvePoints]

horizontalCurvePoints = Case[
  HorizontalCurve[p_]               := % @ HorizontalCurve[{0, 0}, p];
  HorizontalCurve[pos_, p_]         := ToPackedReal @ {pos, pos + {p, 0}};
  HorizontalCurve[pos_, {p1_, p2_}] := ToPackedReal @ {pos + {p1, 0}, pos + {p2, 0}};
];

verticalCurvePoints = Case[
  VerticalCurve[p_]               := % @ VerticalCurve[{0, 0}, p];
  VerticalCurve[pos_, p_]         := ToPackedReal @ {pos, pos + {0, p}};
  VerticalCurve[pos_, {p1_, p2_}] := ToPackedReal @ {pos + {0, p1}, pos + {0, p2}}
];
