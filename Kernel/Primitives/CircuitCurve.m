PublicGraphicsPrimitive[SnakeCurve]

PublicOption[Orientation, SplitPosition]

Options[SnakeCurve] = {
  Orientation -> Horizontal,
  SplitPosition -> "Middle"
};

DeclareCurvePrimitive[SnakeCurve, snakeCurvePoints];

SignPrimitive["Curve", SnakeCurve];

(**************************************************************************************************)

snakeCurvePoints[SnakeCurve[c:{{x_, _}, {x_, _}}, ___Rule]] := c;
snakeCurvePoints[SnakeCurve[c:{{_, y_}, {_, y_}}, ___Rule]] := c;
snakeCurvePoints[SnakeCurve[{a_, b_}, opts:OptionsPattern[SnakeCurve]]] := Scope[
  UnpackOptionsAs[SnakeCurve, {opts}, orientation, splitPosition];
  splitPosition //= Replace[{"Start" -> 0, "Middle" -> 0.5, "End" -> 1}];
  jmpIndex = Switch[orientation, Horizontal, 1, Vertical, 2];
  avgIndex = 3 - jmpIndex;
  avg = Lerp[a, b, splitPosition];
  mid1 = ReplacePart[avg, jmpIndex -> Part[a, jmpIndex]];
  mid2 = ReplacePart[avg, jmpIndex -> Part[b, jmpIndex]];
  DeleteDuplicates @ N @ {a, mid1, mid2, b}
];

(**************************************************************************************************)

PublicGraphicsPrimitive[CircuitCurve]

PublicOption[BendStyle, SetbackDistance, LineThickness]

Options[CircuitCurve] = JoinOptions[
  SnakeCurve,
  BendStyle -> "Smooth",
  SetbackDistance -> {0.075, -0.075},
  LineThickness -> None
];

DeclareCurvePrimitive[CircuitCurve, circuitCurvePoints, circuitCurveBoxes];

SignPrimitive["Curve | Pair", CircuitCurve];

(**************************************************************************************************)

circuitCurveBoxes[CircuitCurve[points:$CoordMat2P, opts:OptionsPattern[CircuitCurve]]] := Scope[
  UnpackOptionsAs[CircuitCurve, {opts}, lineThickness, setbackDistance];
  If[lineThickness === None, Return @ Construct[LineBox, points]];
  If[lineThickness < 0,
    points = ToPackedReal @ circuitCurvePoints @ curve;
    {{xs, ys}, {xe, ye}} = {first, last} = FirstLast @ points;
    dir = Normalize[last - first] * Abs[lineThickness]/2;
    normal = VectorRotate90[dir];
    dist = If[MatrixQ[setbackDistance], PN, Id] @ setbackDistance;
    points2 = SetbackCoordinates[points, -dist/1.1];
    pointsL = Select[points2 + Threaded[normal], ys >= PN[#] >= ye&];
    pointsR = Select[Rev[points2] - Threaded[normal], ys >= PN[#] >= ye&];
    pointsL = Prepend[{Part[pointsL, 1, 1], ys}] @ Append[{Part[pointsL, -1, 1], ye}] @ pointsL;
    pointsR = Append[{Part[pointsR, -1, 1], ys}] @ Prepend[{Part[pointsR, 1, 1], ye}] @ pointsR;
    polygon = Join[{first}, pointsL, {last}, pointsR];
    Return @ {
      StyleBox[Construct[PolygonBox, polygon], EdgeForm[None]],
      StyleBox[Construct[LineBox, {pointsL, pointsR}], GrayLevel[0, .4], AbsoluteThickness[1]]
    };
  ];
  d = VectorRotate90[Normalize[PN[points] - P1[points]]] * lineThickness/2;
  dist = If[MatrixQ[setbackDistance], PN, Id] @ setbackDistance;
  points2 = SetbackCoordinates[points, -dist/1.1];
  pointsL = Offset[d, #]& /@ points2;
  pointsR = Offset[-d, #]& /@ points2;
  {
    StyleBox[Construct[LineBox, points], AbsoluteThickness @ lineThickness],
    StyleBox[{
      Construct[LineBox, pointsL],
      Construct[LineBox, pointsR]},
      GrayLevel[0, .4], AbsoluteThickness[1]
    ]
  }
];

(**************************************************************************************************)

CircuitCurve::badBendStyle = "BendStyle -> `` should be one of 'Arc', 'Smooth', or 'None'."
circuitCurvePoints[CircuitCurve[points:$CoordPairP, opts:OptionsPattern[CircuitCurve]]] := Scope[
  points = snakeCurvePoints @ SnakeCurve[points, FilterOptions @ opts];
  UnpackOptionsAs[CircuitCurve, {opts}, bendStyle, setbackDistance];
  curve = Switch[bendStyle,
    "Arc",       RollingCurve[points, BendRadius -> .3],
    "Smooth",    SmoothedCurve[points],
    None,        Line[points],
    _,           Message[CircuitCurve::badBendStyle, bendStyle]; Line[points]
  ];
  DiscretizeCurve @ SetbackCurve[curve, If[MatrixQ[setbackDistance], P1, Id] @ setbackDistance]
];