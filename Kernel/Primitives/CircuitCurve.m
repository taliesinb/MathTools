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

PublicOption[BendStyle, SetbackDistance, LineThickness, WireTypeSlug, WireTypeSlugStyle]

Options[CircuitCurve] = JoinOptions[
  SnakeCurve,
  BendStyle -> "Smooth",
  SetbackDistance -> {0.075, -0.075},
  LineThickness -> None,
  WireTypeSlug -> None,
  WireTypeSlugStyle -> Automatic
];

DeclareCurvePrimitive[CircuitCurve, circuitCurvePoints, circuitCurveBoxes];

SignPrimitive["Curve | Pair", CircuitCurve];

(**************************************************************************************************)

circuitCurveBoxes[CircuitCurve[points:$CoordMat2P, opts:OptionsPattern[CircuitCurve]]] := Scope[
  UnpackOptionsAs[CircuitCurve, {opts}, lineThickness, setbackDistance,
   wireTypeSlug, wireTypeSlugStyle];
  slugPrims = makeSlugPrims[wireTypeSlug, points];
  If[lineThickness === None, Return @ {Construct[LineBox, points], slugPrims}];
  If[lineThickness < 0,
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
      StyleBox[Construct[LineBox, {pointsL, pointsR}], GrayLevel[0, .4], AbsoluteThickness[1]],
      slugPrims
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
    ],
    slugPrims
  }
];

(**************************************************************************************************)

makeSlugPrims := Case[
  Seq[None, _]        := Nothing;
  Seq[list_, points_] := Scope[
    {pos, dir} = VectorAlongLine[points, .35];
    cols = ToRainbowColor /@ list;
    sz = .1;
    Switch[
      ReplaceAutomatic[wireTypeSlugStyle, "Pie"],
      "Beads",
        ColoredBeads[pos, dir, sz, cols],
      "Squares",
        ColoredSquares[pos, dir, sz, cols],
      "Pie",
        ColoriedPie[pos, sz*1.25, cols],
      None,
        Nothing,
      _,
        BadOptionSetting[CircuitCurve, WireTypeSlugStyle, wireTypeSlugStyle];
        Nothing
    ]
  ]
];

ColoredBeads[pos_, dir_, r_, cols_] := Scope[
  n = Length @ cols;
  prims = MapIndex1[
    StyleBox[
      p2 = pos + 2r * dir * (#2-1.5);
      Construct[DiskBox, p2, r],
      FaceEdgeForm @ ToRainbowColor @ #1
    ]&,
    cols
  ];
  {AbsolutePointSize[2], prims}
];

ColoredSquares[pos_, dir_, r_, cols_] := Scope[
  n = Length @ cols;
  pos = pos + VectorRotate90[dir] * r * 1.5;
  MapIndex1[
    StyleBox[
      p2 = pos + 2r * dir * (#2-1.5);
      Construct[RectangleBox, p2 - r, p2 + r],
      FaceEdgeForm @ ToRainbowColor @ #1
    ]&,
    cols
  ]
];

ColoriedPie[pos_, r_, cols_] := Scope[
  n = Length @ cols;
  angs = Range[0, 2Pi, 2Pi / n] + Pi/2;
  disks = ZipMap[
    {Style[Disk[pos, r, #1], FaceForm[#2]], Style[Circle[pos, r, #1], Darker[#2]]}&,
    Partition[angs, 2, 1],
    cols
  ];
  ToGraphicsBoxes @ Style[disks, AbsoluteThickness[1.5]]
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
