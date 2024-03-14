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
  splitPosition //= Rep[{"Start" -> 0, "Middle" -> 0.5, "End" -> 1}];
  jmpIndex = Switch[orientation, Horizontal, 1, Vertical, 2];
  avgIndex = 3 - jmpIndex;
  avg = Lerp[a, b, splitPosition];
  mid1 = RepPart[avg, jmpIndex -> Part[a, jmpIndex]];
  mid2 = RepPart[avg, jmpIndex -> Part[b, jmpIndex]];
  Dedup @ N @ {a, mid1, mid2, b}
];

(**************************************************************************************************)

PublicGraphicsPrimitive[CircuitCurve]

PublicOption[BendStyle, SetbackDistance, LineThickness, WireTypeSlug, WireTypeSlugPosition, WireTypeSlugStyle, LineEdging, LineDashing]

$customNeatCurveOpts = Sequence[
  JoinStyle -> Vertical,
  BendRadius -> 0.2,
  Setback -> 0
];

Options[CircuitCurve] = JoinOptions[
  $customNeatCurveOpts,
  $neatCurveOptions,
  GraphicsScale -> 40,
  LineThickness -> None,
  LineColor -> Inherited,
  WireTypeSlug -> None,
  WireTypeSlugStyle -> Auto,
  WireTypeSlugPosition -> Auto,
  LineEdging -> False,
  LineDashing -> None
];

DeclareCurvePrimitive[CircuitCurve, circuitCurvePoints, circuitCurveBoxes];

SignPrimitive["Curve | Pair", CircuitCurve];

(**************************************************************************************************)

PublicHead[FanOut]

circuitCurveBoxes[CircuitCurve[lines:$CoordMatsP, opts:OptionsPattern[CircuitCurve]]] :=
  Map[circuitCurveBoxes[CircuitCurve[#, opts]]&, lines];

circuitCurveBoxes[CircuitCurve[points:$CoordMat2P, opts:OptionsPattern[CircuitCurve]]] := Scope @ CatchMessage[CircuitCurve,
  UnpackOptionsAs[CircuitCurve, {opts}, lineThickness, setback,
   wireTypeSlug, wireTypeSlugStyle, wireTypeSlugPosition, graphicsScale, lineColor, lineEdging, lineDashing];

  slugPrims = StyleBox[makeSlugPrims[wireTypeSlug, wireTypeSlugPosition, points], ZOrder -> 3];
  line = Construct[LineBox, points];
  color = If[!ColorQ[lineColor], Seq[], lineColor];
  If[lineDashing === True, lineDashing = Dashed];
  dashing = If[lineDashing === None, Seq[], lineDashing];
  If[lineThickness === None,
    Return @ {StyleBox[line, color, dashing, CapForm[None]], slugPrims}];
  If[lineEdging === False,
    Return @ {StyleBox[line, color, dashing, AbsoluteThickness[lineThickness], CapForm[None], ZOrder -> 2], slugPrims};
  ];
  boxes = ToGraphicsBoxes @ {
    StyleBox[
      Construct[LineBox, SetbackCoordinates[points, 0 / graphicsScale]], color,
      AbsoluteThickness @ lineThickness,
      CapForm[None],
      Haloing[If[lineColor === Inherited, $Gray, Darker[lineColor, .2]], 1, .2]
    ],
    If[lineColor =!= Inherited, StyleBox[
      line, color,
      AbsoluteThickness[lineThickness],
      CapForm[None],
      ZOrder -> 2
    ]]
  };
  boxes
];

(**************************************************************************************************)

makeSlugPrims := Case[
  Seq[None, _, _]        := Nothing;
  Seq[list_, pos_, points_] := Scope[
    {pos, dir} = VectorAlongLine[points, SubAuto[pos, .35]];
    cols = ToRainbowColor /@ list;
    sz = .12;
    Switch[
      SubAuto[wireTypeSlugStyle, "Pie"],
      "Beads",   ColoredBeads[pos, dir, sz, cols],
      "Squares", ColoredSquares[pos, dir, sz, cols],
      "Pie",     ColoriedPie[pos, sz*1.25, cols],
      None,      Nothing,
      _,         OptionMsg[WireTypeSlugStyle, wireTypeSlugStyle]
    ]
  ]
];

ColoredBeads[pos_, dir_, r_, cols_] := Scope[
  n = Len @ cols;
  prims = MapIndex1[
    StyleBox[
      p2 = pos + 2r * dir * (#2-1.5);
      Construct[DiskBox, p2, r],
      FaceEdgeForm @ ToRainbowColor @ #1
    ]&,
    cols
  ];
  {AbsolutePointSize[2], AbsoluteThickness[1], prims}
];

ColoredSquares[pos_, dir_, r_, cols_] := Scope[
  n = Len @ cols;
  pos = pos + VectorRotate90[dir] * r * 1.5;
  prims = MapIndex1[
    StyleBox[
      p2 = pos + 2r * dir * (#2-1.5);
      Construct[RectangleBox, p2 - r, p2 + r],
      FaceEdgeForm @ ToRainbowColor @ #1
    ]&,
    cols
  ];
  {AbsoluteThickness[1], prims}
];

ColoriedPie[pos_, r_, cols_] := Scope[
  n = Len @ cols;
  angs = Range[0, 2Pi, 2Pi / n] + Pi/2;
  disks = ZipMap[
    {Style[Disk[pos, r, #1], FaceForm[#2]], Style[Circle[pos, r, #1], Darker[#2]]}&,
    Partition[angs, 2, 1],
    cols
  ];
  ToGraphicsBoxes @ Style[disks, AbsoluteThickness[1]]
];

(**************************************************************************************************)

CircuitCurve::badBendStyle = "BendStyle -> `` should be one of 'Arc', 'Smooth', or 'None'."

circuitCurvePoints[CircuitCurve[curve_, ___]] := DiscretizeCurve @ curve;

circuitCurvePoints[CircuitCurve[points:$CoordPairP, opts:OptionsPattern[CircuitCurve]]] := Scope[
  neatCurvePoints @ NeatCurve[points, FilterOptions @ opts, $customNeatCurveOpts]
];

circuitCurvePoints[CircuitCurve[FanOut[src_, dsts_], opts:OptionsPattern[CircuitCurve]]] := Scope[
  UnpackOptionsAs[CircuitCurve, {opts}, bendRadius];
  opts = Sequence[BendRadius -> {0, bendRadius}, FilterOptions[NeatCurve, opts], $customNeatCurveOpts];
  Map[
    neatCurvePoints @ NeatCurve[{src, #}, opts]&,
    dsts
  ]
];