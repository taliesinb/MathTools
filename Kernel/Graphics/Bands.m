PublicOption[BandRadii, BandPoints, BandTwists]

(**************************************************************************************************)

$easeSpeed = 50;
bandEasingFunction[x_] := Tanh[$easeSpeed (x - 1)^1/9] + Tanh[$easeSpeed (x + 1)^1/9];

bandThetas[twists_, n_] := Interpolated[0, Pi * twists, n];
bandThetas[1, n_] := Pi/2 * Map[bandEasingFunction, Interpolated[-1, 1, n]];
bandThetas[0, n_] := ConstantArray[0, n];

bandThetas[twists_, x_List] := Pi * twists * x;
bandThetas[1, x_List] := Pi/2 * Map[bandEasingFunction, x];
bandThetas[0, x_List] := ConstantArray[0, Length @ x];


PublicFunction[BandBoundaryPoints]

Options[BandBoundaryPoints] = {
  BandRadii -> {5, 2},
  BandPoints -> 128,
  BandTwists -> 1
};

BandBoundaryPoints[OptionsPattern[]] := Scope[
  UnpackOptions[bandTwists, bandRadii, bandPoints];
  n = bandPoints + 1;
  phi = Interpolated[-Pi, Pi, n];
  theta = bandThetas[bandTwists, n];
  p1 = TorusVector[bandRadii, {phi, theta + Pi/2}];
  p2 = TorusVector[bandRadii, {phi, theta - Pi/2}];
  {p1, p2}
];

(**************************************************************************************************)

PublicFunction[BandGraphicsComplex]

Options[BandGraphicsComplex] = JoinOptions[
  BandBoundaryPoints
];

BandGraphicsComplex[opts:OptionsPattern[]] := Scope[
  UnpackOptions[bandTwists];
  {p1, p2} = BandBoundaryPoints[opts];
  len = Length @ p1;
  p = ToPacked @ N @ Join[p1, p2];
  n1 = Range[len]; n2 = n1 + len;
  lineIndices = If[OddQ[bandTwists], Join[n1, n2], AppendFirst /@ {n1, n2}];
  polyIndices = ApplyWindowed[bandFacePoly, Trans[n1, n2]];
  GraphicsComplex[p, {
      {EdgeForm[None], Polygon @ ToPacked @ polyIndices},
      Line @ ToPacked @ lineIndices
    }
  ]
];

bandFacePoly[{a_, b_}, {c_, d_}] := {a, c, d, b};

(**************************************************************************************************)

PublicFunction[BandCurvePoints]

Options[BandCurvePoints] = JoinOptions[
  BandBoundaryPoints
];

BandCurvePoints[None, ___] := {};

BandCurvePoints[fn_, opts:OptionsPattern[]] := BandCurvePoints[fn, {-1, 1}, opts];

BandCurvePoints[fn_, range_List, opts:OptionsPattern[]] := Scope[
  UnpackOptions[bandRadii, bandPoints, bandTwists];
  {R, r} = bandRadii; n = bandPoints + 1;
  phi = Interpolated[-Pi, Pi, n];
  theta = bandThetas[bandTwists, n];
  {lo, hi} = range;
  r *= N @ Map[fn, Interpolated[lo, hi, n]];
  If[!RealVectorQ[r], ReturnFailed[]];
  ToPacked @ N @ TorusVector[{R, r}, {phi, theta + Pi/2}]
];

(**************************************************************************************************)

Options[BandCurveProject] = JoinOptions[
  BandBoundaryPoints
];

BandCurveProject[point_ ? CoordinateVectorQ, opts:OptionsPattern[]] :=
  First @ BandCurveProject[List @ point, opts];

BandCurveProject[points_ ? CoordinateArrayQ, opts:OptionsPattern[]] :=
  BandCurveProject[#, opts]& /@ points;

BandCurveProject[points_ ? CoordinateMatrixQ, opts:OptionsPattern[]] := Scope[
  UnpackOptions[bandRadii, bandPoints, bandTwists];
  {R, r} = bandRadii; n = bandPoints + 1;
  {x, y} = Transpose @ N @ points;
  phi = Pi * x;
  theta = bandThetas[bandTwists, x];
  r *= y;
  If[!RealVectorQ[r], ReturnFailed[]];
  ToPacked @ N @ TorusVector[{R, r}, {phi, theta + Pi/2}]
];

(**************************************************************************************************)

PublicFunction[BandAxisPoints]

Options[BandAxisPoints] = JoinOptions[
  BandBoundaryPoints
];

BandAxisPoints[opts:OptionsPattern[]] := Scope[
  UnpackOptions[bandRadii, bandPoints, bandTwists];
  {R, r} = bandRadii; n = bandPoints + 1;
  phi = Interpolated[-Pi, Pi, n];
  theta = bandThetas[bandTwists, n];
  ToPacked @ Transpose @ N @ {R * Cos[phi], R * Sin[phi], ConstantArray[0, n]}
]

(**************************************************************************************************)

PublicFunction[BandPlot2D]
PublicOption[AxisColors]

Options[BandPlot2D] = JoinOptions[
  "Endpoints" -> True,
  Epilog -> {},
  PlotStyle -> $DarkPink,
  AxisStyle -> {$Blue, $Red},
  ImageSize -> 200,
  BandBoundaryPoints
];

BandPlot2D[fn_, opts:OptionsPattern[]] := Scope[
  If[fn =!= None, pl = fn[-1]; pr = fn[1]];
  dy = -0.01;
  UnpackOptions[endpoints, imageSize, epilog, plotStyle, axisStyle];
  {hStyle, vStyle} = axisStyle;
  SetAutomatic[hStyle, $Blue]; SetAutomatic[vStyle, $Red];
  Plot[
    Evaluate @ fn[x],
    {x, -1, 1},
    PlotStyle -> plotStyle, Axes -> False,
    FrameStyle -> None, Frame -> False, Ticks -> None,
    Prolog -> {
      Style[{
          If[hStyle === None, {}, Style[Line @ {{-1, 0}, {1, 0}}, hStyle]],
          If[vStyle === None, {}, Style[Line @ {{0, -1}, {0, 1}}, vStyle]]
        },
        AbsoluteThickness[1], Dashed, $Gray],
      EdgeForm @ $Gray, FaceForm @ GrayLevel[0, 0.02], Rectangle[{-1, -1 + dy}, {1, 1 + dy}]
    },
    Epilog -> {
      If[endpoints === False || fn === None, {}, {
        plotStyle, makeClosedPoint @ {-1, pl}, makeOpenPoint @ {1, pr}
      }],
      {AbsolutePointSize[7], epilog}
    },
    Frame -> True, ImageSize -> imageSize, AspectRatio -> 0.5,
    PlotRange -> {{-1, 1}, {-1, 1}}, PlotRangeClipping -> False, PlotRangePadding -> 0,
    ImagePadding -> {{5, 5}, {5, 5}}
  ]
];

makeClosedPoint[p_] :=  {AbsolutePointSize[6.5], Point @ p};
makeOpenPoint[p_] := {AbsolutePointSize[7], Point @ p, White, AbsolutePointSize[4], Point @ p};

makeClosedPoint3D[p_] :=  {AbsolutePointSize[7], Point @ p};
makeOpenPoint3D[p_] := {
  plotStyle, AbsolutePointSize[7], Point @ p, Color3D[White], Sphere[p, 0.15]
}

(**************************************************************************************************)

PublicFunction[BandPlot3D]

Options[BandPlot3D] = JoinOptions[
  EdgeThickness -> 2,
  Axes -> True,
  ImageSize -> {200, 150},
  PlotStyle -> $DarkPink,
  AxisStyle -> {$Blue, $Red},
  Epilog -> {},
  BandBoundaryPoints
];

BandPlot3D[fn_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[edgeThickness, bandRadii, bandTwists, imageSize, plotStyle, axisStyle, epilog];
  {hStyle, vStyle} = axisStyle;
  SetAutomatic[hStyle, $Blue]; SetAutomatic[vStyle, $Red];
  {R, r} = bandRadii;
  p = BandCurvePoints[fn, FilterOptions @ opts];
  tube = Style[Line[p], AbsoluteThickness[edgeThickness], plotStyle];
  band = Style[BandGraphicsComplex[FilterOptions @ opts], FaceForm[GrayLevel[0, 0.05]]];
  hAxis = AppendFirst @ BandAxisPoints[FilterOptions @ opts];
  vAxis = {{R, 0, -r}, {R, 0, r}};
  hvAxes = Style[
    {If[hStyle === None, {}, Style[Line @ hAxis, hStyle]],
     If[vStyle === None, {}, Style[Line @ vAxis, vStyle]]},
    AbsoluteThickness[1], Dashed, $Gray
  ];
  endPoints = If[p === {}, {},
    {beg, end} = FirstLast @ p;
    If[EuclideanDistance[beg, end] < 1*^-4, {}, {
      plotStyle, makeClosedPoint3D @ beg, makeOpenPoint3D @ end
    }]
  ];
  bcOpts = Sequence[BandRadii -> bandRadii, BandTwists -> bandTwists];
  epilog = epilog /. {
    (head:$coordinateHeads)[points_] :> head[BandCurveProject[points, bcOpts]],
    Disk[pos_, args___] :> Sphere[BandCurveProject[pos, bcOpts], args],
    r_Rectangle :> Apply[Rectangle, BandCurveProject[Apply[List, r], bcOpts]]
  };
  Graphics3D[
    {band, tube, hvAxes, endPoints, {AbsolutePointSize[7], epilog}},
    Lighting -> "Neutral", Boxed -> False,
    ViewPoint -> {3, 0, 1.5}, ViewVertical -> {0, 0, 1},
    ImageSize -> imageSize, Method -> {"ShrinkWrap" -> True},
    ImagePadding -> 0, BoxRatios -> {R, R, r},
    ViewProjection -> "Orthographic"
  ]
]

$coordinateHeads = Point|Line|Arrow|Polygon;

(**************************************************************************************************)

PublicFunction[BandMeshRegion]

BandMeshRegion[args___] := Scope[
  {p1, p2} = BandBoundaryPoints[args];
  n = Length @ p1;
  n1 = Range @ n; n2 = n1 + n;
  p = Join[p1, p2];
  bound1 = MapWindowed[Line, n1];
  bound2 = MapWindowed[Line, n2];
  faces = ApplyWindowed[bandFacePoly, Trans[n1, n2]];
  MeshRegion[p, Flatten[{bound1, bound2, Polygon @ faces}], $bandMeshOptions]
];

$bandMeshOptions = Sequence[
  MeshCellStyle -> {
    {2,All}->Directive[{EdgeForm[None], FaceForm[White]}],
    {1,All}->None,
    {0,All}->None
  }
];

