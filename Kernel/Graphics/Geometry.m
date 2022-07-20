PublicFunction[ProjectionOnto]

SetUsage @ "
ProjectionOnto can be applied to a graph via EdgeShapeFunction -> ProjectionOnto[shape] to \
indicate edges should be drawn projected onto shape.
"

(**************************************************************************************************)

PublicHead[Torus]

SetUsage @ "
Torus[r$1, r$2] represents a torus on the x$, y$ plane with small radius r$1 and large radius r$2.
"

Torus /: BoundaryDiscretizeRegion[Torus[{R_, r_}], opts___Rule] := With[
  {largeDF = RegionDistance @ Circle[{0, 0}, N @ R], maxR = N[r + R], r2 = N[r]^2},
  iregion = ImplicitRegion[largeDF[{x, y}] + z^2 <= r2, {x, y, z}];
  iregion = iregion /. Abs[q_] :> Power[q, 2];
  BoundaryDiscretizeRegion[iregion,
    {{-maxR, maxR}, {-maxR, maxR}, {-r, r}},
    opts,
    MaxCellMeasure -> (r/10)^2, PerformanceGoal -> "Quality"
  ]
];

(**************************************************************************************************)

PublicFunction[TorusSurfacePolygon]

TorusSurfacePolygon[t:Torus[{_, r_}], opts___Rule] :=
  First @ RegionPolygon @ BoundaryDiscretizeRegion[
    t, opts,
    MaxCellMeasure -> (r / Tau)/2,
    PerformanceGoal -> "Speed"
  ]

(**************************************************************************************************)

PublicFunction[TubeVector]

TubeVector[r_][{x_, y_}] := {x, r * Sin[y], r * Cos[y]};

(**************************************************************************************************)

PublicFunction[TorusCenterVector]

TorusCenterVector[R_ | {R_, _}, theta_] :=
  tMatrix @ Thread @ {d * Cos[phi], d * Sin[phi], 0};

(**************************************************************************************************)

PublicFunction[TorusVector]

TorusVector[{R_, r_}, {phi_, theta_}] := Scope[
  d = R + r * Cos[theta];
  tMatrix @ {d * Cos[phi], d * Sin[phi], r * Sin[theta]}
];

TorusVector[spec_][angles_] := TorusVector[spec, angles];

tMatrix[m_ ? MatrixQ] := Transpose @ m;
tMatrix[v_] := v;

(**************************************************************************************************)

PublicFunction[TorusAngles]

TorusAngles[{R_, r_}, v_] := Scope[
  phi = ArcTan[Part[v, 1], Part[v, 2]];
  p = R * {Cos @ phi, Sin @ phi, 0};
  v2 = v - p;
  theta = ArcTan[Dot[v2, Normalize @ p], Last @ v];
  {phi, theta}
];

(**************************************************************************************************)

PublicFunction[ArcTan2]

ArcTan2[0.|0, 0|0.] := 0;
ArcTan2[x_, y_] := ArcTan[x, y];

(**************************************************************************************************)

PublicFunction[BoundaryProjection]

BoundaryProjection[shape_] := Quiet @ Check[
  RegionNearest @ RegionBoundary @ BoundaryDiscretizeRegion[
    shape, MaxCellMeasure -> 0.1, PerformanceGoal -> "Quality"
  ],
  $Failed
];

BoundaryProjection[Torus[spec_]] := TorusNearestFunction[spec];

TorusNearestFunction[spec_][vec:{_Real, _Real, _Real}] :=
  TorusVector[spec, TorusAngles[spec, vec]]

(tnf:TorusNearestFunction[_])[vec_List] :=
  tnf /@ vec;

TorusNearestFunction[spec_][{v1_List, v2_List}] :=
  TorusInterpolated[spec, v1, v2];

TorusInterpolated[spec_, v1_, v2_] := Scope[
  {phi1, theta1} = TorusAngles[spec, v1];
  {phi2, theta2} = TorusAngles[spec, v2];
  thetas = AngleRange[theta1, theta2, Into @ 8];
  phis = AngleRange[phi1, phi2, Into @ 8];
  TorusVector[spec] /@ Trans[phis, thetas]
]

(**************************************************************************************************)

PublicFunction[RegionPolygon]

RegionPolygon[region_] :=
  regionComponentPolygon /@ ConnectedMeshComponents[region];

regionComponentPolygon[region_] := Scope[
  polygons = region["BoundaryPolygons"];
  If[Length[polygons] === 1, Return @ First @ polygons];
  outerIndex = MinimumIndexBy[polygons, -Area[#]&];
  outer = Part[polygons, outerIndex];
  holes = Delete[polygons, outerIndex];
  Polygon[Part[outer, 1] -> Part[holes, All, 1]]
];

(**************************************************************************************************)

PublicFunction[DiscretizeCurve]

DiscretizeCurve[points_, f_:BezierCurve] := Scope[
  region = DiscretizeGraphics @ f @ ToPacked @ points;
  ToPacked @ Catenate @ region["EdgeCoordinates"]
];

(**************************************************************************************************)

PublicFunction[PointDilationGraphics]

PointDilationGraphics[points_, r_] := Scope[
  points = ToPacked[points];
  If[Length[points] < 1500 && minimumSquaredDistance[points] > (2r)^2,
    GraphicsComplex[points, Disk[#, r]& /@ Range[Length @ points]],
    RegionPolygon @ pointDilationRegion[points, r]
  ]
];

minimumSquaredDistance[points_] :=
  Min @ Clip[
    SquaredDistanceMatrix @ points,
    {$MachineEpsilon, $MaxMachineNumber}, {$MaxMachineNumber, $MaxMachineNumber}
  ];

pointDilationRegion[points_, d_] := Scope[
  bounds = CoordinateBounds[points, 2 * d];
  is3D = (Length @ First @ points) === 3;
  Check[
    mesh = MeshRegion @ Point @ points;
    rd = RegionDistance[mesh];
    ir = If[is3D,
      ImplicitRegion[rd[{x, y, z}] <= d, {x, y, z}],
      ImplicitRegion[rd[{x, y}] <= d, {x, y}]
    ];
    quality = Length[points] < 1000;
    BoundaryDiscretizeRegion[ir, bounds,
      MaxCellMeasure -> If[quality, d/4, d],
      PerformanceGoal -> If[quality, "Quality", "Speed"]
    ]
  ,
    RegionUnion[Region[Disk[#, d]]& /@ points]
  ]
];

(**************************************************************************************************)

PublicFunction[BoundingBoxPointIndices]

BoundingBoxPointIndices[points_, dscale_:0.01] := Scope[
  bbox = CoordinateBoundingBox @ points;
  diagonal = EuclideanDistance @@ bbox;
  coords = Transpose @ points;
  distances = If[Length[coords] === 2,
    {x, y} = coords;
    {{xl, yl}, {xh, yh}} = bbox;
    MapThread[Min, {Abs[x - xl], Abs[y - yl], Abs[x - xh], Abs[y - yh]}]
  ,
    {x, y, z} = coords;
    {{xl, yl, zl}, {xh, yh, zh}} = bbox;
    MapThread[Min, {Abs[x - xl], Abs[y - yl], Abs[z - zl], Abs[x - xh], Abs[y - yh], Abs[z - zh]}]
  ];
  SelectIndices[distances, LessEqualThan[diagonal * dscale]]
];

(**************************************************************************************************)

PublicFunction[ConvexHullLineIndices]

ConvexHullLineIndices[points_] := Scope[
  If[Length[points] <= 3,
    Range @ Length @ points
  ,
    res = Check[WolframCGL`QuickHull @ ToPackedReal @ points, $Failed];
    If[!MatrixQ[res], res = $Failed];
    res
  ]
];

PublicFunction[ConvexHullPointIndices]

ConvexHullPointIndices[points_] := Scope[
  indices = ConvexHullLineIndices @ points;
  Union @ Flatten @ OnFailed[indices, BoundingBoxPointIndices[points, 1*^-4]]
]

(**************************************************************************************************)

PublicFunction[VectorBetween]

VectorBetween[x_, {l_, h_}] := And @@ ThreadLessEqual[l, x, h];
VectorBetween[{x_, y_}, {{xl_, xh_}, {yl_, yh_}}] := xl <= x <= xh && yl <= y <= yh;
VectorBetween[{x_, y_, z_}, {{xl_, xh_}, {yl_, yh_}, {zl_, zh_}}] := xl <= x <= xh && yl <= y <= yh && zl <= z <= zh;
VectorBetween[lh_][x_] := VectorBetween[x, lh];

(**************************************************************************************************)

PublicFunction[FindRigidTransform]

FindRigidTransform[{a1_, a1_}, {b1_, b1_}] := TranslationTransform[b1 - a1];
FindRigidTransform[{a1_, a2_}, {a1_, a2_}] := Identity;

FindRigidTransform[{a1_, a2_}, {b1_, b2_}] := Scope[
  da = a2 - a1; db = b2 - b1;
  RightComposition[
    TranslationTransform[-a1],
    RotationTransform[{da, db}],
    ScalingTransform[N[Norm[db] / Norm[da]] * {1, 1}],
    TranslationTransform[b1]
  ]
]

(**************************************************************************************************)

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

