PackageExport["ProjectionOnto"]

SetUsage @ "
ProjectionOnto can be applied to a graph via EdgeShapeFunction -> ProjectionOnto[shape] to \
indicate edges should be drawn projected onto shape.
"

(**************************************************************************************************)

PackageExport["Torus"]

SetUsage @ "
Torus[r$1, r$2] represents a torus on the x$, y$ plane with small radius r$1 and large radius r$2.
"

Torus /: BoundaryDiscretizeRegion[Torus[{R_, r_}]] := With[
  {largeDF = RegionDistance @ Circle[{0, 0}, N @ R], maxR = N[r + R], r2 = N[r]^2},
  iregion = ImplicitRegion[largeDF[{x, y}] + z^2 <= r2, {x, y, z}];
  iregion = iregion /. Abs[q_] :> Power[q, 2];
  BoundaryDiscretizeRegion[iregion,
    {{-maxR, maxR}, {-maxR, maxR}, {-r, r}},
    MaxCellMeasure -> (r/10)^2, PerformanceGoal -> "Quality"
  ]
];

(**************************************************************************************************)

PackageExport["TubeVector"]

TubeVector[r_][{x_, y_}] := {x, r * Sin[y], r * Cos[y]};

(**************************************************************************************************)

PackageExport["TorusVector"]

TorusVector[{R_, r_}, {phi_, theta_}] := Scope[
  d = R + r * Cos[theta];
  {d * Cos[phi], d * Sin[phi], r * Sin[theta]}
];

TorusVector[spec_][angles_] := TorusVector[spec, angles];

(**************************************************************************************************)

PackageExport["TorusAngles"]

TorusAngles[{R_, r_}, v_] := Scope[
  phi = ArcTan[Part[v, 1], Part[v, 2]];
  p = R * {Cos @ phi, Sin @ phi, 0};
  v2 = v - p;
  theta = ArcTan[Dot[v2, Normalize @ p], Last @ v];
  {phi, theta}
];

(**************************************************************************************************)

PackageExport["ArcTan2"]

ArcTan2[0.|0, 0|0.] := 0;
ArcTan2[x_, y_] := ArcTan[x, y];

(**************************************************************************************************)

PackageExport["BoundaryProjection"]

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
  TorusVector[spec] /@ Transpose[{phis, thetas}]
]

(**************************************************************************************************)

PackageExport["RegionPolygon"]

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

PackageExport["DiscretizeCurve"]

DiscretizeCurve[points_, f_:BezierCurve] := Scope[
  region = DiscretizeGraphics @ f @ ToPacked @ points;
  ToPacked @ Flatten[region["EdgeCoordinates"], 1]
];

(**************************************************************************************************)

PackageExport["PointDilationGraphics"]

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

PackageExport["ConvexHullLineIndices"]

ConvexHullLineIndices[points_] := If[Length[points] <= 3, Range @ Length @ points,
  WolframCGL`QuickHull @ ToPackedReal @ points
];

PackageExport["ConvexHullPointIndices"]

ConvexHullPointIndices[points_] := Union @ Flatten @ ConvexHullLineIndices @ points;

(**************************************************************************************************)

PackageExport["VectorBetween"]

VectorBetween[x_, {l_, h_}] := And @@ MapThread[LessEqual, {l, x, h}];
VectorBetween[{x_, y_}, {{xl_, xh_}, {yl_, yh_}}] := xl <= x <= xh && yl <= y <= yh;
VectorBetween[{x_, y_, z_}, {{xl_, xh_}, {yl_, yh_}, {zl_, zh_}}] := xl <= x <= xh && yl <= y <= yh && zl <= z <= zh;
VectorBetween[lh_][x_] := VectorBetween[x, lh];