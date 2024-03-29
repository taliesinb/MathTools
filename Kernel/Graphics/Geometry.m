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
  RegionPolygon @ BoundaryDiscretizeRegion[
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
  phi = ArcTan[P1[v], P2[v]];
  p = R * {Cos @ phi, Sin @ phi, 0};
  v2 = v - p;
  theta = ArcTan[Dot[v2, Normalize @ p], L @ v];
  {phi, theta}
];

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

SetUsage @ "
RegionPolygon[%BoundaryMeshRegion[$$]] returns a %Polygon[$$], or list of these, corresponding to the boundary mesh region.
RegionPolygon[primitive$] returns a %Polygon[$$] corresponding to a complex %Graphic or %Graphics3D primitive.
"

RegionPolygon::notregion = "First arg with head `` should be a MeshRegion or BoundaryMeshRegion, or an object (like Cube[]) that can be converted to one."

RegionPolygon[p_Polygon] := p;

RegionPolygon[t:Tube[_, r_]] := RegionPolygon @ BoundaryDiscretizeGraphics[t, MaxCellMeasure -> Max[r^3]/64.];

RegionPolygon[other_] := Scope[
  res = Quiet @ BoundaryDiscretizeRegion @ other;
  If[MatchQ[res, _MeshRegion | _BoundaryMeshRegion],
    RegionPolygon @ res,
    ReturnFailed["notregion", H @ other]
  ]
];

RegionPolygon[region_MeshRegion | region_BoundaryMeshRegion] :=
  Rep[
    regionComponentPolygon /@ ConnectedMeshComponents[region],
    {a_} :> a
  ];

regionComponentPolygon[region_] := Scope[
  polygons = region["BoundaryPolygons"];
  If[Len[polygons] === 1, Return @ F @ polygons];
  outerIndex = MinimumIndexBy[polygons, -Area[#]&];
  outer = Part[polygons, outerIndex];
  holes = Delete[polygons, outerIndex];
  Polygon[F[outer] -> Col1[holes]]
];

(**************************************************************************************************)

(*

TODO: exploit the following functions:

System`Dump`BoundaryCurve can turn a StadiumShape, Annulus, DiskSegment into BSplineCurves

System`Dump`BoundarySurface does same for CapsuleShape, SphericalShell, FilledTorus, Torus

System`Dump`ProcessText handles Text[...]

System`Dump`ProcessInset handles Inset[...]

System`Dump`ProcessGraphicsComplex
*)


PublicFunction[DiscretizeCurve]

SetUsage @ "
DiscretizeCurve[points$] gives a discretized path for a %BezierCurve through those points.
DiscretizeCurve[points$, f$] gives a discretized path for f$[points$].
DiscretizeCurve[object$] supports the following existing graphics primitives:
| %Line[$$] | returns the line unchanged |
| %Circle[$$] | samples the circle as a 32-gon, or between the provided angle endpoints |
| %BezierCurve[$$] | samples the curve using %DiscretizeGraphics |
| %BSplineCurve[$$] | as above |
* Custom path primitives like %ElbowCurve, %RollingCurve, etc. are also supported.
"

(* TODO: remove this usage, i don't use it anymore? *)
DiscretizeCurve[points_List, f_:BezierCurve] := DiscretizeCurve[f[points]];

DiscretizeCurve[Line[points_]] := ToPackedReal @ points;

DiscretizeCurve[Circle[center:$Coord2P, radius_ ? NumericQ, {t1_ ? NumericQ, t2_ ? NumericQ}]] :=
  ToPackedReal @ CircleVector[center, radius, angRange[t1, t2]];

angRange[t_, t_] := {t};
angRange[t1_, t2_] := Dedup @ App[t2] @ Range[t1, t2, (t2 - t1) / Ceiling[32 - Min[24, 16/Abs[t2-t1]]]];

$tau32 = N @ App[0] @ Range[0, Tau - 0.01, Tau / 48];
DiscretizeCurve[Circle[center:$Coord2P, radius_ ? NumericQ]] :=
  ToPackedReal @ CircleVector[center, radius, $tau32];

DiscretizeCurve[curve:(BezierCurve|BSplineCurve)[line:{_, _}]] :=
  ToPackedReal @ line;

(* TODO: optimize this by calling Graphics`Mesh`DiscretizeGraphicsPrimitive directly *)
DiscretizeCurve[curve:(BezierCurve|BSplineCurve)[___]] := Scope[
  region = DiscretizeGraphics @ MapAt[ToPacked, curve, 1];
  ToPackedReal @ Catenate @ region["EdgeCoordinates"]
];

DiscretizeCurve[BezierCurve[points_List]] :=
  DiscretizeCurve @ BezierCurve[points, SplineDegree -> 3];

DiscretizeCurve[BezierCurve[points_List, SplineDegree -> n_Int]] := Rep[
  GeometricFunctions`BezierCurve[ToPackedReal @ points, SplineDegree -> n],
  {GraphicsComplex[coords_List, Line[indices_List]] :> ToPackedReal @ Part[coords, indices], _ :> $Failed}
];

DiscretizeCurve::badcurve = "Cannot discretize unrecognized curve ``. Returning a dummy path.";

DiscretizeCurve[e_] := CurveToPoints[e];

(**************************************************************************************************)

PublicFunction[PointDilationGraphics]

PointDilationGraphics[points_, r_] := Scope[
  points = ToPacked[points];
  If[Len[points] < 1500 && minimumSquaredDistance[points] > (2r)^2,
    GraphicsComplex[points, Array[Disk[#, r]&, Len @ points]],
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
  is3D = (Len @ F @ points) === 3;
  Check[
    mesh = MeshRegion @ Point @ points;
    rd = RegionDistance[mesh];
    ir = If[is3D,
      ImplicitRegion[rd[{x, y, z}] <= d, {x, y, z}],
      ImplicitRegion[rd[{x, y}] <= d, {x, y}]
    ];
    quality = Len[points] < 1000;
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
  diagonal = Dist @@ bbox;
  coords = Transpose @ points;
  distances = If[Len[coords] === 2,
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
  If[Len[points] <= 3,
    Range @ Len @ points
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

PublicFunction[FindRigidTransform]

FindRigidTransform[{a1_, a1_}, {b1_, b1_}] := TranslationTransform[b1 - a1];
FindRigidTransform[{a1_, a2_}, {a1_, a2_}] := Id;

FindRigidTransform[{a1_, a2_}, {b1_, b2_}] := Scope[
  da = a2 - a1; db = b2 - b1;
  RightComposition[
    TranslationTransform[-a1],
    RotationTransform[{da, db}],
    ScalingTransform[N[Norm[db] / Norm[da]] * {1, 1}],
    TranslationTransform[b1]
  ]
]