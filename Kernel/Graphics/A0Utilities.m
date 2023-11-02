PublicFunction[CircleVector]

SetUsage @ "
CircleVector[pos$, r$, theta$] gives the point starting at pos$ and moving r$ in direction theta$ in radians.
CircleVector[pos$, r$, {theta$1, theta$, $$}] gives a list of points.
"

CircleVector[pos_, r_, theta_] := pos + r * {Cos[theta], Sin[theta]};
CircleVector[pos_, r_, theta_List] := Threaded[pos] + r * CosSin[theta];

(**************************************************************************************************)

PublicFunction[CosSin]

SetUsage @ "
CosSin[theta$] gives {%Cos[theta$], %Sin[theta$]}
CosSin[{theta$1, theta$2, $$}] gives a list of pairs.
";

SetListable[CosSin];
CosSin[theta_] := {Cos[theta], Sin[theta]};

(**************************************************************************************************)

PublicFunction[ClockwiseCirclePoints, AnticlockwiseCirclePoints]

SetUsage @ "
ClockwiseCirclePoints[n$] gives n$ equally spaced clockwise points with the first point at {0, 1}.
ClockwiseCirclePoints[n$, side$] starts at a symbolic side like Top, Left, etc.
"

SetUsage @ "
AnticlockwiseCirclePoints[n$] gives n$ equally spaced anti-clockwise points with the first point at {0, 1}.
AnticlockwiseCirclePoints[n$, side$] starts at a symbolic side like Top, Left, etc.
"

ClockwiseCirclePoints[n_, side_:Top] := AngleVector /@ ($SideToRadians[side] - Range[0, Tau-1/n, Tau/n]);
AnticlockwiseCirclePoints[n_, side_:Top] := AngleVector /@ ($SideToRadians[side] + Range[0, Tau-1/n, Tau/n]);

(**************************************************************************************************)

PublicFunction[VectorReflect]

SetUsage @ "
VectorReflect[v$, rv$] reflects the vector v$ in the hyperplane perpendicular to rv$.
"

VectorReflect[v_, rv_] := Expand[v - (2 * Dot[rv, v] / Dot[rv, rv]) * rv];
VectorReflect[v_, rv_ ? ValidQ] := Expand[v - (2 * Dot[rv, v]) * rv]; (* TODO: remove this, it is used in RootSystem somehow *)
VectorReflect[rv_][v_] := VectorReflect[v, rv];

(**************************************************************************************************)

PublicFunction[VectorReflectHorizontal, VectorReflectVertical, VectorTranspose]

SetUsage @ "
VectorReflectHorizontal[v$] reflects the vector v$ horizontally$.
"

VectorReflectHorizontal[v_] := Threaded[{-1, 1}] * v;

SetUsage @ "
VectorReflectVertical[v$] reflects the vector v$ vertically.
"

VectorReflectVertical[v_] := Threaded[{1, -1}] * v;

SetUsage @ "
VectorTranspose[v$] switches the role of x$ and y$.
"

VectorTranspose[v_?MatrixQ] := Reverse[v, 2];
VectorTranspose[v_List] := Reverse[v];

(**************************************************************************************************)

PublicFunction[VectorRotate45, VectorRotate45CW, VectorRotate60, VectorRotate60CW, VectorRotate90, VectorRotate90CW, VectorRotate120, VectorRotate120CW, VectorRotate180]

$rotUsage = StringFunction @ "
#1[vec$] rotates a vector by #2 degrees #3.
#1[array$] threads over an array of vectors.
";

setupRotFunc[sym_, angle_, cw_] := With[
  {matrix = RotationMatrix @ (If[cw, angle, -angle] * Degree)},
  {nmatrix = ToPackedReal @ N @ matrix},
  SetUsage[sym, $rotUsage[SymbolName[sym], angle, If[cw, "clockwise", "counterclockwise"]]];
  sym[vec_List]                                    := Dot[vec, matrix];
  sym[vec_List] /; ArrayQ[vec, _, RealQ] := Dot[ToPackedArray[vec, Real], nmatrix];
];

setupRotFunc @@@ {
  {VectorRotate45,  45,  False}, {VectorRotate45CW,   45, True},
  {VectorRotate60,  60,  False}, {VectorRotate60CW,   60, True},
  {VectorRotate90,  90,  False}, {VectorRotate90CW,   90, True},
  {VectorRotate120, 120, False}, {VectorRotate120CW, 120, True},
  {VectorRotate180, 180, False}
};

(**************************************************************************************************)

PublicFunction[VectorReject]

SetUsage @ "
VectorReject[u$, v$] gives the component of u$ that is orthogonal to v$.
VectorReject[{u$1, u$2, $$}, v$] gives the list of rejections u$i onto v$.
VectorReject[{u$1, u$2, $$}, {v$1, v$2, $$}] gives the list of rejections u$i onto v$i.
"

VectorReject[u_ ? MatrixQ, v_ ? MatrixQ] := MapThread[VectorReject, {u, v}];
VectorReject[u_ ? MatrixQ, v_ ? VectorQ] := Map[VectorReject[u, #]&, v];
VectorReject[u_, v_] := u - Projection[u, v];

(**************************************************************************************************)

PublicFunction[VectorProject]

SetUsage @ "
VectorProject[u$, v$] gives the vector projection of the vector u$ onto the vector v$.
VectorProject[{u$1, u$2, $$}, v$] gives the list of projections u$i onto v$.
VectorProject[{u$1, u$2, $$}, {v$1, v$2, $$}] gives the list of projection u$i onto v$i.
"

VectorProject[u_ ? MatrixQ, v_ ? MatrixQ] := MapThread[VectorProject, {u, v}];
VectorProject[u_ ? MatrixQ, v_ ? VectorQ] := Map[VectorProject[u, #]&, v];
VectorProject[u_, v_] := Projection[u, v];

(**************************************************************************************************)

PublicFunction[LineLineIntersectionPoint]

SetUsage @ "
LineLineIntersectionPoint[line$1, line$2] gives the point where two line segments cross.
* If they do not cross exactly but cross approximately to within 1/10 their minimum length, the mean point of their closest approach will be given.
"

LineLineIntersectionPoint[l1_, l2_] := Scope[
  r = BooleanRegion[And, Line /@ {l1, l2}];
  Switch[r,
    Point[$CoordP],
      Part[r, 1],
    Point[{$CoordP, ___}],
      Part[r, 1, 1],
    Line[_],
      Part[r, 1, 1],
    _,
      p = First @ l2;
      Do[p = ClosestPointOnLine[l2, ClosestPointOnLine[l1, p]], 10];
      d = DistanceToLine[l1, p];
      scale = Min[EuclideanDistance @@ l1, EuclideanDistance @@ l2];
      If[d <= scale / 10,
        Avg[p, ClosestPointOnLine[l1, p]],
        $Failed
      ]
  ]
]

(**************************************************************************************************)

PublicFunction[InfiniteLineLineIntersectionPoint]

SetUsage @ "
InfiniteLineLineIntersectionPoint[{a$1, a$2}, {b$1, b$2}] gives the point where two line segments cross.
* the lines are the taken to be infinite lines passing through {a$1, a$2}, etc.
* if the lines are exactly parallel, None is returned.
* if the intersection point is much further away then the length of a$, None is returned.
"

InfiniteLineLineIntersectionPoint[{a_, b_}, {c_, d_}] := Scope @ Quiet[
  det = Det[Chop @ {a - b, c - d}];
  If[Abs[det] < 0.0001 * Dist[a, b], None,
    (Det[Chop @ {a, b}] * (c - d) - Det[Chop @ {c, d}] * (a - b)) / det
  ]
];

(**************************************************************************************************)

PublicFunction[LineCircleIntersectionPoint]

SetUsage @ "
LineCircleIntersectionPoint[line$1, {p$, r$}] gives the point where %Line[line$] crosses %Circle[p$, r$].
"

LineCircleIntersectionPoint[l1_, {p_, r_}] := Scope[
  r = BooleanRegion[And, {Line @ l1, Circle[p, r]}];
  Switch[r,
    Point[$CoordP],
      Part[r, 1],
    Point[{$CoordP, ___}],
      Part[r, 1, 1],
    Line[_],
      Part[r, 1, 1],
    _,
      $Failed
  ]
]

(**************************************************************************************************)

PublicFunction[InfiniteLineCircleIntersectionPoint]

SetUsage @ "
InfiniteLineCircleIntersectionPoint[{a$, b$}, {p$, r$}] gives the point where %InfiniteLine[{a$, b$}] \
crosses %Circle[p$, r$].
"

InfiniteLineCircleIntersectionPoint[l1_, {p_, r_}] := Scope[
  r = BooleanRegion[And, {InfiniteLine @ l1, Circle[p, r]}];
  Switch[r,
    Point[$CoordP],
      Part[r, 1],
    Point[{$CoordP, ___}],
      Part[r, 1, 1],
    Line[_],
      Part[r, 1, 1],
    _,
      $Failed
  ]
]

(**************************************************************************************************)

PublicFunction[LineRectangleIntersectionPoint]

SetUsage @ "
LineRectangleIntersectionPoint[line$1, rectangle$] gives the point where a line and a rectangle cross.
* The rectangle should be specified as {{x$min, y$min}, {x$max, y$max}}.
"

LineRectangleIntersectionPoint[l1_List, {{x1_, y1_}, {x2_, y2_}}] := Scope[
  points = Map[
    BooleanRegion[And, {Line @ N @ l1, Line @ N @ #1}]&,
    {{{x1, y1}, {x1, y2}},
     {{x1, y2}, {x2, y2}},
     {{x2, y2}, {x2, y1}},
     {{x2, y1}, {x1, y1}}}
  ];
  points = DeepCases[points, $Coord2P];
  p1 = First @ l1;
  If[points === {}, p1,
    MinimumBy[points, EuclideanDistance[#, p1]&]]
]

(**************************************************************************************************)

PublicFunction[ShrinkPolygon]

SetUsage @ "
ShrinkPolygon[points$, d$] shrinks a convex polygon towards its center, moving each segment by distance d$ orthogonal to itself.
* No line will be moved beyond the center of the polygon.
* Currently a polygon wil not be reduced in arity if it is shrunk so much that a side becomes negative length.
"

(* TODO: fix the negative side length issue *)

ShrinkPolygon[{a_, b_, c_}, d_] := Scope[
  {da, db, dc} = {1, 1, 1} * d;
  ab = displaceLineTowardsLimited[{a, b}, c, da];
  bc = displaceLineTowardsLimited[{b, c}, a, db];
  ac = displaceLineTowardsLimited[{a, c}, b, dc];
  a2 = LineLineIntersectionPoint[ab, ac]; If[FailureQ[a2], a2 = PointTowards[a, Avg[c, b], da]];
  b2 = LineLineIntersectionPoint[ab, bc]; If[FailureQ[b2], b2 = PointTowards[b, Avg[a, c], db]];
  c2 = LineLineIntersectionPoint[ac, bc]; If[FailureQ[c2], c2 = PointTowards[c, Avg[a, b], dc]];
  {a2, b2, c2}
]

displaceLineTowardsLimited[line_, point_, dist_] :=
  DisplaceLineTowards[line, point, Min[dist, DistanceToLine[line, point] * 0.45]];

ShrinkPolygon[points_, dist_] := Scope[
  center = Mean[points];
  lines = MapWindowedCyclic[displaceLineTowardsLimited[#, center, dist]&, points];
  newPoints = ApplyWindowedCyclic[LineLineIntersectionPoint, lines];
  MapThread[
    If[CoordinateVectorQ[#1], #1, PointTowards[#2, center, dist]]&,
    {newPoints, points}
  ]
]

(**************************************************************************************************)

PublicFunction[ArcBetween]

SetUsage @ "
ArcBetween[c$, {p$1, p$2}] returns a circular arc centered on c$ that begins at p$1 and ends at p$2.
ArcBetween[c$, {p$1, p$2}, towards$] ensures the arc takes the way around the circle that faces towards$.
* In 2D, a %Circle is returned, in 3D, a Line is returned by discretizing the circle.
"

ArcBetween[center_, {p1_ ? CoordinateVector2DQ, p2_ ? CoordinateVector2DQ}, towards_:Automatic] := Scope[
  If[p1 == center || p2 == center || p1 == p2, Return @ Line @ DeleteDuplicates @ {p1, p2}];
  r = Mean[EuclideanDistance[center, #]& /@ {p1, p2}];
  SetAutomatic[towards, Avg[p1, p2]];
  d1 = p1 - center; d2 = p2 - center; d3 = towards - center;
  theta1 = ArcTan @@ d1; theta2 = ArcTan @@ d2; theta3 = ArcTan @@ d3;
  If[theta2 < theta1, theta2 += Tau];
  If[theta3 < theta1, theta3 += Tau];
  If[theta3 > theta2, theta2 -= Tau];
  Circle[center, r, {theta1, theta2}]
];

(* TODO: supprot towards by lerping *outside* the line and simulating a point at infinity *)
ArcBetween[center_, {p1_ ? CoordinateVector3DQ, p2_ ? CoordinateVector3DQ}, towards_:Automatic] := Scope[
  r = Mean[EuclideanDistance[center, #]& /@ {p1, p2}];
  Line[center + Normalize[# - center] * r& /@ Lerp[p1, p2, Into[24]]]
]

(**************************************************************************************************)

PublicFunction[SetLengthTo]

SetUsage @ "
SetLengthTo[vec$, d$] rescales a vector have length d$.
SetLengthTo[{vec$1, vec$2, $$}, d$] rescales a list of vectors to each have length d$.
SetLengthTo[d$] is the operator form of SetLengthTo.
"

SetLengthTo::notnumeric = "First arg `` was not a numeric vector or matrix."
SetLengthTo[x_ ? NumericVectorQ, r_] := Normalize[N @ x] * r;
SetLengthTo[x_ ? NumericMatrixQ, r_] := ToPackedReal[Normalize[#] * r& /@ N[x]];
SetLengthTo[x_, r_] := (Message[SetLengthTo::notnumeric, MsgExpr @ x]; $Failed);
SetLengthTo[r_][x_] := SetLengthTo[x, r];

(**************************************************************************************************)

PublicFunction[ClosestPointOnLine]

SetUsage @ "
ClosestPointOnLine[path$, p$] returns the point on line path$ closest to point p$.
"

ClosestPointOnLine[line_, p_] := RegionNearest[Line @ line, p];

(**************************************************************************************************)

PublicFunction[ClosestPointOnInfiniteLine]

SetUsage @ "
ClosestPointOnInfiniteLine[{a$, b$}, p$] returns the point on line passing through a$, b$ that is closest to p$.
"

ClosestPointOnInfiniteLine[{a_, b_}, p_] := p + VectorReject[b - p, a - b];

(**************************************************************************************************)

PublicFunction[DistanceToLine]

SetUsage @ "
DistanceToLine[path$, p$] returns the shortest distance from point p$ to line path$.
"

DistanceToLine[{a_, b_}, p_] :=
  Max[
    Norm @ VectorReject[p - b, a - b],
    Min[Dist[p, a], Dist[p, b]]
  ];

DistanceToLine[line_, p_] := RegionDistance[Line @ line, p];

(**************************************************************************************************)

PublicFunction[DistanceToInfiniteLine]

SetUsage @ "
DistanceToInfiniteLine[{a$, b$}, p$] returns the distance from line passing through a$, b$ to point p$.
"

DistanceToInfiniteLine[{a_, b_}, p_] := Norm @ VectorReject[b - p, a - b];

(**************************************************************************************************)

PublicFunction[DisplaceLineTowards]

SetUsage @ "
DisplaceLineTowards[{p$, $q}, t$, d$] shifts the line {p$, q$} orthogonally by distance d$ towards the point t$.
"

DisplaceLineTowards[line:{a_, b_}, point_, dist_] := Scope[
  delta = Normalize[VectorReject[b - point, a - b]] * dist;
  {a - delta, b - delta}
]

(**************************************************************************************************)

PublicFunction[PointTowards]

SetUsage @ "
PointTowards[p$, q$, d$] returns the point at distance d$ from p$ to q$.
"

PointTowards[p_, q_, d_ ? NumericQ] :=
  If[EuclideanDistance[p, q] >= d, q, a + Normalize[b - a] * d];

(**************************************************************************************************)

PublicFunction[PointAlongLine]

SetUsage @ "
PointAlongLine[path$, d$] returns the point at distance d$ along line path$.
PointAlongLine[path$, Scaled[f$]] takes the fraction f$ along the path.
PointAlongLine[d$] is the operator form of PointAlongLine.
"

PointAlongLine[{a_, b_}, d_ ? NumericQ] :=
  a + Normalize[b - a] * d;

PointAlongLine[coords_, Scaled[d_]] :=
  PointAlongLine[coords, LineLength[coords] * d];

PointAlongLine[coords_List, d_ ? NumericQ] :=
  First @ vectorAlongLine[coords, d];

PointAlongLine[d_][coords_] := PointAlongLine[coords, d];

(**************************************************************************************************)

PublicFunction[SampleLineEvery]

SetUsage @ "
SampleLineEvery[path$, d$] returns a list of points sampled every distance d$.
* The initial and endpoint are always sampled.
* The sample distance can be smaller, but no smaller than necessary to sample evenly."

toEveryD[total_, d_] := Into @ Ceiling[total / d];

SampleLineEvery[{a_, b_}, d_] := Lerp[a, b, toEveryD[EuclideanDistance[a, b], d]];

(* TODO: speed up this step! *)
SampleLineEvery[path_List, d_] := PointAlongLine[path, Scaled @ #]& /@ N[Lerp[0, 1, toEveryD[LineLength @ path, d]]];

(**************************************************************************************************)

PublicFunction[VectorAlongLine]

SetUsage @ "
VectorAlongLine[path$, d$] returns the pair {pos$, dir$} for the point distance d$ along line path$.
VectorAlongLine[path$, Scaled[f$]] takes the fraction f$ along the path.
* If f$ is less than 0 or greater than 1 the point is extrapolated from the tangent at the end of the path.
* Paths of length 2 can have Offset endpoints and these will be correctly handled, though dir$ will ignore the offset.
"

VectorAlongLine[p:{_, _}, d_ ? NumericQ] := vectorAlongSegment[p, d];

VectorAlongLine[coords_, Scaled[1|1.]] := getPointAndVec[coords, Length @ coords];
VectorAlongLine[coords_, Scaled[0|0.]|0|0.] := getPointAndVec[coords, 1];

VectorAlongLine[{a:$CoordP, b:$CoordP}, Scaled[d_]] :=
  {Lerp[a, b, d], N @ Normalize[b - a]};

VectorAlongLine[{a_, b_} ? ContainsOffsetsQ, Scaled[d_]] := Scope[
  {ar, aa} = FromOffsetCoord @ a;
  {br, ba} = FromOffsetCoord @ b;
  ca = Lerp[aa, ba, d];
  cr = Lerp[ar, br, d];
  {Offset[ca, cr], coordNormDelta[a, b]}
];

VectorAlongLine[coords_, Scaled[d_]] := VectorAlongLine[coords, LineLength[coords] * d];

(* TODO: this doesn't take the length of the path into account *)
VectorAlongLine[coords_, Scaled[d_] /; d > 1] := Scope[
  {pos, dir} = getPointAndVec[coords, Length @ coords];
  pos = coordPlus[pos, dir * (d - 1) * LineLength[coords]];
  {pos, dir} // SimplifyOffsets
];

VectorAlongLine[coords_, Scaled[d_] /; d < 0] := Scope[
  {pos, dir} = getPointAndVec[coords, 1];
  pos = coordPlus[pos, dir * d * LineLength[coords]];
  {pos, dir} // SimplifyOffsets
];

VectorAlongLine[coords_List, d_ ? NumericQ] := vectorAlongLine[coords, d];

(**************************************************************************************************)

vectorAlongSegment[{a_, b_}, d_] := Scope[
  delta = coordNormDelta[a, b];
  {a + delta * d, delta}
];

vectorAlongLine[coords_, d_] := Scope[
  prev = First @ coords; total = 0;
  n = LengthWhile[coords, curr |-> (total += coordDistance[curr, prev]; prev = curr; total < d)];
  If[n == Length[coords], Return @ getPointAndVec[coords, n]];
  rem = total - d;
  If[rem == 0,
    getPointAndVec[coords, n + 1],
    {1, -1} * vectorAlongSegment[Part[coords, {n + 1, n}], rem]
  ]
];

getPointAndVec[{coord_}, 1] := {coord, {0, 0}};

getPointAndVec[coords_, 1] := Scope[
  {p1, p2} = Part[coords, {1, 2}];
  {p1, coordNormDelta[p1, p2]}
]

getPointAndVec[coords_, n_] /; n == Length[coords] := Scope[
  {p0, p1} = Part[coords, {-2, -1}];
  {p1, coordNormDelta[p0, p1]}
]

getPointAndVec[coords_, i_] := Scope[
  {p0, p1, p2} = Part[coords, i + {-1, 0, 1}];
  {p1, N @ Normalize @ Avg[p1 - p0, p2 - p1]}
]

(**************************************************************************************************)

PublicFunction[VectorListAlongLine]

VectorListAlongLine[{}] := {};

VectorListAlongLine[{a_}] := {{a, a * 0}};

VectorListAlongLine[{a_, b_}] := Scope[
  d = Normalize[b - a];
  {{a, d}, {Avg[a, b], d}, {b, d}}
];

VectorListAlongLine[coords_] := Scope[
  diffs = Normalize /@ Differences[coords];
  n = Length[coords];
  is = Range[1, n, 1/2]; Part[is, -1] = -1; i1 = i2 = 0;
  Map[toVecListElem, is]
,
  toVecListElem[1] := {First @ coords, First @ diffs},
  toVecListElem[-1] := {Last @ coords, Last @ diffs},
  toVecListElem[i_Integer] := {Part[coords, i], Normalize @ Mean @ Part[diffs, {i-1, i}]},
  toVecListElem[i_Rational] := ({i1, i2} = FloorCeiling @ i; {Mean @ Part[coords, {i1, i2}], Part[diffs, i1]})
];

(**************************************************************************************************)

PublicFunction[LineLength]

SetUsage @ "
LineLength[path$] returns the total length of a line.
"

LineLength[{}] := 0;
LineLength[{a_, b_}] := EuclideanDistance @@ RemoveOffsets[{a, b}];
LineLength[list_] := Total @ ApplyWindowed[EuclideanDistance, RemoveOffsets @ list];

(**************************************************************************************************)

PublicFunction[EdgeLengthScale]

SetUsage @ "
EdgeLengthScale[points$, q$] returns the characterstic scale length of a list of paths, using quartile q$.
* Various heuristics are used to deal with self-loops, etc.
"

boundingBoxSideLength[line_] :=
  Total[EuclideanDistance @@@ CoordinateBounds @ line];

adjustedLineLength[line_] :=
  If[First[line] === Last[line], 0.8, 1] * Min[LineLength @ line, boundingBoxSideLength @ line];

EdgeLengthScale[{}, q_] := 1.0;

EdgeLengthScale[edgeCoordinateLists_, q_] := Scope[
  edgeLengths = Chop @ Map[adjustedLineLength, edgeCoordinateLists];
  edgeLengths = DeleteCases[edgeLengths, 0|0.];
  If[edgeLengths === {},
    edgeLengths = Map[boundingBoxSideLength, edgeCoordinateLists]];
  If[q === "Average", Mean @ edgeLengths, Quantile[edgeLengths, q]]
];

(**************************************************************************************************)

PublicFunction[ArcTan2]

SetUsage @ "
ArcTan2[dx$, dy$] acts like %ArcTan but deals with the zero vector by returning an angle of 0.
"

ArcTan2[0.|0, 0|0.] := 0;
ArcTan2[x_, y_] := ArcTan[x, y];

(**************************************************************************************************)

PublicFunction[VectorBetween]

SetUsage @ "
VectorBetween[{x$1, x$2, $$}, {l$, h$}] gives True if all the x$i satisfy l$ <= x$i <= h$.
VectorBetween[{l$, h$}] is the operator form of VectorBetween.
"

VectorBetween[x_, {l_, h_}] := And @@ ThreadLessEqual[l, x, h];
VectorBetween[{x_, y_}, {{xl_, xh_}, {yl_, yh_}}] := xl <= x <= xh && yl <= y <= yh;
VectorBetween[{x_, y_, z_}, {{xl_, xh_}, {yl_, yh_}, {zl_, zh_}}] := xl <= x <= xh && yl <= y <= yh && zl <= z <= zh;
VectorBetween[spec_][x_] := VectorBetween[x, spec];

(**************************************************************************************************)

PublicFunction[Lerp]

SetUsage @ "
Lerp[a$, b$, f$] linearly interpolates between a$ and b$, where f$ = 0 gives a$ and f$ = 1 gives b$.
Lerp[a$, b$, {f$1, f$2, $$}] gives a list of interpolations.
Lerp[a$, b$, Into[n$]] gives the n$ values interpolated between a$ and b$.
Lerp[f$] is the operator form of Lerp$.
* a$ and b$ can be numbers, arrays, etc.
"

Lerp[a_, b_, f_] := a * (1 - f) + b * f;
Lerp[a_, b_, f_List] := Lerp[a, b, #]& /@ f;

Lerp[a_, b_, Into[0]] := {};
Lerp[a_, b_, Into[1]] := (a + b) / 2;
Lerp[a_, b_, Into[2]] := {a, b};
Lerp[a_, b_, Into[n_]] := Lerp[a, b, Range[0, 1, 1/(n-1)]]

Lerp[n_][a_, b_] := Lerp[a, b, n];

(**************************************************************************************************)

PublicFunction[Interpolated]

SetUsage @ "
Interpolated[a$, b$, n$] is equivalent to %Lerp[a$, b$, Into[n$]].
"

Interpolated[a_, b_, n_] := Table[b * i + a * (1 - i), {i, 0, 1, 1/(n-1)}];

(**************************************************************************************************)

PublicFunction[AngleComplex, AnglePair, PairAngle]

AngleComplex[theta_] := Complex @@ AnglePair[theta];
AngleComplex[theta_List] := ToPackedComplex @ Apply[Complex, AnglePair[theta], {-2}];

SetListable[AnglePair];
AnglePair[theta_] := CosSin @ N[Tau * theta];

PairAngle[{x_, y_}] := ArcTan[x, y];
PairAngle[{0|0., 0|0.}] := 0;

(**************************************************************************************************)

PublicFunction[VectorNormSquared]

VectorNormSquared[vec_ ? NumericVectorQ] := Total[vec ^ 2];
VectorNormSquared[array_List] := Total[ToPackedReal[array]^2, {-1}];

(**************************************************************************************************)

PublicFunction[Gaussian, VectorGaussian]

Gaussian[num_, lambda_] := Exp[-(num/lambda)^2];

VectorGaussian[array_List, lambda_] := Exp[VectorNormSquared[array] / -lambda^2];

(**************************************************************************************************)

PublicFunction[Softmax]

Softmax[array_List] := Normalize[Abs @ Exp @ ToPackedReal @ N @ array, Total /* Max];

(**************************************************************************************************)

PublicFunction[RandomUnitComplex]

RandomUnitComplex[] := RandomUnitComplex[{}];
RandomUnitComplex[dims_] := AngleComplex @ RandomReal[1, dims];

(**************************************************************************************************)

PublicFunction[AngleRange]

SetRelatedSymbolGroup[AngleRange, AngleDifference];

SetUsage @ "
AngleRange[a$, b$, Into[n$]] gives n$ angles between a$ and b$.
* The angles are chosen in the direction that yields the shortest distance modulo %%Tau.
* All values are given modulo %%Tau.
"

AngleRange[a_, b_, Into[0]] := {};
AngleRange[a_, b_, Into[1]] := {Mod[(a + b), Tau] / 2};

(* suspicious about this because it does wrong thing on AngleRange[0, Tau] *)
AngleRange[a_, b_, Into[n_]] := NestList[PlusOperator[AngleDifference[a, b] / (n-1)], a, n-1];
AngleRange[a_, b_, da_] := AngleRange[a, b, Into[Ceiling[1 + Abs[AngleDifference[a, b]] / da]]];

(**************************************************************************************************)

PublicFunction[AngleDifference]

SetUsage @ "
AngleDifference[a$, b$, Into[n$]] gives the signed distance between two angles a$ and b$.
* This is the smallest difference between a$ and b$ modulo %%Tau.
"

AngleDifference[a_, b_] := If[Abs[b - a] > Pi, Mod[Mod[b, Tau] - Mod[a, Tau], Tau, -Pi], b - a];

(**************************************************************************************************)

PrivateFunction[ImageToGraphics]

ImageToGraphics[img_, {xalign_, yalign_}, size_] := Scope[
  {w, h} = ImageDimensions[img];
  yrat = h / w;
  x = (xalign - 1)/2;
  y = yrat * (yalign - 1)/2;
  Graphics[
    {Opacity[1], Raster[Reverse[ImageData @ img, {1}], {{x, y}, {x + 1, y + yrat}} * size]},
    ImageSize -> size, AspectRatio -> 1, PlotRangePadding -> None
  ]
];

(**************************************************************************************************)

PrivateFunction[ToAlignmentPair]

ToAlignmentPair[align_] := Switch[align,
  Center,      {Center, Center},
  Left|Right,  {align,  Center},
  Top|Bottom,  {Center, align},
  TopLeft,     {Left,   Top},
  TopRight,    {Right,  Top},
  BottomLeft,  {Left,   Bottom},
  BottomRight, {Right,  Bottom},
  {Left|Right|Center, Top|Bottom|Center}, align,
  _,           $Failed
];

(**************************************************************************************************)

PublicFunction[LineDilation, LineUnionDilation]

LineDilation[line:{_, _}, r_] := Polygon @ ToPackedReal @ Part[First @ RegionDilation[Line @ N @ line, r], Join[Range[1, 37, 4], Range[38, 74, 4]]]
LineDilation[line_, r_] := Polygon @ ToPackedReal @ Part[First @ RegionDilation[Line @ N @ line, r], 1;;;;4];
LineUnionDilation[lines_, r_] := Replace[MeshPrimitives[RegionUnion @@ Map[LineDilation[#, r]&, lines], "Polygon"], {z_} :> z];

(**************************************************************************************************)

PrivateFunction[fixLinearGradientFilling]

fixLinearGradientFilling[dir_][boxes_] :=
  boxes /. SurfaceAppearance["GradientFilling", l___, "GradientAngle" -> ang_, r___] :> RuleCondition[
    With[{ang2 = ang + N[Apply[ArcTan2, dir]]},
      SurfaceAppearance["GradientFilling", l, "GradientAngle" -> ang2, r]
    ]
  ];

