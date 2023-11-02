PublicGraphicsPrimitive[NeatCurve]

PublicOption[JoinStyle, SegmentPosition, ShortcutRadius]

SetUsage @ "
NeatCurve[{src$, tgt$}] is a curve that leaves src$ at a given angle and enters tgt$ at a given angle.

* the way the endpoints are connected depends on the setting of %JoinStyle, which can take these settings:
| dir$ | enter and leave with a given orientation |
| {dir$s, dir$t} | leave src$ with direction dir$s, enter dst$ from direction dir$t |
* individual directions can be:
| %Vertical | enter/leave vertically, with a horizontal segment in the middle |
| %Horizontal | enter/leave horizontally, with a vertical segment in the middle |
| %Axis | enter/leave either horizontally or vertically |

* if the lines extended from the endpoints will not cross, a segment is introduced to create a connection.

* additionally, a single spec side$ ensures a segment *will* be placed in a given position.
| %Top | place a horizontal connecting segment topmost |
| %Bottom | place a horizontal connecting segment bottommost|
| %Left | place a vertical connecting segment leftmost |
| %Right | place a vertical connecting segment rightmost |

* %SegmentPosition -> r% specifies that any segment should be placed scaled position r% between 0 (start) and 1 (end).

* %ShortcutRadius -> r% specifies that a full corner / segment will not be emitted, and instead a distance r%
will be travelled from the endpoints before a shortcut is taken.

* %BendRadius -> r% gives the radius of bends connecting the edges.

* the option %SetbackDistance is applied with respect to the entering and leaving angles.
"

(*
* %SetbackSegmentPosition -> True will place the segment in a position that occurs after the setback.
*)
Options[NeatCurve] = {
  JoinStyle -> Axis,
  SegmentPosition -> 0.5,
  ShortcutRadius -> 0,
  BendRadius -> 0.5,
  Setback -> 0.1
  (* SetbackSegmentPosition -> False *)
};

DeclareCurvePrimitive[NeatCurve, neatCurvePoints];

SignPrimitive["Curve | Pair", NeatCurve];

(**************************************************************************************************)

$dirP = $SidePattern | Horizontal | Vertical | $Coord2P | Axis;

$hor = Horizontal; $ver = Vertical;

neatCurvePoints[NeatCurve[{a_, b_}, opts:OptionsPattern[NeatCurve]]] := Scope[

  UnpackOptionsAs[NeatCurve, {opts},
    joinStyle, segmentPosition, shortcutRadius, bendRadius, setback];

  SetNone[setback, 0];
  {setback1, setback2} = setback * {1, 1};

  segment = Automatic;

  b //= N; a //= N;
  delta = b - a;

  If[MatchQ[joinStyle, $dirP], joinStyle = {joinStyle, joinStyle}];
  Switch[joinStyle,
    {$ver, $ver},   dir1 = dir2 = $ver; segment = $hor,
    {$hor, $hor},   dir1 = dir2 = $hor; segment = $ver,
    Above,          {dir1, dir2} = If[P2[a] < P2[b], {$ver, $hor}, {$hor, $ver}],
    Below,          {dir1, dir2} = If[P2[a] > P2[b], {$ver, $hor}, {$hor, $ver}],
    {$dirP, $dirP}, {dir1, dir2} = joinStyle,
    _,              BadOptionSetting[NeatCurve, JoinStyle, joinStyle]; Return @ {}
  ];

  (* can a and b snap on the X and Y axes? *)
  abs = Abs @ delta;
  {snap1, snap2} = ZipMap[canSnap, {setback1, setback2}, {abs, abs}];

  (* turn Horizontal, Vertical, Automatic, etc. specs into Left, Right, etc.
  we use the snapping to bias towards axial directions for Automatic and Axes *)
  dir1 = resolveHV[dir1, delta,  snap1];
  dir2 = resolveHV[dir2, -delta, snap2];

  (* we now use the setbacks to determine where endpoints starts and its initial direction *)
  {dir1, off1} = resolveDirAndOffset[delta, setback1, dir1];
  {dir2, off2} = resolveDirAndOffset[-delta, setback2, dir2];

  a = a + off1; b = b + off2;
  delta = b - a; dist = Norm[delta];

  (* we now decide if endpoints are within the bounds of Rectangular endpoints and able to be
  turned into straight lines *)
  snapped = applySnap[{a, b}, dir1, dir2, snap1, snap2];
  If[snapped =!= None, Return @ snapped];

  If[segment === Automatic,
    ab = {a, a + dir1 * dist};
    ba = {b, b + dir2 * dist};
    m = InfiniteLineLineIntersectionPoint[ab, ba];
    segment = If[m === None, If[P1[abs] < P2[abs], $hor, $ver], {m}];
  ];

  numBends = Switch[segment, $hor | $ver, 2, _, 1];
  If[bendRadius > 0 && segmentPosition != 0.5,
    (* we must leave buffer for the bend to happen *)
    dbend = Sign[delta] * bendRadius / numBends;
    m = Lerp[a + dbend, b - dbend, segmentPosition];
  ,
    m = Lerp[a, b, segmentPosition];
  ];
  mids = chooseMids[a, b, m, segment];

  If[shortcutRadius > 0,
    as = PointAlongLine[{a, P1 @ mids}, shortcutRadius];
    bs = PointAlongLine[{b, PN @ mids}, shortcutRadius];
    bendRadius = Min[bendRadius, shortcutRadius];
    points = {a, as, bs, b};
  ,
    bendRadius = Min[bendRadius, (Norm /@ delta)/numBends];
    points = Join[{a}, mids, {b}];
  ];
  If[bendRadius > 0,
    (* points //= fixTooClose; *)
    points = DiscretizeCurve @ RollingCurve[points, BendRadius -> bendRadius]
  ];

  points
];

(**************************************************************************************************)

resolveHV = Case[
  Seq[Axis, d:{x_, y_}, s_] := %[If[Abs[x] >= Abs[y], Horizontal, Vertical], d, s];
  (* Seq[Automatic, {x_, y_}, {sx_, sy_}] := $CoordsToSide @ Sign @ List[If[sx, 0, x], If[sy, 0, y]]; *)
  Seq[$hor, {x_, y_}, {sx_, _}]  := If[sx, If[y > 0, Top, Bottom], If[x > 0, Right, Left]];
  Seq[$ver, {x_, y_}, {_, sy_}]  := If[sy, If[x > 0, Right, Left], If[y > 0, Top, Bottom]];
  Seq[e_, _, _]                  := e;
];

(**************************************************************************************************)

resolveDirAndOffset[delta_] := Case[
  Rectangular[r_] := Case[
    s:$SidePattern := With[{v = $SideToCoords @ s}, {Normalize @ v, v * r}];
    v:$Coord2P     := With[{v2 = Normalize @ v}, {v2, LineRectangleIntersectionPoint[{v2 * Norm[delta], {0,0}}, {-r, +r}]}];
  ];
  r:$NumberP := Case[
    s:$SidePattern := With[{v = $SideToCoords @ s}, {Normalize @ v, v * r}];
    v:$Coord2P     := With[{v2 = Normalize @ v}, {v2, v2 * r}];
  ];
];

(**************************************************************************************************)

canSnap[Rectangular[{w_, h_}], {ax_, ay_}] := {ax < w, ay < h};
canSnap[r_ ? NumberQ,          {ax_, ay_}] := {ax < r / 32, ay < r / 32};
canSnap[_, _] := {False, False};

(* if either axis can snap, and the chosen direction is compatible with snapping in that axis,
then apply the snapping. so if A and B are both y-snappable, then set the y coord of the
line to be average of these. if just A is snappable, then y is set to B's value. if neither snappable,
we can't snap.
*)
applySnap[line_, dir1_, dir2_, {s1x_, s1y_}, {s2x_, s2y_}] := Scope[
  line = line;
  Which[
    !Or[s1x, s1y, s2x, s2y],
      Return @ None,
    isV[dir1] && isV[dir2],
      Part[line, All, 1] //= snapCoord[s1x, s2x],
    isH[dir1] && isH[dir2],
      Part[line, All, 2] //= snapCoord[s1y, s2y],
    True,
      Return @ None
  ];
  Return @ line;
]

isH[{x_, y_}] := Abs[x] >= Abs[y];
isV[{x_, y_}] := Abs[y] >= Abs[x];

snapCoord[True,   True] = Mean; (* TODO: choose the narrow one to win *)
snapCoord[True,  False] = Last;
snapCoord[False,  True] = First;
snapCoord[False, False] = Identity;

(**************************************************************************************************)

chooseMids[{ax_, ay_}, {bx_, by_}, {mx_, my_}] := Case[
  $hor       := {{ax, my}, {bx, my}};
  $ver       := {{mx, ay}, {mx, by}};
  Center     := {{mx, my}};
  seg_List   := seg;
  None       := {};
]

(**************************************************************************************************)

(* NOTE: dead code... should I understand if the goal is still valid? *)
tooClose[u_, v_, m_:1] := (Dist[u, v]) < m * bendRadius;

(* push v towards t until it is bendRadius away from s *)
push[s_, v_, t_] := InfiniteLineCircleIntersectionPoint[{v, t}, {s, bendRadius}];

fixTooClose = Case[
  {a_, m_, b_} /; tooClose[a, m] := {a, push[a, m, b], b};
  {a_, m_, b_} /; tooClose[m, b] := {a, push[b, m, a], b};
  {a_, m_, n_, b_} /; tooClose[a, m] := {a, n, b};
  {a_, m_, n_, b_} /; tooClose[n, b] := {a, m, b};
  {a_, m_, n_, b_} /; tooClose[m, n, 2] := {a, push[m, n, a], push[n, m, b], b};
  other_ := other;
]
