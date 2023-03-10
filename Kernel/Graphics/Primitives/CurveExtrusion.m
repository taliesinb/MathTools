PublicHead[CurveExtrusion]

declareGraphicsFormatting[CurveExtrusion[p:$CoordMat2P | $GCurveIntP, r_ ? NumericQ] :> extrudedPathBoxes[p, r], Graphics];

SetUsage @ "
CurveExtrusion[curve$, r$] represents the polygon obtained by extruding curve$ by a radius r$.
"

extrudedPathBoxes[curve_, r_] := Scope[
	points = toCurvePoints @ curve;
  poly = ExtrudeCurveToPolygon[points, r];
  Construct[Typeset`MakeBoxes, poly, StandardForm, Graphics]
]

(**************************************************************************************************)

PublicFunction[ExtrudeCurveToPolygon]

(* not sure why, but if you don't do this the resulting polygon is closed from the start to end point *)
doubleUpCurve = Case[
	BezierCurve[c_] := JoinedCurve[{BezierCurve @ c, BezierCurve @ Reverse @ c}];
	Line[c_]        := JoinedCurve[{Line @ c, Line @ Reverse @ c}];
	points_List     := % @ Line @ points;
	curve_          := % @ DiscretizeCurve @ curve;
];

$extrusionCache = UAssociation[];

ExtrudeCurveToPolygon[curve_, r_] := Scope[
	key = Hash @ {curve, r};
	result = $extrusionCache @ key;
	If[!MissingQ[result], Return @ result];
	doubled = doubleUpCurve @ curve;
  region = DiscretizeGraphics @ doubled;
  dilation = RegionDilation[region, r];
  If[Head[dilation] =!= MeshRegion,
  	result = $Failed;
  ,
		result = RegionPolygon @ BoundaryMesh @ dilation;
	];
	AssociateTo[$extrusionCache, key -> result];
	result
];

(*

(**************************************************************************************************)

toOuterPolygon[poly_] := OuterPolygon @ CanonicalizePolygon[WindingPolygon[poly, "NonzeroRule"], Full];

these both cause crashes and are riddled with defects. keeping them around for historical record.

$cornerPoly = N @ Reverse @ CirclePoints[16];
ExtrudeCurveToPolygon2[coords_, width_] := Scope[
  d = 0;
  poly = ToPackedReal @ Join[ApplyWindowed[toRect, coords], Threaded[#] + width * $cornerPoly& /@ Take[coords, {2, -2}]];
  toOuterPolygon @ poly
,
  toRect[p1_, p2_] := (
    d = VectorRotate90[Normalize[p2 - p1]] * width;
    {p1 + d, p2 + d, p2 - d, p1 - d}
  )
]

PublicFunction[ExtrudeCurveToPolygon, ExtrudeCurveToPolygon2]

ExtrudeCurveToPolygon[coords_, width_] := Scope[
  vecs = ToPackedReal @ vectorListAlongLineEdged[N @ coords, width / 32.];
  {points, dirs} = Transpose @ vecs;
  Z1;
  left = Threaded[points] + width * VectorRotate90[dirs];
  right = Threaded[points] + width * VectorRotate90CW[dirs];
  $failed = False;
  Z2;
  left //= trimIntersections; right //= trimIntersections;
  poly = Join[left, Reverse @ right];
  Z3;
  poly = OuterPolygon @ CanonicalizePolygon[WindingPolygon[poly, "NonzeroRule"], Full];
  Z4;
  poly
]

trimIntersections[p_] := Scope[
  p = p;
  d = Differences[p];
  Do[
    If[Dot[Part[d, i], Part[d, i + 1]] < 0,
      p0 = LineLineIntersectionPoint[Part[p, i + {0, 1}], Part[p, i + {3, 4}]];
      If[FailureQ[p0], $failed ^= True, Part[p, i + {1, 2, 3}] = p0];
    ],
    {i, 1, Length[p] - 4, 3}
  ];
  ToPackedReal @ Map[First, Split[p]]
]

vectorListAlongLineEdged[{a_, b_}, _] := Scope[
  d = Normalize[b - a];
  {{a, d}, {Avg[a, b], d}, {b, d}}
];

vectorListAlongLineEdged[coords_, w_] := Scope[
  diffs = Normalize /@ Differences[coords];
  n = Length[coords];
  is = Range[1, n]; Part[is, -1] = -1; i1 = i2 = 0;
  Map[toVecListElem, is]
,
  toVecListElem[1] := {First @ coords, First @ diffs},
  toVecListElem[-1] := {Last @ coords, Last @ diffs},
  toVecListElem[i_Integer] := Splice @ {
    {a, b, c} = Part[coords, {i - 1, i, i + 1}];
    {d1, d2} = Part[diffs, {i - 1, i}];
    {PointAlongLine[{b, a}, w], d1},
    {Part[coords, i], Normalize @ Mean @ Part[diffs, {i-1, i}]},
    {PointAlongLine[{b, c}, w], d2}
  }
];
 *)