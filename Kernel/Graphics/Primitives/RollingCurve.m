PublicGraphicsPrimitive[RollingCurve]

PublicOption[BendRadius, BendShape]

SetUsage @ "
RollingCurve[path$] represents a curve in which line segments are connected by circular bends.
* RollingCurve supports the following options:
| %BendRadius | the radius of bends between segments |
| %BendShape | what curve to connect the segments with |
* The settings of %BendShape can be:
| 'Arc' | a circular bend of radius r$ (default) |
| 'Bevel' | a linear corner starting where the arc would start |
| 'Bezier' | a bezier curve with control point being the corner |
| 'Spline' | a simple spline curve |
| None | return the path unchanged |
* A circular bend will be possible for the given radius if the points themselves are too close together, in this case a smaller radius is used.
* %DiscretizeCurve can be used to obtain the points for a given rolling curve.
"

Options[RollingCurve] = {BendRadius -> 0.1, BendShape -> "Arc"};

AssociateTo[$MakeBoxesStyleData, Options[RollingCurve]];

DeclareCurvePrimitive[RollingCurve, rollingCurvePoints];

SignPrimitive["Curve", RollingCurve];

(**************************************************************************************************)

(*
TODO: allow the maximum distance from the corner to be specified, after which the radius will be decreased
TODO: fix arcs that interact with eachother badly and cause 'jumps'. *)

RollingCurve::interr = "Internal error."

rollingCurvePoints[RollingCurve[curve_, opts___Rule]] := Scope[
  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    bendRadius, bendShape
  ];
  toRollingCurvePoints[curve, bendRadius, bendShape]
];

CacheSymbol[$RollingCurveCache]

toRollingCurvePoints[points_, 0, _] := points;
toRollingCurvePoints[points_, _, None] := points;
toRollingCurvePoints[points_, radius_:1, shape_:"Arc"] := Scope @ CachedInto[
  $RollingCurveCache, Hash @ {points, radius, shape},

  SetAutomatic[shape, "Arc"];
  radius //= N;
  points //= N;

  dists = ApplyWindowed[Dist, points];

  radii = MinOperator[radius] /@ ApplyWindowed[Min, dists];
  triples = Partition[points, 3, 1];

  If[shape === "Spline",
    splinePoints = ZipMap[makeSplineBend, triples, radii];
    result = DiscretizeCurve @ BSplineCurve @ splinePoints;
  ,
    If[!MatchQ[shape, "Arc" | "Bevel" | "Line" | "Bezier"],
      BadOptionSetting[RollingCurve, BendShape, shape];
      Return @ points;
    ];
    result = populateSegments[shape, FirstLast @ points, triples, radii];
  ];

  result
];

(**************************************************************************************************)

makeSplineBend[{a_, b_, c_}, r_] := {
  PointAlongLine[{b, a}, r],
  b,
  PointAlongLine[{b, c}, r]
};

(**************************************************************************************************)

populateSegments[shape_, {p1_, pn_}, triples_, radii_] := Scope[

  rscale = 1;
  Label[retry];
  tuples = ZipMap[
    {{a, b, c}, r} |-> (
      l1 = {b, a}; l2 = {b, c};
      If[(b - a) == (c - b), Return[Nothing]];
      center = InfiniteLineLineIntersectionPoint[
        DisplaceLineTowards[l1, c, r],
        DisplaceLineTowards[l2, a, r]
      ];
      If[!MatchQ[center, $Coord2P], Nothing,
      List[
        center,
        {ClosestPointOnInfiniteLine[l1, center],
         ClosestPointOnInfiniteLine[l2, center]},
        b
      ]]
    ),
    triples, radii * rscale
  ];

  (* detect if we had a reversal *)
  skeleton = PrependAppend[p1, pn] @ Catenate @ PA2 @ tuples;
  deltas = ApplyWindowed[Subtract, skeleton];
  dots = ApplyWindowed[Dot, deltas];
  If[Min[dots] < 0,
    rscale *= 0.95;
    Goto[retry]
  ];

  (* create corners *)
  If[MatchQ[shape, "Bevel" | "Line"], Return @ ToPacked @ skeleton];

  fn = If[shape == "Arc",
    ArcBetween /* DiscretizeCurve,
    DiscretizeCurve[BezierCurve[Insert[#2, #3, 2]]]&
  ];

  PrependAppend[p1, pn] @ Catenate @ MapApply[fn, tuples]
];


