PublicHead[RollingCurve]
PublicOption[BendRadius, BendShape]

Options[RollingCurve] = {BendRadius -> 0.1, BendShape -> "Arc"};

AssociateTo[$MakeBoxesStyleData, Options[RollingCurve]];

declareGraphicsFormatting[rc:RollingCurve[$CoordMat3P | $GCurveIntP, ___Rule] :> Construct[Line3DBox, rollingCurvePoints @ rc], Graphics3D];
declareGraphicsFormatting[rc:RollingCurve[$CoordMat2P | $GCurveIntP, ___Rule] :> Construct[LineBox, rollingCurvePoints @ rc], Graphics];

SetUsage @ "
RollingCurve[path$] represents a curve in which line segments are connected by circular bends.
* RollingCurve supports the following options:
| %BendRadius | the radius of bends between segments |
| %BendShape | what curve to connect the segments with |
* The settings of %BendShape can be:
| 'Arc' | a circular bend of radius r$ (default) |
| 'Line' | a linear corner starting where the arc would start |
| 'Bezier' | a bezier curve with control point being the corner |
| None | return the path unchanged |
* A circular bend will be possible for the given radius if the points themselves are too close together, in this case a smaller radius is used.
* %DiscretizeCurve can be used to obtain the points for a given rolling curve.
"

(**************************************************************************************************)

PrivateFunction[rollingCurvePoints]

(*
TODO: allow the maximum distance from the corner to be specified, after which the radius will be decreased
TODO: fix arcs that interact with eachother badly and cause 'jumps'. *)

RollingCurve::badshape = "Shape `` not recognized."
RollingCurve::interr = "Internal error."

ClearAll[rollingCurvePoints];

rollingCurvePoints[RollingCurve[curve_, opts___Rule]] := Scope[
  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    bendRadius, bendShape
  ];
  rollingCurvePoints[curve, bendRadius, bendShape]
]

rollingCurvePoints[curve_, radius_, None] := curve;

$rollingCurveCache = UAssociation[];
rollingCurvePoints[curve_, radius_:1, shape_:"Arc"] := Scope[
  key = {curve, radius, shape};
  result = $rollingCurveCache @ key;
  If[!MissingQ[result], Return @ result];
  points = toCurvePoints @ curve;
  $radius = N @ radius; $shape = ReplaceAutomatic[shape, "Arc"];
  coords = ApplyWindowed[makeBend, points, 3];
  result = ToPackedReal @ Prepend[First @ points] @ Append[Last @ points] @ Catenate @ N @ coords;
  AssociateTo[$rollingCurveCache, key -> result];
  result
];

(**************************************************************************************************)

makeBend[a_, b_, c_] := Scope[
  l1 = {a, b}; l2 = {c, b};
  l1p = DisplaceLineTowards[l1, c, $radius];
  l2p = DisplaceLineTowards[l2, a, $radius];
  z = LineLineIntersectionPoint[l1p, l2p];
  If[FailureQ[z],
    r = Min[EuclideanDistance[a, b], EuclideanDistance[c, b]];
    d = EuclideanDistance[PointAlongLine[{b, a}, r], PointAlongLine[{b, c}, r]] * 0.495;
    Return @ Block[{$radius = d}, makeBend[a, b, c]];
  ];
  p1 = ClosestPointOnLine[l1, z];
  p2 = ClosestPointOnLine[l2, z];
  arc = Switch[$shape,
    "Arc",    ArcBetween[z, {p1, p2}, b],
    "Line",   Line[{p1, p2}],
    "Bezier", BezierCurve[{p1, b, p2}],
    _,        Message[RollingCurve::badshape, $shape]; Line[{p1, p2}]
  ];
  arcPoints = ToPackedReal @ DiscretizeCurve @ arc;
  arcPoints
]