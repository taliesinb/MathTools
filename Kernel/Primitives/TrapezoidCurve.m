PublicGraphicsPrimitive[TrapezoidCurve]

SetUsage @ "
TrapezoidCurve[{a$, b$}, d$] represents a curve from a$ to b$ with a trapezoidal bend to the left by distance d$.
TrapezoidCurve[{a$, b$}, side$ -> d$] bends towards the given direction side$ such Top, Bottom, Left, etc.
* The distance d$ can be a numeric value or Scaled[$$] to indicate a proportion of the line's length.
"

DeclareCurvePrimitive[TrapezoidCurve, trapezoidCurvePoints];

SignPrimitive["Curve", TrapezoidCurve];

(**************************************************************************************************)

TrapezoidCurve::baddist = "Elbow distance `` is not a numeric value.";

$d1 = 0.125; $d2 = 0.4;
$lerpPoints = {$d1, $d2, 0.5, 1 - $d2, 1 - $d1};

trapezoidCurvePoints[TrapezoidCurve[{a:$Coord2P, b:$Coord2P}, amount_]] := Scope[
  delta = Normalize @ VectorRotate90[b - a];
  If[MatchQ[amount, $SidePattern|Above|Below -> _],
    dir = Rep[F[amount], $SideToCoords];
    amount = L[amount] * Sign[Dot[delta, dir]];
  ];
  SetAutomatic[amount, Scaled[0.333]];
  SetScaledFactor[amount, Dist[a, b]];
  If[!NumericQ[amount],
    Message[TrapezoidCurve::baddist, amount];
    Return @ {a, b}
  ];
  lerp = Lerp[a, b, $lerpPoints] + Threaded[delta * amount];
  points = Join[{a}, lerp, {b}];
  DiscretizeCurve @ BSplineCurve @ points
];

