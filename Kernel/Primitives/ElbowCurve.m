PublicGraphicsPrimitive[ElbowCurve]

SetUsage @ "
ElbowCurve[{a$, b$}, d$] represents a curve from a$ to b$ bent to the left by a given distance d$.
ElbowCurve[{a$, b$}, side$ -> d$] bends towards the given direction side$ such Top, Bottom, Left, etc.
* The distance d$ can be a numeric value or Scaled[$$] to indicate a proportion of the line's length.
"

DeclareCurvePrimitive[ElbowCurve, elbowCurvePoints];

SignPrimitive["Curve | Curve,Radius", ElbowCurve];

(**************************************************************************************************)

ElbowCurve::baddist = "Elbow distance `` is not a numeric value.";

elbowCurvePoints[ElbowCurve[{a:$Coord2P, b:$Coord2P}, amount_]] := Scope[
  mid = Avg[a, b];
  delta = Normalize @ VectorRotate90[b - a];
  If[MatchQ[amount, $SidePattern|Above|Below -> _],
    dir = Rep[F[amount], $SideToCoords];
    amount = L[amount] * Sign[Dot[delta, dir]];
  ];
  SetAutomatic[amount, Scaled[0.333]];
  SetScaledFactor[amount, Dist[a, b]];
  If[!NumericQ[amount],
    Message[ElbowCurve::baddist, amount];
    Return @ {a, b}
  ];
  delta *= amount * 2;
  DiscretizeCurve @ BezierCurve @ {a, mid + delta, b}
];

