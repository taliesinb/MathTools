PublicForm[ElbowCurve]

SetUsage @ "
ElbowCurve[{a$, b$}, d$] represents a curve from a$ to b$ bent to the left by a given distance d$.
ElbowCurve[{a$, b$}, side$ -> d$] bends towards the given direction side$ such Top, Bottom, Left, etc.
* The distance d$ can be a numeric value or Scaled[$$] to indicate a proportion of the line's length.
"

declareGraphicsFormatting[ElbowCurve[{a:$Coord2P, b:$Coord2P}, amount_:Automatic] :> elbowCurveBoxes[a, b, amount], Graphics];

elbowCurveBoxes[a_, b_, amount_] := Construct[BezierCurveBox, ToPackedReal @ elbowBezierCurvePoints[a, b, amount]];

(**************************************************************************************************)

PrivateFunction[elbowBezierCurvePoints]

ElbowCurve::baddist = "Elbow distance `` is not a numeric value.";

elbowBezierCurvePoints[a_, b_, amount_] := Scope[
  mid = Avg[a, b];
  delta = Normalize @ VectorRotate90[b - a];
  If[MatchQ[amount, $SidePattern|Above|Below -> _],
    dir = Replace[First[amount], $SideToCoords];
    amount = Last[amount] * Sign[Dot[delta, dir]];
  ];
  SetAutomatic[amount, Scaled[0.333]];
  SetScaledFactor[amount, EuclideanDistance[a, b]];
  If[!NumericQ[amount],
    Message[ElbowCurve::baddist, amount];
    Return @ {a, b}
  ];
  delta *= amount;
  {a, mid + delta, b}
];
