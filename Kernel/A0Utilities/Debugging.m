PublicFunction[EchoEdgeList]

EchoEdgeList = EchoFunction[EdgeList]

(**************************************************************************************************)

PublicFunction[EchoGraphicsScope, EchoGraphics]

SetHoldAllComplete[EchoGraphicsScope];
EchoGraphicsScope[e_] := Scope[
  $prims = {};
  res = e;
  n = Length[$prims]; $i = 1;
  Print @ Graphics[
    Map[{Opacity[$i++ / n], #}&, $prims],
    Frame -> True, PlotRangePadding -> Scaled[0.1]
  ];
  res
];

(**************************************************************************************************)

PublicFunction[EchoGraphics]

EchoGraphics[e_] := (AppendTo[$prims, e]; e);
EchoGraphics[{x_ ? RealVectorQ, y_ ? RealVectorQ}] := (EchoGraphics @ Transpose @ {x, y}; {x, y});
EchoGraphics[points_ ? RealMatrixQ] := (AppendTo[$prims, Point @ points]; points);
