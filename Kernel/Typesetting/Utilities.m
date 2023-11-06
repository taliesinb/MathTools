PrivateFunction[StandardizeRowColumnSpec]

StandardizeRowColumnSpec[{pre___, cycle_List, post___}, n_] := Scope[
  pre = ToList[pre]; post = ToList[post];
  {preLen, postLen} = Len /@ {pre, post};
  cycle = PadRight[cycle, n, cycle];
  Which[
    preLen >= n,
      Take[pre, n],
    preLen == postLen == 0,
      cycle,
    preLen + postLen >= n,
      Take[Join[pre, post], n],
    True,
      Join[pre, Take[cycle, n - Len[pre] - Len[post]], post]
  ]
]

StandardizeRowColumnSpec[spec_List, n_] :=
  PadRight[spec, n, spec];

StandardizeRowColumnSpec[item_, n_] :=
  Repeat[item, n];

StandardizeRowColumnSpec[Automatic|None, _] :=
  Automatic;

(**************************************************************************************************)

PrivateFunction[StripLabel]

StripLabel[items:{___Labeled}] := Part[items, All, 1];
StripLabel[Labeled[e_, _]] := e;
StripLabel[e_] := e;

(**************************************************************************************************)

PrivateFunction[applyRiffled]

applyRiffled[f_, op_][args___] := f[riffled[op][args]];

(**************************************************************************************************)

PrivateFunction[riffled]

riffled[op_][] := "";
riffled[op_][a_] := a;
riffled[op_][a_, b_] := {a, op, b}
riffled[op_][a_, b_, c__] := Riffle[{a, b, c}, op];
