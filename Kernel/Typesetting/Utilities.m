PackageExport["StandardizeRowColumnSpec"]

StandardizeRowColumnSpec[{pre___, cycle_List, post___}, n_] := Scope[
  pre = ToList[pre]; post = ToList[post];
  {preLen, postLen} = Length /@ {pre, post};
  cycle = PadRight[cycle, n, cycle];
  Which[
    preLen >= n,
      Take[pre, n],
    preLen == postLen == 0,
      cycle,
    preLen + postLen >= n,
      Take[Join[pre, post], n],
    True,
      Join[pre, Take[cycle, n - Length[pre] - Length[post]], post]
  ]
]

StandardizeRowColumnSpec[spec_List, n_] :=
  PadRight[spec, n, spec];

StandardizeRowColumnSpec[item_, n_] :=
  ConstantArray[item, n];

StandardizeRowColumnSpec[Automatic|None, _] :=
  Automatic;