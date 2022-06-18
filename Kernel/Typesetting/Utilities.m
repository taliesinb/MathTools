PrivateFunction[StandardizeRowColumnSpec]

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

(**************************************************************************************************)

PrivateFunction[StripLabel]

StripLabel[items:{___Labeled}] := Part[items, All, 1];
StripLabel[Labeled[e_, _]] := e;
StripLabel[e_] := e;

(**************************************************************************************************)

PrivateFunction[TBox, SBox, RBox, GBox]

TBox[form_][args___] := TemplateBox[{args}, form];

SBox[form_] := TemplateBox[{}, form];

RBox[args___] := RowBox[{args}];

GBox[entries_, alignments_, rowSpacings_, colSpacings_] :=
  GridBox[
    entries,
    GridBoxAlignment -> {"Columns" -> alignments},
    GridBoxSpacings -> {"Rows" -> prepend0 @ rowSpacings, "Columns" -> prepend0 @ colSpacings}
  ];

prepend0[list_List] := Prepend[list, 0];
prepend0[e_] := e;

(**************************************************************************************************)


PrivateFunction[applyRiffled]

applyRiffled[f_, op_][args___] := f[riffled[op][args]];

(**************************************************************************************************)


PrivateFunction[riffled]

riffled[op_][] := "";
riffled[op_][a_] := a;
riffled[op_][a_, b_] := {a, op, b}
riffled[op_][a_, b_, c__] := Riffle[{a, b, c}, op];
