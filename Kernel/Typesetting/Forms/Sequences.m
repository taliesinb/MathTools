PublicFunction[TupleSequence, ListSequence, MultisetSequence, PlusSequence, CommaSequence]

ListSequence[args___] := ListForm @ EllipsisSequence[args];
MultisetSequence[args___] := MultisetForm @ EllipsisSequence[args];
PlusSequence[args___] := PlusForm @ EllipsisSequence[args];
TupleSequence[args___] := TupleForm @ EllipsisSequence[args];
CommaSequence[args___] := CommaForm @ EllipsisSequence[args];

PublicFunction[EllipsisSequence]

toSeqF = Case[
  f_Function                     := f;
  f_ /; ContainsQ[f, \[FormalI]] := Construct[Function, \[FormalI], f];
  f_                             := f
];

EllipsisSequence[f_] :=
  EllipsisSequence[f, SymbolForm["n"]];

EllipsisSequence[f_, n_, k_Integer:2] := With[
  {f2 = toSeqF @ f,
   n2 = Switch[n, None, None, Automatic | Null, SymbolForm["n"], _Symbol | _String, SymbolForm @ n, _, n]},
  Sequence @@ Flatten[{f2 /@ Range[1, k], {EllipsisSymbol, If[n2 === None, Nothing, f2 @ n2]}}]
];

EllipsisSequence[f_, n_, k_:1, "Reversed" -> True] :=
  Apply[Sequence, Reverse @ List @ EllipsisSequence[f, n, k]]

(**************************************************************************************************)

PublicFunction[MakeSequence]

MakeSequence[f_, n_] := With[
  {f2 = toSeqF @ f},
  f2 /@ Range[1, n]
];

(**************************************************************************************************)

PublicFunction[CreateSequenceVars]

CreateSequenceVars[baseVar_, f_, n_] := With[
  {f2 = toSeqF @ f, symName = If[StringQ[baseVar], baseVar, SymbolName[baseVar]]},
  Sequence @@ Map[
    With[{s = Symbol[symName <> IntegerString[#]]}, Set[s, f2[#]]]&,
    Range[1, n]
  ]
];
