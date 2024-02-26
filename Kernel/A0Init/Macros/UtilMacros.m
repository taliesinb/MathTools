PrivateMacro[MapCol1, MapCol2, MapCol3]

DefineSimpleMacro[MapCol1, {MapCol1[f_] :> MapCol[f, 1], MapCol1[f_, e_] :> MapCol[f, 1, e]}];
DefineSimpleMacro[MapCol2, {MapCol2[f_] :> MapCol[f, 2], MapCol1[f_, e_] :> MapCol[f, 2, e]}];
DefineSimpleMacro[MapCol3, {MapCol3[f_] :> MapCol[f, 2], MapCol3[f_, e_] :> MapCol[f, 3, e]}];

(* TODO: finish this by defining replacing MapColumn with MapCol *)

(**************************************************************************************************)

PublicMutatingFunction[SPrependTo, SAppendTo]

DefineSimpleMacro[SPrependTo, SPrependTo[lhs_, rhs_] :> ApplyTo[lhs, StringJoinLeft[rhs]]];
DefineSimpleMacro[SAppendTo,   SAppendTo[lhs_, rhs_] :> ApplyTo[lhs, StringJoinRight[rhs]]];

(**************************************************************************************************)

PrivateFunction[SingleQ, FromSingle]

SingleQ[{_}] := True;
SingleQ[_]   := False;

SetHoldRest[FromSingle];

FromSingle[{a_}]        := a;
FromSingle[_]           := None;

FromSingle[{a_},     _] := a;
FromSingle[{a_}, else_] := else;
