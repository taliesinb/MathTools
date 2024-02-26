PrivateMutatingFunction[SetAll, SetNone, SetAuto, SetFailed, SetMissing, SetInherited, SetScaledFactor, SetCached]

(* some of these are already defined in GU, but aren't macros, so have our own versions here *)

SetHoldAll[SetAll, SetNone, SetAuto, SetFailed, SetMissing, SetInherited, SetCached, SetScaledFactor];

DefineSimpleMacro[SetAll,       SetAll      [lhs_, rhs_] :> If[lhs === All,       lhs = rhs, lhs]];
DefineSimpleMacro[SetNone,      SetNone     [lhs_, rhs_] :> If[lhs === None,      lhs = rhs, lhs]];
DefineSimpleMacro[SetAuto,      SetAuto     [lhs_, rhs_] :> If[lhs === Auto,      lhs = rhs, lhs]];
DefineSimpleMacro[SetFailed,    SetFailed   [lhs_, rhs_] :> If[FailureQ[lhs],     lhs = rhs, lhs]];
DefineSimpleMacro[SetMissing,   SetMissing  [lhs_, rhs_] :> If[MissingQ[lhs],     lhs = rhs, lhs]];
DefineSimpleMacro[SetInherited, SetInherited[lhs_, rhs_] :> If[lhs === Inherited, lhs = rhs, lhs]];

DefineSimpleMacro[SetScaledFactor, SetScaledFactor[lhs_, rhs_] :> If[MatchQ[lhs, Scaled[_ ? NumericQ]], lhs //= F /* N; lhs *= rhs]];

(* the loader introduces a manual alias from UpSetDelayed to SetCached *)
DefineSimpleMacro[SetCached,             SetCached[lhs_, rhs_] :> SetDelayed[lhs, Set[lhs, rhs]]];

(**************************************************************************************************)

PrivateMutatingFunction[SetInitialValue, SetDelayedInitialValue]

SetHoldAllComplete[SetInitialValue, SetDelayedInitialValue];

SetInitialValue[sym_Symbol, other__Symbol, body_]        := (SetInitialValue[sym, body]; SetInitialValue[other, body]);
SetInitialValue[sym_Symbol, body_]                       := If[!HasImmediateValueQ[sym], MTLoader`DeclarePreservedVariable[sym]; Set[sym, body]];

SetDelayedInitialValue[sym_Symbol, other__Symbol, body_] := (SetDelayedInitialValue[sym, body]; SetDelayedInitialValue[other, body]);
SetDelayedInitialValue[sym_Symbol, body_]                := If[!HasOwnEvaluationsQ[sym], MTLoader`DeclarePreservedVariable[sym]; SetDelayed[sym, body]];

_SetInitialValue        := Print["Bad SetInitialValue"];
_SetDelayedInitialValue := Print["Bad SetDelayedInitialValue"];
