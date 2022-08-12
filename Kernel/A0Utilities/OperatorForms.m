PublicFunction[TakeOperator]

TakeOperator[spec___][e_] := Take[e, spec];

(**************************************************************************************************)

PublicFunction[DropOperator]

DropOperator[spec___][e_] := Drop[e, spec];

(**************************************************************************************************)

PublicFunction[PartOperator]

PartOperator[spec___][e_] := Part[e, spec];

(**************************************************************************************************)

PublicFunction[PartOfOperator]

PartOfOperator[e_][p___] := Part[e, p];

(**************************************************************************************************)

PublicFunction[DotOperator]

DotOperator[matrix_][other_] := Dot[matrix, other]

(**************************************************************************************************)

PublicFunction[DotRightOperator]

DotRightOperator[matrix_][other_] := Dot[other, matrix]

(**************************************************************************************************)

PublicFunction[TimesOperator]

TimesOperator[a_][b_] := a * b;

(**************************************************************************************************)

PublicFunction[PlusOperator]

PlusOperator[a_][b_] := a + b;

(**************************************************************************************************)

PublicFunction[PlusOne]

PlusOne[a_] := a + 1;

(**************************************************************************************************)

PublicFunction[MinusOne]

MinusOne[a_] := a - 1;

(**************************************************************************************************)

PublicFunction[ModOperator]

ModOperator[n_][e_] := If[NumericQ[e], Mod[e, n, 0], e];
ModOperator[n_, m_][e_] := If[NumericQ[e], Mod[e, n, m], e];
ModOperator[Infinity] = Identity;
ModOperator[Infinity, _] = Identity;

(**************************************************************************************************)

PublicFunction[PlusOneMod]

PlusOneMod[Infinity] := PlusOne;
PlusOneMod[Infinity, _] := PlusOne;
PlusOneMod[n_][x_] := Mod[x + 1, n];
PlusOneMod[n_, m_][x_] := Mod[x + 1, n, m];

(**************************************************************************************************)

PublicFunction[MinusOneMod]

MinusOneMod[Infinity] := MinusOne;
MinusOneMod[Infinity, _] := MinusOne;
MinusOneMod[n_][x_] := Mod[x - 1, n];
MinusOneMod[n_, m_][x_] := Mod[x - 1, n, m];

(**************************************************************************************************)

PublicFunction[PlusModOperator]

PlusModOperator[n__] := Plus /* ModOperator[n];

(**************************************************************************************************)

PublicFunction[TimesModOperator]

TimesModOperator[n__] := Times /* ModOperator[n];

(**************************************************************************************************)

PublicFunction[MinusModOperator]

MinusModOperator[n__] := Minus /* ModOperator[n];

(**************************************************************************************************)

PublicFunction[SubtractModOperator]

SubtractModOperator[n__] := Subtract /* ModOperator[n];

(**************************************************************************************************)

PublicFunction[OrOperator]

e_OrOperator[arg_] := AnyTrue[e, #[arg]&];

(**************************************************************************************************)

PublicFunction[AndOperator]

e_AndOperator[arg_] := AllTrue[e, #[arg]&];

(**************************************************************************************************)

PublicFunction[NotOperator]

NotOperator[f_][expr_] := Not @ f @ expr;

(**************************************************************************************************)

PublicFunction[SameOperator]

SameOperator = SameAs;

(**************************************************************************************************)

PublicFunction[UnsameOperator]

UnsameOperator[f_][g_] := UnsameQ[f, g];

(**************************************************************************************************)

PublicFunction[StyleOperator]

StyleOperator[None] = Identity;
StyleOperator[spec___][e_] := Style[e, spec];

(**************************************************************************************************)

PublicFunction[SubscriptOperator]

SubscriptOperator[s_][e__] := Subscript[s, e];

(**************************************************************************************************)

PublicFunction[SetOperator]

SetOperator[value_] := Function[var, Set[var, value], {HoldAllComplete}];

(**************************************************************************************************)

PublicFunction[LookupOperator]

LookupOperator[a_][key_] := Lookup[a1, key];
LookupOperator[a_, (Rule|RuleDelayed)["Default", v_]][key_] := Lookup[a, key, v];
LookupOperator[a_, rest__][key_] := Lookup[a, key, LookupOperator[rest] @ key];
