PackageExport["TakeOperator"]

TakeOperator[spec___][e_] := Take[e, spec];

(**************************************************************************************************)

PackageExport["DropOperator"]

DropOperator[spec___][e_] := Drop[e, spec];

(**************************************************************************************************)

PackageExport["PartOperator"]

PartOperator[spec___][e_] := Part[e, spec];

(**************************************************************************************************)

PackageExport["DotOperator"]

DotOperator[matrix_][other_] := Dot[matrix, other]

(**************************************************************************************************)

PackageExport["TimesOperator"]

TimesOperator[a_][b_] := a * b;

(**************************************************************************************************)

PackageExport["PlusOperator"]

PlusOperator[a_][b_] := a + b;

(**************************************************************************************************)

PackageExport["PlusOne"]

PlusOne[a_] := a + 1;

(**************************************************************************************************)

PackageExport["MinusOne"]

MinusOne[a_] := a - 1;

(**************************************************************************************************)

PackageExport["ModOperator"]

ModOperator[n_][e_] := If[NumericQ[e], Mod[e, n, 0], e];
ModOperator[n_, m_][e_] := If[NumericQ[e], Mod[e, n, m], e];
ModOperator[Infinity] = Identity;
ModOperator[Infinity, _] = Identity;

(**************************************************************************************************)

PackageExport["PlusOneMod"]

PlusOneMod[Infinity] := PlusOne;
PlusOneMod[Infinity, _] := PlusOne;
PlusOneMod[n_][x_] := Mod[x + 1, n];
PlusOneMod[n_, m_][x_] := Mod[x + 1, n, m];

(**************************************************************************************************)

PackageExport["MinusOneMod"]

MinusOneMod[Infinity] := MinusOne;
MinusOneMod[Infinity, _] := MinusOne;
MinusOneMod[n_][x_] := Mod[x - 1, n];
MinusOneMod[n_, m_][x_] := Mod[x - 1, n, m];

(**************************************************************************************************)

PackageExport["PlusModOperator"]

PlusModOperator[n__] := Plus /* ModOperator[n];

(**************************************************************************************************)

PackageExport["TimesModOperator"]

TimesModOperator[n__] := Times /* ModOperator[n];

(**************************************************************************************************)

PackageExport["MinusModOperator"]

MinusModOperator[n__] := Minus /* ModOperator[n];

(**************************************************************************************************)

PackageExport["SubtractModOperator"]

SubtractModOperator[n__] := Subtract /* ModOperator[n];

(**************************************************************************************************)

PackageExport["OrOperator"]

e_OrOperator[arg_] := AnyTrue[e, #[arg]&];

(**************************************************************************************************)

PackageExport["AndOperator"]

e_AndOperator[arg_] := AllTrue[e, #[arg]&];

(**************************************************************************************************)

PackageExport["NotOperator"]

NotOperator[f_][expr_] := Not @ f @ expr;

(**************************************************************************************************)

PackageExport["SameOperator"]

SameOperator[f_][g_] := SameQ[f, g];

(**************************************************************************************************)

PackageExport["UnsameOperator"]

UnsameOperator[f_][g_] := UnsameQ[f, g];

(**************************************************************************************************)

PackageExport["StyleOperator"]

StyleOperator[None] = Identity;
StyleOperator[spec___][e_] := Style[e, spec];

(**************************************************************************************************)

PackageExport["SubscriptOperator"]

SubscriptOperator[s_][e__] := Subscript[s, e];
