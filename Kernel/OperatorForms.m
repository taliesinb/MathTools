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
