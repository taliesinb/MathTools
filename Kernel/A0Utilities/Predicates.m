PackageExport["RuleListQ"]

RuleListQ[$RuleListPattern] := True;
RuleListQ[_] := False;

(**************************************************************************************************)

PackageExport["SameHeadQ"]

SameHeadQ[a_, b_] := Head[a] === Head[b];

(**************************************************************************************************)

PackageExport["SameLengthQ"]

SetUsage @ "
SameLengthQ[a$, b$] gives True if %Length[a$] === %Length[b$].
"

SameLengthQ[a_, b_] := Length[a] === Length[b];
SameLengthQ[a_][b_] := SameLengthQ[a, b];

(**************************************************************************************************)

PackageExport["RealVectorQ"]

SetUsage @ "
RealVectorQ[list$] gives True if list$ is a vector of real-valued numbers.
* Integers, rationals, etc. are considered real-valued numbers.
"

RealVectorQ[list_] := VectorQ[list, Internal`RealValuedNumberQ];

(**************************************************************************************************)

PackageExport["IntegerVectorQ"]

SetUsage @ "
IntegerVectorQ[list$] gives True if list$ is a vector of integers.
"

IntegerVectorQ[list_] := VectorQ[list, IntegerQ];

(**************************************************************************************************)

PackageExport["PositiveIntegerVectorQ"]

SetUsage @ "
IntegerVectorQ[list$] gives True if list$ is a vector of integers.
"

PositiveIntegerVectorQ[list_] := VectorQ[list, PositiveIntegerQ];

(**************************************************************************************************)

PackageExport["UnitIntervalArrayQ"]

SetUsage @ "
UnitIntervalArrayQ[arr$] gives True if arr$ is an array whose values are between 0 and 1 inclusive.
"

UnitIntervalArrayQ[arr_] := Scope[
  {min, max} = MinMax @ arr;
  TrueQ[0 <= min <= max <= 1]
];

(**************************************************************************************************)

PackageExport["RealMatrixQ"]

SetUsage @ "
RealVectorQ[list$] gives True if list$ is a matrix of real-valued numbers.
* Integers, rationals, etc. are considered real-valued numbers.
"

RealMatrixQ[list_] := MatrixQ[list, Internal`RealValuedNumberQ];

(**************************************************************************************************)

PackageExport["ComplexVectorQ"]

SetUsage @ "
ComplexVectorQ[list$] gives True if list$ is a vector of complex-valued numbers.
* At least one element of list$ should be a Complex expression.
* See %ContainsComplexQ.
"

ComplexVectorQ[list_] := VectorQ[list, NumericQ] && !FreeQ[list, Complex];

(**************************************************************************************************)

PackageExport["ContainsComplexQ"]

SetUsage @ "
ContainsComplexQ[expr$] gives True if expr$ contains at least one Complex expression.
* See %ComplexVectorQ.
"

ContainsComplexQ[expr_] := !FreeQ[expr, Complex];

(**************************************************************************************************)

PackageExport["ContainsNegativeQ"]

SetUsage @ "
ContainsNegativeQ[expr$] gives True if expr$ contains at least one negative real, rational, or integer.
"

ContainsNegativeQ[expr_] := !FreeQ[expr, n_Real | n_Rational | n_Integer ? Negative];

(**************************************************************************************************)

PackageExport["RangeQ"]

SetRelatedSymbolGroup[RangeQ, PermutedRangeQ]

SetUsage @ "
RangeQ[list$] gives True if list$ is a permuation of {1, 2, $$, n$}.
"

RangeQ[list_] := PermutedRangeQ[list] && OrderedQ[list];

(**************************************************************************************************)

PackageExport["PermutedRangeQ"]

SetUsage @ "
PermutedRangeQ[list$] gives True if list$ is a permutation of {1, 2, $$, n$}.
"

PermutedRangeQ[list_] := VectorQ[list, IntegerQ] && MinMax[list] == {1, Length @ list};

(**************************************************************************************************)

PackageExport["AllSameQ"]
PackageExport["NotAllSameQ"]

AllSameQ[e_] := SameQ @@ e;
NotAllSameQ[e_] := Not[SameQ @@ e];

(**************************************************************************************************)

PackageExport["AllEqualQ"]
PackageExport["NotAllEqualQ"]

AllEqualQ[e_] := Equal @@ e;
NotAllEqualQ[e_] := Not[Equal @@ e];

(**************************************************************************************************)

PackageExport["NotMatchQ"]

NotMatchQ[a_, b_] := !MatchQ[a, b];
NotMatchQ[b_][a_] := !MatchQ[a, b];
