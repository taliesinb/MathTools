PublicFunction[RuleListQ]

RuleListQ[$RuleListPattern] := True;
RuleListQ[_] := False;

(**************************************************************************************************)

PublicFunction[SameSetQ]

SameSetQ[a_List, b_List] := Sort[a] === Sort[b];
SameSetQ[a_][b_] := SameSetQ[a, b];

(**************************************************************************************************)

PublicFunction[SameHeadQ]

SameHeadQ[a_, b_] := H[a] === H[b];
SameHeadQ[a_][b_] := SameHeadQ[a, b];

(**************************************************************************************************)

PublicFunction[SameLengthQ]

SetUsage @ "
SameLengthQ[a$, b$] gives True if %Len[a$] === %Len[b$].
"

SameLengthQ[a_, b_] := Len[a] === Len[b];
SameLengthQ[a_][b_] := SameLengthQ[a, b];

(**************************************************************************************************)

PublicFunction[ListVectorQ]

ListVectorQ[e_] := VecQ[e, ListQ];

(**************************************************************************************************)

PublicFunction[RealVectorQ]

SetUsage @ "
RealVectorQ[list$] gives True if list$ is a vector of real-valued numbers.
* Integers, rationals, etc. are considered real-valued numbers.
"

RealVectorQ[list_] := VecQ[list, RealValuedNumberQ];

(**************************************************************************************************)

PublicFunction[IntegerVectorQ]

SetUsage @ "
IntegerVectorQ[list$] gives True if list$ is a vector of integers.
"

IntegerVectorQ[list_] := VecQ[list, IntQ];

(**************************************************************************************************)

PublicFunction[PositiveIntegerVectorQ]

SetUsage @ "
IntegerVectorQ[list$] gives True if list$ is a vector of integers.
"

PositiveIntegerVectorQ[list_] := VecQ[list, PositiveIntegerQ];

(**************************************************************************************************)

PublicFunction[PositiveIntegerVectorsQ]

SetUsage @ "
PositiveIntegerVectorsQ[list$] gives True if list$ is a list of vectors of positive integers.
"

PositiveIntegerVectorsQ[list_] := VecQ[list, PositiveIntegerVectorQ];

(**************************************************************************************************)

PublicFunction[UnitIntervalArrayQ]

SetUsage @ "
UnitIntervalArrayQ[arr$] gives True if arr$ is an array whose values are between 0 and 1 inclusive.
"

UnitIntervalArrayQ[arr_] := Scope[
  {min, max} = MinMax @ arr;
  TrueQ[0 <= min <= max <= 1]
];

(**************************************************************************************************)

PublicFunction[RealMatrixQ]

SetUsage @ "
RealVectorQ[list$] gives True if list$ is a matrix of real-valued numbers.
* Integers, rationals, etc. are considered real-valued numbers.
"

RealMatrixQ[list_] := MatrixQ[list, RealValuedNumberQ];

(**************************************************************************************************)

PublicFunction[ComplexVectorQ]

SetUsage @ "
ComplexVectorQ[list$] gives True if list$ is a vector of complex-valued numbers.
* At least one element of list$ should be a Complex expression.
* See %ContainsComplexQ.
"

ComplexVectorQ[list_] := VecQ[list, NumericQ] && ContainsComplexQ[list];

(**************************************************************************************************)

PublicFunction[HoldNumericQ]

SetUsage @ "
HoldNumericQ[n$] returns True if n$ is numeric.
"

SetHoldAllComplete[HoldNumericQ];

HoldNumericQ[e_] := NumericQ[Uneval @ e];

(**************************************************************************************************)

PublicFunction[HoldPackedArrayQ]

SetHoldAllComplete[HoldPackedArrayQ];

SetUsage @ "
HoldPackedArrayQ[arr$] returns True if arr$ is a packed array.
"

HoldPackedArrayQ[e_] := PackedArrayQ[Uneval @ e];

(**************************************************************************************************)

PublicFunction[HoldNumericArrayQ]

SetHoldAllComplete[HoldNumericArrayQ];

SetUsage @ "
HoldNumericArrayQ[arr$] returns True if arr$ is rectangular array containing numeric values.
"

HoldNumericArrayQ[e_] := PackedArrayQ[Uneval @ e] || ArrayQ[Uneval @ e, _, HoldNumericQ];

(**************************************************************************************************)

PublicFunction[ContainsComplexQ]

SetUsage @ "
ContainsComplexQ[expr$] gives True if expr$ contains at least one Complex expression.
* See %ComplexVectorQ.
"

ContainsComplexQ[expr_] := HasComplex[expr];

(**************************************************************************************************)

PublicFunction[ContainsNegativeQ]

SetUsage @ "
ContainsNegativeQ[expr$] gives True if expr$ contains at least one negative real, rational, or integer.
"

ContainsNegativeQ[expr_] := !FreeQ[expr, n_Real | n_Rational | n_Int ? Negative];

(**************************************************************************************************)

PublicFunction[RangeQ]

SetRelatedSymbolGroup[RangeQ, PermutedRangeQ]

SetUsage @ "
RangeQ[list$] gives True if list$ is a permuation of {1, 2, $$, n$}.
"

RangeQ[list_] := PermutedRangeQ[list] && OrderedQ[list];

(**************************************************************************************************)

PublicFunction[PermutedRangeQ]

SetUsage @ "
PermutedRangeQ[list$] gives True if list$ is a permutation of {1, 2, $$, n$}.
"

PermutedRangeQ[list_] := VecQ[list, IntQ] && MinMax[list] == {1, Len @ list};

(**************************************************************************************************)

PublicFunction[AllSameQ, NotAllSameQ]

AllSameQ[e_] := SameQ @@ e;
NotAllSameQ[e_] := Not[SameQ @@ e];

(**************************************************************************************************)

PublicFunction[AllEqualQ, NotAllEqualQ]

AllEqualQ[e_] := Equal @@ e;
NotAllEqualQ[e_] := Not[Equal @@ e];

(**************************************************************************************************)

PublicFunction[NotMatchQ]

NotMatchQ[a_, b_] := !MatchQ[a, b];
NotMatchQ[b_][a_] := !MatchQ[a, b];

(**************************************************************************************************)

PublicFunction[NoneQ]

NoneQ[None] := True;
NoneQ[_] := False;
