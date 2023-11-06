PublicFunction[UnorderedPairs]

UnorderedPairs[list_] := Subsets[list, {2}];

(**************************************************************************************************)

PublicFunction[SignedSubsets]

SignedSubsets[set_] := Scope[
  n = Len[set]; $set = set;
  SortBy[VectorReplace[NegatedForm[z_] :> z]] @ MapTuples[toSignedSubset, {0, 1, -1}, n]
];

toSignedSubset[vals_] := MapThread[
  Switch[#1, -1, NegatedForm[#2], 0, Nothing, 1, #2]&,
  {vals, $set}
];

(**************************************************************************************************)

PublicFunction[BinaryDigits]

BinaryDigits[n_, len_] := IntegerDigits[n, 2, len];
BinaryDigits[len_][n_] := BinaryDigits[n, len];

(**************************************************************************************************)

PublicFunction[BitAndQ]

BitAndQ[a_, b_] := Total[BitAnd[a, b]] =!= 0;

(**************************************************************************************************)

PublicFunction[BitNandQ]

BitNandQ[a_, b_] := Total[BitAnd[a, b]] === 0;
BitNandQ[a___] := DuplicateFreeQ[{a}, BitAndQ];

(**************************************************************************************************)

PublicFunction[RangePartitionIndices]

RangePartitionIndices[n_] := Scope[
  CollectTo[{$partBag}, rangPartRecurse[{}, Range[n]]];
  $partBag
];

rangPartRecurse[parts_, {}] := StuffBag[$partBag, parts];
rangPartRecurse[parts_, rem:{_}] := StuffBag[$partBag, Append[parts, rem]];
rangPartRecurse[parts_, rem_] := Scope @ Scan[
  {first, rest} = TakeDrop[rem, 1];
  rangPartRecurse[
    Append[parts, Join[first, #]],
    Complement[rest, #]
  ]&,
  Subsets[rest, {1, Infinity}]
];

(**************************************************************************************************)

PublicFunction[SignedPermutations]

SignedPermutations[list_List] := Catenate @ Map[SignedLists, Permutations @ list];

(**************************************************************************************************)

PublicFunction[SignedLists]

SignedLists[list_] := MapIndices[Inverted, Subsets @ Range @ Len @ list, list];
