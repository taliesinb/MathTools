PublicFunction[Trans]

Trans = Case[
  Seq[seq___List] := Transpose[{seq}];
];

(**************************************************************************************************)

PublicFunction[TakeSequence]

SetRelatedSymbolGroup[TakeSequence, DropSequence];

TakeSequence[list_, start_:1] := Table[Take[list, i], {i, start, Length @ list}];

(**************************************************************************************************)

PublicFunction[DropSequence]

DropSequence[list_] := Table[Drop[list, i], {i, 0, Length[list] - 1}];

(**************************************************************************************************)

PublicFunction[FirstRest]

SetRelatedSymbolGroup[FirstRest, FirstLast, MostLast];

SetUsage @ "
FirstRest[list$] gives the pair {%First[list$], %Rest[list$]}.
"

FirstRest[list_] := {First @ list, Rest @ list};

(**************************************************************************************************)

PublicFunction[FirstLast]

SetUsage @ "
FirstLast[list$] gives the pair {%First[list$], %Last[list$]}.
"

FirstLast[list_] := {First @ list, Last @ list};

(**************************************************************************************************)

PublicFunction[MostLast]

SetUsage @ "
MostLast[list$] gives the pair {%Most[list$], %Last[list$]}.
"

MostLast[list_] := {Most @ list, Last @ list};

(**************************************************************************************************)

PublicFunction[AppendFirst]

AppendFirst[{}] := {};
AppendFirst[list_] := Append[list, First @ list];

PublicFunction[PrependLast]

PrependLast[{}] := {};
PrependLast[list_] := Prepend[list, Last @ list];

(**************************************************************************************************)

PublicFunction[DropWhile]

SetUsage @ "
DropWhile[{e$1, e$2, $$}, f$] drops the initial elements e$i that all yield f$[ei$] = True.
"

DropWhile[list_, f_] := Drop[list, LengthWhile[list, f]];

(**************************************************************************************************)

PublicFunction[CommonPrefix, CommonPrefixLength]

CommonPrefix[{}] := None;
CommonPrefix[{e_}] := e;
CommonPrefix[e_List] := Take[First @ e, CommonPrefixLength[e]];

CommonPrefixLength[{}] := 0;
CommonPrefixLength[{e_}] := Length @ e;
CommonPrefixLength[list_] := Scope[
  n = 1; minLen = Min @ Map[Length, list];
  Do[
    If[NotAllEqualQ[Take[list, All, n]], Return[n-1, Block]],
    {n, minLen}
  ];
  minLen
];

(**************************************************************************************************)

PublicFunction[CommonSuffix, CommonSuffixLength]

CommonSuffix[{}] := None;
CommonSuffix[{e_}] := e;
CommonSuffix[e_List] := Take[First @ e, -CommonSuffixLength[e]];

CommonSuffixLength[{}] := 0;
CommonSuffixLength[{e_}] := Length @ e;
CommonSuffixLength[list_] := Scope[
  n = 1; minLen = Min @ Map[Length, list];
  Do[
    If[NotAllEqualQ[Take[list, All, -n]], Return[n-1, Block]],
    {n, minLen}
  ];
  minLen
];

(**************************************************************************************************)

PublicFunction[DeleteNull]

SetUsage @ "
DeleteNull[list$] removes any elements that are Null from list$.
"

DeleteNull[e_] := DeleteCases[e, Null];

(**************************************************************************************************)

PublicFunction[SequenceRiffle]

SequenceRiffle[r_] := Sequence[];
SequenceRiffle[a_, r_] := Sequence[r];
SequenceRiffle[a_, b_, r_] := Sequence[a, r, b];
SequenceRiffle[a_, b_, c_, r_] := Sequence[a, r, b, r, c];
SequenceRiffle[a_, b_, c_, d_, r_] := Sequence[a, r, b, r, c, r, d];
SequenceRiffle[seq__, r_] := Sequence @@ Riffle[{seq}, r];