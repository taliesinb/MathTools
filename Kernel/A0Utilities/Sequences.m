PublicFunction[Trans]

Trans[seq___List] := Transpose[{seq}];
Trans[___] := BadArguments[];

(**************************************************************************************************)

PublicFunction[SeqLength]

SeqLength[]        := 0;
SeqLength[_]       := 1;
SeqLength[_, _]    := 2;
SeqLength[_, _, _] := 3;
s_SeqLength        := Length[Unevaluated @ s];

(**************************************************************************************************)

PublicSpecialFunction[HoldSeqLength]

SetHoldAll[HoldSeqLength];

HoldSeqLength[]        := 0;
HoldSeqLength[_]       := 1;
HoldSeqLength[_, _]    := 2;
HoldSeqLength[_, _, _] := 3;
s_HoldSeqLength        := Length[s];

(**************************************************************************************************)

PublicFunction[SeqFirst, SeqLast, SeqMost, SeqRest]

SeqFirst[a_, ___] := a;
SeqLast[___, a_]  := a;
SeqMost[a___, _]  := a;
SeqRest[_, a___]  := a;

(**************************************************************************************************)

PublicFunction[HoldSeqFirst, HoldSeqLast, HoldSeqMost, HoldSeqRest]

SetHoldAll[HoldSeqFirst, HoldSeqLast, HoldSeqMost, HoldSeqRest];

HoldSeqFirst[a_, ___] := a;
HoldSeqLast[___, a_]  := a;
HoldSeqMost[a___, _]  := a;
HoldSeqRest[_, a___]  := a;

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

PublicFunction[PrependAppend]

PrependAppend[{}, a_, b_] := {a, b};
PrependAppend[list_, a_, b_] := Append[Prepend[list, a], b];
PrependAppend[a_, b_][list_] := PrependAppend[list, a, b];

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

(**************************************************************************************************)

PublicFunction[SequenceTake, SequenceDrop]

SequenceTake[r_][seq___] := Sequence @@ Take[{seq}, r];
SequenceDrop[r_][seq___] := Sequence @@ Drop[{seq}, r];

(**************************************************************************************************)

PublicFunction[ReplaceInSequence]

ReplaceInSequence[_][] := Sequence[];
ReplaceInSequence[rule_][seq___] := Sequence @@ Replace[{seq}, rule, {1}];

(**************************************************************************************************)

PublicFunction[DeleteDuplicateOptionKeys]

DeleteDuplicateOptionKeys[] := Sequence[];
DeleteDuplicateOptionKeys[seq___] := Sequence @@ DeleteDuplicatesBy[{seq}, First /* toStringKey];

toStringKey = Case[
  s_Symbol := SymbolName[s];
  str_String := str;
  e_ := e;
]

