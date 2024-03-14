PublicFunction[Trans]

Trans[seq___List] := Transpose[{seq}];
Trans[___] := BadArguments[];

(**************************************************************************************************)

PublicFunction[SeqLength]

SeqLength[]        := 0;
SeqLength[_]       := 1;
SeqLength[_, _]    := 2;
SeqLength[_, _, _] := 3;
s_SeqLength        := Len[Uneval @ s];

(**************************************************************************************************)

PublicSpecialFunction[HoldSeqLength]

SetHoldAll[HoldSeqLength];

HoldSeqLength[]        := 0;
HoldSeqLength[_]       := 1;
HoldSeqLength[_, _]    := 2;
HoldSeqLength[_, _, _] := 3;
s_HoldSeqLength        := Len[Uneval @ s];

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

TakeSequence[list_, start_:1] := Table[Take[list, i], {i, start, Len @ list}];

(**************************************************************************************************)

PublicFunction[DropSequence]

DropSequence[list_] := Table[Drop[list, i], {i, 0, Len[list] - 1}];

(**************************************************************************************************)

PublicFunction[FirstRest]

SetRelatedSymbolGroup[FirstRest, FirstLast, MostLast];

SetUsage @ "
FirstRest[list$] gives the pair {%First[list$], %Rest[list$]}.
"

FirstRest[list_] := {F @ list, Rest @ list};

(**************************************************************************************************)

PublicFunction[FirstLast]

SetUsage @ "
FirstLast[list$] gives the pair {%First[list$], %Last[list$]}.
"

FirstLast[list_] := {F @ list, L @ list};

(**************************************************************************************************)

PublicFunction[MostLast]

SetUsage @ "
MostLast[list$] gives the pair {%Most[list$], %Last[list$]}.
"

MostLast[list_] := {Most @ list, L @ list};

(**************************************************************************************************)

PublicFunction[AppendFirst]

AppendFirst[{}] := {};
AppendFirst[list_] := App[list, F @ list];

PublicFunction[PrependLast]

PrependLast[{}] := {};
PrependLast[list_] := Pre[list, L @ list];

(**************************************************************************************************)

PublicFunction[PrependAppend]

PrependAppend[{}, a_, b_] := {a, b};
PrependAppend[list_, a_, b_] := App[Pre[list, a], b];
PrependAppend[a_, b_][list_] := PrependAppend[list, a, b];

(**************************************************************************************************)

PublicFunction[DropWhile]

SetUsage @ "
DropWhile[{e$1, e$2, $$}, f$] drops the initial elements e$i that all yield f$[ei$] = True.
"

DropWhile[list_, f_] := Drop[list, LengthWhile[list, f]];

(**************************************************************************************************)

PublicFunction[CommonPrefix, CommonSuffix]

SetUsage @ "CommonPrefix[{e$1, e$2, $$}] gives the expression that is the longest common prefix of all the e$i."
SetUsage @ "CommonSuffix[{e$1, e$2, $$}] gives the expression that is the longest common suffix of all the list$i."

CommonPrefix[list_List] := commonPrefixSuffix[list, 1];
CommonSuffix[list_List] := commonPrefixSuffix[list, -1];

commonPrefixSuffix[{}, _] := {};
commonPrefixSuffix[{e_}, _] := e;
commonPrefixSuffix[list_, mult_] := Take[F @ list, mult * commonPrefixSuffixLen[list, mult]];

(**************************************************************************************************)

PublicFunction[CommonPrefixLength, CommonSuffixLength]

SetUsage @ "CommonPrefixLength[{e$1, e$2, $$}] gives the length of the expression that is the longest common prefix of all the e$i."
SetUsage @ "CommonSuffixLength[{e$1, e$2, $$}] gives the length of the expression that is the longest common suffix of all the e$i."

CommonPrefixLength[list_List] := commonPrefixSuffixLen[list, 1];
CommonSuffixLength[list_List] := commonPrefixSuffixLen[list, -1];

commonPrefixSuffixLen[{}, _] := 0;
commonPrefixSuffixLen[{list_}, _] := Len @ list;
commonPrefixSuffixLen[list_, mult_] := Module[
  {min = Min @ Map[Len, list]},
  Do[
    If[NotAllEqualQ[Part[list, All, mult * n]], Return[n-1, Module]],
    {n, min}
  ];
  min
];

(**************************************************************************************************)

PublicFunction[DeleteNull]

SetUsage @ "
DeleteNull[list$] removes any elements that are Null from list$.
"

DeleteNull[e_] := Decases[e, Null];

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
ReplaceInSequence[rule_][seq___] := Sequence @@ Rep[{seq}, rule, {1}];

(**************************************************************************************************)

PublicFunction[DeleteDuplicateOptionKeys]

DeleteDuplicateOptionKeys[] := Sequence[];
DeleteDuplicateOptionKeys[seq___] := Sequence @@ DedupBy[{seq}, F /* toStringKey];

toStringKey = Case[
  s_Symbol := SymbolName[s];
  str_Str := str;
  e_ := e;
]

