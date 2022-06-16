PackageExport["TakeSequence"]

SetRelatedSymbolGroup[TakeSequence, DropSequence];

TakeSequence[list_, start_:1] := Table[Take[list, i], {i, start, Length @ list}];

(**************************************************************************************************)

PackageExport["DropSequence"]

DropSequence[list_] := Table[Drop[list, i], {i, 0, Length[list] - 1}];

(**************************************************************************************************)

PackageExport["FirstRest"]

SetRelatedSymbolGroup[FirstRest, FirstLast, MostLast];

SetUsage @ "
FirstRest[list$] gives the pair {%First[list$], %Rest[list$]}.
"

FirstRest[list_] := {First @ list, Rest @ list};

(**************************************************************************************************)

PackageExport["FirstLast"]

SetUsage @ "
FirstLast[list$] gives the pair {%First[list$], %Last[list$]}.
"

FirstLast[list_] := {First @ list, Last @ list};

(**************************************************************************************************)

PackageExport["MostLast"]

SetUsage @ "
MostLast[list$] gives the pair {%Most[list$], %Last[list$]}.
"

MostLast[list_] := {Most @ list, Last @ list};

(**************************************************************************************************)

PackageExport["AppendFirst"]

AppendFirst[{}] := {};
AppendFirst[list_] := Append[list, First @ list];

PackageExport["PrependLast"]

PrependLast[{}] := {};
PrependLast[list_] := Prepend[list, Last @ list];

(**************************************************************************************************)

PackageExport["DropWhile"]

SetUsage @ "
DropWhile[{e$1, e$2, $$}, f$] drops the initial elements e$i that all yield f$[ei$] = True.
"

DropWhile[list_, f_] := Drop[list, LengthWhile[list, f]];

(**************************************************************************************************)

PackageExport["CommonPrefix"]
PackageExport["CommonPrefixLength"]

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

PackageExport["CommonSuffix"]
PackageExport["CommonSuffixLength"]

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
