PublicFunction[EquivalenceClassIndices]

EquivalenceClassIndices[list_, fn_] :=
  Gather[Range @ Len @ list, fn[Part[list, #1], Part[list, #2]]&];

(**************************************************************************************************)
  
PublicFunction[EquivalenceClassLabels]

EquivalenceClassLabels[list_] := Scope[
  n = Max[list];
  arr = Repeat[0, n];
  ScanIndexed[Set[Part[arr, #1], F[#2]]&, list];
  arr
]

(**************************************************************************************************)

PublicFunction[ArrayLabelIndices]

SetUsage @ "
ArrayLabelIndices[array$, labels$] gives an array of the same shape as array$, whose values are indices of labels$.
* %Part[result$, p$] = i$ if %Part[array$, p$] = %Part[labels$, i$].
* Scalars not present in labels$ are left unchanged.
* ArrayLabelIndices is the inverse of %ArrayLabeling.
"

ArrayLabelIndices[array_, labels_] :=
  VectorReplace[array, RuleRange @ labels];

ArrayLabelIndices[array_, labels_, level_] :=
  Rep[array, RuleRange @ labels, List[level]];

(**************************************************************************************************)

PublicFunction[ArrayLabeling]

SetUsage @ "
ArrayLabeling[list$] gives the result {indices$, assoc$}, where indices$ is a list the same length as array$, \
and assoc$ is an assocation whose values are indices and whose keys are elements of array$.
ArrayLabeling[array$, level$] examines the array$ at level$i, yielding an array of indices of depth level$.
* %Part[indices$, p$] = i$ if %Part[array$, p$] = assoc$[i].
* ArrayLabeling is the inverse of %ArrayLabelIndices.
"

ArrayLabeling[array_, level_:1] := Scope[
  assoc = <||>;
  List[
    Map[
      e |-> Lookup[assoc, Key @ e, assoc[e] = Len[assoc] + 1],
      array, {level}
    ],
    assoc
  ]
];

(**************************************************************************************************)

(* add ability for PositionIndex to index at level 2 *)
Unprotect[PositionIndex];
PositionIndex[list_, 2] := Scope[
  assoc = <||>;
  ScanIndexed[
    {e, part} |-> KAppTo[assoc, e, F @ part],
    list, {2}
  ];
  assoc
];
Protect[PositionIndex];

(**************************************************************************************************)

PublicFunction[ExtractIndices]

SetUsage @ "
ExtractIndices[array$, indices$] gives a list of the parts of array$ given by indices$.
* indices$ can be an array of any depth, whose values are positive integer parts.
"

ExtractIndices[array_, indices_List /; VecQ[indices, NonNegativeMachineIntegerQ]] :=
  Part[array, indices];

ExtractIndices[array_, indices_List /; MatrixQ[indices, NonNegativeMachineIntegerQ]] :=
  Part[array, #]& /@ indices;

ExtractIndices[array_, indices_List] := Map[Part[array, #]&, indices, {-1}]

(**************************************************************************************************)

PublicFunction[InvertIndex]

InvertIndex[assoc_Assoc] :=
  Merge[ReverseRules @ FlattenIndex @ assoc, Id];

(**************************************************************************************************)

PublicFunction[FlattenIndex]

FlattenIndex[assoc_] :=
  Flatten @ KVMap[{k, v} |-> Map[k -> #&, v], assoc];

(**************************************************************************************************)

PublicFunction[FirstIndex]

SetUsage @ "
FirstIndex[{e$1, e$2, $$}, patt$] gives the first i$ for which e$i matches patt$.
"

SetAttributes[FirstIndex, HoldRest];
FirstIndex[list_, pattern_, default_:None] :=
  F @ FirstPosition[list, pattern, {default}, 1, Heads -> False]

(**************************************************************************************************)

PublicFunction[IndexIn]

(* like IndexOf, but arguments work the other way around, and curries the other way *)
IndexIn[item_, index_] := FirstPosition[index, item, Null, {1}];
IndexIn[index_][item_] := IndexIn[item, index];

(**************************************************************************************************)

PublicFunction[DuplicateIndices, DuplicateIndicesBy]

DuplicateIndices[list_List | list_Assoc] := DeleteCases[{_}] @ Values @ PositionIndex @ list;

DuplicateIndicesBy[list_List | list_Assoc, fn_] := DuplicateIndices @ Map[fn, list];

(**************************************************************************************************)

PublicFunction[Duplicates, DuplicatesBy]

Duplicates[list_List] := DeleteCases[{_}] @ Gather[list];

DuplicatesBy[list_List, fn_] := DeleteCases[{_}] @ GatherBy[list, fn];

(**************************************************************************************************)

PublicFunction[GatherAgainst]

GatherAgainst[list_List, against_List] :=
  KeyValueMap[SameLenMsg[list, against], Part[list, #2]&, PositionIndex @ against];


