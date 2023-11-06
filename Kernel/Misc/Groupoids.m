PublicFunction[GroupoidQ]

SetUsage @ "
GroupoidQ[groupoid$] returns True if groupoid$ is a valid groupoid.
"

GroupoidQ[e_ ? GroupQ] := True;

(* general plan: remove these mirror states, which don't make much sense. i can always rescue the code
from git history anyway.

instead: have a sparse groupoid and a dense groupoid. a sparse groupoid has a cayley function that creates
exactly the right successors. a dense groupoid gives an association with keys being cardinals (or their inversions)
and values being functions, which should return NullElement if they can't act on a given state.

then we construct the cayley function from this automatically.

both kinds should list their cardinals ahead of time.
*)

(**************************************************************************************************)

PublicFunction[GroupoidGenerator]

SetUsage @ "
GroupoidGenerator[f$, g$] represents an element of a groupoid that acts on a GroupoidState[$$] by f$, and
acts on a GroupoidMirrorState[$$] by g$.
"

gen_GroupoidGenerator[states_List] := Map[gen, states];
GroupoidGenerator[f_, g_][s_] := makeState[f, s];
GroupoidGenerator[f_, g_][Inverted[s_]] := Inverted @ makeState[g, s];

ToInverseFunction[GroupoidGenerator[f_, g_]] :=
  GroupoidGenerator[g, f];

makeState[f_, s_] := Scope[
  s2 = f[s];
  If[s2 === s || s2 === Nothing, Inverted[s], s2]
];

declareFormatting[
  GroupoidGenerator[f_, g_] :> RawBoxes @ FractionBox[ToBoxes @ f, ToBoxes @ g]
];

(**************************************************************************************************)

PublicObject[GroupoidObject]

PrivateFunction[constructGroupoid]

constructGroupoid[assoc_] := Scope[
  assoc = assoc;
  If[!KeyExistsQ[assoc, "States"], assoc["States"] = Indeterminate];
  If[!KeyExistsQ[assoc, "Generators"],
    assoc["Generators"] = If[KeyExistsQ[assoc, "MirroredGenerators"],
      Part[assoc["MirroredGenerators"], All, 1],
      None
    ];
  ];
  ConstructNoEntry[GroupoidObject, assoc]
];

declareObjectPropertyDispatch[GroupoidObject, groupoidProperty];

groupoidProperty[data_, "CayleyFunction", opts___Rule] :=
  computeCayleyFunction[data, opts];

groupoidProperty[data_, "CayleyGraph", opts___Rule] :=
  computeCayleyGraph[data, opts];

groupoidProperty[data_, "CompleteStates"] := Scope[
  states = data["States"];
  If[states === Indeterminate, Return @ Indeterminate];
  Join[states, Inverted /@ states]
];

(**************************************************************************************************)

MakeBoxes[go_GroupoidObject ? HoldNoEntryQ, form_] :=
  groupoidObjectBoxes[go, form];

groupoidObjectBoxes[object:GroupoidObject[data_], form_] := Scope[
  UnpackAssociation[data, type, generators, states, initialStates];
  BoxForm`ArrangeSummaryBox[
    GroupoidObject, object, None,
    (* Always displayed *)
    {
     {summaryItem["Type", type], SpanFromLeft},
     {summaryItem["Generators", If[generators === None, None, Len @ generators]]},
     {summaryItem["Initial states", Len @ initialStates]},
     If[states =!= Indeterminate, {summaryItem["States", Len @ states]}, Nothing]
     },
    (* Displayed on request *)
    If[generators === None, {}, {{Column[generators], SpanFromLeft}}],
    form,
    "Interpretable" -> Automatic
  ]
];

(**************************************************************************************************)

Options[computeCayleyFunction] = {"Symmetric" -> True, "Labeled" -> True};

computeCayleyFunction[data_, OptionsPattern[]] := Scope[
  UnpackAssociation[data, generators, cayleyFunction];
  If[cayleyFunction =!= Automatic, Return @ cayleyFunction];
  UnpackOptions[symmetric, labeled];
  list = Flatten @ MapIndexed[
    {gen, index} |-> {
      If[labeled, Labeled[P1 @ index], Id] @ gen,
      If[symmetric && (igen = ToInverseFunction[gen]) =!= gen,
        If[labeled, Labeled[Inverted @ P1 @ index], Id] @ igen,
        Nothing
      ]
    },
    generators
  ];
  ApplyThrough[list]
];

(**************************************************************************************************)

computeCayleyGraph[data_, opts___Rule] := Scope[
  init = data["InitialStates"];
  cfunc = computeCayleyFunction[data, "Symmetric" -> False];
  graph = MultiwaySystem[cfunc, init, "Graph", MaxVertices -> 10^3, SelfLoops -> False];
  ExtendedGraph[graph, opts]
];

(**************************************************************************************************)

PublicFunction[GroupoidPermutationGroup]

GroupoidPermutationGroup[groupoid_] := Scope[
  data = getObjectData[groupoid];
  UnpackAssociation[data, mirroredGenerators, states];
  statesBasis = Join[states, Inverted /@ states];
  PermutationGroup @ Table[
    FindPermutation[statesBasis, generator @ statesBasis],
    {generator, generators}
  ]
];

(**************************************************************************************************)

PublicFunction[GroupoidPermutationTable]

GroupoidPermutationTable[groupoid_] := Scope[
  data = getObjectData[groupoid];
  UnpackAssociation[data, generators, states];
  statesBasis = Join[states, Inverted /@ states];
  TableForm[
    # @ statesBasis& /@ generators,
    TableHeadings -> {Automatic, statesBasis}
  ]
];

(**************************************************************************************************)

PublicFunction[GroupoidObjectQ]

GroupoidObjectQ[go_GroupoidObject ? HoldNoEntryQ] := True;
GroupoidObjectQ[_] := False;

(**************************************************************************************************)

PublicFunction[ColoredTokenGroupoid]

ColoredTokenGroupoid[n_Int, colors_Int] := constructGroupoid @ Assoc[
  "Type" -> "ColorToken",
  "Generators" -> makeColoredTokenGenerators[n, colors],
  "MirroredGenerators" -> makeMirroredColoredTokenGenerators[n, colors],
  "InitialStates" -> {ColoredTokens @ Ones[n]},
  "States" -> Map[ColoredTokens, Tuples[Range[colors], n]]
];

makeColoredTokenGenerators[n_, colors_] :=
  Flatten @ Table[
    If[j === k, Nothing, SetTokenColor[i, j, k]],
    {i, 1, n}, {j, colors}, {k, colors}
  ];

makeMirroredColoredTokenGenerators[n_, colors_] :=
  Flatten @ Table[
    If[j === k, Nothing, GroupoidGenerator[SetTokenColor[i, j, k], SetTokenColor[i, k, j]]],
    {i, 1, n}, {j, colors}, {k, colors}
  ];


PublicFunction[ColoredTokens]

$TokenColorPalette = Prepend[$ColorPalette, GrayLevel[0.98]];

declareFormatting[
  ColoredTokens[list_List] :> Pane[Row[Part[$TokenColorPalette, list], " "], ContentPadding -> False, FrameMargins -> {{0, 0}, {2, 2}}]
];


PublicFunction[SetTokenColor]

SetTokenColor[n_, c_][ColoredTokens[list_]] :=
  If[list[[n]] === c, Nothing, ColoredTokens @ ReplacePart[list, n -> c]];

SetTokenColor[n_, c1_, c2_][ColoredTokens[list_]] :=
  If[list[[n]] != c1, Nothing, ColoredTokens @ ReplacePart[list, n -> c2]];

(**************************************************************************************************)

PublicFunction[PermutationActionGroupoid]

PermutationActionGroupoid[initialStates_List, generators_:Automatic] := Scope[
  If[!MatrixQ[initialStates], initialStates = List @ initialStates];
  n = Len @ P1 @ initialStates;
  generators //= toPermutationGenerators;
  If[FailureQ[generators], ReturnFailed[]];
  generatorsAndLabels = Map[{#, toPermutationForm @ #}&, generators];
  cayleyFunction = PermutationActionCayleyFunction[generatorsAndLabels];
  constructGroupoid @ Assoc[
    "Type" -> "Permutation",
    "CayleyFunction" -> cayleyFunction,
    "InitialStates" -> initialStates
  ]
]

toPermutationGenerators = Case[
  Automatic       := Partition[Range @ n, 2, 1];
  "Cyclic"        := {Cycles @ {Range @ n}};
  list:{__List}   := Cycles /@ list;
  c:{__Cycles}    := simplifyCycles /@ c;
  g_ ? GroupQ     := % @ GroupGenerators @ g;
  g_Graph         := DeleteDuplicates[Sort /@ AdjacentPairs[g]];
  _               := $Failed;
]

simplifyCycles = Case[
  Cycles[{{a_, b_}}] := {a, b};
  other_             := other;
];

toPermutationForm = Case[
  {a_Int, b_Int} := TranspositionForm[a, b];
  {a__Int}       := PermutationCycleForm[a];
  other_         := PermutationForm[other];
]

(**************************************************************************************************)

PublicFunction[PermutationActionCayleyFunction]

PermutationActionCayleyFunction[gens_][state_] :=
  Labeled[applyPermutation[state, #], #2]& @@@ gens;

applyPermutation[vec_, {i_, j_}] :=
  ReplacePart[vec, {i -> Part[vec, j], j -> Part[vec, i]}];

applyPermutation[vec_, c_Cycles] :=
  Permute[vec, c];

(**************************************************************************************************)

PublicFunction[CardinalRewriteGroupoid]

CardinalRewriteGroupoid[cardinals_List, initial_, rewriteCount_:1] := Scope[
  Which[
    ListQ[initial],
      n = Len @ initial;
      initialStates = {initial},
    IntegerQ[initial],
      n = initial;
      initialStates = All,
    True,
      ReturnFailed[];
  ];
  unsignedTuples = Select[DuplicateFreeQ] @ Tuples[cardinals, {n}];
  possibleIndices = Subsets[Range @ n];
  allStates = JoinMap[
    MapIndices[Inverted, possibleIndices, #]&,
    unsignedTuples
  ];
  SetAll[initialStates, allStates];
  SetAll[rewriteCount, n];
  cayleyFunction = CardinalRewriteCayleyFunction[allStates, toCountFilter  @ rewriteCount];
  constructGroupoid @ Assoc[
    "Type" -> "CardinalRewrite",
    "CayleyFunction" -> cayleyFunction,
    "InitialStates" -> initialStates,
    "States" -> allStates
  ]
];

toCountFilter = Case[
  i_Int | {i_Int} := EqualTo[i];
  {i_, j_} := Between[{i, j}];
  _ := $Failed;
];

canonicalCardinalTransition[rules_] := Scope[
  rules = DeleteCases[rules, z_ -> z_];
  reps = Sort /@ {rules, ReverseRules @ rules, invertRules @ rules, ReverseRules @ invertRules @ rules};
  minIndex = MinimumIndex[reps];
  If[MatchQ[minIndex, 2 | 4], Inverted, Id] @ CardinalTransition @ Part[reps, minIndex]
];

invertRules[rules_] := MatrixMap[Inverted, rules];

PublicFunction[CardinalRewriteCayleyFunction]

CardinalRewriteCayleyFunction[allStates_, countFilter_][state_] := Scope[
  successors = Select[allStates, countFilter @ HammingDistance[#, state]&];
  labels = canonicalCardinalTransition[RuleThread[state, #]]& /@ successors;
  MapThread[Labeled, {successors, labels}]
];

