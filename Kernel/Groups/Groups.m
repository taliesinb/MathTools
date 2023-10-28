PrivateFunction[makeGenerators, makeIdentityRepresentation]

(* this is the fallback for groups that don't have specific implementations *)
makeGenerators[group_] :=
  RangeAssociation @ PermutationGroupMatrices @ group;

makeIdentityRepresentation[group_] := None;

addLabels = Case[
  assoc_Association := assoc;
  list_List := RangeAssociation @ list;
];

$posIntOrInfinity = (_Integer ? Positive) | Infinity;

$IntZ = TemplateBox[{}, "Integers"] // RawBoxes;

supNo1[str_, 1] := str;
supNo1[str_, n_] := Superscript[str, n];

subSupNo1[str_, n_, 1] := Subscript[str, n];
subSupNo1[str_, n_, m_] := Subsuperscript[str, n, m];

GroupOrder;
Unprotect[GroupOrder];

(* since we are adding Infinity support to CyclicGroup and AbelianGroup, we must
prevent these RuleCondition-based messages *)
GroupTheory`PermutationGroups`Private`CheckGroupDegree[head_][customGroup_ ? CustomGroupQ] :=
  CustomGroupOrder[customGroup];

(**************************************************************************************************)

PublicFunction[GroupGeneratorElements]

SetUsage @ "
GroupGeneratorElements[group$] returns the RepresentationElement[$$] objects corresponding to its generators.
"

GroupGeneratorElements[group_] := Scope[
  rep = LinearGroupRepresentation[group];
  If[!RepresentationObjectQ[rep], ReturnFailed[]];
  rep["Generators"]
];


(* PublicFunction[QuaternionGroup]

SetUsage @ "
QuaternionGroup[] represents the group of unit quaternions.
"
 *)

(**************************************************************************************************)

PublicFunction[GroupQ]

SetUsage @ "
GroupQ[group$] returns True if group$ is a valid group.
"

GroupQ[HoldPattern @ PermutationGroup[{__Cycles}]] := True;
GroupQ[e_] := GroupTheory`PermutationGroups`Private`NamedGroupQ[e];

CustomGroupQ[_] := False;

(**************************************************************************************************)

PublicFunction[AbelianGroupQ]

SetUsage @ "
AbelianGroupQ[group$] returns True if group$ is an Abelian group.
"

AbelianGroupQ[group_] := MatchQ[group,
  CyclicGroup[$posIntOrInfinity] | AbelianGroup[{Repeated[$posIntOrInfinity]}] | InfiniteAbelianGroup[_Integer] |
  GroupDirectProduct[list_List /; VectorQ[list, AbelianGroupQ]]
];

(**************************************************************************************************)

(* Framework to set up custom groups *)

declareGroup[rules__RuleDelayed] := Scan[declareGroup, {rules}];

declareGroup[pattern_ :> {"Generators" :> generators_, "Order" :> order_, "Format" :> format_}] := (
  CustomGroupQ[HoldPattern @ pattern] := True;
  GroupQ[HoldPattern @ pattern] := True;
  makeGenerators[HoldPattern @ pattern] := generators;
  GroupOrder[HoldPattern @ pattern] := order;
  CustomGroupOrder[HoldPattern @ pattern] := order;
  declareFormatting[pattern :> format]
);

declareGroup[___] := Panic["BadDeclareGroup"];

(**************************************************************************************************)

(* Adds support to AbelianGroup for Infinity *)

declareGroup[
  AbelianGroup[orders:{Repeated[$posIntOrInfinity]}] :> {
    "Generators" :> constructDirectProductGenerators[Apply[makeAbelianGeneratorBlocks, orders]],
    "Order" :> RuleCondition[Infinity, MemberQ[orders, Infinity]],
    "Format" :> Row[makeAbelianSymbol /@ orders, "\[CirclePlus]"]
  }
]

makeAbelianGeneratorBlocks[n_Integer, rest___] :=
  Prepend[makeAbelianGeneratorBlocks[rest], makeCylicGenerators[n]];

makeAbelianGeneratorBlocks[infs:Longest[Infinity..], rest___] :=
  Prepend[makeAbelianGeneratorBlocks[rest], makeInfiniteAbelianGenerators[SeqLength[infs]]]

makeAbelianGeneratorBlocks[] := {};

(**************************************************************************************************)

(* Adds support to CyclicGroup for Infinity *)

declareGroup[
  CyclicGroup[n:$posIntOrInfinity] :> {
    "Generators" :> makeCylicGenerators[n],
    "Order" :> n,
    "Format" :> makeAbelianSymbol[n]
  }
];

makeAbelianSymbol[n_Integer] := Subscript[$IntZ, n];
makeAbelianSymbol[Infinity] := $IntZ;

makeCylicGenerators[n_Integer] := <|1 -> {{UnitRoot[n]}}|>;
makeCylicGenerators[Infinity] := makeInfiniteAbelianGenerators[1];


(* add formating for some existing groups *)

declareFormatting[
  AlternatingGroup[n_] :> Subscript[Style["A", Italic], n],
  SymmetricGroup[n_] :> Subscript[Style["S", Italic], n],
  DihedralGroup[n_] :> Subscript[MathTextForm @ "Dih", n]
];

(**************************************************************************************************)

PublicFunction[GroupDirectProduct]

SetUsage @ "
GroupDirectProduct[{g$1, $$, g$n}] represents the product of several groups.
* The generators of the product is the union of generators of the g$i.
* GroupDirectProduct works with LinearGroupRepresentation.
* GroupDirectProduct does not work with the other group theory functions.
";

declareGroup[
  GroupDirectProduct[groups:{Repeated[_ ? GroupQ]}] :> {
    "Generators" :> constructDirectProductGenerators[makeGenerators /@ groups],
    "Order" :> Times @@ Map[GroupOrder, g],
    "Format" :> Apply[If[VectorQ[list, AbelianGroupQ], CirclePlus, CircleTimes], Map[maybeBracket, groups]]
  }
];

constructDirectProductGenerators[generatorLists_List] := Scope[
  identities = IdentityMatrix[Length[First[#]]]& /@ generatorLists;
  labelFn = If[MatchQ[Length /@ generatorLists, {1..}], #2&, Subscript];
  Association @ MapIndex1[
    {genList, i} |-> KeyValueMap[
      {label, gen} |-> (
        labelFn[label, i] -> BlockDiagonalMatrix2[ReplacePart[identities, i -> gen]]
      ),
      genList
    ],
    generatorLists
  ]
];

maybeBracket /: MakeBoxes[maybeBracket[e_], form:StandardForm | TraditionalForm] := Scope[
  subbox = MakeBoxes[e, form];
  If[MatchQ[subbox, TemplateBox[_, "RowWithSeparators"]], RBox["(", subbox, ")"], subbox]
];

(**************************************************************************************************)

PublicFunction[DiscreteHeisenbergGroup]

SetUsage @ "
DiscreteHeisenbergGroup[] represents the Heisenberg group of 3 \[Times] 3 upper-unitriangular matrices with integer entries.
* DiscreteHeisenbergGroup works with LinearGroupRepresentation.
* DiscreteHeisenbergGroup does not work with the other group theory functions, since it has no finite permutation representation.
"

declareGroup[
  DiscreteHeisenbergGroup[] :> {
    "Generators" :> RangeAssociation @ MapIndex1[AugmentedIdentityMatrix[3, #]&, {{1, 2}, {1, 3}, {2, 3}}],
    "Order" :> Infinity,
    "Format" :> Style["H", Italic]
  }
];

(**************************************************************************************************)

PublicFunction[InfiniteAbelianGroup]

SetUsage @ "
InfiniteAbelianGroup[n$] represents an infinite Abelian group on n$ generators.
InfiniteAbelianGroup[n$, 'Redundant'] uses a redundant representation by forming pairs of generators cyclically.
* InfiniteAbelianGroup works with LinearGroupRepresentation.
* InfiniteAbelianGroup does not work with the other group theory functions, since it has no finite permutation representation.
"

declareGroup[
  InfiniteAbelianGroup[n_ ? PositiveIntegerQ] :> {
    "Generators" :> makeInfiniteAbelianGenerators[n],
    "Order" :> Infinity,
    "Format" :> supNo1[$IntZ, n]
  },
  InfiniteAbelianGroup[n_ ? PositiveIntegerQ, "Redundant"] :> {
    "Generators" :> makeInfiniteRedundantAbelianGenerators[n],
    "Order" :> Infinity,
    "Format" :> supNo1[Row[{$IntZ, "*"}], n]
  }
];

makeInfiniteAbelianGenerators[n_] :=
  RangeAssociation @ Array[UnitTranslationMatrix[n, #]&, n];

makeInfiniteRedundantAbelianGenerators[n_] :=
  RangeAssociation @ Array[RedundantUnitTranslationMatrix[n, #]&, n];

(**************************************************************************************************)

(* PublicFunction[InfiniteDihedralGroup]

SetUsage @ "
InfiniteDihedralGroup[] represents the infinite dihedral group.
* InfiniteDihedralGroup works with GroupRepresentation.
* InfiniteDihedralGroup does not work with the other group theory functions, since it has no finite permutation representation.
"
 *)
declareGroup[
  DihedralGroup[Infinity] :> {
    "Generators" :> <|"t" -> {{1, 1}, {0, 1}}, "r" -> {{1, 0}, {0, -1}}|>,
    "Order" :> Infinity,
    "Format" :> Subscript[MathTextForm @ "Dih", Infinity]
  }
];

(**************************************************************************************************)

PublicFunction[TranslationGroup]

SetUsage @ "
TranslationGroup[vectors$] represents an Abelian translation group generated by the translation vectors vectors$.
TranslationGroup[RootSystem[$$]] uses the simple roots of a given root system.
TranslationGroup[$$, 'Redundant'] uses a redundant representation by forming pairs of translations cyclically.
TranslationGroup[$$, modulus$] applies a modulus to all translations.
* The group operation is given by composing translations.
* A generator acts by translating: adding its corresponding vector to its operand.
* In one dimension this is the InfiniteAbelianGroup[1].
"

TranslationGroup[rs_ ? RootSystemObjectQ] :=
  TranslationGroup @ rs["TranslationMatrices"];

TranslationGroup[vecs_ ? MatrixQ, "Redundant"] :=
  TranslationGroup @ MakeRedundantTranslations @ vecs;

TranslationGroup[vecs_, None] := TranslationGroup[vecs];

declareGroup[
  TranslationGroup[vecs_ ? MatrixOrTableQ] :> {
    "Generators" :> addLabels @ Map[TranslationMatrix, vecs],
    "Order" :> Infinity,
    "Format" :> Subsuperscript["T", Length @ vecs, Length @ First @ vecs]
  },
  TranslationGroup[vecs_ ? MatrixOrTableQ, mod_ /; VectorQ[mod] || IntegerQ[mod]] :> {
    "Generators" :> addLabels @ Map[TranslationMatrix[#, mod]&, vecs],
    "Order" :> Infinity,
    "Format" :> Subsuperscript["T%", Length @ vecs, Length @ First @ vecs]
  }
];

MatrixOrTableQ = Case[
  a_List ? MatrixQ := True;
  a_Association /; MatrixQ[Values @ a] := True;
  _ := False
];

(**************************************************************************************************)

PublicFunction[TranslationGroupQ]

SetUsage @ "
TranslationGroupQ[group$] returns True if group$ is a TranslationGroup.
"

TranslationGroupQ[TranslationGroup[_]] := True;
TranslationGroupQ[_] := False;

(**************************************************************************************************)

PublicFunction[ReflectionGroup]

SetUsage @ "
ReflectionGroup[vectors$] represents an reflection group generated by the reflection vectors vectors$.
ReflectionGroup[RootSystem[$$]] uses the positive roots of a given root system.
* The group operation is given by composing reflections.
* A generator acts by reflecting: reflecting its operand by a hyperplane perpendicular to its corresponding vector.
"

declareGroup[
  ReflectionGroup[vecs_ ? MatrixQ] :> {
    "Generators" :> Map[TranslationMatrix, vecs],
    "Order" :> Infinity,
    "Format" :> Subsuperscript["R", Length @ vecs, Length @ First @ vecs]
  },
  ReflectionGroup[rs_ ? RootSystemObjectQ] :> {
    "Generators" :> rs["ReflectionMatrices"],
    "Order" :> rs["Count"],
    "Format" :> Tooltip[Subsuperscript["R", rs["Count"]/2, rs["Dimension"]], rs]
  }
];

(**************************************************************************************************)

PublicFunction[ReflectionGroupQ]

SetUsage @ "
ReflectionGroupQ[group$] returns True if group$ is a ReflectionGroup.
"

ReflectionGroupQ[_ReflectionGroup] := True;
ReflectionGroupQ[_] := False;

(**************************************************************************************************)

PublicFunction[SignedSymmetricGroup]

SetUsage @ "
SignedSymmetricGroup[n$] represents the signed symmetric group on n$ generators.
"

declareGroup[
  SignedSymmetricGroup[n_Integer] :> {
    "Generators" :> constructSignedSymmetricGenerators[n],
    "Order" :> Factorial[n] * Power[2, n],
    "Format" :> Subsuperscript[Style["S", Italic], n, "*"]
  }
]

constructSignedSymmetricGenerators[n_] := Association @ Join[
  Array[Subscript["r", #] -> Permute[IdentityMatrix[n], Cycles[{{#, Mod[# + 1, n, 1]}}]]&, n - 1],
  Array[Subscript["f", #] -> BasisScalingMatrix[n, # -> -1]&, n]
]

(**************************************************************************************************)

PublicFunction[PermutationGroupMatrices]

PermutationGroupMatrices[group_] := Scope[
  generators = GroupGenerators[group];
  If[!ListQ[generators], ReturnFailed[]];
  max = Max[PermutationMax /@ generators];
  Normal /@ Map[cyclesToPermutationMatrix[#, max]&, generators]
];

cyclesToPermutationMatrix[Cycles[cycles_], n_] := Scope[
  edges = Map[
    cycle |-> Map[# -> 1&, Partition[cycle, 2, 1, 1]],
    cycles
  ];
  stable = Complement[Range[n], Union @@ cycles];
  stableEdges = Map[{#, #} -> 1&, stable];
  SparseArray[Flatten @ {edges, stableEdges}, {n, n}]
];

complexAbelianMatrices[dims_] := Scope[
  n = Length[dims];
  Table[
    DiagonalMatrix @ ReplacePart[Ones[n], i -> RootOfUnity[Part[dims, i]]],
    {i, n}
  ]
];

unitRootAbelianMatrices[dims_] := Scope[
  n = Length[dims];
  Table[
    DiagonalMatrix @ ReplacePart[Ones[n], i -> UnitRoot[Part[dims, i]]],
    {i, n}
  ]
];

(**************************************************************************************************)

PublicFunction[StringToWord]

StringToWord[str_String] :=
  Map[If[UpperCaseQ[#], Inverted @ ToLowerCase @ #, #]&, Characters @ str];

(**************************************************************************************************)

PublicFunction[FreeGroup]

SetUsage @ "
FreeGroup[n$] represents the free group on n$ generators.
FreeGroup[{g$1, g$2, $$}] represents the free group on specific generators.
"

FreeGroup[str_String] := FreeGroup[Characters @ str];

declareGroup[
  FreeGroup[n_Integer] :> {
    "Generators" :> makeFreeGenerators[Range @ n],
    "Order" :> Infinity,
    "Format" :> Subscript[Style["F", Italic], n]
  },
  FreeGroup[gens_List] :> {
    "Generators" :> makeFreeGenerators[gens],
    "Order" :> Infinity,
    "Format" :> Subscript[Style["F", Italic], Row[gens,","]]
  }
];

makeIdentityRepresentation[_FreeGroup] := ModForm[{{GroupWord[{}]}}, GroupWordMultiplication];

makeFreeGenerators[gens_] := AssociationMap[g |-> ModForm[{{GroupWord[{g}]}}, GroupWordMultiplication], gens];

(**************************************************************************************************)

PublicFunction[GroupWordMultiplication]

PublicHead[GroupWord]

GroupWord::usage = "GroupWord[list$] represent a word over group generators."

(* TODO: make this into a form *)
declareFormatting[
  GroupWord[w_List] :> Row[w]
]

GroupWordMultiplication[GroupWord[w1_List], GroupWord[w2_List]] := wordJoin[w1, w2];

GroupWord /: Power[GroupWord[w_List], n_] := groupWordPower[w, n];

GroupWord /: Inverted[GroupWord[w_List]] := groupWordPower[w, -1];

groupWordPower[w_List, 0] := GroupWord[{}];

groupWordPower[w_List, 1] := GroupWord[w];

groupWordPower[w_List, -1] := GroupWord[InvertReverse @ w];

groupWordPower[w_List, n_Integer ? Negative] := groupWordPower[InvertReverse @ w, -n];

groupWordPower[w_List, n_Integer ? Positive] := wordJoin @@ Repeat[w, n];

wordJoin[words__List] := GroupWord[Join[words] //. $backtrackingRules];