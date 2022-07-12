PrivateFunction[makeGenerators]

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
  rep = GroupRepresentation[group];
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
  Prepend[makeAbelianGeneratorBlocks[rest], makeInfiniteAbelianGenerators[Length[{infs}]]]

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

makeCylicGenerators[n_Integer] := {{{UnitRoot[n]}}};
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
* GroupDirectProduct works with GroupRepresentation.
* GroupDirectProduct does not work with the other group theory functions.
";

declareGroup[
  GroupDirectProduct[groups:{Repeated[_ ? GroupQ]}] :> {
    "Generators" :> constructDirectProductGenerators[makeGenerators /@ g],
    "Order" :> Times @@ Map[GroupOrder, g],
    "Format" :> Apply[If[VectorQ[list, AbelianGroupQ], CirclePlus, CircleTimes], Map[maybeBracket, groups]]
  }
];

constructDirectProductGenerators[generatorLists_] := Scope[
  identities = IdentityMatrix[Length[First[#]]]& /@ generatorLists;
  Flatten[Table[
    Map[
      gen |-> BlockDiagonalMatrix[ReplacePart[identities, i -> gen]],
      generatorLists[[i]]
    ],
    {i, Length[generatorLists]}
  ], 1]
];

maybeBracket /: MakeBoxes[maybeBracket[e_], form:StandardForm | TraditionalForm] := Scope[
  subbox = MakeBoxes[e, form];
  If[MatchQ[subbox, TemplateBox[_, "RowWithSeparators"]], RBox["(", subbox, ")"], subbox]
];

(**************************************************************************************************)

PublicFunction[DiscreteHeisenbergGroup]

SetUsage @ "
DiscreteHeisenbergGroup[] represents the Heisenberg group of 3 \[Times] 3 upper-unitriangular matrices with integer entries.
* DiscreteHeisenbergGroup works with GroupRepresentation.
* DiscreteHeisenbergGroup does not work with the other group theory functions, since it has no finite permutation representation.
"

declareGroup[
  DiscreteHeisenbergGroup[] :> {
    "Generators" :> Map[AugmentedIdentityMatrix[3, #]&, {{1, 2}, {1, 3}, {2, 3}}],
    "Order" :> Infinity,
    "Format" :> Style["H", Italic]
  }
];

(**************************************************************************************************)

PublicFunction[InfiniteAbelianGroup]

SetUsage @ "
InfiniteAbelianGroup[n$] represents an infinite Abelian group on n$ generators.
InfiniteAbelianGroup[n$, 'Redundant'] uses a redundant representation by forming pairs of generators cyclically.
* InfiniteAbelianGroup works with GroupRepresentation.
* InfiniteAbelianGroup does not work with the other group theory functions, since it has no finite permutation representation.
"

declareGroup[
  InfiniteAbelianGroup[n_ ? Internal`PositiveIntegerQ] :> {
    "Generators" :> makeInfiniteAbelianGenerators[n],
    "Order" :> Infinity,
    "Format" :> supNo1[$IntZ, n]
  },
  InfiniteAbelianGroup[n_ ? Internal`PositiveIntegerQ, "Redundant"] :> {
    "Generators" :> makeInfiniteRedundantAbelianGenerators[n],
    "Order" :> Infinity,
    "Format" :> supNo1[Row[{$IntZ, "*"}], n]
  }
];

makeInfiniteAbelianGenerators[n_] :=
  UnitTranslationMatrix[n, #]& /@ Range[n];

makeInfiniteRedundantAbelianGenerators[n_] :=
  RedundantUnitTranslationMatrix[n, #]& /@ Range[n];

(**************************************************************************************************)

PublicFunction[InfiniteDihedralGroup]

SetUsage @ "
InfiniteDihedralGroup[n$] represents an infinite dihedral group on n$ - 1 generators, and an implicit reflection.
InfiniteDihedralGroup[n$, 'Improper'] represents an infinite dihedral group on n$ generators.
InfiniteDihedralGroup[n$, 'Redundant'] uses a redundant representation by forming pairs of generators cyclically.
* InfiniteDihedralGroup works with GroupRepresentation.
* InfiniteDihedralGroup does not work with the other group theory functions, since it has no finite permutation representation.
"

declareGroup[
  InfiniteDihedralGroup[n_ ? Internal`PositiveIntegerQ] :> {
    "Generators" :> Append[BasisScalingMatrix[n + 1, -1 -> -1]] @ makeInfiniteAbelianGenerators[n],
    "Order" :> Infinity,
    "Format" :> subSupNo1[MathTextForm @ "Dih", Infinity, n]
  },
  InfiniteDihedralGroup[n_ ? Internal`PositiveIntegerQ, "Improper"] :> {
    "Generators" :> MakeDihedralTranslationMatrices @ makeInfiniteAbelianGenerators[n],
    "Order" :> Infinity,
    "Format" :> subSupNo1[MathTextForm @ "Dih", Infinity, n]
  },
  InfiniteDihedralGroup[n_ ? Internal`PositiveIntegerQ, "Redundant"] :> {
    "Generators" :> MakeDihedralTranslationMatrices @ makeInfiniteRedundantAbelianGenerators[n],
    "Order" :> Infinity,
    "Format" :> subSupNo1[MathTextForm @ "Dih*", Infinity, n]
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
  TranslationGroup[vecs_ ? MatrixQ] :> {
    "Generators" :> Map[TranslationMatrix, vecs],
    "Order" :> Infinity,
    "Format" :> Subsuperscript["T", Length @ vecs, Length @ First @ vecs]
  },
  TranslationGroup[vecs_ ? MatrixQ, mod_ /; VectorQ[mod] || IntegerQ[mod]] :> {
    "Generators" :> Map[TranslationMatrix[#, mod]&, vecs],
    "Order" :> Infinity,
    "Format" :> Subsuperscript["T%", Length @ vecs, Length @ First @ vecs]
  }
];

(**************************************************************************************************)

PublicFunction[TranslationGroupQ]

SetUsage @ "
TranslationGroupQ[group$] returns True if group$ is a TranslationGroup.
"

TranslationGroupQ[TranslationGroup[_]] := True;
TranslationGroupQ[_] := False;


(**************************************************************************************************)

PublicFunction[DihedralTranslationGroup]

SetUsage @ "
DihedralTranslationGroup[vectors$] represents an 'improper' translation group generated by the translation
vectors vectors$, where the generators also invert the translations of subsequent group elements.
DihedralTranslationGroup[RootSystem[$$]] uses the simple roots of a given root system.
DihedralTranslationGroup[$$, 'Redundant'] uses a redundant representation by forming pairs of translations \
cyclically.
* The group operation is given by composing translations.
* A generator acts by translating: adding its corresponding vector to its operand, and flipping
the direction of future translations.
In one dimension this is the InfiniteDihedralGroup[1].
"

DihedralTranslationGroup[rs_ ? RootSystemObjectQ] :=
  DihedralTranslationGroup @ rs["TranslationMatrices"];

DihedralTranslationGroup[vecs_ ? MatrixQ, "Redundant"] :=
  DihedralTranslationGroup @ MakeRedundantTranslations @ vecs;

declareGroup[
  DihedralTranslationGroup[vecs_ ? MatrixQ] :> {
    "Generators" :> MakeDihedralTranslationMatrices @ Map[TranslationMatrix, vecs],
    "Order" :> Infinity,
    "Format" :> Subsuperscript["TDih", Length @ vecs, Length @ First @ vecs]
  }
];

(**************************************************************************************************)

PublicFunction[DihedralTranslationGroupQ]

SetUsage @ "
DihedralTranslationGroupQ[group$] returns True if group$ is a DihedralTranslationGroup.
"

DihedralTranslationGroupQ[_DihedralTranslationGroup | _InfiniteDihedralGroup] := True;
DihedralTranslationGroupQ[_] := False;



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

declareGroup[
  SignedSymmetricGroup[n_Integer] :> {
    "Generators" :> constructSignedSymmetricGenerators[n],
    "Order" :> Factorial[n] * Power[2, n],
    "Format" :> Subsuperscript[Style["S", Italic], n, "*"]
  }
]

constructSignedSymmetricGenerators[n_] := Join[
  BasisScalingMatrix[n, # -> -1]& /@ Range[n],
  Permute[IdentityMatrix[n], Cycles[{{#, Mod[# + 1, n, 1]}}]]& /@ Range[n-1]
]
