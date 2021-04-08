Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageScope["customGroupMatrices"]
PackageScope["getGroupMatrices"]
PackageScope["customGroupQ"]


PackageExport["AbelianGroupQ"]

SetUsage @ "
AbelianGroupQ[group$] returns True if group$ is an Abelian group.
"

AbelianGroupQ[group_] := MatchQ[group,
  CyclicGroup[_Integer] | AbelianGroup[{__Integer}] | InfiniteAbelianGroup[_Integer] |
  GroupDirectProduct[list_List /; VectorQ[list, AbelianGroupQ]]
];

GroupOrder;
Unprotect[GroupOrder];
GroupOrder[None] := None;
Protect[GroupOrder];


PackageExport["GroupQ"]

SetUsage @ "
GroupQ[group$] returns True if group$ is a valid group.
"

GroupQ[HoldPattern @ PermutationGroup[{__Cycles}]] := True;
GroupQ[e_] := GroupTheory`PermutationGroups`Private`NamedGroupQ[e];

customGroupQ[_] := False;
customGroupMatrices[_] := $Failed;

declareCustomGroup[pattern_ :> {generators_, order_:Infinity}] := (
    GroupQ[HoldPattern @ pattern] := True;
    customGroupQ[HoldPattern @ pattern] := True;
    customGroupMatrices[HoldPattern @ pattern] := generators;
    GroupOrder[HoldPattern @ pattern] ^:= order;
);

declareCustomGroup[rules__RuleDelayed] := Scan[declareCustomGroup, {rules}];

declareCustomGroup::badrules = "Encountered bad rules.";

declareCustomGroup[_] := Message[declareCustomGroup::badrules];
declareCustomGroup[___] := Message[declareCustomGroup::badrules];


getHead[Verbatim[Condition][e_, _]] := getHead[e];
getHead[Verbatim[PatternTest][e_, _]] := getHead[e];
getHead[Verbatim[Blank][h_]] := h;
getHead[e_] := Head[e];

declareCustomGroupFormat[lhs_ :> rhs_] := (
  Format[lhs, StandardForm] := rhs;
  Format[lhs, TraditionalForm] := rhs;
);

declareBuiltinGroupFormat[lhs_ :> rhs_] :=
  With[{head = getHead[lhs]},
    Unprotect[head];
    Format[lhs, StandardForm] := rhs;
    Format[lhs, TraditionalForm] := rhs;
    Protect[head];
  ];

declareBuiltinGroupFormat[rules__] := Scan[declareBuiltinGroupFormat, {rules}];

declareBuiltinGroupFormat[
  CyclicGroup[n_Integer] :> makeAbelianSymbol[n],
  AlternatingGroup[n_Integer] :> Subscript[Style["A", Italic], n],
  SymmetricGroup[n_Integer] :> Subscript[Style["S", Italic], n],
  DihedralGroup[n_Integer] :> Subscript["Di", n],
  AbelianGroup[n:{__Integer}] :> Row[makeAbelianSymbol /@ n, "\[CirclePlus]"]
];

$IntZ = "\[DoubleStruckCapitalZ]";
$IntZ = TemplateBox[{}, "Integers"] // RawBoxes;
makeAbelianSymbol[n_Integer] := Subscript[$IntZ, n];


PackageExport["GroupDirectProduct"]

SetUsage @ "
GroupDirectProduct[{g$1, $$, g$n}] represents the product of several groups.
* The generators of the product is the union of generators of the g$i.
* GroupDirectProduct works with GroupRepresentation.
* GroupDirectProduct does not work with the other group theory functions.
";

declareCustomGroup[
  GroupDirectProduct[g:{Repeated[_ ? GroupQ]}] :> {
    constructDirectProductGenerators[getGroupMatrices /@ g],
    Times @@ Map[GroupOrder, g]
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
  If[MatchQ[subbox, TemplateBox[_, "RowWithSeparators"]], RowBox[{"(", subbox, ")"}], subbox]
];

declareCustomGroupFormat[
  GroupDirectProduct[list_List] :> formatDirectProduct[list]
];

formatDirectProduct[list_List] := If[VectorQ[list, AbelianGroupQ], CirclePlus, CircleTimes] @@ Map[maybeBracket, list];


PackageExport["DiscreteHeisenbergGroup"]

SetUsage @ "
DiscreteHeisenbergGroup[] represents the Heisenberg group of 3 \[Times] 3 upper-unitriangular matrices with integer entries.
* DiscreteHeisenbergGroup works with GroupRepresentation.
* DiscreteHeisenbergGroup does not work with the other group theory functions, since it has no finite permutation representation.
"
declareCustomGroup[
  DiscreteHeisenbergGroup[] :> {
    Map[UnitAffineMatrix[3, #]&, {{1, 2}, {2, 3}, {1, 3}}]
  }
];

declareCustomGroupFormat[
  DiscreteHeisenbergGroup[] :> Style["H", Italic]
];


PackageExport["InfiniteAbelianGroup"]

SetUsage @ "
InfiniteAbelianGroup[n$] represents an infinite Abelian group on n$ generators.
* InfiniteAbelianGroup works with GroupRepresentation.
* InfiniteAbelianGroup does not work with the other group theory functions, since it has no finite permutation representation.
"

declareCustomGroup[
  InfiniteAbelianGroup[n_ ? Internal`PositiveIntegerQ] :> {
    Table[makeAffineUnitMatrix[i, n+1], {i, n}]
  }
];

makeAffineUnitMatrix[i_, n_] :=
  ReplacePart[IdentityMatrix[n], {i, n} -> 1];


declareCustomGroupFormat[
  InfiniteAbelianGroup[n_Integer] :> Superscript[$IntZ, n]
];



PackageExport["GroupGeneratorElements"]

SetUsage @ "
GroupGeneratorElements[group$] returns the RepresentationElement[$$] objects corresponding to its generators.
"

GroupGeneratorElements[group_] := Scope[
  rep = GroupRepresentation[group];
  If[!RepresentationObjectQ[rep], ReturnFailed[]];
  rep["Generators"]
];


(* PackageExport["QuaternionGroup"]

SetUsage @ "
QuaternionGroup[] represents the group of unit quaternions.
"
 *)

