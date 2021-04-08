Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["RepresentationObjectQ"]

SetUsage @ "
RepresentationObjectQ[rep$] returns True if rep$ is a valid RepresentationObject[$$].
"

RepresentationObjectQ[_RepresentationObject ? System`Private`HoldNoEntryQ] := True;
RepresentationObjectQ[_] := False;


PackageExport["RepresentationObject"]

constructGroupRepresentation[data_] := Scope[
  group = data["Group"];
  matrices = Normal /@ data["Generators"];
  matrices = ExpandUnitRoots[matrices];
  type = Which[
    AnyTrue[matrices, ComplexMatrixQ], Which[
      AllTrue[matrices, UnitaryMatrixQ], "Unitary",
      AllTrue[matrices, HermitianMatrixQ], "Hermitian",
      True, "Complex"
    ],
    AllTrue[matrices, UpperUnitriangularMatrixQ], "Unitriangular",
    AllTrue[matrices, UpperTriangularMatrixQ], "Triangular",
    AllTrue[matrices, PermutationMatrixQ], "Permutation",
    AbelianGroupQ[group] || AllTrue[matrices, AbelianMatrixQ], "Abelian",
    True, "Mixed"
  ];
  System`Private`ConstructNoEntry[RepresentationObject, Append[data, "Type" -> type]]
];

$representationIcon =
 Framed[Style["R", FontSize -> 20], FrameStyle -> Gray,
  ImageSize -> {35, 35}, Alignment -> Center]

RepresentationObject /: MakeBoxes[object:RepresentationObject[data_Association] ? System`Private`HoldNoEntryQ, format_] := ModuleScope[
  UnpackAssociation[data, group, groupOrder, generators, dimension, type];
  BoxForm`ArrangeSummaryBox[
    RepresentationObject, object, $representationIcon; None,
    (* Always displayed *)
    {
     {BoxForm`SummaryItem[{"Group: ", group}], SpanFromLeft},
     {BoxForm`SummaryItem[{"Order: ", groupOrder}], BoxForm`SummaryItem[{"Type: ", type}]},
     {BoxForm`SummaryItem[{"Generators: ", Length[generators]}], BoxForm`SummaryItem[{"Dimension: ", dimension}]}
     },
    (* Displayed on request *)
    {{Row[generators, "  "], SpanFromLeft}},
    format,
    "Interpretable" -> Automatic
  ]
];

getRepData[RepresentationObject[data_Association]] := data;

(rep_RepresentationObject ? System`Private`HoldNoEntryQ)[key_String] :=
  getRepProperty[getRepData[rep], key];

getRepProperty[assoc_, "Identity"] :=
  RepresentationElement @ IdentityMatrix[assoc["Dimension"]];

(rep_RepresentationObject ? System`Private`HoldNoEntryQ)[key_String, opts__Rule] :=
  getRepProperty[getRepData[rep], key, {opts}];

getRepProperty[assoc_, "CayleyGraph"] := computeCayleyQuiver[assoc];

getRepProperty[assoc_, "CayleyFunction"] := computeCayleyFunction[assoc, False, False];

getRepProperty[assoc_, "CayleyFunction", opts_] :=
  computeCayleyFunction[assoc, Lookup[opts, "Symmetric", False], Lookup[opts, "Labeled", False]];

getRepProperty[assoc_, key_] :=
  Lookup[assoc, key];

computeCayleyFunction[data_, isSymmetric_, isLabeled_] := Scope[
  UnpackAssociation[data, generators];
  list = Flatten @ MapIndexed[
    {gen, index} |-> {
      If[isLabeled, Labeled[First @ index], Identity] @ gen,
      If[isSymmetric && (igen = InverseFunction[gen]) =!= gen,
        If[isLabeled, Labeled[Negated @ First @ index], Identity] @ igen,
        Nothing
      ]
    },
    generators
  ];
  ApplyThrough[list]
];


PackageExport["CayleyFunction"]

SetUsage @ "
CayleyFunction[rep$] returns the function that takes an element of rep$ and \
returns a list of successors, labeled by the generator of rep$ that produced them.
* rep can be a CardinalQuiverRepresentationObject or a RepresentationObject.
"

CayleyFunction::badrep =
  "First arguemnt to CayleyFunction should be a valid RepresentationObject or CardinalQuiverRepresentationObject."
CayleyFunction[re ? RepresentationObjectQ] := re["SymmetricCayleyFunction"]

CayleyFunction[_] := (Message[CayleyFunction::badrep]; $Failed);


PackageExport["CayleyCardinalQuiver"]

SetUsage @ "
CayleyCardinalQuiver[obj$] returns the cardinal quiver representing the Cayley graph of a RepresentationObject or Group.
"

CayleyCardinalQuiver::incomplete = "Cayley graph is incomplete."
CayleyCardinalQuiver::notrep = "First argument should be a valid RepresentationObject or group."

CayleyCardinalQuiver[rep_] := Scope[
  rep = toRepresentation[rep, None];
  If[FailureQ[rep], ReturnFailed["notrep"]];
  computeCayleyQuiver @ getRepData @ rep
];

computeCayleyQuiver[data_] := Scope[
  cfunc = computeCayleyFunction[data, True, True];
  istate = List @ getRepProperty[data, "Identity"];
  {graph, reason} = StateTransitionGraph[cfunc, istate, {"Graph", "TerminationReason"}, MaxDepth -> 8];
  If[reason =!= "Complete", Message[CayleyCardinalQuiver::incomplete]];
  CardinalQuiver[graph]
];

Unprotect[Labeled];

Labeled[None] := Identity;
Labeled[label_][g_] := Labeled[g, label];
Labeled[f_, label_][input___] := Labeled[f[input], label];

Protect[Labeled];


PackageExport["RepresentationElement"]

SetUsage @ "
RepresentationElement[matrix$] labels an element of a representation.
"

RepresentationElement /: InverseFunction[RepresentationElement[matrix_]] := RepresentationElement[Inverse[matrix]];

RepresentationElement /: Normal[RepresentationElement[matrix_]] :=
  If[Developer`PackedArrayQ[matrix], matrix, ExpandUnitRoots @ matrix];

Format[RepresentationElement[matrix_?MatrixQ], StandardForm] :=
  renderRepresentationMatrix[matrix, False];

Format[RepresentationElement[matrix_?MatrixQ], TraditionalForm] :=
  renderRepresentationMatrix[matrix, True];

splitImag[e_] := If[ContainsQ[e, _Complex], fmtComplexRow[Re @ e, Im @ e], e];

fmtComplexRow[0, im_] := Row[{im, $imagStr}];
fmtComplexRow[re_, im_] := Row[{re, "+", im, $imagStr}];

RepresentationElement[elem1_][RepresentationElement[elem2_]] :=
  RepresentationElement[Dot[elem1, elem2]];

$blankNum = Style["\[CenterDot]", Gray];

$squareRootStr = "\[DoublePrime]";
$imagStr = "\[ImaginaryI]"

$supStrs = AssociationThread[Range[0, 9], Characters @ "⁰¹²³⁴⁵⁶⁷⁸⁹"];
$subStrs = AssociationThread[Range[0, 9], Characters @ "₀₁₂₃₄₅₆₇₈₉"];

scriptStr[n_, minus_, assoc_] := If[Negative[n], minus, ""] <> StringJoin[Lookup[assoc, IntegerDigits @ n]];
supStr[n_] := scriptStr[n, "⁻", $supStrs];
subStr[n_] := scriptStr[n, "₋", $subStrs];

PackageExport["ToNumberString"]

ToNumberString[e_] := numStr[e];

numStr = MatchValues[
  1/2 := "1/2";
  i_Integer := TextString[i];
  Sqrt[b_] := $squareRootStr <> brackStr[b];
  a_/Sqrt[b_] := brackStr[a] <> $squareRootStr <> brackStr[b];
  Rational[a_, b_] := brackStr[a] <> "/" <> brackStr[b];
  r_Real := TextString[NumberForm[r, 2]];
  p_Plus := plusStr[Map[numStr, List @@ p]];
  Power[e_, -1] := numStr[e] <> supStr[-1];
  Power[e_, k_Integer] := numStr[e] <> supStr[k];
  UnitRoot[n_] := "\[Xi]" <> subStr[n];
  Times[-1, negated_] := "-" <> brackStr[negated];
  Times[r_Rational, Sqrt[b_]] := $squareRootStr <> brackStr[r^2 * b];
  Times[Complex[0, r_Rational], b_] := numStr[r * b] <> $imagStr;
  Times[complex_Complex, other_] := brackStr[other] <> "(" <> numStr @ complex <> ")";
  Complex[0, Rational[1, b_]] := $imagStr <> "/" <> brackStr[b];
  Complex[0, imag_] := brackStr[imag] <> $imagStr;
  Complex[real_, 0] := real;
  Complex[real_, imag_] := plusStr[{numStr @ real, brackStr[imag] <> $imagStr}];
  i_ := TextString[i];
];

plusStr[parts_] := StringRiffle[parts, If[AllTrue[parts, StringFreeQ[" "]], "+", " + "]]

brackStr[e_] := Scope[
  s = numStr[e];
  If[StringContainsQ[s, " "], "(" <> s <> ")", s]
];

possiblyBracket[s_String] := If[StringContainsQ[s, " "], "(" <> s <> ")", s];

algStr = MatchValues[
  Power[e_, n_] := Superscript[e, n];
  e_ := e
];

posNegCol[e_] := If[Negative[e], Red, Black];
compCol[e_] := Hue[Arg[e]/(2 Pi)+.05, Min[Sqrt[Abs[N @ e]],1], .85];

colNumStr[cfunc_][elem_] := Which[
  ContainsUnitRootsQ[elem], algStr @ elem,
  elem == 0, $blankNum,
  True, Style[numStr @ Abs[elem], cfunc[elem]]
];

$matrixElementStyle = {FontFamily -> "Source Code Pro", FontSize -> 12, TextAlignment -> Left};
matrixGridStyle[wspacing_] := {Spacings -> {{{wspacing}}, {0.8,{0.3},0.5}}, FrameStyle -> LightGray, Frame -> True};

PackageScope["renderRepresentationMatrix"]

renderRepresentationMatrix[matrix_, isTraditional_] := Scope[
  wspacing = If[Developer`PackedArrayQ[matrix], 0.8, 0.6];
  If[isTraditional,
    If[ContainsQ[matrix, _Complex], matrix = Map[splitImag, matrix, {2}]];
    entries = matrix;
  ,
    cfunc = If[ContainsQ[matrix, _Complex], compCol, posNegCol];
    entries = Map[colNumStr[cfunc], matrix, {2}]
  ];
  width = Max @ Cases[entries, s_String :> StringLength[s], {2}];
  If[IntegerQ[width] && width > 1,
    entries = Replace[entries, s_String :> StringPadLeft[s, width], {2}]];
  Grid[entries,
    ItemSize -> Full,
    BaseStyle -> $matrixElementStyle,
    Sequence @@ matrixGridStyle[wspacing]
  ]
];


PackageScope["toRepresentation"]

toRepresentation["Abelian", n_] := GroupRepresentation[InfiniteAbelianGroup[n]];
toRepresentation["Redundant", n_] := RedundantAbelianRepresentation[n-1];
toRepresentation[r_RepresentationObject ? System`Private`HoldNoEntryQ, _] := r;
toRepresentation[group_ ? GroupQ, _] := GroupRepresentation[group];
toRepresentation[_, _] := $Failed;


PackageExport["CustomRepresentation"]

SetUsage @ "
CustomRepresentation[{matrix$1, $$, matrix$n}] takes a list of matrices and \
returns a RepresentationObject[$$].
CustomRepresentation[matrices$, group$] specifies that the representation is \
of the group group$.
"

CustomRepresentation::notmat = "First argument should be a list of matrices."
CustomRepresentation::badrepmat = "Matrices have inconsistent dimensions: ``."

CustomRepresentation[matrices_, group_:None] := Scope[
  If[!VectorQ[matrices, MatrixQ], ReturnFailed["notmat"]];
  matrices = Developer`ToPackedArray /@ Normal /@ matrices;
  dims = Dimensions[matrices];
  If[!MatchQ[dims, {_, _, _}], ReturnFailed["badintcode", Dimensions /@ matrices]];
  dim = Part[dims, 2];
  generators = RepresentationElement /@ matrices;
  repData = <|
    "Group" -> group,
    "GroupOrder" -> GroupOrder[group],
    "Generators" -> generators,
    "Dimension" -> dim
  |>;
  constructGroupRepresentation[repData]
];


PackageExport["RedundantAbelianRepresentation"]

SetUsage @ "
RedundantAbelianRepresentation[n$] produces a RepresentationObject whose n$ generators are affine matrices with a \
single 1 and a following neighboring -1 in the final column.
* This representation is the representation of an Abelian group for which g$1 g$2 = g$3, g$2 g$3 = g$4, etc.
"

makeRedundantAffineUnitMatrix[i_, n_] :=
  ReplacePart[IdentityMatrix[n], {{i, n} -> 1, {Mod[i+1, n-1, 1], n} -> -1}];

RedundantAbelianRepresentation::badrepdim = "The provided dimension `` should be >= 2."

RedundantAbelianRepresentation[n_Integer] := If[n < 2,
  Message[RedundantAbelianRepresentation::badrepdim, n],
  CustomRepresentation[
    Table[makeRedundantAffineUnitMatrix[i, n+2], {i, n+1}],
    InfiniteAbelianGroup[n]
  ]
]


PackageExport["RepresentationGenerators"]

SetUsage @ "
RepresentationGenerators[group$] returns a list of RepresentationElement objects \
for the generators of group$.
RepresentationGenerators[rep$] gives the generators for a RepresentationObject.
"

RepresentationGenerators::notrepgroup = "First argument should be a RepresentationObject or a group."

RepresentationGenerators[obj_] := Scope[
  rep = toRepresentation[obj, None];
  If[FailureQ[rep], ReturnFailed["notrepgroup"]];
  rep["Generators"]
]


PackageExport["GroupRepresentation"]

SetUsage @ "
GroupRepresentation[group$] returns a RepresentationObject of a group group$.
"

GroupRepresentation::notgroup = "First argument to GroupRepresentation should be a group."
General::badintcode = "Internal code returned inconsistent matrix dimensions: ``."

getGroupMatrices[group_] := Developer`ToPackedArray /@ Normal /@ If[customGroupQ[group],
    customGroupMatrices[group],
    permutationGroupMatrices[group]
  ];

GroupRepresentation[group_] := Scope[
  If[!GroupQ[group], ReturnFailed["notgroup"]];
  matrices = getGroupMatrices[group];
  dims = Dimensions[matrices];
  If[!MatchQ[dims, {_, _, _}], ReturnFailed["badintcode", Dimensions /@ matrices]];
  dim = Part[dims, 2];
  generators = RepresentationElement /@ matrices;
  repData = <|
    "Group" -> group,
    "GroupOrder" -> GroupOrder[group],
    "Generators" -> generators,
    "Dimension" -> dim
  |>;
  constructGroupRepresentation[repData]
]

permutationGroupMatrices[group_] := Scope[
  generators = GroupGenerators[group];
  If[!ListQ[generators], ReturnFailed[]];
  max = Max[PermutationMax /@ generators];
  Normal /@ Map[CyclesToPermutationMatrix[#, max]&, generators]
];

CyclesToPermutationMatrix[Cycles[cycles_], n_] := Scope[
  edges = Map[
    cycle |-> Map[# -> 1&, Partition[cycle, 2, 1, 1]],
    cycles
  ];
  stable = Complement[Range[n], Union @@ cycles];
  stableEdges = Map[{#, #} -> 1&, stable];
  SparseArray[Flatten @ {edges, stableEdges}, {n, n}]
];

getGroupMatrices[CyclicGroup[n_Integer]] := {{{UnitRoot[n]}}};

getGroupMatrices[AbelianGroup[n:{__Integer}]] := unitRootAbelianMatrices[n];

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