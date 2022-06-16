PackageExport["CatenateVectors"]

CatenateVectors[vecLists_] := Join[Sequence @@ vecLists, 2];


(**************************************************************************************************)
(** Packing                                                                                       *)
(**************************************************************************************************)

PackageScope["ToPacked"]

ToPacked = ToPackedArray;

PackageScope["ToPackedReal"]

ToPackedReal[e_] := ToPackedArray[e, Real];

PackageScope["ToPackedRealArrays"]

ToPackedRealArrays[array_ ? PackedArrayQ] := array;

ToPackedRealArrays[array_] := Scope[
  array = ToPackedReal[array];
  If[PackedArrayQ[array], array, Map[ToPackedRealArrays, array]]
];

(**************************************************************************************************)

PackageExport["PlusVector"]

SetUsage @ "
PlusVector[matrix$, vector$] adds vector$ to each row vector of matrix$.
PlusVector[vector$] is an operator form of PlusVector.
* PlusVector is useful because normally matrix$ + vector$ adds vector$ column-wise to matrix$ via Listability.
"

PlusVector[matrix_, 0|0.|{0.,0.}] := matrix;
PlusVector[matrix_, v_] := v + #& /@ matrix;
PlusVector[v_][matrix_] := PlusVector[matrix, v];

(**************************************************************************************************)
(** Column / row accessors                                                                        *)
(**************************************************************************************************)

PackageExport["FirstColumn"]

SetRelatedSymbolGroup[FirstColumn, LastColumn, MostColumns, RestColumns];

SetUsage @ "
FirstColumn[matrix$] gives a list consisting of the first column of a matrix.
"

FirstColumn[matrix_] := Part[matrix, All, 1];
FirstColumn[None] := None;

(**************************************************************************************************)

PackageExport["LastColumn"]

SetUsage @ "
LastColumn[matrix$] gives a list consisting of the last column of a matrix.
"

LastColumn[matrix_] := Part[matrix, All, -1];
LastColumn[None] := None;

(**************************************************************************************************)

PackageExport["MostColumns"]

SetUsage @ "
MostColumns[matrix$] gives a matrix consisting of the all but the last column of matrix$.
"

MostColumns[matrix_] := Part[matrix, All, All ;; -2];

(**************************************************************************************************)

PackageExport["RestColumns"]

SetUsage @ "
RestColumns[matrix$] gives a matrix consisting of the all but the first column of matrix$.
"

RestColumns[matrix_] := Part[matrix, All, 2 ;; All];

(**************************************************************************************************)
(** Column / row mutation                                                                         *)
(**************************************************************************************************)

PackageExport["PrependColumn"]

SetRelatedSymbolGroup[PrependColumn, AppendColumn];

SetUsage @ "
PrependColumn[matrix$, column$] gives a matrix in which the list column$ has been prepended.
"

PrependColumn[matrix_, column_] := Transpose @ Prepend[Transpose @ matrix, column];
PrependColumn[column_][matrix_] := PrependColumn[matrix, column];

(**************************************************************************************************)

PackageExport["AppendColumn"]

SetUsage @ "
AppendColumn[matrix$, column$] gives a matrix in which the list column$ has been appended.
"

AppendColumn[matrix_, column_] := Transpose @ Append[Transpose @ matrix, column];
AppendColumn[column_][matrix_] := AppendColumn[matrix, column];

(**************************************************************************************************)

PackageExport["AppendConstantColumn"]

AppendConstantColumn[matrix_, item_] := Map[Append[item], matrix];
AppendConstantColumn[item_][matrix_] := AppendConstantColumn[matrix, item];

(**************************************************************************************************)

PackageExport["PrependConstantColumn"]

PrependConstantColumn[matrix_, item_] := Map[Prepend[item], matrix];
PrependConstantColumn[item_][matrix_] := PrependConstantColumn[matrix, item];

(**************************************************************************************************)

PackageExport["AppendConstantRow"]

AppendConstantRow[matrix_, item_] := Append[matrix, ConstantArray[item, Length @ First @ matrix]];
AppendConstantRow[item_][matrix_] := AppendConstantRow[matrix, item];

(**************************************************************************************************)

PackageExport["PrependConstantRow"]

AppendConstantRow[matrix_, item_] := Prepend[matrix, ConstantArray[item, Length @ First @ matrix]];
AppendConstantRow[item_][matrix_] := PrependConstantRow[matrix, item];

(**************************************************************************************************)
(** Idiomatic construction of matrices                                                            *)
(**************************************************************************************************)

PackageExport["Matrix"]

SetHoldAll[Matrix];
Matrix[CompoundExpression[a___]] := Map[List, List[a]];
Matrix[elements___] := SequenceSplit[Flatten[Unevaluated[{elements}] /. HoldPattern[CompoundExpression[a__]] :> Riffle[{a}, EndOfRow]], {EndOfRow}];

(**************************************************************************************************)

PackageExport["InnerDimension"]

InnerDimension[array_] := Last @ Dimensions @ array;

(**************************************************************************************************)

PackageExport["Second"]

Second[a_] := Part[a, 2];

(**************************************************************************************************)

PackageExport["SecondDimension"]

SecondDimension[array_] := Second @ Dimensions @ array;

(**************************************************************************************************)
(** Common matrix predicates                                                                      *)
(**************************************************************************************************)

PackageExport["OnesQ"]

OnesQ[m_] := FreeQ[m, Complex] && MinMax[m] === {1, 1};


PackageExport["ZerosQ"]

ZerosQ[m_] := FreeQ[m, Complex] && MinMax[m] === {0, 0};


PackageExport["CoordinateVectorQ"]

CoordinateVectorQ[{Repeated[_ ? NumericQ, {2, 3}]}] := True;
CoordinateVectorQ[{_ ? NumericQ, _ ? NumericQ}, 2] := True;
CoordinateVectorQ[{_ ? NumericQ, _ ? NumericQ, _ ? NumericQ}, 3] := True;
CoordinateVectorQ[___] := False;


PackageExport["CoordinateMatrixQ"]

CoordinateMatrixQ[matrix_, n_:2|3] :=
  MatrixQ[matrix] && MatchQ[InnerDimension @ matrix, n];


PackageExport["CoordinateMatricesQ"]

CoordinateMatricesQ[list_] :=
  VectorQ[list, CoordinateMatrixQ];

CoordinateMatricesQ[list_, n_] :=
  VectorQ[list, CoordinateMatrixQ[#, n]&];


PackageExport["CoordinateArrayQ"]

CoordinateArrayQ[array_, n_:2|3] :=
  ArrayQ[array, 3] && MatchQ[InnerDimension @ array, n];


PackageExport["ComplexMatrixQ"]

ComplexMatrixQ[e_] := ContainsQ[e, _Complex] && MatrixQ[e];


PackageExport["UpperUnitriangularMatrixQ"]

UpperUnitriangularMatrixQ[matrix_] :=
  UpperTriangularMatrixQ[matrix] && OnesQ[Diagonal[matrix]];


PackageExport["IdentityMatrixQ"]

IdentityMatrixQ[matrix_] :=
  DiagonalMatrixQ[matrix] && OnesQ[Diagonal[matrix]];


PackageExport["ZeroMatrixQ"]

ZeroMatrixQ[matrix_] := MatrixQ[matrix] && ZerosQ[matrix];


PackageExport["RowTotals"]

RowTotals[matrix_] := Total[matrix, {2}];


PackageExport["ColumnTotals"]

ColumnTotals[matrix_] := Total[matrix, {1}];


PackageExport["PermutationMatrixQ"]

PermutationMatrixQ[matrix_] :=
  SquareMatrixQ[matrix] && RealMatrixQ[matrix] && MinMax[matrix] == {0, 1} && Count[matrix, 1, 2] == Length[matrix] &&
    OnesQ[Total[matrix, {1}]] && OnesQ[Total[matrix, {2}]];


PackageExport["SameMatrixUptoPermutationQ"]

mkPerms[n_] := mkPerms[n] = Permutations @ Range[n];
SameMatrixUptoPermutationQ[m1_, m2_] := AnyTrue[mkPerms @ Length @ m1, m1 == Part[m2, #, #]&];

PackageExport["SameMatrixUptoPermutationAndInversionQ"]

SameMatrixUptoPermutationAndInversionQ[m1_, m2_] := AnyTrue[mkPerms @ Length @ m1, MatchQ[Part[m2, #, #], m1 | Transpose[m1]]&];
(* SameMatrixUptoPermutationAndInversionQ[m1_, m2_] := AnyTrue[mkPerms @ Length @ m1, MatchQ[Part[m2, #, #], m1 | Transpose[m1]]&];
 *)

(**************************************************************************************************)
(** Translations matrix functions                                                                 *)
(**************************************************************************************************)

PackageExport["TranslationMatrix"]

TranslationMatrix[vector_] := Scope[
  matrix = IdentityMatrix[Length[vector] + 1];
  matrix[[;;-2, -1]] = vector;
  matrix
];

TranslationMatrix[vector_, mod_] :=
  ModForm[TranslationMatrix @ vector, mod];

TranslationMatrix[vector_, mod_List] := Scope[
  modMatrix = ZeroMatrix[Length[vector] + 1] + Infinity;
  modMatrix[[;;-2, -1]] = mod;
  ModForm[TranslationMatrix @ vector, modMatrix]
];


PackageExport["UnitTranslationMatrix"]

UnitTranslationMatrix[n_, k_] :=
  AugmentedIdentityMatrix[n + 1, {k, n + 1}]


PackageExport["RedundantUnitTranslationMatrix"]

RedundantUnitTranslationMatrix[n_, k_] :=
  ReplacePart[IdentityMatrix[n + 1], {{k, n + 1} -> 1, {Mod[k + 1, n, 1], n + 1} -> -1}];


PackageExport["TranslationMatrixQ"]

TranslationMatrixQ[matrix_] := And[
  UpperUnitriangularMatrixQ[matrix],
  IdentityMatrixQ @ DiagonalBlock[matrix, {1, -2}]
];


(**************************************************************************************************)

PackageScope["MakeDihedralTranslationMatrices"]

MakeDihedralTranslationMatrices[matrices_] :=
  ReplacePart[{-1, -1} -> -1] /@ matrices;


PackageExport["DihedralTranslationMatrixQ"]

DihedralTranslationMatrixQ[matrix_] := And[
  Part[matrix, -1, -1] === -1,
  UpperUnitriangularMatrixQ @ ReplaceDiagonalPart[matrix, -1 -> 1],
  IdentityMatrixQ @ DiagonalBlock[matrix, {1, -2}]
];

(**************************************************************************************************)

PackageScope["MakeRedundantTranslations"]

MakeRedundantTranslations[vec_] :=
  Subtract @@ Partition[vec, 2, 1, 1];


PackageExport["ExtractTranslationVector"]

ExtractTranslationVector[matrix_] := matrix[[;;-2, -1]];


(**************************************************************************************************)
(** Common constructors                                                                          **)
(**************************************************************************************************)

PackageExport["ZeroMatrix"]

SetUsage @ "
ZeroMatrix[n$] represents the zero n$ \[Times] n$ matrix.
"

ZeroMatrix[n_] := ConstantArray[0, {n, n}];


PackageExport["Ones"]

Ones[i_] := ConstantArray[1, i];


PackageExport["AppendOnes"]

typedOne = MatchValues[
  _Real :=  1.;
  _ :=      1;
];

AppendOnes = MatchValues[
  array_ ? VectorQ :=
    Append[array, typedOne @ Part[array, 1]];
  array_ ? MatrixQ :=
    ToPacked @ ArrayFlatten @ {{array, typedOne @ Part[array, 1, 1]}};
  _ := $Failed;
];


PackageExport["Zeros"]

Zeros[i_] := ConstantArray[0, i];


PackageExport["BasisScalingMatrix"]

BasisScalingMatrix[n_, rules_] :=
  ReplaceDiagonalPart[IdentityMatrix @ n, rules];


PackageExport["ReplaceDiagonalPart"]

ReplaceDiagonalPart[matrix_, rules_List] :=
  ReplacePart[matrix, {#1, #1} -> #2& @@@ rules];

ReplaceDiagonalPart[matrix_, i_ -> v_] :=
  ReplacePart[matrix, {i, i} -> v];


PackageExport["AugmentedIdentityMatrix"]

SetUsage @ "
AugmentedIdentityMatrix[n$, {i$, j$}] represents the identity n$ \[Times] n$ matrix with an additional one at position ($i, $j).
AugmentedIdentityMatrix[n$, {{i$1, j$1}, {i$2, j$2}, $$}}] puts additional ones at several positions.
"

AugmentedIdentityMatrix[n_, {i_, j_}] := ReplacePart[IdentityMatrix[n], {i, j} -> 1];
AugmentedIdentityMatrix[n_, list_List] := ReplacePart[IdentityMatrix[n], list -> 1];

(**************************************************************************************************)
(** Padding                                                                                       *)
(**************************************************************************************************)

PackageExport["PadRows"]

PadRows[ragged_, item_] := Scope[
  w = Max[Length /@ ragged];
  ToPacked @ Map[padToLength[w, item], ragged]
]

padToLength[n_, item_][vector_] := Scope[
  l = Length[vector];
  If[l < n, Join[vector, ConstantArray[item, n - l]], vector]
];

PackageExport["PadColumns"]

PadColumns[ragged_, n_, item_] := Scope[
  full = PadRows[ragged, item];
  w = Length @ First @ full;
  padToLength[n, ConstantArray[item, w]] @ full
]

(**************************************************************************************************)
(** Block matrix functions                                                                        *)
(**************************************************************************************************)

PackageExport["BlockDiagonalMatrix"]

BlockDiagonalMatrix[blocks_] := Scope[
  range = Range @ Length @ blocks;
  If[!MatchQ[blocks, {Repeated[_ ? MatrixQ]}], ReturnFailed[]];
  superMatrix = DiagonalMatrix[range] /. RuleThread[range, blocks];
  ToPacked @ ArrayFlatten[superMatrix, 2]
];


PackageExport["FindDiagonalBlockPositions"]

rangeTrueQ[func_, i_, j_] := And @@ Table[func[x], {x, i, j}];
firstTrueInRange[func_, i_, j_] := Block[{}, Do[If[func[x], Return[x, Block]], {x, i, j}]; j + 1];

BooleArrayPlot[arr_] := ArrayPlot[arr, Mesh -> True, PixelConstrained -> 10, ColorRules -> {False -> Red, True -> Green}];

FindDiagonalBlockPositions[matrices_] := Scope[
  trans = Transpose[matrices, {3,1,2}];
  isZero = MatrixMap[ZerosQ, trans];
  n = Length[trans]; n2 = n - 1;
  isZeroD = Table[And @@ isZero[[(i+1);;, j]], {i, n2}, {j, n2}];
  isZeroR = Table[And @@ isZero[[i, (j+1);;]], {i, n2}, {j, n2}];
  If[FreeQ[isZeroR, True] || FreeQ[isZeroD, True], Return[{{1, n}}]];
  pos = 1;
  blockPositions = {};
  While[IntegerQ[pos] && pos <= n,
    lastPos = pos;
    pos = firstTrueInRange[
      next |-> And[
        rangeTrueQ[isZeroR[[#, next]]& , pos, next],
        rangeTrueQ[isZeroD[[next, #]]&, pos, next]
      ],
      pos, n2
    ];
    AppendTo[blockPositions, {lastPos, pos}];
    pos += 1;
  ];
  blockPositions
];

PackageExport["FindDiagonalBlocks"]

FindDiagonalBlocks[matrices_] := Scope[
  positions = FindDiagonalBlockPositions[matrices];
  matrices[[All, #, #]]& /@ (Span @@@ positions)
]


PackageExport["DiagonalBlock"]

DiagonalBlock[matrix_ ? MatrixQ, {i_, j_}] := Part[matrix, i;;j, i;;j];
DiagonalBlock[matrixList_ /; VectorQ[matrixList, MatrixQ], {i_, j_}] := Part[matrixList, All, i;;j, i;;j];

DiagonalBlock[obj_, list:{__List}] := Map[DiagonalBlock[obj, #]&, list];

DiagonalBlock[part_][obj_] := DiagonalBlock[obj, part];


(**************************************************************************************************)
(** Misc utilities                                                                               **)
(**************************************************************************************************)


PackageExport["FindIndependentVectors"]

FindIndependentVectors[vectors_] := Scope[
  rowReduced = RowReduce @ Transpose @ vectors;
  pivotPositions = Flatten[Position[#, Except[0, _ ? NumericQ], 1, 1]& /@ rowReduced];
  Part[vectors,  pivotPositions]
]

(**************************************************************************************************)

PackageExport["MatrixSimplify"]

MatrixSimplify[matrix_] := Scope[
  entries = Flatten[matrix];
  gcd = PolynomialGCD[Sequence @@ entries];
  If[gcd === 1, Return[{matrix, 1}]];
  {Simplify @ Cancel[matrix / gcd], gcd}
];

(**************************************************************************************************)

PackageExport["ExtendedSparseArray"]

ExtendedSparseArray[{} | <||>, sz_] := SparseArray[{}, sz];

ExtendedSparseArray[assoc_Association, sz_] := SparseArray[Normal @ assoc, sz];

ExtendedSparseArray[list:{___List} ? DuplicateFreeQ, sz_] := SparseArray[Thread[list -> 1], sz];

ExtendedSparseArray[list:{___List}, sz_] := SparseArray[Normal @ Counts @ list, sz];

ExtendedSparseArray[list:{___Rule}, sz_] := SparseArray[sumRules @ list, sz];

(**************************************************************************************************)

sumRules[rules_] := Normal @ Merge[rules, Total];

(**************************************************************************************************)

PackageExport["FromSparseRows"]

FromSparseRows[rowSpecs_List, n_Integer] := SparseArray[
  Flatten @ MapIndex1[rowSpecToFullSpec, rowSpecs],
  {Length @ rowSpecs, n}
];

FromSparseRows[rowSpecs_List] := SparseArray[
  Flatten @ MapIndex1[rowSpecToFullSpec, rowSpecs]
];

rowSpecToFullSpec[{}, row_] := {};

rowSpecToFullSpec[cols:{__Rule}, row_] := VectorApply[{row, #1} -> #2&, sumRules @ cols];

rowSpecToFullSpec[cols_List -> k_, row_] := sumRules @ Map[{row, #} -> k&, cols];
rowSpecToFullSpec[cols_List, row_] := sumRules @ Map[{row, #} -> 1&, cols];

rowSpecToFullSpec[col_Integer -> k_, row_] := {{row, col} -> k};
rowSpecToFullSpec[col_Integer, row_] := {{row, col} -> 1};

(**************************************************************************************************)

PackageExport["FromSparseColumns"]

FromSparseColumns[args___] := Transpose @ FromSparseRows[args];

(**************************************************************************************************)

PackageExport["SparseTotalMatrix"]

SparseTotalMatrix[indexSets_, n_] := FromSparseRows[indexSets, n];

(**************************************************************************************************)

PackageExport["SparseAveragingMatrix"]

SparseAveragingMatrix[indexSets_, n_] := FromSparseRows[Map[set |-> set -> 1.0 / Length[set], indexSets], n];

(**************************************************************************************************)

PackageExport["SparseBroadcastMatrix"]

SparseBroadcastMatrix[indexSets_, n_] := FromSparseColumns[indexSets, n];

(**************************************************************************************************)

PackageExport["DifferenceMatrix"]

DifferenceMatrix[{}] := {};

DifferenceMatrix[points_] := Outer[Plus, points, -points, 1];

DifferenceMatrix[points1_, points2_] := Outer[Plus, points1, -points2, 1];

(**************************************************************************************************)

PackageExport["SquaredDistanceMatrix"]

SquaredDistanceMatrix[{}] := {};

SquaredDistanceMatrix[points_ ? RealVectorQ] :=
  Outer[SquaredEuclideanDistance, points, points];

SquaredDistanceMatrix[points1_ ? RealVectorQ, points2_ ? RealVectorQ] :=
  Outer[SquaredEuclideanDistance, points1, points2];

SquaredDistanceMatrix[points_ ? RealMatrixQ] := (
  $loadDM; $distanceMatrixFunction1[points, $squaredEuclideanDistanceCode, False]
);

SquaredDistanceMatrix[{}, _] := {};
SquaredDistanceMatrix[_, {}] := {};

SquaredDistanceMatrix[points1_ ? RealMatrixQ, points2_ ? RealMatrixQ] := (
  $loadDM; $distanceMatrixFunction2[points1, points2, $squaredEuclideanDistanceCode, False]
);

$loadDM := (
  DistanceMatrix[{{1,2}},{{3,4}}, DistanceFunction -> "SquaredEuclideanDistance"];
  $squaredEuclideanDistanceCode := NumericArrayUtilities`DistanceMatrix`PackagePrivate`$extractLLDMMethod["SquaredEuclideanDistance"];
  $distanceMatrixFunction1 = NumericArrayUtilities`DistanceMatrix`PackagePrivate`mTensorDistanceMatrix1Arg;
  $distanceMatrixFunction2 = NumericArrayUtilities`DistanceMatrix`PackagePrivate`mTensorDistanceMatrix2Arg;
  Clear[$loadDM];
);

