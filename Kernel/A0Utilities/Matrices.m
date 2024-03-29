PublicFunction[FormalSymbolArray]

$formalsRoman = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
FormalSymbolArray[dims_, str_Str] := FormalSymbolArray[dims, FF @ SFind[$formalsRoman, str] - 1];
FormalSymbolArray[dims_, offset_:0] := Block[{n = 0 + offset}, Array[Symbol @ FromCharCode[63488 + n++]&, dims]];

(**************************************************************************************************)

PublicFunction[CatenateVectors]

CatenateVectors[vecLists_] := Join[Sequence @@ vecLists, 2];

(**************************************************************************************************)

PublicFunction[LevelPart]

LevelPart[array_, -2 -> part_] := LevelPart[array, (ArrayDepth[array]-1) -> part];
LevelPart[array_, -1 -> part_] := LevelPart[array, ArrayDepth[array] -> part];
LevelPart[array_, 1 -> part_] := Part[array, part];
LevelPart[array_, 2 -> part_] := Part[array, All, part];
LevelPart[array_, 3 -> part_] := Part[array, All, All, part];
LevelPart[array_, 4 -> part_] := Part[array, All, All, All, part];
LevelPart[array_, 5 -> part_] := Part[array, All, All, All, All, part];

LevelPart[array_, depth_Int -> part_] :=
  Part[array, App[Repeat[All, If[depth < 0, depth + ArrayDepth[array], depth - 1]], part]];

LevelPart[array_, spec_List] := Scope[
  part = Repeat[All, ArrayDepth @ array];
  Part[array, Sequence @@ RepPart[part, spec]]
]

LevelPart[spec_][array_] := LevelPart[array, spec];

(**************************************************************************************************)

PublicFunction[TupleArray]

TupleArray[arrays_List] := ToPacked @ Transpose[arrays, RotateRight @ Range[ArrayDepth @ arrays]];

(**************************************************************************************************)

PublicFunction[FromTupleArray]

FromTupleArray[array_] := ToPacked @ Transpose[array, RotateLeft @ Range[ArrayDepth @ array]];

(**************************************************************************************************)
(** Packing                                                                                       *)
(**************************************************************************************************)

PrivateFunction[ToPacked, ToPackedReal, ToPackedComplex]

ToPacked = ToPackedArray;
ToPackedReal[e_] := ToPackedArray[e, Real];
ToPackedComplex[e_] := ToPackedArray[N @ e, Complex];

(**************************************************************************************************)

PrivateFunction[ToPackedRealArrays]

ToPackedRealArrays[array_ ? PackedArrayQ] := array;

ToPackedRealArrays[array_] := Scope[
  array = ToPackedReal[array];
  If[PackedArrayQ[array], array, Map[ToPackedRealArrays, array]]
];

(**************************************************************************************************)

PublicFunction[PlusVector]

SetUsage @ "
PlusVector[matrix$, vector$] adds vector$ to each row vector of matrix$.
PlusVector[vector$] is an operator form of PlusVector.
* PlusVector is useful because normally matrix$ + vector$ adds vector$ column-wise to matrix$ via Listability.
"

PlusVector[matrix_, v_] := Threaded[v] + matrix;
PlusVector[v_][matrix_] := Threaded[v] + matrix;

(**************************************************************************************************)
(** Column / row accessors                                                                        *)
(**************************************************************************************************)

SetRelatedSymbolGroup[Col1, Col2, Col3, ColL, MostColumns, RestColumns];

(**************************************************************************************************)

PublicFunction[MostColumns]

SetUsage @ "
MostColumns[matrix$] gives a matrix consisting of the all but the last column of matrix$.
"

MostColumns[matrix_] := Part[matrix, All, All ;; -2];

(**************************************************************************************************)

PublicFunction[RestColumns]

SetUsage @ "
RestColumns[matrix$] gives a matrix consisting of the all but the first column of matrix$.
"

RestColumns[matrix_] := Part[matrix, All, 2 ;; All];

(**************************************************************************************************)
(** Column / row mutation                                                                         *)
(**************************************************************************************************)

PublicFunction[PrependColumn]

SetRelatedSymbolGroup[PrependColumn, AppendColumn, InsertColumn, DeleteColumn];

SetUsage @ "
PrependColumn[matrix$, column$] gives a matrix in which the list column$ has been prepended.
PrependColumn[column$] is the operator form of PrependColumn.
"

PrependColumn[matrix_, column_] := Transpose @ Pre[Transpose @ matrix, column];
PrependColumn[column_][matrix_] := PrependColumn[matrix, column];

(**************************************************************************************************)

PublicFunction[AppendColumn]

SetUsage @ "
AppendColumn[matrix$, column$] gives a matrix in which the list column$ has been appended.
AppendColumn[column$] is the operator form of AppendColumn.
"

AppendColumn[matrix_, column_] := Transpose @ App[Transpose @ matrix, column];
AppendColumn[column_][matrix_] := AppendColumn[matrix, column];

(**************************************************************************************************)

PublicFunction[InsertColumn]

SetUsage @ "
InsertColumn[matrix$, column$, n$] gives a matrix in which the list column$ has been inserted at position n$.
InsertColumn[matrix$, n$] is the operator form of InsertColumn.
"

InsertColumn[matrix_, column_, n_] := Transpose @ Insert[Transpose @ matrix, column, n];
InsertColumn[column_, n_][matrix_] := PrependColumn[matrix, column, n];

(**************************************************************************************************)

PublicFunction[DeleteColumn]

SetUsage @ "
DeleteColumn[matrix$, n$] gives a matrix in which the n$'th column has been deleted.
DeleteColumn[n$] is the operator form of InsertColumn.
"

DeleteColumn[matrix_, n_] := Transpose @ Delete[Transpose @ matrix, n];
DeleteColumn[n_][matrix_] := DeleteColumn[matrix, n];

(**************************************************************************************************)

PublicFunction[AppendConstantColumn]

AppendConstantColumn[matrix_, item_] := Map[App[item], matrix];
AppendConstantColumn[item_][matrix_] := AppendConstantColumn[matrix, item];

(**************************************************************************************************)

PublicFunction[PrependConstantColumn]

PrependConstantColumn[matrix_, item_] := Map[Pre[item], matrix];
PrependConstantColumn[item_][matrix_] := PrependConstantColumn[matrix, item];

(**************************************************************************************************)

PublicFunction[InsertConstantColumn]

InsertConstantColumn[matrix_, item_, n_] := Map[Insert[item, n], matrix];
InsertConstantColumn[item_, n_][matrix_] := InsertConstantColumn[matrix, item, n];

(**************************************************************************************************)

PublicFunction[AppendConstantRow]

AppendConstantRow[matrix_, item_] := App[matrix, Repeat[item, Len @ F @ matrix]];
AppendConstantRow[item_][matrix_] := AppendConstantRow[matrix, item];

(**************************************************************************************************)

PublicFunction[PrependConstantRow]

PrependConstantRow[matrix_, item_] := Pre[matrix, Repeat[item, Len @ F @ matrix]];
PrependConstantRow[item_][matrix_] := PrependConstantRow[matrix, item];

(**************************************************************************************************)

PublicFunction[InsertConstantRow]

InsertConstantRow[matrix_, item_, n_] := Insert[matrix, Repeat[item, Len @ F @ matrix], n];
InsertConstantRow[item_, n_][matrix_] := InsertConstantRow[matrix, item, n];

(**************************************************************************************************)
(** Idiomatic construction of matrices                                                            *)
(**************************************************************************************************)

PublicFunction[Matrix]

SetHoldAll[Matrix];
Matrix[CompoundExpression[a___]] := Map[List, List[a]];
Matrix[elements___] := SequenceSplit[Flatten[Uneval[{elements}] /. HoldP[CompoundExpression[a__]] :> Riffle[{a}, EndOfRow]], {EndOfRow}];

(**************************************************************************************************)

PublicFunction[InnerDimension]

InnerDimension[array_] := L @ Dims @ array;

(**************************************************************************************************)
(** Common matrix predicates                                                                      *)
(**************************************************************************************************)

PublicFunction[OnesQ]

OnesQ[m_] := FreeQ[m, Complex] && MinMax[m] === {1, 1};

(**************************************************************************************************)

PublicFunction[ZerosQ]

ZerosQ[m_] := FreeQ[m, Complex] && MinMax[m] === {0, 0};

(**************************************************************************************************)

PublicFunction[NumericVectorQ, CoordinateVectorQ, CoordinateVector2DQ, CoordinateVector3DQ]

NumericVectorQ[e_List] := VecQ[e, NumericQ];

CoordinateVectorQ[{Repeated[_ ? NumericQ, {2, 3}]}] := True;
CoordinateVectorQ[{_ ? NumericQ, _ ? NumericQ}, 2] := True;
CoordinateVectorQ[{_ ? NumericQ, _ ? NumericQ, _ ? NumericQ}, 3] := True;

CoordinateVector2DQ[{_ ? NumericQ, _ ? NumericQ}] := True;
CoordinateVector3DQ[{_ ? NumericQ, _ ? NumericQ, _ ? NumericQ}] := True;

NumericVectorQ[___] := False;
CoordinateVectorQ[___] := False;
CoordinateVector2DQ[___] := False;
CoordinateVector3DQ[___] := False;

(**************************************************************************************************)

PublicFunction[CoordinatePairQ, CoordinatePair2DQ, CoordinatePair3DQ]

CoordinatePairQ[{_ ? CoordinateVectorQ, _ ? CoordinateVectorQ}] := True;
CoordinatePair2DQ[{_ ? CoordinateVector2DQ, _ ? CoordinateVector2DQ}] := True;
CoordinatePair3DQ[{_ ? CoordinateVector3DQ, _ ? CoordinateVector3DQ}] := True;

CoordinatePairQ[___] := False;
CoordinatePair2DQ[___] := False;
CoordinatePair3DQ[___] := False;

(**************************************************************************************************)

PublicFunction[AnyMatrixQ, NumericMatrixQ, CoordinateMatrixQ, CoordinateMatrix2DQ, CoordinateMatrix3DQ]

AnyMatrixQ[{} | {{}}] := True;
AnyMatrixQ[list_List] := Len[Dims[list, 2]] == 2;

NumericMatrixQ[matrix_List] := MatrixQ[matrix, NumericQ];

CoordinateMatrixQ[matrix_List, n_:2|3] := MatrixQ[matrix, NumericQ] && MatchQ[InnerDimension @ matrix, n];

CoordinateMatrix2DQ[matrix_List] := CoordinateMatrixQ[matrix, 2];
CoordinateMatrix3DQ[matrix_List] := CoordinateMatrixQ[matrix, 3];

AnyMatrixQ[___] := False;
NumericMatrixQ[___] := False;
CoordinateMatrixQ[___] := False;
CoordinateMatrix2DQ[___] := False;
CoordinateMatrix3DQ[___] := False;

(**************************************************************************************************)

PublicFunction[AnyMatricesQ, NumericMatricesQ, CoordinateMatricesQ, CoordinateMatrices2DQ, CoordinateMatrices3DQ]

AnyMatricesQ[list_List] := VecQ[list, AnyMatrixQ];

NumericMatricesQ[list_List] := VecQ[list, NumericMatrixQ];

CoordinateMatricesQ[list_List] := VecQ[list, CoordinateMatrixQ];
CoordinateMatricesQ[list_List, 2] := VecQ[list, CoordinateMatrix2DQ];
CoordinateMatricesQ[list_List, 3] := VecQ[list, CoordinateMatrix3DQ];
CoordinateMatricesQ[list_List, n_] := VecQ[list, CoordinateMatrixQ[#, n]&];

CoordinateMatrices2DQ[list_List] := VecQ[list, CoordinateMatrix2DQ];
CoordinateMatrices3DQ[list_List] := VecQ[list, CoordinateMatrix3DQ];

AnyMatricesQ[___] := False;
NumericMatricesQ[___] := False;
CoordinateMatricesQ[___] := False;
CoordinateMatrices2DQ[___] := False;
CoordinateMatrices3DQ[___] := False;

(**************************************************************************************************)

PublicFunction[NumericVectorOrMatrixQ, CoordinateVectorOrMatrixQ, CoordinateVectorOrMatrix2DQ, CoordinateVectorOrMatrix3DQ]

NumericVectorOrMatrixQ[list_List] := ArrayQ[list, 1|2, NumericQ];

CoordinateVectorOrMatrixQ[array_List, n_:2|3] := NumericVectorOrMatrixQ[array] && MatchQ[InnerDimension @ array, n];
CoordinateVectorOrMatrix2DQ[array_List] := CoordinateVectorOrMatrixQ[array, 2];
CoordinateVectorOrMatrix3DQ[array_List] := CoordinateVectorOrMatrixQ[array, 3];

NumericVectorOrMatrixQ[___] := False;
CoordinateVectorOrMatrixQ[___] := False;
CoordinateVectorOrMatrix2DQ[___] := False;
CoordinateVectorOrMatrix3DQ[___] := False;

(**************************************************************************************************)

PublicFunction[MatrixOrMatricesQ, NumericMatrixOrMatricesQ, CoordinateMatrixOrMatricesQ, CoordinateMatrixOrMatrices2DQ, CoordinateMatrixOrMatrices3DQ]

MatrixOrMatricesQ[{} | {{}}] := True;
MatrixOrMatricesQ[e_List] := AnyMatrixQ[e] || AnyMatricesQ[e];
MatrixOrMatricesQ[___] := False;

NumericMatrixOrMatricesQ[{} | {{}}] := True;
NumericMatrixOrMatricesQ[e_List] := NumericMatrixQ[e] || NumericMatricesQ[e];
NumericMatrixOrMatricesQ[___] := False;

CoordinateMatrixOrMatricesQ[{} | {{}}] := True;
CoordinateMatrixOrMatricesQ[e_List] := CoordinateMatrixQ[e] || CoordinateMatricesQ[e];
CoordinateMatrixOrMatricesQ[e_List, 2] := CoordinateMatrixOrMatrices2DQ[e];
CoordinateMatrixOrMatricesQ[e_List, 3] := CoordinateMatrixOrMatrices3DQ[e];
CoordinateMatrixOrMatricesQ[e_List, n_] := CoordinateMatrixQ[e, n] || CoordinateMatricesQ[e, n];
CoordinateMatrixOrMatricesQ[___] := False;

CoordinateMatrixOrMatrices2DQ[{} | {{}}] := True;
CoordinateMatrixOrMatrices2DQ[e_List] := CoordinateMatrix2DQ[e] || CoordinateMatrices2DQ[e];
CoordinateMatrixOrMatrices2DQ[___] := False;

CoordinateMatrixOrMatrices3DQ[{} | {{}}] := True;
CoordinateMatrixOrMatrices3DQ[e_List] := CoordinateMatrix3DQ[e] || CoordinateMatrices3DQ[e];
CoordinateMatrixOrMatrices3DQ[___] := False;

(**************************************************************************************************)

PublicFunction[NumArrayQ, CoordinateArrayQ, CoordinateArray2DQ, CoordinateArray3DQ]

(* we call this NumArrayQ because NumericArrayQ exists in System` and checks whether something is a NumericArray *)
NumArrayQ[array_List] := ArrayQ[array, _, NumericQ];
CoordinateArrayQ[array_List, n_:2|3] := ArrayQ[array, 3, NumericQ] && MatchQ[InnerDimension @ array, n];

CoordinateArray2DQ[array_List] := CoordinateArrayQ[array, 2];
CoordinateArray3DQ[array_List] := CoordinateArrayQ[array, 3];

NumArrayQ[___] := False;
CoordinateArrayQ[___] := False;
CoordinateArray2DQ[___] := False;
CoordinateArray3DQ[___] := False;

(**************************************************************************************************)

PublicFunction[ComplexMatrixQ]

ComplexMatrixQ[e_] := ContainsQ[e, _Complex] && MatrixQ[e];

(**************************************************************************************************)

PublicFunction[UpperUnitriangularMatrixQ]

UpperUnitriangularMatrixQ[matrix_] :=
  UpperTriangularMatrixQ[matrix] && OnesQ[Diagonal[matrix]];

(**************************************************************************************************)

PublicFunction[IdentityMatrixQ]

IdentityMatrixQ[matrix_] :=
  DiagonalMatrixQ[matrix] && OnesQ[Diagonal[matrix]];

(**************************************************************************************************)

PublicFunction[ZeroMatrixQ]

ZeroMatrixQ[matrix_] := MatrixQ[matrix] && ZerosQ[matrix];

(**************************************************************************************************)

PublicFunction[RowTotals]

RowTotals[matrix_] := Total[matrix, {2}];

(**************************************************************************************************)

PublicFunction[ColumnTotals]

ColumnTotals[matrix_] := Total[matrix, {1}];

(**************************************************************************************************)

PublicFunction[PermutationMatrixQ]

PermutationMatrixQ[matrix_] :=
  SquareMatrixQ[matrix] && RealMatrixQ[matrix] && MinMax[matrix] == {0, 1} && Count[matrix, 1, 2] == Len[matrix] &&
    OnesQ[Total[matrix, {1}]] && OnesQ[Total[matrix, {2}]];

(**************************************************************************************************)

PublicFunction[SameMatrixUptoPermutationQ]

mkPerms[n_] := mkPerms[n] = Permutations @ Range[n];
SameMatrixUptoPermutationQ[m1_, m2_] := AnyTrue[mkPerms @ Len @ m1, m1 == Part[m2, #, #]&];

PublicFunction[SameMatrixUptoPermutationAndInversionQ]

SameMatrixUptoPermutationAndInversionQ[m1_, m2_] := AnyTrue[mkPerms @ Len @ m1, MatchQ[Part[m2, #, #], m1 | Transpose[m1]]&];
(* SameMatrixUptoPermutationAndInversionQ[m1_, m2_] := AnyTrue[mkPerms @ Len @ m1, MatchQ[Part[m2, #, #], m1 | Transpose[m1]]&];
 *)

(**************************************************************************************************)
(** Translations matrix functions                                                                 *)
(**************************************************************************************************)

PublicFunction[TranslationMatrix]

TranslationMatrix[vector_] := Scope[
  matrix = IdentityMatrix[Len[vector] + 1];
  matrix[[;;-2, -1]] = vector;
  matrix
];

TranslationMatrix[vector_, mod_] :=
  ModForm[TranslationMatrix @ vector, mod];

TranslationMatrix[vector_, mod_List] := Scope[
  modMatrix = ZeroMatrix[Len[vector] + 1] + Inf;
  modMatrix[[;;-2, -1]] = mod;
  ModForm[TranslationMatrix @ vector, modMatrix]
];

(**************************************************************************************************)

PublicFunction[UnitTranslationMatrix]

UnitTranslationMatrix[n_, k_] :=
  AugmentedIdentityMatrix[n + 1, {k, n + 1}]

(**************************************************************************************************)

PublicFunction[RedundantUnitTranslationMatrix]

RedundantUnitTranslationMatrix[n_, k_] :=
  RepPart[IdentityMatrix[n + 1], {{k, n + 1} -> 1, {Mod[k + 1, n, 1], n + 1} -> -1}];

(**************************************************************************************************)

PublicFunction[TranslationMatrixQ]

TranslationMatrixQ[matrix_] := And[
  UpperUnitriangularMatrixQ[matrix],
  IdentityMatrixQ @ DiagonalBlock[matrix, {1, -2}]
];


(**************************************************************************************************)

PrivateFunction[MakeRedundantTranslations]

MakeRedundantTranslations[vec_] :=
  Subtract @@ Partition[vec, 2, 1, 1];


PublicFunction[ExtractTranslationVector]

ExtractTranslationVector[matrix_] := matrix[[;;-2, -1]];


(**************************************************************************************************)
(** Common constructors                                                                          **)
(**************************************************************************************************)

PublicFunction[ZeroMatrix]

SetUsage @ "
ZeroMatrix[n$] represents the zero n$ \[Times] n$ matrix.
"

ZeroMatrix[n_] := Repeat[0, {n, n}];

(**************************************************************************************************)

PublicFunction[Ones]

Ones[i_] := Repeat[1, i];

(**************************************************************************************************)

PublicFunction[AppendOnes]

typedOne = Case[
  _Real :=  1.;
  _ :=      1;
];

AppendOnes = Case[
  array_ ? VecQ :=
    App[array, typedOne @ F @ array];
  array_ ? MatrixQ :=
    ToPacked @ ArrayFlatten @ {{array, typedOne @ FF @ array}};
  _ := $Failed;
];

(**************************************************************************************************)

PublicFunction[Zeros, ZerosLike]

Zeros[i_] := Repeat[0, i];
ZerosLike[arr_] := Repeat[0, Dims @ arr];

(**************************************************************************************************)

PublicFunction[BasisScalingMatrix]

BasisScalingMatrix[n_, rules_] :=
  ReplaceDiagonalPart[IdentityMatrix @ n, rules];

(**************************************************************************************************)

PublicFunction[ReplaceDiagonalPart]

ReplaceDiagonalPart[matrix_, rules_List] :=
  RepPart[matrix, {#1, #1} -> #2& @@@ rules];

ReplaceDiagonalPart[matrix_, i_ -> v_] :=
  RepPart[matrix, {i, i} -> v];

(**************************************************************************************************)

PublicFunction[AugmentedIdentityMatrix]

SetUsage @ "
AugmentedIdentityMatrix[n$, {i$, j$}] represents the identity n$ \[Times] n$ matrix with an additional one at position ($i, $j).
AugmentedIdentityMatrix[n$, {{i$1, j$1}, {i$2, j$2}, $$}}] puts additional ones at several positions.
"

AugmentedIdentityMatrix[n_, {i_, j_}] := RepPart[IdentityMatrix[n], {i, j} -> 1];
AugmentedIdentityMatrix[n_, list_List] := RepPart[IdentityMatrix[n], list -> 1];

(**************************************************************************************************)
(** Padding                                                                                       *)
(**************************************************************************************************)

PublicFunction[PadRows]

PadRows[ragged_, item_] := Scope[
  w = Max[Len /@ ragged];
  ToPacked @ Map[padToLength[w, item], ragged]
]

padToLength[n_, item_][vector_] := Scope[
  l = Len[vector];
  If[l < n, Join[vector, Repeat[item, n - l]], vector]
];

(**************************************************************************************************)

PublicFunction[PadColumns]

PadColumns[ragged_, n_, item_] := Scope[
  full = PadRows[ragged, item];
  w = Len @ F @ full;
  padToLength[n, Repeat[item, w]] @ full
]

(**************************************************************************************************)
(** Block matrix functions                                                                        *)
(**************************************************************************************************)

PublicFunction[BlockDiagonalMatrix2]

BlockDiagonalMatrix2[blocks_] := Scope[
  range = Range @ Len @ blocks;
  If[!MatchQ[blocks, {Repeated[_ ? MatrixQ]}], ReturnFailed[]];
  superMatrix = DiagonalMatrix[range] /. RuleThread[range, blocks];
  ToPacked @ ArrayFlatten[superMatrix, 2]
];

(**************************************************************************************************)

PublicFunction[FindDiagonalBlockPositions]

rangeTrueQ[func_, i_, j_] := And @@ Table[func[x], {x, i, j}];
firstTrueInRange[func_, i_, j_] := Block[{}, Do[If[func[x], Return[x, Block]], {x, i, j}]; j + 1];

BooleArrayPlot[arr_] := ArrayPlot[arr, Mesh -> True, PixelConstrained -> 10, ColorRules -> {False -> Red, True -> Green}];

FindDiagonalBlockPositions[matrices_] := Scope[
  trans = Transpose[matrices, {3,1,2}];
  isZero = MatrixMap[ZerosQ, trans];
  n = Len[trans]; n2 = n - 1;
  isZeroD = Table[And @@ isZero[[(i+1);;, j]], {i, n2}, {j, n2}];
  isZeroR = Table[And @@ isZero[[i, (j+1);;]], {i, n2}, {j, n2}];
  If[FreeQ[isZeroR, True] || FreeQ[isZeroD, True], Return[{{1, n}}]];
  pos = 1;
  blockPositions = {};
  While[IntQ[pos] && pos <= n,
    lastPos = pos;
    pos = firstTrueInRange[
      next |-> And[
        rangeTrueQ[isZeroR[[#, next]]& , pos, next],
        rangeTrueQ[isZeroD[[next, #]]&, pos, next]
      ],
      pos, n2
    ];
    AppTo[blockPositions, {lastPos, pos}];
    pos += 1;
  ];
  blockPositions
];

(**************************************************************************************************)

PublicFunction[FindDiagonalBlocks]

FindDiagonalBlocks[matrices_] := Scope[
  positions = FindDiagonalBlockPositions[matrices];
  matrices[[All, #, #]]& /@ (Span @@@ positions)
]

(**************************************************************************************************)

PublicFunction[DiagonalBlock]

DiagonalBlock[matrix_ ? MatrixQ, {i_, j_}] := Part[matrix, i;;j, i;;j];
DiagonalBlock[matrixList_ /; VecQ[matrixList, MatrixQ], {i_, j_}] := Part[matrixList, All, i;;j, i;;j];

DiagonalBlock[obj_, list:{__List}] := Map[DiagonalBlock[obj, #]&, list];

DiagonalBlock[part_][obj_] := DiagonalBlock[obj, part];

(**************************************************************************************************)
(** Misc utilities                                                                               **)
(**************************************************************************************************)

PublicFunction[FindIndependentVectors]

FindIndependentVectors[vectors_] := Scope[
  rowReduced = RowReduce @ Transpose @ vectors;
  pivotPositions = Flatten[Position[#, Except[0, _ ? NumericQ], 1, 1]& /@ rowReduced];
  Part[vectors,  pivotPositions]
]

(**************************************************************************************************)

PublicFunction[MatrixSimplify]

MatrixSimplify[matrix_] := Scope[
  entries = Flatten[matrix];
  gcd = PolynomialGCD[Sequence @@ entries];
  If[gcd === 1, Return[{matrix, 1}]];
  {Simplify @ Cancel[matrix / gcd], gcd}
];

(**************************************************************************************************)

PublicFunction[ExtendedSparseArray]

ExtendedSparseArray[{} | <||>, sz_] := SparseArray[{}, sz];

ExtendedSparseArray[assoc_Assoc, sz_] := SparseArray[Normal @ assoc, sz];

ExtendedSparseArray[list:{___Int} ? DuplicateFreeQ, sz_] := SparseArray[Thread[list -> 1], sz];

ExtendedSparseArray[list:{___List} ? DuplicateFreeQ, sz_] := SparseArray[Thread[list -> 1], sz];

ExtendedSparseArray[list:{___List}, sz_] := SparseArray[Normal @ Counts @ list, sz];

ExtendedSparseArray[list:{___Rule}, sz_] := SparseArray[sumRules @ list, sz];

(**************************************************************************************************)

sumRules[rules_] := Normal @ Merge[rules, Total];

(**************************************************************************************************)

PublicFunction[FromSparseRows]

FromSparseRows[rowSpecs_List, n_Int] := SparseArray[
  Flatten @ MapIndex1[rowSpecToFullSpec, rowSpecs],
  {Len @ rowSpecs, n}
];

FromSparseRows[rowSpecs_List] := SparseArray[
  Flatten @ MapIndex1[rowSpecToFullSpec, rowSpecs]
];

rowSpecToFullSpec[{}, row_] := {};

rowSpecToFullSpec[cols:{__Rule}, row_] := VectorApply[{row, #1} -> #2&, sumRules @ cols];

rowSpecToFullSpec[cols_List -> k_, row_] := sumRules @ Map[{row, #} -> k&, cols];
rowSpecToFullSpec[cols_List, row_] := sumRules @ Map[{row, #} -> 1&, cols];

rowSpecToFullSpec[col_Int -> k_, row_] := {{row, col} -> k};
rowSpecToFullSpec[col_Int, row_] := {{row, col} -> 1};

(**************************************************************************************************)

PublicFunction[FromSparseColumns]

FromSparseColumns[args___] := Transpose @ FromSparseRows[args];

(**************************************************************************************************)

PublicFunction[SparseTotalMatrix]

SparseTotalMatrix[indexSets_, n_] := FromSparseRows[indexSets, n];

(**************************************************************************************************)

PublicFunction[SparseAveragingMatrix]

SparseAveragingMatrix[indexSets_, n_] := FromSparseRows[Map[set |-> set -> 1.0 / Len[set], indexSets], n];

(**************************************************************************************************)

PublicFunction[SparseBroadcastMatrix]

SparseBroadcastMatrix[indexSets_, n_] := FromSparseColumns[indexSets, n];

(**************************************************************************************************)

PublicFunction[DifferenceMatrix]

DifferenceMatrix[{}] := {};

DifferenceMatrix[points_] := Outer[Plus, points, -points, 1];

DifferenceMatrix[points1_, points2_] := Outer[Plus, points1, -points2, 1];

(**************************************************************************************************)

PublicFunction[DistanceMatrix]

DistanceMatrix[{}] := {};

DistanceMatrix[points_ ? RealVectorQ] :=
  Outer[Dist, points, points];

DistanceMatrix[points1_ ? RealVectorQ, points2_ ? RealVectorQ] :=
  Outer[Dist, points1, points2];

DistanceMatrix[points_ ? RealMatrixQ] := (
  $loadDM; $distanceMatrixFunction1[points, $euclideanDistanceCode, False]
);

DistanceMatrix[{}, _] := {};
DistanceMatrix[_, {}] := {};

DistanceMatrix[points1_ ? RealMatrixQ, points2_ ? RealMatrixQ] := (
  $loadDM; $distanceMatrixFunction2[points1, points2, $euclideanDistanceCode, False]
);

(**************************************************************************************************)

PublicFunction[MinimumDistance]

MinimumDistance[{}] := 0;
MinimumDistance[coords_] := Scope[
  dists = DistanceMatrix[N @ coords];
  Min @ Decases[Flatten @ dists, 0|0.]
];

(**************************************************************************************************)

PublicFunction[SquaredDistanceMatrix]

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

SquaredDistanceMatrix::badarray = "Input was not an real vector or matrix.";
s_SquaredDistanceMatrix := (Message[badarray]; $Failed);

$loadDM := (
  Get["NumericArrayUtilities`"];
  $squaredEuclideanDistanceCode := NumericArrayUtilities`DistanceMatrix`PackagePrivate`$extractLLDMMethod["SquaredEuclideanDistance"];
  $euclideanDistanceCode := NumericArrayUtilities`DistanceMatrix`PackagePrivate`$extractLLDMMethod["EuclideanDistance"];
  $distanceMatrixFunction1 = NumericArrayUtilities`DistanceMatrix`PackagePrivate`mTensorDistanceMatrix1Arg;
  $distanceMatrixFunction2 = NumericArrayUtilities`DistanceMatrix`PackagePrivate`mTensorDistanceMatrix2Arg;
  Clear[$loadDM];
);

(**************************************************************************************************)

PublicFunction[MatrixThread, MatrixMax, MatrixMin]

MatrixMax[m__] := MatrixThread[Max, m];
MatrixMin[m__] := MatrixThread[Min, m];
MatrixThread[f_, m__] := MapThread[f, {m}, 2];

(**************************************************************************************************)

PublicFunction[VectorThread, VectorMax, VectorMin]

VectorMax[m__] := VectorThread[Max, m];
VectorMin[m__] := VectorThread[Min, m];
VectorThread[f_, v__] := MapThread[f, {v}];