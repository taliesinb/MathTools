Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


(**************************************************************************************************)
(** Operators useful for Matrices                                                                 *)
(**************************************************************************************************)

PackageExport["TakeOperator"]

TakeOperator[spec___][e_] := Take[e, spec];


PackageExport["DropOperator"]

DropOperator[spec___][e_] := Drop[e, spec];


PackageExport["PartOperator"]

PartOperator[spec___][e_] := Part[e, spec];


PackageExport["DotOperator"]

DotOperator[matrix_][other_] := Dot[matrix, other]


PackageExport["TimesOperator"]

TimesOperator[a_][b_] := a * b;


PackageExport["PlusOperator"]

PlusOperator[a_][b_] := a + b;


PackageExport["InnerDimension"]

InnerDimension[array_] := Last @ Dimensions @ array;


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
CoordinateVectorQ[_, _] := False;


PackageExport["CoordinateMatrixQ"]

CoordinateMatrixQ[matrix_, n_:2|3] :=
  MatrixQ[matrix] && MatchQ[InnerDimension @ matrix, n];


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


PackageExport["PermutationMatrixQ"]

PermutationMatrixQ[matrix_] :=
  SquareMatrixQ[matrix] && RealMatrixQ[matrix] @@ MinMax[matrix] == {0, 1} && Count[matrix, 1, 2] == Length[matrix] &&
    OnesQ[Total[matrix, {1}]] && OnesQ[Total[matrix, {2}]];


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
(** Block matrix functions                                                                        *)
(**************************************************************************************************)

PackageExport["BlockDiagonalMatrix"]

BlockDiagonalMatrix[blocks_] := Scope[
  range = Range @ Length @ blocks;
  If[!MatchQ[blocks, {Repeated[_ ? MatrixQ]}], ReturnFailed[]];
  superMatrix = DiagonalMatrix[range] /. MapThread[Rule, {range, blocks}];
  ToPacked @ ArrayFlatten[superMatrix, 2]
];


PackageExport["FindDiagonalBlockPositions"]

rangeTrueQ[func_, i_, j_] := And @@ Table[func[x], {x, i, j}];
firstTrueInRange[func_, i_, j_] := Block[{}, Do[If[func[x], Return[x, Block]], {x, i, j}]; j + 1];

BooleArrayPlot[arr_] := ArrayPlot[arr, Mesh -> True, PixelConstrained -> 10, ColorRules -> {False -> Red, True -> Green}];

FindDiagonalBlockPositions[matrices_] := Scope[
  trans = Transpose[matrices, {3,1,2}];
  isZero = Map[ZerosQ, trans, {2}];
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


PackageExport["MatrixSimplify"]

MatrixSimplify[matrix_] := Scope[
  entries = Flatten[matrix];
  gcd = PolynomialGCD[Sequence @@ entries];
  If[gcd === 1, Return[{matrix, 1}]];
  {Simplify @ Cancel[matrix / gcd], gcd}
];


PackageExport["SquaredDistanceMatrix"]

SquaredDistanceMatrix[points_] := (
  $loadDM; $distanceMatrixFunction1[points, $squaredEuclideanDistanceCode, False]
);

SquaredDistanceMatrix[points1_, points2_] := (
  $loadDM; $distanceMatrixFunction2[points1, points2, $squaredEuclideanDistanceCode, False]
);

$loadDM := (
  DistanceMatrix[{{1,2}},{{3,4}}, DistanceFunction -> "SquaredEuclideanDistance"];
  $squaredEuclideanDistanceCode := NumericArrayUtilities`DistanceMatrix`PackagePrivate`$extractLLDMMethod["SquaredEuclideanDistance"];
  $distanceMatrixFunction1 = NumericArrayUtilities`DistanceMatrix`PackagePrivate`mTensorDistanceMatrix1Arg;
  $distanceMatrixFunction2 = NumericArrayUtilities`DistanceMatrix`PackagePrivate`mTensorDistanceMatrix2Arg;
  Clear[$loadDM];
);

