Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["Ones"]
PackageExport["Zeros"]

Ones[i_] := ConstantArray[1, i];
Zeros[i_] := ConstantArray[0, i];

PackageExport["ComplexMatrixQ"]

ComplexMatrixQ[e_] := ContainsQ[e, _Complex] && MatrixQ[e];


PackageExport["OnesQ"]
PackageExport["ZerosQ"]

OnesQ[m_] := MinMax[m] === {1, 1};
ZerosQ[m_] := MinMax[m] === {0, 0};


PackageExport["UpperUnitriangularMatrixQ"]

UpperUnitriangularMatrixQ[matrix_] :=
  UpperTriangularMatrixQ[matrix] && OnesQ[Diagonal[matrix]];


PackageExport["ZeroMatrixQ"]

ZeroMatrixQ[matrix_] := MatrixQ[matrix] && ZerosQ[matrix];


PackageExport["ZeroMatrix"]

SetUsage @ "
ZeroMatrix[n$] represents the zero n$ \[Times] n$ matrix.
"

ZeroMatrix[n_] := ConstantArray[0, {n, n}];


PackageExport["UnitAffineMatrix"]

SetUsage @ "
UnitAffineMatrix[n$, {i$, j$}] represents the identity n$ \[Times] n$ matrix with an additional one at position ($i, $j).
"

UnitAffineMatrix[n_, {i_, j_}] := ReplacePart[IdentityMatrix[n], {i, j} -> 1];


PackageExport["AbelianMatrixQ"]

AbelianMatrixQ[matrix_] :=
  UpperUnitriangularMatrixQ[matrix] && ZeroMatrixQ[matrix[[;;-2, ;;-2]] - IdentityMatrix[Length[matrix] - 1]];


PackageExport["AbelianVector"]

AbelianVector[matrix_] := matrix[[;;-2, -1]];


PackageExport["AbelianMatrix"]

AbelianMatrix[vector_] := Scope[
  matrix = IdentityMatrix[Length[vector] + 1];
  matrix[[;;-2, -1]] = vector;
  matrix
];


PackageExport["BlockDiagonalMatrix"]

BlockDiagonalMatrix[blocks_] := Scope[
  range = Range @ Length @ blocks;
  If[!MatchQ[blocks, {Repeated[_ ? MatrixQ]}], ReturnFailed[]];
  superMatrix = DiagonalMatrix[range] /. MapThread[Rule, {range, blocks}];
  Developer`ToPackedArray @ ArrayFlatten[superMatrix, 2]
];


PackageExport["PermutationMatrixQ"]

PermutationMatrixQ[matrix_] :=
  SquareMatrixQ[matrix] && MinMax[matrix] == {0, 1} && Count[matrix, 1, 2] == Length[matrix] &&
    OnesQ[Total[matrix, {1}]] && OnesQ[Total[matrix, {2}]];


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
