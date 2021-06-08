PackageScope["$ParameterizedLatticeData"]
PackageScope["$ParameterizedLatticeNames"]

$ParameterizedLatticeData = <||>;
$ParameterizedLatticeNames = {};

(**************************************************************************************************)

PackageExport["DefineParameterizedLatticeQuiver"]

DefineParameterizedLatticeQuiver[name_String, func_, params_] := (
  AppendTo[$ParameterizedLatticeNames, name];
  $ParameterizedLatticeData[name] = <|"Factory" -> func, "Parameters" -> params|>;
);

(**************************************************************************************************)

torusNormOptions = MatchValues[
  {m_, Infinity, d_} := {
    MaxDepth -> m + d + 1,
    NormFunction -> (1 -> Abs),
    MaxNorm -> d
  };
  _ := MaxDepth -> Infinity;
];

torusCoordinate3DOptions = MatchValues[
  {m_, Infinity, d_} := {
    Epilog -> {
      EdgeForm[None], Opacity[0.5],
      FaceForm[{Glow @ GrayLevel[1.0], Specularity[1]}], Cylinder[{{-d, 0, 0}, {d, 0, 0}}, m / Tau - (1/Tau)]
    },
    VertexCoordinateFunction -> TimesOperator[{1, Tau / m}] /* TubeVector[m / Tau],
    CoordinateTransformFunction -> ProjectionOnto[Cylinder[{{-d*2*Sqrt[3], 0, 0}, {d*2*Sqrt[3], 0, 0}}, m / Tau - (0.1/Tau)]],
    ViewOptions -> {ViewPoint -> {0.4, 1.5, 0.2}, ViewProjection -> "Orthographic"}
  };
  {m_, n_, _} := {
    VertexCoordinateFunction -> TimesOperator[Tau / {m, n}] /* TorusVector[{m / Tau, (n + m) / Tau}],
    CoordinateTransformFunction -> ProjectionOnto[Torus[{m, (n + m)} / Tau]],
    ViewOptions -> {ViewPoint -> {0.4, 1.5, 0.8}, ViewProjection -> "Orthographic"}
  }
];

chooseTorus3DOptions[spec_, isABC_] := {
  torusNormOptions @ spec,
  torusCoordinate3DOptions @ spec
};

$abcProj = {{0, Sqrt[3]/2, 0}, {1, 0, 0}};

chooseTorus2DOptions[spec_, isABC_] := Scope[
  modSpec = If[isABC, Dot[$abcProj, spec], Take[spec, 2]];
  {
    torusNormOptions @ spec,
    EdgeShapeFunction -> drawModulusEdge[modSpec],
    VertexCoordinateFunction -> torus2DCoords,
    ImagePadding -> 35
  }
];

torus2DCoords[{a_, b_}] := {b, a};
torus2DCoords[vec:{_, _, _}] := Dot[$abcVectors, vec];

chooseTorusOptions[userOpts_, spec_, isABC_] :=
  If[Lookup[userOpts, LayoutDimension] === 2, chooseTorus2DOptions, chooseTorus3DOptions][spec, isABC];

$torusParameters = <|"m" -> 4, "n" -> Infinity, "t" -> 0, "MaxDepth" -> 4|>;

(**************************************************************************************************)

PackageExport["UniqueLabel"]

SetUsage @ "
UniqueLabel[n$] represents a numeric label in a plot that should be numbered in raster order.
"

drawModulusEdge[{m_, n_}][assoc_] := Scope[
  UnpackAssociation[assoc, coordinates, arrowheads, shape, edgeIndex, labelStyle];
  {a, b} = {{ax, ay}, {bx, by}} = FirstLast[coordinates];
  offsets = Delete[Tuples[Reverse @ {{-m, 0, m}, {-n, 0, n}}], 5];
  If[EuclideanDistance[a, b] > 1,
    b2 = findModulusCounterpart[a, b, offsets, .4];
    a2 = findModulusCounterpart[b, a, offsets, .4];
    counter = assoc["Counter"];
    labelPoints = {1.35*(a2 - b) + b, (b2-a)*1.35 + a};
    label = If[labelStyle =!= None, Map[makeWrappedEdgeLabel[counter, labelStyle], labelPoints]];
    arrowheadsA = changeArrowheadPos[arrowheads, 0.8];
    arrowheadsB = changeArrowheadPos[arrowheads, 0.2];
    {
      Style[shape @ {a, b2}, arrowheadsA],
      Style[shape @ {a2, b}, arrowheadsB],
      label
    }
  ,
    Style[shape @ {a, b}, arrowheads]
  ]
];

makeWrappedEdgeLabel[counter_, labelStyle_][pos_] :=
  Text[Style[UniqueLabel @ counter, Opacity[1], labelStyle], pos,
    {0, 0},
    Background -> White, BaseStyle -> {FontSize -> 8}];

changeArrowheadPos[Arrowheads[{{sz_, _, g_}}], pos_] :=
  Arrowheads[{{sz, pos, g}}];

changeArrowheadPos[g_, _] := g;

findModulusCounterpart[a_, b_, offsets_, d_] := Scope[
  bs = PlusOperator[b] /@ offsets;
  b2 = MinimumBy[bs, EuclideanDistance[a, #]&];
  PointAlongLine[a, b2, d]
]

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["SquareTorus", squareTorusFactory, $torusParameters];

squareTorusFactory[<|"m" -> m_, "n" -> n_, "t" -> t_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  rep = QuiverRepresentation[
    BouquetQuiver["xy"],
    TranslationGroup[{{1, 0}, {t, 1}}, {m, n}]
  ];
  opts = chooseTorusOptions[userOpts, {m, n, d}, False] /. (1 -> Abs) -> (2 -> Abs);
  {rep, opts}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["TriangularTorus", triangularTorusFactory, $torusParameters];

$s32 = Sqrt[3]/2;
$abcVectors = Simplify /@ {{0, 1}, {-$s32, -1/2}, {$s32, -1/2}};

makeABCTorusRepresentation[m_, n_, t_] := Scope[
  dx = t/m * $s32;
  TranslationGroup[
    $abcVectors,
    {n * $s32, m} /. 0|1 -> Infinity
  ]
];

triangularTorusFactory[<|"m" -> m_, "n" -> n_, "t" -> t_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  rep = QuiverRepresentation[
    BouquetQuiver["abc"],
    makeABCTorusRepresentation[m, n, t]
  ];
  opts = chooseTorusOptions[userOpts, {m, n, d}, True];
  {rep, opts}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["HexagonalTorus", hexagonalTorusFactory, $torusParameters];

hexagonalTorusFactory[<|"m" -> m_, "n" -> n_, "t" -> t_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  rep = QuiverRepresentation[
    Quiver @ Labeled[1 -> 2, "a" | "b" | "c"],
    makeABCTorusRepresentation[m, n, t]
  ];
  opts = chooseTorusOptions[userOpts, {m, n, d}, True];
  opts = DeleteOptions[CoordinateTransformFunction] @ Flatten @ opts;
  {rep, opts}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["Lamplighter", lamplighterFactory, <|"n" -> 3, "MaxDepth" -> Infinity|>];

lamplighterFactory[<|"n" -> n_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  rep = <|
    "CayleyFunction" -> LamplighterCayleyFunction,
    "InitialStates" -> {LatticeVertex[Zeros[n], 1]}
  |>;
  {rep, MaxDepth -> d}
];

LamplighterCayleyFunction[LatticeVertex[lamps_, pos_]] := {
  Labeled[LatticeVertex[lamps, Mod[pos + 1, Length[lamps], 1]], "x"],
  Labeled[LatticeVertex[lamps, Mod[pos - 1, Length[lamps], 1]], Negated @ "x"],
  Labeled[LatticeVertex[MapAt[1-#&, lamps, pos], pos], "f"]
};

