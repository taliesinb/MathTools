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

chooseTorusNormOptions = MatchValues[
  {m_, Infinity, d_} := {
    MaxDepth -> m + d + 1,
    NormFunction -> (1 -> Abs),
    MaxNorm -> d
  };
  _ := MaxDepth -> Infinity;
];

chooseTorusCoordinateTransform = MatchValues[
  {m_, Infinity, d_} := {
    Epilog -> {
      EdgeForm[None], Opacity[0.5],
      FaceForm[{Glow @ GrayLevel[1.0], Specularity[1]}], Cylinder[{{-d, 0, 0}, {d, 0, 0}}, m / Tau - (1/Tau)]
    },
    CoordinateTransformFunction -> ProjectionOnto[Cylinder[{{-d*2*Sqrt[3], 0, 0}, {d*2*Sqrt[3], 0, 0}}, m / Tau - (0.1/Tau)]]
  };
  {m_, n_, _} := {
    CoordinateTransformFunction -> ProjectionOnto[Torus[{m, (n + m)} / Tau]]
  }
];

chooseTorusViewOptions = MatchValues[
  {_, Infinity, _} :=
    ViewOptions -> {ViewPoint -> {0.4, 1.5, 0.2}, ViewProjection -> "Orthographic"};
  _ :=
    ViewOptions -> {ViewPoint -> {0.4, 1.5, 0.8}, ViewProjection -> "Orthographic"};
];

chooseTorusOptions[spec_] := {
  chooseTorusNormOptions @ spec,
  chooseTorusCoordinateTransform @ spec,
  chooseTorusViewOptions @ spec
};

$torusParameters = <|"m" -> 4, "n" -> Infinity, "t" -> 0, "MaxDepth" -> 4|>;

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["SquareTorus", squareTorusFactory, $torusParameters];

squareTorusFactory[<|"m" -> m_, "n" -> n_, "t" -> t_, "MaxDepth" -> d_|>] := Scope[
  rep = QuiverRepresentation[
    BouquetQuiver["xy"],
    TranslationGroup[{{1, 0}, {t, 1}}, {m, n}]
  ];
  {rep, chooseTorusOptions[{m, n, d}] /. (1 -> Abs) -> (2 -> Abs)}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["TriangularTorus", triangularTorusFactory, $torusParameters];

$s32 = Sqrt[3]/2;

makeABCTorusRepresentation[m_, n_, t_] := Scope[
  dx = t/m * $s32;
  TranslationGroup[
    Simplify /@ {{0, 1}, {-$s32, -1/2}, {$s32, -1/2}},
    {n * $s32, m} /. 0|1 -> Infinity
  ]
];

triangularTorusFactory[<|"m" -> m_, "n" -> n_, "t" -> t_, "MaxDepth" -> d_|>] := Scope[
  rep = QuiverRepresentation[
    BouquetQuiver["abc"],
    makeABCTorusRepresentation[m, n, t]
  ];
  {rep, chooseTorusOptions @ {m, n, d}}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["HexagonalTorus", hexagonalTorusFactory, $torusParameters];

hexagonalTorusFactory[<|"m" -> m_, "n" -> n_, "t" -> t_, "MaxDepth" -> d_|>] := Scope[
  rep = QuiverRepresentation[
    Quiver @ Labeled[1 -> 2, "a" | "b" | "c"],
    makeABCTorusRepresentation[m, n, t]
  ];
  {rep, DeleteOptions[Flatten @ chooseTorusOptions @ {m, n, d}, CoordinateTransformFunction]}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["Lamplighter", lamplighterFactory, <|"n" -> 3, "MaxDepth" -> Infinity|>];

lamplighterFactory[<|"n" -> n_, "MaxDepth" -> d_|>] := Scope[
  UnpackAssocation[params, n];
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

