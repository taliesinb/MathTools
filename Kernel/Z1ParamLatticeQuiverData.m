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
  {Infinity, h_, d_} := {
    SetAutomatic[d, 4];
    MaxDepth -> h + d + 1,
    NormFunction -> (1 -> Abs),
    MaxNorm -> d
  };
  {_, _, d_} := {
    SetAutomatic[d, Infinity];
    MaxDepth -> d
  }
];

LatticeGraph::badtorusopts = "Height should both be integer, and width should be an integer or infinity."

torusCoordinate3DOptions = MatchValues[
  {Infinity, h_Integer, d_} := {
    SetAutomatic[d, 4];
    Epilog -> {
      EdgeForm[None], Opacity[0.5],
      FaceForm[{Glow @ GrayLevel[1.0], Specularity[1]}], Cylinder[{{-d, 0, 0}, {d, 0, 0}}, h / Tau - (1/Tau)]
    },
    VertexCoordinateFunction -> TimesOperator[{1, Tau / h}] /* TubeVector[h / Tau],
    CoordinateTransformFunction -> ProjectionOnto[Cylinder[{{-d*2*Sqrt[3], 0, 0}, {d*2*Sqrt[3], 0, 0}}, h / Tau - (0.1/Tau)]],
    ViewOptions -> {ViewPoint -> {0.4, 1.5, 0.2}, ViewProjection -> "Orthographic"}
  };
  {w_Integer, h_Integer, _} := {
    VertexCoordinateFunction -> TimesOperator[Tau / {w, h} / {$ws, 1}] /* TorusVector[{w * $ws + h, h} / Tau],
    CoordinateTransformFunction -> ProjectionOnto[Torus[{w + h, h} / Tau]],
    ViewOptions -> {ViewPoint -> {0.4, 1.5, 0.8}, ViewProjection -> "Orthographic"}
  };
  _ := (Message[LatticeGraph::badtorusopts]; $Failed);
];

chooseTorus3DOptions[spec_] := {
  torusNormOptions @ spec,
  Block[{$ws = If[$isABC, $s32, 1]}, torusCoordinate3DOptions @ spec]
};

$abcProj = {{Sqrt[3]/2, 0, 0}, {0, 1, 0}};

chooseTorus2DOptions[spec_] := Scope[
(*   translations = Match[$translations,
    {x_, y_} :> {x, y},
    {a_, b_, c_} :> {(b - c)/2, a}
  ];
  {o1, o2} = Take[spec, 2] * translations;
  offsets = {o1, o2, -o1, -o2, o1 + o2, o1 - o2, -o1 + o2, -o1 - o2}; *)
  {w, h} = Take[spec, 2]; If[$isABC, w *= $s32]; dx = {w, 0}; dy = {0, h};
  offsets = {dx, dy, -dx, -dy, dx + dy, dx - dy, -dx + dy, -dx - dy};
  {
    torusNormOptions @ spec,
    EdgeShapeFunction -> drawModulusEdge[offsets],
    VertexCoordinateFunction -> Identity,
    ImagePadding -> If[$doLabels, 35, 20]
  }
];

chooseTorusOptions[userOpts_, spec_, rep_] := Scope[
  $translations = Map[Normal /* ExtractTranslationVector, rep["Representation"]["Generators"]];
  $doLabels = Lookup[userOpts, EdgeLabelStyle] =!= None;
  $isABC = MatchQ[rep["Cardinals"], {"a", "b", "c"}];
  func = If[Lookup[userOpts, LayoutDimension] === 2, chooseTorus2DOptions, chooseTorus3DOptions];
  Flatten @ func @ spec
];

$torusParameters = <|"w" -> 4, "h" -> None, "t" -> 0, "MaxDepth" -> Automatic|>;

(**************************************************************************************************)

PackageExport["UniqueLabel"]

SetUsage @ "
UniqueLabel[n$] represents a numeric label in a plot that should be numbered in raster order.
"

drawModulusEdge[offsets_][assoc_] := Scope[
  UnpackAssociation[assoc, coordinates, arrowheads, shape, edgeIndex, labelStyle];
  {a, b} = {{ax, ay}, {bx, by}} = FirstLast @ coordinates;
  b2 = findModulusCounterpart[a, b, offsets, .4];
  a2 = findModulusCounterpart[b, a, offsets, .4];
  If[a2 =!= None && b2 =!= None,
    counter = assoc["Counter"];
    labelPoints = {1.35*(a2 - b) + b, (b2-a)*1.35 + a};
    label = If[labelStyle === None, Nothing, Map[makeWrappedEdgeLabel[counter, labelStyle], labelPoints]];
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
  If[EuclideanDistance[a, b2] > EuclideanDistance[a, b], None,
    PointAlongLine[a, b2, d]
  ]
]

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["SquareTorus", squareTorusFactory, $torusParameters];

squareTorusFactory[<|"w" -> w_, "h" -> h_, "t" -> t_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  If[h === None, h = w; w = Infinity];
  rep = QuiverRepresentation[
    BouquetQuiver["xy"],
    TranslationGroup[{{1, 0}, {t/h, 1}}, {w, h}]
  ];
  opts = chooseTorusOptions[userOpts, {w, h, d}, rep];
  {rep, opts}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["TriangularTorus", triangularTorusFactory, $torusParameters];

$s32 = Sqrt[3]/2;
$abcVectors = Simplify /@ {{0, 1}, {-$s32, -1/2}, {$s32, -1/2}};

makeABCTorusRepresentation[w_, h_, t_] := Scope[
  If[t != 0,
    dx = $s32/h;
    TranslationGroup[
      {{1/h, 1 + 1/w}, {-(1+h)/h, -(1+h+w)/w}, {1, h/w}},
      {w, h} /. 0|1 -> Infinity
    ]
  ,
    TranslationGroup[
      $abcVectors,
      {w * $s32, h} /. 0|1 -> Infinity
    ]
  ]
];

triangularTorusFactory[<|"w" -> w_, "h" -> h_, "t" -> t_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  If[h === None, h = w; w = Infinity];
  rep = QuiverRepresentation[
    BouquetQuiver["abc"],
    makeABCTorusRepresentation[w, h, t]
  ];
  opts = chooseTorusOptions[userOpts, {w, h, d}, rep];
  {rep, opts}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["HexagonalTorus", hexagonalTorusFactory, $torusParameters];

hexagonalTorusFactory[<|"w" -> w_, "h" -> h_, "t" -> t_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  If[h === None, h = w; w = Infinity];
  rep = QuiverRepresentation[
    Quiver @ Labeled[1 -> 2, "a" | "b" | "c"],
    makeABCTorusRepresentation[w, h, t]
  ];
  opts = chooseTorusOptions[userOpts, {w, h, d}, rep];
  opts = DeleteOptions[CoordinateTransformFunction] @ Flatten @ opts;
  {rep, opts}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["TrihexagonalTorus", trihexagonalTorusFactory, $torusParameters];

trihexagonalTorusFactory[<|"w" -> w_, "h" -> h_, "t" -> t_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  If[h === None, h = w; w = Infinity];
  rep = QuiverRepresentation[
    Quiver[<|"B" -> {1 -> 2, 2 -> 1}, "C" -> {2 -> 3, 3 -> 2}, "A" -> {3 -> 1, 1 -> 3}|>],
    makeABCTorusRepresentation[w, h, t]
  ];
  opts = chooseTorusOptions[userOpts, {w, h, d}, rep];
  opts = DeleteOptions[CoordinateTransformFunction] @ Flatten @ opts;
  {rep, opts}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["RhombilleTorus", rhombilleTorusFactory, $torusParameters];

rhombilleTorusFactory[<|"w" -> w_, "h" -> h_, "t" -> t_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  If[h === None, h = w; w = Infinity];
  rep = QuiverRepresentation[
    Quiver @ Labeled[{1 -> 2, 2 -> 3}, "a" | "b" | "c"],
    makeABCTorusRepresentation[w, h, t]
  ];
  opts = chooseTorusOptions[userOpts, {w, h, d}, rep];
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
  {rep, {MaxDepth -> d}}
];

LamplighterCayleyFunction[LatticeVertex[lamps_, pos_]] := {
  Labeled[LatticeVertex[lamps, Mod[pos + 1, Length[lamps], 1]], "x"],
  Labeled[LatticeVertex[lamps, Mod[pos - 1, Length[lamps], 1]], Negated @ "x"],
  Labeled[LatticeVertex[MapAt[1-#&, lamps, pos], pos], "f"]
};

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["Cube", cubeFactory, <|"n" -> 1, "MaxDepth" -> Infinity|>]

$cubeDirs = {{1,0,0}, {-1,0,0}, {0,1,0}, {0,-1,0}, {0,0,1}, {0,0,-1}};
$cubeAxes = IdentityMatrix[3];

makeCubeFace[n_][dir_, x_, y_] := Scope[
  {a, b} = Select[$cubeAxes, Dot[#, dir] == 0&];
  num = n + 2;
  dn = Interpolated[-1, 1, num]; dx = 2 / (n + 1);
  p = Outer[dir + a * #1 + b * #2&, dn, dn];
  Do[
    If[j < num, makeCubeEdge[x, Part[p, i, {j, j + 1}]]];
    If[i < num, makeCubeEdge[y, Part[p, {i, i + 1}, j]]];
  ,
    {i, num}, {j, num}
  ];
  Internal`StuffBag[$points, p, 2];
];

makeCubeEdge[c_, {a_, b_}] :=
  Internal`StuffBag[$edges, DirectedEdge[a, b, c]];

cubeFactory[<|"n" -> n_, "MaxDepth" -> _|>, userOpts_] := Scope[
  CollectTo[{$points, $edges},
    MapThread[makeCubeFace[n], {$cubeDirs,
      {"a", Negated @ "a", "c", Negated @ "c", Negated @ "c", "c"},
      {"b", Negated @ "b", Negated @ "b", "b", Negated @ "a", "a"}
    }]
  ];
  $points //= DeleteDuplicates;
  $edges //= DeleteDuplicates;
  innerCube = FadeProtected @ {
    EdgeForm[None], Opacity[0.5],
    FaceForm[None, {Glow @ GrayLevel[1.0], Specularity[1]}],
    GraphicsValue["PrimitiveSize",
      With[{z = 1-#PlotRange/2}, Cuboid[-{1,1,1} * z, {1,1,1} * z]]&
    ]
  };
  graph = ExtendedGraph[$points, $edges, VertexCoordinates -> $points,
    Sequence @@ Normal[userOpts],
    ViewOptions -> {ViewVector -> {3,2.5,2}, ViewProjection -> "Orthographic"},
    VertexShapeFunction -> None, ImageSize -> Small, EdgeThickness -> 2,
    ArrowheadSize -> Medium,
    Epilog -> innerCube
  ];
  If[!GraphQ[graph], ReturnFailed[]];
  graph // CombineMultiedges
];
