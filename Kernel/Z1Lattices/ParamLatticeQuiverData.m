DefineLiteralMacro[getRGBCardinals,
  getRGBCardinals[] := (
    {r, g, b} = cardinals = Lookup[userOpts, Cardinals, {"r", "g", "b"}];
    {R, G, B} = Inverted /@ {r, g, b};
  )
];

DefineLiteralMacro[getRGBWCardinals,
  getRGBWCardinals[] := (
    {r, g, b, w} = cardinals = Lookup[userOpts, Cardinals, {"r", "g", "b", "w"}];
    {R, G, B, W} = Inverted /@ {r, g, b, w};
  )
];

(**************************************************************************************************)

PrivateVariable[$ParameterizedLatticeData, $ParameterizedLatticeNames]

$ParameterizedLatticeData = <||>;
$ParameterizedLatticeNames = {};

(**************************************************************************************************)

PublicFunction[DefineParameterizedLatticeQuiver]

DefineParameterizedLatticeQuiver[name_String, func_, params_] := (
  AppendTo[$ParameterizedLatticeNames, name];
  $ParameterizedLatticeData[name] = <|"Factory" -> func, "Parameters" -> params|>;
);

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["Circle", circleFactory, <|"Size" -> 5, "MaxDepth" -> Infinity|>];

circleFactory[assoc_, userOpts_] := Scope[
  UnpackAssociation[assoc, size];
  vertices = Range @ size;
  Quiver[
    vertices,
    DirectedEdge[#1, #2, "x"]& @@@ Partition[vertices, 2, 1, 1],
    Sequence @@ Normal[userOpts],
    VertexCoordinates -> CirclePoints[size],
    GraphOrigin -> 1,
    ImageSize -> 100, ArrowheadShape -> "Line"
  ]
];

(**************************************************************************************************)

torusNormOptions = Case[
  {Infinity, h_, d_} := Scope @ {
    SetAutomatic[d, 4];
    MaxDepth -> h + d + 1,
    NormFunction -> (1 -> Abs),
    MaxNorm -> d
  };
  {_, _, d_} := Scope @ {
    SetAutomatic[d, Infinity];
    MaxDepth -> d
  }
];

LatticeGraph::badtorusopts = "Height should both be integer, and width should be an integer or infinity."

torusCoordinate3DOptions = Case[
  {Infinity, h_Integer, d_} := Scope @ {
    SetAutomatic[d, 4];
    Epilog -> If[AssociationQ @ $solid, {
      EdgeForm[None], Opacity @ Lookup[$solid, "Opacity", 0.5],
      offset = Lookup[$solid, "Offset", Automatic];
      SetAutomatic[offset, 1 / Tau];
      FaceForm[{Glow @ GrayLevel[1.0], Specularity[1]}], Cylinder[{{-d, 0, 0}, {d, 0, 0}}, h / Tau - offset]
    }, {}],
    VertexCoordinateFunction -> TimesOperator[{1, Tau / h}] /* TubeVector[h / Tau],
    CoordinateTransformFunction -> ProjectionOnto[Cylinder[{{-d*2*Sqrt[3], 0, 0}, {d*2*Sqrt[3], 0, 0}}, h / Tau - (0.1/Tau)]],
    ViewOptions -> {ViewPoint -> {0.4, 1.5, 0.2}, ViewProjection -> "Orthographic"}
  };
  {w_Integer, h_Integer, _} := Scope @ {
    VertexCoordinateFunction -> TimesOperator[Tau / {w, h} / {$ws, 1}] /* TorusVector[{w * $ws + h, h} / Tau],
    Epilog -> If[AssociationQ @ $solid, {
      EdgeForm[None], Opacity @ Lookup[$solid, "Opacity", 0.5],
      FaceForm[{Glow @ GrayLevel[1.0], Specularity[1]}],
      offset = Lookup[$solid, "Offset", Automatic];
      SetAutomatic[offset, .4];
      TorusSurfacePolygon[Torus[{w + h, h - offset} / Tau]]
    }, {}],
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
    EdgeShapeFunction -> ModulusEdgeShapeFunction[offsets],
    VertexCoordinateFunction -> Identity,
    ImagePadding -> If[$doLabels, 35, 20]
  }
];

PublicOption[InteriorSolid]

chooseTorusOptions[userOpts_, spec_, rep_] := Scope[
  $translations = Map[Normal /* ExtractTranslationVector, rep["Representation"]["Generators"]];
  $doLabels = Lookup[userOpts, EdgeLabelStyle] =!= None;
  $isABC = MatchQ[rep["Cardinals"], {"a", "b", "c"}];
  $solid = Lookup[userOpts, InteriorSolid, True];
  If[MatchQ[$solid, True | Automatic], $solid = <||>];
  func = If[Lookup[userOpts, LayoutDimension] === 2, chooseTorus2DOptions, chooseTorus3DOptions];
  Flatten @ func @ spec
];

$torusParameters = <|"w" -> 4, "h" -> None, "t" -> 0, "MaxDepth" -> Automatic|>;

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["SquareTorus", squareTorusFactory, $torusParameters];

squareTorusFactory[<|"w" -> w_, "h" -> h_, "t" -> t_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  If[h === None, h = w; w = Infinity];
  rep = PathRepresentation[
    BouquetQuiver["xy"],
    TranslationGroup[{{1, 0}, {t/h, 1}}, {w, h}]
  ];
  opts = chooseTorusOptions[userOpts, {w, h, d}, rep];
  {rep, opts}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["TriangularTorus", triangularTorusFactory, $torusParameters];

$s32 = Sqrt[3]/2;

makeABCTorusRepresentation[w_, h_, t_] := Scope[
  If[t != 0,
    dx = $s32/h;
    TranslationGroup[
      {{1/h, 1 + 1/w}, {-(1+h)/h, -(1+h+w)/w}, {1, h/w}},
      {w, h} /. 0|1 -> Infinity
    ]
  ,
    TranslationGroup[
      $TriangleVectors,
      {w * $s32, h} /. 0|1 -> Infinity
    ]
  ]
];

triangularTorusFactory[<|"w" -> w_, "h" -> h_, "t" -> t_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  If[h === None, h = w; w = Infinity];
  rep = PathRepresentation[
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
  rep = PathRepresentation[
    Quiver @ Labeled[1 -> 2, "a" | "b" | "c"],
    makeABCTorusRepresentation[w, h, t],
    2 (* for backward compatibility with old content *)
  ];
  opts = chooseTorusOptions[userOpts, {w, h, d}, rep];
  (* opts = DeleteOptions[CoordinateTransformFunction] @ Flatten @ opts; *)
  {rep, opts}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["TrihexagonalTorus", trihexagonalTorusFactory, $torusParameters];

trihexagonalTorusFactory[<|"w" -> w_, "h" -> h_, "t" -> t_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  If[h === None, h = w; w = Infinity];
  rep = PathRepresentation[
    Quiver[<|"B" -> {1 -> 2, 2 -> 1}, "C" -> {2 -> 3, 3 -> 2}, "A" -> {3 -> 1, 1 -> 3}|>],
    makeABCTorusRepresentation[w, h, t],
    2 (* for backward compatibility with old content *)
  ];
  opts = chooseTorusOptions[userOpts, {w, h, d}, rep];
  opts = DeleteOptions[CoordinateTransformFunction] @ Flatten @ opts;
  {rep, opts}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["RhombitrihexagonalTorus", rhombitrihexagonalTorusFactory, $torusParameters];

rhombitrihexagonalTorusFactory[<|"w" -> w_, "h" -> h_, "t" -> t_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  If[h === None, h = w; w = Infinity];
  rep = Quiver @ <|
    1 -> {6 -> 4, 1 -> 3},
    2 -> {3 -> 2, 5 -> 6},
    3 -> {1 -> 5, 2 -> 4},
    4 -> {6 -> 1, 4 -> 3},
    5 -> {2 -> 6, 3 -> 5},
    6 -> {1 -> 2, 5 -> 4}
  |>;
  opts = chooseTorusOptions[userOpts, {w, h, d}, rep];
  opts = DeleteOptions[CoordinateTransformFunction] @ Flatten @ opts;
  {rep, opts}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["RhombilleTorus", rhombilleTorusFactory, $torusParameters];

rhombilleTorusFactory[<|"w" -> w_, "h" -> h_, "t" -> t_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  If[h === None, h = w; w = Infinity];
  rep = PathRepresentation[
    Quiver @ Labeled[{1 -> 2, 2 -> 3}, "a" | "b" | "c"],
    makeABCTorusRepresentation[w, h, t],
    2 (* for backward compatibility with old content *)
  ];
  opts = chooseTorusOptions[userOpts, {w, h, d}, rep];
  opts = DeleteOptions[CoordinateTransformFunction] @ Flatten @ opts;
  {rep, opts}
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["Dihedral", dihedralFactory, <|"n" -> 3, "MaxDepth" -> Infinity|>];

dihedralFactory[<|"n" -> n_, "MaxDepth" -> d_|>, userOpts_] := Scope[
  rep = <|
    "CayleyFunction" -> dihedralCayleyFunction[n, If[IntegerQ[n], "r", "t"], "f"],
    "InitialStates" -> {{1, 0}, {-1, 0}}
  |>;
  {rep, {
    MaxDepth -> If[n === Infinity && d === Infinity, 6, d], PeripheralVertices -> 2,
    VertexCoordinateFunction -> dihedralCoords[n]
  }}
];

dihedralCoords[Infinity][{dir_, z_}] := {z, dir/2};
dihedralCoords[n_Integer] := dihedralCoords[Reverse @ CirclePoints[n]];
dihedralCoords[points_List][{dir_, z_}] := Part[points, z + 1] * If[dir == 1, 1, 0.4];

dihedralCayleyFunction[n_, t_, r_][{dir_, z_}] := {
  Labeled[{dir, Mod[z + dir, n]}, t],
  Labeled[{dir, Mod[z - dir, n]}, Inverted @ t],
  Labeled[{-dir, z}, r]
};

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
  Labeled[LatticeVertex[lamps, Mod[pos - 1, Length[lamps], 1]], Inverted @ "x"],
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
  StuffBag[$points, p, 2];
];

makeCubeEdge[b_, {r_, g_}] :=
  StuffBag[$edges, DirectedEdge[r, g, b]];

cubeFactory[<|"n" -> n_, "MaxDepth" -> _|>, userOpts_] := Scope[

  getRGBCardinals[];

  CollectTo[{$points, $edges},
    MapThread[makeCubeFace[n], {$cubeDirs,
      {r, R, b, B, B, b},
      {g, G, G, g, R, r}
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
  graph = ExtendedGraph[
    $points, $edges, VertexCoordinates -> $points,
    toOptsSeq @ userOpts,
    ViewOptions -> {ViewVector -> {3,2.5,2}, ViewProjection -> "Orthographic"},
    VertexShapeFunction -> None, ImageSize -> Small, EdgeThickness -> 2,
    ArrowheadSize -> Medium,
    Epilog -> innerCube
  ];
  If[!GraphQ[graph], ReturnFailed[]];
  graph // CombineMultiedges
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["CubeAtlasDoubleCover", cubeAtlasDoubleCoverFactory, <|"MaxDepth" -> Infinity|>]

cubeAtlasDoubleCoverFactory[assoc_, userOpts_] := Scope[

  getRGBCardinals[];

  crg = ChartSymbol[r <> g];
  cgb = ChartSymbol[g <> b];
  crb = ChartSymbol[r <> b];

  transitions = {
    transAnno[DirectedEdge[crg, cgb, g], r -> b, g],
    transAnno[DirectedEdge[cgb, crg, g], b -> R, g],
    transAnno[DirectedEdge[cgb, crb, b], g -> r, b],
    transAnno[DirectedEdge[crb, cgb, b], r -> G, b],
    transAnno[DirectedEdge[crg, crb, r], g -> B, r],
    transAnno[DirectedEdge[crb, crg, r], b -> g, r]
  };

  atlasQuiver[
    {crg, cgb, crb}, transitions,
    Cardinals -> cardinals, toOptsSeq @ userOpts
  ]
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["CubeAtlas", cubeAtlasFactory, <|"MaxDepth" -> Infinity|>]

cubeAtlasFactory[assoc_, userOpts_] := Scope[

  getRGBCardinals[];

  crg1 = ChartSymbol[r <> g <> "+"]; crg2 = ChartSymbol[r <> g <> "-"];
  cgb1 = ChartSymbol[g <> b <> "+"]; cgb2 = ChartSymbol[g <> b <> "-"];
  crb1 = ChartSymbol[r <> b <> "+"]; crb2 = ChartSymbol[r <> b <> "-"];

  transitions = <|
    g -> {transAnno[crg1 -> cgb1, r -> b, g], transAnno[cgb1 -> crg2, b -> R, g], transAnno[crg2 -> cgb2, r -> b, g], transAnno[cgb2 -> crg1, b -> R, g]},
    b -> {transAnno[cgb1 -> crb1, g -> r, b], transAnno[crb1 -> cgb2, r -> G, b], transAnno[cgb2 -> crb2, g -> r, b], transAnno[crb2 -> cgb1, r -> G, b]},
    r -> {transAnno[crg1 -> crb1, g -> B, r], transAnno[crb1 -> crg2, b -> g, r], transAnno[crg2 -> crb2, g -> B, r], transAnno[crb2 -> crg1, b -> g, r]}
  |>;

  atlasQuiver[
    {crg1, crg2, cgb1, cgb2, crb1, crb2}, transitions,
    Cardinals -> cardinals, toOptsSeq @ userOpts, ArrowheadPosition -> 0.5
  ]
];

transAnno[e_, t_, u_List] :=
  Annotation[e, "CardinalTransitions" -> ToList[t, Thread[u -> u]]];

transAnno[e_, t_, u_] :=
  Annotation[e, "CardinalTransitions" -> ToList[t, u -> u]];

transAnno[e_, Inverted[a_] -> b_, u_] :=
  transAnno[e, a -> Inverted[b], u];

transAnno[e_, t_, Inverted[u_]] :=
  Annotation[e, "CardinalTransitions" -> ToList[t, u -> Inverted @ u]];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["PositiveSquareLatticeDisclination", positiveSquareLatticeDisclinationFactory, <|"RemoveCorner" -> False, "MaxDepth" -> Infinity|>]

positiveSquareLatticeDisclinationFactory[assoc_, userOpts_] := Scope[
  UnpackAssociation[assoc, removeCorner];
  getRGBCardinals[];
  graph = CombineMultiedges @ Quiver[
    <|
      r -> {7 -> 6, 6 -> 5, 2 -> 1, 1 -> 4,  9 -> 2, 8 -> 7, 19 -> 18, 18 -> 17, 17 -> 16, 16 -> 15, 5 -> 14, 4 -> 13},
      g -> {7 -> 2, 6 -> 1, 1 -> 4, 2 -> 3, 19 -> 8, 8 -> 9, 9 -> 10, 10 -> 11, 3 -> 12, 4 -> 13, 18 -> 7, 17 -> 6},
      b -> {3 -> 4, 4 -> 5, 2 -> 1, 1 -> 6, 9 -> 2, 10 -> 3, 11 -> 12, 12 -> 13, 13 -> 14, 14 -> 15, 5 -> 16, 6 -> 17}
    |>,
    Apply[Sequence, DeleteOptions[Cardinals] @ Normal @ userOpts],
    VertexLayout -> SpringLayout[],
    CoordinateTransformFunction -> {{"Rotate", 90}, "ReflectHorizontal"}
  ];
  If[removeCorner, graph = VertexDelete[graph, 1]];
  graph
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["PositiveSquareLatticeDisclinationAtlas", positiveSquareLatticeDisclinationAtlasFactory, <|"MaxDepth" -> Infinity|>]

positiveSquareLatticeDisclinationAtlasFactory[assoc_, userOpts_] := Scope[

  getRGBCardinals[];

  crg = ChartSymbol[r <> g];
  cgb = ChartSymbol[g <> b];
  crb = ChartSymbol[r <> b];

  transitions = {
    Annotation[DirectedEdge[crg, cgb, g], "CardinalTransitions" -> {r -> b}],
    Annotation[DirectedEdge[cgb, crb, b], "CardinalTransitions" -> {g -> r}],
    Annotation[DirectedEdge[crb, crg, r], "CardinalTransitions" -> {b -> G}]
  };

  atlasQuiver[
    {crg, cgb, crb}, transitions,
    VertexCoordinates -> Part[CirclePoints[3], {3,2,1}],
    Cardinals -> cardinals,
    toOptsSeq @ userOpts
  ]
];

(**************************************************************************************************)

$tetraCoords3D = {
  {0, 0, Sqrt[2/3] - 1/(2*Sqrt[6])},
  {-1/2*1/Sqrt[3], -1/2, -1/2*1/Sqrt[6]},
  {-1/2*1/Sqrt[3], 1/2, -1/2*1/Sqrt[6]},
  {1/Sqrt[3], 0, -1/2*1/Sqrt[6]}
};

$tetraCoords2D = Append[CirclePoints[3], {0, 0}];

DefineParameterizedLatticeQuiver["Tetrahedron", tetrahedronFactory, <|"MaxDepth" -> Infinity|>]

tetrahedronFactory[assoc_, userOpts_] := Scope[

  getRGBWCardinals[];
  layoutDim = Lookup[userOpts, LayoutDimension, 3];

(*   edges = <|
    r -> {1 -> 2, 2 -> 3, 3 -> 1},
    g -> {1 -> 2, 2 -> 4, 4 -> 1},
    b -> {1 -> 4, 4 -> 3, 3 -> 1},
    w -> {2 -> 4, 4 -> 3, 3 -> 2}
  |>;
 *)
  edges = <|
    r -> {2 -> 1, 3 -> 2, 1 -> 3},
    g -> {1 -> 2, 2 -> 4, 4 -> 1},
    b -> {1 -> 4, 4 -> 3, 3 -> 1},
    w -> {4 -> 2, 3 -> 4, 2 -> 3}
  |>;

  Quiver[
    {1, 2, 3, 4}, edges,
    Cardinals -> cardinals,
    toOptsSeq @ userOpts,
    VertexCoordinates -> If[layoutDim === 2, $tetraCoords2D, $tetraCoords3D],
    MultiEdgeDistance -> 0.1,
    ArrowheadShape -> If[layoutDim === 2, "Line", "Cone"],
    ArrowheadSize -> Medium
  ]
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["TetrahedronAtlas", tetrahedronAtlasFactory, <|"MaxDepth" -> Infinity|>]

tetrahedronAtlasFactory[assoc_, userOpts_] := Scope[

  getRGBWCardinals[];

  cl = ChartSymbol[r <> g <> b]; (* l = rgb  ORANGE ORBIT *)
  cb = ChartSymbol[r <> g <> w]; (* b = rgw  BLUE ORBIT *)
  cr = ChartSymbol[r <> b <> w]; (* r = rbw  GREEN ORBIT *)
  cf = ChartSymbol[g <> b <> w]; (* f = gbw  RED ORBIT *)

  edges = {
    (* f = gbw -> l = gbr *)
    transAnno[DirectedEdge[cf, cl, b], w -> R, b], transAnno[DirectedEdge[cl, cf, g], r -> W, g],

    (* f = bwg -> r = bwr *)
    transAnno[DirectedEdge[cr, cf, b], r -> G, b], transAnno[DirectedEdge[cf, cr, w], g -> R, w],

    (* r = rbw -> l = rbg *)
    transAnno[DirectedEdge[cr, cl, r], w -> G, r], transAnno[DirectedEdge[cl, cr, b], g -> W, b],

    (* l = rgb -> b = rgw *)
    transAnno[DirectedEdge[cl, cb, r], b -> W, r], transAnno[DirectedEdge[cb, cl, g], w -> B, g],

    (* r = rwb -> b = rwg *)
    transAnno[DirectedEdge[cr, cb, w], b -> G, w], transAnno[DirectedEdge[cb, cr, r], g -> B, r],

    (* b = gwr -> f = gwb *)
    transAnno[DirectedEdge[cb, cf, w], r -> B, w], transAnno[DirectedEdge[cf, cb, g], b -> R, g]
  };

  atlasQuiver[
    {cl, cb, cr, cf},
    edges,
    Cardinals -> cardinals,
    toOptsSeq @ userOpts
  ]
];

(**************************************************************************************************)

DefineParameterizedLatticeQuiver["TetrahedronAtlas", tetrahedronAtlasFactory, <|"MaxDepth" -> Infinity|>]

(**************************************************************************************************)

atlasQuiver[charts_, transitions_, opts___Rule] := Quiver[
  charts, transitions, opts,
  EdgeLabels -> ("CardinalTransitions" -> CardinalTransition),
  EdgeLabelStyle -> {LabelPosition -> Scaled[0.5]}, AdditionalImagePadding->5,
  ArrowheadPosition -> 0.8, ArrowheadShape -> "Line", ArrowheadSize -> Small, ImageSize -> 150
];

(**************************************************************************************************)

toOptsSeq[assoc_] := Sequence @@ Normal[assoc];

(* trimCardinalOpts[userOpts_] := Sequence @@ DeleteOptions[Normal @ userOpts, Cardinals]
 *)