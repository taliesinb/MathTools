$BigFiveThemeRules = {
  AspectRatioClipping -> False,
  ArrowheadSize -> 12,
  VertexSize -> 5,
  ImageSize -> ("ShortestEdge" -> 30)
};

$GraphThemeData["BigFive"] := $BigFiveThemeRules;

(**************************************************************************************************)

PackageExport["BouquetQuiver"]

SetUsage @ "
BouquetQuiver[cardinals$] creates a Bouquet cardinal quiver graph with the given cardinal edges.
BouquetQuiver['string$'] uses the characters of 'string$' as cardinals.
"

DeclareArgumentCount[BouquetQuiver, 1];

Options[BouquetQuiver] = $simpleGraphOptionRules;

declareSyntaxInfo[BouquetQuiver, {_, OptionsPattern[]}];


BouquetQuiver[n_Integer, opts:OptionsPattern[]] := BouquetQuiver[
  Switch[n, 1, "r", 2, "rb", 3, "rgb", 4, "rgbw", _, Take[$alphabet, n]],
  opts
];

BouquetQuiver[str_String, opts:OptionsPattern[]] := BouquetQuiver[Characters[str], opts];

BouquetQuiver[cardinals_List, opts:OptionsPattern[]] :=
  Quiver[Map[c |-> Labeled[1 -> 1, c], cardinals], Cardinals -> cardinals, opts, GraphOrigin -> 1]

(**************************************************************************************************)

PackageExport["TriangularQuiver"]

Options[TriangularQuiver] = $ExtendedGraphOptions;

TriangularQuiver[n_Integer, opts:OptionsPattern[]] :=
  TriangularQuiver[n, {"r", "g", "b"}, opts];

TriangularQuiver[n_Integer, cards:{x_, y_, z_}, opts:OptionsPattern[]] := Scope[
  vertices = Catenate @ Array[VertexProduct, {n, n}];
  edges = Flatten @ {
    Table[DirectedEdge[VertexProduct[i, j], VertexProduct[i + 1, j], x], {i, n-1}, {j, n}],
    Table[DirectedEdge[VertexProduct[i, j], VertexProduct[i, j + 1], y], {i, n}, {j, n-1}],
    Table[DirectedEdge[VertexProduct[i, j], VertexProduct[i + 1, j + 1], z], {i, n-1}, {j, n-1}]
  };
  vertices //= Select[upperTriProdQ];
  edges //= Select[upperTriProdQ];
  ExtendedGraph[
    vertices, edges,
    opts,
    ImageSize -> ("ShortestEdge" -> 33),
    VertexCoordinates -> N[({#1 - #2/2, #2 * Sqrt[3]/2}& @@@ vertices)],
    GraphTheme -> "BigFive",
    Cardinals -> cards
  ]
]

upperTriProdQ[VertexProduct[a_, b_]] := a <= b;
upperTriProdQ[DirectedEdge[t_, h_, _]] := upperTriProdQ[t] && upperTriProdQ[h];

(**************************************************************************************************)

PackageExport["HexagonalQuiver"]

Options[HexagonalQuiver] = $ExtendedGraphOptions;

HexagonalQuiver[n_Integer, opts:OptionsPattern[]] := Scope[
  n2 = 2 * n;
  z = (3 * n2 + 1)/2;
  cards = {"x", "y", "z"};
  edges = makeHexSkeleton[{{-1,1},{-1,1},{-1,1}}*n2, z, cards];
  vertices = AllUniqueVertices @ edges;
  ExtendedGraph[
    vertices, edges, opts,
    VertexCoordinates -> Map[DotABC, List @@@ vertices],
    Cardinals -> cards,
    ImageSize -> ("ShortestEdge" -> 25),
    GraphTheme -> "BigFive"
  ]
];

$hexNormVecs = {{3, 2, 1}, {1, 3, 2}, {2, 1, 3}};

hexNorm[v_List] := Max[Abs @ Dot[$hexNormVecs, v]];

makeHexSkeleton[{{a1_, a2_}, {b1_, b2_}, {c1_, c2_}}, norm_, {x_, y_, z_}] := Scope[
  ab = Tuples[{Range[a1, a2], Range[b1, b2]}];
  abc = Append[#, -Total[#]]& /@ ab;
  abc //= Select[c1 <= Last[#] <= c2&];
  vertices = VertexProduct @@@ abc;
  edges = Flatten @ {
    hexEdgeList[x, abc, {2, 1, 0}, {1, -1, 0}, 1],
    hexEdgeList[y, abc, {0, 2, 1}, {0, 1, -1}, 1],
    hexEdgeList[z, abc, {1, 0, 2}, {-1, 0, 1}, 1]
    };
  abc //= Select[hexNorm[#] <= norm&];
  vertices = VertexProduct @@@ abc;
  isVertex = ConstantAssociation[vertices, True];
  Select[edges, isVertex[Part[#, 1]] && isVertex[Part[#, 2]]&]
];

hexEdgeList[card_, vertexCoords_, normal_, offset_, mod_] := Map[
  vertex |-> If[Mod[Dot[normal, vertex], 3] == mod,
    DirectedEdge[VertexProduct @@ vertex, VertexProduct @@ Plus[vertex, offset], card],
    Nothing
  ],
  vertexCoords
];

(**************************************************************************************************)

PackageExport["SquareQuiver"]

Options[SquareQuiver] = $ExtendedGraphOptions;

SquareQuiver[m:$ModIntP, opts:OptionsPattern[]] :=
  SquareQuiver[{m, m}, opts];

SquareQuiver[spec_, opts:OptionsPattern[]] :=
  SquareQuiver[spec, {"x", "y"}, opts];

SquareQuiver[spec:{$ModIntP, $ModIntP}, {cx_, cy_}, opts:OptionsPattern[]] := Scope[
  {m, n} = StripModulo @ spec;
  {mp1, np1} = toModPlusOne @ spec;
  vertices = Catenate @ Array[VertexProduct, StripModulo @ {m, n}];
  edges = Flatten @ {
    Table[enrichedEdge[VertexProduct[i, j], VertexProduct[mp1 @ i, j], cx, i], {i, m}, {j, n}],
    Table[enrichedEdge[VertexProduct[i, j], VertexProduct[i, np1 @ j], cy, j], {i, m}, {j, n}]
  };
  edges //= Select[MemberQ[vertices, Part[#, 2]]&];
  basis = {{1, 0}, {0, 1}};
  ExtendedGraph[
    vertices, edges,
    opts,
    VertexCoordinates -> (List @@@ vertices),
    Cardinals -> {cx, cy},
    Sequence @@ modEdgeShapeFunctionSpec[spec, basis],
    GraphTheme -> "BigFive"
  ]
]

(**************************************************************************************************)

PackageExport["CubicQuiver"]

Options[CubicQuiver] = $ExtendedGraphOptions;

CubicQuiver[m:$ModIntP, opts:OptionsPattern[]] :=
  CubicQuiver[{m, m, m}, opts];

CubicQuiver[spec_, opts:OptionsPattern[]] :=
  CubicQuiver[spec, {"x", "y", "z"}, opts];

CubicQuiver[spec:{$ModIntP, $ModIntP, $ModIntP}, {cx_, cy_, cz_}, opts:OptionsPattern[]] := Scope[
  {m, n, p} = StripModulo @ spec;
  {mp1, np1, pp1} = toModPlusOne @ spec;
  vertices = Flatten[Array[VertexProduct, {m, n, p}], 2];
  edges = Flatten @ {
    Table[enrichedEdge[VertexProduct[i, j, k], VertexProduct[mp1 @ i, j, k], cx, i], {i, m}, {j, n}, {k, p}],
    Table[enrichedEdge[VertexProduct[i, j, k], VertexProduct[i, np1 @ j, k], cy, j], {i, m}, {j, n}, {k, p}],
    Table[enrichedEdge[VertexProduct[i, j, k], VertexProduct[i, j, pp1 @ k], cz, j], {i, m}, {j, n}, {k, p}]
  };
  edges //= Select[MemberQ[vertices, Part[#, 2]]&];
  basis = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}};
  ExtendedGraph[
    vertices, edges,
    opts,
    VertexCoordinates -> (List @@@ vertices),
    Cardinals -> {cx, cy, cz},
    LayoutDimension -> 3,
    Sequence @@ modEdgeShapeFunctionSpec[spec, basis],
    GraphTheme -> "BigFive3D"
  ]
]

toModPlusOne[spec_] := PlusOneMod[GetModulus @ #, 1]& /@ spec;

modEdgeShapeFunctionSpec[spec_, basis_] /; ContainsQ[spec, Modulo] := Scope[
  dim = InnerDimension @ basis;
  tuples = Tuples @ MapThread[makeModOffset, {spec, basis}];
  offsets = DeleteCases[{0..}] @ Map[Total] @ tuples;
  {
    EdgeShapeFunction -> ModulusEdgeShapeFunction[offsets],
    ImagePadding -> 25
  }
];

makeModOffset[Modulo[n_], vec_] := vec * #& /@ {-n, 0, n};
makeModOffset[_, _] := ConstantArray[0, {1, dim}];

modEdgeShapeFunctionSpec[spec_, basis_] := {};

(**************************************************************************************************)

$BigFive3DThemeRules = {
  ArrowheadSize -> 15,
  VertexSize -> 4,
  VertexShapeFunction -> "Point",
  ImageSize -> ("ShortestEdge" -> 80),
  ViewOptions->{ViewVector->{-3.333,-10,10},ViewProjection->"Orthographic", "ShrinkWrap" -> True}
};

$GraphThemeData["BigFive3D"] := $BigFive3DThemeRules;

(**************************************************************************************************)

PackageExport["LineQuiver"]

Options[LineQuiver] = $ExtendedGraphOptions;

LineQuiver[n_, opts:OptionsPattern[]] :=
  LineQuiver[n, "x", opts];

LineQuiver[spec_, card_, opts:OptionsPattern[]] := Scope[
  n = spec;
  vertices = Switch[n,
    _Integer, Range @ n,
    _Span, Range @@ n,
    _, ReturnFailed[];
  ];
  edges = ApplyWindowed[enrichedEdge[#1, #2, card, #1]&, vertices];
  ExtendedGraph[
    vertices, edges,
    opts,
    GraphTheme -> "BigFive",
    ExtendedGraphLayout -> "Linear"
  ]
]

LineQuiver[Modulo[n_Integer], card_, opts:OptionsPattern[]] := Scope[
  np1 = PlusOneMod[n, 1];
  vertices = Range @ n;
  edges = Table[enrichedEdge[i, np1 @ i, cx, i], {i, n}];
  basis = {{1, 0}};
  ExtendedGraph[
    vertices, edges,
    opts,
    VertexCoordinates -> Transpose[{vertices, Zeros @ n}],
    Cardinals -> {cx, cy},
    Sequence @@ modEdgeShapeFunctionSpec[{Modulo @ n}, basis],
    GraphTheme -> "BigFive"
  ]
]

(**************************************************************************************************)

PackageExport["CycleQuiver"]

Options[CycleQuiver] = $ExtendedGraphOptions;

CycleQuiver[n_Integer, opts:OptionsPattern[]] :=
  CycleQuiver[n, "x", opts];

CycleQuiver[n_Integer, card_, opts:OptionsPattern[]] := Scope[
  vertices = Range[1, n];
  edges = enrichedEdge[#1, #2, card, #1]& @@@ Partition[vertices, 2, 1, 1];
  ExtendedGraph[
    vertices, edges,
    opts, VertexLayout -> LinearLayout[],
    GraphTheme -> "BigFive",
    ImageSize -> ("ShortestEdge" -> 35)
  ]
]

(**************************************************************************************************)

enrichedEdge[a_, b_, card_, n_] :=
  DirectedEdge[a, b, card];

enrichedEdge[a_, b_, cs_SerialCardinal, n_] :=
  enrichedEdge[a, b, ModPart[cs, n], n];

enrichedEdge[a_, b_, p_ParallelCardinal, n_] :=
  DirectedEdge[a, b, CardinalSet @ (List @@ p)];

ModPart[seq_, n_] := Part[seq, Mod[n, Length[seq], 1]];

(**************************************************************************************************)

PackageExport["GridQuiver"]

Options[GridQuiver] = $ExtendedGraphOptions;

GridQuiver[k_Integer, n:$ModIntP, opts:OptionsPattern[]] :=
  Switch[k,
    1, LineQuiver[n, opts],
    2, SquareQuiver[n, opts],
    3, CubicQuiver[n, opts],
    _, generalGridQuiver[k, n, opts]
  ];

generalGridQuiver[k_, n_, opts___] := Scope[
  vertices = Flatten @ Array[VertexProduct, ConstantArray[StripModulo @ n, k]];
  edges = Flatten @ Table[Map[generalGridEdge[n, i], vertices], {i, 1, k}];
  ExtendedGraph[
    vertices, edges,
    opts,
    ExtendedGraphLayout -> "SpringElectrical",
    Cardinals -> Range[k],
    LayoutDimension -> 3,
    Sequence @@ modEdgeShapeFunctionSpec[n, IdentityMatrix[k]],
    GraphTheme -> "BigFive3D"
  ]
]

generalGridEdge[Modulo[n_], i_][vertex_] :=
  DirectedEdge[vertex, MapAt[PlusOneMod[n], vertex, i], i];

generalGridEdge[n_, i_][vertex_] :=
  If[Part[vertex, i] < n,
    DirectedEdge[vertex, MapAt[PlusOne, vertex, i], i],
    {}
  ];

(**************************************************************************************************)

PackageExport["TreeQuiver"]
PackageExport["TreeVertex"]

Options[TreeQuiver] = Prepend[$ExtendedGraphOptions, "AngleOffset" -> 0];

TreeQuiver[k_Integer, n_Integer, opts:OptionsPattern[]] := Scope[
  If[!OddQ[n], ReturnFailed[]];
  n = (n - 1) / 2;
  cards = Join[Range[k], Inverted /@ Range[k]];
  vertices = Flatten @ Table[TreeVertex @@@ Tuples[cards, i], {i, 0, n}];
  vertices = Discard[vertices, MatchQ[TreeVertex[___, c_, Inverted[c_], ___] | TreeVertex[___, Inverted[c_], c_, ___]]];
  UnpackOptions[angleOffset, cardinals];
  $cards = If[ListQ[cardinals], cardinals, Range @ k];
  edges = makeTreeEdge /@ vertices;
  SetAutomatic[angleOffset, If[k == 2, Pi / 4, 0]];
  vectorAssoc = AssociationThread[cards, CirclePoints[{1, angleOffset}, 2 * k]];
  coords = Map[treeVertexCoord, vertices];
  scaling = 1 / k;
  ExtendedGraph[
    vertices, edges,
    FilterOptions @ opts,
    VertexCoordinates -> coords,
    Cardinals -> Range[k],
    GraphLayout -> {"NudgeDistance" -> 0},
    ImageSize -> "AverageEdge" -> 30,
    GraphOrigin -> TreeVertex[],
    GraphTheme -> "BigFive"
  ]
]

treeVertexCoord = Case[
  TreeVertex[] := {0, 0};
  t_TreeVertex := Total[Lookup[vectorAssoc, List @@ t] * Power[scaling, Range @ Length @ t]];
];

makeTreeEdge = Case[
  TreeVertex[] := Nothing;
  t_TreeVertex := DirectedEdge[Most @ t, t, Last[t] /. i_Integer :> Part[$cards, i]];
]

(**************************************************************************************************)

PackageExport["LatticeQuiverCoordinates"]

LatticeQuiverCoordinates[quiver_Graph, Automatic] :=
  LatticeQuiverCoordinates[quiver, chooseLatticeBasisVectors @ Sort @ CardinalList @ quiver];

LatticeQuiverCoordinates[quiver_Graph, "Triangular"] :=
  LatticeQuiverCoordinates[quiver, $TriangularVectors2D];

LatticeQuiverCoordinates[quiver_Graph, name_String] := Scope[
  rep = LatticeQuiverData[name, "Representation"];
  If[!PathRepresentationObjectQ[rep], ReturnFailed[]];
  vectors = ExtractTranslationVector[Normal[#]]& /@ Values[rep["Generators"]];
  If[!CoordinateMatrixQ[vectors], ReturnFailed[]];
  LatticeQuiverCoordinates[quiver, vectors]
];

(**************************************************************************************************)

PackageExport["$TriangularVectors2D"]

$s32 = Sqrt[3]/2;
$TriangularVectors2D = Simplify /@ {{1, 0}, {1/2, $s32}, {-1/2, $s32}};

chooseLatticeBasisVectors = Case[
  {"x", "y"} | {"b", "r"}             := {{1,0}, {0, 1}};
  {"x", "y", "z"} | {"b", "g", "r"}   := {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}};
  {"a", "b", "c"}                     := $TriangularVectors2D;
  other_ := Take[CirclePoints[Length[other] * 2], Length @ other];
]

LatticeQuiverCoordinates::badlen = "Number of cardinals `` didn't match number of basis vectors ``."

LatticeQuiverCoordinates[quiver_Graph, latticeBasis_] := Scope[
  cardinalList = CardinalList @ quiver;
  If[!SameLengthQ[cardinalList, latticeBasis], ReturnFailed["badlen", Length @ cardinalList, Length @ latticeBasis]];
  If[ListQ[latticeBasis], latticeBasis = AssociationThread[cardinalList, latticeBasis]];
  indexGraph = ToIndexGraph @ quiver;
  outTable = VertexOutVertexTagTable @ indexGraph;
  dims = Rest @ Dimensions @ Values @ latticeBasis;
  coords = ConstantArray[0, Prepend[dims, VertexCount @ indexGraph]];
  edgeBasis = Map[latticeBasis, EdgeTagAssociation @ indexGraph];
  edgeBasis = Join[edgeBasis, Map[Minus, KeyMap[Reverse, edgeBasis]]];
  edgeIndex = EdgePairIndexAssociation @ indexGraph;
  edgeIndex = Join[edgeIndex, KeyMap[Reverse, edgeIndex]];
  visitedEdges = CreateDataStructure["HashSet"];
  initial = MinimumIndex @ VertexInDegree @ indexGraph;
  BreadthFirstScan[UndirectedGraph @ indexGraph, initial, {"DiscoverVertex" -> discoverVertex}];
  coords //= ToPackedReal;
  {coords, visitedEdges["Elements"]}
];

discoverVertex[new_, old_, _] := If[new =!= old,
  Set[Part[coords, new], Part[coords, old] + edgeBasis[{old, new}]];
  visitedEdges["Insert", edgeIndex[{old, new}]];
]