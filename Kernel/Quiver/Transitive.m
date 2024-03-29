PublicOption[VertexHead, VertexOrigin]

VertexHead::usage = "VertexHead is an option to SquareQuiver, TriangularQuiver, etc."
VertexOrigin::usage = "VertexOrigin is an option to SquareQuiver, TriangularQuiver, etc."

DefineGraphTheme["TransitiveQuiver",
  AspectRatioClipping -> False,
  ArrowheadSize -> 12,
  VertexSize -> 5,
  ImageSize -> ("ShortestEdge" -> 30)
];

(**************************************************************************************************)

PublicFunction[BouquetQuiver]

SetUsage @ "
BouquetQuiver[cardinals$] creates a Bouquet cardinal quiver graph with the given cardinal edges.
BouquetQuiver['string$'] uses the characters of 'string$' as cardinals.
"

DeclareArgumentCount[BouquetQuiver, 1];

Options[BouquetQuiver] = $ExtendedGraphOptions;

DefineArgumentsPattern[BouquetQuiver, {_, OptionsPattern[]}];


BouquetQuiver[n_Int, opts:OptionsPattern[]] := BouquetQuiver[
  Switch[n, 1, "r", 2, "rb", 3, "rgb", 4, "rgbw", _, Take[$RomanGreekAlphabet, n]],
  opts
];

BouquetQuiver[str_Str, opts:OptionsPattern[]] := BouquetQuiver[Chars[str], opts];

BouquetQuiver[cardinals_List, opts:OptionsPattern[]] :=
  Quiver[
    Map[c |-> Labeled[1 -> 1, c], cardinals], Cardinals -> cardinals, opts, GraphOrigin -> 1,
    GraphTheme -> "TransitiveQuiver"
  ]

(**************************************************************************************************)

$transitiveQuiverOptions = JoinOptions[
  $ExtendedGraphOptions,
  VertexHead -> VertexProduct,
  VertexOrigin -> 0
];

(**************************************************************************************************)

PublicFunction[TriangularQuiver]

Options[TriangularQuiver] = $transitiveQuiverOptions;

TriangularQuiver[n_Int, opts:OptionsPattern[]] :=
  TriangularQuiver[n, {"r", "g", "b"}, opts];

TriangularQuiver[n_Int, cards:{x_, y_, z_}, opts:OptionsPattern[]] := Scope[
  
  UnpackOptions[vertexHead, vertexOrigin, graphOrigin];
  center = Ceiling[{(n + 2) / 3, (2*n + 1) / 3}];
  SetAuto[vertexOrigin, center];
  If[IntQ[vertexOrigin] || IntegerVectorQ[vertexOrigin], vertexHead = List /* PlusOperator[-vertexOrigin] /* Apply[vertexHead]];
  SetAuto[graphOrigin, Apply[vertexHead, center]];
  
  vertices = Catenate @ Table[If[i <= j, vertexHead[i, j], Nothing], {i, n}, {j, n}];
  edges = Flatten @ {
    Table[DirectedEdge[vertexHead[i, j], vertexHead[i + 1, j],     x], {i, n-1}, {j, n}],
    Table[DirectedEdge[vertexHead[i, j], vertexHead[i, j + 1],     y], {i, n},   {j, n-1}],
    Table[DirectedEdge[vertexHead[i, j], vertexHead[i + 1, j + 1], z], {i, n-1}, {j, n-1}]
  };
  edges //= selectValidEdges2[vertices];

  ExtendedGraph[
    vertices, edges,
    GraphOrigin -> graphOrigin,
    FilterOptions @ opts,
    ImageSize -> ("ShortestEdge" -> 33),
    VertexCoordinates -> N[({#1 - #2/2, #2 * Sqrt[3]/2}& @@@ vertices)],
    GraphTheme -> "TransitiveQuiver",
    Cardinals -> cards
  ]
]

upperTriProdQ[VertexProduct[a_, b_]] := a <= b;
upperTriProdQ[DirectedEdge[t_, h_, _]] := upperTriProdQ[t] && upperTriProdQ[h];

(**************************************************************************************************)

PublicFunction[HexagonalQuiver]

Options[HexagonalQuiver] = $ExtendedGraphOptions;

HexagonalQuiver[n_Int, opts:OptionsPattern[]] := Scope[
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
    GraphTheme -> "TransitiveQuiver"
  ]
];

$hexNormVecs = {{3, 2, 1}, {1, 3, 2}, {2, 1, 3}};

hexNorm[v_List] := Max[Abs @ Dot[$hexNormVecs, v]];

makeHexSkeleton[{{a1_, a2_}, {b1_, b2_}, {c1_, c2_}}, norm_, {x_, y_, z_}] := Scope[
  ab = Tuples[{Range[a1, a2], Range[b1, b2]}];
  abc = App[#, -Total[#]]& /@ ab;
  abc //= Select[c1 <= L[#] <= c2&];
  vertices = VertexProduct @@@ abc;
  edges = Flatten @ {
    hexEdgeList[x, abc, {2, 1, 0}, {1, -1, 0}, 1],
    hexEdgeList[y, abc, {0, 2, 1}, {0, 1, -1}, 1],
    hexEdgeList[z, abc, {1, 0, 2}, {-1, 0, 1}, 1]
    };
  abc //= Select[hexNorm[#] <= norm&];
  vertices = VertexProduct @@@ abc;
  isVertex = ConstantAssociation[vertices, True];
  edges //= selectValidEdges2[vertices];
  edges
];

hexEdgeList[card_, vertexCoords_, normal_, offset_, mod_] := Map[
  vertex |-> If[Mod[Dot[normal, vertex], 3] == mod,
    DirectedEdge[VertexProduct @@ vertex, VertexProduct @@ Plus[vertex, offset], card],
    Nothing
  ],
  vertexCoords
];

(**************************************************************************************************)

PublicFunction[SquareQuiver]

Options[SquareQuiver] = $transitiveQuiverOptions;

SquareQuiver[m:$ModIntP, opts:OptionsPattern[]] :=
  SquareQuiver[{m, m}, opts];

SquareQuiver[spec_, opts:OptionsPattern[]] :=
  SquareQuiver[spec, {"x", "y"}, opts];

SquareQuiver[spec:{$ModIntP, $ModIntP}, {cx_, cy_}, opts:OptionsPattern[]] := Scope[

  {m, n} = StripModulo @ spec;
  {mp1, np1} = toModPlusOne @ spec;

  UnpackOptions[vertexHead, vertexOrigin, graphOrigin];
  center = Ceiling[{m, n} / 2];
  SetAuto[vertexOrigin, center];
  If[IntQ[vertexOrigin] || IntegerVectorQ[vertexOrigin], vertexHead = List /* PlusOperator[-vertexOrigin] /* Apply[vertexHead]];
  SetAuto[graphOrigin, Apply[vertexHead, center]];
  
  vertices = Catenate @ Array[vertexHead, StripModulo @ {m, n}];
  edges = Flatten @ {
    Table[enrichedEdge[vertexHead[i, j], vertexHead[mp1 @ i, j], cx, i], {i, m}, {j, n}],
    Table[enrichedEdge[vertexHead[i, j], vertexHead[i, np1 @ j], cy, j], {i, m}, {j, n}]
  };
  edges //= selectValidEdges1[vertices];
  basis = {{1, 0}, {0, 1}};

  ExtendedGraph[
    vertices, edges,
    GraphOrigin -> graphOrigin,
    FilterOptions @ opts,
    VertexCoordinates -> (List @@@ vertices),
    Cardinals -> {cx, cy},
    Sequence @@ modEdgeShapeFunctionSpec[spec, basis],
    GraphTheme -> "TransitiveQuiver"
  ]
]

(**************************************************************************************************)

selectValidEdges1[vertices_][edges_] := Scope[
  vassoc = ConstantAssociation[vertices, True];
  Pick[edges, Lookup[vassoc, Col2[edges], False]]
];

selectValidEdges2[vertices_][edges_] := Scope[
  vassoc = ConstantAssociation[vertices, True];
  Pick[edges, ThreadAnd[
    Lookup[vassoc, Col1[edges], False],
    Lookup[vassoc, Col2[edges], False]
  ]]
];

(**************************************************************************************************)

PublicFunction[SquareModulusEdgeShapeFunction]

(* for users *)
SquareModulusEdgeShapeFunction[x_Int, y_Int] :=
  SquareModulusEdgeShapeFunction[Modulo[x], Modulo[y]];

SquareModulusEdgeShapeFunction[x_, y_] :=
  makeModulusEdgeShapeFunctionInner[{x, y}, {{1, 0}, {0, 1}}]

(**************************************************************************************************)

PrivateFunction[MakeModulusEdgeShapeFunction]

MakeModulusEdgeShapeFunction[specs__Int] :=
  Auto;

MakeModulusEdgeShapeFunction[spec_] :=
  makeModulusEdgeShapeFunctionInner[{spec}, {{1, 0}}];

MakeModulusEdgeShapeFunction[specs:Repeated[_, {2, 3}]] :=
  makeModulusEdgeShapeFunctionInner[{specs}, IdentityMatrix @ SeqLength[specs]];

MakeModulusEdgeShapeFunction[specs__] :=
  Auto;

(**************************************************************************************************)

makeModulusEdgeShapeFunctionInner[spec_List, basis_List] := Scope[
  dim = InnerDimension @ basis;
  tuples = Tuples @ MapThread[makeModOffset, {spec, basis}];
  offsets = Decases[{0..}] @ Map[Total] @ tuples;
  ModulusEdgeShapeFunction[offsets]
];

(**************************************************************************************************)

PublicFunction[CubicQuiver]

Options[CubicQuiver] = $transitiveQuiverOptions;

CubicQuiver[m:$ModIntP, opts:OptionsPattern[]] :=
  CubicQuiver[{m, m, m}, opts];

CubicQuiver[spec_, opts:OptionsPattern[]] :=
  CubicQuiver[spec, {"x", "y", "z"}, opts];

CubicQuiver[spec:{$ModIntP, $ModIntP, $ModIntP}, {cx_, cy_, cz_}, opts:OptionsPattern[]] := Scope[
  
  {m, n, p} = StripModulo @ spec;
  {mp1, np1, pp1} = toModPlusOne @ spec;

  UnpackOptions[vertexHead, vertexOrigin, graphOrigin];
  center = Ceiling[{m, n, p} / 2];
  SetAuto[vertexOrigin, center];
  If[IntQ[vertexOrigin] || IntegerVectorQ[vertexOrigin], vertexHead = List /* PlusOperator[-vertexOrigin] /* Apply[vertexHead]];
  SetAuto[graphOrigin, Apply[vertexHead, vertexOrigin]];

  vertices = Flatten[Array[vertexHead, {m, n, p}], 2];
  edges = Flatten @ {
    Table[enrichedEdge[vertexHead[i, j, k], vertexHead[mp1 @ i, j, k], cx, i], {i, m}, {j, n}, {k, p}],
    Table[enrichedEdge[vertexHead[i, j, k], vertexHead[i, np1 @ j, k], cy, j], {i, m}, {j, n}, {k, p}],
    Table[enrichedEdge[vertexHead[i, j, k], vertexHead[i, j, pp1 @ k], cz, j], {i, m}, {j, n}, {k, p}]
  };
  edges //= selectValidEdges1[vertices];

  basis = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}};

  ExtendedGraph[
    vertices, edges,
    GraphOrigin -> graphOrigin,
    FilterOptions @ opts,
    VertexCoordinates -> (List @@@ vertices),
    Cardinals -> {cx, cy, cz},
    LayoutDimension -> 3,
    Sequence @@ modEdgeShapeFunctionSpec[spec, basis],
    GraphTheme -> "TransitiveQuiver3D"
  ]
]

toModPlusOne[spec_] := PlusOneMod[GetModulus @ #, 1]& /@ spec;

modEdgeShapeFunctionSpec[spec_, basis_] /; ContainsQ[spec, Modulo] := Scope[
  esf = makeModulusEdgeShapeFunctionInner[spec, basis];
  {
    EdgeShapeFunction -> esf,
    ImagePadding -> 25
  }
];

makeModOffset[Modulo[n_], vec_] := vec * #& /@ {-n, 0, n};
makeModOffset[_, _] := Repeat[0, {1, dim}];

modEdgeShapeFunctionSpec[spec_, basis_] := {};

(**************************************************************************************************)

DefineGraphTheme["TransitiveQuiver3D",
  ArrowheadSize -> 15,
  VertexSize -> 4,
  VertexShapeFunction -> "Point",
  ImageSize -> ("ShortestEdge" -> 80),
  ViewOptions->{ViewVector->{-3.333,-10,10},ViewProjection->"Orthographic", "ShrinkWrap" -> True}
];

(**************************************************************************************************)

PublicFunction[DisconnectedQuiver]

Options[DisconnectedQuiver] = $ExtendedGraphOptions;

DisconnectedQuiver[list_List, opts:OptionsPattern[]] :=
  ExtendedGraph[list, {}, VertexLayout -> LinearLayout[], opts];

DisconnectedQuiver[n_Int, opts:OptionsPattern[]] :=
  ExtendedGraph[Range[n], {}, VertexLayout -> LinearLayout[], opts];

(**************************************************************************************************)

PublicFunction[LineQuiver]

Options[LineQuiver] = JoinOptions[
  VertexHead -> Id,
  $transitiveQuiverOptions
];

LineQuiver[n_, opts:OptionsPattern[]] :=
  LineQuiver[n, "x", opts];

LineQuiver[spec_, card_, opts:OptionsPattern[]] := Scope[
  n = spec;
  vertices = Switch[n,
    _Int, Range @ n,
    _Span, Range @@ n,
    _, ReturnFailed[];
  ];

  UnpackOptions[vertexHead, vertexOrigin, graphOrigin];
  center = Ceiling[n / 2];
  SetAuto[vertexOrigin, center];
  If[IntQ[vertexOrigin], vertexHead = List /* PlusOperator[-vertexOrigin] /* Apply[vertexHead]];
  SetAuto[graphOrigin, Apply[vertexHead, vertexOrigin]];
  vertices //= Map[vertexHead];

  edges = ApplyWindowed[enrichedEdge[#1, #2, card, #1]&, vertices];
  ExtendedGraph[
    vertices, edges,
    GraphOrigin -> graphOrigin,
    FilterOptions @ opts,
    GraphTheme -> "TransitiveQuiver",
    VertexLayout -> LinearLayout[]
  ]
]

LineQuiver[Modulo[n_Int], card_, opts:OptionsPattern[]] := Scope[
  np1 = PlusOneMod[n, 1];
  vertices = Range @ n;
  edges = Table[enrichedEdge[i, np1 @ i, cx, i], {i, n}];
  basis = {{1, 0}};
  ExtendedGraph[
    vertices, edges,
    opts,
    VertexCoordinates -> Trans[vertices, Zeros @ n],
    Cardinals -> {cx, cy},
    Sequence @@ modEdgeShapeFunctionSpec[{Modulo @ n}, basis],
    GraphTheme -> "TransitiveQuiver"
  ]
]

(**************************************************************************************************)

PublicFunction[CycleQuiver]

Options[CycleQuiver] = $ExtendedGraphOptions;

CycleQuiver[n_Int, opts:OptionsPattern[]] :=
  CycleQuiver[n, "x", opts];

CycleQuiver[n_Int, card_, opts:OptionsPattern[]] := Scope[
  vertices = Range[1, n];
  edges = enrichedEdge[#1, #2, card, #1]& @@@ Partition[vertices, 2, 1, 1];
  ExtendedGraph[
    vertices, edges,
    opts, VertexLayout -> LinearLayout[],
    GraphTheme -> "TransitiveQuiver",
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

ModPart[seq_, n_] := Part[seq, Mod[n, Len[seq], 1]];

(**************************************************************************************************)

PublicFunction[GridQuiver]

Options[GridQuiver] = $transitiveQuiverOptions;

GridQuiver[1, n:$ModIntP, opts:OptionsPattern[]] := LineQuiver[n, opts];

GridQuiver[2, n:$ModIntP, opts:OptionsPattern[]] := SquareQuiver[n, opts];

GridQuiver[3, n:$ModIntP, opts:OptionsPattern[]] := CubicQuiver[n, opts];

GridQuiver[k_Int, n:$ModIntP, opts:OptionsPattern[]] := Scope[

  UnpackOptions[vertexHead, vertexOrigin, graphOrigin];
  center = Ceiling[n / 2];
  SetAuto[vertexOrigin, center];
  If[IntQ[vertexOrigin] || IntegerVectorQ[vertexOrigin], vertexHead = List /* PlusOperator[-vertexOrigin] /* Apply[vertexHead]];
  SetAuto[graphOrigin, Apply[vertexHead, vertexOrigin]];

  vertices = Flatten[Array[List, Repeat[StripModulo @ n, k]], k-1];
  edges = Flatten @ Table[Map[generalGridEdge[n, i], vertices], {i, 1, k}];
  vertices = vertexHead @@@ vertices;

  ExtendedGraph[
    vertices, edges,
    GraphOrigin -> graphOrigin,
    FilterOptions @ opts,
    VertexLayout -> SpringElectricalLayout[],
    Cardinals -> Range[k],
    LayoutDimension -> 3,
    Sequence @@ modEdgeShapeFunctionSpec[n, IdentityMatrix[k]],
    GraphTheme -> "TransitiveQuiver3D"
  ]
]

generalGridEdge[Modulo[n_], i_][vertex_] :=
  DirectedEdge[vertexHead @@ vertex, vertexHead @@ MapAt[PlusOneMod[n], vertex, i], i];

generalGridEdge[n_, i_][vertex_] :=
  If[Part[vertex, i] < n,
    DirectedEdge[vertexHead @@ vertex, vertexHead @@ MapAt[PlusOne, vertex, i], i],
    {}
  ];

(**************************************************************************************************)

PublicFunction[TreeQuiver, TreeVertex]

Options[TreeQuiver] = Pre[$ExtendedGraphOptions, "AngleOffset" -> 0];

TreeQuiver[k_Int, n_Int, opts:OptionsPattern[]] := Scope[
  If[!OddQ[n], ReturnFailed[]];
  n = (n - 1) / 2;
  cards = Join[Range[k], Inverted /@ Range[k]];
  vertices = Flatten @ Table[TreeVertex @@@ Tuples[cards, i], {i, 0, n}];
  vertices = Discard[vertices, MatchQ[TreeVertex[___, c_, Inverted[c_], ___] | TreeVertex[___, Inverted[c_], c_, ___]]];
  UnpackOptions[angleOffset, cardinals];
  $cards = If[ListQ[cardinals], cardinals, Range @ k];
  edges = makeTreeEdge /@ vertices;
  SetAuto[angleOffset, If[k == 2, Pi / 4, 0]];
  vectorAssoc = AssocThread[cards, CirclePoints[{1, angleOffset}, 2 * k]];
  coords = Map[treeVertexCoord, vertices];
  scaling = 1 / k;
  ExtendedGraph[
    vertices, edges,
    FilterOptions @ opts,
    VertexCoordinates -> coords,
    Cardinals -> Range[k],
    VertexOverlapResolution -> 0,
    ImageSize -> "AverageEdge" -> 30,
    GraphOrigin -> TreeVertex[],
    GraphTheme -> "TransitiveQuiver"
  ]
]

treeVertexCoord = Case[
  TreeVertex[] := {0, 0};
  t_TreeVertex := Total[Lookup[vectorAssoc, List @@ t] * Power[scaling, Range @ Len @ t]];
];

makeTreeEdge = Case[
  TreeVertex[] := Nothing;
  t_TreeVertex := DirectedEdge[Most @ t, t, L[t] /. i_Int :> Part[$cards, i]];
]

(**************************************************************************************************)

PublicFunction[LatticeQuiverCoordinates]

LatticeQuiverCoordinates[quiver_Graph, Auto] :=
  LatticeQuiverCoordinates[quiver, chooseLatticeBasisVectors @ Sort @ CardinalList @ quiver];

LatticeQuiverCoordinates[quiver_Graph, "Triangular"] :=
  LatticeQuiverCoordinates[quiver, $TriangularVectors2D];

LatticeQuiverCoordinates[quiver_Graph, name_Str] := Scope[
  rep = LatticeQuiverData[name, "Representation"];
  If[!PathRepresentationObjectQ[rep], ReturnFailed[]];
  vectors = ExtractTranslationVector[Normal[#]]& /@ Values[rep["Generators"]];
  If[!CoordinateMatrixQ[vectors], ReturnFailed[]];
  LatticeQuiverCoordinates[quiver, vectors]
];

(**************************************************************************************************)

PublicVariable[$TriangularVectors2D]

$s32 = Sqrt[3]/2;
SetCached[$TriangularVectors2D, Simplify /@ {{1, 0}, {1/2, $s32}, {-1/2, $s32}}];

chooseLatticeBasisVectors = Case[
  {"x", "y"} | {"b", "r"}             := {{1,0}, {0, 1}};
  {"x", "y", "z"} | {"b", "g", "r"}   := {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}};
  {"a", "b", "c"}                     := $TriangularVectors2D;
  other_ := Take[CirclePoints[Len[other] * 2], Len @ other];
]

LatticeQuiverCoordinates::badlen = "Number of cardinals `` didn't match number of basis vectors ``."
LatticeQuiverCoordinates::badrules = "No basis rules matched the cardinal ``."
LatticeQuiverCoordinates::badbasis = "Basis `` should be a list of vectors, rules, or an association.";

LatticeQuiverCoordinates[quiver_Graph, latticeBasis_] := Scope[
  cardinalList = CardinalList @ quiver;
  Which[
    RuleListQ[latticeBasis],
      latticeBasis = Assoc @ Map[
        # -> Rep[Total[ReplaceList[#, latticeBasis]], 0 :> ReturnFailed["badrules", #]]&,
        cardinalList
      ],
    ListQ[latticeBasis],
      If[!SameLenQ[cardinalList, latticeBasis], ReturnFailed["badlen", Len @ cardinalList, Len @ latticeBasis]];
      latticeBasis = AssocThread[cardinalList, latticeBasis],
    !AssocQ[latticeBasis],
      ReturnFailed["badbasis", latticeBasis]
  ];
  indexGraph = ToIndexGraph @ quiver;
  outTable = VertexOutVertexTagTable @ indexGraph;
  dims = Rest @ Dims @ Values @ latticeBasis;
  coords = Repeat[0, Pre[dims, VertexCount @ indexGraph]];
  edgeBasis = Map[latticeBasis, EdgeTagAssociation @ indexGraph];
  edgeBasis = Join[edgeBasis, Map[Minus, KMap[Rev, edgeBasis]]];
  edgeIndex = EdgePairIndexAssociation @ indexGraph;
  edgeIndex = Join[edgeIndex, KMap[Rev, edgeIndex]];
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