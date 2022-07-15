PublicFunction[ExtendedGraphProduct]

SetUsage @ "
ExtendedGraphProduct[{g$1, g$2, $$}, type$] takes the direct product of a list of graphs g$i, yielding a single graph with vertices \
VertexProducts[{v$1, v$2, $$}], where v$i is a vertex from g$i.
GraphProduct[<|k$1 -> g$1, k$2 -> g$2, $$|>, type$] yields a graph with vertices VertexProducts[<|k$1 -> v$1, k$2 -> v$2, $$|>].
type* specifies how to determine the edges of the product graph, and can be one of the following:
| 'Cartesian' | Total[d$i] == 1 |
| 'Tensor' | d$i == 1 |
| 'Strong' | Max[d$i] == 1 |
* If the option EdgeTags -> True is specified, than any of the underlying edges have tags, then the tags are merged to \
form tag on the edges of the product graph, in the following way: if a product edge is created due to one of the d$i being \
equal to 1, then there will typically be one directed edge present in the corresponding graph (there could however be more \
than one for a multigraph). The tags for these directed edges are used in an association, with the corresponding keys being \
the graphs responsible for them.
"

ExtendedGraphProduct[graphs:ListOrAssociationOf[_Graph], type_, opts:OptionsPattern[Graph]] := Scope[
  If[AssociationQ[graphs],
    keys = Keys[graphs];
    graphs = Values[graphs]
  ,
    keys = None
  ];
  counts = VertexCount /@ graph;
  outTables = VertexOrientedOutTable /@ graph;
  $NotImplemented
];

(**************************************************************************************************)

PublicFunction[LockedQuiverProduct]

SetUsage @ "
LockedQuiverProduct[g1$, g$2] gives the dependent graph product of graph g$1 and g$2.
* The vertices of the product are tuples {v$1, v$2}, where v$i is a vertex of g$i.
* The edges of the product are edges {v$1, v$2} \[DirectedEdge] {w$1, w$2}.
"

Options[LockedQuiverProduct] = JoinOptions[
  "UseCardinalSet" -> False,
  "FlattenProducts" -> False,
  ExtendedGraph
];

LockedQuiverProduct[a_Graph, b_Graph, opts:OptionsPattern[]] :=
  generalBinaryQuiverProduct[a, b, dependentEdgeProduct, ArrowheadPosition -> 0.55, opts];

dependentEdgeProduct[head_[at_, ah_, ac_], head_[bt_, bh_, bc_]] :=
  head[VertexProduct[at, bt], VertexProduct[ah, bh], CardinalProduct[ac, bc]];

(**************************************************************************************************)

PublicFunction[CartesianQuiverProduct]

SetUsage @ "
CartesianQuiverProduct[g1$, g$2] gives the Cartesian graph product of graph g$1 and g$2.
"

Options[CartesianQuiverProduct] = Options @ ExtendedGraph;

CartesianQuiverProduct[a_Graph, b_Graph, opts:OptionsPattern[]] :=
  generalBinaryQuiverProduct[a, b, cartesianEdgeProduct, ArrowheadPosition -> 0.55, opts];

cartesianEdgeProduct[head_[at_, ah_, ac_], head_[bt_, bh_, bc_]] := {
  head[VertexProduct[at, bt], VertexProduct[ah, bt], CardinalProduct[ac, 1]],
  head[VertexProduct[at, bh], VertexProduct[ah, bh], CardinalProduct[ac, 1]],
  head[VertexProduct[at, bt], VertexProduct[at, bh], CardinalProduct[1, bc]],
  head[VertexProduct[ah, bt], VertexProduct[ah, bh], CardinalProduct[1, bc]]
}

(**************************************************************************************************)

PublicFunction[RightFreeQuiverProduct, LeftFreeQuiverProduct]

Options[LeftFreeQuiverProduct] = Options[RightFreeQuiverProduct] = Options[LockedQuiverProduct];

RightFreeQuiverProduct[a_Graph, b_Graph, opts:OptionsPattern[]] :=
  generalBinaryQuiverProduct[a, b, rightFiberEdgeProduct, opts];

rightFiberEdgeProduct[head_[at_, ah_, ac_], head_[bt_, bh_, bc_]] := {
  head[VertexProduct[at, bt], VertexProduct[ah, bh], CardinalProduct[ac, bc]],
  head[VertexProduct[at, bt], VertexProduct[ah, bt], CardinalProduct[ac, 1]],
  head[VertexProduct[at, bh], VertexProduct[ah, bh], CardinalProduct[ac, 1]],
  head[VertexProduct[at, bh], VertexProduct[ah, bt], CardinalProduct[ac, Inverted @ bc]]
}

LeftFreeQuiverProduct[a_Graph, b_Graph, opts:OptionsPattern[]] :=
  generalBinaryQuiverProduct[a, b, leftFiberEdgeProduct, opts];

leftFiberEdgeProduct[head_[at_, ah_, ac_], head_[bt_, bh_, bc_]] := {
  head[VertexProduct[at, bt], VertexProduct[ah, bh], CardinalProduct[ac, bc]],
  head[VertexProduct[ah, bt], VertexProduct[ah, bh], CardinalProduct[1, bc]],
  head[VertexProduct[at, bt], VertexProduct[at, bh], CardinalProduct[1, bc]],
  head[VertexProduct[ah, bt], VertexProduct[at, bh], CardinalProduct[Inverted @ ac, bc]]
}

(**************************************************************************************************)

PublicFunction[LeftStrongQuiverProduct, RightStrongQuiverProduct, StrongQuiverProduct]

Options[LeftStrongQuiverProduct] = Options[RightStrongQuiverProduct] = Options[StrongQuiverProduct] = Options[LockedQuiverProduct];

LeftStrongQuiverProduct[a_Graph, b_Graph, opts:OptionsPattern[]] :=
  generalBinaryQuiverProduct[a, b, leftStrongEdgeProduct, opts];

leftStrongEdgeProduct[head_[at_, ah_, ac_], head_[bt_, bh_, bc_]] := {
  head[VertexProduct[at, bt], VertexProduct[ah, bh], CardinalProduct[ac, bc]],
  head[VertexProduct[ah, bt], VertexProduct[ah, bh], CardinalProduct[1, bc]],
  head[VertexProduct[at, bt], VertexProduct[at, bh], CardinalProduct[1, bc]],
  head[VertexProduct[at, bt], VertexProduct[ah, bt], CardinalProduct[ac, 1]],
  head[VertexProduct[at, bh], VertexProduct[ah, bh], CardinalProduct[ac, 1]],
  head[VertexProduct[ah, bt], VertexProduct[at, bh], CardinalProduct[Inverted @ ac, bc]]
}

RightStrongQuiverProduct[a_Graph, b_Graph, opts:OptionsPattern[]] :=
  generalBinaryQuiverProduct[a, b, rightStrongEdgeProduct, opts];

rightStrongEdgeProduct[head_[at_, ah_, ac_], head_[bt_, bh_, bc_]] := {
  head[VertexProduct[at, bt], VertexProduct[ah, bh], CardinalProduct[ac, bc]],
  head[VertexProduct[ah, bt], VertexProduct[ah, bh], CardinalProduct[1, bc]],
  head[VertexProduct[at, bt], VertexProduct[at, bh], CardinalProduct[1, bc]],
  head[VertexProduct[at, bt], VertexProduct[ah, bt], CardinalProduct[ac, 1]],
  head[VertexProduct[at, bh], VertexProduct[ah, bh], CardinalProduct[ac, 1]],
  head[VertexProduct[at, bh], VertexProduct[ah, bt], CardinalProduct[ac, Inverted @ bc]]
}

StrongQuiverProduct[a_Graph, b_Graph, opts:OptionsPattern[]] :=
  generalBinaryQuiverProduct[a, b, strongEdgeProduct, opts];

strongEdgeProduct[head_[at_, ah_, ac_], head_[bt_, bh_, bc_]] := {
  head[VertexProduct[at, bt], VertexProduct[ah, bh], CardinalProduct[ac, bc]],
  head[VertexProduct[ah, bt], VertexProduct[ah, bh], CardinalProduct[1, bc]],
  head[VertexProduct[at, bt], VertexProduct[at, bh], CardinalProduct[1, bc]],
  head[VertexProduct[at, bt], VertexProduct[ah, bt], CardinalProduct[ac, 1]],
  head[VertexProduct[at, bh], VertexProduct[ah, bh], CardinalProduct[ac, 1]],
  head[VertexProduct[at, bh], VertexProduct[ah, bt], CardinalProduct[ac, Inverted @ bc]]
}

(**************************************************************************************************)
(*
PublicFunction[StrongQuiverProduct]

Options[StrongQuiverProduct] = Options[LockedQuiverProduct];

StrongQuiverProduct[a_Graph, b_Graph, opts:OptionsPattern[]] :=
  generalBinaryQuiverProduct[a, b, strongIndependentEdgeProduct, opts];

strongIndependentEdgeProduct[head_[at_, ah_, ac_], head_[bt_, bh_, bc_]] := {
  head[VertexProduct[at, bt], VertexProduct[ah, bh], CardinalProduct[ac, bc]],
  head[VertexProduct[at, bh], VertexProduct[ah, bt], CardinalProduct[ac, Inverted @ bc]],
  head[VertexProduct[at, bh], VertexProduct[ah, bh], CardinalProduct[ac, 1]],
  head[VertexProduct[at, bt], VertexProduct[ah, bt], CardinalProduct[ac, 1]]
}
 *)
(**************************************************************************************************)

PublicFunction[GeneralBinaryQuiverProduct]

Options[GeneralBinaryQuiverProduct] = Options[LockedQuiverProduct];

GeneralBinaryQuiverProduct[g1_Graph, g2_Graph, productTerms_List, userOpts:OptionsPattern[]] :=
  generalBinaryQuiverProduct[g1, g2, multiEdgeFactory @ Map[Sort] @ productTerms, userOpts];

GeneralBinaryQuiverProduct[productTerms_List, opts1___Rule][g1_Graph, g2_Graph, opts2___Rule] :=
  GeneralBinaryQuiverProduct[g1, g2, productTerms, opts1, opts2];

multiEdgeFactory[terms_][head_[at_, ah_, ac_], head_[bt_, bh_, bc_]] :=
  VectorApply[head, Catenate @ Map[singleEdgeFactory[ToList @ #][at, ah, ac, bt, bh, bc]&, terms]];

singleEdgeFactory = Case[
  {}          := {{VertexProduct[#1, #4], VertexProduct[#1, #4], CardinalProduct[1, 1]},
                  {VertexProduct[#2, #5], VertexProduct[#2, #5], CardinalProduct[1, 1]},
                  {VertexProduct[#1, #5], VertexProduct[#1, #5], CardinalProduct[1, 1]},
                  {VertexProduct[#2, #4], VertexProduct[#2, #4], CardinalProduct[1, 1]}}&;
  {1}         := {{VertexProduct[#1, #4], VertexProduct[#2, #4], CardinalProduct[#3, 1]},
                  {VertexProduct[#1, #5], VertexProduct[#2, #5], CardinalProduct[#3, 1]}}&;
  {-1}        := {{VertexProduct[#2, #4], VertexProduct[#1, #4], CardinalProduct[Inverted @ #3, 1]},
                  {VertexProduct[#2, #5], VertexProduct[#1, #5], CardinalProduct[Inverted @ #3, 1]}}&;
  {2}         := {{VertexProduct[#1, #4], VertexProduct[#1, #5], CardinalProduct[1, #6]},
                  {VertexProduct[#2, #4], VertexProduct[#2, #5], CardinalProduct[1, #6]}}&;
  {-2}        := {{VertexProduct[#1, #5], VertexProduct[#1, #4], CardinalProduct[1, Inverted @ #6]},
                  {VertexProduct[#2, #5], VertexProduct[#2, #4], CardinalProduct[1, Inverted @ #6]}}&;
  {1, 2}      := {{VertexProduct[#1, #4], VertexProduct[#2, #5], CardinalProduct[#3, #6]}}&;
  {-2, 1}     := {{VertexProduct[#1, #5], VertexProduct[#2, #4], CardinalProduct[#3, Inverted @ #6]}}&;
  {-1, 2}     := {{VertexProduct[#2, #4], VertexProduct[#1, #5], CardinalProduct[Inverted @ #3, #6]}}&;
  {-2, -1}    := {{VertexProduct[#2, #5], VertexProduct[#1, #4], CardinalProduct[Inverted @ #3, Inverted @ #6]}}&;
];

(**************************************************************************************************)

Options[generalBinaryQuiverProduct] = Options[LockedQuiverProduct];

generalBinaryQuiverProduct[a_Graph, b_Graph, edgeProdFn_, userOpts:OptionsPattern[]] := Scope[
  UnpackOptions[useCardinalSet, flattenProducts];
  opts = JoinOptions[ExtractExtendedGraphOptions /@ {a, b}];
  opts //= DeleteOptions[{VertexCoordinates, VertexCoordinateRules, ImageSize, CardinalColors, CardinalColorFunction}];
  opts = JoinOptions[userOpts, opts];
  vertexLists = VertexList /@ {a, b};
  edgeLists = AddEdgeTags[EdgeList[#], None]& /@ {a, b};
  {aColors, bColors} = LookupVertexColors /@ {a, b};
  {aCoords, bCoords} = LookupVertexCoordinates /@ {a, b};
  productVertices = Flatten @ Outer[VertexProduct, Sequence @@ vertexLists, 1];
  productEdges = DeleteDuplicates @ Flatten @ Outer[edgeProdFn, Sequence @@ edgeLists, 1];
  If[!EdgeListQ[productEdges], Print[productEdges]; ReturnFailed["interr", "invalid edges produced"]];
  opts //= DeleteOptions[{VertexAnnotations, EdgeAnnotations, "UseCardinalSet", Cardinals, VertexColorFunction}];
  vertexColorFunction = If[aColors === None || bColors === None, None,
    VertexProductColorFunction[aColors, bColors]
  ];
  If[flattenProducts, {productVertices, productEdges} //= FlattenProductSymbols];
  {aCardColors, bCardColors} = LookupCardinalColors /@ {a, b};
  If[useCardinalSet,
    productEdges //= ReplaceAll[CardinalProduct[z__] :> CardinalSet[{z}]];
    If[aCardColors =!= <||> && bCardColors =!= <||>,
      opts //= ReplaceOptions[{CardinalColors -> Join[aCardColors, bCardColors], ArrowheadStyle -> Automatic}];
    ];
  ,
    opts //= ReplaceOptions[{CardinalColorFunction -> CardinalProductColorFunction[aCardColors, bCardColors], ArrowheadStyle -> Automatic}];
  ];
  If[!AllTrue[{a, b}, EdgeTaggedGraphQ], productEdges //= RemoveEdgeTags];
  {aSize, bSize} = LookupImageSize /@ {a, b};
  coordsCands = Table[Map[fn, productVertices], {fn, {productVertexCoords1, productVertexCoords2, productVertexCoords3, productVertexCoords4}}];
  bestIndex = MaximumIndexBy[coordsCands, countDistinctRounded];
  coords = Part[coordsCands, bestIndex];
  sizeCands = {{First @ bSize, Last @ aSize}, {Last @ bSize, First @ aSize}, {First @ bSize, First @ aSize}, {Last @ bSize, Last @ aSize}};
  size = Part[sizeCands, bestIndex];
  ExtendedGraph[
    productVertices, productEdges,
    Sequence @@ opts,
    VertexCoordinates -> coords,
    VertexColorFunction -> vertexColorFunction,
    ImageSize -> size
  ]
];

productVertexCoords1[VertexProduct[a_, b_]] := List[First @ bCoords @ b, Last @ aCoords @ a];
productVertexCoords2[VertexProduct[a_, b_]] := List[Last @ bCoords @ b, First @ aCoords @ a];
productVertexCoords3[VertexProduct[a_, b_]] := List[First @ bCoords @ b, First @ aCoords @ a];
productVertexCoords4[VertexProduct[a_, b_]] := List[Last @ bCoords @ b, Last @ aCoords @ a];

countDistinctRounded[e_] := CountDistinct[Round[e, 0.001]];

(**************************************************************************************************)

PublicFunction[CardinalProductColorFunction]

CardinalProductColorFunction[<||>, <||>][_] := $Gray;

CardinalProductColorFunction[aColors_, bColors_][CardinalProduct[1, b_]] := Lookup[bColors, b];

CardinalProductColorFunction[aColors_, bColors_][CardinalProduct[a_, 1]] := Lookup[aColors, a];

CardinalProductColorFunction[aColors_, bColors_][CardinalProduct[a_, b_]] :=
  HumanBlend[{Lookup[aColors, StripInverted @ a], Lookup[bColors, StripInverted @ b]}];

(**************************************************************************************************)

PublicFunction[GeneralQuiverProduct]

Options[GeneralQuiverProduct] = JoinOptions[
  "UseCardinalSet" -> False,
  "FlattenProducts" -> True,
  VertexCoordinateFunction -> "DimensionReduce",
  ExtendedGraph
];

$productTermElement = _Integer | Inverted[_Integer];
$productTermP = {$productTermElement...};

GeneralQuiverProduct::badprodexpr = "Product expression `` should be a list of list of possibly inverted integers.";
GeneralQuiverProduct::badgraphs = "First argument is not a list of graphs.";
GeneralQuiverProduct::badcoords = "VertexCoordinateFunction did not produce valid coordinates. First coordinate was: ``.";

GeneralQuiverProduct[graphs_List, productTerms_List, components_:Automatic, userOpts:OptionsPattern[]] := Scope[
  If[productTerms ~!~ {$productTermP...}, ReturnFailed["badprodexpr", productTerms]];
  UnpackOptions[useCardinalSet, vertexCoordinateFunction, flattenProducts];
  graphs = toSimpleQuiver /@ graphs;
  If[graphs ~!~ {__Graph}, ReturnFailed["badgraphs"]];
  opts = DeleteOptions[
    JoinOptions[ExtractExtendedGraphOptions /@ graphs],
    {VertexAnnotations, EdgeAnnotations, VertexCoordinates, VertexLayout, ImageSize, Cardinals}
  ];
  opts = JoinOptions[
    DeleteOptions[{userOpts}, {"UseCardinalSet", "Components", "FlattenProducts", VertexCoordinateFunction}],
    opts
  ];
  vertexLists = VertexList /@ graphs;
  edgeLists = Map[List @@@ EdgeList[#]&, graphs];
  productVertices = VertexProduct @@@ Tuples[vertexLists];
  edgeHead = If[AllTrue[graphs, DirectedGraphQ], DirectedEdge, UndirectedEdge, VertexCoordinateFunction];
  If[!AllTrue[graphs, EdgeTaggedGraphQ], edgeHead = edgeHead /* RemoveEdgeTag];
  tagAssocs = Map[edgeListTaggedTables] @ edgeLists;
  num = Length[graphs]; signLists = toSignLists[num, #]& /@ productTerms;
  productEdges = DeleteDuplicates @ Flatten @ Outer[makeProductEdges, productVertices, signLists, 1];
  productVerticesOld = productVertices;
  If[flattenProducts, {productVertices, productEdges} //= FlattenProductSymbols];
  productVertexRenaming = If[productVertices === productVerticesOld, Identity,
    Map[AssociationThread[productVertices, productVerticesOld]]
  ];

  If[!EdgeListQ[productEdges], Print[productEdges]; ReturnFailed["interr", "invalid edges produced"]];

  If[vertexCoordinateFunction ~!~ Automatic | None,
    coordinateAssocs = LookupVertexCoordinates /@ graphs;
  ];

  If[Head[components] === VertexProduct, components //= VertexPattern];

  If[components === Automatic,
    Return @ toGeneralProductFinalGraph[productVertices, productEdges]];
  
  mainGraph = Graph[productVertices, productEdges];
  compGraphs = ComponentGraphs[mainGraph, components];
  If[FailureQ[compGraphs], ReturnFailed[]];

  If[ListQ[compGraphs],
    toGeneralProductFinalGraph /@ compGraphs,
    toGeneralProductFinalGraph @ compGraphs
  ]
];

toGeneralProductFinalGraph[graph_Graph] :=
  toGeneralProductFinalGraph @@ VertexEdgeList @ graph;

toGeneralProductFinalGraph[productVertices_, productEdges_] := Scope[
  If[vertexCoordinateFunction ~!~ Automatic | None,
    originalProductVertices = productVertexRenaming @ productVertices;
    coordinateTuples = Map[
      vertex |-> MapThread[Lookup, {coordinateAssocs, List @@ vertex}],
      originalProductVertices
    ];
    vcf = toProductCoordFunc @ vertexCoordinateFunction;
    If[CoordinateMatrixQ @ vcf,
      vertexCoords = vcf;
    ,
      vertexCoords = MapThread[vcf, {originalProductVertices, coordinateTuples}];
      If[!CoordinateMatrixQ[vertexCoords], ReturnFailed[GeneralQuiverProduct::badcoords, First @ vertexCoords]];
    ];
  ,
    vertexCoords = Automatic
  ];

  If[useCardinalSet,
    productEdges //= ReplaceAll[CardinalProduct[z__] :> CardinalSet[{z}]];
  ];
  ExtendedGraph[
    productVertices, productEdges,
    Sequence @@ opts,
    VertexCoordinates -> vertexCoords,
    ImageSize -> {200, 200}
  ]
]

(* Ok: just store the edges in an assoc, indexed by source, but in inverted and uninverted form.
then when building product vertex we just look up in the appropriate key,
*)

edgeListTaggedTables[edgeList_] := Scope[
  a = InVertices  @ edgeList;
  b = OutVertices @ edgeList;
  c = If[Length @ First @ edgeList === 3,
    Part[edgeList, All, 3],
    ConstantArray[None, Length @ edgeList]
  ];
  oAssoc = Merge[Identity] @ RuleThread[a, Transpose[{b, c}]];
  iAssoc = Merge[Identity] @ RuleThread[b, Transpose[{a, Inverted /@ c}]];
  {oAssoc, iAssoc}
]

toSignLists[num_, indices_] := Scope[
  arr = Zeros[num];
  indices = indices /. i_Integer ? Negative :> Inverted[Abs @ i];
  Cases[indices, i_Integer :> Part[arr, i]++];
  Cases[indices, Inverted[i_Integer] :> Part[arr, Abs @ i]--];
  arr
];

PrivateFunction[toSimpleQuiver]

toSimpleQuiver = Case[
  g_Graph                  := g;
  n_Integer ? Negative     := CycleQuiver[Abs @ n];
  n_Integer                := LineQuiver[n];
  {m_Integer, n_Integer}   := SquareQuiver[{m, n}];
  card_String -> n_Integer := LineQuiver[n, card];
  other_                   := $Failed;
];

makeProductEdges[vertex_, signs_] := Scope[
  outVerticesAndTags = MapThread[makeEdgeItems, {signs, List @@ vertex, tagAssocs}];
  Map[
    edgeHead[vertex, VertexProduct @@ Part[#, All, 1], CardinalProduct @@ Part[#, All, 2]]&,
    Tuples @ outVerticesAndTags
  ]
]

(* out and in here are assocs from vertex to {outVertex, card} *)
makeEdgeItems[1, v_, {out_, _}] := Lookup[out, Key @ v, {}];
makeEdgeItems[-1, v_, {_, in_}] := Lookup[in, Key @ v, {}];
makeEdgeItems[0, v_, _] := {{v, 1}};

makeEdgeItems[2, v_, {out_, _}] := Catenate[
  Map[toSeqCard[#2], Lookup[out, Key @ #1, {}]]& @@@
    Lookup[out, Key @ v, {}]
];
toSeqCard[c1_][{v2_, c2_}] := {v2, CardinalSequence[c1, c2]};


toProductCoordFunc = Case[
  funcs_List   := ApplyThrough[toSingleCoordFunc /@ funcs];
  "DimensionReduce" := DimensionReduce[coordinateTuples, 2];
  "ABC"        := abcProductCoords;
  "Mean"       := meanProductCoords;
  n_Integer    := Function[Part[#2, n]];
  func_        := func
];

abcProductCoords[_, coords_] := DotABC @ Part[coords, 1;;3, 1];
meanProductCoords[_, coorsd_] := Mean @ coords;

toSingleCoordFunc = Case[
  i_Integer -> j_Integer := Part[#2, i, j]&;
  i_Integer              := Part[#2, i, 1]&;
  func_                  := func;
]

computeProductCoords = Case[
  list_List := Quiet @ Transpose[computedProductSingleCoord /@ list];
  func_     := MapThread[func, vertexCoords];
]

computedProductSingleCoord = Case[
  i_Integer -> j_Integer := Part[vertexCoords, i, All, j];
  _                      := $Failed;
]

(**************************************************************************************************)

PublicFunction[VertexProductColorFunction]

VertexProductColorFunction[c1_, c2_][VertexProduct[v1_, v2_]] :=
  HumanBlend[{Lookup[c1, v1], Lookup[c2, v2]}];

(**************************************************************************************************)

PublicFunction[RestrictedVertexProductGraph]

SetUsage @ "
RestrictedVertexProductGraph[graph$, cond$] yields a graph in which vertices are products VertexProduct[u$, v$] of \
vertices in graph$, and where an edge connects two product vertices VertexProduct[u$1, v$1] and VertexProduct[u$2, v$2] if \
u$1 is connected to u$2 and v$1 = v$2, or vice versa. The restriction is represented by cond$, which can be one of the following:
* GreaterThan[0]: u$ != v$
* GreaterThan[d$]: d(u$, v$) > d$
* LessThan[d$]: d(u$, v$) < d$
* EqualTo[d$]: d(u$, v$) == d$
* Between[{d$min, d$max}]: d$min <= d(u$, v$) <= d$max
The list of vertex products can be obtained directly with the function RestrictedVertexProducts.
The type of edge (either DirectedEdge or UndirectedEdge) that connects two product vertices depends on the type of edge that \
connected the differing vertices in the original graph.
"

expandBiEdge[head_, index_, pair_] :=
  head[index, #]& /@ DeleteCases[Extract[$productIndex, Thread @ pair], 0];

createProductEdges[pos:{i_, j_}, {index_}] := Scope[
  {out1d, out1u} = outTable[[i]];
  {out2d, out2u} = outTable[[j]];
  List[
    expandBiEdge[DirectedEdge, index, {i, out1d}],
    expandBiEdge[DirectedEdge, index, {out2d, j}],
    expandBiEdge[UndirectedEdge, index, {i, out1u}],
    expandBiEdge[UndirectedEdge, index, {out2u, j}]
  ]
];

RestrictedVertexProductGraph[graph_, cond_, opts:OptionsPattern[Graph]] := Scope[
  count = VertexCount[graph];
  outTable = VertexOrientedOutTable[graph];
  productVertices = RestrictedVertexIndexProducts[graph, cond];
  productCount = Length[productSet];
  $productIndex = SparseArray[Thread[products -> Range[productCount]], {count, count}];
  productEdges = MapIndexed[createProductEdges, productVertices];
  Graph[productVertices, Flatten @ productEdges, opts]
]

(**************************************************************************************************)

PublicFunction[RestrictedVertexProducts]

RestrictedVertexProducts[graph_, cond_] :=
  integersToVertices[graph, RestrictedVertexIndexProducts[graph, cond]];

(**************************************************************************************************)

PublicFunction[RestrictedVertexIndexProducts]

RestrictedVertexIndexProducts[graph_, cond_] := Scope[
  count = VertexCount[graph];
  tuples = Switch[cond,
    GreaterThan[0],
      Discard[Tuples[Range[count], 2], Apply[Equal]],
    LessThan[1] | Between[{0, 1}],
      range = Range[count];
      Join[Transpose[{range, range}], EdgePairs[graph]],
    EqualTo[1],
      EdgePairs[graph],
    _LessThan | _EqualTo | _Between | _GreaterThan,
      dist = Normal @ GraphDistanceMatrix[graph, toMax @ cond, Method -> "Johnson"];
      Position[dist, _Integer ? cond, {2}],
    _,
      $Failed
  ]
];

toMax = MatchValues[
  LessThan[d_] := d;
  EqualTo[d_] := d;
  Between[{dmin_, dmax_}] := dmax;
  GreaterThan[d_] := Infinity;
];
