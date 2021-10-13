PackageExport["DiscardVertices"]
PackageExport["SelectVertices"]

DiscardVertices[graph_Graph, filter_] := Subgraph[graph, Discard[VertexList[graph], filter]];
SelectVertices[graph_Graph, filter_] := Subgraph[graph, Select[VertexList[graph], filter]];

(**************************************************************************************************)

PackageExport["GraphRelabel"]

GraphRelabel[graph_Graph, f_] :=
  VertexReplace[graph, Map[# -> f[#]&, VertexList[graph]]];

(**************************************************************************************************)

PackageExport["GraphContract"]

SetUsage @ "
GraphContract[graph$, {v$1, v$2, $$}] contracts a set of vertices into one vertex, yielding ContractedVertex[{v$1, v$2, $$}].
GraphContract[graph$, {{v$11, v$12, $$}, {v$21, v$22, $$}, $$}}] makes multiple contractions, which should be disjoint.
GraphContract[graph$, <|key$1 -> {v$11, v$12, $$}, {v$21, v$22, $$}, $$|>] contracts sets of vertices, naming them \
ContractedVertex[{v$i1, v$i2, $$}, key$].
"

GraphContract[g_, {} | <||>] := g;

GraphContract[g_, contraction_List] :=
  VertexReplace[
    VertexContract[g, vertices],
    Map[
      vertices |-> First[vertices] -> ContractedVertex[vertices],
      toListOfLists @ contraction
    ]
  ];

GraphContract[g_, contraction_Association] :=
  VertexReplace[
    VertexContract[g, Values @ vertices],
    KeyValueMap[
      {key, vertices} |-> First[vertices] -> ContractedVertex[vertices, key],
      contraction
    ]
  ];

(**************************************************************************************************)

PackageExport["GraphContractBy"]

Options[GraphContractBy] = {
  VertexLabels -> Automatic
};

GraphContractBy[graph_Graph, func_] :=
  GraphContract[graph,
    If[OptionValue[VertexLabels] === Automatic,
      GroupBy[VertexList[graph], func],
      GatherBy[VertexList[graph], func]
    ]
  ];

(**************************************************************************************************)

PackageExport["GraphSum"]

SetUsage @ "
GraphSum[{g$1, g$2, $$}] takes the sum of a list of graphs g$i, yielding a single graph with vertices SumVertex[v$, i$], \
where v$ is a vertex from g$i.
GraphSum[<|k$1 -> g$1, k$2 -> g$2, $$|>] yields a graph with vertices SumVertex[v$, k$i].
* Any of the graphs g$i can also be an expression g$i -> vertex$. All such vertices will be contracted.
"

ListOrAssociationOf[pattern_] := {Repeated[pattern]} | Association[Repeated[_ -> pattern]];

indexToLabel[{k_} | {Key[k_]}] := k;

relabelSumComponent[graph_, label_] := GraphRelabel[graph, SumVertex[label]];

relabelSumComponent[graph_ -> joinVertex_, index_] := (
    AppendTo[$contractions, SumVertex[joinVertex, label]];
    relabelSumComponent[graph, index]
);

(* TODO: capture and combine the options of all the subgraphs *)
GraphSum[graphs:ListOrAssociationOf[_Graph | Rule[_Graph, _]], opts:OptionsPattern[Graph]] := Scope[
  $contractions = {};
  union = GraphUnion[Sequence @@ MapIndexed[relabelSumComponent[#1, indexToLabel[#2]], graphs], opts];
  GraphContract[union, $contractions]
];

(**************************************************************************************************)

PackageExport["ExtendedGraphProduct"]

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

PackageExport["DependentQuiverProduct"]

SetUsage @ "
DependentQuiverProduct[g1$, g$2] gives the dependent graph product of graph g$1 and g$2.
* The vertices of the product are tuples {v$1, v$2}, where v$i is a vertex of g$i.
* The edges of the product are edges {v$1, v$2} \[DirectedEdge] {w$1, w$2}.
"

Options[DependentQuiverProduct] = JoinOptions[
  "UseCardinalSet" -> False,
  "FlattenProducts" -> False,
  ExtendedGraph
];

DependentQuiverProduct[a_Graph, b_Graph, opts:OptionsPattern[]] :=
  generalBinaryQuiverProduct[a, b, dependentEdgeProduct, opts];

dependentEdgeProduct[head_[at_, ah_, ac_], head_[bt_, bh_, bc_]] :=
  head[VertexProduct[at, bt], VertexProduct[ah, bh], CardinalProduct[ac, bc]];

dependentEdgeProduct[head_[at_, ah_], head_[bt_, bh_]] :=
  head[VertexProduct[at, bt], VertexProduct[ah, bh]];

(**************************************************************************************************)

PackageExport["CartesianQuiverProduct"]

SetUsage @ "
CartesianQuiverProduct[g1$, g$2] gives the Cartesian graph product of graph g$1 and g$2.
"

Options[CartesianQuiverProduct] = Options @ ExtendedGraph;

CartesianQuiverProduct[a_Graph, b_Graph, opts:OptionsPattern[]] :=
  generalBinaryQuiverProduct[a, b, cartesianEdgeProduct, opts];

cartesianEdgeProduct[head_[at_, ah_, ac___], head_[bt_, bh_, bc___]] := {
  head[VertexProduct[at, bt], VertexProduct[ah, bt], ac],
  head[VertexProduct[at, bh], VertexProduct[ah, bh], ac],
  head[VertexProduct[at, bt], VertexProduct[at, bh], bc],
  head[VertexProduct[ah, bt], VertexProduct[ah, bh], bc]
}

(**************************************************************************************************)

PackageExport["IndependentQuiverProduct"]

SetUsage @ "
IndependentQuiverProduct[g1$, g$2] gives the independent graph product of graph g$1 and g$2.
* The vertices of the product are tuples {v$1, v$2}, where v$i is a vertex of g$i.
* The edges of the product are edges either {v$1, v$2} \[DirectedEdge] {w$1, w$2} or \
{v$1, w$2} \[DirectedEdge] {w$1, v$2}.
"

Options[IndependentQuiverProduct] = Options[DependentQuiverProduct];

IndependentQuiverProduct[a_Graph, b_Graph, opts:OptionsPattern[]] :=
  generalBinaryQuiverProduct[a, b, independentEdgeProduct, opts];

independentEdgeProduct[head_[at_, ah_, ac_], head_[bt_, bh_, bc_]] := {
  head[VertexProduct[at, bt], VertexProduct[ah, bh], CardinalProduct[ac, bc]],
  head[VertexProduct[ah, bt], VertexProduct[at, bh], CardinalProduct[Negated @ ac, bc]]
}

independentEdgeProduct[head_[at_, ah_], head_[bt_, bh_]] := {
  head[VertexProduct[at, bt], VertexProduct[ah, bh]],
  head[VertexProduct[ah, bt], VertexProduct[at, bh]]
}

(**************************************************************************************************)

Options[generalBinaryQuiverProduct] = Options[DependentQuiverProduct];

generalBinaryQuiverProduct[a_Graph, b_Graph, edgeProdFn_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[useCardinalSet, flattenProducts];
  opts = JoinOptions[ExtractExtendedGraphOptions /@ {a, b}, opts];
  vertexLists = VertexList /@ {a, b};
  edgeLists = EdgeList /@ {a, b};
  {aColors, bColors} = LookupVertexColors /@ {a, b};
  {aCoords, bCoords} = LookupVertexCoordinates /@ {a, b};
  productVertices = Flatten @ Outer[VertexProduct, Sequence @@ vertexLists, 1];
  productEdges = DeleteDuplicates @ Flatten @ Outer[edgeProdFn, Sequence @@ edgeLists, 1];
  If[!EdgeListQ[productEdges], ReturnFailed["interr", "invalid edges produced"]];
  opts //= DeleteOptions[{VertexAnnotations, EdgeAnnotations, "UseCardinalSet"}];
  vertexColorFunction = If[aColors === None || bColors === None, None,
    VertexProductColorFunction[aColors, bColors]
  ];
  If[flattenProducts, {productVertices, productEdges} //= FlattenProductSymbols];
  If[useCardinalSet,
    productEdges //= ReplaceAll[CardinalProduct[z__] :> CardinalSet[{z}]];
    {aCardColors, bCardColors} = LookupCardinalColors /@ {a, b};
    If[aCardColors =!= <||> && bCardColors =!= <||>,
      opts //= ReplaceOptions[{CardinalColors -> Join[aCardColors, bCardColors], ArrowheadStyle -> Automatic}];
    ];
  ];
  {aSize, bSize} = LookupImageSize /@ {a, b};
  size = {First @ bSize, Last @ aSize};
  ExtendedGraph[
    productVertices, productEdges,
    VertexCoordinates -> Map[productVertexCoords, productVertices],
    VertexColorFunction -> vertexColorFunction,
    ImageSize -> size,
    Sequence @@ opts
  ]
];

productVertexCoords[VertexProduct[a_, b_]] :=
  List[
    First @ bCoords @ b,
    Last @ aCoords @ a
  ];

(**************************************************************************************************)

PackageExport["GeneralQuiverProduct"]

Options[GeneralQuiverProduct] = JoinOptions[
  "UseCardinalSet" -> False,
  "FlattenProducts" -> True,
  VertexCoordinateFunction -> "DimensionReduce",
  ExtendedGraph
];

$productTermElement = _Integer | Negated[_Integer];
$productTermP = {$productTermElement..};

GeneralQuiverProduct::badprodexpr = "Product expression `` should be a list of list of possibly negated integers.";
GeneralQuiverProduct::badgraphs = "First argument is not a list of graphs.";
GeneralQuiverProduct::badcoords = "VertexCoordinateFunction did not produce valid coordinates. First coordinate was: ``.";

GeneralQuiverProduct[graphs_List, productTerms_List, components_:Automatic, userOpts:OptionsPattern[]] := Scope[
  If[productTerms ~!~ {$productTermP...}, ReturnFailed["badprodexpr", productTerms]];
  UnpackOptions[useCardinalSet, vertexCoordinateFunction, flattenProducts];
  graphs = toSimpleQuiver /@ graphs;
  If[graphs ~!~ {__Graph}, ReturnFailed["badgraphs"]];
  opts = DeleteOptions[
    JoinOptions[ExtractExtendedGraphOptions /@ graphs],
    {VertexAnnotations, EdgeAnnotations, VertexCoordinates, VertexLayout, ImageSize, ExtendedGraphLayout}
  ];
  opts = JoinOptions[
    DeleteOptions[{userOpts}, {"UseCardinalSet", "Components", "FlattenProducts", VertexCoordinateFunction}],
    opts
  ];
  vertexLists = VertexList /@ graphs;
  edgeLists = Map[List @@@ EdgeList[#]&, graphs];
  productVertices = VertexProduct @@@ Tuples[vertexLists];
  edgeHead = If[AllTrue[graphs, DirectedGraphQ], DirectedEdge, UndirectedEdge, VertexCoordinateFunction];
  tagAssocs = Map[edgeListTaggedTables] @ edgeLists;
  num = Length[graphs]; signLists = toSignLists[num, #]& /@ productTerms;
  productEdges = DeleteDuplicates @ Flatten @ Outer[makeProductEdges, productVertices, signLists, 1];
  productVerticesOld = productVertices;
  If[flattenProducts, {productVertices, productEdges} //= FlattenProductSymbols];
  productVertexRenaming = If[productVertices === productVerticesOld, Identity,
    Map[AssociationThread[productVertices, productVerticesOld]]
  ];

  If[!EdgeListQ[productEdges], ReturnFailed["interr", "invalid edges produced"]];

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

(* Ok: just store the edges in an assoc, indexed by source, but in negated and unnegated form.
then when building product vertex we just look up in the appropriate key,
*)

edgeListTaggedTables[edgeList_] := Scope[
  a = InVertices  @ edgeList;
  b = OutVertices @ edgeList;
  c = Part[edgeList, All, 3];
  oAssoc = Merge[Identity] @ RuleThread[a, Transpose[{b, c}]];
  iAssoc = Merge[Identity] @ RuleThread[b, Transpose[{a, Negated /@ c}]];
  {oAssoc, iAssoc}
]

toSignLists[num_, indices_] := Scope[
  arr = Zeros[num];
  indices = indices /. i_Integer ? Negative :> Negated[Abs @ i];
  Cases[indices, i_Integer :> Part[arr, i]++];
  Cases[indices, Negated[i_Integer] :> Part[arr, Abs @ i]--];
  arr
];

PackageScope["toSimpleQuiver"]

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

PackageExport["NegateGraph"]

NegateGraph[g_Graph] := Scope[
  Graph[
    VertexList @ g, ReverseEdges @ EdgeList @ g,
    Options @ g
  ]
]


(**************************************************************************************************)

PackageExport["ReverseEdges"]

ReverseEdges[edges_List] := Map[reverseEdge, edges];

reverseEdge = Case[
  head_[a_, b_] := head[b, a];
  head_[a_, b_, c_] := head[b, a, c];
];

(**************************************************************************************************)

PackageExport["VertexProductColorFunction"]

VertexProductColorFunction[c1_, c2_][VertexProduct[v1_, v2_]] :=
  HumanBlend[{Lookup[c1, v1], Lookup[c2, v2]}];

(**************************************************************************************************)

PackageExport["RestrictedVertexProductGraph"]

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

PackageExport["RestrictedVertexProducts"]

RestrictedVertexProducts[graph_, cond_] :=
  integersToVertices[graph, RestrictedVertexIndexProducts[graph, cond]];

(**************************************************************************************************)

PackageExport["RestrictedVertexIndexProducts"]

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

