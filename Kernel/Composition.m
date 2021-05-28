PackageExport["DiscardVertices"]
PackageExport["SelectVertices"]

DiscardVertices[graph_Graph, filter_] := Subgraph[graph, Discard[VertexList[graph], filter]];
SelectVertices[graph_Graph, filter_] := Subgraph[graph, Select[VertexList[graph], filter]];



PackageExport["GraphRelabel"]

GraphRelabel[graph_Graph, f_] :=
  VertexReplace[graph, Map[# -> f[#]&, VertexList[graph]]];


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


PackageExport["GraphProduct"]

SetUsage @ "
GraphProduct[{g$1, g$2, $$}, type$] takes the direct product of a list of graphs g$i, yielding a single graph with vertices \
ProductVertex[{v$1, v$2, $$}], where v$i is a vertex from g$i.
GraphProduct[<|k$1 -> g$1, k$2 -> g$2, $$|>, type$] yields a graph with vertices ProductVertex[<|k$1 -> v$1, k$2 -> v$2, $$|>].
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

GraphProduct[graphs:ListOrAssociationOf[_Graph], type_, opts:OptionsPattern[Graph]] := Scope[
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


PackageExport["RestrictedVertexProducts"]

RestrictedVertexProducts[graph_, cond_] :=
  integersToVertices[graph, RestrictedVertexIndexProducts[graph, cond]];


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

