PublicFunction[GraphContract]

SetUsage @ "
GraphContract[graph$, {v$1, v$2, $$}] contracts a set of vertices into one vertex, yielding ContractedVertex[{v$1, v$2, $$}].
GraphContract[graph$, {{v$11, v$12, $$}, {v$21, v$22, $$}, $$}}] makes multiple contractions, which should be disjoint.
GraphContract[graph$, <|key$1 -> {v$11, v$12, $$}, {v$21, v$22, $$}, $$|>] contracts sets of vertices, naming them \
ContractedVertex[{v$i1, v$i2, $$}, key$].
"

GraphContract[g_, {} | <||>, ___] := g;

GraphContract[g_, contraction_, opts:OptionsPattern[]] :=
  GraphContract[g, contraction, ContractedVertex, opts];

GraphContract[g_, contraction_List, opts:OptionsPattern[]] :=
  VertexReplace[
    VertexContract[g, vertices],
    Map[
      vertices |-> F[vertices] -> ContractedVertex[vertices],
      toListOfLists @ contraction
    ]
  ] // egraph[opts];

GraphContract[g_, contraction_Assoc, fn:Except[_Rule], opts:OptionsPattern[]] :=
  VertexReplace[
    VertexContract[g, Values @ contraction],
    KVMap[
      {key, vertices} |-> F[vertices] -> fn[vertices, key],
      contraction
    ]
  ] // egraph[opts];


(**************************************************************************************************)

PublicFunction[GraphContractBy]

GraphContractBy[graph_Graph, func_, opts:OptionsPattern[]] :=
  GraphContractBy[graph, func, ContractedVertex, opts];

GraphContractBy[graph_Graph, func_, nameFn_, opts:OptionsPattern[]] := Switch[
  nameFn,
  "Key",          GraphContract[graph,  GroupBy[VertexList @ graph, func], #2&],
  "FirstVertex",  VertexContract[graph, GatherBy[VertexList @ graph, func]],
  "Vertices",     GraphContract[graph,  GatherBy[VertexList @ graph, func]],
  _,              GraphContract[graph,  GroupBy[VertexList @ graph, func], nameFn]
] // egraph[opts];

egraph[] := Id;
egraph[opts__][g_] := ExtendedGraph[g, opts];

(**************************************************************************************************)

PublicFunction[GraphContractionList]

GraphContractionList[graph_, opts_List] := Scope[
  orderGraph = GraphContractionLattice[graph, opts];
  LookupVertexAnnotations[orderGraph, "ContractedGraph"]
]

(**************************************************************************************************)

PublicFunction[GraphContractionLattice]

Options[GraphContractionLattice] = JoinOptions[
  "GreedyEdgeContraction" -> True,
  CombineMultiedges -> False,
  "SpacelikeOnly" -> False,
  "AllowCyclic" -> True,
  "GraphStyle" -> "ContractedGraph",
  "AllowGraphContractions" -> False,
  "EdgeColoring" -> Auto,
  ExtendedGraph
]

GraphContractionLattice[graph_, contractedGraphOptions_List, userOpts:OptionsPattern[]] := Scope[
  
  If[!EdgeTaggedGraphQ[graph], graph //= IndexEdgeTaggedGraph];

  UnpackOptions[combineMultiedges, greedyEdgeContraction, edgeColoring];

  innerSize = LookupOption[contractedGraphOptions, ImageSize, 50];
  contractedGraphOptions = Sequence @@ contractedGraphOptions;
  
  initialGraph = ContractedGraph[graph, contractedGraphOptions, ImageSize -> {innerSize, innerSize},
    EdgeColorFunction -> edgeColoring];
  innerOpts = Sequence @@ ExtractExtendedGraphOptions @ initialGraph;

  edgeList = CanonicalizeEdges @ EdgeList @ graph;
  isDirected = DirectedGraphQ @ graph;
  sorter = If[isDirected, Id, Map[Sort]];

  successorFn = If[greedyEdgeContraction, greedyGraphContractionSuccessors, graphContractionSuccessors];
  {vlist, ielist} = MultiwaySystem[successorFn, {edgeList}, {"VertexList", "IndexEdgeList"}];
  
  irange = Range @ Len @ vlist;
      
  postFn = If[combineMultiedges, CombineMultiedges, Id];

  graphFn = edges |-> ExtendedGraph[
    AllUniqueVertices @ edges,
    edges,
    innerOpts
  ];
  contractedGraphs = Map[graphFn /* postFn, vlist];
  
  ExtendedGraph[
    Range @ Len @ vlist, ielist, FilterOptions @ userOpts,
    GraphLayout -> TreeVertexLayout[Balanced -> True],
    VertexAnnotations -> <|"ContractedGraph" -> contractedGraphs|>,
    ArrowheadShape -> None, VertexSize -> innerSize, VertexShapeFunction -> "ContractedGraph"
  ]
];

greedyGraphContractionSuccessors[edgeList_] :=
  greedyContractEdges /@ (vertexContractionSuccessors @ edgeList);

graphContractionSuccessors[edgeList_] := Join[
  edgeContractionSuccessors @ edgeList,
  vertexContractionSuccessors @ edgeList
];

vertexContractionSuccessors[edgeList_] := Scope[
  vertices = AllUniqueVertices[edgeList];
  rules = toVertexContractionRule /@ UnorderedPairs[vertices];
  gluingResultsList[edgeList, rules]
];

$flattenGlue = e_ContractedEdge | e_ContractedVertex :> Dedup[e];

toVertexContractionRule[{v_}] := Nothing;

toVertexContractionRule[verts_List] := With[
  {alts = Alt @@ verts, glued = ContractedVertex @@ verts},
  {
    head_[alts, alts, c___] :> head[glued, glued, c],
    head_[alts, b_, c___] :> head[glued, b, c],
    head_[a_, alts, c___] :> head[a, glued, c]
  }
];

gluingResult[edgeList_, rules_] :=
  Sort @ CanonicalizeEdges @ Dedup[VectorReplace[edgeList, rules] /. $flattenGlue];

gluingResultsList[edgeList_, rulesList_] := Map[
  gluingResult[edgeList, #]&,
  rulesList
];

edgeContractionSuccessors[edgeList_] := Scope[
  index = Values @ PositionIndex[Take[edgeList, All, 2]];
  index = Select[index, Len[#] >= 2&];
  rules = Flatten[toEdgeContractionRuleList[Part[edgeList, #]]& /@ index];
  Sort[CanonicalizeEdges @ Dedup[VectorReplace[edgeList, #]]]& /@ rules
];

toEdgeContractionRuleList[edges_List] := toEdgeContractionRule @@@ UnorderedPairs[edges];

SetAttributes[{ContractedEdge, ContractedVertex}, {Flat, Orderless}];
toEdgeContractionRule[head_[a1_, b1_, c_], head_[a2_, b2_, d_]] :=
  e:(head[a1, b1, c] | head[a2, b2, d]) :> RepPart[e, 3 -> Dedup[ContractedEdge[c, d]]];

greedyContractEdges[edgeList_] := Scope[
  index = Values @ PositionIndex[CanonicalizeEdges @ Take[edgeList, All, 2]];
  index = Select[index, Len[#] >= 2&];
  If[index === {}, Return @ edgeList];
  rules = Flatten[toEdgeContractionRuleList[Part[edgeList, #]]& /@ index];
  greedyContractEdges @ Sort @ CanonicalizeEdges @ Dedup @ VectorReplace[edgeList, rules]
];

(**************************************************************************************************)

PublicFunction[UnContractedGraph]

UnContractedGraph[graph_Graph, opts___Rule] := Scope[
  {vertexList, edgeList} = VertexEdgeList @ graph;
  If[!MemberQ[vertexList, _ContractedVertex] && !MemberQ[edgeList, _[_, _, _ContractedEdge]],
    Return @ ExtendedGraph[graph, opts]];
  ungluingRules = Cases[vertexList, g_ContractedVertex :> (g -> Splice @ Apply[List, g])];
  vertexList = Dedup @ VectorReplace[vertexList, ungluingRules];
  edgeList = Dedup @ MatrixReplace[edgeList, ungluingRules];
  edgeList = Dedup @ VectorReplace[edgeList, $ContractedVertexExpansionRules];
  edgeList = Dedup @ VectorReplace[edgeList, $ContractedEdgeExpansionRules];
  ExtendedGraph[vertexList, edgeList, opts, Sequence @@ Options @ graph]
]

$ContractedVertexExpansionRules = {
  (head_)[a_Splice, b_Splice, tag___] :>
    Splice @ Flatten @ Outer[head[#1, #2, tag]&, F @ a, F @ b, 1],
  (head_)[a_, b_Splice, tag___] :>
    Splice @ Map[head[a, #, tag]&, F @ b],
  (head_)[a_Splice, b_, tag___] :>
    Splice @ Map[head[#, b, tag]&, F @ a]
};

$ContractedEdgeExpansionRules = {
  head_[a_, b_, e_ContractedEdge] :> Splice @ Map[head[a, b, #]&, List @@ e]
}

(**************************************************************************************************)

PublicFunction[ContractVertices]

Options[ContractVertices] = JoinOptions[
  VertexCoordinates -> Auto,
  ExtendedGraph
];

ContractVertices[graph_Graph, glueList_List, userOpts:OptionsPattern[]] := Scope[
  opts = ExtractExtendedGraphOptions @ graph;
  edgeList = EdgeList @ graph;
  glueList //= ToContractionSet;
  glueRules = Flatten[toVertexContractionRule /@ glueList];
  vertexCoordinates = LookupOption[{userOpts}, VertexCoordinates, Auto];
  If[glueRules === {} && vertexCoordinates =!= "Mean",
    Return @ If[{userOpts} === {}, graph, ExtendedGraph[graph, userOpts]]];
  contractedEdgeList = Fold[gluingResult, edgeList, glueRules];
  opts = Sequence @@ DropOptions[opts, {VertexCoordinates, VertexCoordinateRules}];
  vertexList = Sort @ AllUniqueVertices @ contractedEdgeList;
  If[vertexCoordinates === "Mean",
    oldCoords = LookupVertexCoordinates[graph];
    vertexCoordinates = ContractedVertexCoordinateFunction[oldCoords] /@ vertexList;
    userOpts //= SeqReplaceOptions[VertexCoordinates -> vertexCoordinates];
  ];
  ExtendedGraph[
    vertexList, contractedEdgeList,
    userOpts,
    opts
  ]
]

(**************************************************************************************************)

PublicFunction[ContractedGraph]
PublicHead[ContractedVertex, ContractedEdge]

ContractedGraph[vertices_List, edges_List, opts___Rule] :=
  ContractedGraph[ExtendedGraph[vertices, edges], opts];

ContractedGraph[edges_List, opts___Rule] :=
  ContractedGraph[ExtendedGraph[AllUniqueVertices @ edges, edges], opts];

ContractedGraph[graph_Graph, opts___Rule] := Scope[

  opts = {opts};
  unContractedGraph = UnContractedGraph[graph, Sequence @@ DropOptions[opts, EdgeColorFunction]];

  baseVertexList = VertexList @ unContractedGraph;
  baseVertexColors = LookupVertexColors @ unContractedGraph;
  {baseVertexCoordinates, baseEdgeCoordinateLists} = ExtractGraphPrimitiveCoordinates @ unContractedGraph;
  baseEdgeColors = LookupEdgeColors @ unContractedGraph;

  baseCardinals = CardinalList @ unContractedGraph;
  baseCardinalColors = LookupCardinalColors @ unContractedGraph;

  vertexCoordinateFunction = ContractedVertexCoordinateFunction[AssocThread[baseVertexList, baseVertexCoordinates]];
  
  edgeColorFunction = LookupOption[opts, EdgeColorFunction, Auto];
  edgeColorFunction = Which[
    edgeColorFunction === Auto && baseEdgeColors =!= None,
      ContractedEdgeColorFunction[KMap[PartOperator[3], baseEdgeColors]],
    edgeColorFunction === Inherited && baseCardinalColors =!= None,
      (* ^ if we want to just inherit colors from cardinals -- which is not the default behavior *)
      ContractedEdgeColorFunction[baseCardinalColors],
    True,
      None
  ];

  vertexColorFunction = If[baseVertexColors === None, None, ContractedVertexColorFunction[baseVertexColors]];
  cardinalColorFunction = If[baseCardinalColors === <||>, None, ContractedCardinalColorFunction[baseCardinalColors]];

  padding = LookupOption[opts, PlotRangePadding, Scaled[0.05]];
  bounds = ToSquarePlotRange @ CoordinateBounds[baseVertexCoordinates, padding];

  edgeLengthScale = EdgeLengthScale[baseEdgeCoordinateLists, .5] / 4.0;

  ExtendedGraph[graph,
    VertexColorRules -> None, CoordinateTransformFunction -> None,
    VertexCoordinates -> Auto, VertexCoordinateRules -> None,
    VertexLayout -> None,
    CardinalColorRules -> None,
    EdgeColorRules -> None,
    Sequence @@ DropOptions[opts, {PlotRangePadding, EdgeColorFunction}],
    EdgeColorFunction -> edgeColorFunction,
    VertexColorFunction -> vertexColorFunction,
    VertexCoordinateFunction -> vertexCoordinateFunction,
    CardinalColorFunction -> cardinalColorFunction,
    ArrowheadPosition -> 0.52, PlotRange -> bounds,
    ImagePadding -> 0, AspectRatioClipping -> False,
    SelfLoopRadius -> edgeLengthScale, MultiEdgeDistance -> edgeLengthScale/2,
    Frame -> True,
    EdgeThickness -> 2, EdgeStyle -> GrayLevel[0.8, 1],
    ArrowheadShape -> {"FlatArrow", BorderStyle -> Fn[{Darker[#, .3], AbsoluteThickness[0]}]},
    PrologFunction -> ContractedVertexPrologFunction
  ]
];

(**************************************************************************************************)

PublicFunction[ContractedCardinalColorFunction]

ContractedCardinalColorFunction[baseColors_][cardinal_] :=
  If[H[cardinal] === ContractedEdge,
    HumanBlend @ DeleteMissing @ Lookup[baseColors, List @@ cardinal],
    Lookup[baseColors, cardinal, $DarkGray]
  ];

(**************************************************************************************************)

PublicFunction[ContractedEdgeColorFunction]

ContractedEdgeColorFunction[baseColors_][_[_, _, tag_]] :=
  If[H[tag] === ContractedEdge,
    HumanBlend @ DeleteMissing @ Lookup[baseColors, List @@ tag],
    Lookup[baseColors, tag, $DarkGray]
  ];

(**************************************************************************************************)

PublicFunction[ContractedVertexColorFunction]

ContractedVertexColorFunction[baseColors_][vertex_] :=
  If[H[vertex] === ContractedVertex,
    HumanBlend @ DeleteMissing @ Lookup[baseColors, List @@ vertex],
    Lookup[baseColors, vertex, $DarkGray]
  ];

(**************************************************************************************************)

PublicFunction[ContractedVertexCoordinateFunction]

ContractedVertexCoordinateFunction[baseCoords_][vertex_] :=
  If[H[vertex] === ContractedVertex,
    Mean @ DeleteMissing @ Lookup[baseCoords, List @@ vertex],
    Lookup[baseCoords, vertex]
  ];
  
(**************************************************************************************************)

PublicFunction[ContractedVertexPrologFunction]

ContractedVertexPrologFunction[graph_] := Scope[
  baseCoordFunc = GraphAnnotationData[VertexCoordinateFunction];
  imageWidth = F @ LookupImageSize[graph];
  small = imageWidth < 100;
  Style[
    Map[ContractedVertexPrimitives, VertexList @ graph],
    AbsoluteThickness[If[small, 1, 2]], AbsoluteDashing[If[small, {1, 2}, {2,2}]],
    AbsolutePointSize[If[small, 3, 4]], GrayLevel[0.5]
  ]
];

ContractedVertexPrimitives[_] := Nothing;

ContractedVertexPrimitives[vertex_ContractedVertex] := Scope[
  coords = GraphVertexData[vertex, "Coordinates"];
  gluedCoords = baseCoordFunc /@ (List @@ vertex);
  {Point[gluedCoords], Line[{#, coords}& /@ gluedCoords]}
  (* Line[{#, coords}& /@ gluedCoords] *)
];
