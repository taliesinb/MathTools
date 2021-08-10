Unprotect[PathGraph];
(* fix a weird oversight in the design of PathGraph *)
PathGraph[n_Integer, opts___] := PathGraph[Range[n], opts];


PackageExport["DirectedCycle"]
PackageExport["UndirectedCycle"]

cyclicPairs[first_, vertices___] := Partition[{first, vertices, first}, 2, 1];

DirectedCycle[vertices___] := Splice[DirectedEdge @@@ cyclicPairs[vertices]];
UndirectedCycle[vertices___] := Splice[UndirectedEdge @@@ cyclicPairs[vertices]];


PackageExport["DirectedPath"]
PackageExport["UndirectedPath"]

DirectedPath[vertices___] := Splice[DirectedEdge @@@ Partition[List @ vertices, 2, 1]];
UndirectedPath[vertices___] := Splice[UndirectedEdge @@@ Partition[List @ vertices, 2, 1]];


PackageExport["Clique"]

Clique[vertices___] := Splice[UndirectedEdge @@@ Subsets[{vertices}, {2}]];


(**************************************************************************************************)

PackageExport["EquivalenceGraph"]

EquivalenceGraph[graph_, f_, newOpts___Rule] := Scope[
  vertices = VertexList @ graph;
  vrange = VertexRange @ graph;
  classes = Gather[vrange, f @@ Part[vertices, {#1, #2}]&];
  igraph = IndexGraph[graph];
  contracted = VertexContract[igraph, classes];
  {ivertices, iedges} = VertexEdgeList @ contracted;
  edges = MapAt[Part[vertices, #]&, iedges, {All, 1;;2}];
  vertices = Part[vertices, ivertices];
  opts = Options @ graph;
  ExtendedGraph[vertices, edges, VertexAnnotations -> None, GraphOrigin -> None, newOpts, Sequence @@ opts]
];

(**************************************************************************************************)

PackageExport["GraphAdd"]

SetUsage @ "
GraphAdd[graph$, vertices$, edges$] adds additional vertices and edges to graph$.
* Additional vertices will use vertex coordinates that are given by the mean of their neighbors.
"

GraphAdd[graph_, newVertices_, newEdges_] := Scope[
  CheckIsGraph[1];
  options = Options @ graph;
  {vertices, edges} = VertexEdgeList @ graph;
  newVertices //= ToList;
  newEdges //= ToList;
  newEdges //= Map[toProperEdges];
  If[ContainsQ[newEdges, $Failed], ReturnFailed[]];
  If[IntersectingQ[vertices, newVertices],
    renamedVertices = AdditionalVertex /@ newVertices;
    renamingRules = RuleThread[newVertices, renamedVertices];
    newVertices = renamedVertices;
    newEdges //= ReplaceAll[renamingRules];
  ];
  oldVertexCount = Length @ vertices;
  newVertexCount = Length @ newVertices;
  newGraph = Graph[
    Join[vertices, newVertices],
    Join[edges, newEdges],
    Sequence @@ DeleteOptions[options, VertexCoordinates]
  ];
  If[!GraphQ[newGraph], ReturnFailed[]];
  newGraph //= DeleteVertexAnnotations;
  vertexCoordinates = Lookup[options, VertexCoordinates];
  If[CoordinateMatrixQ[vertexCoordinates],
    vertexCoordinates = Join[vertexCoordinates, ConstantArray[0., {newVertexCount, InnerDimension @ vertexCoordinates}]];
    newVertexIndices = Range[newVertexCount] + oldVertexCount;
    adjTable = Drop[VertexAdjacencyTable @ newGraph, oldVertexCount];
    Do[
      Part[vertexCoordinates, newVertexIndices] = Map[Mean @ Part[vertexCoordinates, #]&, adjTable];
    ,
      {10}
    ];
    newGraph = Graph[newGraph, VertexCoordinates -> vertexCoordinates];
  ];
  newGraph
];

GraphAdd::badedge = "`` is not a valid edge."

toProperEdges = Case[
  a_ -> b_ := DirectedEdge[a, b];
  a_ <-> b_ := UndirectedEdge[a, b];
  d_DirectedEdge := d;
  u_UndirectedEdge := u;
  other_ := (Message[GraphAdd::badedge, other]; $Failed)
];


PackageExport["AdditionalVertex"]

(**************************************************************************************************)

PackageExport["ExtendedSubgraph"]

ExtendedSubgraph[oldGraph_, newVertices_, newEdges_] := Scope[
  options = Options[oldGraph];
  annotations = ExtendedGraphAnnotations[oldGraph];
  vertexCoords = Lookup[options, VertexCoordinates, Automatic];
  oldVertices = VertexList[oldGraph];
  If[newVertices === All,
    newVertexIndices = Range @ Length @ oldVertices;
    newVertices = oldVertices;
  ,
    newVertexIndices = Map[IndexOf[oldVertices, #]&, newVertices];
    newVertexOrdering = Ordering[newVertexIndices];
    newVertices = Part[newVertices, newVertexOrdering];
  ];
  {graphOrigin, vertexAnnotations} = LookupAnnotation[oldGraph, {GraphOrigin, VertexAnnotations}, None];
  If[!MemberQ[newVertices, graphOrigin],
    annotations //= ReplaceOptions[GraphOrigin -> None]];
  sortedNewVertexIndices = Sort @ newVertexIndices;
  If[ListQ[vertexCoords],
    vertexCoords = Part[vertexCoords, sortedNewVertexIndices];
    options //= ReplaceOptions[VertexCoordinates -> vertexCoords];
  ];
  If[AssociationQ[vertexAnnotations],
    vertexAnnotations //= Map[Part[#, sortedNewVertexIndices]&];
    annotations //= ReplaceOptions[VertexAnnotations -> vertexAnnotations];
  ];
  If[newEdges === Automatic,
    newEdges = Select[EdgeList @ oldGraph, MemberQ[newVertices, Part[#, 1]] && MemberQ[newVertices, Part[#, 2]]&]
  ];
  graph = Graph[newVertices, newEdges, options];
  Annotate[graph, annotations]
];

(**************************************************************************************************)

PackageExport["VertexSelect"]

SetUsage @ "
VertexSelect[graph$, predicate$] gives the subgraph of the vertices sastisfying predicate$.
The predicate can be one of the following:
| f$ | function applied to vertex name |
| v$ -> f$ | function applied to value of vertex value v$ |
| v$ -> n$ | part n$ of a list |
| v$ -> 'key$' | key 'key$' of an assocation |
| v$ -> f$ -> g$ | chained function application |
| {v$1, v$2, $$} -> f$ | f$[v$1, v$2, $$] for each vertex |
The vertex values v$i are values defined for each vertex and can be any of the following:
| 'Name' | vertex name |
| 'Index' | vertex integer index |
| 'Coordinates' | graphical coordinates |
| 'Distance' | distance from %GraphOrigin |
| {'Distance', v$} | distance from vertex v$ |
| 'key$' | vertex annotation 'key$' |
| {v$1, v$2, $$} | a combination of the above |
For graphs created with %LatticeGraph and %LatticeQuiver, the following annotations are available:
| 'AbstractCoordinates' | vector of abstract coordinates |
| 'GeneratingVertex' | corresponding vertex in the fundamental quiver |
| 'Norm' | norm of the abstract coordinates |
"

VertexSelect::notvertex = "`` is not a valid vertex of the graph."
VertexSelect::notboolres = "Predicate did not return True or False for input: ``."
VertexSelect::badgraphannokey = "The requested annotation `` is not present in the graph. Present annotations are: ``."

VertexSelect[graph_, f_] := Scope @ Catch[

  vertices = VertexList @ graph;
  $vertexAnnotations = LookupExtendedOption[graph, VertexAnnotations];
  $vertexCoordinates := $vertexCoordinates = First @ ExtractGraphPrimitiveCoordinates[graph];

  GraphScope[graph,
    bools = toVertexResults[f] /. Indeterminate -> True;
    If[!VectorQ[bools, BooleanQ], ReturnFailed[]];
    newVertices = Pick[$VertexList, bools];
  ];

  ExtendedSubgraph[graph, newVertices, Automatic]
,
  VertexSelect
];

toVertexResults = Case[
  data_List -> f_           := MapThread[checkBool @ toFunc @ f, toVertexData @ data];
  data_ -> f_              := Map[checkBool @ toFunc @ f, toVertexData @ data];
  f_                        := Map[checkBool @ f, $VertexList];
];

toVertexData = Case[
  "Name"                    := $VertexList;
  "Index"                   := Range @ $VertexCount;

  (* todo, make the distance work on regions as well *)
  "Coordinates"             := $vertexCoordinates;
  "Distance"                := %[{"Distance", GraphOrigin}];
  {"Distance", v_}          := MetricDistance[$MetricGraphCache, getVertexIndex @ v];
  key_String                := getAnnoValue[$vertexAnnotations, key];

  list_List                 := Map[%, list];
];

toFunc = Case[
  p_Integer := PartOperator[p];
  p_String := PartOperator[p];
  f_ := checkIndet @ f;
  f_ -> g_ := RightComposition[toFunc @ f, toFunc @ g]
];

failSelect[msg_, args___] := (
  Message[MessageName[VertexSelect, msg], args];
  Throw[$Failed, VertexSelect]
);

checkBool[f_][args___] := Catch[
  Replace[
    Check[f[args], $Failed],
    Except[True|False|Indeterminate] :> failSelect["notboolres", SequenceForm[args]]
  ],
  indet
];

checkIndet[f_][args___] :=
  Replace[f[args], Indeterminate :> Throw[Indeterminate, indet]];

getVertexIndex[GraphOrigin] := getVertexIndex @ $GraphOrigin;
getVertexIndex[v_] := Lookup[$VertexIndex, v, failSelect["notvertex", v]];
getAnnoValue[annos_, key_] := Lookup[annos, key, failSelect["badgraphannokey", key, commaString @ Keys @ annos]];

(**************************************************************************************************)

PackageExport["GraphVertexQuotient"]

GraphVertexQuotient[graph_, equivFn_, userOpts___Rule] := Scope[
  setupGraphVertexData[graph];
  {vertexList, edgeList} = VertexEdgeList @ graph;
  opts = ExtractExtendedGraphOptions[graph];
  quotientVertexIndices = EquivalenceClassIndices[vertexList, equivFn];
  quotientVertexLabels = EquivalenceClassLabels[quotientVertexIndices];
  quotientVertexCounts = Length /@ quotientVertexIndices;
  edgePairs = EdgePairs @ graph;
  quotientEdgePairs = edgePairs /. i_Integer :> Part[quotientVertexLabels, i];
  quotientEdgesIndex = PositionIndex[quotientEdgePairs];
  {quotientEdges, quotientEdgesIndices} = KeysValues @ quotientEdgesIndex;
  quotientEdgesCounts = Length /@ quotientEdgesIndices;
  quotientVertices = Range @ Length @ quotientVertexIndices;
  vertexAnnos = <|
    "EquivalenceClassIndices" -> quotientVertexIndices,
    "EquivalenceClassSizes" -> quotientVertexCounts
  |>;
  edgeAnnos = <|
    "EquivalenceClassIndices" -> quotientEdgesIndices,
    "EquivalenceClassSizes" -> quotientEdgesCounts
  |>;
  opts //= ReplaceOptions[{VertexAnnotations -> vertexAnnos, EdgeAnnotations -> edgeAnnos}];
  ExtendedGraph[
    quotientVertices,
    DirectedEdge @@@ quotientEdges, 
    Sequence @@ userOpts,
    Sequence @@ opts
  ]
];

(**************************************************************************************************)

PackageExport["QuiverContractionOrder"]

Options[QuiverContractionOrder] = JoinOptions[
  CombineMultiedges -> False,
  ExtendedGraph  
]

QuiverContractionOrder[quiver_, gluedGraphOptions_List, userOpts:OptionsPattern[]] := Scope[

  vertexList = VertexList @ quiver;
  vertexCount = Length @ vertexList;
  partitionGraph = RangePartitionGraph @ vertexCount;
  partitionGraph //= TransitiveClosureGraph;
  partitions = VertexList @ partitionGraph;
  outTable = TagVertexOutTable @ quiver;
  validPartitions = Select[partitions, validQuiverPartitionQ[outTable, #]&];
  contractionGraph = IndexGraph @ TransitiveReductionGraph @ Subgraph[partitionGraph, validPartitions];
  validPartitions = ExtractIndices[vertexList, validPartitions];

  UnpackOptions[combineMultiedges];
  innerSize = LookupOption[gluedGraphOptions, ImageSize, 50];
  gluedGraphOptions = Sequence @@ gluedGraphOptions;

  postFn = If[combineMultiedges, CombineMultiedges, Identity];
  baseGraph = GluedGraph[quiver, gluedGraphOptions];
  replacements = RuleThread[Range @ vertexCount, vertexList];
  gluedGraphs = postFn[GlueVertices[baseGraph, #]]& /@ validPartitions;

  ExtendedGraph[
    contractionGraph, 
    FilterOptions @ userOpts, 
    GraphLayout -> "CenteredTree",
    VertexAnnotations -> <|"GluedGraph" -> gluedGraphs, "Partitions" -> validPartitions|>,
    ArrowheadShape -> None, VertexSize -> innerSize, 
    VertexLabels -> Tooltip["Partitions"],
    VertexShapeFunction -> "GluedGraph"
  ]
];

validQuiverPartitionQ[outTable_, partition_] := Scope[
  rewrites = RuleThread[Alternatives @@@ partition, -Range[Length @ partition]];
  collapsedTable = outTable /. rewrites;
  Scan[
    subTable |-> Scan[checkForConflicts, ExtractIndices[subTable, partition]],
    collapsedTable
  ];
  True
];

checkForConflicts[list_] := 
  If[CountDistinct[DeleteDuplicates @ DeleteNone @ list] > 1, Return[False, Block]];

(**************************************************************************************************)

PackageExport["GraphContractionList"]

GraphContractionList[graph_, opts_List] := Scope[
  orderGraph = GraphContractionOrder[graph, opts];
  LookupVertexAnnotations[orderGraph, "GluedGraph"]
]

(**************************************************************************************************)

PackageExport["GraphContractionOrder"]

Options[GraphContractionOrder] = Options[QuiverContractionOrder];

GraphContractionOrder[graph_, gluedGraphOptions_List, userOpts:OptionsPattern[]] := Scope[
  
  If[!EdgeTaggedGraphQ[graph], graph //= IndexEdgeTaggedGraph];

  UnpackOptions[combineMultiedges];
  innerSize = LookupOption[gluedGraphOptions, ImageSize, 50];
  gluedGraphOptions = Sequence @@ gluedGraphOptions;
  
  initialGraph = GluedGraph[graph, gluedGraphOptions, ImageSize -> {innerSize, innerSize}];
  innerOpts = Sequence @@ ExtractExtendedGraphOptions @ initialGraph;

  edgeList = CanonicalizeEdges @ EdgeList @ graph;
  isDirected = DirectedGraphQ @ graph;
  sorter = If[isDirected, Identity, Map[Sort]];
  {vlist, ielist} = MultiwaySystem[graphGluingSuccessors, {edgeList}, {"VertexList", "IndexEdgeList"}];
  
  irange = Range @ Length @ vlist;
      
  postFn = If[combineMultiedges, CombineMultiedges, Identity];

  graphFn = edges |-> ExtendedGraph[
    AllUniqueVertices @ edges,
    edges, 
    innerOpts
  ];
  gluedGraphs = Map[graphFn /* postFn, vlist];
  
  ExtendedGraph[
    Range @ Length @ vlist, ielist, FilterOptions @ userOpts, 
    GraphLayout -> "CenteredTree",
    VertexAnnotations -> <|"GluedGraph" -> gluedGraphs|>,
    ArrowheadShape -> None, VertexSize -> innerSize, VertexShapeFunction -> "GluedGraph"
  ]
];

graphGluingSuccessors[edgeList_] := Join[
  edgeGluingSuccessors @ edgeList,
  vertexGluingSuccessors @ edgeList
];

vertexGluingSuccessors[edgeList_] := Scope[
  vertices = AllUniqueVertices[edgeList];
  rules = toVertexGluingRule /@ Subsets[vertices, {2}];
  gluingResultsList[edgeList, rules]
];

$flattenGlue = e_GluedEdge | e_GluedVertex :> DeleteDuplicates[e];

toVertexGluingRule[{v_}] := Nothing;

toVertexGluingRule[verts_List] := With[
  {alts = Alternatives @@ verts, glued = GluedVertex @@ verts},
  {
    head_[alts, alts, c___] :> head[glued, glued, c],
    head_[alts, b_, c___] :> head[glued, b, c],
    head_[a_, alts, c___] :> head[a, glued, c]
  }
];

gluingResult[edgeList_, rules_] :=
  Sort @ CanonicalizeEdges @ DeleteDuplicates[Replace[edgeList, rules, {1}] /. $flattenGlue];

gluingResultsList[edgeList_, rulesList_] := Map[
  gluingResult[edgeList, #]&,
  rulesList
];

edgeGluingSuccessors[edgeList_] := Scope[
  index = Values @ PositionIndex[Take[edgeList, All, 2]];
  index = Select[index, Length[#] >= 2&];
  rules = Flatten[toEdgeGluingRuleList[Part[edgeList, #]]& /@ index];
  Sort[CanonicalizeEdges @ DeleteDuplicates[Replace[edgeList, #, {1}]]]& /@ rules
];

toEdgeGluingRuleList[edges_List] := toEdgeGluingRule @@@ Subsets[edges, {2}]

SetAttributes[{GluedEdge, GluedVertex}, {Flat, Orderless}];
toEdgeGluingRule[head_[a1_, b1_, c_], head_[a2_, b2_, d_]] :=
  e:(head[a1, b1, c] | head[a2, b2, d]) :> ReplacePart[e, 3 -> GluedEdge[c, d]];


(**************************************************************************************************)

PackageExport["UngluedGraph"]

UngluedGraph[graph_Graph, opts___Rule] := Scope[
  {vertexList, edgeList} = VertexEdgeList @ graph;
  If[!MemberQ[vertexList, _GluedVertex] && !MemberQ[edgeList, _[_, _, _GluedEdge]], 
    Return @ ExtendedGraph[graph, opts]];
  ungluingRules = Cases[vertexList, g_GluedVertex :> (g -> Splice @ Apply[List, g])];
  vertexList = DeleteDuplicates @ Replace[vertexList, ungluingRules, {1}];
  edgeList = DeleteDuplicates @ Replace[edgeList, ungluingRules, {2}];
  edgeList = DeleteDuplicates @ Replace[edgeList, $gluedVertexExpansionRules, {1}];
  edgeList = DeleteDuplicates @ Replace[edgeList, $gluedEdgeExpansionRules, {1}];
  ExtendedGraph[vertexList, edgeList, opts, Sequence @@ Options @ graph]
]

$gluedVertexExpansionRules = {
  (head_)[a_Splice, b_Splice, tag___] :>
    Splice @ Flatten @ Outer[head[#1, #2, tag]&, First @ a, First @ b, 1],
  (head_)[a_, b_Splice, tag___] :>
    Splice @ Map[head[a, #, tag]&, First @ b],
  (head_)[a_Splice, b_, tag___] :>
    Splice @ Map[head[#, b, tag]&, First @ a]
};

$gluedEdgeExpansionRules = {
  head_[a_, b_, e_GluedEdge] :> Splice @ Map[head[a, b, #]&, List @@ e]
}

(**************************************************************************************************)

PackageExport["GlueVertices"]

GlueVertices[graph_Graph, glueList_List] := Scope[
  opts = ExtractExtendedGraphOptions @ graph;
  edgeList = EdgeList @ graph;
  glueRules = Flatten[toVertexGluingRule /@ glueList];
  If[glueRules === {}, Return @ graph];
  gluedEdgeList = Fold[gluingResult, edgeList, glueRules];
  ExtendedGraph[
    Sort @ AllUniqueVertices @ gluedEdgeList, gluedEdgeList, 
    Sequence @@ opts
  ]
]

(**************************************************************************************************)

PackageExport["GluedGraph"]
PackageExport["GluedVertex"]
PackageExport["GluedEdge"]

GluedGraph[vertices_List, edges_List, opts___Rule] :=
  GluedGraph[ExtendedGraph[vertices, edges], opts];

GluedGraph[edges_List, opts___Rule] :=
  GluedGraph[ExtendedGraph[AllUniqueVertices @ edges, edges], opts];

GluedGraph[graph_Graph, opts___Rule] := Scope[

  ungluedGraph = UngluedGraph[graph, opts];
  
  baseVertexList = VertexList @ ungluedGraph;
  baseVertexColors = LookupVertexColors @ ungluedGraph;
  {baseVertexCoordinates, baseEdgeCoordinateLists} = ExtractGraphPrimitiveCoordinates @ ungluedGraph;
  baseEdgeColors = LookupEdgeColors @ ungluedGraph;

  baseCardinals = CardinalList @ ungluedGraph;
  baseCardinalColors = LookupCardinalColors @ ungluedGraph;

  vertexCoordinateFunction = GluedVertexCoordinateFunction[AssociationThread[baseVertexList, baseVertexCoordinates]];
  edgeColorFunction = Which[
    baseEdgeColors =!= None, GluedEdgeColorFunction[KeyMap[PartOperator[3], baseEdgeColors]],
    baseCardinalColors =!= None, GluedEdgeColorFunction[baseCardinalColors],
    True, None
  ];
  vertexColorFunction = If[baseVertexColors === None, None, GluedVertexColorFunction[baseVertexColors]];
  cardinalColorFunction = If[baseCardinalColors === <||>, None, GluedCardinalColorFunction[baseCardinalColors]];

  opts = {opts};
  padding = LookupOption[opts, PlotRangePadding, Scaled[0.05]];
  bounds = ToSquarePlotRange @ CoordinateBounds[baseVertexCoordinates, padding];

  edgeLengthScale = EdgeLengthScale[baseEdgeCoordinateLists, .5] / 4.0;

  ExtendedGraph[graph,
    VertexColorRules -> None, CoordinateTransformFunction -> None,
    VertexCoordinates -> Automatic, VertexCoordinateRules -> None,
    CardinalColorRules -> None,
    EdgeColorRules -> None,
    Sequence @@ DeleteOptions[opts, PlotRangePadding],
    EdgeColorFunction -> edgeColorFunction,
    VertexColorFunction -> vertexColorFunction,
    VertexCoordinateFunction -> vertexCoordinateFunction,
    CardinalColorFunction -> cardinalColorFunction,
    ArrowheadPosition -> 0.52, PlotRange -> bounds,
    ImagePadding -> 0, AspectRatioClipping -> False,
    SelfLoopRadius -> edgeLengthScale, MultiEdgeDistance -> edgeLengthScale/2,
    Frame -> True,
    EdgeThickness -> 2, EdgeStyle -> GrayLevel[0.8, 1],
    ArrowheadShape -> {"FlatArrow", BorderStyle -> Function[{Darker[#, .3], AbsoluteThickness[0]}]},
    PrologFunction -> GluedVertexPrologFunction
  ]
];

(**************************************************************************************************)

PackageExport["GluedCardinalColorFunction"]

GluedCardinalColorFunction[baseColors_][cardinal_] := 
  If[Head[cardinal] === GluedEdge,
    HumanBlend @ DeleteMissing @ Lookup[baseColors, List @@ cardinal],
    Lookup[baseColors, cardinal, $DarkGray]
  ];

(**************************************************************************************************)

PackageExport["GluedEdgeColorFunction"]

GluedEdgeColorFunction[baseColors_][_[_, _, tag_]] :=
  If[Head[tag] === GluedEdge,
    HumanBlend @ DeleteMissing @ Lookup[baseColors, List @@ tag],
    Lookup[baseColors, tag, $DarkGray]
  ];

(**************************************************************************************************)

PackageExport["GluedVertexColorFunction"]

GluedVertexColorFunction[baseColors_][vertex_] :=
  If[Head[vertex] === GluedVertex,
    HumanBlend @ DeleteMissing @ Lookup[baseColors, List @@ vertex],
    Lookup[baseColors, vertex, $DarkGray]
  ];

(**************************************************************************************************)

PackageExport["GluedVertexCoordinateFunction"]

GluedVertexCoordinateFunction[baseCoords_][vertex_] := 
  If[Head[vertex] === GluedVertex,
    Mean @ DeleteMissing @ Lookup[baseCoords, List @@ vertex],
    Lookup[baseCoords, vertex]
  ];
  
(**************************************************************************************************)

PackageExport["GluedVertexPrologFunction"]

GluedVertexPrologFunction[graph_] := Scope[
  baseCoordFunc = GraphAnnotationData[VertexCoordinateFunction];
  Style[
    Map[gluedVertexPrimitives, VertexList @ graph],
    AbsoluteThickness[2], AbsoluteDashing[{2,2}],
    AbsolutePointSize[4], GrayLevel[0.5]
  ]
];

gluedVertexPrimitives[_] := Nothing;

gluedVertexPrimitives[vertex_GluedVertex] := Scope[
  coords = GraphVertexData[vertex, "Coordinates"];
  gluedCoords = baseCoordFunc /@ (List @@ vertex);
  {Point[gluedCoords], Line[{#, coords}& /@ gluedCoords]}
  (* Line[{#, coords}& /@ gluedCoords] *)
];