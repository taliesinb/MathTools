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

PackageExport["QuiverContractionList"]

QuiverContractionList[graph_, opts_List] := Scope[
  orderGraph = QuiverContractionLattice[graph, opts];
  LookupVertexAnnotations[orderGraph, "ContractedGraph"]
]

(**************************************************************************************************)

PackageExport["QuiverContractionLattice"]

Options[QuiverContractionLattice] = JoinOptions[
  CombineMultiedges -> False,
  "SpacelikeOnly" -> False,
  "AllowCyclic" -> True,
  "GraphStyle" -> "ContractedGraph",
  "AllowGraphContractions" -> False,
  ExtendedGraph
]

QuiverContractionLattice[quiver_, contractedGraphOptions_List, userOpts:OptionsPattern[]] := Scope[

  vertexList = VertexList @ quiver;
  outTable = TagVertexOutTable @ quiver;
  vertexCount = Length @ vertexList;

  UnpackOptions[combineMultiedges, spacelikeOnly, allowCyclic, graphStyle, allowGraphContractions];

  permittedMatrix = If[spacelikeOnly, SpacelikeVertexRelationMatrix @ quiver, None];
  quiverForCycleDetection = If[allowCyclic, None, ToIndexGraph @ quiver];

  key = {vertexList, outTable, allowGraphContractions, permittedMatrix, quiverForCycleDetection};
  {contractionGraph, validPartitions, isQuiverContraction} = CacheTo[$validPartitionCache, key, computeValidPartitions[key]];

  plotRangePadding = Lookup[contractedGraphOptions, PlotRangePadding, Inherited];
  plotRange = Lookup[contractedGraphOptions, PlotRange, GraphicsPlotRange[quiver, PlotRangePadding -> plotRangePadding]];
  PrependTo[contractedGraphOptions, PlotRange -> plotRange];

  innerSize = LookupOption[contractedGraphOptions, ImageSize, 50];
  contractedGraphOptions = Sequence @@ contractedGraphOptions;

  vertexAnnotations = <||>;
  postFn = If[combineMultiedges, CombineMultiedges, Identity];
  graphFn = If[graphStyle === "ContractedGraph", ContractedGraph, ExtendedGraph];
  baseGraph = graphFn[quiver, contractedGraphOptions];
  
  contractedGraphs = postFn[ContractVertices[baseGraph, #]]& /@ validPartitions;
  vertexAnnotations["ContractionSet"] = validPartitions;
  vertexAnnotations["ContractedGraph"] = contractedGraphs;
  vertexAnnotations["QuiverContractionQ"] = isQuiverContraction;

  vertexCoordsAssoc = LookupVertexCoordinates @ quiver;
  vertexCoords = Values @ vertexCoordsAssoc;
  vertexCoordsBounds = CoordinateBounds[vertexCoords, Scaled[0.1]];

  iconOptions = Sequence[PlotRange -> vertexCoordsBounds, ImagePadding -> 2, PlotRangePadding -> Scaled[0.2]];
  vertexAnnotations["ColorIcon"] = VertexPartitionGraphics[quiver, validPartitions, iconOptions, "Style" -> "Color"];
  vertexAnnotations["EdgeIcon"] = VertexPartitionGraphics[quiver, validPartitions, iconOptions,"Style" -> "Edges"];

  ExtendedGraph[
    contractionGraph,
    FilterOptions @ userOpts,
    GraphLayout -> "CenteredTree",
    VertexAnnotations -> vertexAnnotations,
    ArrowheadShape -> None, VertexSize -> Max[innerSize],
    VertexTooltips -> "ContractionSet",
    VertexShapeFunction -> "ContractedGraph"
  ]
];

$validPartitionCache = <||>;
computeValidPartitions[{vertexList_, outTable_, allowGraphContractions_, permittedMatrix_, quiver_}] := Scope[

  path = CacheFilePath["QuiverContractions", vertexList, outTable, permittedMatrix];
  Hold @ If[FileExistsQ[path], Return @ Import @ path];

  vertexCount = Length @ vertexList;
  partitionGraph = RangePartitionGraph @ vertexCount;
  partitionGraph //= TransitiveClosureGraph;
  partitions = VertexList @ partitionGraph;
  
  If[permittedMatrix =!= None,
    $permittedMatrix = permittedMatrix;
    partitions //= Select[permittedPartitionQ];
  ];
  If[quiver =!= None,
    partitions //= Select[AcyclicGraphQ[SimpleGraph @ ContractVertices[quiver, #]]&];
  ];

  isQuiverContraction = validQuiverPartitionQ[outTable, #]& /@ partitions;
  If[!allowGraphContractions,
    partitions = Pick[partitions, isQuiverContraction];
    isQuiverContraction = ConstantArray[True, Length @ partitions];
  ];

  contractionGraph = IndexGraph @ TransitiveReductionGraph @ Subgraph[partitionGraph, partitions];
  partitions = ExtractIndices[vertexList, partitions];

  EnsureExport[path, {contractionGraph, partitions, isQuiverContraction}]
];

permittedPartitionQ[partition_] := AllTrue[partition, permittedTermQ];

permittedTermQ[{_}] := True;
permittedTermQ[{a_, b_}] := Part[$permittedMatrix, a, b] == 1;
permittedTermQ[set_] := AllTrue[Tuples[set, {2}], Extract[$permittedMatrix, #] == 1&];

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

PackageExport["SpacelikeVertexRelationMatrix"]

SpacelikeVertexRelationMatrix[graph_Graph] := Scope[
  dist = GraphDistanceMatrix[graph] /. {_Integer -> 0, Infinity -> 1};
  dist = MapThread[Min, {dist, Transpose @ dist}, 2];
  ToPacked @ dist
];

(**************************************************************************************************)

PackageExport["QuiverContractionLatticeFast"]

Options[QuiverContractionLatticeFast] = JoinOptions[
  "ComputeIcons" -> True,
  "IconOptions" -> {PlotRangePadding -> Scaled[0.2], ImagePadding -> 2},
  "IconStyle" -> "Color",
  "IconSize" -> 30,
  "SkipIndices" -> {},
  CombineMultiedges -> True,
  ExtendedGraph
];

QuiverContractionLatticeFast[quiver_, opts:OptionsPattern[]] := Scope[

  UnpackOptions[computeIcons, iconStyle, skipIndices];

  quiverVertices = VertexList @ quiver;
  
  contractionSets = MinimalContractionSets[quiver];
  contractionSets = Delete[contractionSets, List /@ skipIndices];
  partitionLattice = PartitionLattice[contractionSets];

  latticeVertices = VertexList @ partitionLattice;
  supports = Part[latticeVertices, All, 1];
  partitions = Part[latticeVertices, All, 2];
  completePartitions = CompleteContractionSet[quiverVertices] /@ partitions;
  
  annos = <||>;
  annos["Support"] = supports;
  annos["Partition"] = completePartitions;

  If[computeIcons,
    UnpackOptions[vertexSize, iconOptions, iconSize];
    iconOptions = Sequence @@ iconOptions;
    contractionGraphs = createContractionGraph[quiver, vertexSize] /@ completePartitions;
    AssociateTo[annos, {
      "ColorIcon" -> VertexPartitionGraphics[quiver, completePartitions, iconOptions, "Style" -> "Color"],
      "EdgeIcon" -> VertexPartitionGraphics[quiver, completePartitions, iconOptions, "Style" -> "Edges"],
      "GraphIcon" -> contractionGraphs
    }];
  ];
  
  If[StringQ[iconStyle] && computeIcons,
    extraOpts = Sequence[
      VertexShapeFunction -> (iconStyle <> "Icon"),
      VertexSize -> iconSize, ImagePadding -> iconSize,
      ImageSize -> "ShortestEdge" -> iconSize * 1.75,
      EdgeSetback -> PointSize[iconSize/2]
    ];
  ,
    extraOpts = Sequence[];
  ];

  ExtendedGraph[partitionLattice,
    VertexAnnotations -> annos,
    FilterOptions @ opts, extraOpts
  ]
];

createContractionGraph[quiver_, vertexSize_][partition_] :=
  CombineMultiedges @ ContractVertices[
    quiver, partition,
    VertexCoordinates -> "Mean",
    GraphTheme -> "ContractionGraphIcon",
    ImageSize -> ({1,1} * vertexSize + 15),
    PlotRange -> "Square"
  ];

$GraphThemeData["ContractionGraphIcon"] = {
  Frame -> True, Background -> White,
  PlotRange -> "Square",
  ArrowheadShape -> {"Line", TwoWayStyle -> "CrossLine"},
  VertexSize -> 8, VertexCoordinates -> "Mean", SelfLoopRadius -> 0.5,
  ImagePadding -> 10
};

(**************************************************************************************************)

PackageExport["DisjointUnionLattice"]

Options[DisjointUnionLattice] = Options @ Graph;

DisjointUnionLattice[generators:{__List}, opts:OptionsPattern[]] := Scope[
  graph = MultiwaySystem[
    disjointUnionCayleyFunction[generators],
    {{}},
    "Graph"
  ];
  ExtendedGraph[graph, opts]
];

disjointUnionCayleyFunction[generators_][subsets_] :=
  MapIndexed[{gen, index} |-> If[
    AllTrue[subsets, DisjointQ[gen]],
    Labeled[Sort @ Append[subsets, gen], First @ index],
    Nothing
  ], generators];

(**************************************************************************************************)

PackageExport["ContractionSetUnionLattice"]

Options[ContractionSetUnionLattice] = Options @ Graph;

ContractionSetUnionLattice[contractionSets:{__List}, opts:OptionsPattern[]] := Scope[
  contractionSets //= Map @ SortContractionSet;
  {vertexList, edgeList} = MultiwaySystem[
    contractionSetUnionCayleyFunction[contractionSets],
    {{}},
    {"VertexList", "EdgeList"}
  ];
  edgeList = Values @ GroupBy[edgeList, TakeOperator[2], combineTags];
  TransitiveReductionGraph @ ExtendedGraph[vertexList, edgeList, opts, VertexLayout -> TreeLayout[]]
];

combineTags[{edge_}] := edge;
combineTags[edges_] := Append[Part[edges, 1, 1;;2], CardinalSet[Part[edges, All, 3]]];

contractionSetUnionCayleyFunction[generators_][contractionSet_] := Scope[
  MapIndexed[
    {gen, index} |-> If[SubsetQ[contractionSet, gen],
      Nothing,
      res = ContractionSetUnion[contractionSet, gen];
      If[res === contractionSet, Nothing, Labeled[ContractionSetUnion[contractionSet, gen], First @ index]]
    ],
    generators
  ]
];

(**************************************************************************************************)

PackageExport["ContractionSetUnion"]

(* DisjointSet doesn not have the right API yet to make this fast *)
ContractionSetUnion[contractionSet1_, contractionSet2_] :=
  Sort @ Fold[ContractionSetAppend, contractionSet1, contractionSet2];

(**************************************************************************************************)

PackageExport["ContractionSetAppend"]

ContractionSetAppend[contractionSet_, term_] := Scope[
  jointIndices = SelectIndices[contractionSet, IntersectingQ[term]];
  Sort @ If[jointIndices === {},
    Append[contractionSet, term],
    Append[
      Delete[contractionSet, List /@ jointIndices],
      Union @@ Append[Part[contractionSet, jointIndices], term]
    ]
  ]
];


(**************************************************************************************************)

PackageExport["ToContractionSet"]

ToContractionSet[term:{___Integer}] := List @ Sort @ term;

ToContractionSet[contractionSet_] :=
  SortContractionSet @ If[DuplicateFreeQ[contractionSet, IntersectingQ],
    contractionSet,
    Fold[ContractionSetAppend, {}, contractionSet]
  ];

(**************************************************************************************************)

PackageExport["SortContractionSet"]

SortContractionSet[contractionSet_] :=
  LexicographicSort @ Map[Sort, contractionSet];

SortContractionSet[contractionSet_, index_] :=
  LexicographicSort @ Map[OrderSort[index], contractionSet];

(**************************************************************************************************)

PackageExport["CompleteContractionSet"]

CompleteContractionSet[vertices_][contractionSet_] := Scope[
  remaining = Complement[vertices, Union @@ contractionSet];
  SortContractionSet @ Join[contractionSet, List /@ remaining]
];

(**************************************************************************************************)

PackageExport["MinimalContractedQuivers"]

Options[MinimalContractedQuivers] = Options[ExtendedGraph];

MinimalContractedQuivers[quiver_, opts:OptionsPattern[]] := Scope[
  contractionSets = MinimalContractionSets[quiver];
  ContractVertices[quiver, #]& /@ contractionSets
]

(**************************************************************************************************)

PackageExport["MinimalContractionSets"]

Options[MinimalContractionSets] = {
  "Complete" -> False,
  "SubsetSize" -> {2},
  "DeleteDominated" -> False
};

MinimalContractionSets[quiver_, OptionsPattern[]] := Scope[
  UnpackOptions[complete, subsetSize, deleteDominated];
  cards = CardinalList @ quiver;
  allCards = Join[cards, Negated /@ cards];
  outTable = TagVertexOutTable @ quiver;
  vertices = VertexRange @ quiver;
  vertexPairs = Subsets[vertices, subsetSize];
  alreadySeen = Data`UnorderedAssociation[];
  contractionSets = DeleteDuplicates @ Map[growTerms, vertexPairs];
  If[deleteDominated,
    contractionSets //= Discard[AnyTrue[contractionSets, contractionDominates[#]]&]];
  If[complete, contractionSets //= Map[CompleteContractionSet[vertices]]];
  ExtractIndices[
    VertexList @ quiver,
    contractionSets
  ]
];

(* c2 dominates c1 if every term in c2 is a subset of a term in c1 *)
contractionDominates[c1_][c1_] := False;
(* contractionDominates[c1_][c2_] := AllTrue[c2, t2 |-> AnyTrue[c1, t1 |-> StrictSubsetQ[t1, t2]]];
contractionDominates[c1_][c2_] := AllTrue[c1, t1 |-> AnyTrue[c2, t2 |-> StrictSubsetQ[t1, t2]]];
 *)
contractionDominates[c1_][c2_] := And[
  AllTrue[c2, t2 |-> AnyTrue[c1, t1 |-> StrictSubsetQ[t1, t2]]],
  AllTrue[c1, t1 |-> AnyTrue[c2, t2 |-> StrictSubsetQ[t1, t2]]]
];

SupersetQ[a_, b_] := SubsetQ[b, a];
StrictSupersetQ[a_, b_] := SubsetQ[b, a] && !SubsetQ[a, b];
StrictSubsetQ[a_, b_] := SubsetQ[a, b] && !SubsetQ[b, a];

growTerms[init_] := Scope[
  (* If[KeyExistsQ[alreadySeen, init], Return @ Nothing]; *)
  $ds = CreateDataStructure["DisjointSet"];
  $hs = CreateDataStructure["HashSet"];
  addTerm[init];
  Label["Start"];
  Scan[If[checkSubset[#], Goto["Start"]]&, $ds["Subsets"]];
  subsets = $ds["Subsets"];
  result = SortContractionSet @ subsets;
  AssociateTo[alreadySeen, Flatten[Map[# -> True&, Subsets[#, {2}]]& /@ result]];
  result
(*   remaining = Complement[vertices, Union @@ subsets];
  SortContractionSet @ Join[subsets, List /@ remaining]
 *)
];
  
checkSubset[subset_] := Scope[
  (* If[!$hs["Insert", Sort @ subset], Return[False]]; *)
  $dirty = False;
  KeyValueMap[
    {card, table} |-> addTerm @ DeleteNone @ Part[table, subset],
    outTable
  ];
  $dirty
];

addTerm = Case[
  {_} | {} := Null;
  {a_, b_} := (
    $ds["Insert", a];
    $ds["Insert", b];
    If[!$ds["CommonSubsetQ", a, b], $ds["Unify", a, b]; $dirty = True];
  );
  elems_List := Block[{f, rest},
    Scan[$ds["Insert", #]&, elems];
    f = First @ elems;
    Scan[
      If[!$ds["CommonSubsetQ", f, #],
        $dirty = True;
        $ds["Unify", f, #];
      ]&,
      Rest @ elems
    ];
  ];
];

(**************************************************************************************************)

PackageExport["VertexPartitionGraphics"]

Options[VertexPartitionGraphics] = JoinOptions[
  "Style" -> "Color",
  Graphics
];

VertexPartitionGraphics[graph_, partition_List, opts:OptionsPattern[]] := Scope[
  UnpackOptions[plotRangePadding, style];
  vertexCoordsAssoc = LookupVertexCoordinates @ graph;
  vertexCoords = Values @ vertexCoordsAssoc;
  vertexCoordsBounds = CoordinateBounds[vertexCoords, Scaled[0.1]];
  $partitionGraphicsOpts = Sequence @@ DeleteOptions[{opts}, "Style"];
  termGraphicsFn = Switch[style,
    "Color", makeColoredCliqueGraphics,
    "Edges", makeEdgeCliqueGraphics,
    _, ReturnFailed[]
  ];
  If[VectorQ[partition, IntegerVectorQ], termGraphicsFn, Map @ termGraphicsFn] @ partition
]

$partitionGraphicsOpts = Sequence[];

makeEdgeCliqueGraphics[partitionList_] := Scope[
  partitionPoints = Lookup[vertexCoordsAssoc, #]& /@ partitionList;
  primitives = {
    {GrayLevel[0, 0.2], CapForm["Round"], AbsoluteThickness[4], Line @ Catenate[makeClique /@ partitionPoints]},
    AbsolutePointSize[4], Point @ vertexCoords
  };
  graphics = Graphics[primitives,
    $partitionGraphicsOpts,
    PlotRange -> vertexCoordsBounds,
    FrameStyle -> LightGray, FrameTicks -> None,
    ImageSize -> 35, Frame -> True, Background -> White,
    PlotRangePadding -> plotRangePadding
  ];
  graphics
];

choosePalette = Case[
  0 := {};
  1 := {$Gray};
  2 := {$LightRed, $LightBlue};
  n_ := Take[$LightColorPalette, n];
]

makeColoredCliqueGraphics[partitionList_] := Scope[
  termPoints = Lookup[vertexCoordsAssoc, #]& /@ partitionList;
  i = 1;
  numColors = Count[partitionList, Except[{_}]];
  palette = choosePalette @ numColors;
  termColors = Map[If[MatchQ[#, {_}], White, Part[palette, i++]]&, partitionList];
  primitives = MapThread[
    Style[Thread @ Disk[#1, .2], EdgeForm[Darker[#2, .3]], FaceForm[#2]]&,
    {termPoints, termColors}
  ];
  graphics = Graphics[primitives,
    $partitionGraphicsOpts,
    PlotRange -> vertexCoordsBounds,
    FrameStyle -> LightGray, FrameTicks -> None,
    ImageSize -> 35, Frame -> True, Background -> White,
    PlotRangePadding -> plotRangePadding
  ];
  graphics
];

makeClique[list_] := Subsets[list, {2}];

(**************************************************************************************************)

PackageExport["GraphContractionList"]

GraphContractionList[graph_, opts_List] := Scope[
  orderGraph = GraphContractionLattice[graph, opts];
  LookupVertexAnnotations[orderGraph, "ContractedGraph"]
]

(**************************************************************************************************)

PackageExport["GraphContractionLattice"]

Options[GraphContractionLattice] = JoinOptions[
  "GreedyEdgeContraction" -> True,
  QuiverContractionLattice
];

GraphContractionLattice[graph_, contractedGraphOptions_List, userOpts:OptionsPattern[]] := Scope[
  
  If[!EdgeTaggedGraphQ[graph], graph //= IndexEdgeTaggedGraph];

  UnpackOptions[combineMultiedges, greedyEdgeContraction];
  innerSize = LookupOption[contractedGraphOptions, ImageSize, 50];
  contractedGraphOptions = Sequence @@ contractedGraphOptions;
  
  initialGraph = ContractedGraph[graph, contractedGraphOptions, ImageSize -> {innerSize, innerSize}];
  innerOpts = Sequence @@ ExtractExtendedGraphOptions @ initialGraph;

  edgeList = CanonicalizeEdges @ EdgeList @ graph;
  isDirected = DirectedGraphQ @ graph;
  sorter = If[isDirected, Identity, Map[Sort]];

  successorFn = If[greedyEdgeContraction, greedyGraphContractionSuccessors, graphContractionSuccessors];
  {vlist, ielist} = MultiwaySystem[successorFn, {edgeList}, {"VertexList", "IndexEdgeList"}];
  
  irange = Range @ Length @ vlist;
      
  postFn = If[combineMultiedges, CombineMultiedges, Identity];

  graphFn = edges |-> ExtendedGraph[
    AllUniqueVertices @ edges,
    edges,
    innerOpts
  ];
  contractedGraphs = Map[graphFn /* postFn, vlist];
  
  ExtendedGraph[
    Range @ Length @ vlist, ielist, FilterOptions @ userOpts,
    GraphLayout -> "CenteredTree",
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
  rules = toVertexContractionRule /@ Subsets[vertices, {2}];
  gluingResultsList[edgeList, rules]
];

$flattenGlue = e_ContractedEdge | e_ContractedVertex :> DeleteDuplicates[e];

toVertexContractionRule[{v_}] := Nothing;

toVertexContractionRule[verts_List] := With[
  {alts = Alternatives @@ verts, glued = ContractedVertex @@ verts},
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

edgeContractionSuccessors[edgeList_] := Scope[
  index = Values @ PositionIndex[Take[edgeList, All, 2]];
  index = Select[index, Length[#] >= 2&];
  rules = Flatten[toEdgeContractionRuleList[Part[edgeList, #]]& /@ index];
  Sort[CanonicalizeEdges @ DeleteDuplicates[Replace[edgeList, #, {1}]]]& /@ rules
];

toEdgeContractionRuleList[edges_List] := toEdgeContractionRule @@@ Subsets[edges, {2}]

SetAttributes[{ContractedEdge, ContractedVertex}, {Flat, Orderless}];
toEdgeContractionRule[head_[a1_, b1_, c_], head_[a2_, b2_, d_]] :=
  e:(head[a1, b1, c] | head[a2, b2, d]) :> ReplacePart[e, 3 -> DeleteDuplicates[ContractedEdge[c, d]]];

greedyContractEdges[edgeList_] := Scope[
  index = Values @ PositionIndex[CanonicalizeEdges @ Take[edgeList, All, 2]];
  index = Select[index, Length[#] >= 2&];
  If[index === {}, Return @ edgeList];
  rules = Flatten[toEdgeContractionRuleList[Part[edgeList, #]]& /@ index];
  greedyContractEdges @ Sort @ CanonicalizeEdges @ DeleteDuplicates @ Replace[edgeList, rules, {1}]
];

(**************************************************************************************************)

PackageExport["UnContractedGraph"]

UnContractedGraph[graph_Graph, opts___Rule] := Scope[
  {vertexList, edgeList} = VertexEdgeList @ graph;
  If[!MemberQ[vertexList, _ContractedVertex] && !MemberQ[edgeList, _[_, _, _ContractedEdge]],
    Return @ ExtendedGraph[graph, opts]];
  ungluingRules = Cases[vertexList, g_ContractedVertex :> (g -> Splice @ Apply[List, g])];
  vertexList = DeleteDuplicates @ Replace[vertexList, ungluingRules, {1}];
  edgeList = DeleteDuplicates @ Replace[edgeList, ungluingRules, {2}];
  edgeList = DeleteDuplicates @ Replace[edgeList, $ContractedVertexExpansionRules, {1}];
  edgeList = DeleteDuplicates @ Replace[edgeList, $ContractedEdgeExpansionRules, {1}];
  ExtendedGraph[vertexList, edgeList, opts, Sequence @@ Options @ graph]
]

$ContractedVertexExpansionRules = {
  (head_)[a_Splice, b_Splice, tag___] :>
    Splice @ Flatten @ Outer[head[#1, #2, tag]&, First @ a, First @ b, 1],
  (head_)[a_, b_Splice, tag___] :>
    Splice @ Map[head[a, #, tag]&, First @ b],
  (head_)[a_Splice, b_, tag___] :>
    Splice @ Map[head[#, b, tag]&, First @ a]
};

$ContractedEdgeExpansionRules = {
  head_[a_, b_, e_ContractedEdge] :> Splice @ Map[head[a, b, #]&, List @@ e]
}

(**************************************************************************************************)

PackageExport["ContractVertices"]

Options[ContractVertices] = JoinOptions[
  VertexCoordinates -> Automatic,
  ExtendedGraph
];

ContractVertices[graph_Graph, glueList_List, userOpts:OptionsPattern[]] := Scope[
  opts = ExtractExtendedGraphOptions @ graph;
  edgeList = EdgeList @ graph;
  glueList //= ToContractionSet;
  glueRules = Flatten[toVertexContractionRule /@ glueList];
  vertexCoordinates = LookupOption[{userOpts}, VertexCoordinates, Automatic];
  If[glueRules === {} && vertexCoordinates =!= "Mean",
    Return @ If[{userOpts} === {}, graph, ExtendedGraph[graph, userOpts]]];
  contractedEdgeList = Fold[gluingResult, edgeList, glueRules];
  opts = Sequence @@ DeleteOptions[opts, {VertexCoordinates, VertexCoordinateRules}];
  vertexList = Sort @ AllUniqueVertices @ contractedEdgeList;
  If[vertexCoordinates === "Mean",
    oldCoords = LookupVertexCoordinates[graph];
    vertexCoordinates = ContractedVertexCoordinateFunction[oldCoords] /@ vertexList;
    userOpts = Sequence @@ ReplaceOptions[{userOpts}, VertexCoordinates -> vertexCoordinates];
  ];
  ExtendedGraph[
    vertexList, contractedEdgeList,
    userOpts,
    opts
  ]
]

(**************************************************************************************************)

PackageExport["ContractedGraph"]
PackageExport["ContractedVertex"]
PackageExport["ContractedEdge"]

ContractedGraph[vertices_List, edges_List, opts___Rule] :=
  ContractedGraph[ExtendedGraph[vertices, edges], opts];

ContractedGraph[edges_List, opts___Rule] :=
  ContractedGraph[ExtendedGraph[AllUniqueVertices @ edges, edges], opts];

ContractedGraph[graph_Graph, opts___Rule] := Scope[

  unContractedGraph = UnContractedGraph[graph, opts];
  
  baseVertexList = VertexList @ unContractedGraph;
  baseVertexColors = LookupVertexColors @ unContractedGraph;
  {baseVertexCoordinates, baseEdgeCoordinateLists} = ExtractGraphPrimitiveCoordinates @ unContractedGraph;
  baseEdgeColors = LookupEdgeColors @ unContractedGraph;

  baseCardinals = CardinalList @ unContractedGraph;
  baseCardinalColors = LookupCardinalColors @ unContractedGraph;

  vertexCoordinateFunction = ContractedVertexCoordinateFunction[AssociationThread[baseVertexList, baseVertexCoordinates]];
  edgeColorFunction = Which[
    baseEdgeColors =!= None, ContractedEdgeColorFunction[KeyMap[PartOperator[3], baseEdgeColors]],
    baseCardinalColors =!= None, ContractedEdgeColorFunction[baseCardinalColors],
    True, None
  ];
  vertexColorFunction = If[baseVertexColors === None, None, ContractedVertexColorFunction[baseVertexColors]];
  cardinalColorFunction = If[baseCardinalColors === <||>, None, ContractedCardinalColorFunction[baseCardinalColors]];

  opts = {opts};
  padding = LookupOption[opts, PlotRangePadding, Scaled[0.05]];
  bounds = ToSquarePlotRange @ CoordinateBounds[baseVertexCoordinates, padding];

  edgeLengthScale = EdgeLengthScale[baseEdgeCoordinateLists, .5] / 4.0;

  ExtendedGraph[graph,
    VertexColorRules -> None, CoordinateTransformFunction -> None,
    VertexCoordinates -> Automatic, VertexCoordinateRules -> None,
    VertexLayout -> None,
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
    PrologFunction -> ContractedVertexPrologFunction
  ]
];

(**************************************************************************************************)

PackageExport["ContractedCardinalColorFunction"]

ContractedCardinalColorFunction[baseColors_][cardinal_] :=
  If[Head[cardinal] === ContractedEdge,
    HumanBlend @ DeleteMissing @ Lookup[baseColors, List @@ cardinal],
    Lookup[baseColors, cardinal, $DarkGray]
  ];

(**************************************************************************************************)

PackageExport["ContractedEdgeColorFunction"]

ContractedEdgeColorFunction[baseColors_][_[_, _, tag_]] :=
  If[Head[tag] === ContractedEdge,
    HumanBlend @ DeleteMissing @ Lookup[baseColors, List @@ tag],
    Lookup[baseColors, tag, $DarkGray]
  ];

(**************************************************************************************************)

PackageExport["ContractedVertexColorFunction"]

ContractedVertexColorFunction[baseColors_][vertex_] :=
  If[Head[vertex] === ContractedVertex,
    HumanBlend @ DeleteMissing @ Lookup[baseColors, List @@ vertex],
    Lookup[baseColors, vertex, $DarkGray]
  ];

(**************************************************************************************************)

PackageExport["ContractedVertexCoordinateFunction"]

ContractedVertexCoordinateFunction[baseCoords_][vertex_] :=
  If[Head[vertex] === ContractedVertex,
    Mean @ DeleteMissing @ Lookup[baseCoords, List @@ vertex],
    Lookup[baseCoords, vertex]
  ];
  
(**************************************************************************************************)

PackageExport["ContractedVertexPrologFunction"]

ContractedVertexPrologFunction[graph_] := Scope[
  baseCoordFunc = GraphAnnotationData[VertexCoordinateFunction];
  imageWidth = First @ LookupImageSize[graph];
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