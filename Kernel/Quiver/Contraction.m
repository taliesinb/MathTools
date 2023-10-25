PublicFunction[QuiverContractionList]

QuiverContractionList[graph_, opts_List] := Scope[
  orderGraph = QuiverContractionLattice[graph, opts];
  LookupVertexAnnotations[orderGraph, "ContractedGraph"]
]

(**************************************************************************************************)

PublicFunction[QuiverContractionLattice]

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
    VertexLayout -> TreeVertexLayout[Balanced -> True],
    VertexAnnotations -> vertexAnnotations,
    ArrowheadShape -> None, VertexSize -> Max[innerSize],
    VertexTooltips -> "ContractionSet",
    VertexShapeFunction -> "ContractedGraph"
  ]
];

$validPartitionCache = <||>;
computeValidPartitions[{vertexList_, outTable_, allowGraphContractions_, permittedMatrix_, quiver_}] := Scope[

  path = CacheFilePath["QuiverContractions", vertexList, outTable, permittedMatrix];
  Hold @ If[FileExistsQ[path], Return @ ImportMX @ path];

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

PublicFunction[SpacelikeVertexRelationMatrix]

SpacelikeVertexRelationMatrix[graph_Graph] := Scope[
  dist = GraphDistanceMatrix[graph] /. {_Integer -> 0, Infinity -> 1};
  dist = MapThread[Min, {dist, Transpose @ dist}, 2];
  ToPacked @ dist
];

(**************************************************************************************************)

PublicFunction[QuiverContractionLatticeFast]

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

DefineGraphTheme["ContractionGraphIcon",
  Frame -> True, Background -> White,
  PlotRange -> "Square",
  ArrowheadShape -> {"Line", TwoWayStyle -> "CrossLine"},
  VertexSize -> 8, VertexCoordinates -> "Mean", SelfLoopRadius -> 0.5,
  ImagePadding -> 10
];

(**************************************************************************************************)

PublicFunction[ContractionSetUnionLattice]

Options[ContractionSetUnionLattice] = Options @ Graph;

ContractionSetUnionLattice[contractionSets:{__List}, opts:OptionsPattern[]] := Scope[
  contractionSets //= Map @ SortContractionSet;
  {vertexList, edgeList} = MultiwaySystem[
    contractionSetUnionCayleyFunction[contractionSets],
    {{}},
    {"VertexList", "EdgeList"}
  ];
  edgeList = Values @ GroupBy[edgeList, TakeOperator[2], combineTags];
  TransitiveReductionGraph @ ExtendedGraph[vertexList, edgeList, opts, VertexLayout -> TreeVertexLayout[]]
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

PublicFunction[ContractionSetUnion]

(* DisjointSet doesn not have the right API yet to make this fast *)
ContractionSetUnion[contractionSet1_, contractionSet2_] :=
  Sort @ Fold[ContractionSetAppend, contractionSet1, contractionSet2];

(**************************************************************************************************)

PublicFunction[ContractionSetAppend]

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

PublicFunction[ToContractionSet]

ToContractionSet[term:{___Integer}] := List @ Sort @ term;

ToContractionSet[contractionSet_] :=
  SortContractionSet @ If[DuplicateFreeQ[contractionSet, IntersectingQ],
    contractionSet,
    Fold[ContractionSetAppend, {}, contractionSet]
  ];

(**************************************************************************************************)

PublicFunction[SortContractionSet]

SortContractionSet[contractionSet_] :=
  LexicographicSort @ Map[Sort, contractionSet];

SortContractionSet[contractionSet_, index_] :=
  LexicographicSort @ Map[OrderSort[index], contractionSet];

(**************************************************************************************************)

PublicFunction[CompleteContractionSet]

CompleteContractionSet[vertices_][contractionSet_] := Scope[
  remaining = Complement[vertices, Union @@ contractionSet];
  SortContractionSet @ Join[contractionSet, List /@ remaining]
];

(**************************************************************************************************)

PublicFunction[MinimalContractedQuivers]

Options[MinimalContractedQuivers] = Options[ExtendedGraph];

MinimalContractedQuivers[quiver_, opts:OptionsPattern[]] := Scope[
  contractionSets = MinimalContractionSets[quiver];
  ContractVertices[quiver, #]& /@ contractionSets
]

(**************************************************************************************************)

PublicFunction[MinimalContractionSets]

Options[MinimalContractionSets] = {
  "Complete" -> False,
  "SubsetSize" -> {2},
  "DeleteDominated" -> False
};

MinimalContractionSets[quiver_, OptionsPattern[]] := Scope[
  UnpackOptions[complete, subsetSize, deleteDominated];
  cards = CardinalList @ quiver;
  allCards = Join[cards, Inverted /@ cards];
  outTable = TagVertexOutTable @ quiver;
  vertices = VertexRange @ quiver;
  vertexPairs = Subsets[vertices, subsetSize];
  alreadySeen = UAssociation[];
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
  AssociateTo[alreadySeen, Flatten[Map[# -> True&, UnorderedPairs[#]]& /@ result]];
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

PublicFunction[VertexPartitionGraphics]

Options[VertexPartitionGraphics] = JoinOptions[
  "Style" -> "Color",
  Graphics
];

VertexPartitionGraphics[graph_, partition_List, opts:OptionsPattern[]] := Scope[
  UnpackOptions[plotRangePadding, style];
  vertexCoordsAssoc = LookupVertexCoordinates @ graph;
  vertexCoords = Values @ vertexCoordsAssoc;
  vertexCoordsBounds = CoordinateBounds[vertexCoords, Scaled[0.1]];
  $partitionGraphicsOpts = SeqDropOptions["Style"][opts];
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
    {GrayLevel[0, 0.2], CapForm["Round"], AbsoluteThickness[4], Line @ Catenate[UnorderedPairs /@ partitionPoints]},
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
