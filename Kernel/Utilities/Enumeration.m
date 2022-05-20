PackageExport["GraphVertexAssignments"]

SetUsage @ "
GraphVertexAssignments[graph$, integers$] assigns values from integers$ to the vertices of graph in all possible ways,
returning lists of assignments in the vertex order. The default value of 0 is implied.
The automorphism group of the graph will be used to return only one representative of each assignment.
"

GraphVertexAssignments[graph_, values_] := Scope[
  n = VertexCount[graph];
  values = PadRight[values, n];
  perms = Permutations[values];
  group = Quiet @ Check[GraphAutomorphismGroup[graph], $Failed];
  If[FailureQ[group], Return[perms]];
  DeleteDuplicates[perms, MemberQ[Permute[#2, group], #1]&]
]

(**************************************************************************************************)

PackageExport["EdgeAutomorphismGroup"]

EdgeAutomorphismGroup[graph_Graph] :=
  GraphAutomorphismGroup @ LineGraphFixed @ graph;

(**************************************************************************************************)

PackageExport["UniqueEdgeColorings"]

UniqueEdgeColorings[graph_] := Scope[
  group = EdgeAutomorphismGroup @ graph;
  edgeCount = EdgeCount @ graph;
  edgeRange = Range @ edgeCount;
  First /@ GroupOrbits[group, Permutations @ edgeRange]
]

(**************************************************************************************************)

PackageExport["AllCardinalSets"]

AllCardinalSets[cardinals_] := Scope[
  cardinals = Join[cardinals, Inverted /@ cardinals];
  numCardinals = Length @ cardinals;
  Subsets[cardinals, {1, numCardinals}]
];

(**************************************************************************************************)

PackageExport["CardinalRenamings"]

CardinalRenamings[cardinals_] := Scope[
  Map[Dispatch @ DeleteCases[RuleThread[cardinals, #], Rule[z_, z_]]&, Permutations @ cardinals]
];

(**************************************************************************************************)

PackageExport["CountQuivers"]

CountQuivers[graph_, cardCount_] := CountQuivers[graph, cardCount] = Scope[
  res = EnumerateQuivers[graph, cardCount, False, True];
  Which[
    MissingQ[res], Indeterminate,
    MemberQ[res, _Missing], AtLeast @ Total @ Map[Length] @ DeleteMissing @ res,
    True, Total @ Map[Length] @ res
  ]
];

(**************************************************************************************************)

PackageExport["$EnableEnumerateCache"]
PackageExport["$EnumerateVerboseMode"]

$EnumerateVerboseMode = False;
$EnableEnumerateCache = True;

(**************************************************************************************************)

PackageExport["EnumerateQuivers"]

EnumerateQuivers[n_Integer, args___] := Scope[
  skeletons = EnumerateQuiverSkeletons[n];
  Join @@ Map[
    skeleton |-> Replace[EnumerateQuivers[skeleton, args], m_Missing :> {m}],
    skeletons
  ]
];

EnumerateQuivers[graph_, UpTo[cardCount_], wrap_:True, noComp_:False] :=
  Join @@ Table[EnumerateQuivers[graph, i, wrap, noComp], {i, 1, cardCount}];

EnumerateQuivers[graph_, cardCount_, wrap_:True, noComp_:False] := Scope[
  cardinals = Range @ cardCount;
  graph = CanonicalGraph @ UndirectedGraph @ graph;
  If[!GraphQ[graph], ReturnFailed[]];
  graphs = iEnumerateQuivers[graph, cardCount, noComp];
  vertices = VertexList @ graph;
  vertexCount = Length @ vertices;
  If[wrap,
    opts = makeSmallGraphOpts[VertexCount @ graph, .5];
    CombineMultiedges @ ExtendedGraph[vertices, #, opts]& /@ graphs
  ,
    graphs
  ]
];

$enumerateQuiversCache = <||>;

quiverCacheLoad[name_, edges_, count_] := Module[
  {hash, fileName, memCache, diskCache, key = {name, edges, count}},
  memCache = $enumerateQuiversCache[key];
  If[!MissingQ[memCache], Return @ memCache];
  fileName = CacheFilePath[name, edges, count];
  If[FileExistsQ[fileName],
    diskCache = Import @ fileName;
    $enumerateQuiversCache[key] = diskCache;
    diskCache
  ,
    None
  ]
];

quiverCacheStore[name_, edges_, count_, result_] :=
  $enumerateQuiversCache[{name, edges, count}] = EnsureExport[CacheFilePath[name, edges, count], result];

PackageExport["$EnumerationImplementation"]

$EnumerationImplementation = 1;

iEnumerateQuivers[graph_, cardCount_, noComp_] /; $EnumerationImplementation === 1 := Scope[
  keyEdges = edges = EdgeList @ graph;
  cachedResult = If[$EnableEnumerateCache, quiverCacheLoad["EnumerateQuivers", edges, cardCount], None];
  If[cachedResult =!= None, Return @ cachedResult];
  If[noComp, Return @ Missing["NotComputed"]];
  vertices = VertexList @ graph;
  vertexCount = Length @ vertices;
  undirectedEdgeVertices = List @@@ edges;
  undirectedEdgeCount = Length @ edges;
  undirectedEdgeRange = Range @ undirectedEdgeCount;
  edges = DirectedEdge @@@ edges;
  edges = Join[edges, Reverse /@ edges];
  edgeMask = Join[undirectedEdgeRange, undirectedEdgeRange];
  edgeCount = Length @ edges;
  edgeRange = Range @ edgeCount;
  edgeSubsets = Subsets[edgeRange, {1, edgeCount}];
  (* some edgesubsets are forbidden because they would mean that a cardinal labels a given vertex twice *)
  (* these are associations from vertex to the edge indices whose source vertex is that vertex, etc. *)
  inVertices = InVertices @ edges;
  outVertices = OutVertices @ edges;
  edgeSubsets //= Select[DuplicateFreeQ[Part[inVertices, #]] && DuplicateFreeQ[Part[outVertices, #]]&];
  If[$EnumerateVerboseMode, Print["# edge subsets: ", Length @ edgeSubsets]];
  edgeTuples = Tuples[edgeSubsets, cardCount];
  edgeTuples = DeleteDuplicates[Sort /@ edgeTuples];
  edgeTuples = Select[edgeTuples, Union[Part[edgeMask, Flatten @ #]] === undirectedEdgeRange&];
  If[$EnumerateVerboseMode, Print["# edge tuples: ", Length @ edgeTuples]];
  (* edgeTuples is a list of length cardCount of which edges is cardinal is attached to *)
  graphs = Map[
    tuple |-> Graph[vertices, Flatten @ MapIndexed[{indices, cardinal} |-> Map[Append[First @ cardinal], Part[edges, indices]], tuple]],
    edgeTuples
  ];
  If[$EnumerateVerboseMode, Print["# total graphs: ", Length @ graphs]];
  connectedIndices = SelectIndices[graphs, WeaklyConnectedGraphQ];
  graphs = Part[graphs, connectedIndices];
  edgeTuples = Part[edgeTuples, connectedIndices];
  lineGraphs = LineGraphFixed /@ graphs; edgeCounts = EdgeCount /@ graphs; edgeTags = EdgeTags /@ graphs;
  graphRange = Range @ Length @ graphs;
  collapseTupleElement = indices |-> Mod[indices, undirectedEdgeCount, 1];
  undirectedEdgeTuples = Map[tuples |-> Map[collapseTupleElement, tuples], edgeTuples];
  graphSignature = i |-> {
    Part[edgeCounts, i],
    countsSignature @ Part[edgeTags, i],
    (* these are the number of times an undirected edge is labelled by a cardinal *)
    uet = Part[undirectedEdgeTuples, i];
    flatCountsSignature @ uet,
    vertexDegrees = VertexDegree @ Part[graphs, i];
    Sort @ vertexDegrees,
    (* the sorted degrees of the vertices incident to edges labeled with this cardinal *)
(*     Sort @ Map[
      tuple |-> Sort @ Part[vertexDegrees, Union @@ Part[undirectedEdgeVertices, tuple]],
      uet
    ],
 *)    (* the number of edges labelled by pairs of cardinals *)
    Sort[Length[Intersection[#1, #2]]& @@@ UnorderedPairs[uet]],
    (* these are the number of times a given cardinal is labeled *)
    Sort @ Map[flatCountsSignature, uet]
  };
  graphRangeGrouped = GatherBy[graphRange, graphSignature];
  If[$EnumerateVerboseMode,
    Print["# groups: ", Length @ graphRangeGrouped];
    Print["max group size: ", Max[Length /@ graphRangeGrouped]];
    Print["line graphs size: ", ByteCount @ lineGraphs];
  ];
  tagIsoQ = {a, b} |-> And[
    (isoList = FindGraphIsomorphism[Part[lineGraphs, a], Part[lineGraphs, b]]) =!= {},
    graphsEqualModuloEdgeNaming[Part[edgeTags, a], Part[edgeTags, b], isoList]
  ];
  map = If[$EnumerateVerboseMode, MonitoredMap, Map];
  uniqueIndices = Union @@ map[group |-> DeleteDuplicates[group, tagIsoQ], graphRangeGrouped];
  result = EdgeList /@ Part[graphs, uniqueIndices];
  quiverCacheStore["EnumerateQuivers", keyEdges, cardCount, result];
  result
];

iEnumerateQuivers[graph_, cardCount_, noComp_] /; $EnumerationImplementation === 2 := Scope[
  keyEdges = edges = EdgeList @ graph;
  cachedResult = If[$EnableEnumerateCache, quiverCacheLoad["EnumerateQuivers", edges, cardCount], None];
  If[cachedResult =!= None, Return @ cachedResult];
  If[noComp, Return @ Missing["NotComputed"]];
  vertices = VertexList @ graph;
  vertexCount = Length @ vertices;
  undirectedEdgeVertices = List @@@ edges;
  undirectedEdgeCount = Length @ edges;
  undirectedEdgeRange = Range @ undirectedEdgeCount;
  edges = DirectedEdge @@@ edges;
  edges = Join[edges, Reverse /@ edges];
  edgeMask = Join[undirectedEdgeRange, undirectedEdgeRange];
  edgeCount = Length @ edges;
  edgeRange = Range @ edgeCount;
  edgeSubsets = Subsets[edgeRange, {1, edgeCount}];
  (* some edgesubsets are forbidden because they would mean that a cardinal labels a given vertex twice *)
  (* these are associations from vertex to the edge indices whose source vertex is that vertex, etc. *)
  inVertices = InVertices @ edges;
  outVertices = OutVertices @ edges;
  edgeSubsets //= Select[DuplicateFreeQ[Part[inVertices, #]] && DuplicateFreeQ[Part[outVertices, #]]&];
  If[$EnumerateVerboseMode, Print["# edge subsets: ", Length @ edgeSubsets]];
  graphs = Internal`Bag[]; undirectedEdgeTuples = Internal`Bag[];
(*   Array[
    List /* (indexTuple |-> If[OrderedQ[indexTuple],
      edgeTuple = Part[edgeSubsets, indexTuple];
      If[Union[Part[edgeMask, Flatten @ edgeTuple]] === undirectedEdgeRange,
        graph = Graph[vertices, Flatten @ MapIndexed[{indices, cardinal} |-> Map[Append[First @ cardinal], Part[edges, indices]], edgeTuple]];
        If[WeaklyConnectedGraphQ @ graph,
          Internal`StuffBag[graphs, graph];
          Internal`StuffBag[undirectedEdgeTuples, Mod[edgeTuple, undirectedEdgeCount, 1]];
         ]
      ];
    ]),
    ConstantArray[Length @ edgeSubsets, cardCount]
  ]; *)
  If[$EnumerateVerboseMode, Print["creating graphs"]]; i1o = 0;
  OrderedTupleScan[
    indexTuple |-> (
      If[$EnumerateVerboseMode, i1 = Take[indexTuple, 2]; If[i1 =!= i1o, Print[i1]; i1o = i1]];
      edgeTuple = Part[edgeSubsets, indexTuple];
      If[Union[Part[edgeMask, Flatten @ edgeTuple]] === undirectedEdgeRange,
        graph = Graph[vertices, Flatten @ MapIndexed[{indices, cardinal} |-> Map[Append[First @ cardinal], Part[edges, indices]], edgeTuple]];
        If[WeaklyConnectedGraphQ @ graph,
          Internal`StuffBag[graphs, graph];
          Internal`StuffBag[undirectedEdgeTuples, Mod[edgeTuple, undirectedEdgeCount, 1]];
        ]
      ];
    ),
    Length @ edgeSubsets, cardCount
  ];
  graphs = Internal`BagPart[graphs, All];
  graphRange = Range @ Length @ graphs;
  If[$EnumerateVerboseMode, Print["creating line graphs"]];
  lineGraphs = LineGraphFixed /@ graphs; edgeCounts = EdgeCount /@ graphs; edgeTags = EdgeTags /@ graphs;
  If[$EnumerateVerboseMode, Print["# total graphs: ", Length @ graphs]];
  undirectedEdgeTuples = Internal`BagPart[undirectedEdgeTuples, All];
  graphSignature = i |-> {
    Part[edgeCounts, i],
    countsSignature @ Part[edgeTags, i],
    (* these are the number of times an undirected edge is labelled by a cardinal *)
    uet = Part[undirectedEdgeTuples, i];
    flatCountsSignature @ uet,
    vertexDegrees = VertexDegree @ Part[graphs, i];
    Sort @ vertexDegrees,
    (* the sorted degrees of the vertices incident to edges labeled with this cardinal *)
(*     Sort @ Map[
      tuple |-> Sort @ Part[vertexDegrees, Union @@ Part[undirectedEdgeVertices, tuple]],
      uet
    ],
 *)    (* the number of edges labelled by pairs of cardinals *)
    Sort[Length[Intersection[#1, #2]]& @@@ UnorderedPairs[uet]],
    (* these are the number of times a given cardinal is labeled *)
    Sort @ Map[flatCountsSignature, uet]
  };
  If[$EnumerateVerboseMode, Print["creating groups"]];
  graphRangeGrouped = GatherBy[graphRange, graphSignature];
  If[$EnumerateVerboseMode,
    Print["# groups: ", Length @ graphRangeGrouped];
    Print["max group size: ", Max[Length /@ graphRangeGrouped]];
    Print["graphs size: ", ByteCount @ graphs];
    Print["line graphs size: ", ByteCount @ lineGraphs];
  ];
  tagIsoQ = {a, b} |-> And[
    (isoList = FindGraphIsomorphism[Part[lineGraphs, a], Part[lineGraphs, b]]) =!= {},
    graphsEqualModuloEdgeNaming[Part[edgeTags, a], Part[edgeTags, b], isoList]
  ];
  map = If[$EnumerateVerboseMode, MonitoredMap, Map];
  uniqueIndices = Union @@ map[group |-> DeleteDuplicates[group, tagIsoQ], graphRangeGrouped];
  result = EdgeList /@ Part[graphs, uniqueIndices];
  quiverCacheStore["EnumerateQuivers", keyEdges, cardCount, result];
  result
];

OrderedTupleScan[f_, n_, m_] := Block[
  {$f = f, $n = n, $m = m, ots},
  ots[l_, 0] := $f[l];
  ots[l_, j_] := Do[ots[Append[l, i], j-1], {i, Last[l], $n}];
  Do[ots[{i}, $m], {i, $n}]
];

flatCountsSignature[list_] := countsSignature @ Flatten @ list;
countsSignature[list_] := Sort @ Values @ Counts @ list;

graphsEqualModuloEdgeNaming[tags1_, tags2_, isoList_] :=
  AnyTrue[isoList, iso |-> equalModuloNaming[Part[tags1, ReplaceAll[Range @ Length @ tags1, iso]], tags2]];

equalModuloNaming[list1_, list2_] := First[ArrayLabeling[list1]] === First[ArrayLabeling[list2]];

(**************************************************************************************************)

PackageExport["IsomorphicTaggedGraphsQ"]

IsomorphicTaggedGraphsQ[g1_, g2_] := Scope[
  g1 //= ExpandCardinalSetEdges;
  g2 //= ExpandCardinalSetEdges;
  isoList = FindEdgeIsomorphism[g1, g2, All];
  If[isoList === {}, Return @ False];
  edges1 = EdgeList @ g1; edges2 = EdgeList @ g2;
  Do[
    If[equalModuloNaming[
      Part[edges1, ReplaceAll[Range @ Length @ edges1, iso], 3],
      Part[edges2, All, 3]], Return[True, Block]],
    {iso, isoList}
  ];
  False
];

(**************************************************************************************************)

PackageExport["FindEdgeIsomorphism"]

FindEdgeIsomorphism[g1_, g2_, args___] :=
  If[EdgeCount[g1] =!= EdgeCount[g2], {},
    FindGraphIsomorphism[LineGraphFixed @ g1, LineGraphFixed @ g2, args]];

(**************************************************************************************************)

PackageExport["LineGraphFixed"]

LineGraphFixed[g_] := Scope[
  edges = EdgeList[g];
  inVertices = InVertices[edges];
  outVertices = OutVertices[edges];
  inIndices = PositionIndex[inVertices];
  Graph[
    Range @ Length @ edges,
    Flatten @ MapIndexed[
      Thread[DirectedEdge[First @ #2, #1]]&,
      Lookup[inIndices, outVertices]
    ]
  ]
]

(**************************************************************************************************)

PackageExport["EnumerateQuiverSkeletons"]

mkCirclePoints[1] = {{0, 0}};
mkCirclePoints[2] = {{-1, -1}, {1, 1}} * 0.7;
mkCirclePoints[3] = {{0.866025, -0.75}, {0., 0.75}, {-0.866025, -0.75}};
mkCirclePoints[4] = Part[CirclePoints[4], {1, 3, 2, 4}];
mkCirclePoints[n_] := N @ CirclePoints[n];

makeSmallGraphOpts[n_, r_] := Sequence[
  MultiEdgeDistance -> 0.2, SelfLoopRadius -> r,
  ArrowheadSize -> Small, ArrowheadShape -> {"Line", TwoWayStyle -> "CrossLine"}, VertexSize -> Medium,
  VertexCoordinates -> mkCirclePoints[n], ImageSize -> {40, 40}, ImagePadding -> 0,
  PlotRange -> Switch[n,
    1, {{-1, 0}, {-1, 1}} * 0.6,
    2, {{-1, 1}, {-1, 1}} * 1.4,
    _, {{-1, 1}, {-1, 1}} * 1.3
  ],
  BaselinePosition -> "Coordinate" -> 0
];

EnumerateQuiverSkeletons[n_Integer] := Scope[
  edgeLists = iEnumerateQuiverSkeletons[n];
  opts = makeSmallGraphOpts[n, .3];
  vertices = Range[n];
  ExtendedGraph[vertices, #, opts]& /@ edgeLists
];

iEnumerateQuiverSkeletons[n_Integer] := Scope[
  cachedResult = quiverCacheLoad["EnumerateQuiverSkeletons", None, n];
  If[cachedResult =!= None, Return @ cachedResult];
  vertices = Range[n];
  edges = Flatten @ Table[UndirectedEdge[i, j], {i, 1, n}, {j, i, n}];
  edgeSubsets = Subsets[edges, {1, Length @ edges}];
  bag = Internal`Bag[];
  Scan[subset |-> If[Union @ AllVertices[edges] === vertices,
    graph = Graph[vertices, subset];
    If[ConnectedGraphQ[graph], Internal`StuffBag[bag, graph]]],
    edgeSubsets
  ];
  graphs = Internal`BagPart[bag, All];
  graphs = GroupBy[graphs, VertexDegree /* Sort];
  graphs = Join @@ Map[
    DeleteDuplicates[#, IsomorphicGraphQ]&,
    Values @ graphs
  ];
  graphs = Sort @ Map[CanonicalGraph, graphs];
  edgeLists = EdgeList /@ graphs;
  quiverCacheStore["EnumerateQuiverSkeletons", None, n, edgeLists];
  edgeLists
];

(**************************************************************************************************)

PackageExport["EnumerateLattices"]

EnumerateLattices[quivers_, cardinals_, group_, opts___Rule] := Scope[
  rules = MapIndexed[First[#2] -> #1&, cardinals];
  quivers = Map[
    Quiver[VertexList @ #, MapAt[ReplaceAll[rules], EdgeList @ #, {All, 3}]]&,
    ExpandCardinalSetEdges /@ quivers
  ];
  userOpts = {opts};
  {directedEdges} = LookupOption[userOpts, {DirectedEdges}];
  userOpts //= DeleteOptions[DirectedEdges];
  lattices = Map[
    quiver |-> LatticeGraph[
      PathRepresentation[quiver, group], DirectedEdges -> False, MaxNorm -> 3
    ],
    quivers
  ];
  lattices = Select[lattices, DuplicateFreeQ[GraphVertexCoordinates[#]]&];
  lattices = Join @@ Map[
    DeleteDuplicates[#, IsomorphicGraphQ]&,
    Values @ GroupBy[lattices, VertexDegree /* Sort]
  ];
  lattices = SortBy[lattices, VertexCount];
  edgeHead = If[TrueQ @ directedEdges, DirectedEdge, UndirectedEdge];
  userOpts = Sequence @@ TakeOptions[userOpts, Keys @ Options @ ExtendedGraph];
  lattices //= Map @ LatticeGraphToLatticeQuiver
]

LatticeGraphToLatticeQuiver[g_Graph] :=
  ExtendedGraph[
    VertexList @ g, edgeHead @@@ EdgeList[g],
    VertexCoordinates -> LookupOption[g, VertexCoordinates],
    userOpts,
    GraphTheme -> "EnumeratedLattice"
  ]

(**************************************************************************************************)

$enumeratedLatticeStyleOpts = {
  ImageSize -> {100, 100}, VertexSize -> 5,
  EdgeShapeFunction -> "StyledLine", EdgeColorFunction -> "Cardinal",
  EdgeThickness -> 2.5, EdgeStyle -> Opacity[0.7],
  VertexStyle -> $DarkGray, VertexSize -> 5, Frame -> True
  (* ArrowheadShape -> {"Line", EdgeThickness -> 2}, *)
};

$GraphThemeData["EnumeratedLattice"] := $enumeratedLatticeStyleOpts;
