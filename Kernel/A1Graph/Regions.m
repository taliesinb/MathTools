PackageExport["RegionSubgraph"]

SetUsage @ "
RegionSubgraph[graph$, region$] gives a subgraph of graph$ described by region$, which can be one or more:
<*$graphRegionTable*>
"

RegionSubgraph::empty = "The specified region is empty."

DeclareArgumentCount[RegionSubgraph, 2];

declareSyntaxInfo[RegionSubgraph, {_, _, OptionsPattern[]}];

RegionSubgraph[graph_, region_] := Scope[
  graph = CoerceToGraph[1];
  iRegionSubgraph[graph, region, False]
];

(**************************************************************************************************)

PackageExport["RegionDelete"]

SetUsage @ "
RegionDelete[graph$, region$] deletes the region specified by region$, which can be one or more of:
<*$graphRegionTable*>
"

RegionDelete[graph_, region_] := Scope[
  graph = CoerceToGraph[1];
  iRegionSubgraph[graph, region, True]
]

(**************************************************************************************************)

iRegionSubgraph[graph_, region_, comp_] := Scope[
  regionData = GraphScope[graph, RegionDataUnion @ processRegionSpec @ region];
  If[FailureQ[result], ReturnFailed[]];
  vertices =  Part[regionData, 1];
  edges = Part[regionData, 2];
  If[vertices === edges === {}, ReturnFailed[RegionSubgraph::empty]];
  If[comp,
    vertices = Complement[Range @ VertexCount @ graph, vertices];
    edges = Complement[Range @ EdgeCount @ graph, edges];
  ];
  vertices = Part[VertexList @ graph, vertices];
  edges = Part[EdgeList @ graph, edges];
  vertices = DeleteDuplicates @ Join[vertices, AllVertices @ edges];
  ExtendedSubgraph[graph, vertices, edges]
];

(**************************************************************************************************)

PackageExport["GraphRegion"]

SetUsage @ "
GraphRegion[graph$, region$] returns a list of %GraphRegionData and %GraphPathData objects, representing \
the computed regions of graph$.
<*$graphRegionTable*>
"

GraphRegion[graph_, region_] := Scope[
  graph = CoerceToGraph[1];
  GraphScope[graph, processRegionSpec @ region]
]

(**************************************************************************************************)

PackageExport["GraphPathVertices"]

SetUsage @ "
GraphPathVertices[graph$, path$] returns the list of vertices specified by path$.
GraphPathVertices[graph$, {path$1, path$2, $$}] returns a list for each path$i.
GraphPathVertices[graph$, <|key$1 -> path$1, $$|>] as above.
* path$ can be region specifications like %Path, %Line, %HalfLine, %InfiniteLine, etc.
"

GraphPathVertices[garph_, specs_] := Scope @ Catch[
  graph = CoerceToGraph[1];
  vertexList = VertexList[graph];
  GraphScope[graph,
    If[ListOrAssociationQ[specs],
      Map[
        spec |-> toPathVertices[processRegionSpec @ spec, spec],
        specs
      ],
      toPathVertices[processRegionSpec @ specs, specs]
    ]
  ]
,
  GraphPathVertices
];

toPathVertices[{GraphPathData[vertices_, _, _]}, _] :=
  Part[vertexList, vertices];

GraphPathVertices::notpath = "The specification `` is not a valid path."

toPathVertices[_, spec_] :=
  (Message[GraphPathVertices::notpath, spec]; Throw[$Failed, GraphPathVertices]);

(**************************************************************************************************)

PackageExport["GraphRegionCollection"]

SetUsage @ "
GraphRegionCollection[<|name$1 -> region$1, $$|>] represents a collection of named regions.
"

PackageExport["GraphRegionData"]

SetUsage @ "
GraphRegionData[vertices$, edges$] represents a region in a graph with vertex indices vertices$ \
and edge indices edges$.
"

PackageExport["GraphPathData"]

SetUsage @ "
GraphPathData[vertices$, edges$, inversions$] represents a path in a graph with vertex indices \
vertices$, edge indices edges$, and a list of indices into edges$ of which edges were traversed \
in their reverse direction.
"

PackageExport["GraphRegionAnnotation"]

SetUsage @ "
GraphRegionAnnotation[data$, anno$] is a wrapper around %GraphPathData and %GraphRegionData \
that attaches additional annotations anno$ for interpretation by GraphRegionHighlights etc.
"

$boxColor = GrayLevel[0.9];
declareBoxFormatting[
  g:GraphRegionData[v:{___Integer}, e:{___Integer}] :>
    Construct[InterpretationBox, skeletonBox["GraphRegionData", $boxColor, Length /@ {v, e}], g],
  GraphPathData[v:{__Integer}, e:{___Integer}, c:{___Integer}] :>
    Construct[InterpretationBox, skeletonBox["GraphPathData", $boxColor, Length /@ {v, e, c}], g]
];

colorBox[box_, color_] := StyleBox[box, Background -> color];

PackageScope["skeletonBox"]

skeletonBox[head_, color_, args_] :=
  RowBox @ Flatten @ {head, "[", Riffle[colorBox[skeletonString @ #, color]& /@ args, ","], "]"};

skeletonString[e_] := StringJoin["\[LeftAngleBracket]", TextString @ e, "\[RightAngleBracket]"];

regionDataListVertices[regionDataElements_] :=
  regionDataElements[[All, 1]];

regionDataListEdges[regionDataElements_] :=
  regionDataElements[[All, 2]];

pathToRegion[GraphPathData[a_, b_, c_]] :=
  GraphRegionData[a, b];

InvertPath[GraphPathData[a_, b_, c_]] := Scope[
  cn = Range @ Length @ b;
  GraphPathData[Reverse @ a, Reverse @ b, Complement[cn, c]]
];

(**************************************************************************************************)

GraphRegion::badcardinals = "The region ``[...] includes a path `` with invalid cardinals."

ParseRegionWord[word_] :=
  ToPathWord[word, $Cardinals, failAuto["badcardinals", word]]

$inRegionFunc = True;
checkCardinals[list_List] :=
  If[!ListQ[$Cardinals] || SubsetQ[$Cardinals, StripInverted /@ list], list,
    If[$inRegionFunc, failAuto["badcardinals", list], $Failed]];

(**************************************************************************************************)

PackageExport["TakePath"]

TakePath[GraphPathData[v_, e_, c_], n_] := Scope[
  v2 = Take[v, n]; e2 = Take[e, n];
  c2 = Take[SparseArray[Thread[c -> 1], Length @ e], n]["NonzeroPositions"] // Flatten;
  GraphPathData[v2, e2, c2]
];

(**************************************************************************************************)

PackageExport["GraphRegionElementQ"]

SetUsage @ "
GraphRegionElementQ[elem$] returns True if elem$ is an expression describing a graph region.
"

GraphRegionElementQ[_] := False;
GraphRegionElementQ[GraphOrigin] := True;
GraphRegionElementQ[IndexedVertex[_Integer]] := True;
GraphRegionElementQ[IndexedEdge[_Integer]] := True;

(**************************************************************************************************)

PackageExport["VertexPattern"]
PackageExport["EdgePattern"]

SetUsage @ "
VertexPattern[pattern$] represent a vertex that matches pattern$.
"

SetUsage @ "
EdgePattern[src$, dst$] represents an edge that matches src$ \[DirectedEdge] dst$.
"

(* zSetUsage @ "
Path[src$, {c$1, $$, c$n}] represents a path starting at src$ and taking cardinals c$i in tern.
Path[src$, 'cards$'] interpreted the characters of 'cards$' as cardinals.
"
 *)

PackageExport["IndexedVertex"]
PackageExport["IndexedEdge"]

SetUsage @ "
IndexedVertex[i$] represents the vertex with index i$.
"

SetUsage @ "
IndexedEdge[i$] represents the edge with index i$.
"

(**************************************************************************************************)

PackageExport["GraphRegionVertexEdgeList"]

SetUsage @ "
GraphRegionVertexEdgeList[graph$, region$] returns {vertices$, edges$} for region$.
GraphRegionVertexEdgeList[graph$, {r$1, r$2, $$}] returns {{v$1, v$2, $$}, {e$1, e$2, $$}}.
* The pair {$Failed, $Failed} is returned if an error occurs.
"

GraphRegionVertexEdgeList[graph_, region_] :=
  Match[
    GraphRegion[graph, region],
    GraphRegionData[v_, e_] :> {v, e},
    _ :> {$Failed, $Failed}
  ];

GraphRegionVertexEdgeList[graph_, region_List] :=
  Match[
    GraphRegion[graph, region],
    r:{___GraphRegionData} :> {Part[r, All, 1], Part[r, All, 2]},
    _ :> ConstantArray[$Failed, {2, Length @ region}]
  ]

(**************************************************************************************************)

PackageScope["processRegionVerticesEdges"]

processRegionVerticesEdges[spec_] := Scope[
  region = RegionDataUnion @ processRegionSpec[spec];
  If[!MatchQ[region, _GraphPathData | _GraphRegionData], Return @ {$Failed, $Failed}];
  {Part[region, 1], Part[region, 2]}
];

(**************************************************************************************************)

PackageScope["processRegionSpec"]

processRegionSpec[region_] := Scope[
  $VertexInEdgeTable := $VertexInEdgeTable = VertexInEdgeTable[$Graph];
  $VertexOutEdgeTable := $VertexOutEdgeTable = VertexOutEdgeTable[$Graph];
  $VertexAdjacencyTable := $VertexAdjacencyTable = VertexAdjacencyTable[$Graph];
  $TagVertexAdjacentEdgeTable := $TagVertexAdjacentEdgeTable = TagVertexAdjacentEdgeTable[$Graph, Signed -> True];
  $TagIndices := $TagIndices = TagIndices[$Graph];
  $EdgePairs := $EdgePairs = EdgePairs[$Graph];
  $Cardinals := $Cardinals = CardinalList[$Graph];
  Map[outerProcessRegion, ToList @ region]
]

outerProcessRegion[region_] := Scope[
  $currentRegionHead = Head[region];
  Catch[processRegion[region], outerProcessRegion]
];

(********************************************)
(** framework code                         **)
(********************************************)

failAuto[msgName_, args___] := (
  Message[MessageName[GraphRegion, msgName], $currentRegionHead, args];
  Throw[Nothing, outerProcessRegion]
);

fail[msgName_, args___] := (
  Message[MessageName[GraphRegion, msgName], args];
  Throw[Nothing, outerProcessRegion]
);

(********************************************)

SetHoldFirst[collectRegionData, collectPathData];

collectPathData[body_] := Scope[
  CollectTo[{$vertexBag, $edgeBag, $inversionBag}, body];
  GraphPathData[$vertexBag, $edgeBag, $inversionBag]
];

(********************************************)

sowVertex[v_] := Internal`StuffBag[$vertexBag, v];

sowVertexList[v_] := Internal`StuffBag[$vertexBag, v, 1];

sowEdge[i_Integer] := (Internal`StuffBag[$edgeBag, i]; True);

sowEdge[Inverted[i_Integer]] := (
  Internal`StuffBag[$edgeBag, i];
  Internal`StuffBag[$inversionBag, Internal`BagLength[$edgeBag]];
  True
);

sowEdge[_] := False;

sowEdgeList[i_List] := Internal`StuffBag[$edgeBag, i, 1];

(********************************************)

SetHoldRest[findStrictEdge];
findStrictEdge[v1_, v2_, else_:None] := First[
  Intersection[
    Part[$VertexInEdgeTable, v2],
    Part[$VertexOutEdgeTable, v1]
  ],
  else
];

(********************************************)

findEdge[v1_, v2_] := findStrictEdge[v1, v2, Inverted @ findStrictEdge[v2, v1]]

findAndSowEdge[v1_, v2_] := sowEdge @ findEdge[v1, v2];

(********************************************)

GraphRegion::nfvertex = "No vertex matching `` was found in the graph."
GraphRegion::nfedge = "No edge matching `` was found in the graph.";
GraphRegion::malformedrspec = "The region specification `` was malformed.";


(********************************************)
(** literal vertices and edges             **)

$regionHeads = Alternatives[
  Disk, Circle, Annulus, Line, HalfLine, InfiniteLine, Path, Polygon,
  EdgePattern, VertexPattern,
  GraphRegionBoundary, GraphRegionComplement, GraphRegionUnion, GraphRegionIntersection
];

processRegion[spec_] := If[MatchQ[Head @ spec, $regionHeads],
  fail["malformedrspec", spec],
  GraphRegionData[{findVertex @ spec}, {}]
];

processRegion[IndexedEdge[i_Integer]] /; 1 <= i <= $EdgeCount :=
  edgeIndicesToPathData @ {i};

(********************************************)
(** edge pattern                           **)

processRegion[assoc_Association] :=
  NamedGraphRegionData @ Map[processRegion, assoc];

(********************************************)

GraphRegionElementQ[e:_[___, GraphMetric -> _]] := GraphRegionElementQ @ Most @ e;
GraphRegionElementQ[_Rule | _TwoWayRule | _DirectedEdge | _UndirectedEdge] := True;

processRegion[spec:((Rule|TwoWayRule|DirectedEdge|UndirectedEdge)[l_, r_])] := Scope[
  e = findEdge[findVertex @ l, findVertex @ r];
  If[!IntegerQ[e], fail["nfedge", spec]];
  edgeIndicesToPathData @ {e}
];

processRegion[DirectedEdge[a_, b_, c_]] :=
  edgeIndicesToPathData @ findEdgeIndices @ verbatimEdgePattern[a, b, c]

processRegion[UndirectedEdge[a_, b_, c_]] :=
  edgeIndicesToPathData @ StripInverted @ findEdgeIndices @ verbatimEdgePattern[a, b, c]

verbatimEdgePattern[a_, b_, c_] :=
  Verbatim /@ EdgePattern[a, b, c];

(********************************************)
(** edge pattern                           **)

GraphRegionElementQ[EdgePattern[_, _, _] | EdgePattern[_, _]] := True;

processRegion[p:EdgePattern[_, _, ___]] :=
  edgeIndicesToPathData @ findEdgeIndices[p]

processRegion[EdgePattern[a:(_IndexedVertex | GraphOrigin), b_, rest__]] :=
  processRegion @ EdgePattern[Part[$VertexList, findVertex @ a], b, rest];

processRegion[EdgePattern[a_, b:(_IndexedVertex | GraphOrigin), rest___]] :=
  processRegion @ EdgePattern[a, Part[$VertexList, findVertex @ b], rest];

processRegion[EdgePattern[a_, b_, Inverted[c_]]] :=
  edgeIndicesToPathData @ Map[Inverted, findEdgeIndices @ EdgePattern[a, b, c]];

findEdgeIndices[p:EdgePattern[a_, b_]] := Scope @ Which[
  NotEmptyQ[i = MatchIndices[$EdgeList, DirectedEdge[a, b, ___]]], i,
  NotEmptyQ[i = MatchIndices[$EdgeList, DirectedEdge[b, a, ___]]], Inverted /@ i,
  NotEmptyQ[i = MatchIndices[$EdgeList, UndirectedEdge[a, b, ___] | UndirectedEdge[b, a, ___]]], i,
  True, fail["nfedge", p]
];

findEdgeIndices[p:EdgePattern[a_, b_, c_]] := Scope @ With[{cp = toCardinalPattern @ c}, Which[
  NotEmptyQ[i = MatchIndices[$EdgeList, DirectedEdge[a, b, cp]]], i,
  NotEmptyQ[i = MatchIndices[$EdgeList, DirectedEdge[b, a, cp]]], Inverted /@ i,
  NotEmptyQ[i = MatchIndices[$EdgeList, UndirectedEdge[a, b, cp] | UndirectedEdge[b, a, c]]], i,
  True, fail["nfedge", p]
]];

supersetOf[sub_][sup_] := SubsetQ[sup, sub];

toCardinalPattern = MatchValues[
  CardinalSet[s_] := CardinalSet[_ ? (supersetOf[s])];
  a_Alternatives  := Map[%, a];
  s_              := s | CardinalSet[_ ? (MemberQ[s | Inverted[s]])];
];

edgeIndicesToPathData[edgeIndices_] := Scope[
  normalEdgeIndices = StripInverted /@ edgeIndices;
  GraphPathData[
    allEdgeVertices @ normalEdgeIndices,
    normalEdgeIndices,
    MatchIndices[edgeIndices, _Inverted]
  ]
];

allEdgeVertices[edgeIndices_] :=
  Union @ AllVertices @ Part[$IndexGraphEdgeList, edgeIndices];


(**************************************************************************************************)

GraphRegionElementQ[_ChartSymbol] := True;

processRegion[ChartSymbol[s_, comp_:All]] :=
  processRegion @ ChartRegion[ParseRegionWord @ StringDelete[s, "+"|"-"], comp];

(**************************************************************************************************)

PackageExport["ChartRegion"]

SetUsage @ "
ChartRegion[{c$1, c$2, $$}] represents the domain of a compass with cardinals c$i.
ChartRegion[cardinals$, i$] represents the i$'th connected component.
* The domain is the connected subgraph induced by the vertices that have all of the c$i.
"

GraphRegionElementQ[ChartRegion[_List, ___]] := True;

processRegion[ChartRegion[cards_List, i_:All]] := Scope[
  edgeLists = Map[$TagIndices, cards];
  overlappingEdges = Union @@ Intersection @@@ Subsets[edgeLists, {2, Infinity}];
  edgeLists = Complement[#, overlappingEdges]& /@ edgeLists;
  vertices = Intersection @@ Map[allEdgeVertices, edgeLists];
  edges = Union @@ edgeLists;
  {vertices, subgraphEdges} = FirstLast @ subgraphRegionData @ vertices;
  edges = Intersection[subgraphEdges, edges];
  If[i =!= All,
    vertices = Part[WeaklyConnectedComponents[Subgraph[$IndexGraph, vertices]], i];
    edgePairs = EdgePairs @ $IndexGraph;
    edges //= Select[MemberQ[vertices, Part[edgePairs, #, 1] | Part[edgePairs, #, 2]]&];
  ];
  GraphRegionData[vertices, edges]
];


(********************************************)
(** Point                                  **)

GraphRegionElementQ[Point[_]] := True;

processRegion[Point[v_]] :=
  GraphRegionData[List @ findVertex @ v, {}];


(********************************************)
(** vertex pattern                         **)

GraphRegionElementQ[VertexPattern[_]] := True;

vertexPatternQ[v_] := ContainsQ[v, Pattern | Alternatives | Blank];

processRegion[lv_LatticeVertex ? vertexPatternQ] :=
  processRegion @ VertexPattern @ lv;

processRegion[p:VertexPattern[v_]] := Scope[
  indices = MatchIndices[$VertexList, v];
  If[indices === {}, fail["nfvertex", p]];
  GraphRegionData[indices, {}]
];


(********************************************)
(** complex region specifications          **)
(********************************************)

processRegion[list_List /; VectorQ[list, GraphRegionElementQ]] :=
  RegionDataUnion @ Map[processRegion, list];

(********************************************)

PackageScope["findVertexIndex"]

findVertexIndex[e_] := Block[{failAuto = Function[$Failed]}, findVertex @ e];

GraphRegion::invv = "The region ``[...] contained an invalid vertex specification ``.";

findVertex[GraphOrigin] := findVertex[$GraphOrigin];

findVertex[IndexedVertex[i_Integer]] /; 1 <= i <= $VertexCount := i;

findVertex[RandomPoint] := RandomInteger[{1, $VertexCount}];

findVertex[Offset[v_, path_]] := offsetWalk[findVertex @ v, path];

findVertex[spec_] := Lookup[$VertexIndex, Key[spec],
  failAuto["invv", spec]];

findVertex[lv_LatticeVertex ? vertexPatternQ] :=
  findVertex @ VertexPattern @ lv;

findVertex[p:VertexPattern[v_]] :=
  FirstIndex[$VertexList, v, failAuto["invv", p]];

GraphRegion::notlist = "The region ``[...] required a list of vertices, but got `` instead."

findVertices[spec_] := Scope[
  res = Lookup[$VertexIndex, spec, $Failed];
  If[FreeQ[res, $Failed], res, resolveComplexVertexList @ spec]
];

resolveComplexVertexList[spec_] := Which[
  vertexPatternQ[spec], MatchIndices[$VertexList, spec],
  ListQ[spec] && !EmptyQ[spec], Map[findVertex, spec],
  True, failAuto["notlist", spec]
];

(********************************************)
(** WeightedData                           **)

PackageExport["Weighted"]

processRegion[Weighted[region_, weight_]] :=
  GraphRegionAnnotation[
    If[ListQ[region], processRegion /@ region, processRegion @ region],
    <|"Weight" -> weight|>
  ];

(********************************************)
(** GraphRegionData|GraphPathData[...]     **)

GraphRegionElementQ[GraphRegionData[_List, _List]] := True;
GraphRegionElementQ[GraphPathData[_List, _List, _List]] := True;

processRegion[g_GraphRegionData] := g;
processRegion[g_GraphPathData] := g;

(********************************************)

$metricRegionHeads = Alternatives[
  Line, Disk, Annulus, Circle, HalfLine, InfiniteLine, Polygon, StarPolygon, Path
];

processRegion[spec:$metricRegionHeads[__, GraphMetric -> metric_]] := Scope[
  $GraphMetric = metric;
  processRegion @ Most @ spec
];

(********************************************)
(** Path[...]                              **)

GraphRegionElementQ[Path[_, _]] := True;
GraphRegionElementQ[Path[_, _, Repeated[(PathAdjustments|PathCancellation) -> _]]] := True;

processRegion[Path[start_, path_]] :=
  collectPathData @ sowPath[start, path, False];

processRegion[Path[l__, PathCancellation -> bool_, r___]] :=
  Block[{$pathCancellation = bool}, processRegion[Path[l, r]]];

processRegion[Path[args__, ps:Rule[PathAdjustments, _]]] :=
  GraphRegionAnnotation[processRegion @ Path @ args, Association @ ps];

PackageExport["PathAdjustments"]

SetUsage @ "
PathAdjustments is an option to Path that specifies which steps to foreshorten.
"

PackageExport["PathCancellation"]

SetUsage @ "
PathCancellation is an option to Path that specifies whether to cancel neighboring inverted cardinals.
"

(********************************************)

sowPath[start_, path_, repeating_, stop_:None] := Scope[
  startId = findVertex @ start;
  stopId = If[stop === None, None, findVertex @ stop];
  pathWord = ParseRegionWord @ path;
  sowVertex[startId];
  doWalk[
    startId, stopId, pathWord, repeating,
    {vertex, edge} |-> (
      sowVertex[vertex];
      sowEdge[edge];
    )
  ];
];

(********************************************)

GraphRegion::notdir = "The region ``[...] includes a path ``, but paths cannot be defined on undirected graphs."

GraphRegion::nocard = "The region ``[...] specified a cardinal '``' path step at vertex ``, but the only available \
cardinals are: ``"

doWalk[startId_, stopId_, pathWord_, shouldRepeat_, func_] := Scope[
  If[!DirectedGraphQ[$Graph], failAuto["notdir", path]];
  wordLen = Length @ pathWord;
  vertexId = startId;
  totalLen = If[shouldRepeat, 10^6, wordLen];
  Do[
    cardinal = Part[pathWord, Mod[i, wordLen, 1]];
    invertedQ = InvertedQ[cardinal];
    edgeId = Part[$TagVertexAdjacentEdgeTable, Key @ cardinal, vertexId];
    If[InvertedQ[edgeId], edgeId //= First; invertedQ = True];
    If[edgeId === None,
      If[shouldRepeat, Break[]];
      failWalk[cardinal, vertexId]];
    vertexId = Part[$EdgePairs, edgeId, If[invertedQ, 1, 2]];
    func[vertexId, If[invertedQ, Inverted @ edgeId, edgeId]];
    If[vertexId == stopId, Break[]];
    If[vertexId == startId && shouldRepeat, Break[]];
  ,
    {i, 1, totalLen}
  ];
  vertexId
];

failWalk[cardinal_, vertexId_] := Scope[
  available = Join[
    Part[$EdgeTags, Part[$VertexOutEdgeTable, vertexId]],
    Inverted /@ Part[$EdgeTags, Part[$VertexInEdgeTable, vertexId]]
  ];
  failAuto["nocard", cardinal, Part[$VertexList, vertexId], available];
];

(********************************************)

offsetWalk[startId_, path_] := Scope[
  pathWord = ParseRegionWord @ path;
  doWalk[startId, None, pathWord, False, Null&]
];


(********************************************)
(** Cycles[...]                            **)

GraphRegionElementQ[Cycles[_List] | Cycles[_String]] := True;

processRegion[Cycles[word_]] := Scope[
  word //= ParseRegionWord;
  If[word === {}, Return @ Splice[GraphPathData[{#}, {}, {}]& /@ Range[$VertexCount]]];
  init = First @ word;
  edges = Lookup[$TagIndices, init];
  startVertices = Sort @ Part[$EdgePairs, edges, 1];
  cycles = {};
  While[startVertices =!= {},
    startVertex = First @ startVertices;
    cycle = processRegion @ Line[IndexedVertex /@ {startVertex, startVertex}, word];
    pathVertices = First @ cycle;
    {vertexFirst, vertexLast} = FirstLast @ pathVertices;
    If[vertexFirst === vertexLast,
      AppendTo[cycles, cycle];
    ];
    startVertices = Complement[startVertices, pathVertices];
  ];
  Splice @ cycles
];

(********************************************)
(** Inverted[path...]                       **)

GraphRegionElementQ[Inverted[_ ? GraphRegionElementQ]] := True;

processRegion[spec:Inverted[region_]] := Scope[
  paths = ToList @ processRegion @ region;
  If[!MatchQ[paths, {__GraphPathData}], fail["badnegpath", spec]];
  InvertPath /@ paths
]

GraphRegion::badnegpath = "Error in ``: can only invert a path or set of paths.";

(********************************************)
(** Line[...]                              **)

GraphRegionElementQ[Line[_List] | Line[_List, _]] := True;

processRegion[Line[{vertex_}]] :=
  collectPathData @ sowVertex @ findVertex @ vertex;

processRegion[Line[vertices_]] := Scope[
  vertices //= findVertices;
  collectPathData[
    sowVertex @ First @ vertices;
    ApplyWindowed[findAndSowGeodesic, vertices];
  ]
];

processRegion[Line[vertices_, None]] :=
  processRegion @ Line @ vertices;

processRegion[Line[{start_, stop_}, c_]] :=
  collectPathData @ sowPath[start, c, True, stop];

findAndSowGeodesic[v1_, v2_] := Scope[
  geodesicVertices = MetricFindShortestPath[$MetricGraphCache, v1, v2, GraphMetric -> $GraphMetric];
  sowVertexList @ Rest @ geodesicVertices;
  ApplyWindowed[findAndSowEdge, geodesicVertices]
];


(********************************************)
(** HalfLine[...]                              **)

GraphRegionElementQ[HalfLine[{_, _}] | HalfLine[_, _]] := True;

(* we must work around an automatic rewriting that HalfLine does here *)
processRegion[HalfLine[{v1_, v2_}] | HalfLine[{0, 0}, {v1_, v2_}]] := Scope[
  v1 //= findVertex; v2 //= findVertex;
  word = findWordBetween[v1, v2];
  processRegion @ HalfLine[v1, word]
];

processRegion[HalfLine[v_, dir_]] :=
  collectPathData @ sowPath[v, dir, True];

findWordBetween[v1_, v2_] := Scope[
  geodesicVertices = MetricFindShortestPath[$MetricGraphCache, v1, v2, GraphMetric -> $GraphMetric];
  ApplyWindowed[findCardinalBetween, geodesicVertices]
];

GraphRegion::grinterror = "An internal error occurred while procession region ``[...].";

findCardinalBetween[v1_, v2_] := Scope[
  edgeIndex = findEdge[v1, v2];
  If[edgeIndex === None, failAuto["grinterror"]];
  If[InvertedQ[edgeIndex],
    Inverted @ Part[$EdgeTags, StripInverted @ edgeIndex],
    Part[$EdgeTags, edgeIndex]
  ]
];

(********************************************)
(** InfiniteLine[...]                      **)

GraphRegionElementQ[InfiniteLine[{_, _}] | InfiniteLine[_, _]] := True;

processRegion[InfiniteLine[v_, dir_]] := Scope[
  cardinalWord = ParseRegionWord @ dir;
  {posVerts, posEdges, posInversions} = List @@ processRegion @ HalfLine[v, cardinalWord];
  If[Length[posVerts] > 0 && Last[posVerts] === First[posVerts],
    Return @ GraphPathData[posVerts, posEdges, {}]];
  {negVerts, negEdges, negInversions} = List @@ processRegion @ HalfLine[v, Inverted /@ Reverse @ cardinalWord];
  negEdgeLen = Length[negEdges];
  GraphPathData[
    Join[Reverse @ Rest @ negVerts, posVerts],
    Join[Reverse @ negEdges, posEdges],
    Join[Complement[Range @ negEdgeLen, negEdgeLen + 1 - negInversions], negEdgeLen + posInversions]
  ]
];

(********************************************)
(** Polygon[...]                           **)

GraphRegionElementQ[Polygon[_List]] := True;

processRegion[Polygon[vertices_]] := Scope[
  vertices = findVertices @ vertices;
  sowVertex @ First @ vertices;
  collectPathData[
    findAndSowGeodesic @@@ Partition[vertices, 2, 1, 1]
  ]
];

(********************************************)
(** Locus[...]                             **)

PackageExport["Locus"]

SetUsage @ "
Locus[r$1, r$2] represents the locus of points that are equally distance from \
regions r$1 and r$2.
Locus[r$1, r$2, \[CapitalDelta]] represents a 'thickened' locus that allows the \
two distances to differ by up to \[CapitalDelta].
"

GraphRegionElementQ[Locus[_ ? GraphRegionElementQ, _ ? GraphRegionElementQ]] := True;
GraphRegionElementQ[Locus[_ ? GraphRegionElementQ, _ ? GraphRegionElementQ, (_ ? NumericQ) | "Polar"]] := True;

processRegion[Locus[r1_, r2_, "Polar"]] :=
  processRegion[Locus[r1, r2, -1]];

processRegion[l:Locus[r1_, r2_, d_:0 ? NumericQ]] := Scope[
  r1 = First @ processRegion @ r1;
  r2 = First @ processRegion @ r2;
  If[r1 === {} || r2 === {}, fail["emptyarea", l]];
  d1 = extractDistanceToRegion @ r1;
  d2 = extractDistanceToRegion @ r2;
  isPolar = d === -1; d = Max[d, 0];
  indices = SelectIndices[
    Transpose @ {d1, d2},
    Apply[Abs[#1 - #2] <= d&]
  ];
  If[isPolar,
    d3 = d1 - d2;
    indices1 = SelectIndices[d3, Positive];
    indices2 = SelectIndices[d3, Negative];
    indices = Union[
      indices,
      Select[indices1, IntersectingQ[Part[$VertexAdjacencyTable, #], indices2]&]
    ];
  ];
  If[indices === {}, fail["emptyarea", l]];
  subgraphRegionData[indices]
];

(*
(* this appears to be slower! *)
extractDistanceToRegion[{v_}] :=
  MetricDistance[$MetricGraphCache, v, All, GraphMetric -> $GraphMetric];
*)
extractDistanceToRegion[v_List] :=
  Min /@ Part[MetricDistanceMatrix[$MetricGraphCache, GraphMetric -> $GraphMetric], All, v];

(********************************************)
(** Disk[...]                              **)

GraphRegionElementQ[Disk[_, _ ? NumericQ]] := True;

processRegion[d:Disk[center_, r_ ? NumericQ]] :=
  circularRegionData[d, center, LessEqualThan[r]];


(********************************************)
(** Annulus[...]                           **)

GraphRegionElementQ[Annulus[_, {_ ? NumericQ, _ ? NumericQ}]] := True;

processRegion[a:Annulus[center_, {r1_ ? NumericQ, r2_ ? NumericQ}]] :=
  circularRegionData[a, center, Between[{r1, r2}]];


(********************************************)
(** Circle[...]                            **)

GraphRegionElementQ[Circle[_, _ ? NumericQ]] := True;

processRegion[c:Circle[center_, r_ ? NumericQ]] :=
  circularRegionData[c, center, ApproxEqualTo[r]];

ApproxEqualTo[e_][r_] := Abs[e - r] < 0.5;

(********************************************)

GraphRegion::emptyarea = "The area defined by `` contains no points."

extendedConditionQ[cond_] := ContainsComplexQ[cond] || ContainsNegativeQ[cond];

complexToVector[z_] := AngleVector @ AbsArg @ z;

leftOf[z_][e_] := Dot[z - e, z] >= 0;
rightOf[z_][e_] := Dot[z - e, z] <= 0;
andOperator[f_, g_][e_] := f[e] && g[e];

toComplexCond = MatchValues[
  LessEqualThan[n_] := leftOf @ complexToVector @ n;
  ApproxEqualTo[n_] := ApproxEqualTo[complexToVector @ n];
  Between[{a_, b_}] := andOperator[leftOf @ complexToVector @ a, rightOf @ complexToVector @ b];
];

circularRegionData[spec_, center_, condition_] := Scope[
  centerInd = findVertex @ center;
  distances = MetricDistance[$MetricGraphCache, centerInd, All, GraphMetric -> $GraphMetric];
  cond = condition;
  Which[
    ContainsComplexQ[distances] && !extendedConditionQ[cond],
      distances //= Re,
    extendedConditionQ[cond],
      cond //= toComplexCond;
      distances //=  Map[complexToVector],
    True,
      Null
  ];
  vertices = SelectIndices[distances, cond];
  If[vertices === {}, fail["emptyarea", spec]];
  subgraphRegionData @ vertices
];

subgraphRegionData[vertices_] := Scope[
  forward = Flatten @ Part[$VertexOutEdgeTable, vertices];
  backward = Flatten @ Part[$VertexInEdgeTable, vertices];
  candidates = Intersection[forward, backward];
  vertexAssoc = ConstantAssociation[vertices, True];
  edges = Select[candidates, Apply[And, Lookup[vertexAssoc, Part[$EdgePairs, #]]]&];
  GraphRegionData[
    vertices,
    edges
  ]
];

(********************************************)
(** GraphRegionBoundary[...]               **)

PackageExport["GraphRegionBoundary"]

SetUsage @ "
GraphRegionBoundary[region$] represents the boundary of region$.
"

GraphRegionElementQ[GraphRegionBoundary[_]] := True;

processRegion[GraphRegionBoundary[region_]] := Scope[
  vertices = First @ processRegion @ region;
  complement = Complement[Range @ $VertexCount, vertices];
  edgeVertices = Select[vertices, IntersectingQ[Part[$VertexAdjacencyTable, #], complement]&];
  subgraphRegionData @ edgeVertices
];


(********************************************)
(** ConnectedEdges[...]                    **)

PackageExport["ConnectedSubgraph"]

SetUsage @ "
ConnectedSubgraph[region$] represents region$ extended by all edges that connect vertices within region$.
"

GraphRegionElementQ[ConnectedSubgraph[r_]] := AllTrue[ToList @ r, GraphRegionElementQ];

processRegion[ConnectedSubgraph[region_]] := Scope[
  region = RegionDataUnion @ Map[processRegion, ToList @ region];
  vertices = First @ region;
  RegionDataUnion[{region, subgraphRegionData @ vertices}]
];

(********************************************)
(** GraphRegionComplement[...]             **)

PackageExport["GraphRegionComplement"]

SetUsage @ "
GraphRegionComplement[r$, c$1, c$2, $$] represents the complement of region r$ with the c$i.
"

GraphRegionElementQ[GraphRegionComplement[_, ___]] := True;

processRegion[GraphRegionComplement[regions__]] :=
  RegionDataComplement @ Map[processRegion, {regions}];

(********************************************)

RegionDataComplement[{a_}] := a;

RegionDataComplement[{a_, b_, c__}] :=
  RegionDataComplement @ {a, RegionDataUnion[{b, c}]};

RegionDataComplement[e:{_, _}] := Scope[
  {va, vb} = regionDataListVertices /@ e;
  {ea, eb} = regionDataListEdges /@ e;
  danglingEdges = Pick[ea, MemberQ[vb, #1 | #2]& @@@ Part[$IndexGraphEdgeList, ea]];
  GraphRegionData[
    Complement[va, vb],
    Complement[ea, eb, danglingEdges]
  ]
]


(********************************************)
(** GraphRegionIntersection[...]           **)

PackageExport["GraphRegionIntersection"]

SetUsage @ "
GraphRegionIntersection[r$1, r$2, $$] represents the intersection of regions r$i.
"

declareFormatting[
  GraphRegionIntersection[r___] :> Row[{r}, Style["\[ThinSpace]\[Intersection]\[ThinSpace]", ScriptSizeMultipliers -> 1]]
];

GraphRegionElementQ[GraphRegionIntersection[___]] := True;

processRegion[GraphRegionIntersection[regions__]] :=
  RegionDataIntersection @ Map[processRegion, {regions}];

(********************************************)

RegionDataIntersection[{a_}] := a;

RegionDataIntersection[list_List] :=
  GraphRegionData[
    Intersection @@ regionDataListVertices @ list,
    Intersection @@ regionDataListEdges @ list
  ]


(********************************************)
(** GraphRegionUnion[...]                  **)

PackageExport["GraphRegionUnion"]

declareFormatting[
  GraphRegionUnion[r___] :> Row[{r}, " \[Union] "]
];


SetUsage @ "
GraphRegionUnion[r$1, r$2, $$] represents the union of regions r$i.
"

GraphRegionElementQ[GraphRegionUnion[___]] := True;

processRegion[GraphRegionUnion[regions__]] :=
  RegionDataUnion @ Map[processRegion, {regions}];

(********************************************)

PackageScope["RegionDataUnion"]

RegionDataUnion[{a_}] := a;

RegionDataUnion[assoc_Association] :=
  RegionDataUnion @ Values @ assoc;

RegionDataUnion[list_List] :=
  GraphRegionData[
    Union @@ regionDataListVertices @ list,
    Union @@ regionDataListEdges @ list
  ]
