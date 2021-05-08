Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

PackageExport["RegionSubgraph"]

SetUsage @ "
RegionSubgraph[graph$, region$] gives a subgraph of graph$ described by region$, which can be one or more of the following:
<*$GraphRegionTable*>
"

RegionSubgraph::empty = "The specified region is empty."

DeclareArgumentCount[RegionSubgraph, 2];

declareSyntaxInfo[RegionSubgraph, {_, _, OptionsPattern[]}];

RegionSubgraph[graph_, region_] := Scope[
  graph = CoerceToGraph[1];
  regionData = GraphScope[graph, RegionDataUnion @ processRegionSpec @ region];
  If[FailureQ[result], ReturnFailed[]];
  vertices = Part[regionData, 1];
  edges = Part[regionData, 2];
  If[vertices === edges === {}, ReturnFailed["empty"]];
  vertices = DeleteDuplicates @ Join[vertices, AllVertices @ edges];
  ExtendedSubgraph[graph, vertices, edges]
];

(**************************************************************************************************)

PackageExport["GraphRegion"]

SetUsage @ "
GraphRegion[graph$, region$] returns a list of GraphRegionData and GraphPathData objects, which represent \
the results of computing regions on the graph, according to:
<*$GraphRegionTable*>
"

GraphRegion[graph_, region_] := Scope[
  graph = CoerceToGraph[1];
  GraphScope[graph, processRegionSpec @ region]
]

(**************************************************************************************************)

PackageExport["GraphRegionData"]

SetUsage @ "
GraphRegionData[vertices$, edges$] represents a region in a graph with vertex indices vertices$ \
and edge indices edges$.
"

PackageExport["GraphPathData"]

SetUsage @ "
GraphPathData[vertices$, edges$, negations$] represents a path in a graph with vertex indices \
vertices$, edge indices edges$, and a list of indices into edges$ of which edges were traversed \
in their reverse direction.
"

$boxColor = GrayLevel[0.9];
declareBoxFormatting[
  GraphRegionData[v:{___Integer}, e:{___Integer}] :>
    skeletonBox["GraphRegionData", $boxColor, Length /@ {v, e}],
  GraphPathData[v:{__Integer}, e:{___Integer}, {___Integer}] :>
    skeletonBox["GraphRegionData", $boxColor, Length /@ {v, e}]
];

colorBox[box_, color_] := StyleBox[box, Background -> color];

skeletonBox[head_, color_, args_] :=
  RowBox @ Flatten @ {head, "[", Riffle[colorBox[skeletonString @ #, color]& /@ args, ","], "]"};

skeletonString[e_] := StringJoin["\[LeftAngleBracket]", TextString @ e, "\[RightAngleBracket]"];

regionDataListVertices[regionDataElements_] :=
  regionDataElements[[All, 1]];

regionDataListEdges[regionDataElements_] :=
  regionDataElements[[All, 2]];

pathToRegion[GraphPathData[a_, b_, c_]] :=
  GraphRegionData[a, b];

(**************************************************************************************************)

PackageExport["GraphRegionElementQ"]

SetUsage @ "
GraphRegionElementQ[elem$] returns True if elem$ is an expression describing a graph region.
"

(**************************************************************************************************)

PackageExport["VertexPattern"]
PackageExport["EdgePattern"]
PackageExport["Path"]

SetUsage @ "
VertexPattern[pattern$] represent a vertex that matches pattern$.
"

SetUsage @ "
EdgePattern[src$, dst$] represents an edge that matches src$ \[DirectedEdge] dst$.
"

zSetUsage @ "
Path[src$, {c$1, $$, c$n}] represents a path starting at src$ and taking cardinals c$i in tern.
Path[src$, 'cards$'] interpreted the characters of 'cards$' as cardinals.
"

(**************************************************************************************************)

PackageScope["processRegionSpec"]

processRegionSpec[region_] :=
  Map[outerProcessRegion, Developer`ToList @ region]

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

(* collectRegionData[body_] := Scope[
  CollectTo[{$vertexBag, $edgeBag}, body];
  GraphRegionData[Union @ $vertexBag, Union @ $edgeBag]
];
 *)
collectPathData[body_] := Scope[
  CollectTo[{$vertexBag, $edgeBag, $negationBag}, body];
  GraphPathData[Union @ $vertexBag, $edgeBag, $negationBag]
];

(********************************************)

sowVertex[v_] := Internal`StuffBag[$vertexBag, v];

sowVertexList[v_] := Internal`StuffBag[$vertexBag, v, 1];

sowEdge[i_Integer] := (Internal`StuffBag[$edgeBag, i]; True);

sowEdge[Negated[i_Integer]] := (
  Internal`StuffBag[$edgeBag, i];
  Internal`StuffBag[$edgeNegationBag, Internal`BagLength[$edgeBag]];
  True
);

sowEdge[_] := False;

sowEdgeList[i_List] := Internal`StuffBag[$edgeBag, i, 1];

(********************************************)

findStrictEdgePattern[a_, b_, c_] := Scope @ Which[
  IntegerQ[i = FirstIndex[$GraphEdgeList, DirectedEdge[a, b, c]]], i,
  IntegerQ[i = FirstIndex[$GraphEdgeList, UndirectedEdge[a, b, c] | UndirectedEdge[b, a, c]]], i,
  True, $Failed
];

findStrictEdgePattern[a_, b_, Negated[c_]] :=
  Negated @ findStrictEdgePattern[b, a, c];

(********************************************)

SetHoldRest[findStrictEdge];
findStrictEdge[v1_, v2_, else_:None] := First[
  Intersection[
    Part[$GraphVertexInEdgeTable, v2],
    Part[$GraphVertexOutEdgeTable, v1]
  ],
  else
];

(********************************************)

findEdge[v1_, v2_] := findStrictEdge[v1, v2, Negated @ findStrictEdge[v2, v1]]

findAndSowEdge[v1_, v2_] := sowEdge @ findEdge[v1, v2];

(********************************************)

GraphRegion::nfvertex = "No vertex matching `` was found in the graph."
GraphRegion::nfedge = "No edge matching `` was found in the graph.";
GraphRegion::malformedrspec = "The region specification `` was malformed.";


(********************************************)
(** literal vertices and edges             **)

$regionHeads = Alternatives[
  Disk, Circle, Annulus, Line, HalfLine, InfiniteLine, Path, StarPolygon, Polygon,
  EdgePattern, VertexPattern,
  RegionComplement, RegionUnion, RegionIntersection
];

processRegion[spec_] := If[MatchQ[Head @ spec, $regionHeads],
  fail["malformedrspec", spec],
  GraphRegionData[{findVertex @ spec}, {}]
];

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
  edgeIndicesToPathData @ StripNegated @ findEdgeIndices @ verbatimEdgePattern[a, b, c]

verbatimEdgePattern[a_, b_, c_] :=
  Verbatim /@ EdgePattern[a, b, c];

(********************************************)
(** edge pattern                           **)

GraphRegionElementQ[EdgePattern[_, __]] := True;

processRegion[p:EdgePattern[___]] :=
  edgeIndicesToPathData @ findEdgeIndices[p]

processRegion[EdgePattern[a_, b_, Negated[c_]]] :=
  edgeIndicesToPathData @ findEdgeIndices @ Map[Negated, EdgePattern[a, b, c]];

findEdgeIndices[p:EdgePattern[a_, b_, c___]] := Scope[
  Which[
    Developer`NotEmptyQ[i = MatchIndices[$GraphEdgeList, DirectedEdge[a, b, c]]],
      i,
    Developer`NotEmptyQ[i = MatchIndices[$GraphEdgeList, DirectedEdge[b, a, c]]],
      Negated /@ i,
    Developer`NotEmptyQ[i = MatchIndices[$GraphEdgeList, UndirectedEdge[a, b, c] | UndirectedEdge[b, a, c]]],
      i,
    True,
      fail["nfedge", p]
  ]
];

edgeIndicesToPathData[indices_] :=
  GraphPathData[
    Union @ AllVertices @ Part[$IndexGraphEdgeList, indices],
    StripNegated /@ indices,
    MatchIndices[indices, _Negated]
  ]


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
  indices = MatchIndices[$GraphVertexList, v];
  If[indices === {}, fail["nfvertex", p]];
  GraphRegionData[indices, {}]
];


(********************************************)
(** complex region specifications          **)
(********************************************)

processRegion[list_List /; VectorQ[list, GraphRegionElementQ]] :=
  RegionDataUnion @ Map[processRegion, list];

(********************************************)

GraphRegion::invv = "The region ``[...] contained an invalid vertex specification ``.";

findVertex[spec_] := Lookup[$GraphVertexIndices, Key[spec],
  failAuto["invv", spec]];

findVertex[lv_LatticeVertex ? vertexPatternQ] :=
  findVertex @ VertexPattern @ lv;

findVertex[p:VertexPattern[v_]] :=
  FirstIndex[$GraphVertexList, v, failAuto["invv", p]];

GraphRegion::notlist = "The region ``[...] required a list of vertices, but got `` instead."

findVertices[spec_] := Lookup[$GraphVertexIndices, spec, resolveComplexVertexList @ spec];

resolveComplexVertexList[spec_] := Which[
  vertexPatternQ[spec], MatchIndices[$GraphVertexList, spec],
  ListQ[spec] && !Developer`EmptyQ[spec], Map[findVertex, spec],
  True, failAuto["notlist", spec]
];


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

parseCardinalWord[path_String] /; StringLength[path] > 1 := Scope[
  chars = Characters[path];
  str = StringReplace[StringRiffle[chars, " "], " '" -> "'"];
  Map[
    If[StringMatchQ[#, _ ~~ "'"], Negated @ StringTake[#, 1], #]&,
    StringSplit[str]
  ]
];

parseCardinalWord[path_String] := {path};
parseCardinalWord[list_List] := list;

GraphRegion::nocard = "The region ``[...] specified a cardinal '``' path step at vertex ``, but none exists."

processRegion[Path[start_, path_]] :=
  collectPathData @ sowPath[start, path, False];

(********************************************)

sowPath[start_, path_, repeating_] := Scope[
  id = findVertex @ start;
  cardList = parseCardinalWord[path];
  numCards = Length @ cardList;
  sowVertex[id];
  i = 1; n = If[repeating, 10^6, numCards];
  Do[
    card = Part[cardList, Mod[i, numCards, 1]];
    vert = Part[$GraphVertexList, id];
    edgeIndex = findStrictEdgePattern[vert, _, card];
    If[!FreeQ[edgeIndex, $Failed], If[!repeating, failAuto["nocard", card, vert], Break[]]];
    edge = Part[$GraphEdgeList, StripNegated @ edgeIndex];
    isFlipped = First[edge] =!= vert;
    other = Part[edge, If[isFlipped, 1, 2]];
    id = $GraphVertexIndices[other];
    sowEdge[edgeIndex];
    sowVertex[id];
  ,
    {i, 1, n}
  ];
];

GraphRegion::badmetric = "`` is not a valid setting for GraphMetric."

$MetricFindShortestPath := Switch[$GraphMetric,
  None | Automatic | "Uniform", $GraphFindShortestPath,
  "Quadratic", $LatticeFindShortestPath,
  True, fail["badmetric", $GraphMetric]
];

$MetricDistance := Switch[$GraphMetric,
  None | Automatic | "Uniform", $GraphDistance,
  "Quadratic", $LatticeDistance,
  True, fail["badmetric", $GraphMetric]
];


(********************************************)
(** Line[...]                              **)

GraphRegionElementQ[Line[_]] := True;

processRegion[Line[vertices_]] :=
  collectPathData @ MapStaggered[findAndSowGeodesic, findVertices @ vertices]

findAndSowGeodesic[v1_, v2_] := Scope[
  geodesicVertices = $MetricFindShortestPath[v1, v2];
  sowVertexList[geodesicVertices];
  MapStaggered[findAndSowEdge, geodesicVertices]
];


(********************************************)
(** HalfLine[...]                              **)

GraphRegionElementQ[HalfLine[{_, _}] | HalfLine[_, _]] := True;

processRegion[HalfLine[v_, dir_]] :=
  collectPathData @ sowPath[v, dir, True];

processRegion[HalfLine[{v1_, v2_}]] := Scope[
  None;
];


(********************************************)
(** InfiniteLine[...]                              **)

GraphRegionElementQ[InfiniteLine[{_, _}] | InfiniteLine[_, _]] := True;

processRegion[InfiniteLine[v_, dir_]] := Scope[
  cardinalWord = parseCardinalWord[dir];
  {posVerts, posEdges, posNegations} = List @@ processRegion @ HalfLine[v, cardinalWord];
  {negVerts, negEdges, negNegations} = List @@ processRegion @ HalfLine[v, Negated /@ Reverse @  cardinalWord];
  GraphPathData[
    Join[Reverse @ negVerts, posVerts],
    Join[Reverse @ negEdges, posEdges],
    Join[Complement[Range @ Length @ negEdges, negNegations], posNegations]
  ]
];


(********************************************)
(** Polygon[...]                           **)

GraphRegionElementQ[Polygon[_List]] := True;

processRegion[Polygon[vertices_]] := Scope[
  vertices = findVertices @ vertices;
  collectPathData[
    findAndSowGeodesic @@@ Partition[vertices, 2, 1, 1]
  ]
];


(********************************************)
(** StarPolygon[...]                       **)

PackageExport["StarPolygon"]

SetUsage @ "
StarPolygon[vertices$] represents a polygon that connects all vertices$ \
with geodesics.
"

GraphRegionElementQ[StarPolygon[_List]] := True;

processRegion[StarPolygon[vertices_]] := Scope[
  vertices = findVertices @ vertices;
  pathToRegion @ collectPathData[
    findAndSowGeodesic @@@ Tuples[vertices, {2}]
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
GraphRegionElementQ[Locus[_ ? GraphRegionElementQ, _ ? GraphRegionElementQ, _ ? NumericQ]] := True;

processRegion[Locus[r1_, r2_, d_:0 ? NumericQ]] := Scope[
  r1 = First @ processRegion @ r1;
  r2 = First @ processRegion @ r2;
  d1 = extractDistanceVectors @ r1;
  d2 = extractDistanceVectors @ r2;
  Panic["NotImplemented"];
];

extractDistanceVectors[{v_}] :=
  List @ $MetricDistance[v, All];

extractDistanceVectors[v_List] :=
  List @ $MetricDistance[v, All];

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

ApproxEqualTo[e_][r_] := Round[e] == Round[r];

(********************************************)

GraphRegion::emptyarea = "The area defined by `` contains no points."

circularRegionData[spec_, center_, condition_] := Scope[
  centerInd = findVertex @ center;
  distances = $MetricDistance[centerInd, All];
  vertices = SelectIndices[distances, condition];
  If[vertices === {}, fail["emptyarea", spec]];
  subgraphRegionData @ vertices
];

subgraphRegionData[vertices_] := Scope[
  forward = Flatten @ Part[$GraphVertexOutEdgeTable, vertices];
  backward = Flatten @ Part[$GraphVertexInEdgeTable, vertices];
  GraphRegionData[
    vertices,
    Intersection[forward, backward]
  ]
];


(********************************************)
(** RegionComplement[...]                  **)

PackageExport["RegionComplement"]

GraphRegionElementQ[RegionComplement[_, ___]] := True;

processRegion[RegionComplement[regions__]] :=
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
(** RegionIntersection[...]                **)

GraphRegionElementQ[RegionIntersection[___]] := True;

processRegion[RegionIntersection[regions__]] :=
  RegionDataIntersection @ Map[processRegion, {regions}];

(********************************************)

RegionDataIntersection[{a_}] := a;

RegionDataIntersection[list_List] :=
  GraphRegionData[
    Intersection @@ regionDataListVertices @ list,
    Intersection @@ regionDataListEdges @ list
  ]


(********************************************)
(** RegionUnion[...]                       **)

GraphRegionElementQ[RegionUnion[___]] := True;

processRegion[RegionUnion[regions__]] :=
  RegionDataUnion @ Map[processRegion, {regions}];

(********************************************)

RegionDataUnion[{a_}] := a;

RegionDataUnion[list_List] :=
  GraphRegionData[
    Union @@ regionDataListVertices @ list,
    Union @@ regionDataListEdges @ list
  ]
