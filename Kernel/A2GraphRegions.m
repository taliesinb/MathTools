Package["GraphTools`"]

PackageImport["GeneralUtilities`"]



PackageExport["GraphRegion"]


PackageExport["HighlightGraphRegion"]

Options[HighlightGraphRegion] = {
  DirectedEdges -> False
};

HighlightGraphRegion::notgraph = "The first argument to HighlightGraphRegion should be a valid Graph."

$highlightArrowheads = Arrowheads[{{Medium, .85}}];
$reverseHighlightArrowheads = Arrowheads[{{-Medium, 0.25}}];

HighlightGraphRegion[graph_, region_, OptionsPattern[]] := Scope[
  UnpackOptions[directedEdges];
  If[!GraphQ[graph], ReturnFailed["notgraph"]];
  result = GraphRegion[graph, region];
  Print[result];
  If[FailureQ[result], Return[graph]];
  hstyle = "DehighlightFade";
  If[AssociationQ[result],
    i = 1;
    result = Map[
      Style[Join[#1, #2], $ColorPalette[[i++]], $highlightArrowheads]&,
      Values @ result
    ];
    hstyle = Automatic;
  ,
    result = Style[result, First @ $ColorPalette, $highlightArrowheads];
  ];
  result = result /. {
    Negated[d_DirectedEdge] :> Style[d, $reverseHighlightArrowheads],
    Negated[d_UndirectedEdge] :> d
  };

  HighlightGraph[graph, result, GraphHighlightStyle -> hstyle]
];


PackageExport["VertexPattern"]
PackageExport["EdgePattern"]
PackageExport["Path"]

SetUsage @ "
VertexPattern[pattern$] represent a vertex that matches pattern$.
"

SetUsage @ "
EdgePattern[src$, dst$] represents an edge that matches pattern.
"


SetUsage @ "
GraphRegion[graph$, region$] gives a subgraph of graph$ described by region$, which can be one or more of the following:
| {$$} | a list of edges and/or vertices |
| Line[{v$1, v$2}] | the geodesic between v$1 and v$2
| Line[{v$1, v$2}, c$] | start at v$1, move in cardinal direction c$, and end when v$2 is reached. |
| Line[{v$1, $$, v$n}] | the geodesic path between v$1 and v$2, v$2 and v$3, etc. |
| Path[v$, {c$1, $$, c$n} ] | starting at v$, walking along the path defined by cardinals c$i. |
| HalfLine[v$, c$] | a geodesic starting at v$ and continuing in the cardinal direction c$. |
| HalfLine[{v$1, v$2}] | a geodesic starting at v$1 and continuing through v$2. |
| InfiniteLine[v$, c$] | a geodesic with midpoint v$, and continuing in directions c$ and Negated[c$]. |
| InfiniteLine[{v$1, v$2}] | a geodesic intersecting v$1 and v$2. |
| Polygon[{v$1, $$, v$n}] | all vertices surrounded by the given vertices, and their mutual edges. |
| Disk[v$, r$] | all vertices reachable in up to r$ edges from v$, and their mutual edges. |
| Circle[v$, r$] | all vertices exactly r$ edges from v$, and their mutual edges. |
* In the specifications above, a cardinal c$ can also be a list {c$1, ..., c$n}, which indicates that the \
geodesic should take the cardinals c$1, c$2, $$, c$n, c$1, c$2, $$.
* The returned value is a pair of {vertices$, edges$} in the graph region.
* In addition if region$ is an assocation whose values are regions, an association of results will be produced.
"

GraphRegion::notgraph = "The first argument to GraphRegion should be a valid Graph."

GraphRegion[graph_, region_] := Scope[
  If[!GraphQ[graph], ReturnFailed["notgraph"]];
  GraphScope[graph, iGraphRegion[region]]
];

iGraphRegion[region_Association] :=
  KeyValueMap[iGraphRegion, region];

iGraphRegion[region_] := Scope[
  {vertices, edges, negations} = processRegionSpec[region];
  List[
    Part[$GraphVertexList, vertices],
    applyNegations[Part[$GraphEdgeList, edges], negations]
  ]
];

applyNegations[edges_, {}] := edges;
applyNegations[edges_, neg_] := MapAt[Negated, edges, List /@ neg];



PackageScope["processRegionSpec"]

processRegionSpec[region_] := Scope[
  $vertexBag = Internal`Bag[];
  $edgeBag = Internal`Bag[];
  $edgeNegationBag = Internal`Bag[];
  Catch[
      Scan[
        reg |-> (
          $currentRegionHead = Head[reg];
          processRegion[reg]
        )
      ,
        Developer`ToList @ region
      ],
      processRegion
    ];
  {
    Internal`BagPart[$vertexBag, All],
    Internal`BagPart[$edgeBag, All],
    Internal`BagPart[$edgeNegationBag, All]
  }
];

(********************************************)
(** framework code                         **)
(********************************************)

failAuto[msgName_, args___] := (
  Message[MessageName[GraphRegion, msgName], $currentRegionHead, args];
  Throw[Null, processRegion]
);

fail[msgName_, args___] := (
  Message[MessageName[GraphRegion, msgName], args];
  Throw[Null, processRegion]
);

sowVertexList[v_] := Internal`StuffBag[$vertexBag, v, 1];
sowVertex[v_] := Internal`StuffBag[$vertexBag, v];

sowEdge[i_Integer] := (
  Internal`StuffBag[$edgeBag, i];
  True
);

sowEdge[Negated[i_Integer]] := (
  Internal`StuffBag[$edgeBag, i];
  Internal`StuffBag[$edgeNegationBag, Internal`BagLength[$edgeBag]];
  True
);

findEdgePattern[a_, b_, c_] := Scope @ Which[
  IntegerQ[i = FirstIndex[$GraphEdgeList, DirectedEdge[a, b, c]]], i,
  IntegerQ[i = FirstIndex[$GraphEdgeList, DirectedEdge[b, a, c]]], Negated @ i,
  IntegerQ[i = FirstIndex[$GraphEdgeList, UndirectedEdge[a, b, c] | UndirectedEdge[b, a, c]]], i,
  True, $Failed
];

findEdgePattern[a_, b_, Negated[c_]] :=
  Negated @ findEdgePattern[b, a, c];

sowEdge[_] := False;

findEdge[v1_, v2_] :=
  Lookup[$GraphIOIndexToEdgeIndex, Key @ {v1, v2},
    Negated @ Lookup[$GraphIOIndexToEdgeIndex, Key @ {v2, v1}]];

findAndSowEdge[v1_, v2_] :=
  sowEdge @ findEdge[v1, v2];


GraphRegion::badrspec = "The region specification `` was not a vertex, edge, or one of the recognized region forms.";
GraphRegion::nfvertex = "No vertex matching `` was found in the graph."
GraphRegion::noedge = "The edge `` does not exist.";
GraphRegion::nfedge = "No edge matching `` was found in the graph.";
GraphRegion::malformedrspec = "The region specification `` was malformed.";

(********************************************)
(** directly specified edges               **)

processRegion[spec_] :=
  If[Not @ sowVertex @ $GraphVertexIndices @ spec, fail["badrspec", spec]]

processRegion[rule:((Rule|TwoWayRule|DirectedEdge|UndirectedEdge)[l_, r_])] := Scope[
  {l, r} = Lookup[$GraphVertexIndices, {l, r}];
  If[Not @ findAndSowEdge[l, r], fail["nfedge", rule]]
];

processRegion[edge:(_DirectedEdge | _UndirectedEdge)] :=
  If[Not @ sowEdge @ $GraphVertexEdges @ edge, fail["noedge", edge]];

(********************************************)
(** vertex or edge patterns                **)

processRegion[p:EdgePattern[a_, b_]] := sowEdge @ findEdgePattern[a, b, _];
processRegion[p:EdgePattern[a_, b_, c_]] := sowEdge @ findEdgePattern[a, b, c];

processRegion[p:VertexPattern[v_]] :=
  If[Not @ sowVertex @ FirstIndex[$GraphVertexList, v], fail["nfvertex", p]];

(********************************************)
(** complex region specifications          **)
(********************************************)

mapStaggered[f_, list_] := f @@@ Partition[list, 2, 1];


sowPathEdges[v1_, v2_] :=
  mapStaggered[findAndSowEdge, $GraphFindShortestPath[v1, v2]];

GraphRegion::invv = "The region ``[...] contained an invalid vertex specification ``.";

findVertex[spec_] := Lookup[$GraphVertexIndices, Key[spec],
  failAuto["invv", spec]];

findVertex[p:VertexPattern[v_]] :=
  FirstIndex[$GraphVertexList, v, failAuto["invv", p]];

GraphRegion::notlist = "The region ``[...] required a list of vertices, but got `` instead."

findVertices[spec_] := Lookup[$GraphVertexIndices, spec,
  If[!ListQ[spec], failAuto["notlist", spec],
    Map[findVertex, spec]]];

(********************************************)
(** Path[...]                              **)

toCardinals[path_String] /; StringLength[path] > 1 := Scope[
  chars = Characters[path];
  str = StringReplace[StringRiffle[chars, " "], " '" -> "'"];
  Map[
    If[StringMatchQ[#, _ ~~ "'"], Negated @ StringTake[#, 1], #]&,
    StringSplit[str]
  ]
];

toCardinals[path_String] := {path};
toCardinals[list_List] := list;

GraphRegion::nocard = "The region ``[...] specified a cardinal '``' path step at vertex ``, but none exists."

processRegion[Path[start_, path_]] := Scope[
  id = findVertex @ start;
  cardinals = toCardinals[path];
  sowVertex[id];
  Do[
    vert = Part[$GraphVertexList, id];
    edgeIndex = findEdgePattern[vert, _, c];
    If[FailureQ[edgeIndex], failAuto["nocard", c, vert]];
    edge = Part[$GraphEdgeList, StripNegated @ edgeIndex];
    isFlipped = First[edge] =!= vert;
    other = Part[edge, If[isFlipped, 1, 2]];
    id = $GraphVertexIndices[other];
    sowEdge[edgeIndex];
    sowVertex[id];
  ,
    {c, cardinals}
  ];
];

(********************************************)
(** Line[...]                              **)

processRegion[Line[vertices_]] :=
  mapStaggered[sowPathEdges, findVertices @ vertices]

processRegion[s_Line] := failAuto["malformedrspec", s];

(********************************************)
(** Disk[...]                              **)

sowSubgraphEdges[vertices_] :=
  Do[findAndSowEdge[v1, v2], {v1, vertices}, {v2, vertices}];

processRegion[Disk[center_, r_]] := Scope[
  centerInd = findVertex @ center;
  distances = $GraphDistanceMatrix[[centerInd, All]];
  vertices = SelectIndices[distances, LessEqualThan[r]];
  sowSubgraphEdges @ vertices;
  sowVertexList @ vertices;
];

processRegion[s_Disk] := failAuto["malformedrspec", s];

(********************************************)
(** Circle[...]                            **)

sowOuterPath[v1_, v2_] := Scope[
  minDist = GraphDistance[annulusGraph, v1, v2];
  pathListVertices = FindPath[annulusGraph, v1, v2, {minDist}, All];
  mapStaggered[findAndSowEdge // Tap, MinimumBy[pathListVertices, -Mean[Part[distances, #]]&]]
]

GraphRegion::trivcircle = "The circle defined by `` is trivial."
GraphRegion::discircle = "The circle defined by `` is disconnected."

processRegion[c:Circle[center_, r_]] := Scope[
  centerInd = findVertex @ center;
  distances = $GraphDistanceMatrix[[centerInd, All]];
  vertices = SelectIndices[distances, EqualTo[r]];
  If[Length[vertices] <= 1, fail["trivcircle", c]];
  annulusVertices = SelectIndices[distances, Between[{r, r + 2}]];
  annulusGraph = Subgraph[$SymmetricIndexGraph, annulusVertices];
  If[!ConnectedGraphQ[annulusGraph], fail["discircle", c]];
  sowVertexList @ vertices;
  {vertex, vertices} = {First @ vertices, Rest @ vertices};
  firstVertex = vertex;
  n = Length[vertices];
  Do[
    nextIndex = MinimumIndexBy[vertices, GraphDistance[annulusGraph, vertex, #]&];
    nextVertex = Part[vertices, nextIndex]; vertices //= Delete[nextIndex];
    Print[vertex -> nextVertex];
    sowOuterPath[vertex, nextVertex];
    vertex = nextVertex
  ,
    {n}
  ];
  sowOuterPath[vertex, firstVertex];
];

processRegion[s_Circle] := failAuto["malformedrspec", s];
