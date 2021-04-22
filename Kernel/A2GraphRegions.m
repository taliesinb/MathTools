Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

PackageExport["GraphRegion"]

SetUsage @ "
GraphRegion[graph$, region$] gives a subgraph of graph$ described by region$, which can be one or more of the following:
<*$GraphRegionTable*>
* The returned value is a pair of {vertices$, edges$} in the graph region.
* In addition if region$ is an assocation whose values are regions, an association of results will be produced.
"

SetArgumentCount[GraphRegion, 2];

GraphRegion::empty = "The specified region is empty."

GraphRegion[graph_, region_] := Scope[
  CheckGraphArg[1];
  result = GraphScope[graph, iGraphRegion @ region];
  If[FailureQ[result], ReturnFailed[]];
  {vertices, edges} = result;
  If[vertices === edges === {}, ReturnFailed["empty"]];
  vertices = DeleteDuplicates @ Join[vertices, AllVertices @ edges];
  ExtendedSubgraph[graph, vertices, edges]
];

iGraphRegion[region_Association] :=
  KeyValueMap[iGraphRegion, region];

iGraphRegion[region_] := Scope[
  {vertices, edges, negations} = processRegionSpec[region];
  List[
    Part[$GraphVertexList, vertices],
    Part[$GraphEdgeList, edges]
  ]
];

applyNegations[edges_, {}] := edges;
applyNegations[edges_, neg_] := MapAt[Negated, edges, List /@ neg];

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

SetUsage @ "
Path[src$, {c$1, $$, c$n}] represents a path starting at src$ and taking cardinals c$i in tern.
Path[src$, 'cards$'] interpreted the characters of 'cards$' as cardinals.
"

(**************************************************************************************************)

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

findStrictEdgePattern[a_, b_, c_] := Scope @ Which[
  IntegerQ[i = FirstIndex[$GraphEdgeList, DirectedEdge[a, b, c]]], i,
  IntegerQ[i = FirstIndex[$GraphEdgeList, UndirectedEdge[a, b, c] | UndirectedEdge[b, a, c]]], i,
  True, $Failed
];

findStrictEdgePattern[a_, b_, Negated[c_]] :=
  Negated @ findStrictEdgePattern[b, a, c];

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

GraphRegionElementQ[_Rule | _TwoWayRule | _DirectedEdge | _UndirectedEdge] := True;

processRegion[rule:((Rule|TwoWayRule|DirectedEdge|UndirectedEdge)[l_, r_])] := Scope[
  {l, r} = Lookup[$GraphVertexIndices, {l, r}];
  If[Not @ findAndSowEdge[l, r], fail["nfedge", rule]]
];

processRegion[edge:(_DirectedEdge | _UndirectedEdge)] :=
  If[Not @ sowEdge @ $GraphVertexEdges @ edge, fail["noedge", edge]];

(********************************************)
(** vertex or edge patterns                **)

GraphRegionElementQ[EdgePattern[_, __]] := True;

processRegion[p:EdgePattern[a_, b_]] := sowEdge @ findEdgePattern[a, b, _];
processRegion[p:EdgePattern[a_, b_, c_]] := sowEdge @ findEdgePattern[a, b, c];

GraphRegionElementQ[VertexPattern[_]] := True;

processRegion[p:VertexPattern[v_]] :=
  If[Not @ sowVertex @ FirstIndex[$GraphVertexList, v], fail["nfvertex", p]];

(********************************************)
(** complex region specifications          **)
(********************************************)

sowPathEdges[v1_, v2_] :=
  MapStaggered[findAndSowEdge, $GraphFindShortestPath[v1, v2]];

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

GraphRegionElementQ[Path[_, _]] := True;

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
    edgeIndex = findStrictEdgePattern[vert, _, c];
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

GraphRegionElementQ[Line[_]] := True;

processRegion[Line[vertices_]] :=
  MapStaggered[sowPathEdges, findVertices @ vertices]

processRegion[s_Line] := failAuto["malformedrspec", s];

(********************************************)
(** Disk[...]                              **)

GraphRegionElementQ[Disk[_, _]] := True;

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

GraphRegionElementQ[Circle[_, _]] := True;

sowOuterPath[v1_, v2_] := Scope[
  minDist = GraphDistance[annulusGraph, v1, v2];
  pathListVertices = FindPath[annulusGraph, v1, v2, {minDist}, All];
  MapStaggered[findAndSowEdge, MinimumBy[pathListVertices, -Mean[Part[distances, #]]&]]
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
    sowOuterPath[vertex, nextVertex];
    vertex = nextVertex
  ,
    {n}
  ];
  sowOuterPath[vertex, firstVertex];
];

processRegion[s_Circle] := failAuto["malformedrspec", s];
