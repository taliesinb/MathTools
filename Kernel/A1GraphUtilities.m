Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["VertexAnnotations"]

(**************************************************************************************************)

$extendedGraphOptionsRules = {
  GraphPlottingFunction -> None,
  GraphRegionHighlight -> None,
  GraphLegend -> None,
  ArrowheadSize -> Automatic,
  ArrowheadStyle -> Automatic,
  VertexColorFunction -> None,
  VertexAnnotations -> None
};

$extendedGraphOptionSymbols = Keys @ $extendedGraphOptionsRules;

$extendedGraphOptionSymbolPattern = Alternatives @@ $extendedGraphOptionSymbols;

$extendedGraphOptionRulePattern = Rule[$extendedGraphOptionSymbolPattern, _];

$notIntercepted = True;

Graph;

Unprotect[Graph];
Options[Graph] = Sort @ JoinOptions[Graph, $extendedGraphOptionsRules];
SyntaxInformation[Graph] = ReplaceOptions[SyntaxInformation[Graph], "OptionNames" -> Map[SymbolName, Keys[Options[Graph]]]];
g:Graph[___] /; MemberQ[Unevaluated @ g, $extendedGraphOptionRulePattern] && $notIntercepted :=
  Block[{$notIntercepted = False}, interceptedGraphConstructor[g]];
Protect[Graph];

SetHoldAllComplete[interceptedGraphConstructor];

interceptedGraphConstructor[Graph[Shortest[args__], options__Rule]] := Scope[
  annotations = TakeOptions[{options}, $extendedGraphOptionSymbols];
  newOptions = DeleteOptions[{options}, $extendedGraphOptionSymbols];
  result = Graph[args, Sequence @@ newOptions];
  If[!GraphQ[result], ReturnFailed[]];
  Annotate[result, DeleteDuplicatesBy[annotations, First]]
];

interceptedGraphConstructor[e_] := e;

(**************************************************************************************************)

PackageExport["ExtendedGraphQ"]

ExtendedGraphQ[g_Graph ? GraphQ] :=
  Count[AnnotationValue[g, $extendedGraphOptionSymbols], $Failed] =!= Length[$extendedGraphOptionSymbols];

ExtendedGraphQ[_] := False;

(**************************************************************************************************)

PackageScope["LookupExtendedGraphAnnotations"]

LookupExtendedGraphAnnotations[graph_, keys_List] :=
  MapThread[
    If[#1 === $Failed, #2, #1]&,
    {AnnotationValue[graph, keys], Lookup[$extendedGraphOptionsRules, keys]}
  ];

(**************************************************************************************************)

PackageScope["ExtendedGraphAnnotations"]

ExtendedGraphAnnotations[graph_] :=
  Normal @ DeleteCases[$Failed] @ AssociationThread[
    $extendedGraphOptionSymbols,
    AnnotationValue[graph, $extendedGraphOptionSymbols]
  ];

(**************************************************************************************************)

PackageScope["$simpleGraphOptions"]
PackageScope["$simpleGraphOptionRules"]

$simpleGraphOptionRules = JoinOptions[{
  EdgeLabels -> None, GraphLayout -> Automatic, ImagePadding -> All,
  ImageSize -> Automatic, VertexCoordinates -> Automatic,
  VertexLabels -> None, VertexSize -> Automatic},
  Rest @ $extendedGraphOptionsRules
]

$simpleGraphOptions = Keys @ $simpleGraphOptionRules;

(**************************************************************************************************)

PackageExport["VertexEdgeList"]

SetUsage @ "
VertexEdgeList[graph$] returns {vertices$, edges$}
"

VertexEdgeList[graph_] := {
  VertexList[graph],
  EdgeList[graph]
}

(**************************************************************************************************)

PackageExport["EdgePairs"]

SetUsage @ "
EdgePairs[graph$] gives the list of {{u$1, v$1}, {u$2, v$2}, $$}} such that \
vertex with index u$i is connected to vertex with index v$i.
"

EdgePairs[graph_] := AdjacencyMatrix[graph]["NonzeroPositions"];

(**************************************************************************************************)

PackageExport["VertexOutTable"]
PackageExport["VertexInTable"]

SetUsage @ "
VertexOutTable[graph$] returns a list of lists {out$1, out$2, $$} where out$i is a list of the \
indices of the vertices that are have a connection from vertex i$.
"

SetUsage @ "
VertexInTable[graph$] returns a list of lists {in$1, in$2, $$} where in$i consists of the \
indices of the vertices that are have a connection to vertex i$.
"

VertexOutTable[graph_] := AdjacencyMatrix[graph]["AdjacencyLists"];
VertexInTable[graph_] := Transpose[AdjacencyMatrix[graph]]["AdjacencyLists"];

(**************************************************************************************************)

PackageExport["VertexInOutTable"]

SetUsage @ "
VertexInOutTable[graph$] returns a list of pairs of lists {{in$1, out$1}, {in$2, out$2}, $$} where in$i \
is the list of indices of vertices that are have a connection to vertex i$, and out$i is the \
list of indices of vertices that have a connection from vertex i$.
"

VertexInOutTable[graph_] := Scope[
  adj = AdjacencyMatrix[graph];
  Transpose[{adj["AdjacencyLists"], Transpose[adj]["AdjacencyLists"]}]
];

(**************************************************************************************************)

PackageExport["VertexIndexAssociation"]

VertexIndexAssociation[graph_] := AssociationRange @ VertexList @ graph;

(**************************************************************************************************)

PackageExport["EdgeIndexAssociation"]

EdgeIndexAssociation[graph_] := AssociationRange @ EdgeList @ graph;

(**************************************************************************************************)

PackageExport["VertexOrientedOutTable"]

SetUsage @ "
VertexOrientedOutTable[graph$] returns a list of pairs of lists {{dout$1, uout$1}, {dout$2, uout$2}, $$} \
where dout$i is the list of indices of vertices that are have a directed edge from vertex i$, and uout$i is \
the list of indices of vertices that have a undirected edge from vertex i$.
"

toOutTable[count_, edges_] := Lookup[GroupBy[edges, First -> Last], Range[count], {}];

VertexOrientedOutTable[graph_] := Scope[
  edges = EdgeList @ IndexGraph @ graph; count = VertexCount[graph];
  dir = Cases[edges, _DirectedEdge];
  undir = Cases[edges, _UndirectedEdge];
  Transpose @ {
    toOutTable[count, dir],
    toOutTable[count, Join[undir, Reverse[undir, 2]], 1]
  }
];

(**************************************************************************************************)

PackageExport["FromVertexOrientedOutTable"]

SetUsage @ "
FromVertexOrientedOutTable[{{dout$1, uout$1}, {dout$2, uout$2}, $$}, {v$1, v$2, $$}] returns a graph in which
vertex v$i is connected to v$j if j is an element of dout$i or uout$i.
* This function is the inverse of VertexOrientedOutTable.
"

(**************************************************************************************************)

PackageExport["VertexOutAssociation"]
PackageExport["VertexInAssociation"]

SetUsage @ "
VertexOutAssociation[graph$] returns an association of lists <|v$1 -> out$1, v$2 -> out$2, $$|> \
where out$i is a list of the vertices that have a connection from v$i.
"

SetUsage @ "
VertexInAssociation[graph$] returns an association of lists <|v$1 -> in$1, v$2 -> in$2, $$|> \
where in$i is a list of the vertices that have a connection to v$i.
"

tableToAssoc[vertices_, table_] := Association @ MapIndexed[
  Part[vertices, First @ #2] -> Part[vertices, #1]&,
  table
];

VertexOutAssociation[graph_] :=
  tableToAssoc[VertexList @ graph, VertexOutTable @ vertices];

VertexInAssociation[graph_] :=
  tableToAssoc[VertexList @ graph, VertexInTable @ vertices];

(**************************************************************************************************)

PackageExport["VertexInOutAssociation"]

SetUsage @ "
VertexInOutAssociation[graph$] returns an association of lists <|v$1 -> {in$1, out$1}, v$2 -> {in$2, out$2}, $$|> \
where in$i is the list of indices of vertices that are have a connection to vertex i$, and out$i is the \
list of indices of vertices that have a connection from vertex i$.
"

VertexInOutAssociation[graph_] := Scope[
  vertices = VertexList[graph];
  Association @ MapIndexed[
    Part[vertices, First @ #2] -> {Part[vertices, First[#1]], Part[vertices, Last[#1]]}&,
    VertexInOutTable[graph]
  ]
];

(**************************************************************************************************)

PackageExport["InVertices"]
PackageExport["OutVertices"]
PackageExport["AllVertices"]

InVertices[edges_] := edges[[All, 1]];
OutVertices[edges_] := edges[[All, 2]];
AllVertices[edges_] := Join[InVertices @ edges, OutVertices @ edges];

(**************************************************************************************************)

PackageExport["GraphCorners"]

GraphCorners[graph_] := Scope[
  degree = DegreeCentrality[graph];
  vertices = Pick[VertexList[graph], degree, Min[degree]];
  SortBy[vertices, LatticeVertexAngle]
];

(**************************************************************************************************)

PackageScope["integersToVertices"]

integersToVertices[graph_Graph, expr_] :=
  integersToVertices[VertexList[graph], expr];

integersToVertices[vertices_List, expr_] :=
  expr /. {i:{__Integer} :> Part[vertices, i], i_Integer :> Part[vertices, i]};

(**************************************************************************************************)

PackageExport["ToGraph"]

SetUsage @ "
ToGraph[obj$] attempts to convert obj$ to a Graph[$$] object.
* If obj$ is already a Graph, it is returned unchanged.
* If obj$ is a list of rules, it is converted to a Graph object.
* Otherwise, $Failed is returned.
"

$edgeP = _DirectedEdge | _UndirectedEdge | _Rule | _TwoWayRule;

ToGraph = MatchValues[
  g_Graph := g;
  list:{Repeated[$edgeP]} := Graph[list];
  _ := $Failed
];

(**************************************************************************************************)

PackageExport["AttachVertexAnnotations"]

AttachVertexAnnotations[graph_, annotations_] := Scope[
  CheckGraphArg[1];
  oldAnnotations = LookupAnnotation[graph, VertexAnnotations, None];
  SetNone[oldAnnotations, <||>];
  Annotate[graph, VertexAnnotations -> Join[oldAnnotations, annotations]]
];

(**************************************************************************************************)

PackageExport["ExtendedSubgraph"]

ExtendedSubgraph[oldGraph_, newVertices_, newEdges_] := Scope[
  options = Options[oldGraph];
  annotations = ExtendedGraphAnnotations[oldGraph];
  vertexCoords = Lookup[options, VertexCoordinates, Automatic];
  oldVertices = VertexList[oldGraph];
  newVertexIndices = Map[IndexOf[oldVertices, #]&, newVertices];
  newVertexOrdering = Ordering[newVertexIndices];
  newVertices = Part[newVertices, newVertexOrdering];
  vertexAnnotations = LookupAnnotation[oldGraph, VertexAnnotations, None];
  sortedNewVertexIndices = Sort @ newVertexIndices;
  If[ListQ[vertexCoords],
    vertexCoords = Part[vertexCoords, sortedNewVertexIndices];
    options = ReplaceOptions[options, VertexCoordinates -> vertexCoords];
  ];
  If[AssociationQ[vertexAnnotations],
    vertexAnnotations //= Map[Part[#, sortedNewVertexIndices]&];
    annotations = ReplaceOptions[annotations, VertexAnnotations -> vertexAnnotations];
  ];
  If[newEdges === Automatic,
    newEdges = Select[EdgeList @ oldGraph, MemberQ[newVertices, Part[#, 1]] && MemberQ[newVertices, Part[#, 2]]&]
  ];
  graph = Graph[newVertices, newEdges, options];
  Annotate[graph, annotations]
];

(**************************************************************************************************)

PackageScope["GraphScope"]

PackageScope["NotInGraphScopeOfQ"]

NotInGraphScopeOfQ[graph_] := !GraphQ[$Graph] || (graph =!= $Graph)

PackageScope["$Graph"]
PackageScope["$GraphVertexList"]
PackageScope["$GraphEdgeList"]
PackageScope["$GraphEdgeTags"]
PackageScope["$GraphVertexIndices"]
PackageScope["$GraphEdgeIndices"]
PackageScope["$GraphVertexCount"]
PackageScope["$GraphEdgeCount"]

PackageScope["$IndexGraph"]
PackageScope["$IndexGraphEdgeList"]
PackageScope["$IndexGraphVertexList"]
PackageScope["$GraphIOIndexToEdgeIndex"]
PackageScope["$SymmetricIndexGraph"]
PackageScope["$GraphDistanceMatrix"]
PackageScope["$GraphDistance"]
PackageScope["$GraphFindShortestPath"]
PackageScope["$GraphFindPath"]

SetAttributes[GraphScope, HoldRest];

SetUsage @ "
GraphScope[graph$, body$] sets up various dynamically scoped variables that make it easy to
access properties and computed results from a single graph.
The following variables are blocked during the execution of GraphScope:
| $Graph | graph$ |
| $GraphVertexList | VertexList[graph$] |
| $GraphEdgeList | EdgeList[graph$] |
| $GraphEdgeTags | EdgeTags[graph$] |
| $GraphVertexIndices | association mapping a vertex to its index |
| $GraphEdgesIndices | association mapping an edge to its index |
| $GraphVertexCount | GraphVertexCount[$graph] |
| $GraphEdgeCount | GraphEdgeCount[$graph] |
| $IndexGraph | IndexGraph[graph$] |
| $SymmetricIndexGraph | undirected version of $IndexGraph |
| $GraphIOIndexToEdgeIndex | an association mapping the vertex index pair {i$, o$} to the \
index of the first edge matching it.
| $GraphDistanceMatrix | GraphDistanceMatrix[$SymmetricIndexGraph] |
| $GraphDistance | function looking up distance between a pair of vertex indices |
| $GraphFindShortestPath | function returning shortest path from a pair of vertex indices |
| $GraphFindPath | FindPath[$SymmetricIndexGraph, $$] |
* All of the expensive properties are computed (and then cached) on first use.
* Functions like $GraphDistance, $GraphFindPath, etc. take and return vertex *indices*. You can use \
Lookup[$GraphVertexIndices, list$] to obtain these indices quickly.
"

GraphScope[graph_, body_] := Block[
  {
    $Graph = graph,
    $GraphVertexList := $GraphVertexList = VertexList[$Graph],
    $GraphEdgeList := $GraphEdgeList = EdgeList[$Graph],
    $GraphEdgeTags := $GraphEdgeTags = Replace[EdgeTags[$Graph], {} -> None],
    $GraphVertexIndices := $GraphVertexIndices = VertexIndexAssociation[$Graph],
    $GraphEdgeIndices := $GraphEdgeIndices = EdgeIndexAssociation[$Graph],

    $GraphVertexCount = VertexCount[$Graph],
    $GraphEdgeCount = EdgeCount[$Graph],

    $IndexGraph := $IndexGraph = IndexGraph[$Graph],
    $IndexGraphEdgeList := $IndexGraphEdgeList = EdgeList[$IndexGraph],
    $IndexGraphVertexList := $IndexGraphVertexList = Range @ VertexCount @ $IndexGraph,
    $GraphIOIndexToEdgeIndex := $GraphIOIndexToEdgeIndex = AssociationRange[{#1, #2}& @@@ $IndexGraphEdgeList],
    $SymmetricIndexGraph := $SymmetricIndexGraph = Graph[VertexList[$IndexGraph], UndirectedEdge @@@ EdgeList[$IndexGraph]],

    $GraphDistanceMatrix := $GraphDistanceMatrix = GraphDistanceMatrix[$SymmetricIndexGraph],
    $GraphDistance = {v1, v2} |-> Extract[$GraphDistanceMatrix, {v1, v2}],
    $GraphFindShortestPath := $GraphFindShortestPath = FindShortestPath[$SymmetricIndexGraph, All, All],
    $GraphFindPath = Function[FindPath[$SymmetricIndexGraph, ##]]
  },
  body
];

