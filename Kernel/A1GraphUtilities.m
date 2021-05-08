Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["VertexAnnotations"]
PackageExport["VertexPairAnnotations"]
PackageExport["LayoutDimension"]
PackageExport["GraphMetric"]

(**************************************************************************************************)

$extendedGraphOptionsRules = {
  GraphPlottingFunction -> None,
  GraphRegionHighlight -> None,
  GraphLegend -> None,
  ArrowheadSize -> Automatic,
  ArrowheadStyle -> Automatic,
  ArrowheadShape -> Automatic,
  ArrowheadPosition -> Automatic,
  VertexColorFunction -> None,
  VertexAnnotations -> None,
  VertexPairAnnotations -> None,
  LayoutDimension -> Automatic,
  GraphMetric -> "Uniform"
};

$extendedGraphOptionSymbols = Keys @ $extendedGraphOptionsRules;

$extendedGraphOptionSymbolPattern = Alternatives @@ $extendedGraphOptionSymbols;

$extendedGraphOptionRulePattern = Rule[$extendedGraphOptionSymbolPattern, _];

$notIntercepted = True;

Graph;
SyntaxInformation[Graph];
Options[Graph];

Unprotect[Graph];
Options[Graph] = Sort @ JoinOptions[Graph, $extendedGraphOptionsRules];
SyntaxInformation[Graph] = ReplaceOptions[SyntaxInformation[Graph], "OptionNames" -> Map[SymbolName, Keys[Options[Graph]]]];
g:Graph[___] /; MemberQ[Unevaluated @ g, $extendedGraphOptionRulePattern] && $notIntercepted :=
  Block[{$notIntercepted = False}, interceptedGraphConstructor[g]];
Protect[Graph];

SetHoldAllComplete[interceptedGraphConstructor];

interceptedGraphConstructor[Graph[Shortest[args__], options__Rule]] := Scope[
  annotations = TakeOptions[{options}, $extendedGraphOptionSymbols];
  newOptions = Map[collapseStyleLists] @ DeleteOptions[{options}, $extendedGraphOptionSymbols];
  result = Graph[args, Sequence @@ newOptions];
  If[!GraphQ[result], result = makeNewGraph[args, newOptions]];
  If[!GraphQ[result], ReturnFailed[]];
  Annotate[result, checkGraphAnnotations @ DeleteDuplicatesBy[annotations, First]]
];

makeNewGraph[graph_Graph ? GraphQ, newOptions_List] :=
  Graph[VertexList @ graph, EdgeList @ graph, Sequence @@ newOptions, Sequence @@ Options @ graph];

makeNewGraph[___] := $Failed;

collapseStyleLists = MatchValues[
  Rule[sym:(EdgeStyle|VertexStyle), val_] := Rule[sym, toDirective[val]];
  other_ := other;
];

interceptedGraphConstructor[e_] := e;

(**************************************************************************************************)

$extendedGraphOptionPatterns = <|
  ArrowheadSize -> $arrowheadSizePattern,
  VertexAnnotations -> $vertexAnnotationsPattern,
  LayoutDimension -> $layoutDimensionPattern,
  GraphMetric -> $graphMetricPattern
|>;

checkGraphAnnotations[rules_List] := Map[checkGraphAnnotationRule, rules];

General::badextopt = "The extended option `` -> `` is invalid and will be ignored."

checkGraphAnnotationRule[key_ -> value_] /; And[
  KeyExistsQ[$extendedGraphOptionPatterns, key],
  !MatchQ[value, $extendedGraphOptionPatterns @ key]] := (
    Message[Graph::badextopt, key, value];
    Nothing
  );

checkGraphAnnotationRule[rule_] := rule;

$arrowheadSizePattern = Alternatives[
  _ ? NumericQ, Scaled[_ ? NumericQ],
  sym_Symbol /; KeyExistsQ[$ImageWidthTable, sym],
  Automatic | None
];

$vertexAnnotationsPattern = Alternatives[
  Association[RepeatedNull[_String -> _List]],
  None
];

$layoutDimensionPattern = Alternatives[
  Automatic, None, 2, 3
];

$graphMetricPattern = Alternatives[
  Automatic, "Uniform", "Quadratic", _QuadraticForm ? QuadraticFormQ
];

(**************************************************************************************************)

PackageExport["AttachGraphOptions"]

AttachGraphOptions[graph_Graph ? GraphQ, opts___] := Scope[
  result = Graph[graph, opts];
  If[GraphQ[result], result, makeNewGraph[graph, {opts}]]
];

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
  VertexLabels -> None, VertexSize -> Automatic,
  VertexStyle -> Automatic, EdgeStyle -> Automatic,
  VertexShapeFunction -> Automatic, PlotLabel -> None
  },
  Rest @ $extendedGraphOptionsRules
]

$simpleGraphOptions = Keys @ $simpleGraphOptionRules;

(**************************************************************************************************)

PackageExport["ExtendedGraph"]

Options[ExtendedGraph] = $simpleGraphOptionRules;
ExtendedGraph[args___] :=
  interceptedGraphConstructor[Graph[args, GraphPlottingFunction -> ExtendedGraphPlottingFunction]];

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

PackageExport["ToIndexGraph"]

ToIndexGraph[graph_ ? IndexGraphQ] := graph;
ToIndexGraph[graph_] := IndexGraph @ graph;

(**************************************************************************************************)

PackageExport["CombineMultiedges"]

SetUsage @ "
CombineMultiedges[graph$] combines edges that share the same endpoints into
single edges, combining any cardinals they have.
"

CombineMultiedges[graph_] := Scope[
  {vertices, edges} = VertexEdgeList[graph];
  {edges, tags} = Transpose @ Map[separateTag, edges];
  edgeGroups = PositionIndex[edges];
  If[Length[edgeGroups] === Length[edges], Return @ graph];
  edges = KeyValueMap[
    {edge, indices} |-> reattachTag[edge, DeleteNone @ Part[tags, indices]],
    edgeGroups
  ];
  opts = Options[graph];
  Graph[vertices, edges, opts]
];

separateTag = MatchValues[
  DirectedEdge[a_, b_, t_] /; Order[a, b] == -1 := {DirectedEdge[b, a], Negated @ t};
  DirectedEdge[a_, b_, t_] := {DirectedEdge[a, b], t};
  UndirectedEdge[a_, b_, t_] := {Sort @ UndirectedEdge[a, b], t};
  edge_ := {Sort @ edge, None}
];

reattachTag[edge_, {}] := edge;
reattachTag[edge_, {tag_}] := Append[edge, tag];
reattachTag[edge_, tags_List] := Append[edge, CardinalSet @ tags];

(**************************************************************************************************)

PackageExport["CardinalSet"]

SetUsage @ "
CardinalSet[cardinals$] represents a set of cardinals that is simultaneously present on an edge.
"

PackageScope["SpliceCardinalSets"]

SpliceCardinalSets[e_] := ReplaceAll[ReplaceAll[e, CardinalSet -> Splice], Negated[z_] :> z];

(**************************************************************************************************)

PackageExport["VertexRange"]

SetUsage @ "
VertexRange[graph$] returns {1, 2, $$, n$} where n$ is the number of vertices in graph.
"

VertexRange[graph_] := Range @ VertexCount @ graph;

(**************************************************************************************************)

PackageExport["AdjacentPairs"]

SetUsage @ "
AdjacentPairs[graph$] gives the list of {{u$1, v$1}, {u$2, v$2}, $$}} such that \
vertex with index u$i is adjacent to vertex with index v$i.
* Note that AdjacentPairs is not given in the same order as EdgeList[graph$], and \
in general might have fewer values when there are multiple edges between the same \
pair of vertices.
* The relation is undirected, so that a$ \[DirectedEdge] b$ generates both {a$, b$} and {b$, a$}.
* Use AdjacentPairs[graph, 'Directed'] to obtain the directed form.
"

AdjacentPairs[graph_] := AdjacencyMatrix[graph]["NonzeroPositions"];

AdjacentPairs[graph_ ? DirectedGraphQ] := Scope[
  adj = AdjacencyMatrix[graph];
  (adj + Transpose[adj])["NonzeroPositions"]
];

AdjacentPairs[graph_, "Undirected"] := AdjacentPairs[graph];
AdjacentPairs[graph_, "Directed"] := AdjacencyMatrix[graph]["NonzeroPositions"];

(**************************************************************************************************)

PackageExport["EdgePairs"]

SetUsage @ "
EdgePairs[graph$] gives the list of {{u$1, v$1}, {u$2, v$2}, $$}} such that \
these is an vertex with index u$i is connected to vertex with index v$i.
* EdgePairs[graph$] has the same length and order as EdgeList[graph$].
* If the correspondence with EdgeList does not matter, consider using AdjacentPairs,
which is faster.
"

(* todo: find a better way of obtaining these than via indexgraph! it seems like
vertex renaming might be expensive, and there is all the option processing that goes along with it.
unfortunately i can't find a way of extracting the list of edges in indexed form directly. *)
EdgePairs[graph_ ? EdgeTaggedGraphQ] := {#1, #2}& @@@ EdgeList @ ToIndexGraph @ graph;
EdgePairs[graph_] := List @@@ EdgeList @ ToIndexGraph @ graph;

(**************************************************************************************************)

PackageExport["VertexOutTable"]
PackageExport["VertexInTable"]

SetUsage @ "
VertexOutTable[graph$] returns a list of lists {out$1, out$2, $$} where out$i is a list of the \
indices of the vertices that are have a connection from vertex v$i.
"

SetUsage @ "
VertexInTable[graph$] returns a list of lists {in$1, in$2, $$} where in$i consists of the \
indices of the vertices that are have a connection to vertex v$i.
"

VertexOutTable[graph_] := AdjacencyMatrix[graph]["AdjacencyLists"];
VertexInTable[graph_] := Transpose[AdjacencyMatrix[graph]]["AdjacencyLists"];

(**************************************************************************************************)

PackageExport["VertexInOutTable"]

SetUsage @ "
VertexInOutTable[graph$] returns a list of pairs of lists {{in$1, out$1}, {in$2, out$2}, $$} where in$i \
is the list of indices of vertices that are have an edge to vertex v$i, and out$i is the \
list of indices of vertices that have a edge from vertex v$i.
"

VertexInOutTable[graph_] := Scope[
  adj = AdjacencyMatrix[graph];
  Transpose[{adj["AdjacencyLists"], Transpose[adj]["AdjacencyLists"]}]
];

(**************************************************************************************************)

PackageExport["VertexAdjacencyTable"]

SetUsage @ "
VertexAdjacencyTable[graph$] returns a list of lists {adj$1, adj$2, $$} where adj$i \
is the list of indices of vertices that are have a connection to vertex v$i.
"

VertexAdjacencyTable[graph_] := Scope[
  adj = AdjacencyMatrix[graph];
  MapThread[Union, {adj["AdjacencyLists"], Transpose[adj]["AdjacencyLists"]}]
];

(**************************************************************************************************)

PackageExport["VertexOutEdgeTable"]
PackageExport["VertexInEdgeTable"]

SetUsage @ "
VertexOutEdgeTable[graph$] returns a list of lists {out$1, out$2, $$} where out$i is a list of the \
indices of edges whose origin is the vertex v$i.
"

SetUsage @ "
VertexInEdgeTable[graph$] returns a list of lists {in$1, in$2, $$} where in$i is a list of the \
indices of edges whose destination is the vertex v$i.
"

VertexOutEdgeTable[graph_] :=
  Lookup[PositionIndex @ FirstColumn @ EdgePairs @ graph, VertexRange @ graph, {}];

VertexInEdgeTable[graph_] :=
  Lookup[PositionIndex @ LastColumn @ EdgePairs @ graph, VertexRange @ graph, {}];

(**************************************************************************************************)

PackageExport["VertexInOutEdgeTable"]

SetUsage @ "
VertexInOutEdgeTable[graph$] returns a list of lists {{in$1, out$1}, {in$2, out$2}, $$}  where in$i \
is a list of the indices of edges whose destination is the vertex v$i, and out$i is a list of the \
indices of edges whose origin is the vertex v$i.
"

VertexInOutEdgeTable[graph_] := Scope[
  pairs = EdgePairs @ graph;
  vertices = VertexRange @ graph;
  Transpose[{
    Lookup[PositionIndex @ FirstColumn @ pairs, vertices, {}],
    Lookup[PositionIndex @ LastColumn @ pairs, vertices, {}]
  }]
];

(**************************************************************************************************)

PackageExport["VertexAdjacentEdgeTable"]

SetUsage @ "
VertexAdjacentEdgeTable[graph$] returns a list of lists {adj$1, adj$2, $$}  where adj$i \
is a list of the indices of edges which begin or end at vertex v$i.
"

VertexAdjacentEdgeTable[graph_] := Scope[
  pairs = EdgePairs @ graph;
  vertices = VertexRange @ graph;
  MapThread[Union, {
    Lookup[PositionIndex @ FirstColumn @ EdgePairs @ graph, vertices, {}],
    Lookup[PositionIndex @ LastColumn @ EdgePairs @ graph, vertices, {}]
  }]
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

PackageExport["GraphVertexCoordinates"]

GraphVertexCoordinates[graph_Graph] :=
  GraphEmbedding[graph];

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
  CheckIsGraph[1];
  joinAnnotation[graph, VertexAnnotations, annotations]
];

PackageExport["AttachVertexPairAnnotations"]

AttachVertexPairAnnotations[graph_, annotations_] := Scope[
  CheckIsGraph[1];
  joinAnnotation[graph, VertexPairAnnotations, annotations]
];

(**************************************************************************************************)

joinAnnotation[graph_, key_, newAnnotations_] := Scope[
  oldAnnotations = LookupAnnotation[graph, key, None];
  SetNone[oldAnnotations, <||>];
  Annotate[graph, key -> Join[oldAnnotations, newAnnotations]]
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

PackageExport["IndexGraphQ"]

IndexGraphQ[g_Graph ? GraphQ] :=
  RangeQ @ VertexList @ g;

IndexGraphQ[_] := False;

(**************************************************************************************************)

PackageExport["CanonicalizeEdges"]

CanonicalizeEdges[edges_] := Map[sortUE, edges];
sortUE[UndirectedEdge[a_, b_, tag___]] /; Order[a, b] === 1 := UndirectedEdge[b, a, tag];
sortUE[other_] := other;

(**************************************************************************************************)

PackageExport["ExtractGraphPrimitiveCoordinates"]

SetUsage @ "
ExtractGraphPrimitiveCoordinates[graph$] returns the pair {vcoords$, ecoords$}, where \
vcoords$ is a list of coordinate tuples in the same order as VertexList[graph$], and \
ecoords$ is a list of coordinate matrices in the same order as EdgeList[graph$].
"

ExtractGraphPrimitiveCoordinates[graph_] := Scope[

  If[!GraphQ[graph], ReturnFailed[]];
  igraph = ToIndexGraph[graph];
  If[!GraphQ[igraph], ReturnFailed[]];

  graphLayout = LookupOption[igraph, GraphLayout];
  layoutDimension = AnnotationValue[graph, LayoutDimension];
  SetAutomatic[graphLayout, {}];
  is3D = ContainsQ[graphLayout, "Dimension" -> 3] || layoutDimension === 3;

  vertexCoordinates = ConstantArray[If[is3D, {0, 0, 0}, {0, 0}], VertexCount @ igraph];

  edgeList = EdgeList @ igraph;
  edgeCoordinateLists = ConstantArray[{}, Length @ edgeList];
  If[UndirectedGraphQ[igraph] || MixedGraphQ[igraph],
    edgeList //= CanonicalizeEdges];

  isMulti = MultigraphQ[igraph];
  If[isMulti,
    edgeIndices = PositionIndex[edgeList];
    edgeIndexOffset = ConstantAssociation[edgeList, 1];
    edgeCaptureFunction = storeMultiEdgeCoords;
  ,
    edgeIndices = AssociationRange[edgeList];
    edgeCaptureFunction = storeEdgeCoords;
  ];

  If[isMulti || !DuplicateFreeQ[edgeList],
    graphLayout = Developer`ToList[graphLayout, "MultiEdgeDistance" -> 0.3];
  ];

  newGraph = If[is3D, Graph3D, Graph][
    VertexList @ igraph, EdgeList @ igraph,
    VertexShapeFunction -> captureVertexCoordinates,
    EdgeShapeFunction -> ({coords, edge} |-> edgeCaptureFunction[coords, sortUE @ edge]),
    GraphLayout -> graphLayout, VertexCoordinates -> LookupOption[igraph, VertexCoordinates]
  ];

  GraphComputation`GraphDrawing @ newGraph;

  {Developer`ToPackedArray @ vertexCoordinates, toPackedArrayOfArrays @ edgeCoordinateLists}
];

captureVertexCoordinates[coords_, vertex_, _] :=
  Part[vertexCoordinates, vertex] = coords;

storeEdgeCoords[coords_, edge_] :=
  Part[edgeCoordinateLists, edgeIndices @ edge] = coords;

storeMultiEdgeCoords[coords_, edge_] :=
  Part[edgeCoordinateLists, Part[edgeIndices @ edge, edgeIndexOffset[edge]++]] = coords;

toPackedArrayOfArrays[array_ ? Developer`PackedArrayQ] := array;

toPackedArrayOfArrays[array_] := Scope[
  array = Developer`ToPackedArray[array];
  If[!Developer`PackedArrayQ[array],
    array = Map[Developer`ToPackedArray, array]];
  array
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
PackageScope["$LatticeDistance"]
PackageScope["$LatticeFindShortestPath"]

PackageScope["$IndexGraph"]
PackageScope["$IndexGraphEdgeList"]
PackageScope["$IndexGraphVertexList"]
PackageScope["$GraphVertexInOutEdgeTable"]
PackageScope["$GraphVertexInEdgeTable"]
PackageScope["$GraphVertexOutEdgeTable"]
PackageScope["$SymmetricIndexGraph"]

PackageScope["$GraphMetric"]
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
    $GraphVertexInOutEdgeTable := $GraphVertexInOutEdgeTable = VertexInOutEdgeTable @ $IndexGraph,
    $GraphVertexInEdgeTable := $GraphVertexInEdgeTable = FirstColumn @ $GraphVertexInOutEdgeTable,
    $GraphVertexOutEdgeTable := $GraphVertexOutEdgeTable = LastColumn @ $GraphVertexInOutEdgeTable,

    $SymmetricIndexGraph := $SymmetricIndexGraph = Graph[VertexList[$IndexGraph], UndirectedEdge @@@ EdgeList[$IndexGraph]],

    $GraphMetric := $GraphMetric = LookupAnnotation[$Graph, GraphMetric, Automatic],
    $GraphDistanceMatrix := $GraphDistanceMatrix = GraphDistanceMatrix[$SymmetricIndexGraph],
    $LatticeDistance := $LatticeDistance = LatticeDistance[$SymmetricIndexGraph],
    $GraphDistance = {v1, v2} |-> Part[$GraphDistanceMatrix, v1, v2],
    $LatticeFindShortestPath := $LatticeFindShortestPath = FindShortestLatticePath[$SymmetricIndexGraph, All, All],
    $GraphFindShortestPath := $GraphFindShortestPath = FindShortestPath[$SymmetricIndexGraph, All, All],
    $GraphFindPath = Function[FindPath[$SymmetricIndexGraph, ##]]
  },
  body
];
