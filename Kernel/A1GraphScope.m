Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


PackageExport["GraphPlottingFunction"]


PackageScope["$Graph"]
PackageScope["$GraphVertexCoordinates"]
PackageScope["$GraphVertexCoordinateBoundingBox"]
PackageScope["$GraphVertexList"]
PackageScope["$GraphEdgeList"]

PackageScope["$GraphVertexList"]
PackageScope["$GraphEdgeList"]
PackageScope["$GraphVertexIndices"]
PackageScope["$GraphEdgeIndices"]


PackageScope["$GraphIOIndexToEdgeIndex"]
PackageScope["$IndexGraph"]
PackageScope["$IndexGraphEdgeList"]
PackageScope["$SymmetricIndexGraph"]
PackageScope["$GraphDistanceMatrix"]
PackageScope["$GraphDistance"]
PackageScope["$GraphFindShortestPath"]
PackageScope["$GraphFindPath"]



PackageScope["$GraphVertexCoordinateDistanceMatrix"]
PackageScope["$GraphVertexCoordinateMinDistance"]


PackageScope["GraphScope"]

SetAttributes[GraphScope, HoldRest];

SetUsage @ "
GraphScope[graph$, body$] sets up various dynamically scoped variables that make it easy to
access properties and computed results from a single graph.
The following variables are blocked during the execution of GraphScope:
| $Graph | graph$ |
| $GraphVertexList | VertexList[graph$] |
| $GraphEdgeList | EdgeList[graph$] |
| $GraphVertexIndices | association mapping a vertex to its index |
| $GraphEdgesIndices | association mapping an edge to its index |
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
    $GraphVertexIndices := $GraphVertexIndices = VertexIndexAssociation[$Graph],
    $GraphEdgeIndices := $GraphEdgeIndices = EdgeIndexAssociation[$Graph],

    $IndexGraph := $IndexGraph = IndexGraph[$Graph],
    $IndexGraphEdgeList := $IndexGraphEdgeList = EdgeList[$IndexGraph],
    $GraphIOIndexToEdgeIndex := $GraphIOIndexToEdgeIndex = AssociationRange[{#1, #2}& @@@ $IndexGraphEdgeList],
    $SymmetricIndexGraph := $SymmetricIndexGraph = Graph[VertexList[$IndexGraph], UndirectedEdge @@@ EdgeList[$IndexGraph]],

    $GraphDistanceMatrix := $GraphDistanceMatrix = GraphDistanceMatrix[$SymmetricIndexGraph],
    $GraphDistance = {v1, v2} |-> Extract[$GraphDistanceMatrix, {v1, v2}],
    $GraphFindShortestPath := $GraphFindShortestPath = FindShortestPath[$SymmetricIndexGraph, All, All],
    $GraphFindPath = Function[FindPath[$SymmetricIndexGraph, ##]]
  },
  body
];