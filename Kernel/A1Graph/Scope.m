(**************************************************************************************************)

PackageScope["GraphScope"]

PackageScope["NotInGraphScopeOfQ"]

NotInGraphScopeOfQ[graph_] := !GraphQ[$Graph] || (graph =!= $Graph)

PackageScope["$Graph"]
PackageScope["$GraphOrigin"]
PackageScope["$VertexList"]
PackageScope["$EdgeList"]
PackageScope["$EdgeTags"]
PackageScope["$VertexIndex"]
PackageScope["$VertexCount"]
PackageScope["$EdgeCount"]

PackageScope["$IndexGraph"]
PackageScope["$IndexGraphEdgeList"]

PackageScope["$MetricGraphCache"]
PackageScope["$GraphMetric"]

SetAttributes[GraphScope, HoldRest];

SetUsage @ "
GraphScope[graph$, body$] sets up some dynamically scoped variables that make it easy to
access properties and computed results from a single graph.
The following variables are blocked during the execution of GraphScope:
| $Graph | graph$ |
| $VertexList | VertexList[graph$] |
| $EdgeList | EdgeList[graph$] |
| $EdgeTags | EdgeTags[graph$] |
| $VertexIndex | VertexIndexAssociation[graph$] |
| $VertexCount | VertexCount[$graph] |
| $EdgeCount | EdgeCount[$graph] |
| $IndexGraph | IndexGraph[graph$] |
| $IndexGraphEdgeList | EdgeList[$IndexGraph] |
| $MetricGraphCache | GraphCache[$$] object for a symmetric version of the index graph |
| $GraphMetric | the current graph metric |
* All of the expensive properties are computed (and then cached) on first use.
"

GraphScope[graph_, body_] := Block[
  {
    $Graph = graph,
    $VertexCount = VertexCount @ graph,
    $EdgeCount = EdgeCount @ graph,

    $GraphOrigin := $GraphOrigin = LookupExtendedOption[$Graph, GraphOrigin],
    $VertexList := $VertexList = VertexList @ $Graph,
    $EdgeList := $EdgeList = EdgeList @ $Graph,
    $EdgeTags := $EdgeTags = Replace[EdgeTags @ $Graph, {} -> None],
    $VertexIndex := $VertexIndex = VertexIndexAssociation @ $Graph,

    $IndexGraph := $IndexGraph = ToIndexGraph @ $Graph,
    $IndexGraphEdgeList := $IndexGraphEdgeList = EdgeList @ $IndexGraph,

    $metricGraphCacheSymbol = Null,
    $MetricGraphCache := $MetricGraphCache = createMetricGraphCache[],
    $GraphMetric = Inherited
  },
  body
];

createMetricGraphCache[] := CreateGraphCache[
  Annotate[ToSymmetricGraph @ $IndexGraph, GraphMetric -> LookupAnnotation[$Graph, GraphMetric, Automatic]],
  $metricGraphCacheSymbol
];
