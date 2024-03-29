PublicFunction[ChessboardNorm]

ChessboardNorm[e_] := Max @ Abs @ e;

(**************************************************************************************************)

PublicFunction[L1Norm]

L1Norm[e_] := Total @ Abs @ e;

(**************************************************************************************************)

$metricDistanceOptions = {
  GraphMetric -> Inherited
};

PrivateVariable[$metricUsage]

$metricUsage = STrim @ "
* The setting of GraphMetric determines how the metric is computed:
| Inherited | use the GraphMetric present in graph$ (default) |
| Automatic | length of the shortest path in graph$ |
| 'Chessboard' | maximum of the per-cardinal distances |
| 'Euclidean' | square root of sum of squares per-cardinal distances |
| {s$1, s$2, $$} | signature of the metric, of same length as cardinals |
| QuadraticForm[$$] | apply the quadratic form to the per-cardinal distances |
| func$ | apply func$ to an association of per-cardinal distances |
";

(**************************************************************************************************)

PublicFunction[MetricDistance]

SetUsage @ "
MetricDistance[graph$, v$1, v$2] computes a graph distance between v$1 and v$2.
* If either v$1 or v$2 is All, a function will be returned.
<*$metricUsage*>
"

declareGraphCacheFriendly[MetricDistance]

Options[MetricDistance] = $metricDistanceOptions;

Default[MetricDistance, 3] = All;

MetricDistance[graph_, vertex1_, Optional[vertex2:Except[_Rule]], OptionsPattern[]] := Scope[

  metric = chooseMetric[graph, OptionValue @ GraphMetric];
  If[metric === Auto,
    Return @ GraphDistance[graph, vertex1, Rep[vertex2, All -> Sequence[]]]];

  If[!EdgeTaggedGraphQ[graph], ReturnFailed["nottaggraph"]];

  toMetricDistanceOperator[metric] @ TaggedGraphDistance[graph, vertex1, vertex2]
]

chooseMetric[graph_, Inherited] := LookupAnnotation[graph, GraphMetric, Auto];
chooseMetric[graph_, value_] := value;

(**************************************************************************************************)

PublicFunction[MetricDistanceMatrix]

SetUsage @ "
MetricDistanceMatrix[graph$] returns a matrix giving the distances between every pair \
of vertices.
<*$metricUsage*>
"

declareGraphCacheFriendly[MetricDistanceMatrix]

Options[MetricDistanceMatrix] = $metricDistanceOptions;

MetricDistanceMatrix[graph_, OptionsPattern[]] := Scope[

  metric = chooseMetric[graph, OptionValue @ GraphMetric];
  If[metric === Auto, Return @ GraphDistanceMatrix @ graph];

  If[!EdgeTaggedGraphQ[graph], ReturnFailed["nottaggraph"]];

  toMetricDistanceOperator[metric] @ TaggedGraphDistanceMatrix[graph]
];

(**************************************************************************************************)

PublicFunction[MetricFindShortestPath]

SetUsage @ "
MetricFindShortestPath[graph$, v$1, v$2] returns the shortest path between v$1 and v$2.
* If either or both v$1 and v$2 are All, a MetricShortestPathFunction will be returned.
<*$metricUsage*>
"

Options[MetricFindShortestPath] = $metricDistanceOptions;

declareGraphCacheFriendly[MetricFindShortestPath]

MetricFindShortestPath[graph_, start_, end_, OptionsPattern[]] := Scope[

  metric = chooseMetric[graph, OptionValue @ GraphMetric];
  If[metric === Auto, Return @ FindShortestPath[graph, start, end]];

  If[!EdgeTaggedGraphQ[graph], ReturnFailed["nottaggraph"]];

  data = computeShortestPathFunctionData[graph, metric];

  If[start =!= All && end =!= All,
    findShortestPath[start, end, data],
    ConstructNoEntry[MetricShortestPathFunction, {start, end}, data]
  ]
];

declareGraphCacheFriendly[computeShortestPathFunctionData];

computeShortestPathFunctionData[graph_, metric_] := Scope[

  distanceMatrix = toMetricDistanceOperator[metric] @ TaggedGraphDistanceMatrix[graph];

  adjacentVertexTable = adjacentEdgeTags = Repeat[{}, VertexCount @ graph];
  Scan[
    Apply[{a, b, t} |-> (
        AppTo[Part[adjacentVertexTable, a], b];
        AppTo[Part[adjacentVertexTable, b], a];
        AppTo[Part[adjacentEdgeTags, a], t];
        AppTo[Part[adjacentEdgeTags, b], t];
    )],
    EdgeList @ graph
  ];

  {
    distanceMatrix,
    adjacentVertexTable,
    adjacentEdgeTags,
    CardinalList @ graph
  }
];

(**************************************************************************************************)

PublicFunction[MetricShortestPathFunction]

declareFormatting[
  MetricShortestPathFunction[spec_, data_] ? HoldNoEntryQ :>
    MetricShortestPathFunction[spec, Skeleton @ Len @ F @ data]
];

fsp_MetricShortestPathFunction[args__] := fspEval1[fsp, args];

fspEval1[MetricShortestPathFunction[{All, All}, data_], v1_, v2_] :=
  findShortestPath[v1, v2, data];

fspEval1[MetricShortestPathFunction[{All, v2_}, data_], v1_] :=
  findShortestPath[v1, v2, data];

fspEval1[MetricShortestPathFunction[{v1_, All}, data_], v2_] :=
  findShortestPath[v1, v2, data];

findShortestPath[start_, end_, {distanceMatrix_, adjacentVertexTable_, adjacentEdgeTags_, cardinals_}] := Scope[
  vertex = start;
  pathBag = Bag[{start}];
  moveCounts = ConstantAssociation[cardinals, 0];
  While[vertex =!= end,
    adjacentVertices = Part[adjacentVertexTable, vertex];
    adjacentTags = Part[adjacentEdgeTags, vertex];
    distanceTies = MinimumIndices @ Part[distanceMatrix, adjacentVertices, end];
    bestIndex = MinimumBy[distanceTies, moveCounts[Part[adjacentTags, #]]&];
    vertex = Part[adjacentVertices, bestIndex];
    moveCounts[Part[adjacentTags, bestIndex]] += 1;
    StuffBag[pathBag, vertex];
  ];
  BagPart[pathBag, All]
];

(**************************************************************************************************)

toMetricDistanceOperator = Case[
  "Euclidean"                       := RootSumSquare;
  "Chessboard"                      := ChessboardMetric;
  qf_QuadraticFormObject            := Values /* ToPacked /* qf;
  list_List ? RealVectorQ           := SignatureMetric[list];
  func_ ? MightEvaluateWhenAppliedQ := func /* checkMetricNumeric;
  n_Int                             := PowerMetric[n];
  expr_                             := Message[General::badgmetricfn, expr];
];

General::badgmetricfn =
  "Setting GraphMetric -> `` should be either Automatic, \"Euclidean\", \"Chessboard\", QuadraticForm[...], {...}, or a function that will evaluate when applied."

General::badgmetricfnres =
  "GraphMetric function returned a non-numeric result."

checkMetricNumeric[value_] /; NumericQ[value] || RealVectorQ[value] := value;
checkMetricNumeric[_] := (Message[General::badgmetricfnres]; 0);

(**************************************************************************************************)

PublicFunction[ChessboardMetric]

ChessboardMetric[array_] := Map[ChessboardNorm, Transpose @ Values @ array];

(**************************************************************************************************)

PublicFunction[SignatureMetric]

SignatureMetric[list_List][array_] := Sqrt @ Dot[list, Power[N @ Values @ array, 2]];

(**************************************************************************************************)

PublicFunction[RootSumSquare]

RootSumSquare[assoc_Assoc] := RootSumSquare @ Values @ assoc;
RootSumSquare[array_] := Sqrt @ Total @ Power[N @ array, 2];

PowerMetric[n_][array_] := Surd[Total @ Power[N @ Values @ array, n], n];

(**************************************************************************************************)

PublicFunction[QuadraticFormMetric]

QuadraticFormMetric[matrix_][vectors_ ? MatrixQ] := Sqrt @ Dot[Transpose @ vectors, matrix, vectors];
QuadraticFormMetric[matrix_][vector_ ? VecQ] := Sqrt @ Dot[vector, matrix, vector];

(**************************************************************************************************)

$taggedDistanceOptions = {
  DirectedEdges -> False,
  MaxDepth -> Inf
};

PublicFunction[TaggedGraphDistance]

Options[TaggedGraphDistance] = $taggedDistanceOptions;

Default[TaggedGraphDistance, 3] = All;
Default[TaggedGraphDistance, 4] = All;

declareGraphCacheFriendly[TaggedGraphDistance]

TaggedGraphDistance[graph_, vertex1_, vertex2_., tagSpec_., OptionsPattern[]] := Scope[

  {tagList, edgeTags} = processTagSpec[TaggedGraphDistance, graph, tagSpec];

  If[!directedEdges, graph = ToSymmetricGraph @ graph];
  dfunc = If[vertex2 === All, GraphDistance[#, vertex1]&, GraphDistance[#, vertex1, vertex2]&];

  result = AssocMap[
    tag |-> dfunc[toMaskWeightedGraph[graph, edgeTags, tag]],
    tagList
  ];

  If[$stripList, F @ result, result]
];

(**************************************************************************************************)

PublicFunction[TaggedGraphDistanceMatrix]

Options[TaggedGraphDistanceMatrix] = $taggedDistanceOptions;

Default[TaggedGraphDistanceMatrix, 2] = All;

declareGraphCacheFriendly[TaggedGraphDistanceMatrix]

TaggedGraphDistanceMatrix[graph_, tagSpec_., OptionsPattern[]] := Scope[

  {tagList, edgeTags} = processTagSpec[TaggedGraphDistanceMatrix, graph, tagSpec];

  UnpackOptions[maxDepth, directedEdges];

  If[!directedEdges, graph = ToSymmetricGraph @ graph];

  result = AssocMap[
    tag |-> GraphDistanceMatrix[toMaskWeightedGraph[graph, edgeTags, tag], maxDepth],
    tagList
  ];

  If[$stripList, F @ result, result]
];

(**************************************************************************************************)

metricFail[head_, msgname_, args___] := (
  Message[MessageName[head, msgname], args];
  Return[$Failed, Block];
);

General::nottaggraph = "First argument should be an edge-tagged graph.";
General::badtagspec = "Tag specification `` is not All, a tag, or a list of tags in the graph."

processTagSpec[head_, graph_, tagSpec_] := Module[
  {allTags, tagList},

  If[!EdgeTaggedGraphQ[graph], metricFail[head, "nottaggraph"]];

  allTags = CardinalList @ graph;
  $stripList = False;
  tagList = Switch[tagSpec,
    All, allTags,
    _List, tagSpec,
    _, $stripList = True; List @ tagSpec
  ];

  If[!SubsetQ[allTags, tagList], metricFail[head, "badtagspec", tagSpec]];

  {tagList, EdgeTags @ graph}
];

(* TODO: cache these using a weak expression table in case *)
toMaskWeightedGraph[graph_, edgeTags_, tag_] :=
  Graph[graph, EdgeWeight -> Boole[Thread[edgeTags != tag]]];
