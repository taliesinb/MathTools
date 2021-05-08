Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

PackageExport["MetricSignature"]

SetUsage @ "MetricSignature is an option to LatticeNorm and EuclideanNorm."

$latticeNormUsage = "
* NormFunction takes the following options:
| QuadrationForm[%%] | |
| HomogenousForm[%%] | |
| None | use the ordinary graph distance |
"
(**************************************************************************************************)

$latticeMetricFunctionOptions = {
  DirectedEdges -> False,
  NormFunction -> Automatic,
  MaxDepth -> Infinity,
  MetricSignature -> Automatic
};


(**************************************************************************************************)

PackageExport["LatticeDistanceMatrix"]

Options[LatticeDistanceMatrix] = $latticeMetricFunctionOptions;

LatticeDistanceMatrix[graph_, opts:OptionsPattern[]] := Scope[
  result = TaggedGraphDistanceMatrix[graph, FilterOptions @ opts];
  If[FailureQ[result], ReturnFailed[]];
  toLatticeMetric[LatticeDistanceMatrix, result, OptionValue[{MetricSignature, NormFunction}]]
];

(**************************************************************************************************)

PackageExport["LatticeDistance"]

Options[LatticeDistance] = $latticeMetricFunctionOptions;

Default[LatticeDistance, 3] = All;

LatticeDistance[graph_][args___] := LatticeDistance[graph, args];
LatticeDistance[graph_, vertex1_, Optional[vertex2:Except[_Rule]], opts:OptionsPattern[]] := Scope[
  result = TaggedGraphDistance[graph, vertex1, vertex2, FilterOptions @ opts];
  If[FailureQ[result], ReturnFailed[]];
  toLatticeMetric[LatticeDistance, result, OptionValue[{MetricSignature, NormFunction}]]
];

(**************************************************************************************************)

toLatticeMetric[_, distances_, {Automatic, Automatic}] := RootTotalSquare @ distances;

General::badsignature = "The setting MetricSignature -> `` should be either Automatic or a list matching the number of cardinals (``).";

(**************************************************************************************************)

PackageExport["LatticeNorm"]
PackageExport["EuclideanNorm"]

toLatticeMetric[head_, distances_, {signature_, normFunction_}] := Module[{},
  If[!ListQ[signature] || Length[distances] =!= Length[signature], metricFail[head, "badsignature", signature, Length @ distances]];
  RootTotalSquare[distances, signature]
];

(**************************************************************************************************)

NormVector[array_, p_] := Surd[Total @ Power[N @ Values @ array, p], p];

RootTotalSquare[array_] := Sqrt @ Total @ Power[N @ Values @ array, 2];

RootTotalSquare[array_, signature_] := Sqrt @ Total @ Times[signature, Power[N @ Values @ array, 2]];

(**************************************************************************************************)

PackageExport["FindShortestLatticePath"]

Options[FindShortestLatticePath] = $latticeMetricFunctionOptions;

computeShortestPathData[graph_, opts___] := {
  LatticeDistanceMatrix[graph, opts],
  VertexAdjacencyTable[graph]
};

FindShortestLatticePath[graph_, start_, end_, opts:OptionsPattern[]] := Scope[
  System`Private`ConstructNoEntry[
    LatticeShortestPathFunction, {start, end}, computeShortestPathData[graph, opts]
  ]
];

FindShortestLatticePath[graph_, start:Except[All], end:Except[All], opts:OptionsPattern[]] := Scope[
  findShortestPath[{start, end}, computeShortestPathData[graph, opts]]
];

(**************************************************************************************************)

PackageExport["LatticeShortestPathFunction"]

declareFormatting[
  LatticeShortestPathFunction[spec_, data_] ? System`Private`HoldNoEntryQ :>
    LatticeShortestPathFunction[spec, Skeleton @ Length @ First @ data]
];

fsp_LatticeShortestPathFunction[args__] := fspEval1[fsp, args];

fspEval1[LatticeShortestPathFunction[{All, All}, data_], v1_, v2_] :=
  findShortestPath[{v1, v2}, data];

fspEval1[LatticeShortestPathFunction[{All, v2_}, data_], v1_] :=
  findShortestPath[{v1, v2}, data];

fspEval1[LatticeShortestPathFunction[{v1_, All}, data_], v2_] :=
  findShortestPath[{v1, v2}, data];

findShortestPath[{start_, end_}, {distanceMatrix_, adjacencyTable_}] := Scope[
  vertex = start;
  pathBag = Internal`Bag[{start}];
  While[vertex =!= end,
    adjacent = Part[adjacencyTable, vertex];
    vertex = Part[adjacent, MinimumIndex @ Part[distanceMatrix, adjacent, end]];
    Internal`StuffBag[pathBag, vertex];
  ];
  Internal`BagPart[pathBag, All]
];


(**************************************************************************************************)

$metricFunctionOptions = {
  DirectedEdges -> False,
  MaxDepth -> Infinity
};

PackageExport["TaggedGraphDistance"]

Options[TaggedGraphDistance] = $metricFunctionOptions;

Default[TaggedGraphDistance, 3] = All;
Default[TaggedGraphDistance, 4] = All;

TaggedGraphDistance[graph_, vertex1_, vertex2_., tagSpec_., OptionsPattern[]] := Scope[

  {tagList, edgeTags} = processTagSpec[TaggedGraphDistance, graph, tagSpec];

  graph = toPossiblyUndirectedGraph[graph, OptionValue[DirectedEdges]];
  dfunc = If[vertex2 === All, GraphDistance[#, vertex1]&, GraphDistance[#, vertex1, vertex2]&];

  result = AssociationMap[
    tag |-> dfunc[toMaskWeightedGraph[graph, edgeTags, tag]],
    tagList
  ];

  If[$stripList, First @ result, result]
];

(**************************************************************************************************)

PackageExport["TaggedGraphDistanceMatrix"]

Options[TaggedGraphDistanceMatrix] = $metricFunctionOptions;

Default[TaggedGraphDistanceMatrix, 2] = All;

TaggedGraphDistanceMatrix[graph_, tagSpec_., OptionsPattern[]] := Scope[

  {tagList, edgeTags} = processTagSpec[TaggedGraphDistanceMatrix, graph, tagSpec];

  UnpackOptions[maxDepth, directedEdges];

  graph = toPossiblyUndirectedGraph[graph, directedEdges];

  result = AssociationMap[
    tag |-> GraphDistanceMatrix[toMaskWeightedGraph[graph, edgeTags, tag], maxDepth],
    tagList
  ];

  If[$stripList, First @ result, result]
];

(**************************************************************************************************)

metricFail[head_, msgname_, args___] := (
  Message[MessageName[head, msgname], args];
  Return[$Failed, Block];
);

General::notetaggraph = "First argument should be an edge-tagged graph.";
General::badtagspec = "Tag specification `` is not All, a tag, or a list of tags in the graph."

processTagSpec[head_, graph_, tagSpec_] := Module[
  {allTags, tagList},

  If[!EdgeTaggedGraph[graph], metricFail[head, "nottagged"]];

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

toPossiblyUndirectedGraph[graph_ ? DirectedGraphQ, False] :=
    Graph[VertexList[graph], EdgeList[graph] /. DirectedEdge -> UndirectedEdge];

toPossiblyUndirectedGraph[graph_, _] := graph;


(**************************************************************************************************)

PackageScope["AllPairsShortestTaggedPaths"]

SetUsage @ "
AllPairsShortestTaggedPaths[quiver$] returns {distances$, moves$}, where distances$ is an array of \
shape (n$, n$, c$, 2), and moves$ is a matrix of shape (n$, n$, c$), where n$ is the number of vertices \
and c$ is the number of cardinals.
The option DirectedEdges (which defaults to False) controls whether edge orientation is respected.
"

Options[AllPairsShortestTaggedPaths] = $metricFunctionOptions;

AllPairsShortestTaggedPaths[graph_, OptionsPattern[]] := Scope[
  If[!EdgeTaggedGraph[graph], ReturnFailed["nottagged"]];

  tagList = CardinalList[graph];
  If[!ListQ[tagList], ReturnFailed["nottagged"]];
  numTags = Length @ tagList;

  numVertices = VertexCount[graph];

  max = 2^30;
  shape = {numTags, numVertices, numVertices};
  moves = ConstantArray[0, shape];
  tDists = ConstantArray[max, shape];
  uDists = ConstantArray[max, shape];

  Do[
    (* you can reach any vertex from itself in 0 steps by moving to itself *)
    Part[moves, All, i, i] = i;
    Part[uDists, All, i, i] = 0;
    Part[tDists, All, i, i] = 0;
  ,
    {i, numVertices}
  ];

  UnpackOptions[directedEdges];
  indexEdgeList = EdgeList @ IndexGraph @ graph;
  indexEdgeList[[All, 3]] = ArrayLabelIndices[EdgeTags @ graph, tagList];
  edgeTuples = Flatten[
    If[Not[directedEdges] && DirectedGraphQ[graph],
      Function[{a, b, t}, {{a, b, t}, {b, a, t}}] @@@ indexEdgeList
    ,
      ReplaceAll[indexEdgeList, {
        DirectedEdge[a_, b_, t_] :> {{a, b, t}},
        UndirectedEdge[a_, b_, t_] :> {{a, b, t}, {b, a, t}}
      }]
    ],
    1
  ];

  Scan[setupNonTagDistanceForEdge, edgeTuples];
  Scan[setupTagDistanceForEdge, edgeTuples];

  {tDists, uDists, moves} = $compiledAPSTPLoop[tDists, uDists, moves, numVertices, numTags];

  AssociationThread[tagList, #]& /@ {tDists, uDists, moves}
];

setupNonTagDistanceForEdge[{i_, j_, t_}] := (
  Part[tDists, All, i, j] = 0;
  Part[uDists, All, i, j] = 1;
  Part[moves, All, i, j] = j;
);

setupTagDistanceForEdge[{i_, j_, t_}] := (
  Part[tDists, t, i, j] = 1;
  Part[moves, t, i, j] = j;
);

$compiledAPSTPLoop = Compile[
  {{tDistsIn, _Integer, 3}, {uDistsIn, _Integer, 3}, {movesIn, _Integer, 3}, {numVertices, _Integer}, {numTags, _Integer}}, Module[
  {tDists = tDistsIn, uDists = uDistsIn, moves = movesIn, uij, tij, uikj, tikj},
  Do[
    Do[
      tij = Part[tDists, t, i, j]; tikj = Part[tDists, t, i, k] + Part[tDists, t, k, j];
      If[(tikj < tij) || (tikj == tij),
        uij = Part[uDists, t, i, j]; uikj = Part[uDists, t, i, k] + Part[uDists, t, k, j];
        If[(uikj < uij),
          Part[uDists, t, i, j] = uikj;
          Part[tDists, t, i, j] = tikj;
          Part[moves, t, i, j] = Part[moves, t, i, k];
        ];
      ];
    ,
      {i, numVertices}, {j, numVertices}, {t, numTags}
    ];
  ,
    {k, numVertices}
  ];
  {
    tDists,
    uDists,
    moves
  }
]];