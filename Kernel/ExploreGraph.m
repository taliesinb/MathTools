Package["GraphTools`"]

PackageImport["GeneralUtilities`"]

PackageExport["MaxVertices"]
PackageExport["MaxVerticesPerComponent"]
PackageExport["MaxEdges"]
PackageExport["MaxDepth"]
PackageExport["MaxTime"]
PackageExport["MaxFunctionEvaluations"]
PackageExport["GraphProgressFunction"]

MaxVertices::usage = "MaxVertices is an option to StateTransitionGraph."
MaxVerticesPerComponent::usage = "MaxVerticesPerComponent is an option to StateTransitionGraph."
MaxEdges::usage = "MaxEdges is an option to StateTransitionGraph."
MaxDepth::usage = "MaxDepth is an option to StateTransitionGraph."
MaxTime::usage = "MaxTime is an option to StateTransitionGraph.";
GraphProgressFunction::usage = "ProgressFunction is an option to StateTransitionGraph.";

Options[StateTransitionGraph] = Join[
  List[
    MaxVertices -> Infinity,
    MaxEdges -> Infinity,
    MaxDepth -> Infinity,
    MaxFunctionEvaluations -> Infinity,
    MaxVerticesPerComponent -> Infinity,
    MaxTime -> Infinity,
    DirectedEdges -> True,
    GraphProgressFunction -> None
  ],
  DeleteCases[Options[Graph], DirectedEdges -> _]
];

stackPushList[stack_, list_] := Scan[item |-> stack["Push", item], list];


PackageExport["StateTransitionGraph"]

SetUsage @ "
StateTransitionGraph[transition$, {state$1, state$2, $$}] starts at the initial states state$i, \
and recursively evaluates transition$[state$] to obtain a list of sucessor states, returning \
the directed graph whose vertices are the states and whose edges are the transitions between states.
StateTransitionGraph[transition$, states$, 'result$'] returns a named result.
* The possible values for result$ are one or a list of the following:
| 'Graph' | the graph of states and their transitions (default) |
| 'LabeledGraph' | a graph with states displayed as labels |
| 'CayleyGraph' | a graph with labeled edges displayed with colored arrows |
| 'IndexGraph' | a graph whose vertices are integers |
| 'VertexList' | a list of vertices in the same order as the vertices of 'IndexGraph' |
| 'EdgeList' | a list of edges |
| 'IndexEdgeList' | a list of edges referring to integer vertices, ordered like 'Vertices' |
| 'TransitionLists' | a list of transitions, each of the form {istate$, {ostate$1, ostate$2, $$}}. |
| 'IndexTransitionLists' | like 'TransitionLists', but where states are given by index |
| 'TerminationReason' | one of 'MaxVertices', 'MaxEdges', 'MaxDepth', 'MaxTime', 'MaxFunctionEvaluations', or 'Complete' |
| 'Elements' | a list of valid results |
* transition$ should be a function that takes a single state and should return a list of \
successor states.
* transition$ may return states in the form Labeled[ostate$, label$]; these labels will \
produces tagged edges of the form DirectedEdge[istate$, state$, label$].
* If transition$ is of the form {f$1, f$2, $$}, the individual functions f$i will be called \
and their successor states labeled as i$. Any of the f$i can also be of the form \
Labeled[f$, label$] or f$ -> label$ to attach a custom label label$ instead.
* States will be explored in breadth-first order.
* The following options control how the graph of states is explored:
| MaxVertices | Infinity | how many states to obtain before terminating the exploration |
| MaxEdges | Infinity | how many transitions to obtain before terminating the exploration |
| MaxDepth | Infinity | how many transitions from the initial set to explore |
| MaxTime | Infinity | maximum number of seconds to run for |
| MaxFunctionEvaluations | Infinity | maximum number of times to evaluate transition$ |
| MaxNorm | Infinity | do not explore states with norm greater than this |
| NormFunction | Automatic | function that evaluates the norm of a given state |
| DirectedEdges | True | whether to return a directed or undirected graph |
| MaxVerticesPerComponent | Infinity | how many vertices per graph component |
| GraphProgressFunction | None | a function to call with an association with information about the exploration |
* In addition, StateTransitionGraph accepts the same options as Graph.
"

Clear[StateTransitionGraph];

$graphExploreElements = {
  "TransitionLists",
  "IndexTransitionLists",
  "EdgeList",
  "IndexEdgeList",
  "VertexList",
  "Graph", "LabeledGraph", "CayleyGraph",
  "IndexGraph",
  "TerminationReason",
  "Elements"
};

$graphExploreElementsPattern = Alternatives @@ $graphExploreElements;

StateTransitionGraph::badlimit = "The setting for `` should be a positive integer or infinity.";
StateTransitionGraph::badinitstates = "The initial states spec should be a non-empty list of states."
StateTransitionGraph::badelement = "Requested element `` is not one of `` or a list of these."
StateTransitionGraph::notlist = "The successor function did not return a list for state `` (at depth ``), which had head `` instead; halting early."

StateTransitionGraph[f_, initialVertices_, opts:OptionsPattern[]] :=
  StateTransitionGraph[f, initialVertices, "Graph", opts];

If[$Notebooks,
  With[{elems = $graphExploreElements},
    FE`Evaluate[FEPrivate`AddSpecialArgCompletion["StateTransitionGraph" -> {0, 0, elems}]]
  ];
];

StateTransitionGraph[f_, initialVertices_, result:Except[_Rule], opts:OptionsPattern[]] := Scope[

  UnpackOptions[
    directedEdges, graphProgressFunction,
    maxVertices, maxVerticesPerComponent, maxEdges, maxDepth,
    maxTime, maxFunctionEvaluations
  ];

  If[!ListQ[initialVertices] || Length[initialVertices] == 0,
    Message[StateTransitionGraph::badinitstates];
    Return[$Failed, Block];
  ];

  If[!MatchQ[result, $graphExploreElementsPattern | {Repeated[$graphExploreElementsPattern]}],
    Message[StateTransitionGraph::badelement, result, $graphExploreElements];
    Return[$Failed, Block];
  ];

  Scan[
    symbol |-> If[!MatchQ[OptionValue[symbol], _Integer ? Positive | Infinity],
      Message[StateTransitionGraph::badlimit, symbol];
      Return[$Failed, Block];
    ],
    {MaxVertices, MaxEdges, MaxDepth, MaxVerticesPerComponent}
  ];

  If[ListQ[f], f = makeSuperTransitionFunc[f]];

  thisGenVertices = CreateDataStructure["Stack"];
  nextGenVertices = CreateDataStructure["Stack"];
  vertexArray = CreateDataStructure["DynamicArray"];
  vertexIndex = Data`UnorderedAssociation[];
  visitedVerftices = CreateDataStructure["HashSet"];
  initialIds = Range @ Length @ initialVertices;

  ScanThread[Set[vertexIndex[#1], #2]&, {initialVertices, initialIds}];

  If[trackDCs = IntegerQ[maxVerticesPerComponent],

    (* set up hash sets to store the sets of vertexIds in each component *)
    dcVertexSets = MapThread[
      {vertex, id} |-> (
        set = CreateDataStructure["HashSet"];
        set["Insert", id];
        set
      ),
      {initialVertices, initialIds}
    ];

    (* set up a mechanism to allow us to follow DC ids when they are merged *)
    mergedDcId = initialIds;
    mergedDcChildren = AssociationThread[initialIds, List /@ initialIds];
    vertexIdToDcId = AssociationThread[initialIds, initialIds];
    dcActive = ConstantArray[True, Length[initialIds]];

    (* set up the function that will attempt to merge two components *)
    mergeDcs = {id1, id2} |-> If[id1 =!= id2, With[
      {dcVertexSet1 = Part[dcVertexSets, id1]},
      {dcVertexSet2 = Part[dcVertexSets, id2]},
      (* echo["Merging ", id1, " ", id2]; *)
      dcVertexSet1["Union", Normal @ dcVertexSet2];
      mergedDcId[[mergedDcChildren[id2]]] = id1;
      JoinTo[mergedDcChildren[id1], mergedDcChildren[id2]];
      dcActive[[id1]] = And[dcActive[[id1]], dcActive[[id2]], dcVertexSet1["Length"] < maxVerticesPerComponent];
      dcActive[[id2]] = False;
      dcId = id1
    ]];
  ];

  Switch[graphProgressFunction,
    None,
      progress = None,
    _,
      startTime = SessionTime[];
      $currentTime := (SessionTime[] - startTime);
      $currentVertexCount := Length[vertexIndex];
      progress := graphProgressFunction[<|
        "State" -> vertex, "Successors" -> successors,
        "Depth" -> generation, "Time" -> $currentTime,
        "EdgeCount" -> edgeCount, "VertexCount" -> $currentVertexCount
      |>];
  ];

  transitionListsBag = Internal`Bag[];
  indexTransitionListsBag = Internal`Bag[];

  stackPushList[thisGenVertices, Transpose[{initialIds, initialVertices, initialIds}]];
  edgeGenerations = {}; edgeCount = 0; lastCount = 0; generation = 1; evaluations = 0;
  (* main loop *)

  If[NumberQ[maxTime],
    stopTime = SessionTime[] + maxTime;
    $timeCondition := SessionTime[] < stopTime
  ,
    $timeCondition = True
  ];

  removeStaleSuccessors = {succId, succ} |-> If[succId > lastCount, {succId, succ, dcId}, Nothing];

  While[And[Length[vertexIndex] <= maxVertices, edgeCount <= maxEdges, $timeCondition, evaluations < maxFunctionEvaluations],

    (* whenever we reach the end of this generation, move to the next generation *)
    If[thisGenVertices["EmptyQ"],
      If[(generation += 1) >= maxDepth, Break[]];
      If[nextGenVertices["EmptyQ"], Break[]];
      Swap[thisGenVertices, nextGenVertices];
    ];

    (* obtain a vertex to explore *)
    {vertexId, vertex, dcId} = (* echoAs["pop"] @  *)thisGenVertices["Pop"];
    If[trackDCs, dcId = mergedDcId[[dcId]]];

    (* call the successor function to obtain successor vertices *)
    evaluations += 1;
    successors = f[vertex];
    progress[];
    If[!ListQ[successors], Message[StateTransitionGraph::notlist, vertex, generation, Head[successors]]; Break[]];

    (* add the transitions lists to its bag *)
    Internal`StuffBag[transitionListsBag, {vertex, successors}];
    edgeCount += Length[successors];

    (* strip labels *)
    successors = (* echoAs["successors"] @  *)Replace[successors, Labeled[z_, _] :> z, {1}];

    (* detect new vertices, and obtain all the Ids of successor vertices *)
    lastCount = Length[vertexIndex];

    successorsIds = (* echoAs["successorIds"] @  *)Map[
      succ |-> Lookup[vertexIndex, Key[succ],
        vertexArray["Append", succ];
        vertexIndex[succ] = Length[vertexIndex] + 1
      ], successors];

    (* add the corresponding indexed transitions list to its bag *)
    Internal`StuffBag[indexTransitionListsBag, {vertexId, successorsIds}];

    (* if we are tracking components, find the new old ids, because these represent
    sites of potential merging, and merge together their vertex sets where necessary *)
    If[trackDCs,
      {newIds, oldIds} = SelectDiscard[successorsIds, GreaterThan[lastCount]];
      Scan[oldId |-> mergeDcs[mergedDcId[[vertexIdToDcId @ oldId]], dcId], oldIds];
      currentDcVertexSet = Part[dcVertexSets, dcId];
      currentDcVertexSet["Union", newIds];
      If[currentDcVertexSet["Length"] >= maxVerticesPerComponent, dcActive[[dcId]] = False];
      AssociateTo[vertexIdToDcId, # -> dcId& /@ successorsIds];
    ];

    If[!trackDCs || dcActive[[dcId]],
      (* add all new vertices to the next generation *)
      stackPushList[nextGenVertices, (* echoAs["next"] @  *)
        If[!DuplicateFreeQ[successorsIds],
          KeyValueMap[removeStaleSuccessors, AssociationThread[successorsIds, successors]],
          MapThread[removeStaleSuccessors, {successorsIds, successors}]
        ]
      ]
    ];
  ];

  terminationReason = Which[
    Length[vertexIndex] > maxVertices, "MaxVertices",
    edgeCount > maxEdges, "MaxEdges"
    generation > maxDepth, "MaxDepth"
    $timeCondition === False, "MaxTime",
    evaluations == maxFunctionEvaluations, "MaxFunctionEvaluations",
    !ListQ[successors], "SuccessorFunctionError",
    True, "Complete"
  ];

  (* calculate results *)
  transitionLists := transitionLists = Internal`BagPart[transitionListsBag, All];
  indexTransitionLists := indexTransitionLists = Internal`BagPart[indexTransitionListsBag, All];

  vertices := Normal[vertexArray];

  edgeSymbol = If[directedEdges, toDirectedEdge, toUndirectedEdge];
  deduper = If[directedEdges, Identity, DeleteDuplicatesBy[Sort]];

  edges := edges = deduper @ Flatten @ Apply[
    {from, to} |-> Map[edgeSymbol[from, #]&, to],
    transitionLists, {1}
  ];

  indexEdges := indexedEdges = deduper @ Flatten @ Apply[
    {from, to} |-> Map[edgeSymbol[from, #]&, to],
    indexTransitionLists, {1}
  ];

  graph := Graph[edges, GeneralUtilities`FilterOptions[opts]];
  indexGraph := Graph[indexEdges, GeneralUtilities`FilterOptions[opts]];

  resultsAssoc = <|
    "TransitionLists" :> transitionLists,
    "IndexTransitionLists" :> indexTransitionLists,
    "EdgeList" :> edges,
    "IndexEdgeList" :> indexEdges,
    "VertexList" :> vertices,
    "Graph" :> graph,
    "LabeledGraph" :> Graph[graph, VertexLabels -> Automatic],
    "CayleyGraph" :> CardinalGraph[graph],
    "IndexGraph" :> indexGraph,
    "TerminationReason" :> terminationReason,
    "Elements" :> $graphExploreElements
  |>;

  Lookup[resultsAssoc, result, $Failed]
];

makeSuperTransitionFunc[list_] := ApplyThrough[MapIndexed[makeTFuncElem, list]] /* Catenate;
makeTFuncElem[Labeled[f_, label_] | (f_ -> label_), _] := f /* Map[Labeled[#, label]&]
makeTFuncElem[f_, {i_}] := f /* Map[Labeled[#, i]&];

toDirectedEdge[from_, Labeled[to_, label_]] := DirectedEdge[from, to, label];
toDirectedEdge[from_, to_] := DirectedEdge[from, to];

toUndirectedEdge[from_, Labeled[to_, label_]] := UndirectedEdge[from, to, label];
toUndirectedEdge[from_, to_] := UndirectedEdge[from, to];
