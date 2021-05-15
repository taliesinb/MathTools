Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


PackageExport["MaxVertices"]
PackageExport["MaxVerticesPerComponent"]
PackageExport["MaxEdges"]
PackageExport["MaxDepth"]
PackageExport["MaxTime"]
PackageExport["MaxFunctionEvaluations"]
PackageExport["MaxNorm"]
PackageExport["ProgressFunction"]
PackageExport["IncludeFrontier"]
PackageExport["DepthTermination"]

MaxVertices::usage = "MaxVertices is an option to StateTransitionGraph."
MaxVerticesPerComponent::usage = "MaxVerticesPerComponent is an option to StateTransitionGraph."
MaxEdges::usage = "MaxEdges is an option to StateTransitionGraph."
MaxDepth::usage = "MaxDepth is an option to StateTransitionGraph."
MaxTime::usage = "MaxTime is an option to StateTransitionGraph.";
ProgressFunction::usage = "ProgressFunction is an option to StateTransitionGraph.";
MaxNorm::usage = "MaxNorm is an option to StateTransitionGraph.";
IncludeFrontier::usage = "IncludeFrontier is an option to StateTransitionGraph.";
DepthTermination::usage = "DepthTermination is an option to StateTransitionGraph."

PackageExport["StateTransitionGraph"]

DeclareArgumentCount[StateTransitionGraph, 2];

declareSyntaxInfo[StateTransitionGraph, {_, _, OptionsPattern[]}];

Options[StateTransitionGraph] = JoinOptions[
  List[
    MaxVertices -> Infinity,
    MaxEdges -> Infinity,
    MaxDepth -> Infinity,
    MaxFunctionEvaluations -> Infinity,
    MaxVerticesPerComponent -> Infinity,
    MaxTime -> Infinity,
    DirectedEdges -> True,
    ProgressFunction -> None,
    NormFunction -> Automatic,
    MaxNorm -> None,
    IncludeFrontier -> True,
    DepthTermination -> "Immediate",
    SelfLoops -> True
  ],
  $simpleGraphOptionRules
];

stackPushList[stack_, list_] := Scan[item |-> stack["Push", item], list];
arrayAppendList[array_, list_] := Scan[item |-> array["Append", item], list];

SetUsage @ "
StateTransitionGraph[transition$, {state$1, state$2, $$}] starts at the initial states state$i, \
and recursively evaluates transition$[state$] to obtain a list of sucessor states, returning \
the directed graph whose vertices are the states and whose edges are the transitions between states.
StateTransitionGraph[transition$, states$, 'element$'] returns a named element.
* The possible values for element$ are one or a list of the following:
| 'Graph' | the graph of states and their transitions (default) |
| 'LabeledGraph' | a graph with states displayed as labels |
| 'CayleyGraph' | a graph with labeled edges displayed with colored arrows |
| 'VertexList' | a list of vertices in the same order as the vertices of 'IndexGraph' |
| 'EdgeList' | a list of edges |
| 'TransitionLists' | a list of transitions, each of the form {istate$, {ostate$1, ostate$2, $$}}. |
| 'VertexDepthAssociation' | an association between a vertex and its distance from the initial set |
| 'VertexTagDepthAssociation' | an association between labels and minimum distances counting only that label |
| 'TerminationReason' | one of 'MaxVertices', 'MaxEdges', 'MaxDepth', 'MaxTime', 'MaxFunctionEvaluations', or 'Complete' |
| 'EdgeLabels' | a list of edge labels in the same order as 'EdgeList' |
| 'Elements' | a list of valid element names |
* Most of the above elements, e.g. 'Graph', 'EdgeList', etc. have a corresponding element \
'IndexGraph', 'IndexEdgeList', etc. that label vertices with consecutive integers in the same \
order as 'VertexList'.
* The elements 'VertexDepthAssociation' and 'VertexTagDepthAssociation' also have 'prop$List' forms \
that use the same order as 'VertexList'.
* transition$ should be a function that takes a single state and should return a list of \
successor states.
* transition$ may return states in the form Labeled[ostate$, label$]; these labels will \
produces tagged edges of the form DirectedEdge[istate$, state$, label$].
* If transition$ is of the form {f$1, f$2, $$}, the individual functions f$i will be called \
and their successor states labeled as i$. Any of the f$i can also be of the form \
Labeled[f$, label$] or f$ -> label$ to attach a custom label label$ instead.
* States will be explored in breadth-first order.
* StateTransitionGraph accepts all the options accepted by Graph, in addition to the following:
| MaxVertices | Infinity | how many states to obtain before terminating the exploration |
| MaxEdges | Infinity | how many transitions to obtain |
| MaxDepth | Infinity | how many transitions from the initial set to explore |
| MaxTime | Infinity | maximum number of seconds to run for |
| MaxFunctionEvaluations | Infinity | maximum number of times to evaluate transition$ |
| MaxNorm | Infinity | ignore states with norm greater than this |
| NormFunction | Automatic | function that evaluates the norm of a given state |
| DirectedEdges | True | whether to return a directed or undirected graph |
| SelfLoops | True | whether to include transitions from a state to itself |
| MaxVerticesPerComponent | Infinity | how many vertices per graph component |
| IncludeFrontier | True | whether to include edges to vertices beyond maximum depth |
| DepthTermination | 'Immediate' | specifies when termination take effect |
| ProgressFunction | None | a function to call with an association with information about the exploration |
* For ProgressFunction -> p$, p$ will be called with an association containing the following keys:
| 'State' | the current state being explored |
| 'Successors' | the list of successors that was returned by transition$ |
| 'Depth' | the number of transitions since the initial set |
| 'Time' | the number of seconds since exploration began |
| 'EdgeCount' | the number of edges seen so far |
| 'VertexCount' | the number of vertices seen so far |
* For DepthTermination -> 'Delayed', the depth currently being explored will be finished before termination occurs.
* If MaxDepth -> <|label$1 -> d$1, $$, label$n -> d$n|> is specified, a per-label maximum depth will be applied, so \
that a vertex will be explored as long as it can be reached in under d$i steps of edges labeled label$i.
"

$backEdgeElements = {
  "SpanningTree", "IndexSpanningTree"
};

$vertexDepthElements = {
  "VertexDepthList", "VertexDepthAssociation", "VertexTagDepthList", "VertexTagDepthAssociation"
};

$stgElements = {
  "TransitionLists", "IndexTransitionLists",
  "EdgeList", "IndexEdgeList",
  "VertexList",
  "Graph", "IndexGraph", "CayleyGraph", "LabeledGraph",
  Splice[$backEdgeElements],
  Splice[$vertexDepthElements],
  "TerminationReason", "EdgeLabels",
  "Elements"
};

$stgElementsPattern = Alternatives @@ $stgElements;

StateTransitionGraph::badlimit = "The setting for `` should be a positive integer or infinity.";
StateTransitionGraph::badinitstates = "The initial states spec should be a non-empty list of states."
StateTransitionGraph::badelement = "Requested element `` is not one of `` or a list of these."
StateTransitionGraph::notlist = "The successor function did not return a list for state `` (at depth ``), which had head `` instead; halting early."
StateTransitionGraph::badnorm = "The setting for NormFunction should be function that takes a state and returns a positive number."
StateTransitionGraph::badmaxdepth = "The setting for MaxDepth should be an integer, Infinity, or an association from labels to depths."
StateTransitionGraph::undedgelabels = "Cannot obtain element \"EdgeLabels\" when DirectedEdges -> False.";

StateTransitionGraph[f_, initialVertices_, opts:OptionsPattern[]] :=
  StateTransitionGraph[f, initialVertices, "Graph", opts];

declareFunctionAutocomplete[StateTransitionGraph, {0, 0, $stgElements}];

checkNorm[r_ ? NonNegative] := r;
checkNorm[_] := (Message[StateTransitionGraph::badnorm]; Return[$Failed, Module]);

StateTransitionGraph[f_, initialVertices_, result:Except[_Rule], opts:OptionsPattern[]] := Scope[

  UnpackOptions[
    directedEdges, progressFunction, normFunction,
    maxVertices, maxVerticesPerComponent, maxEdges, maxDepth,
    maxTime, maxFunctionEvaluations, maxNorm,
    includeFrontier, depthTermination, selfLoops
  ];

  If[!ListQ[initialVertices] || Length[initialVertices] == 0,
    ReturnFailed["badinitstates"];
  ];

  initialVertices = DeleteDuplicates[initialVertices];

  If[!MatchQ[result, $stgElementsPattern | {Repeated[$stgElementsPattern]}],
    ReturnFailed["badelement", result, TextString[Row[$stgElements, ", "]]];
  ];

  Scan[
    symbol |-> If[!MatchQ[OptionValue[symbol], $posIntOrInfinityP],
      ReturnFailed["badlimit", symbol];
    ],
    {MaxVertices, MaxEdges, MaxVerticesPerComponent, MaxTime, MaxFunctionEvaluations}
  ];

  depthLabels = Automatic;
  If[MatchQ[maxDepth, <|All -> _Integer|>],
    depthLabels = DeepCases[f, Labeled[_, label_] :> label];
    depthLabels = DeleteDuplicates @ Replace[depthLabels, Negated[c_] :> c, {1}];
    If[depthLabels === {}, ReturnFailed["badmaxdepth"]];
    maxDepth = ConstantAssociation[depthLabels, First @ maxDepth];
  ];

  Switch[maxDepth,
    _Integer ? Positive | Infinity,
      trackLabelDepth = limitLabelDepth = False,
    Association[Repeated[_ -> (_Integer | Infinity)]],
      trackLabelDepth = True;
      limitLabelDepth = True;
      {depthLabels, depthValues} = KeysValues[maxDepth];
      maxDepth = Total[maxDepth];
      With[{compare = Append[depthValues, maxDepth]},
        withinDepthTest = depth |-> Apply[And, MapThread[LessEqual, {depth, compare}]];
        withinDepthTest = withinDepthTest;
      ],
    _,
      ReturnFailed["badmaxdepth"];
  ];

  trackLabelDepth = trackLabelDepth || ContainsQ[result, Alternatives @@ $vertexDepthElements];

  If[trackLabelDepth,
    SetAutomatic[depthLabels, DeepUniqueCases[f, Labeled[_, label_] :> label]];
    labels = DeepUniqueCases[f, Labeled[_, label_] :> label];
    numDepthLabels = Length[depthLabels];
    labelToPos = AssociationThread[depthLabels, Range[numDepthLabels]];
    zeroDepthVector = ConstantArray[0, numDepthLabels + 1];
  ];


  If[NumberQ[maxNorm],
    If[normFunction === None, checkNorm[Null]]; (* trigger message *)
    normTest = normFunction /* LessEqual[maxNorm];
  ,
    normTest = None;
  ];

  If[ListQ[f], f = makeSuperTransitionFunc[f]];

  thisGenVertices = CreateDataStructure["Stack"];
  nextGenVertices = CreateDataStructure["Stack"];
  vertexArray = CreateDataStructure["DynamicArray"];
  vertexIndex = Data`UnorderedAssociation[];
  visitedVerftices = CreateDataStructure["HashSet"];
  initialIds = Range @ Length @ initialVertices;

  arrayAppendList[vertexArray, initialVertices];

  ScanThread[Set[vertexIndex[#1], #2]&, {initialVertices, initialIds}];
  If[trackLabelDepth,
    depthVectorAssoc = ConstantAssociation[initialIds, zeroDepthVector]];

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
      dcVertexSet1["Union", Normal @ dcVertexSet2];
      mergedDcId[[mergedDcChildren[id2]]] = id1;
      JoinTo[mergedDcChildren[id1], mergedDcChildren[id2]];
      dcActive[[id1]] = And[dcActive[[id1]], dcActive[[id2]], dcVertexSet1["Length"] < maxVerticesPerComponent];
      dcActive[[id2]] = False;
      dcId = id1
    ]];
  ];

  Switch[progressFunction,
    None,
      progress = None,
    _,
      startTime = SessionTime[];
      $currentTime := (SessionTime[] - startTime);
      $currentVertexCount := Length[vertexIndex];
      progress := progressFunction[<|
        "State" -> vertex, "Successors" -> successors,
        "Depth" -> generation, "Time" -> $currentTime,
        "EdgeCount" -> edgeCount, "VertexCount" -> $currentVertexCount
      |>];
  ];

  transitionListsBag = Internal`Bag[];
  indexTransitionListsBag = Internal`Bag[];

  stackPushList[thisGenVertices, Transpose[{initialIds, initialVertices, initialIds}]];

  If[NumberQ[maxTime],
    stopTime = SessionTime[] + maxTime;
    $timeCondition := SessionTime[] < stopTime
  ,
    $timeCondition = True
  ];

  (* build the function that decides which vertices to visit next *)
  removeStaleSuccessors = {succId, succ} |-> If[$staleTest, {succId, succ, dcId}, Nothing];
  If[normTest =!= None,
    removeStaleSuccessors //= ReplaceAll[$staleTest :> And[$staleTest, normTest[succ]]]
  ];
  If[limitLabelDepth,
    removeStaleSuccessors //= ReplaceAll[$staleTest -> shouldRevisitAssoc[succId]],
    removeStaleSuccessors //= ReplaceAll[$staleTest -> succId > lastCount]
  ];
  If[trackBackEdges = ContainsQ[result, Alternatives @ $backEdgeElements],
    backEdgesAssociation = Association[];
    removeStaleSuccessors //= Insert[removeStaleSuccessors, $Unreachable]; (* TODO: implement me *)
  ];

  (* set up blocks of code to run at specific parts of the main loop *)

  If[trackLabelDepth,
    $extractLabelBlock := (
      labels = Replace[successors, {Labeled[_, l_] :> Replace[l, Negated[c_] :> c], _ -> None}, {1}];
    );

    If[limitLabelDepth,
      $trackLabelDepthBlock := (
        currentDepthVector = depthVectorAssoc[vertexId];
        shouldRevisitAssoc = Association @ MapThread[
          {id, labelPos} |-> (
            newDepth = currentDepthVector + UnitVector[numDepthLabels + 1, labelPos];
            If[id > lastCount,
              (* fresh vertices should only be visited if they pass the depth test.
              if they fail we might revisit them later if their depth gets shallower from a new path *)
              depthVectorAssoc[id] = newDepth;
              id -> withinDepthTest[newDepth]
            ,
              (* update the depth of stale vertices *)
              oldDepth = depthVectorAssoc[id];
              depthVectorAssoc[id] = MapThread[Min, {oldDepth, newDepth}];
              (* and revisit them if their new depth starts to pass *)
              id -> And[withinDepthTest[newDepth], !withinDepthTest[oldDepth]]
            ]
          ),
          {successorsIds, Lookup[labelToPos, labels, numDepthLabels + 1]}
        ];
      );
    ,
      $trackLabelDepthBlock := (
        currentDepthVector = depthVectorAssoc[vertexId];
        ScanThread[
          {id, labelPos} |-> (
            newDepth = currentDepthVector + UnitVector[numDepthLabels + 1, labelPos];
            depthVectorAssoc[id] = If[id > lastCount, newDepth,
              MapThread[Min, {depthVectorAssoc[id], newDepth}]
            ]),
          {successorsIds, Lookup[labelToPos, labels, numDepthLabels + 1]}
        ];
      );
    ];
  ];

  If[ContainsQ[result, "EdgeLabels"],
    If[directedEdges === False, ReturnFailed["undedgelabels"]];
    edgeLabelsBag = Internal`Bag[];
    $extractLabelBlock := (
      labels = Replace[successors, {Labeled[_, l_] :> l, _ -> None}, {1}];
      Internal`StuffBag[edgeLabelsBag, labels, 1];
      labels = Replace[labels, Negated[c_] :> c, {1}];
    );
  ];

  If[trackDCs,
    (* if we are tracking components, find the new old ids, because these represent
    sites of potential merging, and merge together their vertex sets where necessary *)
    $trackDCsBlock := (
      {newIds, oldIds} = SelectDiscard[successorsIds, GreaterThan[lastCount]];
      Scan[oldId |-> mergeDcs[mergedDcId[[vertexIdToDcId @ oldId]], dcId], oldIds];
      currentDcVertexSet = Part[dcVertexSets, dcId];
      currentDcVertexSet["Union", newIds];
      If[currentDcVertexSet["Length"] >= maxVerticesPerComponent, dcActive[[dcId]] = False];
      AssociateTo[vertexIdToDcId, # -> dcId& /@ successorsIds];
    );
  ];

  If[!selfLoops,
    $removeSelfLoopsBlock := (
      successors //= DeleteCases[vertex | Labeled[vertex, _]];
    );
  ];

  If[!includeFrontier,
    $excludeFrontierBlock := (
      interiorMask = UnsameQ[#, $notFound]& /@ Lookup[vertexIndex, successors, $notFound];
      successors = Pick[successors, interiorMask, True];
      labeledSuccessors = Pick[labeledSuccessors, interiorMask, True];
    )
  ];

  $condition := And[Length[vertexIndex] <= maxVertices, edgeCount <= maxEdges, $timeCondition, evaluations < maxFunctionEvaluations];
  If[depthTermination === "Immediate",
    $loopCondition := $condition;
    $generationCondition = True;
  ,
    $loopCondition = True;
    $generationCondition := $condition;
  ];

  (* main loop *)
  edgeGenerations = {}; edgeCount = 0; lastCount = 0; generation = 1; evaluations = 0; maxDepthReached = False;
  While[$loopCondition,

    (* whenever we reach the end of this generation, move to the next generation *)
    If[thisGenVertices["EmptyQ"],
      If[generation >= maxDepth, maxDepthReached = True; Break[]];
      If[!$generationCondition, Break[]];
      If[nextGenVertices["EmptyQ"], Break[]];
      Swap[thisGenVertices, nextGenVertices];
      generation += 1;
      isLastGeneration = (generation == maxDepth);
    ];

    (* obtain a vertex to explore *)
    {vertexId, vertex, dcId} = thisGenVertices["Pop"];
    If[trackDCs, dcId = mergedDcId[[dcId]]];

    (* call the successor function to obtain successor vertices *)
    evaluations += 1;
    successors = f[vertex];
    progress[];
    If[MissingQ[successors], successors = {}];
    If[!ListQ[successors], Message[StateTransitionGraph::notlist, vertex, generation, Head[successors]]; Break[]];
    If[successors === {}, Continue[]];
    $removeSelfLoopsBlock;

    (* add the transitions lists to its bag *)
    Internal`StuffBag[transitionListsBag, {vertex, successors}];
    edgeCount += Length[successors];

    (* obtain pure labels if necessary *)
    $extractLabelBlock;

    (* strip labels *)
    labeledSuccessors = successors;
    successors = Replace[successors, Labeled[z_, _] :> z, {1}];

    (* detect new vertices, and obtain all the Ids of successor vertices *)
    lastCount = Length[vertexIndex];

    If[isLastGeneration, $excludeFrontierBlock];

    successorsIds = Map[
      succ |-> Lookup[vertexIndex, Key[succ],
        vertexArray["Append", succ];
        vertexIndex[succ] = Length[vertexIndex] + 1
      ], successors];

    (* update the per-label depth, if necessary *)
    $trackLabelDepthBlock;

    (* add the corresponding indexed transitions list to its bag *)
    relabeledSuccessorsIds = If[FreeQ[labeledSuccessors, _Labeled, {1}],
      successorsIds,
      MapThread[
        Replace[#2, {Labeled[_, l_] :> Labeled[#1, l], _ -> #1}]&,
        {successorsIds, labeledSuccessors}
      ]
    ];
    Internal`StuffBag[indexTransitionListsBag, {vertexId, relabeledSuccessorsIds}];

    (* update the connected component state, if necessary *)
    $trackDCsBlock;

    If[!trackDCs || dcActive[[dcId]],
      (* add all new vertices to the next generation *)
      stackPushList[nextGenVertices,
        If[!DuplicateFreeQ[successorsIds],
          KeyValueMap[removeStaleSuccessors, AssociationThread[successorsIds, successors]],
          MapThread[removeStaleSuccessors, {successorsIds, successors}]
        ]
      ]
    ];
  ];

  terminationReason = Which[
    Length[vertexIndex] > maxVertices, "MaxVertices",
    edgeCount > maxEdges, "MaxEdges",
    maxDepthReached, "MaxDepth",
    $timeCondition === False, "MaxTime",
    evaluations == maxFunctionEvaluations, "MaxFunctionEvaluations",
    !ListQ[successors], "SuccessorFunctionError",
    True, "Complete"
  ];

  (* trim edges if we collected too many *)
  If[edgeCount > maxEdges && !finishGeneration,
    extraCount = edgeCount - maxEdges;
    edgeTrimmer = MapAt[DropOperator[-extraCount], -1];
    labelTrimer = DropOperator[-extraCount];
  ,
    edgeTrimmer = labelTrimmer = Identity;
  ];

  (* calculate results *)
  transitionLists := transitionLists = edgeTrimmer @ Internal`BagPart[transitionListsBag, All];
  indexTransitionLists := indexTransitionLists = edgeTrimmer @ Internal`BagPart[indexTransitionListsBag, All];

  vertices := Normal[vertexArray];
  vertexCount := vertexArray["Length"];

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

  edgeLabels := edgeLabels = labelTrimer @ Internal`BagPart[edgeLabelsBag, All];

  graph := ExtendedGraph[vertices, edges, FilterOptions @ opts];
  indexGraph := Graph[Range @ vertexCount, indexEdges, FilterOptions @ opts];

  depths := Map[Total, Values @ depthVectorAssoc];
  tagDepths := AssociationThread[Append[depthLabels, None], Transpose @ Values @ depthVectorAssoc];

  resultsAssoc = <|
    "TransitionLists" :> transitionLists,
    "IndexTransitionLists" :> indexTransitionLists,
    "EdgeList" :> edges,
    "IndexEdgeList" :> indexEdges,
    "VertexList" :> vertices,
    "Graph" :> graph,
    "VertexDepthList" :> depths,
    "VertexDepthAssociation" :> AssociationThread[vertices, depths],
    "VertexTagDepthList" :> tagDepths,
    "VertexTagDepthAssociation" :> Map[AssociationThread[vertices, #]&, tagDepths],
    "LabeledGraph" :> Graph[graph, VertexLabels -> Automatic],
    "CayleyGraph" :> ExtendedGraph[CombineMultiedges @ graph, GraphLegend -> "Cardinals", GraphLayout -> "SpringElectricalEmbedding"],
    "IndexGraph" :> indexGraph,
    "EdgeLabels" :> edgeLabels,
    "TerminationReason" :> terminationReason,
    "Elements" :> $stgElements
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
