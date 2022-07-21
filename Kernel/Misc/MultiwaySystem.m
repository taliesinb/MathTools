PublicOption[MaxVertices, MaxEdges, MaxDepth, MaxTime, MaxFunctionEvaluations, MaxNorm]
PublicOption[ProgressFunction, IncludeFrontier, DepthTermination, PrologVertices, CanonicalizationFunction]

MaxVertices::usage = "MaxVertices is an option to MultiwaySystem."
MaxEdges::usage = "MaxEdges is an option to MultiwaySystem."
MaxDepth::usage = "MaxDepth is an option to MultiwaySystem."
MaxTime::usage = "MaxTime is an option to MultiwaySystem.";
ProgressFunction::usage = "ProgressFunction is an option to MultiwaySystem.";
MaxNorm::usage = "MaxNorm is an option to MultiwaySystem.";
IncludeFrontier::usage = "IncludeFrontier is an option to MultiwaySystem.";
DepthTermination::usage = "DepthTermination is an option to MultiwaySystem."
CanonicalizationFunction::usage = "CanonicalizationFunction is an option to MultiwaySystem."

(**************************************************************************************************)

PublicFunction[MultiwaySystem]

DeclareArgumentCount[MultiwaySystem, 2];

declareSyntaxInfo[MultiwaySystem, {_, _, OptionsPattern[]}];

Options[MultiwaySystem] = JoinOptions[
  MaxVertices -> Infinity,
  MaxEdges -> Infinity,
  MaxDepth -> Infinity,
  MaxFunctionEvaluations -> Infinity,
  MaxTime -> Infinity,
  DirectedEdges -> True,
  ProgressFunction -> None,
  NormFunction -> Automatic,
  MaxNorm -> None,
  IncludeFrontier -> True,
  DepthTermination -> "Immediate",
  CanonicalizationFunction -> None,
  SelfLoops -> True,
  PrologVertices -> {}
];

stackPushList[stack_, list_] := Scan[item |-> stack["Push", item], list];
arrayAppendList[array_, list_] := Scan[item |-> array["Append", item], list];

SetUsage @ "
MultiwaySystem[transition$, {state$1, state$2, $$}] starts at the initial states state$i, \
and recursively evaluates transition$[state$] to obtain a list of sucessor states, returning \
the directed graph whose vertices are the states and whose edges are the transitions between states.
MultiwaySystem[transition$, states$, 'element$'] returns a named element.
* The possible values for element$ are one or a list of the following:
| 'Graph' | the graph of states and their transitions (default) |
| 'VertexList' | a list of vertices in the same order as the vertices of 'IndexGraph' |
| 'EdgeList' | a list of edges |
| 'TransitionLists' | a list of transitions, each of the form {istate$, {ostate$1, ostate$2, $$}}. |
| 'TerminationReason' | one of 'MaxVertices', 'MaxEdges', 'MaxDepth', 'MaxTime', 'MaxFunctionEvaluations', or 'Complete' |
| 'EdgeLabels' | a list of edge labels in the same order as 'EdgeList' |
| 'Elements' | a list of valid element names |
* Most of the above elements, e.g. 'Graph', 'EdgeList', etc. have a corresponding element \
'IndexGraph', 'IndexEdgeList', etc. that label vertices with consecutive integers in the same \
order as 'VertexList'.
* transition$ should be a function that takes a single state and should return a list of \
successor states.
* transition$ may return states in the form %Labeled[ostate$, label$]; these labels will \
produces tagged edges of the form %DirectedEdge[istate$, state$, label$].
* If transition$ is of the form {f$1, f$2, $$}, the individual functions f$i will be called \
and their successor states labeled as i$. Any of the f$i can also be of the form \
%Labeled[f$, label$] or f$ -> label$ to attach a custom label label$ instead.
* States will be explored in breadth-first order.
* MultiwaySystem accepts all the options accepted by %Graph, in addition to the following:
| %MaxVertices | Infinity | how many states to obtain before terminating the exploration |
| %MaxEdges | Infinity | how many transitions to obtain |
| %MaxDepth | Infinity | how many transitions from the initial set to explore |
| %MaxTime | Infinity | maximum number of seconds to run for |
| %MaxFunctionEvaluations | Infinity | maximum number of times to evaluate transition$ |
| %MaxNorm | Infinity | ignore states with norm greater than this |
| %NormFunction | Automatic | function that evaluates the norm of a given state |
| %DirectedEdges | True | whether to return a directed or undirected graph |
| %SelfLoops | True | whether to include transitions from a state to itself |
| %IncludeFrontier | True | whether to include edges to vertices beyond maximum depth |
| %DepthTermination | 'Immediate' | specifies when termination take effect |
| %ProgressFunction | None | a function to call with an association with information about the exploration |
| %CanonicalizationFunction | None | function to apply to states returned by transition$ |
* For %ProgressFunction -> p$, p$ will be called with an association containing the following keys:
| 'State' | the current state being explored |
| 'Successors' | the list of successors that was returned by transition$ |
| 'Depth' | the number of transitions since the initial set |
| 'Time' | the number of seconds since exploration began |
| 'EdgeCount' | the number of edges seen so far |
| 'VertexCount' | the number of vertices seen so far |
* For %DepthTermination -> 'Delayed', the depth currently being explored will be finished before termination occurs.
"

$backEdgeElements = {
  "SpanningTree", "IndexSpanningTree"
};

$stgElements = {
  "TransitionLists", "IndexTransitionLists",
  "EdgeList", "IndexEdgeList",
  "VertexList",
  "Graph", "IndexGraph",
  Splice[$backEdgeElements],
  "TerminationReason", "EdgeLabels",
  "Elements"
};

$stgElementsPattern = Alternatives @@ $stgElements;

MultiwaySystem::badlimit = "The setting for `` should be a positive integer or infinity.";
MultiwaySystem::badinitstates = "The initial states spec should be a non-empty list of states."
MultiwaySystem::badelement = "Requested element `` is not one of `` or a list of these."
MultiwaySystem::notlist = "The successor function did not return a list for state `` (at depth ``), which had head `` instead; halting early."
MultiwaySystem::badnorm = "The setting for NormFunction should be function that takes a state and returns a positive number."
MultiwaySystem::badmaxdepth = "The setting for MaxDepth should be an integer or Infinity."
MultiwaySystem::undedgelabels = "Cannot obtain element \"EdgeLabels\" when DirectedEdges -> False.";

MultiwaySystem[f_, initialVertices_, opts:OptionsPattern[]] :=
  MultiwaySystem[f, initialVertices, "Graph", opts];

declareFunctionAutocomplete[MultiwaySystem, {0, 0, $stgElements}];

checkNorm[r_ ? NonNegative] := r;
checkNorm[_] := (Message[MultiwaySystem::badnorm]; Return[$Failed, Module]);

MultiwaySystem[f_, initialVertices_, result:Except[_Rule], opts:OptionsPattern[]] :=
  iMultiwaySystem[f, initialVertices, result, opts];

iMultiwaySystem[f_, initialVertices_, result:Except[_Rule], opts:OptionsPattern[MultiwaySystem]] :=
  Scope[

  UnpackOptions[
    directedEdges, progressFunction, normFunction,
    maxVertices, maxEdges, maxDepth,
    maxTime, maxFunctionEvaluations, maxNorm,
    includeFrontier, depthTermination, selfLoops,
    prologVertices, canonicalizationFunction
  ];

  If[!ListQ[initialVertices] || Length[initialVertices] == 0,
    ReturnFailed[MultiwaySystem::badinitstates];
  ];

  initialVertices = DeleteDuplicates[initialVertices];

  If[!MatchQ[result, $stgElementsPattern | {Repeated[$stgElementsPattern]}],
    ReturnFailed[MultiwaySystem::badelement, result, TextString[Row[$stgElements, ", "]]];
  ];

  Scan[
    symbol |-> If[!MatchQ[OptionValue[symbol], $PosIntOrInfinityP],
      ReturnFailed[MultiwaySystem::badlimit, symbol];
    ],
    {MaxVertices, MaxTime, MaxFunctionEvaluations}
  ];

  If[!MatchQ[maxDepth, $PosIntOrInfinityP], ReturnFailed[MultiwaySystem::badmaxdepth]];

  If[NumberQ[maxNorm],
    If[normFunction === None, checkNorm[Null]]; (* trigger message *)
    normTest = normFunction /* LessEqual[maxNorm];
  ,
    normTest = None;
  ];

  If[ListQ[f], f = makeSuperTransitionFunc[f]];

  If[!MatchQ[canonicalizationFunction, None | Identity | (#&)],
    canonFn = VectorReplace[{Labeled[e_, l_] :> Labeled[canonicalizationFunction[e], l], e_ :> canonicalizationFunction[e]}] /* DeleteDuplicates;
    initialVertices //= canonFn;
    If[ListQ[prologVertices], prologVertices = canonFn @ prologVertices];
    $canonicalizationBlock := (
      successors //= canonFn;
    );
  ];

  numPrologVertices = Length @ prologVertices;
  If[numPrologVertices > 0,
    initialVertices = Join[prologVertices, initialVertices]];

  thisGenVertices = CreateDataStructure["Stack"];
  nextGenVertices = CreateDataStructure["Stack"];
  vertexArray = CreateDataStructure["DynamicArray"];
  vertexIndex = UAssociation[];
  initialIds = Range @ Length @ initialVertices;

  arrayAppendList[vertexArray, initialVertices];

  ScanThread[Set[vertexIndex[#1], #2]&, {initialVertices, initialIds}];

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

  stackPushList[thisGenVertices, DropOperator[numPrologVertices] @ Trans[initialIds, initialVertices]];

  If[NumberQ[maxTime],
    stopTime = SessionTime[] + maxTime;
    $timeCondition := SessionTime[] < stopTime
  ,
    $timeCondition = True
  ];

  (* build the function that decides which vertices to visit next *)
  removeStaleSuccessors = {succId, succ} |-> If[$staleTest, {succId, succ}, Nothing];
  If[normTest =!= None,
    removeStaleSuccessors //= ReplaceAll[$staleTest :> And[$staleTest, normTest[succ]]]
  ];
  removeStaleSuccessors //= ReplaceAll[$staleTest -> succId > lastCount];
  If[trackBackEdges = ContainsQ[result, Alternatives @ $backEdgeElements],
    backEdgesAssociation = Association[];
    removeStaleSuccessors //= Insert[removeStaleSuccessors, $Unreachable]; (* TODO: implement me *)
  ];

  If[ContainsQ[result, "EdgeLabels"],
    If[directedEdges === False, ReturnFailed[MultiwaySystem::undedgelabels]];
    edgeLabelsBag = Internal`Bag[];
    $extractLabelBlock := (
      labels = Replace[successors, {Labeled[_, l_] :> l, _ -> None}, {1}];
      Internal`StuffBag[edgeLabelsBag, labels, 1];
      labels = Replace[labels, Inverted[c_] :> c, {1}];
    );
  ];

  If[!selfLoops,
    $removeSelfLoopsBlock := (
      successors //= DeleteCases[vertex | Labeled[vertex, _]];
    );
  ];

  If[!includeFrontier,
    $excludeFrontierBlock := (
      interiorMask = SameAs[$notFound] /@ Lookup[vertexIndex, successors, $notFound];
      successors = Pick[successors, interiorMask, False];
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
    {vertexId, vertex} = thisGenVertices["Pop"];

    (* call the successor function to obtain successor vertices *)
    evaluations += 1;
    successors = f[vertex];
    progress[];
    If[MissingQ[successors], successors = {}];
    If[!ListQ[successors], Message[MultiwaySystem::notlist, vertex, generation, Head[successors]]; Break[]];
    $canonicalizationBlock;
    If[successors === {}, Continue[]];
    $removeSelfLoopsBlock;

    (* avoid including edges to unvisited vertices when on final depth *)
    If[isLastGeneration, $excludeFrontierBlock];

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
    successorsIds = Map[
      succ |-> Lookup[vertexIndex, Key[succ],
        vertexArray["Append", succ];
        vertexIndex[succ] = Length[vertexIndex] + 1
      ], successors];

    (* add the corresponding indexed transitions list to its bag *)
    relabeledSuccessorsIds = If[FreeQ[labeledSuccessors, _Labeled, {1}],
      successorsIds,
      MapThread[
        Replace[#2, {Labeled[_, l_] :> Labeled[#1, l], _ -> #1}]&,
        {successorsIds, labeledSuccessors}
      ]
    ];
    Internal`StuffBag[indexTransitionListsBag, {vertexId, relabeledSuccessorsIds}];

    (* add all new vertices to the next generation *)
    stackPushList[nextGenVertices,
      If[!DuplicateFreeQ[successorsIds],
        KeyValueMap[removeStaleSuccessors, AssociationThread[successorsIds, successors]],
        MapThread[removeStaleSuccessors, {successorsIds, successors}]
      ]
    ]
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

  (* calculate results *)
  transitionLists := transitionLists = Internal`BagPart[transitionListsBag, All];
  indexTransitionLists := indexTransitionLists = Internal`BagPart[indexTransitionListsBag, All];

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

  edgeLabels := edgeLabels = Internal`BagPart[edgeLabelsBag, All];

  graph := ExtendedGraph[vertices, edges, FilterOptions @ opts];
  indexGraph := Graph[Range @ vertexCount, indexEdges, FilterOptions @ opts];

  resultsAssoc = <|
    "TransitionLists" :> transitionLists,
    "IndexTransitionLists" :> indexTransitionLists,
    "EdgeList" :> edges,
    "IndexEdgeList" :> indexEdges,
    "VertexList" :> vertices,
    "Graph" :> graph,
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

(**************************************************************************************************)

PrivateFunction[CachedMultiwaySystem]
PublicFunction[ClearMultiwayCache]

ClearMultiwayCache[] := (QuiverGeometryLoader`$MultiwaySystemCache = CreateDataStructure["LeastRecentlyUsedCache", 8]);

SetInitialValue[QuiverGeometryLoader`$MultiwaySystemCache, None];

CachedMultiwaySystem[args___] := Scope[
  hash = Hash @ {args};
  If[QuiverGeometryLoader`$MultiwaySystemCache === None, ClearMultiwayCache[]];
  cachedValue = QuiverGeometryLoader`$MultiwaySystemCache["Lookup", hash, Null&];
  If[cachedValue =!= Null, Return @ cachedValue];
  result = MultiwaySystem[args];
  If[!FailureQ[result],
    QuiverGeometryLoader`$MultiwaySystemCache["Insert", hash -> result]];
  result
];

