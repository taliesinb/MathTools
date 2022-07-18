(* PublicFunction[GraphGasRewritingSystem]

GraphGasRewritingSystem::arg1 = "First argument should be a graph with labeled edges."
GraphGasRewritingSystem::arg2 = "Second argument should be a list of rules of the form {word$1, $$} -> {word$1, $$}."

GraphGasRewritingSystem[graph_, rules_] := Scope[
  If[!EdgeTaggedGraphQ[graph], ReturnFailed["arg1"]];
  If[!RuleListQ[rules], ReturnFailed["arg2"]];
  data = makeGraphGasData[graph];
  props = <|"Graph" -> graph, "Data" -> data|>;
  constructRewritingSystem["BundleSection", rules, "CustomProperties" -> props]
]

_GraphGasRewritingSystem := (Message[GraphGasRewritingSystem::args, GraphGasRewritingSystem]; $Failed);

declareRewritingSystemDispatch["GraphGas", quiverGasRewritingSystemProperty]

quiverGasRewritingSystemProperty[data_, "CayleyFunction", opts___Rule] := Scope[
  UnpackStringOptions[{opts}, True, labeled];
  If[labeled,
    LabeledSectionRewritingCayleyFunction,
    SectionRewritingCayleyFunction
  ]
];

(**************************************************************************************************)

PublicHead[GraphGasState]

GraphGasState::badedge = "One or more edges were not present in the graph."

GraphGasState[edges_List, graph_Graph] := Scope[
  UnpackAssociation[makeGraphGasData[graph], edgeIndex, hash];
  edgeIds = Lookup[edgeIndex, VectorReplace[edges, r_Rule :> Apply[DirectedEdge, r]], ReturnFailed["badedge"]];
  GraphGasState[Sort @ edgeIds, hash]
];

(* plotting
GraphGasState[{___Integer}, hash_Integer] :=  *) *)

(**************************************************************************************************)

PublicFunction[QuiverGas]

PublicOption[RuleSymmetries, UnspecifiedIdentity]

Options[QuiverGas] = {
  Method -> 2,
  RuleSymmetries -> None,
  UnspecifiedIdentity -> True
};

QuiverGas::arg1 = "First argument should be an edge tagged graph.";
QuiverGas::arg2 = "Second argument should be a list of cardinal flow rules.";
QuiverGas::method = "Unknown method ``.";

QuiverGas[graph_, rules_, OptionsPattern[]] := Scope @ CatchMessage[

  If[!EdgeTaggedGraphQ[graph], ReturnFailed["arg1"]];

  If[!RuleListQ[rules], ReturnFailed["arg2"]];

  UnpackOptions[method, ruleSymmetries, unspecifiedIdentity];

  dataFn = Switch[method, 2, quiverGasDataNew, 1, quiverGasDataOld, _, ReturnFailed["method", method]];

  rules = processGasRules[CardinalList @ graph, rules, ruleSymmetries, unspecifiedIdentity];
  data = dataFn[graph, rules];
  If[!AssociationQ[data], ReturnFailed[]];

  {vertexCoordinates, edgeCoordinateLists} = ExtractGraphPrimitiveCoordinates @ graph;

  data = Join[data, <|
    "Graph" -> graph,
    "GraphHash" -> Hash[graph],
    "Rules" -> rules,
    "SymmetricEdgeCoordinates" -> Join[edgeCoordinateLists, Reverse[edgeCoordinateLists, 2]],
    "VertexCoordinates" -> vertexCoordinates
  |>];

  System`Private`ConstructNoEntry[QuiverGasObject, data]
];

(**************************************************************************************************)

QuiverGas::invsym = "Invalid symmetries ``.";

processGasRules[cardinals_, rules_, symmetries_, unspecifiedIdentity_] := Scope[
  $cardinals = Join[cardinals, Inverted /@ cardinals];
  rules = Map[processGasRule, rules];
  If[symmetries =!= None && rules =!= {},
    sub = SubstitutionRewritingSystem[symmetries];
    If[FailureQ[sub], ThrowMessage["invsym", symmetries]];
    rules = RewriteStates[sub, rules];
    rules = DeleteDuplicates @ Map[InvertAwareSort, rules, {2}];
    If[!RuleVectorQ[rules], ThrowMessage["invsym", symmetries]];
  ];
  If[unspecifiedIdentity,
    allLHS = InvertAwareSort /@ Rest[Subsets @ $cardinals];
    unspecifiedLHS = Complement[allLHS, Keys @ rules];
    rules = Join[rules, RuleThread[unspecifiedLHS, unspecifiedLHS]];
  ];
  DeleteDuplicates @ rules
];

processGasRule[lhs_ -> rhs_] := Rule[processLHS @ lhs, processRHS @ rhs];

QuiverGas::invspec = "Rule component `` is not valid."

processRHS = Case[
  Weighted[spec_, 0|0.]             := Nothing;
  Weighted[spec_, 1|1.]             := % spec;
  Weighted[spec_, w_ /; 0 < w < 1]  := Weighted[% @ spec, N @ w];
  Times[w_ /; 0 < w < 1, spec_]     := Weighted[% @ spec, N @ w];
  str_String                        := toCardinalList @ stringToCardList @ str;
  list_List                         := toCardinalList @ list;
  inv_                              := ThrowMessage["invspec", inv];
];

processLHS = Case[
  str_String                                := toCardinalList @ stringToCardList @ str;
  list_List                                 := toCardinalList @ list;
  inv_                                      := ThrowMessage["invspec", inv];
];

PrivateFunction[stringToCardList]

stringToCardList[str_String] :=
  Map[If[UpperCaseQ[#], Inverted @ ToLowerCase @ #, #]&, Characters @ str];

QuiverGas::invcards = "Spec `` contains invalid cardinals. Allowed cardinals are: ``."
toCardinalList[list_] := If[!SubsetQ[$cardinals, list],
  ThrowMessage["invcard", list, $cardinals],
  DeleteDuplicates @ list
];

(**************************************************************************************************)

PublicHead[QuiverGasObject]

MakeBoxes[object_QuiverGasObject ? System`Private`HoldNoEntryQ, form_] :=
  quiverGasObjectBoxes[object, form];

quiverGasObjectBoxes[object:QuiverGasObject[data_], form_] := Scope[
  UnpackAssociation[data, rules, graph];
  vcount = VertexCount @ graph;
  ecount = EdgeCount @ graph;
  rules = SortBy[rules, ruleOrderer];
  formattedRules = Grid[
    Map[fmtRule, SplitBy[rules, First /* Length], {2}],
    Alignment -> Left, BaselinePosition -> 1,
    ColumnSpacings -> 3
  ];
  BoxForm`ArrangeSummaryBox[
    QuiverGasObject, object, None,
    (* Always displayed *)
    {
     {padSummaryItem["Vertices", vcount]},
     {padSummaryItem["Edges", ecount]},
     {padSummaryItem["Rules", Length @ rules]}
    },
    (* Displayed on request *)
    {BoxForm`SummaryItem[{formattedRules}]},
    form,
    "Interpretable" -> Automatic
  ]
];

fmtRule[a_List -> b_List] := Pane[Row[a] -> Row[b], BaselinePosition -> Baseline];

ruleOrderer[r:Rule[a_, b_]] :=
  {Length[a], Length[b], ReplaceAll[r, Inverted[f_] :> f], Position[r, _Inverted]};

(**************************************************************************************************)

PublicFunction[QuiverGasEvolve]

Options[QuiverGasEvolve] = {
  RandomSeeding -> Automatic
};

QuiverGasEvolve::badinit = "Could not resolve initial condition.";
QuiverGasEvolve[qg:QuiverGasObject[assoc_] ? System`Private`NoEntryQ, init_, steps_Integer, OptionsPattern[]] := Scope[

  UnpackAssociation[assoc, flowVectorSize, updateFunction, signedEdgeToIndex];

  UnpackOptions[randomSeeding];

  RandomSeeded[
    init = Switch[init,
      _List,                                 Normal @ ExtendedSparseArray[Lookup[signedEdgeToIndex, init, ReturnFailed["badinit"]], flowVectorSize],
      i_Integer /; 1 <= i <= flowVectorSize, RandomSample @ Join[ConstantArray[1, init], ConstantArray[0, flowVectorSize - init]],
      _Real | _Rational,                     RandomChoice[{init, 1 - init} -> {1, 0}, flowVectorSize],
      _,                                     ReturnFailed[]
    ],
    randomSeeding
  ];

  init //= ToPackedReal;
  If[!VectorQ[init, RealQ], ReturnFailed[]];

  If[!VectorQ[updateFunction[init], RealQ], ReturnFailed[]];

  evolution = NestList[updateFunction, init, steps];
  evolution = Part[evolution, All, 1;;-2];

  QuiverGasEvolutionData[qg, evolution]
];

(**************************************************************************************************)

pureFunctionToStringRules[fn:HoldPattern[Function[args:{__Symbol}, _]]] := Scope[
  names = MapUnevaluated[HoldSymbolName, args];
  names = If[UpperCaseQ[#], Inverted[ToLowerCase[#]]]& /@ names;
  nameP = Alternatives @@ names;
  res = Apply[fn, names];
  If[!RuleListQ[res], ReturnFailed[]];
  res = Replace[res, {p:nameP :> {p}, HoldPattern[Times][n:nameP..] :> {n}}, {2}];
  If[!MatchQ[res, {Rule[{nameP..}, {nameP..}]..}], ReturnFailed[]];
  res
];

toSignedChars[Times[r_Rational, s_String]] := Weighted[toSignedChars @ s, N @ r];
toSignedChars[str_String] := Map[If[UpperCaseQ[#], Inverted @ ToLowerCase @ #, #]&, Characters @ str];

stringRulesToCompiledFunction[names_List, rules:{(_String -> (_String | Times[_Rational | _Real, _String]))..}] :=
  stringRulesToCompiledFunction[names, Map[toSignedChars, rules, {2}]];

toWeightedRule[a_ -> Weighted[b_, w_]] := {a -> b, w};
toWeightedRule[a_ -> b_] := {a -> b, 1};

(* optimizations:
1. for binary (determinstic) rulesets, we can use BitAnd, BitOr, and BitNot, and we can pack the flow into
a list of integers (the number being flow size / 63), applying it bitwise. the only step that cannot then
be done bitwise is the gather and scatter steps
*)

stringRulesToCompiledFunction[names_List, rules:{({__} -> ({__} | Weighted[{__}, _]))...}] := Scope[
  (* all RHS that share a LHS are weighted equally *)
  rules = Flatten @ KeyValueMap[{k, vs} |-> Map[k -> #&, vs], GroupBy[rules, First -> Last, averageGroup]];
  {rules, ruleWeights} = Transpose @ Map[toWeightedRule, rules];
  ruleIndex = PositionIndex[Values @ rules, 2];
  outList = Map[out |-> (
    pos = Lookup[ruleIndex, out, {}];
    inTrue = Part[rules, pos, 1]; (* list of lists *)
    weights = Part[ruleWeights, pos]; (* list of numbers *)
    sum = MapThread[
      {weight, true} |-> Apply[Times, Join[{weight}, true, 1 - Complement[names, true]]],
      {weights, inTrue}
    ];
    Apply[Plus, sum]),
    names
  ];
  outList = simplifySum[names, outList];
  nameToSlot = Slot /@ AssociationRange[names];
  fn = Construct[Function, outList] /. nameToSlot /. 0 :> 0 * Slot[1];
  compilePureFunction[fn, Length @ names, Real]
];

$formals = {\[FormalA], \[FormalB], \[FormalC], \[FormalD], \[FormalE], \[FormalF], \[FormalG], \[FormalH], \[FormalI], \[FormalJ], \[FormalK]};

averageGroup = Case[
  {a_}    := {a};
  l_List  := With[{n = 1 / Length[l]}, Weighted[#, n]& /@ l]
];

compilePureFunction[fn_, nargs_, type_] := With[
  {syms = Take[$formals, nargs]},
  {argSpecs = {#, Blank[type], 1}& /@ syms},
  cfn = Construct[Compile, argSpecs, Unevaluated @ Apply[fn, syms]];
  CompiledFunctionTools`CompilePrint @ cfn;
  cfn
];

simplifySum[names_, sum_] := Scope[
  formals = Take[$formals, Length @ names];
  Simplify[sum /. RuleThread[names, formals]] /. RuleThread[formals, names]
];

(**************************************************************************************************)

PublicFunction[quiverGasDataOld]

quiverGasDataOld[g_, rules_] := Scope[
  
  numEdges = EdgeCount @ g;
  edgeRange = Range @ numEdges;
  noneEdgeIndex = 2 * numEdges + 1;
  
  signedEdgeToIndex = AssociationRange[edgeRange];
  signedEdgeToIndex = Association[signedEdgeToIndex, KeyMap[Inverted, signedEdgeToIndex + numEdges], None -> noneEdgeIndex];
  
  tagAdj = KeySort @ TagVertexAdjacentEdgeTable[g, Signed -> True];
  tags = Keys @ tagAdj;
  inAdj = MatrixMap[Inverted, Lookup[tagAdj, Inverted /@ tags]];
  outAdj = Lookup[tagAdj, tags];
  
  gatherMatrix = inAdj /. signedEdgeToIndex;

  invertedGather = outAdj /. signedEdgeToIndex;
  scatterVector = ConstantArray[Null, noneEdgeIndex];
  ScanIndex1[Set[Part[scatterVector, #1], #2]&, Flatten @ invertedGather];
  scatterVector = ToPacked @ Most @ scatterVector;
  
  fn = stringRulesToCompiledFunction[tags, rules];

  With[{gatherMatrix = gatherMatrix},
  update = (ExtractIndices[#, gatherMatrix]&) /* Apply[fn] /* Flatten /* PartOperator[scatterVector] /* Append[0];
  ];

  <|"Neighborhood" -> tags, "UpdateFunction" -> update, "FlowVectorSize" -> noneEdgeIndex, "SignedEdgeToIndex" -> signedEdgeToIndex|>
]

(**************************************************************************************************)

PublicFunction[quiverGasDataNew]

quiverGasDataNew[g_, rules_] := Scope[
  
  numEdges = EdgeCount @ g;
  edgeRange = Range @ numEdges;
  noneEdgeIndex = 2 * numEdges + 1;
  
  signedEdgeToIndex = AssociationRange[edgeRange];
  signedEdgeToIndex = Association[signedEdgeToIndex, KeyMap[Inverted, signedEdgeToIndex + numEdges], None -> noneEdgeIndex];
  
  tagAdj = KeySort @ TagVertexAdjacentEdgeTable[g, Signed -> True];
  tags = Keys @ tagAdj;
  inAdj = MatrixMap[Inverted, Lookup[tagAdj, Inverted /@ tags]];
  outAdj = Lookup[tagAdj, tags];
  
  gatherMatrix = inAdj /. signedEdgeToIndex;

  invertedGather = outAdj /. signedEdgeToIndex;

  (* todo: compile this *)
  scatterVector = ConstantArray[Null, noneEdgeIndex];
  ScanIndex1[Set[Part[scatterVector, #1], #2]&, Flatten @ invertedGather];
  scatterVector = Most @ scatterVector;
  
  fn = stringRulesToPureFunction[tags, rules];
  gatherMatrix //= ToPacked;
  scatterVector //= ToPacked;

  cfn = compileScatterGather[fn, Real, gatherMatrix, scatterVector];

  <|"Neighborhood" -> tags, "UpdateFunction" -> cfn, "FlowVectorSize" -> noneEdgeIndex, "SignedEdgeToIndex" -> signedEdgeToIndex|>
]

(* this is the optimized version that does the gather internally *)

stringRulesToPureFunction[names_List, rules:{(_String -> (_String | Times[_Rational | _Real, _String]))..}] :=
  stringRulesToPureFunction[names, Map[toSignedChars, rules, {2}]];

stringRulesToPureFunction[names_List, rules:{({__} -> ({__} | Weighted[{__}, _]))...}] := Scope[
  (* all RHS that share a LHS are weighted equally *)
  rules = Flatten @ KeyValueMap[{k, vs} |-> Map[k -> #&, vs], GroupBy[rules, First -> Last, averageGroup]];
  {rules, ruleWeights} = Transpose @ Map[toWeightedRule, rules];
  ruleIndex = PositionIndex[Values @ rules, 2];
  outList = Map[out |-> (
    pos = Lookup[ruleIndex, out, {}];
    inTrue = Part[rules, pos, 1]; (* list of lists *)
    weights = Part[ruleWeights, pos]; (* list of numbers *)
    sum = MapThread[
      {weight, true} |-> Apply[Times, Join[{weight}, true, 1 - Complement[names, true]]],
      {weights, inTrue}
    ];
    Apply[Plus, sum]),
    names
  ];
  outList = simplifySum[names, outList];
  nameToSlot = Slot /@ AssociationRange[names];
  Construct[Function, outList] /. nameToSlot /. 0 :> 0 * Slot[1]
];

compileScatterGather[fn_, type_, gatherMatrix_ ? PackedArrayQ, scatterVector_ ? PackedArrayQ] := With[
  {flow = \[FormalCapitalF], gatheredFlow = \[FormalCapitalG], newFlow = \[FormalCapitalH],
   scatterVectorSymbol = \[FormalCapitalV]},
  {gatheredSymbols = Take[$formals, Length @ gatherMatrix]},
  {gatherSet = MapThread[{var, gather} |-> Hold[Set[var, Part[flow, gather]]], {gatheredSymbols, Developer`FromPackedArray[gatherMatrix, 1]}]},
  {body = HoldComplete[Compile[
    {{flow, _type, 1}}, HModule[
      Eval @ Join[{gatheredFlow, newFlow}, gatheredSymbols],
      gatherSet;
      newFlow = Flatten @ Apply[fn, gatheredSymbols];
      Append[Part[newFlow, scatterVector], 0]
    ]
  ]] /. {Eval[e_] :> RuleCondition[e], HModule -> Module, Hold[e_] :> e}},
  First @ body
];

(**************************************************************************************************)

PublicHead[QuiverGasEvolutionData]

MakeBoxes[QuiverGasEvolutionData[_QuiverGasObject, flow_List], form_] :=
  RowBox @ Flatten @ {"QuiverGasEvolutionData", "[",
    StyleBox[skeletonString @ Row[Dimensions @ flow, ", "], Background -> Lighter[$LightPink, .8]],
  "]"};

QuiverGasEvolutionData /: Normal[QuiverGasEvolutionData[_, flow_]] := flow;

getObjAndFlow[QuiverGasEvolutionData[QuiverGasObject[assoc_], flow_List]] := {assoc, flow};


QuiverGasEvolutionData::badframe = "Only `` frames available.";

getFlow[flow_, Into[n_]] :=
  getFlow[flow, Span[1, -1, Ceiling[Length[flow] / n]]];

getFlow[flow_, spec_] := Scope[
  res = Quiet @ Check[Part[flow, spec], $Failed];
  If[FailureQ[res],
    Message[QuiverGasEvolutionData::badframe, Length @ flow];
    Part[flow, 1]
  ,
    res
  ]
];


(**************************************************************************************************)

$plotDataCache = UAssociation[];

qg_QuiverGasEvolutionData["ParticlePlotImage", part_, userOpts___Rule] := Scope[
  res = qg["ParticlePlot", part, userOpts];
  If[VectorQ[res], Rasterize /@ res, Rasterize @ res]
];

qg_QuiverGasEvolutionData["ParticlePlot", part_, userOpts___Rule] := Scope[

  {obj, flow} = getObjAndFlow @ qg;
  flow = getFlow[flow, part];

  UnpackAssociation[obj, graph, graphHash, symmetricEdgeCoordinates, vertexCoordinates, flowVectorSize];

  mesh = TrueQ @ Lookup[{userOpts}, Mesh, True];
  plotGraphics = If[mesh,
    Part[CacheTo[$plotDataCache, graphHash, DeleteOptions[ImageSizeRaw] @ ExtendedGraphPlot[
      graph, ImagePadding -> 5, EdgeThickness -> 1, VertexSize -> 5,
      ArrowheadShape -> None, EdgeStyle -> $LightGray, VertexStyle -> Black,
      EdgeLabelStyle -> None
    ]], 1, 1],
    {}
  ];

  flowTable @ Graphics[
    {plotGraphics,
    {Thickness[Large], MapThread[
        {segment, weight} |-> If[weight > 0, Style[drawArrowAlong[segment], GrayLevel[1 - weight]]],
        {symmetricEdgeCoordinates, flow}
      ]
    }},
    userOpts,
    Sequence @@ Options[plotGraphics]
  ]
];

$gasArrow := $gasArrow = Part[ArrowheadData["EqualLine"], 1, 4];

drawArrowAlong[list_List] :=
  drawArrowAlong @ Part[list, {1, -1}];

drawArrowAlong[{a_, b_}] /; EuclideanDistance[a, b] >= 2 :=
  drawArrowAlong[{a, a - Normalize[b - a]}];

drawArrowAlong[{a_, b_}] := Scope[
  y = 0.5 * Normalize[b - a];
  m = Mean[{b, a}];
  x = VectorRotate90[y];
  GeometricTransformation[$gasArrow, {Trans[y, x], m}]
];

(**************************************************************************************************)

$densityAveragingCache = UAssociation[];

makeDensityAveragingMatrix[vertexCoordinates_, symmetricEdgeCoordinates_, w_, h_] := Scope[
  {{xmin, ymin}, {xmax, ymax}} = CoordinateBoundingBox[vertexCoordinates];
  xrange = coordRange[xmin, xmax, w];
  yrange = coordRange[ymin, ymax, h];
  points = Tuples[{xrange, yrange}];
  indices = Tuples[Range /@ {w, h}];
  (* symEdgeInd -> {xInd, yInd} *)
  indices = FirstColumn @ Nearest[points -> indices, Mean /@ symmetricEdgeCoordinates];
  (* {xInd, yInd} -> {symEdgeInd...} *)
  index = PositionIndex[indices];
  avg3 = KeyValueMap[{xy, inds} |-> Map[z |-> (Append[xy, z] -> 1.0 / Length[inds]), inds], index];
  SparseArray[Flatten @ avg3, {w, h, Length @ symmetricEdgeCoordinates}]
];

coordRange[min_, max_, n_] := Scope[
  dx = (max - min) / (n+2.);
  ToPacked[min + dx/2 + dx * Range[n]]
];

(**************************************************************************************************)

qg_QuiverGasEvolutionData["DensityPlot", part_, {w_Integer, h_Integer}, userOpts___Rule] := Scope[

  {obj, flow} = getObjAndFlow @ qg;
  flow = getFlow[flow, part];

  UnpackAssociation[obj, graphHash, graph, symmetricEdgeCoordinates, vertexCoordinates, flowVectorSize];

  densityAvg = CacheTo[$densityAveragingCache, {graphHash, w, h},
    makeDensityAveragingMatrix[vertexCoordinates, symmetricEdgeCoordinates, w, h]
  ];

  If[MatrixQ[flow],
    flows = Map[Dot[densityAvg, #]&, flow];
    flows /= Max[flows];
    FadedMeshImage[1 - #, {10, 10}, FilterOptions @ userOpts]& /@ flows
  ,
    flow = Dot[densityAvg, flow];
    flow /= Max[flow];
    FadedMeshImage[1 - flow, {10, 10}, FilterOptions @ userOpts]
  ]
];

SetHoldAll[flowTable];

flowTable[body_] := If[MatrixQ[flow],
  Map[Construct[Function, Unevaluated @ flow, Unevaluated @ body], flow],
  body
];