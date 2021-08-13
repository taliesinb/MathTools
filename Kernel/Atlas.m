PackageExport["ChartSymbol"]

SetUsage @ "
ChartSymbol[sub$] represents a chart and formats as C$sub.
"

declareFormatting[
  ChartSymbol[sym_String] :> formatChartSymbol[sym, Automatic],
  ChartSymbol[other__] :> ChartSymbolForm[Row[{other}]]
];

(**************************************************************************************************)

PackageScope["formatChartSymbol"]

formatChartSymbol[sym_String, colors_] := Scope[
  Style[
    ChartSymbolForm[sym]
  ,
    cards = ToPathWord @ StringTrim[sym, {"+" | "-"}];
    c = Lookup[
      If[colors === Automatic, ChooseCardinalColors @ cards, colors],
      cards,
      $Failed
    ];
    If[ContainsQ[c, $Failed], Sequence @@ {},
      FontColor -> OklabDarker[HumanBlend @ Discard[c, GrayColorQ], .2]
    ]
  ]
];

(**************************************************************************************************)

PackageExport["ChartSymbolCardinals"]

SetUsage @ "
ChartSymbolCardinals[chart$] returns the list of cardinals in a ChartSymbol[$$].
"

ChartSymbolCardinals = Case[
  ChartSymbol[s_, ___] := ToPathWord[s];
  (GraphRegionIntersection|GraphRegionUnion)[s__] := Union @@ Map[%, {s}];
];

(**************************************************************************************************)

PackageExport["CardinalTransitionMatrices"]

CardinalTransitionMatrices[atlas_Graph] := Scope[
  edgeAnnos = LookupExtendedOption[atlas, EdgeAnnotations];
  cardinals = CardinalList @ atlas;
  transitions = Lookup[Replace[edgeAnnos, None -> <||>], "CardinalTransitions", None];
  If[!AssociationQ[transitions], ReturnFailed[]];
  tagIndices = TagIndices @ atlas;
  numCardinals = Length @ cardinals;
  cardinalIndex = AssociationRange @ cardinals;
  matrixShape = {numCardinals, numCardinals};
  KeySortBy[IndexOf[cardinals, #]&] @ Association @ KeyValueMap[buildTagMatrices, tagIndices]
];

(* TODO: check for compatibility *)
buildTagMatrices[tag_, edgeIndices_] := Scope[
  trans = Flatten @ Lookup[transitions, edgeIndices, {}];
  matrix = Normal @ SparseArray[procTransRule /@ trans, matrixShape];
  tag -> matrix
];

procTransRule = Case[
  a_ -> b_          := Lookup[cardinalIndex, {b, a}] -> 1;
  a_ -> Negated[b_] := Lookup[cardinalIndex, {b, a}] -> -1;
];

(**************************************************************************************************)

PackageExport["CardinalTransitionRepresentation"]

CardinalTransitionRepresentation[atlas_Graph] := Scope[
  matrices = CardinalTransitionMatrices[atlas];
  If[FailureQ @ matrices, ReturnFailed[]];
  QuiverRepresentation[
    atlas,
    matrices
  ]
];

(**************************************************************************************************)

PackageExport["CardinalAtlas"]

CardinalAtlas[quiver_, charts_] := Scope @ Catch[
  If[!EdgeTaggedGraphQ[quiver], ReturnFailed[]];
  charts = toChart /@ charts;
  If[!MatchQ[charts, {__List}], ReturnFailed[]];
  $quiver = ToIndexGraph @ quiver;
  $charts = charts;
  {chartVertices, chartEdges} = Transpose @ Map[getChartRegion, charts];
  vertexToChart = PositionIndex[chartVertices, 2];
  Print[VertexList @ $quiver];
  edgeList = EdgeList @ $quiver;
  sharedEdges = Select[PositionIndex[chartEdges, 2], Length[#] > 1&];
  groupedSharedEges = GroupBy[sharedEdges, Identity, Keys];
  transitionEdges' = Merge[MapIndexed[toTransitionEdge, edgeList], Identity];
  zOuter[
    findCardinale,
    z]
,
  CardinalAtlas
];

CardinalAtlas::notchart = "Chart specification `` is not a list of cardinals or a ChartSymbol.";

toChart = Case[
  list_List       := list;
  cs_ChartSymbol  := ChartSymbolCardinals[cs];
  other_          := failCA["notchart", other];
];

toTransitionEdge[DirectedEdge[a_, b_, c_], {p_}] := Scope[
  aChart = vertexToChart @ a;
  bChart = vertexToChart @ b;
  comp1 = Complement[bChart, aChart];
  comp2 = Complement[aChart, bChart];
  List[
    Outer[DirectedEdge[#1, #2, sharedCardinal[#1, #2, c]] -> p&, Complement[aChart, comp1], comp1, 1],
    Outer[DirectedEdge[#1, #2, sharedCardinal[#1, #2, Negated @ c]] -> p&, Complement[bChart, comp2], comp2, 1]
  ]
];

sharedCardinal[c1_, c2_, cs:CardinalSet[cards_]] := Scope[
  int = Intersection[Part[$charts, c1], Part[$charts, c2]];
  SelectFirst[cards, MemberQ[int, # | Negated[#]]&, cs]
];

sharedCardinal[_, _, c_] := c;

CardinalAtlas::notatlas = "Cardinals `` do not form a chart.";

getChartRegion[cardinals_] := Match[
  GraphRegion[$quiver, ChartSymbol[cardinals]],
  {GraphRegionData[vertices_, edges_]} :> {vertices, edges},
  _ :> failCA["notatlas", cardinals];
];

failCA[msg_String, args___] := (
  Message[MessageName[CardinalAtlas, msg], args];
  Throw[$Failed, CardinalAtlas]
);