PublicTypesettingForm[ChartSymbol]

SetUsage @ "
ChartSymbol[sub$] represents a chart and formats as C$sub.
"

declareFormatting[
  ChartSymbol[sym_Str] :> formatChartSymbol[sym, Auto],
  ChartSymbol[list_List]  :> ChartSymbolForm[list],
  ChartSymbol[other__]    :> ChartSymbolForm[Row[{other}]]
];

(**************************************************************************************************)

PrivateFunction[formatChartSymbol]

formatChartSymbol[sym_Str, colors_] := Scope[
  Style[
    ChartSymbolForm[sym]
  ,
    cards = ToPathWord @ STrim[sym, {"+" | "-"}];
    c = Lookup[
      If[colors === Auto, ChooseCardinalColors @ cards, colors],
      cards,
      $Failed
    ];
    If[ContainsQ[c, $Failed], Sequence @@ {},
      FontColor -> OklabDarker[HumanBlend @ Discard[c, GrayColorQ], .2]
    ]
  ]
];

(**************************************************************************************************)

PublicFunction[ChartSymbolCardinals]

SetUsage @ "
ChartSymbolCardinals[chart$] returns the list of cardinals in a ChartSymbol[$$].
"

ChartSymbolCardinals = Case[
  ChartSymbol[s_, ___] := ToPathWord[s];
  ChartSymbol[s_List, ___] := s;
  (GraphRegionIntersection|GraphRegionUnion)[s__] := Union @@ Map[%, {s}];
];

(**************************************************************************************************)

PublicFunction[CardinalTransitionMatrices]

CardinalTransitionMatrices[atlas_Graph] := Scope[
  edgeAnnos = LookupExtendedOption[atlas, EdgeAnnotations];
  cardinals = CardinalList @ atlas;
  transitions = Lookup[Rep[edgeAnnos, None -> <||>], "CardinalTransitions", None];
  If[!AssocQ[transitions], ReturnFailed[]];
  tagIndices = TagIndices @ atlas;
  numCardinals = Len @ cardinals;
  cardinalIndex = AssociationRange @ cardinals;
  matrixShape = {numCardinals, numCardinals};
  KSortBy[IndexOf[cardinals, #]&] @ Assoc @ KVMap[buildTagMatrices, tagIndices]
];

(* TODO: check for compatibility *)
buildTagMatrices[tag_, edgeIndices_] := Scope[
  trans = Flatten @ Lookup[transitions, edgeIndices, {}];
  matrix = Normal @ SparseArray[procTransRule /@ trans, matrixShape];
  tag -> matrix
];

procTransRule = Case[
  a_ -> b_          := Lookup[cardinalIndex, {b, a}] -> 1;
  a_ -> Inverted[b_] := Lookup[cardinalIndex, {b, a}] -> -1;
];

(**************************************************************************************************)

PublicFunction[CardinalTransitionRepresentation]

CardinalTransitionRepresentation[atlas_Graph] := Scope[
  matrices = CardinalTransitionMatrices[atlas];
  If[FailureQ @ matrices, ReturnFailed[]];
  PathRepresentation[
    atlas,
    matrices
  ]
];

(**************************************************************************************************)

PublicFunction[TransportAtlas]

TransportAtlas[quiver_, charts_, opts___Rule] := Scope @ Catch[
  If[!EdgeTaggedGraphQ[quiver], ReturnFailed[]];
  colors = LookupCardinalColors @ quiver;
  $charts = toChart /@ charts;
  $fullCharts = Join[#, Map[Inverted, #]]& /@ $charts;
  If[!MatchQ[$charts, {__List}], ReturnFailed[]];
  $quiver = ToIndexGraph @ quiver;
  {chartVertices, chartEdges} = Transpose @ Map[getChartRegion, $charts];
  vertexToCharts = PositionIndex[chartVertices, 2];
  edgeToChart = PositionIndex[chartEdges, 2];
  edgeList = EdgeList @ $quiver;
  sharedEdges = Select[vertexToCharts, Len[#] > 1&];
  groupedSharedEges = GroupBy[sharedEdges, Id, Keys];
  overlapCardinalRelations = findOverlapRelations[];
  transitionEdges = Dedup @ Flatten @ Map[toTransitionEdge, edgeList];
  cardinalTransitionAnnos = Assoc @ MapIndexed[toCardTransAnnos, transitionEdges];
  (* the next step is to build CardinalTransitions for these chart transitions.
  we should do this by finding cardinals that are part of the 'from' but not part of the 'to',
  and vice versa, and see how they are identified on the overlap.
  *)
  chartSymbolAnno = Map[toChartSymbol, $charts];
  ExtendedGraph[
    Range @ Len @ $charts,
    transitionEdges, opts,
    VertexAnnotations -> <|"ChartSymbol" -> chartSymbolAnno|>,
    EdgeAnnotations -> <|"CardinalTransitions" -> cardinalTransitionAnnos|>,
    GraphTheme -> "TransportAtlas",
    CardinalColors -> colors,
    GraphOrigin -> 1,
    BaselinePosition -> Scaled[0.5]
  ]
,
  TransportAtlas
];

DefineGraphTheme["TransportAtlas",
  VertexLabels -> "ChartSymbol",
  VertexLabelPosition -> Auto,

  EdgeLabels -> ("CardinalTransitions" -> CardinalTransition),
  EdgeLabelPosition -> {Scaled[0.5], Above},
  ArrowheadPosition -> 0.8
];

toChartSymbol = Case[
  list_List ? StrVecQ := ChartSymbol @ SJoin @ list;
  other_                    := ChartSymbol @ other;
];

toCardTransAnnos[DirectedEdge[a_, b_, _], {index_}] := Scope[
  relations = Lookup[overlapCardinalRelations, Key @ {a, b}, {}];
  If[relations === {}, Nothing, index -> relations]
];

findOverlapRelations[] := Scope[
  relations = Dedup @ Flatten @ Cases[
    edgeList,
    DirectedEdge[a_, b_, CardinalSet[set_List]] :> (
      aCharts = vertexToCharts @ a;
      bCharts = vertexToCharts @ b;
      (* head and tail have to both be in the same chart *)
      charts = Inter[aCharts, bCharts];
      Outer[
        {aChart, bChart} |-> If[aChart === bChart, Nothing,
          {aCards, bCards} = Part[$fullCharts, {aChart, bChart}];
          aNotBCards = Comp[aCards, bCards];
          bNotACards = Comp[bCards, aCards];
          {aChart, bChart} -> Outer[
            normRule,
            Inter[set, aNotBCards],
            Inter[set, bNotACards],
            1
          ]
        ],
        charts, charts, 1
      ]
    )
  ];
  Merge[relations, Flatten /* Dedup]
]

normRule[Inverted[a_], b_] := a -> Inverted[b];
normRule[a_, b_] := a -> b;

setToRelations[set_List] := TwoWayRule @@@ UnorderedPairs[set];

TransportAtlas::notchart = "Chart specification `` is not a list of cardinals or a ChartSymbol.";

toChart = Case[
  list_List       := list;
  cs_ChartSymbol  := ChartSymbolCardinals[cs];
  other_          := failCA["notchart", other];
];

(* this finds all transitions between charts that this edge could correspond to. the vertices
   are chart indices, the tags are which cardinal moves between those charts *)
toTransitionEdge[DirectedEdge[a_, b_, c_]] := Scope[
  aCharts = vertexToCharts @ a;
  bCharts = vertexToCharts @ b;
  bNotACharts = Comp[bCharts, aCharts];
  bAndACharts = Inter[aCharts, bCharts];
  (* transitions will be pairs where a B chart was absent on the left but present on the right,
  but A was present on both *)
  If[bNotACharts === {} && aNotBCharts === {}, Return[Nothing]];
  Outer[
    DirectedEdge[#1, #2, sharedCardinal[#1, #2, c]]&,
    bAndACharts, bNotACharts, 1
  ]
];

sharedCardinal[c1_, c2_, cs:CardinalSet[cards_]] := Scope[
  set = Inter[cards, Part[$fullCharts, c1], Part[$fullCharts, c2]];
  If[Len[set] === 1, F @ set, CardinalSet @ set]
];

sharedCardinal[_, _, c_] := c;

TransportAtlas::notatlas = "Cardinals `` do not form a chart.";

getChartRegion[cardinals_] := Match[
  GraphRegion[$quiver, ChartSymbol[cardinals]],
  {GraphRegionData[vertices_, edges_]} :> {vertices, edges},
  _ :> failCA["notatlas", cardinals];
];

failCA[msg_Str, args___] := (
  Message[MessageName[TransportAtlas, msg], args];
  Throw[$Failed, TransportAtlas]
);