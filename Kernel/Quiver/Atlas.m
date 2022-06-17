PublicForm[ChartSymbol]

SetUsage @ "
ChartSymbol[sub$] represents a chart and formats as C$sub.
"

declareFormatting[
  ChartSymbol[sym_String] :> formatChartSymbol[sym, Automatic],
  ChartSymbol[list_List]  :> ChartSymbolForm[list],
  ChartSymbol[other__]    :> ChartSymbolForm[Row[{other}]]
];

(**************************************************************************************************)

PrivateFunction[formatChartSymbol]

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
  sharedEdges = Select[vertexToCharts, Length[#] > 1&];
  groupedSharedEges = GroupBy[sharedEdges, Identity, Keys];
  overlapCardinalRelations = findOverlapRelations[];
  transitionEdges = DeleteDuplicates @ Flatten @ Map[toTransitionEdge, edgeList];
  cardinalTransitionAnnos = Association @ MapIndexed[toCardTransAnnos, transitionEdges];
  (* the next step is to build CardinalTransitions for these chart transitions.
  we should do this by finding cardinals that are part of the 'from' but not part of the 'to',
  and vice versa, and see how they are identified on the overlap.
  *)
  chartSymbolAnno = Map[toChartSymbol, $charts];
  ExtendedGraph[
    Range @ Length @ $charts,
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

$TransportAtlasThemeRules = {
  VertexLabels -> "ChartSymbol",
  VertexLabelPosition -> Automatic,

  EdgeLabels -> ("CardinalTransitions" -> CardinalTransition),
  EdgeLabelPosition -> {Scaled[0.5], Above},
  ArrowheadPosition -> 0.8
}

$GraphThemeData["TransportAtlas"] := $TransportAtlasThemeRules;

toChartSymbol = Case[
  list_List ? StringVectorQ := ChartSymbol @ StringJoin @ list;
  other_                    := ChartSymbol @ other;
];

toCardTransAnnos[DirectedEdge[a_, b_, _], {index_}] := Scope[
  relations = Lookup[overlapCardinalRelations, Key @ {a, b}, {}];
  If[relations === {}, Nothing, index -> relations]
];

findOverlapRelations[] := Scope[
  relations = DeleteDuplicates @ Flatten @ Cases[
    edgeList,
    DirectedEdge[a_, b_, CardinalSet[set_List]] :> (
      aCharts = vertexToCharts @ a;
      bCharts = vertexToCharts @ b;
      (* head and tail have to both be in the same chart *)
      charts = Intersection[aCharts, bCharts];
      Outer[
        {aChart, bChart} |-> If[aChart === bChart, Nothing,
          {aCards, bCards} = Part[$fullCharts, {aChart, bChart}];
          aNotBCards = Complement[aCards, bCards];
          bNotACards = Complement[bCards, aCards];
          {aChart, bChart} -> Outer[
            normRule,
            Intersection[set, aNotBCards],
            Intersection[set, bNotACards],
            1
          ]
        ],
        charts, charts, 1
      ]
    )
  ];
  Merge[relations, Flatten /* DeleteDuplicates]
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
  bNotACharts = Complement[bCharts, aCharts];
  bAndACharts = Intersection[aCharts, bCharts];
  (* transitions will be pairs where a B chart was absent on the left but present on the right,
  but A was present on both *)
  If[bNotACharts === {} && aNotBCharts === {}, Return[Nothing]];
  Outer[
    DirectedEdge[#1, #2, sharedCardinal[#1, #2, c]]&,
    bAndACharts, bNotACharts, 1
  ]
];

sharedCardinal[c1_, c2_, cs:CardinalSet[cards_]] := Scope[
  set = Intersection[cards, Part[$fullCharts, c1], Part[$fullCharts, c2]];
  If[Length[set] === 1, First @ set, CardinalSet @ set]
];

sharedCardinal[_, _, c_] := c;

TransportAtlas::notatlas = "Cardinals `` do not form a chart.";

getChartRegion[cardinals_] := Match[
  GraphRegion[$quiver, ChartSymbol[cardinals]],
  {GraphRegionData[vertices_, edges_]} :> {vertices, edges},
  _ :> failCA["notatlas", cardinals];
];

failCA[msg_String, args___] := (
  Message[MessageName[TransportAtlas, msg], args];
  Throw[$Failed, TransportAtlas]
);