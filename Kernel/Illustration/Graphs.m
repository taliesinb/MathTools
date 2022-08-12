PublicFunction[ColoredGraph]

ColoredGraph[edges_List, opts___Rule] :=
  ColoredGraph[AllUniqueVertices @ edges, edges, opts];

ColoredGraph[vertices_List, edges_List, opts___Rule] := Scope[
  vertexColors = AssociationMap[ColoredGraphCardinalColorFunction, vertices];
  cardinalColors = If[Length[First @ edges] =!= 3, None,
    AssociationMap[ColoredGraphCardinalColorFunction, DeleteDuplicates @ Part[edges, All, 3]]
  ];
  ExtendedGraph[
    vertices, edges, opts,
    VertexColorFunction -> vertexColors,
    If[cardinalColors === None,
      ArrowheadShape -> "Line",
      CardinalColorFunction -> ColoredGraphCardinalColorFunction
    ],
    GraphTheme -> "ColoredGraph"
  ]
];

(**************************************************************************************************)

DefineGraphTheme["ColoredGraph",
    VertexSize -> 10,
    ArrowheadSize -> MediumSmall,
    ImageSize -> {50, 50},
    ImagePadding -> 10,
    SelfLoopRadius -> 0.4, MultiEdgeDistance -> 0.4,
    AspectRatioClipping -> False, (* Frame -> True, *)
    EdgeThickness -> 1, EdgeStyle -> Directive[{AbsoluteThickness[0], GrayLevel[0.7, 1]}],
    ArrowheadShape -> {"FlatArrow", BorderStyle -> Function[{Darker[#, .3], AbsoluteThickness[0]}]}
];

(**************************************************************************************************)

PublicFunction[ColoredGraphCardinalColorFunction]

$colorRules = <|
  "r" -> $Red, "b" -> $Blue, "g" -> $Green,
  "R" -> $LightRed, "B" -> $LightBlue, "G" -> $LightGreen,
  "o" -> $Orange, "t" -> $Teal, "p" -> $Pink,
  "O" -> $LightOrange, "T" -> $LightTeal, "P" -> $LightPink,
  "0" -> Black, "1" -> $DarkGray, "2" -> $Gray, "3" -> $LightGray, "4" -> White,
  "w" -> White
|>;

ColoredGraphCardinalColorFunction[str_String] :=
  HumanBlend @ Lookup[$colorRules, Characters @ str, Nothing];

(**************************************************************************************************)

PublicFunction[PartialOrderGraph]

PartialOrderGraph[vertices_, edges_, opts___Rule] := Scope[
  If[MatchQ[vertices, {__Graph} -> _List],
    {graphs, opts} = FirstLast @ vertices;
    plots = ExtendedGraphPlot[#, opts]& /@ graphs;
    vertices = Range @ Length @ vertices;
    shapes = AssociationThread[vertices, plots];
    vsize = 100;
  ,
    shapes = Automatic;
    vsize = 6;
  ];
  ExtendedGraph[vertices, edges,
    opts,
    VertexShapeFunction -> shapes,
    VertexLayout -> TreeVertexLayout[Balanced -> True],
    GraphOrigin -> First @ vertices,
    ArrowheadShape -> None, ArrowheadSize -> Huge,
    ImageSize -> 250, VertexSize -> vsize, EdgeThickness -> 3
  ]
];

(**************************************************************************************************)

PublicFunction[SimpleLabeledGraph]

SimpleLabeledGraph[args___] := ExtendedGraph[args, GraphTheme -> "SimpleLabeledGraph"]

(**************************************************************************************************)

DefineGraphTheme["SimpleLabeledGraph",
  CardinalColors -> None,
  VertexLabels -> "Name",
  VertexLabelPosition -> Automatic,
  VertexLabelBaseStyle -> $MathLabelStyle,
  EdgeLabels -> "Cardinal",
  EdgeLabelBaseStyle -> $CardinalLabelStyle,
  EdgeLabelSpacing -> -0.3,
  ArrowheadShape -> {"Line", EdgeThickness -> 2},
  ImagePadding -> {{0,0}, {0, 25}},
  VertexLayout -> SmartLayout[],
  MultiEdgeDistance -> 0.3, ArrowheadPosition -> 0.525,
  ArrowheadSize -> Large, ArrowheadStyle -> $Gray,
  ImageSize -> "ShortestEdge" -> 90
];

(**************************************************************************************************)

PublicFunction[BasicGraph]

BasicGraph[graph_Graph, opts___Rule] :=
  BasicGraph[EdgeList[graph], opts, GraphOrigin -> LookupExtendedOption[graph, GraphOrigin]];

BasicGraph[spec_, opts___Rule] := BasicGraph[toGraph @ spec, opts];

BasicGraph[edges_List ? EdgeListQ, opts___Rule] := ExtendedGraph[edges, opts, GraphTheme -> "BasicGraph"];

toGraph = Case[
  i_Integer                         := LineQuiver[i, GraphOrigin -> Automatic, VertexOrigin -> Automatic];
  "Line"                            := LineQuiver[6, GraphOrigin -> Automatic, VertexOrigin -> Automatic, PeripheralVertices -> 1];
  {w_Integer, h_Integer}            := SquareQuiver[{w, h}, GraphOrigin -> Automatic, VertexOrigin -> Automatic];
  "Square"                          := SquareQuiver[6, GraphOrigin -> Automatic, VertexOrigin -> Automatic, PeripheralVertices -> 3];
  "Triangle"                        := TriangularQuiver[6, GraphOrigin -> Automatic, VertexOrigin -> Automatic, PeripheralVertices -> 5];
  {w_Integer, h_Integer, d_Integer} := CubicQuiver[{w, h, d}, GraphOrigin -> Automatic, VertexOrigin -> Automatic];
];

DefineGraphTheme["BasicGraph",
  ArrowheadShape -> None,
  EdgeLength -> 60,
  EdgeThickness -> 2,
  ImagePadding -> 10,
  VertexLabelPosition -> Below,
  VertexLabelBaseStyle -> $MathLabelStyle,
  VertexLayout -> SmartLayout[]
];

(**************************************************************************************************)

PublicFunction[PathedBasicGraph]

PathedBasicGraph[spec_, paths_, opts___Rule] :=
  BasicGraph[
    spec,
    GraphHighlight -> {DotLine /@ paths},
    opts,
    HighlightStyle -> {PathRadius -> 8, DiskRadius -> 8},
    HighlightColor -> $Green
  ];

(**************************************************************************************************)

PublicFunction[SimpleLabeledQuiver]

$rgbList = {"r", "g", "b"};
$abcList = {"a", "b", "c"};

SimpleLabeledQuiver[args___] := Scope[
  res = Quiver[args];
  cards = DeleteDuplicates @ EdgeTags[res];
  Which[
    SubsetQ[$rgbList, cards], cards = Select[$rgbList, MemberQ[cards, #]&],
    SubsetQ[$abcList, cards], cards = Select[$abcList, MemberQ[cards, #]&],
    True, Null
  ];
  ExtendedGraph[res, Cardinals -> cards, GraphTheme -> "SimpleLabeledQuiver"]
];

(**************************************************************************************************)

DefineGraphTheme["SimpleLabeledQuiver",
  VertexLabels -> "Name",
  VertexLabelPosition -> Automatic,
  VertexLabelBaseStyle -> $MathLabelStyle,
  VertexLayout -> LinearLayout[],
  MultiEdgeDistance -> 0.1,
  ArrowheadShape -> {"Line", EdgeThickness -> 2},
  ArrowheadPosition -> 0.59,
  ArrowheadSize -> Medium,
  ImageSize -> "ShortestEdge" -> 90
];

(**************************************************************************************************)
