PublicFunction[PlotDirectedHypergraphEncodedGraph]

$defaultCardinalColors = <|1 -> $Red, 2 -> $Blue, 3 -> $Purple, 4 -> $Green, 5 -> $Orange, 6 -> $Teal, 7 -> $Pink|>;

Options[PlotDirectedHypergraphEncodedGraph] = Options[ExtendedGraph];

PlotDirectedHypergraphEncodedGraph[vertices_List, hyperedges_List, opts:OptionsPattern[]] := ExtendedGraphPlot @ ExtendedGraph[
	vertices, DirectedEdge @@@ hyperedges,
	opts,
	GraphTheme -> "ColoredGraphUnion",
	CardinalColors -> $defaultCardinalColors
];

PlotDirectedHypergraphEncodedGraph[hyperedges_List, opts:OptionsPattern[]] := ExtendedGraphPlot @ ExtendedGraph[
	DirectedEdge @@@ hyperedges,
	opts,
	GraphTheme -> "ColoredGraphUnion",
	CardinalColors -> $defaultCardinalColors
];

DefineGraphTheme["ColoredGraphUnion",
	ArrowheadSize -> 10, ArrowheadShape -> {"Line", TwoWayStyle -> "CrossBar"}, ArrowheadPosition -> 0.5,
	VertexSize -> 5, EdgeColorFunction -> "Cardinal",
	ImageSize -> "ShortestEdge" -> 50
];

(**************************************************************************************************)

PublicFunction[DirectedHypergraphToGraphs]

DirectedHypergraphToGraphs[hyperedges_] := Scope[
	maxIndex = Max[Part[hyperedges, All, 3]];
	vertices = Union[Part[hyperedges, All, 1], Part[hyperedges, All, 2]];
	Table[
		ExtendedGraph[vertices, Cases[hyperedges, {i_, j_, n} :> DirectedEdge[i, j]]],
		{n, 1, maxIndex}
	]
]

(**************************************************************************************************)

PublicFunction[GraphsToDirectedHypergraph]

GraphsToDirectedHypergraph[gs__Graph] :=
	Join @@ MapIndex1[AppendConstantColumn[List @@@ EdgeList[#1], #2]&, {gs}]
