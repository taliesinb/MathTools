PackageExport["PlotDirectedHypergraphEncodedGraph"]

Options[PlotDirectedHypergraphEncodedGraph] = Options[ExtendedGraph];

PlotDirectedHypergraphEncodedGraph[vertices_List, hyperedges_List, opts:OptionsPattern[]] := ExtendedGraphPlot @ ExtendedGraph[
	vertices, DirectedEdge @@@ hyperedges,
	opts,
	GraphTheme -> "ColoredGraphUnion",
	CardinalColors -> <|1 -> $Red, 2 -> $Blue, 3 -> $Purple|>
];

PlotDirectedHypergraphEncodedGraph[hyperedges_List, opts:OptionsPattern[]] := ExtendedGraphPlot @ ExtendedGraph[
	DirectedEdge @@@ hyperedges,
	opts,
	GraphTheme -> "ColoredGraphUnion",
	CardinalColors -> <|1 -> $Red, 2 -> $Blue, 3 -> $Purple|>
];

$GraphThemeData["ColoredGraphUnion"] = {
	ArrowheadSize -> 10, ArrowheadShape -> {"Line", TwoWayStyle -> "CrossBar"}, ArrowheadPosition -> 0.5,
	VertexSize -> 5, EdgeColorFunction -> "Cardinal",
	ImageSize -> "ShortestEdge" -> 50
};

(**************************************************************************************************)

PackageExport["DirectedHypergraphToGraphs"]

DirectedHypergraphToGraphs[hyperedges_] := Scope[
	maxIndex = Max[Part[hyperedges, All, 3]];
	vertices = Union[Part[hyperedges, All, 1], Part[hyperedges, All, 2]];
	Table[
		ExtendedGraph[vertices, Cases[hyperedges, {i_, j_, n} :> DirectedEdge[i, j]]],
		{n, 1, maxIndex}
	]
]

(**************************************************************************************************)

PackageExport["GraphsToDirectedHypergraph"]

GraphsToDirectedHypergraph[gs__Graph] :=
	Join @@ MapIndexed[AppendConstantColumn[List @@@ EdgeList[#1], First @ #2]&, {gs}]
