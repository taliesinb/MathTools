PublicGraphicsPrimitive[BoundingGrid]

DeclareGraphicsPrimitive[BoundingGrid, "Pair", toBoundingGridBoxes]

(**************************************************************************************************)

PublicOption[GridLineColor, GridLineOpacity, GridLineThickness, MinGridLineSpacing, MaxGridLines, TickOptions]

$baseGridLineBoxesOpts = {
	MinGridLineSpacing -> 5,
	MaxGridLines -> 50,
	GraphicsScale -> Automatic,
	GridLines -> True,
	Ticks -> True,
	TickOptions -> {},
	GridLineColor -> {GrayLevel[0.2], GrayLevel[0.5], GrayLevel[0.8]},
	GridLineOpacity -> 0.5,
	GridLineThickness -> {1.25, .75, .25}
};

Options[BoundingGrid] = $baseGridLineBoxesOpts;

toBoundingGridBoxes[BoundingGrid[range_List, opts___Rule]] :=
	GridLineBoxes[range, opts];

(**************************************************************************************************)

PublicFunction[GridLineBoxes]

Options[GridLineBoxes] = $baseGridLineBoxesOpts;

GridLineBoxes[plotRange_ ? NumericMatrixQ, OptionsPattern[]] := Scope[

	UnpackOptions[gridLines, ticks, gridLineThickness, gridLineColor, gridLineOpacity, graphicsScale, minGridLineSpacing, maxGridLines, ticks, tickOptions];

	SetAutomatic[graphicsScale, $MakeBoxesStyleData[GraphicsScale]];
	SetNone[graphicsScale, 1];
	minGap = minGridLineSpacing / graphicsScale;

	doLines = ToPair @ gridLines;
	divs = ZipMap[
		If[#1, Values @ NiceTickDivisions[#2, #3, #4], {}]&,
		doLines, plotRange, ToPair @ minGap, ToPair @ maxGridLines
	];
	specLen = Max @ Map[Len, divs];

	$depthStyles = MapThread[
		LineStyleBoxOperator,
		ParseListSpec[#, specLen]& /@ {gridLineColor, gridLineOpacity, gridLineThickness}
	];

	{$xBounds, $yBounds} = plotRange;
	lineBoxes = ZipMap[MapIndex1, {xLineBox, yLineBox}, divs];
	lineBoxes = StyleBox[lineBoxes, Antialiasing -> False];

	doTicks = ThreadAnd[ToPair @ ticks, doLines];
	tickBoxes = FrameTickLabelBoxes[plotRange, getDivsToTick /@ divs, Ticks -> doTicks, Seq @@ tickOptions];

	If[tickBoxes =!= {}, {lineBoxes, tickBoxes}, lineBoxs]
];

getDivsToTick[{}] := {};
getDivsToTick[{a_}] := a;
getDivsToTick[{a_, b_, ___}] := SelectFirst[{a, Union[a, b]}, 2 < Length[#] <= 15&, a];

_GridLineBoxes := BadArguments[];

xLineBox[divs_, d_] := makeLineBox[d, Thread[{#, $yBounds}]& /@ divs];
yLineBox[divs_, d_] := makeLineBox[d, Thread[{$xBounds, #}]& /@ divs];
makeLineBox[d_, coords_] := Part[$depthStyles, d] @ Cons[LineBox, ToPacked @ coords];

