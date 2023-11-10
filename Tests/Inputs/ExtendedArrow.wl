(* ::Package:: *)

TestRaster @ DebugGraphics[Table[
	Arrowhead[{0, p*4}, {1, 0}, ArrowheadAnchor -> p, ArrowheadLength -> 1],
	{p, 0, 1, .25}], GraphicsScale -> 20]


TestRaster @ DebugGraphics[{Circle[{0,0},1], 
	ExtendedArrow[{{0,0},{1,0}}, 
	ArrowThickness -> 3, ArrowheadShape -> "Square",
	ArrowheadLength -> Unthread[{.1, .5, 1}]], Red, Point[{1,0}]}]


TestRaster @ FixedGraphics[ExtendedArrow[{{0,0},{1,0}}, ArrowheadColor -> $Red]]


TestRaster @ FixedGraphics[ExtendedArrow[{{0,0},{1,0}}, ArrowColor -> $Red]]


TestRaster @ FixedGraphics[ExtendedArrow[{{0,0},{1,0}}, ArrowColor -> $Red, ArrowheadColor -> $Green]]


TestRaster @ FixedGraphics[ExtendedArrow[{{0,0},{1,0}}, ArrowColor -> $Red, ArrowheadColor -> $Green]]
