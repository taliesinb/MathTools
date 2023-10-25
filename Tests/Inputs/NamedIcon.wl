(* ::Package:: *)

(* ::Subsection:: *)
(*icon typesetting form*)


TestBoxes @ NamedIcon["Triangle"]


(* ::Subsection:: *)
(*ordinary (inset) graphics*)


TestRaster @ Graphics[NamedIcon[{0, 0}, {1, 0}, "Triangle"], ImageSize -> 50]


(* ::Subsection:: *)
(*ordinary (inset) graphics with an explicit scale*)


TestRaster @ Graphics[NamedIcon[{0, 0}, {1, 0}, "Triangle", GraphicsScale -> 25], PlotRange -> {{-1, 1}, {-1, 1}}, ImageSize -> 50]


(* ::Subsection:: *)
(*fixed graphics*)


TestRaster @ FixedGraphics @ NamedIcon[{0, 0}, {1, 0}, "Triangle"]


(* ::Subsection:: *)
(*all icons, alignment point*)

$icons = Select[NamedIconData[], StringFreeQ[{"Upper","Lower"}]]; $n = Length @ $icons;
TestRaster @ FixedGraphics @ Table[
	pos = {i/2.25,2*r};
	{NamedIcon[pos,{1,0}, Part[$icons, i], ImageSize -> 30, IconThickness -> 2, AlignmentPoint -> r], $Orange, Point[pos]},
	{r,0,1,.2}, {i, $n}
]
