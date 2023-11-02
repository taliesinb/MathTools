(* ::Package:: *)

RQG;
Clear[makeShape, neatLine];
makeShape[pos_, col_, Rectangular[sz_]] := {FaceEdgeForm[None, col], CenteredRectangle[pos, sz * 2]};
makeShape[pos_, col_, r_] := {col, Circle[pos, r]};
neatLine[{pos1_, pos2_}, {sz1_, sz2_}, opts___] := List[
	NeatCurve[{pos1, pos2}, FilterOptions @ opts, Setback -> {sz1, sz2}],
	makeShape[pos1, $Green, sz1],
	makeShape[pos2, $Red, sz2]
];


RQG;
Clear[neatLinePlot];
neatPlot[prims_, opts___] := FixedGraphics[prims, GraphicsScale -> Lookup[{opts}, GraphicsScale, 50], Frame -> True];

neatLinePlot[pos_, size_, opts___] :=
	neatPlot[neatLine[{{0,0}, pos}, size, opts], opts];
	
neatLinePlot[pos_ ? MatrixQ, size_, opts___] := 
	neatPlot[neatLine[{{0,0}, Unthread[pos, 2]}, size, opts], opts];

$recrec = {Rectangular[{.3,.2}], Rectangular[{.1,.1}]};
$recdisk = {Rectangular[{.3,.2}], .12};
$diskdisk = {.2, .12};


(* ::Subsubsection:: *)
(*test angled*)


TestRaster @ Row @ neatLinePlot[{1, 2}, $recrec, JoinStyle -> Unthread[{{TopRight, Bottom}, {Top,BottomLeft}, {TopRight, BottomLeft}}]]


(* ::Subsubsection:: *)
(*test snapping happens properly*)


$sizes = {$recrec, $recdisk, $diskdisk};
TestRaster @ Row @ neatLinePlot[{{-0.1, 1}, {0.4, 1}}, Unthread[$sizes], JoinStyle -> Axis]


(* ::Subsubsection:: *)
(*test axis, horizontal, vertical*)


clock[n_] := AnticlockwiseCirclePoints[n];
upclock[n_] := Select[clock[n], P2 /* Positive];
downclock[n_] := Select[clock[n], P2 /* Negative];
$joinStyles = {Axis,Horizontal, Vertical, Above, Below};
TestRaster @ Grid @ neatLinePlot[clock[16], Unthread[$sizes], JoinStyle -> Unthread[$joinStyles], GraphicsScale -> 70]


(* ::Subsubsection:: *)
(*test segment position*)


TestRaster @ Row @ neatLinePlot[downclock[16], $recrec, JoinStyle -> Vertical, SegmentPosition -> Unthread[{0,.5,1}], BendRadius -> 0.1, GraphicsScale -> 100]


(* ::Subsubsection:: *)
(*test shortcut*)


TestRaster @ Grid @ Transpose @ neatLinePlot[clock[16], $diskdisk, JoinStyle -> Unthread[$joinStyles], ShortcutRadius -> Unthread[{0, .1}], GraphicsScale -> 70]
