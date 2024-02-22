PublicGraphicsPrimitive[BoundingGrid]

DeclareGraphicsPrimitive[BoundingGrid, "Pair", boundingGridBoxes]

PrivateFunction[boundingGridBoxes]

boundingGridBoxes[BoundingGrid[{x:{xl_, xh_}, y:{yl_, yh_}}, scale_:100]] := Scope[
	$cutoff = 10 / scale; margin = 10 / scale;
	dx = base[xh - xl]; dy = base[yh - yl];
	{xl2, xh2} = {xl, xh} + {-1,1} * margin;
	{yl2, yh2} = {yl, yh} + {-1,1} * margin;
	dxs = Select[Between[{xl2, xh2}]];
	dys = Select[Between[{yl2, yh2}]];
	xl = Floor[xl, dx]; xh = Ceiling[xh, dx];
	yl = Floor[yl, dy]; yh = Ceiling[yh, dy];
	$xb = {xl2, xh2}; $yb = {yl2, yh2};
	doLabelH = (xh - xl) > $cutoff * 5;
	doLabelV = (yh - yl) > $cutoff * 5;
	List[
		AbsoluteThickness[0.5], Antialiasing -> False,
		$glf = vlineBox; $lbl = If[doLabelH, hlabel, Nothing&]; $sel = dxs; gridLine[{xl, xh}],
		$glf = hlineBox; $lbl = If[doLabelV, vlabel, Nothing&]; $sel = dys; gridLine[{yl, yh}],
		If[xl <= 0 <= xh, StyleBox[Construct[LineBox, {{0, yl2}, {0, yh2}}], GrayLevel[0.2]], Nothing],
		If[yl <= 0 <= yh, StyleBox[Construct[LineBox, {{xl2, 0}, {xh2, 0}}], GrayLevel[0.2]], Nothing],
		If[!doLabelH, hlabel[{Avg[xl, xh]}], Nothing],
		If[!doLabelV, vlabel[{Avg[yl, yh]}], Nothing]
	]
];

$colors = {GrayLevel[0.2, .8], GrayLevel[0.7, .5], GrayLevel[0.8, .3]};
gridLineBox[coords_, d_] := StyleBox[Construct[LineBox, ToPacked @ coords], Part[$colors, d+1]];

$labelStyle = {FontSize -> 8, FontFamily -> "Fira Code"};

lblString[0.|0] := "0";
lblString[e_] := Scope[
	col = If[Negative[e], $Red, $Blue];
	str = TextString @ Abs @ e;
	str //= SRep[StartOfString ~~ "0." -> "."];
	str //= StringTrimRight["0"];
	str //= StringTrimRight["."];
	StyleBox[str, col]
];

hlabel[xb_] := Construct[InsetBox, lblString[#], Offset[{0, -2}, {#, yl2}], ImageScaled[{.5, 1}], BaseStyle -> $labelStyle]& /@ xb;
vlabel[yb_] := Construct[InsetBox, lblString[#], Offset[{2, 0}, {xh2, #}], ImageScaled[{0, 0.5}], BaseStyle -> $labelStyle]& /@ yb;

hlineBox[yb_, d_] := gridLineBox[Thread[{$xb, #}]& /@ yb, d];
vlineBox[xb_, d_] := gridLineBox[Thread[{#, $yb}]& /@ xb, d];

base[n_] := Power[10, Ceiling[Log10[n/10]]];

gridLine[{l_, h_}] := Scope[
	dx = base[h - l];
	range = $sel @ Range[l, h, dx];
	If[dx < $cutoff, range = {l, h}];
	labels = $lbl @ $sel @ Which[
		Len[range] > 5, Range[l, h, dx * 2],
		Len[range] <= 3, Range[l, h, dx / 5],
		True, range
	];
	boxes = {$glf[range, 0], labels}; d = 0;
	While[(dx /= If[d >= 2, 5, 10]) >= $cutoff && (d++ < 2),
		range = Comp[$sel @ Range[l, h, dx], range];
		AppTo[boxes, $glf[range, d]];
	];
	If[d == 0, boxes //= RepAll[_GrayLevel :> Part[$colors, 2]]];
	boxes
];