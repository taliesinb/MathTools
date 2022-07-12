(* PublicFunction[ToSVGString]

ToSVGString[g_Graphics] := Scope[
	{{xmin, xmax}, {ymin, ymax}} = GraphicsPlotRange @ g;
	xsize = (xmax - xmin); ysize = (ymax - ymin);
	{width, height} = LookupImageSize[g, AspectRatio -> 1];
	
	prims = First @ g;
	$context = <|"fill" -> Black, "stroke" -> Black, "pointsize" -> apsToCoords[3], "thickness" -> apsToCoords[1]|>;
	contents = StringJoin @ svg[prims];

	background = LookupOption[g, Background];
	If[ColorQ @ background, contents = StringJoin[makeBackgroundRect @ background, contents]];

	StringTrim @ $svgOuterTemplate @ Association[
		"width" -> width, "height" -> height,
		"xmin" -> xmin, "xsize" -> xsize, "ymin" -> -ymax, "ysize" -> ysize,
		"contents" -> StringTrim[contents]
	]
];

$svgOuterTemplate = StringTemplate @ """
<svg width="`width`" height="`height`" viewBox="`xmin` `ymin` `xsize` `ysize`" xmlns="http://www.w3.org/2000/svg">
`contents`
</svg>
"""

(**************************************************************************************************)

nq = NumericQ;
cmq = CoordinateMatrixQ;
caq = CoordinateArrayQ;

(**************************************************************************************************)

SetHoldFirst @ freshContext;

freshContext[e_] := Block[{$context = $context}, e];

(**************************************************************************************************)

svgFail[msg_String, args___] := Message[MessageName[ToSVGString, msg], args];

(**************************************************************************************************)

parseContext = Case[
	color_ ? ColorQ 				         := $context["fill"] = $context["stroke"] = color;
	PointSize[ps_ ? nq]     		     := $context["pointsize"] = psToCoords @ ps;
	AbsolutePointSize[ps_ ? nq]      := $context["pointsize"] = apsToCoords @ ps;
	EdgeForm[e_]					           := parseEdgeForm[e];
	FaceForm[e_]					           := parseFaceForm[e];
  Thickness[e_]                    := parseThickness[e];
	other_                           := $Failed;
];

$thicknessRules = <|Tiny -> 0.2, Small -> 0.5, Medium -> 1, Automatic -> 1, Large -> 2|>;

parseThickness = Case[
  (Thickness|AbsoluteThickness)[s_Symbol] := % @ AbsoluteThickness @ Lookup[$thicknessRules, s];
  AbsoluteThickness[s_ ? nq]              := $context["thickness"] = apsToCoords @ s;
  Thickness[s_ ? nq]                      := $context["thickness"] = psToCoords @ s;
];

parseEdgeForm = Case[
	color_ ? ColorQ 				         := $context["stroke"] = color;
	(* thickness etc *)
]
	
parseFaceForm = Case[
	color_ ? ColorQ 				         := $context["fill"] = color;
	(* thickness etc *)
]

(**************************************************************************************************)

svg[list_List] := freshContext @ StringRiffle[Map[parseListElem, list] /. Null -> "", "\n"]

parseListElem[e_] := OnFailed[parseContext @ e, svg @ e, Nothing];

(**************************************************************************************************)

ToSVGString::unkstyle = "Unknown style directive ``."

parseStyleElem[e_] := OnFailed[parseContext @ e, svgFail["unkstyle", e]];

svg[Style[prim_, specs___]] := freshContext[
	Scan[parseStyleElem, {specs}];
	svg[prim];
];

(**************************************************************************************************)

svgMap[f_, coords_] := StringRiffle[Map[svg[f[#]]&, coords], "\n"];

(**************************************************************************************************)

$circleTemplate = StringTemplate @ """<circle cx="`1`" cy="`2`" r="`3`"/>""";

svg[Disk[{x_ ? nq, y_ ? nq}, r_:1]] := $circleTemplate[x, -y, r];

svg[Disk[coords_ ? cmq, r_:1]] := svgMap[Disk[#, r]&, coords];

************************************************************************************************

$rectangleTemplate = StringTemplate @ """<rect x="`1`" y="`2`" width="`3`" height="`4`" fill="`5`" stroke="`6`"/>""";

svg[Rectangle[{x1_ ? nq, y1_ ? nq}, {x2_ ? nq, y2_ ? nq}]] := $rectangleTemplate[x1, -y1, x2 - x1, y2 - y1, toColStr @ $context["fill"], toColStr @ $context["stroke"]];

makeBackgroundRect[color_] := $rectangleTemplate[xmin, ymin, xsize, ysize, toColStr @ color, "none"];

(**************************************************************************************************)

$polygonTemplate = StringTemplate @ """<polygon points="`1`" fill="`2` stroke="`3` stroke-width="`4`" "/>""";

svg[Polygon[points_ ? cmq]] := $polygonTemplate[pairList[points], toColStr @ $context["fill"], toColStr @ $context["stroke"], $context["thickness"]];

svg[Polygon[coords_ ? caq]] := svgMap[Polygon, coords];

(**************************************************************************************************)

$polylineTemplate = StringTemplate @ """<polyline points="`1`" fill="none" stroke="`2`" stroke-width="`3`" />""";

svg[Line[points_ ? cmq]] := $polylineTemplate[pairList[points], toColStr @ $context["stroke"], $context["thickness"]];

$lineTemplate = StringTemplate @ """<line x1="`1`" y1="`2`" x2="`3`" y2="`4`" stroke="`3`" />""";

svg[Line[{{x1_, y1_}, {x2_, y2_}} ? cmq]] := $lineTemplate[x1, -y1, x2, -y2, toColStr @ $context["stroke"]];

svg[Line[coords_ ? caq]] := svgMap[Line, coords];

(**************************************************************************************************)

svg[Point[{x_ ? nq, y_ ? nq}]] := $circleTemplate[x, -y, $context["pointsize"] / 2];

svg[Point[coords_ ? cmq]] := svgMap[Point, coords];

psToCoords[ps_] := ps * xsize;
apsToCoords[ps_] := ps / width * xsize;

(**************************************************************************************************)

(* TODO:
* create a strokestyle string that encapsulates stroke, thickness, opacity, etc, and gets inserted
in all the right places

* use named styles to avoid repeating styles again and again, or maybe use a style element?

* support Inset

* support Text

* support VertexColors

* support animation

* render Arrowheads, figure out how to put them on curves

* implement a higher-level Arrow graphics API
*)


(**************************************************************************************************)

$pathTemplate = StringTemplate @ """<path d="`1`" fill="none" stroke="`2`" stroke-width="`3`"/>""";

svg[BezierCurve[pnts_ ? cmq]] := $pathTemplate[bezierToPath @ pnts, toColStr @ $context["stroke"], $context["thickness"]];

bezierToPath[{a_, b_, c_, d_}] := StringJoin[
  "M", pair @ a, " Q", pairList @ {b, (b + c)/2}, " T", pair @ d
];

(* bezierToPath[pnts_] := Scope[
  {p1, p2, pn} = Part[pnts, {1, 2, -1}];
  pms = MapWindowed[Mean, Take[pnts, {2, -2}]];
  pairs = {" S", pairList[{#1, (#1 + #2)/2}]}& @@@ Partition[pnts, 2, 2];
  StringJoin["M", pair @ p1, " Q", pairList[p2, p3], pairs]
]
*)
(* bezierToPath[pnts_] := Scope[
  {first, last} = FirstLast @ pnts;
  next = Part[pnts, 2];
  pairs = {" S", pairList[{#1, (#1 + #2)/2}]}& @@@ Partition[pnts, 2, 2];
  StringJoin[{"M", pair @ first}, pairs]
]
 *)
(**************************************************************************************************)

ToSVGString::unkprim = "Unknown primitive ``."

svg[other_] := (svgFail["unkprim", other]; "");

(**************************************************************************************************)

pair[{x_, y_}] := StringRiffle[{x, -y}, " "];
pairList[points_] := StringRiffle[MapColumn[Minus, 2, points], " ", ","];

toColStr = Case[
	Black 				:= "black";
	White 				:= "white";
	c_ ? ColorQ 		:= Set[toColStr[c], "#" <> ColorHexString[c]];
];

 *)