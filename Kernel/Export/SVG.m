PublicFunction[ToSVGString]

$width = $height = $xsize = $ysize = 100;
$xmin = $ymin = 0; $xmax = $ymax = 100;
$context = $styleNames = $styles = <||>;

$xpad = $ypad = 0;
ToSVGString[g_Graphics] := Scope[
	{{$xmin, $xmax}, {$ymin, $ymax}} = GraphicsPlotRange @ g;
	$xsize = ($xmax - $xmin + $xpad); $ysize = ($ymax - $ymin + $ypad);
  ratio = $ysize / $xsize;
	{$width, $height} = Round @ LookupImageSize[g, AspectRatio -> ratio];

	background = LookupOption[g, Background];

	prims = First @ g;
	$styleNames = <||>;
	$styles = <||>;

  Block[{$context = $context},
    parseContext[AbsoluteThickness[1]];
    parseContext[AbsolutePointSize[3]];
  	contents = StringJoin @ svg[prims];
  	If[ColorQ @ background, contents = StringJoin[makeBackgroundRect @ toColStr @background, contents]];
  ];

  If[$styles =!= <||>,
    entries = KeyValueMap[$styleEntryTemplate, $styles];
    contents = StringRiffle[Flatten @ {"<style>", entries, "</style>", contents}, "\n"];
  ];

	StringTrim @ $svgOuterTemplate @ Association[
		"width" -> $width + 1, "height" -> $height + 1,
    "xmin" -> -1, "ymin" -> -1, "xmax" -> $width + 2, "ymax" -> $height + 2,
		"contents" -> StringTrim[contents]
	]
];

psToPixels[ps_] := ps * $xsize;
apsToPixels[ps_] := ps;

toX[x_] := Round[(x - $xmin) / $xsize * $width];
toY[y_] := Round[($ymax - y) / $ysize * $height];
toP[r_] := Round[r / $xsize * $width];

$styleEntryTemplate = StringFunction @ """.#1 { #2 }"""

$svgOuterTemplate = StringFunction @ """
<svg width="#width" height="#height" viewBox="#xmin #ymin #xmax #ymax" xmlns="http://www.w3.org/2000/svg">
#contents
</svg>
"""

(**************************************************************************************************)

nq = NumericQ;
rmq = RealMatrixQ;
rvq = RealVectorQ;
cmq = CoordinateMatrixQ;
caq = CoordinateArrayQ;

(**************************************************************************************************)

SetHoldFirst @ freshContext;

freshContext[e_] := Block[{$context = $context}, e];

(**************************************************************************************************)

svgFail[msg_String, args___] := Message[MessageName[ToSVGString, msg], args];

(**************************************************************************************************)

parseContext = Case[
  color_ ? ColorQ                         := $context["fill"] = $context["pathcolor"] = toColStr @ color;
  Opacity[a_]                             := ($context["fill"] //= fromColStr /* SetColorOpacity[o] /* toColStr; $context["pathcolor"] //= fromColStr /* SetColorOpacity[o] /* toColStr);
  PointSize[ps_ ? nq]                     := $context["pointsize"] = psToPixels @ ps;
  AbsolutePointSize[ps_ ? nq]             := $context["pointsize"] = apsToPixels @ ps;
  EdgeForm[e_]                            := parseEdgeForm[e];
  FaceForm[e_]                            := parseFaceForm[e];
  Thickness[e_]                           := parseThickness[e];
  (Thickness|AbsoluteThickness)[s_Symbol] := % @ AbsoluteThickness @ Lookup[$thicknessRules, s];
  AbsoluteThickness[s_ ? nq]              := $context["paththickness"] = apsToPixels @ s;
  Thickness[s_ ? nq]                      := $context["paththickness"] = psToPixels @ s;
  d_Directive                             := Scan[%, d];
  other_                                  := $Failed;
];

$thicknessRules = <|Tiny -> 0.2, Small -> 0.5, Medium -> 1, Automatic -> 1, Large -> 2|>;

parseEdgeForm = Case[
  Opacity[o_, c_ ? ColorQ]                := % @ SetColorOpacity[c, o];
  (Thickness|AbsoluteThickness)[s_Symbol] := % @ AbsoluteThickness @ Lookup[$thicknessRules, s];
  AbsoluteThickness[s_ ? nq]              := $context["edgethickness"] = apsToPixels @ s;
  Thickness[s_ ? nq]                      := $context["edgethickness"] = psToPixels @ s;
  color_ ? ColorQ                         := $context["edgecolor"] = toColStr @ color;
  None | Transparent                      := $context["edgecolor"] = None;
  Opacity[a_]                             := $context["edgecolor"] //= fromColStr /* SetColorOpacity[o] /* toColStr;
  list_List                               := Scan[%, list];
	(* thickness etc *)
]
	
(* need to keep track of opacity separetly, and applyit to any colors that come after *)
parseFaceForm = Case[
	Opacity[o_, c_ ? ColorQ]         := % @ SetColorOpacity[c, o];
  Opacity[o_]                      := $context["fill"] //= fromColStr /* SetColorOpacity[o] /* toColStr;
	color_ ? ColorQ 				         := $context["fill"] = toColStr @ color;
  None | Transparent               := $context["fill"] = None;
  list_List                        := Scan[%, list];
]

Scan[parseContext, {
  AbsoluteThickness[1],
  AbsolutePointSize[3],
  Black,
  EdgeForm[None]
}];

(**************************************************************************************************)

svg[list_List] := freshContext @ StringRiffle[Map[parseListElem, list] /. Null -> "", "\n"]

parseListElem[e_] := OnFailed[parseContext @ e, svg @ e, Nothing];

(**************************************************************************************************)

ToSVGString::unkstyle = "Unknown style directive ``."

parseStyleElem[e_] := OnFailed[parseContext @ e, svgFail["unkstyle", e]];
parseStyleElem[StripOnInput -> _] := Null;

svg[Style[prim_, specs___]] := freshContext[
	Scan[parseStyleElem, {specs}];
	svg[prim]
];

(**************************************************************************************************)

parseStyleElem[FontSlant -> Italic|"Italic"]  := setContext[FontSlant -> "italic"];
parseStyleElem[FontSlant -> Normal|"Normal"]  := setContext[FontSlant -> "normal"];
parseStyleElem[FontWeight -> Bold|"Bold"]   := setContext[FontWeight -> "bold"];
parseStyleElem[FontWeight -> Normal|"Normal"] := setContext[FontWeight -> "normal"];
parseStyleElem[FontSize -> r_]       := setContext[FontSize -> TextString[r] <> "px"];
parseStyleElem[FontFamily -> f_]     := setContext[FontFamily -> f];

Scan[parseStyleElem, {
  FontSlant -> Normal,
  FontWeight -> Normal,
  FontSize -> 15,
  FontFamily -> "sans-serif"
}];

toFS[fsz_] := fsz / $width * $xsize;

svg[Inset[e:Except[_Graphics], pos_, ImageScaled[o:{_, _}]]] :=
  svg[Text[e, pos, o * 2 - 1]];

svg[Inset[e:Except[_Graphics], pos_]] :=
  svg[Text[e, pos]];

svg[Text[Style[content_, sargs___], targs___]] :=
  svg[Style[Text[content, targs], sargs]];

svg[Text[content_, {x_, y_}]] := svg[Text[content, {x, y}, {0, 0}]]

svg[_Arrowheads] := "";

svg[_Arrow] := "";

svg[Text[content_, {x_, y_}, {ax_, ay_}]] := Scope[
	style = cachedStyle[$textStyleTemplate] @ contextSeq[FontSlant, FontWeight, FontSize, FontFamily];
  content2 = content //. Column[a_, __] :> Column[a];
  anchor = Switch[clip[ax], -1, "start", 0, "middle", 1, "end"];
  baseline = Switch[clip[ay], -1, "text-after-edge", 0, "middle", 1, "text-before-edge"];
	$textTemplate[toX @ x, toY @ y, style, TextString @ content2, anchor, baseline]
];

clip[e_] := Clip @ Round @ e;

$fontReplacements = {"Source Sans Pro" -> "Menlo"};

(* TODO: HTML ESCAPE *)

$textTemplate = StringFunction @ """<text x="#1" y="#2" class="#3" text-anchor="#5" dominant-baseline="#6">#4</text>""";

$textStyleTemplate = StringFunction @ """font-style: #1; font-weight: #2; font-size: #3; font-family: #4""";

(**************************************************************************************************)

setContext[rules_] := AssociateTo[$context, rules];
contextSeq[keys___] := Sequence @@ Lookup[$context, {keys}, ""];

(**************************************************************************************************)

cachedStyle[f_][args___] := Scope[
	sname = Lookup[$styleNames, Key @ {args}];
	If[!MissingQ[sname], Return @ sname];
	sname = "st" <> (IntegerDigits[Length @ $styles] /. $digitRules);
	$styleNames[{args}] ^= sname;
	$styles[sname] ^= StringReplace[f[args], Repeated[" ", {2, Infinity}] -> " "];
	sname
];

$digitRules = RuleThread[Range[0,9], Characters @ "abcdefghij"];

(**************************************************************************************************)

svg[Inset[g_Graphics, pos_, ___]] := svg[Translate[g, pos]];

(**************************************************************************************************)

svg[Translate[g_, {x_, y_}]] :=  $translateTemplate[toX @ x, toY @ y, svg @ g];

$translateTemplate = StringFunction @ """<g transform="translate(#1 #2)">#3</g>""";

(**************************************************************************************************)

svg[Rotate[g_, theta_]] := svg[Rotate[g, theta, {0, 0}]];
svg[Rotate[g_, theta_, {x_, y_}]] :=  $rotateTemplate[theta, toX @ x, toY @ y, svg @ g];

$rotateTemplate = StringFunction @ """<g transform="rotate(#1 #2 #3)>#3</g>""";

(**************************************************************************************************)

svg[Rotate[g_, theta_]] := svg[Rotate[g, theta, {0, 0}]];
svg[Rotate[g_, theta_, {x_, y_}]] :=  $rotateTemplate[theta, toX @ x, toY @ y, svg @ g];

$rotateTemplate = StringFunction @ """<g transform="rotate(#1 #2 #3)>#3</g>""";

(**************************************************************************************************)

svg[GeometricTransformation[g_, m:{{_, _}, {_, _}} ? rmq]] = svg[GeometricTransformation[g, {m, {0, 0}}]];
svg[GeometricTransformation[g_, {{{a_, c_}, {b_, d_}} ? rmq, {e_, f_} ? rvq}]] := Scope[
  {e, f} = {toX @ e, toY @ f};
  $transformTemplate[theta, a, b, e, c, d, f];
];

$transformTemplate = StringFunction @ """<g transform="matrix(#2 #3 #4 #5 #6 #7)>#1</g>""";

(**************************************************************************************************)

svgMap[f_, coords_] := StringRiffle[Map[svg[f[#]]&, coords], "\n"];

(**************************************************************************************************)

(* TODO: factor fill, stroke, stroke-width *)
$circleTemplate = StringFunction @ """<circle fill="none" stroke="#4" stroke-width="#5" cx="#1" cy="#2" r="#3"/>""";

svg[Circle[{x_ ? nq, y_ ? nq}, r_:1]] := $circleTemplate[toX @ x, toY @ y, toP @ r, $context["pathcolor"], $context["paththickness"]];

svg[Circle[coords_ ? cmq, r_:1]] := svgMap[Circle[#, r]&, coords];

(**************************************************************************************************)

$diskTemplate = StringFunction @ """<circle fill="#4" stroke="#5" stroke-width="#6" cx="#1" cy="#2" r="#3"/>""";

svg[Disk[{x_ ? nq, y_ ? nq}, r_:1]] := $diskTemplate[toX @ x, toY @ y, r, $context["fill"], $context["edgecolor"], $context["edgethickness"]];

svg[Disk[coords_ ? cmq, r_:1]] := svgMap[Disk[#, r]&, coords];

(**************************************************************************************************)

$rectangleTemplate = StringFunction @ """<rect x="#1" y="#2" width="#3" height="#4" fill="#5" stroke="#6" stroke-width="#7"/>""";

svg[Rectangle[{x1_ ? nq, y1_ ? nq}, {x2_ ? nq, y2_ ? nq}]] :=
	$rectangleTemplate[rectSeq[x1, y1, x2, y2], $context["fill"], $context["edgecolor"], $context["edgethickness"]];

$roundedRectangleTemplate = StringFunction @ """<rect x="#1" y="#2" width="#3" height="#4" fill="#5" stroke="#6" stroke-width="#7" rx="#8"/>""";

svg[Rectangle[{x1_ ? nq, y1_ ? nq}, {x2_ ? nq, y2_ ? nq}, RoundingRadius -> r_]] :=
	$rectangleTemplate[rectSeq[x1, y1, x2, y2], $context["fill"], $context["edgecolor"], $context["edgethickness"], r];

rectSeq[x1_, y1_, x2_, y2_] := Seq[toX @ Min[x1, x2], toY @ Max[y1, y2], toP @ Abs[x2 - x1], toP @ Abs[y2 - y1]];
makeBackgroundRect[color_] := $rectangleTemplate[$xmin, $ymax, $xsize, $ysize, color, "none"];

(**************************************************************************************************)

$polygonTemplate = StringFunction @ """<polygon points="#1" fill="#2 stroke="#3 stroke-width="#4" "/>""";

svg[Polygon[points_ ? cmq]] := $polygonTemplate[pairList[points], $context["fill"], $context["edgecolor"], $context["edgethickness"]];

svg[Polygon[coords_ ? caq]] := svgMap[Polygon, coords];

(**************************************************************************************************)

$polylineTemplate = StringFunction @ """<polyline points="#1" fill="none" stroke="#2" stroke-width="#3"/>""";

svg[Line[points_ ? cmq]] := $polylineTemplate[pairList[points], $context["pathcolor"], $context["paththickness"]];

$lineTemplate = StringFunction @ """<line x1="#1" y1="#2" x2="#3" y2="#4" stroke="#5" stroke-width="#6"/>""";

svg[Line[{{x1_, y1_}, {x2_, y2_}} ? cmq]] := $lineTemplate[toX @ x1, toY @ y1, toX @ x2, toY @ y2, $context["pathcolor"], $context["paththickness"]];

svg[Line[coords_ ? caq]] := svgMap[Line, coords];

(**************************************************************************************************)

svg[Point[{x_ ? nq, y_ ? nq}]] := $circleTemplate[x, -y, $context["pointsize"] / 2];

svg[Point[coords_ ? cmq]] := svgMap[Point, coords];

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

$pathTemplate = StringFunction @ """<path d="#1" fill="none" stroke="#2" stroke-width="#3"/>""";

svg[BezierCurve[pnts_ ? cmq]] := $pathTemplate[bezierToPath @ pnts,  $context["pathcolor"], $context["paththickness"]];

bezierToPath[{a_, b_, c_, d_}] := StringJoin[
  "M", toPair @ a, " Q", toPairList @ {b, Avg[b, c]}, " T", toPair @ d
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

toPair[{x_, y_}] := StringJoin[IntegerString @ toX @ x, " ", IntegerString @ toY @ y];
toPairList[points_] := StringJoin @ Riffle[toPair /@ points, ","];

toColStr = Case[
  None          := "none";
	Black 				:= "black";
	White 				:= "white";
	c_ ? ColorQ   := Set[toColStr[c], toHexColor[c]];
];

toHexColor[e_] := toHexColor[e] = Image`Utilities`toHEXcolor[e];
fromHexColor[s_] := RGBColor @ Map[FromDigits[#, 16]/255.&, StringPartition[StringDrop[s, 1], 2]];

fromColStr = Case[
  "none"                            := None;
  "black"                           := Black;
  "white"                           := White;
  s_String /; StringStartsQ[s, "#"] := fromHexColor[s];
];

(**************************************************************************************************)

PublicFunction[LegacyToSVGString]

$vectorTemporaryFile := $vectorTemporaryFile = TemporaryPath["vector.svg"];

LegacyToSVGString[g_] := Scope[
  res = System`ConvertersDump`ExportVectorFormat[$vectorTemporaryFile, g, "SVG"];
  If[res === $vectorTemporaryFile, ReadString @ res, $Failed]
];

