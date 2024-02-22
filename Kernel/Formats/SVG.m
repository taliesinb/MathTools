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

	prims = F @ g;
	$styleNames = <||>;
	$styles = <||>;

  Block[{$context = $context},
    parseContext[AbsoluteThickness[1]];
    parseContext[AbsolutePointSize[3]];
  	contents = SJoin @ toSvg[prims];
  	If[ColorQ @ background, contents = SJoin[makeBackgroundRect @ HTMLColorString @ background, contents]];
  ];

  If[$styles =!= <||>,
    entries = KVMap[$styleEntryTemplate, $styles];
    contents = SRiffle[Flatten @ {"<style>", entries, "</style>", contents}, "\n"];
  ];

	STrim @ $svgOuterTemplate @ Assoc[
		"width" -> $width + 1, "height" -> $height + 1,
    "xmin" -> -1, "ymin" -> -1, "xmax" -> $width + 2, "ymax" -> $height + 2,
		"contents" -> STrim[contents]
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

svgFail[msg_Str, args___] := Message[MessageName[ToSVGString, msg], args];

(**************************************************************************************************)

SetHTMLColorOpacity[o_][col_] := HTMLColorString @ SetColorOpacity[o] @ FromHTMLColorString @ col;

parseContext = Case[
  color_ ? ColorQ                         := $context["fill"] = $context["pathcolor"] = HTMLColorString @ color;
  Opacity[a_]                             := ($context["fill"] //= SetHTMLColorOpacity[o]; $context["pathcolor"] //= SetHTMLColorOpacity[o];)
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

$thicknessRules = <|Tiny -> 0.2, Small -> 0.5, Medium -> 1, Auto -> 1, Large -> 2|>;

parseEdgeForm = Case[
  Opacity[o_, c_ ? ColorQ]                := % @ SetColorOpacity[c, o];
  (Thickness|AbsoluteThickness)[s_Symbol] := % @ AbsoluteThickness @ Lookup[$thicknessRules, s];
  AbsoluteThickness[s_ ? nq]              := $context["edgethickness"] = apsToPixels @ s;
  Thickness[s_ ? nq]                      := $context["edgethickness"] = psToPixels @ s;
  color_ ? ColorQ                         := $context["edgecolor"] = HTMLColorString @ color;
  None | Transparent                      := $context["edgecolor"] = None;
  Opacity[a_]                             := $context["edgecolor"] //= SetHTMLColorOpacity[o];
  list_List                               := Scan[%, list];
  (* thickness etc *)
]

(* need to keep track of opacity separetly, and applyit to any colors that come after *)
parseFaceForm = Case[
  Opacity[o_, c_ ? ColorQ]         := % @ SetColorOpacity[c, o];
  Opacity[o_]                      := $context["fill"] //= SetHTMLColorOpacity[o];
  color_ ? ColorQ                  := $context["fill"] = HTMLColorString @ color;
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

toSvg[list_List] := freshContext @ SRiffle[Map[parseListElem, list] /. Null -> "", "\n"]

parseListElem[e_] := OnFailed[parseContext @ e, toSvg @ e, Nothing];

(**************************************************************************************************)

ToSVGString::unkstyle = "Unknown style directive ``."

parseStyleElem[e_] := OnFailed[parseContext @ e, svgFail["unkstyle", e]];
parseStyleElem[StripOnInput -> _] := Null;

toSvg[Style[prim_, specs___]] := freshContext[
	Scan[parseStyleElem, {specs}];
	toSvg[prim]
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

toSvg[Inset[e:Except[_Graphics], pos_, ImageScaled[o:{_, _}]]] :=
  toSvg[Text[e, pos, o * 2 - 1]];

toSvg[Inset[e:Except[_Graphics], pos_]] :=
  toSvg[Text[e, pos]];

toSvg[Text[Style[content_, sargs___], targs___]] :=
  toSvg[Style[Text[content, targs], sargs]];

toSvg[Text[content_, {x_, y_}]] := toSvg[Text[content, {x, y}, {0, 0}]]

toSvg[_Arrowheads] := "";

toSvg[_Arrow] := "";

toSvg[Text[content_, {x_, y_}, {ax_, ay_}]] := Scope[
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
	sname = "st" <> (IntDigits[Len @ $styles] /. $digitRules);
	$styleNames[{args}] ^= sname;
	$styles[sname] ^= SRep[f[args], Repeated[" ", {2, Inf}] -> " "];
	sname
];

$digitRules = RuleThread[Range[0,9], Chars @ "abcdefghij"];

(**************************************************************************************************)

toSvg[Inset[g_Graphics, pos_, ___]] := toSvg[Translate[g, pos]];

(**************************************************************************************************)

toSvg[Translate[g_, {x_, y_}]] :=  $translateTemplate[toX @ x, toY @ y, toSvg @ g];

$translateTemplate = StringFunction @ """<g transform="translate(#1 #2)">#3</g>""";

(**************************************************************************************************)

toSvg[Rotate[g_, theta_]] := toSvg[Rotate[g, theta, {0, 0}]];
toSvg[Rotate[g_, theta_, {x_, y_}]] :=  $rotateTemplate[theta, toX @ x, toY @ y, toSvg @ g];

$rotateTemplate = StringFunction @ """<g transform="rotate(#1 #2 #3)>#3</g>""";

(**************************************************************************************************)

toSvg[Rotate[g_, theta_]] := toSvg[Rotate[g, theta, {0, 0}]];
toSvg[Rotate[g_, theta_, {x_, y_}]] :=  $rotateTemplate[theta, toX @ x, toY @ y, toSvg @ g];

$rotateTemplate = StringFunction @ """<g transform="rotate(#1 #2 #3)>#3</g>""";

(**************************************************************************************************)

toSvg[GeometricTransformation[g_, m:{{_, _}, {_, _}} ? rmq]] = toSvg[GeometricTransformation[g, {m, {0, 0}}]];
toSvg[GeometricTransformation[g_, {{{a_, c_}, {b_, d_}} ? rmq, {e_, f_} ? rvq}]] := Scope[
  {e, f} = {toX @ e, toY @ f};
  $transformTemplate[theta, a, b, e, c, d, f];
];

$transformTemplate = StringFunction @ """<g transform="matrix(#2 #3 #4 #5 #6 #7)>#1</g>""";

(**************************************************************************************************)

svgMap[f_, coords_] := SRiffle[Map[toSvg[f[#]]&, coords], "\n"];

(**************************************************************************************************)

(* TODO: factor fill, stroke, stroke-width *)
$circleTemplate = StringFunction @ """<circle fill="none" stroke="#4" stroke-width="#5" cx="#1" cy="#2" r="#3"/>""";

toSvg[Circle[{x_ ? nq, y_ ? nq}, r_:1]] := $circleTemplate[toX @ x, toY @ y, toP @ r, $context["pathcolor"], $context["paththickness"]];

toSvg[Circle[coords_ ? cmq, r_:1]] := svgMap[Circle[#, r]&, coords];

(**************************************************************************************************)

$diskTemplate = StringFunction @ """<circle fill="#4" stroke="#5" stroke-width="#6" cx="#1" cy="#2" r="#3"/>""";

toSvg[Disk[{x_ ? nq, y_ ? nq}, r_:1]] := $diskTemplate[toX @ x, toY @ y, r, $context["fill"], $context["edgecolor"], $context["edgethickness"]];

toSvg[Disk[coords_ ? cmq, r_:1]] := svgMap[Disk[#, r]&, coords];

(**************************************************************************************************)

$rectangleTemplate = StringFunction @ """<rect x="#1" y="#2" width="#3" height="#4" fill="#5" stroke="#6" stroke-width="#7"/>""";

toSvg[Rectangle[{x1_ ? nq, y1_ ? nq}, {x2_ ? nq, y2_ ? nq}]] :=
	$rectangleTemplate[rectSeq[x1, y1, x2, y2], $context["fill"], $context["edgecolor"], $context["edgethickness"]];

$roundedRectangleTemplate = StringFunction @ """<rect x="#1" y="#2" width="#3" height="#4" fill="#5" stroke="#6" stroke-width="#7" rx="#8"/>""";

toSvg[Rectangle[{x1_ ? nq, y1_ ? nq}, {x2_ ? nq, y2_ ? nq}, RoundingRadius -> r_]] :=
	$rectangleTemplate[rectSeq[x1, y1, x2, y2], $context["fill"], $context["edgecolor"], $context["edgethickness"], r];

rectSeq[x1_, y1_, x2_, y2_] := Seq[toX @ Min[x1, x2], toY @ Max[y1, y2], toP @ Abs[x2 - x1], toP @ Abs[y2 - y1]];
makeBackgroundRect[color_] := $rectangleTemplate[$xmin, $ymax, $xsize, $ysize, color, "none"];

(**************************************************************************************************)

$polygonTemplate = StringFunction @ """<polygon points="#1" fill="#2 stroke="#3 stroke-width="#4" "/>""";

toSvg[Polygon[points_ ? cmq]] := $polygonTemplate[pairList[points], $context["fill"], $context["edgecolor"], $context["edgethickness"]];

toSvg[Polygon[coords_ ? caq]] := svgMap[Polygon, coords];

(**************************************************************************************************)

$polylineTemplate = StringFunction @ """<polyline points="#1" fill="none" stroke="#2" stroke-width="#3"/>""";

toSvg[Line[points_ ? cmq]] := $polylineTemplate[pairList[points], $context["pathcolor"], $context["paththickness"]];

$lineTemplate = StringFunction @ """<line x1="#1" y1="#2" x2="#3" y2="#4" stroke="#5" stroke-width="#6"/>""";

toSvg[Line[{{x1_, y1_}, {x2_, y2_}} ? cmq]] := $lineTemplate[toX @ x1, toY @ y1, toX @ x2, toY @ y2, $context["pathcolor"], $context["paththickness"]];

toSvg[Line[coords_ ? caq]] := svgMap[Line, coords];

(**************************************************************************************************)

toSvg[Point[{x_ ? nq, y_ ? nq}]] := $circleTemplate[x, -y, $context["pointsize"] / 2];

toSvg[Point[coords_ ? cmq]] := svgMap[Point, coords];

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

toSvg[BezierCurve[pnts_ ? cmq]] := $pathTemplate[bezierToPath @ pnts,  $context["pathcolor"], $context["paththickness"]];

bezierToPath[{a_, b_, c_, d_}] := SJoin[
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

toSvg[other_] := (svgFail["unkprim", other]; "");

(**************************************************************************************************)

toPair[{x_, y_}] := SJoin[IntStr @ toX @ x, " ", IntStr @ toY @ y];
toPairList[points_] := SJoin @ Riffle[toPair /@ points, ","];

(**************************************************************************************************)

PublicFunction[LegacyToSVGString]

$vectorTemporaryFile := $vectorTemporaryFile = TemporaryPath["vector.svg"];

LegacyToSVGString[g_] := Scope[
  res = System`ConvertersDump`ExportVectorFormat[$vectorTemporaryFile, g, "SVG"];
  If[res === $vectorTemporaryFile, ReadString @ res, $Failed]
];

(**************************************************************************************************)

PublicIOFunction[ExportSVG]

ExportSVG[file_Str, graphics_] := Scope[
  svg = ToSVGString @ graphics;
  If[!StrQ[svg], ReturnFailed[]];
  ExportUTF8[file, svg]
];
