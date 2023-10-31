toExportNotebook[expr_, background_] := Scope[
	nbOrCell = toExportCell[expr, Background -> background];
	(* fast path: custom stylesheet makes rasterization substantially slower. *)
	If[containsQGTemplates[nbOrCell], AppendTo[nbOrCell, StyleDefinitions -> $styleDefsFileName]];
	nbOrCell
];

$styleDefsFileName := $styleDefsFileName = Construct[FileName, {}, $LightStylesheetPath, CharacterEncoding -> "UTF-8"];

containsQGTemplates[expr_] := Or[
	!FreeQ[expr, TemplateBox[_, $qgTemplateBoxP]],
	!FreeQ[expr, s_String /; StringContainsQ[s, "StyleDefinitions"]] (* not sure what this second thing is for *)
]

(* TODO: just save these names as we populate them, rather than loading them from disk *)
$qgTemplateBoxP := $qgTemplateBoxP = Apply[Alternatives, QuiverGeometryStyleNames[]];

(*************************************************************************************************)

$exportCellOptions = Sequence[
	ShowCellBracket -> False, CellMargins -> 0,
	CellFrame -> None, CellFrameMargins -> 0, CellContext -> "Global`",
	GraphicsBoxOptions -> {ImageSize -> Medium}, Graphics3DBoxOptions -> {ImageSize -> Medium}
];

toExportCell[c:(Cell[CellGroupData[cells_, ___], ___] | CellGroupData[cells_, ___]), opts___] :=
	Notebook[List @ c, opts, ShowCellBracket -> False, CellContext -> "Global`"];

toExportCell[Cell[args___], opts___] :=
	Cell[args, $exportCellOptions, opts];

toExportCell[expr_, opts___] := Cell[
	BoxData @ ToBoxes @ expr, "Output",
	$exportCellOptions, opts
];

(*************************************************************************************************)

toExportPacket[expr_] := toExportPacket[expr, None];

toExportPacket[expr_, background_, verbose_:False] := ExportPacket[
	toExportNotebook[expr, background],
	"ImageObjectPacket", ColorSpace -> RGBColor, Verbose -> verbose, "AlphaChannel" -> False,
	"DataCompression" -> False, ImageResolution -> 144
];

toExportPacket[expr_, Transparent, verbose_:False] := ExportPacket[
	toExportNotebook[expr, Transparent],
	"ImageObjectPacket", ColorSpace -> RGBColor, Verbose -> verbose, "AlphaChannel" -> True,
	"DataCompression" -> False, ImageResolution -> 144
];

(*************************************************************************************************)

PublicFunction[FastRasterize]

Options[FastRasterize] = {Background -> Automatic};

FastRasterize::fail = "Failed to rasterize input with head ``. Result printed below.";
FastRasterize[expr_, opts:OptionsPattern[]] := Scope[
	res = CallFrontEnd @ toExportPacket[expr, OptionValue[Background]];
	If[ImageQ[res], res,
		Message[FastRasterize::fail, Head @ expr];
		Print[res];
		ConstantImage[Pink, {10, 10}]
	]
];

(*************************************************************************************************)

PublicFunction[FastRasterizeWithMetadata]

Options[FastRasterizeWithMetadata] = {Background -> Automatic};

FastRasterizeWithMetadata[expr_, opts:OptionsPattern[]] := Scope[
	res = CallFrontEnd @ toExportPacket[expr, OptionValue[Background], True];
	If[!MatchQ[res, {_Image, {__Rule}}],
		Message[FastRasterize::fail, Head @ expr];
		Return @ {ConstantImage[Pink, {10, 10}], {10, 10, 1}, {}};
	];
	{image, metadata} = res;
	{boundingBox, baseline, regions} = Lookup[metadata, {"BoundingBox", Baseline, "Regions"}];
	rasterSize = bboxToRasterSize[{Transpose @ boundingBox, baseline}];
	regions = Map[toImageReg, regions];
	{image, rasterSize, regions}
];

toImageReg[{anno_, {{a_, b_}, {c_, d_}}}] := anno -> {{a, d}, {b, c}};

(*************************************************************************************************)

PublicFunction[FastRasterSize]

FastRasterSize::fail = "Failed to obtain raster size for input with head ``. Result printed below.";
FastRasterSize[expr_, returnBaseline_:False] := Scope[
	res = CallFrontEnd @ Insert[toExportPacket[expr, None], "BoundingBox", 2];
	If[!MatchQ[res, {{{_, _}, {_, _}}, _}],
		Message[FastRasterize::fail, Head @ expr];
		If[returnBaseline, {10, 10, 0}, {10, 10}]
	,
		size = bboxToRasterSize @ res;
		If[returnBaseline, size, Take[size, 2]]
 	]
 ];

 bboxToRasterSize[{{{x1_, x2_}, {y1_, y2_}}, d_}] := Scope[
 	{w, h} = Floor[({y1 - x1, y2 - x2} * 2) + 1 / 2];
 	d = Floor[d * 2 + 1/2];
 	{w, h, d}
 ]

(*************************************************************************************************)

PublicFunction[CachedFastRasterize]

CacheSymbol[$RasterizationCache]

CachedFastRasterize[expr_] := MaybeCacheTo[$RasterizationCache, Hash[expr], FastRasterize[expr]];

(*************************************************************************************************)

PublicFunction[FastRasterizeList]

FastRasterizeList[expr_List] := CallFrontEnd @ Map[toExportPacket, expr];

(*************************************************************************************************)

PublicFunction[FastRasterizeListCenterPadded]

FastRasterizeListCenterPadded[expr_List] := CenterPadImages @ FastRasterizeList[expr];
 
(*************************************************************************************************)

PublicFunction[CachedFastRasterizeList]

(* we might need to delete ImageSizeRaw here ... *)

CachedFastRasterizeList[expr_List] := MaybeCacheTo[$RasterizationCache, Hash[expr], FastRasterizeList[expr]];

(*************************************************************************************************)

PublicFunction[VideoRasterizeList]

toCachedVideoFileName[File[path_]] := path;
toCachedVideoFileName[name_] := CacheVideoFilePath[name];
toCachedVideoFileName[{name__}] := CacheVideoFilePath[name];

PrivateFunction[CacheVideoFilePath]

CacheVideoFilePath[args___] := CacheFilePath["Video", args, FileExtension -> $DefaultVideoExtension];

$tempID = 0;
VideoRasterizeList[frames_, name_:Automatic, frameRate_:30] := Scope[
    SetAutomatic[name, {"temp", $ProcessID, $tempID++}];
    path = toCachedVideoFileName[name];
    If[FileExistsQ[path], Return @ Video[path]];
	Export[path, frames, FrameRate -> frameRate, VideoEncoding -> $DefaultVideoEncoding];
	Video[path]
];

(*************************************************************************************************)

PrivateFunction[VideoFilePath]

VideoFilePath[HoldPattern @ v_Video] := First @ Information[v, "ResourcePath"]

(*************************************************************************************************)

PublicVariable[$DefaultVideoExtension, $DefaultVideoEncoding]

$DefaultVideoExtension = "mp4"
$DefaultVideoEncoding = "H264-AVF";

(**************************************************************************************************)

PublicFunction[CenterPadImages]

CenterPadImages[images_List] := Scope[
  If[!VectorQ[images, ImageQ], ReturnFailed[]];
  ConformImages[images, {Max, Max}, "Pad", Padding -> White]
];

(**************************************************************************************************)

PublicFunction[FloodFill]

CacheSymbol[$FloodFillCache, $RegionRasterizationCache, $TransparentRasterizationCache]

Options[FloodFill] = {
	"Sensitivity" -> 0.001
}

FloodFill[g_Graphics, rules___] := FloodFill[g -> g, rules];

FloodFill::errimg = "Rasterized graphics contains an error: ``.";
FloodFill[lhs:(Graphics[prims_, opts___] -> g2_Graphics), rules:{__Rule}, OptionsPattern[]] := Scope[
	UnpackOptions[sensitivity];
	baseline = Lookup[List @@ Rest[g2], BaselinePosition, Automatic];
	fullHash = Hash[{lhs, rules, sensitivity}];
	If[$CachingEnabled,
		result = Lookup[$FloodFillCache, fullHash];
		If[ImageQ[result], Return @ result];
	];
  len = Length @ rules;
  annos = Style[Annotation[Invisible @ Point[#1], #2, "FillPoint"], Antialiasing->None]& @@@ rules;
  annoGraphics = Graphics[{{AbsolutePointSize[1], annos}, prims}, opts];
  {image, bounds, regions} = cachedImageRegionRasterize @ annoGraphics;
  image2 = cachedTransparentRasterize @ g2;
  If[ErrorImageQ[image] || ErrorImageQ[image2], Message[FloodFill::errimg, lhs]; Return @ ConstantImage[0.5, {5,5}]];
  fillPoints = Cases[regions, ({color_, "FillPoint"} -> {{x1_, y1_}, {x2_, y2_}}) :> {Round @ {Avg[x1, x2], Avg[y1, y2]}, color}];
  bin = ColorNegate @ Binarize[ColorNegate @ image, sensitivity];
  comps = MorphologicalComponents[bin, 0.001];
  comps = Dilation[comps, 1];
  fillCoords = 2 * Part[fillPoints, All, 1];
  fillColors = ToRGB @ Part[fillPoints, All, 2];
  compValues = Extract[comps, Reverse[fillCoords, 2]];
  compRules = ToPackedReal /@ AssociationMap[v |-> (
    i = IndexOf[compValues, v];
    If[MissingQ[i], {0.,0.,0.,0.}, Append[Part[fillColors, i], 1.]]
  ), Range[0, Max[comps]]];
  compColors = ToPackedReal @ Map[compRules, comps, {2}];
  compImage = Image[compColors, "Real32", ColorSpace->"RGB"];
  {image, bin, fillPoints, compImage};
  result = ImageCompose[compImage, image2];
  result = Image[result, BaselinePosition -> baseline];
  If[$CachingEnabled, $FloodFillCache[fullHash] ^= result];
  result
];

(* this needs to be upgraded to ask for QG stylesheets !*)
cachedImageRegionRasterize[graphics_] :=
	MaybeCacheTo[
		$RegionRasterizationCache, Hash[graphics],
		FastRasterizeWithMetadata[graphics]
	];

cachedTransparentRasterize[graphics_] :=
	MaybeCacheTo[
		$TransparentRasterizationCache, Hash[graphics],
		FastRasterize[graphics, Background -> Transparent]
	];

(**************************************************************************************************)

PublicFunction[ErrorImageQ]

ErrorImageQ[image_] := Count[Catenate @ ImageData[image, "Byte"], {255, 242, 242} | {255, 89, 89, 20}] > 50;

(**************************************************************************************************)

PublicFunction[TextRasterize]

TextRasterize[Text[content_, Shortest[___], opts___Rule]] :=
	FastRasterize[styleAsText[content, opts]];

(**************************************************************************************************)

PublicFunction[TextRasterSize]

TextRasterSize[Text[content_, Shortest[___], opts___Rule], returnBaseline_:False] :=
  1 + CachedRasterSize[styleAsText[content, opts], returnBaseline] / 2;

TextRasterSize[_, returnBaseline_:False] := If[returnBaseline, {0, 0, 0}, {0, 0}];

styleAsText[a_, l___] := Style[applyCurrentFormModifiers @ a, "Graphics", l];
styleAsText[a_, l___, BaseStyle -> s_, r___] := Style[applyCurrentFormModifiers @ a, "Graphics", Sequence @@ ToList @ s, l, r];

(**************************************************************************************************)

PublicFunction[CachedRasterSize]

CacheSymbol[$RasterSizeCache]

CachedRasterSize[Null, returnBaseline_:False] := If[returnBaseline, {0, 0, 0}, {0, 0}];
CachedRasterSize[expr_, returnBaseline_:False] := MaybeCacheTo[
	$RasterSizeCache, {expr, returnBaseline},
	FastRasterSize[expr /. $rasterSizeFixupRules, returnBaseline]
];

$rasterSizeFixupRules = {
  Inverted[z_] :> z,
  (ImageSizeRaw -> _) :> Sequence[] (* <- we use ImageSizeRaw for internal purposes but it messes up rasterization *)
};