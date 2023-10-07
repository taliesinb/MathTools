toExportNotebook[expr_, background_] :=
	toExportCell[expr, Background -> background, StyleDefinitions -> $QGStyleDefs];

(* we do this because custom stylesheet makes rasterization substantially slower. TODO: also check for known TemplateBox string names! *)
toExportNotebook[expr_ ? DoesNotRequireQGStylesQ, background_] :=
	toExportCell[expr, Background -> background];
	
$qgTemplateBoxP := $qgTemplateBoxP = Apply[Alternatives, QuiverGeometryStyleNames[]];

$exportCellOptions = Sequence[
	ShowCellBracket -> False, CellMargins -> 0,
	CellFrame -> None, CellFrameMargins -> 0, CellContext -> "Global`",
	GraphicsBoxOptions -> {ImageSize -> Medium}, Graphics3DBoxOptions -> {ImageSize -> Medium}
];

(* toExportNotebook[Cell[CellGroupData[cells_, ___], ___] | CellGroupData[cells_, ___], background_] :=
	Notebook
 *)

toExportCell[c:(Cell[CellGroupData[cells_, ___], ___] | CellGroupData[cells_, ___]), opts___] :=
	Notebook[List @ c, opts, ShowCellBracket -> False, CellContext -> "Global`"];

toExportCell[Cell[args___], opts___] :=
	Cell[args, $exportCellOptions, opts];

toExportCell[expr_, opts___] := Cell[
	BoxData @ ToBoxes @ expr, "Output",
	$exportCellOptions, opts
];

DoesNotRequireQGStylesQ[expr_] :=
	FreeQ[expr, _Symbol ? QuiverGeometrySymbolQ] && FreeQ[expr, TemplateBox[_, $qgTemplateBoxP]] && FreeQ[expr, s_String /; StringContainsQ[s, "StyleDefinitions"]];

$QGStyleDefs := $QGStyleDefs = With[{path = $QuiverGeometryStylesheetPath}, FileName[{}, path, CharacterEncoding -> "UTF-8"]];

SetHoldAll[QuiverGeometrySymbolQ]
QuiverGeometrySymbolQ[StyleDefinitions] := True;
QuiverGeometrySymbolQ[s_Symbol] := StringStartsQ[Context[s], "QuiverGeometry`"];

toExportPacket[expr_] := toExportPacket[expr, None];

toExportPacket[expr_, background_] := ExportPacket[
	toExportNotebook[expr, background],
	"ImageObjectPacket", ColorSpace -> RGBColor, Verbose -> False, "AlphaChannel" -> False,
	"DataCompression" -> False, ImageResolution -> 144
];

toExportPacket[expr_, Transparent] := ExportPacket[
	toExportNotebook[expr, Transparent],
	"ImageObjectPacket", ColorSpace -> RGBColor, Verbose -> False, "AlphaChannel" -> True,
	"DataCompression" -> False, ImageResolution -> 144
];

(*************************************************************************************************)

PublicFunction[FastRasterize]

Options[FastRasterize] = {Background -> Automatic};

FastRasterize::fail = "Failed to rasterize input with head ``. Result printed below.";
FastRasterize[expr_, OptionsPattern[]] := Scope[
	res = MathLink`CallFrontEnd @ toExportPacket[expr, OptionValue[Background]];
	If[!ImageQ[res], Message[FastRasterize::fail, Head @ expr]; Print[res]; ConstantImage[Pink, {10, 10}], res]
];

(*************************************************************************************************)

PublicFunction[FastRasterSize]

FastRasterSize::fail = "Failed to obtain raster size for input with head ``. Result printed below.";
FastRasterSize[expr_, returnBaseline_:False] := Scope[
	res = MathLink`CallFrontEnd @ Insert[toExportPacket[expr, None], "BoundingBox", 2];
	If[!MatchQ[res, {{{_, _}, {_, _}}, _}],
		Message[FastRasterize::fail, Head @ expr];
		If[returnBaseline, {1, 1, 0}, {1, 1}]
	,
		{{{x1, x2}, {y1, y2}}, d} = res;
		{w, h} = Floor[({y1 - x1, y2 - x2} * 144 / 72) + 1 / 2];
		If[returnBaseline, {w, h, Floor[d + 1/2]}, {w, h}]
 	]
 ];

(*************************************************************************************************)

PublicFunction[ClearRasterizationCache]

ClearRasterizationCache[] := (
	QuiverGeometryCaches`$GradientRasterizationCache = UAssociation[];
	QuiverGeometryCaches`$RasterSizeCache = UAssociation[];
	QuiverGeometryCaches`$RasterizationCache = UAssociation[];
	QuiverGeometryCaches`$RegionRasterizationCache = UAssociation[];
	QuiverGeometryCaches`$TransparentRasterizationCache = UAssociation[];
	QuiverGeometryCaches`$Base64RasterizationCache = UAssociation[];
	QuiverGeometryCaches`$TextureBoxCache = UAssociation[];
	clearFloodFillCaches[];
)

If[!AssociationQ[QuiverGeometryCaches`$GradientRasterizationCache],
	ClearRasterizationCache[];
];

(*************************************************************************************************)

PublicFunction[CachedFastRasterize]

CachedFastRasterize[expr_] := CacheTo[QuiverGeometryCaches`$RasterizationCache, Hash[expr], FastRasterize[expr]];

(*************************************************************************************************)

PublicFunction[FastRasterizeList]

FastRasterizeList[expr_List] := MathLink`CallFrontEnd @ Map[toExportPacket, expr];

(*************************************************************************************************)

PublicFunction[FastRasterizeListCenterPadded]

FastRasterizeListCenterPadded[expr_List] := CenterPadImages @ FastRasterizeList[expr];
 
(*************************************************************************************************)

PublicFunction[CachedFastRasterizeList]

(* we might need to delete ImageSizeRaw here ... *)

If[!AssociationQ[QuiverGeometryCaches`$RasterizationCache],
	QuiverGeometryCaches`$RasterizationCache = UAssociation[]];

CachedFastRasterizeList[expr_List] := CacheTo[QuiverGeometryCaches`$RasterizationCache, Hash[expr], FastRasterizeList[expr]];

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

VideoFilePath[v_Video] := First @ Information[v, "ResourcePath"]

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

clearFloodFillCaches[] := (
	QuiverGeometryCaches`$FloodFillHash = UAssociation[];
	QuiverGeometryCaches`$RegionRasterizationCache = UAssociation[];
	QuiverGeometryCaches`$TransparentRasterizationCache = UAssociation[];
)

If[!AssociationQ[QuiverGeometryCaches`$FloodFillHash] || Length[QuiverGeometryCaches`$FloodFillHash] > 32,
	clearFloodFillCaches[];
];

Options[FloodFill] = {
	"Sensitivity" -> 0.001
}

FloodFill[g_Graphics, rules___] := FloodFill[g -> g, rules];

FloodFill::errimg = "Rasterized graphics contains an error: ``.";
FloodFill[lhs:(Graphics[prims_, opts___] -> g2_Graphics), rules:{__Rule}, OptionsPattern[]] := Scope[
	UnpackOptions[sensitivity];
	baseline = Lookup[Options[g2], BaselinePosition, Automatic];
	fullHash = Hash[{lhs, rules, sensitivity}];
	result = Lookup[QuiverGeometryCaches`$FloodFillHash, fullHash];
	If[ImageQ[result], Return @ result];
  len = Length @ rules;
  annos = Style[Annotation[Invisible @ Point[#1], #2, "FillPoint"], Antialiasing->None]& @@@ rules;
  annoGraphics = Graphics[{{AbsolutePointSize[1], annos}, prims}, opts];
  {image, regions} = cachedImageRegionRasterize @ annoGraphics;
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
  QuiverGeometryCaches`$FloodFillHash[fullHash] ^= result;
  result
];

cachedImageRegionRasterize[graphics_] :=
	CacheTo[QuiverGeometryCaches`$RegionRasterizationCache, Hash[graphics], Rasterize[graphics, {"Image", "Regions"}]];

cachedTransparentRasterize[graphics_] :=
	CacheTo[QuiverGeometryCaches`$TransparentRasterizationCache, Hash[graphics], Rasterize[graphics, Background -> Transparent]];

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

CachedRasterSize[Null, returnBaseline_:False] := If[returnBaseline, {0, 0, 0}, {0, 0}];
CachedRasterSize[expr_, returnBaseline_:False] := CacheTo[
	QuiverGeometryCaches`$RasterSizeCache, {expr, returnBaseline},
	FastRasterSize[expr /. $rasterSizeFixupRules, returnBaseline]
];

$rasterSizeFixupRules = {
  Inverted[z_] :> z,
  (ImageSizeRaw -> _) :> Sequence[] (* <- we use ImageSizeRaw for internal purposes but it messes up rasterization *)
};