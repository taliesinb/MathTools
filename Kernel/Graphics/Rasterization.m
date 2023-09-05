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
	If[!ImageQ[res], Message[FastRasterize::fail, Head @ expr]; Print[expr]; ConstantImage[Pink, {10, 10}], res]
];

(*************************************************************************************************)

PublicFunction[ClearRasterizationCache]

ClearRasterizationCache[] := (
	QuiverGeometryLoader`$RasterizationCache = UAssociation[];
	QuiverGeometryLoader`$FloodFillHash = UAssociation[];
	QuiverGeometryLoader`$RegionRasterizationCache = UAssociation[];
	QuiverGeometryLoader`$TransparentRasterizationCache = UAssociation[];
	QuiverGeometryLoader`$Base64RasterizationCache = UAssociation[];
)

(*************************************************************************************************)

PublicFunction[CachedFastRasterize]

CachedFastRasterize[expr_] := CacheTo[QuiverGeometryLoader`$RasterizationCache, Hash[expr], FastRasterize[expr]];

(*************************************************************************************************)

PublicFunction[FastRasterizeList]

FastRasterizeList[expr_List] := MathLink`CallFrontEnd @ Map[toExportPacket, expr];

(*************************************************************************************************)

PublicFunction[FastRasterizeListCenterPadded]

FastRasterizeListCenterPadded[expr_List] := CenterPadImages @ FastRasterizeList[expr];
 
(*************************************************************************************************)

PublicFunction[CachedFastRasterizeList]

(* we might need to delete ImageSizeRaw here ... *)

If[!AssociationQ[QuiverGeometryLoader`$RasterizationCache],
	QuiverGeometryLoader`$RasterizationCache = UAssociation[]];

CachedFastRasterizeList[expr_List] := CacheTo[QuiverGeometryLoader`$RasterizationCache, Hash[expr], FastRasterizeList[expr]];

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

If[!AssociationQ[QuiverGeometryLoader`$FloodFillHash],
	QuiverGeometryLoader`$FloodFillHash = UAssociation[];
	QuiverGeometryLoader`$RegionRasterizationCache = UAssociation[];
	QuiverGeometryLoader`$TransparentRasterizationCache = UAssociation[];
];

FloodFill[g_Graphics, rules_] := FloodFill[g -> g, rules];

FloodFill[lhs:(Graphics[prims_, opts___] -> g2_Graphics), rules:{__Rule}] := Scope[
	fullHash = Hash[{lhs, rules}];
	result = Lookup[QuiverGeometryLoader`$FloodFillHash, fullHash];
	If[ImageQ[result], Return @ result];
  len = Length @ rules;
  annos = Style[Annotation[Invisible @ Point[#1], #2, "FillPoint"], Antialiasing->None]& @@@ rules;
  annoGraphics = Graphics[{{AbsolutePointSize[1], annos}, prims}, opts];
  {image, regions} = cachedImageRegionRasterize @ annoGraphics;
  image2 = cachedTransparentRasterize @ g2;
  fillPoints = Cases[regions, ({color_, "FillPoint"} -> {{x1_, y1_}, {x2_, y2_}}) :> {Round @ {Avg[x1, x2], Avg[y1, y2]}, color}];
  bin = Binarize @ image;
  comps = MorphologicalComponents[bin, 0.01];
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
  QuiverGeometryLoader`$FloodFillHash[fullHash] ^= result;
  result
];

cachedImageRegionRasterize[graphics_] :=
	CacheTo[QuiverGeometryLoader`$RegionRasterizationCache, Hash[graphics], Rasterize[graphics, {"Image", "Regions"}]];

cachedTransparentRasterize[graphics_] :=
	CacheTo[QuiverGeometryLoader`$TransparentRasterizationCache, Hash[graphics], Rasterize[graphics, Background -> Transparent]];
