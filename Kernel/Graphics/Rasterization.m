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


