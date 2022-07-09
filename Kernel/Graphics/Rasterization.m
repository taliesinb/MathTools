exprToCell[g_] := exprToCell[g, Medium];
exprToCell[expr_, isize_] := Cell[
	BoxData[ToBoxes[expr]],
	"Output", ShowCellBracket -> False, Background -> Automatic, CellMargins -> 0,
	CellFrame -> None, CellFrameMargins -> 0, CellContext -> "Global`",
	GraphicsBoxOptions -> {ImageSize -> isize}, Graphics3DBoxOptions -> {ImageSize -> Medium},
	StyleDefinitions -> FrontEnd`FileName[{}, $QuiverGeometryStylesheetPath, CharacterEncoding -> "UTF-8"]
];

cellToExportPacket[cell_] := ExportPacket[
	cell,
	"BitmapPacket", ColorSpace -> RGBColor, Verbose -> False, "AlphaChannel" -> False,
	"DataCompression" -> True, ImageResolution -> 144
];

exprToExportPacket[expr_] := cellToExportPacket[exprToCell[expr]];

resultToImage[System`ConvertersDump`Bitmap[rawString_, {width_, height_, depth_}, ___]] := Scope[
	bytes = NumericArray[Developer`RawUncompress @ rawString, "Byte"];
	Internal`ArrayReshapeTo[bytes, {height, width, depth}];
	Image[Image`ReverseNumericArray[bytes, False], Interleaving -> True, Magnification -> 0.5]
]

(*************************************************************************************************)

PublicFunction[FastRasterize]

FastRasterize[expr_] := resultToImage @ MathLink`CallFrontEnd @ exprToExportPacket @ expr;

(*************************************************************************************************)

PublicFunction[FastRasterizeList]

FastRasterizeList[expr_List] := Map[resultToImage, MathLink`CallFrontEnd @ Map[exprToExportPacket, expr]];

(*************************************************************************************************)

PublicFunction[FastRasterizeListCenterPadded]

FastRasterizeListCenterPadded[expr_List] = CenterPadImages @ FastRasterizeList[expr];

(*************************************************************************************************)

PublicFunction[CachedFastRasterizeList]

(* we might need to delete ImageSizeRaw here ... *)

$RasterizationCache = <||>;
CachedFastRasterizeList[expr_List] := CacheTo[$RasterizationCache, Hash[expr], FastRasterizeList[expr]];

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


