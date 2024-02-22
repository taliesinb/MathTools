toExportNotebook[expr_, background_] := Scope[
	nbOrCell = toExportCell[expr, Background -> background];
	(* fast path: custom stylesheet makes rasterization substantially slower. *)
	If[containsQGTemplates[nbOrCell], AppTo[nbOrCell, StyleDefinitions -> $styleDefsFileName]];
	nbOrCell
];

$styleDefsFileName := $styleDefsFileName = Construct[FileName, {}, $LightStylesheetPath, CharacterEncoding -> "UTF-8"];

containsQGTemplates[expr_] := Or[
	!FreeQ[expr, TemplateBox[_, $qgTemplateBoxP]],
	!FreeQ[expr, s_Str /; SContainsQ[s, "StyleDefinitions"]] (* not sure what this second thing is for *)
]

(* TODO: just save these names as we populate them, rather than loading them from disk *)
$qgTemplateBoxP := $qgTemplateBoxP = Apply[Alt, QuiverGeometryStyleNames[]];

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

PublicFunction[MakeImage]

SetUsage @ "
MakeImage[expr$] is like %Rasterize, but faster.
MakeImage[expr$, background$] specifies a background color.
* the QG stylesheet will be used if QG template boxes are present.
"

CacheVariable[$MakeImageCache]

MakeImage::fail = "Failed to rasterize expression ``.";
MakeImage[expr_, bg_:None] := Scope @ CachedInto[
	$MakeImageCache, Hash @ {expr, bg},
	res = CallFrontEnd @ toExportPacket[expr, bg];
	If[ImageQ[res], res,
		Message[MakeImage::fail, MsgExpr @ expr];
		Return @ ConstantImage[Pink, {10, 10}]
	]
];

(*************************************************************************************************)

PublicFunction[MakeImageAndMetadata]

SetUsage @ "
MakeImageAndMetadata[expr$] is like %MakeImage but returns a tuple containing the image and various metadata.
MakeImageAndMetadata[expr$, background$] specifies a background color.
* the tuple is {image$, size$, bounds$, regions$}.
"


CacheVariable[$MakeImageAndMetadataCache]

MakeImageAndMetadata[expr_, bg_:None] := Scope @ CachedInto[
	$MakeImageAndMetadataCache, Hash @ {expr, bg},
	res = CallFrontEnd @ toExportPacket[expr, bg, True];
	If[!MatchQ[res, {_Image, {__Rule}}],
		Message[MakeImage::fail, MsgExpr @ expr];
		Return @ {ConstantImage[Pink, {10, 10}], {10, 10, 1}, {}};
	];
	{image, metadata} = res;
	{boundingBox, baseline, regions} = Lookup[metadata, {"BoundingBox", Baseline, "Regions"}];
	rasterSize = bboxToRasterSize[{Transpose @ boundingBox, baseline}];
	regions = Map[toImageReg, regions];
	{image, rasterSize, boundingBox, regions}
];

toImageReg[{anno_, {{a_, b_}, {c_, d_}}}] := anno -> {{a, d}, {b, c}};

(*************************************************************************************************)

PublicFunction[MakeImageSize]

CacheVariable[$MakeImageSizeCache]

MakeImageSize::fail = "Failed to obtain raster size for input with head ``.";
MakeImageSize[expr_, returnBaseline_:False] :=
	If[returnBaseline, Id, TakeOperator[2]] @ iMakeImageSize[expr];

iMakeImageSize[expr_] := Scope @ CachedInto[
	$MakeImageSizeCache, Hash @ expr,
	res = CallFrontEnd @ Insert[toExportPacket[expr, None], "BoundingBox", 2];
	bbox = bboxToRasterSize @ res;
	If[FailureQ[bbox],
		Message[MakeImageSize::fail, MsgExpr @ expr];
		Return @ {10, 10, 0}
	];
	bbox
];

bboxToRasterSize[other_] := $Failed;
bboxToRasterSize[{{{x1_, x2_}, {y1_, y2_}}, d_}] := Scope[
 	{w, h} = Floor[({y1 - x1, y2 - x2} * 2) + 1 / 2];
 	d = Floor[d * 2 + 1/2];
 	{w, h, d}
 ]

(**************************************************************************************************)

PublicFunction[ErrorImageQ]

ErrorImageQ[image_] := Count[Catenate @ ImageData[image, "Byte"], {255, 242, 242} | {255, 89, 89, 20}] > 50;

(**************************************************************************************************)

PublicFunction[MakeTextImage]

MakeTextImage[Text[content_, Shortest[___], opts___Rule]] :=
	MakeImage[styleAsText[content, opts]];

(*************************************************************************************************)

PublicFunction[MakeTextImageSize]

MakeTextImageSize[Text[content_, Shortest[___], opts___Rule], returnBaseline_:False] :=
  MakeImageSize[styleAsText[content, opts], returnBaseline] / 2;

(*************************************************************************************************)

PublicFunction[MakeTextImageAndMetadata]

MakeTextImageAndMetadata[Text[content_, Shortest[___], opts___Rule]] :=
  MakeImageAndMetadata[styleAsText[content, opts]];

(*************************************************************************************************)

(* TODO: remove this form modifier thing *)
styleAsText[a_, l___] := Style[applyCurrentFormModifiers @ a, "Graphics", l];
styleAsText[a_, l___, BaseStyle -> s_, r___] := Style[applyCurrentFormModifiers @ a, "Graphics", Sequence @@ ToList @ s, l, r];

(*************************************************************************************************)

PublicFunction[MakeImageList]

MakeImageList[expr_List] := CallFrontEnd @ Map[toExportPacket, expr];

(*************************************************************************************************)

PublicFunction[MakeImageListCenterPadded]

MakeImageListCenterPadded[expr_List] := CenterPadImages @ MakeImageList[expr];

(*************************************************************************************************)

PublicFunction[VideoRasterizeList]

toCachedVideoFileName[File[path_]] := path;
toCachedVideoFileName[name_] := CacheVideoFilePath[name];
toCachedVideoFileName[{name__}] := CacheVideoFilePath[name];

PrivateFunction[CacheVideoFilePath]

CacheVideoFilePath[args___] := CacheFilePath["Video", args, FileExtension -> $DefaultVideoExtension];

$tempID = 0;
VideoRasterizeList[frames_, name_:Auto, frameRate_:30] := Scope[
    SetAutomatic[name, {"temp", $ProcessID, $tempID++}];
    path = toCachedVideoFileName[name];
    If[FileExistsQ[path], Return @ Video[path]];
	Export[path, frames, FrameRate -> frameRate, VideoEncoding -> $DefaultVideoEncoding];
	Video[path]
];

(*************************************************************************************************)

PrivateFunction[VideoFilePath]

VideoFilePath[HoldP @ v_Video] := F @ Information[v, "ResourcePath"]

(*************************************************************************************************)

PublicVariable[$DefaultVideoExtension, $DefaultVideoEncoding]

$DefaultVideoExtension = "mp4"
$DefaultVideoEncoding = "H264-AVF";

(**************************************************************************************************)

PublicFunction[CenterPadImages]

CenterPadImages[images_List] := Scope[
  If[!VecQ[images, ImageQ], ReturnFailed[]];
  ConformImages[images, {Max, Max}, "Pad", Padding -> White]
];
