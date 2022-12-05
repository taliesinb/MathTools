PrivateFunction[cellToRasterMarkdown]

ToMarkdownString::badimgtemp = "Setting of FileImageTemplate (or StringImageTemplate) did not return a markdown string: ``.";
ToMarkdownString::badrastres = "Setting of RasterizationFunction did not return a valid association.";

cellToRasterMarkdown[cell_] := Scope[

  If[$rasterizationFunction === None, Return["#### Placeholder for image"]];

  thisTag = Nothing;
  cell = cell /. TagBox[contents_, "ClassTaggedForm"[tag_]] :> (thisTag = tag; contents);
  rasterizationResult = $rasterizationFunction @ cell;

  If[!AssociationQ[rasterizationResult],
    Message[ToMarkdownString::badrastres];
    Return["#### Invalid rasterization result"];
  ];

  rasterizationResult["classlist"] = StringRiffle[{"raster", thisTag}, " "];
  rasterizationResult["caption"] = Replace[$lastCaption, None -> ""];

  Switch[rasterizationResult["type"],
    "String",
      If[$stringImageTemplate === None, Print[MsgExpr[cell], rasterizationResult]];
      markdown = $stringImageTemplate @ rasterizationResult,
    "File",
      If[$fileImageTemplate === None, Print[MsgExpr[cell], rasterizationResult]];
      markdown = $fileImageTemplate @ rasterizationResult,
    _,
      Message[ToMarkdownString::badrastres];
      Return @ "#### Invalid rasterization result"
  ];

  If[!StringQ[markdown],
    Message[ToMarkdownString::badimgtemp, MsgExpr @ markdown];
    markdown = "#### Invalid image template result";
  ];

  markdown
];

(**************************************************************************************************)

PrivateFunction[linearSyntaxRasterizationFunction]

linearSyntaxRasterizationFunction[cell_] := Association[
  "type" -> "String",
  "linearSyntax" -> ToString[RawBoxes @ Part[cell, 1, 1], StandardForm]
];

(**************************************************************************************************)

PrivateFunction[base64RasterizationFunction]

If[!AssociationQ[QuiverGeometryLoader`$Base64RasterizationCache],
  QuiverGeometryLoader`$Base64RasterizationCache = UAssociation[]];

base64RasterizationFunction[type_, retina_][e_] := Scope[
  
  type //= ToUpperCase;
  hash = Hash[{e, type, retina}];
  result = Lookup[QuiverGeometryLoader`$Base64RasterizationCache, hash];
  If[AssociationQ[result], Return @ result];

  If[type === "PNG",
    If[Head[e] === Cell, e = Append[e, Antialiasing -> False], e = Style[e, Antialiasing -> False]];
  ];

  img = Rasterize[e, ImageResolution -> If[retina, 144, 72]];
  width = Round[First[ImageDimensions @ img] / If[retina, 2, 1]];
  encoded = ExportBase64[img, type, CompressionLevel -> If[type === "PNG", 1.0, 0.2]];
  result = Association[
    "type" -> "String",
    "width" -> width,
    "format" -> ToLowerCase[type],
    "encoded" -> encoded
  ];
  QuiverGeometryLoader`$Base64RasterizationCache[hash] ^= result;
  result
];

(**************************************************************************************************)

PrivateFunction[ImportBase64, ExportBase64]

ImportBase64[str_String, args___] := ImportString[FromCharacterCode @ FromBase64Digits @ str, args];
ExportBase64[img_Image, args___] := Base64String @ ToCharacterCode @ ExportString[img, args, IncludeMetaInformation -> False];

(**************************************************************************************************)

PrivateFunction[standardRasterizationFunction]

standardRasterizationFunction[Cell[BoxData[t:TagBox[_, _BoxForm`AnimatedImageTag]], ___]] :=
  standardRasterizationFunction @ ToExpression[t, StandardForm];

standardRasterizationFunction[a_AnimatedImage] :=
  cachedGenericRasterize[a, rasterizeAnimatedImage, "gif"];

rasterizeAnimatedImage[a_AnimatedImage] := {a, a["RasterSize"]};

standardRasterizationFunction[cell_] :=
  cachedGenericRasterize[cell, rasterizeImage, "png", CompressionLevel -> 1];

rasterizeImage[obj_] := Scope[
  img = If[TrueQ @ $rasterizationCaching, CachedFastRasterize, FastRasterize] @ obj;
  {img, ImageDimensions @ img}
]

(**************************************************************************************************)

PrivateFunction[jpegRasterizationFunction]

jpegRasterizationFunction[cell_] :=
  cachedGenericRasterize[cell, rasterizeImage, "jpeg", CompressionLevel -> 0.3];

(**************************************************************************************************)

$rasterMetadataCache = UAssociation[];

cachedGenericRasterize[obj_, rasterizeFn_, fileExt_, exportArgs___] := Scope[

  If[$rasterizationPath === None, Return @ None];

  (* did we already export this result in this session? *)
  objHash = Base36Hash @ obj;
  cacheKey = {objHash, $rasterizationURL, $rasterizationPath};
  cacheValue = Lookup[$rasterMetadataCache, Key @ cacheKey, None];
  If[ListQ[cacheValue] && $rasterizationCaching,
    (* this cache gives us enough info to generate the markdown without looking for the file on disk *)
    {imageDims, imageFileName, imagePath} = cacheValue;
    Goto[skipRasterization];
  ];

  whenWet @ EnsureDirectory @ $rasterizationPath;

  (* did we already export this result in a previous session? *)
  imagePath = First[FileNames[objHash <> "_*_*." <> fileExt, $rasterizationPath], None];
  If[StringQ[imagePath] && $rasterizationCaching,
    imageFileName = FileNameTake @ imagePath;
    imageDims = FromDigits /@ StringExtract[FileBaseName @ imagePath, "_" -> {3, 4}];
    Goto[skipRasterization]];

  (* rasterize *)
  If[$dryRun,
    imageFileName = StringJoin[objHash, "_dummy.", fileExt];
  ,
    result = rasterizeFn[obj];
    If[!MatchQ[result, {_, _List}], ReturnFailed[]];
    {image, imageDims} = result;
    imageHash = Base36Hash @ image;
    {w, h} = imageDims;
    VPrint["* Rasterization yielded image of size ", w, " by ", h];
    dimsStr = StringJoin[IntegerString[w, 10, 4], "_", IntegerString[h, 10, 4]];
    imageFileName = StringJoin[objHash, "_", imageHash, "_", dimsStr, ".", fileExt];
  ];

  imagePath = FileNameJoin[{$rasterizationPath, imageFileName}];

  (* export *)
  If[!FileExistsQ[imagePath] || !$rasterizationCaching,
    VPrint["* Exporting image to ", MsgPath @ imagePath];
    whenWet @ Check[
      Export[imagePath, image, exportArgs],
      Print["Rasterization failed: ", Thumbnail @ image];
    ];
  ];

  (* create and return markdown *)
  Label[skipRasterization];

  width = Ceiling @ First[imageDims * 0.5];
  url = toEmbedPath[$rasterizationURL, imageFileName, imagePath];

  whenWet[$rasterMetadataCache[cacheKey] ^= {imageDims, imageFileName, imagePath}];

  Association[
    "type" -> "File",
    "path" -> imagePath,
    "filename" -> imageFileName,
    "url" -> url,
    "width" -> width
  ]
]

PrivateFunction[toEmbedPath]

toEmbedPath[None, imageFileName_, imagePath_] := "file://" <> imagePath;
toEmbedPath[rasterizationURL_, imageFileName_, _] := NormalizePath @ FileNameJoin[{rasterizationURL, imageFileName}];

toDimsString[{w_, h_}] := StringJoin[IntegerString[w, 10, 4], "_", IntegerString[h, 10, 4]];

