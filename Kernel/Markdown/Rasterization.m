PrivateSymbol[linearSyntaxRasterizationFunction]

linearSyntaxRasterizationFunction[cell_] := Association[
  "type" -> "String",
  "linearSyntax" -> ToString[RawBoxes @ Part[cell, 1, 1], StandardForm]
];

(**************************************************************************************************)

PrivateSymbol[standardRasterizationFunction]

standardRasterizationFunction[Cell[BoxData[t:TagBox[_, _BoxForm`AnimatedImageTag]], ___]] :=
  standardRasterizationFunction @ ToExpression[t, StandardForm];

standardRasterizationFunction[a_AnimatedImage] :=
  cachedGenericRasterize[a, rasterizeAnimatedImage, "gif"];

rasterizeAnimatedImage[a_AnimatedImage] := {a, a["RasterSize"]};

standardRasterizationFunction[cell_] :=
  cachedGenericRasterize[cell, rasterizeImage, "png", CompressionLevel -> 1];

rasterizeImage[obj_] := Scope[
  img = Rasterize[obj, ImageFormattingWidth -> Infinity, ImageResolution -> 144];
  {img, ImageDimensions @ img}
]

(**************************************************************************************************)

$rasterMetadataCache = Data`UnorderedAssociation[];

cachedGenericRasterize[obj_, rasterizeFn_, fileExt_, exportArgs___] := Scope[

  If[$rasterizationPath === None, Return @ None];

  (* did we already export this result in this session? *)
  objHash = Base36Hash @ obj;
  cacheKey = {objHash, $relativeRasterizationPath, $rasterizationPath};
  cacheValue = Lookup[$rasterMetadataCache, Key @ cacheKey, None];
  If[ListQ[cacheValue] && $rasterizationCaching,
    (* this cache gives us enough info to generate the markdown without looking for the file on disk *)
    {imageDims, imageFileName, imagePath} = cacheValue;
    Goto[skipRasterization];
  ];

  If[!$dryRun, EnsureDirectory @ $rasterizationPath];

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
    mdvPrint["* Rasterization yielded image of size ", w, " by ", h];
    dimsStr = StringJoin[IntegerString[w, 10, 4], "_", IntegerString[h, 10, 4]];
    imageFileName = StringJoin[objHash, "_", imageHash, "_", dimsStr, ".", fileExt];
  ];

  imagePath = FileNameJoin[{$rasterizationPath, imageFileName}];

  (* export *)
  If[!FileExistsQ[imagePath] || !$rasterizationCaching,
    mdvPrint["* Exporting image to ", File @ imagePath];
    If[!$dryRun, Check[
      Export[imagePath, image, exportArgs],
      Print["Rasterization failed: ", Thumbnail @ image];
    ]];
  ];

  (* create and return markdown *)
  Label[skipRasterization];

  width = Ceiling @ First[imageDims * 0.5];
  imageRelativePath = toEmbedPath[$relativeRasterizationPath, imageFileName, imagePath];

  If[!$dryRun, $rasterMetadataCache[cacheKey] ^= {imageDims, imageFileName, imagePath}];

  Association[
    "type" -> "File",
    "path" -> imagePath,
    "relativepath" -> imageRelativePath,
    "width" -> width
  ]
]

PrivateFunction[toEmbedPath]

toEmbedPath[None, imageFileName_, imagePath_] := "file://" <> imagePath;
toEmbedPath[relative_, imageFileName_, _] := NormalizePath @ FileNameJoin[{relative, imageFileName}];

toDimsString[{w_, h_}] := StringJoin[IntegerString[w, 10, 4], "_", IntegerString[h, 10, 4]];

