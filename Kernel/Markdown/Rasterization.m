PrivateFunction[cellToRasterMarkdown]

ToMarkdownString::badimgtemp = "Setting of FileImageTemplate (or StringImageTemplate) did not return a markdown string: ``.";
ToMarkdownString::badrastres = "Setting of RasterizationFunction did not return a valid association.";

cellToRasterMarkdown[cell_] := Scope[

  If[$rasterizationFunction === None, Return["#### Placeholder for image"]];

  thisTag = Nothing;
  cell = cell /. TagBox[contents_, "ClassTaggedForm"[tag_]] :> (thisTag = tag; contents);

  rasterizationOptions = Sequence[];
  cell = cell /. TagBox[contents_, "RasterizationOptions"[opts___]] :> (rasterizationOptions = opts; contents);

  rasterizationResult = $rasterizationFunction[cell, rasterizationOptions];

  If[!AssocQ[rasterizationResult],
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

linearSyntaxRasterizationFunction[cell_, ___] := Assoc[
  "type" -> "String",
  "linearSyntax" -> ToString[RawBoxes @ Part[cell, 1, 1], StandardForm]
];

(**************************************************************************************************)

PrivateFunction[base64RasterizationFunction]

CacheVariable[$Base64RasterizationCache]

base64RasterizationFunction[type_, retina_][e_] := Scope[
  
  type //= ToUpperCase;

  CachedInto[$Base64RasterizationCache, Hash[{e, type, retina}],

    If[type === "PNG",
      If[H[e] === Cell, e = Append[e, Antialiasing -> False], e = Style[e, Antialiasing -> False]];
    ];

    img = Rasterize[e, ImageResolution -> If[retina, 144, 72]];
    width = Round[P1[ImageDimensions @ img] / If[retina, 2, 1]];
    encoded = ExportBase64[img, type, CompressionLevel -> If[type === "PNG", 1.0, 0.2]];
    Assoc[
      "type" -> "String",
      "width" -> width,
      "format" -> ToLowerCase[type],
      "encoded" -> encoded
    ]
  ]
];

(**************************************************************************************************)

PrivateFunction[ImportBase64, ExportBase64]

ImportBase64[str_Str, args___] := ImportString[FromCharacterCode @ FromBase64Digits @ str, args];
ExportBase64[img_Image, args___] := Base64String @ ToCharacterCode @ ExportString[img, args, IncludeMetaInformation -> False];

(**************************************************************************************************)

PrivateFunction[standardRasterizationFunction]

standardRasterizationFunction[Cell[BoxData[t:TagBox[_, _BoxForm`AnimatedImageTag]], ___], ___] :=
  standardRasterizationFunction @ ToExpression[t, StandardForm];

standardRasterizationFunction[HoldPattern @ a_AnimatedImage, opts___] :=
  cachedGenericRasterize[a, rasterizeAnimatedImage, {"Format" -> "gif", opts}];

rasterizeAnimatedImage[HoldPattern @ a_AnimatedImage] := {a, a["RasterSize"]};

standardRasterizationFunction[cell_, opts___Rule] :=
  cachedGenericRasterize[cell, rasterizeImage, {opts}];

rasterizeImage[obj_] := Scope[
  img = If[TrueQ @ $rasterizationCaching, MakeImage, uncachedMakeImage] @ obj;
  {img, ImageDimensions @ img}
]

uncachedMakeImage[obj_] := Block[{$EnableCaching = False}, MakeImage @ obj];

(**************************************************************************************************)

$rasterMetadataCache = UAssoc[];

cachedGenericRasterize[obj_, rasterizeFn_, exportOpts_] := Scope[

  exportOpts = Join[exportOpts, $rasterizationOptions];
  fileExt = ToLowerCase @ Lookup[exportOpts, "Format", "png"];
  exportOpts //= DeleteCases["Format" -> _];

  If[$rasterizationPath === None, Return @ None];

  (* did we already export this result in this session? *)
  fileExt //= ToLowerCase;
  objHash = Base36Hash @ obj;
  exportOpts = Sort @ DeleteDuplicatesBy[exportOpts, P1];
  optsHash = If[exportOpts === {}, None, Base36Hash @ exportOpts];
  optsHashStr = If[optsHash =!= None, "_" <> optsHash, ""];

  cacheKey = {objHash, optsHash, $rasterizationURL, $rasterizationPath};
  cacheValue = Lookup[$rasterMetadataCache, Key @ cacheKey, None];
  If[ListQ[cacheValue] && $rasterizationCaching,
    (* this cache gives us enough info to generate the markdown without looking for the file on disk *)
    {imageDims, imageFileName, imagePath} = cacheValue;
    If[FileExistsQ[imagePath], Goto[skipRasterization]];
  ];

  whenWet @ EnsureDirectory @ $rasterizationPath;

  (* did we already export this result in a previous session? here, we will take the most recent file,
  which we assume represents the image dimensions that were passed to this function, which in theory
  might not be the case but probably is! *)
  filePattern = StringJoin[objHash, "_*_*", optsHashStr, ".", fileExt];
  imagePath = First[FileNames[filePattern, $rasterizationPath], None];
  If[StringQ[imagePath] && $rasterizationCaching,
    imageFileName = FileNameTake @ imagePath;
    baseName = FileBaseName @ imagePath;
    imageDims = FromDigits /@ StringExtract[baseName, "_" -> {3, 4}];
    Goto[skipRasterization];
  ];

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
    imageFileName = StringJoin[objHash, "_", imageHash, "_", dimsStr, optsHashStr, ".", fileExt];
  ];

  imagePath = PathJoin[$rasterizationPath, imageFileName];

  (* export *)
  If[!FileExistsQ[imagePath] || !$rasterizationCaching,
    VPrint["* Exporting image to ", MsgPath @ imagePath];
    whenWet @ Check[
      If[fileExt == "png" && !MemberQ[exportOpts, CompressionLevel -> _],
        AppendTo[exportOpts, CompressionLevel -> 1]];
      VPrint["Export[", MsgPath @ imagePath, ", ", ImageDimensions @ image, ", ", exportOpts, "]"];
      Export[imagePath, image, Sequence @@ exportOpts],
      Print["Export[", MsgPath @ imagePath, ", ", Thumbnail @ image, ", ...] failed."];
    ];
  ];

  (* create and return markdown *)
  Label[skipRasterization];

  width = Ceiling @ P1[imageDims * 0.5];
  url = toEmbedPath[$rasterizationURL, imageFileName, imagePath];

  whenWet[$rasterMetadataCache[cacheKey] ^= {imageDims, imageFileName, imagePath}];

  Assoc[
    "type" -> "File",
    "path" -> imagePath,
    "filename" -> imageFileName,
    "url" -> url,
    "width" -> width
  ]
]

PrivateFunction[toEmbedPath]

toEmbedPath[None, imageFileName_, imagePath_] := "file://" <> imagePath;
toEmbedPath[rasterizationURL_, imageFileName_, _] := NormalizePath @ PathJoin[rasterizationURL, imageFileName];

toDimsString[{w_, h_}] := StringJoin[IntegerString[w, 10, 4], "_", IntegerString[h, 10, 4]];

