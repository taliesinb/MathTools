PrivateFunction[videoCellToMarkdown]

videoCellToMarkdown[TemplateBox[{_, srcPath_, ___}, "VideoBox1", ___]] := Scope[
  
  If[!FileExistsQ[srcPath], Return @ "#### Missing video file"];
  
  videoDims = getVideoRasterSize @ srcPath;

  videoFileName = FileNameTake @ srcPath;

  videoPath = FileNameJoin[{$rasterizationPath, videoFileName}];

  If[!$dryRun,
    EnsureDirectory[$rasterizationPath];
    If[!FileExistsQ[videoPath], CopyFile[srcPath, videoPath]];
  ];
  mdvPrint["Copying file from ", srcPath, " to ", videoPath];

  {w, h} = Ceiling[videoDims / 2];
  relativePath = toEmbedPath[$relativeRasterizationPath, videoFileName, videoPath];
  
  $fileVideoTemplate @ Association[
    "width" -> w, "height" -> h,
    "videopath" -> videoPath,
    "relativepath" -> relativePath
  ]
]

videoCellToMarkdown[_] := "#### Invalid Video"

$rasterSizeCache = <||>;

getVideoRasterSize[path_] := CacheTo[$rasterSizeCache, path, First @ Information[Video[path], "OriginalRasterSize"]];
