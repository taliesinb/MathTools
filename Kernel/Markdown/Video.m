PrivateFunction[videoBoxToMarkdown]

videoBoxToMarkdown[TemplateBox[{_, srcPath_, ___}, "VideoBox1", ___]] :=
  videoPathToMarkdown @ srcPath;

videoBoxToMarkdown[TemplateBox[assoc_Assoc, "VideoBox2", ___]] :=
  videoPathToMarkdown @ assoc["resourcePath"]

videoBoxToMarkdown[_] := videoPathToMarkdown[None];

videoPathToMarkdown[other_] := "#### Invalid Video";

videoPathToMarkdown[srcPath_Str | File[srcPath_Str]] := Scope[
  
  If[!FileExistsQ[srcPath], Return @ "#### Missing video file"];
  
  videoDims = getVideoRasterSize @ srcPath;

  videoFileName = FileNameTake @ srcPath;

  videoPath = PathJoin[$rasterizationPath, videoFileName];

  If[!$dryRun,
    EnsureDirectory[$rasterizationPath];
    If[!FileExistsQ[videoPath], CopyFile[srcPath, videoPath]];
  ];
  VPrint["Copying file from ", MsgPath @ srcPath, " to ", MsgPath @ videoPath];

  {w, h} = Ceiling[videoDims / 2];
  relativePath = toEmbedPath[$rasterizationURL, videoFileName, videoPath];
  
  $fileAnimatedImageTemplate @ Assoc[
    "width" -> w, "height" -> h,
    "videopath" -> videoPath,
    "url" -> relativePath
  ]
]

$rasterSizeCache = <||>;

getVideoRasterSize[path_] := CachedInto[$rasterSizeCache, path, P1 @ Information[Video[path], "OriginalRasterSize"]];

