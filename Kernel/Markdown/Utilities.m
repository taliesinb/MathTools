PublicFunction[SiteGarbageCollectRasters]

Options[SiteGarbageCollectRasters] = {
  DryRun -> False,
  Verbose -> Auto
}

SiteGarbageCollectRasters[site_, OptionsPattern[]] := Scope[

  UnpackOptions[$dryRun, $verbose];
  UnpackAssociation[siteData, $markdownFlavor];

  $dryRun = $dryRun =!= False;
  SetAuto[$verbose, $dryRun];
  
  flavorFields = Lookup[$flavorData, $markdownFlavor, ReturnFailed[]];
  UnpackAssociation[flavorFields, $fileImageTemplate, $fileAnimatedImageTemplate];

  (* TODO: Fix me *)
  setupRasterizationPath[markdownSearchPath, "OutputImages"];
  If[FileType[markdownSearchPath] =!= Directory || FileType[$rasterizationPath] =!= Directory, ReturnFailed[]];
  
  markdownPattern = $fileImageTemplate @ Assoc[
    "path" -> "YYY",
    "relativepath" -> "YYY",
    "width" -> "XXX",
    "caption" -> "XXX"
  ];
  markdownPattern = SRep[markdownPattern, {"YYY" -> Shortest[path___], "XXX" -> Shortest[___]}];
  markdownPattern = Construct[Rule, markdownPattern, path];

  markdownFiles = FileNames["*.md", markdownSearchPath];
  markdownContent = ImportUTF8 /@ markdownFiles;
  VPrint["* Searching ", Len @ markdownFiles, " markdown files in ", MsgPath @ markdownSearchPath, "."];
 
  matches = Flatten @ SCases[markdownContent, markdownPattern];
  matches = Map[PathJoin[$rasterizationPath, FileNameTake[#]]&, matches];
  VPrint["* Found ", Len @ matches, " referenced images."];

  If[Len[matches] == 0, ReturnFailed[]];

  existingFiles = FileNames["*.png", $rasterizationPath];
  VPrint["* Checking ", Len @ existingFiles, " existing images in ", MsgPath @ $rasterizationPath,  "."];

  garbageFiles = Comp[existingFiles, matches];

  VPrint["* Deleting ", Len @ garbageFiles, " unreferenced image files."];

  If[!$dryRun, Scan[DeleteFile, garbageFiles]];

  Return @ garbageFiles;
]

(**************************************************************************************************)

PublicFunction[SiteExportNavigationPage]

SetUsage @ "IndexPagePath is an option to various markdown-related functions."

Options[SiteExportNavigationPage] = {
  IndexPagePath -> None
}

SiteExportNavigationPage[files_, relativePrefix_Str, navPath_, OptionsPattern[]] := Scope[
  UnpackOptions[indexPagePath];
  $mdFileCache = UAssoc[]; (* for inserting by id from one page to another *)
  If[StrQ[files],
    If[FileType[files] =!= Directory, ReturnFailed[]];
    files = FileNames["*.md", files];
  ];
  titles = getFileTitle /@ files;
  If[!StrVecQ[titles], ReturnFailed[]];
  fileNames = SJoin[relativePrefix, FileBaseName[#]]& /@ files;
  navData = MapThread[
    <|"path" -> #1, "title" -> toNavTitle[#2]|>&,
    {fileNames, titles}
  ];
  $titleToRelPath = AssocThread[titles, fileNames];
  $titleToAbsPath = AssocThread[titles, files];
  Which[
    StrQ[navPath],
      navOutputPath = SRep[navPath, ".template" -> ""],
    RuleQ[navPath],
      navPath = F @ navPath;
      navOutputPath = L @ navPath,
    True,
      ReturnFailed[]
  ];
  If[!FileExistsQ[navPath], ReturnFailed[]];
  navTemplate = FileTemplate @ navPath;
  If[navOutputPath === navPath, ReturnFailed[]];
  nextPageFooters = Map[$nextPageTemplate, Rest @ navData];
  ScanThread[insertNextPageFooter, {files, App[None] @ nextPageFooters}];
  FileTemplateApply[navTemplate, {navData}, navOutputPath]
];

toUnicode[str_] := FromCharCode[ToCharCode[str], "UTF8"];

getFileTitle[path_] := STrim @ SDelete[toUnicode @ F @ FileLines[path, 1], "#"];

toNavTitle[e_] := SRep[e, " and " -> " &amp; "];

$nextPageTemplate = StringFunction @ "

@@next-link
[#title](#path)
@@
"

$nextLinkPrefix = "@@next-link";

$idInsertionRegexp = RegularExpression["""(?m)^\[\[\[([^]]+)#([^]@]+)(@\w+)?\]\]\]$"""];

insertNextPageFooter[file_, footer_] := Scope[
  newContent = content = ImportUTF8 @ file; $currentFile = file;
  If[SContainsQ[content, $nextLinkPrefix],
    cutoff = FF[SFind[content, $nextLinkPrefix, 1]] - 1;
    newContent = STake[content, cutoff];
  ];
  If[SContainsQ[content, $idInsertionRegexp],
    newContent = SRep[newContent, $idInsertionRegexp :> toInsertedContent[SRep["$1", "\n" -> " "], "$2", "$3"]]
  ];
  If[SContainsQ[content, $markdownLinkRegexp],
    newContent = SRep[newContent, $markdownLinkRegexp :> toInlineLink[SRep["$1", "\n" -> " "]]];
  ];
  If[footer =!= None,
    newContent = STrim[newContent] <> footer];
  If[content =!= newContent, ExportUTF8[file, newContent]];
];

PrivateFunction[toAnchorString]

toAnchorString[str_] := SRep[ToLowerCase[str], {":" -> "", " " -> "_"}];

toInlineLink[title_] := Scope[
  If[SContainsQ[title, "#"],
    {title, anchor} = SSplit[title, "#", 2],
    anchor = None;
  ];
  If[SContainsQ[title, ":"],
    {label, title} = SSplit[title, ":", 2],
    label = title
  ];
  If[label == "", label = title];
  relPath = If[title === "","",
    relPath = Lookup[$titleToRelPath, title, None]];
  If[relPath === None,
    Print["Could not resolve inline link to \"", title, "\" in file ", $currentFile];
    Print["Available: ", Keys @ $titleToRelPath];
    Return @ SJoin["\"", title, "\""]
  ];
  If[anchor =!= None, relPath = SJoin[relPath, "#", toAnchorString @ anchor]];
  SJoin["[", label, "](", relPath, ")"]
];

toInsertedContent[title_, id_, div_] := Scope[
  absPath = Lookup[$titleToAbsPath, title, None];
  If[absPath === None,
    Print["Could not resolve insertion request to \"", title, "\" (", id, ") in file ", $currentFile];
    Return @ "";
  ];
  markdown = CacheTo[$mdFileCache, absPath, ImportUTF8 @ absPath];
  foundContent = SCases[markdown, "\\label{" <> id <> "}\n\n" ~~ Shortest[zzz__] ~~ "\n\n" :> zzz, 1];
  foundContent = F[foundContent, None];
  If[!StrQ[foundContent],
    Print["Could not find ID \"", id, "\" in \"", absPath, "\" referenced from file ", $currentFile];
    Return @ "";
  ];
  foundContent //= STrim;
  If[div =!= "", foundContent = SJoin["@@", STrim[div, "@"], "\n", foundContent, "\n", "@@"]];
  foundContent
]