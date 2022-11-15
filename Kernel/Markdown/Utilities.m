PublicFunction[SiteGarbageCollectRasters]

Options[SiteGarbageCollectRasters] = {
  DryRun -> False,
  Verbose -> Automatic
}

SiteGarbageCollectRasters[site_, OptionsPattern[]] := Scope[

  UnpackOptions[$dryRun, $verbose];
  UnpackAssociation[siteData, $markdownFlavor];

  $dryRun = $dryRun =!= False;
  SetAutomatic[$verbose, $dryRun];
  
  flavorFields = Lookup[$flavorData, $markdownFlavor, ReturnFailed[]];
  UnpackAssociation[flavorFields, $fileImageTemplate, $fileAnimatedImageTemplate];

  (* TODO: Fix me *)
  setupRasterizationPath[markdownSearchPath, "OutputImages"];
  If[FileType[markdownSearchPath] =!= Directory || FileType[$rasterizationPath] =!= Directory, ReturnFailed[]];
  
  markdownPattern = $fileImageTemplate @ Association[
    "path" -> "YYY",
    "relativepath" -> "YYY",
    "width" -> "XXX",
    "caption" -> "XXX"
  ];
  markdownPattern = StringReplace[markdownPattern, {"YYY" -> Shortest[path___], "XXX" -> Shortest[___]}];
  markdownPattern = Construct[Rule, markdownPattern, path];

  markdownFiles = FileNames["*.md", markdownSearchPath];
  markdownContent = ImportUTF8 /@ markdownFiles;
  VPrint["* Searching ", Length @ markdownFiles, " markdown files in ", MsgPath @ markdownSearchPath, "."];
 
  matches = Flatten @ StringCases[markdownContent, markdownPattern];
  matches = Map[FileNameJoin[{$rasterizationPath, FileNameTake[#]}]&, matches];
  VPrint["* Found ", Length @ matches, " referenced images."];

  If[Length[matches] == 0, ReturnFailed[]];

  existingFiles = FileNames["*.png", $rasterizationPath];
  VPrint["* Checking ", Length @ existingFiles, " existing images in ", MsgPath @ $rasterizationPath,  "."];

  garbageFiles = Complement[existingFiles, matches];

  VPrint["* Deleting ", Length @ garbageFiles, " unreferenced image files."];

  If[!$dryRun, Scan[DeleteFile, garbageFiles]];

  Return @ garbageFiles;
]

(**************************************************************************************************)

PublicFunction[SiteExportNavigationPage]

SetUsage @ "IndexPagePath is an option to various markdown-related functions."

Options[SiteExportNavigationPage] = {
  IndexPagePath -> None
}

SiteExportNavigationPage[files_, relativePrefix_String, navPath_, OptionsPattern[]] := Scope[
  UnpackOptions[indexPagePath];
  $mdFileCache = UAssociation[]; (* for inserting by id from one page to another *)
  If[StringQ[files],
    If[FileType[files] =!= Directory, ReturnFailed[]];
    files = FileNames["*.md", files];
  ];
  titles = getFileTitle /@ files;
  If[!StringVectorQ[titles], ReturnFailed[]];
  fileNames = StringJoin[relativePrefix, FileBaseName[#]]& /@ files;
  navData = MapThread[
    <|"path" -> #1, "title" -> toNavTitle[#2]|>&,
    {fileNames, titles}
  ];
  $titleToRelPath = AssociationThread[titles, fileNames];
  $titleToAbsPath = AssociationThread[titles, files];
  Which[
    StringQ[navPath],
      navOutputPath = StringReplace[navPath, ".template" -> ""],
    RuleQ[navPath],
      navPath = First @ navPath;
      navOutputPath = Last @ navPath,
    True,
      ReturnFailed[]
  ];
  If[!FileExistsQ[navPath], ReturnFailed[]];
  navTemplate = FileTemplate @ navPath;
  If[navOutputPath === navPath, ReturnFailed[]];
  nextPageFooters = Map[$nextPageTemplate, Rest @ navData];
  ScanThread[insertNextPageFooter, {files, Append[None] @ nextPageFooters}];
  FileTemplateApply[navTemplate, {navData}, navOutputPath]
];

toUnicode[str_] := FromCharacterCode[ToCharacterCode[str], "UTF8"];

getFileTitle[path_] := StringTrim @ StringDelete[toUnicode @ First @ FileLines[path, 1], "#"];

toNavTitle[e_] := StringReplace[e, " and " -> " &amp; "];

$nextPageTemplate = StringFunction @ "

@@next-link
[#title](#path)
@@
"

$nextLinkPrefix = "@@next-link";

$idInsertionRegexp = RegularExpression["""(?m)^\[\[\[([^]]+)#([^]@]+)(@\w+)?\]\]\]$"""];

insertNextPageFooter[file_, footer_] := Scope[
  newContent = content = ImportUTF8 @ file; $currentFile = file;
  If[StringContainsQ[content, $nextLinkPrefix],
    cutoff = Part[StringPosition[content, $nextLinkPrefix, 1], 1, 1] - 1;
    newContent = StringTake[content, cutoff];
  ];
  If[StringContainsQ[content, $idInsertionRegexp],
    newContent = StringReplace[newContent, $idInsertionRegexp :> toInsertedContent[StringReplace["$1", "\n" -> " "], "$2", "$3"]]
  ];
  If[StringContainsQ[content, $markdownLinkRegexp],
    newContent = StringReplace[newContent, $markdownLinkRegexp :> toInlineLink[StringReplace["$1", "\n" -> " "]]];
  ];
  If[footer =!= None,
    newContent = StringTrim[newContent] <> footer];
  If[content =!= newContent, ExportUTF8[file, newContent]];
];

PrivateFunction[toAnchorString]

toAnchorString[str_] := StringReplace[ToLowerCase[str], {":" -> "", " " -> "_"}];

toInlineLink[title_] := Scope[
  If[StringContainsQ[title, "#"],
    {title, anchor} = StringSplit[title, "#", 2],
    anchor = None;
  ];
  If[StringContainsQ[title, ":"],
    {label, title} = StringSplit[title, ":", 2],
    label = title
  ];
  If[label == "", label = title];
  relPath = If[title === "","",
    relPath = Lookup[$titleToRelPath, title, None]];
  If[relPath === None,
    Print["Could not resolve inline link to \"", title, "\" in file ", $currentFile];
    Print["Available: ", Keys @ $titleToRelPath];
    Return @ StringJoin["\"", title, "\""]
  ];
  If[anchor =!= None, relPath = StringJoin[relPath, "#", toAnchorString @ anchor]];
  StringJoin["[", label, "](", relPath, ")"]
];

toInsertedContent[title_, id_, div_] := Scope[
  absPath = Lookup[$titleToAbsPath, title, None];
  If[absPath === None,
    Print["Could not resolve insertion request to \"", title, "\" (", id, ") in file ", $currentFile];
    Return @ "";
  ];
  markdown = CacheTo[$mdFileCache, absPath, ImportUTF8 @ absPath];
  foundContent = StringCases[markdown, "\\label{" <> id <> "}\n\n" ~~ Shortest[zzz__] ~~ "\n\n" :> zzz, 1];
  foundContent = First[foundContent, None];
  If[!StringQ[foundContent],
    Print["Could not find ID \"", id, "\" in \"", absPath, "\" referenced from file ", $currentFile];
    Return @ "";
  ];
  foundContent //= StringTrim;
  If[div =!= "", foundContent = StringJoin["@@", StringTrim[div, "@"], "\n", foundContent, "\n", "@@"]];
  foundContent
]
