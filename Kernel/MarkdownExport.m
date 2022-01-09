PackageExport["$DefaultImportPath"]
PackageExport["$DefaultExportPath"]

$DefaultImportPath = ExpandFileName @ "~";
$DefaultExportPath = ExpandFileName @ "~";

SetUsage @ "
$DefaultImportPath determines the default used when %ImportPath is not specified.
"

SetUsage @ "
$DefaultExportPath determines the default used when %ExportPath is not specified.
"

(**************************************************************************************************)

PackageExport["ImportPath"]

SetUsage @ "
ImportPath is an option to various markdown-related functions that determines the import path.
* The default value of ImportPath is $DefaultImportPath.
"

(**************************************************************************************************)

PackageExport["ExportPath"]

SetUsage @ "
ExportPath is an option to various markdown-related functions that determines the export path.
* The default value of ExportPath is $DefaultExportPath.
"

(**************************************************************************************************)

PackageExport["MarkdownFlavor"]

SetUsage @ "
MarkdownFlavor is an option to various markdown-related functions.
"

(**************************************************************************************************)

PackageExport["RasterizationPath"]

SetUsage @ "
RasterizationPath is an option to various markdown-related functions that determines the path to place rasterized outputs.
* The default value of RasterizationPath is Automatic, which will use the relative directory './Outputs'.
"

(**************************************************************************************************)

PackageExport["DisplayProgress"]

SetUsage @ "
DisplayProgress is an option to various markdown-related functions that determines whether to display progress interactively.
"

(**************************************************************************************************)

PackageExport["HeadingDepthOffset"]

SetUsage @ "
HeadingDepthOffset is an option to various markdown-related functions.
"

(**************************************************************************************************)

PackageExport["IndexPagePath"]

SetUsage @ "
IndexPagePath is an option to various markdown-related functions.
"

(**************************************************************************************************)

PackageExport["IncludePrelude"]

SetUsage @ "
IncludePrelude is an option to various markdown-related functions.
"

(**************************************************************************************************)

PackageExport["DryRun"]

SetUsage @ "
DryRun is an option to various markdown-related functions.
"

(**************************************************************************************************)

$genericMarkdownOptions = {
  MarkdownFlavor -> "Preview",
  RasterizationPath -> Automatic,
  DisplayProgress -> False,
  HeadingDepthOffset -> 0,
  IncludePrelude -> Automatic,
  DryRun -> False,
  Verbose -> Automatic
}

$genericMarkdownExportOptions = {
  ImportPath :> $DefaultImportPath,
  ExportPath :> $DefaultExportPath,
  Splice @ $genericMarkdownOptions
}

DefineLiteralMacro[setupMarkdownGlobals,
setupMarkdownGlobals[] := Quoted[
  UnpackOptions[markdownFlavor, headingDepthOffset, includePrelude];
  SetAutomatic[includePrelude, !MatchQ[markdownFlavor, "Simple" | "Preview"]];
  flavorFields = Lookup[$flavorData, markdownFlavor, ReturnFailed[]];
  UnpackAssociation[flavorFields,
    rasterizationFunction, stringImageTemplate, fileImageTemplate, fileVideoTemplate, anchorTemplate,
    inlineMathTemplate, multilineMathTemplate, markdownPostprocessor, inlineHTML
  ];
]];

DefineLiteralMacro[setupRasterizationPath,
setupRasterizationPath[baseDir_, default_] := Quoted[
  UnpackOptions[rasterizationPath];
  SetAutomatic[rasterizationPath, default];
  Switch[rasterizationPath,
    _String ? AbsolutePathQ,
      relativeRasterizationPath = None,
    _String,
      relativeRasterizationPath = rasterizationPath,
    {_String, _String},
      {rasterizationPath, relativeRasterizationPath} = rasterizationPath,
    None,
      Return[],
    _,
      ReturnFailed[];
  ];
  rasterizationPath //= NormalizePath;
  If[!AbsolutePathQ[rasterizationPath],
    rasterizationPath = ToFileName[baseDir, rasterizationPath]];
]];

(**************************************************************************************************)

ioTypeSymbol = <|"Import" -> ImportPath, "Export" -> ExportPath|>;

resolvePath[type_, Automatic, None] :=
  ThrowMessage["noautopath", type, ioTypeSymbol @ type];

resolvePath[type_, Automatic, base_String] :=
  checkFileExt[type] @ NormalizePath @ base;

resolvePath[type_, File[path_], base_] :=
  resolvePath[type, path, base];

resolvePath[type_, path_String ? AbsolutePathQ, base_] :=
  checkFileExt[type] @ NormalizePath @ path;

resolvePath[type_, path_String, base_String] /; StringFreeQ[path, "~"] :=
  checkFileExt[type] @ ToFileName[base, path];

resolvePath[type_, path_String, None] :=
  ThrowMessage["badrelpath", ToLowerCase @ type, path, ioTypeSymbol @ type];

resolvePath["Import", nb_NotebookObject, base_] :=
  nb;

resolvePath[type_, path_, base_] :=
  ThrowMessage["badpathspec", type, path];

ioFileExt = <|"Import" -> "" | "nb", "Export" -> "" | "md"|>;

checkFileExt[type_][file_] := If[
  MatchQ[FileExtension @ file, ioFileExt @ type], file,
  ThrowMessage["notmdext", type, file]
];

resolveImportExportPaths[ispec_, ipath_, ospec_, opath_] := Scope[
  i = resolvePath["Import", ispec, ipath];
  o = resolvePath["Export", ospec, opath];
  If[StringQ[i] && !FileExistsQ[i], ThrowMessage["neimportpath", i]];
  If[StringContainsQ[o, "*"], ThrowMessage["noexportwc", o]];
  {i, o}
]

General::noexportwc = "Export path `` cannot contain wildcards."
General::noautopath = "`` path specification of Automatic requires `` to be set."
General::badrelpath = "Relative `` path specification `` requires `` to be set."
General::badpathspec = "`` path specification `` does not appear to be valid."
General::neimportpath = "Import specification `` does not correspond to an existing file or directory."
General::notmdext = "`` file specification `` does not end in \".md\"."

(**************************************************************************************************)

PackageExport["ExportToMarkdown"]
PackageExport["CollatedPagePath"]

General::badcpp = "Bad CollatedPagePath ``."

General::expdirne = "Export directory `` does not exist."

Options[ExportToMarkdown] = JoinOptions[
  MaxItems -> Infinity,
  CollatedPagePath -> None,
  "NotebookCaching" -> True,
  $genericMarkdownExportOptions
];

$dryRun = False;
$verbose = False;

SetHoldAllComplete[vPrint];
vPrint[args___] /; TrueQ[$verbose] := Print @@ ReplaceAll[{args}, File[path_] :> StringJoin["\"", path, "\""]];

ExportToMarkdown[importSpec_, exportSpec_, OptionsPattern[]] := Scope @ CatchMessage[
  UnpackOptions[importPath, exportPath, $dryRun, $verbose, maxItems, includePrelude, collatedPagePath, $notebookCaching];
  $dryRun = $dryRun =!= False;
  SetAutomatic[$verbose, $dryRun];
  setupMarkdownGlobals[];
  {ipath, opath} = resolveImportExportPaths[importSpec, importPath, exportSpec, exportPath];
  iexpr = parseInputSpec @ ipath;
  If[ListQ[iexpr] && maxItems =!= Infinity,
    iexpr = If[IntegerQ[maxItems], Take[iexpr, UpTo @ maxItems], Take[iexpr, maxItems]]];
  outputDir = If[StringEndsQ[opath, ".md"], FileNameDrop @ opath, opath];
  If[FileType[outputDir] =!= Directory, ThrowMessage["expdirne", outputDir]];
  setupRasterizationPath[outputDir, "OutputImages"];
  vPrint["Rasterizing to ", File @ rasterizationPath, " embedded as \"", relativeRasterizationPath, "\""];
  If[StringQ[includePrelude],
    preludePath = NormalizePath @ includePrelude;
    If[Not @ AbsolutePathQ @ preludePath,
      preludePath = ToFileName[outputDir, preludePath]];
    vPrint["Writing prelude to ", File @ preludePath, "."];
    ExportUTF8[preludePath, StringReplace[markdownPostprocessor @ $KatexPrelude, "\n" -> " "]];
  ];
  fileList = doImportExport[iexpr, opath, StringEndsQ[opath, ".md"]];
  If[StringQ[collatedPagePath],
    Which[
      !StringEndsQ[collatedPagePath, ".md"], ReturnFailed["badcpp", collatedPagePath],
      AbsolutePathQ[collatedPagePath], Null,
      DirectoryQ[opath], collatedPagePath = ToFileName[opath, collatedPagePath],
      True, ReturnFailed["badcpp", collatedPagePath]
    ];
    doImportExport[iexpr, collatedPagePath, True];
  ];
  fileList
];

General::emptynbdir = "Import directory `` does not contain any notebooks."

parseInputSpec = Case[
  nb_NotebookObject                    := nb;
  path_String ? DirectoryQ             := enumerateFiles["*.nb", path];
  path_String ? (StringContainsQ["*"]) := enumerateFiles[path];
  path_String                          := File[path];
]

enumerateFiles[spec___, path_] := Scope[
  files = File /@ FileNames[spec, path];
  If[files === {}, ThrowMessage["emptynbdir", path]];
  files
];

$notebookP = _File | _NotebookObject;

PackageExport["$LastFailedMarkdownResult"]

General::nbimportfail = "Failed to convert notebook or notebooks ``. Invalid result available as $LastFailedMarkdownResult."
doImportExport[spec:($notebookP | {$notebookP..}), exportPath_, True] := Scope[
  str = iToMarkdownString @ spec;
  If[!StringQ[str], ThrowMessage["nbimportfail", spec]];
  If[$dryRun,
    Print["* Exporting ", StringCount[str, "\n"], " lines from ", Length @ ToList @ spec, " inputs to ", exportPath]; exportPath,
    ExportUTF8[exportPath, str]
  ]
];

doImportExport[single:$notebookP, exportPath_, False] :=
  doImportExport[single, ToFileName[exportPath, getNotebookName @ single], True];

doImportExport[multi:{$notebookP..}, exportPath_, False] :=
  doImportExport[#, exportPath, False]& /@ multi;

doImportExport[spec_, _, _] := ThrowMessage["nbimportfail", spec];

getNotebookName = Case[
  nb_Notebook := % @ CurrentValue[nb, "NotebookFileName"];
  File[file_] := toMDFileName @ FileBaseName @ file;
];

$mdFileNameTrim = DigitCharacter.. ~~ Repeated[" - ", {0, 1}];
toMDFileName[string_] := Scope[
  string = StringDelete[ToLowerCase @ string, StartOfString ~~ $mdFileNameTrim];
  StringReplace[StringTrim @ string, (Whitespace | "-").. -> "-"] <> ".md"
];

(**************************************************************************************************)

PackageExport["GarbageCollectOutputImages"]

Options[GarbageCollectOutputImages] = {
  MarkdownFlavor -> "Franklin",
  RasterizationPath -> None,
  DryRun -> False,
  Verbose -> Automatic
}

GarbageCollectOutputImages[markdownSearchPath_, OptionsPattern[]] := Scope[
  UnpackOptions[markdownFlavor, $dryRun, $verbose];

  $dryRun = $dryRun =!= False;
  SetAutomatic[$verbose, $dryRun];
  
  flavorFields = Lookup[$flavorData, markdownFlavor, ReturnFailed[]];
  UnpackAssociation[flavorFields, fileImageTemplate];

  setupRasterizationPath[markdownSearchPath, "OutputImages"];
  If[FileType[markdownSearchPath] =!= Directory || FileType[rasterizationPath] =!= Directory, ReturnFailed[]];
  
  markdownPattern = fileImageTemplate @ Association[
    "imagepath" -> "YYY",
    "relativepath" -> "YYY",
    "width" -> "XXX",
    "caption" -> "XXX"
  ];
  markdownPattern = StringReplace[markdownPattern, {"YYY" -> Shortest[path___], "XXX" -> Shortest[___]}];
  markdownPattern = Construct[Rule, markdownPattern, path];

  markdownFiles = FileNames["*.md", markdownSearchPath];
  markdownContent = ImportUTF8 /@ markdownFiles;
  vPrint["* Searching ", Length @ markdownFiles, " markdown files in ", File @ markdownSearchPath, "."];
 
  matches = Flatten @ StringCases[markdownContent, markdownPattern];
  matches = Map[FileNameJoin[{rasterizationPath, FileNameTake[#]}]&, matches];
  vPrint["* Founding ", Length @ matches, " referenced images."];

  If[Length[matches] == 0, ReturnFailed[]];

  existingFiles = FileNames["*.png", rasterizationPath];
  vPrint["* Checking ", Length @ existingFiles, " existing images in ", File @ rasterizationPath,  "."];

  garbageFiles = Complement[existingFiles, matches];

  vPrint["* Deleting ", Length @ garbageFiles, " markdown files."];

  If[!$dryRun, Scan[DeleteFile, garbageFiles]];

  Return @ garbageFiles;
]

(**************************************************************************************************)

PackageExport["ExportNavigationPage"]

Options[ExportNavigationPage] = {
  IndexPagePath -> None
}

ExportNavigationPage[files_, relativePrefix_String, navPath_, OptionsPattern[]] := Scope[
  UnpackOptions[indexPagePath];
  $mdFileCache = Data`UnorderedAssociation[]; (* for inserting by id from one page to another *)
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

$nextPageTemplate = StringTemplate @ "

@@next-link
[`title`](`path`)
@@
"

$nextLinkPrefix = "@@next-link";

insertNextPageFooter[file_, footer_] := Scope[
  newContent = content = ImportUTF8 @ file; $currentFile = file;
  If[StringContainsQ[content, $nextLinkPrefix],
    cutoff = Part[StringPosition[content, $nextLinkPrefix, 1], 1, 1] - 1;
    newContent = StringTake[content, cutoff];
  ];
  If[StringContainsQ[content, $idInsertionRegexp],
    newContent = StringReplace[newContent, $idInsertionRegexp :> toInsertedContent[StringReplace["$1", "\n" -> " "], "$2", "$3"]]
  ];
  If[StringContainsQ[content, $linkRegexp],
    newContent = StringReplace[newContent, $linkRegexp :> toInlineLink[StringReplace["$1", "\n" -> " "]]];
  ];
  If[footer =!= None,
    newContent = StringTrim[newContent] <> footer];
  If[content =!= newContent, ExportUTF8[file, newContent]];
];

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
  relPath = If[title === "", "",
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
  targetRegex = "\\label{" <> id <> "}\n\n" ~~ Shortest[content__] ~~ "\n\n";
  foundContent = StringCases[markdown, targetRegex :> content, 1];
  foundContent = First[foundContent, None];
  If[!StringQ[foundContent],
    Print["Could not find ID \"", id, "\" in \"", absPath, "\" referenced from file ", $currentFile];
    Return @ "";
  ];
  foundContent //= StringTrim;
  If[div =!= "", foundContent = StringJoin["@@", StringTrim[div, "@"], "\n", foundContent, "\n", "@@"]];
  foundContent
]

$idInsertionRegexp = RegularExpression["""(?m)^\[\[\[([^]]+)#([^]@]+)(@\w+)?\]\]\]$"""];

$linkRegexp = RegularExpression["""\[\[\[([^]@]+)\]\]\]"""];

(**************************************************************************************************)

$flavorData = <||>;

wrapDollar[e_] := StringJoin["$", e, "$"];
wrapDoubleDollar[e_] := StringJoin["$$\n", e, "\n$$"];
wrapDoubleBrace[e_] := StringJoin["\\\\[\n", e, "\n\\\\]"];

(**************************************************************************************************)

$flavorData["Franklin"] = <||>;

$flavorData["Franklin", "RasterizationFunction"] = PNGRasterizationFunction;

$flavorData["Franklin", "StringImageTemplate"] = None;

$flavorData["Franklin", "FileImageTemplate"] = StringTemplate @ StringTrim @  """
~~~
<img src="`relativepath`" width="`width`" alt="`caption`">
~~~
""";

$flavorData["Franklin", "FileVideoTemplate"] = StringTemplate @ StringTrim @  """
~~~
<video width="`width`" height="`height`" controls>
  <source src="`relativepath`" type="video/mp4">
</video>
~~~
""";

$flavorData["Franklin", "AnchorTemplate"] = StringTemplate @ """\label{``}""";

$flavorData["Franklin", "InlineMathTemplate"] = wrapDollar;

$flavorData["Franklin", "MultilineMathTemplate"] = wrapDoubleDollar;

$flavorData["Franklin", "MarkdownPostprocessor"] = franklinFixup;

$flavorData["Franklin", "InlineHTML"] = franklinInlineHTML;

franklinInlineHTML[a_] := StringJoin["~~~", a, "~~~"];

franklinFixup[str_String] = StringReplaceRepeated[str, $franklinFixes];

$franklinFixes = {
  "{{" -> "{ {"
}

(**************************************************************************************************)

$flavorData["Simple"] = <||>;

$flavorData["Simple", "RasterizationFunction"] = PNGRasterizationFunction;

$flavorData["Simple", "StringImageTemplate"] = None;

$flavorData["Simple", "FileImageTemplate"] = StringTemplate @ "![](`relativepath`)";

$flavorData["Simple", "FileVideoTemplate"] = StringTemplate @ "![](`relativepath`)";

$flavorData["Simple", "AnchorTemplate"] = "";

$flavorData["Simple", "InlineHTML"] = Function[""];

$flavorData["Simple", "InlineMathTemplate"] = wrapDollar;

$flavorData["Simple", "MultilineMathTemplate"] = wrapDoubleDollar;

$flavorData["Simple", "MarkdownPostprocessor"] = Identity;

(**************************************************************************************************)

$flavorData["Preview"] = $flavorData["Simple"];

$flavorData["Preview", "StringImageTemplate"] = Key["linearSyntax"];

$flavorData["Preview", "RasterizationFunction"] = WLLinearSyntaxRasterizationFunction;

(**************************************************************************************************)

$flavorData["IAWriter"] = $flavorData["Simple"];

$flavorData["IAWriter", "MultilineMathTemplate"] = wrapDoubleBrace;

(**************************************************************************************************)

$flavorData[None] = $flavorData["Simple"];

$flavorData[None, "FileImageTemplate"] = None;

$flavorData[None, "FileVideoTemplate"] = None;

MacroEvaluate @ UnpackAssociation[$flavorData[None], fileImageTemplate, fileVideoTemplate, anchorTemplate, inlineMathTemplate, multilineMathTemplate, markdownPostprocessor];

(**************************************************************************************************)

PackageExport["ToMarkdownString"]

PackageScope["$inlineMathTemplate"]

Options[ToMarkdownString] = $genericMarkdownOptions;

ToMarkdownString[spec_, opts:OptionsPattern[]] := Scope[
  setupMarkdownGlobals[];
  setupRasterizationPath[$TemporaryDirectory, ToFileName[$TemporaryDirectory, "wl_md_images"]];
  iToMarkdownString @ spec
];

iToMarkdownString[spec_] := Scope[
  lines = toMarkdownLines @ spec;
  If[!StringVectorQ[lines], $LastFailedMarkdownResult ^= lines; ReturnFailed[]];
  If[TrueQ @ includePrelude,
    lines = insertAtFirstNonheader[lines, {multilineMathTemplate @ $KatexPrelude}]];
  result = StringJoin @ {Riffle[lines, "\n\n"], "\n\n"};
  result = StringReplace[result, $tableCreation];
  StringTrim @ markdownPostprocessor @ result
];

$tableCreation = StartOfLine ~~ " "... ~~ text:Repeated["* " ~~ Except["\n"].. ~~ "\n\n", {2, Infinity}] :> createTable[text];

createTable[ostr_String] := Scope[
  str = StringTrim @ ostr;
  lines = StringDrop[StringSplit[str, "\n"..], 2];
  allowCompact = True;
  If[StringStartsQ[First @ lines, "META: "],
    {meta, lines} = FirstRest @ lines;
    allowCompact = StringFreeQ[meta, "WIDE"];
  ];
  If[Min[StringCount[DeleteCases["SPACER"] @ lines, "\t"..]] == 0,
    Return @ ostr];
  grid = StringTrim /@ StringSplit[lines, "\t"..];
  first = First @ grid;
  ncols = Length @ first;
  grid //= MatrixMap[StringReplace["\"" -> "'"]];
  grid //= MatrixReplace["**_**" -> ""];
  grid //= VectorReplace[{"SPACER"} :> ConstantArray["", ncols]];
  If[!MatrixQ[grid],
    Print["Bad table!"];
    Print["First row is: ", First @ grid];
    Print["Row lengths are: ", Length /@ grid];
  ];
  hasHeader = VectorQ[first, boldedQ];
  strikeRow = ConstantArray["---", ncols];
  If[markdownFlavor === "Franklin" && !hasHeader,
    prefix = "@@table-no-header\n";
    dummyRow = ConstantArray["z", ncols];
    grid = Join[{dummyRow, strikeRow}, grid];
    postfix = "@@\n"
  ,
    prefix = postfix = "\n";
    grid = Insert[grid, strikeRow, If[hasHeader, 2, 1]];
  ];
  tableRowStrings = Map[toTableRowString, grid];
  If[ncols > 3 && allowCompact,
    prefix = "@@table-compact\n" <> prefix; postfix = "@@\n" <> postfix];
  StringJoin[prefix, tableRowStrings, postfix]
];

toTableRowString[cols_] := StringJoin["| ", Riffle[cols, " | "], " |\n"];

boldedQ[str_] := StringMatchQ[str, Verbatim["**"] ~~ __ ~~ Verbatim["**"]];

insertAtFirstNonheader[lines_List, template_] := Scope[
  index = SelectFirstIndex[lines, !StringStartsQ[#, "#"] && !StringMatchQ[#, Whitespace]&, 1];
  Insert[lines, template, index]
];

(**************************************************************************************************)

General::badnbread = "Could not read notebook ``, will be replaced with placeholder.";

$notebookToMarkdownCache = Data`UnorderedAssociation[];
$notebookCaching = True;

toMarkdownLines[File[path_String]] /; FileExtension[path] === "nb" := Scope[
  fileDate = Quiet @ FileDate @ path;
  If[$notebookCaching,
    {cachedResult, cachedDate} = Lookup[$notebookToMarkdownCache, path, {None, None}];
    If[ListQ[cachedResult] && fileDate === cachedDate, Return @ cachedResult];
  ];
  nb = Quiet @ Get @ path;
  If[MatchQ[nb, Notebook[{__Cell}, ___]],
    result = toMarkdownLines @ nb;
    If[$notebookCaching, CacheTo[$notebookToMarkdownCache, path, {result, fileDate}]];
    result
  ,
    Message[General::badnbread, path];
    List["#### toMarkdownLines: placeholder for " <> path]
  ]
]


toMarkdownLines[nb_NotebookObject] :=
  toMarkdownLines @ NotebookGet @ nb;

(* TODO: add support for .m here, extracting usage messages *)
(* toMarkdownLines[] :=
  toMarkdownLines @ NotebookRead @ nb;
 *)

toMarkdownLines[list_List] :=
  Flatten[Riffle[Map[toMarkdownLines, list], "---"], 1];

toMarkdownLines[Notebook[cells_List, ___]] := Scope[
  $lastCaption = None; Map[outerCellToMarkdown, cells]
]

toMarkdownLines[cell_Cell] := Scope[
  $lastCaption = None;
  List @ outerCellToMarkdown @ cell
];

toMarkdownLines[e_] := List[
  "#### toMarkdownLines: unknown expression with head " <> TextString @ Head @ e
];

trimCells[cells_List] := Take[cells, All, UpTo @ 2];

(**************************************************************************************************)

$lastCaption = None;

$textCellP = "Section" | "Subsection" | "Subsubsection" | "Text";

$outputCaptionPattern = RowBox[{"(*", RowBox[{"CAPTION", ":", caption___}], "*)"}] :>
  StringJoin @ iTextCellToMDOuter @ RowBox[{caption}];

outerCellToMarkdown = Case[

  Cell[e_, style_, ___, CellTags -> tag_String, ___] :=
    Splice @ {anchorTemplate @ tag, cellToMarkdown @ Cell[e, style]};

  Cell[e_, style_, ___] :=
    cellToMarkdown @ Cell[e, style];
  
  Cell[CellGroupData[cells_List, Open]] :=
    Splice[outerCellToMarkdown /@ cells];

  Cell[_CellGroupData] :=
    Nothing;
];

cellToMarkdown = Case[

  Cell["Under construction.", _] := makeBannerMD @ "UNDER CONSTRUCTION";

  Cell[str_String /; StringStartsQ[str, "BANNER: "], _] := makeBannerMD @ StringTrim[str, "BANNER: "];

  Cell[e_, "Chapter"]            := StringJoin[headingDepth @ 0, textCellToMD @ e];
  Cell[e_, "Section"]            := StringJoin[headingDepth @ 1, textCellToMD @ e];
  Cell[e_, "Subsection"]         := StringJoin[headingDepth @ 2, textCellToMD @ e];
  Cell[e_, "Subsubsection"]      := StringJoin[headingDepth @ 3, textCellToMD @ e];

  Cell[e_, "Text"]               := insertLinebreaksOutsideKatex[textCellToMD @ e, 120];
  Cell[e_, "Item"]               := StringJoin["* ", textCellToMD @ e];
  Cell[e_, "Subitem"]            := StringJoin["\t* ", textCellToMD @ e];
  Cell[e_, "SubsubItem"]         := StringJoin["\t\t* ", textCellToMD @ e];
  Cell[b:BoxData[TemplateBox[_, _ ? textTagQ]], "Output"]
                                 := textCellToMD @ b;

  Cell[BoxData[t:TemplateBox[_, "VideoBox1", ___]], "Output"] := videoCellToMD @ t;

  e:Cell[_, "Output"]            := If[ContainsQ[e, "LinkHand"], Nothing, outputCellToMD @ e];

  Cell[e_, "Code"]               := (
    $lastCaption = First[Cases[e, $outputCaptionPattern], None];
    Nothing
  );

  c_ := (Nothing);
];

(* recognizes tags generated by StylesheetForms *)
textTagQ[tag_String] := StringEndsQ[tag, "Form" | "Symbol"];
textTagQ[_] := False;

headingDepth[n_] := StringRepeat["#", Max[n + headingDepthOffset, 1]] <> " ";

(**************************************************************************************************)

makeBannerMD[text_String] := makeBannerMD[text] = outputCellToMD @ makeBannerCell[text];

makeBannerCell[str_] := Cell[BoxData @ makeBannerGraphics[str, 5], "Output"];

makeBannerGraphics[str_, n_] := ToBoxes @
  Framed[
    Graphics[
      Text[Style[str, Bold, FontColor -> Black,
        FontFamily -> "Helvetica", FontSize -> 14]],
      Background -> RGBColor[0.9558361921359645, 0.8310111921713484, 0.0999649884717605, 1.],
      FrameStyle -> None,
      PlotRange -> {{-1, 1}, {-1, 1}/n},
      ImageSize -> {200, 200/n}
    ],
    ImageMargins -> 25, FrameStyle -> None
  ];

(**************************************************************************************************)

insertLinebreaksOutsideKatex[str_String, n_] := Scope[
  katexSpans = StringPosition[str, Verbatim["$"] ~~ Shortest[___] ~~ Verbatim["$"], Overlaps -> False];
  katexStrings = StringTake[str, katexSpans];
  If[katexSpans === {}, Return @ InsertLinebreaks[str, n]];
  str = InsertLinebreaks[StringReplacePart[str, "€", katexSpans], n];
  i = 1;
  StringReplace[str, "€" :> Part[katexStrings, i++]]
];

(**************************************************************************************************)

outputCellToMD[cell_] := rasterizeCell[cell, $lastCaption];

$rasterMetadataCache = Data`UnorderedAssociation[];

rasterizeCell[cell_, caption_:None] := Scope[

  If[rasterizationFunction === None, Return["#### Placeholder for image"]];

  rasterizationResult = rasterizationFunction @ cell;

  If[!AssociationQ[rasterizationResult],
    Print["RasterizationFunction did not return an association: ", Head @ rasterizationResult];
    Return["#### Invalid rasterization result"];
  ];

  rasterizationResult["caption"] = Replace[$lastCaption, None -> ""];

  markdown = Switch[rasterizationResult["type"],
    "String",
      stringImageTemplate @ rasterizationResult,
    "File",
      fileImageTemplate @ rasterizationResult,
    _,
      Print["RasterizationFunction returned invalid association."];
      "#### Invalid rasterization result"
  ];

  markdown
];

(**************************************************************************************************)

videoCellToMD[TemplateBox[{_, srcPath_, ___}, "VideoBox1", ___]] := Scope[
  If[!FileExistsQ[srcPath], Return @ "#### Missing video file"];
  videoDims = getVideoRasterSize @ srcPath;

  videoFileName = FileNameTake @ srcPath;

  videoPath = FileNameJoin[{rasterizationPath, videoFileName}];

  If[!$dryRun,
    EnsureDirectory[rasterizationPath];
    If[!FileExistsQ[videoPath], CopyFile[srcPath, videoPath]];
  ];
  vPrint["Copying file from ", srcPath, " to ", videoPath];

  {w, h} = Ceiling[videoDims / 2];
  relativePath = toEmbedPath[relativeRasterizationPath, videoFileName, videoPath];
  fileVideoTemplate @ Association[
    "width" -> w, "height" -> h,
    "videopath" -> videoPath,
    "relativepath" -> relativePath
  ]
]

videoCellToMD[_] := "#### Invalid Video"

$rasterSizeCache = <||>;

getVideoRasterSize[path_] := CacheTo[$rasterSizeCache, path, First @ Information[Video[path], "OriginalRasterSize"]];

(**************************************************************************************************)

PackageExport["WLLinearSyntaxRasterizationFunction"]

WLLinearSyntaxRasterizationFunction[cell_] := Association[
  "type" -> "String",
  "linearSyntax" -> ToString[RawBoxes @ Part[cell, 1, 1], StandardForm]
];

(**************************************************************************************************)

PackageExport["PNGRasterizationFunction"]

PNGRasterizationFunction[cell_] := Scope[

  If[rasterizationPath === None, Return @ None];

  (* did we already export this result in this session? *)
  cellHash = Base36Hash @ cell;
  cacheKey = {cellHash, relativeRasterizationPath, rasterizationPath};
  cacheValue = Lookup[$rasterMetadataCache, Key @ {cacheKey}, None];
  If[ListQ[cacheValue],
    (* this cache gives us enough info to generate the markdown without looking for the file on disk *)
    {imageDims, imageFileName, imagePath} = cacheValue;
    vPrint["Using cached metadata for ", imagePath];
    Goto[skipRasterization];
  ];

  If[!$dryRun, EnsureDirectory[rasterizationPath]];

  (* did we already export this result in a previous session? *)
  imagePath = First[FileNames[cellHash <> "_*_*.png", rasterizationPath], None];
  If[StringQ[imagePath],
    imageFileName = FileNameTake @ imagePath;
    imageDims = FromDigits /@ StringExtract[FileBaseName @ imagePath, "_" -> {3, 4}];
    vPrint["* Using cached image at ", File @ imagePath];
    Goto[skipRasterization]];

  (* rasterize *)
  If[$dryRun,
    imageFileName = StringJoin[cellHash, "_dummy.png"];
  ,
    image = Rasterize[cell, ImageFormattingWidth -> Infinity, ImageResolution -> 144];
    If[!ImageQ[image], ReturnFailed[]];
    imageHash = Base36Hash @ image;
    imageDims = ImageDimensions @ image;
    imageFileName = StringJoin[cellHash, "_", imageHash, "_", toDimsString @ imageDims, ".png"];
  ];

  imagePath = FileNameJoin[{rasterizationPath, imageFileName}];

  (* export *)
  If[!FileExistsQ[imagePath],
    vPrint["* Exporting image to ", File @ imagePath];
    If[!$dryRun, Export[imagePath, image, CompressionLevel -> 1]];
  ];

  (* create and return markdown *)
  Label[skipRasterization];

  width = Ceiling @ First[imageDims * 0.5];
  imageRelativePath = toEmbedPath[relativeRasterizationPath, imageFileName, imagePath];

  If[!$dryRun, $rasterMetadataCache[cacheKey] ^= {imageDims, imageFileName, imagePath}];

  Association[
    "type" -> "File",
    "imagepath" -> imagePath,
    "relativepath" -> imageRelativePath,
    "width" -> width
  ]
]

toEmbedPath[None, imageFileName_, imagePath_] := "file://" <> imagePath;
toEmbedPath[relative_, imageFileName_, _] := NormalizePath @ FileNameJoin[{relative, imageFileName}];

toDimsString[{w_, h_}] := StringJoin[IntegerString[w, 10, 4], "_", IntegerString[h, 10, 4]];

(**************************************************************************************************)

PackageScope["textCellToMD"]

(* Todo: introduce simple caching *)

$forbiddenStrings = "XXX" | "XXXX";

textCellToMD[e_] := Scope[
  text = StringTrim @ StringJoin @ iTextCellToMDOuter @ e;
  If[StringContainsQ[text, "\\badDispatch"], CellPrint @ Cell[e, "Text", Background -> RGBColor[1,0.95,0.95]]];
  If[StringContainsQ[text, $forbiddenStrings], Return[""]];
  text // StringReplace[$finalStringFixups1] // StringReplace[$finalStringFixups2]
]

$finalStringFixups1 = {
  "  $" -> " $",
  "$  " -> "$ "
};

makeFontSmallCaps[text_] := inlineHTML["<span style=\"font-variant: small-caps;\">" <> ToLowerCase[text] <> "</span>"];

$finalStringFixups2 = {
  "$ --" -> "$ --",
  "$ -" -> "$\\-",
  "$ ." -> "$.",
  "$ ," -> "$,",
  "$ ?" -> "$?",
  "$ !" -> "$!",
  "$ :" -> "$:",
  "$ *" -> "\,$ *",
  "\\text{\\,and\\,}" -> "$ and $", (* improves linebreaking *)
  "LHS" :> makeFontSmallCaps["LHS"],
  "RHS" :> makeFontSmallCaps["RHS"],
  "1'st" -> "$1^{\\textrm{st}}$",
  "2'nd" -> "$2^{\\textrm{nd}}$",
  "3'rd" -> "$3^{\\textrm{rd}}$",
  "n-ary" -> "$n$-ary",
  (d:DigitCharacter ~~ "'th") :> StringJoin["$", d, "^{\\textrm{th}}$"],
  "n'th" -> "$n^{\\textrm{th}}$",
  "i'th" -> "$i^{\\textrm{th}}$",
  l:("f"|"p") ~~ "-" ~~ r:("vert"|"edge"|"cardinal"|"quiver") :>
    StringJoin["_", l, "_\\-", r]
};

WhiteString = _String ? (StringMatchQ[Whitespace]);

iTextCellToMDOuter = Case[
  BoxData[box_] :=
    multilineMathTemplate @ boxesToKatexString @ box;
  TextData @ Cell[BoxData[box_FormBox], ___] :=
    multilineMathTemplate @ boxesToKatexString @ box;
  TextData @ Cell[BoxData[boxes:{Repeated[_FormBox | WhiteString]}], ___] :=
    multilineMathTemplate @ boxesToKatexString @ RowBox @ replaceIndentingNewlines @ boxes;
  other_ :=
    iTextCellToMD @ other;
];

replaceIndentingNewlines[boxes_] :=
  VectorReplace[boxes, s_String :> StringReplace[s, "\[IndentingNewLine]"|"\n" -> "\\\\\n"]];

PackageScope["iTextCellToMD"]

iTextCellToMD = Case[
  str_String :=
    str;
  list_List :=
    Map[%, list];
  TextData[e_] :=
    % @ e;
  RowBox[e_List] :=
    % @ e;
  Cell[BoxData[boxes_, ___], ___] :=
    inlineMathTemplate @ boxesToKatexString @ RowBox @ ToList @ boxes;
  Cell[TextData[text_, ___], ___] :=
    % @ text;
  StyleBox[str_String /; StringMatchQ[str, Whitespace], ___] :=
    " ";
  ButtonBox[title_String, BaseStyle -> "Hyperlink", ButtonData -> {URL[url_String], _}, ___] :=
    StringJoin["[", % @ title, "](", url, ")"];
  StyleBox[boxes_, opts___] := Scope[
    {weight, slant, color} = Lookup[{opts}, {FontWeight, FontSlant, FontColor}, None];
    styledMD[% @ boxes, weight === "Bold", slant === "Italic"]
  ];
];

styledMD[e_, False, False] := e;
styledMD[e_, False, True] := wrapWith[e, "*"];
styledMD[e_, True, False] := wrapWith[e, "**"];
styledMD[e_, True, True] := wrapWith[e, "***"];

wrapWith[e_, wrap_] := Scope[
  e = StringJoin @ e;
  p1 = StringStartsQ[e, Whitespace];
  p2 = StringEndsQ[e, Whitespace];
  {If[p1, " ", {}], wrap, StringTrim @ e, wrap, If[p2, " ", {}]}
];
