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
  MarkdownFlavor -> "Simple",
  RasterizationPath -> Automatic,
  DisplayProgress -> False,
  HeadingDepthOffset -> 0,
  IncludePrelude -> True,
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
  flavorFields = Lookup[$flavorData, markdownFlavor, ReturnFailed[]];
  UnpackAssociation[flavorFields, imageTemplate, inlineMathTemplate, multilineMathTemplate, markdownPostprocessor];
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

General::expdirne = "Export directory `` does not exist."

Options[ExportToMarkdown] = JoinOptions[
  MaxItems -> Infinity,
  $genericMarkdownExportOptions
];

$dryRun = False;
ExportToMarkdown[importSpec_, exportSpec_, OptionsPattern[]] := Scope @ CatchMessage[
  UnpackOptions[importPath, exportPath, dryRun, verbose, maxItems, includePrelude];
  $dryRun = dryRun =!= False;
  SetAutomatic[verbose, $dryRun];
  $verbose = verbose;
  setupMarkdownGlobals[];
  {ipath, opath} = resolveImportExportPaths[importSpec, importPath, exportSpec, exportPath];
  iexpr = parseInputSpec @ ipath;
  If[ListQ[iexpr] && maxItems =!= Infinity,
    iexpr = If[IntegerQ[maxItems], Take[iexpr, UpTo @ maxItems], Take[iexpr, maxItems]]];
  outputDir = If[StringEndsQ[opath, ".md"], FileNameDrop @ opath, opath];
  If[FileType[outputDir] =!= Directory, ThrowMessage["expdirne", outputDir]];
  setupRasterizationPath[outputDir, "OutputImages"];
  If[$verbose,
    Print["Rasterizing to \"", rasterizationPath, "\" embedded as \"", relativeRasterizationPath, "\""]];
  If[StringQ[includePrelude],
    preludePath = NormalizePath @ includePrelude;
    If[Not @ AbsolutePathQ @ preludePath,
      preludePath = ToFileName[outputDir, preludePath]];
    If[$verbose, Print["Writing prelude to \"", preludePath, "\"."]];
    ExportUTF8[preludePath, StringReplace[markdownPostprocessor @ $KatexPrelude, "\n" -> " "]];
  ];
  doImportExport[iexpr, opath, StringEndsQ[opath, ".md"]]
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

General::nbimportfail = "Failed to convert notebook or notebooks ``."
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

PackageExport["ExportNavigationPage"]

ExportNavigationPage[files_, relativePrefix_String, navPath_] := Scope[
  If[StringQ[files],
    If[FileType[files] =!= Directory, ReturnFailed[]];
    files = FileNames["*.md", files];
  ];
  titles = getFileTitle /@ files;
  If[!StringVectorQ[titles], ReturnFailed[]];
  fileNames = FileBaseName /@ files;
  navData = MapThread[
    <|"path" -> StringJoin[relativePrefix, #1], "title" -> toNavTitle[#2]|>&,
    {fileNames, titles}
  ];
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
  FileTemplateApply[navTemplate, {navData}, navOutputPath]
];

toUnicode[str_] := FromCharacterCode[ToCharacterCode[str], "UTF8"];

getFileTitle[path_] := StringTrim @ StringDelete[toUnicode @ First @ FileLines[path, 1], "#"];

toNavTitle[e_] := StringReplace[e, " and " -> " &amp; "];

(**************************************************************************************************)

$flavorData = <||>;

wrapDollar[e_] := StringJoin["$", e, "$"];
wrapDoubleDollar[e_] := StringJoin["$$\n", e, "\n$$"];
wrapDoubleBrace[e_] := StringJoin["\\\\[\n", e, "\n\\\\]"];

(**************************************************************************************************)

$flavorData["Franklin"] = <||>;

$flavorData["Franklin", "ImageTemplate"] = StringTemplate @ StringTrim @  """
~~~
<img src="`relativepath`" width="`width`">
~~~
""";

$flavorData["Franklin", "InlineMathTemplate"] = wrapDollar;

$flavorData["Franklin", "MultilineMathTemplate"] = wrapDoubleDollar;

$flavorData["Franklin", "MarkdownPostprocessor"] = franklinFixup;

franklinFixup[str_String] = StringReplaceRepeated[str, $franklinFixes];

$franklinFixes = {
  "{{" -> "{ {"
}

(**************************************************************************************************)

$flavorData["Simple"] = <||>;

$flavorData["Simple", "ImageTemplate"] = StringTemplate @ "![](`relativepath`)";

$flavorData["Simple", "InlineMathTemplate"] = wrapDollar;

$flavorData["Simple", "MultilineMathTemplate"] = wrapDoubleDollar;

$flavorData["Simple", "MarkdownPostprocessor"] = Identity;

(**************************************************************************************************)

$flavorData["IAWriter"] = $flavorData["Simple"];

$flavorData["IAWriter", "MultilineMathTemplate"] = wrapDoubleBrace;

(**************************************************************************************************)

$flavorData[None] = $flavorData["Simple"];

$flavorData[None, "ImageTemplate"] = None;

MacroEvaluate @ UnpackAssociation[$flavorData[None], imageTemplate, inlineMathTemplate, multilineMathTemplate, markdownPostprocessor];

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
  If[!StringVectorQ[lines], ReturnFailed[]];
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
  If[Min[StringCount[lines, "\t"..]] == 0, Return @ ostr];
  grid = StringTrim /@ StringSplit[lines, "\t"..];
  If[!MatrixQ[grid], Print["Bad table"]];
  first = First @ grid;
  ncols = Length @ first;
  strikeRow = ConstantArray["---", ncols];
  If[markdownFlavor === "Franklin",
    dummyRow = ConstantArray["z", ncols];
    grid = Join[{dummyRow, strikeRow}, grid];
  ,
    grid = Insert[grid, strikeRow, If[VectorQ[first, boldedQ], 1, 0]];
  ];
  StringJoin @ {Map[toTableRowString, grid], "\n\n"}
];

toTableRowString[cols_] := StringJoin["| ", Riffle[cols, " | "], " |\n"];

boldedQ[str_] := StringMatchQ[str, "**" ~~ __ ~~ "**"];

insertAtFirstNonheader[lines_List, template_] := Scope[
  index = SelectFirstIndex[lines, !StringStartsQ[#, "#"] && !StringMatchQ[#, Whitespace]&, 1];
  Insert[lines, template, index]
];

(**************************************************************************************************)

General::badnbread = "Could not read notebook ``, will be replaced with placeholder.";

toMarkdownLines[File[path_String]] /; FileExtension[path] === "nb" := Scope[
  nb = Quiet @ Get @ path;
  If[MatchQ[nb, Notebook[{__Cell}, ___]],
    toMarkdownLines @ nb
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

toMarkdownLines[Notebook[cells_List, ___]] :=
  Map[cellToMarkdown, trimCells @ cells]

toMarkdownLines[c_Cell] := List @ cellToMarkdown @ Take[c, UpTo @ 2];

toMarkdownLines[e_] := List[
  "#### toMarkdownLines: unknown expression with head " <> TextString @ Head @ e
];

trimCells[cells_List] := Take[cells, All, UpTo @ 2];

(**************************************************************************************************)

$textCellP = "Section" | "Subsection" | "Subsubsection" | "Text";
cellToMarkdown = Case[

  Cell[e_, "Chapter"]            := StringJoin[headingDepth @ 0, textCellToMD @ e];
  Cell[e_, "Section"]            := StringJoin[headingDepth @ 1, textCellToMD @ e];
  Cell[e_, "Subsection"]         := StringJoin[headingDepth @ 2, textCellToMD @ e];
  Cell[e_, "Subsubsection"]      := StringJoin[headingDepth @ 3, textCellToMD @ e];
  Cell[e_, "Text"]               := textCellToMD @ e;
  Cell[e_, "Item"]               := StringJoin["* ", textCellToMD @ e];
  Cell[e_, "Subitem"]            := StringJoin["\t* ", textCellToMD @ e];
  Cell[e_, "SubsubItem"]         := StringJoin["\t\t* ", textCellToMD @ e];
  Cell[b:BoxData[TemplateBox[_, _ ? textTagQ]], "Output"]
                                 := textCellToMD @ b;
  e:Cell[_, "Output"]            := outputCellToMD @ e;

  Cell[CellGroupData[cells_List, Open]] := Splice[% /@ trimCells[cells]];

  c_ := (Nothing);
];

(* recognizes tags generated by StylesheetForms *)
textTagQ[tag_String] := StringEndsQ[tag, "Form" | "Symbol"];
textTagQ[_];

headingDepth[n_] := StringRepeat["#", Max[n + headingDepthOffset, 1]] <> " ";

(**************************************************************************************************)

outputCellToMD[cell_] := rasterizeCell @ cell

$rasterMetadataCache = Data`UnorderedAssociation[];

rasterizeCell[cell_] := Scope[

  If[rasterizationPath === None || imageTemplate === None,
    Return["#### Placeholder for image"]];

  (* did we already export this result in this session? *)
  cellHash = Base36Hash @ cell;
  cacheKey = {cellHash, relativeRasterizationPath, rasterizationPath};
  cacheValue = Lookup[$rasterMetadataCache, Key @ {cacheKey}, None];
  If[ListQ[cacheValue],
    (* this cache gives us enough info to generate the markdown without looking for the file on disk *)
    {imageDims, imageFileName, imagePath} = cacheValue;
    If[$verbose, Print["Using cached metadata for ", imagePath]];
    Goto[skipRasterization];
  ];

  If[!$dryRun, EnsureDirectory[rasterizationPath]];

  (* did we already export this result in a previous session? *)
  imagePath = First[FileNames[cellHash <> "_*_*.png", rasterizationPath], None];
  If[StringQ[imagePath],
    imageFileName = FileNameTake @ imagePath;
    imageDims = FromDigits /@ StringExtract[FileBaseName @ imagePath, "_" -> {3, 4}];
    If[$verbose, Print["* Using cached image at ", imagePath]];
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
    If[$verbose, Print["* Exporting image to ", imagePath]];
    If[!$dryRun, Export[imagePath, image, CompressionLevel -> 1]];
  ];

  (* create and return markdown *)
  Label[skipRasterization];
  width = Ceiling @ First[imageDims * 0.5];
  imageRelativePath = toEmbedPath[relativeRasterizationPath, imageFileName, imagePath];
  markdown = imageTemplate[<|"imagepath" -> imagePath, "relativepath" -> imageRelativePath, "width" -> width|>];
  
  If[!$dryRun, $rasterMetadataCache[cacheKey] ^= {imageDims, imageFileName, imagePath}];

  markdown
];

toEmbedPath[None, imageFileName_, imagePath_] := "file://" <> imagePath;
toEmbedPath[relative_, imageFileName_, _] := NormalizePath @ FileNameJoin[{relative, imageFileName}];

toDimsString[{w_, h_}] := StringJoin[IntegerString[w, 10, 4], "_", IntegerString[h, 10, 4]];

(**************************************************************************************************)

PackageScope["textCellToMD"]

(* Todo: introduce simple caching *)

$forbiddenStrings = "XXX" | "XXXX";

textCellToMD[e_] := Scope[
  text = StringTrim @ StringJoin @ iTextCellToMDOuter @ e;
  If[StringContainsQ[text, $forbiddenStrings], Return[""]];
  text // StringReplace[$finalStringFixups1] // StringReplace[$finalStringFixups2]
]

$finalStringFixups1 = {
  "  $" -> " $",
  "$  " -> "$ "
};

$finalStringFixups2 = {
  "$ -" -> "$\\-",
  "$ ." -> "$.",
  "$ ," -> "$,",
  "$ ?" -> "$?",
  "$ !" -> "$!",
  "$ :" -> "$:"
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
  Replace[boxes, s_String :> StringReplace[s, "\[IndentingNewLine]"|"\n" -> "\\\\\n"], {1}];

PackageScope["iTextCellToMD"]

iTextCellToMD = Case[
  str_String :=
    str;
  list_List :=
    Map[%, list];
  TextData[e_] :=
    % @ e;
  Cell[BoxData[boxes_, ___], ___] :=
    inlineMathTemplate @ boxesToKatexString @ RowBox @ ToList @ boxes;
  Cell[TextData[text_, ___], ___] :=
    % @ text;
  StyleBox[str_String /; StringMatchQ[str, Whitespace], ___] :=
    " ";
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

(**************************************************************************************************)

PackageExport["PreviousTextCell"]

PreviousTextCell[] := Scope[
  cellExpr = NotebookRead @ PreviousCell[];
  If[Head[cellExpr] =!= Cell, ThrowFailure["exportmdnocell"]];
  cellType = Replace[cellExpr, {Cell[_, type_String, ___] :> type, _ :> $Failed}];
  If[!MatchQ[cellType, "Text"], ReturnFailed[]];
  Take[cellExpr, 2]
];
