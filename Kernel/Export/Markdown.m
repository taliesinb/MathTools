PublicFunction[ExportToMarkdown]

Options[ExportToMarkdown] = $genericExportMarkdownOptions;

PrivateVariable[$dryRun, $baseImportPath, $baseExportPath]

$dryRun = False;

(* avoids a problem with GIFImageQuantize running the first time on a fresh kernel *)
initPNGExport[] := (Clear[initPNGExport]; Quiet @ ExportString[ConstantImage[1, {2, 2}], "PNG", CompressionLevel -> 1]);

ExportToMarkdown::badcpp = "Bad CollatedPagePath ``."
ExportToMarkdown::expdirne = "Export directory `` does not exist."

$includeFrontMatter = False;
$frontMatterFunction = None;

ExportToMarkdown[importSpec_, exportSpec_, OptionsPattern[]] := Scope @ CatchMessage[
  
  UnpackOptions[
    $baseImportPath, $baseExportPath, $dryRun, $verbose,
    $includeFrontMatter, $frontMatterFunction,
    importPath, exportPath, maxItems, collatedPagePath
  ];
  $dryRun = $dryRun =!= False;
  SetAutomatic[$verbose, $dryRun];
  If[$verbose === "KeyModifiers", $verbose = ModifierKeysPressedQ[]];
  
  $baseImportPath //= NormalizePath;
  $baseExportPath //= NormalizePath;

  setupMarkdownGlobals[];
  initPNGExport[];

  {ipath, opath} = resolveImportExportPaths[importSpec, importPath, exportSpec, exportPath];
  inputs = parseInputSpec @ ipath;
  
  If[ListQ[inputs] && maxItems =!= Infinity,
    inputs = If[IntegerQ[maxItems], Take[inputs, UpTo @ maxItems], Take[inputs, maxItems]]];
  
  outputDir = If[StringEndsQ[opath, ".md"], FileNameDrop @ opath, opath];
  If[FileType[outputDir] =!= Directory,
    If[StringStartsQ[outputDir, $TemporaryDirectory], EnsureDirectory @ outputDir,
      ThrowMessage["expdirne", MsgPath @ outputDir]]];

  setupRasterizationPath[outputDir, "OutputImages"];
  mdvPrint["Rasterizing to ", MsgPath @ $rasterizationPath, " embedded as \"", $rasterizationURL, "\""];

  If[StringQ[$katexPreludePath],
    preludePath = NormalizePath @ $katexPreludePath;
    If[Not @ AbsolutePathQ @ preludePath,
      preludePath = ToFileName[outputDir, preludePath]];
    mdvPrint["Writing prelude to ", MsgPath @ preludePath, "."];
    prelude = $KatexPrelude;
    If[StringEndsQ[preludePath, ".md"],
      prelude = $markdownPostprocessor @ $multilineMathTemplate @ $katexPostprocessor @ prelude];
    ExportUTF8[preludePath, StringReplace[prelude, "\n" -> " "]];
  ];

  fileList = doImportExport[inputs, opath, StringEndsQ[opath, ".md"]];

  If[StringQ[collatedPagePath],
    Which[
      !StringEndsQ[collatedPagePath, ".md"], ReturnFailed["badcpp", collatedPagePath],
      AbsolutePathQ[collatedPagePath], Null,
      DirectoryQ[opath], collatedPagePath = ToFileName[opath, collatedPagePath],
      True, ReturnFailed["badcpp", collatedPagePath]
    ];
    doImportExport[inputs, collatedPagePath, True];
  ];

  mdvPrint["Wrote files: ", MsgPath @ fileList];
  
  fileList
];

(**************************************************************************************************)

ExportToMarkdown::badinspec = "Import specification should be a directory, file, notebook, or CellGroup."

parseInputSpec = Case[
  nb_NotebookObject                    := nb;
  c_CellObject | c_CellGroup           := c;
  path_String ? DirectoryQ             := enumerateFiles["*.nb", path];
  path_String ? (StringContainsQ["*"]) := enumerateFiles[path];
  path_String                          := File[path];
  other_                               := ThrowMessage["badinspec", other];
]

ExportToMarkdown::emptynbdir = "Import directory `` does not contain any notebooks."

enumerateFiles[spec___, path_] := Scope[
  files = FileNames[spec, path, Infinity] // Select[StringFreeQ["XXX"]];
  files = File /@ files;
  If[files === {}, ThrowMessage["emptynbdir", MsgPath @ path]];
  files
];

(**************************************************************************************************)

$notebookP = _File | _NotebookObject | _CellGroup | _Cell;

ExportToMarkdown::msgs = "Messages issued during export of ``."

(* this joins several notebooks into a single file *)
doImportExport[spec:($notebookP | {$notebookP..}), exportPath_, True] := Scope[
  If[$notebookCaching && MatchQ[spec, _File],
    If[getMarkdownUnixTime[exportPath] === UnixTime[FileDate[First @ spec]],
      mdvPrint["* Skipping unchanged notebook ", MsgPath @ spec];
      Return @ exportPath;
    ];
  ];
  dbgSpec = If[Head[spec] === File, MsgPath @@ spec, spec];
  mdvPrint["* Exporting ", dbgSpec, " to ", MsgPath @ exportPath];
  Check[
    result = toMarkdownStringInner @ spec,
    Message[ExportToMarkdown::msgs, dbgSpec];
  ];
  If[!StringQ[result], ThrowMessage["nbimportfail", spec]];
  If[$includeFrontMatter && MatchQ[spec, _NotebookObject | _File],
    frontMatter = Developer`ToJSON @ NotebookFrontMatter @ spec;
    frontMatter //= StringReplace["\\/" -> "/"]; (* weird bug in ToJSON *)
    result = StringJoin[frontMatter, "\n\n", result];
  ];
  If[$dryRun, exportPath, ExportUTF8[exportPath, result]]
];

doImportExport[single:$notebookP, exportPath_, False] :=
  doImportExport[single, toExportFileName[exportPath, single], True];

doImportExport[multi:{$notebookP..}, exportPath_, False] :=
  doImportExport[#, exportPath, False]& /@ multi;

ExportToMarkdown::nbimportfail = "Failed to convert notebook or notebooks ``."
doImportExport[spec_, _, _] := ThrowMessage["nbimportfail", spec];


(**************************************************************************************************)

PrivateFunction[toExportFileName]

General::nbrelpath = "Could not express path `` relative to BaseImportPath of ``."
General::badexppf = "Output of ExportPathFunction was not a string."

toExportFileName[exportPath_, obj_] := Scope[
  nbPath = getNotebookPath @ obj;
  mdFileName = filenameToMDFileName @ FileBaseName @ nbPath;
  If[StringQ[$baseImportPath],
    relPath = RelativePath[$baseImportPath, FileNameDrop @ nbPath];
    If[!StringQ[relPath], ThrowMessage["nbrelpath", MsgPath @ nbPath, MsgPath @ $baseImportPath]];
    dirPrefix = ToLowerCase @ relPath;
    If[dirPrefix =!= "", mdFileName = FileNameJoin[{dirPrefix, mdFileName}]];
  ];
  If[$exportPathFunction =!= None,
    mdFileName //= $exportPathFunction;
    If[!StringQ[mdFileName], ThrowMessage["badexppf"]];
  ];
  FileNameJoin[{exportPath, mdFileName}]
];

getNotebookPath = Case[
  nb_NotebookObject                 := NotebookFileName @ nb;
  nb_Notebook                       := "Untitled.nb";
  CellGroup[{c_, ___}, ___]         := % @ c;
  co_CellObject                     := % @ ParentNotebook @ co;
  None                              := "Untitled.nb";
  File[file_] | file_String         := file;
];

filenameToMDFileName = Case[
  ""         := "Untitled.md";
  path_      := titleToURL[path] <> ".md";
];

(**************************************************************************************************)

ExportToMarkdown::noexportwc = "Export path `` cannot contain wildcards."
ExportToMarkdown::neimportpath = "Import specification `` does not correspond to an existing file or directory."

resolveImportExportPaths[ispec_, ipath_, ospec_, opath_] := Scope[
  i = resolvePath["Import", ispec, ipath];
  o = resolvePath["Export", ospec, opath];
  If[StringQ[i] && !FileExistsQ[i], ThrowMessage["neimportpath", MsgPath @ i]];
  If[StringContainsQ[o, "*"], ThrowMessage["noexportwc", MsgPath @ o]];
  {i, o}
]

(**************************************************************************************************)

ioTypeSymbol = <|"Import" -> ImportPath, "Export" -> ExportPath|>;

ExportToMarkdown::noautopath = "`` path specification of Automatic requires `` to be set."
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

ExportToMarkdown::badrelpath = "Relative `` path specification `` requires `` to be set."
resolvePath[type_, path_String, None] :=
  ThrowMessage["badrelpath", ToLowerCase @ type, MsgPath @ path, ioTypeSymbol @ type];

resolvePath["Import", nb_NotebookObject, base_] :=
  nb;

resolvePath["Import", c_CellObject | c_CellGroup, base_] :=
  c;

ExportToMarkdown::badpathspec = "`` path specification `` does not appear to be valid."
resolvePath[type_, path_, base_] :=
  ThrowMessage["badpathspec", type, path];

$ioFileExt = <|"Import" -> "" | "nb", "Export" -> "" | "md"|>;

ExportToMarkdown::notmdext = "`` file specification `` does not end in \".md\"."
checkFileExt[type_][file_] := If[
  MatchQ[FileExtension @ file, $ioFileExt @ type], file,
  ThrowMessage["notmdext", type, MsgPath @ file]
];
