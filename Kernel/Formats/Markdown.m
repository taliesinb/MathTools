PublicFunction[CreateMarkdownTable]

CreateMarkdownTable[grid_List, headings_List] := Scope[
  columns = Transpose @ Pre[grid, headings];
  widths = Max[SLen[#]]& /@ columns;
  columns = MapThread[SPadRight, {columns, widths}];
  columns = MatrixMap[SJoin[" ", #, " "]&, columns];
  columns = MapThread[Insert[#, SRepeat["-", #2 + 2], 2]&, {columns, widths}];
  rows = Transpose @ columns;
  rows = Map[row |-> SJoin[App["|"] @ Map[{"|", #}&, row]], rows];
  SRiffle[rows, "\n"]
];

(**************************************************************************************************)

PublicIOFunction[ExportToMarkdown]

SetUsage @ "
ExportToMarkdown[inputdir$, opts$$] converts all notebooks in inputdir$ to markdown files that are written to disk.
ExportToMarkdown[inputfile$, opts$$] converts input notebook to markdown and writes it to disk.
ExportToMarkdown[notebook$, opts$$] converts a notebook object.
* A list of markdown files written is returned.
* The following options are supported:
| %BaseNotebookPath | Automatic | the directory that determines relative paths for input files, defaults to location of input |
| %BaseExportPath | Automatic | the directory that determines how relative output paths are interpreted, defaults to a fresh temporary directory |
| %MarkdownPath | 'markdown' | the directory to write markdown material to |
| %ExportPathFunction | None | function that determines the path to write markdown files to |
| %NotebookCaching | False | whether to skip converting notebook files that have already been processed and haven't changed |
| %KatexPreludePath | 'katex.tex' | the path to write katex definitions to |
| %DryRun | False | don't actually write any files |
| %Verbose | Automatic | print all actions |
| %RasterizationPath | 'raster' | the directory to write rasterized images and video to |
| %RasterizationURL | 'raster' | the URL fragment to use for output images and video |
| %MaxItems | limit the number of items written |
| %IncludeFrontMatter | if True, will embed front matter into written markdown files |
| %EmbedKatexPrelude | if True, will embed Katex prelude directly in markdown, if 'Link', will be linked externally |
| %FrontMatterFunction | function to synthesize additional front matter data to be written into markdown files |
* %ExportPathFunction
* %RasterizationPath, %MarkdownPath, %KatexPreludePath can be relative (resolved relative to %BaseExportPath), or absolute.
* %BaseExportPath -> Automatic will cause a fresh temporary directory to be created whenever any of the rasterization, markdown, or katex output paths are relative.
* If %KatexPreludePath ends in '.md', the Katex prelude will be embedded in markdown and written to the path. If it ends in '.tex', it will be written verbatim.
"

Options[ExportToMarkdown] = $genericExportMarkdownOptions;

$dryRun = False;

ExportToMarkdown::expdirne = "Export directory `` does not exist."
ExportToMarkdown::badinspec = "Import specification should be an existing directory, file, or NotebookObject."
ExportToMarkdown::emptyinp = "No files found to export."

$includeFrontMatter = False;
$frontMatterFunction = None;

ExportToMarkdown[inputSpec_, opts:OptionsPattern[]] := Scope @ CatchMessage[
  
  UnpackOptions[
    $baseNotebookPath, $baseExportPath, $markdownPath, $katexPreludePath,
    $exportPathFunction, $notebookCaching,
    maxItems, $verbose, $dryRun
  ];

  $dryRun = $dryRun =!= False;
  SetAuto[$verbose, $dryRun];
  VPrint["ExportToMarkdown[\n", "  ", ToPrettifiedString @ inputSpec, ", ", STake[ToPrettifiedString @ {DeleteDuplicateOptionKeys @ opts}, 2;;-2], "]"];
  If[$verbose === "KeyModifiers", $verbose = ModifierKeysPressedQ[]];
  
  (* input will be a NotebookObject or list of File objects *)
  {input, inputPath} = Match[inputSpec,
    nb_NotebookObject                                                  :> {nb, NotebookDirectory @ nb},
    co_CellObject                                                      :> {co, NotebookDirectory @ ParentNotebook @ co},
    (path_Str | File[path_Str]) ? DirectoryQ                     :> {enumerateFiles["*.nb", path], path},
    (path_Str | File[path_Str]) /; FileExtension[path] === "nb"  :> {File @ path, DirectoryName @ path},
    _                                                                  :> ReturnFailed["badinspec"]
  ];

  SetAuto[$baseNotebookPath, inputPath];
  $baseNotebookPath //= NormalizePath;

  (* MarkdownFlavor, RastizationPath, RastizerationURL, RasterizationCaching, etc. *)
  setupMarkdownGlobals[];
  SetAuto[$rasterizationPath, "raster"];
  SetAuto[$markdownPath, "markdown"];
  SetAuto[$katexPreludePath, "katex.tex"];

  (* when BaseExportPath unspecified, only bother to create the temporary directory if one of the output paths is specified as relative, because otherwise BEP has no utilitiy *)
  SetAuto[$baseExportPath, If[AnyTrue[{$rasterizationPath, $markdownPath, $katexPreludePath}, RelativePathQ], CreateDirectory[], None]];
  $rasterizationPath //= ToAbsolutePath[$baseExportPath];
  $markdownPath      //= ToAbsolutePath[$baseExportPath];
  $katexPreludePath  //= ToAbsolutePath[$baseExportPath];

  If[ListQ[input] && maxItems =!= Inf,
    input = If[IntQ[maxItems], Take[input, UpTo @ maxItems], Take[input, maxItems]]];
  If[input === {}, ReturnFailed["emptyinp"]];

  VPrint["Rasterizing to ", MsgPath @ $rasterizationPath, " embedded as \"", $rasterizationURL, "\""];
  ensureDirectory[{$rasterizationPath, $markdownPath, If[StrQ[$katexPreludePath], FileNameDrop @ $katexPreludePath, None]}];

  If[StrQ[$katexPreludePath],
    VPrint["Writing prelude to ", MsgPath @ $katexPreludePath, "."];
    prelude = $KatexPrelude;
    If[SEndsQ[$katexPreludePath, ".md"],
      prelude = $markdownPostprocessor @ $multilineMathTemplate @ $katexPostprocessor @ prelude];
    whenWet[
      EnsureDirectory @ FileNameDrop @ $katexPreludePath;
      ExportUTF8[$katexPreludePath, SRep[prelude, "\n" -> " "]];
    ];
  ];

  VPrint["Writing markdown files to: ", MsgPath @ $markdownPath];
  fileList = Map[exportItem, ToList @ input];

  VPrint["Wrote files: ", MsgPath @ fileList];
  
  fileList
];

(**************************************************************************************************)

ensureDirectory[list_List] := Scan[ensureDirectory, list];
ensureDirectory[dir_Str] :=
  If[!DirectoryQ[dir],
    VPrint["Directory ", MsgPath @ dir, " does not exist, creating."];
    whenWet @ EnsureDirectory @ dir;
  ];

enumerateFiles[spec___, path_] := Scope[
  files = FileNames[spec, path, Inf] // Select[SFreeQ["XXX"]];
  files = File /@ files;
  If[files === {}, Msg::emptynbdir[MsgPath @ path]];
  files
];

(**************************************************************************************************)

exportItem[item_] := exportItemTo[item, itemMarkdownPath @ item];

(**************************************************************************************************)

ExportToMarkdown::msgs = "Messages issued during export of ``."

notebookAlreadyExportedQ[nbPath_, mdPath] := And[
  $notebookCaching,
  getMarkdownUnixTime[mdPath] === UnixTime[FileDate @ nbPath]
];

exportItemTo[File[nbPath_], mdPath_] /; notebookAlreadyExportedQ[nbPath, mdPath] := (
  VPrint["* Skipping unchanged notebook ", MsgPath @ nbPath];
  Nothing
);

General::nbmdfail = "Cannot convert notebook at `` to markdown."

exportItemTo[item_, mdPath_] := Scope[
  dbgSpec = If[H[item] === File, MsgPath @ item, item];
  VPrint["* Exporting ", dbgSpec, " to ", MsgPath @ mdPath];
  Check[
    result = toMarkdownStringInner @ item,
    Message[ExportToMarkdown::msgs, dbgSpec];
  ];
  If[!StrQ[result], Msg::nbmdfail[dbgSpec]];
  If[$dryRun, mdPath, ExportUTF8[mdPath, result]]
];

(**************************************************************************************************)

PrivateFunction[itemMarkdownPath]

General::badexppf = "Output of ExportPathFunction was not a string."
General::badnbpath = "There is no path associated with the given notebook."

itemMarkdownPath[item_] := Scope[
  nbPath = ToNotebookPath @ item;
  If[FailureQ[nbPath], ThrowFailure["badnbpath"]];
  mdFileName = SJoin[titleToURL @ FileBaseName @ nbPath, ".md"];

  (* if there is a BaseNotebookPath set, we will extract the difference between item's path and it,
  and use this fragment as a prefix for the output path *)
  If[StrQ[$baseNotebookPath] && $baseNotebookPath =!= "",
    relPath = RelativePath[$baseNotebookPath, FileNameDrop @ nbPath];
    relPath //= SubNone[""]; (* paths outside $baseNotebookPath will be put at top level *)
    mdFileName = PathJoin[ToLowerCase @ relPath, mdFileName];
  ];

  If[$exportPathFunction =!= None,
    mdFileName //= $exportPathFunction;
    If[!StrQ[mdFileName], ThrowMessage["badexppf"]];
  ];

  PathJoin[$markdownPath, mdFileName]
];
