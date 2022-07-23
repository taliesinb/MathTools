PublicFunction[BuildQGSiteFranklin]

$QGSiteImportPath = NormalizePath @ "~/Dropbox/QG";
$QGSiteFranklinExportPath = NormalizePath @ "~/sites/qg-franklin";

Options[BuildQGSite] = $genericExportMarkdownOptions;

BuildQGSiteFranklin[opts:OptionsPattern[]] := BuildQGSiteFranklin[All, opts];

BuildQGSiteFranklin[src:Except[_Rule], opts:OptionsPattern[]] := Scope[
  SetAll[src, $QGSiteImportPath];
  files = ExportToMarkdown[
    src, File[$QGSiteFranklinExportPath],
    MarkdownFlavor -> "Franklin",
    opts,
    BaseImportPath -> $QGSiteImportPath,
    KatexPreludePath -> "_layout/prelude.md",
    EmbedPreludeLink -> False,
    NotebookCaching -> True,
    HeadingDepthOffset -> 1,
    RasterizationPath -> {"_assets/output", "/assets/output/"}
  ];
  If[!StringOrStringVectorQ[files], ReturnFailed[]];
  ExportNavigationPage[$QGSiteTargetPath, "/", "/Users/tali/sites/qg-franklin/_layout/nav.template.html"];
  first = First[files, files];
  If[StringQ[first], FocusWebpage["http://localhost:8000/chapters/" <> FileBaseName @ first]];
  files
];

(**************************************************************************************************)

PublicFunction[BuildQGSite]

Options[BuildQGSite] = $genericExportMarkdownOptions;

$QGSiteExportPath = NormalizePath @ "~/sites/qg-hugo/content";

BuildQGSite[opts:OptionsPattern[]] := BuildQGSite[All, opts];

BuildQGSite[src:Except[_Rule], opts:OptionsPattern[]] := Scope[
  SetAll[src, $QGSiteImportPath];
  If[StringQ[src] && StringFreeQ[src, $PathnameSeparator],
    src = FileNames["*" <> src <> "*.nb", $QGSiteImportPath, Infinity];
    src = Discard[src, StringContainsQ["XXX"]];
    If[src === {}, ReturnFailed[]];
    src = First @ src;
  ];
  files = ExportToMarkdown[
    src, File[$QGSiteExportPath],
    MarkdownFlavor -> "Hugo",
    opts,
    IncludeFrontMatter -> True,
    BaseImportPath -> $QGSiteImportPath,
    KatexPreludePath -> "../data/katex-prelude.tex",
    EmbedPreludeLink -> False,
    NotebookCaching -> True,
    ExportPathFunction -> hugoExportPathFunction,
    FrontMatterFunction -> hugoFrontMatterFunction,
    HeadingDepthOffset -> 1,
    RasterizationPath -> {"../static/raster", "/raster/"}
  ];
  If[!StringOrStringVectorQ[files], ReturnFailed[]];
  files
];

baseFileQ[relPath_] := StringFreeQ[relPath, $PathnameSeparator];

$hugoPathReplacements = {
  "index.md" -> "_index.md",
  "chapters" -> "docs",
  "pages" -> "docs",
  "blog" -> "posts"
};

hugoFrontMatterFunction[assoc_] := Scope[
  assoc["type"] = type = Replace[ToLowerCase @ assoc["relativepath"], $hugoPathReplacements];
  If[type === "docs", assoc["url"] = titleToURL @ assoc["title"]];
  assoc
];

hugoExportPathFunction[relPath_] :=
  FileNameJoin @ Map[StringReplace[$hugoPathReplacements], FileNameSplit @ relPath];

(**************************************************************************************************)

PublicFunction[ClearQGSite]

Options[ClearQGSite] = {"ClearRasters" -> False};

ClearQGSite[OptionsPattern[]] := Scope[
  
  If[OptionValue["ClearRasters"],
    rasterPath = ToFileName[$QGSiteExportPath, "../static/raster"];
    Quiet @ DeleteDirectory[rasterPath, DeleteContents -> True];
    EnsureDirectory[rasterPath];
  ,
    rasterPath = Nothing;
  ];

  EnsureDirectory[$QGSiteExportPath];

  Scan[tryDeleteFile, FileNames["*.md", $QGSiteExportPath, Infinity]];
  
  {rasterPath, $QGSiteExportPath}
];

tryDeleteFile[path_String] :=
  If[StringContainsQ[ReadString @ path, "\"notebookpath\":"], DeleteFile @ path];

(**************************************************************************************************)

PublicFunction[QGSiteMarkdownPath]

QGSiteMarkdownPath[] := QGSiteMarkdownPath @ EvaluationNotebook[];
QGSiteMarkdownPath[nb_NotebookObject] := QGSiteMarkdownPath @ NotebookFileName @ nb;

QGSiteMarkdownPath[nb_String] := Scope @ CatchMessage[
  $baseImportPath = NormalizePath @ $QGSiteImportPath;
  $exportPathFunction = hugoExportPathFunction;
  toExportFileName[$QGSiteExportPath, nb]
];

(**************************************************************************************************)

PublicFunction[QGSiteWebpageURL]

QGSiteWebpageURL[] := QGSiteWebpageURL @ EvaluationNotebook[];
QGSiteWebpageURL[nb_NotebookObject] := QGSiteWebpageURL @ NotebookFileName @ nb;

QGSiteWebpageURL[nb_] := Scope[
  mdPath = QGSiteMarkdownPath[nb];
  If[!StringQ[mdPath] || !FileExistsQ[mdPath], ReturnFailed[]];
  frontMatter = MarkdownFrontMatter @ mdPath;
  url = Lookup[frontMatter, "url", None];
  If[!StringQ[url],
    exportPath = NormalizePath @ $QGSiteExportPath;
    url = RelativePath[exportPath, StringTrim[mdPath, ".md"]];
  ];
  If[StringQ[url], StringJoin[$urlBase, url], $Failed]
];

$urlBase = "http://localhost:1313/";

(**************************************************************************************************)

PublicFunction[OpenQGSiteWebpageURL]

OpenQGSiteWebpageURL[] := OpenQGSiteWebpageURL @ EvaluationNotebook[];

OpenQGSiteWebpageURL[nb_] := Scope[
  url = QGSiteWebpageURL[nb];
  If[!StringQ[url], ReturnFailed[]];
  If[Run["/usr/bin/curl " <> $urlBase] =!= 0, ServeQGSite[]; Pause[1.0]];
  WebpageOpen @ url
];

(**************************************************************************************************)

PublicFunction[DeployQGSite]

DeployQGSite[] := Scope[
  siteRoot = NormalizePath @ "~/sites/qg-hugo";
  deployScript = FileNameJoin[{siteRoot, "iterm_deploy.sh"}];
  Run[deployScript];
];

(**************************************************************************************************)

PublicFunction[ServeQGSite]

ServeQGSite[] := Scope[
  siteRoot = NormalizePath @ "~/sites/qg-hugo";
  serveScript = FileNameJoin[{siteRoot, "iterm_serve.sh"}];
  Run[serveScript];
  GoodBeep[];
];

(**************************************************************************************************)

PublicFunction[QGNotebookSaveAndExport]

QGNotebookSaveAndExport[] := QGNotebookSaveAndExport[EvaluationNotebook[]];
QGNotebookSaveAndExport[nb_NotebookObject] := Scope[
  If[Lookup[NotebookInformation[nb], "ModifiedInMemory"],
    Beep[]; NotebookSave[nb]; Pause[0.25];
  ];
  WithExternalMessageCapture @ BuildQGSite[nb, NotebookCaching -> False];
  GoodBeep[];
];

(**************************************************************************************************)

PublicFunction[QGNotebookExport]

QGNotebookExport[] := QGNotebookExport @ EvaluationNotebook[];
QGNotebookExport[nb_NotebookObject] := WithExternalMessageCapture @ BuildQGSite[nb, NotebookCaching -> False];

