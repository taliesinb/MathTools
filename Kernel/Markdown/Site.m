$serverFunctions = <||>;

$hugoNewSite = Fn @ If[!FileExistsQ[PathJoin[#BaseExportPath, "config.toml"]], HugoNewSite[#BaseExportPath, FilterOptions @ #]];
$hugoBuildSite = Fn @ HugoBuild[#BaseExportPath, FilterOptions @ #];
$hugoServeSite = Fn @ HugoServe[#BaseExportPath, FilterOptions @ #];

$serverFunctions["Hugo"] = Assoc[
  "NewSite"         -> $hugoNewSite,
  "BuildSite"       -> $hugoBuildSite,
  "BuildSitePage"   -> $hugoBuildSite,
  "ServeSite"       -> $hugoServeSite,
  "ServeSitePage"   -> $hugoServeSite
];

$pandocBuildSite = Fn @ PandocExportMDToHTML[#AbsoluteMarkdownPath, #AbsoluteHTMLPath];
$pandocBuildPage = Fn @ PandocExportMDToHTML[#AbsoluteMarkdownPath -> #2, #AbsoluteHTMLPath];

$serverFunctions["Pandoc"] = Assoc[
  "NewSite"         -> None,
  "BuildSite"       -> $pandocBuildSite,
  "BuildSitePage"   -> $pandocBuildPage,
  "ServeSite"       -> $pandocBuildSite,
  "ServeSitePage"   -> $pandocBuildPage
];

General::nositegen = "Site `` has no site generator and cannot produce HTML."
$noserver = Fn @ ThrowMessage["nositegen", #SiteName];

$serverFunctions[None] = Assoc[
  "NewSite"         -> None,
  "BuildSite"       -> $noserver,
  "BuildSitePage"   -> $noserver,
  "ServeSite"       -> $noserver,
  "ServeSitePage"   -> $noserver
];

(**************************************************************************************************)

PublicFunction[ListSites]

SetUsage @ "
ListSites['site'] will list the names of all saved sites.
";

ListSites[] := FileBaseName /@ FileNames[toSiteFile["*"]];

(**************************************************************************************************)

doSiteAutocomplete[] := Scope[
  currentSites = ListSites[];
  declareFunctionAutocomplete[BuildSite, {currentSites}];
  declareFunctionAutocomplete[ServeSite, {currentSites}];
  declareFunctionAutocomplete[ClearSite, {currentSites}];
  declareFunctionAutocomplete[DeleteSite, {currentSites}];
];

(**************************************************************************************************)

PublicFunction[DeleteSite]

SetUsage @ "
DeleteSite['site'] will delete a site.
* The option %DeleteContents -> True will delete the corresponding directory on disk.
";

Options[DeleteSite] = {DeleteContents -> False, DryRun -> False, Verbose -> Automatic};

DeleteSite[siteName_Str, OptionsPattern[]] := Scope @ CatchMessage[
  UnpackOptions[deleteContents, $dryRun, $verbose];
  SetAutomatic[$verbose, $dryRun];
  unpackSiteData[siteName, baseExportPath];
  siteFile = toSiteFile @ siteName;
  VPrint["Deleting file ", MsgPath @ siteFile];
  whenWet @ DeleteFile @ siteFile;
  whenWet @ doSiteAutocomplete[];
  If[deleteContents,
    VPrint["Deleting directory ", MsgPath @ baseExportPath];
    whenWet @ DeleteDirectory[baseExportPath, DeleteContents -> True];
  ];
]

(**************************************************************************************************)

PublicFunction[SiteData]

SiteData[siteName_Str] := CatchMessage @ getSiteData[siteName];

General::badsitename = "No site named ``, existing sites are: ``.";
General::corruptsite = "Site file `` is corrupt.";

toSiteFile[name_] := LocalPath["Data", "Sites", name <> ".wl"];

getSiteData[siteName_] := Scope[
  siteFile = toSiteFile @ siteName;
  If[!FileExistsQ[siteFile], ThrowMessage["badsitename", siteName, ListSites[]]];
  siteData = Get @ siteFile;
  If[!AssocQ[siteData], ThrowMessage["corruptsite", siteFile]];
  UnpackAssociation[siteData, markdownPath, htmlPath:"HTMLPath", rasterizationPath, baseExportPath];
  siteData["AbsoluteHTMLPath"] = ToAbsolutePath[htmlPath, baseExportPath];
  siteData["AbsoluteMarkdownPath"] = ToAbsolutePath[markdownPath, baseExportPath];
  siteData["AbsoluteRasterizationPath"] = ToAbsolutePath[rasterizationPath, baseExportPath];
  siteData["BaseURL"] //= resolveStringFunction[siteData];
  siteData
];

resolveStringFunction[data_][str_] := If[!StringQ[str] || StringFreeQ[str, "#"], str, StringFunction[str] @ data];

DefineLiteralMacro[unpackSiteData, unpackSiteData[siteName_, args___] := UnpackAssociation[$siteData = getSiteData[siteName], args]];

(**************************************************************************************************)

General::sitenotfound = "No site corresponding to path `` was found."
General::badsiteobj = "`` is not a directory, file, or notebook object with a path."

findContainingSite[source_] := Scope[

  path = ToNotebookPath @ source;
  If[!StringQ[path], ThrowMessage["badsiteobj", source]];

  matchLen = 0;
  matchSite = None;
  Scan[siteFile |-> (
      siteData = Get[siteFile];
      If[!AssocQ[siteData], ReturnFailed["corruptsite", siteFile]];
      UnpackAssociation[siteData, siteName, notebookPath, baseExportPath];
      If[StringQ[notebookPath] && StringStartsQ[path, notebookPath] && (len = StringLength[notebookPath]) > matchLen,
        matchLen = len; matchSite = siteName];
    ),
    FileNames @ toSiteFile["*"]
  ];
  If[matchSite === None, ThrowMessage["sitenotfound", path]];

  {matchSite, path}
];

(**************************************************************************************************)

PublicFunction[CreateSite]

PublicVariable[$SitesDirectory]

$SitesDirectory = "~/sites";

declareFunctionAutocomplete[CreateSite, {None, File}]

PublicOption[SiteGenerator, BaseURL, HTMLPath, SiteName, ServingPort]

SetUsage @ "
CreateSite['name$', 'notebookPath$', 'exportPath$'] will create a new markdown-based static site, using 'Hugo' conventions by default.
* When built, notebooks in notebookPath$ will be exported to markdown files in a subdirectory of exportPath$.
* The option %SiteGenerator determines the tool used to generate the site, and can take the following values:
| 'Hugo' | use the Hugo SSG tool (default) |
| 'Pandoc' | use Pandoc to convert MD to HTML |
| None | do not produce HTML |
* The following options control where generated files go, and are interpreted relative to %BaseExportPath.
| %HTMLPath | 'public' | the path to put generated HTML files in |
| %MarkdownPath | 'content' | the path to put markdown files in |
| %RasterizationPath | 'static/raster' | the path to put rasterized files in |
* The following options control general behavior:
| %DryRun | False | whether to print but not perform potentially destructive changes |
| %Verbose | Automatic | whether to print all changes that are made |
* The following additional options can also be given to override the defaults (which are based on the setting of %SiteGenerator):
| %BaseURL | Automatic | the URL for local serving, normally 'localhost:port$' for active serving or 'file://path$' for passive serving |
| %MarkdownFlavor | Automatic | the flavor of markdown to use, which establishes conventions for Katex, images, etc |
| %RasterizationURL | '/raster' | the HTML path used for links to raster files |
| %KatexPreludePath | 'data/katex-prelude.tex' | where shared Katex code is written |
| %EmbedKatexPrelude | Automatic | how to embed the Katex prelude into pages |
| %IncludeFrontMatter | True | whether to write frontmatter to markdown files |
| %FrontMatterFunction | HugoFrontMatterFunction | function to add additional frontmatter to the markdown files |
| %ExportPathFunction | HugoExportPathFunction | function to rewrite relative paths when choosing an output file |
| %NotebookCaching | True | whether to avoid reconverting notebook files that have already been converted |
";

Options[CreateSite] = JoinOptions[
  BaseURL -> Automatic,
  ServingPort -> Automatic,
  SiteGenerator -> "Hugo",
  MarkdownFlavor -> Automatic,
  MarkdownPath -> "content",
  RasterizationPath -> "static/raster",
  HTMLPath -> "public",
  KatexPreludePath -> "data/katex-prelude.tex",
  FrontMatterFunction -> HugoFrontMatterFunction,
  ExportPathFunction -> HugoExportPathFunction,
  NotebookCaching -> True,
  OverwriteTarget -> False,
  $genericExportMarkdownOptions
];

$generatorDefaults = <|
  "Hugo"   -> <|MarkdownFlavor -> "Hugo",   IncludeFrontMatter -> True,  BaseURL -> "http://localhost:#ServingPort", RasterizationURL -> "/raster"|>,
  "Pandoc" -> <|MarkdownFlavor -> "Pandoc", IncludeFrontMatter -> False, BaseURL -> "file://#AbsoluteHTMLPath", ServingPort -> None, RasterizationURL -> None, EmbedKatexPrelude -> True|>,
  None     -> <|MarkdownFlavor -> "Base"|>
|>;

CreateSite::arg1 = "First argument should be the name of the site.";
CreateSite::pathns = "Option `` must be specified.";
CreateSite::pathne = "Setting of `` -> `` does not exist.";
CreateSite::badsitegen = "Setting SiteGenerator -> `` should be one of \"Hugo\", \"Pandoc\", or None.";
CreateSite::siteexists = "Site called \"``\" already registered, used OverwriteTarget -> True to replace it.";

CreateSite[siteName_Str, nbPath_Str, opts:OptionsPattern[]] :=
  CreateSite[siteName, NotebookPath -> nbPath, opts];

CreateSite[siteName_Str, nbPath_Str, ePath_Str, opts:OptionsPattern[]] :=
  CreateSite[siteName, NotebookPath -> nbPath, BaseExportPath -> ePath, opts];

CreateSite[siteName_Str, opts:OptionsPattern[]] := CatchMessage @ Scope[
  If[!StringQ[siteName], ReturnFailed["arg1"]];
  UnpackOptions[notebookPath, markdownFlavor, baseExportPath, baseURL, siteGenerator, overwriteTarget, $dryRun, $verbose];

  SetAutomatic[baseExportPath, If[!DirectoryQ[$SitesDirectory], Automatic, PathJoin[$SitesDirectory, siteName]]];

  If[!StringQ[notebookPath], ReturnFailed["pathns", NotebookPath]];
  If[!StringQ[baseExportPath], ReturnFailed["pathns", BaseExportPath]];

  If[FileExistsQ @ toSiteFile[siteName] && !overwriteTarget,
    ReturnFailed["siteexists", siteName]];

  notebookPath //= NormalizePath; baseExportPath //= NormalizePath;
  If[!EnsureDirectoryShallow[notebookPath], ReturnFailed["pathne", NotebookPath, MsgPath @ notebookPath]];
  If[!EnsureDirectoryShallow[baseExportPath], ReturnFailed["pathne", BaseExportPath, MsgPath @ baseExportPath]];
  siteFile = toSiteFile @ siteName;

  If[!MatchQ[siteGenerator, "Hugo" | "Pandoc" | None], ReturnFailed["badsitegen", siteGenerator]];
  defaults = $generatorDefaults[siteGenerator];

  assoc = Map[Id] @ KeyMap[SymbolName] @ KeySort @ Assoc[
    Options[CreateSite],
    ServingPort -> toUniquePort[siteName],
    defaults, opts,
    NotebookPath -> notebookPath,
    BaseExportPath -> baseExportPath,
    SiteName -> siteName
  ];
  KeyDropFrom[assoc, {"OverwriteTarget", "DryRun"}];

  If[FailureQ[$serverFunctions[siteGenerator]["NewSite"][assoc]], ReturnFailed[]];

  VPrint["Writing settings to ", MsgPath @ siteFile];
  VPrint["Settings are: ", ToPrettifiedString @ assoc];

  whenWet @ doSiteAutocomplete[];
  whenWet @ PrettyPut[assoc, siteFile];
];

(* CreateSite[name_Str -> templateName_Str, opts:OptionsPattern[]] := CatchMessage @ Scope[
  assoc = getSiteData @ templateName;
  KeyDropFrom[assoc, "SiteName"];
  assoc = Assoc[assoc, opts];
  CreateSite[name, Sequence @@ Normal[assoc]]
];
 *)
toUniquePort[siteName_Str] := 1000 + Mod[Hash @ ToLowerCase @ siteName, 8999];

(**************************************************************************************************)

PublicFunction[BuildSite]

Options[BuildSite] = JoinOptions[CreateSite];

SetUsage @ "
BuildSite['site'] will build the named site.
BuildSite[] will build the site containing the current notebook.
";

BuildSite[opts:OptionsPattern[]] := BuildSite[EvaluationNotebook[], opts];

BuildSite[siteSpec:Except[_Rule], extraOpts:OptionsPattern[]] := CatchMessage @ Scope[

  {siteData, result} = buildSite[siteSpec, extraOpts];

  $serverFunctions[siteData["SiteGenerator"], "BuildSite"][siteData, result]
];

General::badsite = "Spec `` does not identify a site.";
General::exportfailed = "Failed to export notebooks in site ``."

buildSite[siteSpec_, extraOpts___] := Scope[

  Which[
    StringQ[siteSpec] && StringFreeQ[siteSpec, $PathnameSeparator],
      siteName = siteSpec,
    MatchQ[siteSpec, $NotebookOrPathP],
      {siteName, temp} = findContainingSite @ siteSpec,
    True,
      ThrowMessage["badsite", siteSpec]
  ];

  siteData = getSiteData[siteName];
  source = siteData["NotebookPath"];
  result = ExportToMarkdown[source, extraOpts, FilterOptions @ siteData];
  If[!StringVectorQ[result], ThrowMessage["exportfailed", siteName]];

  {siteData, result}
];

(**************************************************************************************************)

PublicFunction[BuildSitePage]

Options[BuildSitePage] = Options[CreateSite];

declareFunctionAutocomplete[BuildSitePage, {File}];

SetUsage @ "
BuildSitePage['path$'] will build the site page corresponding to the given path.
BuildSitePage[nb$] will build the site page corresponding to the given notebook.
BuildSitePage[] will build the site page corresponding to the current notebook.
";

BuildSitePage[opts:OptionsPattern[]] := BuildSitePage[EvaluationNotebook[], opts];

General::badsitepage = "Source `` is not a notebook in a site.";

BuildSitePage[sourceSpec:Except[_Rule], extraOpts:OptionsPattern[]] := CatchMessage @ Scope[

  If[!MatchQ[sourceSpec, $NotebookOrPathP], ThrowMessage["badsitepage", sourceSpec]];
  {siteName, temp} = findContainingSite @ sourceSpec;

  siteData = getSiteData[siteName];
  outputFiles = ExportToMarkdown[sourceSpec, FilterOptions @ extraOpts, NotebookCaching -> False, FilterOptions @ siteData];
  If[!StringVectorQ[outputFiles], ThrowMessage["exportfailed", siteName]];

  $serverFunctions[siteData["SiteGenerator"], "BuildSitePage"][siteData, outputFiles];

  P1 @ outputFiles
];

(**************************************************************************************************)

PublicFunction[ServeSite]

General::servefail = "Could not start `` server for site `` to serve url ``."

SetUsage @ "
ServeSite['site'] will rebuild the site and start to actively serve the given site to a web browser.
ServeSite[] will serve the site containing the current notebook.
* A web browser will be opened at the root page of the site.
* If the site is already being served, the browser will be opened but nothing else will change.
";

Options[ServeSite] = Options[BuildSite];

ServeSite[opts:OptionsPattern[]] := ServeSite[P1 @ findContainingSite @ EvaluationNotebook[], opts];

ServeSite[siteName_Str, extraOpts:OptionsPattern[]] := Scope @ CatchMessage[

  {siteData, outputFiles} = buildSite[siteName, extraOpts];
  UnpackAssociation[siteData, baseURL, siteGenerator];

  If[URLAvailableQ[baseURL], Goto["Open"]];
  $serverFunctions[siteGenerator, "ServeSite"][siteData, outputFiles];
  Pause[0.33];
  If[!URLAvailableQ[baseURL], ReturnFailed["servefail", siteGenerator, siteName, baseURL]];

  Label["Open"];
  WebpageOpen[baseURL]
]

(**************************************************************************************************)

PublicFunction[ServeSitePage]

declareFunctionAutocomplete[ServeSitePage, {File}];

SetUsage @ "
ServeSitePage['site'] will rebuild the page and open it in a web browser.
ServeSitePage[] will serve the page corresponding to the current notebook.
* If the site is already being served, the page will still be rebuilt and the browser opened.
";

Options[ServeSite] = Options[BuildSitePage];

ServeSitePage[opts:OptionsPattern[]] := ServeSitePage[EvaluationNotebook[], opts];

ServeSitePage[nb:Except[_Rule], extraOpts:OptionsPattern[]] := Scope @ CatchMessage[

  pageData = SitePageData[nb];
  If[!AsssociationQ[pageData], ReturnFailed["badsitepage", nb]];

  UnpackAssociation[pageData, siteData, pageURL, source, target];
  UnpackAssociation[siteData, siteName, siteGenerator];

  outputFiles = ExportToMarkdown[nb, FilterOptions @ extraOpts, NotebookCaching -> False, FilterOptions @ siteData];
  If[!StringVectorQ[outputFiles], ThrowMessage["exportfailed", siteName]];

  If[URLAvailableQ[pageURL],
  $serverFunctions[siteGenerator, "BuildSitePage"][siteData, outputFiles];
  Goto["Open"]];

  $serverFunctions[siteGenerator, "ServeSitePage"][siteData, outputFiles];
  Pause[0.33];
  If[!URLAvailableQ[pageURL], ReturnFailed["servefail", siteGenerator, siteName, pageURL]];

  Label["Open"];
  WebpageOpen[pageURL]
];

(**************************************************************************************************)

$hugoPathReplacements = {
  "index.md" -> "_index.md",
  "chapters" -> "docs",
  "pages" -> "docs",
  "blog" -> "posts"
};

PublicFunction[HugoFrontMatterFunction]

HugoFrontMatterFunction[assoc_] := Scope[
  assoc["type"] = type = Replace[ToLowerCase @ assoc["relativepath"], $hugoPathReplacements];
  If[type === "docs", assoc["url"] = titleToURL @ assoc["title"]];
  assoc["katex"] = True;
  assoc["katexPrelude"] = True;
  assoc
];

PublicFunction[HugoExportPathFunction]

HugoExportPathFunction[relPath_] :=
  FileNameJoin @ Map[StringReplace[$hugoPathReplacements], FileNameSplit @ relPath];

(**************************************************************************************************)

PublicFunction[ClearSite]
PublicOption[DeleteRasters, DeleteMarkdown, DeleteHTML]

SetUsage @ "
ClearSite['site$'] will clear the markdown pages from the named site.
ClearSite[] will clear the markdown pages from the site containing the current notebook.
ClearSite takes the following options:
| %DeleteRasters | False | whether to delete raster files from %RasterizationPath |
| %DeleteMarkdown | False | whether to delete markdown files from %MarkdownPath (which must have front matter) |
| %DeleteHTML | False | whether to delete generated files from %HTMLPath |
";

Options[ClearSite] = {
  DeleteRasters -> False,
  DeleteMarkdown -> True,
  DeleteHTML -> False,
  DryRun -> False,
  Verbose -> Automatic
};

ClearSite[siteName_Str, OptionsPattern[]] := Scope[

  UnpackOptions[deleteRasters, deleteMarkdown, deleteHTML, $verbose, $dryRun];
  SetAutomatic[$verbose, $dryRun];

  unpackSiteData[siteName, baseExportPath, absoluteRasterizationPath, absoluteMarkdownPath, absoluteHTMLPath];

  If[deleteRasters && FileExistsQ[absoluteRasterizationPath],
    VPrint["Deleting rasterization path ", MsgPath @ absoluteRasterizationPath];
    Quiet @ whenWet[
      DeleteDirectory[absoluteRasterizationPath, DeleteContents -> True];
      EnsureDirectory[absoluteRasterizationPath];
    ];
  ,
    absoluteRasterizationPath = Nothing;
  ];

  If[deleteHTML && FileExistsQ[absoluteHTMLPath],
    VPrint["Deleting HTML path ", MsgPath @ absoluteHTMLPath];
    Quiet @ whenWet[
      DeleteDirectory[absoluteHTMLPath, DeleteContents -> True];
    ];
  ,
    absoluteHTMLPath = Nothing;
  ];

  If[deleteMarkdown && FileExistsQ[absoluteMarkdownPath],
    markdownFiles = FileNames["*.md", absoluteMarkdownPath, Infinity];
    markdownFiles //= Select[ReadString /* StringContainsQ["\"notebookpath\":"]];
    VPrint["Deleting ", Len @ markdownFiles, " files in ", MsgPath @ absoluteMarkdownPath];
    whenWet @ Scan[DeleteFile, markdownFiles];
  ,
    markdownFiles = {};
  ];

  Join[{absoluteRasterizationPath, absoluteHTMLPath}, markdownFiles]
];

(**************************************************************************************************)

PublicFunction[SitePageData]

SetUsage @ "
SitePageData['path$'] will return an association of information about the page corresponding to the given source notebook.
SitePageData[] returns information about the current notebook.
* The returned information includes:
| 'Source' | the path of the source notebook |
| 'Target' | the path of the generated markdown file |
| 'URL' | the URL of the generated markdown file |
| 'SiteData' | association describing the site containing the page |
";

SitePageData[] := SitePageData @ EvaluationNotebook[];

SitePageData[nb_] := Scope @ CatchMessage[

  {siteName, path} = findContainingSite @ nb;
  unpackSiteData[siteName, $exportPathFunction, $notebookPath, $markdownPath, $baseExportPath, baseURL, SiteGenerator];
  (* TODO: support additional 'subpath' that goes within markdownpath, used to put things under content/XXX *)
  $markdownPath //= ToAbsolutePath[$baseExportPath];

  mdPath = itemMarkdownPath[path];
  If[!StringQ[mdPath], ReturnFailed[]];

  frontMatter = MarkdownFrontMatter @ mdPath;
  If[AssocQ[frontMatter],
    url = Lookup[frontMatter, "url", None],
    url = None
  ];
  If[!StringQ[url],
    url = RelativePath[$markdownPath, StringTrim[mdPath, ".md"]];
  ];

  pageURL = If[StringQ[url], StringJoin[baseURL, "/", url, If[StringStartsQ[baseURL, "file://"], ".html", ""]], $Failed];
  Assoc[
    "Source" -> path,
    "Target" -> mdPath,
    "PageURL" -> pageURL,
    "SiteData" -> $siteData
  ]
];

doSiteAutocomplete[]