PublicFunction[BuildQGSiteFranklin]

$QGSiteImportPath = "~/Dropbox/QuiverGeometry";
$QGSiteFranklinExportPath = "~/sites/qg-franklin";

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

PublicFunction[BuildQGSiteHugo]

Options[BuildQGSite] = $genericExportMarkdownOptions;

$QGSiteHugoExportPath = "~/sites/qg-hugo/content";

BuildQGSiteHugo[opts:OptionsPattern[]] := BuildQGSiteHugo[All, opts];

BuildQGSiteHugo[src:Except[_Rule], opts:OptionsPattern[]] := Scope[
  SetAll[src, $QGSiteImportPath];
  files = ExportToMarkdown[
    src, File[$QGSiteHugoExportPath],
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

PublicFunction[ClearQGSiteHugo]

Options[ClearQGSiteHugo] = {"ClearRasters" -> False};

ClearQGSiteHugo[OptionsPattern[]] := Scope[
  
  If[OptionValue["ClearRasters"],
    rasterPath = ToFileName[$QGSiteHugoExportPath, "../static/raster"];
    Quiet @ DeleteDirectory[rasterPath, DeleteContents -> True];
    EnsureDirectory[rasterPath];
  ,
    rasterPath = Nothing;
  ];

  EnsureDirectory[$QGSiteHugoExportPath];

  Scan[tryDeleteFile, FileNames["*.md", $QGSiteHugoExportPath, Infinity]];
  
  {rasterPath, $QGSiteHugoExportPath}
];

tryDeleteFile[path_String] :=
  If[StringContainsQ[ReadString @ path, "\"notebookpath\":"], DeleteFile @ path];


