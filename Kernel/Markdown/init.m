PublicVariable[$SymbolTranslationTable]

$SymbolTranslationTable = Block[{str},
  rawString = ImportUTF8 @ LocalPath["Kernel", "Markdown", "SymbolTranslation.txt"];
  rawString //= StringReplace[{StartOfLine ~~ " "... ~~ "\n" -> "", " \\" -> " \\\\", "\"" -> "\\\""}];
  parsedString = StringTrim[ToExpression["\"" <> rawString <> "\"", InputForm], " " | "\n"];
  table = StringExtract[parsedString, "\n" -> All, " ".. -> All] /. "_" -> None;
  table
];

(**************************************************************************************************)

PublicFunction[SymbolTranslationData]

SymbolTranslationData[assoc_Association] :=
  Association @ SymbolTranslationData[Normal @ assoc];

SymbolTranslationData[schema_] := Scope[
  func = Construct[Function, schema /. {
    "Symbol" -> #1, "InputForm" -> #2, "Katex" -> #3, "Unicode" -> #4
  }];
  results = func @@@ $SymbolTranslationTable;
  Discard[results, ContainsQ[None]]
];

(**************************************************************************************************)

makeLiteralReplacementRule[assoc_, wrap_] := ModuleScope[
  If[!wrap, assoc = Association @ Select[Normal @ assoc, Apply[#1 =!= assoc[#2]&]]];
  keys = Keys[assoc];
  patt = StringJoin["(", Riffle[keys, "|"], ")"];
  re = RegularExpression @ StringReplace[patt, "$" -> "\\$"];
  $table = assoc;
  If[wrap,
    re :> " " <> $table["$1"] <> " ",
    re :> $table["$1"]
  ]
];

PublicVariable[$WLSymbolToKatexRegex]

$WLSymbolToKatexRegex = makeLiteralReplacementRule[SymbolTranslationData[<|"Symbol" -> "Katex"|>], True]

PublicVariable[$WLSymbolToUnicode]

$WLSymbolToUnicode = makeLiteralReplacementRule[SymbolTranslationData[<|"Symbol" -> "Unicode"|>], False]

(**************************************************************************************************)

PrivateFunction[wlCharactersToUnicode]

wlCharactersToUnicode[s_String] := StringReplace[s, $WLSymbolToUnicode];

(**************************************************************************************************)

PrivateVariable[$TemplateKatexFunction]

$TemplateKatexFunction = <||>;

(**************************************************************************************************)

PrivateFunction[titleToURL, trimNumberPrefix]

trimNumberPrefix[title_] := StringTrim @ StringDelete[title, StartOfString ~~ DigitCharacter.. ~~ Repeated[" - ", {0, 1}]];

titleToURL[title_] := StringReplace[trimNumberPrefix @ ToLowerCase @ title, (Whitespace | "-").. -> "-"];

(**************************************************************************************************)

PrivateFunction[insertAtFirstNonheader]

insertAtFirstNonheader[lines_List, template_] := Scope[
  index = SelectFirstIndex[lines, !StringStartsQ[#, "#"] && !StringMatchQ[#, Whitespace]&, 1];
  Insert[lines, template, index]
];

(**************************************************************************************************)

PrivateMacro[setupMarkdownGlobals]

PrivateVariable[$markdownGlobalsHash]

PrivateVariable[$allowTableHeaderSkip]
PrivateVariable[$anchorTemplate]
PrivateVariable[$classAttributeTemplate]
PrivateVariable[$externalImportTemplate]
PrivateVariable[$fileAnimatedImageTemplate]
PrivateVariable[$fileImageTemplate]
PrivateVariable[$fileAnimatedImageTemplate]
PrivateVariable[$inlineLinkTemplate]
PrivateVariable[$inlineMathTemplate]
PrivateVariable[$katexFontTemplate]
PrivateVariable[$katexPostprocessor]
PrivateVariable[$markdownPostprocessor]
PrivateVariable[$multilineMathTemplate]
PrivateVariable[$rasterizationFunction]
PrivateVariable[$rawHTMLTemplate]
PrivateVariable[$stringImageTemplate]
PrivateVariable[$customStylesTemplate]

General::badmdflav = "`` is not a valid setting for MarkdownFlavor.";

$rasterizationCaching = True;

DefineLiteralMacro[setupMarkdownGlobals,
setupMarkdownGlobals[] := Quoted[
  UnpackOptions[
    $embedKatexPrelude,
    $headingDepthOffset,
    $markdownFlavor,
    $rasterizationCaching,
    $rasterizationPath,
    $rasterizationURL,
    $includeFrontMatter,
    $frontMatterFunction,
    $rasterizeInputOutputPairs
  ];
  SetAutomatic[$rasterizationURL, "/raster"];
  flavorFields = Lookup[$flavorData, $markdownFlavor, Message[General::badmdflav, $markdownFlavor]; ReturnFailed[]];
  UnpackAssociation[flavorFields,
    $allowTableHeaderSkip,
    $anchorTemplate,
    $classAttributeTemplate,
    $externalImportTemplate,
    $fileAnimatedImageTemplate, (* currently unused *)
    $fileImageTemplate,
    $fileAnimatedImageTemplate,
    $inlineLinkTemplate,
    $inlineMathTemplate,
    $katexFontTemplate,
    $katexPostprocessor,
    $markdownPostprocessor,
    $multilineMathTemplate,
    $rasterizationFunction,
    $rawHTMLTemplate,
    $stringImageTemplate,
    $customStylesTemplate
  ];
  rfunc = OptionValue[RasterizationFunction];
  If[rfunc =!= Automatic, $rasterizationFunction = rfunc];
  SetInherited[$fileAnimatedImageTemplate, $fileImageTemplate];
  (* this lets us cache against this hash: *)
  $markdownGlobalsHash = Hash[{$embedKatexPrelude, $headingDepthOffset, $markdownFlavor, $rasterizationURL, $markdownFlavor, $rasterizationFunction}];
]];

(**************************************************************************************************)

PublicScopedOption[FrontMatterFunction]
SetUsage @ "FrontMatterFunction is a markdown export option that is applied to the association of front matter and can add or modify it."

PublicScopedOption[HeadingDepthOffset]
SetUsage @ "HeadingDepthOffset is a markdown export option that applies an offset to the numeric weight of heading-type styles ('Title' = 0, 'Section' = 1, etc) to determine their HTML / markdown equivalent."

PublicScopedOption[IncludeFrontMatter]
SetUsage @ "IncludeFrontMatter  is a markdown export option that specifies frontmatter should be written at the start of the markdown."

PublicScopedOption[MarkdownFlavor]
SetUsage @ "MarkdownFlavor is a markdown export option that customizes many details of markdown export behavior to one of a set of named behaviors."

PublicScopedOption[RasterizationCaching]
SetUsage @ "RasterizationCaching is a markdown export option that enables per-raster caching of rasterized output cells."

PublicScopedOption[RasterizationPath]
SetUsage @ "RasterizationPath is a markdown export option that determines the directory to which rasterized images and video are written.
* If a relative path is provided it is resolved under the setting of %BaseExportPath.
* If Automatic is provided a temporary directory will be used."
$rasterizationPath := $rasterizationPath = TemporaryPath["rasters"];

PublicScopedOption[RasterizationURL]
SetUsage @ "RasterizationURL is a markdown export option that determines the URL prefix to use for raster URLs."
$rasterizationURL = None;

PublicScopedOption[RasterizationFunction]
SetUsage @ "RasterizationFunction is a markdown export option that overrides the default rasterization function used by the current markdown flavor."

PublicScopedOption[EmbedKatexPrelude]
SetUsage @ "EmbedKatexPrelude is a markdown export option that specifies whether the Katex prelude should be embedded (if True), or linked (if 'Link') at the top of each file."

PublicScopedOption[RasterizeInputOutputPairs]
SetUsage @ "RasterizeInputOutputPairs is a markdown export option that specifies whether the input / output cell pairs should be rasterized as one unit."
$rasterizeInputOutputPairs = False;


PrivateVariable[$genericMarkdownOptions]
(* these are the core options supported by ToMarkdownString, but that don't involve creating output files *)

$genericMarkdownOptions = {
  EmbedKatexPrelude -> False,
  FrontMatterFunction -> None,
  HeadingDepthOffset -> 0,
  IncludeFrontMatter -> False,
  MarkdownFlavor -> "Preview",
  RasterizationCaching -> True,
  RasterizationPath -> Automatic,
  RasterizationURL -> None,
  RasterizationFunction -> Automatic,
  RasterizeInputOutputPairs -> False
}

(**************************************************************************************************)

PublicScopedOption[NotebookPath]
SetUsage @ "NotebookPath is a markdown export option that gives the containing directory that is used to choose the relative URL to use for a given input notebook file.
* If None, or if the file is outside this directory, the relative URL is at top-level.
* If Automatic, the path is taken to be the container of the input file, or the input directory itself."

PublicScopedOption[NotebookCaching]
SetUsage @ "NotebookCaching is a markdown export option that enables per-notebook caching of emitted markdown."
$notebookCaching = False;

PublicScopedOption[BaseExportPath]
SetUsage @ "BaseExportPath is a markdown export option that determines how relative output paths are interpreted, defaults to a fresh temporary directory.
* BaseExportPath only controls how relative paths for other options are resolved."

PublicScopedOption[KatexPreludePath]
SetUsage @ "KatexPreludePath is a markdown export option that specifies a path to write global katex definitions to.
* If a relative path is provided it is resolved under the setting of %BaseExportPath."

PublicScopedOption[MarkdownPath]
SetUsage @ "MarkdownPath is a markdown export option that determines the directory to write markdown files to.
* If a relative path is provided it is resolved under the setting of %BaseExportPath."

PublicScopedOption[ExportPathFunction]
SetUsage @ "ExportPathFunction is a markdown export option that gives a function to rewrite relative paths of input notebooks to determine their URL."
$exportPathFunction = None;

PublicScopedOption[Verbose]
SetUsage @ "Verbose is a markdown export option that prints every action that will be taken."
$verbose = False;

PublicScopedOption[DryRun]
SetUsage @ "DryRun is a markdown export option that specifies no files should be written."

PrivateFunction[whenWet]
SetHoldAll[whenWet];
whenWet[args___, last_] /; !TrueQ[$dryRun] := last;
whenWet[___] := Null;

PrivateVariable[$genericExportMarkdownOptions]
(* these are the additional options consumed by ExportToMarkdown, BuildSite, etc.
also change defaults to be more meaningful for large-scale exports, as opposed to ToMarkdownString *)

$genericExportMarkdownOptions = JoinOptions[
  MarkdownFlavor -> "Base",
  IncludeFrontMatter -> True,
  $genericMarkdownOptions,
  NotebookPath -> Automatic,
  NotebookCaching -> False,
  BaseExportPath -> Automatic,
  KatexPreludePath -> None,
  MarkdownPath -> Automatic,
  ExportPathFunction -> None,
  MaxItems -> Infinity,
  Verbose -> Automatic,
  DryRun -> False
];


