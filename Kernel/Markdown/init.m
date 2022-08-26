PublicVariable[$SymbolTranslationTable]

$SymbolTranslationTable = Block[{str},
  rawString = ImportUTF8 @ LocalPath["Kernel", "Markdown", "SymbolTranslation.txt"];
  rawString //= StringReplace[{StartOfLine ~~ " "... ~~ "\n" -> "", " \\" -> " \\\\", "\"" -> "\\\""}];
  parsedString = StringTrim @ ToExpression["\"" <> rawString <> "\"", InputForm];
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

PrivateVariable[$verbose]

$verbose = False;

PrivateFunction[mdvPrint]

SetHoldAllComplete[mdvPrint];
mdvPrint[args___] /; TrueQ[$verbose] :=
  Print[args];

(**************************************************************************************************)

PrivateMacro[setupMarkdownGlobals]

PrivateVariable[$anchorTemplate]
PrivateVariable[$classAttributeTemplate]
PrivateVariable[$externalImportTemplate]
PrivateVariable[$fileAnimatedImageTemplate]
PrivateVariable[$fileImageTemplate]
PrivateVariable[$fileVideoTemplate]
PrivateVariable[$inlineLinkTemplate]
PrivateVariable[$inlineMathTemplate]
PrivateVariable[$katexPostprocessor]
PrivateVariable[$markdownPostprocessor]
PrivateVariable[$multilineMathTemplate]
PrivateVariable[$rasterizationFunction]
PrivateVariable[$rawHTMLTemplate]
PrivateVariable[$stringImageTemplate]

PrivateVariable[$embedPreludeLink]
PrivateVariable[$exportPathFunction]
PrivateVariable[$frontMatterFunction]
PrivateVariable[$headingDepthOffset]
PrivateVariable[$includeFrontMatter]
PrivateVariable[$katexPreludePath]
PrivateVariable[$markdownFlavor]
PrivateVariable[$notebookCaching]
PrivateVariable[$rasterizationCaching]

$frontMatterFunction = None;

DefineLiteralMacro[setupMarkdownGlobals,
setupMarkdownGlobals[] := Quoted[
  UnpackOptions[
    $embedPreludeLink,
    $exportPathFunction,
    $headingDepthOffset,
    $katexPreludePath,
    $markdownFlavor,
    $notebookCaching,
    $rasterizationCaching
  ];
  flavorFields = Lookup[$flavorData, $markdownFlavor, ReturnFailed[]];
  UnpackAssociation[flavorFields,
    $anchorTemplate,
    $classAttributeTemplate,
    $externalImportTemplate,
    $fileAnimatedImageTemplate, (* currently unused *)
    $fileImageTemplate,
    $fileVideoTemplate,
    $inlineLinkTemplate,
    $inlineMathTemplate,
    $katexPostprocessor,
    $markdownPostprocessor,
    $multilineMathTemplate,
    $rasterizationFunction,
    $rawHTMLTemplate,
    $stringImageTemplate
  ];
  rfunc = OptionValue[RasterizationFunction];
  If[rfunc =!= Automatic, $rasterizationFunction = rfunc];
  SetInherited[$fileAnimatedImageTemplate, $fileImageTemplate];
  SetInherited[$fileVideoTemplate, $fileImageTemplate];
]];

(**************************************************************************************************)

PrivateMacro[setupRasterizationPath]

PrivateVariable[$rasterizationPath, $rasterizationURL]

$rasterizationPath = $TemporaryDirectory;
$rasterizationURL = None;

DefineLiteralMacro[setupRasterizationPath,
setupRasterizationPath[baseDir_, default_] := Quoted[
  UnpackOptions[$rasterizationPath, $rasterizationURL];
  SetAutomatic[$rasterizationPath, default];
  $rasterizationPath //= NormalizePath;
  If[!AbsolutePathQ[$rasterizationPath],
    $rasterizationPath = ToFileName[baseDir, $rasterizationPath]];
]];

(**************************************************************************************************)

PublicOption[EmbedPreludeLink, ExportPathFunction, HeadingDepthOffset, KatexPreludePath, MarkdownFlavor, NotebookCaching]
PublicOption[RasterizationCaching, RasterizationPath, RasterizationURL, RasterizationFunction]

SetUsage @ "EmbedPreludeLink is an option to various markdown-related functions."
SetUsage @ "ExportPathFunction is an option to various markdown-related functions."
SetUsage @ "HeadingDepthOffset is an option to various markdown-related functions."
SetUsage @ "KatexPreludePath is an option to various markdown-related functions."
SetUsage @ "MarkdownFlavor is an option to various markdown-related functions."
SetUsage @ "NotebookCaching is an option to various markdown-related functions."
SetUsage @ "RasterizationCaching is an option to various markdown-related functions."
SetUsage @ "RasterizationPath is an option to various markdown-related functions that determines the path to place rasterized outputs.
* The default value of RasterizationPath is Automatic, which will use the relative directory './OutputImages'."
SetUsage @ "RasterizationURL is an option to various markdown-related functions that determines the path prefix to use for raster URLs."
SetUsage @ "RasterizationFunction is an override of the default rasterization function used by the given markdown flavor."


PrivateVariable[$genericMarkdownOptions]

$genericMarkdownOptions = {
  EmbedPreludeLink -> True,
  ExportPathFunction -> None,
  HeadingDepthOffset -> 0,
  KatexPreludePath -> None,
  MarkdownFlavor -> "Preview",
  NotebookCaching -> False,
  RasterizationCaching -> True,
  RasterizationPath -> Automatic,
  RasterizationURL -> None,
  RasterizationFunction -> Automatic
}

(**************************************************************************************************)

PublicVariable[$DefaultImportPath, $DefaultExportPath]

$DefaultImportPath = ExpandFileName @ "~";
$DefaultExportPath = FileNameJoin[{$TemporaryDirectory, "QuiverGeometryMarkdown"}];

SetUsage @ "$DefaultImportPath determines the default used when %ImportPath is not specified."
SetUsage @ "$DefaultExportPath determines the default used when %ExportPath is not specified."

PublicOption[BaseExportPath, BaseImportPath, CollatedPagePath, DryRun, IncludeFrontMatter, ExportPath, FrontMatterFunction, ImportPath, Verbose]

SetUsage @ "ImportPath is an option to various markdown-related functions that determines the import path.
* The default value of ImportPath is $DefaultImportPath."
SetUsage @ "ExportPath is an option to various markdown-related functions that determines the export path.
* The default value of ExportPath is $DefaultExportPath."
SetUsage @ "BaseImportPath is an option to various markdown-related functions that determines the base import path.
* BaseImportPath determines the base path with which relative paths are resolved."
SetUsage @ "BaseExportPath is an option to various markdown-related functions that determines the base export path.
* BaseExportPath determines the base path with which relative paths are resolved."
SetUsage @ "CollatedPagePath is an option to various markdown exporting functions."
SetUsage @ "IncludeFrontMatter is an option to various markdown-related functions."
SetUsage @ "FrontMatterFunction is an option to various markdown exporting functions."
SetUsage @ "DryRun is an option to various markdown exporting functions."
SetUsage @ "Verbose is an option to various markdown exporting functions."

PrivateVariable[$genericExportMarkdownOptions]

$genericExportMarkdownOptions = JoinOptions[
  $genericMarkdownOptions,
  BaseExportPath -> None,
  BaseImportPath -> None,
  CollatedPagePath -> None,
  DryRun -> False,
  ExportPath :> $DefaultExportPath,
  FrontMatterFunction -> None,
  ImportPath :> $DefaultImportPath,
  IncludeFrontMatter -> False,
  MaxItems -> Infinity,
  Verbose -> Automatic
];

