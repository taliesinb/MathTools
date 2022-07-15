PublicFunction[ToMarkdownString]

Options[ToMarkdownString] = $genericMarkdownOptions;

ToMarkdownString[spec_, opts:OptionsPattern[]] := Scope[
  setupMarkdownGlobals[];
  setupRasterizationPath[$TemporaryDirectory, ToFileName[$TemporaryDirectory, "wl_md_images"]];
  toMarkdownStringInner @ spec
];

(**************************************************************************************************)

PrivateFunction[toMarkdownStringInner]

toMarkdownStringInner[spec_] := Scope[
  $textPostProcessor = StringReplace @ {
    If[$inlineLinkTemplate === None, Nothing, $markdownLinkReplacement],
    $inlineWLExprReplacement,
    $inlineWLVarReplacement
  };
  lines = toMarkdownLines @ spec;
  If[!StringVectorQ[lines], ReturnFailed[]];
  If[StringQ[$katexPreludePath] && $embedPreludeLink,
    lines = insertAtFirstNonheader[lines, {$externalImportTemplate @ $katexPreludePath}]];
  result = StringJoin @ {Riffle[lines, "\n\n"], "\n\n"};
  If[!StringQ[result], ReturnFailed[]];
  result //= StringReplace[$markdownTableReplacement];
  StringTrim @ $markdownPostprocessor @ result
];

(**************************************************************************************************)

General::badnbread = "Could not read notebook ``, will be replaced with placeholder.";

$notebookToMarkdownCache = UAssociation[];
$notebookCaching = False;

toMarkdownLines[File[path_String]] /; FileExtension[path] === "nb" := Scope[
  fileDate = Quiet @ FileDate @ path;
  If[$notebookCaching,
    {cachedResult, cachedDate} = Lookup[$notebookToMarkdownCache, path, {None, None}];
    If[ListQ[cachedResult] && fileDate === cachedDate,
      mdvPrint["  Using cached markdown for ", MsgPath @ path];
      Return @ cachedResult
    ];
  ];
  baseName = FileNameTake[path];
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

PrivateFunction[$localKatexDefinitions, $localTemplateToKatexFunctions]

$localKatexDefinitions = None;
$localTemplateToKatexFunctions = None;

toMarkdownLines[Notebook[cells_List, opts___]] := Scope[
  taggingRules = Lookup[{opts}, TaggingRules, <||>];
  If[StringQ[katexDefs = taggingRules["KatexDefinitions"]],
    mdvPrint["Applying local katex definitions: \n", katexDefs];
    $localKatexDefinitions = katexDefs];
  If[AssociationQ[katexFns = taggingRules["TemplateToKatexFunctions"]],
    mdvPrint["Applying local template to katex functions: \n", katexFns];
    $localTemplateToKatexFunctions = katexFns
  ];
  $lastCaption = None; Map[cellToMarkdown, cells]
]

toMarkdownLines[cell_Cell | cell_CellObject | cell_CellGroup] := Scope[
  $lastCaption = None;
  List @ cellToMarkdown @ cell
];

toMarkdownLines[e_] := List[
  "#### toMarkdownLines: unknown expression with head " <> TextString @ Head @ e
];

(**************************************************************************************************)

cellToMarkdown = Case[

  cell_CellObject :=
    cellToMarkdown @ NotebookRead @ cell;

  Cell[e_, style:Except["Input" | "Code"], ___, CellTags -> tag_String, ___] :=
    Splice @ {$anchorTemplate @ tag, cellToMarkdownInner0 @ Cell[e, style]};

  Cell[e_, style_, ___] :=
    cellToMarkdownInner0 @ Cell[e, style];

  CellGroup[cells_, openI_] :=
    cellToMarkdown @ Part[cells, openI];
  
  Cell[CellGroupData[cells_List, Open]] | CellGroup[cells_List] :=
    Splice[cellToMarkdown /@ cells];

  Cell[_CellGroupData] :=
    Nothing;
];

(**************************************************************************************************)

PublicVariable[$LastFailedMarkdownInput]
PublicVariable[$LastFailedMarkdownOutput]
PrivateFunction[PrintBadCell]

$LastFailedMarkdownInput = $LastFailedMarkdownOutput = None;

PrintBadCell[c_Cell] := CellPrint @ ReplaceOptions[c, Background -> RGBColor[1,0.95,0.95]];
PrintBadCell[e_] := CellPrint @ Cell[e, "Text", Background -> RGBColor[1,0.95,0.95]]

(**************************************************************************************************)

ToMarkdownString::badcell = "Error occurred while rasterizing cell.";

cellToMarkdownInner0[cell_] := Scope[
  result = cellToMarkdownInner1[cell];
  If[StringQ[result] || result === Nothing, result,
    $LastFailedMarkdownInput ^= cell; $LastFailedMarkdownOutput ^= result;
    Message[ToMarkdownString::badcell]; PrintBadCell[cell]; "### BAD CELL"]
];

cellToMarkdownInner1 = Case[

  Cell["Under construction.", _] := bannerToMarkdown @ "UNDER CONSTRUCTION";

  Cell[str_String /; StringStartsQ[str, "BANNER: "], _] := bannerToMarkdown @ StringTrim[str, "BANNER: "];

  Cell[e_, "Title"]              := StringJoin[headingDepth @ -1, textCellToMarkdown @ e];
  Cell[e_, "Chapter"]            := StringJoin[headingDepth @ 0,  textCellToMarkdown @ e];
  Cell[e_, "Section"]            := StringJoin[headingDepth @ 1,  textCellToMarkdown @ e];
  Cell[e_, "Subsection"]         := StringJoin[headingDepth @ 2,  textCellToMarkdown @ e];
  Cell[e_, "Subsubsection"]      := StringJoin[headingDepth @ 3,  textCellToMarkdown @ e];

  Cell[e_, "Text"]               := insertLinebreaksOutsideKatex[textCellToMarkdown @ e, 120];
  Cell[e_, "Item"]               := StringJoin["* ",     textCellToMarkdown @ e];
  Cell[e_, "Subitem"]            := StringJoin["\t* ",   textCellToMarkdown @ e];
  Cell[e_, "SubsubItem"]         := StringJoin["\t\t* ", textCellToMarkdown @ e];

  Cell[e_, "ItemNumbered"]       := StringJoin["1. ",        textCellToMarkdown @ e];
  Cell[e_, "SubitemNumbered"]    := StringJoin["\t1.1 ",     textCellToMarkdown @ e];
  Cell[e_, "SubsubItemNumbered"] := StringJoin["\t\t1.1.1 ", textCellToMarkdown @ e];

  Cell[b:BoxData[TemplateBox[_, _ ? textTagQ]], "Output"]     := textCellToMarkdown @ b;
  Cell[BoxData[t:TemplateBox[_, "VideoBox1", ___]], "Output"] := videoCellToMarkdown @ t;

  e:Cell[_, "Output"]            := If[ContainsQ[e, "LinkHand"], Nothing, outputCellToMarkdown @ e];
  Cell[e_, "Code"]               := ($lastCaption = First[Cases[e, $outputCaptionPattern], None]; Nothing);

  c_ := (Nothing);
];

PrivateVariable[$lastCaption]

$lastCaption = None;

$outputCaptionPattern = RowBox[{"(*", RowBox[{"CAPTION", ":", caption___}], "*)"}] :>
  StringJoin @ textCellToMarkdownOuter @ RowBox[{caption}];

(* recognizes tags generated by StylesheetForms *)
textTagQ[tag_String] := StringEndsQ[tag, "Form" | "Symbol"] || TemplateBoxNameQ[tag];
textTagQ[_] := False;

headingDepth[n_] := StringRepeat["#", Max[n + $headingDepthOffset, 1]] <> " ";

(**************************************************************************************************)

insertLinebreaksOutsideKatex[str_String, n_] := Scope[
  If[$markdownFlavor === "Hugo", Return @ str];
  katexSpans = StringPosition[str, Verbatim["$"] ~~ Shortest[___] ~~ Verbatim["$"], Overlaps -> False];
  katexStrings = StringTake[str, katexSpans];
  If[katexSpans === {}, Return @ InsertLinebreaks[str, n]];
  str = InsertLinebreaks[StringReplacePart[str, "€", katexSpans], n];
  i = 1;
  StringReplace[str, "€" :> Part[katexStrings, i++]]
];

