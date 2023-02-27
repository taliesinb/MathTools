PublicFunction[ToMarkdownString]

Options[ToMarkdownString] = $genericMarkdownOptions;

ToMarkdownString[spec_, returnVec:True|False:False, opts:OptionsPattern[]] := Scope[
  setupMarkdownGlobals[];
  SetAutomatic[$rasterizationPath, TemporaryPath["raster"]];
  toMarkdownStringInner[spec, returnVec]
];

(**************************************************************************************************)

PrivateFunction[toMarkdownStringInner]

toMarkdownStringInner[spec_, returnVec_:False] := Scope[
  initPNGExport[];
  $textPostProcessor = StringReplace @ {
    If[$inlineLinkTemplate === None, Nothing, $markdownLinkReplacement],
    $inlineWLExprReplacement, $inlineWLExprReplacement2,
    $inlineWLVarReplacement
  };
  lines = toMarkdownLines @ spec;
  If[!StringVectorQ[lines], ReturnFailed[]];
  If[returnVec, Return @ lines];
  If[StringQ[$katexPreludePath] && $embedKatexPrelude === "Link",
      lines = insertAtFirstNonheader[lines, {$externalImportTemplate @ $katexPreludePath}]];
  If[$embedKatexPrelude === True,
    katexMarkdown = $markdownPostprocessor @ $multilineMathTemplate @ $katexPostprocessor @ $KatexPrelude;
    lines = insertAtFirstNonheader[lines, {katexMarkdown}]];
  result = StringJoin @ {Riffle[lines, "\n\n"], "\n\n"};
  If[!StringQ[result], ReturnFailed[]];
  result //= StringReplace[$markdownTableReplacement] /* $markdownPostprocessor /* StringTrim;
  If[$includeFrontMatter && AssociationQ[frontMatter = NotebookFrontMatter @ spec],
    frontMatter = frontMatter /. None -> Null;
    frontMatter //= Developer`WriteRawJSONString /* StringReplace["\\/" -> "/"]; (* weird bug in ToJSON *)
    result = StringJoin[frontMatter, "\n\n", result];
  ];
  If[!StringQ[result], ReturnFailed[]];
  result
];

(**************************************************************************************************)

(* avoids a problem with GIFImageQuantize running the first time on a fresh kernel *)
initPNGExport[] := (Clear[initPNGExport]; Quiet @ ExportString[ConstantImage[1, {2, 2}], "PNG", CompressionLevel -> 1]);

(**************************************************************************************************)

General::badnbread = "Could not read notebook ``, will be replaced with placeholder.";

$notebookToMarkdownCache = UAssociation[];

toMarkdownLines[File[path_String]] /; FileExtension[path] === "nb" := Scope[
  path = NormalizePath @ path;
  fileDate = Quiet @ FileDate @ path;
  cacheKey = <|"Path" -> path, "Hash" -> $markdownGlobalsHash, "DryRun" -> $dryRun|>;
  If[$notebookCaching,
    {cachedResult, cachedDate} = Lookup[$notebookToMarkdownCache, path, {None, None}];
    If[ListQ[cachedResult] && fileDate === cachedDate,
      VPrint["  Using cached markdown for ", MsgPath @ path];
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
    VPrint["Applying local katex definitions: \n", katexDefs];
    $localKatexDefinitions = katexDefs];
  If[AssociationQ[katexFns = taggingRules["TemplateToKatexFunctions"]],
    VPrint["Applying local template to katex functions: \n", katexFns];
    $localTemplateToKatexFunctions = katexFns
  ];
  $lastCaption = None; Map[cellToMarkdown, cells]
]

toMarkdownLines[cell_Cell | cell_CellObject | cell_CellGroup] := Scope[
  $lastCaption = $lastExternalCodeCell = None;
  List @ cellToMarkdown @ cell
];

toMarkdownLines[e_] := List[
  "#### toMarkdownLines: unknown expression with head " <> TextString @ Head @ e
];

(**************************************************************************************************)

$textStyleP = "Text" | "Section" | "Subsection";

$graphicsCell = Cell[BoxData[_GraphicsBox], __];

cellToMarkdown = Case[

  cell_CellObject :=
    cellToMarkdown @ NotebookRead @ cell;

  Cell[___, CellDingbat -> "!", ___] := Nothing;

  Cell[e_, style:Except["Input" | "Code"], ___, CellTags -> tag_String, ___] :=
    Splice @ {$anchorTemplate @ tag, cellToMarkdownInner0 @ Cell[e, style]};

  (* JS: style applied incorrectly to contents of cell *)
  Cell[TextData[StyleBox[contents_, style_String]], "Text", ___] :=
    % @ Cell[contents, style];

  (* JS: cell missing style, can happen in embedded cells *)
  Cell[b_BoxData] := % @ Cell[b, "Text"];

  (* JS: this is a degenerate kind of cell pasted in another cell *)
  Cell[TextData[c_Cell], ___] := % @ c;

  (* JS: this handles image-containing cells that are embedded as lines of text *)
  Cell[TextData[{linesL___String, outCell:$graphicsCell, linesR___String}], s:$textStyleP, ___] :=
    Splice @ Map[%, {
      Cell[TextData[{linesL}], s],
      outCell,
      Cell[TextData[{linesR}], s]
    }];

  (* JS: empty cells produce by previous splicing rule *)
  Cell[TextData[{("" | "\n")...}], ___] := Nothing;

  (* JS: no-op cells *)
  Cell[TextData[""] | BoxData[""] | "", ___] := Nothing;

  (* JS: the copyright notice *)
  Cell[BoxData[{linesL___, g_GraphicsBox, linesR___}], s:$textStyleP, ___] :=
    Splice @ Map[%, {
      Cell[BoxData[{linesL}], s],
      Cell[BoxData[g], "Output"],
      Cell[BoxData[{linesR}], s]
    }];

  c:Cell[CellGroupData[{Cell[_, "Input", ___], Cell[_, "Output", ___]}, Open]] /; TrueQ[$rasterizeInputOutputPairs] :=
    cellToRasterMarkdown @ c;

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

PrintBadCell[c_Cell] :=
  CellPrint @ ReplaceOptions[c, {Background -> RGBColor[1,0.95,0.95], CellDingbat -> "!"}];

PrintBadCell[e_] :=
  CellPrint @ Cell[e, "Text", Background -> RGBColor[1,0.95,0.95], CellDingbat -> "!"];

(**************************************************************************************************)

ToMarkdownString::badcell = "Error occurred while converting cell. cellToMarkdownInner1 did not return a string (or Nothing). Instead it returned expression ``, see $LastFailedMarkdownOutput. Entire bad cell is printed below.";

cellToMarkdownInner0[cell_] := Scope[
  result = cellToMarkdownInner1[cell];
  If[StringQ[result] || result === Nothing,
    result
  ,
    $LastFailedMarkdownInput ^= cell; $LastFailedMarkdownOutput ^= result;
    Message[ToMarkdownString::badcell, MsgExpr @ result];
    PrintBadCell[MsgExpr @ result];Beep[];
    PrintBadCell[cell];
    "### BAD CELL"
  ]
];

(* TODO: use stylesheets to call a specific function that will generate markdown per-style *)

cellToMarkdownInner1 = Case[

  Cell["Under construction.", _]         := bannerToMarkdown @ "UNDER CONSTRUCTION";
  Cell[e_, "Banner"]                     := bannerToMarkdown @ e;

  c:Cell[BoxData[GraphicsBox[TagBox[_, _BoxForm`ImageTag, ___], ___]], _] := cellToRasterMarkdown @ c;

  Cell[e_, "Title"]                      := StringJoin[headingDepth @ -1, textCellToMarkdown @ e];
  Cell[e_, "Chapter"]                    := StringJoin[headingDepth @ 0,  textCellToMarkdown @ e];
  Cell[e_, "Section"]                    := StringJoin[headingDepth @ 1,  textCellToMarkdown @ e];
  Cell[e_, "Subsection"]                 := StringJoin[headingDepth @ 2,  textCellToMarkdown @ e];
  Cell[e_, "Subsubsection"]              := StringJoin[headingDepth @ 3,  textCellToMarkdown @ e];
       
  Cell[e_, "Author" | "Affiliation" | "Institution"] := StringJoin["## ", textCellToMarkdown @ e];

  Cell[e_, "Equation"]                   := mathCellToMarkdown @ e;
  Cell[e_, "Exercise"]                   := StringJoin["**Task**: ", textCellToMarkdown @ e];

  Cell[e_, "Text"]                       := insertLinebreaksOutsideKatex[textCellToMarkdown @ e, 120];
  Cell[e_, "Item" | "Item1"]             := StringJoin["* ",     textCellToMarkdown @ e];
  Cell[e_, "Subitem"]                    := StringJoin["\t* ",   textCellToMarkdown @ e];
  Cell[e_, "SubsubItem"]                 := StringJoin["\t\t* ", textCellToMarkdown @ e];
       
  Cell[e_, "ItemNumbered"]               := StringJoin["1. ",        textCellToMarkdown @ e];
  Cell[e_, "SubitemNumbered"]            := StringJoin["\t1.1 ",     textCellToMarkdown @ e];
  Cell[e_, "SubsubItemNumbered"]         := StringJoin["\t\t1.1.1 ", textCellToMarkdown @ e];
       
  Cell[code_, "ExternalLanguage"]        := ($lastExternalCodeCell = code; StringJoin["```python\n", code, "\n```"]);

  (* Cell[b_, "Output"] /; ContainsQ[b, "LinkHand"] := (Beep[]; Nothing); *)
  Cell[b_, "Output"]                     := outputCellToMarkdown @ b;

  Cell[s_String, "PythonOutput"]         := plaintextCodeToMarkdown @ s;
  e:Cell[_, "PythonOutput"]              := cellToRasterMarkdown @ e;

  e:Cell[BoxData[_GraphicsBox], "Print"] := cellToRasterMarkdown @ e;
  
  Cell[m_String, "Markdown"|"Program"]   := m; (* verbatim markdown *)

  Cell[e_, "Code"]                       := ($lastCaption = First[Cases[e, $outputCaptionPattern], None]; Nothing);

  c_                                     := (Nothing);
];

(**************************************************************************************************)

PrivateVariable[$lastCaption, $lastExternalCodeCell]

$lastCaption = $lastExternalCodeCell = None;

$outputCaptionPattern = RowBox[{"(*", RowBox[{"CAPTION", ":", caption___}], "*)"}] :>
  StringJoin @ textBoxesToMarkdownOuter @ RowBox[{caption}];

headingDepth[n_] := StringRepeat["#", Max[n + $headingDepthOffset, 1]] <> " ";

(**************************************************************************************************)

insertLinebreaksOutsideKatex[str_String, n_] := Scope[
  If[$markdownFlavor === "Hugo" || StringLength[str] < n, Return @ str];
  katexSpans = StringPosition[str, Verbatim["$"] ~~ Shortest[___] ~~ Verbatim["$"], Overlaps -> False];
  katexStrings = StringTake[str, katexSpans];
  If[katexSpans === {}, Return @ InsertLinebreaks[str, n]];
  str = InsertLinebreaks[StringReplacePart[str, "€", katexSpans], n];
  i = 1;
  StringReplace[str, "€" :> Part[katexStrings, i++]]
];

insertLinebreaksOutsideKatex[other_, _] := (Print[other]; $Failed)

