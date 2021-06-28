

PackageExport["TextCellToMarkdown"]

$MarkdownExportDirectory = "/Users/taliesinb/Library/Mobile Documents/27N4MQEA55~pro~writer/Documents/Quiver Geometry"

(**************************************************************************************************)

PackageExport["CollateNotebooksToMarkdown"]

CollateNotebooksToMarkdown[notebooks_, targetName_] := Scope[
  If[!AllTrue[notebooks, FileType[#] === File&], ReturnFailed[]];
  baseName = FileBaseName @ importPath;
  EnsureDirectory[$MarkdownExportDirectory];
  exportPath = FileNameJoin[{$MarkdownExportDirectory, targetName}];
  imagePath = FileNameJoin[{$MarkdownExportDirectory, "Images"}];
  strings = MonitoredMap[ExportNotebookToMarkdownString[#, imagePath]&, notebooks];
  If[!StringVectorQ[strings], ReturnFailed[]];
  finalString = StringRiffle[StringTrim /@ strings, "\n\n"];
  If[!StringQ[finalString], ReturnFailed[]];
  Export[exportPath, finalString, "Text", CharacterEncoding -> "UTF-8"]
];

(**************************************************************************************************)

PackageExport["ConvertNotebookFileToMarkdown"]

ConvertNotebookFileToMarkdown[importPath_] := Scope[
  If[FileType[importPath] =!= File, ReturnFailed[]];
  baseName = FileBaseName @ importPath;
  EnsureDirectory[$MarkdownExportDirectory];
  exportPath = FileNameJoin[{$MarkdownExportDirectory, baseName <> ".md"}];
  imagePath = FileNameJoin[{$MarkdownExportDirectory, "Images"}];
  string = ExportNotebookToMarkdownString[importPath, imagePath];
  If[!StringQ[string], ReturnFailed[]];
  Export[exportPath, string, "Text", CharacterEncoding -> "UTF-8"]
];

(**************************************************************************************************)

PackageExport["ExportCurrentNotebookToMarkdownClipboard"]

ExportCurrentNotebookToMarkdownClipboard[] := Scope[
  CopyAsUnicode @ ExportCurrentNotebookToMarkdownClipboard @ EvaluationNotebook[];
];

(**************************************************************************************************)

PackageExport["ExportNotebookToMarkdownString"]

ExportNotebookToMarkdownString[nb_, imagePath_:Automatic] := Scope[
  $imageDirectory = imagePath; $skip = False;
  cells = NotebookImport[nb, _ -> "Cell"];
  cells = Take[cells, All, 2];
  skipIndex = FirstIndex[cells, Cell["SKIPREMAINING", _]];
  If[skipIndex =!= None, cells = Take[cells, skipIndex - 1]];
  cells //= DeleteCases[Cell[e_, $textCellP] /; ContainsQ[e, s_String /; StringStartsQ[s, "XXX"]]];
  lines = MonitoredMap[cellToMarkdown, cells];
  result = StringRiffle[lines, "\n\n"];
  result
]

$textCellP = "Section" | "Subsection" | "Subsubsection" | "Text";
cellToMarkdown = Case[

  Cell[e_, "Section"]         := StringJoin["# ", TextToMD @ e];
  Cell[e_, "Subsection"]      := StringJoin["## ", TextToMD @ e];
  Cell[e_, "Subsubsection"]   := StringJoin["### ", TextToMD @ e];
  Cell[e_, "Text"]            := TextToMD @ e;
  e:Cell[_, "Output"]         := OutputToMD @ e;

  _ := Nothing;
];

(**************************************************************************************************)

OutputToMD[cell_] := rasterizeCell @ cell

rasterizeCell[cell_] := Scope[
  image = Rasterize[cell, ImageFormattingWidth -> Infinity, ImageResolution -> 144];
  If[!ImageQ[image], ReturnFailed[]];
  hashString = Base36Hash @ image;
  imageDims = ImageDimensions @ image;
  imageFileName = StringJoin[hashString, "_", toDimsString @ imageDims, ".png"];
  imagePath = FileNameJoin[{$imageDirectory, imageFileName}];
  If[$imageDirectory === Automatic,
    $imageDirectory ^= EnsureDirectory @ FileNameJoin[{$TemporaryDirectory, "wl_md_images"}]];
  If[!FileExistsQ[imagePath] || True,
    Export[imagePath, image, CompressionLevel -> 1]];
  $imageMarkdownTemplate[imagePath, Ceiling[First[imageDims] * 0.4]]
];

$imageMarkdownTemplate = StringTemplate["<img src=\"``\" width=\"``\">"];

$imageMarkdownTemplate = StringTemplate["<img src=\"``\" width=\"``\" style=\"position: relative; display: inline-block; left: 50%; transform: translate(-50%);\">"];

toDimsString[{w_, h_}] := StringJoin[IntegerString[w, 10, 4], "_", IntegerString[h, 10, 4]];

(**************************************************************************************************)

TextToMD[e_] := StringReplace[$finalStringFixups] @ StringTrim @ StringJoin @ iTextToMD @ e;

$finalStringFixups = {
  "  $" -> " $",
  "$  " -> "$ "
};

iTextToMD = Case[
  s_String          := s;
  list_List         := Map[%, list];
  TextData[e_]      := % @ e;
  Cell[BoxData[b_, ___], ___] := inlineMathMD @ b;
  Cell[TextData[t_, ___], ___] := % @ t;
  StyleBox[e_, opts___] := Scope[
    {weight, slant, color} = Lookup[{opts}, {FontWeight, FontSlant, FontColor}, None];
    styledMD[% @ e, weight === "Bold", slant === "Italic"]
  ];
];

styledMD[e_, False, False] := e;
styledMD[e_, False, True] := wrapWith[e, "*"];
styledMD[e_, True, False] := wrapWith[e, "**"];
styledMD[e_, True, True] := wrapWith[e, "***"];

wrapWith[e_, char_] := Scope[
  e = StringJoin @ e;
  p1 = StringStartsQ[e, Whitespace];
  p2 = StringEndsQ[e, Whitespace];
  {If[p1, " ", {}], char, StringTrim @ e, char, If[p2, " ", {}]}
];

(**************************************************************************************************)

inlineMathMD[e_] := {" $", toInlineMathString @ e, "$ "};

toInlineMathString[e_] := StringTrim @ StringReplace[$charReplacements] @ StringJoin @ parseInlineMath @ cleanupInlineBoxes @ e

$directedEdgeChar = "⟶";

$charReplacements = {
  "\[DirectedEdge]" -> $directedEdgeChar,
  "\[CapitalGamma]" -> "\\Gamma ",
  "\[Gamma]" -> "\\gamma ",
  "\[CapitalDelta]" -> "\\Delta ",
  "\[Delta]" -> "\\delta ",
  "\[ScriptS]" -> "\\mathscr{s}",
  "\[ScriptL]" -> "\\mathscr{l}",
  "\[Mu]" -> "\\mu ",
  "\[Phi]" -> "\\phi ",
  "\[Rho]" -> "\\rho ",
  "\[Pi]" -> "\\pi",
  "\[DoubleLeftRightArrow]" -> "\\iff ",
  "\[Star]" -> "\\star ",
  "\[Element]" -> "\\in ",
  "\[CenterDot]" -> "\\cdot ",
  "\[SquareSuperset]" -> "\\sqsupset ",
  "\[SquareSubset]" -> "\\sqsubset ",
  "\[Product]" -> "\\prod ",
  "!=" -> "\\neq ",
  "\[Rule]" -> "\\to "
};

parseInlineMath = Case[
  RowBox[{"{", e__, "}"}] := {"\{", % /@ {e}, "\}"};
  RowBox[e_] := Map[%, e];
  "," := ",";
  " " := " ";
  "_" := "\\_";
  e_String := e;
  c_Cell := iTextToMD @ c;
  "\[DoubleStruckCapitalN]" := "\\mathbb{N}";
  "\[DoubleStruckCapitalF]" := "\\mathbb{F}";
  TemplateBox[{}, "Integers"] := "\\mathbb{Z}";
  StyleBox[e_, opts___] := applyInlineStyle[% @ e, Lookup[{opts}, {FontWeight, FontSlant, FontColor}, None]];
  UnderscriptBox[e_, "_"] := {"\\underline{", % @ e, "}"};
  OverscriptBox[e_, "_"] := {"\\overline{", % @ e, "}"};
  SuperscriptBox[e_, b_] := {% @ e, "^", toBracket @ b};
  SubscriptBox[e_, b_] := {% @ e, "_", toBracket @ b};
  OverscriptBox[e_, "^"] := {"\\hat{", % @ e, "}"};
  TemplateBox[{e_, b_, c_}, "Subsuperscript"] := % @ SuperscriptBox[SubscriptBox[e, b], c];
  TagBox[e_, _] := % @ e;
  RowBox[{"(", "\[NoBreak]", GridBox[grid_, ___], "\[NoBreak]", ")"}] := {"\\begin{pmatrix}", StringRiffle[Map[%, grid, {2}], "\\\\", "&"], "\\end{pmatrix}"};
  UnderoverscriptBox[e_, b_, c_] := % @ SuperscriptBox[SubscriptBox[e, b], c];
  TemplateBox[{a_, b_, tag_}, "DirectedEdge"] :=
    {% @ a, "\\overset{", % @ tag, "}{", $directedEdgeChar, "}", % @ b};
  FractionBox[a_, b_] := {"\\frac{", a, "}{", b, "}"};
  other_ := (Print["UNRECOGNIZED: ", other]; Abort[])
];

applyInlineStyle[e_, {_, _, c:$ColorPattern}] :=
 {"\\htmlStyle{color: #", ColorHexString @ c, ";}{", e, "}"};

applyInlineStyle[e_, _] := e;

toBracket = Case[
  e_String /; StringFreeQ[e, " "] := e;
  other_ := {"{", parseInlineMath @ other, "}"};
];

cleanupInlineBoxes = RightComposition[
  ReplaceRepeated @ {
    FormBox[e_, TraditionalForm] :> e,
    TemplateBox[a_, b_, __] :> TemplateBox[a, b],
    InterpretationBox[e_, ___] :> e,
    AdjustmentBox[a_, ___] :> a,
    TemplateBox[{a_, RowBox[{b_, rest__}], c_}, "DirectedEdge"] :>
      RowBox[{
        TemplateBox[{a, b, c}, "DirectedEdge"],
        rest
      }]
  }
];

(**************************************************************************************************)

CopyAsUnicode[text_] := Scope[
  out = FileNameJoin[{$TemporaryDirectory, "temp_unicode.txt"}];
  Export[out, text, CharacterEncoding -> "UTF-8"];
  Run["osascript -e 'set the clipboard to ( do shell script \"cat " <> out <> "\" )'"];
  DeleteFile[out];
];