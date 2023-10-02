PublicFunction[StartStringPythonSession]

StartStringPythonSession[] := Scope[
  DeleteObject /@ ExternalSessions["Python"];
  StartExternalSession[<|"System" -> "Python", "Name" -> "DefaultPythonSession", "ReturnType" -> "String", "SessionProlog" -> $pythonPrelude|>]
];

$pythonPrelude = """
import matplotlib.pyplot as plt

oldimshow = plt.imshow
def newimshow(*args, **kwargs):
    oldimshow(*args, **kwargs)
    plt.savefig('/tmp/savefig.png', bbox_inches='tight')
    return "file:///tmp/savefig.png"
    
plt.imshow = newimshow
"""

$evaluationModeOuter = True;
FrontEnd`Private`EvaluationModeEvaluate[str_, TextForm, "ExternalLanguage"] /; TrueQ[$evaluationModeOuter] := Block[{$evaluationModeOuter = False},
  pythonPostEval @ FrontEnd`Private`EvaluationModeEvaluate[str, TextForm, "ExternalLanguage"];
];

pythonPostEval["None"] := Null;

pythonPostEval[s_String] :=
  CellPrint @ Cell[s, "PythonOutput"];

pythonPostEval[s_String /; StringStartsQ[s, "'file://"]] :=
  CellPrint @ Cell[BoxData @ ToBoxes @ Import[StringTrimLeftRight[s, "'file://", "'"]], "PythonOutput"];

PublicFunction[CopyImageToInlineHTML]

CopyImageToInlineHTML[e_] := Scope[
  img = Rasterize[Style[e, Antialiasing -> False], ImageResolution-> 72];
  str = ExportString[img, "PNG", CompressionLevel -> 1, "ColorMapLength" -> 16, IncludeMetaInformation -> False];
  code = ToCharacterCode[str];
  result = StringJoin["<center><img width=\"", IntegerString[Round[First[ImageDimensions[img]]*2/3]], "\" src=\"data:image/png;base64,", Base64String[code], "\"></center>"];
  CopyToClipboard[result];
  import = ImportString[str, "PNG"];
  GoodBeep[];
  import
]

JupyterRasterizationFunction[e_] := Scope[
  Which[($lastExternalCodeCell =!= None),
    base64RasterizationFunction["PNG", False] @ e,
  $rasterizationPath =!= None,
    standardRasterizationFunction[e],
  True,
    jpeg = base64RasterizationFunction["JPEG", True] @ e;
    png = base64RasterizationFunction["PNG", True] @ e;
    If[StringLength[jpeg["encoded"]] < StringLength[png["encoded"]], jpeg, png]
  ]
];

PrivateVariable[$JupyterStringImageTemplate]
PrivateVariable[$JupyterFileImageTemplate]

$JupyterStringImageTemplate = StringFunction @ """<center><img src="data:image/#format;base64,#encoded" width="#width"></center>"""
$JupyterFileImageTemplate = StringFunction @ """<center><img width="#width" src="#url"></center>"""

$pngPrefix = StringTake[$JupyterStringImageTemplate[<|"format" -> "png", "width" -> "", "encoded" -> ""|>], 36];

PublicFunction[ExportToJupyter]

Options[ExportToJupyter] = {
  RasterizationPath -> None,
  RasterizationURL -> None,
  MaxItems -> Infinity
};

$JupyterTemplate := $JupyterTemplate = ReadRawJSONFile[LocalPath["Kernel", "Export", "Template.ipynb"]];

$isJupyterTarget = False;

ExportToJupyter[nb:_NotebookObject:Automatic, target_String, OptionsPattern[]] := Scope[
  UnpackOptions[$rasterizationURL, $rasterizationPath, maxItems];
  SetAutomatic[nb, EvaluationNotebook[]];
  paras = ToMarkdownString[nb, True,
    MarkdownFlavor -> "Jupyter",
    RasterizationPath -> $rasterizationPath,
    RasterizationURL -> $rasterizationURL,
    RasterizationFunction -> JupyterRasterizationFunction
  ];
  paras = SequenceReplace[paras, {
    {bull__ ? bulletLineQ} :> clump[bull],
    {in_ ? pyInputQ, out__ ? pyOutputQ} :> inOutPair[in, out]
  }];
  paras = Take[paras, UpTo[maxItems]];
  cellsData = Map[ipyPara, paras];
  ipyNb = ReplacePart[$JupyterTemplate, "cells" -> cellsData];
  outStr = WriteRawJSONString[ipyNb, Compact -> 4];
  outStr = StringReplace[outStr, "\\/" -> "/"];
  target = NormalizePath @ target;
  dir = FileNameDrop[target];
  If[!FileExistsQ[dir] || FileType[dir] =!= Directory, ReturnFailed[]];
  If[FileExtension[target] =!= "ipynb", ReturnFailed[]];
  Print["Writing ", StringLength @ outStr, " to ", target];
  ExportUTF8[target, outStr];
];

pyInputQ[s_] := StringStartsQ[s, "```python"];
pyOutputQ[s_] := StringStartsQ[s, "```" |  $pngPrefix];
bulletLineQ[s_] := StringStartsQ[s, "* "];

ipyPara[inOutPair[in_, out___]] := ipyCode[in, {out}];

ipyPara[str_] := Which[
  StringStartsQ[str, "<center><img>"],      ipyImage @ str,
  StringStartsEndsQ[str, "```", "```"],     ipyCode @ trimBackticks @ str,
  True,                                     ipyMD @ str
];

trimBackticks[str_] := StringTrimLeftRight[str, "```python\n" | "```\n", "\n```"];

ipyPara[clump[strs__]] := <|
  "cell_type" -> "markdown",
  "metadata" -> <||>,
  "source" -> normalizeLines[{strs}]
|>;

ipyMD[str_] := <|
  "cell_type" -> "markdown",
  "metadata" -> <||>,
  "source" -> {str}
|>;

ipyCode[in_, out_:{}] := makeCodeCell[normalizeLines @ splitLines @ trimBackticks @ in, toCodeOutput /@ out];

toCodeOutput[out_] /; StringStartsQ[out, "```"] := <|
  "output_type" -> "execute_result",
  "data" -> <|"text/plain" -> trimBackticks[out]|>,
  "metadata" -> <||>,
  "execution_count" -> 1
|>;

toCodeOutput[out_] /; StringStartsQ[out, $pngPrefix] := <|
  "output_type" -> "display_data",
  "data" -> <|"image/png" -> FirstStringCase[out, "image/png;base64," ~~ Shortest[base64___] ~~ "\"" :> base64]|>,
  "metadata" -> <|"needs_background" ->"light"|>
|>

makeCodeCell[in_, out_List] := <|
  "cell_type" -> "code",
  "execution_count" -> Null,
  "metadata" -> <|"id" -> Base36Hash[out]|>,
  "outputs" -> out,
  "source" -> in
|>

ipyImage[str_] := <|
  "cell_type" -> "markdown",
  "source" -> {str}
|>;

joinLines[s_] := StringRiffle[s, "\n"];

splitLines[s_] := StringSplit[s, "\n"];

normalizeLines = Case[
  str_String := If[StringEndsQ[str, "\n"], str, str <> "\n"];
  list_List  := MapMost[normalizeLines, list];
  other_     := Print[other];
];

PublicFunction[ImportJupyterNotebook]

ImportJupyterNotebook::nefile = "File `` doesn't exist.";
ImportJupyterNotebook::invdata = "Invalid data: got ``, expected ``.";

ImportJupyterNotebook[path_String] := Scope[
  path //= AbsoluteFileName;
  If[!FileExistsQ[path], ReturnFailed["nefile", MsgPath @ path]];
  json = ReadRawJSONFile @ path;
  If[!Association[json], ReturnFailed["invdata", Head @ json, Association]];
  cellData = json["cells"];
  If[!ListQ[cellData], ReturnFailed["invdata", Head @ cellData, List]];
  results = cellImporter[#["cell_type"], #]& /@ cellData;
  CreateDocument @ Flatten @ results
];

cellImporter["code", data_] := {
  Cell[
    StringJoin @ data["source"],
    "ExternalLanguage"
  ],
  Function[procCodeOutput[#["output_type"], #]] /@ data["outputs"]
};

(* need to use  \<\" to stop RowBox from being applied to the OUtput? *)
procCodeOutput["execute_result", data_] :=
  Cell[StringTrim @ data["data", "text/plain"], "PythonOutput"]

procCodeOutput["display_data", data_] := Scope[
  {imgData, textData} = Lookup[data["data"], {"image/png", "text/plain"}];
  If[StringQ[imgData],
    img = ImportBase64[imgData, "PNG"];
    Cell[BoxData @ ToBoxes[img], "PythonOutput"]
  ,
    Cell[BoxData @ StringTrim @ textData, "PythonOutput"]
  ]
];

procCodeOutput["stream", data_] :=
  Cell[BoxData @ StringTrim @ data["text"], "PythonOutput"]

cellImporter["markdown", data_] := Scope[
  source = SequenceReplace[data["source"], {
    {tab__ ? tableLineQ} :> StringJoin[tab]
  }];
  Map[procTextCellLine, source]
];

tableLineQ[s_String] := StringStartsQ[s, "| "];

procTextCellLine[s_String] := Which[
  StringStartsQ[s, "<center><img "],
    Cell[
      BoxData @ ToBoxes @ importDataUrlAsImage @ s,
      "Output"
    ],
  StringStartsQ[s, "* "], Cell[parseMD @ StringTrim @ StringTrim[s, "* "], "Item"],
  StringStartsQ[s, "\t* "], Cell[parseMD @ StringTrim @ StringTrim[s, "* "], "Subitem"],
  StringStartsQ[s, "\t\t* "], Cell[parseMD @ StringTrim @ StringTrim[s, "* "], "Subsubitem"],
  StringStartsQ[s, "# "], Cell[parseMD @ StringTrim @ StringTrim[s, "# "], "Section"],
  StringStartsQ[s, "## "], Cell[parseMD @ StringTrim @ StringTrim[s, "## "], "Subsection"],
  StringStartsQ[s, "### "], Cell[parseMD @ StringTrim @ StringTrim[s, "### "], "Subsubsection"],
  StringStartsQ[s, "#### "], Cell[parseMD @ StringTrim @ StringTrim[s, "#### "], "Subsubsubsection"],
  StringMatchQ[s, Whitespace], Nothing,
  True, markdownCell @ s
];

procTextCell[s_List] := markdownCell @ StringJoin @ s;

parseMD[str_String] /; StringFreeQ[str, "*"|"_"] := str;

Quiet[
markdownSpan[left_, symbol_, right_] := left ~~ symbol:((WordCharacter) | (WordCharacter ~~ Shortest[___] ~~ WordCharacter)) ~~ right;
];

parseMD[str_String] := toTextData @ parseMD1 @ str;

parseMD1[str_String] := StringReplace[str, {
  "<font color='" ~~ color:WordCharacter.. ~~ "'>" ~~ span___ ~~ "</font>" :> StyleBox[parseMD1 @ span, fromHTMLColor @ color],
  markdownSpan["**", span, "**"] :> StyleBox[span, FontWeight->"Bold"],
  markdownSpan["*", span, "*"] :> StyleBox[span, FontSlant->"Italic"],
  markdownSpan["_", span, "_"] :> StyleBox[span, Underlined]
}];

fromHTMLColor["Gray"] = Gray;

toTextData[str_String] := str;
toTextData[str_StringExpression] := TextData[List @@ str];

markdownCell[s_String] := If[
  StringFreeQ[s, "|" | "#" | "<font"] ,
  Cell[parseMD @ StringTrim @ s, "Text"],
  Cell[StringTrim @ s, "Program"]
];

importDataUrlAsImage[str_String] :=
  FirstStringCase[str, "image/png;base64," ~~ Shortest[base64___] ~~ "\"" :> ImportBase64[base64, "PNG"],
    Print[StringTake[str, 100]]; None];

cellImporter[name_] := Cell["UNKNOWN TYPE: " <> name, "Text"]&;
