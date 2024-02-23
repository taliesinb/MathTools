PublicIOFunction[StartStringPythonSession]

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

pythonPostEval[s_Str] :=
  CellPrint @ Cell[s, "PythonOutput"];

pythonPostEval[s_Str /; SStartsQ[s, "'file://"]] :=
  CellPrint @ Cell[BoxData @ ToBoxes @ Import[StringTrimLeftRight[s, "'file://", "'"]], "PythonOutput"];

PublicIOFunction[CopyImageToInlineHTML]

CopyImageToInlineHTML[e_] := Scope[
  img = Rasterize[Style[e, Antialiasing -> False], ImageResolution-> 72];
  str = ExportString[img, "PNG", CompressionLevel -> 1, "ColorMapLength" -> 16, IncludeMetaInformation -> False];
  code = ToCharCode[str];
  result = SJoin["<center><img width=\"", IntStr[Round[F[ImageDimensions[img]]*2/3]], "\" src=\"data:image/png;base64,", Base64String[code], "\"></center>"];
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
    If[SLen[jpeg["encoded"]] < SLen[png["encoded"]], jpeg, png]
  ]
];

PrivateVariable[$JupyterStringImageTemplate]
PrivateVariable[$JupyterFileImageTemplate]

$JupyterStringImageTemplate = StringFunction @ """<center><img src="data:image/#format;base64,#encoded" width="#width"></center>"""
$JupyterFileImageTemplate = StringFunction @ """<center><img width="#width" src="#url"></center>"""

$pngPrefix = STake[$JupyterStringImageTemplate[<|"format" -> "png", "width" -> "", "encoded" -> ""|>], 36];

PublicIOFunction[ExportToJupyter]

Options[ExportToJupyter] = {
  RasterizationPath -> None,
  RasterizationURL -> None,
  MaxItems -> Inf
};

SetCached[$JupyterTemplate, ReadRawJSONFile @ DataPath["Jupyter", "Template.ipynb"]];

$isJupyterTarget = False;

ExportToJupyter[nb:_NotebookObject:Auto, target_Str, OptionsPattern[]] := Scope[
  UnpackOptions[$rasterizationURL, $rasterizationPath, maxItems];
  SetAuto[nb, EvaluationNotebook[]];
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
  ipyNb = RepPart[$JupyterTemplate, "cells" -> cellsData];
  outStr = WriteRawJSONString[ipyNb, Compact -> 4];
  outStr = SRep[outStr, "\\/" -> "/"];
  target = NormalizePath @ target;
  dir = FileNameDrop[target];
  If[!FileExistsQ[dir] || FileType[dir] =!= Directory, ReturnFailed[]];
  If[FileExtension[target] =!= "ipynb", ReturnFailed[]];
  Print["Writing ", SLen @ outStr, " to ", target];
  ExportUTF8[target, outStr];
];

pyInputQ[s_] := SStartsQ[s, "```python"];
pyOutputQ[s_] := SStartsQ[s, "```" |  $pngPrefix];
bulletLineQ[s_] := SStartsQ[s, "* "];

ipyPara[inOutPair[in_, out___]] := ipyCode[in, {out}];

ipyPara[str_] := Which[
  SStartsQ[str, "<center><img>"],      ipyImage @ str,
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

toCodeOutput[out_] /; SStartsQ[out, "```"] := <|
  "output_type" -> "execute_result",
  "data" -> <|"text/plain" -> trimBackticks[out]|>,
  "metadata" -> <||>,
  "execution_count" -> 1
|>;

toCodeOutput[out_] /; SStartsQ[out, $pngPrefix] := <|
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

joinLines[s_] := SRiffle[s, "\n"];

splitLines[s_] := SSplit[s, "\n"];

normalizeLines = Case[
  str_Str := If[SEndsQ[str, "\n"], str, str <> "\n"];
  list_List  := MapMost[normalizeLines, list];
  other_     := Print[other];
];

PublicIOFunction[ImportJupyterNotebook]

ImportJupyterNotebook::nefile = "File `` doesn't exist.";
ImportJupyterNotebook::invdata = "Invalid data: got ``, expected ``.";

ImportJupyterNotebook[path_Str] := Scope[
  path //= AbsoluteFileName;
  If[!FileExistsQ[path], ReturnFailed["nefile", MsgPath @ path]];
  json = ReadRawJSONFile @ path;
  If[!Assoc[json], ReturnFailed["invdata", H @ json, Assoc]];
  cellData = json["cells"];
  If[!ListQ[cellData], ReturnFailed["invdata", H @ cellData, List]];
  results = cellImporter[#["cell_type"], #]& /@ cellData;
  CreateDocument @ Flatten @ results
];

cellImporter["code", data_] := {
  Cell[
    SJoin @ data["source"],
    "ExternalLanguage"
  ],
  Fn[procCodeOutput[#["output_type"], #]] /@ data["outputs"]
};

(* need to use  \<\" to stop RowBox from being applied to the OUtput? *)
procCodeOutput["execute_result", data_] :=
  Cell[STrim @ data["data", "text/plain"], "PythonOutput"]

procCodeOutput["display_data", data_] := Scope[
  {imgData, textData} = Lookup[data["data"], {"image/png", "text/plain"}];
  If[StrQ[imgData],
    img = ImportBase64[imgData, "PNG"];
    Cell[BoxData @ ToBoxes[img], "PythonOutput"]
  ,
    Cell[BoxData @ STrim @ textData, "PythonOutput"]
  ]
];

procCodeOutput["stream", data_] :=
  Cell[BoxData @ STrim @ data["text"], "PythonOutput"]

cellImporter["markdown", data_] := Scope[
  source = SequenceReplace[data["source"], {
    {tab__ ? tableLineQ} :> SJoin[tab]
  }];
  Map[procTextCellLine, source]
];

tableLineQ[s_Str] := SStartsQ[s, "| "];

procTextCellLine[s_Str] := Which[
  SStartsQ[s, "<center><img "],
    Cell[
      BoxData @ ToBoxes @ importDataUrlAsImage @ s,
      "Output"
    ],
  SStartsQ[s, "* "], Cell[parseMD @ STrim @ STrim[s, "* "], "Item"],
  SStartsQ[s, "\t* "], Cell[parseMD @ STrim @ STrim[s, "* "], "Subitem"],
  SStartsQ[s, "\t\t* "], Cell[parseMD @ STrim @ STrim[s, "* "], "Subsubitem"],
  SStartsQ[s, "# "], Cell[parseMD @ STrim @ STrim[s, "# "], "Section"],
  SStartsQ[s, "## "], Cell[parseMD @ STrim @ STrim[s, "## "], "Subsection"],
  SStartsQ[s, "### "], Cell[parseMD @ STrim @ STrim[s, "### "], "Subsubsection"],
  SStartsQ[s, "#### "], Cell[parseMD @ STrim @ STrim[s, "#### "], "Subsubsubsection"],
  SMatchQ[s, Whitespace], Nothing,
  True, markdownCell @ s
];

procTextCell[s_List] := markdownCell @ SJoin @ s;

parseMD[str_Str] /; SFreeQ[str, "*"|"_"] := str;

Block[{Message, MessageName}, (* prevents the RuleDelayed::rhs message from even loading, which is slow disk access *)
markdownSpan[left_, symbol_, right_] := left ~~ symbol:((WordCharacter) | (WordCharacter ~~ Shortest[___] ~~ WordCharacter)) ~~ right;
];

parseMD[str_Str] := toTextData @ parseMD1 @ str;

parseMD1[str_Str] := SRep[str, {
  "<font color='" ~~ color:WordCharacter.. ~~ "'>" ~~ span___ ~~ "</font>" :> StyleBox[parseMD1 @ span, fromHTMLColor @ color],
  markdownSpan["**", span, "**"] :> StyleBox[span, FontWeight->"Bold"],
  markdownSpan["*", span, "*"] :> StyleBox[span, FontSlant->"Italic"],
  markdownSpan["_", span, "_"] :> StyleBox[span, Underlined]
}];

fromHTMLColor["Gray"] = Gray;

toTextData[str_Str] := str;
toTextData[str_StringExpression] := TextData[List @@ str];

markdownCell[s_Str] := If[
  SFreeQ[s, "|" | "#" | "<font"] ,
  Cell[parseMD @ STrim @ s, "Text"],
  Cell[STrim @ s, "Program"]
];

importDataUrlAsImage[str_Str] :=
  FirstStringCase[str, "image/png;base64," ~~ Shortest[base64___] ~~ "\"" :> ImportBase64[base64, "PNG"],
    Print[STake[str, 100]]; None];

cellImporter[name_] := Cell["UNKNOWN TYPE: " <> name, "Text"]&;