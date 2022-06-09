$ArrowSVGTemplate = StringTemplate @ StringTrim @ """
<svg width="24000" height="800" xmlns="http://www.w3.org/2000/svg">

<path d="``"/>

</svg>
""";

PackageExport["LoadKatexArrowPathData"]

$defaultKatexDir = "~/git/tools/KaTeX";

LoadKatexArrowPathData[katexDir_:Automatic] := Scope[
  SetAutomatic[katexDir, $defaultKatexDir];
  svgJSFile = FileNameJoin[{katexDir, "src", "svgGeometry.js"}];
  If[!FileExistsQ[svgJSFile], ReturnFailed[]];
  svgDir = EnsureDirectory @ FileNameJoin[{katexDir, "SVG"}];
  svgJS = ImportUTF8 @ svgJSFile;
  arrows = First @ StringCases[svgJS, $ArrowSVGTop ~~ ___ ~~ $ArrowSVGBot];
  Association @ StringCases[
    arrows,
    StartOfLine ~~ "    " ~~ name:WordCharacter.. ~~ ": `" ~~ path:Except["`"].. ~~ "`," :>
      name -> path
  ]
];

$ArrowSVGInsertion = {
  StartOfLine ~~ "export const path: {[string]: string} = {" ~~ EndOfLine,
  __,
  StartOfLine ~~ "};" ~~ EndOfLine
};

PackageExport["SaveKatexArrowPathData"]

General::noinjpoint = "Could not find injection point for file ``.";
SaveKatexArrowPathData::noarrowpaths = "Could not load arrow data from SVG files in ``.";
SaveKatexArrowPathData::badresult = "Could not produce a string.";

SaveKatexArrowPathData[data_:Automatic, katexDir_:Automatic] := Scope[
  SetAutomatic[katexDir, $defaultKatexDir];
  svgJSFile = FileNameJoin[{katexDir, "src", "svgGeometry.js"}];
  If[!FileExistsQ[svgJSFile], ReturnFailed[]];
  svgJS = ImportUTF8 @ svgJSFile;
  SetAutomatic[data, LoadSVGArrowPathData[katexDir]];
  If[!AssociationQ[data], ReturnFailed["noarrowpaths", katexDir]];
  arrowDataPos = StringFindDelimitedPosition[svgJS, $ArrowSVGInsertion];
  If[!ListQ[arrowDataPos], ReturnFailed["noinjpoint", svgJSFile]];
  {start, stop} = arrowDataPos;
  svgArrowJs = StringTake[svgJS, arrowDataPos];
  commentaryRules = JavascriptCommentaryRules @ svgArrowJs;
  data = KeySortBy[data, arrowEntrySortPosition];
  newArrowPathData = StringJoin @ KeyValueMap[toArrowPathData, data];
  newSvgJS = StringReplacePart[svgJS, newArrowPathData, arrowDataPos];
  newSvgJS = StringReplace[newSvgJS, commentaryRules];
  If[!StringQ[newSvgJS], ReturnFailed["badresult"]];
  ExportUTF8WithBackup[svgJSFile, newSvgJS, svgJS];
];

arrowEntrySortPosition[name_] := {
  First[First[StringPosition[svgArrowJs, WordBoundary ~~ name ~~ WordBoundary, 1], None], Infinity],
  name
};

toArrowPathData[name_, path_] := "\n    " <> name <> ": `" <> path <> "`,\n";

(**************************************************************************************************)

JavascriptCommentaryRules[js_] :=
  toCommentaryRule[js] /@ StringPosition[js, $multilineCommentPattern, Overlaps -> False];

$singleLineCommentPattern = StartOfLine ~~ Whitespace ~~ "//" ~~ Except["\n"].. ~~ EndOfLine;

$emptyLinePattern = StartOfLine ~~ WhitespaceCharacter... ~~ EndOfLine;

$multilineCommentPattern = $singleLineCommentPattern ~~ RepeatedNull["\n" ~~ ($emptyLinePattern | $singleLineCommentPattern)];

toCommentaryRule[js_][{start_, stop_}] := Scope[
  lhs = StringTake[js, {stop + 1, UpTo[stop + 40]}];
  rhs = StringTake[js, {start, UpTo[stop + 40]}];
  If[StringCount[js, lhs] > 1, Nothing, lhs -> rhs]
];


(**************************************************************************************************)

PackageExport["SaveSVGArrowPathData"]

SaveSVGArrowPathData[data_Association, katexDir_:Automatic] := Scope[
  SetAutomatic[katexDir, $defaultKatexDir];
  svgDir = EnsureDirectory @ FileNameJoin[{katexDir, "SVG"}];
  KeyValueMap[saveSVGArrow, data]
];

saveSVGArrow[name_, path_] := ExportUTF8[FileNameJoin[{svgDir, name <> ".svg"}], $ArrowSVGTemplate @ path];

PackageExport["LoadSVGArrowPathData"]

LoadSVGArrowPathData[katexDir_:Automatic] := Scope[
  SetAutomatic[katexDir, $defaultKatexDir];
  svgDir = FileNameJoin[{katexDir, "SVG"}];
  If[!FileExistsQ[svgDir], ReturnFailed[]];
  svgFiles = Sort @ Select[StringFreeQ["-"]] @ FileNames["*.svg", svgDir];
  data = Association @ Map[loadSVGArrow, svgFiles];
  If[ContainsQ[data, $Failed], ReturnFailed[]];
  data
];

loadSVGArrow[name_] := StringTrim[FileNameTake[name], ".svg"] -> extractSVGPath[name];

$ArrowSVGPathPattern = "<path d=\"" ~~ path:Except["\""].. ~~ "\"" :> path;

LoadSVGArrowPathData::badpath = "Could not find path in ``."

extractSVGPath[name_] :=
  FirstStringCase[
    ImportUTF8 @ name,
    $ArrowSVGPathPattern,
    ReturnFailed[LoadSVGArrowPathData::badpath, name]
  ];

(**************************************************************************************************)

PackageExport["InjectCustomArrows"]

$LeftArrowSpecs = {
  named["bar", "leftbar"],
  named["barup", "leftbarup"],
  named["bardown", "leftbardown"],
  named["bullet", "leftbullet"],
  "line",
  "leftharpoonup", "twoleftharpoonsup",
  "leftarrow", "twoleftarrows",
  named["hook", "lefthook"],
  named["downhook", "leftdownhook"],
  named["updownhook", "leftupdownhook"],
  "leftflatarrow"
};

$MidArrowSpecs = {
  "line", "midbullet", "midbar", "midbarup", "midbardown", "midtilde"
};

$RightArrowSpecs = {
  named["bar", "rightbar"],
  named["barup", "rightbarup"],
  named["bardown", "rightbardown"],
  named["bullet", "rightbullet"],
  "line",
  "rightharpoon", "tworightharpoons",
  "rightarrow", "tworightarrows",
  "rightflatarrow"
};

$existingTuples = Alternatives[
  {"line", "line", "rightarrow"},
  {"leftarrow", "line", "line"}
];

InjectCustomArrows[katexDir_:Automatic] := Scope[
  SetAutomatic[katexDir, $defaultKatexDir];
  stretchyFile = FileNameJoin[{katexDir, "src", "stretchy.js"}];
  arrowFile = FileNameJoin[{katexDir, "src", "functions", "arrow.js"}];
  If[!FileExistsQ[stretchyFile] || !FileExistsQ[arrowFile], ReturnFailed[]];
  stretchyJS = ImportUTF8[stretchyFile];
  customPos = StringFindDelimitedPosition[stretchyJS, $StretchyJSInsertion];
  If[customPos === $Failed, ReturnFailed["noinjpoint", stretchyFile]];
  arrowTuples = Tuples[{$LeftArrowSpecs, $MidArrowSpecs, $RightArrowSpecs}];
  arrowTuples //= DeleteCases[$existingTuples];
  arrowNames = Map[toArrowName, ReplaceAll[arrowTuples, named[k_, _] :> k]];
  arrowValues = ReplaceAll[arrowTuples, named[_, v_] :> v];
  arrowSpecs = Map[toArrowSpec /* quotedTuple, arrowValues];
  arrowLens = Map[toArrowLen, arrowValues];
  arrowAlignments = Map[toArrowAlignments /* quotedTuple, arrowValues];
  arrowStrings = MapThread[$CustomArrowStringTemplate, {arrowNames, arrowSpecs, arrowLens, arrowAlignments}];
  newStretchyJS = StringReplacePart[stretchyJS, StringJoin["\n", arrowStrings, "\n"], customPos];
  ExportUTF8WithBackup[stretchyFile, newStretchyJS, stretchyJS];
  
  arrowJS = ImportUTF8[arrowFile];
  customPos = StringFindDelimitedPosition[arrowJS, $ArrowJSInsertion];
  If[customPos === $Failed, ReturnFailed["noinjpoint", arrowFile]];
  newArrowString = StringJoin["\n\n        ", Riffle[Map[QuotedString["\\\\" <> #]&, arrowNames], ", "], "\n"];
  newArrowJS = StringReplacePart[arrowJS, newArrowString, customPos];
  ExportUTF8WithBackup[arrowFile, newArrowJS, arrowJS];
];

$StretchyJSInsertion = {
"""    xleftequilibrium: [["shortbaraboveleftharpoon",
        "shortrightharpoonabovebar"], 1.75, 716],""",
__,
"""};

const groupLength"""
};

$ArrowJSInsertion = {
"""
        "\\\\cdrightarrow", "\\\\cdleftarrow", "\\\\cdlongequal",""",
__,
"""    ],
    props: """
};

$CustomArrowStringTemplate = StringTemplate @ """
    ``: [``, ``, 522, ``],""";

toArrowName[tuple_] :=
  StringJoin["x", DeleteCases[tuple, "line"]];

toArrowSpec = Case[
  {"line", "midbullet", v_}  := {"rightbullet", v};   {v_, "midbullet", "line"}      := {v, "leftbullet"};
  {"line", "midbar", v_}     := {"rightbar", v};      {v_, "midbar", "line"}         := {v, "leftbar"};
  {"line", "midbarup", v_}   := {"rightbarup", v};    {v_, "midbarup", "line"}       := {v, "leftbarup"};
  {"line", "midbardown", v_} := {"rightbardown", v};  {v_, "midbardown", "line"}     := {v, "leftbardown"};
  tuple_ := toArrowSpec2 @ tuple
];

toArrowLen = Case[
  {_, "line", _}                                  := "1.08";
  {_, _, "tworightarrows" | "tworightharpoons"}   := "1.5";
  {"twoleftarrows" | "twoleftharpoons", _, _}     := "1.5";
  _                                               := "1.08";
];

toArrowSpec2 = Case[
  {"line", "line", x_}  := {x};
  {x_, "line", y_}      := {x, y};
  other_                := other;
];

$mids = "midbullet"|"midbar"|"midbarup"|"midbardown";
toArrowAlignments = Case[
  {"line", $mids, _}  := {"xMaxYMin", "xMaxYMin"};
  {_, $mids, "line"}  := {"xMinYMin", "xMinYMin"};
  e_                  := "xMaxYMin";
];

quotedTuple = Case[
  e_String := QuotedString[e];
  e_List := StringRiffle[Map[%, e], {"[", ",", "]"}];
];