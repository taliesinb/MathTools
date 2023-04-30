PrivateVariable[$markdownTableReplacement]

$markdownTableReplacement = StartOfLine ~~ " "... ~~ text:Repeated["* " ~~ Except["\n"].. ~~ "\n\n", {2, Infinity}] :> createMarkdownTable[text];

(**************************************************************************************************)

createMarkdownTable[ostr_String] := Scope[
  str = StringTrim @ ostr;
  lines = StringDrop[StringSplit[str, "\n"..], 2];
  allowCompact = True;
  If[StringStartsQ[First @ lines, "META: "],
    {meta, lines} = FirstRest @ lines;
    allowCompact = StringFreeQ[meta, "WIDE"];
  ];
  If[Min[StringCount[DeleteCases["SPACER"] @ lines, "\t"..]] == 0, Return @ ostr];
  grid = StringTrim /@ StringSplit[lines, "\t"..];
  ncols = Length @ First @ grid;
  grid //= MatrixMap[StringReplace["\"" -> "'"]];
  grid //= MatrixReplace["**_**" -> ""];
  grid //= VectorReplace[{"SPACER"} :> ConstantArray[" ", ncols]];
  toMarkdownTableString[grid, allowCompact]
];

(**************************************************************************************************)

toMarkdownTableString[grid_, allowCompact_] := Scope[
  If[!MatrixQ[grid],
    Print["Bad table!"];
    Print["First row is: ", First @ grid];
    Print["Row lengths are: ", Length /@ grid];
    Return["BADTABLE"];
  ];
  ncols = Length @ First @ grid;
  hasHeader = VectorQ[First @ grid, boldedQ];
  strikeRow = ConstantArray["---", ncols];
  attrs = {};
  If[!hasHeader && $allowTableHeaderSkip,
    AppendTo[attrs, "table-no-header"];
    dummyRow = ConstantArray["z", ncols];
    grid = Join[{dummyRow, strikeRow}, grid];
  ,
    postFn = Identity;
    grid = Insert[grid, strikeRow, If[hasHeader || !$allowTableHeaderSkip, 2, 1]];
  ];
  If[ncols > 3 && allowCompact, AppendTo[attrs, "table-compact"]];
  tableStr = StringJoin @ Map[toMarkdownTableRowString, grid];
  If[attrs =!= {}, tableStr //= $classAttributeTemplate[attrs]];
  StringJoin[StringTrim @ tableStr, "\n\n"]
];

toMarkdownTableRowString[cols_] := StringJoin["| ", Riffle[cols, " | "], " |\n"];

boldedQ[str_] := StringMatchQ[str, Verbatim["*"] ~~ __ ~~ Verbatim["*"]] || StringMatchQ[str, "<span style='font-weight:bold'>" ~~ __ ~~ "</span>"];

(**************************************************************************************************)

PrivateFunction[textGridToMarkdown]

textGridToMarkdown[GridBox[grid_, opts___]] := Scope[
  elems = MatrixMap[gridElementToMarkdown /* StringReplace["\n" -> "<br>"], grid];
  elems //= DeleteCases[{"\[Placeholder]"..}];
  toMarkdownTableString[elems, False]
];

gridElementToMarkdown = Case[
  Cell[TextData[Cell[BoxData @ FormBox[boxes_, ___], ___], ___], ___] := inlineCellToMarkdown[boxes, False];
  Cell[text_, "Text"] := textBoxesToMarkdown[text];
  str_String := textBoxesToMarkdown[str];
  other_ := Print["UNKNOWN GRID ELEMENT: ", MsgExpr[other, 5, 40]];
];