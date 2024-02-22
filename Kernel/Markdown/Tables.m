PrivateVariable[$markdownTableReplacement]

$markdownTableReplacement = StartOfLine ~~ " "... ~~ text:Repeated["* " ~~ Except["\n"].. ~~ "\n\n", {2, Inf}] :> createMarkdownTable[text];

(**************************************************************************************************)

createMarkdownTable[ostr_Str] := Scope[
  str = STrim @ ostr;
  lines = SDrop[SSplit[str, "\n"..], 2];
  allowCompact = True;
  If[SStartsQ[F @ lines, "META: "],
    {meta, lines} = FirstRest @ lines;
    allowCompact = SFreeQ[meta, "WIDE"];
  ];
  If[Min[StringCount[Decases["SPACER"] @ lines, "\t"..]] == 0, Return @ ostr];
  grid = STrim /@ SSplit[lines, "\t"..];
  ncols = Len @ F @ grid;
  grid //= MatrixMap[SRep["\"" -> "'"]];
  grid //= MatrixReplace["**_**" -> ""];
  grid //= VectorReplace[{"SPACER"} :> Repeat[" ", ncols]];
  toMarkdownTableString[grid, allowCompact]
];

(**************************************************************************************************)

toMarkdownTableString[grid_, allowCompact_] := Scope[
  If[!AnyMatrixQ[grid],
    Print["Bad table!"];
    Print["First row is: ", F @ grid];
    Print["Row lengths are: ", Len /@ grid];
    PrintIF[grid];
    Return["BADTABLE"];
  ];
  ncols = Len @ F @ grid;
  hasHeader = VecQ[F @ grid, boldedQ];
  strikeRow = Repeat["---", ncols];
  attrs = {};
  If[!hasHeader && $allowTableHeaderSkip,
    AppTo[attrs, "table-no-header"];
    dummyRow = Repeat["z", ncols];
    grid = Join[{dummyRow, strikeRow}, grid];
  ,
    postFn = Id;
    grid = Insert[grid, strikeRow, If[hasHeader || !$allowTableHeaderSkip, 2, 1]];
  ];
  If[ncols > 3 && allowCompact, AppTo[attrs, "table-compact"]];
  tableStr = SJoin @ Map[toMarkdownTableRowString, grid];
  If[attrs =!= {}, tableStr //= $classAttributeTemplate[attrs]];
  SJoin[STrim @ tableStr, "\n\n"]
];

toMarkdownTableRowString[cols_] := SJoin["| ", Riffle[SRep[cols, $tableEscape], " | "], " |\n"];

$tableEscape = "|" -> "&#x007C;"

boldedQ[str_] := SMatchQ[str, Verbatim["*"] ~~ __ ~~ Verbatim["*"]] || SMatchQ[str, "<span style='font-weight:bold'>" ~~ __ ~~ "</span>"];

(**************************************************************************************************)

PrivateFunction[textGridToMarkdown]

textGridToMarkdown[GridBox[grid_, opts___]] := Scope[
  elems = MatrixMap[gridElementToMarkdown /* SRep["\n" -> "<br>"], grid];
  elems //= Decases[{"\[Placeholder]"..}];
  toMarkdownTableString[elems, False]
];

gridElementToMarkdown = Case[
  Cell[TextData[Cell[BoxData @ FormBox[boxes_, ___], ___], ___], ___] := inlineCellToMarkdown[boxes, False];
  Cell[text_, "Text"] := textToMarkdown @ text;
  str_Str := textToMarkdown @ str;
  other_ := Print["UNKNOWN GRID ELEMENT: ", MsgExpr[other, 5, 40]];
];