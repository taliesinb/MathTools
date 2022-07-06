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
  If[Min[StringCount[DeleteCases["SPACER"] @ lines, "\t"..]] == 0,
    Return @ ostr];
  grid = StringTrim /@ StringSplit[lines, "\t"..];
  first = First @ grid;
  ncols = Length @ first;
  grid //= MatrixMap[StringReplace["\"" -> "'"]];
  grid //= MatrixReplace["**_**" -> ""];
  grid //= VectorReplace[{"SPACER"} :> ConstantArray[" ", ncols]];
  If[!MatrixQ[grid],
    Print["Bad table!"];
    Print["First row is: ", First @ grid];
    Print["Row lengths are: ", Length /@ grid];
  ];
  hasHeader = VectorQ[first, boldedQ];
  strikeRow = ConstantArray["---", ncols];
  attrs = {};
  If[!hasHeader,
    AppendTo[attrs, "table-no-header"];
    dummyRow = ConstantArray["z", ncols];
    grid = Join[{dummyRow, strikeRow}, grid];
  ,
    postFn = Identity;
    grid = Insert[grid, strikeRow, If[hasHeader, 2, 1]];
  ];
  If[ncols > 3 && allowCompact,
    AppendTo[attrs, "table-compact"]];
  tableStr = StringJoin @ Map[toTableRowString, grid];
  If[attrs =!= {}, tableStr //= $classAttributeTemplate[attrs]];
  StringJoin[StringTrim @ tableStr, "\n\n"]
];

toTableRowString[cols_] := StringJoin["| ", Riffle[cols, " | "], " |\n"];

boldedQ[str_] := StringMatchQ[str, Verbatim["*"] ~~ __ ~~ Verbatim["*"]];
