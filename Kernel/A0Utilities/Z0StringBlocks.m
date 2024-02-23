PrivateHead[$block, $hspace]

(**************************************************************************************************)


PublicFunction[ToStringBlock]
PublicVariable[$StringBlockHeads]

$StringBlockHeads = {};

PrivateFunction[processCustomBlock]

ClearAll[processCustomBlock];

ToStringBlock /: SetDelayed[ToStringBlock[lhs_], rhs_] := With[
  {head = F @ PatternHead[lhs]},
  AppTo[$StringBlockHeads, head];
  DefineStandardTraditionalForm[block:lhs :> ToBoxes @ StringBlockForm @ block];
  held = HoldC[processCustomBlock[lhs] := rhs];
  held = held /. {
    ToStringBlock -> processBlock,
    Verbatim[OptionsPattern[]] -> $opts___Rule,
    UnpackOptions[syms___] :> UnpackOptionsAs[head, $opts, syms]
  };
  F @ held
];

(**************************************************************************************************)

PublicTypesettingForm[StringTable]

PublicOption[TableHeadingStyle]

Options[StringTable] = {
  TableHeadings -> None,
  TableHeadingStyle -> $Gray
}

ToStringBlock[StringTable[rows_List, OptionsPattern[]]] := Scope[
  If[!AnyMatrixQ[rows], ThrowMessage["notmatrix", MsgExpr @ rows]];
  items = MatrixMap[ToStringBlock, rows];
  UnpackOptions[tableHeadings, tableHeadingStyle];
  {rowStyle, colStyle} = tableHeadingStyle * {1, 1};
  SetAuto[tableHeadings, {Auto, Auto}];
  SetNone[tableHeadings, {None, None}];
  If[!MatchQ[tableHeadings, {_, _}], ReturnFailed[]];
  {rowHeadings, colHeadings} = tableHeadings;
  {numRows, numCols} = Dims[rows, 2];
  colAlignment = Repeat[Left, numCols];
  If[rowHeadings =!= None,
    $headingStyleFn = StyleOperator @ rowStyle;
    rowHeadings = procTableHeadings[rowHeadings, numRows];
    items = MapThread[Pre, {items, rowHeadings}];
  ];
  If[colHeadings =!= None,
    $headingStyleFn = StyleOperator @ colStyle;
    colHeadings = procTableHeadings[colHeadings, numCols];
    If[rowHeadings =!= None, PreTo[colHeadings, spacerBlock[1, 1]]];
    PreTo[colAlignment, Right];
    PreTo[items, colHeadings];
  ];
  gstackBlocks[items, ColumnSpacings -> 1, ColumnAlignments -> colAlignment]
];

StringBlock::badtableheading = "`` is not a recognized setting for TableHeadings."
procTableHeadings[Auto, n_] := procTableHeadings[Range @ n, n];
procTableHeadings[str_Str, n_] := procTableHeadings[Chars @ str, n]
procTableHeadings[list_List, n_] := processBlock[$headingStyleFn[#]]& /@ PadRight[list, n, ""];
procTableHeadings[other_, _] := ThrowMessage["badtableheading", other];

(**************************************************************************************************)

PublicTypesettingForm[StringMatrix]

PublicOption[FramePadding, RowFrames, RowFrameStyle, RowFramePadding, SpanningFrame]

Options[StringMatrix] = {
  RowAlignments -> Top,
  ColumnAlignments -> Left,
  RowSpacings -> 0,
  ColumnSpacings -> 1,
  Dividers -> None,
  Frame -> None,
  FramePadding -> None,
  FrameStyle -> None,
  RowFrames -> None,
  RowFrameStyle -> None,
  RowFramePadding -> None,
  SpanningFrame -> False,
  Background -> None
};

StringBlock::notmatrix = "Expected a matrix, found ``."

ToStringBlock[StringMatrix[rows_List, opts___Rule]] := Scope[
  If[!AnyMatrixQ[rows], ThrowMessage["notmatrix", MsgExpr @ rows]];
  gstackBlocks[MatrixMap[ToStringBlock, rows], opts]
];

(**************************************************************************************************)

PublicTypesettingForm[StringBlockTemplate]

StringBlock::badtemplate = "Template `` should be a string or list of strings."
StringBlock::badtemplatearg = "`` reqeuested, but only `` available."
ToStringBlock[StringBlockTemplate[template_, args___]] := Scope[
  lines = Switch[template,
    _Str,     SSplit[template, "\n"],
    {___Str}, template,
    _,           ThrowMessage["badtemplate", template];
  ];
  items = {args};
  $sbtAlign = Center;
  If[MatchQ[L[items, None], Alignment -> _], $sbtAlign = Part[items, -1, 2]; items = Most[items]];
  items = Map[ToStringBlock, items];
  lines = SRep[lines, {
    "#" ~~ i:DigitCharacter :> $rawBlock @ SafePart[items, FromDigits @ i],
    "%" ~~ i:DigitCharacter :> $rawBlock @ blockToHSpace @ SafePart[items, FromDigits @ i]
  }] /. Missing["PartAbsent", j_] :> ThrowMessage["badtemplatearg", j, Len @ items];
  verticalBlocks = Map[blockLine, lines];
  vstackBlocks[verticalBlocks, Left]
];

blockLine = Case[
  SExpr[a___] := % @ {a};
  s_Str                  := % @ {s};
  list_List              := hstackBlocks[Map[processBlock, list], $sbtAlign];
];

blockToHSpace[$block[_, w_, h_]] := spacerBlock[w, 1];

(**************************************************************************************************)

PublicTypesettingForm[StringRow]

Options[StringRow] = {
  RowAlignments -> Top,
  ColumnSpacings -> 0,
  Frame -> None,
  FramePadding -> None,
  FrameStyle -> None,
  SpanningFrame -> False,
  Background -> None
}

ToStringBlock[StringRow[items_List, OptionsPattern[]]] := Scope[
  items = Map[ToStringBlock, items];
  UnpackOptions[rowAlignments, columnSpacings, frame, framePadding, frameStyle, spanningFrame, background];
  frameStyle //= normFrameStyle;
  If[columnSpacings =!= 0, items = riffle[items, Spacer[columnSpacings]]];
  block = hstackBlocks[items, rowAlignments];
  block = blockPadding[block, framePadding];
  hframeBlock[block, frame, frameStyle, spanningFrame, background]
];

(**************************************************************************************************)

PublicTypesettingForm[StringColumn]

Options[StringColumn] = {
  ColumnAlignments -> Left,
  RowSpacings -> 0,
  Frame -> None,
  FramePadding -> None,
  FrameStyle -> None,
  SpanningFrame -> False,
  Background -> None
}

ToStringBlock[StringColumn[items_List, OptionsPattern[]]] := Scope[
  items = Map[ToStringBlock, items];
  UnpackOptions[columnAlignments, rowSpacings, frame, framePadding, frameStyle, spanningFrame, background];
  frameStyle //= normFrameStyle;
  If[rowSpacings =!= 0, items = riffle[items, Spacer[{1, rowSpacings}]]];
  block = vstackBlocks[items, columnAlignments];
  block = blockPadding[block, framePadding];
  hframeBlock[block, frame, frameStyle, spanningFrame, background]
];

(**************************************************************************************************)

PublicTypesettingForm[StringFrame]

Options[StringFrame] = {
  Frame -> True,
  FrameStyle -> None,
  Background -> None
};

ToStringBlock[StringFrame[item_, OptionsPattern[]]] := Scope[
  UnpackOptions[frame, frameStyle, background];
  hframeBlock[ToStringBlock @ item, frame, frameStyle, True, background]
];

(**************************************************************************************************)

PublicTypesettingForm[FrameLabeled]

Options[FrameLabeled] = {
  LabelSpacing -> {1, 0},
  LabelStyle -> $Gray,
  FrameTicks -> False (* TODO: make this introduce little marks in the gap *)
};

ToStringBlock[FrameLabeled[item_, specs_, OptionsPattern[]]] := Scope[
  item //= ToStringBlock;
  UnpackOptions[labelSpacing, frameTicks, labelStyle];
  {hlabelSpacing, vlabelSpacing} = {1, 1} * labelSpacing;
  $hoffset = 0; $voffset = 0; $labelStyleFn = StyleOperator @ labelStyle;
  Fold[applyFrameLabel, item, ToList @ specs]
];

StringBlock::badflspec = "Bad FrameLabel spec ``."

applyFrameLabel[block_, side_ -> Style[items_, style___]] := Scope[
  $labelStyleFn = StyleOperator @ style;
  applyFrameLabel[block, side -> items]
];

toLabelItems[pos_Int -> label_] := pos -> label;
toLabelItems[pos_ -> labels_] := Scope[
  labels //= toLabelValues;
  pos = If[H[pos] == Span, Part[Range[100], pos], pos];
  Splice @ RuleThread[Take[pos, Len @ labels], labels]
];

toLabelValues = Case[
  str_Str := Chars @ str;
  list_List := list;
]

applyFrameLabel[block_, side:(Left|Right) -> spec_] := Scope[
  items = Map[toLabelItems, ToList @ spec];
  isLeft = side === Left;
  rows = Repeat[$block[{""}, 0, 1], P3[block]];
  rules = (#1 + $voffset) -> clipOneLine[processBlock @ $labelStyleFn @ #2]& @@@ Sort[items];
  rows = RepPart[rows, rules];
  labelBlock = vstackBlocks[rows, If[isLeft, Right, Left]];
  labelBlock = padBlock[labelBlock, If[isLeft, Right, Left] -> hlabelSpacing];
  If[isLeft, $hoffset ^= $hoffset + P2[labelBlock]];
  hstackBlocks[If[isLeft, {labelBlock, block}, {block, labelBlock}], Top]
];

StringBlock::overlapfl = "Overlapping FrameLabel spec ``."

applyFrameLabel[block_, side:(Top|Bottom) -> spec_] := Scope[
  isTop = side === Top;
  spec = Map[toLabelItems, ToList @ spec];
  {positions, items} = KeysValues @ Sort @ spec;
  positions += $hoffset;
  itemBlocks = Map[processBlock @ $labelStyleFn @ #&, items];
  itemWidths = Col2[itemBlocks];
  gaps = Differences @ Pre[1] @ positions;
  gaps -= Pre[0] @ Most @ itemWidths;
  If[Min[gaps] < 0, ThrowMessage["overlapfl", side -> spec]];
  spacerBlocks = Map[spacerBlock[#, 1]&, gaps];
  blocks = Catenate @ Transpose[{spacerBlocks, itemBlocks}];
  labelBlock = hstackBlocks[blocks, If[isTop, Bottom, Top]];
  If[isTop, $voffset ^= $voffset + P3[labelBlock]];
  vstackBlocks[If[isTop, {labelBlock, block}, {block, labelBlock}], Left]
];

applyFrameLabel[_, spec_] := ThrowMessage["badflspec", spec];

clipOneLine = Case[
  block:$block[_, _, 1] := block;
  $block[rows_, w_, _] := $block[Take[rows, 1], w, 1]
];

(**************************************************************************************************)

PublicTypesettingForm[StringBlockForm]

Options[StringBlockForm] = {
  StylingFunction -> "Linear"
}

DefineStandardTraditionalForm[
  sb:StringBlockForm[_, ___Rule] :> makeStringBlockFormBoxes[sb]
];

(* the first template slot is linear syntax for FE display,
the second is HTML, which is ready to be inserted straight into markdown as-is.
*)
makeStringBlockFormBoxes[StringBlockForm[b_, opts___Rule]] :=
  TemplateBox[{
    ToString[#, InputForm]& @ fixExtensibleChars @ StringBlock[b, StylingFunction -> "Linear", opts],
    StringBlock[b, StylingFunction -> "HTML", opts]
  },
  "StringBlockForm"
];

(**************************************************************************************************)

PublicFunction[HighlightBackgroundAt]

HighlightBackgroundAt[n_Int, parts___] := MapAt[BackgroundNForm[n], {{parts}}];

(**************************************************************************************************)

PublicFunction[StringBlock]

PublicOption[StylingFunction]

Options[StringBlock] = Options[StringBlockForm];

StringBlock[e_, OptionsPattern[]] := CatchMessage @ Scope[
  UnpackOptions[stylingFunction];
  $styleStack = {};
  $stylingFunction = Switch[stylingFunction,
    "Linear", linearStyling,
    "HTML",   htmlStyling,
    "Input",  inputStyling,
    "Cell",   cellStyling,
    None,     #1&,
    _,        stylingFunction
  ];
  fragments = blockToStrings @ processBlock @ e;
  If[stylingFunction === "Cell",
    Cell[
      TextData @ RepRep[Flatten @ fragments, {l___, ls_Str, rs_Str, r___} :> {l, ls <> rs, r}],
      "PreformattedCode"
    ],
    SJoin @ fragments
  ]
];

(* these have special code in toCodeMarkdown to pick them up *)
cellStyling[e_, {l___, "Subscript", r___}] := Cell[BoxData @ SubscriptBox["", cellStyling[e, {l, r}]]];
cellStyling[e_, {l___, "Midscript", r___}] := StyleBox[cellStyling[e, {l, r}], FontSize -> 9.2];
cellStyling[e_, {l___, "Superscript", r___}] := Cell[BoxData @ SuperscriptBox["", cellStyling[e, {l, r}]]]
cellStyling[e_, {s__}] := StyleBox[e, s];
cellStyling[e_, {}] := e;

wrapCode[code_, str_] := If[SingleLetterQ[str], {name, ":", str}, {name, "{", str, "}"}];

inputStyling[e_, {l___, "Subscript", r___}] := {"_{", inputStyling[e, {l, r}], "}"};
inputStyling[e_, {l___, "Midscript", r___}] := {"_{", inputStyling[e, {l, r}], "}"};
inputStyling[e_, {l___, "Superscript", r___}] := {"^{", inputStyling[e, {l, r}], "}"};
inputStyling[e_, {currentStyleSetting[FontColor, cname_]}] := wrapCode[{"F", STake[cname, -1]}, e];
inputStyling[e_, {FontColor -> color_}] := wrapCode[$colorShortcodeMappingInverse @ color, e];
inputStyling[e_, s_] := e;

htmlStyling[e_, {l___, "Subscript", r___}] := {"<sub>", htmlStyling[e, {l, r}], "</sub>"};
htmlStyling[e_, {l___, "Midscript", r___}] := {"<sub>", htmlStyling[e, {l, r}], "</sub>"};
htmlStyling[e_, {l___, "Superscript", r___}] := {"<sup>", htmlStyling[e, {l, r}], "</sup>"};
htmlStyling[e_, style_] := htmlStyledString[e, style];

linearStyling[e_, {l___, "Subscript", r___}] := linearStyling[Subscript["", e], {l, r}];
linearStyling[e_, {l___, "Midscript", r___}] := linearStyling[Subscript["", e], {l, r}];
linearStyling[e_, {l___, "Superscript", r___}] := linearStyling[Superscript["", e], {l, r}];
linearStyling[e_Str, {}] := e;
linearStyling[e_, {}] := ToString[e, StandardForm];
linearStyling[e_, {s__}] := ToString[Style[e, s], StandardForm];

StringBlock::badalign = "Bad alignment ``: should be one of ``."

blockToStrings = Case[
  $block[e_List, _, _] := Riffle[rowToStrings[Flatten @ {#}]& /@ e, "\n"];
];

StringBlock::spacefail = "Spacer `` could not be achieved with combination of normal and superscript characters."

repChar[s_, w_Int] := Repeat[s, w];
repChar[s_, w_] := Scope[
  w2 = w; c = 0;
  While[Abs[w2 - Round[w2]] > 0.0001, w2 -= 3/4; c++];
  If[w2 < 0, ThrowMessage["spacefail", w]];
  Flatten[{
    repChar[s, Round[w2]],
    Style[SJoin @ repChar[s, c], "Midscript"]
  }]
];

rowToStrings = Case[
  $hspace[w_Int] := repChar[" ", w];
  $hspace[w_]        := % @ repChar[" ", w];
  {l___, $sbg[col_], Shortest[m___], $ebg[col_], r___} /; Count[{l}, _$sbg] === Count[{l}, _$ebg] :=
    {% @ {l}, $stylingFunction[SJoin @ % @ {m}, normStyle /@ {Background -> col}], % @ {r}};
  e_Str           := e;
  None               := "";
  Style[e_, args___] := $stylingFunction[% @ e, ToList[args]];
  list_List          := % /@ list;
];

processBlock = Case[

  $rawBlock[block_]                                   := block;

  Invisible[elem_]                                    := Apply[spacerBlock[#2, #3]&, % @ elem];

  (Row|RowBox)[args_List, opts___Rule]                         := % @ HBlock[args, Lookup[{opts}, Alignment, Top], None];
  (Row|RowBox)[args_List, delim:Except[_Rule], opts___Rule]    := % @ HBlock[args, Lookup[{opts}, Alignment, Top], delim];
  HBlock[args_, align_, delim_]                       := hstackBlocks[riffle[% /@ args, delim], align];

  TagBox[GridBox[rows_List, ___], "Column"]           := % @ Column[rows];
  Column[args_List, opts___Rule]                      := % @ Column[args, Lookup[{opts}, Alignment, Left], Lookup[{opts}, Spacings, 0]];
  Column[args_List, align:Except[_Rule]:Left, spacing:Except[_Rule]:0] := % @ VBlock[args, align, Spacer @ spacing];
  VBlock[args_, align_, delim_]                       := vstackBlocks[riffle[% /@ args, delim], align];

  gb_GridBox | TagBox[gb_GridBox, "Grid"]             := processGridBox[gb];
  Grid[rows_List, opts___Rule]                        := processGrid[rows, opts];

  Labeled[a_, l_]                                     := % @ StringColumn[{a, l}, ColumnAlignments -> Center, RowSpacings -> 1];

  ParenthesesForm[e_]                                 := delimBlock[{e}, "()", None];
  TupleForm[e___]                                     := delimBlock[{e}, "()", None];
  ListForm[e___]                                      := delimBlock[{e}, "[]", None];
  SetForm[e___]                                       := delimBlock[{e}, "{}", None];
  StyleDecorated[s_, TupleForm|ParenthesesForm][e___] := delimBlock[{e}, "()", s];
  StyleDecorated[s_, ListForm][e___]                  := delimBlock[{e}, "[]", s];
  StyleDecorated[s_, SetForm][e___]                   := delimBlock[{e}, "{}", s];

  SubscriptForm[a_, b_]                               := % @ Subscript[a, b];
  SubscriptForm[a_, b_, c__]                          := % @ Subscript[a, Row[{b, c}, ","]];
  (Subscript|SubscriptBox)[a_, b_]                    := % @ Row[{a, Style[b, "Subscript"]}, Alignment -> Bottom];
  (Superscript|SuperscriptBox)[a_, b_]                := % @ Row[{a, Style[b, "Superscript"]}, Alignment -> Top];

  Padded[e_, spec_]                                   := padBlock[% @ e, spec];

  StringForm[t_Str, args___, Alignment -> align_]     := stringFormBlock[t, {args}, align];
  StringForm[t_Str, args___]                          := stringFormBlock[t, {args}, Top];

  Undersegment[e_]                                    := vframeBlock[% @ e, {None, "SquareBottom"}, True];
  UnderlinedForm[e_]                                  := vframeBlock[% @ e, {None, "-"}, True];
  p_PixelForm                                         := % @ ToBoxes[p];
  r_FadedRationalForm                                 := % @ ToBoxes[r];

  FunctionTypeForm[a_, b_]                            := hstackBlocks[{% @ a, makeSingleBlock @ "->", % @ b}, Center];

  na_NestedArrayForm                                  := % @ nestedArrayRender @ na;
  (head_Symbol ? $styleFormHeadQ)[arg_]               := % @ Style[arg, StyleFormData @ head];

  f_FractionBox                                       := makeSingleBlock @ evalFracBox[f];

  str_Str /; SContainsQ[str, "\!\(\*"]           := % @ parseLinearSyntax @ str;
  str_Str                                             := makeSingleBlock @ str;
  Null                                                := $block[{""}, 0, 1];
  item_                                               := processCustomBlock @ item;

(*   LiftedForm[s_Str]                                := makeSingleBlock @ StringJoin[s, "\:0302"];
  MappedForm[s_Str]                                   := makeSingleBlock @ StringJoin[s, "\:0302"];
 *)
  (Style|StyleBox)[item_, style___]                   := InheritedBlock[
    {$styleStack},
    $styleStack = joinStyles[normStyle /@ {style}, $styleStack];
    % @ item
  ];
  Spacer[w_Int]                                       := % @ Spacer[{w, 1}];
  Spacer[{w_Int, h_Int}]                              := spacerBlock[w, h];
  Spacer[0] | Spacer[{0,0}] | None | Nothing          := None;

  Framed[e_, opts__Rule]                              := % @ StringFrame[e, opts];
  Framed[e_]                                          := makeFrame[% @ e, False];
  Framed[e_, RoundingRadius -> n_]                    := makeFrame[% @ e, n > 0];

  RawBoxes[b_]                                        := processBoxes @ b;
];

(**************************************************************************************************)

processCustomBlock[item_] :=
  If[HasBoxFormQ[item],
    processBoxes @ ToBoxes @ item,
    makeSingleBlock @ TextString @ item];

processBoxes[boxes_] :=
  processBlock @ RepAll[EvaluateTemplateBoxFull @ boxes, $boxFixups]

$boxFixups = SubscriptBox[a_Str, b_] /; SEndsQ[a, " "] :> SubscriptBox[STrim @ a, b];

evalFracBox = Case[
(*   FractionBox["1", "2"] := "½";
  FractionBox["1", "3"] := "⅓";
  FractionBox["2", "3"] := "⅔";
  FractionBox["1", "4"] := "¼";
  FractionBox["3", "4"] := "¾";
  FractionBox["1", "5"] := "⅕";
  FractionBox["2", "5"] := "⅖";
  FractionBox["3", "5"] := "⅗";
  FractionBox["4", "5"] := "⅘";
 *)
  FractionBox[i_Str, j_Str] := SJoin[i, "/", j];
]

(**************************************************************************************************)

trimBlock[block:$block[rows_, w_, h_]] := Scope[
  rows = rows //. {
    {l___, $hspace[s1_], $hspace[s2_]} :> {l, $hspace[s1 + s2]},
    {l___, s:" ".., r___$hspace} :> {l, $hspace[Len @ {s}], r}
  };
  spaceLens = rightSpaceLen /@ rows;
  trimCount = Min[spaceLens];
  If[trimCount == 0, Return @ block];
  $block[trimRowRight, w - trimCount, h]
];

rightSpaceLen = Case[
  {___, $hspace[s_]} := s;
  _                  := 0;
]

trimRowRight = Case[
  {l___, $hspace[s_]} := If[s - trimCount <= 0, {l}, {l, $hspace[s - trimCount]}];
]

(**************************************************************************************************)

stringFormBlock[template_, args_List, align_] :=
  processBlock @ Row[
    Riffle[SSplit[template, "``", All], args],
    Alignment -> align
  ];

(**************************************************************************************************)

parseLinearSyntax[str_] /; SFreeQ[str, "\!\(\*"] := str;

linearBalancedQ[str_] := StringCount[str, "\("] == StringCount[str, "\)"];

parseLinearSyntax[str_] := Row @ SCases[str, {
  "\!\(\*" ~~ inner:Shortest[___] ~~ "\)" /; linearBalancedQ[inner] :> parseInnerLinearSyntax[inner],
  fragment__ /; SFreeQ[fragment, "\!"] :> fragment
}];

parseInnerLinearSyntax[str_] := Scope[
  str2 = SRep[str,
    "\(" ~~ inner:Shortest[___] ~~ "\)" /; linearBalancedQ[inner] :> "\"" <> inner <> "\""
  ];
  ToExpression[str] /. {StyleBox -> Style, SubscriptBox -> Subscript, SuperscriptBox -> Superscript, RowBox -> Row}
];

(**************************************************************************************************)

StringBlock::badfs = "Setting of FrameStyle -> `` should be a color or None.";

normFrameStyle = Case[
  color_ ? ColorQ      := FontColor -> color;
  i_Int                := currentStyleSetting[FontColor, "Color" <> IntStr[i]];
  r:Rule[FontColor|Background, _] := r;
  other_               := (Message[StringBlock::badfs, other]; None);
  None                 := None;
]

normStyle = Case[
  color_ ? ColorQ                                      := FontColor -> color;
  i_Int                                                := %[FontColor -> i];
  FontColor -> i_Int                                   := currentStyleSetting[FontColor, "Color" <> IntStr[i]];
  Background -> i_Int                                  := currentStyleSetting[Background, "Background" <> IntStr[i]];
  Bold                                                 := FontWeight -> Bold;
  Italic                                               := FontSlant -> Italic;
  Plain                                                := Splice[{FontWeight -> Plain, FontSlant -> Plain}];
  Rule[FontVariations, vars_List]                      := Splice @ Map[normFontVar, vars];
  Underlined                                           := Underlined;
  Struckthrough                                        := "Subscript";
  r:Rule[FontWeight|FontSlant|FontColor|Background, _] := r;
  r:RuleDelayed[FontColor|Background, _]               := r;
  s:"Subscript"|"Superscript"                          := s;
  _                                                    := Nothing;
];

normFontVar = Case[
  "Underline" -> True     := Underlined;
  "StrikeThrough" -> True := "Subscript";
  _                       := Nothing
]

joinStyles[s1_, s2_] := Dedup[Join[s1, s2], F[#1, "ZZ"] === F[#2, "YY"]&];

applyStyleStack[e_] := applyStyle[Sequence @@ $styleStack] @ e;

applyStyle[] := Id;
applyStyle[None] := Id;
applyStyle[s_List] := Apply[applyStyle, s];

as_applyStyle[list_List] := Map[as, list];
_applyStyle[s_$hspace] := s;
_applyStyle[" "] := " ";
_applyStyle[None] := None;
applyStyle[s___][e_Str] := Style[e, s];

(**************************************************************************************************)

$gridBoxDefaults = <|GridBoxAlignment -> {}, GridBoxSpacings -> {}, GridBoxFrame -> None, GridBoxBackground -> None, GridBoxDividers -> None|>;

processGridBox[GridBox[rows_List, opts___Rule]] := Scope[

  UnpackAnonymousOptions[{opts}, $gridBoxDefaults, gridBoxAlignment, gridBoxSpacings, gridBoxFrame, gridBoxBackground, gridBoxDividers];
  {h, w} = Dims[rows, 2];

  background = If[MatchQ[gridBoxBackground, {"Columns" -> {{_}}}], Part[gridBoxBackground, 1, 2, 1, 1], None];
  frame = MatchQ[gridBoxFrame, {"ColumnsIndexed" -> {{{1, -1}, {1, -1}} -> True}}];
  dividers = Switch[gridBoxDividers,
    {"Columns" -> {{True}}, "Rows" -> {{True}}}, frame = True; Center,
    {"Columns" -> {False, {True}, False}, "Rows" -> {False, {True}, False}}, Center,
    _, None
  ];

  rowAlignments = Lookup[{opts},    RowAlignments, ParseCyclicSpec[h] @ Lookup[gridBoxAlignment, "Rows", Top]];
  colAlignments = Lookup[{opts}, ColumnAlignments, ParseCyclicSpec[w] @ Lookup[gridBoxAlignment, "Columns", Left]];
  rowSpacings = Lookup[{opts},        RowSpacings, ParseCyclicSpec[h] @ RepAll[Lookup[gridBoxSpacings, "Rows", 1.0], {Auto -> 1, None -> 0}]];
  colSpacings = Lookup[{opts},     ColumnSpacings, ParseCyclicSpec[w] @ RepAll[Lookup[gridBoxSpacings, "Columns", 0.8], {Auto -> 0.8, None -> 0}]];

  rowSpacings = Round[rowSpacings - 1.0];
  colSpacings = Round[colSpacings];

  gstackBlocks[
    MatrixMap[processBlock, rows],
    ColumnAlignments -> colAlignments, RowAlignments -> rowAlignments,
    ColumnSpacings -> colSpacings, RowSpacings -> rowSpacings,
    Frame -> frame, Background -> background, Dividers -> dividers
  ]
]

(**************************************************************************************************)

delimBlock[e_List, frame_, style_] := hframeBlock[processHoriz @ e, frame, style, True, None];

processHoriz[{}] := spacerBlock[1, 1];
processHoriz[{a_}] := processBlock @ a;
processHoriz[{a__}] := processBlock @ Row[{a}, ", ", Alignment -> Center];

processVert[{}] := spacerBlock[1, 1];
processVert[{a_}] := processBlock @ a;
processVert[{a__}] := processBlock @ Column[{a}];

Options[processGrid] = JoinOptions[StringMatrix,
  Alignment -> {Left, Top},
  Spacings -> {0, 0}
];

processGrid[rows_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[alignment, spacings];
  {calign, ralign} = alignment * {1, 1};
  {cspace, rspace} = spacings * {1, 1};
  gstackBlocks[
    MatrixMap[processBlock, rows],
    ColumnAlignments -> calign, RowAlignments -> ralign,
    ColumnSpacings -> cspace, RowSpacings -> rspace,
    FilterOptions @ opts
  ]
]

spacerBlock[w_, h_] := $block[Repeat[$hspace[w], h], w, h];

riffle[{}, _] := {};
riffle[args_, Spacer[0] | Spacer[{0,0}] | None | Nothing] := args;
riffle[args_, e_] := ScalarRiffle[args, processBlock @ e];

makeSingleBlock[s_Str] /; SContainsQ[s, "\n"] := makeBlock @ SSplit[s, "\n"];
makeSingleBlock[elem_] := Scope[frags = applyStyleStack @ elem;
  $block[List @ frags, atomWidth @ frags, 1]
];

makeBlock[rows_List, align_:Left] := Scope[
  widths = atomWidth /@ rows;
  maxWidth = Max @ widths;
  paddedRows = MapThread[hpadAtom[maxWidth, align], {rows, widths}];
  $block[paddedRows, maxWidth, Len @ rows]
];

(**************************************************************************************************)

hpadAtom[tw_, halign_][atom_, w_] := Scope[
  d = tw - w;
  If[d <= 0, atom, Switch[halign,
    Left,   padAtomLeftRight[{0, d}] @ atom,
    Right,  padAtomLeftRight[{d, 0}] @ atom,
    Center, padAtomLeftRight[FloorCeiling[d/2]] @ atom,
    _,      ThrowMessage["badalign", halign, {Left, Center, Right}]
  ]]
];

padAtomLeftRight[{0, 0}] := Id;
padAtomLeftRight[{0, r_}][row_] := Flatten @ {row, $hspace @ r};
padAtomLeftRight[{l_, 0}][row_] := Flatten @ {$hspace @ l, row};
padAtomLeftRight[{l_, r_}][row_] := Flatten @ {$hspace @ l, row, $hspace @ r};

atomWidth = Case[
  Style[e_, ___, "Superscript" | "Subscript", ___] := (% @ e) * 3 / 4;
  Style[e_, ___]    := % @ e;
  e_List            := Total[% /@ e];
  s_Str          := SLen @ s;
  $hspace[n_]       := n;
];

(**************************************************************************************************)

padBlock[block:$block[cols_List, w_, h_], pspec_] := Scope[
  {{l, r}, {b, t}} = Round @ StandardizePadding @ pspec;
  If[l == r == b == t == 0, Return @ block];
  w2 = w + l + r; h2 = h + b + t;
  $block[
    padBottomTop[{b, t}, w2] @ padLeftRight[{l, r}] @ cols,
    w2, h2
  ]
];

padLeftRight[{0, 0}] := Id;
padLeftRight[spec_][rows_] := Map[padAtomLeftRight[spec], rows];

(**************************************************************************************************)

vstackBlocks[{b_$block}, _] := b;

vstackBlocks[rows_List, halign_] := Scope[
  widths = Col2[rows];
  heights = Col3[rows];
  maxWidth = Max @ widths;
  paddedRows = Map[hpadBlock[maxWidth, halign], rows];
  $block[Catenate @ Col1[paddedRows], maxWidth, Total[heights]]
];

hpadBlock[tw_, halign_][$block[rows_, w_, h_]] :=
  $block[hpadAtom[tw, halign][#, w]& /@ rows, tw, h];

(**************************************************************************************************)

hstackBlocks[{b_$block}, _] := b;

hstackBlocks[cols_List, valign_] := Scope[
  widths = Col2[cols];
  heights = Col3[cols];
  maxHeight = Max @ heights;
  paddedCols = Map[vpadBlock[maxHeight, valign], cols];
  $block[MapThread[List, Col1[paddedCols]], Total @ widths, maxHeight]
];

vpadBlock[th_, valign_][$block[rows_, w_, h_]] := Scope[
  d = th - h;
  extendedRows = If[d <= 0, rows, Switch[valign,
    Top,        padBottomTop[{d, 0}, w] @ rows,
    Bottom,     padBottomTop[{0, d}, w] @ rows,
    Center,     padBottomTop[FloorCeiling[d/2], w] @ rows,
    _Int,   o = Clip[valign - 1, {0, d}]; padBottomTop[{d - o, o}, w] @ rows,
    _,      ThrowMessage["badalign", valign, {Top, Center, Bottom}]
  ]];
  $block[extendedRows, w, th]
];

hspaceList[h_, w_] := Repeat[$hspace[w], h];

padBottomTop[{0, 0}, _] := Id;
padBottomTop[{b_, t_}, w_][rows_] := Join[hspaceList[t, w], rows, hspaceList[b, w]];

(**************************************************************************************************)

blockPadding[block_, None | 0] := block;

blockPadding[block:$block[elems_, w_, h_], framePadding_] := Scope[
  {{l, r}, {b, t}} = Round @ StandardizePadding @ framePadding;
  If[l == r == b == t == 0, Return @ block];
  If[t > 0, PreTo[elems, $hspace[w]]; h = h + t];
  If[b > 0, AppTo[elems, $hspace[w]]; h = h + b];
  If[l > 0, space = hspaceList[h, l]; elems = MapThread[Flatten @ {#1, #2}&, {space, elems}]];
  If[r > 0, space = hspaceList[h, r]; elems = MapThread[Flatten @ {#1, #2}&, {elems, space}]];
  $block[elems, w + l + r, h]
];

(**************************************************************************************************)

$extFrameP = _Str | None | Dashed;

StringBlock::badgridframe = "Frame -> `` is not a valid setting."
Options[gstackBlocks] = Options[StringMatrix];

(* TODO: fix column spacings producing spurious central frames in Dividers -> Center *)
$hframeSpec = {$extFrameP, $extFrameP} | _Str | _StyleDecorated[{$extFrameP, $extFrameP} | _Str];
gstackBlocks[rows_List, OptionsPattern[]] := Scope[
  UnpackOptions[
    rowAlignments, rowSpacings,
    columnAlignments, columnSpacings,
    dividers, frame, framePadding, frameStyle,
    rowFrames, rowFrameStyle, rowFramePadding,
    spanningFrame, background
  ];
  frameStyle //= normFrameStyle;
  rowFrameStyle //= normFrameStyle;
  {numRows, numCols} = Dims[rows, 2];
  rows = riffleCols[columnSpacings] @ riffleRows[rowSpacings] @ rows;
  widths = Part[rows, All, All, 2]; heights = Part[rows, All, All, 3]; maxWidths = Max /@ Transpose[widths]; maxHeights = Max /@ heights;
  If[!ListQ[rowAlignments], rowAlignments = Table[rowAlignments, numRows]];
  If[!ListQ[columnAlignments], columnAlignments = Table[columnAlignments, numCols]];
  If[Total[rowSpacings] != 0, rowAlignments = Riffle[rowAlignments, Top]];
  If[Total[columnSpacings] != 0, columnAlignments = Riffle[columnAlignments, Left]];
  items = MapIndexed[gpadBlock, rows, {2}];
  If[StrQ[rowFrames],
    {lext, rext} = Lookup[$hextTableNames, rowFrames];
    jump = If[Total[rowSpacings] === 0, All, 1;;-1;;2];
    If[IntQ[rowFramePadding] && rowFramePadding > 0,
      items = MapAt[padBlock[#, Left -> rowFramePadding]&, items, {All, 1}];
      items = MapAt[padBlock[#, Right -> rowFramePadding]&, items, {All, -1}];
    ];
    If[lext =!= None, items = MapAt[hframeBlock[#, {lext, None}, rowFrameStyle, spanningFrame, None]&, items, {jump, 1}]];
    If[rext =!= None, items = MapAt[hframeBlock[#, {None, rext}, rowFrameStyle, spanningFrame, None]&, items, {jump, -1}]];
    If[Total[rowSpacings] =!= 0,
      If[lext =!= None, items = MapAt[hframeBlock[#, {" ", None}, None, True, None]&, items, {2;;-1;;2, 1}]];
      If[rext =!= None, items = MapAt[hframeBlock[#, {None, " "}, None, True, None]&, items, {2;;-1;;2, -1}]];
    ];
    widths = Part[items, All, All, 2]; heights = Part[items, All, All, 3]; maxWidths = Max /@ Transpose[widths]; maxHeights = Max /@ heights;
  ];
  totalWidth = Total @ maxWidths; totalHeight = Total @ maxHeights;
  If[dividers === Center,
    items = addDivs[items, maxWidths, maxHeights];
    totalWidth += Len[maxWidths] - 1;
    totalHeight += Len[maxHeights] - 1;
  ];
  grid = Catenate @ Map[Flatten /@ Transpose[Col1[#]]&, items];
  block = $block[grid, totalWidth, totalHeight];
  framePadding //= StandardizePadding;
  block = blockPadding[block, framePadding];
  Switch[frame,
    Part[maxWidths,   1] += Part[framePadding, 1, 1];
    Part[maxWidths,  -1] += Part[framePadding, 1, 2];
    Part[maxHeights, -1] += Part[framePadding, 2, 1];
    Part[maxHeights, -1] += Part[framePadding, 2, 2];
    totalWidth = P2[block]; totalHeight = P3[block];
    True | "Round" | "Square",
      sfn = applyStyle @ frameStyle;
      If[dividers === Center,
        we = makeNotchedSides["│", {"├", "┤"}, totalHeight, maxHeights];
        sn = makeNotchedSides["─", {"┴", "┬"}, totalWidth, maxWidths];
      ,
        we = Repeat["│", {2, totalHeight}];
        sn = Repeat[repChar["─", totalWidth], 2];
      ];
      $block[
        apply8patch[F @ block, MatrixMap[sfn, we], sfn /@ sn, sfn /@ If[frame === "Round", $roundCompass, $squareCompass]],
        totalWidth + 2, totalHeight + 2
      ],
    {$extFrameP, $extFrameP} | _Str,
      hframeBlock[block, frame, frameStyle, spanningFrame, background],
    False | None,
      block,
    _,
      ThrowMessage["badgridframe", frame];
  ]
,
  gpadBlock[elem_, {r_, c_}] := vpadBlock[Part[maxHeights, r], Part[rowAlignments, r]] @ hpadBlock[Part[maxWidths, c], Part[columnAlignments, c]] @ elem
];

StringBlock::fracwid = "Fractional width `` occurred in unsupported context."
makeNotchedSides[char_, {notch1_, notch2_}, n_, sizes_] := Scope[
  If[!IntQ[n], ThrowMessage["fracwid", n]];
  notches = Accumulate[Most[sizes] + 1];
  side = Repeat[char, n];
  List[
    RepPart[side, Transpose[List @ notches] -> notch1],
    RepPart[side, Transpose[List @ notches] -> notch2]
  ]
];

riffleCols[0] := Id;
riffleCols[n_][rows_] := riffle[#, Spacer[n]]& /@ rows;
riffleCols[ns_List][rows_] := If[Total[ns] == 0, rows, MapThread[riffle[#1, $rawBlock @ spacerBlock[#2, 1]]&, {rows, PadRight[ns, Len @ rows]}]];

riffleRows[0] = Id;
riffleRows[n_][rows_] := Transpose[riffle[#, Spacer[{1, n}]]& /@ Transpose[rows]];
riffleRows[ns_List][rows_] := If[Total[ns] == 0, rows, Transpose @ MapThread[riffle[#1, Spacer[{1, #2}]]&, {Transpose @ rows, PadRight[ns, Len @ Transpose @ rows]}]];

addDivs[items_, ws_, hs_] := Scope[
  Table[
    Switch[IntQ /@ {r, c},
      {True, True},  Part[items, r, c],
      {True, False}, makeVBar[Part[hs, r]],
      {False, True}, makeHBar[Part[ws, c]],
      {False, False}, $block[{"┼"}, 1, 1]
    ],
    {r, 1, Len[hs], 1/2},
    {c, 1, Len[ws], 1/2}
  ]
];

makeVBar[h_] := $block[Repeat["│", h], 1, h];
makeHBar[w_] := $block[List @ repChar["─", w], w, 1];

(**************************************************************************************************)

$roundCompass = {"╭", "╮", "╰", "╯"};
$squareCompass = {"┌", "┐", "└", "┘"};

makeFrame[$block[rows_, w_, h_], r_] := Scope[
  we = Repeat["│", {2, h}];
  sn = Repeat[repChar["─", w], 2];
  rows2 = apply8patch[rows, we, sn, If[r, $roundCompass, $squareCompass]];
  $block[rows2, w + 2, h + 2]
]

apply8patch[rows_, {w_, e_}, {s_, n_}, {nw_, ne_, sw_, se_}] :=
  Join[
    List @ Flatten @ {nw, n, ne},
    MapThread[Flatten @ {#1, #2, #3}&, {w, rows, e}],
    List @ Flatten @ {sw, s, se}
  ];

(**************************************************************************************************)

hextTable = Case[
  None                := None;
  "|"                 := ext1["│"];
  " "                 := ext1[" "];
  Dashed              := ext1["┊"];
  "RoundLeft"         := ext3["(", "╭", "│", "╰"];
  "RoundRight"        := ext3[")", "╮", "│", "╯"];
  "MidRoundLeft"      := ext5["(", "╭", "│", "┤", "│", "╰"];
  "MidRoundRight"     := ext5[")", "╮", "│", "├", "│", "╯"];
  "SquareLeft"        := ext3["[", "┌", "│", "└"];
  "SquareRight"       := ext3["]", "┐", "│", "┘"];
  "MidSquareLeft"     := ext5["[", "┌", "│", "┤", "│", "└"];
  "MidSquareRight"    := ext5["]", "┐", "│", "├", "│", "┘"];
  "LeftFloor"         := ext3["⌊", "⌊", "│", "│"];
  "RightFloor"        := ext3["⌋", "⌋", "│", "│"];
  "LeftCeiling"       := ext3["⌈", "⌈", "│", "│"];
  "RightCeiling"      := ext3["⌉", "⌉", "│", "│"];
  "DoubleSquareLeft"  := ext3["⟦", "╓", "║", "╙"];
  "DoubleSquareRight" := ext3["⟧", "╖", "║", "╜"];
  "["                 := ext3["[", "⎡", "⎢", "⎣"];
  "]"                 := ext3["]", "⎤", "⎥", "⎦"];
  "{"                 := ext5["}", "⎧", "⎸", "⎨", "⎸", "⎩"];
  "}"                 := ext5["}", "⎫", "⎹", "⎬", "⎹", "⎭"];
  "("                 := ext3["(", "⎛", "⎜", "⎝"];
  ")"                 := ext3[")", "⎞", "⎟", "⎠"];
];

$hextTableNames = Assoc[
  "Round"        -> {"RoundLeft", "RoundRight"},
  "MidRound"     -> {"MidRoundLeft", "MidRoundRight"},
  "Square"       -> {"SquareLeft", "SquareRight"},
  "MidSquare"    -> {"MidSquareLeft", "MidSquareRight"},
  "DoubleSquare" -> {"DoubleSquareLeft", "DoubleSquareRight"},
  "Floor"        -> {"LeftFloor", "RightFloor"},
  "Ceiling"      -> {"LeftCeiling", "RightCeiling"},
  "["            -> {"[", None},
  "{"            -> {"{", None},
  "("            -> {"(", None},
  "[]"           -> {"[", "]"},
  "{}"           -> {"{", "}"},
  "()"           -> {"(", ")"}
];


(**************************************************************************************************)

StringBlock::badframe = "`` is not a valid spec for Frame, which should be one of ``."

hframeBlock[block_, name_Str, rest___] :=
  hframeBlock[block, Lookup[$hextTableNames, name, ThrowMessage["badframe", name, Keys @ $hextTableNames]], rest];

hframeBlock[block_, None | {None, None}, ___] := block;

hframeBlock[$block[grid_, w_, h_], {l_, r_}, style_, spanning_, background_] := Scope[
  $n = h; sfn = applyStyle @ style;
  {lext, rext} = MapThread[Map[sfn, extend[hextTable[#1], spanning, #2]]&, {{l, r}, {False, True}}];
  grid2 = MapThread[DeleteNone @ Flatten @ {#1, #2, #3}&, {lext, grid, rext}];
  If[background =!= None, grid2 = ToList[$sbg[background], #, $ebg[background]]& /@ grid2];
  $block[grid2, w + If[l === None, 0, 1] + If[r === None, 0, 1], h]
];

(**************************************************************************************************)

vextTable = Case[
  None                 := None;
  "-"                  := ext1["─"];
  Dashed               := ext1["┈"];
  "RoundTop"           := ext3["⏜", "╭", "─", "╮"];
  "RoundBottom"        := ext3["⏝", "╰", "─", "╯"];
  "MidRoundTop"        := ext3["⏜", "╭", "─", "┴", "─", "╮"];
  "MidRoundBottom"     := ext3["⏝", "╰", "─", "┬", "─", "╯"];

  "SquareTop"          := ext3["⎴", "┌", "─", "┐"];
  "SquareBottom"       := ext3["⎵", "└", "─", "┘"];
  "MidSquareTop"       := ext3["⎴", "┌", "─", "┐"];
  "MidSquareBottom"    := ext3["⎵", "└", "─", "┘"];

  "DoubleSquareTop"    := ext3["═", "╒", "═", "╕"];
  "DoubleSquareBottom" := ext3["═", "╘", "═", "╛"];
];

$vextTableNames = Assoc[
  "Round"        -> {"RoundTop", "RoundBottom"},
  "MidRound"     -> {"MidRoundTop", "MidRoundBottom"},
  "Square"       -> {"SquareTop", "SquareBottom"},
  "MidSquare"    -> {"MidSquareTop", "MidSquareBottom"},
  "DoubleSquare" -> {"DoubleSquareTop", "DoubleSquareBottom"}
];

vframeBlock[arg1_, arg2_] := vframeBlock[arg1, arg2, None, True];

vframeBlock[block_, name_Str, style_, spanning_] :=
  vframeBlock[block, Lookup[$vextTableNames, name, ThrowMessage["badframe", name, Keys @ $vextTableNames]], style, spanning];

vframeBlock[block_, None | {None, None}, _, _] := block;

vframeBlock[$block[grid_, w_, h_], {t_, b_}, style_, spanning_] := Scope[
  $n = w; sfn = StyleOperator @ style;
  {bext, text} = MapThread[Map[sfn, extend[vextTable[#1], spanning, #2]]&, {{b, t}, {False, True}}];
  grid2 = grid;
  If[text =!= None, PreTo[grid2, text]];
  If[bext =!= None, AppTo[grid2, bext]];
  $block[grid2, w, h + If[b === None, 0, 1] + If[t === None, 0, 1]]
];

(**************************************************************************************************)

extend[None, _, _] := Repeat[None, $n];
extend[s_, False, side_] := RepPart[Repeat[" ", $n], If[side, -1, 1] -> F[s]];
extend[s_, True, side_] := extendSpanning[s];

extendSpanning = Case[
  ext1[c_]                 := Repeat[c, $n];
  ext3[s_, l_, m_, r_]     := If[$n === 1, {s}, Flatten @ {l, Repeat[m, $n - 2], r}];
  ext5[s_, l_, a_, m_, b_, r_] := Scope[
    If[$n === 1, Return @ {s}];
    n2 = ($n - 3) / 2;
    Flatten @ {l, Repeat[a, Floor @ n2], m, Repeat[a, Ceiling @ n2], r}
  ]
];