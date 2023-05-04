PublicForm[StringMatrix]

PublicOption[RowAlignment, ColumnAlignment, FramePadding, RowFrames, RowFrameStyle, SpanningFrame]

Options[StringMatrix] = {
  RowAlignment -> Top,
  ColumnAlignment -> Left,
  RowSpacings -> 0,
  ColumnSpacings -> 1,
  Dividers -> None,
  Frame -> None,
  FramePadding -> None,
  FrameStyle -> None,
  RowFrames -> None,
  RowFrameStyle -> None,
  SpanningFrame -> False
};

DefineStandardTraditionalForm[
  grid:StringMatrix[_List, ___Rule] :> ToBoxes @ StringBlockForm @ grid
]

(**************************************************************************************************)

PublicForm[StringRow]

Options[StringRow] = {
  RowAlignment -> Top,
  ColumnSpacings -> 0,
  Frame -> None,
  FramePadding -> None,
  FrameStyle -> None,
  SpanningFrame -> False
}

DefineStandardTraditionalForm[
  row:StringRow[_List, ___Rule] :> ToBoxes @ StringBlockForm @ row
];

Options[rowBlock] = Options[StringRow];

rowBlock[items_, OptionsPattern[]] := Scope[
  UnpackOptions[rowAlignment, columnSpacings, frame, framePadding, frameStyle, spanningFrame];
  frameStyle //= normFrameStyle;
  If[columnSpacings =!= 0, items = riffle[items, Spacer[columnSpacings]]];
  block = hstackBlocks[items, rowAlignment];
  block = blockPadding[block, framePadding];
  hframeBlock[block, frame, frameStyle, spanningFrame]
];

(**************************************************************************************************)

PublicForm[StringColumn]

Options[StringColumn] = {
  ColumnAlignment -> Left,
  RowSpacings -> 0,
  Frame -> None,
  FramePadding -> None,
  FrameStyle -> None,
  SpanningFrame -> False
}

DefineStandardTraditionalForm[
  col:StringColumn[_List, ___Rule] :> ToBoxes @ StringBlockForm @ col
];

Options[columnBlock] = Options[StringColumn];

columnBlock[items_, OptionsPattern[]] := Scope[
  UnpackOptions[columnAlignment, rowSpacings, frame, framePadding, frameStyle, spanningFrame];
  frameStyle //= normFrameStyle;
  If[rowSpacings =!= 0, items = riffle[items, Spacer[{1, rowSpacings}]]];
  block = vstackBlocks[items, columnAlignment];
  block = blockPadding[block, framePadding];
  hframeBlock[block, frame, frameStyle, spanningFrame]
];

(**************************************************************************************************)

PublicForm[StringBlockForm]

Options[StringBlockForm] = {
  StylingFunction -> "Linear"
}

declareBoxFormatting[
  sb:StringBlockForm[_, ___Rule] :> makeStringBlockFormBoxes[sb]
]

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
      TextData @ ReplaceRepeated[Flatten @ fragments, {l___, ls_String, rs_String, r___} :> {l, ls <> rs, r}],
      "PreformattedCode"
    ],
    StringJoin @ fragments
  ]
];

(* these have special code in toCodeMarkdown to pick them up *)
cellStyling[e_, {l___, "Subscript", r___}] := Cell[BoxData @ SubscriptBox["", cellStyling[e, {l, r}]]];
cellStyling[e_, {l___, "Midscript", r___}] := StyleBox[cellStyling[e, {l, r}], FontSize -> 9.2];
cellStyling[e_, {l___, "Superscript", r___}] := Cell[BoxData @ SuperscriptBox["", cellStyling[e, {l, r}]]]
cellStyling[e_, {s__}] := StyleBox[e, s];
cellStyling[e_, {}] := e;

inputStyling[e_, {l___, "Subscript", r___}] := {"_{", inputStyling[e, {l, r}], "}"};
inputStyling[e_, {l___, "Midscript", r___}] := {"_{", inputStyling[e, {l, r}], "}"};
inputStyling[e_, {l___, "Superscript", r___}] := {"^{", inputStyling[e, {l, r}], "}"};
inputStyling[e_ ? SingleLetterQ, {FontColor -> color_}] := {$colorShortcodeMappingInverse[color], ":", e};
inputStyling[e_, {FontColor -> color_}] := {$colorShortcodeMappingInverse[color], "{", e, "}"};
inputStyling[e_, s_] := e;

htmlStyling[e_, {l___, "Subscript", r___}] := {"<sub>", htmlStyling[e, {l, r}], "</sub>"};
htmlStyling[e_, {l___, "Midscript", r___}] := {"<sub>", htmlStyling[e, {l, r}], "</sub>"};
htmlStyling[e_, {l___, "Superscript", r___}] := {"<sup>", htmlStyling[e, {l, r}], "</sup>"};
htmlStyling[e_, style_] := htmlStyledString[e, style];

linearStyling[e_, {l___, "Subscript", r___}] := linearStyling[Subscript["", e], {l, r}];
linearStyling[e_, {l___, "Midscript", r___}] := linearStyling[Subscript["", e], {l, r}];
linearStyling[e_, {l___, "Superscript", r___}] := linearStyling[Superscript["", e], {l, r}];
linearStyling[e_String, {}] := e;
linearStyling[e_, {}] := ToString[e, StandardForm];
linearStyling[e_, {s__}] := ToString[Style[e, s], StandardForm];

StringBlock::badalign = "Bad alignment ``: should be one of ``."

blockToStrings = Case[
  $block[e_List, _, _] := Riffle[rowToStrings /@ e, "\n"];
];

repChar[s_, w_Integer] := ConstantArray[s, w];
repChar[s_, w_] := Flatten[{
  repChar[s, Floor[w]],
  Style[#, "Midscript"]& /@ repChar[s, Round[FractionalPart[w] * 4/3]]
}];

rowToStrings = Case[
  $hspace[w_Integer] := repChar[" ", w];
  $hspace[w_]        := % @ repChar[" ", w];
  e_String           := e;
  None               := "";
  Style[e_, args___] := $stylingFunction[% @ e, ToList[args]];
  list_List          := % /@ list;
];

processBlock = Case[
  Row[args_List, opts___Rule]                         := % @ HBlock[args, Lookup[{opts}, Alignment, Top], None];
  Row[args_List, delim:Except[_Rule], opts___Rule]    := % @ HBlock[args, Lookup[{opts}, Alignment, Top], delim];
  HBlock[args_, align_, delim_]                       := hstackBlocks[riffle[% /@ args, delim], align];

  Column[args_List, opts___Rule]                      := % @ Column[args, Lookup[{opts}, Alignment, Left], Lookup[{opts}, Spacings, 0]];
  Column[args_List, align:Except[_Rule]:Left, spacing:Except[_Rule]:0] := % @ VBlock[args, align, Spacer @ spacing];
  VBlock[args_, align_, delim_]                       := vstackBlocks[riffle[% /@ args, delim], align];

  Grid[rows_List, opts___Rule]                        := processGrid[rows, opts];

  StringMatrix[rows_List, opts___Rule]                := gstackBlocks[MatrixMap[%, rows], opts];

  StringColumn[items_, opts___Rule]                   := columnBlock[% /@ items, opts];
  StringRow[items_, opts___Rule]                      := rowBlock[% /@ items, opts];

  VerticalModifierForm[TupleForm[e___]]               := hframeBlock[processVert @ e, "()"];
  VerticalModifierForm[ListForm[e___]]                := hframeBlock[processVert @ e, "[]"];
  VerticalModifierForm[SetForm[e___]]                 := hframeBlock[processVert @ e, "{}"];

  Labeled[a_, l_]                                     := columnBlock[% /@ {a, l}, ColumnAlignment -> Center, RowSpacings -> 1];

  ParenthesesForm[e_]                                 := hframeBlock[% @ e, "()"];
  TupleForm[e___]                                     := hframeBlock[processHoriz @ e, "()"];
  ListForm[e___]                                      := hframeBlock[processHoriz @ e, "[]"];
  SetForm[e___]                                       := hframeBlock[processHoriz @ e, "{}"];
  Subscript[a_, b_]                                   := % @ Row[{a, Style[b, "Subscript"]}, Alignment -> Bottom];
  Superscript[a_, b_]                                 := % @ Row[{a, Style[b, "Superscript"]}, Alignment -> Top];

  StyleDecorated[s_, TupleForm][e___]                 := hframeBlock[processHoriz @ e, "()", s];
  StyleDecorated[s_, ListForm][e___]                  := hframeBlock[processHoriz @ e, "[]", s];
  StyleDecorated[s_, SetForm][e___]                   := hframeBlock[processHoriz @ e, "{}", s];

  Undersegment[e_]                                    := vframeBlock[% @ e, {None, "SquareBottom"}, True];
  UnderlinedForm[e_]                                  := vframeBlock[% @ e, {None, "-"}, True];

  na_NestedArrayForm                                  := % @ nestedArrayRender @ na;
  head_Symbol[arg_] /; StyleFormHeadQ[head]           := % @ Style[arg, StyleFormData @ head];

  str_String /; StringContainsQ[str, "\!\(\*"]        := % @ parseLinearSyntax @ str;
  str_String                                          := makeSingleBlock @ str;
  item_                                               := makeSingleBlock @ TextString @ item;

  Style[item_, style___]                              := Internal`InheritedBlock[
    {$styleStack},
    $styleStack = joinStyles[normStyle /@ {style}, $styleStack];
    % @ item
  ];
  Spacer[w_Integer]                                   := % @ Spacer[{w, 1}];
  Spacer[{w_Integer, h_Integer}]                      := spacerBlock[w, h];
  Spacer[0] | Spacer[{0,0}] | None | Nothing          := None;
  Framed[e_]                                          := makeFrame[% @ e, False];
  Framed[e_, RoundingRadius -> n_]                    := makeFrame[% @ e, n > 0];
];

(**************************************************************************************************)

parseLinearSyntax[str_] /; StringFreeQ[str, "\!\(\*"] := str;

linearBalancedQ[str_] := StringCount[str, "\("] == StringCount[str, "\)"];

parseLinearSyntax[str_] := Row @ StringCases[str, {
  "\!\(\*" ~~ inner:Shortest[___] ~~ "\)" /; linearBalancedQ[inner] :> parseInnerLinearSyntax[inner],
  fragment__ /; StringFreeQ[fragment, "\!"] :> fragment
}];

parseInnerLinearSyntax[str_] := Scope[
  str2 = StringReplace[str,
    "\(" ~~ inner:Shortest[___] ~~ "\)" /; linearBalancedQ[inner] :> "\"" <> inner <> "\""
  ];
  ToExpression[str] /. {StyleBox -> Style, SubscriptBox -> Subscript, SuperscriptBox -> Superscript, RowBox -> Row}
];

(**************************************************************************************************)

StringBlock::badfs = "Setting of FrameStyle -> `` should be a color or None.";

normFrameStyle = Case[
  color_ ? ColorQ      := FontColor -> color;
  r:Rule[FontColor, _] := r;
  other_               := (Message[StringBlock::badfs, other]; None);
  None                 := None;
]

normStyle = Case[
  color_ ? ColorQ := FontColor -> color;
  Bold            := FontWeight -> Bold;
  Italic          := FontSlant -> Italic;
  Plain           := Splice[{FontWeight -> Plain, FontSlant -> Plain}];
  r:Rule[FontWeight|FontSlant|FontColor, _] := r;
  s:"Subscript"|"Superscript" := s;
  _               := Nothing;
];

joinStyles[s1_, s2_] := DeleteDuplicates[Join[s1, s2], First[#1, "ZZ"] === First[#2, "YY"]&];

applyStyleStack[e_] := applyStyle[Sequence @@ $styleStack] @ e;

applyStyle[] := Identity;
applyStyle[None] := Identity;
applyStyle[s_List] := Apply[applyStyle, s];

as_applyStyle[list_List] := Map[as, list];
_applyStyle[s_$hspace] := s;
_applyStyle[" "] := " ";
_applyStyle[None] := None;
applyStyle[s___][e_String] := Style[e, s];

(**************************************************************************************************)

processHoriz[] := spacerBlock[1, 1];
processHoriz[a_] := processBlock @ a;
processHoriz[a__] := processBlock @ Row[{a}, ","];

processVert[] := spacerBlock[1, 1];
processVert[a_] := processBlock @ a;
processVert[a__] := processBlock @ Column[{a}];

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
    ColumnAlignment -> calign, RowAlignment -> ralign,
    ColumnSpacings -> cspace, RowSpacings -> rspace,
    FilterOptions @ opts
  ]
]

spacerBlock[w_, h_] := $block[ConstantArray[$hspace[w], h], w, h];

riffle[{}, _] := {};
riffle[args_, Spacer[0] | Spacer[{0,0}] | None | Nothing] := args;
riffle[args_, e_] := Most @ Catenate @ Transpose @ {args, ConstantArray[processBlock @ e, Length @ args]};

makeSingleBlock[s_String] /; StringContainsQ[s, "\n"] := makeBlock @ StringSplit[s, "\n"];
makeSingleBlock[elem_] := Scope[frags = applyStyleStack @ elem;
  $block[List @ frags, atomWidth @ frags, 1]
];

makeBlock[rows_List, align_:Left] := Scope[
  widths = atomWidth /@ rows;
  maxWidth = Max @ widths;
  paddedRows = MapThread[hpadAtom[maxWidth, align], {rows, widths}];
  $block[paddedRows, maxWidth, Length @ rows]
];

(**************************************************************************************************)

hpadAtom[tw_, halign_][atom_, w_] := Scope[
  d = tw - w;
  If[d <= 0, atom, Switch[halign,
    Left,  Flatten @ {atom, $hspace @ d},
    Right, Flatten @ {$hspace @ d, atom},
    Center, Flatten @ {$hspace @ Floor[d/2], atom, $hspace @ Ceiling[d / 2]},
    _,      ThrowMessage["badalign", halign, {Left, Center, Right}]
  ]]
];

atomWidth = Case[
  Style[e_, ___, "Superscript" | "Subscript", ___] := (% @ e) * 3 / 4;
  Style[e_, ___]    := % @ e;
  e_List            := Total[% /@ e];
  s_String          := StringLength @ s;
  $hspace[n_]       := n;
];

(**************************************************************************************************)

vstackBlocks[rows_List, halign_] := Scope[
  widths = Part[rows, All, 2];
  heights = Part[rows, All, 3];
  maxWidth = Max @ widths;
  paddedRows = Map[hpadBlock[maxWidth, halign], rows];
  $block[Catenate @ Part[paddedRows, All, 1], maxWidth, Total[heights]]
];

hpadBlock[tw_, halign_][$block[rows_, w_, h_]] :=
  $block[hpadAtom[tw, halign][#, w]& /@ rows, tw, h];

(**************************************************************************************************)

hstackBlocks[cols_List, valign_] := Scope[
  widths = Part[cols, All, 2];
  heights = Part[cols, All, 3];
  maxHeight = Max @ heights;
  paddedCols = Map[vpadBlock[maxHeight, valign], cols];
  $block[MapThread[List, Part[paddedCols, All, 1]], Total @ widths, maxHeight]
];

vpadBlock[th_, valign_][$block[rows_, w_, h_]] := Scope[
  d = th - h;
  extendedRows = If[d <= 0, rows, Switch[valign,
    Top,    Join[rows, hspaceList[d, w]],
    Bottom, Join[hspaceList[d, w], rows],
    Center, Join[hspaceList[Floor[d / 2], w], rows, hspaceList[Ceiling[d / 2], w]],
    _,      ThrowMessage["badalign", valign, {Top, Center, Bottom}]
  ]];
  $block[extendedRows, w, th]
];

hspaceList[h_, w_] := ConstantArray[$hspace[w], h];

(**************************************************************************************************)

blockPadding[block_, None | 0] := block;

blockPadding[block:$block[elems_, w_, h_], framePadding_] := Scope[
  {{l, r}, {b, t}} = Round @ StandardizePadding @ framePadding;
  If[l == r == b == t == 0, Return @ block];
  If[t > 0, PrependTo[elems, $hspace[w]]; h = h + t];
  If[b > 0, AppendTo[elems, $hspace[w]]; h = h + b];
  If[l > 0, space = hspaceList[h, l]; elems = MapThread[Flatten @ {#1, #2}&, {space, elems}]];
  If[r > 0, space = hspaceList[h, r]; elems = MapThread[Flatten @ {#1, #2}&, {elems, space}]];
  $block[elems, w + l + r, h]
];

(**************************************************************************************************)

$extFrameP = _String | None | Dashed;

Options[gstackBlocks] = Options[StringMatrix];

$hframeSpec = {$extFrameP, $extFrameP} | _String | _StyleDecorated[{$extFrameP, $extFrameP} | _String];
gstackBlocks[rows_List, OptionsPattern[]] := Scope[
  UnpackOptions[rowAlignment, columnAlignment, rowSpacings, columnSpacings, dividers, frame, framePadding, frameStyle, rowFrames, rowFrameStyle, spanningFrame];
  frameStyle //= normFrameStyle;
  rowFrameStyle //= normFrameStyle;
  rows = riffleCols[columnSpacings] @ riffleRows[rowSpacings] @ rows;
  widths = Part[rows, All, All, 2]; heights = Part[rows, All, All, 3]; maxWidths = Max /@ Transpose[widths]; maxHeights = Max /@ heights;
  items = MapIndexed[gpadBlock, rows, {2}];
  If[StringQ[rowFrames],
    {lext, rext} = Lookup[$hextTableNames, rowFrames];
    jump = If[rowSpacings === 0, All, 1;;-1;;2];
    If[lext =!= None, items = MapAt[hframeBlock[#, {lext, None}, rowFrameStyle, spanningFrame]&, items, {jump, 1}]];
    If[rext =!= None, items = MapAt[hframeBlock[#, {None, rext}, rowFrameStyle, spanningFrame]&, items, {jump, -1}]];
    If[rowSpacings =!= 0,
      If[lext =!= None, items = MapAt[hframeBlock[#, {" ", None}, None, True]&, items, {2;;-1;;2, 1}]];
      If[rext =!= None, items = MapAt[hframeBlock[#, {None, " "}, None, True]&, items, {2;;-1;;2, -1}]];
    ];
    widths = Part[items, All, All, 2]; heights = Part[items, All, All, 3]; maxWidths = Max /@ Transpose[widths]; maxHeights = Max /@ heights;
  ];
  totalWidth = Total @ maxWidths; totalHeight = Total @ maxHeights;
  If[dividers === All,
    items = addDivs[items, maxWidths, maxHeights];
    totalWidth += Length[maxWidths] - 1;
    totalHeight += Length[maxHeights] - 1;
  ];
  grid = Catenate @ Map[Flatten /@ Transpose[Part[#, All, 1]]&, items];
  block = $block[grid, totalWidth, totalHeight];
  framePadding //= StandardizePadding;
  block = blockPadding[block, framePadding];
  Switch[frame,
    Part[maxWidths,   1] += Part[framePadding, 1, 1];
    Part[maxWidths,  -1] += Part[framePadding, 1, 2];
    Part[maxHeights, -1] += Part[framePadding, 2, 1];
    Part[maxHeights, -1] += Part[framePadding, 2, 2];
    totalWidth = Part[block, 2]; totalHeight = Part[block, 3];
    True | "Round" | "Square",
      sfn = applyStyle @ frameStyle;
      If[dividers === All,
        we = makeNotchedSides["│", {"├", "┤"}, totalHeight, maxHeights];
        sn = makeNotchedSides["─", {"┴", "┬"}, totalWidth, maxWidths];
      ,
        we = ConstantArray["│", {2, totalHeight}];
        sn = ConstantArray[repChar["─", totalWidth], 2];
      ];
      $block[
        apply8patch[First @ block, MatrixMap[sfn, we], sfn /@ sn, sfn /@ If[frame === "Round", $roundCompass, $squareCompass]],
        totalWidth + 2, totalHeight + 2
      ],
    {$extFrameP, $extFrameP} | _String,
      hframeBlock[block, frame, frameStyle, spanningFrame],
    False | None,
      block
  ]
,
  gpadBlock[elem_, {r_, c_}] := vpadBlock[Part[maxHeights, r], rowAlignment] @ hpadBlock[Part[maxWidths, c], columnAlignment] @ elem
];

StringBlock::fracwid = "Fractional width `` occurred in unsupported context."
makeNotchedSides[char_, {notch1_, notch2_}, n_, sizes_] := Scope[
  If[!IntegerQ[n], ThrowMessage["fracwid", n]];
  notches = Accumulate[Most[sizes] + 1];
  side = ConstantArray[char, n];
  List[
    ReplacePart[side, Transpose[List @ notches] -> notch1],
    ReplacePart[side, Transpose[List @ notches] -> notch2]
  ]
];

riffleCols[0] := Identity;
riffleCols[n_][rows_] := riffle[#, Spacer[n]]& /@ rows;

riffleRows[0] = Identity;
riffleRows[n_][rows_] := Transpose[riffle[#, Spacer[{1, n}]]& /@ Transpose[rows]];

addDivs[items_, ws_, hs_] := Scope[
  Table[
    Switch[IntegerQ /@ {r, c},
      {True, True},  Part[items, r, c],
      {True, False}, makeVBar[Part[hs, r]],
      {False, True}, makeHBar[Part[ws, c]],
      {False, False}, $block[{"┼"}, 1, 1]
    ],
    {r, 1, Length[hs], 1/2},
    {c, 1, Length[ws], 1/2}
  ]
];

makeVBar[h_] := $block[ConstantArray["│", h], 1, h];
makeHBar[w_] := $block[List @ repChar["─", w], w, 1];

(**************************************************************************************************)

$roundCompass = {"╭", "╮", "╰", "╯"};
$squareCompass = {"┌", "┐", "└", "┘"};

makeFrame[$block[rows_, w_, h_], r_] := Scope[
  we = ConstantArray["│", {2, h}];
  sn = ConstantArray[repChar["─", w], 2];
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

$hextTableNames = Association[
  "Round"        -> {"RoundLeft", "RoundRight"},
  "MidRound"     -> {"MidRoundLeft", "MidRoundRight"},
  "Square"       -> {"SquareLeft", "SquareRight"},
  "MidSquare"    -> {"MidSquareLeft", "MidSquareRight"},
  "DoubleSquare" -> {"DoubleSquareLeft", "DoubleSquareRight"},
  "Floor"        -> {"LeftFloor", "RightFloor"},
  "Ceiling"      -> {"LeftCeiling", "RightCeiling"},
  "["            -> {"[", None},
  "[]"           -> {"[", "]"},
  "{}"           -> {"{", "}"},
  "()"           -> {"(", ")"}
];

StringBlock::badframe = "`` is not a valid spec for Frame, which should be one of ``."

hframeBlock[arg1_, arg2_] := hframeBlock[arg1, arg2, None, True];

hframeBlock[block_, name_String, style_, spanning_] :=
  hframeBlock[block, Lookup[$hextTableNames, name, ThrowMessage["badframe", name, Keys @ $hextTableNames]], style, spanning];

hframeBlock[block_, None | {None, None}, style_, _] := block;

hframeBlock[$block[grid_, w_, h_], {l_, r_}, style_, spanning_] := Scope[
  $n = h; sfn = applyStyle @ style;
  {lext, rext} = MapThread[Map[sfn, extend[hextTable[#1], spanning, #2]]&, {{l, r}, {False, True}}];
  grid2 = MapThread[DeleteNone @ Flatten @ {#1, #2, #3}&, {lext, grid, rext}];
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

$vextTableNames = Association[
  "Round"        -> {"RoundTop", "RoundBottom"},
  "MidRound"     -> {"MidRoundTop", "MidRoundBottom"},
  "Square"       -> {"SquareTop", "SquareBottom"},
  "MidSquare"    -> {"MidSquareTop", "MidSquareBottom"},
  "DoubleSquare" -> {"DoubleSquareTop", "DoubleSquareBottom"}
];

vframeBlock[arg1_, arg2_] := vframeBlock[arg1, arg2, None, True];

vframeBlock[block_, name_String, style_, spanning_] :=
  vframeBlock[block, Lookup[$vextTableNames, name, ThrowMessage["badframe", name, Keys @ $vextTableNames]], style, spanning];

vframeBlock[block_, None | {None, None}, _, _] := block;

vframeBlock[$block[grid_, w_, h_], {t_, b_}, style_, spanning_] := Scope[
  $n = w; sfn = StyleOperator @ style;
  {bext, text} = MapThread[Map[sfn, extend[vextTable[#1], spanning, #2]]&, {{b, t}, {False, True}}];
  grid2 = grid;
  If[text =!= None, PrependTo[grid2, text]];
  If[bext =!= None, AppendTo[grid2, bext]];
  $block[grid2, w, h + If[b === None, 0, 1] + If[t === None, 0, 1]]
];

(**************************************************************************************************)

extend[None, _, _] := ConstantArray[None, $n];
extend[s_, False, side_] := ReplacePart[ConstantArray[" ", $n], If[side, -1, 1] -> First[s]];
extend[s_, True, side_] := extendSpanning[s];

extendSpanning = Case[
  ext1[c_]                 := ConstantArray[c, $n];
  ext3[s_, l_, m_, r_]     := If[$n === 1, {s}, Flatten @ {l, ConstantArray[m, $n - 2], r}];
  ext5[s_, l_, a_, m_, b_, r_] := Scope[
    If[$n === 1, Return @ {s}];
    n2 = ($n - 3) / 2;
    Flatten @ {l, ConstantArray[a, Floor @ n2], m, ConstantArray[a, Ceiling @ n2], r}
  ]
];

(* left vertical box line:  ⎸
right vertical box line: ⎹
open box: ␣
H       ─ ━ ┄ ┅ ┈ ┉
V       │ ┃ ┆ ┇ ┊ ┋
D R     ┌ ┏
U R     └ ┗
D L     ┐ ┓
U L     ┘ ┛
V R     ├ ┣  ┠
V L     ┤ ┫  ┨
H D     ┬ ┳  ┯
H U     ┴ ┻  ┷
H V     ┼ ╋

═ ║ ╔ ╗ ╚ ╝ ╠ ╦ ╪ ╬ ╟ ╢ ╤ ╧
╭ ╮
╯ ╰ ╱ ╲ ╳ ╴ ╵ ╶ ╷ ╸ ╹ ╺ ╻

corners:
 ⌟  ⌞⌝⌜
 ⌏⌎⌍
*)

(**************************************************************************************************)

