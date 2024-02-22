PublicOption[ItemForm, GridSpacings, SpillLength]

(**************************************************************************************************)

PublicTypesettingForm[MultiGridForm]

Options[MultiGridForm] = {
  Alignment -> Auto,
  ItemForm -> None,
  ColumnSpacings -> 1,
  RowSpacings -> Auto,
  GridSpacings -> 2,
  RowLabels -> None,
  ColumnLabels -> None
};

DefineStandardTraditionalForm[
  MultiGridForm[grids__List, opts___Rule] :> makeMultiGridBoxes[{grids}, opts]
];

(**************************************************************************************************)

PublicTypesettingForm[GridForm]

Options[GridForm] = {
  Alignment -> None,
  ItemForm -> None,
  ColumnSpacings -> 1,
  RowSpacings -> Auto,
  SpillLength -> Inf,
  GridSpacings -> 2,
  LabelSpacing -> {2, 1},
  LabelFunction -> Bold,
  RowLabels -> None,
  ColumnLabels -> None
};

DefineStandardTraditionalForm[
  GridForm[rows_List, opts___Rule] :> makeSingleGridBoxes[rows, opts]
];

(**************************************************************************************************)

PublicTypesettingForm[GridRowsForm]

Options[GridRowsForm] = Options[GridForm];

DefineStandardTraditionalForm[
  GridRowsForm[rows__, opts___Rule] :> makeSingleGridBoxes[{rows}, opts]
];

(**************************************************************************************************)

PublicTypesettingForm[GridColumnsForm]

Options[GridColumnsForm] = Options[multiColumnBoxes] = {
  ItemForm -> None,
  ColumnSpacings -> 3,
  RowSpacings -> Auto,
  Alignment -> Center
}

DefineStandardTraditionalForm[
  GridColumnsForm[cols__, opts___Rule] :> multiColumnBoxes[{cols}, opts]
];

multiColumnBoxes[cols_List, opts:OptionsPattern[]] := Scope[
  UnpackOptions[rowSpacings, columnSpacings, itemForm, alignment];
  grids = Flatten[{#}]& /@ cols;
  makeMultiGridBoxes[
    grids,
    RowSpacings -> rowSpacings,
    GridSpacings -> columnSpacings,
    ItemForm -> itemForm,
    Alignment -> alignment
  ]
]

(**************************************************************************************************)

PublicTypesettingForm[SingleColumnForm]

DefineStandardTraditionalForm[
  SingleColumnForm[args__, opts___Rule] :> singleColumnBoxes[{args}, opts]
]

Options[SingleColumnForm] = Options[singleColumnBoxes] = {
  ItemForm -> None,
  ColumnSpacings -> 3,
  RowSpacings -> Auto,
  SpillLength -> Inf,
  Alignment -> Center
}

singleColumnBoxes[items_List, opts:OptionsPattern[]] := Scope[
  UnpackOptions[rowSpacings, columnSpacings, itemForm, spillLength, alignment];
  items = List /@ Flatten @ items;
  makeSingleGridBoxes[
    items,
    RowSpacings -> rowSpacings,
    GridSpacings -> columnSpacings,
    ItemForm -> itemForm,
    Alignment -> alignment,
    SpillLength -> spillLength
  ]
]

(**************************************************************************************************)

DefineNotebookDisplayFunction["GridForm", #1&];

(**************************************************************************************************)

Options[makeSingleGridBoxes] = Options[GridForm];

makeSingleGridBoxes[grid_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[itemForm, alignment, rowSpacings, columnSpacings, spillLength, rowLabels, columnLabels, labelFunction, labelSpacing];
  If[Len[Uneval @ grid] > spillLength,
    items = Partition[grid, UpTo @ spillLength];
    Return @ makeMultiGridBoxes[items, opts];
  ];
  $itemForm = itemForm;
  TBoxOp["GridForm"] @ createGridBox[MapUnevaluated[makeGridRowBoxes, grid], alignment, rowSpacings, columnSpacings, rowLabels, columnLabels, labelFunction, labelSpacing]
];

(**************************************************************************************************)

Options[makeMultiGridBoxes] = JoinOptions[
  MultiGridForm,
  SpillLength -> Inf (* to prevent messages, doesn't do anything *)
];

makeMultiGridBoxes[gridList_, OptionsPattern[]] := Scope[
  
  UnpackOptions[alignment, columnSpacings, rowSpacings, gridSpacings, itemForm, rowLabels, columnLabels];

  $itemForm = itemForm;
  {gridListBoxes, gridListAlignments, gridListRowSpacings, gridListColSpacings} =
    Transpose @ MapUnevaluated[
      createGridBoxData[MapUnevaluated[makeGridRowBoxes, #], alignment, rowSpacings, columnSpacings]&,
      gridList
    ];
  
  gridListHeights = Len /@ gridListBoxes;
  maxHeight = Max[gridListHeights];
  
  gridListBoxes //= Map[PadColumns[#, maxHeight, ""]&];
  gridListBoxes = ArrayFlatten @ List @ gridListBoxes;

  rowSpacings = StandardizeRowColumnSpec[rowSpacings, maxHeight - 1];

  TBoxOp["GridForm"] @ GBox[gridListBoxes, Flatten @ gridListAlignments, rowSpacings, Flatten @ Riffle[gridListColSpacings, gridSpacings]]
];

(**************************************************************************************************)

PrivateFunction[createGridBox]

createGridBox[args___] := GBox @@ createGridBoxData[args];

(**************************************************************************************************)

createGridBoxData[rows_, alignments_, rowSpacings_, colSpacings_, rowLabels_:None, colLabels_:None, labelFn_:Id, labelSpacing_:{1, 1}] := Scope[

  entries = padArray @ rows;
  {numRows, numCols} = Dims[entries, 2];
  
  SetAutomatic[alignments, autoAlignmentSpec @ numCols];
  alignments = StandardizeRowColumnSpec[alignments, numCols];

  colSpacings = StandardizeRowColumnSpec[colSpacings, numCols - 1];
  rowSpacings = StandardizeRowColumnSpec[rowSpacings, numRows - 1];

  hasColLabels = MatchQ[colLabels, _List | Placed[_List, _]];

  rowLabelSpacing = F[labelSpacing, labelSpacing];
  colLabelSpacing = L[labelSpacing, labelSpacing];
  If[hasColLabels,
    If[!ListQ[rowSpacings], rowSpacings = Ones[numRows - 1] / 2];
    isBelow = MatchQ[colLabels, Placed[_, Below]];
    rowSpacings = Insert[rowSpacings, colLabelSpacing, If[isBelow, -1, 1]];
    colLabelBoxes = labelBoxes[labelFn, Rep[colLabels, Placed[l_, _] :> l]];
    If[isBelow, AppTo, PreTo][entries, PadRight[colLabelBoxes, numCols, ""]];
  ];

  If[ListQ[rowLabels],
    If[!ListQ[colSpacings], colSpacings = Ones[numCols - 1]];
    colSpacings = Insert[colSpacings, rowLabelSpacing, 1];
    rowLabelBoxes = labelBoxes[labelFn, rowLabels];
    rowLabelBoxes = If[hasColLabels,
      PadRight[If[isBelow, App, Pre][rowLabelBoxes, ""], numRows + 1, ""],
      PadRight[rowLabelBoxes, numRows, ""]
    ];
    entries = PrependColumn[entries, rowLabelBoxes];
  ];

  {entries, alignments, rowSpacings, colSpacings}
];

autoAlignmentSpec = Case[
  1 := {Center};
  2 := {Right, Left};
  _ := {Right, {Center}, Left};
]

labelBoxes[None, labels_] := labelBoxes[Id, labels];
labelBoxes[s:Bold|Italic, labels_] := labelBoxes[Style[#, s]&, labels];
labelBoxes[labelFn_, labels_] := ToBoxes[labelFn[#]]& /@ labels;

PrivateVariable[$infixFormCharacters]

(* TODO: pull this programmatically from DefineInfixForm, SymbolTranslation.txt etc *)

$infixFormCharacters = Assoc[
  EqualForm             -> "=",                 NotEqualForm            -> "≠",
  DefEqualForm          -> "≝",
  ColonEqualForm        -> "≔",
  DotEqualForm          -> "≐",
  IdenticallyEqualForm  -> "≡",

  ElementOfForm         -> "∈",
  Subset                -> "⊂",                 SubsetEqual             -> "⊆",
  Superset              -> "⊃",                 SupersetEqual           -> "⊇",
  CartesianProductForm  -> "×",

  Less                  -> "<",                 Greater                 -> ">",
  LessEqual             -> "≤",                 GreaterEqual            -> "≥",

  AndForm               -> "∧",                 OrForm                  -> "∨",
  
  ImpliesForm           -> "⟹",
  EquivalentForm        -> "⟺",
  IsomorphicForm        -> "≃",
  HomeomorphicForm      -> "≅",
  BijectiveForm         -> "≈",

  PlusForm              -> "+",                 TimesForm               -> "×",

  GraphUnionForm        -> "⊔",
  SetUnionForm          -> "⋃",                 SetIntersectionForm     -> "⋂",
  SetRelativeComplementForm -> "∖",

  RewriteForm           -> "\[Rule]"
];
  
makeGridRowBoxes = Case[
  
  item_List            := MapUnevaluated[makeGridEntryBox, item];

  a_ -> b_             := % @ {a, b};

  e:((head_Symbol)[___]) /; KeyQ[$infixFormCharacters, head] := riffledGridRowBox[$infixFormCharacters @ head, e];

  e_SyntaxEqualForm    := riffledGridRowBox["≑", e];

  e_                   := List @ makeGridEntryBox @ e;

];

riffledGridRowBox[div_, _[]] := {div};

riffledGridRowBox[div_, _[args__]] :=
  Riffle[MapUnevaluated[makeGridEntryBox, {args}], div];

makeGridEntryBox = Case[
  None | Null                       := "";
  Form[item_] /; $itemForm =!= None := MakeMathBoxes @ item;
  item_ /; $itemForm =!= None       := With[{if = $itemForm}, MakeMathBoxes @ if @ item];
  item_                             := MakeMathBoxes @ item
];

(**************************************************************************************************)

DefineKatexDisplayFunction["GridForm", katexGrid[#]&];

katexGrid[g_GridBox] :=
  katexGridBoxDispatch @@ getGridData[g];

katexGridBoxDispatch[entries_, alignments_, rowSpacings_, {0..}] :=
  {"\\begin{nsarray}{",
    toKatexAlignmentSpec[alignments, Len @ F @ entries], "}\n",
    createKatexGridBody[entries, rowSpacings],
    "\n\\end{nsarray}\n"
  };

katexGridBoxDispatch[entries_, alignments_, rowSpacings_, Auto | {0, (1 | Auto)..}] :=
  {"\\begin{array}{",
    toKatexAlignmentSpec[alignments, Len @ F @ entries], "}\n",
    createKatexGridBody[entries, rowSpacings],
    "\n\\end{array}\n"
  };

katexGridBoxDispatch[entries_, alignments_, rowSpacings_, colSpacings_] :=
  {"\\begin{csarray}{",
    toKatexAlignmentSpec[alignments, Len @ F @ entries], "}{",
    toKatexCSArraySpacingSpec[colSpacings],
    "}\n",
    createKatexGridBody[entries, rowSpacings],
    "\n\\end{csarray}\n"
  };

createKatexGridBody[entries_, rowSpacings_] :=
  assembleKatexGridRows[
    Riffle[#, " & "]& /@ MatrixMap[stripQuotes, entries],
    rowSpacings
  ];

stripQuotes = Case[
  s_Str /; SMatchQ[s, "\"*\""] := STake[s, {2, -2}];
  StyleBox[e_, other___]               := StyleBox[stripQuotes @ e, other];
  other_                               := other;
];

assembleKatexGridRows[lines_, Auto | {Auto..}] :=
  Riffle[lines, "\\\\\n"];

assembleKatexGridRows[lines_, rowSpacings_] :=
  Riffle[lines,
    Map[
      If[# === Auto, "\\\\\n", "\\\\[" <> TextString[#] <> "em]\n"]&,
      Rest @ rowSpacings
    ]
  ];

$spacingAlphabet = Chars["abcdefghijklmnopqrstuvwxyz"];

toKatexCSArraySpacingSpec[spacings_] :=
  SJoin @ Part[$spacingAlphabet, Round[spacings * 4] + 1]

toKatexAlignmentSpec[Auto, n_] :=
  toKatexAlignmentSpec[Table[Center, n], n];

toKatexAlignmentSpec[aligns_List, n_] :=
  SJoin[
    toAlignmentLetter /@ If[Len[aligns] >= n, aligns, PadRight[aligns, n, aligns]]
  ];

toAlignmentLetter = Case[
  Left    := "l";
  Center  := "c";
  Right   := "r";
  None    := "c";
];

(**************************************************************************************************)

PublicSymbol[Aligner]

DefineStandardTraditionalForm[
  Aligner :> SBox["Aligner"]
];

$TemplateKatexFunction["Aligner"] = "&"&;

(**************************************************************************************************)

PublicTypesettingForm[FlowAlignedGridForm]
PublicOption[AlignmentSet]

Options[FlowAlignedGridForm] = {
  AlignmentSet -> {EqualForm, NotEqualForm},
  ColumnSpacings -> 1/2,
  RowSpacings -> Auto,
  Alignment -> Center
}

DefineStandardTraditionalForm[
  FlowAlignedGridForm[args__, opts___Rule] :> makeFlowAlignedGridForm[{args}, opts]
]

SetHoldAllComplete[makeFlowAlignedGridForm, flowAlignedRow]

makeFlowAlignedGridForm[rawRows_List, OptionsPattern[FlowAlignedGridForm]] := Scope[
  UnpackOptions[alignmentSet, columnSpacings, rowSpacings, alignment];
  rows = MapUnevaluated[flowAlignedRow, rawRows];
  rows = Flatten[{#}]& /@ rows;
  patt = Alt @@ Flatten[toFlowAlignmentSplits /@ alignmentSet];
  JoinTo[patt, "" | TemplateBox[_, "Spacer1"]];
  rows = Map[toRowBox, SequenceSplit[#, e:{patt} :> e]]& /@ rows;
  rows = PadRows[rows, ""];
  numColumns = Len @ F @ rows;
  TBoxOp["GridForm"] @ createGridBox[rows, alignment, rowSpacings, columnSpacings]
]

toFlowAlignmentSplits[s_] := Scope[
  If[KeyQ[$flowAlignmentTable, s],
    Return @ $flowAlignmentTable @ s];
  {left, boxFn, right} = Lookup[$flowAlignmentContainerTable, s,
    Print["BAD ALIGNSET: ", alignmentSet]; Return @ Nothing];
  res = {left, ", ", right};
  If[s === AssociativeArrayForm, JoinTo[res, {"", SBox["MapsToSymbol"]}]];
  res
]

$flowAlignmentTable = <|
  FunctionSignatureForm -> ":"|"\[RightArrow]",
  InfixStateJoinForm -> "\[SquareUnion]",
  InfixStateMeetForm -> "\[SquareIntersection]",
  EqualForm -> "=", NotEqualForm -> "\[NotEqual]",
  DefEqualForm -> "≝", ColonEqualForm -> "≔", DotEqualForm -> "≐", SyntaxEqualForm -> "≑",
  Subset -> "\[Subset]", SubsetEqual -> "\[SubsetEqual]", Superset -> "\[Superset]", SupersetEqual -> "\[SupersetEqual]",
  ElementOfForm -> "\[Element]",
  AndForm -> "\[And]", OrForm -> "\[Or]", ImpliesForm -> "\[Implies]", EquivalentForm -> "\[Equivalent]",
  IsomorphicForm -> "\[TildeEqual]", HomeomorphicForm -> "\[TildeFullEqual]",
  "" -> "",
  ForAllForm -> "\[ForAll]" | ":", ExistsForm -> "\[Exists]" | ":",
  SetUnionForm -> "\[Union]", SetIntersectionForm -> "\[Intersection]", SetRelativeComplementForm -> "\[Backslash]"
|>;


$flowAlignmentContainerTable = <|
  SetForm -> {SBox["LeftBrace"], MakeMathBoxesList, SBox["RightBrace"]},
  ListForm -> {SBox["LeftBrace"], MakeMathBoxesList, SBox["RightBrace"]},
  TupleForm -> {"(", MakeMathBoxesList, ")"},
  MultisetForm -> {SBox["LeftMultisetBracket"], MakeMathBoxesList, SBox["RightMultisetBracket"]},
  AssociativeArrayForm -> {"\[LeftAngleBracket]", makeRuleBoxes, "\[RightAngleBracket]"}
|>;

MakeMathBoxesList = MakeMathBoxes /* List;
makeRuleBoxes[a_ -> b_] := {MakeMathBoxes @ a, " ", SBox["MapsToSymbol"], " ", MakeMathBoxes @ b};

flowAlignedRow = Case[

  e_List := MapUnevaluated[%, e];

  FunctionSignatureForm[f_, d_, c_] :=
    {MakeMathBoxes @ f, ":", MakeMathBoxes @ d, "\[RightArrow]", MakeMathBoxes @ c};

  AppliedForm[f_, h_Symbol[a_, b_]] /; KeyQ[$flowAlignmentTable, h] :=
    {RBox[MakeMathBoxes @ f, "(", MakeMathBoxes @ a], $flowAlignmentTable @ h, RBox[MakeMathBoxes @ b, ")"]};

  ForAllForm[v_, b_] := {"\[ForAll]", MakeMathBoxes @ v, ":", MakeMathBoxes @ b};
  ExistsForm[v_, b_] := {"\[Exists]", MakeMathBoxes @ v, ":", MakeMathBoxes @ b};

  Form[e_] := MakeMathBoxes @ e;

  e:((h_Symbol)[_, __]) /; KeyQ[$flowAlignmentTable, h] :=
    riffledFlowAlignedGridRow[$flowAlignmentTable @ h, e];

  (h_Symbol)[args___] /; KeyQ[$flowAlignmentContainerTable, h] :=
    makeContainerFlow[h, args];

  e_ := MakeMathBoxes @ e;

];

makeContainerFlow[h_, args___] := Scope[
  {pre, boxFn, post} = $flowAlignmentContainerTable @ h;
  Join[{pre}, MapMost[attachComma, Map[boxFn, {args}]], {post}]
]

attachComma[{a_, " ", s:SBox["MapsToSymbol"], " ", b_}] := {a, " ", s, " ", addComma[b], ""}
attachComma[e_] := {e, ", "};

riffledFlowAlignedGridRow[div_, _[args__]] :=
  Riffle[MapUnevaluated[flowAlignedRow, {args}], div];

toRowBox = Case[
  {e_} := e;
  e_List := RowBox[Riffle[e, " "]];
];

(**************************************************************************************************)

PublicTypesettingForm[EquationCascadeForm]

SetHoldAllComplete[equationCascadeFormBoxes]
DefineStandardTraditionalForm[
  EquationCascadeForm[first_, second_, rest__] :>
    equationCascadeFormBoxes[first, second, rest]
]

equationCascadeFormBoxes[None, second_, rest__] :=
  TBoxOp["EquationGridForm"] @ createGridBox[
    Pre[equationGridRow[{None, None, second}]] @
          MapUnevaluated[equationGridRow[{None, "=", #}]&, {rest}],
    {Right, Center, Left}, Auto, Auto
  ];

equationCascadeFormBoxes[first_, second_, rest__] :=
  TBoxOp["EquationGridForm"] @ createGridBox[
    Pre[equationGridRow[EqualForm[first, second]]] @
          MapUnevaluated[equationGridRow[{None, "=", #}]&, {rest}],
    {Right, Center, Left}, Auto, Auto
  ];

(**************************************************************************************************)

PublicTypesettingForm[EquationGridForm]
PublicTypesettingForm[Aligned]
PublicSymbol[Divider]

SetUsage @ "
Divider is used in EquationGridForm.
"

SetHoldAllComplete[MakeMathBoxesOrNull, equationGridRow, riffledEqGridRow];
SetHoldFirst[equationGridFormBoxes];

Options[EquationGridForm] = {
  RowSpacings -> Auto,
  ColumnSpacings -> Auto,
  Alignment -> {Right, Center, Left}
}

DefineStandardTraditionalForm[
  EquationGridForm[args__, opts:OptionsPattern[]] :>
    equationGridFormBoxes[{args}, OptionValue[EquationGridForm, {opts}, {Alignment, RowSpacings, ColumnSpacings}]]
];

DefineNotebookDisplayFunction["EquationGridForm", #1&];

equationGridFormBoxes[rows_, {alignment_, rowSpacings_, colSpacings_}] :=
  TBoxOp["EquationGridForm"] @ createGridBox[
    MapUnevaluated[equationGridRow, rows],
    alignment, rowSpacings, colSpacings
  ];

equationGridRow = Case[
  Divider                := {"---"};
  e_List                 := MapUnevaluated[MakeMathBoxesOrNull, e];
  Aligned[e_]            := % @ List @ Aligned @ e;
  e_EqualForm            := riffledEqGridRow["=", e];
  e_NotEqualForm         := riffledEqGridRow["\[NotEqual]", e];
  e_DefEqualForm         := riffledEqGridRow["≝", e];
  e_DotEqualForm         := riffledEqGridRow["≐", e];
  e_ColonEqualForm       := riffledEqGridRow["≔", e];
  e_SyntaxEqualForm      := riffledEqGridRow["≑", e];
  e_TripleEqualForm      := riffledEqGridRow["\[Congruent]", e];
  e_IdenticallyEqualForm := riffledEqGridRow["\[Congruent]", e];
  e_Less                 := riffledEqGridRow["<", e];
  e_Greater              := riffledEqGridRow[">", e];
  e_LessEqual            := riffledEqGridRow["≤", e];
  e_GreaterEqual         := riffledEqGridRow["≥", e];
  e_Subset               := riffledEqGridRow["\[Subset]", e];
  e_SubsetEqual          := riffledEqGridRow["\[SubsetEqual]", e];
  e_Superset             := riffledEqGridRow["\[Superset]", e];
  e_SupersetEqual        := riffledEqGridRow["\[SupersetEqual]", e];
  e_ElementOfForm        := riffledEqGridRow["\[Element]", e];
  e_AndForm              := riffledEqGridRow["\[And]", e];
  e_OrForm               := riffledEqGridRow["\[Or]", e];
  e_ImpliesForm          := riffledEqGridRow["\[Implies]", e];
  e_EquivalentForm       := riffledEqGridRow["\[Equivalent]", e];
  e_IsomorphicForm       := riffledEqGridRow["\[TildeEqual]", e];
  e_HomeomorphicForm     := riffledEqGridRow["\[TildeFullEqual]", e];
  e_BijectiveForm        := riffledEqGridRow["\[TildeTilde]", e];
  e_RewriteForm          := riffledEqGridRow["\[Rule]", e];
  e_MapsBetweenForm      := riffledEqGridRow["\[LeftRightArrow]", e];
];

riffledEqGridRow[div_, _[args__]] :=
  Riffle[MapUnevaluated[MakeMathBoxesOrNull, {args}], div];

PrivateTypesettingBoxFunction[MakeMathBoxesOrNull]

MakeMathBoxesOrNull[Aligned[e_]] := Splice @ MapUnevaluated[MakeMathBoxes, {Aligner, e, Aligner}];
MakeMathBoxesOrNull[Null|None] := "";
MakeMathBoxesOrNull[other_] := MakeMathBoxes[other]

padArray[rows_] := Scope[
  maxLen = Max[Len /@ rows];
  PadRight[#, maxLen, ""]& /@ rows
];

DefineKatexDisplayFunction["EquationGridForm", katexEquationGrid[#1]&];

(* TODO: replace this with riffled! *)
$equationSymbolRules = {
  "="                                             -> "&= ",
  "\[NotEqual]"                                   -> "&\\neq ",
  "+"                                             -> "&+ ",
  "\[Element]"                                    -> "&\\in ",
  "≔"                                             -> "&\\coloneqq ",
  "≐"                                             -> "&\\doteq ",
  "≑"                                             -> "&≝",
  "≝"                                             -> "&≝",
  "---"                                           -> "\\hline",
  "\[Subset]"                                     -> "&\\subset ",
  "\[SubsetEqual]"                                -> "&\\subseteq ",
  "\[Superset]"                                   -> "&\\supset ",
  "\[SupersetEqual]"                              -> "&\\supseteq ",
  "where"                                         -> "&\\text{where} ",
  "\[TildeEqual]"                                 -> "&\[TildeEqual]",
  "\[TildeFullEqual]"                             -> "&\[TildeFullEqual]",
  "\[LeftRightArrow]"                             -> "&\[LeftRightArrow]",
  "\[TildeTilde]"                                 -> "&\[TildeTilde] ",
  (* "=>"                                            -> "&\[Implies] ", TODO: work around DirectedEdge sugar *)
  "\[Implies]"                                    -> "&\[Implies] ",
  "\[And]"                                        -> "&\[And] ",
  "\[Or]"                                         -> "&\[Or] ",
  "\[Congruent]"                                  -> "&\[Congruent]",
  "\[Rule]"                                       -> "&\[Rule]",
  "<"                                             -> "&\\lt",
  ">"                                             -> "&\\gt",
  "<="                                            -> "&\\le",
  ">="                                            -> "&\\ge",
  " "                                             -> "\\, ",
  "\t"                                            -> "&\\quad "
}

$equationSplitP = Alt @@ Values[$equationSymbolRules];

katexEquationGrid[g_GridBox] := Scope[
  {entries, alignments, rowSpacings, columnSpacings} = getGridData[g];
  lines = assembleKatexGridRows[entries, rowSpacings];
  lines = MatrixReplace[lines, $equationSymbolRules];
  lines = VectorReplace[lines, {"\\hline", h___} :> {"\\hline \\\\[-2ex]"}];
  {"\\begin{aligned}\n", lines, "\\end{aligned}\n"}
]

getGridData[GridBox[entries_, opts:OptionsPattern[GridBox]]] := Scope[
  {gridBoxAlignment, gridBoxSpacings} = OptionValue[GridBox, {opts}, {GridBoxAlignment, GridBoxSpacings}];
  Join[
    {entries, Lookup[gridBoxAlignment, "Columns", Auto]},
    Lookup[gridBoxSpacings, {"Rows", "Columns"}, Auto]
  ]
]