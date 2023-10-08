PublicOption[ItemForm, GridSpacings, SpillLength, RowLabels, ColumnLabels]

(**************************************************************************************************)

PublicForm[MultiGridForm]

Options[MultiGridForm] = {
  Alignment -> Automatic,
  ItemForm -> None,
  ColumnSpacings -> 1,
  RowSpacings -> Automatic,
  GridSpacings -> 2,
  RowLabels -> None,
  ColumnLabels -> None
};

DefineStandardTraditionalForm[
  MultiGridForm[grids__List, opts___Rule] :> makeMultiGridBoxes[{grids}, opts]
];

(**************************************************************************************************)

PublicForm[GridForm]

Options[GridForm] = {
  Alignment -> None,
  ItemForm -> None,
  ColumnSpacings -> 1,
  RowSpacings -> Automatic,
  SpillLength -> Infinity,
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

PublicForm[GridRowsForm]

Options[GridRowsForm] = Options[GridForm];

DefineStandardTraditionalForm[
  GridRowsForm[rows__, opts___Rule] :> makeSingleGridBoxes[{rows}, opts]
];

(**************************************************************************************************)

PublicForm[GridColumnsForm]

Options[GridColumnsForm] = Options[multiColumnBoxes] = {
  ItemForm -> None,
  ColumnSpacings -> 3,
  RowSpacings -> Automatic,
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

PublicForm[SingleColumnForm]

DefineStandardTraditionalForm[
  SingleColumnForm[args__, opts___Rule] :> singleColumnBoxes[{args}, opts]
]

Options[SingleColumnForm] = Options[singleColumnBoxes] = {
  ItemForm -> None,
  ColumnSpacings -> 3,
  RowSpacings -> Automatic,
  SpillLength -> Infinity,
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
  If[Length[Unevaluated @ grid] > spillLength,
    items = Partition[grid, UpTo @ spillLength];
    Return @ makeMultiGridBoxes[items, opts];
  ];
  $itemForm = itemForm;
  TBoxOp["GridForm"] @ createGridBox[MapUnevaluated[makeGridRowBoxes, grid], alignment, rowSpacings, columnSpacings, rowLabels, columnLabels, labelFunction, labelSpacing]
];

(**************************************************************************************************)

Options[makeMultiGridBoxes] = JoinOptions[
  MultiGridForm,
  SpillLength -> Infinity (* to prevent messages, doesn't do anything *)
];

makeMultiGridBoxes[gridList_, OptionsPattern[]] := Scope[
  
  UnpackOptions[alignment, columnSpacings, rowSpacings, gridSpacings, itemForm, rowLabels, columnLabels];

  $itemForm = itemForm;
  {gridListBoxes, gridListAlignments, gridListRowSpacings, gridListColSpacings} =
    Transpose @ MapUnevaluated[
      createGridBoxData[MapUnevaluated[makeGridRowBoxes, #], alignment, rowSpacings, columnSpacings]&,
      gridList
    ];
  
  gridListHeights = Length /@ gridListBoxes;
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

createGridBoxData[rows_, alignments_, rowSpacings_, colSpacings_, rowLabels_:None, colLabels_:None, labelFn_:Identity, labelSpacing_:{1, 1}] := Scope[

  entries = padArray @ rows;
  {numRows, numCols} = Dimensions[entries, 2];
  
  SetAutomatic[alignments, autoAlignmentSpec @ numCols];
  alignments = StandardizeRowColumnSpec[alignments, numCols];

  colSpacings = StandardizeRowColumnSpec[colSpacings, numCols - 1];
  rowSpacings = StandardizeRowColumnSpec[rowSpacings, numRows - 1];

  hasColLabels = MatchQ[colLabels, _List | Placed[_List, _]];

  rowLabelSpacing = First[labelSpacing, labelSpacing];
  colLabelSpacing = Last[labelSpacing, labelSpacing];
  If[hasColLabels,
    If[!ListQ[rowSpacings], rowSpacings = Ones[numRows - 1] / 2];
    isBelow = MatchQ[colLabels, Placed[_, Below]];
    rowSpacings = Insert[rowSpacings, colLabelSpacing, If[isBelow, -1, 1]];
    colLabelBoxes = labelBoxes[labelFn, Replace[colLabels, Placed[l_, _] :> l]];
    If[isBelow, AppendTo, PrependTo][entries, PadRight[colLabelBoxes, numCols, ""]];
  ];

  If[ListQ[rowLabels],
    If[!ListQ[colSpacings], colSpacings = Ones[numCols - 1]];
    colSpacings = Insert[colSpacings, rowLabelSpacing, 1];
    rowLabelBoxes = labelBoxes[labelFn, rowLabels];
    rowLabelBoxes = If[hasColLabels,
      PadRight[If[isBelow, Append, Prepend][rowLabelBoxes, ""], numRows + 1, ""],
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

labelBoxes[None, labels_] := labelBoxes[Identity, labels];
labelBoxes[s:Bold|Italic, labels_] := labelBoxes[Style[#, s]&, labels];
labelBoxes[labelFn_, labels_] := ToBoxes[labelFn[#]]& /@ labels;

PrivateVariable[$infixFormCharacters]

(* TODO: pull this programmatically from DefineInfixForm, SymbolTranslation.txt etc *)

$infixFormCharacters = Association[
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

  e:((head_Symbol)[___]) /; KeyExistsQ[$infixFormCharacters, head] := riffledGridRowBox[$infixFormCharacters @ head, e];

  e_SyntaxEqualForm    := riffledGridRowBox["≑", e];

  e_                   := List @ makeGridEntryBox @ e;

];

riffledGridRowBox[div_, _[]] := {div};

riffledGridRowBox[div_, _[args__]] :=
  Riffle[MapUnevaluated[makeGridEntryBox, {args}], div];

makeGridEntryBox = Case[
  None | Null                       := "";
  Form[item_] /; $itemForm =!= None := MakeQGBoxes @ item;
  item_ /; $itemForm =!= None       := With[{if = $itemForm}, MakeQGBoxes @ if @ item];
  item_                             := MakeQGBoxes @ item
];

(**************************************************************************************************)

DefineKatexDisplayFunction["GridForm", katexGrid[#]&];

katexGrid[g_GridBox] :=
  katexGridBoxDispatch @@ getGridData[g];

katexGridBoxDispatch[entries_, alignments_, rowSpacings_, {0..}] :=
  {"\\begin{nsarray}{",
    toKatexAlignmentSpec[alignments, Length @ First @ entries], "}\n",
    createKatexGridBody[entries, rowSpacings],
    "\n\\end{nsarray}\n"
  };

katexGridBoxDispatch[entries_, alignments_, rowSpacings_, Automatic | {0, (1 | Automatic)..}] :=
  {"\\begin{array}{",
    toKatexAlignmentSpec[alignments, Length @ First @ entries], "}\n",
    createKatexGridBody[entries, rowSpacings],
    "\n\\end{array}\n"
  };

katexGridBoxDispatch[entries_, alignments_, rowSpacings_, colSpacings_] :=
  {"\\begin{csarray}{",
    toKatexAlignmentSpec[alignments, Length @ First @ entries], "}{",
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
  s_String /; StringMatchQ[s, "\"*\""] := StringTake[s, {2, -2}];
  StyleBox[e_, other___]               := StyleBox[stripQuotes @ e, other];
  other_                               := other;
];

assembleKatexGridRows[lines_, Automatic | {Automatic..}] :=
  Riffle[lines, "\\\\\n"];

assembleKatexGridRows[lines_, rowSpacings_] :=
  Riffle[lines,
    Map[
      If[# === Automatic, "\\\\\n", "\\\\[" <> TextString[#] <> "em]\n"]&,
      Rest @ rowSpacings
    ]
  ];

$spacingAlphabet = Characters["abcdefghijklmnopqrstuvwxyz"];

toKatexCSArraySpacingSpec[spacings_] :=
  StringJoin @ Part[$spacingAlphabet, Round[spacings * 4] + 1]

toKatexAlignmentSpec[Automatic, n_] :=
  toKatexAlignmentSpec[Table[Center, n], n];

toKatexAlignmentSpec[aligns_List, n_] :=
  StringJoin[
    toAlignmentLetter /@ If[Length[aligns] >= n, aligns, PadRight[aligns, n, aligns]]
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

PublicForm[FlowAlignedGridForm]
PublicOption[AlignmentSet]

Options[FlowAlignedGridForm] = {
  AlignmentSet -> {EqualForm, NotEqualForm},
  ColumnSpacings -> 1/2,
  RowSpacings -> Automatic,
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
  patt = Alternatives @@ Flatten[toFlowAlignmentSplits /@ alignmentSet];
  JoinTo[patt, "" | TemplateBox[_, "Spacer1"]];
  rows = Map[toRowBox, SequenceSplit[#, e:{patt} :> e]]& /@ rows;
  rows = PadRows[rows, ""];
  numColumns = Length @ First @ rows;
  TBoxOp["GridForm"] @ createGridBox[rows, alignment, rowSpacings, columnSpacings]
]

toFlowAlignmentSplits[s_] := Scope[
  If[KeyExistsQ[$flowAlignmentTable, s],
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
  SetForm -> {SBox["LeftBrace"], MakeQGBoxesList, SBox["RightBrace"]},
  ListForm -> {SBox["LeftBrace"], MakeQGBoxesList, SBox["RightBrace"]},
  TupleForm -> {"(", MakeQGBoxesList, ")"},
  MultisetForm -> {SBox["LeftMultisetBracket"], MakeQGBoxesList, SBox["RightMultisetBracket"]},
  AssociativeArrayForm -> {"\[LeftAngleBracket]", makeRuleBoxes, "\[RightAngleBracket]"}
|>;

MakeQGBoxesList = MakeQGBoxes /* List;
makeRuleBoxes[a_ -> b_] := {MakeQGBoxes @ a, " ", SBox["MapsToSymbol"], " ", MakeQGBoxes @ b};

flowAlignedRow = Case[

  e_List := MapUnevaluated[%, e];

  FunctionSignatureForm[f_, d_, c_] :=
    {MakeQGBoxes @ f, ":", MakeQGBoxes @ d, "\[RightArrow]", MakeQGBoxes @ c};

  AppliedForm[f_, h_Symbol[a_, b_]] /; KeyExistsQ[$flowAlignmentTable, h] :=
    {RBox[MakeQGBoxes @ f, "(", MakeQGBoxes @ a], $flowAlignmentTable @ h, RBox[MakeQGBoxes @ b, ")"]};

  ForAllForm[v_, b_] := {"\[ForAll]", MakeQGBoxes @ v, ":", MakeQGBoxes @ b};
  ExistsForm[v_, b_] := {"\[Exists]", MakeQGBoxes @ v, ":", MakeQGBoxes @ b};

  Form[e_] := MakeQGBoxes @ e;

  e:((h_Symbol)[_, __]) /; KeyExistsQ[$flowAlignmentTable, h] :=
    riffledFlowAlignedGridRow[$flowAlignmentTable @ h, e];

  (h_Symbol)[args___] /; KeyExistsQ[$flowAlignmentContainerTable, h] :=
    makeContainerFlow[h, args];

  e_ := MakeQGBoxes @ e;

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

PublicForm[EquationCascadeForm]

SetHoldAllComplete[equationCascadeFormBoxes]
DefineStandardTraditionalForm[
  EquationCascadeForm[first_, second_, rest__] :>
    equationCascadeFormBoxes[first, second, rest]
]

equationCascadeFormBoxes[first_, second_, rest__] :=
  TBoxOp["EquationGridForm"] @ createGridBox[
    Prepend[equationGridRow[EqualForm[first, second]]] @
          MapUnevaluated[equationGridRow[{None, "=", #}]&, {rest}],
    {Right, Center, Left}, Automatic, Automatic
  ];

(**************************************************************************************************)

PublicForm[EquationGridForm]
PublicForm[Aligned]
PublicSymbol[Divider]

SetUsage @ "
Divider is used in EquationGridForm.
"

SetHoldAllComplete[MakeQGBoxesOrNull, equationGridRow, riffledEqGridRow];
SetHoldFirst[equationGridFormBoxes];

Options[EquationGridForm] = {
  RowSpacings -> Automatic,
  ColumnSpacings -> Automatic,
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
  e_List                 := MapUnevaluated[MakeQGBoxesOrNull, e];
  Aligned[e_]            := % @ List @ Aligned @ e;
  e_EqualForm            := riffledEqGridRow["=", e];
  e_NotEqualForm         := riffledEqGridRow["\[NotEqual]", e];
  e_DefEqualForm         := riffledEqGridRow["≝", e];
  e_DotEqualForm         := riffledEqGridRow["≐", e];
  e_ColonEqualForm       := riffledEqGridRow["≔", e];
  e_SyntaxEqualForm      := riffledEqGridRow["≑", e];
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
  Riffle[MapUnevaluated[MakeQGBoxesOrNull, {args}], div];

PrivateSymbol[MakeQGBoxesOrNull]

MakeQGBoxesOrNull[Aligned[e_]] := Splice @ MapUnevaluated[MakeQGBoxes, {Aligner, e, Aligner}];
MakeQGBoxesOrNull[Null|None] := "";
MakeQGBoxesOrNull[other_] := MakeQGBoxes[other]

padArray[rows_] := Scope[
  maxLen = Max[Length /@ rows];
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

$equationSplitP = Alternatives @@ Values[$equationSymbolRules];

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
    {entries, Lookup[gridBoxAlignment, "Columns", Automatic]},
    Lookup[gridBoxSpacings, {"Rows", "Columns"}, Automatic]
  ]
]