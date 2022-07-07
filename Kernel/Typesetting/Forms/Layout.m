PublicForm[ConcatenationForm]

declareNAryForm[ConcatenationForm];

$TemplateKatexFunction["ConcatenationForm"] = applyRiffled["concat", " "];

(**************************************************************************************************)

PublicForm[SpacedConcatenationForm]

declareNAryForm[SpacedConcatenationForm];

$TemplateKatexFunction["SpacedConcatenationForm"] = applyRiffled["concat", "\,"];

(**************************************************************************************************)

(* unfortunate we have to do this *)
Unprotect[GeneralUtilities`CommaForm];
GeneralUtilities`CommaForm[arg:Except[_List], rest___] :=
  CommaRowForm[arg, rest];

(**************************************************************************************************)

PublicForm[SpacedCommaForm, SpacedForm]

declareBoxFormatting[
(*   CommaForm[args___] :> CommaRowForm[args], *)
  SpacedCommaForm[args___] :> MakeBoxes @ SpacedCommaRowForm[args],
  SpacedForm[args___] :> MakeBoxes @ SpacedRowForm[args]
];

(* TODO: migrate away from the old ones, rename the form names *)

(**************************************************************************************************)

PublicForm[CommaAndForm]

declareBoxFormatting[
  CommaAndForm[a_, b_] :> MakeBoxes @ TextAndForm[a, b],
  CommaAndForm[most__, a_, b_] :> MakeBoxes @ CommaRowForm[most, TextAndForm[a, b]]
]

(**************************************************************************************************)

PublicForm[TextAndForm]

declareInfixSymbol[TextAndForm] // usingCustomKatex["textAnd"];

(**************************************************************************************************)

PublicForm[CommaRowForm]

declareNAryForm[CommaRowForm];

$TemplateKatexFunction["CommaRowForm"] = riffled[","];

(**************************************************************************************************)

PublicForm[SpacedCommaRowForm]

declareNAryForm[SpacedCommaRowForm];

$TemplateKatexFunction["SpacedCommaRowForm"] = riffled[",\;"];

(**************************************************************************************************)

PrivateFunction[spacerBox]

$TemplateKatexFunction["Spacer1"] = katexPtSpacer;

katexEmSpacer[0] := "";
katexEmSpacer[spacing_] := katexEmSpacer[spacing] = StringJoin["\\kern{", TextString @ spacing, "em}"];

katexPtSpacer[0] := "";
katexPtSpacer[spacing_] := katexPtSpacer[spacing] = StringJoin["\\kern{", TextString @ spacing, "pt}"];

spacerBox[n_] := TemplateBox[{n}, "Spacer1"];

(**************************************************************************************************)

PublicForm[SpacedRowForm]

declareNAryForm[SpacedRowForm];

$TemplateKatexFunction["SpacedRowForm"] = katexAliasRiffled["quad"];

(**************************************************************************************************)

PublicForm[ThinSpacedForm]

declareNAryForm[ThinSpacedForm];

$TemplateKatexFunction["ThinSpacedForm"] = katexAliasRiffled["enspace"];

(**************************************************************************************************)

PublicForm[MapsToForm]

declareBinaryForm[MapsToForm] // usingCustomKatex["mto"];
$TemplateKatexFunction["MapsToSymbol"] = katexAlias["mtoSymbol"];

(**************************************************************************************************)

PublicForm[RowForm, TermRowForm]

declareNAryForm[RowForm];
declareNAryForm[TermRowForm];

$TemplateKatexFunction["RowForm"] = riffled[" "];
$TemplateKatexFunction["TermRowForm"] = riffled[" "];

(**************************************************************************************************)

PublicForm[PiecewiseForm]

declareBoxFormatting[
  PiecewiseForm[cases__Rule] :> makePiecewiseBoxes[{cases}],
  OtherwiseSymbol :> SBox["OtherwiseSymbol"]
];

SetHoldAllComplete[makePiecewiseBoxes, makePiecewiseRow]

makePiecewiseBoxes[rules_List] := Scope[
  entries = MapUnevaluated[makePiecewiseRow, rules];
  grid = GridBox[
    {{"\[Piecewise]", GridBox[
      entries,
      ColumnAlignments -> {Left}, ColumnSpacings -> 1.2, ColumnWidths -> Automatic
    ]}},
    ColumnAlignments -> {Left}, ColumnSpacings -> 0.5, ColumnWidths -> Automatic
  ];
  TemplateBox[List @ grid, "PiecewiseForm"]
]

$TemplateKatexFunction["PiecewiseForm"] = katexPiecewise;
$TemplateKatexFunction["OtherwiseSymbol"] = "\\text{otherwise}"&;

katexPiecewise[GridBox[{{_, GridBox[entries_, ___]}}, ___]] := {
  "\\begin{cases}\n",
  katexPiecewiseRow @@@ entries,
  "\\end{cases}\n"
};

katexPiecewiseRow[case_, value_] :=
  {case, " &\\text{if } ", value, "\\\\\n"};

katexPiecewiseRow[case_, o:SBox["OtherwiseSymbol"]] :=
  {case, " &", o, "\n"};

makePiecewiseRow[All -> value_] :=
  {makeQGBoxes @ value, MakeBoxes @ OtherwiseSymbol}

makePiecewiseRow[case_ -> value_] :=
  {makeQGBoxes @ value, makeQGBoxes @ case};

(**************************************************************************************************)

PublicForm[SubstackForm]

declareBoxFormatting[
  SubstackForm[list_List] :> TemplateBox[
    List @ GridBox[MapUnevaluated[List @ makeQGBoxesOrComma @ #&, list], RowSpacings -> 0],
    "SubstackForm"
  ]
]

SetHoldAllComplete[makeQGBoxesOrComma]
makeQGBoxesOrComma = Case[
  {elems__} := MakeBoxes @ SpacedCommaRowForm @ elems;
  other_    := makeQGBoxes @ other
];

$TemplateKatexFunction["SubstackForm"] = substackKatex

substackKatex[GridBox[entries_, ___]] :=
  "substack" @ Riffle[Catenate @ entries, "\\\\"];

(**************************************************************************************************)

PublicForm[InfixForm]

declareBoxFormatting[
  InfixForm[symbol_String][args___] :> makeTemplateBox[symbol, args, "InfixForm"],
  InfixForm[symbol_][args___] :> makeTemplateBox[symbol[], args, "InfixForm"],
  InfixForm[symbol_String] :> symbol,
  InfixForm[symbol_] :> MakeBoxes @ symbol[]
];

$TemplateKatexFunction["InfixForm"] = katexInfix;

katexInfix[op_, rest___] := Riffle[{rest}, RBox["\\,", op, "\\,"]];

(**************************************************************************************************)

PublicForm[StyledInfixForm]

declareBoxFormatting[
  StyledInfixForm[style_, symbol_][args__] :> makeTemplateBox[style @ symbol[], args, "InfixForm"],
  StyledInfixForm[style_, symbol_] :> MakeBoxes @ style @ symbol[]
];

(**************************************************************************************************)

PublicForm[ParenthesesLabeledForm, ParenthesesRepeatedForm, UnderbraceLabeledForm, UnderbraceRepeatedForm, OverbraceLabeledForm, OverbraceRepeatedForm]

declareBoxFormatting[
  ParenthesesLabeledForm[a_, l_] :> makeTemplateBox[a, l, "ParenthesesLabeledForm"],
  ParenthesesRepeatedForm[a_, n_] :> makeTemplateBox[a, n, "ParenthesesRepeatedForm"],
  UnderbraceLabeledForm[a_, l_] :> makeTemplateBox[a, l, "UnderbraceLabeledForm"],
  UnderbraceRepeatedForm[a_, n_] :> makeTemplateBox[a, n, "UnderbraceRepeatedForm"],
  OverbraceLabeledForm[a_, l_] :> makeTemplateBox[a, l, "OverbraceLabeledForm"],
  OverbraceRepeatedForm[a_, n_] :> makeTemplateBox[a, n, "OverbraceRepeatedForm"]
];

$TemplateKatexFunction["ParenthesesLabeledForm"] = "parenLabeled";
$TemplateKatexFunction["ParenthesesRepeatedForm"] = "parenRepeated";
$TemplateKatexFunction["UnderbraceLabeledForm"] = "underLabeled";
$TemplateKatexFunction["UnderbraceRepeatedForm"] = "underRepeated";
$TemplateKatexFunction["OverbraceLabeledForm"] = "overLabeled";
$TemplateKatexFunction["OverbraceRepeatedForm"] = "overRepeated";

(**************************************************************************************************)

PublicForm[ModulusLabeledForm]

declareBoxFormatting[
  ModulusLabeledForm[a_, m_] :> makeTemplateBox[a, m, "ModulusLabeledForm"]
];

$TemplateKatexFunction["ModulusLabeledForm"] = "modLabeled";