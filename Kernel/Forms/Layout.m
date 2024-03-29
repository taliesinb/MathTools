PublicTypesettingForm[ConcatenationForm]

DefineNAryForm[ConcatenationForm, KBox[RBox[$$1], RiffledBox[" "][$$1]]]

(**************************************************************************************************)

PublicTypesettingForm[SpacedConcatenationForm]

DefineInfixForm[SpacedConcatenationForm, KBox[" ", "\\,"]];

(**************************************************************************************************)

(* unfortunate we have to do this *)
Unprotect[GeneralUtilities`CommaForm];
GeneralUtilities`CommaForm[arg:Except[_List], rest___] :=
  CommaRowForm[arg, rest];

(**************************************************************************************************)

PublicTypesettingForm[SpacedCommaForm, SpacedForm]

declareBoxFormatting[
(*   CommaForm[args___] :> CommaRowForm[args], *)
  SpacedCommaForm[args___] :> MakeBoxes @ SpacedCommaRowForm[args],
  SpacedForm[args___] :> MakeBoxes @ SpacedRowForm[args]
];

(**************************************************************************************************)

PublicTypesettingForm[PadForm]

DefineUnaryForm[PadForm, KBox[RBox[" ", $1, " "], RBox["\;", $1, "\;"]]]
(* TODO: migrate away from the old ones, rename the form names *)

(**************************************************************************************************)

PublicTypesettingForm[CommaAndForm]

declareBoxFormatting[
  CommaAndForm[a_] :> tagAsMath @ MakeBoxes @ a,
  CommaAndForm[a_, b_] :> tagAsMath @ MakeBoxes @ TextAndForm[a, b],
  CommaAndForm[most__, a_, b_] :> tagAsMath @ MakeBoxes @ CommaRowForm[most, TextAndForm[a, b]]
]

(**************************************************************************************************)

PublicTypesettingForm[TextAndForm]

DefineInfixForm[TextAndForm, MathTextBox[" and "]];

(**************************************************************************************************)

PublicTypesettingForm[EmSpace, EnSpace]

PrivateVariable[$EmSpaceBox, $EnSpaceBox]

$EmSpaceBox = KBox["\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThinSpace]", "\\quad "];
$EnSpaceBox = KBox["\[ThickSpace]\[MediumSpace]", "\\enspace "];

DefineSymbolForm[{EmSpace -> $EmSpaceBox, EnSpace -> $EnSpaceBox}]

(**************************************************************************************************)

PublicTypesettingForm[SpacedCommaRowForm]

DefineInfixForm[SpacedCommaRowForm, KBox[",  ", ",\\;"]]

(**************************************************************************************************)

PrivateTypesettingBoxFunction[spacerBox, katexPadded]

$TemplateKatexFunction["Spacer1"] = katexPtSpacer;

katexEmSpacer[0] := "";
katexEmSpacer[spacing_] := katexEmSpacer[spacing] = SJoin["\\kern{", TextString @ spacing, "em}"];

katexPtSpacer[0] := "";
katexPtSpacer[spacing_] := katexPtSpacer[spacing] = SJoin["\\kern{", TextString @ spacing, "pt}"];

spacerBox[n_] := TemplateBox[{n}, "Spacer1"];

katexPadded[b_, {l_, r_}] := {katexEmSpacer[l], b, katexEmSpacer[r]};

(**************************************************************************************************)

PublicTypesettingForm[SpacedRowForm]

DefineInfixForm[SpacedRowForm, $EmSpaceBox];

(**************************************************************************************************)

PublicTypesettingForm[ThinSpacedForm]

DefineInfixForm[ThinSpacedForm, $EnSpaceBox];

(**************************************************************************************************)

PublicTypesettingForm[RowForm]

DefineInfixForm[RowForm, KBox["\[ThinSpace]", "\\,"]];

(**************************************************************************************************)

PublicTypesettingForm[PiecewiseForm, OtherwiseSymbol]

DefineSymbolForm[OtherwiseSymbol -> KBox[RomanBox["otherwise"], "\\text{otherwise}"]];

DefineStandardTraditionalForm[
  PiecewiseForm[cases__Rule] :> makePiecewiseBoxes[{cases}]
];

(**************************************************************************************************)

SetHoldAllComplete[makePiecewiseBoxes, makePiecewiseRow]

makePiecewiseBoxes[rules_List] := Scope[
  entries = MapUnevaluated[makePiecewiseRow, rules];
  casesGrid = GridBox[entries, ColumnAlignments -> {Left}, ColumnSpacings -> 1.2, ColumnWidths -> Auto];
  TBox[casesGrid, "PiecewiseForm"]
];

makePiecewiseRow[All -> value_] :=
  {MakeMathBoxes @ value, MakeBoxes @ OtherwiseSymbol}

makePiecewiseRow[case_ -> value_] :=
  {MakeMathBoxes @ value, MakeMathBoxes @ case};

DefineNotebookDisplayFunction["PiecewiseForm", GridBox[
  {{"\[Piecewise]", #1}},
  ColumnAlignments -> {Left}, ColumnSpacings -> 0.5, ColumnWidths -> Auto
]&];

(**************************************************************************************************)

DefineKatexDisplayFunction["PiecewiseForm", katexPiecewise[#]&]

katexPiecewise[GridBox[entries_, ___]] := {
  "\\begin{cases}\n",
  katexPiecewiseRow @@@ entries,
  "\\end{cases}\n"
};

katexPiecewiseRow[case_, value_] :=
  {case, " &\\text{if } ", value, "\\\\\n"};

katexPiecewiseRow[case_, o:SBox["OtherwiseSymbol"]] :=
  {case, " &", o, "\n"};

(**************************************************************************************************)

PublicTypesettingForm[SubstackForm]

DefineStandardTraditionalForm[
  SubstackForm[list_List] :> TBox[
    GridBox[MapUnevaluated[makeSubstackRow, list], RowSpacings -> 0, BaseStyle -> Small],
    "SubstackForm"
  ]
];

SetHoldAllComplete[makeSubstackRow]

makeSubstackRow = Case[
  {elems__} := List @ MakeBoxes @ RiffledForm[","][elems];
  other_    := List @ MakeMathBoxes @ other
];

DefineNotebookDisplayFunction["SubstackForm", #1&];

DefineKatexDisplayFunction["SubstackForm", substackKatex[#]&]

substackKatex[GridBox[entries_, ___]] :=
  "substack" @ Riffle[Catenate @ entries, "\\\\"];

(**************************************************************************************************)

PublicTypesettingForm[LabeledForm]

DefineStandardTraditionalForm[
  LabeledForm[thing_, label_] :> ToBoxes @ Labeled[thing, MathTextForm @ label]
]

(**************************************************************************************************)

PublicTypesettingForm[DoubleFractionForm]

DefineBinaryForm[DoubleFractionForm,
  GridBox[
    {{$1}, {""}, {""}, {$2}},
    RowSpacings -> {.2, 0, 1},
    RowLines -> {False, True, True, False},
    RowMinHeight -> 0
  ]
]

(**************************************************************************************************)

PublicTypesettingForm[DoubleFractionGrid]

DefineStandardTraditionalForm[
  DoubleFractionGrid[row1_, row2_] :> With[
    {n = Max[Len @ row1, Len @ row2]},
    {s = Repeat["", n]},
    TBox[GridBox[
      {MakeMathBoxes /@ PadRight[row1, n, ""], s, s, MakeMathBoxes /@ PadRight[row2, n, ""]},
      RowSpacings -> {.2, 0, 1},
      RowLines -> {False, True, True, False},
      RowMinHeight -> 0
    ], "MathForm"]
  ]
];

