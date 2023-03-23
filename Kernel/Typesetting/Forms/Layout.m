PublicForm[ConcatenationForm]

DefineNAryForm[ConcatenationForm, KBox[RBox[$$1], RiffledBox[" "][$$1]]]

(**************************************************************************************************)

PublicForm[SpacedConcatenationForm]

DefineInfixForm[SpacedConcatenationForm, KBox[" ", "\\,"]];

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

(**************************************************************************************************)

PublicForm[PadForm]

DefineUnaryForm[PadForm, KBox[RBox[" ", $1, " "], RBox["\;", $1, "\;"]]]
(* TODO: migrate away from the old ones, rename the form names *)

(**************************************************************************************************)

PublicForm[CommaAndForm]

declareBoxFormatting[
  CommaAndForm[a_] :> tagAsMath @ MakeBoxes @ a,
  CommaAndForm[a_, b_] :> tagAsMath @ MakeBoxes @ TextAndForm[a, b],
  CommaAndForm[most__, a_, b_] :> tagAsMath @ MakeBoxes @ CommaRowForm[most, TextAndForm[a, b]]
]

(**************************************************************************************************)

PublicForm[TextAndForm]

DefineInfixForm[TextAndForm, MathTextBox[" and "]];

(**************************************************************************************************)

PublicForm[EmSpace, EnSpace]

PrivateVariable[$EmSpaceBox, $EnSpaceBox]

$EmSpaceBox = KBox["\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThinSpace]", "\\quad "];
$EnSpaceBox = KBox["\[ThickSpace]\[MediumSpace]", "\\enspace "];

DefineSymbolForm[{EmSpace -> $EmSpaceBox, EnSpace -> $EnSpaceBox}]

(**************************************************************************************************)

PublicForm[SpacedCommaRowForm]

DefineInfixForm[SpacedCommaRowForm, KBox[",  ", ",\\;"]]

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

DefineInfixForm[SpacedRowForm, $EmSpaceBox];

(**************************************************************************************************)

PublicForm[ThinSpacedForm]

DefineInfixForm[ThinSpacedForm, $EnSpaceBox];

(**************************************************************************************************)

PublicForm[RowForm]

DefineInfixForm[RowForm, KBox["\[ThinSpace]", "\\,"]];

(**************************************************************************************************)

PublicForm[PiecewiseForm, OtherwiseSymbol]

DefineSymbolForm[OtherwiseSymbol -> KBox[RomanBox["otherwise"], "\\text{otherwise}"]];

DefineStandardTraditionalForm[
  PiecewiseForm[cases__Rule] :> makePiecewiseBoxes[{cases}]
];

(**************************************************************************************************)

SetHoldAllComplete[makePiecewiseBoxes, makePiecewiseRow]

makePiecewiseBoxes[rules_List] := Scope[
  entries = MapUnevaluated[makePiecewiseRow, rules];
  casesGrid = GridBox[entries, ColumnAlignments -> {Left}, ColumnSpacings -> 1.2, ColumnWidths -> Automatic];
  TBox[casesGrid, "PiecewiseForm"]
];

makePiecewiseRow[All -> value_] :=
  {MakeQGBoxes @ value, MakeBoxes @ OtherwiseSymbol}

makePiecewiseRow[case_ -> value_] :=
  {MakeQGBoxes @ value, MakeQGBoxes @ case};

DefineNotebookDisplayFunction["PiecewiseForm", GridBox[
  {{"\[Piecewise]", #1}},
  ColumnAlignments -> {Left}, ColumnSpacings -> 0.5, ColumnWidths -> Automatic
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

PublicForm[SubstackForm]

DefineStandardTraditionalForm[
  SubstackForm[list_List] :> TBox[
    GridBox[MapUnevaluated[makeSubstackRow, list], RowSpacings -> 0, BaseStyle -> Small],
    "SubstackForm"
  ]
];

SetHoldAllComplete[makeSubstackRow]

makeSubstackRow = Case[
  {elems__} := List @ MakeBoxes @ RiffledForm[","][elems];
  other_    := List @ MakeQGBoxes @ other
];

DefineNotebookDisplayFunction["SubstackForm", #1&];

DefineKatexDisplayFunction["SubstackForm", substackKatex[#]&]

substackKatex[GridBox[entries_, ___]] :=
  "substack" @ Riffle[Catenate @ entries, "\\\\"];
