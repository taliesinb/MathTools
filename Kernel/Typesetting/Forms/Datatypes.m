PublicForm[TupleForm]

DefineCommaForm[TupleForm, ParenthesesBox[$1], KatexMacroName -> "tuple"]

(**************************************************************************************************)

PublicForm[SetForm, StyledSetForm]

DefineCommaForm[SetForm, BracesBox[$1]]
DefineCommaForm[SignedSetForm, BracesBox[$1]];

DefineKatexDisplayFunction["setForm", "set"[#]&];
(* ^ override the KatexDisplayFunction that DefineCommaForm set up *)

DeclareStyledSequenceTemplateBox[StyledSetForm, "{", "}"];

(**************************************************************************************************)

PublicForm[AssociativeArrayForm]

DefineCommaForm[AssociativeArrayForm, AngleBracketBox["\[VeryThinSpace]", $1, "\[VeryThinSpace]"]];

DefineRuleAsMapsTo[AssociativeArrayForm];

(**************************************************************************************************)

PublicForm[SignedSetForm]

(**************************************************************************************************)

PublicForm[ListForm, StyledListForm]

DefineCommaForm[ListForm, SquareBracketBox["\[VeryThinSpace]", $1, "\[VeryThinSpace]"], KatexMacroName -> "list"];

declareStyledCommaRiffledForm[StyledListForm, "styledList"];

(**************************************************************************************************)

PublicFunction[LeftRightDotBox]

LeftRightDotBox[l_, inner___, r_] := KBox[
  RBox[
    DelimiterBox @ UnlimitedSpanBox @ l,
    MarginBox["\[CenterDot]", {-0.3, 0.2}],
    inner,
    MarginBox["\[CenterDot]", {0.2, -0.3}],
    DelimiterBox @ UnlimitedSpanBox @ r
  ],
  RBox[
    LeftBox @ l, """\negthickspace\cdot\thinspace""", inner, """\thinspace\cdot\negthickspace""", RightBox @ r
  ]
];

BracesDotBox[inner___] := LeftRightDotBox["{", inner, "}"]

(**************************************************************************************************)

PublicForm[MultisetForm, StyledMultisetForm, SignedMultisetForm, StyledSignedMultisetForm]

(* DefineCommaForm[MultisetForm, RBox[ToBoxes @ MultisetOpen, $1, ToBoxes @ MultisetClose]]
DefineCommaForm[SignedMultisetForm, RBox[ToBoxes @ MultisetOpen, $1, ToBoxes @ MultisetClose]]
 *)
DefineCommaForm[MultisetForm,       BracesDotBox[$1], BoxFunction -> MultisetBox]
DefineCommaForm[SignedMultisetForm, BracesDotBox[$1]]

declareStyledCommaRiffledForm[StyledMultisetForm, "styledMultiset"];
declareStyledCommaRiffledForm[StyledSignedMultisetForm, "styledSignedMultiset"];

(**************************************************************************************************)

PublicForm[RepeatedMultisetForm]

DefineInfixBinaryForm[RepeatedMultisetForm, "\[ThinSpace]"];

(**************************************************************************************************)

PublicForm[SetUnionForm, SetIntersectionForm, SetRelativeComplementForm]

DefineInfixForm[SetUnionForm, OpBox @ "\[Union]"]
DefineInfixForm[SetIntersectionForm, OpBox @ "\[Intersection]"]
DefineInfixBinaryForm[SetRelativeComplementForm, OpBox @ "-"]

(**************************************************************************************************)

PublicForm[PowerSetForm]

DefineNamedFunctionSymbolForm[
  PowerSetForm -> CaligraphicBox @ "P"
]

(**************************************************************************************************)

PublicForm[SetSymbolForm, SetElementSymbolForm]

DefineTaggedForm[{SetSymbolForm, SetElementSymbolForm}];

PublicForm[MultisetSymbolForm, MultisetElementSymbolForm]

DefineTaggedForm[{MultisetSymbolForm, MultisetElementSymbolForm}];

PublicForm[SignedSetSymbolForm, SignedSetElementSymbolForm]

DefineTaggedForm[{SignedSetSymbolForm, SignedSetElementSymbolForm}]

PublicForm[SignedMultisetSymbolForm, SignedMultisetElementSymbolForm]

DefineTaggedForm[{SignedMultisetSymbolForm, SignedMultisetElementSymbolForm}]

(**************************************************************************************************)

PublicForm[SubsetsForm, MultisetsForm]

PublicForm[SignedSubsetsForm, SignedMultisetsForm]

DefineNamedFunctionSymbolForm[{
  SubsetsForm -> CaligraphicBox @ "S",
  MultisetsForm -> CaligraphicBox @ "M",
  SignedSubsetsForm -> SignedBox @ CaligraphicBox @ "S",
  SignedMultisetsForm -> SignedBox @ CaligraphicBox @ "M"
}]

(**************************************************************************************************)

PublicSymbol[SmallSharpSymbol, MultisetMultiplicitySymbol, SignedMultisetMultiplicitySymbol]

DefineSymbolForm[{
  SmallSharpSymbol                 -> KBox["\[Sharp]", "\\,\raisebox{.1em}{\\small\\#}\\mkern{.1em}\\,"],
  MultisetMultiplicitySymbol       -> BoldBox @ ToBoxes @ SmallSharpSymbol,
  SignedMultisetMultiplicitySymbol -> BoldBox @ ToBoxes @ SmallSharpSymbol
}]

(**************************************************************************************************)

PublicForm[MultisetMultiplicityForm, SignedMultisetMultiplicityForm]

DefineInfixBinaryForm[MultisetMultiplicityForm, ToBoxes @ MultisetMultiplicitySymbol];
DefineInfixBinaryForm[SignedMultisetMultiplicityForm, ToBoxes @ SignedMultisetMultiplicitySymbol];

(**************************************************************************************************)

PublicForm[BoundMultiplicityFunctionForm, BoundSignedMultiplicityFunctionForm]

DefineUnaryForm[BoundMultiplicityFunctionForm, SuperscriptBox[$1, ToBoxes @ MultisetMultiplicitySymbol]]
DefineUnaryForm[BoundSignedMultiplicityFunctionForm, SuperscriptBox[$1, ToBoxes @ SignedMultisetMultiplicitySymbol]]

(**************************************************************************************************)

PublicForm[SetToMultisetForm, MultisetToSetForm]

DefineUnaryForm[SetToMultisetForm, SuperscriptBox[$1, "\[UpArrow]"]]
DefineUnaryForm[MultisetToSetForm, SuperscriptBox[$1, "\[DownArrow]"]]

(**************************************************************************************************)

PublicForm[SetConstructorForm, ListConstructorForm, MultisetConstructorForm, AssociativeArrayConstructorForm, CardinalityConstructorForm]

SetHoldAllComplete[makeConstructorBoxes, toColGrid];

makeConstructorBoxes[lhs_, rhs_, name_] := TBox[toColGrid @ lhs, toColGrid @ rhs, name];

toColGrid = Case[
  {a_}   := MakeQGBoxesOrNull @ a;
  {a__}  := MakeBoxes @ CommaRowForm[a];
  s_SingleColumnForm := MakeQGBoxes @ s;
  a_     := MakeQGBoxesOrNull @ a
];

DefineStandardTraditionalForm[{
  SetConstructorForm[lhs_, rhs_]              :> makeConstructorBoxes[lhs, rhs, "SetConstructorForm"],
  ListConstructorForm[lhs_, rhs_]             :> makeConstructorBoxes[lhs, rhs, "ListConstructorForm"],
  MultisetConstructorForm[lhs_, rhs_]         :> makeConstructorBoxes[lhs, rhs, "MultisetConstructorForm"],
  AssociativeArrayConstructorForm[lhs_, rhs_] :> makeConstructorBoxes[lhs, rhs, "AssociativeArrayConstructorForm"],
  CardinalityConstructorForm[lhs_, rhs_]      :> makeConstructorBoxes[lhs, rhs, "CardinalityConstructorForm"]
}];

SetAttributes[makeConstructorDisplayFunction, SequenceHold];

makeConstructorDisplayFunction[lbrac_, rbrac_, lmod_:Nothing, rmod_:Nothing] := StyleBox[GridBox[
  {{lbrac, StyleBox[#1, SpanAdjustments -> Automatic], "|", StyleBox[#2, SpanAdjustments -> Automatic], rbrac}},
    GridBoxSpacings -> {"Columns" -> {0, lmod, .2, .3, .4, .2, rmod, 0}},
    GridBoxItemSize -> {"Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
    GridBoxFrame -> {"ColumnsIndexed" -> {{{1, -1}, {1, -1}} -> GrayLevel[0, 0]}},
    GridBoxSpacings -> {"Columns"->{{0}}, "Rows"->{{0}}},
    GridBoxDividers -> {"Columns" -> cspec}
    (* GridBoxAlignment -> {"Rows" ->  {{Center}}} *)
  ], SpanAdjustments -> {{0, 0}, {0, 0}}, SpanMaxSize->Infinity]&;

(*
TODO: i used to put DelimiterBox @ "{" etc below, but this breaks the spanning-ness of the font.
the solution to this is use KaTeX_Size1 through 4 based on the contents of the left and right sides...
so estimate their needed size and choose the corresponding font *)

DefineNotebookDisplayFunction["SetConstructorForm",              makeConstructorDisplayFunction["{", "}"]];
DefineNotebookDisplayFunction["ListConstructorForm",             makeConstructorDisplayFunction["[", "]"]];
DefineNotebookDisplayFunction["MultisetConstructorForm",         makeConstructorDisplayFunction[Sequence["{", "\[CenterDot]"], Sequence["\[CenterDot]", "}"], -0.1, -0.1]];
DefineNotebookDisplayFunction["AssociativeArrayConstructorForm", makeConstructorDisplayFunction["\[LeftAngleBracket]", "\[RightAngleBracket]"]];
DefineNotebookDisplayFunction["CardinalityConstructorForm",      makeConstructorDisplayFunction["\[LeftBracketingBar]", "\[RightBracketingBar]"]];

strutted[lhs_, rhs_] := Sequence["\\;", lhs, """\,\,\middle|{\large\mathstrut}\,\,""", rhs, "\\;"];

DefineKatexDisplayFunction["SetConstructorForm",              BracesBox[strutted[#1, #2]]&];
DefineKatexDisplayFunction["ListConstructorForm",             SquareBracketBox[strutted[#1, #2]]&];
DefineKatexDisplayFunction["MultisetConstructorForm",         BracesDotBox[strutted[#1, #2]]&];
DefineKatexDisplayFunction["CardinalityConstructorForm",      BarBox[strutted[#1, #2]]&];
DefineKatexDisplayFunction["AssociativeArrayConstructorForm", AngleBracketBox[strutted[#1, #2]]&];

(**************************************************************************************************)

PublicForm[CartesianProductForm, CartesianSumForm, TensorProductForm, DirectSumForm]

DefineInfixForm[CartesianProductForm, OpBox @ "\[Times]"];
DefineInfixForm[CartesianSumForm,     OpBox @  "+"];
DefineInfixForm[TensorProductForm,    OpBox @ "\[CircleTimes]"];
DefineInfixForm[DirectSumForm,        OpBox @ "\[CirclePlus]"];

(**************************************************************************************************)

PublicForm[IndexedUnionForm, IndexedIntersectionForm]

DefineLegacyIndexedForm[IndexedUnionForm, KBox["\[Union] ", "\\bigcup"]]
DefineLegacyIndexedForm[IndexedIntersectionForm, KBox["\[Intersection] ", "\\bigcap"]]

(**************************************************************************************************)

PublicForm[SetCardinalityForm, MultisetCardinalityForm]

DefineUnaryForm[SetCardinalityForm,      RBox["\[LeftBracketingBar]", RBox["\[ThinSpace]", $1, "\[ThinSpace]"], "\[RightBracketingBar]"]]
DefineUnaryForm[MultisetCardinalityForm, RBox["\[LeftBracketingBar]", RBox["\[ThinSpace]", $1, "\[ThinSpace]"], "\[RightBracketingBar]"]]

