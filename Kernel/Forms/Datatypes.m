PublicTypesettingForm[TupleForm, StyledTupleForm]

DefineCommaForm[TupleForm, ParenthesesBox[$1], KatexMacroName -> "tuple"]

StyledTupleForm[style_] := StyleDecorated[style, TupleForm];

(**************************************************************************************************)

PublicTypesettingForm[SetForm, StyledSetForm]

DefineCommaForm[SetForm, BracesBox[$1]]
DefineCommaForm[SignedSetForm, BracesBox[$1]];

DefineKatexDisplayFunction["setForm", "set"[#]&];
(* ^ override the KatexDisplayFunction that DefineCommaForm set up *)

DefineStandardTraditionalForm[
  StyledSetForm[style_][args___] :> ToBoxes @ StyleDecorated[style, SetForm][args]
]

(**************************************************************************************************)

PublicTypesettingForm[AssociativeArrayForm]

DefineCommaForm[AssociativeArrayForm, AngleBracketBox["\[VeryThinSpace]", $1, "\[VeryThinSpace]"]];

DefineRuleAsMapsTo[AssociativeArrayForm];

(**************************************************************************************************)

PublicTypesettingForm[SignedSetForm]

(**************************************************************************************************)

PublicTypesettingForm[ListForm, StyledListForm]

DefineCommaForm[ListForm, SquareBracketBox["\[VeryThinSpace]", $1, "\[VeryThinSpace]"], KatexMacroName -> "list"];

StyledListForm[style_] := StyleDecorated[style, ListForm];

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

PublicTypesettingForm[MultisetForm, StyledMultisetForm, SignedMultisetForm, StyledSignedMultisetForm]

(* DefineCommaForm[MultisetForm, RBox[ToBoxes @ MultisetOpen, $1, ToBoxes @ MultisetClose]]
DefineCommaForm[SignedMultisetForm, RBox[ToBoxes @ MultisetOpen, $1, ToBoxes @ MultisetClose]]
 *)
DefineCommaForm[MultisetForm,       BracesDotBox[$1], BoxFunction -> MultisetBox]
DefineCommaForm[SignedMultisetForm, BracesDotBox[$1]]

StyledMultisetForm[style_] := StyleDecorated[style, MulitsetForm];
StyledSignedMultisetForm[style_] := StyleDecorated[style, SignedMulitsetForm];

(**************************************************************************************************)

PublicTypesettingForm[RepeatedMultisetForm]

DefineInfixBinaryForm[RepeatedMultisetForm, "\[ThinSpace]"];

(**************************************************************************************************)

PublicTypesettingForm[SetUnionForm, SetIntersectionForm, SetRelativeComplementForm]

DefineInfixForm[SetUnionForm, OpBox @ "\[Union]"]
DefineInfixForm[SetIntersectionForm, OpBox @ "\[Intersection]"]
DefineInfixBinaryForm[SetRelativeComplementForm, OpBox @ "-"]

(**************************************************************************************************)

PublicTypesettingForm[PowerSetForm]

DefineNamedFunctionSymbolForm[
  PowerSetForm -> CaligraphicBox @ "P"
]

(**************************************************************************************************)

PublicTypesettingForm[SetSymbolForm, SetElementSymbolForm]

DefineTaggedForm[{SetSymbolForm, SetElementSymbolForm}];

PublicTypesettingForm[MultisetSymbolForm, MultisetElementSymbolForm]

DefineTaggedForm[{MultisetSymbolForm, MultisetElementSymbolForm}];

PublicTypesettingForm[SignedSetSymbolForm, SignedSetElementSymbolForm]

DefineTaggedForm[{SignedSetSymbolForm, SignedSetElementSymbolForm}]

PublicTypesettingForm[SignedMultisetSymbolForm, SignedMultisetElementSymbolForm]

DefineTaggedForm[{SignedMultisetSymbolForm, SignedMultisetElementSymbolForm}]

(**************************************************************************************************)

PublicTypesettingForm[SubsetsForm, MultisetsForm]

PublicTypesettingForm[SignedSubsetsForm, SignedMultisetsForm]

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

PublicTypesettingForm[MultisetMultiplicityForm, SignedMultisetMultiplicityForm]

DefineInfixBinaryForm[MultisetMultiplicityForm, ToBoxes @ MultisetMultiplicitySymbol];
DefineInfixBinaryForm[SignedMultisetMultiplicityForm, ToBoxes @ SignedMultisetMultiplicitySymbol];

(**************************************************************************************************)

PublicTypesettingForm[BoundMultiplicityFunctionForm, BoundSignedMultiplicityFunctionForm]

DefineUnaryForm[BoundMultiplicityFunctionForm, SuperscriptBox[$1, ToBoxes @ MultisetMultiplicitySymbol]]
DefineUnaryForm[BoundSignedMultiplicityFunctionForm, SuperscriptBox[$1, ToBoxes @ SignedMultisetMultiplicitySymbol]]

(**************************************************************************************************)

PublicTypesettingForm[SetToMultisetForm, MultisetToSetForm]

DefineUnaryForm[SetToMultisetForm, SuperscriptBox[$1, "\[UpArrow]"]]
DefineUnaryForm[MultisetToSetForm, SuperscriptBox[$1, "\[DownArrow]"]]

(**************************************************************************************************)

PublicTypesettingForm[SetConstructorForm, ListConstructorForm, MultisetConstructorForm, AssociativeArrayConstructorForm, CardinalityConstructorForm]

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
    GridBoxSpacings -> {"Columns"->{{0}}, "Rows"->{{0}}}
    (* GridBoxDividers -> {"Columns" -> cspec} *)
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

PublicTypesettingForm[CartesianProductForm, CartesianSumForm, TensorProductForm, DirectSumForm]

DefineInfixForm[CartesianProductForm, OpBox @ "\[Times]"];
DefineInfixForm[CartesianSumForm,     OpBox @  "+"];
DefineInfixForm[TensorProductForm,    OpBox @ "\[CircleTimes]"];
DefineInfixForm[DirectSumForm,        OpBox @ "\[CirclePlus]"];

(**************************************************************************************************)

PublicTypesettingForm[IndexedUnionForm, IndexedIntersectionForm]

DefineLegacyIndexedForm[IndexedUnionForm, KBox["\[Union] ", "\\bigcup"]]
DefineLegacyIndexedForm[IndexedIntersectionForm, KBox["\[Intersection] ", "\\bigcap"]]

(**************************************************************************************************)

PublicTypesettingForm[SetCardinalityForm, MultisetCardinalityForm]

DefineUnaryForm[SetCardinalityForm,      RBox["\[LeftBracketingBar]", RBox["\[ThinSpace]", $1, "\[ThinSpace]"], "\[RightBracketingBar]"]]
DefineUnaryForm[MultisetCardinalityForm, RBox["\[LeftBracketingBar]", RBox["\[ThinSpace]", $1, "\[ThinSpace]"], "\[RightBracketingBar]"]]

