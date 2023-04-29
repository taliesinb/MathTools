SystemSymbol[Naturals, PositiveNaturals, Primes]

DefineSymbolForm[{
  Naturals         -> KBox["\[DoubleStruckCapitalN]", "\\N"],
  Reals            -> KBox["\[DoubleStruckCapitalR]", "\\R"],
  Integers         -> KBox["\[DoubleStruckCapitalZ]", "\\Z"],
  Complexes        -> KBox["\[DoubleStruckCapitalC]", "\\Complex"],
  Rationals        -> KBox["\[DoubleStruckCapitalQ]", "\\mathbb{Q}"],
  Primes           -> "\[DoubleStruckCapitalP]",
  PositiveNaturals -> SuperscriptBox[KBox["\[DoubleStruckCapitalN]", "\\N"], "+"],
  PositiveIntegers -> SuperscriptBox[KBox["\[DoubleStruckCapitalZ]", "\\Z"], "+"],
  PositiveReals    -> SuperscriptBox[KBox["\[DoubleStruckCapitalR]", "\\R"], "+"]
}];

(**************************************************************************************************)

PublicSymbol[PiSymbol, TauSymbol]

DefineSymbolForm[{
  PiSymbol                   -> "\[Pi]",
  TauSymbol                  -> "\[Tau]"
}];

(**************************************************************************************************)

PublicForm[ConditionedForm]

DefineStandardTraditionalForm[{
  ConditionedForm[(i_IndexedForm)[a_], cond_] :> MakeBoxes[i[a, cond]],
  ConditionedForm[a_, b_] :> MakeBoxes[SuchThatForm[a, b]]
}];

(**************************************************************************************************)

PublicForm[SumForm, ProductForm]

DefineLegacyIndexedForm[SumForm, "\[Sum] "];
DefineLegacyIndexedForm[ProductForm, "\[Product] "];

(**************************************************************************************************)

PublicForm[TimesForm, PlusForm]

DefineInfixForm[PlusForm,  OpBox @ "+"]
DefineInfixForm[TimesForm, OpBox @ "\[Times]"]

(**************************************************************************************************)

PublicForm[MinusForm, SubtractForm, DivideForm, InlineDivideForm]

DefineInfixBinaryForm[SubtractForm, OpBox @ "-"];
DefineUnaryForm[MinusForm, RBox["\[Minus]\!", $1]];
DefineBinaryForm[DivideForm, KBox[FractionBox[$1, $2], "frac"[$1, $2]]];
DefineBinaryForm[InlineDivideForm, RBox[$1, KBox[OpBox @ "/", "/"], $2]]

PublicForm[MinusOneForm, PlusOneForm]

DefineUnaryForm[MinusOneForm, RBox[$1, OpBox @ "\[Minus]", "1"]];
DefineUnaryForm[PlusOneForm, RBox[$1, OpBox @ "+", "1"]];

(**************************************************************************************************)

PublicForm[FloorForm, CeilingForm]

DefineUnaryForm[FloorForm, LeftRightBox["\[LeftFloor]", $1, "\[RightFloor]"]];
DefineUnaryForm[CeilingForm, LeftRightBox["\[LeftCeiling]", $1, "\[RightCeiling]"]];

(**************************************************************************************************)

PublicForm[ImplicitTimesForm, SpacedImplicitTimesForm]

DefineInfixForm[ImplicitTimesForm,       KBox["\[VeryThinSpace]", "\\,"]];
DefineInfixForm[SpacedImplicitTimesForm, KBox["\[VeryThinSpace] ", "\\;\\,"]];

(**************************************************************************************************)

PublicForm[LimitForm]

DefineBinaryForm[LimitForm, RBox[SubscriptBox[FunctionBox @ "lim", $2], " ", $1]]

(**************************************************************************************************)

PublicForm[DividesForm]

DefineInfixForm[DividesForm, WideOpBox @ "\[Divides]"];

(**************************************************************************************************)

PublicForm[InverseForm]

DefineUnaryForm[InverseForm, InverseBox @ $1];

(**************************************************************************************************)

PublicForm[ClosedIntervalForm, OpenIntervalForm, ClosedOpenIntervalForm, OpenClosedIntervalForm]

DefineBinaryForm[ClosedIntervalForm,     RBox["[", $1, ",", " ", $2, "]"]];
DefineBinaryForm[OpenIntervalForm,       RBox["(", $1, ",", " ", $2, ")"]];
DefineBinaryForm[ClosedOpenIntervalForm, RBox["[", $1, ",", " ", $2, ")"]];
DefineBinaryForm[OpenClosedIntervalForm, RBox["(", $1, ",", " ", $2, "]"]];

(**************************************************************************************************)

PublicForm[IntegerRangeForm]

DefineBinaryForm[IntegerRangeForm, RBox[$1, "\[InvisibleSpace]..", $2]];

(**************************************************************************************************)

PublicForm[HomomorphismMappingForm]

DefineCommaForm[HomomorphismMappingForm, RBox[LeftBox @ "\[LeftAngleBracket]", $wlThinSpace, $1, $wlThinSpace, RightBox @ "\[RightAngleBracket]"]];

DefineRuleAsMapsTo[HomomorphismMappingForm];

(**************************************************************************************************)

PublicForm[FactorialForm, PowerForm]

DefineUnaryForm[FactorialForm, RBox[$1, "!"]]
DefineBinaryForm[PowerForm, SuperscriptBox[$1, $2]]

(**************************************************************************************************)

PublicForm[KroneckerDeltaForm]

DefineCommaForm[KroneckerDeltaForm, SubscriptBox["\[Delta]", $1], HeadBoxes -> "\[Delta]"]



