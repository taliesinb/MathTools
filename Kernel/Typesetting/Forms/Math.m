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

PublicTypesettingForm[ConditionedForm]

DefineStandardTraditionalForm[{
  ConditionedForm[(i_IndexedForm)[a_], cond_] :> MakeBoxes[i[a, cond]],
  ConditionedForm[a_, b_] :> MakeBoxes[SuchThatForm[a, b]]
}];

(**************************************************************************************************)

PublicTypesettingForm[SumForm, ProductForm]

DefineLegacyIndexedForm[SumForm, "\[Sum] "];
DefineLegacyIndexedForm[ProductForm, "\[Product] "];

(**************************************************************************************************)

PublicTypesettingForm[TimesForm, PlusForm]

DefineInfixForm[PlusForm,  OpBox @ "+"]
DefineInfixForm[TimesForm, OpBox @ "\[Times]"]

(**************************************************************************************************)

PublicTypesettingForm[MinusForm, SubtractForm, DivideForm, InlineDivideForm]

DefineInfixBinaryForm[SubtractForm, OpBox @ "-"];
DefineUnaryForm[MinusForm, RBox["\[Minus]\!", $1]];
DefineBinaryForm[DivideForm, KBox[FractionBox[$1, $2], "frac"[$1, $2]]];
DefineBinaryForm[InlineDivideForm, RBox[$1, KBox[OpBox @ "/", "/"], $2]]

PublicTypesettingForm[MinusOneForm, PlusOneForm]

DefineUnaryForm[MinusOneForm, RBox[$1, OpBox @ "\[Minus]", "1"]];
DefineUnaryForm[PlusOneForm, RBox[$1, OpBox @ "+", "1"]];

(**************************************************************************************************)

PublicTypesettingForm[FloorForm, CeilingForm]

DefineUnaryForm[FloorForm, LeftRightBox["\[LeftFloor]", $1, "\[RightFloor]"]];
DefineUnaryForm[CeilingForm, LeftRightBox["\[LeftCeiling]", $1, "\[RightCeiling]"]];

(**************************************************************************************************)

PublicTypesettingForm[ImplicitTimesForm, SpacedImplicitTimesForm]

DefineInfixForm[ImplicitTimesForm,       KBox["\[VeryThinSpace]", "\\,"]];
DefineInfixForm[SpacedImplicitTimesForm, KBox["\[VeryThinSpace] ", "\\;\\,"]];

(**************************************************************************************************)

PublicTypesettingForm[LimitForm]

DefineBinaryForm[LimitForm, RBox[SubscriptBox[FunctionBox @ "lim", $2], " ", $1]]

(**************************************************************************************************)

PublicTypesettingForm[DividesForm]

DefineInfixForm[DividesForm, WideOpBox @ "\[Divides]"];

(**************************************************************************************************)

PublicTypesettingForm[InverseForm]

DefineUnaryForm[InverseForm, InverseBox @ $1];

(**************************************************************************************************)

PublicTypesettingForm[InverseSubscriptForm]

DefineNAryForm[InverseSubscriptForm, SubsuperscriptBox[$1, RiffledBox[","][$$2], RBox["-", "1"]]];

(**************************************************************************************************)

PublicTypesettingForm[ClosedIntervalForm, OpenIntervalForm, ClosedOpenIntervalForm, OpenClosedIntervalForm]

DefineBinaryForm[ClosedIntervalForm,     RBox["[", $1, ",", " ", $2, "]"]];
DefineBinaryForm[OpenIntervalForm,       RBox["(", $1, ",", " ", $2, ")"]];
DefineBinaryForm[ClosedOpenIntervalForm, RBox["[", $1, ",", " ", $2, ")"]];
DefineBinaryForm[OpenClosedIntervalForm, RBox["(", $1, ",", " ", $2, "]"]];

(**************************************************************************************************)

PublicTypesettingForm[IntegerRangeForm]

DefineBinaryForm[IntegerRangeForm, RBox[$1, "\[InvisibleSpace]..", $2]];

(**************************************************************************************************)

PublicTypesettingForm[HomomorphismMappingForm]

DefineCommaForm[HomomorphismMappingForm, RBox[LeftBox @ "\[LeftAngleBracket]", $wlThinSpace, $1, $wlThinSpace, RightBox @ "\[RightAngleBracket]"]];

DefineRuleAsMapsTo[HomomorphismMappingForm];

(**************************************************************************************************)

PublicTypesettingForm[FactorialForm, PowerForm]

DefineUnaryForm[FactorialForm, RBox[$1, "!"]]
DefineBinaryForm[PowerForm, SuperscriptBox[$1, $2]]

(**************************************************************************************************)

PublicTypesettingForm[KroneckerDeltaForm]

DefineCommaForm[KroneckerDeltaForm, SubscriptBox["\[Delta]", $1], HeadBoxes -> "\[Delta]"]



