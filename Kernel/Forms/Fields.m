PublicTypesettingForm[FiniteFieldSymbol]

DefineUnaryForm[FiniteFieldSymbol, SubscriptBox["\[DoubleStruckCapitalF]", $1]];

(**************************************************************************************************)

PublicTypesettingForm[BaseFieldSymbol]

DefineTaggedForm[BaseFieldSymbol];

(**************************************************************************************************)

PublicTypesettingForm[FieldSymbol]

DefineTaggedForm[FieldSymbol, Aliases -> <|
  "C" -> Complexes,
  "R" -> Reals,
  "Q" -> Rationals
|>];

(**************************************************************************************************)

PublicTypesettingForm[LinearCombinationCoefficientSymbol]

DefineTaggedForm[LinearCombinationCoefficientSymbol];

(**************************************************************************************************)

PublicTypesettingForm[RealVectorSpaceForm, ComplexVectorSpaceForm]

DefineTaggedForm[RealVectorSpaceForm]
DefineTaggedForm[ComplexVectorSpaceForm]
