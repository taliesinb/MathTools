PublicForm[FiniteFieldSymbol]

DefineUnaryForm[FiniteFieldSymbol, SubscriptBox["\[DoubleStruckCapitalF]", $1]];

(**************************************************************************************************)

PublicForm[BaseFieldSymbol]

DefineTaggedForm[BaseFieldSymbol];

(**************************************************************************************************)

PublicForm[FieldSymbol]

DefineTaggedForm[FieldSymbol, Aliases -> <|
  "C" -> Complexes,
  "R" -> Reals,
  "Q" -> Rationals
|>];

(**************************************************************************************************)

PublicForm[LinearCombinationCoefficientSymbol]

DefineTaggedForm[LinearCombinationCoefficientSymbol];