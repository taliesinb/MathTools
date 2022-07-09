SystemSymbol[Naturals, PositiveNaturals, Primes]

(* SetUsage @ "Naturals represents the natural numbers."
SetUsage @ "PositiveNaturals represents the positive natural numbers."
SetUsage @ "Primes represents the prime numbers."
 *)
DeclareTemplateBoxRules[
  Naturals                   -> "\[DoubleStruckN]",
  PositiveNaturals           -> SuperscriptBox["\[DoubleStruckCapitalN]", "+"],
  PositiveReals              -> SuperscriptBox["\[DoubleStruckCapitalR]", "+"],
  Primes                     -> "\[DoubleStruckCapitalP]",
  Reals                      -> "\[DoubleStruckCapitalR]"
]

(**************************************************************************************************)

PublicSymbol[PiSymbol, TauSymbol]

DeclareTemplateBoxRules[
  PiSymbol                   -> "\[Pi]",
  TauSymbol                  -> "\[Tau]"
];

(**************************************************************************************************)

PublicForm[IndexedMaxForm, IndexedMinForm]

declareSumLikeFormatting[IndexedMaxForm, "indexMax"];
declareSumLikeFormatting[IndexedMinForm, "indexMin"];

(**************************************************************************************************)

PublicForm[ConditionedForm]

declareBoxFormatting[
  ConditionedForm[SumForm[a_, i_], cond_] :> MakeBoxes @ SumForm[a, i, cond]
];

(**************************************************************************************************)

PublicForm[SumForm, ProductForm]

declareSumLikeFormatting[SumForm, "indexSum"];
declareSumLikeFormatting[ProductForm, "indexProd"];

(**************************************************************************************************)

PublicForm[PlusForm]

declareInfixSymbol[PlusForm] // usingCustomKatex[" + "];

(**************************************************************************************************)

PublicForm[SubtractForm]

 declareInfixSymbol[SubtractForm] // usingCustomKatex[" - "];

(**************************************************************************************************)

PublicForm[MinusForm]

declareUnaryForm[MinusForm];

(**************************************************************************************************)

PublicForm[TimesForm]

declareInfixSymbol[TimesForm] // usingCustomKatex["times"];

(**************************************************************************************************)

PublicForm[ParenthesesForm, SpacedParenthesesForm]

declareCommaRiffledForm[ParenthesesForm, "paren"];
declareCommaRiffledForm[SpacedParenthesesForm, "paren"];

(**************************************************************************************************)

PublicForm[NoParenthesesForm]

declareBoxFormatting[
  NoParenthesesForm[e_] :> makeQGBoxes @ e
];

(**************************************************************************************************)

PublicForm[CeilingForm, FloorForm]

declareUnaryForm[CeilingForm]
declareUnaryForm[FloorForm]

(**************************************************************************************************)

PublicForm[DivideForm, InlineDivideForm]

declareBinaryForm[DivideForm] // usingCustomKatex["frac"];
declareInfixSymbol[InlineDivideForm] // usingCustomKatex[" / "];

(**************************************************************************************************)

PublicForm[ImplicitTimesForm, SpacedImplicitTimesForm]

declareInfixSymbol[ImplicitTimesForm] // usingCustomKatex[" \\, "];
declareInfixSymbol[SpacedImplicitTimesForm] // usingCustomKatex[" \\; "];

(**************************************************************************************************)

PublicForm[LimitForm]

declareBinaryForm[LimitForm];

(**************************************************************************************************)

PublicForm[DividesForm]

declareInfixSymbol[DividesForm];

(**************************************************************************************************)

PublicForm[InverseForm]

declareUnaryForm[InverseForm];

(**************************************************************************************************)

PublicForm[ClosedIntervalForm, OpenIntervalForm, ClosedOpenIntervalForm, OpenClosedIntervalForm]

declareBinaryForm[ClosedIntervalForm];
declareBinaryForm[OpenIntervalForm];
declareBinaryForm[ClosedOpenIntervalForm];
declareBinaryForm[OpenClosedIntervalForm];

(**************************************************************************************************)

PublicForm[IntegerRangeForm]

declareBoxFormatting[
  IntegerRangeForm[1, n_] :> makeTemplateBox[n, "OneToNForm"],
  IntegerRangeForm[0, n_] :> makeTemplateBox[n, "ZeroToNForm"]
]

$TemplateKatexFunction["OneToNForm"] = "oneTo"
$TemplateKatexFunction["ZeroToNForm"] = "zeroTo"

(**************************************************************************************************)

PublicForm[HomomorphismMappingForm]

declareBoxFormatting[
  HomomorphismMappingForm[rules__] :>
    TemplateBox[
      MapUnevaluated[assocRuleBox, {rules}],
      "HomomorphismMappingForm"
    ]
]

SetHoldAllComplete[assocRuleBox];
assocRuleBox = Case[
  a_ -> b_ := MakeBoxes @ MapsToForm[a, b];
  other_   := makeQGBoxes @ other;
];

$TemplateKatexFunction["HomomorphismMappingForm"] = applyRiffled["homomorphismMapping", ","];

(**************************************************************************************************)

PublicForm[FactorialForm, PowerForm]

declareUnaryForm[FactorialForm];
declareBinaryForm[PowerForm];

(**************************************************************************************************)

PublicForm[KroneckerDeltaForm]
PublicSymbol[KroneckerDeltaSymbol]

declareNAryForm[KroneckerDeltaForm, KroneckerDeltaSymbol, ","];



