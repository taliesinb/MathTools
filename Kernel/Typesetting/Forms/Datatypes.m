PublicForm[TupleForm]

DeclareSequenceTemplateBox[TupleForm, "(", ")"];

(**************************************************************************************************)

PublicForm[SetForm, StyledSetForm]

DeclareSequenceTemplateBox[SetForm, "{", "}"];
DeclareStyledSequenceTemplateBox[StyledSetForm, "{", "}"];

(**************************************************************************************************)

PublicForm[SignedSetForm]

declareCommaRiffledForm[SignedSetForm, "signedSet"];

(**************************************************************************************************)

PublicForm[ListForm, StyledListForm]

declareCommaRiffledForm[ListForm, "list"];
declareStyledCommaRiffledForm[StyledListForm, "styledList"];

(**************************************************************************************************)

PublicForm[MultisetForm, StyledMultisetForm, SignedMultisetForm, StyledSignedMultisetForm]

declareCommaRiffledForm[MultisetForm, "multiset"];
declareCommaRiffledForm[SignedMultisetForm, "signedMultiset"];
declareStyledCommaRiffledForm[StyledMultisetForm, "styledMultiset"];
declareStyledCommaRiffledForm[StyledSignedMultisetForm, "styledSignedMultiset"];

(**************************************************************************************************)

PublicForm[RepeatedMultisetForm]

declareBinaryForm[RepeatedMultisetForm];

(**************************************************************************************************)

PublicForm[SetUnionForm, SetIntersectionForm, SetRelativeComplementForm]

declareInfixSymbol[{SetUnionForm, SetIntersectionForm, SetRelativeComplementForm}];

(**************************************************************************************************)

PublicForm[MultisetUnionForm, MultisetIntersectionForm, MultisetRelativeComplementForm, MultisetSumForm]

declareInfixSymbol[{MultisetUnionForm, MultisetIntersectionForm, MultisetRelativeComplementForm, MultisetSumForm}];

(**************************************************************************************************)

PublicForm[PowerSetForm]

declareUnaryForm[PowerSetForm];

(**************************************************************************************************)

PublicForm[SetSymbolForm, SignedSetSymbolForm, SetElementSymbolForm, SignedSetElementSymbolForm, MultisetSymbolForm, MultisetElementSymbolForm, SignedMultisetSymbolForm, SignedMultisetElementSymbolForm]

declareSymbolFormExplicit[SetSymbolForm];
declareSymbolFormExplicit[MultisetSymbolForm];
declareSymbolFormExplicit[SignedSetSymbolForm];
declareSymbolFormExplicit[SignedMultisetSymbolForm];
declareSymbolFormExplicit[SetElementSymbolForm];
declareSymbolFormExplicit[SignedSetElementSymbolForm];
declareSymbolFormExplicit[MultisetElementSymbolForm];
declareSymbolFormExplicit[SignedMultisetElementSymbolForm];

(**************************************************************************************************)

PublicForm[SubsetsForm, MultisetsForm]

PublicForm[SignedSubsetsForm, SignedMultisetsForm]

declareUnaryForm[SubsetsForm];
declareUnaryForm[MultisetsForm];

declareUnaryForm[SignedSubsetsForm];
declareUnaryForm[SignedMultisetsForm];

(**************************************************************************************************)

PublicForm[MultisetMultiplicityForm, SignedMultisetMultiplicityForm]

declareInfixSymbol[MultisetMultiplicityForm];
declareInfixSymbol[SignedMultisetMultiplicityForm];

(**************************************************************************************************)

PublicForm[BoundMultiplicityFunctionForm, BoundSignedMultiplicityFunctionForm]

declareUnaryForm[BoundMultiplicityFunctionForm]
declareUnaryForm[BoundSignedMultiplicityFunctionForm]

(**************************************************************************************************)

PublicForm[SetToMultisetForm, MultisetToSetForm]

declareUnaryForm[SetToMultisetForm]
declareUnaryForm[MultisetToSetForm]

(**************************************************************************************************)

PublicForm[SetConstructorForm, MultisetConstructorForm, CardinalityConstructorForm]

declareBoxFormatting[
  SetConstructorForm[lhs_, rhs_] :>
    TemplateBox[{toColGrid @ lhs, toColGrid @ rhs}, "SetConstructorForm"],

  MultisetConstructorForm[lhs_, rhs_] :>
    TemplateBox[{toColGrid @ lhs, toColGrid @ rhs}, "MultisetConstructorForm"],

  CardinalityConstructorForm[lhs_, rhs_] :>
    TemplateBox[{toColGrid @ lhs, toColGrid @ rhs}, "CardinalityConstructorForm"]
];

SetHoldAllComplete[toColGrid]

toColGrid = Case[
  {a_}   := MakeQGBoxesOrNull @ a;
  {a__}  := MakeBoxes @ CommaRowForm[a];
  s_SingleColumnForm := MakeQGBoxes @ s;
  a_     := MakeQGBoxesOrNull @ a
];

$TemplateKatexFunction["SetConstructorForm"] = "setConstructor"
$TemplateKatexFunction["MultisetConstructorForm"] = "multisetConstructor"
$TemplateKatexFunction["CardinalityConstructorForm"] = "cardinalityConstructor"

(**************************************************************************************************)

PublicForm[ConstructorForm]

declareBoxFormatting[
  ConstructorForm[lhs_, rhs_] :>
    TemplateBox[{toColGrid @ lhs, toColGrid @ rhs}, "ConstructorForm"]
];

$TemplateKatexFunction["ConstructorForm"] = "constructor"

(**************************************************************************************************)

PublicForm[AssociativeArrayForm]

declareBoxFormatting[
  AssociativeArrayForm[rules__] :>
    TemplateBox[
      MapUnevaluated[assocRuleBox, {rules}],
      "AssociativeArrayForm"
    ]
]

SetHoldAllComplete[assocRuleBox];
assocRuleBox = Case[
  a_ -> b_ := MakeBoxes @ MapsToForm[a, b];
  other_   := MakeQGBoxes @ other;
];

$TemplateKatexFunction["AssociativeArrayForm"] = applyRiffled["assocArray", ","];

(**************************************************************************************************)

PublicForm[CartesianProductForm]

declareInfixSymbol[CartesianProductForm];

(**************************************************************************************************)

PublicForm[IndexedUnionForm, IndexedIntersectionForm]

declareSumLikeFormatting[IndexedUnionForm, "indexUnion"];
declareSumLikeFormatting[IndexedIntersectionForm, "indexIntersection"];

(**************************************************************************************************)

PublicForm[SetCardinalityForm, MultisetCardinalityForm]

declareUnaryForm[SetCardinalityForm];
declareUnaryForm[MultisetCardinalityForm];
