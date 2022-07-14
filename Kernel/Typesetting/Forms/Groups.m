PublicForm[GeneralLinearAlgebraForm, GeneralLinearGroupForm]

declareLieGroupOrAlgebraForm[{GeneralLinearAlgebraForm, GeneralLinearGroupForm}];

PublicForm[SpecialLinearAlgebraForm, SpecialLinearGroupForm]

declareLieGroupOrAlgebraForm[{SpecialLinearAlgebraForm, SpecialLinearGroupForm}];

PublicForm[ProjectiveGeneralLinearAlgebraForm, ProjectiveGeneralLinearGroupForm]

declareLieGroupOrAlgebraForm[{ProjectiveGeneralLinearAlgebraForm, ProjectiveGeneralLinearGroupForm}];

PublicForm[ProjectiveSpecialLinearAlgebraForm, ProjectiveSpecialLinearGroupForm]

declareLieGroupOrAlgebraForm[{ProjectiveSpecialLinearAlgebraForm, ProjectiveSpecialLinearGroupForm}];

PublicForm[OrthogonalAlgebraForm, OrthogonalGroupForm]

declareLieGroupOrAlgebraForm[{OrthogonalAlgebraForm, OrthogonalGroupForm}];

PublicForm[SpecialOrthogonalAlgebraForm, SpecialOrthogonalGroupForm]

declareLieGroupOrAlgebraForm[{SpecialOrthogonalAlgebraForm, SpecialOrthogonalGroupForm}];

PublicForm[UnitaryAlgebraForm, UnitaryGroupForm]

declareLieGroupOrAlgebraForm[{UnitaryAlgebraForm, UnitaryGroupForm}];

PublicForm[SpecialUnitaryAlgebraForm, SpecialUnitaryGroupForm]

declareLieGroupOrAlgebraForm[{SpecialUnitaryAlgebraForm, SpecialUnitaryGroupForm}];

PublicForm[SpinAlgebraForm, SpinGroupForm]

declareLieGroupOrAlgebraForm[{SpinAlgebraForm, SpinGroupForm}];

PublicForm[PinAlgebraForm, PinGroupForm]

declareLieGroupOrAlgebraForm[{PinAlgebraForm, PinGroupForm}];

PublicForm[SymmetricGroupForm]

declareUnaryForm[SymmetricGroupForm];

(**************************************************************************************************)

PublicForm[GroupSymbol]

GroupSymbol[] := GroupSymbol["G"];

declareAlgebraicSymbol[GroupSymbol, $groupoidAliases];

(**************************************************************************************************)

PublicForm[FreeGroupForm]

declareUnaryForm[FreeGroupForm];

(**************************************************************************************************)

PublicForm[CyclicGroupForm]

declareBoxFormatting[
  CyclicGroupForm[n_] :> makeTemplateBox[n, "CyclicGroupForm"]
]

$TemplateKatexFunction["CyclicGroupForm"] = "cyclicGroup";

(**************************************************************************************************)

PublicForm[GroupDirectProductForm]

declareInfixSymbol[GroupDirectProductForm, GroupSymbol, True];

(**************************************************************************************************)

PublicForm[GroupPresentationSymbol]

declareSymbolForm[GroupPresentationSymbol] // usingCustomKatex["presentation"];

(**************************************************************************************************)

PublicForm[GroupPresentationForm, GroupRelationForm, GroupGeneratorSymbol, GroupRelatorSymbol]

declareBoxFormatting[
  GroupPresentationForm[lhs_, rhs_] :>
    TemplateBox[
      {groupGeneratorBoxes @ lhs,
       groupRelationSetBoxes @ rhs},
      "GroupPresentationForm"
    ]
];

SetHoldAllComplete[groupGeneratorBoxes, groupRelationSetBoxes, groupRelationBoxes];

groupRelationSetBoxes = Case[
  {}         := MakeBoxes @ EmptySetSymbol;
  list_List  := TemplateBox[MapUnevaluated[groupRelationBoxes, list], "CommaRowForm"];
  relation_  := groupRelationBoxes @ relation;
]

groupRelationBoxes = Case[
  EqualForm[a_, b_]     := MakeBoxes @ GroupRelationForm[a, b];
  gr_GroupRelationForm  := MakeBoxes @ gr;
  r_GroupRelatorSymbol  := MakeBoxes @ r;
  a_                    := groupRelationTermBoxes @ a;
];

$TemplateKatexFunction["GroupPresentationForm"] = "groupPresentation"

groupGeneratorBoxes = Case[
  list_List               := TemplateBox[MapUnevaluated[%, list], "CommaRowForm"];
  s:(symP | _Integer)     := MakeBoxes @ GroupGeneratorSymbol @ s;
  CardinalSymbol[s_]      := MakeBoxes @ GroupGeneratorSymbol @ s;
  e_ ? unaryWrappedQ      := recurseWrapperBoxes[e, %];
  gr_GroupGeneratorSymbol := MakeBoxes @ gr;
,
  symP -> $rawSymbolP
]

declareSymbolForm[GroupGeneratorSymbol];
declareSymbolForm[GroupRelatorSymbol];

declareBoxFormatting[
  GroupRelationForm[a_, b_] :>
    TemplateBox[
      MapUnevaluated[groupRelationTermBoxes, {a, b}],
      "GroupRelationForm"
    ],
  GroupRelationForm[a_] :>
    TemplateBox[List @ groupRelationTermBoxes @ a, "GroupRelationForm"],
  GroupRelationForm[] :>
    SBox["GroupRelationSymbol"]
]

SetHoldAllComplete[groupRelationTermBoxes];

groupRelationTermBoxes = Case[
  list_List                   := TemplateBox[MapUnevaluated[%, list], "ImplicitGroupMultiplicationForm"];
  (Power|PowerForm|GroupPowerForm)[g_, e_]  := TemplateBox[{% @ g, MakeQGBoxes @ e}, "GroupPowerForm"];
  1                           := MakeBoxes @ GroupElementSymbol["e"];
  s:symP                      := MakeBoxes @ GroupElementSymbol @ s;
  GroupInverseForm[e_]        := TemplateBox[List @ % @ e, "GroupInverseForm"];
  CardinalSymbol[s_]          := MakeBoxes @ GroupElementSymbol @ s;
  e_ ? unaryWrappedQ          := recurseWrapperBoxes[e, %] /. "InvertedForm" -> "GroupInverseForm";
  ge_GroupElementSymbol       := MakeBoxes @ ge;
  ge_GroupIdentitySymbol      := MakeBoxes @ ge;
  gg_GroupGeneratorSymbol     := MakeBoxes @ gg;
  gm_GroupMultiplicationForm  := MakeBoxes @ gm;
  gm_ImplicitGroupMultiplicationForm  := MakeBoxes @ gm;
  GroupCommutatorForm[a_, b_] := TemplateBox[{% @ a, % @ b}, "GroupCommutatorForm"];
,
  symP -> $rawSymbolP
];

declareInfixKatexAlias["GroupRelation", "groupRelationIso"];

(**************************************************************************************************)

$TemplateKatexFunction["IdentityElementForm"] = "identityElement"

(**************************************************************************************************)

PublicForm[GroupCommutatorForm]

declareBinaryForm[GroupCommutatorForm];

(**************************************************************************************************)

PublicForm[GroupPowerForm]

declareBinaryForm[GroupPowerForm];

(**************************************************************************************************)

PublicForm[GroupFunctionSymbol, GroupHomomorphismSymbol]

GroupFunctionSymbol[] := GroupoidFunctionSymbol["\[Pi]"]

declareSymbolForm[GroupFunctionSymbol];
declareSymbolForm[GroupHomomorphismSymbol];

(**************************************************************************************************)

PublicForm[GroupoidFunctionSymbol, GroupoidHomomorphismSymbol]

GroupoidFunctionSymbol[] := GroupoidFunctionSymbol["\[Mu]"]

declareSymbolForm[GroupoidFunctionSymbol];
declareSymbolForm[GroupoidHomomorphismSymbol];

(**************************************************************************************************)

PublicForm[GroupoidSymbol]

$groupoidAliases = <|
  "N" -> "Naturals",
  "C" -> "Complexes",
  "R" -> "Reals",
  "Z" -> "Integers",
  "Q" -> "Rationals"
|>

GroupoidSymbol["\[Gamma]"] := PathGroupoidSymbol["Q"];

declareAlgebraicSymbol[GroupoidSymbol, $groupoidAliases];

(**************************************************************************************************)

PublicForm[ActionGroupoidSymbol]

declareSymbolForm[ActionGroupoidSymbol, ActionSymbol];

(**************************************************************************************************)

PublicForm[ActionSymbol, SelfActionSymbol]

declareSymbolForm[ActionSymbol];
declareSymbolForm[SelfActionSymbol, GroupSymbol];

(**************************************************************************************************)

PublicForm[GroupElementSymbol, GroupoidElementSymbol]

GroupElementSymbol[] := GroupElementSymbol["g"];
GroupoidElementSymbol[] := GroupoidElementSymbol["g"];

declareSymbolForm[GroupElementSymbol];
declareSymbolForm[GroupoidElementSymbol];

(**************************************************************************************************)

PublicForm[GroupIdentitySymbol, GroupoidIdentitySymbol]

GroupIdentitySymbol[] := GroupIdentitySymbol["e"];
GroupoidIdentitySymbol[] := GroupoidIdentitySymbol["e"];

declareSymbolForm[GroupIdentitySymbol];
declareSymbolForm[GroupoidIdentitySymbol];

(**************************************************************************************************)

PublicForm[GroupoidIdentityElement]

declareBinaryForm[GroupoidIdentityElement]

GroupoidIdentityElement[e_] := GroupoidIdentityElement["1", e];

(**************************************************************************************************)

PublicForm[GroupInverseForm, GroupoidInverseForm]

declareUnaryForm[GroupInverseForm, maybeParen[GroupElementSymbol|GroupGeneratorSymbol]];
declareUnaryForm[GroupoidInverseForm, maybeParen[GroupoidElementSymbol]];

(**************************************************************************************************)

PublicForm[GroupMultiplicationForm, ImplicitGroupMultiplicationForm, GroupoidMultiplicationForm]

$grouplikeTerms = Alternatives[
  GroupElementSymbol, GroupElementSymbol, GroupIdentitySymbol, GroupGeneratorSymbol, GroupPowerForm, GroupInverseForm, PathSymbol, TupleForm,
  RingElementSymbol, RingBasisElementForm, WordRingElementSymbol, WordRingBasisElementForm
];

$groupoidlikeTerms = Alternatives[
  GroupoidElementSymbol, GroupoidIdentitySymbol, GroupIdentitySymbol, GroupElementSymbol, GroupGeneratorSymbol, GroupPowerForm, GroupInverseForm, PathSymbol, TupleForm
];

declareInfixSymbol[GroupMultiplicationForm, maybeParen @ $grouplikeTerms] // usingCustomKatex["Gmult"];
declareInfixSymbol[ImplicitGroupMultiplicationForm, maybeParen @ $grouplikeTerms] // usingCustomKatex["iGmult"];
declareInfixSymbol[GroupoidMultiplicationForm, maybeParen @ $groupoidlikeTerms] // usingCustomKatex["gmult"];

(**************************************************************************************************)

PublicForm[GroupSmallDotForm, GroupLargeDotForm]

declareInfixSymbol[GroupSmallDotForm, maybeParen @ $grouplikeTerms] // usingCustomKatex["gdot"];
declareInfixSymbol[GroupLargeDotForm, maybeParen @ $grouplikeTerms] // usingCustomKatex["gDot"];

(**************************************************************************************************)

PublicForm[GroupWordRewritingForm]

declareBoxFormatting[
  GroupWordRewritingForm[args__] :>
    TemplateBox[
      MapUnevaluated[groupWordRewritingRuleBox, {args}],
      "GroupWordRewritingForm"
  ]
];

SetHoldAllComplete[groupWordRewritingRuleBox];
groupWordRewritingRuleBox = Case[
  a_ -> b_ := MakeBoxes @ RewritingRuleForm[a, b];
  other_   := MakeQGBoxes @ other;
];

$TemplateKatexFunction["GroupWordRewritingForm"] = applyRiffled["groupWordRewriting", ","];

(**************************************************************************************************)

PublicForm[SemigroupProductForm]

declareInfixSymbol[SemigroupProductForm] // usingCustomKatex["sgdot"];

(**************************************************************************************************)

PublicForm[MonoidProductForm]

declareInfixSymbol[MonoidProductForm] // usingCustomKatex["mdot"];
