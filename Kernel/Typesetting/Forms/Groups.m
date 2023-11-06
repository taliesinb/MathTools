PublicTypesettingForm[NamedLieGroup]

DefineStandardTraditionalForm[{
  NamedLieGroup[name_Str] :> TBox[name, "NamedLieGroup"],
  NamedLieGroup[name_Str, dim_] :> AppliedBox[MakeBoxes @ NamedLieGroup[name], MakeQGBoxes @ dim],
  NamedLieGroup[name_Str, dim_, field_] :> AppliedBox[MakeBoxes @ NamedLieGroup[name], MakeQGBoxes @ dim, fieldOrRingBoxes @ field]
}];

DefineTemplateBox[NamedLieGroup, "NamedLieGroup", SansSerifBox[$1], None];

(**************************************************************************************************)

PublicTypesettingForm[NamedLieAlgebra]

DefineStandardTraditionalForm[{
  NamedLieAlgebra[name_Str] :> TBox[name, "NamedLieAlgebra"],
  NamedLieAlgebra[name_Str, dim_] :> AppliedBox[MakeBoxes @ NamedLieAlgebra[name], MakeQGBoxes @ dim],
  NamedLieAlgebra[name_Str, dim_, field_] :> AppliedBox[MakeBoxes @ NamedLieAlgebra[name], MakeQGBoxes @ dim, fieldOrRingBoxes @ field]
}];

DefineTemplateBox[NamedLieAlgebra, "NamedLieAlgebra", FrakturBox[$1], None];

(**************************************************************************************************)

fieldOrRingBoxes = Case[
  f:fieldsP      := MakeBoxes @ FieldSymbol @ f;
  r:ringsP       := MakeBoxes @ RingSymbol @ r;
  sr:semiringsP  := MakeBoxes @ SemiringSymbol @ sr;
  n_Int          := MakeBoxes @ FiniteFieldSymbol[n];
  other_         := MakeQGBoxes @ other,
  {
    fieldsP     -> Alternatives[Reals, Complexes, Rationals, "R", "C", "Q", "K"],
    ringsP      -> Alternatives[Integers, "Z"],
    semiringsP  -> Alternatives[Naturals, "N"]
  }
]

(**************************************************************************************************)

PublicTypesettingForm[GeneralLinearGroupForm, SpecialLinearGroupForm, ProjectiveGeneralLinearGroupForm, ProjectiveSpecialLinearGroupForm, OrthogonalGroupForm, SpecialOrthogonalGroupForm, UnitaryGroupForm, SpecialUnitaryGroupForm, SpinGroupForm, PinGroupForm]
PublicTypesettingForm[GeneralLinearAlgebraForm, SpecialLinearAlgebraForm, ProjectiveGeneralLinearAlgebraForm, ProjectiveSpecialLinearAlgebraForm, OrthogonalAlgebraForm, SpecialOrthogonalAlgebraForm, UnitaryAlgebraForm, SpecialUnitaryAlgebraForm, SpinAlgebraForm, PinAlgebraForm]

defineLieGroupAlgebra[name_Str, groupSym_Symbol, algebraSym_Symbol] := With[
  {algebraName = ToLowerCase @ name},
  DefineStandardTraditionalForm[{
    groupSym[args___] :>   MakeBoxes @ NamedLieGroup[name, args],
    algebraSym[args___] :> MakeBoxes @ NamedLieAlgebra[algebraName, args]
  }];
];

_defineLieGroupAlgebra := BadArguments[];

defineLieGroupAlgebra @@@ ExpressionTable[
  "GL"    GeneralLinearGroupForm              GeneralLinearAlgebraForm
  "SL"    SpecialLinearGroupForm              SpecialLinearAlgebraForm
  "PGL"   ProjectiveGeneralLinearGroupForm    ProjectiveGeneralLinearAlgebraForm
  "PSL"   ProjectiveSpecialLinearGroupForm    ProjectiveSpecialLinearAlgebraForm
  "O"     OrthogonalGroupForm                 OrthogonalAlgebraForm
  "SO"    SpecialOrthogonalGroupForm          SpecialOrthogonalAlgebraForm
  "U"     UnitaryGroupForm                    UnitaryAlgebraForm
  "SU"    SpecialUnitaryGroupForm             SpecialUnitaryAlgebraForm
  "Spin"  SpinGroupForm                       SpinAlgebraForm
  "Pin"   PinGroupForm                        PinAlgebraForm
]

(**************************************************************************************************)

PublicTypesettingForm[GroupSymbol]

DefineTaggedForm[GroupSymbol, Aliases -> <|"Z" -> Integers|>]

(**************************************************************************************************)

PublicTypesettingForm[SymmetricGroupForm, AlternatingGroupForm, FreeGroupForm, CyclicGroupForm]

DefineUnaryForm[SymmetricGroupForm, AppliedBox[RomanBox @ "Sym", $1]];
DefineUnaryForm[AlternatingGroupForm, AppliedBox[RomanBox @ "Alt", $1]];
DefineUnaryForm[FreeGroupForm, SubscriptBox["F", $1]]
DefineUnaryForm[CyclicGroupForm, SubscriptBox["\[DoubleStruckCapitalZ]", $1]]

(**************************************************************************************************)

PublicTypesettingForm[GroupDirectProductForm]

DefineInfixForm[GroupDirectProductForm, WideOpBox @ "\[Times]"];

(**************************************************************************************************)

PublicTypesettingForm[GroupPresentationSymbol]

DefineTaggedForm[GroupPresentationSymbol];

(**************************************************************************************************)

PublicTypesettingForm[GroupPresentationForm]

DefineBinaryForm[GroupPresentationForm, AngleBracketBox[$1, "\[MediumSpace]", $PipeBox, "\[MediumSpace]", $2]]

DefineStandardTraditionalForm[
  GroupPresentationForm[lhs_, rhs_] :>
    TBox[
      groupGeneratorBoxes @ lhs,
      groupRelationSetBoxes @ rhs,
      "GroupPresentationForm"
    ]
];

(* ^ override the ruels set up by DefineBinaryForm *)

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

groupGeneratorBoxes = Case[
  list_List               := TemplateBox[MapUnevaluated[%, list], "CommaRowForm"];
  s:(symP | _Int)     := MakeBoxes @ GroupGeneratorSymbol @ s;
  CardinalSymbol[s_]      := MakeBoxes @ GroupGeneratorSymbol @ s;
  e_ ? unaryWrappedQ      := recurseWrapperBoxes[e, %];
  gr_GroupGeneratorSymbol := MakeBoxes @ gr;
,
  symP -> $rawSymbolP
]

(**************************************************************************************************)

PublicTypesettingForm[GroupRelationForm]

DefineInfixBinaryForm[GroupRelationForm, OpBox["="], TemplateName -> "BinaryGroupRelationForm", Boxification -> groupRelationTermBoxes]
DefineTaggedForm[GroupRelationForm, TemplateName -> "UnaryGroupRelationForm", Boxification -> groupRelationTermBoxes]

SetHoldAllComplete[groupRelationTermBoxes];

groupRelationTermBoxes = Case[
  list_List                                := TemplateBox[MapUnevaluated[%, list], "ImplicitGroupMultiplicationForm"];
  (Power|PowerForm|GroupPowerForm)[g_, e_] := TemplateBox[{% @ g, MakeQGBoxes @ e}, "GroupPowerForm"];
  1                                        := MakeBoxes @ GroupElementSymbol["e"];
  s:symP                                   := MakeBoxes @ GroupElementSymbol @ s;
  GroupInverseForm[e_]                     := TemplateBox[List @ % @ e, "GroupInverseForm"];
  CardinalSymbol[s_]                       := MakeBoxes @ GroupElementSymbol @ s;
  e_ ? unaryWrappedQ                       := recurseWrapperBoxes[e, %] /. "InvertedForm" -> "GroupInverseForm";
  ge_GroupElementSymbol                    := MakeBoxes @ ge;
  ge_GroupIdentitySymbol                   := MakeBoxes @ ge;
  gg_GroupGeneratorSymbol                  := MakeBoxes @ gg;
  gm_GroupMultiplicationForm               := MakeBoxes @ gm;
  gm_ImplicitGroupMultiplicationForm       := MakeBoxes @ gm;
  GroupCommutatorForm[a_, b_]              := TemplateBox[{% @ a, % @ b}, "GroupCommutatorForm"];
,
  symP -> $rawSymbolP
];

(**************************************************************************************************)

PublicTypesettingForm[GroupGeneratorSymbol, GroupRelatorSymbol]

DefineTaggedForm[{GroupGeneratorSymbol, GroupRelatorSymbol}];

(**************************************************************************************************)

PublicTypesettingForm[GroupCommutatorForm]

DefineBinaryForm[GroupCommutatorForm, SquareBracketBox[$1, ", ", $2]];

(**************************************************************************************************)

PublicTypesettingForm[GroupPowerForm]

DefineBinaryForm[GroupPowerForm, SuperscriptBox[$1, $2]];

(**************************************************************************************************)

PublicTypesettingForm[GroupFunctionSymbol, GroupHomomorphismSymbol]

DefineTaggedForm[{GroupFunctionSymbol, GroupHomomorphismSymbol}];

(**************************************************************************************************)

PublicTypesettingForm[GroupoidFunctionSymbol, GroupoidHomomorphismSymbol]

DefineTaggedForm[{GroupoidFunctionSymbol, GroupoidHomomorphismSymbol}];

(**************************************************************************************************)

PublicTypesettingForm[GroupoidSymbol]

DefineTaggedForm[GroupoidSymbol, Aliases -> <|"N" -> Naturals|>];

(**************************************************************************************************)

PublicTypesettingForm[ActionGroupoidSymbol]

DefineTaggedForm[ActionGroupoidSymbol];

(**************************************************************************************************)

PublicTypesettingForm[ActionSymbol, SelfActionForm]

(* SelfActionForm was SelfActionSymbol *)

DefineTaggedForm[ActionSymbol];

DefineUnaryForm[SelfActionForm, HatBox[$1]];

(**************************************************************************************************)

PublicTypesettingForm[GroupElementSymbol, GroupoidElementSymbol]

DefineTaggedForm[{GroupElementSymbol, GroupoidElementSymbol}];

(**************************************************************************************************)

PublicTypesettingForm[GroupIdentitySymbol, GroupoidIdentitySymbol]

DefineSymbolForm[{GroupIdentitySymbol -> "1", GroupoidIdentitySymbol -> "1"}]
(* What are these for? *)

(**************************************************************************************************)

PublicTypesettingForm[GroupoidIdentityElement]

DefineUnaryForm[GroupoidIdentityElement, SubscriptBox["1", $1]]

(**************************************************************************************************)

PublicTypesettingForm[GroupInverseForm]

DefineUnaryForm[GroupInverseForm, InverseBox[$1]]

(**************************************************************************************************)

PublicTypesettingForm[GroupMultiplicationForm, ImplicitGroupMultiplicationForm, GroupoidMultiplicationForm]

DefineInfixForm[GroupMultiplicationForm, OpBox @ "\[Star]"];
DefineInfixForm[ImplicitGroupMultiplicationForm, "\[VeryThinSpace]"];
DefineInfixForm[GroupoidMultiplicationForm, OpBox @ "\[Star]"];

(**************************************************************************************************)

PublicTypesettingForm[GroupSmallDotForm, GroupLargeDotForm]

DefineInfixForm[GroupSmallDotForm, KBox[OpBox @ "\[CenterDot]", """\cdot """]];
DefineInfixForm[GroupLargeDotForm, KBox[OpBox @ "\[Bullet]", """\,\raisebox{0.1em}{\tiny∙}\,"""]];

(**************************************************************************************************)

PublicTypesettingForm[GroupWordRewritingForm]

DefineCommaForm[GroupWordRewritingForm, AngleBracketBox[$1], Boxification -> groupWordRewritingRuleBox]

SetHoldAllComplete[groupWordRewritingRuleBox];
groupWordRewritingRuleBox = Case[
  a_ -> b_ := MakeBoxes @ MapsToForm[a, b];
  other_   := MakeQGBoxes @ other;
];

(**************************************************************************************************)

PublicTypesettingForm[SemigroupProductForm]

DefineInfixForm[SemigroupProductForm, KBox[OpBox @ "\[SmallCircle]", """\mathbin{\raisebox{0.15em}{\tiny∙}}"""]]

(**************************************************************************************************)

PublicTypesettingForm[MonoidProductForm]

DefineInfixForm[MonoidProductForm, OpBox @ "\[CenterDot]"]
