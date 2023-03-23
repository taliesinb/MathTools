PublicForm[NamedLieGroup]

DefineStandardTraditionalForm[{
  NamedLieGroup[name_String] :> TBox[name, "NamedLieGroup"],
  NamedLieGroup[name_String, dim_] :> AppliedBox[MakeBoxes @ NamedLieGroup[name], MakeQGBoxes @ dim],
  NamedLieGroup[name_String, dim_, field_] :> AppliedBox[MakeBoxes @ NamedLieGroup[name], MakeQGBoxes @ dim, fieldOrRingBoxes @ field]
}];

DefineTemplateBox[NamedLieGroup, "NamedLieGroup", SansSerifBox[$1], None];

(**************************************************************************************************)

PublicForm[NamedLieAlgebra]

DefineStandardTraditionalForm[{
  NamedLieAlgebra[name_String] :> TBox[name, "NamedLieAlgebra"],
  NamedLieAlgebra[name_String, dim_] :> AppliedBox[MakeBoxes @ NamedLieAlgebra[name], MakeQGBoxes @ dim],
  NamedLieAlgebra[name_String, dim_, field_] :> AppliedBox[MakeBoxes @ NamedLieAlgebra[name], MakeQGBoxes @ dim, fieldOrRingBoxes @ field]
}];

DefineTemplateBox[NamedLieAlgebra, "NamedLieAlgebra", FrakturBox[$1], None];

(**************************************************************************************************)

fieldOrRingBoxes = Case[
  f:fieldsP      := MakeBoxes @ FieldSymbol @ f;
  r:ringsP       := MakeBoxes @ RingSymbol @ r;
  sr:semiringsP  := MakeBoxes @ SemiringSymbol @ sr;
  n_Integer      := MakeBoxes @ FiniteFieldSymbol[n];
  other_         := MakeQGBoxes @ other,
  {
    fieldsP     -> Alternatives[Reals, Complexes, Rationals, "R", "C", "Q", "K"],
    ringsP      -> Alternatives[Integers, "Z"],
    semiringsP  -> Alternatives[Naturals, "N"]
  }
]

(**************************************************************************************************)

PublicForm[GeneralLinearGroupForm, SpecialLinearGroupForm, ProjectiveGeneralLinearGroupForm, ProjectiveSpecialLinearGroupForm, OrthogonalGroupForm, SpecialOrthogonalGroupForm, UnitaryGroupForm, SpecialUnitaryGroupForm, SpinGroupForm, PinGroupForm]
PublicForm[GeneralLinearAlgebraForm, SpecialLinearAlgebraForm, ProjectiveGeneralLinearAlgebraForm, ProjectiveSpecialLinearAlgebraForm, OrthogonalAlgebraForm, SpecialOrthogonalAlgebraForm, UnitaryAlgebraForm, SpecialUnitaryAlgebraForm, SpinAlgebraForm, PinAlgebraForm]

defineLieGroupAlgebra[name_String, groupSym_Symbol, algebraSym_Symbol] := With[
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

PublicForm[GroupSymbol]

DefineTaggedForm[GroupSymbol, Aliases -> <|"Z" -> Integers|>]

(**************************************************************************************************)

PublicForm[SymmetricGroupForm, AlternatingGroupForm, FreeGroupForm, CyclicGroupForm]

DefineUnaryForm[SymmetricGroupForm, AppliedBox[RomanBox @ "Sym", $1]];
DefineUnaryForm[AlternatingGroupForm, AppliedBox[RomanBox @ "Alt", $1]];
DefineUnaryForm[FreeGroupForm, SubscriptBox["F", $1]]
DefineUnaryForm[CyclicGroupForm, SubscriptBox["\[DoubleStruckCapitalZ]", $1]]

(**************************************************************************************************)

PublicForm[GroupDirectProductForm]

DefineInfixForm[GroupDirectProductForm, WideOpBox @ "\[Times]"];

(**************************************************************************************************)

PublicForm[GroupPresentationSymbol]

DefineTaggedForm[GroupPresentationSymbol];

(**************************************************************************************************)

PublicForm[GroupPresentationForm]

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
  s:(symP | _Integer)     := MakeBoxes @ GroupGeneratorSymbol @ s;
  CardinalSymbol[s_]      := MakeBoxes @ GroupGeneratorSymbol @ s;
  e_ ? unaryWrappedQ      := recurseWrapperBoxes[e, %];
  gr_GroupGeneratorSymbol := MakeBoxes @ gr;
,
  symP -> $rawSymbolP
]

(**************************************************************************************************)

PublicForm[GroupRelationForm]

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

PublicForm[GroupGeneratorSymbol, GroupRelatorSymbol]

DefineTaggedForm[{GroupGeneratorSymbol, GroupRelatorSymbol}];

(**************************************************************************************************)

PublicForm[GroupCommutatorForm]

DefineBinaryForm[GroupCommutatorForm, SquareBracketBox[$1, ", ", $2]];

(**************************************************************************************************)

PublicForm[GroupPowerForm]

DefineBinaryForm[GroupPowerForm, SuperscriptBox[$1, $2]];

(**************************************************************************************************)

PublicForm[GroupFunctionSymbol, GroupHomomorphismSymbol]

DefineTaggedForm[{GroupFunctionSymbol, GroupHomomorphismSymbol}];

(**************************************************************************************************)

PublicForm[GroupoidFunctionSymbol, GroupoidHomomorphismSymbol]

DefineTaggedForm[{GroupoidFunctionSymbol, GroupoidHomomorphismSymbol}];

(**************************************************************************************************)

PublicForm[GroupoidSymbol]

DefineTaggedForm[GroupoidSymbol, Aliases -> <|"N" -> Naturals|>];

(**************************************************************************************************)

PublicForm[ActionGroupoidSymbol]

DefineTaggedForm[ActionGroupoidSymbol];

(**************************************************************************************************)

PublicForm[ActionSymbol, SelfActionForm]

(* SelfActionForm was SelfActionSymbol *)

DefineTaggedForm[ActionSymbol];

DefineUnaryForm[SelfActionForm, HatBox[$1]];

(**************************************************************************************************)

PublicForm[GroupElementSymbol, GroupoidElementSymbol]

DefineTaggedForm[{GroupElementSymbol, GroupoidElementSymbol}];

(**************************************************************************************************)

PublicForm[GroupIdentitySymbol, GroupoidIdentitySymbol]

DefineSymbolForm[{GroupIdentitySymbol -> "1", GroupoidIdentitySymbol -> "1"}]
(* What are these for? *)

(**************************************************************************************************)

PublicForm[GroupoidIdentityElement]

DefineUnaryForm[GroupoidIdentityElement, SubscriptBox["1", $1]]

(**************************************************************************************************)

PublicForm[GroupInverseForm]

DefineUnaryForm[GroupInverseForm, InverseBox[$1]]

(**************************************************************************************************)

PublicForm[GroupMultiplicationForm, ImplicitGroupMultiplicationForm, GroupoidMultiplicationForm]

DefineInfixForm[GroupMultiplicationForm, OpBox @ "\[Star]"];
DefineInfixForm[ImplicitGroupMultiplicationForm, "\[VeryThinSpace]"];
DefineInfixForm[GroupoidMultiplicationForm, OpBox @ "\[Star]"];

(**************************************************************************************************)

PublicForm[GroupSmallDotForm, GroupLargeDotForm]

DefineInfixForm[GroupSmallDotForm, KBox[OpBox @ "\[CenterDot]", """\cdot """]];
DefineInfixForm[GroupLargeDotForm, KBox[OpBox @ "\[Bullet]", """\,\raisebox{0.1em}{\tiny∙}\,"""]];

(**************************************************************************************************)

PublicForm[GroupWordRewritingForm]

DefineCommaForm[GroupWordRewritingForm, AngleBracketBox[$1], Boxification -> groupWordRewritingRuleBox]

SetHoldAllComplete[groupWordRewritingRuleBox];
groupWordRewritingRuleBox = Case[
  a_ -> b_ := MakeBoxes @ MapsToForm[a, b];
  other_   := MakeQGBoxes @ other;
];

(**************************************************************************************************)

PublicForm[SemigroupProductForm]

DefineInfixForm[SemigroupProductForm, KBox[OpBox @ "\[SmallCircle]", """\mathbin{\raisebox{0.15em}{\tiny∙}}"""]]

(**************************************************************************************************)

PublicForm[MonoidProductForm]

DefineInfixForm[MonoidProductForm, OpBox @ "\[CenterDot]"]
