PublicForm[CategorySymbol, CategoryObjectSymbol, CategoryArrowSymbol]

DefineTaggedForm[CategorySymbol]
DefineTaggedForm[CategoryObjectSymbol]
DefineTaggedForm[CategoryArrowSymbol]

(**************************************************************************************************)

PublicForm[SliceCategoryForm, PullbackStarForm]

DefineInfixBinaryForm[SliceCategoryForm, "/"];
DefineUnaryForm[PullbackStarForm, SuperscriptBox[$1, "*"]];

(**************************************************************************************************)

PublicForm[DualObjectForm]

DefineUnaryForm[DualObjectForm, SuperscriptBox[$1, "*"]];

(**************************************************************************************************)

PublicForm[OppositeCategoryForm]

DefineUnaryForm[OppositeCategoryForm, SuperscriptBox[$1, "𝗈𝗉"]];

(**************************************************************************************************)

PublicForm[MonoidalProductForm, TightMonoidalProductForm]

DefineInfixForm[MonoidalProductForm, OpBox @ "\[CircleTimes]"]
DefineInfixForm[TightMonoidalProductForm, "\[CircleTimes]"]

(**************************************************************************************************)

PublicForm[NaryMonoidalFunctionForm]

DefineUnaryForm[NaryMonoidalFunctionForm, SuperscriptBox["\[CircleTimes]", $1]];

DefineStandardTraditionalForm[
  n_NaryMonoidalFunctionForm[args___] :> MakeBoxes @ AppliedForm[n, args]
];

(**************************************************************************************************)

PublicForm[NaturalTransformationSymbol]

DefineTaggedForm[NaturalTransformationSymbol]

DefineStandardTraditionalForm[{
  InverseForm[n_NaturalTransformationSymbol[args___]] :> MakeBoxes[InverseSubscriptForm[n, args]],
  InverseForm[n_NaturalTransformationSymbol][args___] :> MakeBoxes[InverseForm[n[args]]],
  n_NaturalTransformationSymbol[args___] :> MakeBoxes @ Subscript[n, args],
  n_NaturalTransformationSymbol[args___][args2___] :> MakeBoxes @ AppliedForm[Subscript[n, args], args2],
  n_NaturalTransformationSymbol[][args2___] :> MakeBoxes @ AppliedForm[n, args2]
}]

(**************************************************************************************************)

PublicForm[AssociatorForm, LeftUnitorForm, RightUnitorForm, BraidingForm]

$ntFormToSym = <|AssociatorForm -> "\[Alpha]", LeftUnitorForm -> "\[Lambda]", RightUnitorForm -> "\[Rho]", BraidingForm -> "\[Beta]"|>;
$ntP = Alternatives @@ Keys[$ntFormToSym];

DefineStandardTraditionalForm[{
  InverseForm[(h:$ntP)[a__]]  :> MakeBoxes[InverseForm[h][a]],
  InverseForm[h:$ntP][a__]    :> With[{nt = $ntFormToSym[h]}, MakeBoxes[InverseForm[NaturalTransformationSymbol[nt]][a]]],
  AssociatorForm              :> MakeBoxes[NaturalTransformationSymbol["\[Alpha]"]],
  AssociatorForm[a_, b_, c_]  :> MakeBoxes[NaturalTransformationSymbol["\[Alpha]"][a,b,c]],
  LeftUnitorForm              :> MakeBoxes[NaturalTransformationSymbol["\[Lambda]"]],
  LeftUnitorForm[a_]          :> MakeBoxes[NaturalTransformationSymbol["\[Lambda]"][a]],
  RightUnitorForm             :> MakeBoxes[NaturalTransformationSymbol["\[Rho]"]],
  RightUnitorForm[a_]         :> MakeBoxes[NaturalTransformationSymbol["\[Rho]"][a]],
  BraidingForm                :> MakeBoxes[NaturalTransformationSymbol["\[Beta]"]],
  BraidingForm[a_, b_]        :> MakeBoxes[NaturalTransformationSymbol["\[Beta]"][a,b]]
}];

(**************************************************************************************************)

PublicForm[MorphismForm, NamedMorphismForm]

DefineBinaryForm[MorphismForm, RBox[$1, OpBox["\[Rule]"], $2], BoxFunction -> MorphismBox]
DefineTernaryForm[NamedMorphismForm, RBox[$1, OpBox[":"], MorphismBox[$2, $3]]]

(**************************************************************************************************)

PublicForm[ThenForm]

DefineInfixForm[ThenForm, KBox[StyleBox[" ﹔ ", FontFamily -> "KaTeX_AMS"], "﹔"], BoxFunction -> ThenBox];

(**************************************************************************************************)

PublicForm[CatCat, CatSet, CatFinSet, CatGrp, CatMat, CatKVect, CatMon, CatPoly, CatRel, CatRing, CatRMod, CatTop, CatFunct, CatAlg]

DefineSymbolForm @ {
  CatCat      -> "𝖢𝖺𝗍",
  CatSet      -> "𝖲𝖾𝗍",
  CatFinSet   -> "𝖥𝗂𝗇𝖲𝖾𝗍",
  CatGrp      -> "𝖦𝗋𝗉",
  CatMat      -> "𝖬𝖺𝗍",
  CatKVect    -> "𝖪𝖵𝖾𝖼𝗍",
  CatMon      -> "𝖬𝗈𝗇",
  CatPoly     -> "𝖯𝗈𝗅𝗒",
  CatRel      -> "𝖱𝖾𝗅",
  CatRing     -> "𝖱𝗂𝗇𝗀",
  CatRMod     -> "𝖱𝖬𝗈𝖽",
  CatTop      -> "𝖳𝗈𝗉",
  CatFunct    -> "𝖥𝗎𝗇𝖼𝗍",
  CatAlg      -> "𝖠𝗅𝗀"
};

(**************************************************************************************************)

PublicForm[InternalHomSymbol, FunctorPlaceholderSymbol]

DefineInfixBinaryForm[InternalHomSymbol, "\[ThinSpace]⊸\[ThinSpace]"];

DefineSymbolForm[FunctorPlaceholderSymbol -> "\[Dash]"];

(**************************************************************************************************)

PublicForm[FunctorSignatureForm]

FunctorSignatureForm[f_, a_List, b_] :=
  FunctorSignatureForm[f, CartesianProductForm @@ a, b];

FunctionSignatureForm[f_, a_, b_List] :=
  FunctorSignatureForm[f, a, CartesianProductForm @@ b];

DefineTernaryForm[FunctorSignatureForm, RBox[$1, OpBox @ ":", $2, OpBox @ "\[Rule]", $3], KatexMacroName -> "fs"];

(**************************************************************************************************)

PublicForm[DiagonalFunctorForm, LimitFunctorForm, ColimitFunctorForm, LeftKanExtensionForm, RightKanExtensionForm]

DefineUnaryForm[DiagonalFunctorForm, SubscriptBox["\[CapitalDelta]", $1]]
DefineUnaryForm[LimitFunctorForm, SubscriptBox[FunctionBox["lim"], $1]]
DefineUnaryForm[ColimitFunctorForm, SubscriptBox[FunctionBox["colim"], $1]]

DefineUnaryForm[LeftKanExtensionForm, SubscriptBox[FunctionBox["Lan"], $1]]
DefineUnaryForm[RightKanExtensionForm, SubscriptBox[FunctionBox["Ran"], $1]]

(**************************************************************************************************)

PublicForm[FunctorCategoryForm, CompactFunctorCategoryForm]

DefineBinaryForm[FunctorCategoryForm, RBox["[", $1, ",", $2, "]"]]
DefineBinaryForm[CompactFunctorCategoryForm, SuperscriptBox[$2, $1]]

(**************************************************************************************************)

PublicForm[HorizontalCompositionForm, VerticalCompositionForm, DiskCompositionForm, SpacedDiskCompositionForm, TightCompositionForm, CompositionForm]

DefineInfixForm[HorizontalCompositionForm, RaiseBox[Nest[StyleBox[#, Smaller]&, "\[VeryThinSpace]\[EmptySmallCircle]\[VeryThinSpace]", 3], 0.5]];
DefineInfixForm[VerticalCompositionForm, RaiseBox[Nest[StyleBox[#, Smaller]&, "\[VeryThinSpace]\[FilledSmallCircle]\[VeryThinSpace]", 3], 0.5]];

DefineInfixForm[SpacedDiskCompositionForm, "\[ThinSpace]\[SmallCircle]\[ThinSpace]"];
DefineInfixForm[DiskCompositionForm, "\[SmallCircle]"];
DefineInfixForm[TightCompositionForm, "\[NegativeThinSpace]"];
DefineInfixForm[CompositionForm, ""];

(**************************************************************************************************)

PublicForm[IdArrow, OneArrow, HomForm, TightHomForm, ExplicitHomForm]

DefineUnaryForm[IdArrow, SubscriptBox[FunctionBox["id"], $1]]
DefineUnaryForm[OneArrow, SubscriptBox[FunctionBox["1"], $1]]

DefineBinaryForm[HomForm, AppliedBox[FunctionBox["hom"], $1, $2]]
DefineBinaryForm[TightHomForm, TightAppliedBox[FunctionBox["hom"], $1, $2]]

DefineTernaryForm[ExplicitHomForm, AppliedBox[$1, $2, $3]]

(**************************************************************************************************)

PublicForm[CovariantHomFunctorForm, ContravariantHomFunctorForm]

DefineStandardTraditionalForm[{
  CovariantHomFunctorForm[arg_]     :> ToBoxes @ HomForm[arg, FunctorPlaceholderSymbol],
  ContravariantHomFunctorForm[arg_] :> ToBoxes @ HomForm[FunctorPlaceholderSymbol, arg]
}];

(**************************************************************************************************)

PublicForm[CompactCovariantHomFunctorForm, CompactContravariantHomFunctorForm]

DefineUnaryForm[CompactCovariantHomFunctorForm, SubscriptBox[FunctionBox["h"], $1]]
DefineUnaryForm[CompactContravariantHomFunctorForm, SuperscriptBox[FunctionBox["h"], $1]]

(**************************************************************************************************)

PublicForm[FunctorSymbol, LeftFunctorSymbol, RightFunctorSymbol]
PublicVariable[$UseLeftRightArrowFunctors]

SetInitialValue[$UseLeftRightArrowFunctors, True];

DefineTaggedForm[FunctorSymbol]

DefineStandardTraditionalForm[{
  FunctorSymbol["Left"] :> ToBoxes[FunctorSymbol[If[$UseLeftRightArrowFunctors, "\[FilledLeftTriangle]", "L"]]],
  FunctorSymbol["Right"] :> ToBoxes[FunctorSymbol[If[$UseLeftRightArrowFunctors, "\[FilledRightTriangle]", "R"]]],
  fn_FunctorSymbol[args___] :> MakeBoxes[TightAppliedForm[fn, args]],
  LeftFunctorSymbol :> MakeBoxes[FunctorSymbol["Left"]],
  RightFunctorSymbol :> MakeBoxes[FunctorSymbol["Right"]]
}];

(**************************************************************************************************)

PublicForm[ColoredFunctorSymbol]

DefineStandardTraditionalForm[{
  ColoredFunctorSymbol[FunctorSymbol[name_], args___] :> MakeBoxes[ColoredFunctorSymbol[name, args]],
  ColoredFunctorSymbol[name_, col1_, col2_, sz_] :> MakeBoxes[
    ColorGradientForm[
      Style[FunctorSymbol @ name, FontSize -> sz],
      {col1, col2},
      "DilationFactor" -> 1, "CompressionFactor" -> 0.5
    ]
  ],
  cf_ColoredFunctorSymbol[args___] :> NoSpanBox @ ToBoxes @ AppliedForm[cf, args]
}]


(**************************************************************************************************)

PublicForm[ImplicitAppliedForm]

DefineBinaryForm[ImplicitAppliedForm, RBox[$1, $2]]

