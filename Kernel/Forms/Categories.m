PublicTypesettingForm[CategorySymbol, CategoryObjectSymbol, CategoryArrowSymbol]

DefineTaggedForm[CategorySymbol]
DefineTaggedForm[CategoryObjectSymbol]
DefineTaggedForm[CategoryArrowSymbol]

(**************************************************************************************************)

PublicTypesettingForm[SliceCategoryForm, PullbackStarForm]

DefineInfixBinaryForm[SliceCategoryForm, "/"];
DefineUnaryForm[PullbackStarForm, SuperscriptBox[$1, "*"]];

(**************************************************************************************************)

PublicTypesettingForm[DualObjectForm]

DefineUnaryForm[DualObjectForm, SuperscriptBox[$1, "*"]];

(**************************************************************************************************)

PublicTypesettingForm[OppositeCategoryForm]

DefineUnaryForm[OppositeCategoryForm, SuperscriptBox[$1, "𝗈𝗉"]];

(**************************************************************************************************)

PublicTypesettingForm[MonoidalProductForm, TightMonoidalProductForm]

DefineInfixForm[MonoidalProductForm, OpBox @ "\[CircleTimes]"]
DefineInfixForm[TightMonoidalProductForm, "\[CircleTimes]"]

(**************************************************************************************************)

PublicTypesettingForm[NaryMonoidalFunctionForm]

DefineUnaryForm[NaryMonoidalFunctionForm, SuperscriptBox["\[CircleTimes]", $1]];

DefineStandardTraditionalForm[
  n_NaryMonoidalFunctionForm[args___] :> MakeBoxes @ AppliedForm[n, args]
];

(**************************************************************************************************)

PublicTypesettingForm[NaturalTransformationSymbol]

DefineTaggedForm[NaturalTransformationSymbol]

DefineStandardTraditionalForm[{
  InverseForm[n_NaturalTransformationSymbol[args___]] :> MakeBoxes[InverseSubscriptForm[n, args]],
  InverseForm[n_NaturalTransformationSymbol][args___] :> MakeBoxes[InverseForm[n[args]]],
  n_NaturalTransformationSymbol[args___] :> MakeBoxes @ Subscript[n, args],
  n_NaturalTransformationSymbol[args___][args2___] :> MakeBoxes @ AppliedForm[Subscript[n, args], args2],
  n_NaturalTransformationSymbol[][args2___] :> MakeBoxes @ AppliedForm[n, args2]
}]

(**************************************************************************************************)

PublicTypesettingForm[SrcFunction, SourceFunction, TargetFunction, TgtFunction, DomainFunction, CodomainFunction, IdentityFunction]

DefineNamedFunctionSymbolForm[{
  DomainFunction -> "dom",
  CodomainFunction -> "cod",
  IdentityFunction -> "id",
  SrcFunction -> "src", SourceFunction,
  TgtFunction -> "tgt", TargetFunction
}]

(**************************************************************************************************)

PublicTypesettingForm[AssociatorForm, LeftUnitorForm, RightUnitorForm, BraidingForm]

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

PublicTypesettingForm[MorphismForm, NamedMorphismForm]

DefineBinaryForm[MorphismForm, RBox[$1, OpBox["\[Rule]"], $2], BoxFunction -> MorphismBox]
DefineTernaryForm[NamedMorphismForm, RBox[$1, OpBox[":"], MorphismBox[$2, $3]]]

(**************************************************************************************************)

PublicTypesettingForm[ThenForm]

DefineInfixForm[ThenForm, KBox[StyleBox[" ﹔ ", FontFamily -> "KaTeX_AMS"], "﹔"], BoxFunction -> ThenBox];

(**************************************************************************************************)

PublicTypesettingForm[CatCat, CatSet, CatFinSet, CatGrp, CatMat, CatKVect, CatMon, CatPoly, CatRel, CatRing, CatRMod, CatTop, CatFunct, CatAlg]

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

PublicTypesettingForm[InternalHomSymbol, FunctorPlaceholderSymbol]

DefineInfixBinaryForm[InternalHomSymbol, "\[ThinSpace]⊸\[ThinSpace]"];

DefineSymbolForm[FunctorPlaceholderSymbol -> "\[Dash]"];

(**************************************************************************************************)

PublicTypesettingForm[FunctorSignatureForm]

FunctorSignatureForm[f_, a_List, b_] :=
  FunctorSignatureForm[f, CartesianProductForm @@ a, b];

FunctionSignatureForm[f_, a_, b_List] :=
  FunctorSignatureForm[f, a, CartesianProductForm @@ b];

DefineTernaryForm[FunctorSignatureForm, RBox[$1, OpBox @ ":", $2, OpBox @ "\[Rule]", $3], KatexMacroName -> "fs"];

(**************************************************************************************************)

PublicTypesettingForm[FunctorAppliedForm]
PublicVariable[$FunctorAppliedForm]

SetInitialValue[$FunctorAppliedForm, ImplicitAppliedForm];

DefineStandardTraditionalForm[{
  FunctorAppliedForm[head_, arg_] :> ToBoxes @ $FunctorAppliedForm[head, arg],
  FunctorAppliedForm[head_, args__] :> MakeBoxes @ TightAppliedForm[head, args]
}];

$functorLikeQ = UAssoc[];

SetListable[declareFunctorLike];
declareFunctorLike[sym_Symbol] := (
  $functorLikeQ[sym] = True;
  DefineStandardTraditionalForm[head_sym[args__] :> ToBoxes @ FunctorAppliedForm[head, args]]
);

(**************************************************************************************************)

PublicTypesettingForm[FunctorSymbol]

DefineTaggedForm[FunctorSymbol]

DefineStandardTraditionalForm[f_FunctorSymbol[g_FunctorSymbol[x_]] :> ToBoxes @ FunctorAppliedForm[TightCompositionForm[f, g], x]]

declareFunctorLike[FunctorSymbol]

(**************************************************************************************************)

PublicTypesettingForm[FunctorPowerForm]

DefineStandardTraditionalForm[
  FunctorPowerForm[f_, n_] :> ToBoxes @ PowerForm[f, n]
];

declareFunctorLike[FunctorPowerForm];

(**************************************************************************************************)

PublicTypesettingForm[DiagonalFunctorForm, LimitFunctorForm, ColimitFunctorForm, LeftKanExtensionForm, RightKanExtensionForm]

DefineUnaryForm[DiagonalFunctorForm, SubscriptBox["\[CapitalDelta]", $1]]
DefineUnaryForm[LimitFunctorForm, SubscriptBox[FunctionBox["lim"], $1]]
DefineUnaryForm[ColimitFunctorForm, SubscriptBox[FunctionBox["colim"], $1]]

DefineUnaryForm[LeftKanExtensionForm, SubscriptBox[FunctionBox["Lan"], $1]]
DefineUnaryForm[RightKanExtensionForm, SubscriptBox[FunctionBox["Ran"], $1]]

declareFunctorLike[{DiagonalFunctorForm, LimitFunctorForm, ColimitFunctorForm, LeftKanExtensionForm, RightKanExtensionForm}];

(**************************************************************************************************)

PublicTypesettingForm[FunctorCategoryForm, CompactFunctorCategoryForm]

DefineBinaryForm[FunctorCategoryForm, RBox["[", $1, ",", $2, "]"]]
DefineBinaryForm[CompactFunctorCategoryForm, SuperscriptBox[$2, $1]]

(**************************************************************************************************)

PublicTypesettingForm[HorizontalCompositionForm, VerticalCompositionForm, DiskCompositionForm, SpacedDiskCompositionForm, SpacedCompositionForm, TightCompositionForm, CompositionForm]

(* we can use Application *)
DefineInfixForm[HorizontalCompositionForm, RaiseBox[Nest[StyleBox[#, Smaller]&, "\[VeryThinSpace]\[EmptySmallCircle]\[VeryThinSpace]", 3], 0.5]];
DefineInfixForm[VerticalCompositionForm, RaiseBox[Nest[StyleBox[#, Smaller]&, "\[VeryThinSpace]\[FilledSmallCircle]\[VeryThinSpace]", 3], 0.5]];

DefineInfixForm[SpacedCompositionForm, "\[VeryThinSpace]"]
DefineInfixForm[SpacedDiskCompositionForm, "\[ThinSpace]\[SmallCircle]\[ThinSpace]"];
DefineInfixForm[DiskCompositionForm, "\[SmallCircle]"];
DefineInfixForm[TightCompositionForm, "\[NegativeThinSpace]"];
DefineInfixForm[CompositionForm, ""];

(**************************************************************************************************)

PublicTypesettingForm[IdArrow, OneArrow, HomForm, TightHomForm, CompactHomForm, ExplicitHomForm]

DefineUnaryForm[IdArrow, SubscriptBox[FunctionBox["id"], $1]]
DefineUnaryForm[OneArrow, SubscriptBox[FunctionBox["1"], $1]]

DefineBinaryForm[HomForm, NoSpanBox @ AppliedBox[FunctionBox["hom"], $1, $2]]
DefineBinaryForm[TightHomForm, NoSpanBox @ TightAppliedBox[FunctionBox["hom"], $1, $2]]
DefineBinaryForm[CompactHomForm, GridBox[
  {{ItemBox[FunctionBox["h"], Alignment -> Center], SmallerBox @ SmallerBox @ SmallerBox @ $1}, {"\[SpanFromAbove]", SmallerBox @ SmallerBox @ SmallerBox @ $2}},
  RowSpacings -> -0.1, ColumnSpacings -> .1, RowAlignments->Bottom, GridFrameMargins -> {{0, 0}, {0, 0}}, RowMinHeight -> 0.5]];

DefineTernaryForm[ExplicitHomForm, NoSpanBox @ TightAppliedBox[$1, $2, $3]]

(**************************************************************************************************)

PublicTypesettingForm[CovariantHomFunctorForm, ContravariantHomFunctorForm]

DefineStandardTraditionalForm[{
  CovariantHomFunctorForm[arg_]     :> ToBoxes @ HomForm[arg, FunctorPlaceholderSymbol],
  ContravariantHomFunctorForm[arg_] :> ToBoxes @ HomForm[FunctorPlaceholderSymbol, arg]
}];

(**************************************************************************************************)

PublicTypesettingForm[CompactHomForm, CompactCovariantHomFunctorForm, CompactContravariantHomFunctorForm]

DefineUnaryForm[CompactCovariantHomFunctorForm, SuperscriptBox[FunctionBox["h"], $1]]
DefineUnaryForm[CompactContravariantHomFunctorForm, SubscriptBox[FunctionBox["h"], AdjustmentBox[$1, BoxMargins -> {{.1, 0}, {0, 0}}]]]

DefineStandardTraditionalForm[{
  c_CompactCovariantHomFunctorForm[arg_] :> NoSpanBox @ MakeBoxes[AppliedForm[c, arg]],
  c_CompactContravariantHomFunctorForm[arg_] :> NoSpanBox @ MakeBoxes[AppliedForm[c, arg]]
}]

(**************************************************************************************************)

PublicTypesettingForm[ImplicitAppliedForm]

DefineBinaryForm[ImplicitAppliedForm, RBox[$1, $2]]

(**************************************************************************************************)

PublicTypesettingForm[MonoidalTreeForm]

DefineStandardTraditionalForm[{MonoidalTreeForm[e_, opts___Rule] :> monoidalTreeFormBoxes[e, opts]}];

monoidalTreeFormBoxes[cd_CommutativeDiagram, ___] :=
  ToBoxes @ Append[cd, TextModifiers -> <|
    "Objects"   -> MonoidalTreeForm,
    "Morphisms" -> Fn[label, MonoidalTreeForm[label, GraphicsScale -> 10, VertexSize -> 4]]
  |>]

monoidalTreeFormBoxes[e_, opts___Rule] :=
  ToBoxes @ MonoidalTree[e, opts];

(**************************************************************************************************)

PublicFunction[MonoidalTree]

MonoidalTree[e_, opts___Rule] := Scope[
  $epilog = {};
  treeList = toMonoidalTreeList[e, {}];
  RainbowTree[
    treeList,
    AdditionalVertexLayoutOptions -> {
      BendRadius -> 0.3,
      FanoutStyle -> Top,
      LayerDepths -> 1,
      FilterOptions[TreeVertexLayout, opts]
    },
    Epilog -> $epilog,
    opts,
    GraphicsScale -> 15
  ]
];

(**************************************************************************************************)

toMonoidalTreeList = Case[

  Seq[(MonoidalProductForm|TightMonoidalProductForm)[args__], pos_] :=
    MapIndexStack[%, pos, {args}];

  Seq[color:($ColorPattern | _Int), _] := color;

  Seq[(OneArrow|IdentityArrow)[e_], pos_] := %[e, pos];

  Seq[(CategoryObjectSymbol|CategoryArrowSymbol|NaturalTransformationSymbol)[sym_Str], _] :=
    ToRainbowInteger @ sym;

  Seq[AssociatorForm[a_, b_, c_], pos_] := (
    addAssociatorDecoration[pos, False];
    MapIndexStack[%, pos, {a, b, c}]
  );

 Seq[InverseForm[AssociatorForm[a_, b_, c_]], pos_] := (
    addAssociatorDecoration[pos, True];
    MapIndexStack[%, pos, {a, b, c}]
  );

 Seq[BraidingForm[a_, b_], pos_] := (
    addBraidingDecoration[pos];
    MapIndexStack[%, pos, {a, b}]
  );

 Seq[LeftUnitorForm[a_], pos_] := (
    addUnitorDecoration[pos, 1];
    MapIndexStack[%, pos, {-1, a}]
  );

 Seq[RightUnitorForm[a_], pos_] := (
    addUnitorDecoration[pos, 2];
    MapIndexStack[%, pos, {a, -1}]
  );

  Seq[other_, pos_] := (Message[MonoidalTree::badleaf, MsgExpr @ other, pos]; Null)
];

MonoidalTree::badleaf = "Unrecognized leaf expression `` at position ``.";

(**************************************************************************************************)

PublicTypesettingForm[RainbowCategoryForm]

(* TODO: retire this, since DiagramColorRules is a much more precise mechanism for this *)

DefineStandardTraditionalForm[
  RainbowCategoryForm[form_] :> rainbowCategoryFormBoxes[form]
];

$rainbowCDOptions = {ColorRules -> "GradientArrows", TextModifiers -> ObjectArrowIconForm};
rainbowCategoryFormBoxes[form_] := ToBoxes[form /. {
  cd_CommutativeDiagram                :> RuleCondition @ ReplaceOptions[cd, $rainbowCDOptions],
  (* TODO: remove these, they are outdated *)
  CategoryObjectSymbol[s_]             :> symbolToRainbowSymbol[s],
  CategoryArrowSymbol[s_]              :> symbolToRainbowSymbol[s]
}];

toGradientMorphism[thing_, f_, g_] :=
  GradientSymbol[thing, ToRainbowInteger @ f, ToRainbowInteger @ g, 16];

toGradientMorphism[_CategoryArrowSymbol, f_, g_] :=
  GradientSymbol["\[RightArrow]", ToRainbowInteger @ f, ToRainbowInteger @ g, 16];

symbolToRainbowSymbol["I"|"1"] := "\[EmptyCircle]";
symbolToRainbowSymbol[s_] := Style["\[FilledCircle]", ToRainbowColor @ ToRainbowInteger @ s]

(**************************************************************************************************)

addUnitorDecoration[pos_, i_] := AppendTo[$epilog,
  Line[
    GraphicsValue[{"VertexCoordinates", {Append[pos, i]}}, P1 /* Fn[{{-.2, .4} + #, {.2, .4} + #}]]
  ]
];

(* TODO: use NamedIcon instead here *)
addAssociatorDecoration[pos_, isRev_] := AppendTo[$epilog, {
  associatorArrowhead[Append[pos, If[isRev, 3, 1]],  {0, 1}/4],
  associatorArrowhead[Append[pos, If[isRev, 1, 3]], -{0, 1}/4]
}];

associatorArrowhead[pos_, dir_] := Arrowhead[
  GraphicsValue[{"VertexCoordinates", {pos}}, P1 /* PlusOperator[{0, .5}]], dir,
  ArrowheadColor -> $Black, ArrowheadShape -> "Arrow", ArrowheadAnchor -> Center
];

addBraidingDecoration[vertex_] := AppendTo[$epilog,
  Text["\[LeftRightArrow]", GraphicsValue[{"VertexCoordinates", {vertex}}, P1], {-0.1, 1}, BaseStyle -> {FontSize -> 9}]
];

(**************************************************************************************************)

PublicTypesettingForm[ReversedChainForm]

SetUsage @ "ReversedChainForm[form$] indicates that form$ should compose in reverse order."

DefineStandardTraditionalForm[ReversedChainForm[form_] :> TagBox[ToBoxes[form], "ReverseChain"]];

(**************************************************************************************************)

PublicTypesettingForm[ObjectArrowIconForm, CategoryFunctorIconForm]

SetUsage @ "ObjectArrowIconForm[expr$] depicts all %CategoryObjectSymbols as disks and %CategoryArrowSymbols as arrows, preserving colors."
SetUsage @ "CategoryFunctorIconForm[expr$] depicts all %CategorySymbols as disks and %CategoryFunctorSymbols as arrows, preserving colors."

DefineStandardTraditionalForm[{
  ObjectArrowIconForm[expr_] :> ToBoxes @ ReplaceAll[expr, {
    c_CommutativeDiagram :> ReplaceOptions[c, TextModifiers -> ObjectArrowIconForm],
    c_CategoryObjectSymbol :> replaceFormContents[c, "\[FilledCircle]"],
    c_CategoryArrowSymbol :> replaceFormContents[c, ReversedChainForm @ BoldRightArrowSymbol]
  }],
  CategoryFunctorIconForm[expr_] :> ToBoxes @ ReplaceAll[expr, {
    c_CommutativeDiagram :> ReplaceOptions[c, TextModifiers -> CategoryFunctorIconForm],
    c_CategorySymbol :> replaceFormContents[c, "\[FilledCircle]"],
    c_FunctorSymbol :> replaceFormContents[c, ReversedChainForm @ BoldRightArrowSymbol]
  }]
}];

replaceFormContents[(tag_Symbol ? $taggedFormHeadQ)[sub_], new_] := tag[replaceFormContents[sub, new]];
replaceFormContents[(mod_Symbol ? $styleFormHeadQ)[sub_, s___], new_] := mod[replaceFormContents[sub, new], s];
replaceFormContents[(mod_Symbol ? $modifierFormHeadQ)[sub_, s___], new_] := replaceFormContents[sub, new];
replaceFormContents[_, new_] := new;

