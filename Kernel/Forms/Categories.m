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

PublicTypesettingForm[FilledCircleMonoidalProductForm, EmptyCircleMonoidalProductForm]

DefineInfixForm[FilledCircleMonoidalProductForm, OpBox @ "\[FilledCircle]"]
DefineInfixForm[EmptyCircleMonoidalProductForm, OpBox @ "\[EmptyCircle]"]

PublicTypesettingForm[ColoredCircleMonoidalProductForm]

DefineStandardTraditionalForm[{
  ColoredCircleMonoidalProductForm[color_] :> StyleBox[OpBox @ "\[FilledCircle]", color],
  cmp_ColoredCircleMonoidalProductForm[args___] :> RiffledBox[ToBoxes @ cmp][MakeMathBoxSequence[args]]
}];

(**************************************************************************************************)

PublicTypesettingForm[ColoredCircleMonoidalProductModifierForm]

DefineStandardTraditionalForm[{
  ColoredCircleMonoidalProductModifierForm[e_, White] :>
    ToBoxes @ rewriteMonoidalProductTo[EmptyCircleMonoidalProductForm] @ e,
  ColoredCircleMonoidalProductModifierForm[e_, Black] :>
    ToBoxes @ rewriteMonoidalProductTo[FilledCircleMonoidalProductForm] @ e,
  ColoredCircleMonoidalProductModifierForm[e_, color_] :>
   ToBoxes @ rewriteMonoidalProductTo[ColoredCircleMonoidalProductForm[color]] @ e
}];

rewriteMonoidalProductTo[new_] := RepAll[{e_ColoredCircleMonoidalProductModifierForm -> e, MonoidalProductForm -> new}];

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
$ntP = Alt @@ Keys[$ntFormToSym];

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

PublicTypesettingForm[CatCat, CatPred, CatSet, CatFinSet, CatFinOrd, CatOrd, CatGrp, CatMat, CatKVect, CatMon, CatPoly, CatRel, CatRing, CatRMod, CatTop, CatFunct, CatAlg, CatHask, CatArray, CatRainbowArray]

(*
$LowercaseSanSerifLetters = "𝖺𝖻𝖼𝖽𝖾𝖿𝗀𝗁𝗂𝗃𝗄𝗅𝗆𝗇𝗈𝗉𝗊𝗋𝗌𝗍𝗎𝗏𝗐𝗑𝗒𝗓";
$UppercaseSanSerifLetters = "𝖠𝖡𝖢𝖣𝖤𝖥𝖦𝖧𝖨𝖩𝖪𝖫𝖬𝖭𝖮𝖯𝖰𝖱𝖲𝖳𝖴𝖵𝖶𝖷𝖸𝖹";
*)
DefineSymbolForm @ {
  CatCat          -> "𝖢𝖺𝗍",
  CatPred         -> "𝖯𝗋𝖾𝖽",
  CatSet          -> "𝖲𝖾𝗍",
  CatFinSet       -> "𝖥𝗂𝗇𝖲𝖾𝗍",
  CatFinOrd       -> "𝖥𝗂𝗇𝖮𝗋𝖽",
  CatGrp          -> "𝖦𝗋𝗉",
  CatMat          -> "𝖬𝖺𝗍",
  CatKVect        -> "𝖪𝖵𝖾𝖼𝗍",
  CatMon          -> "𝖬𝗈𝗇",
  CatPoly         -> "𝖯𝗈𝗅𝗒",
  CatRel          -> "𝖱𝖾𝗅",
  CatRing         -> "𝖱𝗂𝗇𝗀",
  CatRMod         -> "𝖱𝖬𝗈𝖽",
  CatTop          -> "𝖳𝗈𝗉",
  CatFunct        -> "𝖥𝗎𝗇𝖼𝗍",
  CatAlg          -> "𝖠𝗅𝗀",
  CatHask         -> "𝖧𝖺𝗌𝗄",
  CatArray        -> "𝖠𝗋𝗋",
  CatRainbowArray -> "𝖱𝖠𝗋𝗋"
};

(**************************************************************************************************)

PublicTypesettingForm[CategoryObjectsForm, CategoryArrowsForm]

DefineUnaryForm[CategoryObjectsForm, SubscriptBox[$1, "0"]];
DefineUnaryForm[CategoryArrowsForm, SubscriptBox[$1, "1"]];

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

DefineTernaryForm[FunctorSignatureForm, RBox[$1, OpBox @ ":", $2, OpBox @ "\[Rule]", $3]];

(**************************************************************************************************)

PublicTypesettingForm[ArrowSignatureForm]

DefineTernaryForm[ArrowSignatureForm, RBox[$1, OpBox @ ":", $2, OpBox @ "\[Rule]", $3]];

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

PublicTypesettingForm[DiscreteCategoryForm, SkeletonCategoryForm]

DefineNamedSansSerifFunctionSymbolForm[{
  DiscreteCategoryForm -> "Disc",
  SkeletonCategoryForm -> "Skel"
}];

(**************************************************************************************************)

PublicTypesettingForm[MonoidalUnitSymbol]

DefineSymbolForm[MonoidalUnitSymbol -> "I"]

(**************************************************************************************************)

PublicTypesettingForm[RelativeToCategoryForm]

DefineBinaryForm[RelativeToCategoryForm, SubscriptBox[$1, $2]]

(**************************************************************************************************)

PublicTypesettingForm[OrdinalForm]

DefineUnaryForm[OrdinalForm, UnderlinedBox @ $1]

(**************************************************************************************************)

PublicTypesettingForm[HorizontalCompositionForm, VerticalCompositionForm, DiskCompositionForm, SpacedDiskCompositionForm, SpacedCompositionForm, TightCompositionForm, CompositionForm]

(* we can use Application *)
DefineInfixForm[HorizontalCompositionForm, RaiseBox[Nest[StyleBox[#, Smaller]&, "\[MediumSpace]\[EmptySmallCircle]\[MediumSpace]", 3], 0.5]];
DefineInfixForm[VerticalCompositionForm, RaiseBox[Nest[StyleBox[#, Smaller]&, "\[MediumSpace]\[FilledSmallCircle]\[MediumSpace]", 3], 0.5]];

DefineInfixForm[SpacedCompositionForm, "\[VeryThinSpace]"]
DefineInfixForm[SpacedDiskCompositionForm, "\[ThinSpace]\[SmallCircle]\[ThinSpace]"];
DefineInfixForm[DiskCompositionForm, "\[VeryThinSpace]\[SmallCircle]\[VeryThinSpace]"];
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

PublicTypesettingForm[MonoidalFunctorUnitSymbol, MonoidalFunctorMultiplicationForm]

DefineSymbolForm[MonoidalFunctorUnitSymbol -> "\[Phi]"]

DefineStandardTraditionalForm[{
  MonoidalFunctorMultiplicationForm :> MakeMathBoxes[NaturalTransformationSymbol["\[Phi]"]],
  MonoidalFunctorMultiplicationForm[a_, b_] :> MakeMathBoxes[NaturalTransformationSymbol["\[Phi]"][a,b]]
}];

(**************************************************************************************************)

PublicTypesettingForm[MonoidalTreeForm]

DefineStandardTraditionalForm[{MonoidalTreeForm[e_, opts___Rule] :> monoidalTreeFormBoxes[e, opts]}];

monoidalTreeFormBoxes[cd_CommutativeDiagram, opts___Rule] := ModuleScope[
  labelFontSize = Lookup[{opts}, LabelFontSize, LookupOption[cd, LabelFontSize]];
  fontSize = Lookup[{opts}, FontSize, LookupOption[cd, FontSize]];
  SetAutomatic[fontSize, LookupOption[CommutativeDiagram, FontSize]];
  SetAutomatic[labelFontSize, LookupOption[CommutativeDiagram, LabelFontSize]];
  SetScaledFactor[labelFontSize, fontSize];
  ToBoxes @ App[cd, TextModifiers -> <|
    "Objects"   -> Fn[label, MonoidalTreeForm[label, FontSize -> fontSize]],
    "Morphisms" -> Fn[label, MonoidalTreeForm[label, FontSize -> labelFontSize]]
  |>]
];

monoidalTreeFormBoxes[e_, opts___Rule] := Scope[
  fontSize = Lookup[{opts}, FontSize, Auto];
  If[NumberQ[fontSize], opts = Sequence[opts, GraphicsScale -> fontSize, VertexSize -> Ceiling[fontSize / 3 + 0.5]]];
  ToBoxes @ MonoidalTree[e, opts]
];

(**************************************************************************************************)

PublicFunction[MonoidalTree]

MonoidalTree[e_, opts___Rule] := Scope[
  $epilog = {}; $monoidalProductColor = None;
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
    FilterOptions[opts],
    GraphicsScale -> 15,
    PlotRangePadding -> If[ContainsQ[$epilog, BoundingRectangle], .3, 0]
  ]
];

(**************************************************************************************************)

toMonoidalTreeList = Case[

  Seq[(MonoidalProductForm|TightMonoidalProductForm)[args__], pos_] := (
    addMonoidalProductDecoration[pos, $monoidalProductColor];
    MapIndexStack[%, pos, {args}]
  );

  Seq[FunctorSymbol[sym_][arg_], pos_] := (
    addFunctorDecoration[pos, sym];
    %[arg, pos]
  );

  Seq[ParenthesesForm[a_], pos_] := %[a, pos];

  Seq[(h:FilledCircleMonoidalProductForm|EmptyCircleMonoidalProductForm)[args__], pos_] := (
    addMonoidalProductDecoration[pos, If[h === FilledCircleMonoidalProductForm, $Black, $White]];
    MapIndexStack[%, pos, {args}]
  );

  Seq[ColoredCircleMonoidalProductModifierForm[arg_, color_], pos_] := (
    $monoidalProductColor = color;
    %[arg, pos]
  );

  Seq[MonoidalFunctorUnitSymbol, pos_] := (
    addPhiDecoration1[pos];
    -1
  );

  Seq[MonoidalFunctorMultiplicationForm[a_, b_], pos_] := (
    addPhiDecoration2[pos];
    MapIndexStack[%, pos, {a, b}]
  );

  Seq[RelativeToCategoryForm[e_, _], pos_] := %[e, pos];

  Seq[MonoidalUnitSymbol, pos_] := -1;

  Seq[color:($ColorPattern | _Int), _] := color;

  Seq[(OneArrow|IdentityArrow)[e_], pos_] := %[e, pos];

  Seq[(CategoryObjectSymbol|CategoryArrowSymbol|NaturalTransformationSymbol)[sym_Str], _] :=
    ToRainbowInteger @ sym;

  Seq[AssociatorForm[a_, b_, c_], pos_] := (
    addMonoidalProductDecoration[pos, $monoidalProductColor];
    addAssociatorDecoration[pos, False];
    MapIndexStack[%, pos, {a, b, c}]
  );

 Seq[InverseForm[AssociatorForm[a_, b_, c_]], pos_] := (
    addMonoidalProductDecoration[pos, $monoidalProductColor];
    addAssociatorDecoration[pos, True];
    MapIndexStack[%, pos, {a, b, c}]
  );

 Seq[BraidingForm[a_, b_], pos_] := (
    addMonoidalProductDecoration[pos, $monoidalProductColor];
    addBraidingDecoration[pos];
    MapIndexStack[%, pos, {a, b}]
  );

 Seq[LeftUnitorForm[a_], pos_] := (
    addMonoidalProductDecoration[pos, $monoidalProductColor];
    addUnitorDecoration[pos, 1];
    MapIndexStack[%, pos, {-1, a}]
  );

 Seq[RightUnitorForm[a_], pos_] := (
    addMonoidalProductDecoration[pos, $monoidalProductColor];
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
  cd_CommutativeDiagram                :> RuleEval @ ReplaceOptions[cd, $rainbowCDOptions],
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

addDecoration[e_] := AppTo[$epilog, e];

$rrOpt = 0.25;

addPhiDecoration1[pos_] := addDecoration @
  Line[
    GraphicsValue[{"VertexCoordinates", {pos}}, F /* Fn[{{-.2, -.2} + #, {.2, .2} + #}]]
  ];

addPhiDecoration1[pos_] :=
  addFunctorDecoration[pos, "F", EdgeForm[{$Gray, AbsoluteThickness[.5], AbsoluteDashing[{2, 2}]}]];

addPhiDecoration2[pos_] := (
  addDiskDecoration[pos, $LightGray];
  addDecoration @ GraphicsValue[{"VertexCoordinates", VertexPattern @ App[pos, Repeated[_, {0, 1}]]}, CoordinateBounds[#, .35]& /* phiDec2];
);

addPhiDecoration2[pos_] := (
  addDiskDecoration[pos, $LightGray];
  addFunctorDecoration[App[pos, 1], "F"];
  addFunctorDecoration[App[pos, 2], "F"];
  addFunctorDecoration[pos, "F", EdgeForm[{$Gray, AbsoluteThickness[.5], AbsoluteDashing[{2, 2}]}]];
  (* addDecoration @ GraphicsValue[{"VertexCoordinates", VertexPattern @ Append[pos, Repeated[_, {0, 1}]]}, CoordinateBounds[#, .35]& /* phiDec2]; *)
);

phiDec2[{{x1_, x2_}, {y1_, y2_}}] := {
  Style[Rectangle[{x1, y1}, {x2, y2 - .8}, RoundingRadius -> $rrOpt], FaceEdgeForm[None, $Gray]],
  Style[Rectangle[{x1, y1}, {x2, y2 + .2}, RoundingRadius -> $rrOpt], FaceEdgeForm[None, $Gray]],
  Style[Rectangle[{x1, y1}, {x2, y2 - 1}, RoundingRadius -> $rrOpt], FaceForm @ None, EdgeForm[{White, AbsoluteThickness[2]}]]
};

phiDec2[{{x1_, x2_}, {y1_, y2_}}] := {
  Style[RollingCurve[{{x1, y2 - .8}, {x1, y2 + .1}, {x2, y2 + .1}, {x2, y2 - .8}}, BendRadius -> $rrOpt], $Gray, Dashing[{.05, .05}]],
  Style[Line[{{x1, y2 - .8}, {x2, y2 - .8}}], $Gray]
};

addMonoidalProductDecoration[pos_, None] := Null;
addMonoidalProductDecoration[pos_, color_] := addDiskDecoration[pos, color];

addDiskDecoration[pos_, fc_] := addDecoration @
  Style[
    Disk[GraphicsValue[{"VertexCoordinates", {pos}}, F], .2],
    FaceEdgeForm[fc, Darker[fc, .5]]
  ];

addFunctorDecoration[pos_, sym_, opts___] := addDecoration @ Style[
  BoundingRectangle[
    GraphicsValue[{"VertexCoordinates", VertexPattern @ App[pos, ___]}, CoordinateBounds[#, .33]&],
    RoundingRadius -> $rrOpt
  ],
  FaceForm[None], EdgeForm[{AbsoluteThickness[1], $Gray}], opts
];

addUnitorDecoration[pos_, i_] := addDecoration @
  Line[
    GraphicsValue[{"VertexCoordinates", {App[pos, i]}}, F /* Fn[{{-.2, .4} + #, {.2, .4} + #}]]
  ];

(* TODO: use NamedIcon instead here *)
addAssociatorDecoration[pos_, isRev_] := addDecoration @ {
  associatorArrowhead[App[pos, If[isRev, 3, 1]],  {0, 1}/4],
  associatorArrowhead[App[pos, If[isRev, 1, 3]], -{0, 1}/4]
};

associatorArrowhead[pos_, dir_] := Arrowhead[
  GraphicsValue[{"VertexCoordinates", {pos}}, F /* PlusOperator[{0, .5}]], dir,
  ArrowheadColor -> $Black, ArrowheadShape -> "Arrow", ArrowheadAnchor -> Center
];

addBraidingDecoration[vertex_] := addDecoration @
  Text["\[LeftRightArrow]", GraphicsValue[{"VertexCoordinates", {vertex}}, F], {-0.1, .9}, BaseStyle -> {FontSize -> 9}];

(**************************************************************************************************)

PublicTypesettingForm[ReversedChainForm]

SetUsage @ "ReversedChainForm[form$] indicates that form$ should compose in reverse order."

DefineStandardTraditionalForm[ReversedChainForm[form_] :> TagBox[ToBoxes[form], "ReverseChain"]];

(**************************************************************************************************)

PublicTypesettingForm[ObjectArrowIconForm, CategoryFunctorIconForm]

SetUsage @ "ObjectArrowIconForm[expr$] depicts all %CategoryObjectSymbols as disks and %CategoryArrowSymbols as arrows, preserving colors."
SetUsage @ "CategoryFunctorIconForm[expr$] depicts all %CategorySymbols as disks and %CategoryFunctorSymbols as arrows, preserving colors."

DefineStandardTraditionalForm[{
  ObjectArrowIconForm[expr_] :> ToBoxes @ RepAll[expr, {
    c_CommutativeDiagram :> ReplaceOptions[c, TextModifiers -> ObjectArrowIconForm],
    c_CategoryObjectSymbol :> replaceFormContents[c, "\[FilledCircle]"],
    c_CategoryArrowSymbol :> replaceFormContents[c, ReversedChainForm @ BoldRightArrowSymbol]
  }],
  CategoryFunctorIconForm[expr_] :> ToBoxes @ RepAll[expr, {
    c_CommutativeDiagram :> ReplaceOptions[c, TextModifiers -> CategoryFunctorIconForm],
    c_CategorySymbol :> replaceFormContents[c, "\[FilledCircle]"],
    c_FunctorSymbol :> replaceFormContents[c, ReversedChainForm @ BoldRightArrowSymbol]
  }]
}];

replaceFormContents[(tag_Symbol ? $taggedFormHeadQ)[sub_], new_] := tag[replaceFormContents[sub, new]];
replaceFormContents[(mod_Symbol ? $styleFormHeadQ)[sub_, s___], new_] := mod[replaceFormContents[sub, new], s];
replaceFormContents[(mod_Symbol ? $modifierFormHeadQ)[sub_, s___], new_] := replaceFormContents[sub, new];
replaceFormContents[_, new_] := new;

(**************************************************************************************************)

PublicTypesettingFormBox[OverArrowForm, UnderArrowForm]

DefineUnaryForm[OverArrowForm, LowerBox[OverscriptBox["\[LongRightArrow]", LowerBox[$1, 5]], .15]];
DefineUnaryForm[UnderArrowForm, RaiseBox[UnderscriptBox["\[LongRightArrow]", RaiseBox[$1, 5]], .15]];
