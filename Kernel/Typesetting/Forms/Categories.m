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
}]

SetListable[DeclareFunctorlike];
DeclareFunctorlike[sym_Symbol] := DefineStandardTraditionalForm[
  head_sym[args__] :> ToBoxes @ FunctorAppliedForm[head, args]
];

(**************************************************************************************************)

PublicTypesettingForm[FunctorSymbol, LeftFunctorSymbol, RightFunctorSymbol]
PublicVariable[$UseLeftRightArrowFunctors]

SetInitialValue[$UseLeftRightArrowFunctors, False];

DefineTaggedForm[FunctorSymbol]

DefineStandardTraditionalForm[{
  FunctorSymbol["Left"] :> ToBoxes[FunctorSymbol[If[$UseLeftRightArrowFunctors, "\[FilledLeftTriangle]", "L"]]],
  FunctorSymbol["Right"] :> ToBoxes[FunctorSymbol[If[$UseLeftRightArrowFunctors, "\[FilledRightTriangle]", "R"]]],
  LeftFunctorSymbol :> MakeBoxes[FunctorSymbol["Left"]],
  RightFunctorSymbol :> MakeBoxes[FunctorSymbol["Right"]]
}];

DeclareFunctorlike[FunctorSymbol]

(**************************************************************************************************)

PublicTypesettingForm[FunctorPowerForm]

DefineStandardTraditionalForm[
  FunctorPowerForm[f_, n_] :> ToBoxes @ PowerForm[f, n]
];

DeclareFunctorlike[FunctorPowerForm];

(**************************************************************************************************)

PublicTypesettingForm[DiagonalFunctorForm, LimitFunctorForm, ColimitFunctorForm, LeftKanExtensionForm, RightKanExtensionForm]

DefineUnaryForm[DiagonalFunctorForm, SubscriptBox["\[CapitalDelta]", $1]]
DefineUnaryForm[LimitFunctorForm, SubscriptBox[FunctionBox["lim"], $1]]
DefineUnaryForm[ColimitFunctorForm, SubscriptBox[FunctionBox["colim"], $1]]

DefineUnaryForm[LeftKanExtensionForm, SubscriptBox[FunctionBox["Lan"], $1]]
DefineUnaryForm[RightKanExtensionForm, SubscriptBox[FunctionBox["Ran"], $1]]

DeclareFunctorlike[{DiagonalFunctorForm, LimitFunctorForm, ColimitFunctorForm, LeftKanExtensionForm, RightKanExtensionForm}];

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

DefineStandardTraditionalForm[
  SpacedCompositionForm[gs:(GradientSymbol[RightArrowSymbol | "\[RightArrow]", ___]..)] :>
    RowBox @ Riffle[Map[ToBoxes, Reverse @ {gs}], "\[VeryThinSpace]"]
]

(**************************************************************************************************)

PublicTypesettingForm[IdArrow, OneArrow, HomForm, TightHomForm, CompactHomForm, ExplicitHomForm]

DefineUnaryForm[IdArrow, SubscriptBox[FunctionBox["id"], $1]]
DefineUnaryForm[OneArrow, SubscriptBox[FunctionBox["1"], $1]]

DefineBinaryForm[HomForm, NoSpanBox @ AppliedBox[FunctionBox["hom"], $1, $2]]
DefineBinaryForm[TightHomForm, NoSpanBox @ TightAppliedBox[FunctionBox["hom"], $1, $2]]
DefineBinaryForm[CompactHomForm, GridBox[
  {{FunctionBox["h"], SmallerBox @ SmallerBox @ SmallerBox @ $1}, {"\[SpanFromAbove]", SmallerBox @ SmallerBox @ SmallerBox @ $2}},
  RowSpacings -> 0, ColumnSpacings -> .1, RowAlignments->Center, GridFrameMargins -> {{0, 0}, {0, 0}}, RowMinHeight -> 0.5]];

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

PublicTypesettingForm[GradientSymbol, GradientArrowSymbol]

GradientArrowSymbol[args___] := GradientSymbol[RightArrowSymbol, args];

DefineStandardTraditionalForm[{

(*    GradientSymbol[sym_, col1_, col2_, sz_:16] :>
    ToBoxes @ AdjustmentForm[ColorGradientForm[
      Style[sym, FontSize -> sz],
      ToRainbowColor /@ {col1, col2},
      "DilationFactor" -> 1, "CompressionFactor" -> 0.5
    ], {{0, 0}, {0, -0.7}}],
  (*     ^ this ensures that e.g. AppliedForm doesn't have big brackets when wrapping a GradientSymbol *),
 *)
  GradientSymbol[sym_, col1_, col2_, sz_:16] :>
    ToBoxes @ ColorGradientForm[
      Style[sym, FontSize -> sz],
      ToRainbowColor /@ {col1, col2},
      "DilationFactor" -> 1, "CompressionFactor" -> 0.5
    ],

  GradientSymbol[RightArrowSymbol | "\[RightArrow]", col1_, col2_, sz_:16] :>
    gradientArrowBoxes[col1, col2, sz],

  (g:GradientSymbol[_FunctorSymbol, ___])[args___] :>
    NoSpanBox @ ToBoxes @ FunctorAppliedForm[g, args],

  (h_GradientSymbol)[args___] :>
    NoSpanBox @ ToBoxes @ AppliedForm[h, args]
}];

$rightArrowPath := $rightArrowPath = Uncompress @ "
1:eJxTTMoPSmViYGDQB2IQLf1oz12BWLYDhiolp+TXrNr/2WIBi5gI2wETYxA4vH9l45H9b7tYDzgn
TPrWmnV2f3HCD1nlSywHVm2Z/uUY16X9DGDAcgCs3PjyfrsdZwNXLL1oD+P3ifyvUlE/vf+OlcNDM7
OX+7n9XsisT7u4/0tpy5T4j6/3/+ZZwVqvcHl/rJ9Q5DnN9/vteSZ93959aX/dgpY8TruP+9uvSs6b
XnJ+v6Xdmu7zBz7tP/IxMbaG7dj+jVaHP09693n/XXavF9MebN4v0Pzk79tFX/anmWy7a3Jslr09c9
ZB+RNf9htpvFvbaHXAvknY6vjqyC8w99hPehTf/yTl8/5H6hM6mZZ9sl8uv++jfP75/YHMnfW3RT/b
u0D9+6zQgVNH4wvUP4f3mx+p3K2464u9KTS8oPbYZ0HtNYPK/5swz/qk21I43x6i3v7+ko25yxZ+sa
99fcTPpWujvdZb1b0bm77YR/Hx7E922WVvN5EjfLnDF/u8Y7n/JCQP2qPZb384deXxHJ4v9qo1/jf3
Wh+xh4aD/WFIuNiHRc3jd636bM9yPjfZYO1JezT/2D/4sC3q3stP9tvck79uMj1n3yXl8ef2uhP2HD
6X9gWs/WwfDXUHzF0MUHD2DAh8sY+EyO+HyXdC9O+H6Z9xnFF1w4IL+9PTQOCT/S2B52/bXlzar7Qw
+F/C1Q/2t6H8vXsjmO1C39nPgqqHpLfX9kL17RccfsLTjz1a+rJHS3/2sPQJ8x8s/RpBwwuWvq2h4Q
8AA9xygg==";

gradientArrowBoxes[col1_, col2_, sz_] := Construct[
  GraphicsBox,
  {
    ToGraphicsBoxes @ LinearGradientFilling[{0.4 -> ToRainbowColor[col1], 0.7 -> ToRainbowColor[col2]}],
    Construct[PolygonBox, $rightArrowPath]
  },
  PlotRange -> {{-2.8, 1.3}, {-1.3, 1.3}},
  ImagePadding -> {{0, 1}, {0, 0}},
  BaselinePosition -> Scaled[0.05],
  ImageSize -> {Automatic, sz/2+1}
];

(**************************************************************************************************)

PublicTypesettingForm[ImplicitAppliedForm]

DefineBinaryForm[ImplicitAppliedForm, RBox[$1, $2]]

(**************************************************************************************************)

PublicTypesettingForm[MonoidalTreeForm]

DefineStandardTraditionalForm[{MonoidalTreeForm[e_, opts___Rule] :> monoidalTreeFormBoxes[e, opts]}];

monoidalTreeFormBoxes[cd_CommutativeDiagram, ___] :=
  ToBoxes @ Append[cd, TextModifiers -> <|
    "Objects"   -> MonoidalTreeForm,
    "Morphisms" -> Function[morphisms, MonoidalTreeForm[morphisms, GraphicsScale -> 10, VertexSize -> 4]]
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

  Seq[color:($ColorPattern | _Integer), _] := color;

  Seq[(OneArrow|IdentityArrow)[e_], pos_] := %[e, pos];

  Seq[(CategoryObjectSymbol|CategoryArrowSymbol|NaturalTransformationSymbol)[sym_String], _] :=
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

DefineStandardTraditionalForm[
  RainbowCategoryForm[form_] :> rainbowCategoryFormBoxes[form]
];

$rainbowCDOptions = {ColorRules -> "Rainbow", SymbolReplacements -> "DiskArrow"};
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
    GraphicsValue[{"VertexCoordinates", {Append[pos, i]}}, First /* Function[{{-.2, .4} + #, {.2, .4} + #}]]
  ]
];

(* TODO: use NamedIcon instead here *)
addAssociatorDecoration[pos_, isRev_] := AppendTo[$epilog, {
  associatorArrowhead[Append[pos, If[isRev, 3, 1]],  {0, 1}/4],
  associatorArrowhead[Append[pos, If[isRev, 1, 3]], -{0, 1}/4]
}];

associatorArrowhead[pos_, dir_] := Arrowhead[
  GraphicsValue[{"VertexCoordinates", {pos}}, First /* PlusOperator[{0, .5}]], dir,
  ArrowheadColor -> $Black, ArrowheadShape -> "Arrow", ArrowheadAnchor -> Center
];

addBraidingDecoration[vertex_] := AppendTo[$epilog,
  Text["\[LeftRightArrow]", GraphicsValue[{"VertexCoordinates", {vertex}}, First], {-0.1, 1}, BaseStyle -> {FontSize -> 9}]
];

