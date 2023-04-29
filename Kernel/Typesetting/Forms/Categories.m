PublicForm[CategorySymbol, CategoryObjectSymbol, CategoryArrowSymbol]

DefineTaggedForm[CategorySymbol]
DefineTaggedForm[CategoryObjectSymbol]
DefineTaggedForm[CategoryArrowSymbol]

(**************************************************************************************************)

PublicForm[MonoidalProductForm]

DefineInfixForm[MonoidalProductForm, OpBox @ "\[CircleTimes]"]

(**************************************************************************************************)

PublicForm[NaryMonoidalFunctionForm]

DefineUnaryForm[NaryMonoidalFunctionForm, SuperscriptBox["\[CircleTimes]", $1]];

DefineStandardTraditionalForm[
  n_NaryMonoidalFunctionForm[args___] :> MakeBoxes @ AppliedForm[n, args]
];

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

PublicForm[IdArrow, HomForm, ExplicitHomForm]

DefineUnaryForm[IdArrow, SubscriptBox[FunctionBox["id"], $1]]

DefineBinaryForm[HomForm, AppliedBox[FunctionBox["hom"], $1, $2]]

DefineTernaryForm[ExplicitHomForm, AppliedBox[$1, $2, $3]]

(**************************************************************************************************)

PublicForm[FunctorSymbol]

DefineTaggedForm[FunctorSymbol]

(**************************************************************************************************)
