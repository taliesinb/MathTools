PublicForm[CategorySymbol, CategoryObjectSymbol, CategoryArrowSymbol]

DefineSymbolForm[CategorySymbol]
DefineSymbolForm[CategoryObjectSymbol]
DefineSymbolForm[CategoryArrowSymbol]

(**************************************************************************************************)

PublicForm[ThenForm]

DefineRiffledForm[ThenForm, RowBox[$1], "；\[NegativeThickSpace]\[NegativeThickSpace]", ThenBox];

(**************************************************************************************************)

PublicForm[CatCat, CatSet, CatFinSet, CatGrp, CatMat, CatKVect, CatMon, CatPoly, CatRel, CatRing, CatRMod, CatTop, CatFunct, CatAlg]

DefineConstantSymbolForm[CatCat,    "𝖢𝖺𝗍"]
DefineConstantSymbolForm[CatSet,    "𝖲𝖾𝗍"]
DefineConstantSymbolForm[CatFinSet, "𝖥𝗂𝗇𝖲𝖾𝗍"]
DefineConstantSymbolForm[CatGrp,    "𝖦𝗋𝗉"]
DefineConstantSymbolForm[CatMat,    "𝖬𝖺𝗍"]
DefineConstantSymbolForm[CatKVect,  "𝖪𝖵𝖾𝖼𝗍"]
DefineConstantSymbolForm[CatMon,    "𝖬𝗈𝗇"]
DefineConstantSymbolForm[CatPoly,   "𝖯𝗈𝗅𝗒"]
DefineConstantSymbolForm[CatRel,    "𝖱𝖾𝗅"]
DefineConstantSymbolForm[CatRing,   "𝖱𝗂𝗇𝗀"]
DefineConstantSymbolForm[CatRMod,   "𝖱𝖬𝗈𝖽"]
DefineConstantSymbolForm[CatTop,    "𝖳𝗈𝗉"]
DefineConstantSymbolForm[CatFunct,  "𝖥𝗎𝗇𝖼𝗍"]
DefineConstantSymbolForm[CatAlg,    "𝖠𝗅𝗀"]

(**************************************************************************************************)

PublicForm[IdArray, HomForm, ExplicitHomForm]

DefineUnaryForm[IdArrow, SubscriptBox[FunctionBox["id"], $1]]

DefineBinaryForm[HomForm, ZAppliedBox[FunctionBox["hom"], $1, $2]]

DefineTernaryForm[ExplicitHomForm, ZAppliedBox[$1, $1, $2]]

(**************************************************************************************************)

PublicForm[FunctorSymbol]

DefineSymbolForm[FunctorSymbol]

(**************************************************************************************************)
