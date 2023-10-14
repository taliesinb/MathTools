PublicTypesettingForm[QuotientForm, CompactQuotientForm, MultilineQuotientForm]

declareBoxFormatting[
  QuotientForm[a_, b_] :>
    makeTemplateBox[a, b, "QuotientForm"],
  MultilineQuotientForm[a_, b_] :>
    makeTemplateBox[a, b, "MultilineQuotientForm"],
  CompactQuotientForm[f_, x_, v_] :>
    makeHintedTemplateBox[f -> QuiverSymbol, x -> VertexSymbol, v -> FunctionSymbol, "CompactQuotientForm"]
];

$TemplateKatexFunction["QuotientForm"] = "quotient";
$TemplateKatexFunction["MultilineQuotientForm"] = "multilineQuotient";
$TemplateKatexFunction["CompactQuotientForm"] = "compactQuotient";

(**************************************************************************************************)

PublicTypesettingForm[TranslationVectorForm]

declareCommaRiffledForm[TranslationVectorForm, "translationVector"];

(*
Cell[StyleData["TranslationVectorForm", StyleDefinitions -> StyleData[
 "QuiverGeometryBase"]],
 TemplateBoxOptions->{DisplayFunction->(SubscriptBox[
   RowBox[{"[",
     RowBox[{
       TemplateSlotSequence[1,
        RowBox[{",", " "}]]}], "]"}],
   StyleBox[
   "T", "MathTextFont"]]& \
)},ExpressionUUID->"74712420-1447-4ac5-8bd1-6c54ce1bd94b"],

*)

(**************************************************************************************************)

PublicTypesettingForm[PathQuotientSymbol]

SetUsage @ "
PathQuotientSymbol[q$, mu$] represents the path quiver on quiver q$.
"

declareBoxFormatting[
  PathQuotientSymbol[q_, u_] :> makeTypedTemplateBox[q -> QuiverSymbol, u -> PathMapSymbol, "PathQuotientSymbolForm"]
];
