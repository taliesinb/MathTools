PublicForm[QuotientForm, CompactQuotientForm, MultilineQuotientForm]

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

PublicForm[TranslationVectorForm]

declareCommaRiffledForm[TranslationVectorForm, "translationVector"];

(**************************************************************************************************)

PublicForm[PathQuotientSymbol]

SetUsage @ "
PathQuotientSymbol[q$, mu$] represents the path quiver on quiver q$.
"

declareBoxFormatting[
  PathQuotientSymbol[q_, u_] :> makeTypedTemplateBox[q -> QuiverSymbol, u -> PathMapSymbol, "PathQuotientSymbolForm"]
];
