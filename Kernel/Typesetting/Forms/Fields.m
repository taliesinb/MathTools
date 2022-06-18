PublicForm[FiniteFieldSymbol]

declareBoxFormatting[
  FiniteFieldSymbol[n_] :> makeTemplateBox[n, "FiniteField"]
];

$TemplateKatexFunction["FiniteField"] = "finiteField";

(**************************************************************************************************)

PublicForm[BaseFieldSymbol]

BaseFieldSymbol[] := BaseFieldSymbol["K"];

declareBoxFormatting[
  BaseFieldSymbol[s_] :> makeTemplateBox[s, "BaseFieldSymbolForm"]
];

$TemplateKatexFunction["BaseFieldSymbolForm"] = "baseField";

(**************************************************************************************************)

PublicForm[FieldSymbol]

FieldSymbol[] := FieldSymbol["K"];

$fieldAliases = <|
  "C" -> "Complexes",
  "R" -> "Reals",
  "Q" -> "Rationals"
|>

declareAlgebraicSymbol[FieldSymbol, $fieldAliases];

(**************************************************************************************************)

PublicForm[LinearCombinationCoefficientSymbol]

declareSymbolForm[LinearCombinationCoefficientSymbol];
