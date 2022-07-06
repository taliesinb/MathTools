PublicForm[ExistsForm, ForAllForm]

declareBinaryForm[ExistsForm] // usingCustomKatex["existsForm"];
declareBinaryForm[ForAllForm] // usingCustomKatex["forAllForm"];


$TemplateKatexFunction["ForAllSymbol"] = katexAlias["forall"];
$TemplateKatexFunction["ExistsSymbol"] = katexAlias["exists"];

(**************************************************************************************************)

PublicForm[AndForm, OrForm]

declareInfixSymbol[{AndForm, OrForm}];

(**************************************************************************************************)

PublicForm[NotForm]

declareUnaryForm[NotForm, maybeParen[SymbolForm]] // usingCustomKatex["notted"];

(**************************************************************************************************)

PublicForm[ImpliesForm, ImpliedByForm]

declareInfixSymbol[ImpliesForm] // usingCustomKatex["implies"];
declareInfixSymbol[ImpliedByForm] // usingCustomKatex["implied"];

(**************************************************************************************************)

PublicForm[EquivalentForm]

declareInfixSymbol[EquivalentForm] // usingCustomKatex["iff"];

(**************************************************************************************************)

PublicForm[SuchThatForm]

declareBoxFormatting[
  SuchThatForm[a_, b_] :> makeTemplateBox[a, b, "SuchThatForm"]
];

$TemplateKatexFunction["SuchThatForm"] = "suchThat";
