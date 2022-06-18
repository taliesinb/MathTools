PublicForm[ExistsForm, ForAllForm]

declareBinaryForm[ExistsForm] // usingCustomKatex["existsForm"];
declareBinaryForm[ForAllForm] // usingCustomKatex["forAllForm"];

(**************************************************************************************************)

PublicForm[AndForm, OrForm]

declareInfixSymbol[{AndForm, OrForm}];

(**************************************************************************************************)

PublicForm[NotForm]

declareUnaryForm[NotForm, maybeParen[SymbolForm]] // usingCustomKatex["notted"];

(**************************************************************************************************)

PublicForm[ImpliesForm]

declareInfixSymbol[ImpliesForm] // usingCustomKatex["implies"];

(**************************************************************************************************)

PublicForm[EquivalentForm]

declareInfixSymbol[EquivalentForm] // usingCustomKatex["iff"];

(**************************************************************************************************)

PublicForm[SuchThatForm]

declareBoxFormatting[
  SuchThatForm[a_, b_] :> makeTemplateBox[a, b, "SuchThatForm"]
];

$TemplateKatexFunction["SuchThatForm"] = "suchThat";
