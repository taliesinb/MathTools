PublicTypesettingForm[VariableForm]

declareBoxFormatting[
  VariableForm[p_] :>
    TemplateBox[List @ symbolBoxes @ p, "VariableForm"]
];

$TemplateKatexFunction["VariableForm"] = "variable";


(**************************************************************************************************)

PublicTypesettingForm[PolynomialSymbol]

declareSymbolForm[PolynomialSymbol];

(**************************************************************************************************)

PublicTypesettingForm[PolyForm]

declareBoxFormatting[
  PolyForm[args__] :>
    generalPolyBoxes[
      PolyForm, "PolynomialForm", "PowerForm", "PlusForm", "ImplicitTimesForm", MakeQGBoxes,
      args
    ]
];

$TemplateKatexFunction["PolynomialForm"] = "poly";

(**************************************************************************************************)

PublicTypesettingForm[QuiverProductPolyForm]

declareBoxFormatting[
  QuiverProductPolyForm[args__] :>
    generalPolyBoxes[
      QuiverProductPolyForm,
      "QuiverProductPolynomialForm", "QuiverProductPowerForm", "QuiverProductPlusForm", "QuiverProductTimesForm", graphOrQuiverBoxes,
      args
    ]
];

$TemplateKatexFunction["QuiverProductPowerForm"] = "quiverProdPower";
$TemplateKatexFunction["QuiverProductPolynomialForm"] = "quiverProdPoly";
$TemplateKatexFunction["QuiverProductPlusForm"] = riffled["+"];
$TemplateKatexFunction["QuiverProductTimesForm"] = riffled["\,"];

(**************************************************************************************************)

SetHoldAllComplete[generalPolyBoxes, polyTermForm, makeInnerPolyParamQGBoxes, innerPolyBoxes, longQ]

generalPolyBoxes[polyHead_, polyForm_, powerForm_, plusForm_, timesForm_, scalarForm_, args___] := Scope[
  $polyHead = polyHead;
  $polyPlusForm = plusForm;
  $polyPowerForm = powerForm;
  $polyTimesForm = timesForm;
  $scalarBoxes = scalarForm;
  TemplateBox[
    List @ innerPolyBoxes @ args,
    polyForm
  ]
];

innerPolyBoxes[a_, n_Integer ? Negative] :=
  TemplateBox[{polyTermForm @ a, IntegerString @ Abs @ n}, "SubtractForm"];

innerPolyBoxes[args___] :=
  TemplateBox[MapUnevaluated[polyTermForm, {args}], $polyPlusForm];
    
polyTermForm = Case[
  HoldForm[a_]                := % @ a;
  Style[e_, s_]               := StyleBox[% @ e, s];
  p_Plus | p_PlusForm         := Apply[innerPolyBoxes, Unevaluated @ p];
  a_Times | a_TimesForm       := Construct[%, Apply[List, Unevaluated @ a]];
  Power[a_, b_]               := TemplateBox[{% @ a, MakeQGBoxes @ b}, $polyPowerForm];
  (Inverted|InvertedForm)[n_]   := TemplateBox[List @ % @ n, "InvertedForm"];
  a_ ? longPolyQ              := TemplateBox[List @ Apply[innerPolyBoxes, Unevaluated @ a], "SpacedParenthesesForm"];
  a_List                      := TemplateBox[MapUnevaluated[makeInnerPolyParamQGBoxes, a], $polyTimesForm];
  a_                          := $scalarBoxes @ a;
];

makeInnerPolyParamQGBoxes = Case[
  Style[e_, s_]               := StyleBox[% @ e, s];
  a_List | a_Times | a_TimesForm | ParenthesesForm[a_] := TemplateBox[{polyTermForm @ a}, "SpacedParenthesesForm"];
  Power[a_, b_]               := TemplateBox[{% @ a, MakeQGBoxes @ b}, $polyPowerForm];
  a_ ? longPolyQ              := TemplateBox[List @ Apply[innerPolyBoxes, Unevaluated @ a], "SpacedParenthesesForm"];
  (Inverted|InvertedForm)[n_]   := TemplateBox[List @ % @ n, "InvertedForm"];
  a_                          := $scalarBoxes @ a;
];

longPolyQ[e_PolyForm] := Length[Unevaluated @ e] > 1;
longPolyQ[e_] := Head[Unevaluated @ e] === $polyHead && Length[Unevaluated @ e] > 1;

