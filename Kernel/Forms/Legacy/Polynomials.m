PublicTypesettingForm[VariableForm]

DefineTaggedForm[VariableForm];

(**************************************************************************************************)

PublicTypesettingForm[PolynomialSymbol]

DefineTaggedForm[PolynomialSymbol];

(**************************************************************************************************)

PublicTypesettingForm[PolyForm]

declareBoxFormatting[
  PolyForm[args__] :>
    generalPolyBoxes[
      PolyForm, "PolynomialForm", "PowerForm", "PlusForm", "ImplicitTimesForm", MakeMathBoxes,
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

innerPolyBoxes[a_, n_Int ? Negative] :=
  TemplateBox[{polyTermForm @ a, IntStr @ Abs @ n}, "SubtractForm"];

innerPolyBoxes[args___] :=
  TemplateBox[MapUnevaluated[polyTermForm, {args}], $polyPlusForm];
    
polyTermForm = Case[
  HoldForm[a_]                := % @ a;
  Style[e_, s_]               := StyleBox[% @ e, s];
  p_Plus | p_PlusForm         := Apply[innerPolyBoxes, Uneval @ p];
  a_Times | a_TimesForm       := Construct[%, Apply[List, Uneval @ a]];
  Power[a_, b_]               := TemplateBox[{% @ a, MakeMathBoxes @ b}, $polyPowerForm];
  (Inverted|InvertedForm)[n_]   := TemplateBox[List @ % @ n, "InvertedForm"];
  a_ ? longPolyQ              := TemplateBox[List @ Apply[innerPolyBoxes, Uneval @ a], "SpacedParenthesesForm"];
  a_List                      := TemplateBox[MapUnevaluated[makeInnerPolyParamQGBoxes, a], $polyTimesForm];
  a_                          := $scalarBoxes @ a;
];

makeInnerPolyParamQGBoxes = Case[
  Style[e_, s_]               := StyleBox[% @ e, s];
  a_List | a_Times | a_TimesForm | ParenthesesForm[a_] := TemplateBox[{polyTermForm @ a}, "SpacedParenthesesForm"];
  Power[a_, b_]               := TemplateBox[{% @ a, MakeMathBoxes @ b}, $polyPowerForm];
  a_ ? longPolyQ              := TemplateBox[List @ Apply[innerPolyBoxes, Uneval @ a], "SpacedParenthesesForm"];
  (Inverted|InvertedForm)[n_]   := TemplateBox[List @ % @ n, "InvertedForm"];
  a_                          := $scalarBoxes @ a;
];

longPolyQ[e_PolyForm] := Len[Uneval @ e] > 1;
longPolyQ[e_] := H[Uneval @ e] === $polyHead && Len[Uneval @ e] > 1;

