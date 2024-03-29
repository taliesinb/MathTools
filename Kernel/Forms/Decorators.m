PublicTypesettingForm[StyleDecorated]

StyleDecorated[style_][head_[args___]] := StyleDecorated[style, head][args];

DefineStandardTraditionalForm[
  StyleDecorated[style_, head_][args___] :> styleDecoratedBoxes[head, style, args]
]

SetHoldAll[styleDecoratedBoxes];

styleDecoratedBoxes[head_, style_, args___] := Scope[
  kboxes = boxes = MakeBoxes[head[args]];
  boxes //= EvaluateTemplateBox;
  kboxes //= evalKatexRepeated;
  $style = StyleBoxOperator @@ ToList[style];
  KBox[
    StyleBox[applyDecoratedStyle @ boxes, "MathFont"],
    $style = stripLR /* $style; applyDecoratedStyle @ kboxes
  ]
];

$LRSisterPatt = "\[CenterDot]" | AdjustmentBox["\[CenterDot]", ___] | (_Str ? (SContainsQ["\\cdot"]));

applyDecoratedStyle = Case[
  StyleBox[b_, opts___] := StyleBox[% @ b, opts];
  AdjustmentBox[b_, opts___] := AdjustmentBox[% @ b, opts];
  RowBox[{left_, ileft:$LRSisterPatt, middle___, iright:$LRSisterPatt, right_}] :=
    RowBox[{$style @ left, $style @ ileft, middle, $style @ iright, $style @ right}];
  RowBox[{left_, middle___, right_}] :=
    RowBox[{$style @ left, middle, $style @ right}];
  GridBox[{{left_, ileft:$LRSisterPatt, middle___, iright:$LRSisterPatt, right_}}, opts___] :=
    GridBox[{{$style @ left, $style @ ileft, middle, $style @ iright, $style @ right}}, opts];
  GridBox[{{left_, middle___, right_}}, opts___] :=
    GridBox[{{$style @ left, middle, $style @ right}}, opts];
  UnderscriptBox[a_, b_] := UnderscriptBox[a, $style @ b];
  "underlinesegment"[a_] := $style @ "underlinesegment"[StyleBox[a, Black]];
];

stripLR = Case[
  s_Str := StringTrimLeft[s, {"\\left", "\\right"}];
  other_   := other;
];

evalKatexRepeated = Case[
  TemplateBox[args_List, name_Str] /; KeyQ[$katexDisplayFunction, name] :=
    % @ Apply[Rep[$katexDisplayFunction @ name, macro_Str :> $katexMacros[macro]], args];
  other_ := other;
]

(**************************************************************************************************)

PublicTypesettingForm[Undersegment]

DefineUnaryModifierForm[Undersegment, KBox[
  UnderscriptBox[$1, "\[LeftRightVector]"],
  "underlinesegment"[$1]
]]

(**************************************************************************************************)

PublicTypesettingFormBox[PrimedForm, DoublePrimedForm]

DefineUnaryModifierForm[PrimedForm, StyleBox[SuperscriptBox[$1, "\[Prime]"], ScriptBaselineShifts->{0.35, 0.8}, ScriptSizeMultipliers -> 0.71], BoxFunction -> PrimedBox]
DefineUnaryModifierForm[DoublePrimedForm, SuperscriptBox[$1, "\[DoublePrime]"], BoxFunction -> DoublePrimedBox]

PublicTypesettingForm[InvisiblePrimedForm]

DefineUnaryModifierForm[InvisiblePrimedForm, SuperscriptBox[$1, StyleBox["\[Prime]", ShowContents -> False]]]

(**************************************************************************************************)

PublicTypesettingForm[UnderdotForm, OverdotForm, OverdoubledotForm]

DefineUnaryModifierForm[UnderdotForm, UnderdotBox[$1]]
DefineUnaryModifierForm[OverdotForm, OverdotBox[$1]]
DefineUnaryModifierForm[OverdoubledotForm, OverdoubledotBox[$1]]

(**************************************************************************************************)

PublicTypesettingForm[PositiveSignedPartForm, NegativeSignedPartForm]

DefineUnaryModifierForm[PositiveSignedPartForm, SuperscriptBox[$1, "+"]]
DefineUnaryModifierForm[NegativeSignedPartForm, SuperscriptBox[$1, "-"]]

(**************************************************************************************************)

PublicTypesettingForm[SignedForm]

(* TODO: introduce DaggerForm ? *)
DefineUnaryModifierForm[SignedForm, SuperStarBox[$1]]

(**************************************************************************************************)

PublicTypesettingFormBox[FamilyModifierForm]

DefineUnaryModifierForm[FamilyModifierForm, StyleBox[$1, Bold], BoxFunction -> FamilyModifierBox]

(**************************************************************************************************)

PublicTypesettingFormBox[WhiteCircleModifierForm, BlackCircleModifierForm]

DefineUnaryModifierForm[WhiteCircleModifierForm, SuperscriptBox[$1, "\[SmallCircle]"], BoxFunction -> WhiteCircleModifierBox]
DefineUnaryModifierForm[BlackCircleModifierForm, SuperscriptBox[$1, "\[FilledSmallCircle]"], BoxFunction -> BlackCircleModifierBox]

(**************************************************************************************************)

PublicTypesettingForm[ImageModifierForm, PreimageModifierForm, MultiImageModifierForm, MultiPreimageModifierForm]

PublicTypesettingForm[MultiImageColorModifierForm, MultiPreimageColorModifierForm]

(* i haven't thought through which of these should really be modifiers and which not *)
DefineUnaryModifierForm[ImageModifierForm, SuperscriptBox[$1, "\[RightArrow]"]]
DefineUnaryModifierForm[PreimageModifierForm, SuperscriptBox[$1, "\[LeftArrow]"]]
DefineUnaryModifierForm[MultiImageModifierForm, SuperscriptBox[$1, "\[RightTeeArrow]"]]
DefineUnaryModifierForm[MultiPreimageModifierForm, SuperscriptBox[$1, "\[LeftTeeArrow]"]]
DefineUnaryModifierForm[MultiImageColorModifierForm, StyleBox[$1, RGBColor[{0.73, 0.27, 0.27}]]]
DefineUnaryModifierForm[MultiPreimageColorModifierForm, StyleBox[$1, RGBColor[{0.05, 0.48, 0.50}]]]

(**************************************************************************************************)

PublicTypesettingForm[NegatedForm]

DefineUnaryModifierForm[NegatedForm, KBox[OverscriptBox[$1, "_"], "bar"[$1]]]
DefineUnaryModifierForm[InvertedForm, KBox[UnderscriptBox[$1, "_"], "underbar"[$1]]];

(**************************************************************************************************)

PublicTypesettingForm[VerticalModifierForm]

DefineStandardTraditionalForm[
  VerticalModifierForm[inner_, args___] :> rewriteToVerticalOuter[MakeMathBoxes[inner], args]
];

$verticalFormData = <|
  "GroupWordRewritingForm" -> {"\[LeftAngleBracket]", "\[RightAngleBracket]"},
  "ListForm" -> {SBox["LeftBrace"], SBox["RightBrace"]},
  "TupleForm" -> {"(", ")"},
  "SetForm" -> {SBox["LeftBrace"], SBox["RightBrace"]},
  "MultisetForm" -> {SBox["LeftMultisetBracket"], SBox["RightMultisetBracket"]},
  "AssociativeArrayForm" -> {"\[LeftAngleBracket]", "\[RightAngleBracket]"}
|>;

$TemplateKatexFunction["LeftBrace"] = katexAlias["lbrace"];
$TemplateKatexFunction["RightBrace"] = katexAlias["rbrace"];

$TemplateKatexFunction["LeftMultisetBracket"] = katexAlias["openMultiset"];
$TemplateKatexFunction["RightMultisetBracket"] = katexAlias["closeMultiset"];

makeFixedSpanning[char_] :=
  StyleBox[char, SpanMinSize -> 1.5, SpanMaxSize -> 1.5];

rewriteToVerticalOuter[arg_, opts:OptionsPattern[]] :=
  rewriteToVerticalOuter[arg, 1, opts];

Options[rewriteToVerticalOuter] = Options[VerticalModifierForm] = {
  Alignment -> Auto
}

rewriteToVerticalOuter[arg_, n_Int, opts:OptionsPattern[]] :=
  rewriteToVertical[arg, n, OptionValue[Alignment]]

rewriteToVertical[TemplateBox[args_, "AssociativeArrayForm"] /; Len[args] >= 2, n_, align_] := Scope[
  {l, r} = makeFixedSpanning /@ $verticalFormData["AssociativeArrayForm"];
  args = toSplicedRow /@ args;
  rows = Partition[args, n]; nrows = Len[rows];
  rows = PrependColumn[rows, Pre[l] @ Table["", nrows-1]];
  rows = AppendColumn[rows, App[r] @ Table["", nrows-1]];
  rows = rows /. splice -> Splice;
  rows = MapAt[addComma, rows, {All, 4 ;; -3 ;; 3}];
  rows = MapAt[addComma, rows, {;;-2, -2}];
  colSpacings = Flatten[{1, Table[{1, 1, 2}, n], 0} / 4];
  colSpacings[[-2]] = 0;
  TBoxOp["GridForm"] @ createGridBox[rows, {Right, {Right, SubAuto[align, Center], Left}, Left}, Auto, colSpacings]
];

toSplicedRow = Case[
  TemplateBox[{a_, b_}, "MapsToForm"]        := splice[{a, SBox["MapsToSymbol"], b}];
  s:TemplateBox[{}, sym_]                    := splice[{"", s, ""}];
];

PrivateFunction[addComma]

addComma[""] := "";
addComma[a_] := RBox[a, ","];

rewriteToVertical[TemplateBox[args_, form_Str] /; KeyQ[$verticalFormData, form], 1, align_] /; Len[args] >= 2 := Scope[
  {l, r} = makeFixedSpanning /@ $verticalFormData[form]; gap = spacerBox[2];
  l = RBox[l, gap]; r = RBox[gap, r];
  first = F[args]; last = L[args]; middle = Part[args, 2 ;; -2];
  spacer = TemplateBox[{l}, "InvisibleForm"];
  rows = Map[List, Flatten @ {
    RBox[l, first, ","],
    RBox[spacer, #, ","]& /@ middle,
    RBox[spacer, last, r]
  }];
  TBoxOp["GridForm"] @ createGridBox[rows, {SubAuto[align, Left]}, Auto, 0]
];

rewriteToVertical[TemplateBox[{style_, rest___}, form_ /; SStartsQ[form, "Styled"]], args___] := Scope[
  res = rewriteToVertical[TemplateBox[{rest}, SDrop[form, 6]], args];
  res /. GridBox[g_, opts___] :> GridBox[
      g // MapAt[styleOpenItem[style], {1, 1}] // MapAt[styleCloseItem[style], {-1, -1}],
      opts
  ]
];

styleOpenItem[s_][e_] := TBox[e, s];
styleOpenItem[s_][r_RowBox] := MapAt[styleOpenItem[s], r, {1, 1}];

styleCloseItem[s_][e_] := TBox[e, s];
styleCloseItem[s_][r_RowBox] := MapAt[styleCloseItem[s], r, {1, -1}];

rewriteToVertical[TemplateBox[args_, form_Str] /; KeyQ[$verticalFormData, form], n_, align_] /; Len[args] >= 2 := Scope[
  {l, r} = makeFixedSpanning /@ $verticalFormData[form];
  rows = Partition[args, n];
  spacer = TemplateBox[{l}, "InvisibleForm"];
  rows = {
    middleRow[l] @ F @ rows,
    Splice[middleRow[spacer] /@ Part[rows, 2 ;; -2]],
    lastRow[spacer, r] @ L @ rows
  };
  TBoxOp["GridForm"] @ createGridBox[rows, {Right, {SubAuto[align, Center]}, Left}, Auto, 0]
];

rewriteToVertical[other_, _, _] := other;

middleRow[l_][{most__}] := Join[{l}, commaRowBox /@ {most}, {""}];
lastRow[l_, r_][{most__, last_}] := Join[{l}, commaRowBox /@ {most}, {last, r}];

commaRowBox[""] := "";
commaRowBox[e_] := RBox[TemplateBox[List @ ",", "InvisibleForm"], e, ","];
