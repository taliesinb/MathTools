PublicForm[StyleDecorated]

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
    StyleBox[applyDecoratedStyle @ boxes, "QuiverGeometryBase"],
    $style = stripLR /* $style; applyDecoratedStyle @ kboxes
  ]
];

$LRSisterPatt = "\[CenterDot]" | AdjustmentBox["\[CenterDot]", ___] | (_String ? (StringContainsQ["\\cdot"]));

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
  s_String := StringTrimLeft[s, {"\\left", "\\right"}];
  other_   := other;
];

evalKatexRepeated = Case[
  TemplateBox[args_List, name_String] /; KeyExistsQ[$katexDisplayFunction, name] :=
    % @ Apply[Replace[$katexDisplayFunction @ name, macro_String :> $katexMacros[macro]], args];
  other_ := other;
]

(**************************************************************************************************)

PublicForm[Undersegment]

DefineUnaryForm[Undersegment, KBox[
  UnderscriptBox[$1, "\[LeftRightVector]"],
  "underlinesegment"[$1]
]]

(**************************************************************************************************)

PublicFormBox[Primed, DoublePrimed]

DefineUnaryForm[PrimedForm, SuperscriptBox[$1, "\[Prime]"], BoxFunction -> PrimedBox]
DefineUnaryForm[DoublePrimedForm, SuperscriptBox[$1, "\[DoublePrime]"], BoxFunction -> DoublePrimedBox]

PublicForm[InvisiblePrimedForm]

DefineUnaryForm[InvisiblePrimedForm, SuperscriptBox[$1, StyleBox["\[Prime]", ShowContents -> False]]]

(**************************************************************************************************)

PublicForm[UnderdotForm, OverdotForm, OverdoubledotForm]

DefineUnaryForm[UnderdotForm, UnderdotBox[$1]]
DefineUnaryForm[OverdotForm, OverdotBox[$1]]
DefineUnaryForm[OverdoubledotForm, OverdoubledotBox[$1]]

(**************************************************************************************************)

PublicForm[PositiveSignedPartForm, NegativeSignedPartForm]

DefineUnaryForm[PositiveSignedPartForm, SuperscriptBox[$1, "+"]]
DefineUnaryForm[NegativeSignedPartForm, SuperscriptBox[$1, "-"]]

(**************************************************************************************************)

PublicFormBox[Signed]

DefineUnaryForm[SignedForm, SuperscriptBox[$1, "*"], BoxFunction -> SignedBox]

(**************************************************************************************************)

PublicFormBox[FamilyModifier]

DefineUnaryForm[FamilyModifierForm, StyleBox[$1, Bold], BoxFunction -> FamilyModifierBox]

(**************************************************************************************************)

PublicFormBox[WhiteCircleModifier, BlackCircleModifier]

DefineUnaryForm[WhiteCircleModifierForm, SuperscriptBox[$1, "\[SmallCircle]"], BoxFunction -> WhiteCircleModifierBox]
DefineUnaryForm[BlackCircleModifierForm, SuperscriptBox[$1, "\[FilledSmallCircle]"], BoxFunction -> BlackCircleModifierBox]

(**************************************************************************************************)

PublicForm[ImageModifierForm, PreimageModifierForm, MultiImageModifierForm, MultiPreimageModifierForm]

PublicForm[MultiImageColorModifierForm, MultiPreimageColorModifierForm]

DefineUnaryForm[ImageModifierForm, SuperscriptBox[$1, "\[RightArrow]"]]
DefineUnaryForm[PreimageModifierForm, SuperscriptBox[$1, "\[LeftArrow]"]]
DefineUnaryForm[MultiImageModifierForm, SuperscriptBox[$1, "\[RightTeeArrow]"]]
DefineUnaryForm[MultiPreimageModifierForm, SuperscriptBox[$1, "\[LeftTeeArrow]"]]
DefineUnaryForm[MultiImageColorModifierForm, StyleBox[$1, RGBColor[{0.73, 0.27, 0.27}]]]
DefineUnaryForm[MultiPreimageColorModifierForm, StyleBox[$1, RGBColor[{0.05, 0.48, 0.50}]]]

(**************************************************************************************************)

PublicForm[NegatedForm]

DefineUnaryForm[NegatedForm, KBox[OverscriptBox[$1, "_"], "bar"[$1]]]
DefineUnaryForm[InvertedForm, KBox[UnderscriptBox[$1, "_"], "underbar"[$1]]];

(**************************************************************************************************)

PublicForm[VerticalModifierForm]

DefineStandardTraditionalForm[
  VerticalModifierForm[inner_, args___] :> rewriteToVerticalOuter[MakeQGBoxes[inner], args]
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
  Alignment -> Automatic
}

rewriteToVerticalOuter[arg_, n_Integer, opts:OptionsPattern[]] :=
  rewriteToVertical[arg, n, OptionValue[Alignment]]

rewriteToVertical[TemplateBox[args_, "AssociativeArrayForm"] /; Length[args] >= 2, n_, align_] := Scope[
  {l, r} = makeFixedSpanning /@ $verticalFormData["AssociativeArrayForm"];
  args = toSplicedRow /@ args;
  rows = Partition[args, n]; nrows = Length[rows];
  rows = PrependColumn[rows, Prepend[l] @ Table["", nrows-1]];
  rows = AppendColumn[rows, Append[r] @ Table["", nrows-1]];
  rows = rows /. splice -> Splice;
  rows = MapAt[addComma, rows, {All, 4 ;; -3 ;; 3}];
  rows = MapAt[addComma, rows, {;;-2, -2}];
  colSpacings = Flatten[{1, Table[{1, 1, 2}, n], 0} / 4];
  colSpacings[[-2]] = 0;
  TBoxOp["GridForm"] @ createGridBox[rows, {Right, {Right, ReplaceAutomatic[align, Center], Left}, Left}, Automatic, colSpacings]
];

toSplicedRow = Case[
  TemplateBox[{a_, b_}, "MapsToForm"]        := splice[{a, SBox["MapsToSymbol"], b}];
  s:TemplateBox[{}, sym_]                    := splice[{"", s, ""}];
];

PrivateFunction[addComma]

addComma[""] := "";
addComma[a_] := RBox[a, ","];

rewriteToVertical[TemplateBox[args_, form_String] /; KeyExistsQ[$verticalFormData, form], 1, align_] /; Length[args] >= 2 := Scope[
  {l, r} = makeFixedSpanning /@ $verticalFormData[form]; gap = spacerBox[2];
  l = RBox[l, gap]; r = RBox[gap, r];
  first = First[args]; last = Last[args]; middle = Part[args, 2 ;; -2];
  spacer = TemplateBox[{l}, "InvisibleForm"];
  rows = Map[List, Flatten @ {
    RBox[l, first, ","],
    RBox[spacer, #, ","]& /@ middle,
    RBox[spacer, last, r]
  }];
  TBoxOp["GridForm"] @ createGridBox[rows, {ReplaceAutomatic[align, Left]}, Automatic, 0]
];

rewriteToVertical[TemplateBox[{style_, rest___}, form_ /; StringStartsQ[form, "Styled"]], args___] := Scope[
  res = rewriteToVertical[TemplateBox[{rest}, StringDrop[form, 6]], args];
  res /. GridBox[g_, opts___] :> GridBox[
      g // MapAt[styleOpenItem[style], {1, 1}] // MapAt[styleCloseItem[style], {-1, -1}],
      opts
  ]
];

styleOpenItem[s_][e_] := TBox[e, s];
styleOpenItem[s_][r_RowBox] := MapAt[styleOpenItem[s], r, {1, 1}];

styleCloseItem[s_][e_] := TBox[e, s];
styleCloseItem[s_][r_RowBox] := MapAt[styleCloseItem[s], r, {1, -1}];

rewriteToVertical[TemplateBox[args_, form_String] /; KeyExistsQ[$verticalFormData, form], n_, align_] /; Length[args] >= 2 := Scope[
  {l, r} = makeFixedSpanning /@ $verticalFormData[form];
  rows = Partition[args, n];
  spacer = TemplateBox[{l}, "InvisibleForm"];
  rows = {
    middleRow[l] @ First @ rows,
    Splice[middleRow[spacer] /@ Part[rows, 2 ;; -2]],
    lastRow[spacer, r] @ Last @ rows
  };
  TBoxOp["GridForm"] @ createGridBox[rows, {Right, {ReplaceAutomatic[align, Center]}, Left}, Automatic, 0]
];

rewriteToVertical[other_, _, _] := other;

middleRow[l_][{most__}] := Join[{l}, commaRowBox /@ {most}, {""}];
lastRow[l_, r_][{most__, last_}] := Join[{l}, commaRowBox /@ {most}, {last, r}];

commaRowBox[""] := "";
commaRowBox[e_] := RBox[TemplateBox[List @ ",", "InvisibleForm"], e, ","];

