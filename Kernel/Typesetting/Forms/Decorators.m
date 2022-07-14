(**************************************************************************************************)

PublicForm[PrimedForm]

declareUnaryWrapperForm[PrimedForm];

(**************************************************************************************************)

PublicForm[PositiveSignedPartForm, NegativeSignedPartForm]

declareUnaryWrapperForm[PositiveSignedPartForm];
declareUnaryWrapperForm[NegativeSignedPartForm];

(**************************************************************************************************)

PublicForm[SignedForm]

declareUnaryWrapperForm[SignedForm]
declareAppliedFormatting[SignedForm];

(**************************************************************************************************)

PublicForm[FamilyModifierForm]

declareUnaryWrapperForm[FamilyModifierForm]

(**************************************************************************************************)

PublicForm[WhiteCircleModifierForm, BlackCircleModifierForm]

declareUnaryForm[WhiteCircleModifierForm];
declareUnaryForm[BlackCircleModifierForm];

(**************************************************************************************************)

PublicForm[ImageModifierForm, PreimageModifierForm, MultiImageModifierForm, MultiPreimageModifierForm]

PublicForm[MultiImageColorModifierForm, MultiPreimageColorModifierForm]

declareUnaryForm[ImageModifierForm];
declareUnaryForm[PreimageModifierForm];
declareUnaryForm[MultiImageModifierForm];
declareUnaryForm[MultiPreimageModifierForm];
declareUnaryForm[MultiImageColorModifierForm];
declareUnaryForm[MultiPreimageColorModifierForm];

declareAppliedFormatting[ImageModifierForm];
declareAppliedFormatting[PreimageModifierForm];
declareAppliedFormatting[MultiImageModifierForm];
declareAppliedFormatting[MultiPreimageModifierForm];
declareAppliedFormatting[MultiImageColorModifierForm];
declareAppliedFormatting[MultiPreimageColorModifierForm];

declareBoxFormatting[
  f_ImageModifierForm[args___] :> MakeBoxes @ AppliedForm[f, args],
  f_PreimageModifierForm[args___] :> MakeBoxes @ AppliedForm[f, args],
  f_MultiImageModifierForm[args___] :> MakeBoxes @ AppliedForm[f, args],
  f_MultiPreimageModifierForm[args___] :> MakeBoxes @ AppliedForm[f, args],
  f_MultiImageColorModifierForm[args___] :> MakeBoxes @ AppliedForm[f, args],
  f_MultiPreimageColorModifierForm[args___] :> MakeBoxes @ AppliedForm[f, args]
];

(**************************************************************************************************)

PublicForm[InvertedBoxForm]

SetHoldFirst[InvertedBoxForm];
InvertedBoxForm[e_] := makeTemplateBox[e, "InvertedForm"]

$TemplateKatexFunction["InvertedForm"] = "inverted";

InvertedForm[(c:$colorFormP)[e_]] := c[InvertedForm[e]];

(**************************************************************************************************)

PublicForm[NegatedForm]

declareUnaryWrapperForm[NegatedForm]

(**************************************************************************************************)

PublicForm[VerticalModifierForm]

declareBoxFormatting[
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

