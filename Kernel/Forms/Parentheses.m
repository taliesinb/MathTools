PublicTypesettingForm[ParenthesesForm, TightParenthesesForm, SpacedParenthesesForm]

DefineCommaForm[ParenthesesForm, ParenthesesBox[$1]];

DefineCommaForm[TightParenthesesForm, RBox["(", $1, ")"]];

DefineStandardTraditionalForm[SpacedParenthesesForm[a___] :> MakeBoxes @ ParenthesesForm[SpacedCommaRowForm[a]]];

(**************************************************************************************************)

PublicTypesettingForm[NoParenthesesForm]

DefineTaggedForm[NoParenthesesForm]

DefineStandardTraditionalForm[{
  ParenthesesForm[NoParenthesesForm[e_]] :> MakeMathBoxes @ e,
  NoParenthesesForm[ParenthesesForm[e_]] :> MakeMathBoxes @ e
}];

(**************************************************************************************************)

PublicTypesettingForm[NTimesForm]

DefineUnaryForm[NTimesForm, RBox[$1, MathTextBox @ " times"], BoxFunction -> nTimesBox]

(**************************************************************************************************)

PublicTypesettingForm[ParenthesesLabeledForm, ParenthesesRepeatedForm, ModulusLabeledForm]

DefineBinaryForm[ParenthesesLabeledForm, RBox[$1, $EmSpaceBox, ParenthesesBox[$2]], BoxFunction -> parenthesesLabeledBox]
DefineBinaryForm[ParenthesesRepeatedForm, parenthesesLabeledBox[$1, nTimesBox @ $2]]
DefineBinaryForm[ModulusLabeledForm, RBox[$1, $EmSpaceBox, RomanBox[ParenthesesBox["mod", "\[MediumSpace]", $2]]]];

(**************************************************************************************************)

PublicTypesettingForm[UnderbraceLabeledForm, UnderbraceRepeatedForm]

DefineBinaryForm[UnderbraceLabeledForm, UnderbraceBox[$1, $2]]
DefineBinaryForm[UnderbraceRepeatedForm, UnderbraceBox[$1, nTimesBox @ $2]]

(**************************************************************************************************)

PublicTypesettingForm[OverbraceLabeledForm, OverbraceRepeatedForm]

DefineBinaryForm[OverbraceLabeledForm, OverbraceBox[$1, $2]]
DefineBinaryForm[OverbraceRepeatedForm, OverbraceBox[$1, nTimesBox @ $2]]

(**************************************************************************************************)

PublicTypesettingForm[ShapeLabeledForm]

DefineRestCommaForm[ShapeLabeledForm, OverbraceBox[$1, ParenthesesBox @ $2]];
