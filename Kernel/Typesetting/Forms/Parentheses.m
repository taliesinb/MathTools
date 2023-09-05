PublicForm[ParenthesesForm, TightParenthesesForm, SpacedParenthesesForm]

DefineCommaForm[ParenthesesForm, ParenthesesBox[$1]];

DefineCommaForm[TightParenthesesForm, RBox["(", $1, ")"]];

DefineStandardTraditionalForm[SpacedParenthesesForm[a___] :> MakeBoxes @ ParenthesesForm[SpacedCommaRowForm[a]]];

(**************************************************************************************************)

PublicForm[NoParenthesesForm]

DefineTaggedForm[NoParenthesesForm]

DefineStandardTraditionalForm[{
  ParenthesesForm[NoParenthesesForm[e_]] :> MakeQGBoxes @ e,
  NoParenthesesForm[ParenthesesForm[e_]] :> MakeQGBoxes @ e
}];

(**************************************************************************************************)

PublicForm[NTimesForm]

DefineUnaryForm[NTimesForm, RBox[$1, MathTextBox @ " times"], BoxFunction -> nTimesBox]

(**************************************************************************************************)

PublicForm[ParenthesesLabeledForm, ParenthesesRepeatedForm, ModulusLabeledForm]

DefineBinaryForm[ParenthesesLabeledForm, RBox[$1, $EmSpaceBox, ParenthesesBox[$2]], BoxFunction -> parenthesesLabeledBox]
DefineBinaryForm[ParenthesesRepeatedForm, parenthesesLabeledBox[$1, nTimesBox @ $2]]
DefineBinaryForm[ModulusLabeledForm, RBox[$1, $EmSpaceBox, RomanBox[ParenthesesBox["mod", "\[MediumSpace]", $2]]]];

(**************************************************************************************************)

PublicForm[UnderbraceLabeledForm, UnderbraceRepeatedForm]

DefineBinaryForm[UnderbraceLabeledForm, UnderbraceBox[$1, $2]]
DefineBinaryForm[UnderbraceRepeatedForm, UnderbraceBox[$1, nTimesBox @ $2]]

(**************************************************************************************************)

PublicForm[OverbraceLabeledForm, OverbraceRepeatedForm]

DefineBinaryForm[OverbraceLabeledForm, OverbraceBox[$1, $2]]
DefineBinaryForm[OverbraceRepeatedForm, OverbraceBox[$1, nTimesBox @ $2]]

(**************************************************************************************************)

PublicForm[ShapeLabeledForm]

DefineRestCommaForm[ShapeLabeledForm, OverbraceBox[$1, ParenthesesBox @ $2]];
