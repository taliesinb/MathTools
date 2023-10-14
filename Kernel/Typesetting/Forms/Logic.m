PublicTypesettingForm[ExistsForm, ForAllForm]

existsBox[a_] := RBox["\[Exists]", "\[ThinSpace]", a];
forAllBox[a_] := RBox["\[ForAll]", "\[ThinSpace]", a];

(* TODO: replace part of this with DefineRestCommaForm *)

DefineStandardTraditionalForm[{
  ExistsForm[a_, rest__] :> TBox[MakeQGBoxes @ a, CommaRowBox @ MakeQGBoxSequence[rest], "ExistsForm"],
  ExistsForm[a_] :> TBox[MakeQGBoxes @ a, "UnconditionalExistsForm"],
  ExistsForm :> "\[Exists]",
  ForAllForm[a_, rest__] :> TBox[MakeQGBoxes @ a, CommaRowBox @ MakeQGBoxSequence[rest], "ForAllForm"],
  ForAllForm[a_] :> TBox[MakeQGBoxes @ a, "UnconditionalForAllForm"],
  ForAllForm :> "\[ForAll]"
}];

DefineTemplateBox[ExistsForm, "UnconditionalExistsForm", existsBox[$1], None];
DefineTemplateBox[ForAllForm, "UnconditionalForAllForm", forAllBox[$1], None];
DefineTemplateBox[ExistsForm, "ExistsForm", RBox[existsBox @ $1, OpBox @ ":", $2], None]
DefineTemplateBox[ForAllForm, "ForAllForm", RBox[forAllBox @ $1, OpBox @ ":", $2], None]

(**************************************************************************************************)

PublicTypesettingForm[AndForm, OrForm, XorForm, NandForm]

DefineInfixForm[AndForm,  OpBox @ "\[And]"];
DefineInfixForm[OrForm,   OpBox @ "\[Or]"];
DefineInfixForm[XorForm,  OpBox @ "\[Xor]"];
DefineInfixForm[NandForm, OpBox @ "\[Nand]"];

(**************************************************************************************************)

PublicTypesettingForm[NotForm]

DefineUnaryForm[NotForm, RBox["\[Not]", $1], HeadBoxes -> "\[Not]"];

(**************************************************************************************************)

PublicTypesettingForm[ImpliesForm, ImpliedByForm, EquivalentForm]

DefineInfixBinaryForm[ImpliesForm, WideOpBox @ "⟹"]
DefineInfixBinaryForm[ImpliedByForm, WideOpBox @ "⟸"];
DefineInfixBinaryForm[EquivalentForm, WideOpBox @ "⟺"]

(**************************************************************************************************)

PublicTypesettingForm[SuchThatForm]

DefineInfixBinaryForm[SuchThatForm, KBox[StyleBox["\[ThinSpace]\[VerticalSeparator]\[ThickSpace]", FontSize -> 20], KBin["\\vert"]]]
