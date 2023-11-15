PublicTypesettingForm[TranspositionForm]

DefineInfixBinaryForm[TranspositionForm, OpBox @ "\[TwoWayRule]"]

(**************************************************************************************************)

PublicTypesettingForm[PermutationCycleForm]

DefineInfixForm[PermutationCycleForm, OpBox @ "\[Rule]"]

DefineStandardTraditionalForm[{
  PermutationCycleForm[l_, r_]      :> ToBoxes @ TranspositionForm[l, r],
  PermutationCycleForm[l_, m__, r_] :> TBox[MakeMathBoxSequence[l, m , r], MakeMathBoxes @ l, "PermutationCycleForm"]
}];

(**************************************************************************************************)

PublicTypesettingForm[StandardPermutationForm]

DefineStandardTraditionalForm[{
  StandardPermutationForm[Cycles[{}]] :> TBox[standardCycleBoxes @ {}, "StandardPermutationForm"],
  StandardPermutationForm[Cycles[list:{___List}]] :> TBox[Apply[TightRowBox, standardCycleBoxes /@ list], "StandardPermutationForm"]
}];

DefineTemplateBox[StandardPermutationForm, "StandardPermutationForm", $1, None];

(**************************************************************************************************)

PublicTypesettingForm[StandardPermutationCycleForm]

DefineStandardTraditionalForm[
  StandardPermutationCycleForm[list_List] :> standardCycleBoxes @ list
];

standardCycleBoxes[list_List] := TemplateBox[MakeMathBoxes /@ list, "StandardPermutationCycleForm"]

DefineTemplateBox[StandardPermutationCycleForm, "StandardPermutationCycleForm", ParenthesesBox[$$1], None];

(**************************************************************************************************)

PublicTypesettingForm[PermutationForm]

DefineTemplateBox[StandardPermutationCycleForm, "StandardPermutationCycleForm", RiffledBox[OpBox[";"]] @ $$1, None];

DefineStandardTraditionalForm[
  PermutationForm[Cycles[{}]] :> MakeBoxes @ GroupIdentitySymbol
];

(* declareBoxFormatting[

  PermutationForm[Cycles[list:{___List}] | list:{___List}] :>
    TemplateBox[
      ToBoxes /@ (PermutationCycleForm @@@ list),
      "PermutationSetForm"
    ],

  PermutationForm[Cycles[{list_List}]] :>
    ToBoxes @ (PermutationCycleForm @@ list),

  PermutationForm[Cycles[{}]] :>
    MakeBoxes @ GroupElementSymbol @ "e"
];
 *)
$TemplateKatexFunction["PermutationSetForm"] =
  applyRiffled["permutationSet", "\\permutationSetSymbol "]

