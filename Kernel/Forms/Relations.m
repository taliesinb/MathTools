PublicTypesettingForm[ElementOfForm, NotElementOfForm]

DefineInfixBinaryForm[ElementOfForm, WideOpBox @ "\[Element]"];
DefineInfixBinaryForm[NotElementOfForm, WideOpBox @ "\[NotElement]"];

DefineStandardTraditionalForm[{
  ElementOfForm[a_, as__, b_] :> MakeBoxes @ ElementOfForm[CommaRowForm[a, as], b],
  NotElementOfForm[a_, as__, b_] :> MakeBoxes @ ElementOfForm[CommaRowForm[a, as], b]
}];

(**************************************************************************************************)

PublicTypesettingForm[PathRelationForm]

(* declareBoxFormatting[
  PathRelationForm[args__] :>
    TemplateBox[MapUnevaluated[wordBoxes, {args}], "PathRelationForm"],
  PathRelationForm[] :>
    SBox["PathRelationSymbol"]
]

$TemplateKatexFunction["PathRelationForm"] = katexAliasRiffled["pathIso"]
$TemplateKatexFunction["PathRelationSymbol"] = katexAlias["pathIso"];
 *)
(* Cell[StyleData["PathRelationForm", StyleDefinitions -> StyleData[
 "CardinalFont"]],
 TemplateBoxOptions->{DisplayFunction->(RowBox[{
    TemplateSlotSequence[1,
     TemplateBox[{},
      "PathRelationSymbol"]]}]& \
)},ExpressionUUID->"53ad7f34-c559-4822-af7a-78183f5961fb"],

Cell[StyleData["PathRelationSymbol", StyleDefinitions -> StyleData[
 "CardinalFont"]],
 TemplateBoxOptions->{
 DisplayFunction->(
  "\[VeryThinSpace]\[TildeEqual]\[VeryThinSpace]"& \
)},ExpressionUUID->"abd84d09-6f9a-4dce-850a-402058fb4bb2"]
}, Open  ]],
 *)

(**************************************************************************************************)

PublicTypesettingForm[TailEqualForm, HeadEqualForm]
PublicTypesettingForm[BijectiveForm, ApproxEqualForm, IsomorphicForm, HomeomorphicForm, CongruentForm, IdenticallyEqualForm, HomotopicForm, DefEqualForm, TripleEqualForm, SyntaxEqualForm, UnderdotEqualForm, DotEqualForm, ColonEqualForm]
PublicTypesettingForm[EqualForm, NotEqualForm, LessForm, LessEqualForm, GreaterForm, GreaterEqualForm]
PublicTypesettingForm[SubsetForm, SubsetEqualForm, SupersetForm, SupersetEqualForm]
PublicTypesettingForm[SubmultisetForm, SubmultisetEqualForm, SupermultisetForm, SupermultisetEqualForm]

DefineInfixForm[#1, OpBox @ #2]& @@@ ExpressionTable[
  TailEqualForm                UnderdotBox["="]
  HeadEqualForm                OverdotBox["="]
  BijectiveForm                "≈"
  ApproxEqualForm              "≈"
  IsomorphicForm               "≅"
  HomeomorphicForm             "≅"
  HomotopicForm                "≃"
  DefEqualForm                 "≝"
  TripleEqualForm              "≡"
  SyntaxEqualForm              GrayBox["≡"]
  UnderdotEqualForm            UnderdotBox["="]
  DotEqualForm                 OverdotBox["="]
  ColonEqualForm               "≔"
  CongruentForm                "≡"
  IdenticallyEqualForm         "≡"
  EqualForm                    "="
  NotEqualForm                 "≠"
  LessForm                     "<"
  LessEqualForm                "≤"
  GreaterForm                  ">"
  GreaterEqualForm             "≥"
  SubsetForm                   "⊂"
  SubsetEqualForm              "⊆"
  SupersetForm                 "⊃"
  SupersetEqualForm            "⊇"
  SubmultisetForm              OverdotBox @ "⊂"
  SubmultisetEqualForm         OverdotBox @ "⊆"
  SupermultisetForm            OverdotBox @ "⊃"
  SupermultisetEqualForm       OverdotBox @ "⊇"
];

(**************************************************************************************************)

PublicTypesettingForm[BinaryRelationForm]

DefineStandardTraditionalForm[{
  BinaryRelationForm[r_Str] :> OpBox[r],
  br_BinaryRelationForm[args___] :> RiffledBox[MakeBoxes @ br][MakeMathBoxSequence @ args]
}];

(**************************************************************************************************)

(* AppliedRelationForm was removed, not sure it's use *)
