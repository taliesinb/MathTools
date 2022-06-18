PublicForm[TranspositionForm]

declareBinaryForm[TranspositionForm];

(**************************************************************************************************)

PublicForm[PermutationCycleForm]

declareBoxFormatting[
  PermutationCycleForm[a_, b_] :> MakeBoxes @ TranspositionForm[a, b],

  PermutationCycleForm[args__] :> TemplateBox[
    With[{list = MapUnevaluated[makeQGBoxes, {args}]},
      Append[list, First @ list]
    ],
    "PermutationCycleForm"
  ]
];

$TemplateKatexFunction["PermutationCycleForm"] =
  applyRiffled["permutationCycle", "\\permutationCycleSymbol "];

(**************************************************************************************************)

PublicForm[StandardPermutationForm]

declareBoxFormatting[
  StandardPermutationForm[Cycles[{}]] :>
    TemplateBox[
      List @ standardCycleBoxes @ {},
      "StandardPermutationForm"
    ],
  StandardPermutationForm[Cycles[list:{___List}]] :>
    TemplateBox[
      standardCycleBoxes /@ list,
      "StandardPermutationForm"
    ]
];

(**************************************************************************************************)

PublicForm[StandardPermutationCycleForm]

declareBoxFormatting[
  StandardPermutationCycleForm[list_List] :>
    standardCycleBoxes @ list
];

standardCycleBoxes[list_] := TemplateBox[makeQGBoxes /@ list, "StandardPermutationCycleForm"]

(**************************************************************************************************)

PublicForm[PermutationForm]

declareBoxFormatting[

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

$TemplateKatexFunction["PermutationSetForm"] =
  applyRiffled["permutationSet", "\\permutationSetSymbol "]

