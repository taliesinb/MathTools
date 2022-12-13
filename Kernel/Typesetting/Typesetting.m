PrivateForm[ModForm]

ModForm[x_, Infinity|0] := x;

declareBoxFormatting[
  ModForm[a_, b_] :> modBox[MakeBoxes @ a, b],
  ModForm[a_, b_List] :> RowBox[{MakeBoxes @ a, " % ", StyleBox[MakeBoxes @ b, $Blue]}]
];

modBox[a_, b_] := SubscriptBox[a, StyleBox[numBox @ b, $Blue]];

(**************************************************************************************************)

PublicFunction[LargeLabeled]

Options[LargeLabeled] = JoinOptions[
  Spacings -> 0,
  Labeled
];

LargeLabeled[e_, l_, opts:OptionsPattern[]] :=
  Labeled[
    e, l, opts,
    FrameMargins -> {{0, 0}, {OptionValue[Spacings], 0}},
    LabelStyle -> Prepend[$LabelStyle, FontSize -> 16]
  ];


(**************************************************************************************************)

PublicForm[EllipsisForm]

EllipsisForm[list_, n_] := If[Length[list] > n, Append[Take[list, n], $LargeEllipsis], list];
EllipsisForm[n_][list_] := EllipsisForm[list, n];

(**************************************************************************************************)

PublicFormBox[Click]

SetHoldRest[ClickForm, ClickBox];

MakeBoxes[ClickForm[elem_, fn_], form_] := ClickBox[MakeBoxes[elem, form], fn];
Typeset`MakeBoxes[ClickForm[elem_, fn_], form_, type:Graphics|Graphics3D] := ClickBox[Typeset`MakeBoxes[elem, form, type], fn];

$customGraphicsHeadQ[ClickBox] = True;

ClickBox[box_, body_] := TagBox[
  TagBox[box, EventHandlerTag[{"MouseClicked" :> body, Method -> "Preemptive", PassEventsDown -> Automatic, PassEventsUp -> True}]],
  MouseAppearanceTag["LinkHand"]
];

(**************************************************************************************************)

PublicFunction[NiceTooltip]

MakeBoxes[NiceTooltip[a_, b_], form_] := NiceTooltipBoxes[MakeBoxes[a, form], MakeBoxes[b, form]];
MakeBoxes[NiceTooltip[a_, None], form_] := MakeBoxes[a, Form];

Typeset`MakeBoxes[NiceTooltip[a_, b_], form_, graphics_] := NiceTooltipBoxes[Typeset`MakeBoxes[a, form, graphics], MakeBoxes[b, form]];
Typeset`MakeBoxes[NiceTooltip[a_, None], form_, graphics_] := MakeBoxes[a, form, graphics];

$customGraphicsHeadQ[NiceTooltip] = True;

(**************************************************************************************************)

PublicFunction[Flipper]

MakeBoxes[Flipper[a_, b_], form_] := FlipperBoxes[MakeBoxes[a, form], MakeBoxes[b, form]];

FlipperBoxes[a_, b_] :=
  DynamicModuleBox[
    {flippervar$$ = 1},
    TagBox[TagBox[
      PaneSelectorBox[{1 -> a,  2 -> b}, Dynamic[flippervar$$], ImageSize -> Automatic, ImageMargins -> 5],
      EventHandlerTag[{"MouseClicked" :> Set[flippervar$$, Mod[flippervar$$ + 1, 2, 1]]}]
    ], MouseAppearanceTag["LinkHand"]],
    DynamicModuleValues -> {flippervar$$}
  ];

(**************************************************************************************************)

PrivateFunction[NiceTooltipBoxes]

NiceTooltipBoxes[a_, b_] := TooltipBox[a,
  PaneBox[b,
    BaseStyle -> {FontSize -> 15, "Output"},
    ImageMargins -> {{5, 5}, {5, 5}},
    ImageSize -> {{30, 300}, {30, 300}}, Alignment -> Center,
    ImageSize -> 20
  ],
  TooltipStyle -> {Background -> GrayLevel[1], CellFrameColor -> None, CellFrame -> 0},
  TooltipDelay -> 0
] ~TagBox~ MouseAppearanceTag["Arrow"];

(**************************************************************************************************)

PublicFunction[ColorFramed]

ColorFramed[boxes_, color_] := Framed[boxes, ContentPadding -> False, FrameStyle -> color];

(**************************************************************************************************)

PublicFunction[LabelForm]

LabelForm[e_, args___] := Style[e, args, $LabelStyle];
