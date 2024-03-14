PublicTypesettingForm[ModForm]

ModForm[x_, Inf|0] := x;

DefineStandardTraditionalForm[{
  ModForm[a_, b_] :> modBox[MakeBoxes @ a, b],
  ModForm[a_, b_List] :> RowBox[{MakeBoxes @ a, " % ", StyleBox[MakeBoxes @ b, $Blue]}]
}];

modBox[a_, b_] := SubscriptBox[a, StyleBox[numBox @ b, $Blue]];

(**************************************************************************************************)

PublicFunction[TightFramed]

TightFramed[expr_, opts___] := Framed[expr, ContentPadding -> False, FrameMargins -> None, opts];

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
    LabelStyle -> Pre[$LabelStyle, FontSize -> 16]
  ];


(**************************************************************************************************)

PublicTypesettingForm[EllipsisForm]

EllipsisForm[list_, n_] := If[Len[list] > n, App[Take[list, n], $LargeEllipsis], list];
EllipsisForm[n_][list_] := EllipsisForm[list, n];

(**************************************************************************************************)

PublicTypesettingFormBox[ClickForm]
PublicTypesettingFormBox[RightClickForm]

SetHoldRest[ClickForm, ClickBox, RightClickForm, RightClickBox];

MakeBoxes[ClickForm[elem_, fn_], form_] := ClickBox[MakeBoxes[elem, form], fn];
MakeBoxes[RightClickForm[elem_, fn_], form_] := RightClickBox[MakeBoxes[elem, form], fn];

Typeset`MakeBoxes[ClickForm[elem_, fn_], form_, type:Graphics|Graphics3D] := ClickBox[Typeset`MakeBoxes[elem, form, type], fn];
Typeset`MakeBoxes[RightClickForm[elem_, fn_], form_, type:Graphics|Graphics3D] := RightClickBox[Typeset`MakeBoxes[elem, form, type], fn];

$customGraphicsHeadQ[ClickBox] = True;
$customGraphicsHeadQ[RightClickBox] = True;

ClickBox[box_, None] := box;

ClickBox[box_, body_] := TagBox[
  TagBox[box, EventHandlerTag[{{"MouseClicked", 1} :> body, Method -> "Preemptive", PassEventsDown -> Auto, PassEventsUp -> True}]],
  MouseAppearanceTag["LinkHand"]
];

RightClickBox[box_, None] := box;

RightClickBox[box_, body_] := TagBox[
  TagBox[box, EventHandlerTag[{{"MouseClicked", 2} :> body, Method -> "Preemptive", PassEventsDown -> Auto, PassEventsUp -> True}]],
  MouseAppearanceTag["LinkHand"]
];

(**************************************************************************************************)

$customGraphicsHeadQ[NiceTooltip] = True;

(**************************************************************************************************)

PublicFunction[Flipper]

MakeBoxes[Flipper[a_, b_], form_] := FlipperBoxes[MakeBoxes[a, form], MakeBoxes[b, form]];

FlipperBoxes[a_, b_] :=
  DynamicModuleBox[
    {flippervar$$ = 1},
    TagBox[TagBox[
      FrameBox @ PaneSelectorBox[{1 -> a,  2 -> b}, Dynamic @ flippervar$$, ImageSize -> Auto, ImageMargins -> 5],
      EventHandlerTag[{
        "MouseClicked" :> Set[flippervar$$, Mod[flippervar$$ + 1, 2, 1]],
        "MouseDragged" :> Set[flippervar$$, Mod[flippervar$$ + 1, 2, 1]]
      }]
    ], MouseAppearanceTag["LinkHand"]],
    DynamicModuleValues -> {flippervar$$}
  ];

(**************************************************************************************************)

PublicFunction[TightRowGridBox, TightColumnGridBox]

TightRowGridBox[list_] :=
  GridBox[{list},
    GridBoxAlignment -> {"Columns" -> {{Left}}, "Rows" -> {{Top}}},
    GridBoxSpacings  -> {"Rows" -> {0, {0.5}, 0}, "Columns" -> {0, {0.5}, 0}},
    GridFrameMargins -> {{0, 0}, {0, 0}}
  ]

TightColumnGridBox[list_] :=
  GridBox[List /@ list,
    GridBoxAlignment -> {"Columns" -> {{Left}}, "Rows" -> {{Top}}},
    GridBoxSpacings  -> {"Rows" -> {0, {0.5}, 0}, "Columns" -> {0, {0.5}, 0}},
    GridFrameMargins -> {{0, 0}, {0, 0}}
  ];

(**************************************************************************************************)

PublicFunction[OpenerColumnBox]

OpenerColumnBox[a_] := a;

OpenerColumnBox[a_, b__] := With[
  {a1 = ClickBox[a, open$$ = False],
   a2 = ClickBox[a, open$$ = True]},
  {a1b = TightColumnGridBox[Pre[{b}, a1]]},
  DynamicModuleBox[
    {open$$ = 1},
    DynamicBox[
      If[TrueQ @ open$$, a1b, a2],
      TrackedSymbols :> {open$$}
    ],
    DynamicModuleValues -> {open$$}
  ]
];

OpenerColumnBox[a_, b_] := With[
  {a1 = ClickBox[a, open$$ = False],
   a2 = ClickBox[a, open$$ = True]},
  {a1b = TightColumnGridBox[Pre[{b}, a1]]},
  DynamicModuleBox[
    {open$$ = 1},
    DynamicBox[
      If[TrueQ @ open$$, a1b, a2],
      TrackedSymbols :> {open$$}
    ],
    DynamicModuleValues -> {open$$}
  ]
];

(**************************************************************************************************)

PublicFunction[DynamicProgressBarBox]

SetHoldFirst @ DynamicProgressBarBox;

DynamicProgressBarBox[{i_, n_}, {w_, h_}, color_:$LightPurple] := mouseMoveBox[
  DeployBox @ GraphicsBox[
    {color, RectangleBox[{0, 0}, {Dynamic @ i, 1}]},
    ImageSize -> {w, h}, PlotRange -> {{0, n}, {0, 1}}, PlotRangePadding -> 0,
    ImagePadding -> 0, AspectRatio -> Full
  ],
  Rep[
    MousePosition["Graphics"],
    {x_, y_} :> With[{z = Clip[Round @ x, {1, n}]}, If[z =!= i, Set[i, z]]]
  ]
];

SetHoldRest[mouseMoveBox]

mouseMoveBox[box_, body_] := TagBox[
  TagBox[box, EventHandlerTag[{"MouseClicked" :> body, "MouseDragged" :> body, Method -> "Preemptive", PassEventsDown -> Auto, PassEventsUp -> True}]],
  MouseAppearanceTag["FrameLRResize"]
];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[StyledClickBox]

SetHoldRest[StyledClickBox];

StyledClickBox[text_, action_] := StyledClickBox[text, action, Auto];
StyledClickBox[text_, action_, color_] := ClickBox[buttonBox[text, color], action];

buttonBox[e_, c_Str] := buttonBox[e, $bboxColors @ c];
buttonBox[e_, Auto] := buttonBox[e, {$LightBlue, $Blue}];
buttonBox[e_, c_] := buttonBox[e, {OklabLighter @ c, c}];
buttonBox[e_, {c1_, c2_}] := FrameBox[
  StyleBox[DeployBox @ e, Bold, 15],
  Background -> c1, FrameStyle -> c2,
  Alignment -> Baseline, FrameMargins -> {{5, 5}, {2, 0}}
];

$bboxColors = <|
  "Gray" -> {GrayLevel[0.9], $Gray},
  "Blue" -> {$LightBlue, $Blue},
  "Green" -> {$LightGreen, $Green},
  "Red" -> {$LightRed, $Red},
  "Pink" -> {$LightPink, $Pink},
  "LightPurple" -> {$LightPurple, $Purple},
  "Orange" -> {$LightOrange, $Orange}
|>;

(**************************************************************************************************)

PrivateTypesettingBoxFunction[DeployBox]

DeployBox[b_] := TagBox[b, "Deploy", DefaultBaseStyle -> "Deploy"];

(**************************************************************************************************)

PublicFunction[ColorFramed]

ColorFramed[boxes_, color_] := Framed[boxes, ContentPadding -> False, FrameStyle -> color];

(**************************************************************************************************)

PublicFunction[LabelForm]

LabelForm[e_, args___] := Style[e, args, $LabelStyle];
