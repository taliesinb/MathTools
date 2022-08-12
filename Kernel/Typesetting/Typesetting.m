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
(* MakeBoxes[ClickForm[elem_, fn_], TraditionalForm] := ClickBox[MakeBoxes[elem, TraditionalForm], fn];
 *)
Typeset`MakeBoxes[ClickForm[elem_, fn_], form_, type:Graphics|Graphics3D] := ClickBox[Typeset`MakeBoxes[elem, form, type], fn];

ClickBox[box_, body_] := TagBox[
  TagBox[box, EventHandlerTag[{"MouseClicked" :> body, Method -> "Preemptive", PassEventsDown -> Automatic, PassEventsUp -> True}]],
  MouseAppearanceTag["LinkHand"]
];

(**************************************************************************************************)

PublicFunction[NiceTooltip]

NiceTooltip[g_, None] := g;

NiceTooltip[g_, e_] :=
  Tooltip[g,
    Pane[e, 20, BaseStyle -> {FontSize -> 15, "Output"},
      ImageMargins -> {{10, 10}, {5, 5}}, ImageSize -> {{30, 300}, {30, 300}},
      Alignment -> Center
    ],
    TooltipStyle -> {Background -> White, CellFrameColor -> None, CellFrame -> 0}
  ];

(**************************************************************************************************)

PublicFunction[ColorFramed]

ColorFramed[boxes_, color_] := Framed[boxes, ContentPadding -> False, FrameStyle -> color];

(**************************************************************************************************)

PublicFunction[LabelForm]

LabelForm[e_, args___] := Style[e, args, $LabelStyle];
