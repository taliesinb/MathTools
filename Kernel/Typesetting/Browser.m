PublicTypesettingForm[ListBrowser]

Options[ListBrowser] = {
  ClickFunction -> None
};

MakeBoxes[ListBrowser[list_List, opts___Rule], form_] := listBrowserBoxes[list, opts];

listBrowserBoxes[list_List, opts:OptionsPattern[]] := With[
  {n$$ = Length @ list},
  {blue = $LightBlue, gray = GrayLevel[0.95], purple = $LightPurple},
  {clickFn = OptionValue[ListBrowser, {opts}, ClickFunction]},
  {itemBox = FrameBox[
    DynamicBox[
      ClickBox[
        ToBoxes @ Set[r$$, Part[list, i$$]],
        clickFn @ r$$
      ],
      TrackedSymbols :> {i$$}],
    FrameMargins -> 10, FrameStyle -> {Thick, $LightGray}
   ],
  buttonRowBox = makeStandardBrowseArrowBoxes[
    i$$, n$$,
    StyledClickBox["\[DownArrow]", PrintInputCell @ Part[list, i$$], "Pink"]
  ],
  progressBox = DynamicProgressBarBox[{i$$, n$$}, {200, 10}]},
  DynamicModuleBox[
    {i$$ = 1, r$$ = None},
    GridBox[
      {{buttonRowBox}, {progressBox}, {itemBox}},
      GridBoxSpacings -> {"Rows" -> {0.1, 0.1, {.5}}},
      GridBoxAlignment -> {"Columns" -> {{Left}}}
    ],
    DynamicModuleValues -> {i$$}
  ]
];

(**************************************************************************************************)

PublicTypesettingForm[LabeledFlipView]

MakeBoxes[LabeledFlipView[list_List, opts___Rule], form_] := labeledFlipViewBoxes[list, opts];

labeledFlipViewBoxes[list_List, opts___] := labeledFlipViewBoxes[MapIndex1[#2 -> #1&, list], opts];

LabeledFlipView::badLabelPos = "LabelPosition -> `` should be either Top or Left."
labeledFlipViewBoxes[items:{___Rule}, opts___] := With[
  {range = Range @ Length @ items, keys = ToBoxes /@ Keys @ items, vals = ToBoxes /@ Values @ items},
  {keys$$ = RuleThread[range, keys], vals$$ = RuleThread[range, vals]},
  {isize = Lookup[{opts}, ImageSize, Automatic], labelPos = Lookup[{opts}, LabelPosition, Top]},
  {togglerBox = StyleBox[TogglerBox[Dynamic[i$$], keys$$, ImageSize -> All], Bold, FontFamily -> "Fira", FontSize -> 10]},
  {paneSelectorBox = PaneSelectorBox[vals$$, Dynamic[i$$], ImageSize -> isize, Alignment -> {Left, Top}]},
  Switch[labelPos,
    Top,
    DynamicModuleBox[{i$$ = 1},
      GridBox[{{togglerBox}, {paneSelectorBox}}, ColumnAlignments -> Left, RowAlignments -> Center]
    ],
    Left,
    DynamicModuleBox[{i$$ = 1},
      GridBox[{{RotationBox[togglerBox, BoxRotation -> 1.5708], paneSelectorBox}}, ColumnAlignments -> Left, RowAlignments -> Left]
    ],
    _,
    Message[LabeledFlipView::badLabelPos, labelPos]; $Failed
  ]
];

(**************************************************************************************************)

PublicTypesettingForm[PickBrowser]

Options[PickBrowser] = Options[ListBrowser];

MakeBoxes[PickBrowser[list_List], form_] := pickBrowserBoxes[list];

pickBrowserBoxes[list_List, opts:OptionsPattern[]] := With[
  {n$$ = Length @ list},
  {blue = $LightBlue, gray = GrayLevel[0.95], purple = $LightPurple},
  {clickFn = OptionValue[PickBrowser, {opts}, ClickFunction]},
  {itemBox = FrameBox[
    DynamicBox[
      ClickBox[
        ToBoxes @ Set[r$$, Part[list, i$$]],
        selected$$ //= If[MemberQ[selected$$, i$$], DeleteCases[i$$], Append[i$$] /* Sort]
      ],
      TrackedSymbols :> {i$$}
    ],
    FrameMargins -> 10,
    FrameStyle -> Dynamic[
      If[MemberQ[selected$$, i$$], {Thick, $LightGreen}, {Thick, $LightGray}],
      TrackedSymbols :> {selected$$, i$$}
    ]
  ],
  buttonRowBox = makeStandardBrowseArrowBoxes[
    i$$, n$$,
    StyledClickBox["\[DownArrow]", PrintInputCell @ Part[list, i$$], "Pink"]
  ],
  progressBox =   DynamicProgressBarBox[{i$$, n$$}, {200, 10}],
  pickedButtonRowBox = makeBrowseArrowBoxes[
    {"Green", "Green"},
    i$$ = First[selected$$, i$$], i$$ = Replace[Max @ Select[selected$$, LessThan @ i$$], -Infinity -> i$$],
    fractionBox[IndexOf[selected$$, i$$, "?"], Length @ selected$$], None,
    i$$ = Replace[Min @ Select[selected$$, GreaterThan @ i$$], Infinity -> i$$], i$$ = Last[selected$$, i$$],
    StyledClickBox[
      "\[DownArrow]",
      CellPrint @ ExpressionCell[selected$$, "Input"],
      "Pink"
    ]
  ]},
  DynamicModuleBox[
    {i$$ = 1, r$$ = None, selected$$ = {}},
    GridBox[
      {{buttonRowBox}, {pickedButtonRowBox}, {progressBox}, {itemBox}},
      GridBoxSpacings -> {"Rows" -> {0.1, 0.1, {.5}}},
      GridBoxAlignment -> {"Columns" -> {{Left}}}
    ],
    DynamicModuleValues -> {i$$}
  ]
];

(**************************************************************************************************)

PublicFunction[MappedBrowser]

SetUsage @ "
MappedBrowser[f$, list$] maps f$ over list$, showing the results in an interactive browser.
"

MakeBoxes[MappedBrowser[f_, list_List], form_] := mappedBrowserBoxes[f, list];

mappedBrowserBoxes[f_, list_List] := With[
  {n$$ = Length @ list},
  {blue = $LightBlue, gray = GrayLevel[0.95], purple = $LightPurple},
  {itemBox = FrameBox[
    DynamicBox[ToBoxes @ Set[r$$, f @ Part[list, i$$]], TrackedSymbols :> {i$$}],
    FrameMargins -> 10, FrameStyle -> $LightGray],
  buttonRowBox = makeStandardBrowseArrowBoxes[
    i$$, n$$,
    StyledClickBox["\[DownArrow]", PrintInputCell @ deferSub[f, Part[list, i$$]], "Pink"],
    StyledClickBox["\[DownArrowBar]", PrintInputCell @ {Part[list, i$$], r$$}, "Pink"]
  ],
  progressBox = DynamicProgressBarBox[{i$$, n$$}, {200, 10}]},
  DynamicModuleBox[
    {i$$ = 1, r$$ = None},
    GridBox[
      {{buttonRowBox}, {progressBox}, {itemBox}},
      GridBoxSpacings -> {"Rows" -> {0.1, 0.1, {.5}}},
      GridBoxAlignment -> {"Columns" -> {{Left}}}
    ],
    DynamicModuleValues -> {i$$}
  ]
];

deferSub[f_, i_] := Apply[Defer, ConstructHoldComplete[f, i]];

(**************************************************************************************************)

SetHoldAll[makeBrowseArrowBoxes, makeStandardBrowseArrowBoxes]

makeStandardBrowseArrowBoxes[i_, n_, rest___] := makeBrowseArrowBoxes[
  {Automatic, "Gray"},
  i = 1, ModDecrement[i, n],
  fractionBox[i, n], If[CurrentValue["ShiftKey"], ModDecrement[i, n], ModIncrement[i, n]],
  ModIncrement[i, n], i = n,
  rest
]

fractionBox[i_, n_] := StyleBox[RowBox[{i, "/", n}], Plain, 14];

_makeStandardBrowseArrowBoxes := BadArguments[];

makeBrowseArrowBoxes[{col1_, col2_}, first_, prev_, main_, mainBody_, next_, last_, rest___] := RowBox[{
  StyledClickBox["\[LeftArrowBar]", first, col1],
  StyledClickBox["\[LeftArrow]",    prev, col1],
  StyledClickBox[
    DynamicBox @ PaneBox[main, ImageSize -> {82, 10}, Alignment -> Center, BaselinePosition -> Baseline],
    mainBody,
    col2
  ],
  StyledClickBox["\[RightArrow]",    next, col1],
  StyledClickBox["\[RightArrowBar]", last, col1],
  rest
}];

_makeBrowseArrowBoxes := BadArguments[];