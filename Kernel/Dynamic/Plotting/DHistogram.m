PublicTypesettingBoxFunction[DHistogramBox]

Options[DHistogramBox] = JoinOptions[
  $baseDPlotOptions,
  BinTicks -> None
];

DHistogramBox[data_, n_Integer, opts:OptionsPattern[]] := ModuleScope[

  $dvaluePrefix = "Hist";
  UnpackOptions[binTicks];
  extraPadding = If[ListQ[binTicks], Bottom -> 20, 0];
  UnpackAssociation[
    procDPlotOptions[DHistogramBox, {opts, AdditionalImagePadding -> extraPadding}, "HeightFactor" -> 2],
    dFilter, dSelection, xScale, yScale, otherData
  ];

  (* this is a static DValue to save space and time *)
  {dData, dBinData, binCounts} = setupDBinData[data, n];
  countToFrac = 1.0 / Max[binCounts];
  plotRange = {{1, n+1}, {0, 1}};

  dx = 3 * n * xScale;
  plotBoxes = DBoxes @ MapIndex1[{h, i} |-> rectBox[i, dx, countToFrac, h], BinToCounts[Part[dData, dFilter], n]];

  dL = DValue[-1, "L"];
  dR = DValue[-1, "R"];
  dLast = DValue[None, "Last"];
  dCurr = DValue[None, "Curr"];
  handler = makeDCoordsHandler[coordToBinID[n],
    dLast = dCurr = $;
    dL = dR = If[dL <= $ <= dR, -1, $]
  ,
    If[$ =!= dCurr, dCurr = $;
      {dL, dR} = MinMax @ {dLast, dCurr};
      dSelection = calcHistSelection[dL, dR, dBinData]
    ]
  ,
    dSelection = calcHistSelection[dL, dR, dBinData]
  ];

  selectionBoxes = DBoxes @ RectangleBox[{dL - dx, $dFrameBottom}, {dR + 1, $dFrameTop}];

  If[ListQ[binTicks],
    If[Len[binTicks] =!= n, Return[$Failed]];
    tickCoords = Range[n] + 0.5 - dx/2;
    tickLabels = {binTicks, {}};
    tickBoxes = FrameTickLabelBoxes[plotRange - {0, .01}, {tickCoords, {}},
      TickLabels -> tickLabels, PrintPrecision -> 2, TickLength -> 0,
      TicksStyle -> AbsoluteThickness[0.5],
      ClipEndLabels -> 0, MaxTicks -> 10, LabelSpacing -> 0
    ];
  ,
    tickBoxes = {}
  ];

  graphicsBoxes = makeDGraphicsBox[
    "PlotBoxes"      -> plotBoxes,
    "SelectionBoxes" -> selectionBoxes,
    "LabelBoxes"     -> tickBoxes,
    "PlotRange"      -> plotRange,
    "FramePadding"   -> {{3, 0}, {3, 3}},
    "Handler"        -> handler,
    "OtherData"      -> otherData
  ]
];

_DHistogramBox := BadArguments[];

(*************************************************************************************************)

rectBox[x_, dx_, toFrac_, 0] := {};
rectBox[x_, dx_, toFrac_, h_] := Cons[TooltipBox, Cons[RectangleBox, {x, 0}, {x + 1 - dx, h * toFrac}], IntStr @ h];

(*************************************************************************************************)

coordToBinID[n_][{x_, y_}] := Floor @ Clip[x, {1, n}];

calcHistSelection[dL_, dR_, dBinData_] := If[dL < 0, All, Flatten @ Take[dBinData, {dL, dR}]];



