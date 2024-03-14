PublicTypesettingBoxFunction[DLinearChartBox]

Options[DLinearChartBox] = JoinOptions[
  $baseDPlotOptions,
  ChartLabels -> None
];

DLinearChartBox[data_, n_Integer, opts:OptionsPattern[]] := ModuleScope[

  $dvaluePrefix = "Linear";
  UnpackOptions[chartLabels];
  extraPadding = If[Len[chartLabels] =!= n, chartLabels = None; 0, {{0, 0}, {20, 0}}];
  UnpackAssociation[
    procDPlotOptions[DLinearChartBox, {opts, AdditionalImagePadding -> extraPadding}, "HeightFactor" -> 7.5],
    dFilter, dSelection, xScale, otherData
  ];

  gap = xScale * 3;
  {dData, dBinData, binCounts} = setupDBinData[data, n];
  total = Total @ binCounts;
  {dBinL, dBinR, binM, countToFrac} = setupBinFractions[binCounts, gap, False];

  plotBoxes = DBoxes @ MapThread[barBox[countToFrac, total], {dBinL, BinToCounts[Part[dData, dFilter], n]}];

  labelBoxes = If[chartLabels === None, {}, ZipMap[
    Cons[InsetBox, ToBoxes @ #1, {#2, 0}, ImageScaled[{0.5, 1.2}]]&,
    chartLabels, binM
  ]];

  dSelInd = DValue[None, "SelectedIndex"];
  selectionBoxes = DBoxes @ If[dSelInd === None, {}, RectangleBox[
    {Part[dBinL, dSelInd] - gap, $dFrameBottom},
    {Part[dBinR, dSelInd] + gap, $dFrameTop}
  ]];

  handler = makeDCoordsHandler[coordToBarID[dBinR],
    dSelInd    = If[IntQ[$] && $ =!= dSelInd, $, None],
    dSelection = If[IntQ[dSelInd], Part[dBinData, dSelInd], All]
  ];

  makeDGraphicsBox[
    "PlotBoxes"      -> plotBoxes,
    "LabelBoxes"     -> labelBoxes,
    "SelectionBoxes" -> selectionBoxes,
    "PlotRange"      -> {{0, 1}, {0, 1}},
    "FramePadding"   -> {{3, 3}, {6, 6}},
    "Frame"          -> LeftRight,
    "Handler"        -> handler,
    "OtherData"      -> otherData
  ]
];

_DLinearChartBox := BadArguments[];

(*************************************************************************************************)

barBox[_, _][x_, 0] :=
  Cons[TooltipBox, Cons[LineBox, {{x, 0.05}, {x, 0.95}}], "0"];

barBox[toFrac_, total_][x1_, count_] := Scope[
  x2 = x1 + toFrac * count;
  rect = Cons[RectangleBox, {x1, 0}, {x2, 1}];
  perc = If[count / total < .08, None, "\"" <> IntStr[Round[count / total * 100]] <> "%\""];
  If[perc =!= None,
    percBox = Cons[InsetBox, StyleBox[perc, GrayLevel[0, 0.7]], {Avg[x1, x2], .5}, ImageScaled[{0.5, 0.5}]];
    rect = {rect, percBox}
  ];
  Cons[TooltipBox, rect, IntStr @ count]
];

(*************************************************************************************************)

coordToBarID[binR_][{x_, y_}] := SelectFirstIndex[binR, GreaterThan[x], Len @ binR];

