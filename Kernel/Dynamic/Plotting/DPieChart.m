PublicTypesettingBoxFunction[DPieChartBox]

Options[DPieChartBox] = $baseDPlotOptions;

DPieChartBox[data_, n_Integer, opts:OptionsPattern[]] := ModuleScope[

  $dvaluePrefix = "Pie";
  UnpackAssociation[
    procDPlotOptions[DPieChartBox, {opts}, "ScaleFactor" -> .5],
    dFilter, dSelection, otherData, xScale
  ];

  {dData, dBinData, binCounts} = setupDBinData[data, n];

  totalCount = Total @ binCounts;

  gap = 1. * xScale; (* what fraction gap corresponds to 3 points at radius of pies? *)
  {dBinL, dBinR, binM, countToFrac} = setupBinFractions[binCounts, gap, True];

  plotBoxes = DBoxes @ MapThread[sliceBox, {dBinL, countToFrac * BinToCounts[Part[dData, dFilter], n]}];

  dSelInd = DValue[None, "SelectedIndex"];
  selectionBoxes = DBoxes @ If[dSelInd === None, {},
    cursorBox[Part[dBinL, dSelInd] - gap, Part[dBinR, dSelInd] + gap]];

  handler = makeDCoordsHandler[getSectorID[dBinR],
    dSelInd = If[IntQ[$] && $ =!= dSelInd, $, None],
    dSelection = If[IntQ[dSelInd], Part[dBinData,dSelInd], All]
  ];

  makeDGraphicsBox[
    "PlotBoxes"      -> plotBoxes,
    "SelectionBoxes" -> selectionBoxes,
    "PlotRange"      -> {{-1, 1}, {-1, 1}},
    "Frame"          -> False,
    "Handler"        -> handler,
    "OtherData"      -> otherData
  ]
];

_DPieChartBox := BadArguments[];

(*************************************************************************************************)

$ang0 = -N[Pi];

annulusBox[ri_, ro_, f1_, f2_] :=
  ToGraphicsBoxes @ Annulus[{0, 0}, {ri, ro}, Sort[$ang0 - {f1, f2}*2Pi]];

sliceBox[_, 0.]           := {};
sliceBox[frac_, dFrac_]   := annulusBox[0.8, 1.0, frac, frac + dFrac];
cursorBox[frac1_, frac2_] := annulusBox[0.75, 1.05, frac1, frac2];

(*************************************************************************************************)

coordToFrac[{x_, y_}] := Mod[(-$ang0 - ArcTan2[x, y])/(2.*Pi), 1];

getSectorID[fracs_][xy_] := If[0.6 < Norm[xy] < 1.1,
  SelectFirstIndex[fracs, GreaterThan[coordToFrac @ xy], Len @ fracs],
  None
];

