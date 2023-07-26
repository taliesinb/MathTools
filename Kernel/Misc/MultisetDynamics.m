rulesToRates[rules_, vars_] := rulesToRates[rules, vars] = MapAt[N, 2] @ Construct[Function, vars, Keys @ rules];
rulesToEffects[rules_, vars_] := rulesToEffects[rules, vars] = variablePowers[#2, vars] - variablePowers[#1, vars]& @@@ rules
variablePowers[expr_, vars_] := CoefficientRules[expr, vars][[1, 1]]

(**************************************************************************************************)

PublicFunction[MultisetDynamics]

MultisetDynamics[rules_, init_, T_] := Scope[
  vars = Keys[init];
  rates = rulesToRates[rules, vars];
  effects = rulesToEffects[rules, vars];
  state = Values[init]; nrules = Length[rules]; indices = Range[nrules]; steps = 0; tweight = 0;
  list = {}; t = 0; times = Internal`Bag[{0.}]; states = Internal`Bag[{state}]; densities = Internal`Bag[];
  While[t <= T && steps++ < 10^8,
    weights = rates @@ state;
    If[Total[weights] == 0, Break[]];
    index = If[nrules === 1, 1, RandomChoice[weights -> indices]];
    state = Ramp[state + effects[[index]]];
    tweight = Total[weights];
    If[tweight > 10^9, Break[]];
    t += RandomVariate[ExponentialDistribution[tweight]];
    Internal`StuffBag[states, state];
    Internal`StuffBag[times, t];
    Internal`StuffBag[densities, tweight];
  ];
  states = Internal`BagPart[states, All];
  times = Internal`BagPart[times, All];
  densities = Internal`BagPart[densities, All];
  If[MatchQ[Dimensions[states], {_, 1}], states = Flatten[states]];
  metadata = {"EventDensities" -> densities, "InitialEventDensity" -> First[densities], "TimePeriod" -> T};
  TimeSeries[states, {times}, MetaInformation -> metadata]
]
✂️🪨📜
(**************************************************************************************************)

PublicFunction[MultisetDynamicsEnsemble]

MultisetDynamicsEnsemble[rules_, init_, t_, n_] := Scope[
  series = Table[MultisetDynamics[rules, init, t], {n}];
  TemporalData[series, MetaInformation -> {
    "InitialEventDensity" -> First[series]["InitialEventDensity"],
    "TimePeriod" -> T
  }
]];

(**************************************************************************************************)

PublicFunction[MultisetDynamicsEnsemblePlot]

MultisetDynamicsEnsemblePlot[rules_, init_, t_, n_:100, loglog_:None] := Scope[
  ensemble = MultisetDynamicsEnsemble[rules, init, t, n];
  logx = MatchQ[loglog, "X" | "XY"];
  quantiles = TemporalDataQuantiles[ensemble, 50, logx];
  plot = TemporalDataQuantilePlot[quantiles, loglog];
  legend = LineLegend[Take[$NormalColorPalette, Length[init]], Keys[init]];
  Legended[plot, legend]
]

Power10[e_] := Power[10, e];

(**************************************************************************************************)

PublicFunction[TimeSeriesTransformTimes]

TimeSeriesTransformTimes[f_, series_] := TemporalData @ MapAt[f, series["Paths"], {All, All, 1}]

toQuant[e_ ? MatrixQ] := Map[toQuant, Transpose[e]];
toQuant[e_] := Quantile[N[e], {0.05, 0.95, 0.1, 0.9, 0.2, 0.8, 0.3, 0.7, 0.4, 0.6, 0.5}];

(**************************************************************************************************)

PublicFunction[TemporalDataQuantiles]

TemporalDataQuantiles[series_, n_:50, logx_:False] := Scope[
  If[logx && NumberQ[edensity = Quiet @ Check[series["InitialEventDensity"], None]],
    tStart = 1.0 / edensity;
    lastTime = Quiet @ Check[N @ series["TimePeriod"], series["LastTime"]];
    series = TimeSeriesWindow[series, {tStart, lastTime}, IncludeWindowTimes -> True];
    tStart = Log10[tStart];
    tEnd = Log10[lastTime];
    series = TimeSeriesTransformTimes[Log10, series];
  ,
    tStart = series["FirstTime"];
    tEnd = series["LastTime"];
  ];
  dt = (tEnd - tStart) / n;
  series = Quiet @ TimeSeriesAggregate[series, dt];
  resampled = Quiet @ TimeSeriesResample[series, {tStart, tEnd, dt}];
  result = TimeSeriesThread[toQuant, resampled["Components"]];
  If[logx, result = TimeSeriesTransformTimes[Power10, result]];
  result
];

quantileListPlot[times_, lists_, color_, {xlog_, ylog_}, opts___] := ListPlot[
  Map[Transpose[{times, #}]&] @ Transpose @ lists,
  Filling -> {
    1 -> {{2}, Opacity[0.1, color]},
    3 -> {{4}, Opacity[0.1, color]},
    5 -> {{6}, Opacity[0.1, color]},
    7 -> {{8}, Opacity[0.2, color]},
    9 -> {{10}, Opacity[0.3, color]}
  }, PlotStyle -> {None, None, None, None, None, None, None, None, None, None, color},
  Joined -> True, PlotRange -> All, ScalingFunctions -> {If[xlog,"Log10",None], If[ylog,"Log",None]},
  Frame -> True, PerformanceGoal -> "Speed",
  opts
];

(**************************************************************************************************)

PublicFunction[TemporalDataQuantilePlot]

TemporalDataQuantilePlot[tdata_, loglog_:None] := Scope[
  dims = tdata["ValueDimensions"];
  If[!MatchQ[dims, {_, 11} | 11], Return[$Failed]];
  components = If[Length[dims] == 2, First[dims], None];
  values = tdata["ValueList"];
  If[components =!= None, values = Transpose @ First @ values];
  times = First @ tdata["TimeList"];
  max = Max[values];
  {xlog, ylog} = xylog = Switch[loglog, None, {False, False}, "X", {True, False}, "Y", {False, True}, "XY", {True, True}];
  plots = MapIndexed[
    quantileListPlot[times, #, Part[$NormalColorPalette, First[#2]], xylog]&,
    values
  ];
  If[components === None, Return @ First @ plots];
  Show[plots, Frame -> True, PlotRange -> All]
];