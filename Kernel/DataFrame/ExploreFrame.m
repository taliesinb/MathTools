PublicFunction[ExploreFrame]

(*
TODO: make ExploreFrame emit things left-to-right until they hit a max.

they should be put in panes to prevent jumping around.
have it work on ordinary data, allowing keys to be integers for columns.
this will allow easy testing!

add recognition of consecutive non-repeating integers, produce a pixel plot showing
which are occupied and which aren't.

support for dates and times in all kinds of ways!
*)

ExploreFrame[frame_List | frame_Assoc, queries__] :=
  ExploreFrame[DataFrame[frame], queries];

ExploreFrame[frame_ ? DataFrameQ, queries__] := Scope @ CatchMessage[
  $colRules = dataFrameColRules @ frame;
  $colNames = Keys @ $colRules;
  queries = {queries};
  num = Len @ queries;
  $i = 0;
  DModule[{selections, filters, boxes},
    selections = DValue[#, "Selection"]& /@ Repeat[All, num];
    filters = toFilters @ selections;
    boxes = ZipMap[procQuery, queries, selections, filters];
    rows = Partition[boxes, UpTo @ 3];
    rows = PadRight[rows, Automatic, ""];
    StyleBox[GridBox[rows, $exploreGridOpts], Selectable -> False]
  ] // RawBoxes
];

$exploreGridOpts = Seq[
  ColumnAlignments -> Left, RowAlignments -> Top,
  ColumnSpacings -> 2,
  ItemSize -> All,
  FrameStyle -> GrayLevel[0.9],
  GridBoxDividers -> {"Columns" -> {False, {True}, False}, "Rows" -> {False, {True}, False}}
];

toFilters = Case[
  {a_}      := {All};
  {a_, b_}  := {b, a};
  sels_List := Table[
    DHold[PartIntersection[##]]& @@ Delete[sels, i],
    {i, Len @ sels}
  ];
];

(**************************************************************************************************)

PublicFunction[PartIntersection]

PartIntersection[All...] := All;
PartIntersection[a___] := Intersection @@ DeleteCases[{a}, All];

(**************************************************************************************************)

procQuery[query_, selection_, filter_] := Scope[
  results = Map[procQueryElem, ToList @ query];
  names = Col2 @ results;
  name = StringRiffle[names, " vs "];
  $dopts = Seq[DynamicFilter -> filter, DynamicSelection -> selection, PlotLabel -> name];
  exploreResult @@ Map[procQueryElem, ToList @ query]
];

$colID = _Int | _String;

procQueryElem = Case[
  name:$colID        := evalQuery[name, Id];
  name:$colID -> fn_ := evalQuery[name, fn];
];

ExploreFrame::badDataTrans = "Transformation of column `` yielded messages or wrong length.";
ExploreFrame::inferFailure = "Cannot infer type of column ``.";

procCompoundFn = Case[
  i_Int        := PartOperator[i];
  fn_          := fn;
  fn1_ -> fn2_ := fn1 /* %[fn2];
];

ExploreFrame::badColIndex = "No column with name or index ``.";

evalQuery[i_Int, fn_] := Scope[
  name = IntStr @ i;
  If[!KeyExistsQ[$colRules, name],
    name = PartMsg::badColIndex[$colNames, i]];
  evalQuery[name, fn]
];

ExploreFrame::badColName = "No column with name or index ``.";

evalQuery[name_, fn_] := Scope[
  data = ToPacked @ LookupMsg::badColName[$colRules, name];
  olen = Len @ data;
  fn //= procCompoundFn;
  If[fn =!= Id,
    data = ToPacked @ Check[Map[fn, data], $Failed];
    If[!ListQ[data] || olen =!= Len[data], Msg::badDataTrans[name]];
  ];
  data2 = DeleteCases[data, _Missing | None];
  n = Len @ data2;
  sample := sample = If[n < 100, data2, RandomSample[data2, 50]];
  labelOr = Function[t, If[DuplicateFreeQ[sample] || Max[Counts @ sample] < Max[Min[n, 100] * .05, 3], t, Label]];
  type = Which[
    VectorQ[data2, IntegerQ], labelOr[Integer],
    VectorQ[data2, NumberQ],  Number,
    VectorQ[data2, StringQ],  labelOr[String],
    VectorQ[data2, BooleanQ], Boole,
    VectorQ[data2, SymbolQ],  labelOr[Symbol],
    True,                     Msg::inferFailure[name];
  ];
  {type, name, data}
];

(**************************************************************************************************)

ExploreFrame::autoFailure = "Cannot automatically visualize column(s) of type: ``.";
exploreResult[tuples___List] := Msg::autoFailure[Col1 @ List[tuples]];

$numInt = Number|Integer;

exploreResult[{$numInt, name1_, data1_}, {$numInt, name2_, data2_}] := Scope[
  {xy, plotRange} = toPackedXY[data1, data2];
  DScatterPlotBox[xy, PlotRange -> plotRange, $dopts]
];

exploreResult[{$numInt, name1_, data1_}, {$numInt, name2_, data2_}, {Label, name3_, data3_}] := Internal`InheritedBlock[
  AppendTo[$dopts, DataLabels -> data3];
  exploreResult[{Number, name1, data1}, {Number, name2, data2}]
];

ExploreFrame::badCoords = "Could not form coordinates from columns.";
toPackedXY[data1_, data2_] := Scope[
  pairs = ToPacked @ Transpose[{data1, data2}];
  If[PackedArrayQ[pairs], Return @ {pairs, Automatic}];

  {bl, tr} = plotBox = CoordinateBoundingBox[Select[pairs, NumericVectorQ], Scaled[0.05]];
  dummy = Lerp[bl, tr, .99];
  pairs //= Map[If[NumericVectorQ[#], #, dummy]&] /* ToPacked;
  Assert::badCoords @ PackedArrayQ[pairs];
  {pairs, Transpose @ plotBox}
];

(**************************************************************************************************)

(* TODO: distinguish int bins from real bins, int bins should have int label go under the bin *)
exploreResult[{$numInt, name_, data_}] := Scope[
  data //= ToPacked;
  containsNone = containsNoneQ[data];
  pureData = If[containsNone, DeleteNone @ data, data];
  pureRange = MinMax @ pureData;
  divs = NiceBinDivisions[pureRange, 20];
  {min, max} = FirstLast @ divs;
  n = Len[divs]; d = Dist[min, max] / 10000;
  binInts = Round @ Rescale[N @ data, {min-d, max+d}, {1, n}];
  colorRules = {};
  If[containsNone, n++;
    binInts //= VectorReplace[_Round -> n];
    colorRules = {n -> $dPlotNoneColor};
    AppendTo[divs, $dPlotNoneSymbol];
  ];
  DHistogramBox[ToPacked @ binInts, n, ColorRules -> colorRules, BinTicks -> divs, $dopts]
];

(**************************************************************************************************)

exploreResult[{Boole, name_, data_}] := Scope[
  n = 2;
  ints = 2 - Boole[data]; (* 1 is True, 2 is False, 3 is other (e.g. None) *)
  colorRules = {1 -> $LightGreen, 2 -> $LightRed};
  chartLabels = {"T", "F"};
  If[!IntegerVectorQ[ints], n = 3;
    ints //= VectorReplace[_Plus -> n];
    AppendTo[colorRules, 3 -> $dPlotNoneColor];
    AppendTo[chartLabels, $dPlotNoneSymbol];
  ];
  DLinearChartBox[
    ints, n,
    ColorRules -> colorRules,
    ChartLabels -> chartLabels,
    $dopts
  ]
];

(**************************************************************************************************)

exploreResult[{String, name_, data_}] := Module[
  {dFilter, dData},
  dFilter = Lookup[{$dopts}, DynamicFilter];
  dData = DValue[data, "PaneData"];
  DBoxes @ stringListColBoxes[Part[dData, dFilter], 450, 200]
];

$paneBoxBaseStyle = {
  FontSize                -> 12,
  ShowAutoStyles          -> False,
  ShowStringCharacters    -> False,
  LineSpacing             -> {1, 0},
  AutoIndent              -> False,
  AutoSpacing             -> False,
  Hyphenation             -> False,
  FontWeight              -> "DemiBold",
  NumberMarks             -> False,
  ShowInvisibleCharacters -> True
};

stringListRowBoxes[list_, limit_, w_, h_] := Scope[
  rem = limit;
  res = TakeWhile[list, !StrQ[#] || (rem -= Max[SLen[#], 30]) > 0&];
  res = Map[toStrBoxes[#, 30]&, res];
  If[Len[res] < Len[list], AppendTo[res, "\[Ellipsis]"]];
  PaneBox[
    RowBox @ Riffle[res, ", "],
    BaseStyle -> $paneBoxBaseStyle,
    ImageSize -> {w, h}
  ]
];

$fontW = 6.8; $fontH = 16;
stringListColBoxes[list_, w_, h_] := Scope[
  wLimit = Floor[w / ($fontW + 1)] - 1;
  hLimit = Ceiling[h / ($fontH)];
  res = Map[toStrBoxes[#, wLimit]&, Take[list, UpTo @ hLimit]];
  If[Len[res] < Len[list], AppendTo[res, "\[Ellipsis]"]];
  If[res === {}, res = {"\[LongDash]"}];
  PaneBox[
    GridBox[
      Map[List, res], ColumnAlignments -> Left,
      RowSpacings -> 0, RowMinHeight -> 1.2,
      BaseStyle -> $paneBoxBaseStyle
    ],
    ImageSize -> {w + 5, h + 5}
  ]
];

toStrBoxes[e_, lim_]    := ToBoxes @ e;
toStrBoxes[e_Str, lim_] := StyleBox[ToBoxes @ shortenStrLen[e, lim], Background -> GrayLevel[0.9]];
shortenStrLen[s_Str, lim_] /; StringLength[s] > lim := StringTake[s, Floor[lim*2/3]] <> "\[CenterEllipsis]" <> StringTake[s, -Floor[lim * 1/3]];
shortenStrLen[e_, _] := e;

(**************************************************************************************************)

exploreResult[{Label, name_, data_}] := Scope[
  containsNone = containsNoneQ[data];
  If[containsNone, data //= VectorReplace[_Missing -> None]];
  counts = DeleteNone @ Counts @ data;
  maxCount = Max[counts];
  cutoff = Total[counts] / 100.;
  mergeKeys = Keys @ Select[counts, LessThan[cutoff]];
  If[Len[mergeKeys] === 1, mergeKeys = {}];
  keys = Keys @ ReverseSort @ KeyDrop[mergeKeys] @ counts;
  n = Length @ keys;
  chartLabels = Map[shortenStr, keys];
  colorRules = {};
  defaultKey = $Failed; (* this should never be used unless we merged small-count keys *)
  If[mergeKeys =!= {},
    n++; defaultKey = n;
    AppendTo[chartLabels, NiceTooltip["\[VerticalEllipsis]", mergeKeys]];
    AppendTo[colorRules, n -> GrayLevel[0.9]];
  ];
  If[containsNone,
    n++;
    AppendTo[chartLabels, $dPlotNoneSymbol];
    AppendTo[colorRules, n -> $dPlotNoneColor];
  ];
  dict = UAssoc @ AssociationRange @ keys;
  ints = ToPacked @ Lookup[dict, data, defaultKey];
  If[!IntegerVectorQ[ints], Return[$Failed]];
  DLinearChartBox[ints, n, ChartLabels -> chartLabels, ColorRules -> colorRules, $dopts]
];

shortenStr[s_Str] /; StringLength[s] > 10 := StringTake[s, 6] <> "\[CenterEllipsis]" <> StringTake[s, -3];
shortenStr[e_] := e;

(* stringBinner[name_, data_] := Scope[
  counts = Counts @ data;
  If[Max[counts] < Max[Length[data] * 0.05, 3], Return[None]];
  keys = Keys @ ReverseSort @ counts;
  dict = AssociationRange @ keys;
  ints = Lookup[dict, data];
  n = Length @ keys;
  DPieChartBox[DValue[ints, "pieData"], n, $opts]
]; *)