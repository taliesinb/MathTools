PublicTypesettingBoxFunction[FrameTickLabelBoxes]

PublicOption[LabelSide, TickLength, TransformParameters, ClipEndLabels, MaxTicks]

SetUsage @ "
FrameTickLabelBoxes[plotRange$, {pos$x, pos$y}] constructs frame tick boxes for a given plotrange, at positions pos$x and pos$y.

* The following options are supported:
| Ticks               | {True, True} | whether to create ticks horizontal and/or vertical ticks |
| TickLength          | 0            | length of tick mark (in plot space) |
| LabelSpacing        | 5            | gap (in pixel space) between end of tick mark and label |
| LabelSide           | After        | which side to put label on (%After, %Before, %BeforeAfter) |
| LabelOrientation    | Horizontal   | absolute orientation (%Horizontal, %Vertical, %Aligned) |
| LabelPosition       | Center       | where to put the label relative to tick (%After, %Center, %Before) |
| TickLabels          | Automatic    | actual label values to use |
| ClipEndLabels       | True         | whether to clip the end label positions to be within bounds |

* TickLabels -> Automatic just uses the tick positions.
* TickLabels -> {n$1, n$2, $$} uses specific values.

* The options %FamilyFamily, %FontSize, %FontWeight, %FontColor apply to tick labels.
* FontColor -> {neg$, pos$} specifies different colors for negative and positive numbers.
"

Options[FrameTickLabelBoxes] = {
  Ticks               -> {True, True},
  MaxTicks            -> 20,
  TickLength          -> 0,
  TicksStyle          -> None,
  TickLabels          -> Automatic,

  LabelSide           -> After,
  LabelSpacing        -> 5,
  LabelPosition       -> Center,
  LabelOrientation    -> Horizontal,
  ClipEndLabels       -> 0.5,
  PrintPrecision      -> 3,

  FontFamily          -> "Source Code Pro",
  FontSize            -> 10,
  FontWeight          -> Plain,
  FontColor           -> {$Red, Black, $Blue}
};

FrameTickLabelBoxes::badOption = "Option setting `` -> `` is not valid.";

FrameTickLabelBoxes[plotRange_, tickPositions:{_, _}, OptionsPattern[]] := Scope @ CatchMessage[
  UnpackOptions[
    ticks, tickLength, ticksStyle, tickLabels, maxTicks,
    labelSide, labelSpacing, labelPosition, labelOrientation, clipEndLabels,
    fontFamily, fontSize, fontWeight, fontColor, printPrecision
  ];

  doTicks = ToPair @ ticks;
  If[doTicks === {False, False}, Return @ {}];

  tickLabelStyle = Seq[FontFamily -> fontFamily, FontSize -> fontSize, FontWeight -> fontWeight];
  signColors = None;
  Switch[fontColor,
    {_, _, _}, signColors = fontColor,
    $ColorP,   AppendTo[tickLabelStyle, FontColor -> fontColor],
    None,      Null,
    _,         OptionMsg[FontColor, fontColor]
  ];

  If[ListQ[tickLabels] && !MatchQ[tickLabels, {_List, _List}], OptionMsg[TickLabels, tickLabels]];
  {labelBoxes, tickBoxes} = Transpose @ ZipMap[
    makeTicks,
    MapSequence[ToPair, labelSide, labelSpacing, labelPosition, tickLength, tickLabels, maxTicks],
    tickPositions, Rev @ plotRange, plotRange, {True, False}, doTicks
  ];

  If[tickBoxes =!= {{}, {}} && ticksStyle =!= None,
    tickBoxes = StyleBox[tickBoxes, ticksStyle]
  ];

  labelBoxes = StyleBox[labelBoxes, tickLabelStyle, $baseTickOptions];

  {tickBoxes, labelBoxes}
];

(**************************************************************************************************)

$baseTickOptions = Seq[
  ScriptSizeMultipliers -> 0.75, ScriptMinSize -> 0.5, AutoSpacing -> False, ScriptBaselineShifts -> {0.2, 0.31}
];

FrameTickLabelBoxes::badTickLength = "TickLabels length `` doesn't match coordinate length ``.";

(**************************************************************************************************)

makeTicks[BeforeAfter, args__] := Catenate /@ Trans[makeTicks[Before, args], makeTicks[After, args]];

makeTicks[labelSide_, spacing_, labelPos_, tickLen_, tickLabels_, maxTicks_, varCoords_, fixedRange_, orthFixedRange_, isX_, doTicks_] := Scope[

  If[!doTicks || varCoords === {}, Return @ {{}, {}}];

  n = Len[varCoords];
  tickInds = Range[n];
  tickInds = If[n <= maxTicks, Range @ n, Range[1, n, Ceiling[n / maxTicks]]];

  (* everything is written wrt to x ticks above, and we compensate, orth: orthog to tick lines: parr: along tick lines *)
  orth = If[isX, 1, -1];
  parr = orth * Switch[labelSide, Before, 1, After, -1, _, OptionMsg[LabelSide, side]];
  sgnVec = {orth, parr};

  rev = If[isX, Id, Rev];
  fixedCoord = If[parr == 1, L, F] @ fixedRange;

  If[tickLen > 0.,
    tickStartCoords = Thread @ rev[{varCoords, fixedCoord}];
    fixedCoord += parr * tickLen;
  ];

  SetAuto[tickLabels, varCoords];
  SameLenMsg::badTickLength[tickLabels, varCoords];

  labelBoxes = NiceDivisionStringBoxes[tickLabels, printPrecision, signColors];

  Switch[labelPos,
    Before, offset = {-1,  0}; anchor = { 1,  1},
    Center, offset = {0,   1}; anchor = { 0, -1},
    After,  offset = {1,   0}; anchor = {-1,  1},
    _,      OptionMsg[LabelPosition, labelPos];
  ];
  offset *= sgnVec; anchor *= sgnVec;
  offset = rev[offset * spacing];
  anchor = ImageScaled[rev @ (anchor/2 + .5)];

  labelCoords = Thread @ rev[{varCoords, fixedCoord}];
  offsetCoords = Offset[offset, #]& /@ labelCoords;

  labelInsetBoxes = ZipMap[
    Cons[InsetBox, #1, Offset[offset, #2], anchor]&,
    Part[labelBoxes, tickInds], Part[labelCoords, tickInds]
  ];
  labelInsetBoxes //= MapFirstLast[If[isX, fixScientificOffset1, fixScientificOffset2]];
  If[clipEndLabels =!= 0 && labelPos === Center,
    (* target the appropriate coordinate of the ImageScaled *)
    i = If[isX, 1, 2];
    endValue = clipEndLabels / 2;
    delta = (Dist @@ orthFixedRange) / 100;
    {c1, c2} = FirstLast @ varCoords;
    If[Min[Dist[c1, #]& /@ orthFixedRange] < delta, labelInsetBoxes //= ReplacePart[{1, 3, 1, i} -> 0.5 - endValue]];
    If[Min[Dist[c2, #]& /@ orthFixedRange] < delta, labelInsetBoxes //= ReplacePart[{-1, 3, 1, i} -> 0.5 + endValue]];
  ];

  tickBoxes = If[tickLen > 0., Cons[LineBox, Trans[tickStartCoords, labelCoords]], {}];
  {labelInsetBoxes, tickBoxes}
];

fixScientificOffset1[other_] := other;
fixScientificOffset1[InsetBox[RowBox[{n_, t_, p_}], pos_, ImageScaled[{x_, 1.}]]] :=
  Cons[InsetBox, GridBox[{{n}, {p}}, RowSpacings -> 0], pos, ImageScaled[{x, 1.08}]];

fixScientificOffset1[InsetBox[RowBox[{n_, t_, p_}], pos_, ImageScaled[{x_, 0.}]]] :=
  Cons[InsetBox, GridBox[{{p}, {n}}, RowSpacings -> 0], pos, ImageScaled[{x, 0.1}]];

fixScientificOffset2[other_] := other;
fixScientificOffset2[InsetBox[r_RowBox, pos_, ImageScaled[{x_, y_}]]] :=
  Cons[InsetBox, r, pos, ImageScaled @ sciScaled[x, y]];

Clear[sciScaled];
sciScaled[0.5, 1.0] := {0.3, 1};
sciScaled[0.5, 0.0] := {0.3, 0};
sciScaled[0.0, 0.5] := {0.0, 0.4};
sciScaled[1.0, 0.5] := {0.9, 0.4};
zsciScaled[x_, y_] := {x, y};
