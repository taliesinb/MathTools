PrivateFunction[ToSquarePlotRange]

ToSquarePlotRange[{{x1_, x2_}, {y1_, y2_}}] := Scope[
  w = x2 - x1; h = y2 - y1;
  d2 = Max[w, h] / 2; x = Avg[x1, x2]; y = Avg[y1, y2];
  {{x - d2, x + d2}, {y - d2, y + d2}}
];

(**************************************************************************************************)

PrivateFunction[NormalizePlotRange]

SetUsage @ "
NormalizePlotRange[graphics$] updates the value of %PlotRange to span all elements in graphics$.
* graphics$ can be a %Graphics[$$], %Graphics3D[$$], or %Graph[$$] expression.
* The value of %PlotRangePadding is taken into account, and %PlotRangePadding is set to zero in the result.
* Providing the option %PlotRangePadding will override the %PlotRangePadding present in graphics$.
"

Options[NormalizePlotRange] = {
  PlotRangePadding -> Inherited,
  ExternalPadding -> None
};

NormalizePlotRange[graphics_, OptionsPattern[]] := Scope[
  CheckIsGraphics[1];
  UnpackOptions[plotRangePadding];
  plotRange = iGraphicsPlotRange[graphics];
  ReplaceOptions[graphics, {
    PlotRangePadding -> 0,
    PlotRange -> plotRange
  }]
];

(**************************************************************************************************)

PublicFunction[GraphicsPlotRange]

SetUsage @ "
GraphicsPlotRange[graphics$] yields the plot range that will be used when graphics$ is rendered.

* the range is given as {{x$min, x$max}, {y$min, y$max}}.

* graphics$ can be a %Graphics[$$], %Graphics3D[$$], %FixedGraphics[$$], or %Graph[$$] which will be boxified.

* graphics$ can also %GraphicsBox[$$], %Graphics3DBox[$$].

* graphics$ can also be a list of graphics primitives.

* %Legended, %Labeled, etc will be skipped.

* %Epilog and %Prolog are not included.

* the option %PlotRangePadding specifies whether padding should be included in the resulting range, \
and has the following settings:
| None | do not include any padding (default) |
| Inherited | apply the padding specified in the graphics object |
| custom$ | apply the custom padding |

* padding is applied used %PlotRangePad.

* if an existing PlotRange is already set, it is returned, with padding applied.
"

Options[GraphicsPlotRange] = {
  PlotRangePadding -> None
};

GraphicsPlotRange[expr_, OptionsPattern[]] := Scope[
  UnpackOptions[plotRangePadding];
  iGraphicsPlotRange[expr]
];

iGraphicsPlotRange = Case[
  g:(_Graphics | _Graphics3D) := Scope[
    plotRange = LookupOption[g, PlotRange];
    If[!CoordinateMatrixQ[plotRange],
      plotRange = PrimitiveBoxesBounds @ F @ ToBoxes @ g];
    padRange[g, plotRange]
  ];
  g_FixedGraphics := Scope[
    plotRange = LookupOption[g, PlotRange];
    If[CoordinateMatrixQ[plotRange],
      Return @ padRange[g, plotRange]];
    LookupOption[ToBoxes @ g, PlotRange]
  ];
  g:(_GraphicsBox | _Graphics3DBox) := Scope[
    plotRange = LookupOption[g, PlotRange];
    If[!CoordinateMatrixQ[plotRange],
      plotRange = PrimitiveBoxesBounds @ F @ g];
    padRange[g, plotRange]
  ];
  g_Graph := padRange[g,
    Rep[
      LookupOption[g, PlotRange],
      Auto | None | All :> (
        CoordinateBounds @ Values @ LookupVertexCoordinates @ g
      )
    ]
  ];
  (Labeled|Legended)[e_, ___] := %[e];
  elems_ := %[Graphics @ elems];
];

padRange[g_, plotRange_] :=
  PlotRangePad[plotRange, Rep[plotRangePadding, Inherited :> LookupOption[g, PlotRangePadding]]];

(**************************************************************************************************)

PrivateFunction[PlotRangePad]

SetUsage @ "
PlotRangePad[range$, padding$] expands the plot range range$ by the amount padding$.
* padding$ can be None, Automatic, Scaled[r$], p$, or {{l$, r$}, {b$, t$}}.
"

PlotRangePad[range_, None | 0 | 0.] :=
  range;

PlotRangePad[range_, Auto] :=
  PlotRangePad[range, Scaled[0.02]];

PlotRangePad[range_, Scaled[s_]] :=
  PlotRangePad[range, Max @ scaleToPadding[s, PlotRangeSize @ range]]

PlotRangePad[range_, padding_ ? NumericQ] :=
  Map[expandRange[#, padding]&, range];

PlotRangePad[range_, padding_List] :=
  MapThread[expandRange, {range, padding}];


expandRange[ab_, None|0|0.] :=
  ab;

expandRange[ab_, Auto] :=
  expandRange[ab, Scaled[0.02]];

expandRange[{a_, b_}, dx_] :=
  {a - dx, b + dx};

expandRange[{a_, b_}, {da_, db_}] :=
  {a - da, b + db};

expandRange[ab:{a_, b_}, Scaled @ s_] :=
  expandRange[ab, scaleToPadding[s, b - a]];

expandRange[ab:{a_, b_}, {Scaled @ sa_, Scaled @ sb_}] :=
  expandRange[ab, scaleToPadding[{sa, sb}, b - a]];


scaleToPadding[s_, w_] := w * (s / (1 - 2 * Min[s, 0.45]));
scaleToPadding[s:{_, _}, w_] := w * s / (1 - Min[Total @ s, 0.45]);

(**************************************************************************************************)

PrivateFunction[PlotRangeSize]

SetUsage @ "
PlotRangeSize[range$] returns the size of the plot range.
PlotRangeSize[graphics$] returns the size of a %Graphics or %Graphics3D object.
* The size is a tuple of {width$, height$}, or {width$, height$, depth$}.
"

PlotRangeSize[g_Graphics | g_Graphics3D] := PlotRangeSize @ GraphicsPlotRange @ g;
PlotRangeSize[range_] := Dist @@@ range;

(**************************************************************************************************)

PrivateFunction[PlotRangeScale]

SetUsage @ "
PlotRangeScale[range$] returns the scale of a 2D or 3D range.
PlotRangeScale[graphics$] returns the scale of a %Graphics or %Graphics3D object.
* For a 2D range, the scale is the width.
* For a 3D range, the scale is the length of the diagonal across all axes.
"

PlotRangeScale[g_Graphics | g_Graphics3D] := PlotRangeScale @ GraphicsPlotRange @ g;
PlotRangeScale[{{xl_, xh_}, {yl_, yh_}}] := Dist[xl, xh];
PlotRangeScale[{{xl_, xh_}, {yl_, yh_}, {zl_, zh_}}] := Dist[{xl, yl, zl}, {xh, yh, zh}];
