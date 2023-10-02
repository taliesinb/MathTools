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

(* TODO: it makes more sense to have a single function that determines a good plot range AND
extra image padding at the same time. you can't avoid doing toboxes to do this in any case since
that's what FindAutomaticPadding does. the below code is such a shitshow and the kernel ignores all
kinds of corner cases like apply geometric transformations that you're building a half-baked full solution
in any case. *)

SetUsage @ "
GraphicsPlotRange[graphics$] yields the %PlotRange that will be used when graphics$ is rendered.
* graphics$ can be a %Graphics[$$], %Graphics3D[$$], their box equivalents, or a list of graphics primitives.
* %Legended, %Labeled, etc will be skipped.
* The option %PlotRangePadding specifies whether padding should be included in the resulting range, \
and has the following settings:
| None | do not include any padding (default) |
| Inherited | apply the padding specified in the graphics object |
| custom$ | apply the custom padding |
* Padding is applied used %PlotRangePad.
* If an existing PlotRange is already set, it is returned.
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
    padRange[g, If[CoordinateMatrixQ[plotRange], plotRange, kernelPlotRange @ g]]
  ];
  g_Graph := padRange[g,
    Replace[
      LookupOption[g, PlotRange],
      Automatic | None | All :> (
        CoordinateBounds @ Values @ LookupVertexCoordinates @ g
      )
    ]
  ];
  g:(_GraphicsBox | _Graphics3DBox) := %[g /. $graphicsBoxReplacements];
  (Labeled|Legended)[e_, ___] := %[g];
  elems_ := %[Graphics @ elems];
];

padRange[g_, plotRange_] :=
  PlotRangePad[plotRange, Replace[plotRangePadding, Inherited :> LookupOption[g, PlotRangePadding]]];

(**************************************************************************************************)

$centerOSpecP = Center | Automatic | {Center, Center} | Scaled[{.5, .5}] | {Scaled[.5], Scaled[.5]};

$ghead = Graphics;

GraphicsPlotRange::failed = "Cannot find plot range for ``, using unit plot range.";

kernelPlotRange[g_Graphics3D | g_Graphics] := Module[
  {g2, res, coord, $ghead = Head @ g},
  g2 = InheritedBlock[{$MakeBoxesStyleData}, plotRangeRecurse @ g];
  g2 = plotRangeExpand @ g2;
  res = Quiet @ PlotRange @ g2;
  If[!MatrixQ[res],
    g2 //= DeleteCases[(s_Symbol -> _) /; Context[s] =!= "System`"];
  (* ^ deal with weird option 'ViewSize' that appears on reinterpreting Graphics3D *)
    res = Quiet @ PlotRange @ g2;
  ];
  If[!MatrixQ[res],
    Message[GraphicsPlotRange::failed, MsgExpr[g, 6, 30]];
    res = {{-1, 1}, {-1, 1}};
  ];
  If[First[res] === {-1., 1.} || Last[res] === {-1., 1.},
    (* this occurs when a plot is completely flat horizontally or vertically *)
    coord = DeepFirstCase[g2, $CoordP] + $MachineEpsilon;
    res = Quiet @ PlotRange @ MapAt[{#, Point[coord]}&, g2, 1];
  ];
  res
];

expandMultiArrowInGC[g_] := g /.
  Arrow[segments:{{__Integer}..}, opts___] :> RuleCondition[Map[Arrow[#, opts]&, segments]];

$expanderRules := $expanderRules = Dispatch @ {
  RawBoxes[b_]                             :> RuleCondition[b /. $graphicsBoxReplacements],
  Invisible[e_]                            :> e,
  Text[_, pos_, ___]                       :> RuleCondition[Point[pos]],
  StadiumShape[{a_, b_}, r_]               :> RuleCondition[{Disk[a, r], Disk[b, r]}],
  CapsuleShape[{a_, b_}, r_]               :> RuleCondition[{Sphere[a, r], Sphere[b, r]}],
  Cube[p:{_, _, _}:{0,0,0}, l_:1]          :> RuleCondition[Sphere[p, l/2]],
  (JoinedCurve|JoinedCurveBox)[e_List]     :> e,
  a:$AnnotationP                           :> RuleCondition[plotRangeExpand @ First @ a],
  (StyleBox|Style)[e_, ___]                :> e,
  Inset[_, pos_, ___]                      :> Point[pos],
  Translate[g_, t_]                        :> RuleCondition[applyGeomTrans[g, None, t]],
  Rotate[g_, t_]                           :> RuleCondition[applyGeomTrans[g, RotationMatrix @ t, None]],
  GeometricTransformation[g_, m_ ? MatrixQ]       :> RuleCondition[applyGeomTrans[g, m, None]],
  GeometricTransformation[g_, {m_ ? MatrixQ, t_}] :> RuleCondition[applyGeomTrans[g, m, t]],
  Inset[_, pos_, $centerOSpecP, {w_, h_}]  :> RuleCondition[Rectangle[pos - {w,h}/2, pos + {w,h}/2]]
};

(* TODO: put this into their own function *)
applyGeomTrans[g_, None | {{1., 0.}, {0., 1.}}, t_] := GraphicsTransformCoordinates[PlusOperator[Threaded[t]], g];
applyGeomTrans[g_, m_, t_]   := GraphicsTransformCoordinates[DotRightOperator[m] /* PlusOperator[Threaded[t]], g];
applyGeomTrans[g_, m_, None] := GraphicsTransformCoordinates[DotRightOperator[m], g];

(* this tracks $MakeBoxesStyleData changes in the primitive tree, so that when we use render $customGraphicsP
we will pick up the right options for them *)
plotRangeRecurse = Case[
  (head:Translate|Rotate|GeometricTransformation|Graphics|Graphics3D)[g_, a___] := head[% @ g, a];
  l_List                         := InheritedBlock[{$MakeBoxesStyleData}, Map[%, l]];
  Rule[key_Symbol, val_]         := ($MakeBoxesStyleData[key] = val);
  other_                         := If[TrueQ @ $customGraphicsHeadQ @ Head @ other,
    With[{h = $ghead}, Typeset`MakeBoxes[other, StandardForm, h]] //. $graphicsBoxReplacements,
    other
  ]
];


plotRangeExpand[g_] := g /.
  gc_GraphicsComplex :> RuleCondition[Normal @ expandMultiArrowInGC @ gc] //. $expanderRules /. t_Translate :> RuleCondition @ BakeGraphicsTransformations[t];

$graphicsBoxSymbols = {
  PointBox, Point3DBox,
  CircleBox, DiskBox, RectangleBox, PolygonBox, Polygon3DBox, PolyhedronBox,
  LineBox, Line3DBox, ArrowBox, Arrow3DBox,
  TextBox, Text3DBox,
  TooltipBox, StyleBox,
  InsetBox, Inset3DBox,
  GeometricTransformationBox, GeometricTransformation3DBox,
  GraphicsBox, Graphics3DBox,
  GraphicsGroupBox, GraphicsGroup3DBox, GraphicsComplexBox, GraphicsComplex3DBox,
  RasterBox, Raster3DBox,
  BSplineCurveBox, BSplineCurve3DBox, BezierCurveBox, BezierCurve3DBox, FilledCurveBox, JoinedCurveBox,
  SphereBox, CylinderBox, TubeBox, ConeBox, CuboidBox, HexahedronBox, TetrahedronBox,
  ConicHullRegionBox, ConicHullRegion3DBox
};

$boxSymbolToOrdinarySymbol := $boxSymbolToOrdinarySymbol =
  AssociationMap[SymbolName /* boxNameToOrdinaryName /* Symbol, $graphicsBoxSymbols];

boxNameToOrdinaryName[name_] := StringDelete[name, {"3D", "Box"}];

$graphicsBoxReplacements := $graphicsBoxReplacements =
  Dispatch @ Join[Normal @ $boxSymbolToOrdinarySymbol, {InterpretationBox[a_, _] :> a, Typeset`Hold[h_] :> h}];

(**************************************************************************************************)

PrivateFunction[PlotRangePad]

SetUsage @ "
PlotRangePad[range$, padding$] expands the plot range range$ by the amount padding$.
* padding$ can be None, Automatic, Scaled[r$], p$, or {{l$, r$}, {b$, t$}}.
"

PlotRangePad[range_, None | 0 | 0.] :=
  range;

PlotRangePad[range_, Automatic] :=
  PlotRangePad[range, Scaled[0.02]];

PlotRangePad[range_, Scaled[s_]] :=
  PlotRangePad[range, Max @ scaleToPadding[s, PlotRangeSize @ range]]

PlotRangePad[range_, padding_ ? NumericQ] :=
  Map[expandRange[#, padding]&, range];

PlotRangePad[range_, padding_List] :=
  MapThread[expandRange, {range, padding}];


expandRange[ab_, None|0|0.] :=
  ab;

expandRange[ab_, Automatic] :=
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
PlotRangeSize[range_] := EuclideanDistance @@@ range;

(**************************************************************************************************)

PrivateFunction[PlotRangeScale]

SetUsage @ "
PlotRangeScale[range$] returns the scale of a 2D or 3D range.
PlotRangeScale[graphics$] returns the scale of a %Graphics or %Graphics3D object.
* For a 2D range, the scale is the width.
* For a 3D range, the scale is the length of the diagonal across all axes.
"

PlotRangeScale[g_Graphics | g_Graphics3D] := PlotRangeScale @ GraphicsPlotRange @ g;
PlotRangeScale[{{xl_, xh_}, {yl_, yh_}}] := EuclideanDistance[xl, xh];
PlotRangeScale[{{xl_, xh_}, {yl_, yh_}, {zl_, zh_}}] := EuclideanDistance[{xl, yl, zl}, {xh, yh, zh}];
