Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

PackageExport["LegendForm"]

SetUsage @ "
LegendForm[expr$] renders as a form of expr$ that is suitable for use in Legended.
"

LegendForm[Placed[a_, pos_]] :=
  Placed[LegendForm @ a, pos];

LegendForm[lf_LegendForm] := lf;

declareFormatting[
  LegendForm[list_List] :> Row[Map[LegendForm, list], Spacer[5]],
  LegendForm[assoc_Association] :> Grid[
    Transpose @ KeyValueMap[{LegendForm[#2], Style[#1, $LegendLabelStyle, Bold]}&, assoc],
    Spacings -> {{0.2, {1.2}}, {{0.5}}}
  ],
  LegendForm[e_] :> e
];

(* fix a bug in mathematica *)
DownValues[WrappersDump`makeLabeledCore] = ReplaceAll[DownValues[WrappersDump`makeLabeledCore], "SkipImageSizeLevel" -> "ZSkipImageSizeLevel"];

(**************************************************************************************************)

PackageExport["$LegendLabelStyle"]

$LegendLabelStyle = {FontFamily -> "Avenir", FontSize -> 12};

(**************************************************************************************************)

PackageScope["ImageToGraphics"]

ImageToGraphics[img_, {xalign_, yalign_}, size_] := Scope[
  {w, h} = ImageDimensions[img];
  yrat = h / w;
  x = (xalign - 1)/2;
  y = yrat * (yalign - 1)/2;
  Graphics[
    {Opacity[1], Raster[Reverse[ImageData @ img, {1}], {{x, y}, {x + 1, y + yrat}} * size]},
    ImageSize -> size, AspectRatio -> 1, PlotRangePadding -> None
  ]
];

(**************************************************************************************************)

PackageExport["GraphicsQ"]

SetUsage @ "
GraphicsQ[object$] returns True if object$ is a Graphics[$$] or Graphics3D[$$] expression.
"

GraphicsQ[_Graphics | _Graphics3D] := True;
GraphicsQ[_] := False;

(**************************************************************************************************)

PackageExport["ImageSizePad"]

SetUsage @ "
ImageSizePad[size$, padding$] expands the image size size$ by padding it according to padding$.
* padding$ can be any of the specifications supported by ImagePadding.
"

ImageSizePad::badpadding = "`` is not a valid padding spec.";

ImageSizePad[imageSize_, paddingSpec_] := Scope[
  If[!MatchQ[imageSize, {_ ? NumericQ, _ ? NumericQ}], ReturnFailed[]];
  {pw, ph} = padding = processPadding[ImageSizePad, paddingSpec];
  {w, h} = imageSize;
  {w + Total[pw], h + Total[ph]}
];

processPadding[head_, paddingSpec_] := Scope[
  padding = Ceiling @ toPadding @ paddingSpec;
  If[RealMatrixQ[padding] && Dimensions[padding] === {2, 2},
    padding,
    ReturnFailed[MessageName[head, "badpadding"], paddingSpec]
  ]
];

toPadding = MatchValues[
  p_ ? NumericQ := {{p, p}, {p, p}};
  {pw_ ? NumericQ, ph_ ? NumericQ} := {{pw, pw}, {ph, ph}};
  other_ := other;
];

(**************************************************************************************************)

PackageExport["NormalizePlotRange"]

SetUsage @ "
NormalizePlotRange[graphics$] updates the value of PlotRange to span all elements in graphics$.
* graphics$ can be a Graphics[$$] or Graphics3D[$$] expression.
* The value of PlotRangePadding is taken into account, and PlotRangePadding is set to zero in the result.
* Providing the option PlotRangePadding will override the PlotRangePadding present in graphics$.
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

PackageExport["GraphicsPlotRange"]

SetUsage @ "
GraphicsPlotRange[graphics$] yields the PlotRange that will be used when graphics$ is rendered.
* graphics$ can be a Graphics[$$], Graphics3D[$$], their box equivalents, or a list of graphics primitives.
* Legended, Labeled, etc will be skipped.
* The option PlotRangePadding specifies whether padding should be included in the resulting range, \
and has the following settings:
| None | do not include any padding (default) |
| Inherited | apply the padding specified in the graphics object |
| custom$ | apply the custom padding |
* Padding is applied used PlotRangePad.
"

Options[GraphicsPlotRange] = {
  PlotRangePadding -> None
};

GraphicsPlotRange[expr_, OptionsPattern[]] := Scope[
  UnpackOptions[plotRangePadding];
  iGraphicsPlotRange[expr]
];

iGraphicsPlotRange = MatchValues[
  g:(_Graphics | _Graphics3D) := Scope[
    plotRange = PlotRange @ expandGC @ g;
    If[plotRangePadding === None, Return @ plotRange];
    PlotRangePad[plotRange, Replace[plotRangePadding, Inherited :> LookupOption[g, PlotRangePadding]]]
  ];
  g:(_GraphicsBox | _Graphics3DBox) := %[g /. $graphicsBoxReplacements];
  (Labeled|Legended)[e_, ___] := %[g];
  elems_ := %[Graphics @ elems];
];

expandMultiArrowInGC[g_] := g /.
  Arrow[segments:{{__Integer}..}, opts___] :> RuleCondition[Map[Arrow[#, opts]&, segments]];

expandGC[g_] := g /.
  gc_GraphicsComplex :> RuleCondition[Normal @ expandMultiArrowInGC @ gc];

$graphicsBoxSymbols = {
  PointBox, CircleBox, DiskBox, RectangleBox, PolygonBox,
  LineBox, ArrowBox,
  TextBox, TooltipBox, StyleBox, InsetBox, RotationBox, GeometricTransformationBox,
  GraphicsBox, GraphicsGroupBox, RasterBox, GraphicsComplexBox,
  BSplineCurveBox, BezierCurveBox, FilledCurveBox, JoinedCurveBox,
  SphereBox, CylinderBox, TubeBox, ConeBox, CuboidBox
};

$boxSymbolToOrdinarySymbol := $boxSymbolToOrdinarySymbol =
  AssociationMap[Symbol[StringDrop[SymbolName[#], -3]]&, $graphicsBoxSymbols];

$graphicsBoxReplacements := $graphicsBoxReplacements =
  Dispatch @ Normal @ $boxSymbolToOrdinarySymbol;

(**************************************************************************************************)

PackageExport["PlotRangePad"]

SetUsage @ "
PlotRangePad[range$, padding$] expands the plot range range$ by the amount padding$.
* padding$ can be None, Automatic, Scaled[r$], or {{l$, r$}, {b$, t$}}.
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

PackageExport["PlotRangeSize"]

SetUsage @ "
PlotRangeSize[range$] returns the size of the plot range.
"

PlotRangeSize[range_] := EuclideanDistance @@@ range;

(**************************************************************************************************)

PackageExport["MediumSmall"]
PackageExport["MediumLarge"]
PackageExport["Huge"]

SetUsage @ "MediumSmall represents a size betwen Small and Medium."
SetUsage @ "MediumLarge represents a size betwen Medium and Large."
SetUsage @ "Huge represents a size greater than Large."

(**************************************************************************************************)

PackageExport["LookupImageSize"]

SetUsage @ "
LookupImageSize[object$] returns the ImageSize that a given object will use when rendered.
* object$ can be a Graphics[$$] or Graphics3D[$$] object.
* A numeric hard-coded size will be returned as-is.
* Symbolic sizes like Tiny, Small, Medium, Large will be converted to their numeric equivalents.
* The size is returned as a pair {width$, height$}, where height$ may be Automatic.
"

DeclareArgumentCount[LookupImageSize, 1];

LookupImageSize[obj_] := Scope[
  CheckIsGraphics[1];
  resolveRawImageSize @ LookupOption[obj, ImageSize]
];

resolveRawImageSize = MatchValues[
  sz:{_ ? NumberQ, Automatic | (_ ? NumberQ)} := sz;
  w_ ? NumberQ := {w, Automatic};
  s_Symbol := {Lookup[$ImageWidthTable, s], Automatic};
  _ := {720, Automatic};
];

(**************************************************************************************************)

PackageExport["ToNumericImageSize"]

SetUsage @ "
ToNumericImageSize[spec$, ratio$] resolves an ImageSize specificiaton spec$ using a target aspect ratio, \
returning {w$, h$}.
"

ToNumericImageSize[imageSize_, aspectRatio_] := Scope[
  {width, height} = resolveRawImageSize[imageSize];
  SetAutomatic[height, width * aspectRatio];
  {width, height}
];

(**************************************************************************************************)

PackageScope["$ImageWidthTable"]

$ImageWidthTable = <|
  Tiny -> 100,
  Small -> 180,
  MediumSmall -> 270,
  Medium -> 360, Automatic -> 360,
  MediumLarge -> 468,
  Large -> 576,
  Huge -> 720
|>;

PackageScope["toStandardImageSize"]

toStandardImageSize[sym:(MediumSmall|MediumLarge|Huge)] := Lookup[$ImageWidthTable, sym];
toStandardImageSize[other_] := other;

(**************************************************************************************************)

$colorNormalizationRules = {
  Red -> $Red, Orange -> $Orange, Green -> $Green, Yellow -> $Yellow, Blue -> $Blue, Purple -> $Purple, Pink -> $Pink
};

(**************************************************************************************************)

PackageExport["VeryTransparent"]
PackageExport["HalfTransparent"]
PackageExport["PartlyTransparent"]
PackageExport["Opaque"]

$opacityNormalizationRules = {
  VeryTransparent -> Opacity[0.2],
  HalfTransparent -> Opacity[0.5],
  PartlyTransparent -> Opacity[0.8],
  Opaque -> Opacity[1.0]
};

(**************************************************************************************************)

PackageExport["VeryThick"]
PackageExport["MediumThick"]
PackageExport["SlightlyThick"]
PackageExport["SlightlyThin"]
PackageExport["MediumThin"]
PackageExport["VeryThin"]

(* how AbsoluteThickness works:
  Thin/AT[Tiny] = AT[0.25],
  Small = AT[0.5],
  Medium = AT[1],
  Large / Thick is AT[2] *)

$thicknessNormalizationRules = {
  VeryThin -> AbsoluteThickness[0.1],
  MediumThin -> AbsoluteThickness[0.5],
  SlightlyThin -> AbsoluteThickness[0.8],
  SlightlyThick -> AbsoluteThickness[1.2],
  MediumThick -> AbsoluteThickness[1.5],
  VeryThick -> AbsoluteThickness[3]
};

(**************************************************************************************************)

PackageScope["toDirective"]

toDirective[e_List] := normalizeStyles @ Directive @ Flatten @ e;
toDirective[e_] := normalizeStyles @ e;

$styleNormalizationRules = Dispatch @ Flatten @ {
  $colorNormalizationRules,
  $opacityNormalizationRules,
  $thicknessNormalizationRules
};

PackageScope["normalizeStyles"]

normalizeStyles[e_] := ReplaceAll[e, $styleNormalizationRules];

(**************************************************************************************************)

PackageExport["ApplyEpilog"]

ApplyEpilog[graphics_, None | {}] := graphics;

ApplyEpilog[graphics_Graphics, epilog_] :=
  UpdateOptions[graphics, Epilog, Function[Append[Developer`ToList @ #1, epilog]]];

(**************************************************************************************************)

PackageExport["ApplyProlog"]

ApplyProlog[graphics_, None | {}] := graphics;

ApplyProlog[graphics_Graphics, prolog_] :=
  UpdateOptions[graphics, Prolog, Function[Append[Developer`ToList @ #1, prolog]]];

(**************************************************************************************************)

PackageExport["ApplyLegend"]

ApplyLegend[expr_, None | Automatic | {}] := expr;

ApplyLegend[expr_, itemList_List] := Legended[expr, LegendForm /@ itemList];

ApplyLegend[expr_, item_] := Legended[expr, LegendForm @ item];

ApplyLegend[Legended[expr_, oldLegend_], newLegend_] :=
 Legended[expr, Developer`ToList[oldLegend, LegendForm /@ newLegend]];

(**************************************************************************************************)

PackageExport["NiceTooltip"]

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

PackageScope["ColorFramed"]

ColorFramed[boxes_, color_] := Framed[boxes, ContentPadding -> False, FrameStyle -> color];

