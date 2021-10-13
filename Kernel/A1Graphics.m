PackageExport["LegendForm"]

SetUsage @ "
LegendForm[expr$] renders as a form of expr$ that is suitable for use in %Legended.
"

LegendForm[Placed[a_, pos_]] :=
  Placed[LegendForm @ a, pos];

LegendForm[lf_LegendForm] := lf;

declareFormatting[
  LegendForm[list_List] :> Row[Map[LegendForm, list], Spacer[5]],
  LegendForm[assoc_Association] :> Grid[
    Transpose @ KeyValueMap[{LegendForm[#2], LabelForm[#1, Bold]}&, assoc],
    Spacings -> {{0.2, {1.2}}, {{0.5}}}
  ],
  LegendForm[e_] :> e
];

(* fix a bug in mathematica *)
DownValues[WrappersDump`makeLabeledCore] = ReplaceAll[DownValues[WrappersDump`makeLabeledCore], "SkipImageSizeLevel" -> "ZSkipImageSizeLevel"];

(**************************************************************************************************)

PackageExport["$LabelStyle"]

$LabelStyle = {
  FontFamily -> "Avenir", FontSize -> 12
};

(**************************************************************************************************)

PackageExport["$MathFont"]

$MathFont = "KaTeX_Main";

(**************************************************************************************************)

PackageExport["$CardinalFont"]

$CardinalFont = "KaTeX_Typewriter"

(**************************************************************************************************)

PackageExport["$CardinalLabelStyle"]

$CardinalLabelStyle = {
  FontFamily -> $CardinalFont, FontSize -> 14
};

(**************************************************************************************************)

PackageExport["$MathLabelStyle"]
PackageScope["$alphabet"]

$alphabet = Join[Alphabet["English"], Alphabet["Greek"]];
$alphabet = Join[$alphabet, ToUpperCase[$alphabet]];

$MathLabelStyle = {
  FontFamily -> $MathFont, FontSize -> 14,
  SingleLetterItalics -> True, ShowStringCharacters -> False,
  AutoItalicWords -> $alphabet
};

(**************************************************************************************************)

PackageExport["LabelForm"]

LabelForm[e_, args___] := Style[e, args, $LabelStyle];

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

PackageExport["GraphicsPrimitivesQ"]

$directivesP = Alternatives[
  _Thickness, _AbsoluteThickness, _PointSize, _AbsolutePointSize, _Dashing,
  _EdgeForm, _FaceForm, _Directive, _CapForm, _JoinForm
];

GraphicsPrimitivesQ = Case[
  list_List                                        := AllTrue[list, GraphicsPrimitivesQ];
  Style[s_, ___]                                   := % @ s;
  Annotation[g_, ___]                              := % @ g;
  Rectangle[_ ? vQ, _ ? vQ]                        := True;
  (Line|Arrow|BezierCurve|BSplineCurve)[_ ? maQ]   := True;
  Arrow[_ ? maQ, _]                                := True;
  (JoinedCurve|FilledCurve)[list_List]             := % @ list;
  (Disk|Circle|Sphere)[_ ? vQ, Optional[_, None]]  := True;
  Annulus[_ ? vQ, _]                               := True;
  Tube[_ ? maQ, Optional[_, None]]                 := True;
  Point[_ ? vmQ]                                   := True;
  Polygon[_ ? maQ]                                 := True;
  Text[_, _ ? vQ, ___]                             := True;
  $ColorPattern | _Opacity                         := True;
  $directivesP                                     := True;
  _Arrowheads                                      := True;
  e_                                               := False,
  {maQ -> CoordinateMatrixOrArrayQ, vQ -> CoordinateVectorQ, vmQ -> CoordinateVectorOrMatrixQ}
];

CoordinateVectorOrMatrixQ[array_] := ArrayQ[array, 2|3] && MatchQ[InnerDimension @ array, 2|3];

CoordinateMatrixOrArrayQ[{} | {{}}] := True;
CoordinateMatrixOrArrayQ[array_] := CoordinateMatrixQ[array] || VectorQ[array, CoordinateMatrixQ];

(**************************************************************************************************)

PackageExport["ExpandPrimitives"]

$expandPrimitivesDispatch = Dispatch[{
  p:Point[_ ? CoordinateMatrixQ] :> Thread[p],
  p:Disk[_ ? CoordinateMatrixQ] :> Thread[p],
  p:Line[l_ /; VectorQ[l, CoordinateMatrixQ]] :> Thread[p],
  Style[p_, s__] :> expandStyle @ Replace[p, $expandPrimitivesDispatch]
}];

expandStyle = Case[
  Style[p_List, s__] := Map[Style[#, s]&, p];
  other_             := other
];

ExpandPrimitives[primitives_, level_:{0,1}] := Replace[
  primitives,
  $expandPrimitivesDispatch,
  level
];

(**************************************************************************************************)

PackageExport["GraphicsQ"]

SetUsage @ "
GraphicsQ[object$] returns True if object$ is a %Graphics[$$] or %Graphics3D[$$] expression.
"

GraphicsQ[_Graphics | _Graphics3D] := True;
GraphicsQ[_] := False;

(**************************************************************************************************)

PackageExport["EffectiveImageSize"]

EffectiveImageSize[{width_, height_}, trueAspectRatio_] := Scope[
  imageAspectRatio = height / width;
  Which[
    (* perfect, full image will be used *)
    imageAspectRatio == trueAspectRatio,
      {width, height},
    (* image is taller than necessary, but full width will be used *)
    imageAspectRatio > trueAspectRatio,
      {width, trueAspectRatio / width},
    (* image is wider than necessary, but full height will be used *)
    True,
      {height / trueAspectRatio, height}
  ]
];

(**************************************************************************************************)

PackageExport["ImageSizePad"]

SetUsage @ "
ImageSizePad[size$, padding$] expands the image size size$ by padding it according to padding$.
* padding$ can be any of the specifications supported by %ImagePadding.
"

ImageSizePad::badpadding = "`` is not a valid padding spec.";

ImageSizePad[imageSize_, paddingSpec_] := Scope[
  If[!MatchQ[imageSize, {_ ? NumericQ, _ ? NumericQ}], ReturnFailed[]];
  {pw, ph} = padding = processPadding[ImageSizePad, paddingSpec];
  {w, h} = imageSize;
  {w + Total[pw], h + Total[ph]}
];

processPadding[head_, paddingSpec_] := Scope[
  padding = Ceiling @ StandardizePadding @ paddingSpec;
  If[RealMatrixQ[padding] && Dimensions[padding] === {2, 2},
    padding,
    ReturnFailed[MessageName[head, "badpadding"], paddingSpec]
  ]
];

(**************************************************************************************************)

PackageExport["StandardizePadding"]

SetUsage @ "
StandardizePadding[spec$] standardizes a padding specification spec$.
* StandardizePadding returns {{l$, r$}, {b$, t$}}.
* StandardizePadding accepts the following forms:
| None | no padding |
| n$ | pad by n$ on all sides |
| {h$, v$} | pad by h$ horizontally and v$ vertically |
| {{l$, r$}, {b$, t$}} | explicit padding |
| {Left -> l$, $$} | per-side padding |
"

StandardizePadding = Case[
  All                   := All;
  None                  := {{0, 0}, {0, 0}};
  p_ ? NQ               := N @ {{p, p}, {p, p}};
  {pw_ ? NQ, ph_ ? NQ}  := N @ {{pw, pw}, {ph, ph}};
  spec:{{_ ? NQ, _ ? NQ}, {_ ? NQ, _ ? NQ}} := N @ spec;

  rules:{Rule[Left|Right|Bottom|Top|All, _ ? NQ]...} :=
    N @ Map[Lookup[rules, #, Lookup[rules, All, 0]]&, {{Left, Right}, {Bottom, Top}}, {2}];

  _ := $Failed;

  {NQ -> NumericQ}
];

(**************************************************************************************************)

PackageExport["ToSquarePlotRange"]

ToSquarePlotRange[{{x1_, x2_}, {y1_, y2_}}] := Scope[
  w = x2 - x1; h = y2 - y1;
  d2 = Max[w, h] / 2; x = Mean[{x1, x2}]; y = Mean[{y1, y2}];
  {{x - d2, x + d2}, {y - d2, y + d2}}
];

(**************************************************************************************************)

PackageExport["NormalizePlotRange"]

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

PackageExport["GraphicsPlotRange"]

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
    padRange[g, PlotRange @ expandGC @ g]
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

PackageExport["PlotRangeSize"]

SetUsage @ "
PlotRangeSize[range$] returns the size of the plot range.
"

PlotRangeSize[range_] := EuclideanDistance @@@ range;

(**************************************************************************************************)

PackageExport["MediumSmall"]
PackageExport["MediumLarge"]
PackageExport["Huge"]

SetUsage @ "MediumSmall represents a size betwen %Small and %Medium."
SetUsage @ "MediumLarge represents a size betwen %Medium and %Large."
SetUsage @ "Huge represents a size greater than %Large."

(**************************************************************************************************)

PackageExport["LookupImageSize"]

SetUsage @ "
LookupImageSize[object$] returns the setting of %ImageSize that a given object will use when rendered.
* object$ can be a %Graphics[$$] or %Graphics3D[$$] object.
* A numeric hard-coded size will be returned as-is.
* Symbolic sizes like %Tiny, %Small, %Medium, %Large will be converted to their numeric equivalents.
* The size is returned as a pair {width$, height$}, where height$ may be Automatic.
"

DeclareArgumentCount[LookupImageSize, 1];

LookupImageSize[obj_] := Scope[
  {imageSize, aspectRatio} = LookupOption[obj, {ImageSize, AspectRatio}];
  imageSize = resolveRawImageSize @ imageSize;
  If[NumberQ[aspectRatio] && MatchQ[Part[imageSize, 2], Automatic],
    Part[imageSize, 2] = Part[imageSize, 1] * aspectRatio];
  imageSize
];

resolveRawImageSize = Case[
  sz:{_ ? NQ, Automatic | (_ ? NQ)} := sz;
  w_ ? NQ                           := {w, Automatic};
  s_Symbol                          := {Lookup[$ImageWidthTable, s], Automatic};
  _                                 := {720, Automatic};
  {NQ -> NumberQ}
];

(**************************************************************************************************)

PackageExport["ToNumericImageSize"]

SetUsage @ "
ToNumericImageSize[spec$, ratio$] resolves an %ImageSize specificiaton spec$ using a target aspect ratio, \
returning {w$, h$}.
"

ToNumericImageSize[imageSize_, aspectRatio_] := Scope[
  {width, height} = resolveRawImageSize[imageSize];
  SetAutomatic[height, width * aspectRatio];
  {width, height}
];

(**************************************************************************************************)

PackageScope["$SymbolicSizePattern"]

$SymbolicSizePattern = Alternatives[
  Tiny, Small, MediumSmall, Medium, MediumLarge, Large, Huge
];

(**************************************************************************************************)

PackageScope["$SymbolicPointSizes"]

$SymbolicPointSizes = <|
  Tiny -> 2, Small -> 3,
  MediumSmall -> 4, Medium -> 5, MediumLarge -> 6,
  Large -> 7, Huge -> 10
|>;

(**************************************************************************************************)

PackageScope["$SymbolicSizeFractions"]

$SymbolicSizeFractions = <|
  Tiny -> 0.25, Small -> 0.5,
  MediumSmall -> 0.75, Medium -> 1.0, MediumLarge -> 1.25,
  Large -> 1.5, Huge -> 2.0
|>;

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


PackageScope["$sizePattern"]
PackageScope["toNumericSizeScale"]

$sizeScaleAssoc = KeyDrop[$ImageWidthTable / $ImageWidthTable[Medium], Automatic];
$sizePattern = Alternatives @@ Append[Scaled[_ ? NumericQ]] @ Keys @ $sizeScaleAssoc;

toNumericSizeScale = Case[
  sym_Symbol := Lookup[$sizeScaleAssoc, sym, 1];
  Scaled[r_] := Clip[N @ r, {.01, 10.}];
  _          := 1
];

(**************************************************************************************************)

PackageExport["TopLeft"]
PackageExport["TopRight"]
PackageExport["BottomLeft"]
PackageExport["BottomRight"]

(**************************************************************************************************)

PackageScope["$colorNormalizationRules"]

$colorNormalizationRules = {
  Red -> $Red, Orange -> $Orange, Yellow -> $Yellow, Green -> $Green, Blue -> $Blue, Purple -> $Purple, Pink -> $Pink, Cyan -> $Teal,
  LightRed -> $LightRed, LightYellow -> $LightYellow, LightGreen -> $LightGreen, LightBlue -> $LightBlue, LightPink -> $LightPink, LightPurple -> $LightPurple, LightCyan -> $LightTeal
};

(**************************************************************************************************)

PackageScope["$opacityNormalizationRules"]

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

PackageScope["$opacityPattern"]
PackageScope["toNumericOpacity"]

$opacityPattern = Alternatives @@ Append[Opacity[_ ? NumericQ]] @ Keys @ $opacityNormalizationRules;

toNumericOpacity = Case[
  r_ ? NumericQ := Clip[N @ r, {0, 1}];
  sym_Symbol    := Lookup[$opacityNormalizationRules, sym, 1];
  _             := 1;
];

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

PackageScope["$thicknessNormalizationRules"]

$thicknessNormalizationRules = {
  VeryThin -> AbsoluteThickness[0.1],
  MediumThin -> AbsoluteThickness[0.5],
  SlightlyThin -> AbsoluteThickness[0.8],
  SlightlyThick -> AbsoluteThickness[1.2],
  MediumThick -> AbsoluteThickness[1.5],
  VeryThick -> AbsoluteThickness[3]
};

(**************************************************************************************************)

PackageScope["NormalizeThickness"]

NormalizeThickness = Case[
  Automatic             := AbsoluteThickness[1.2];
  t:Thickness[s_Symbol] := t;
  s_Symbol              := Lookup[$thicknessNormalizationRules, s, $Failed];
  n_ ? NumericQ         := AbsoluteThickness @ N @ n;
  _                     := $Failed;
];

(**************************************************************************************************)

PackageScope["toMultiDirective"]

iToMultiDirective = Case[
  {}                            := Automatic;
  {spec_}                       := toDirective @ spec;
  spec_List | spec_Association  := Map[toDirective, spec];
  spec_                         := toDirective @ spec
];

toMultiDirective[spec_] := Scope[
  res = ToColorPalette @ spec;
  If[FailureQ[res], iToMultiDirective[spec], res]
];

(**************************************************************************************************)

PackageScope["toDirective"]

toDirective = Case[
  Automatic           := Automatic;
  {d_Directive}       := % @ d;
  e_List              := normalizeStyles[
    Directive @@ DeleteNone @ Flatten @ ReplaceAll[e, Directive[d_] :> d]
  ];
  Directive[e_List]   := %[e];
  e_                  := normalizeStyles @ e
];

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

ApplyEpilog[Labeled[graphics_, args__], epilog_] :=
  Labeled[ApplyEpilog[graphics, epilog], args];

ApplyEpilog[graphics_Graphics, epilog_] :=
  UpdateOptions[graphics, Epilog, Function[If[#1 === {}, epilog, {epilog, #1}]]];

ApplyEpilog[Graphics3D[primitives_, opts___], epilog_] :=
  Graphics3D[{primitives, epilog}, opts];

ApplyEpilog[epilog_][graphics_] := ApplyEpilog[graphics, epilog];

(**************************************************************************************************)

PackageExport["ApplyProlog"]

ApplyProlog[graphics_, None | {}] := graphics;

ApplyProlog[Labeled[graphics_, args__], prolog_] :=
  Labeled[ApplyProlog[graphics, prolog], args];

ApplyProlog[graphics_Graphics, prolog_] :=
  UpdateOptions[graphics, Prolog, Function[If[#1 === {}, prolog, {#1, prolog}]]];

ApplyProlog[Graphics3D[primitives_, opts___], prolog_] :=
  Graphics3D[{prolog, primitives}, opts];

ApplyProlog[prolog_][graphics_] := ApplyProlog[graphics, prolog];

(**************************************************************************************************)

PackageExport["ApplyLegend"]

ApplyLegend[expr_, None | Automatic | {}] :=
  expr;

ApplyLegend[expr_, item_] :=
  updateLegendMargins @ Legended[expr, If[ListQ[item], Map[LegendForm], LegendForm] @ item];

ApplyLegend[Labeled[graphics_, args__], newLegend_] :=
  Labeled[ApplyLegend[graphics, newLegend], args];

ApplyLegend[Legended[expr_, oldLegend_], newLegend_] :=
 updateLegendMargins @ Legended[expr, ToList[oldLegend, LegendForm /@ newLegend]];

ApplyLegend[legend_][graphics_] := ApplyLegend[graphics, legend];

updateLegendMargins[other_] := other;
updateLegendMargins[Legended[g_Graphics | g_Graphics3D, legendSpec_]] := Scope[
  l = 0; r = 0; b = 0; t = 0;
  Cases[legendSpec, Placed[_, s_Symbol ? updateMargin], {0, 2}];
  If[Min[l,r,b,t] == 0, r = $legendMargin];
  Legended[
    ReplaceOptions[g, ImageMargins -> {{l, r}, {b, t}}],
    legendSpec
  ]
];

$legendMargin = 15;
updateMargin = <|
  Left :> (l = $legendMargin), Right :> (r = $legendMargin),
  Bottom :> (b = $legendMargin), Top :> (r = $legendMargin)
|>;

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

(**************************************************************************************************)

PackageExport["ConstructGraphicsViewTransform"]

(* adapted from https://mathematica.stackexchange.com/questions/3528/extract-values-for-viewmatrix-from-a-graphics3d *)

ConstructGraphicsViewTransform[viewAssoc_Association] := Scope[

  viewAssoc = Association[$defaultViewOpts, viewAssoc];
  {plotRange, viewPoint, viewVector, viewMatrix, viewCenter, viewVertical, viewAngle, viewProjection} =
    Lookup[viewAssoc, {PlotRange, ViewPoint, ViewVector, ViewMatrix, ViewCenter, ViewVertical, ViewAngle, ViewProjection}];

  If[Dimensions[viewMatrix] === {2, 4, 4},
    Return @ GraphicsViewTransform[Dot @@ viewMatrix, viewMatrix]];

  plotSize = EuclideanDistance @@@ plotRange;
  If[Total[plotSize] == 0, Return[Indeterminate]];

  plotRangeLower = Part[plotRange, All, 1];
  SetAutomatic[viewPoint, {1.3, -2.4, 2}];
  SetAutomatic[viewProjection, If[FreeQ[viewPoint, Infinity], "Perspective", "Orthographic"]];
  viewPoint //= Replace[$symbolicViewpointRules];
  viewPoint = Clip[viewPoint, {-1*^5, 1*^5}];

  SetAutomatic[viewCenter, {0.5, 0.5, 0.5}];
  viewCenter = plotRangeLower + viewCenter * plotSize;

  SetAutomatic[viewVector, viewPoint * Max[plotSize] + viewCenter];
  If[MatrixQ[viewVector],
    {cameraPoint, lookAtPoint} = viewVector,
    cameraPoint = viewVector; lookAtPoint = viewCenter
  ];
  transVec = cameraPoint - lookAtPoint;

  SetAutomatic[viewVertical, {0, 0, 1}];
  SetAutomatic[viewAngle, $defaultViewAngle];

  viewAngle /= 2;
  isOrtho = viewProjection === "Orthographic";
  scaling = If[False && isOrtho, 1, Cot[viewAngle] / Norm[transVec]];
  boxScale = 1 / plotSize;

  trans = Dot[
    RotationTransform[-alpha[viewVertical/boxScale, transVec], {0, 0, 1}],
    RotationTransform[-theta[transVec], {0, 1, 0}],
    RotationTransform[-phi[transVec], {0, 0, 1}],
    ScalingTransform[scaling * {1, 1, 1}],
    TranslationTransform[-lookAtPoint]
  ];

  transMatrix = ToPackedReal @ TransformationMatrix @ trans;

(*   transMatrix = ToPackedReal @ BlockDiagonalMatrix[{
    ToPackedReal @ trans["AffineMatrix"], Ones[{1,1}]
  }];
 *)
  projMatrix = If[isOrtho, IdentityMatrix[4], makeProjMatrix @ Tan[viewAngle]];

  finalMatrix = ToPackedReal @ Dot[projMatrix, transMatrix];

  GraphicsViewTransform[finalMatrix, {transMatrix, projMatrix}]
];

makeProjMatrix[tanAngle_] := ToPackedReal @ {
  {1, 0, -1 * tanAngle, 1},
  {0, 1, -1 * tanAngle, 1},
  {0, 0, -1 * tanAngle, 0},
  {0, 0, -2 * tanAngle, 2}
};

theta[{x_, y_, z_}] := ArcTan[z, Norm[{x, y}]]

phi[{x_, y_, _}] := ArcTan2[x, y];

alpha[viewVertical_, viewVector_] := Scope[
  p = phi[viewVector];
  v = {-Sin[p], Cos[p], 0};
  ArcTan2[v . viewVertical, Cross[viewVector / Norm[viewVector], v] . viewVertical]
];

$symbolicViewpointRules = {
  Above|Top -> { 0,  0,  2},
  Below|Bottom -> { 0,  0, -2},
  Front -> { 0, -2,  0},
  Back ->  { 0,  2,  0},
  Left ->  {-2,  0,  0},
  Right -> { 2,  0,  0}
};

$defaultViewAngle = 35. * Degree;

$viewOptionSymbols = {ViewPoint, ViewVector, ViewMatrix, ViewCenter, ViewVertical, ViewAngle, ViewProjection};
$defaultViewOpts = Thread[$viewOptionSymbols -> Automatic];

PackageScope["$automaticViewOptions"]

$automaticViewOptions = {ViewProjection -> "Orthographic", ViewPoint -> {-0.2, -2, 0.5}};

ConstructGraphicsViewTransform[g_Graphics3D] := Scope[
  viewOpts = Options[g, $viewOptionSymbols];
  plotRange = GraphicsPlotRange @ g;
  viewAssoc = Association[viewOpts, PlotRange -> plotRange];
  ConstructGraphicsViewTransform[viewAssoc]
];

(**************************************************************************************************)

PackageExport["GraphicsViewTransform"]

GraphicsViewTransform::badinput = "Received input of dimensions ``: ``."

GraphicsViewTransform[tmatrix_, _][input_] :=
  Which[
    VectorQ[input], gvtVector[tmatrix, input],
    MatrixQ[input], gvtMatrix[tmatrix, input],
    True, Message[GraphicsViewTransform::badinput, Dimensions @ input, input]; input
  ];

gvtVector[tmatrix_, input_] :=
  toImageSpaceVec @ Dot[tmatrix, AppendOnes @ input];

toImageSpaceVec[vec_] :=
  ToPacked[Take[vec, 2] / Part[vec, 4]];

gvtMatrix[tmatrix_, input_] :=
  toImageSpaceMatrix @ Transpose @ Dot[tmatrix, Transpose @ AppendOnes @ N @ input];

toImageSpaceMatrix[matrix_] :=
  ToPacked[Take[matrix, All, 2] / Part[matrix, All, 4]];

(**************************************************************************************************)

PackageExport["Graphics3DProjection"]

$canonicalizeShapes = {
  c_Cuboid :> CanonicalizePolyhedron @ c
};

$to2DShapes = {
  Polyhedron -> Polygon,
  Sphere -> Circle (* todo: Cone, Tube, etc *)
};

Graphics3DProjection[g:Graphics3D[primitives_, ___]] := Scope[
  trans = ConstructGraphicsViewTransform[g];
  primitives //= ReplaceAll[$canonicalizeShapes];
  transformedPrimitives = GraphicsTransformCoordinates[trans, primitives];
  transformedPrimitives //= ReplaceAll[$to2DShapes];
  elements = {EdgeForm[$DarkRed], FaceForm[Opacity[0.2, $Red]], transformedPrimitives};
  Graphics[
    elements, ImageSize -> 400,
    Frame -> True, PlotRange -> All
  ]
];


(**************************************************************************************************)

PackageExport["GraphicsTransformCoordinates"]

$ctfDispatch = Dispatch @ {
  Line[c_] :> Line[$ctf @ c],
  InfiniteLine[c_] :> InfiniteLine[$ctf @ c],
  InfiniteLine[c_, p_] :> InfiniteLine[$ctf @ c, $ctf @ p],
  HalfLine[c_] :> HalfLine[$ctf @ c],
  HalfLine[c_, p_] :> HalfLine[$ctf @ c, $ctf @ p],
  Arrow[c_, opts___] :> Arrow[$ctf @ c, opts],
  Point[c_] :> Point[$ctf @ c],
  Polygon[c_] :> Polygon[$ctf @ c],
  Polygon[c_, d_] :> Polygon[$ctf @ c, d],
  FilledCurve[c_, i_] :> FilledCurve[$ctf @ c, i],
  FilledCurve[c_] :> FilledCurve[$ctf /@ c],
  BezierCurve[c_] :> BezierCurve[$ctf /@ c],
  BSplineCurve[c_] :> BSplineCurve[$ctf /@ c],
  Tube[c_, r___] :> Tube[$ctf @ c, r],
  Cone[c_, r___] :> Cone[$ctf @ c, r],
  Disk[c_, r___] :> Disk[$ctf @ c, r],
  Sphere[c_, r___] :> Sphere[$ctf @ c, r],
  Circle[c_, r___] :> Circle[$ctf @ c, r],
  Polyhedron[c_, d_] :> Polyhedron[$ctf @ c, d],
  Rectangle[c_] :> Rectangle[$ctf @ c],
  GraphicsComplex[c_, g_] :> RuleCondition @ GraphicsComplex[$ctf @ c, g],
  i_Inset :> i,
  a_Arrowheads :> a
};

GraphicsTransformCoordinates[ctf_, expr_] := Scope[
  $ctf = ctf;
  ReplaceAll[expr, $ctfDispatch]
];

(**************************************************************************************************)

PackageExport["SetbackCoordinates"]

SetbackCoordinates[spec_, 0|0.] :=
  spec;

SetbackCoordinates[spec_, d_ ? NumericQ] :=
  SetbackCoordinates[spec, {d, d}];

SetbackCoordinates[spec_ ? CoordinateArrayQ, d_] :=
  SetbackCoordinates[#, d]& /@ spec;

SetbackCoordinates[{a_, b_}, {d1_, d2_}] := Scope[
  If[EuclideanDistance[a, b] < d1 + d2, Return[{}]];
  dx = Normalize[b - a];
  {a + dx * d1 , b - dx * d2}
];

SetbackCoordinates[coords_, {d1_, d2_}] :=
  setbackHalf[setbackHalf[coords, d1], -d2]

setbackHalf[{}, _] := {};
setbackHalf[coords_, 0|0.] := coords;
setbackHalf[coords_, d_ ? Negative] := Reverse @ setbackHalf[Reverse @ coords, Abs[d]];
setbackHalf[coords_, d_] := takeLine[coords, d];

takeLine[coords_List, d_] := Scope[
  prev = First @ coords; total = 0;
  n = LengthWhile[coords, curr |-> (total += EuclideanDistance[curr, prev]; prev = curr; total < d)];
  If[n == Length[coords], Return @ Last @ coords];
  rem = total - d;
  newCoords = Drop[coords, n];
  If[rem == 0,
    newCoords,
    Prepend[newCoords, PointAlongLine[Part[coords, {n + 1, n}], rem]]
  ]
];
(**************************************************************************************************)

PackageExport["PointAlongLine"]

PointAlongLine[{a_, b_}, d_ ? NumericQ] :=
  a + Normalize[b - a] * d;

PointAlongLine[coords_, Scaled[d_]] :=
  PointAlongLine[coords, LineLength[coords] * d];

PointAlongLine[coords_List, d_ ? NumericQ] := Scope[
  prev = First @ coords; total = 0;
  n = LengthWhile[coords, curr |-> (total += EuclideanDistance[curr, prev]; prev = curr; total < d)];
  If[n == Length[coords], Return @ Last @ coords];
  rem = total - d;
  newCoords = Drop[coords, n];
  If[rem == 0,
    Part[coords, n + 1],
    PointAlongLine[Part[coords, {n + 1, n}], rem]
  ]
];

(**************************************************************************************************)

PackageExport["LineLength"]

LineLength[{a_, b_}] := EuclideanDistance[a, b];
LineLength[list_] := Total @ MapStaggered[EuclideanDistance, list];

(**************************************************************************************************)

PackageExport["EdgeLengthScale"]

boundingBoxSideLength[line_] :=
  Total[EuclideanDistance @@@ CoordinateBounds @ line];

(* boundingBoxSideLength[line_] :=
  EuclideanDistance @@ CoordinateBoundingBox @ line;
 *)
adjustedLineLength[line_] :=
  If[First[line] === Last[line], 0.8, 1] * Min[LineLength @ line, boundingBoxSideLength @ line];

EdgeLengthScale[{}, q_] := 1.0;

EdgeLengthScale[edgeCoordinateLists_, q_] := Scope[
  edgeLengths = Chop @ Map[adjustedLineLength, edgeCoordinateLists];
  edgeLengths = DeleteCases[edgeLengths, 0|0.];
  If[edgeLengths === {},
    edgeLengths = Map[boundingBoxSideLength, edgeCoordinateLists]];
  If[q === "Average", Mean @ edgeLengths, Quantile[edgeLengths, q]]
];

(**************************************************************************************************)

PackageExport["TransformArrowheads"]

$arrowheadTransforms = <|
  "Reverse" -> reverseArrowhead,
  "OverBar" -> addNegationBar[True],
  "UnderBar" -> addNegationBar[False],
  "Identity" -> Identity
|>;

TransformArrowheads[primitives_, transform_String] := Scope[
  func = Lookup[$arrowheadTransforms, transform];
  ReplaceAll[primitives,
    g:{_, _, (Graphics|Graphics3D)[Except[{}], ___]} :> RuleCondition @ func @ g
  ]
];

TransformArrowheads[transform_][primitives_] :=
  TransformArrowheads[primitives, transform];

reverseArrowhead[{size_, pos_, graphics_}] :=
  {-size, pos, graphics};

addNegationBar[isOver_][{size_, pos_, graphics:Graphics[primitives_, opts___]}] := Scope[
  {{xl, xh}, {yl, yh}} = GraphicsPlotRange @ graphics;
  yb = If[isOver, yh * 1.4, yl * 1.4];
  graphics = Graphics[{primitives, {Opacity[1], Black, AbsoluteThickness[0.5], Line @ {{xl, yb}, {xh, yb}}}}, opts];
  {size, pos, graphics}
];

(**************************************************************************************************)

PackageExport["SetFrameColor"]

SetFrameColor[g_Graphics, color_] :=
  If[LookupOption[g, Frame] === True,
    ReplaceOptions[g, FrameStyle -> color],
    ReplaceAll[g, a:Annotation[_, "Frame"] :> ReplaceAll[a, EdgeForm[_] :> EdgeForm[color]]]
  ];

SetFrameColor[expr_, _] := expr;

(**************************************************************************************************)

PackageExport["MeshImage"]

MeshImage[array_, blockSize_] := Scope[
  {b1, b2} = blockSize + {-1, 1};
  dims = Dimensions @ array;
  If[!ArrayQ[array, _, Developer`MachineRealQ] || !MatchQ[dims, {_, _, 3} | {_, _}],
    ReturnFailed[]];
  hasColor = Length[dims] == 3;
  {h, w} = Take[dims, 2];
  {h2, w2} = 1 + {h, w} * b2;
  pixels = ConstantArray[.7, If[hasColor, {h2, w2, 3}, {h2, w2}]];
  frame = 0.4;
  If[hasColor, paintFrame[All], paintFrame[]];
  ScanIndexed[paintBlock, Developer`ToPackedArray @ array, {2}];
  Image[pixels]
];

paintFrame[cspec___] := (
  Part[pixels, All, 1, cspec] = frame;
  Part[pixels, All, w2, cspec] = frame;
  Part[pixels, 1, All, cspec] = frame;
  Part[pixels, h2, All, cspec] = frame;
);

paintBlock[v_, {r_, c_}] := Module[
  {r1 = (r * b2), c1 = (c * b2)},
  Part[pixels, (r1 - b1) ;; r1, (c1 - b1) ;; c1] = v;
];

(**************************************************************************************************)

PackageExport["CompactArrayPlot"]
PackageExport["ColorLegend"]

Options[CompactArrayPlot] = {
  PixelConstrained -> 4,
  ColorFunction -> Automatic,
  ColorLegend -> None
};

CompactArrayPlot::badrank = "Array should be of rank 2 or 3, but had rank ``.";
CompactArrayPlot::rank3chans = "Rank 3 array should have 3 channels.";
CompactArrayPlot::rank3vals = "Rank 3 array should be numeric in interval [0, 1].";
CompactArrayPlot::interr = "Internal error while processing array.";
CompactArrayPlot::badcvals = "ColorFunction produced non-RGB values, first was: ``.";

CompactArrayPlot[array_, OptionsPattern[]] := Scope[
  UnpackOptions[pixelConstrained, colorFunction, colorLegend];
  array //= ToPackedReal;
  dims = Dimensions @ array; ndims = Length @ dims;
  If[array === {} || MemberQ[dims, 0], Return[Spacer[1]]];
  If[ndims < 2 || ndims > 3, ReturnFailed["badrank", ndims]];
  isRGB = ndims === 3;
  If[isRGB,
    If[Last[dims] =!= 3, ReturnFailed["badchans"]];
    If[!PackedArrayQ[array] || !UnitIntervalArrayQ[array], ReturnFailed["rank3vals"]];
  ];
  SetAutomatic[colorFunction, Which[
    isRGB,
      None,
    Developer`PackedArrayQ[array, Real] && UnitIntervalArrayQ[array],
      None,
    Developer`PackedArrayQ[array, Integer] && UnitIntervalArrayQ[array],
      None,
    True,
      $BooleanColors = {White, Black};
      Last @ ApplyColoring @ Catenate @ array
  ]];
  If[colorFunction =!= None,
    cfunc = colorFunction;
    If[ColorFunctionObjectQ @ cfunc, cfunc //= Normal];
    cfunc //= stripFinalRGB;
    array = ToPackedArray @ Map[cfunc, array, {2}];
    If[ArrayQ[array, 2, ColorQ],
      array = ToPackedArray @ ToRGB @ array];
    If[!PackedArrayQ[array], ReturnFailed["badcvals"]];
  ];
  If[!PackedArrayQ[array], ReturnFailed["interr"]];
  graphics = MeshImage[array, pixelConstrained];
  SetAutomatic[colorLegend, colorFunction];
  If[colorLegend =!= None, graphics //= ApplyLegend[colorLegend]];
  graphics
];

stripFinalRGB[RightComposition[fns___, RGBColor]] := RightComposition[fns];
stripFinalRGB[other_] := other;

(**************************************************************************************************)

PackageExport["BinaryArrayPlot"]

Options[BinaryArrayPlot] = {
  PixelConstrained -> 4
}

BinaryArrayPlot[array_, opts:OptionsPattern[]] :=
  BinaryArrayPlot[array, Automatic, opts];

BinaryArrayPlot[array_, digits:(_Integer|Automatic), OptionsPattern[]] := Scope[
  UnpackOptions[pixelConstrained];
  {min, max} = MinMax @ array;
  Which[
    VectorQ[array, Internal`NonNegativeIntegerQ],
      SetAutomatic[digits, If[max == 0, 0, Floor[1 + Log2 @ max]]];
      array = BinaryDigits[array, digits];
    ,
    MatrixQ[array, Internal`NonNegativeIntegerQ],
      If[IntegerQ[digits] && InnerDimension[array] > digits,
        array = Take[array, All, digits]];
      If[max > 1, ReturnFailed[]];
    ,
    True,
      ReturnFailed[];
  ];
  CompactArrayPlot[1 - array, PixelConstrained -> pixelConstrained]
];

(**************************************************************************************************)

PackageExport["FadeGraphics"]

FadeGraphics[g_, 0 | 0.] := g;
FadeGraphics[g_Graphics, opacity_] := ApplyEpilog[g,
  Style[
    Rectangle[{-1000, -1000}, {1000, 1000}],
    EdgeForm[None], FaceForm[GrayLevel[1, opacity]]
  ]
];

(**************************************************************************************************)

PackageExport["ListShowFaded"]

ListShowFaded[list_, i_, opacity_:0.9] := Scope[
  list = Replace[list, g_Graph :> ExtendedGraphPlot[g], {1}];
  {xs, ys} = Transpose[GraphicsPlotRange /@ list];
  {xmin, xmax} = MinMax @ xs;
  {ymin, ymax} = MinMax @ ys;
  faded = Show[
    Sequence @@ Delete[list, i],
    Graphics[Style[Rectangle[{xmin, ymin} - 1, {xmax, ymax} + 1], EdgeForm[None], FaceForm[GrayLevel[1, opacity]]]],
    Part[list, i],
    PlotRange -> {{xmin, xmax}, {ymin, ymax}}
  ]
]

(**************************************************************************************************)

PackageExport["ShowFaded"]

ShowFaded[items__, opacity_?NumericQ] := ListShowFaded[{items}, -1, opacity];
ShowFaded[items__] := ListShowFaded[{items}, -1];