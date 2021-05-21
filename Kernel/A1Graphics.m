Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

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
GraphicsQ[object$] returns True if object$ is a %Graphics[$$] or %Graphics3D[$$] expression.
"

GraphicsQ[_Graphics | _Graphics3D] := True;
GraphicsQ[_] := False;

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
NormalizePlotRange[graphics$] updates the value of %PlotRange to span all elements in graphics$.
* graphics$ can be a %Graphics[$$] or %Graphics3D[$$] expression.
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
ToNumericImageSize[spec$, ratio$] resolves an %ImageSize specificiaton spec$ using a target aspect ratio, \
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


PackageScope["$sizePattern"]
PackageScope["toNumericSizeScale"]

$sizeScaleAssoc = KeyDrop[$ImageWidthTable / $ImageWidthTable[Medium], Automatic];
$sizePattern = Alternatives @@ Append[Scaled[_ ? NumericQ]] @ Keys @ $sizeScaleAssoc;

toNumericSizeScale = MatchValues[
  sym_Symbol := Lookup[$sizeScaleAssoc, sym, 1];
  Scaled[r_] := Clip[N @ r, {.01, 10.}];
  _ := 1
];


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

toNumericOpacity = MatchValues[
  r_ ? NumericQ := Clip[N @ r, {0, 1}];
  sym_Symbol := Lookup[$opacityNormalizationRules, sym, 1];
  _ := 1;
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

$thicknessNormalizationRules = {
  VeryThin -> AbsoluteThickness[0.1],
  MediumThin -> AbsoluteThickness[0.5],
  SlightlyThin -> AbsoluteThickness[0.8],
  SlightlyThick -> AbsoluteThickness[1.2],
  MediumThick -> AbsoluteThickness[1.5],
  VeryThick -> AbsoluteThickness[3]
};

(**************************************************************************************************)

PackageScope["toMultiDirective"]

iToMultiDirective = MatchValues[
  {} := Automatic;
  {spec_} := toDirective @ spec;
  spec_List | spec_Association := Map[toDirective, spec];
  spec_ := toDirective[spec]
];

toMultiDirective[spec_] := Scope[
  res = ToColorPalette @ spec;
  If[FailureQ[res], iToMultiDirective[spec], res]
];

(**************************************************************************************************)

PackageScope["toDirective"]

toDirective = MatchValues[
  Automatic := Automatic;
  {d_Directive} := % @ d;
  e_List := normalizeStyles @ Directive @@ Flatten @ e;
  Directive[e_List] := %[e];
  e_ := normalizeStyles @ e
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

ApplyEpilog[graphics_Graphics, epilog_] :=
  UpdateOptions[graphics, Epilog, Function[Append[Developer`ToList @ #1, epilog]]];

ApplyEpilog[Graphics3D[primitives_, opts___], epilog_] :=
  Graphics3D[{primitives, epilog}, opts];

(**************************************************************************************************)

PackageExport["ApplyProlog"]

ApplyProlog[graphics_, None | {}] := graphics;

ApplyProlog[graphics_Graphics, prolog_] :=
  UpdateOptions[graphics, Prolog, Function[Append[Developer`ToList @ #1, prolog]]];

ApplyProlog[Graphics3D[primitives_, opts___], prolog_] :=
  Graphics3D[{prolog, primitives}, opts];

(**************************************************************************************************)

PackageExport["ApplyLegend"]

ApplyLegend[expr_, None | Automatic | {}] :=
  expr;

ApplyLegend[expr_, item_] :=
  updateLegendMargins @ Legended[expr, If[ListQ[item], Map[LegendForm], LegendForm] @ item];

ApplyLegend[Legended[expr_, oldLegend_], newLegend_] :=
 updateLegendMargins @ Legended[expr, Developer`ToList[oldLegend, LegendForm /@ newLegend]];

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

$automaticViewOptions = {ViewProjection -> "Orthographic", ViewPoint -> {-0.2, -2, 0}};

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