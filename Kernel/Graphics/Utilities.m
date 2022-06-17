PublicForm[LegendForm]

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

PrivateVariable[$LabelStyle]

$LabelStyle = {
  FontFamily -> "Avenir", FontSize -> 12
};

(**************************************************************************************************)

PrivateVariable[$MathFont]

$MathFont = "KaTeX_Main";

(**************************************************************************************************)

PrivateVariable[$CardinalFont]

$CardinalFont = "KaTeX_Typewriter"

(**************************************************************************************************)

PrivateVariable[$CardinalLabelStyle]

$CardinalLabelStyle = {
  FontFamily -> $CardinalFont, FontSize -> 14
};

(**************************************************************************************************)

PrivateVariable[$MathLabelStyle]

$MathLabelStyle = {
  FontFamily -> $MathFont, FontSize -> 14,
  SingleLetterItalics -> True, ShowStringCharacters -> False,
  AutoItalicWords -> $Alphabet
};

(**************************************************************************************************)

PublicHead[LabelForm]

LabelForm[e_, args___] := Style[e, args, $LabelStyle];

(**************************************************************************************************)

PrivateFunction[ImageToGraphics]

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

PrivateFunction[GraphicsPrimitivesQ]

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

PrivateFunction[ExpandPrimitives]

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

PrivateFunction[GraphicsQ]

SetUsage @ "
GraphicsQ[object$] returns True if object$ is a %Graphics[$$] or %Graphics3D[$$] expression.
"

GraphicsQ[_Graphics | _Graphics3D] := True;
GraphicsQ[_] := False;

(**************************************************************************************************)

PrivateFunction[EffectiveImageSize]

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

PrivateFunction[ImageSizePad]

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

PrivateFunction[StandardizePadding]

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

  rules:{Rule[sideP, _ ? NQ]...} :=
    N @ LookupSide[rules, {{Left, Right}, {Bottom, Top}}];

  _ := $Failed;

  {NQ -> NumericQ, sideP -> $SidePattern|Horizontal|Vertical|All}
];


(**************************************************************************************************)

PrivateFunction[LookupSide]

LookupSide[rules_, sides_List] :=
  Map[LookupSide[rules, #]&, sides];

LookupSide[rules_, side_] :=
  Lookup[rules, side, Lookup[rules, toSideClass @ side, Lookup[rules, All, 0]]];

LookupSide[rules_, side_] :=
  Lookup[rules, side,
  Lookup[rules, toSideClass @ side,
  Lookup[rules, toMultiClassC @ side,
  Lookup[rules, toMultiClassA @ side,
  Lookup[rules, All, 0]]]]];

toMultiClassC = <|Bottom -> BottomLeft, Left -> TopLeft, Top -> TopRight, Right -> BottomRight|>;
toMultiClassA = <|Bottom -> BottomRight, Left -> BottomLeft, Top -> TopLeft, Right -> TopRight|>;

LookupSide[rules_][side_] :=
  LookupSide[rules, side];

toSideClass = Case[
  Left|Right := Horizontal;
  Bottom|Top := Vertical;
  other_     := Null;
]

(**************************************************************************************************)

PrivateFunction[ToSquarePlotRange]

ToSquarePlotRange[{{x1_, x2_}, {y1_, y2_}}] := Scope[
  w = x2 - x1; h = y2 - y1;
  d2 = Max[w, h] / 2; x = Mean[{x1, x2}]; y = Mean[{y1, y2}];
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

PrivateFunction[GraphicsPlotRange]

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
"

PlotRangeSize[range_] := EuclideanDistance @@@ range;

(**************************************************************************************************)

PrivateFunction[LookupImageSize]

SetUsage @ "
LookupImageSize[object$] returns the setting of %ImageSize that a given object will use when rendered.
* object$ can be a %Graphics[$$] or %Graphics3D[$$] object.
* A numeric hard-coded size will be returned as-is.
* Symbolic sizes like %Tiny, %Small, %Medium, %Large will be converted to their numeric equivalents.
* The size is returned as a pair {width$, height$}, where height$ may be Automatic.
"

DeclareArgumentCount[LookupImageSize, 1];

Options[LookupImageSize] = {AspectRatio -> Automatic};

LookupImageSize[obj_, OptionsPattern[]] := Scope[
  {imageSize, aspectRatio} = LookupOption[obj, {ImageSize, AspectRatio}];
  SetAutomatic[aspectRatio, OptionValue @ AspectRatio];
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

PrivateFunction[ToNumericImageSize]

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

PrivateVariable[$SymbolicPointSizes]

$SymbolicPointSizes = <|
  Tiny -> 2, Small -> 3,
  MediumSmall -> 4, Medium -> 5, MediumLarge -> 6,
  Large -> 7, Huge -> 10
|>;

(**************************************************************************************************)

PrivateVariable[$SymbolicSizeFractions]

$SymbolicSizeFractions = <|
  Tiny -> 0.25, Small -> 0.5,
  MediumSmall -> 0.75, Medium -> 1.0, MediumLarge -> 1.25,
  Large -> 1.5, Huge -> 2.0
|>;

(**************************************************************************************************)

PrivateVariable[$ImageWidthTable]

$ImageWidthTable = <|
  Tiny -> 100,
  Small -> 180,
  MediumSmall -> 270,
  Medium -> 360, Automatic -> 360,
  MediumLarge -> 468,
  Large -> 576,
  Huge -> 720
|>;

(**************************************************************************************************)

PrivateFunction[toStandardImageSize]

toStandardImageSize[sym:(MediumSmall|MediumLarge|Huge)] := Lookup[$ImageWidthTable, sym];
toStandardImageSize[other_] := other;


PrivateFunction[toNumericSizeScale]

$sizeScaleAssoc = KeyDrop[$ImageWidthTable / $ImageWidthTable[Medium], Automatic];

toNumericSizeScale = Case[
  sym_Symbol := Lookup[$sizeScaleAssoc, sym, 1];
  Scaled[r_] := Clip[N @ r, {.01, 10.}];
  _          := 1
];

(**************************************************************************************************)

PrivateFunction[ApplyEpilog]

ApplyEpilog[graphics_, None | {}] := graphics;

ApplyEpilog[Labeled[graphics_, args__], epilog_] :=
  Labeled[ApplyEpilog[graphics, epilog], args];

ApplyEpilog[graphics_Graphics, epilog_] :=
  UpdateOptions[graphics, Epilog, Function[If[#1 === {}, epilog, {epilog, #1}]]];

ApplyEpilog[Graphics3D[primitives_, opts___], epilog_] :=
  Graphics3D[{primitives, epilog}, opts];

ApplyEpilog[epilog_][graphics_] := ApplyEpilog[graphics, epilog];

(**************************************************************************************************)

PrivateFunction[ApplyProlog]

ApplyProlog[graphics_, None | {}] := graphics;

ApplyProlog[Labeled[graphics_, args__], prolog_] :=
  Labeled[ApplyProlog[graphics, prolog], args];

ApplyProlog[graphics_Graphics, prolog_] :=
  UpdateOptions[graphics, Prolog, Function[If[#1 === {}, prolog, {#1, prolog}]]];

ApplyProlog[Graphics3D[primitives_, opts___], prolog_] :=
  Graphics3D[{prolog, primitives}, opts];

ApplyProlog[prolog_][graphics_] := ApplyProlog[graphics, prolog];

(**************************************************************************************************)

PrivateFunction[ApplyLegend]

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

PublicFunction[NiceTooltip]

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

PublicFunction[ColorFramed]

ColorFramed[boxes_, color_] := Framed[boxes, ContentPadding -> False, FrameStyle -> color];

(**************************************************************************************************)

PrivateFunction[TransformArrowheads]

$arrowheadTransforms = <|
  "Reverse" -> reverseArrowhead,
  "OverBar" -> addInversionBar[True],
  "UnderBar" -> addInversionBar[False],
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

addInversionBar[isOver_][{size_, pos_, graphics:Graphics[primitives_, opts___]}] := Scope[
  {{xl, xh}, {yl, yh}} = GraphicsPlotRange @ graphics;
  yb = If[isOver, yh * 1.4, yl * 1.4];
  graphics = Graphics[{primitives, {Opacity[1], Black, AbsoluteThickness[0.5], Line @ {{xl, yb}, {xh, yb}}}}, opts];
  {size, pos, graphics}
];

(**************************************************************************************************)

PublicFunction[SetFrameColor]

SetFrameColor[g_Graphics, color_] :=
  If[LookupOption[g, Frame] === True,
    ReplaceOptions[g, FrameStyle -> color],
    ReplaceAll[g, a:Annotation[_, "Frame"] :> ReplaceAll[a, EdgeForm[_] :> EdgeForm[color]]]
  ];

SetFrameColor[expr_, _] := expr;

(**************************************************************************************************)

PublicFunction[FadeGraphics]

FadeGraphics[g_, 0 | 0.] := g;
FadeGraphics[g_Graphics, opacity_] := ApplyEpilog[g,
  Style[
    Rectangle[{-1000, -1000}, {1000, 1000}],
    EdgeForm[None], FaceForm[GrayLevel[1, opacity]]
  ]
];

(**************************************************************************************************)

PublicFunction[ListShowFaded]

ListShowFaded[list_, i_, opacity_:0.9] := Scope[
  list = VectorReplace[list, g_Graph :> ExtendedGraphPlot[g]];
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

PublicFunction[ShowFaded]

ShowFaded[items__, opacity_?NumericQ] := ListShowFaded[{items}, -1, opacity];
ShowFaded[items__] := ListShowFaded[{items}, -1];

(**************************************************************************************************)

PrivateFunction[SimplifyGraphicsPrimitives]

SimplifyGraphicsPrimitives[primitives_] :=
  simplifyPrimitiveAnnotations @ simplifyPrimitiveStyles @ primitives;

$simplifyPrimitiveStyleRules = Dispatch @ {
  Directive[{}] :> {},
  Style[g_] :> g,
  Style[g_, {} | {{}}] :> g,
  Style[Style[g_, a___], b___] :> Style[g, a, b]
};

simplifyPrimitiveStyles[primitives_] :=
  ReplaceRepeated[primitives, $simplifyPrimitiveStyleRules];

$simplifyPrimitiveAnnotationRules = Dispatch @ {

  (* a single annotation with uniform primitives can use a single larger primitive *)
  anno:Annotation[{__Point} | {__Line} | {__Arrow}, __] :> joinAnnotationPrimitives[anno],

  (* fragmented uniform primitives can be combined into a single larger primitive *)
  annos:{Annotation[(head:(Point | Line | Arrow))[_], _List, type_]..} :> joinHeadAnnotations[annos, type],

  (* fragmented list primitives can be joined  *)
  annos:{Annotation[_List, _List, type_]..} :> joinListAnnotations[annos, type],

  (* singleton annos can be joined, even if they are non-uniform / wrapped in style *)
  annos:{Annotation[_, {_}, type_]..} :> joinSingletonAnnotations[annos, type]
};

joinHeadPrimitives[prims_] := Scope[
  head = Part[prims, 1, 0];
  coords = Part[prims, All, 1];
  normFunc = If[head === Point, toCoordinateMatrix, toCoordinateArray];
  coordsArray = ToPackedRealArrays @ Apply[Join] @ Map[normFunc] @ coords;
  head[coordsArray]
];

joinAnnotationPrimitives[Annotation[prims_, args__]] :=
  Annotation[joinHeadPrimitives @ prims, args];

joinHeadAnnotations[annos_, type_] := Scope[
  primitives = joinHeadPrimitives @ Part[annos, All, 1];
  indices = Join @@ Part[annos, All, 2];
  Annotation[primitives, indices, type]
];

toCoordinateArray[e_] := If[CoordinateArrayQ[e] || VectorQ[e, CoordinateMatrixQ], e, List @ e];
toCoordinateMatrix[e_] := If[CoordinateMatrixQ[e], e, List @ e];

joinListAnnotations[annos_, type_] :=
  Annotation[Join @@ Part[annos, All, 1], Join @@ Part[annos, All, 2], type];

joinSingletonAnnotations[annos_, type_] :=
  Annotation[Part[annos, All, 1], Part[annos, All, 2, 1], type];

(* a list of singleton-index annotations can be replaced with a single such primitive annotation *)
simplifyPrimitiveAnnotations[primitives_] :=
  ReplaceRepeated[primitives, $simplifyPrimitiveAnnotationRules];

(**************************************************************************************************)

PublicFunction[Lerp]

SetUsage @ "
Lerp[a$, b$, f$] linearly interpolates between a$ and b$, where f$ = 0 gives a$ and f$ = 1 gives b$.
Lerp[a$, b$, {f$1, f$2, $$}] gives a list of interpolations.
Lerp[a$, b$, Into[n$]] gives the n$ values interpolated between a$ and b$.
Lerp[f$] is the operator form of Lerp$.
* a$ and b$ can be numbers, arrays, etc.
"

Lerp[a_, b_, f_] := a * (1 - f) + b * f;
Lerp[a_, b_, f_List] := Lerp[a, b, #]& /@ f;

Lerp[a_, b_, Into[0]] := {};
Lerp[a_, b_, Into[1]] := (a + b) / 2;
Lerp[a_, b_, Into[2]] := {a, b};
Lerp[a_, b_, Into[n_]] := Lerp[a, b, Range[0, 1, 1/(n-1)]]

Lerp[n_][a_, b_] :=Lerp[a, b, n];

(**************************************************************************************************)

PublicFunction[Interpolated]

SetUsage @ "
Interpolated[a$, b$, n$] is equivalent to %Lerp[a$, b$, Into[n$]].
"

Interpolated[a_, b_, n_] := Table[b * i + a * (1 - i), {i, 0, 1, 1/(n-1)}];

(**************************************************************************************************)

PublicFunction[AngleRange]

SetRelatedSymbolGroup[AngleRange, AngleDifference];

SetUsage @ "
AngleRange[a$, b$, Into[n$]] gives n$ angles between a$ and b$.
* The angles are chosen in the direction that yields the shortest distance modulo %%Tau.
* All values are given modulo %%Tau.
"

AngleRange[a_, b_, Into[0]] := {};
AngleRange[a_, b_, Into[1]] := {Mod[(a + b), Tau] / 2};
AngleRange[a_, b_, Into[n_]] := NestList[PlusOperator[AngleDifference[a, b] / (n-1)], a, n-1];
AngleRange[a_, b_, da_] := AngleRange[a, b, Into[Ceiling[1 + Abs[AngleDifference[a, b]] / da]]];

(**************************************************************************************************)

PublicFunction[AngleDifference]

SetUsage @ "
AngleDifference[a$, b$, Into[n$]] gives the signed distance between two angles a$ and b$.
* This is the smallest difference between a$ and b$ modulo %%Tau.
"

AngleDifference[a_, b_] := If[Abs[b - a] > Pi, Mod[Mod[b, Tau] - Mod[a, Tau], Tau, -Pi], b - a];
