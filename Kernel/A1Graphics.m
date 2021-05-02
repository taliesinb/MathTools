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

(**************************************************************************************************)

PackageExport["$LegendLabelStyle"]

$LegendLabelStyle = {FontFamily -> "Avenir", FontSize -> 12};

(**************************************************************************************************)

PackageExport["GraphicsPlotRange"]

SetUsage @ "
GraphicsPlotRange[graphics$] yields the PlotRange that will be used when graphics$ is rendered.
* graphics$ can be a Graphics[$$] or GraphicsBox[$$] expression.
"

expandMultiArrowInGC[g_] := g /.
  Arrow[segments:{{__Integer}..}, opts___] :> RuleCondition[Map[Arrow[#, opts]&, segments]];

expandGC[g_] := g /.
  gc_GraphicsComplex :> RuleCondition[Normal @ expandMultiArrowInGC @ gc];

GraphicsPlotRange[g_Graphics] := Scope[
  {{l, r}, {b, t}} = plotRange = PlotRange @ expandGC @ g;
  w = r - l; h = t - b;
  padding = LookupOption[g, PlotRangePadding];
  SetAutomatic[padding, Scaled[0.02]];
  Switch[padding,
    Scaled[_],
      scale = First[padding];
      factor = scale / (1 - 2 * scale); dw = dh = Max[{w, h} * factor];
      {{l - dw, r + dw}, {b - dh, t + dh}},
    _, plotRange
  ]
];

GraphicsPlotRange[g_GraphicsBox] := GraphicsPlotRange @ ReplaceAll[g, $graphicsBoxReplacements];

GraphicsPlotRange[elems_] := PlotRange @ Graphics[elems, PlotRange -> Automatic];

$graphicsBoxSymbols = {
  TooltipBox, StyleBox, GraphicsBox, GraphicsGroupBox, RectangleBox, DiskBox, CircleBox, TextBox, PointBox,
  LineBox, ArrowBox, PolygonBox, BSplineCurveBox, BezierCurveBox, RasterBox, GraphicsComplexBox
};

$boxSymbolToOrdinarySymbol = AssociationMap[Symbol[StringDrop[SymbolName[#], -3]]&, $graphicsBoxSymbols];

$graphicsBoxReplacements = Dispatch @ Normal @ $boxSymbolToOrdinarySymbol;

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
* A numeric hard-coded size will be returned as-is.
* Symbolic sizes like Tiny, Small, Medium, Large will be converted to their numeric equivalents.
* The size is returned as a pair {width$, height$}.
"

DeclareArgumentCount[LookupImageSize, 1];

LookupImageSize[obj_] := Scope[
  size = LookupOption[obj, ImageSize];
  Switch[size,
    {_ ? NumberQ, Automatic | (_ ? NumberQ)}, size,
    _ ? NumberQ, {size, Automatic},
    s_Symbol, {Lookup[$ImageWidthTable, s], Automatic},
    _, {720, Automatic}
  ]
];


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

PackageScope["toNumericImageSize"]

toNumericImageSize[sym_Symbol] := Lookup[$ImageWidthTable, sym, 360];
toNumericImageSize[w_] := w;
toNumericImageSize[{w_, h_}] := w;

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
      ImageMargins -> 5, ImageSize -> {{30, 300}, {30, 300}},
      Alignment -> Center
    ],
    TooltipStyle -> {Background -> White, CellFrameColor -> None, CellFrame -> 0}
  ];

(**************************************************************************************************)

PackageExport["LabeledMatrixForm"]

declareFormatting[
  LabeledMatrixForm[expr_] :> formatLabeledMatrices[expr]
];

formatLabeledMatrices[expr_] := ReplaceAll[expr,
  matrix_ /; MatrixQ[Unevaluated @ matrix] /; Length[Unevaluated @ matrix] =!= 1 :> RuleCondition @ formatLabeledMatrix @ matrix
]

formatLabeledMatrix[matrix_] := Scope[
  tooltips = MapIndexed[Tooltip, matrix, {2}];
  MatrixForm[tooltips, TableHeadings -> Automatic]
];

