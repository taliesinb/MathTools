Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

PackageExport["$ColorPattern"]

$ColorPattern = _RGBColor | _GrayLevel | _CMYKColor | _Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor;

SetUsage @ "
$ColorPattern is a pattern that matches a valid color, like RGBColor[$$] etc.
"

(**************************************************************************************************)

PackageExport["OklabColor"]

SetUsage @ "
OklabColor[l$, a$, b$] returns an RGBColor[$$] corresponding to the given color in the OkLAB colorspace.
"

DeclareArgumentCount[OklabColor, 3];

$OklabToLMS = {
  {+1, +0.3963377774, +0.2158037573},
  {+1, -0.1055613458, -0.0638541728},
  {+1, -0.0894841775, -1.2914855480}
};

$LMSToSRGB = {
  {+4.0767416621, -3.3077115913, +0.2309699292},
  {-1.2684380046, +2.6097574011, -0.3413193965},
  {-0.0041960863, -0.7034186147, +1.7076147010}
};

$SRGBToLMS = {
  {0.4122214708, 0.5363325363, 0.0514459929},
  {0.2119034982, 0.6806995451, 0.1073969566},
  {0.0883024619, 0.2817188376, 0.6299787005}
};

$LMSToOklab = {
  {+0.2104542553, +0.7936177850, -0.0040720468},
  {+1.9779984951, -2.4285922050, +0.4505937099},
  {+0.0259040371, +0.7827717662, -0.8086757660}
};

OklabColor[l_, a_, b_] := FromOklab[{l, a, b}];

OklabToSRGB[lab_List] := Dot[$LMSToSRGB, Dot[$OklabToLMS, lab]^3];
OklabToSRGB[lab_List ? MatrixQ] := Map[OklabToSRGB, lab];

SRGBToOklab[srgb_List] := Dot[$LMSToOklab, CubeRoot @ Dot[$SRGBToLMS, srgb]];
SRGBToOklab[srgb_List ? MatrixQ] := Map[SRGBToOklab, srgb];

SetListable[RGBToSRGB, SRGBToRGB];
SRGBToRGB[x_] := If[x >= 0.0031308, 1.055 * x^(1.0/2.4) - 0.055, 12.92 * x];
RGBToSRGB[x_] := If[x >= 0.04045, ((x + 0.055)/(1 + 0.055))^2.4, x / 12.92];

RGBToOklab[rgb_List] := SRGBToOklab @ RGBToSRGB @ rgb;
OklabToRGB[lab_List] := SRGBToRGB @ OklabToSRGB @ lab;

ToOklab[RGBColor[r_, g_, b_]] := RGBToOklab[{r, g, b}];
ToOklab[RGBColor[rgb:{_, _, _}]] := RGBToOklab[rgb];
ToOklab[c:$colorPattern] := RGBToOklab[List @@ ColorConvert[c, "RGB"]];
ToOklab[e_] := RGBToOklab[ToRGB[e]];

FromOklab[lab_List] := RGBColor @ OklabToRGB[lab];
FromOklab[lab_List ? MatrixQ] := RGBColor /@ OklabToRGB[lab]

$toRGBRules = Dispatch[{
  RGBColor[r_, g_, b_] :> {r, g, b},
  RGBColor[{r_, g_, b_}] :> {r, g, b},
  c:(_GrayLevel | XYZColor | CMYKColor | Hue | XYZColor | LABColor | LCHColor | LUVColor) :>
    RuleCondition[List @@ ColorConvert[c, "RGB"]]
}];

normalizeLightness[colors_] := Scope[
  {l, a, b} = Transpose @ ToOklab[colors];
  l[[All]] = Mean[l];
  FromOklab[Transpose[{l, a, b}]]
];

ToRGB[e_] := ReplaceAll[e, $toRGBRules];

(**************************************************************************************************)

PackageExport["OklabBlend"]

SetUsage @ "
OklabBlend[colors$] blends a list of ordinary colors, but in OkLAB colorspace.
"

DeclareArgumentCount[OklabBlend, 1];

OklabBlend[colors_List] := FromOklab @ Mean @ ToOklab[colors];

(**************************************************************************************************)

PackageExport["BlendFunction"]

SetUsage @ "
BlendFunction[{v$1, $$, v$n}, {c$1, $$, c$n}] returns a function that will take a value \
in the range [v$1, v$n] and interpolate a corresponding color based on the matching colors \
c$1 to c$n.
* Colors are blended in the OkLAB colorspace.
* BlendFunction returns a ColorFunctinoObject[$$].
"

DeclareArgumentCount[BlendFunction, 2];

BlendFunction[values_, colors_] := Scope[
  okLabValues = ToOklab[colors];
  interp = Interpolation[Transpose[{values, okLabValues}], InterpolationOrder -> 1];
  ColorFunctionObject[values, interp /* OklabToRGB, "Linear"]
];

(**************************************************************************************************)

PackageExport["ColorFunctionObject"]

SetUsage @ "
ColorFunctionObject[$$] represents a function that takes values and returns colors.
"

Format[cf:ColorFunctionObject[_List, func_, "Linear"|"Log"], StandardForm] :=
  formatColorFunction[cf];

ColorFunctionObject[_, func_, _][value_] := RGBColor @ func[value];
ColorFunctionObject[_, func_, _][value_List] := Map[RGBColor, Map[func, value]];

formatColorFunction[ColorFunctionObject[values_List, func_, "Linear"]] := Scope[
  {min, max} = MinMax[values]; range = max - min; dx = range / 100;
  spaced = N @ Range[min, max, dx]; offsets = (values - min) / dx;
  row = func /@ spaced;
  plot = Image[{row, row, row, row, row}, ImageSize -> 200, BaselinePosition -> Scaled[0.2]];
  Row[{plot, "  ", "(", min, " to ", max, ")"}, BaseStyle -> {FontFamily -> "Avenir"}]
]
bf = BlendFunction[{-1,0, 1},{Red,White,Blue}]

toBalancedGradientColorMap[{min_, max_}] := Scope[
  max = Max[Abs[min], Abs[max]];
  BlendFunction[{-max, 0, max}, {Blue, White, Red}]
];

toBalancedGradientColorMap[{0, max_}] := BlendFunction[{0, max}, {White, Red}];

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

PackageExport["$Red"]
PackageExport["$Green"]
PackageExport["$Blue"]
PackageExport["$Orange"]
PackageExport["$Pink"]
PackageExport["$Cyan"]
PackageExport["$Gray"]

$Red = RGBColor[0.91, 0.23, 0.14];
$Green = RGBColor[0.24, 0.78, 0.37];
$Blue = RGBColor[0.21, 0.53, 0.86];
$Orange = RGBColor[1, 0.59, 0.25];
$Pink = RGBColor[0.87, 0.19, 0.75];
$Cyan = RGBColor[0, 0.75, 0.74];
$Gray = GrayLevel[0.53];

(**************************************************************************************************)

PackageExport["$ColorPalette"]

$ColorPalette = {$Red, $Green, $Blue, $Orange, $Pink, $Gray};

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

PackageExport["ApplyEpilog"]

ApplyEpilog[graphics_, None | {}] := graphics;

ApplyEpilog[graphics_Graphics, epilog_] :=
  UpdateOptions[graphics, Epilog, Function[Append[Developer`ToList @ #1, epilog]]];

(**************************************************************************************************)

PackageExport["ApplyLegend"]

ApplyLegend[expr_, None | Automatic] := expr;

ApplyLegend[expr_, legend_] := Legended[expr, legend];

(**************************************************************************************************)

NiceTooltip[g_, None] := g;

NiceTooltip[g_, e_] :=
  Tooltip[g, Pane[e, BaseStyle -> {FontSize -> 15, "Output"}, ImageMargins -> 5]];
