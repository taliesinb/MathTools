PublicFunction[HexColorString]

HexColorString[c:$ColorPattern] := HexColorString[c] = toHexColor @ ToRGB @ c;
_HexColorString := BadArguments[];

toHexColor[c_List] := toHexColor[c] = StringJoin @ IntegerString[Floor[(255 * c) + 1 / 2], 16, 2];
_toHexColor := BadArguments[];

(**************************************************************************************************)

PublicFunction[FromHexColorString]

FromHexColorString[str_String] /; StringLength[str] == 6 := fromHexColor[str];
_FromHexColorString := BadArguments[];

SetListable[fromHexValue];
fromHexColor[s_] := fromHexColor[s] = RGBColor @ N @ fromHexValue @ StringTake[s, {{1, 2}, {3, 4}, {5, 6}}];
fromHexValue[s_] := FromDigits[s, 16] / 255.;

(**************************************************************************************************)

PublicFunction[HTMLColorString, FromHTMLColorString]

HTMLColorString[None] := "none";
HTMLColorString[Black] := "black";
HTMLColorString[White] := "white";
HTMLColorString[c:$ColorPattern] := HTMLColorString[c] = StringJoin["#", HexColorString @ c];
_HTMLColorString := BadArguments[];

FromHTMLColorString["none"] := None;
FromHTMLColorString["black"] := Black;
FromHTMLColorString["white"] := White;
FromHTMLColorString[str_String] /; StringLength[str] == 7 := FromHexColorString @ StringDrop[str, 1];
_FromHTMLColorString := BadArguments[];

(**************************************************************************************************)

PublicFunction[SetColorOpacity]

$setColorOpacityRules = Dispatch[{
  RGBColor[r_, g_, b_] :> RGBColor[r, g, b, $opacity],
  RGBColor[{r_, g_, b_}] :> RGBColor[r, g, b, $opacity],
  GrayLevel[g_] :> GrayLevel[g, $opacity],
  Hue[h_] :> Hue[h, 1, 1, $opacity],
  c:(_Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor) /; Length[c] === 3 :> Append[c, $opacity]
}];

SetColorOpacity[expr_, None] :=
  expr;

SetColorOpacity[expr_, opacity_] :=
  expr /. $setColorOpacityRules /. ($opacity -> opacity)

SetColorOpacity[opacity_][expr_] :=
  SetColorOpacity[expr, opacity];

(**************************************************************************************************)

PublicFunction[SetOpaque]

$opaqueRules = Dispatch[{
  RGBColor[r_, g_, b_, ___] :> RGBColor[r, g, b, 1],
  RGBColor[{r_, g_, b_, ___}] :> RGBColor[r, g, b, 1],
  GrayLevel[g_, ___] :> GrayLevel[g, 1],
  Hue[h_, s_:1, v_:1, ___] :> Hue[h, s, v, 1],
  c:(_Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor) /; Length[c] === 3 :> Append[c, 1]
}];

SetOpaque[expr_] := expr /. $opaqueRules;

(**************************************************************************************************)

PublicFunction[SetColorLightness]

SetColorLightness[expr_, lightness_] :=
  expr /. c:$ColorPattern :> RuleCondition @ OklabSetLightness[c, lightness];

SetColorLightness[lightness_][expr_] :=
  SetColorLightness[expr, lightness];

(**************************************************************************************************)

PublicFunction[RemoveColorOpacity]

$removeColorOpacityRules = Dispatch[{
  Opacity[_, c_] :> c,
  RGBColor[r_, g_, b_, _] :> RGBColor[r, g, b],
  RGBColor[{r_, g_, b_, _}] :> RGBColor[r, g, b],
  GrayLevel[g_, _] :> GrayLevel[g],
  c:(_Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor) /; Length[c] === 4 :> Take[c, 3]
}];

RemoveColorOpacity[expr_] :=
  expr /. $removeColorOpacityRules;

(**************************************************************************************************)

PublicFunction[ContainsOpacityColorsQ]

$opacityColorsPattern = Alternatives[
  RGBColor[{_, _, _, _}],
  GrayLevel[_, _],
  _Opacity,
  c:(_RGBColor | _Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor) /; Length[c] === 4
];

ContainsOpacityColorsQ[expr_] := !FreeQ[expr, $opacityColorsPattern];

(**************************************************************************************************)

PublicFunction[ExtractFirstOpacity]

$opacityRule = Alternatives[
  RGBColor[{_, _, _, o_}], GrayLevel[_, o_], Opacity[o_], Opacity[o_, _],
  RGBColor[_, _, _, o_], Hue[_, _, _, o_], XYZColor[_, _, _, o_], LABColor[_, _, _, o_],
  LCHColor[_, _, _, o_], LUVColor[_, _, _, o_]
] :> o;

ExtractFirstOpacity[expr_] := FirstCase[expr /. $opacityNormalizationRules, $opacityRule, None, {0, Infinity}];

(**************************************************************************************************)

PublicFunction[ColorOpacity]

ColorOpacity[color_] := FirstCase[Replace[color, $opacityNormalizationRules], $opacityRule, 1, {0}];

(**************************************************************************************************)

PublicFunction[ColorVectorQ]

ColorVectorQ[{Repeated[$ColorPattern]}] := True;
ColorVectorQ[_] := False;

(**************************************************************************************************)

PublicFunction[OklabColor]

SetUsage @ "
OklabColor[l$, a$, b$] returns an %RGBColor[$$] corresponding to the given color in the OkLAB colorspace.
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

(**************************************************************************************************)

PublicFunction[OklabLightness]

OklabLightness[color_] := Scope[
  ok = ToOklab[color];
  If[MatrixQ[ok], ok[[All, 1]], First[ok]]
];

(**************************************************************************************************)

PublicFunction[OklabSetLightness]

(* TODO: do this properly inside ToOklab *)
OklabSetLightness[color:$opacityColorsPattern, lightness_] :=
  SetColorOpacity[
    OklabSetLightness[RemoveColorOpacity @ color, lightness],
    ExtractFirstOpacity @ color
  ];

OklabSetLightness[Opacity[o_, color_], lightness_] :=
  Opacity[o, OklabSetLightness[color, lightness]];

OklabSetLightness[color_, lightness_] := Scope[
  ok = ToOklab[color];
  If[MatrixQ[ok], ok[[All, 1]] = lightess, ok[[1]] = lightness];
  FromOklab[ok]
]

(**************************************************************************************************)

PublicFunction[OklabDarker, OklabLighter]

timesMatrix[vec1_, vec2_] := If[MatrixQ[vec2], vec1 * #& /@ vec2, vec1 * vec2];
OklabDarker[color_, amount_:.2] := FromOklab @ timesMatrix[{1 - amount, 1, 1}, ToOklab @ color];
OklabLighter[color_, amount_:.25] := OklabDarker[color, -amount];

(**************************************************************************************************)

PublicFunction[OklabPaler, OklabDeeper]

OklabPaler[color_, amount_:.2] := FromOklab @ timesMatrix[{1 + amount, 1 - amount, 1 - amount}, ToOklab @ color];
OklabDeeper[color_, amount_:.2] := OklabPaler[color, -amount];

(**************************************************************************************************)

PublicFunction[OklabToRGB, RGBToOklab]

RGBToOklab[rgb_List] := SRGBToOklab @ RGBToSRGB @ rgb;
OklabToRGB[lab_List] := Clip[SRGBToRGB @ OklabToSRGB @ lab, {0., 1.}];

(**************************************************************************************************)

PublicFunction[ToOklab, FromOklab]

ToOklab[RGBColor[r_, g_, b_]] := RGBToOklab[{r, g, b}];
ToOklab[RGBColor[rgb:{_, _, _}]] := RGBToOklab[rgb];
ToOklab[c:$colorPattern] := RGBToOklab[List @@ ColorConvert[c, "RGB"]];
ToOklab[e_] := RGBToOklab[ToRGB[e]];

FromOklab[lab_List] := RGBColor @@ OklabToRGB[lab];
FromOklab[lab_List ? MatrixQ] := RGBColor @@@ OklabToRGB[lab]

normalizeLightness[colors_, fraction_:1] := Scope[
  {l, a, b} = Transpose @ ToOklab[colors];
  l[[All]] = (Mean[l] * fraction) + l[[All]] * (1 - fraction);
  FromOklab @ Trans[l, a, b]
];

(**************************************************************************************************)

PublicFunction[ToOkLCH, FromOkLCH]

ToOkLCH[color_] := toLCH @ ToOklab[color];

toLCH[{l_, a_, b_}] := {l, Norm @ {a, b}, Mod[ArcTan2[a, b] / Tau, 1]};
toLCH[matrix_ ? MatrixQ] := ToPackedReal @ Map[toLCH, matrix];

FromOkLCH[lch_List] := FromOklab @ fromLCH[lch];

fromLCH[{l_, c_, h_}] := {l, c * Cos[h * Tau], c * Sin[h * Tau]};
fromLCH[matrix_ ? MatrixQ] := ToPackedReal @ Map[fromLCH, matrix];

(**************************************************************************************************)

PrivateFunction[ToRGB]

ToRGB[e_] := ReplaceAll[e, $toRGBRules];

$toRGBRules = Dispatch[{
  RGBColor[r_, g_, b_, ___] :> {r, g, b},
  RGBColor[{r_, g_, b_, ___}] :> {r, g, b},
  c:(_GrayLevel | _XYZColor | _CMYKColor | _Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor) :>
    RuleCondition[List @@ ColorConvert[c, "RGB"]]
}];

(**************************************************************************************************)

PublicFunction[NormalizeColorLightness]

NormalizeColorLightness[colors_List, fraction_:1] :=
  normalizeLightness[colors, fraction];

(**************************************************************************************************)

PublicVariable[$ColorPalette, $NormalColorPalette]

PublicVariable[$Blue, $Red, $Yellow, $Green, $Pink, $Teal, $Orange, $Purple, $Gray]

{$Blue, $Red, $Green, $Pink, $Teal, $Yellow, $Orange, $Purple, $Gray} =
  Map[RGBColor, StringSplit @ "#3e81c3 #e1432d #4ea82a #c74883 #47a5a7 #f6e259 #dc841a #8b7ebe #929292"]

$ColorPalette = $NormalColorPalette = {$Red, $Blue, $Green, $Orange, $Purple, $Teal, $Gray, $Pink, $Yellow};

PublicVariable[$DarkColorPalette, $DarkBlue, $DarkRed, $DarkYellow, $DarkGreen, $DarkPink, $DarkTeal, $DarkOrange, $DarkPurple, $DarkGray]

{$DarkRed, $DarkBlue, $DarkGreen, $DarkOrange, $DarkPurple, $DarkTeal, $DarkGray, $DarkPink, $DarkYellow} =
  $DarkColorPalette = OklabDarker @ $ColorPalette;

PublicVariable[$LightColorPalette, $LightBlue, $LightRed, $LightYellow, $LightGreen, $LightPink, $LightTeal, $LightOrange, $LightPurple, $LightGray]

{$LightRed, $LightBlue, $LightGreen, $LightOrange, $LightPurple, $LightTeal, $LightGray, $LightPink, $LightYellow} =
  $LightColorPalette = OklabLighter @ $ColorPalette;

(**************************************************************************************************)

PublicHead[Paletted]

PrivateVariable[$paletteUsageString]

$paletteUsageString = StringTrim @ "
* palette$ can be an one of the following:
| Automatic | the default color palette |
| 'Light', 'Dark' | a lighter or darker version of the default palette |
| spec$ -> 'Light' | a lighter version of spec$ |
| spec$ -> 'Dark' | a darker version of spec$ |
| {c$1, c$2, $$} | an explicit list of colors |
| %Offset[spec$, n$] | a rotated version of spec$ |
| %Opacity[o$, spec$] | spec$ with opacity set to o$ |
| 'Basic', 'Cool', 'Subdued' | a named palette |
| 'set$' -> 'variant$' | a lightness variant of 'set$' |
* Lightness variants are 'VeryLight', 'Light', 'Medium', 'Dark'.
";

SetUsage @ "
Paletted[f$, palette$] indictes that a color function f$ should be applied using the color palette
palette$.
<*$paletteUsageString*>
"

(**************************************************************************************************)

PublicFunction[ToColorPalette]

SetUsage @ "
ToColorPalette[palette$] returns an explicit list of colors from a palette specification.
ToColorPalette[palette$, n$] returns exactly n$ colors from the palette.
<*$paletteUsageString*>
"

$ecnp = Alternatives @@ $ExtendedColorNames;

ToColorPalette::invalid = "`` is not a valid color palette.";

ToColorPalette = Case[
  Automatic | "Medium"          := $NormalColorPalette;
  "Light"                       := $LightColorPalette;
  "Dark"                        := $DarkColorPalette;
  spec_ -> "Light"              := OklabLighter @ % @ spec;
  spec_ -> "Dark"               := OklabDarker @ % @ spec;
  set:$ecnp                     := getNamedColorSet[set, "Medium"];
  set:$ecnp -> variant_String   := getNamedColorSet[set, variant];
  list_List ? ColorVectorQ      := list /. $colorNormalizationRules;
  Offset[spec_, n_Integer]      := RotateLeft[% @ spec, n];
  Opacity[o_, spec_]            := SetColorOpacity[% @ spec, N @ o];
  spec_                         := (Message[ToColorPalette::invalid, spec]; $Failed)
];

getNamedColorSet[set_, variant_] := Scope[
  colors = $ExtendedColorsGrouped[set, variant];
  If[MissingQ[colors], ReturnFailed[]];
  Values @ colors
];

ToColorPalette[spec_, n_Integer] := Scope[
  colors = ToColorPalette[spec];
  If[FailureQ[colors], ReturnFailed[]];
  If[n > 40, Return @ Table[RandomColor[], n]];
  If[Length[colors] < n, colors //= expandColorPalette];
  Take[colors, n]
];

expandColorPalette[colors_] := Join[colors, OklabBlend /@ (UnorderedPairs @ colors)];

(**************************************************************************************************)

PublicFunction[GrayColorQ]

GrayColorQ[_GrayLevel] := True;
GrayColorQ[r_RGBColor] := SameQ @@ Round[Flatten @ Apply[List, r], .05];
GrayColorQ[_] := False;

(**************************************************************************************************)

PublicFunction[OklabBlend]

SetUsage @ "
OklabBlend[colors$] blends a list of ordinary colors, but in OkLAB colorspace.
OklabBlend[colors$, i$] blends part $i between the colors, where $i runs from 0 to 1.
"

DeclareArgumentCount[OklabBlend, 1];

OklabBlend[colors_List] := FromOklab @ Mean @ ToOklab[colors];

OklabBlend[colors_List, i_] := oklabInterpolation[colors, i] // FromOklab;

(**************************************************************************************************)

PublicFunction[OklabBlendFunction]

OklabBlendFunction[colors_, Automatic] := OklabInterpolation[colors] /* FromOklab

oklabInterpolation[colors_, args___] :=
  Interpolation[
    Trans[Lerp[0, 1, Into @ Length @ colors], ToOklab @ colors],
    args
  ];

(**************************************************************************************************)

PublicFunction[HumanBlend]

HumanBlend[colors_List] := iHumanBlend @ Sort @ colors;

iHumanBlend = Case[
  {}                := w2;
  {c_}              := c;
  {w0, w2}          := w1;     {w0, w1, w2}      := w1;        {bl, wh}          := w1;
  {b0, g0, r0}      := w0;     {b1, g1, r1}      := w1;        {b2, g2, r2}      := w2;
  {t0, p0, o0}      := w0;     {t1, p1, o1}      := w1;        {t2, p2, o2}      := w2;
  {b0, r0}          := p0;     {b1, r1}          := p1;        {b2, r2}          := p2;
  {b0, g0}          := t0;     {b1, g1}          := t1;        {b2, r2}          := t2;
  {g0, r0}          := o0;     {g1, r1}          := o1;        {g2, r2}          := o2;
  {bl, r1}          := r0;     {bl, g1}          := g0;        {bl, b1}          := b0;
  {wh, r0}          := r1;     {wh, g0}          := g1;        {wh, b0}          := b1;
  {wh, r1}          := r2;     {wh, g1}          := g2;        {wh, b1}          := b2;
  {bl, r2}          := r1;     {bl, g2}          := g1;        {bl, b2}          := b1;
  {wh, w0}          := w2;     {bl, w2}          := w0;
  {w0, o1}          := blend @ {o0, o1}; {t1, w0} := blend @ {t0, t1}; {w0, p1} := blend @ {p0, p1};
  {w3, o1}          := blend @ {o1, o2}; {t1, w3} := blend @ {t1, t2}; {w3, p1} := blend @ {p1, p2};
  colors_List := OklabBlend[colors];
  {g0 -> $DarkGreen,  g1 -> $Green,  g2 -> $LightGreen,
   r0 -> $DarkRed,    r1 -> $Red,    r2 -> $LightRed,
   b0 -> $DarkBlue,   b1 -> $Blue,   b2 -> $LightBlue,
   t0 -> $DarkTeal,   t1 -> $Teal,   t2 -> $LightTeal,
   o0 -> $DarkOrange, o1 -> $Orange, o2 -> $LightOrange,
   p0 -> $DarkPink,   p1 -> $Pink,   p2 -> $LightPink,
   w0 -> $DarkGray,   w1 -> $Gray,   w2 -> $LightGray,
   bl -> Black, wh -> White,
   blend -> OklabBlend}
];

(**************************************************************************************************)

PublicFunction[ContinuousColorFunction]

SetUsage @ "
ContinuousColorFunction[{v$1, $$, v$n}, {c$1, $$, c$n}] returns a function that will take a value \
in the range [v$1, v$n] and interpolate a corresponding color based on the matching colors c$1 to c$n.
ContinuousColorFunction[%Interval[{a$, b$}], $$] will interpolate between colors over the range a$ to b$.
ContinuousColorFunction[Automatic, $$] will interpolate between colors over the range 0 to 1.
* ContinuousColorFunction[{v$1 -> c$1, $$, v$n -> c$n}] and ContinuousColorFunction[vlist$ -> clist$] are also supported.
* Colors are blended in the OkLAB colorspace.
* ContinuousColorFunction returns a %ColorFunctionObject[$$].
* The option %Ticks determines how ticks will be drawn, and accepts these options:
| n$ | choose n$ evenly-spaced ticks |
| All | place a tick at every value |
| Automatic | choose ticks automatically |
"

DeclareArgumentCount[ContinuousColorFunction, {1, 2}];

Options[ContinuousColorFunction] = {
  Ticks -> Automatic
};

General::notcolorvec = "Color list contains non-colors.";
General::badcolorvaluevec = "Value and color lists must be lists of the same length.";
checkColArgs[head_, values_, colors_] := (
  If[Length[values] =!= Length[colors], Message[head::badcolorvaluevec]; Return[$Failed, Block]];
  If[!ColorVectorQ[colors], Message[head::notcolorvec]; Return[$Failed, Block]];
)

toColorList[str_String] := RGBColor /@ StringSplit[str];
toColorList[other_] := other;

General::colfuncfirstarg = "First arg should be a rule or list of rules between values and colors."
setupColorRuleDispatch[head_] := (
  head[rules:{__Rule} | rules_Association, opts:OptionsPattern[]] := head[Keys @ rules, Values @ rules, opts];
  head[values_List -> colors_, opts:OptionsPattern[]] := head[values, colors, opts];
  head[_, OptionsPattern[]] := (Message[head::colfuncfirstarg]; $Failed);
);

setupColorRuleDispatch[ContinuousColorFunction]

ContinuousColorFunction::interpsize = "Value and color lists must have length at least 2.";
ContinuousColorFunction::badvalues = "Cannot choose an automatic coloring for non-numeric values."

ContinuousColorFunction[values_List, Automatic, opts:OptionsPattern[]] := Scope[
  Which[
    VectorQ[values, Internal`RealValuedNumericQ],
      ChooseContinuousColorFunction[values, opts],
    True,
      Message[ContinuousColorFunction::badvalues]; $Failed
  ]
];

ContinuousColorFunction[values_, colors_, OptionsPattern[]] := Scope[
  colors = toColorList @ colors;
  SetAutomatic[values, Interval[{0, 1}]];
  values //= Replace[Interval[{a_, b_}] :> Lerp[a, b, Into @ Length @ colors]];
  checkColArgs[ContinuousColorFunction, values, colors];
  If[Length[values] < 2, ReturnFailed["interpsize"]];
  okLabValues = ToOklab[colors];
  values = N @ values;
  interp = Interpolation[Trans[values, okLabValues], InterpolationOrder -> 1];
  UnpackOptions[ticks];
  System`Private`ConstructNoEntry[
    ColorFunctionObject, "Linear", MinMax @ values, values, interp /* OklabToRGB, ticks
  ]
];

(**************************************************************************************************)

PublicFunction[DiscreteColorFunction]

SetUsage @ "
DiscreteColorFunction[{v$1, $$, v$n}, {c$1, $$, c$n}] returns a %ColorFunctionObject that takes a value \
in the set v$i and returns a corresponding color c$i.
DiscreteColorFunction[vals$, Automatic] chooses colors automatically for the values.
"

DeclareArgumentCount[DiscreteColorFunction, {1, 2}];

Options[DiscreteColorFunction] = {};

DiscreteColorFunction::notvalid = "First argument should be either values -> colors or {value -> color, ...}.";
DiscreteColorFunction::toobig = "The list of values is too large (having `` values) to choose an automatic coloring."

setupColorRuleDispatch[DiscreteColorFunction];

PrivateVariable[$BooleanColors]

$BooleanColors = {GrayLevel[0.9], GrayLevel[0.1]};

DiscreteColorFunction[values_List, Automatic] := Scope[
  values = Union @ values; count = Length[values];
  If[MatchQ[values, {_ ? BooleanQ}], values = {False, True}];
  Which[
    values === {False, True},
      colors = $BooleanColors,
    count <= Length[$ColorPalette],
      colors = Take[$ColorPalette, count],
    count <= 2 * Length[$ColorPalette],
      colors = Take[Join[OklabLighter @ $ColorPalette, OklabDarker @ $ColorPalette], count],
    True,
      ReturnFailed["toobig", count]
  ];
  DiscreteColorFunction[values, colors]
]

DiscreteColorFunction[values_, colors_] := Scope[
  colors = toColorList @ colors;
  checkColArgs[DiscreteColorFunction, values, colors];
  order = Ordering[values];
  values = Part[values, order]; colors = Part[colors, order];
  System`Private`ConstructNoEntry[
    ColorFunctionObject, "Discrete", AssociationThread[values, colors]
  ]
];

(**************************************************************************************************)

PublicFunction[ColorFunctionCompose]

ColorFunctionCompose[cfunc_ColorFunctionObject ? System`Private`NoEntryQ, func_] :=
  cfuncCompose[cfunc, func];

(* TODO: update minMax and values by inverting composedFunc where possible *)
cfuncCompose[ColorFunctionObject[type_, minMax_, values_, func_, ticks_], composedFunc_] :=
  System`Private`ConstructNoEntry[
    ColorFunctionObject, type, {-Infinity, Infinity}, values, composedFunc /* ClipOperator[minMax] /* func, ticks
  ];

(**************************************************************************************************)

PublicFunction[ColorFunctionObjectQ]

ColorFunctionObjectQ[_ColorFunctionObject ? System`Private`NoEntryQ] := True;
ColorFunctionObjectQ[_] := False;

(**************************************************************************************************)

PublicObject[ColorFunctionObject]

SetUsage @ "
ColorFunctionObject[$$] represents a function that takes values and returns colors.
"

ColorFunctionObject[_, minMax_, _, func_, _][value_] := RGBColor @ func @ Clip[value, minMax];
ColorFunctionObject[_, minMax_, _, func_, _][value_List] := Map[RGBColor, Map[func, Clip[value, minMax]]];

ColorFunctionObject["Discrete", assoc_][value_] := Lookup[assoc, Key @ value, Gray];
ColorFunctionObject["Discrete", assoc_][value_List] := Lookup[assoc, Key @ value, Lookup[assoc, value, Gray]];

ColorFunctionObject /: Normal[cf_ColorFunctionObject ? System`Private`NoEntryQ] := getNormalCF[cf];

getNormalCF[ColorFunctionObject[_, minMax_, _, func_, _]] := ClipOperator[minMax] /* func /* RGBColor;
getNormalCF[ColorFunctionObject["Discrete", assoc_]] := assoc;

(**************************************************************************************************)

declareFormatting[
  cf_ColorFunctionObject ? System`Private`HoldNoEntryQ :> formatColorFunction[cf]
];

makeGradientRaster[values_, func_, size_, transposed_] := Scope[
  {min, max} = MinMax[values]; range = max - min; dx = range / size;
  spaced = N @ Range[min, max, dx]; offsets = (values - min) / dx;
  row = func /@ spaced; array = {row};
  arrayRange = {{min - dx, 0}, {max, 1}};
  If[transposed, array //= Transpose; arrayRange = Reverse /@ arrayRange];
  Raster[array, arrayRange]
];

formatColorFunction[ColorFunctionObject["Linear", {min_, max_}, values_, func_, ticks_]] := Scope[
  raster = makeGradientRaster[values, func, 200, False];
  graphics = Graphics[raster,
    ImageSize -> {200, 8},
    PlotRangePadding -> 0, PlotRange -> {All, {0, 1}},
    ImagePadding -> 0, BaselinePosition -> Scaled[0.05], AspectRatio -> Full
  ];
  Row[{graphics, "  ", "(", min, " to ", max, ")"}, BaseStyle -> {FontFamily -> "Avenir"}]
]

formatColorFunction[ColorFunctionObject["Discrete", assoc_Association]] :=
  Apply[AngleBracket,
    KeyValueMap[{val, color} |-> (val -> simpleColorSquare[color]), assoc]
  ];

formatColorFunction[ColorFunctionObject["Discrete", Identity]] :=
  "ColorFunctionObject"["Discrete", Identity];

declareFormatting[
  LegendForm[cf_ColorFunctionObject ? System`Private`HoldNoEntryQ] :>
    colorFunctionLegend[cf]
];

$colorLegendHeight = 100; $colorLegendWidth = 5;
colorFunctionLegend[ColorFunctionObject["Linear", _, values_, func_, ticks_]] :=
  ContinuousColorLegend[values, func, ticks];

(**************************************************************************************************)

PublicFunction[ContinuousColorLegend]

ContinuousColorLegend[values_, func_, ticks_] := Scope[
  raster = makeGradientRaster[values, func, $colorLegendHeight - 2, True];
  {min, max} = MinMax[values];
  includeSign = min < 0 && max > 0;
  If[ticks === All, ticks = values];
  If[IntegerQ[ticks] || ticks === Automatic, ticks = chooseTicks[ticks, min, max]];
  paddingAbove = 2;
  paddingBelow = 2;
  paddingRight = 12;
  If[ticks === None,
    tickPrimitives = {};
    tickPaddingWidth = tickPaddingHeight = 0;
    paddingAbove = paddingBelow = paddingRight = 0;
    multiplier = None;
  ,
    {niceTicksForm, multiplier} = niceTickListForm @ ticks;
    tickPrimitives = MapThread[
      {value, tickForm} |-> {
        {GrayLevel[0.7], Line[{{1.2, value}, {2, value}}]},
        Text[tickForm, {2.8, value}, {-1, 0}]
      },
      {ticks, niceTicksForm}
    ];
    paddingAbove += If[ContainsQ[Last @ niceTicksForm, _Subscript], 15, 8];
    paddingBelow += 3;
    maxTickWidth = Max[estimateTickWidth /@ niceTicksForm];
    If[multiplier =!= None,
      AppendTo[tickPrimitives, Text[multiplier, {1.3, max}, {-1, -2.4}]];
      paddingAbove += 14;
      maxTickWidth = Max[maxTickWidth, estimateTickWidth @ multiplier];
    ];
    paddingRight += 5 * maxTickWidth;
  ];
  paddingLeft = 0;
  imageWidth = $colorLegendWidth + paddingRight + paddingLeft;
  imageHeight = $colorLegendHeight + paddingAbove + paddingBelow;
  graphics = Graphics[
    {raster, GraphicsGroup @ tickPrimitives},
    ImageSize -> {imageWidth, imageHeight},
    PlotRangePadding -> 0, PlotRange -> {{0, 1}, {min, max}},
    PlotRangeClipping -> False,
    ImagePadding -> {{paddingLeft, paddingRight}, {paddingBelow, paddingAbove}},
    BaselinePosition -> Scaled[0.05], AspectRatio -> Full,
    BaseStyle -> {ScriptSizeMultipliers -> 0.2, ScriptMinSize -> 7}
  ];
  graphics
  (* If[multiplier === None, graphics, Labeled[graphics, multiplier, Left, LabelStyle -> "Graphics"]] *)
];

estimateTickWidth = Case[
  Row[list_, ___]       := Total[% /@ list] + .5;
  Superscript["10", b_] := 3;
  Style[s_, ___]        := %[s];
  s_String              := StringLength[s];
  _                     := 1;
];

chooseTicks[2, min_, max_] :=
  {min, max};

chooseTicks[3, min_, max_] :=
  {min, Avg[min, max], max};

chooseTicks[n_, min_, max_] := Scope[
  dx = max - min;
  If[n === Automatic,
    n = 4; scaling = .4; n1 = 2; n2 = 9,
    scaling = 2; n1 = Max[n - 2, 2]; n2 = n + 3
  ];
  ranges = Table[
    Range[min, max, dx / (i - 1)],
    {i, n1, n2}
  ];
  MinimumBy[ranges, {tickListComplexity[n, scaling][#], -Length[#]}&]
];

tickListComplexity[target_, scaling_][ticks_] := Scope[
  tickComplexities = tickComplexity /@ First[niceTickListForm[ticks]];
  targetMismatchPenalty = scaling / (1.0 + Abs[Length[ticks] - target]);
  GeometricMean[tickComplexities] - targetMismatchPenalty
];


tickComplexity = Case[
  Row[{str_String, ___}] := decimalComplexity[str];
  str_String := decimalComplexity[str];
  _ := 0
];

(*
tickComplexity[str_] := Scope[
  decimalComp = decimalComplexity @ str;
  parsedTick = ToExpression[tickString];
  penalize for destroying or creating 'nice' ticks
  errorPenalty = If[(niceTickQ[tick] || niceTickQ[parsedTick]) && (tick != parsedTick), 2, 0];
  decimalComp + errorPenalty
];
 *)
decimalComplexity[str_] :=
  If[StringFreeQ[str, "."],
    0.5 * StringLength[str],
    Dot[{.5, 1}, decimalChunkComplexity /@ StringSplit[str, ".", 2]]
  ];

$niceChunks = "5" | "2" | "4" | "6" | "8";
$okayChunks = "25" | "75";

decimalChunkComplexity[str_] := StringLength[str] - Switch[str,
  $niceChunks, 0.25,
  $okayChunks, 0.5,
  _, 0
];

(**************************************************************************************************)

multiplierForm[base_][0|0.] := "0";
multiplierForm[base_][num_] := Scope[
  extraBase = Log10Length[num];
  baseString = niceDecimalString[num / Power[10, extraBase]];
  power = Style[Superscript["10", TextString[base + extraBase]], Gray];
  If[baseString == "1",
    power,
    Row[{baseString, Style["\[ThinSpace]\[Times]", Gray], power}]
  ]
];

Log10Length[n_] := Floor @ Log10 @ n;
niceTickListForm[list_List] := Scope[
  base = Min @ Log10Length @ DeleteCases[Abs @ list, 0|0.];
  If[base < 2, base = 0];
  $addPlusSign = Min[list] < 0 && Max[list] > 0;
  If[base === 0, Return[{niceDecimalString /@ list, None}]];
  list = list / Power[10, base];
  {multiplierForm[base] /@ list, None}
];

niceTickListForm[list_List] /; Length[list] > 3 := Scope[
  base = Min @ Log10Length @ DeleteCases[Abs @ list, 0|0.];
  If[base < 2, base = 0];
  $addPlusSign = Min[list] < 0 && Max[list] > 0;
  list = list / Power[10, base];
  multiplier = If[base === 0, None,
    Row[{"\[Times]", Superscript["10", base]}, "\[ThinSpace]", BaseStyle -> Gray]
  ];
  {niceDecimalString /@ list, multiplier}
];

$addPlusSign = False;
niceDecimalString[n_] := Which[
  Abs[n] < 1000 && Round[n] == n, TextString @ Round[n],
  Abs[n] < 1000, trimPoint @ TextString @ NumberForm[n, 3],
  True, trimPoint @ TextString @ NumberForm[n, 3]
] // If[$addPlusSign && Positive[n], addPlus, Identity];

trimPoint[str_] := StringTrim[str, "." ~~ EndOfString];
addPlus[str_] := "+" <> str;

(**************************************************************************************************)

colorFunctionLegend[ColorFunctionObject["Discrete", Identity]] := "";

colorFunctionLegend[ColorFunctionObject["Discrete", assoc_Association]] :=
  DiscreteColorLegend[assoc];

PublicFunction[DiscreteColorLegend]

DiscreteColorLegend[assoc_] :=
  Grid[
    KeyValueMap[{val, color} |-> {"", simpleColorSquare @ color, val}, assoc],
    Spacings -> {{0., {0.6}, 5}, {{0.5}}},
    BaseStyle -> $LabelStyle
  ];

simpleColorSquare[color_] := Graphics[
  {color, EdgeForm[Darker[color, .08]], Rectangle[]},
  ImageSize -> 9, BaselinePosition -> Scaled[0.05]
];

(**************************************************************************************************)

PublicFunction[ChooseContinuousColorFunction]

DeclareArgumentCount[ChooseContinuousColorFunction, 1];

Options[ChooseContinuousColorFunction] = {
  Ticks -> Automatic
};

ChooseContinuousColorFunction[ab:{$NumberP, $NumberP}, OptionsPattern[]] := Scope[
  UnpackOptions[ticks];
  {values, colors, newTicks} = pickBiGradient @@ Sort[ab];
  SetAutomatic[ticks, newTicks];
  ContinuousColorFunction[values, colors, Ticks -> ticks]
];

ChooseContinuousColorFunction[list_List, opts:OptionsPattern[]] := Scope[
  If[!VectorQ[list, Internal`RealValuedNumericQ] || Length[list] < 2, ReturnFailed[]];
  ChooseContinuousColorFunction[MinMax @ list, opts]
];

$negativePoints = {-1., -0.9, -0.8, -0.6, -0.3, 0.};
$negativeColors := $negativeColors =
  toColorList @ "#31437e #165e9d #3a7dbf #7aacce #ceefef #ffffff";

$positivePoints = {0., 0.3, 0.6, 0.8, 0.9, 1.};
$positiveColors := $positiveColors =
  toColorList @ "#ffffff #efef7b #ff7b4a #d63822 #b50700 #722a40";

$negativePositivePoints = Join[$negativePoints, Rest @ $positivePoints];
$negativePositiveColors := $negativePositiveColors = Join[$negativeColors, Rest @ $positiveColors];

pickBiGradient[min_ ? Negative, max_ ? Positive] := Scope[
  max = Max[Abs[min], Abs[max]];
  max = pickNice[max, max - min, Ceiling];
  {$negativePositivePoints * max, $negativePositiveColors, 3}
];

pickBiGradient[0|0., max_] :=
  {$positivePoints * pickNice[max, max, Ceiling], $positiveColors, 2};

pickBiGradient[min_, 0|0.] :=
  {$negativePoints * -pickNice[-min, -min, Ceiling], $negativeColors, 2};

$rainbowColors = {$Red, $Orange, $Green, $Blue, $Pink};
$rainbowLength = Length @ $rainbowColors;

pickBiGradient[min_ ? Negative, max_ ? Negative] :=
  MapAt[Minus, pickBiGradient[-min, -max], 1];

pickBiGradient[min_ ? Positive, max_ ? Positive] /; min <= max / 10. :=
  pickBiGradient[0, max];

pickBiGradient[min_ ? Positive, max_ ? Positive] /; min <= max / 5. :=
  {Interpolated[pickNice[min, min, Floor], pickNice[max, max, Ceiling], Length @ $positiveColors], $positiveColors, Automatic};

pickBiGradient[min_ ? Positive, max_ ? Positive] := Scope[
  dx = max - min;
  min = pickNice[min, dx, Floor];
  max = pickNice[max, dx, Ceiling];
  range = Range[min, max, (max - min) / ($rainbowLength - 1)];
  {range, $rainbowColors, Automatic}
];

powerNext[val_ ? Negative, func_] := -powerNext[val, func];
powerNext[val_, func_] := Power[10, func @ Log10 @ val];
powerNext[0|0., _] := {0, 0};

roundNext[val_, func_, Full] := powerNext[val, func];
roundNext[val_, func_, 0] := val;
roundNext[val_, func_, frac_] := func[val, frac * powerNext[val, Ceiling]];

pickNice[val_, dx_, func_] := Scope[
  candidates = roundNext[val, func, #]& /@ {Full, 1., .5, .25, .2, .1, .05, .02, .01, 0};
  tol = dx/8.;
  SelectFirst[candidates, Abs[# - val] <= tol&]
];

(**************************************************************************************************)

PublicFunction[ApplyColoring]

SetUsage @ "
ApplyColoring[list$] determines an automatic coloring for items of list, returning a tuple \
{clist$, groups$, cfunc$}.
ApplyColoring[list$, palette$] uses a palette specification to choose colors.
The clist$, groups$, cfunc$ are as following:
* clist$ is a list of color$i for each list$i.
* groups$ is an association from pairs of colors and values to positions of list$ at which they occur
* cfunc$ is a color function that can be applied to new values.
* See %ToColorPalette for allowed settings of palette$ (the default used is Automatic).
* cfunc$ will be either a %ContinuousColorFunction or a %DiscreteColorFunction.
* If list$ consists of colors, these will be used verbatim.
"

ApplyColoring::badpalette = "The palette `` was not a valid form."

$discreteColors = RGBColor /@ {"#da3b26", "#eebb40", "#4ba526", "#4aa59d", "#4184c6", "#ca4a86", "#6b6b6b", "#929292", "#c5c5c5"};

discreteColorPalette = Case[
  4 := Part[$discreteColors, {1,2,3,5}];
  5 := Part[$discreteColors, {1,2,3,5,6}];
  n_ /; n <= 9 := Take[$discreteColors, n];
  _ := $discreteColors;
];

coloringColorPalette = Case[
  Automatic := Part[ToColorPalette[Automatic], {1, 2, 3, 5, 4, 6}];
  other_ := ToColorPalette[other];
];

ApplyColoring[data_List, palette_:Automatic] := Scope[
  If[ColorVectorQ[data], Return @ literalColorFunction[data]];
  $ColorPalette = coloringColorPalette[palette];
  If[FailureQ[$ColorPalette],
    Message[ApplyColoring::badpalette, palette];
    Return[{$Failed, $Failed, $Failed}];
  ];
  posIndex = KeySort @ PositionIndex @ data;
  containsInd = KeyExistsQ[posIndex, Indeterminate];
  containsNull = KeyExistsQ[posIndex, Null];
  If[containsInd, indPos = posIndex[Indeterminate]; KeyDropFrom[posIndex, Indeterminate]];
  If[containsNull, nullPos = posIndex[Null]; KeyDropFrom[posIndex, Null]];
  uniqueValues = Keys @ posIndex;
  count = Length @ uniqueValues;
  colorFunction = Which[
    ColorVectorQ[uniqueValues],
      Identity,
    uniqueValues === {0, 1} || uniqueValues === {1, 0},
      DiscreteColorFunction[{0, 1}, {White, Black}],
    Length[uniqueValues] == 1,
      DiscreteColorFunction[uniqueValues, {Gray}],
    (PermutedRangeQ[uniqueValues] || PermutedRangeQ[uniqueValues + 1]) && count <= 12,
      If[palette === Automatic, $ColorPalette = discreteColorPalette @ count];
      DiscreteColorFunction[uniqueValues, Automatic],
    RealVectorQ[nUniqueValues = N[uniqueValues]],
      ContinuousColorFunction[nUniqueValues, Automatic],
    ComplexVectorQ[nUniqueValues],
      nUniqueValues //= Re;
      ColorFunctionCompose[ContinuousColorFunction[nUniqueValues, Automatic], Re],
    MatrixQ[nUniqueValues, Internal`RealValuedNumericQ],
      norms = Norm /@ nUniqueValues;
      ColorFunctionCompose[ContinuousColorFunction[norms, Automatic], Norm],
    True,
      DiscreteColorFunction[uniqueValues, Automatic]
  ];
  If[FailureQ[colorFunction], Return @ {$Failed, $Failed, $Failed}];
  normalFunction = Normal @ colorFunction;
  colors = Map[normalFunction, uniqueValues];
  colorsValues = Trans[colors, uniqueValues];
  If[containsInd, AppendTo[colorsValues, {White, Indeterminate}]; AppendTo[posIndex, Indeterminate -> indPos]];
  If[containsNull, AppendTo[colorsValues, {Transparent, Null}]; AppendTo[posIndex, Null -> nullPos]];
  colorGroups = Merge[RuleThread[colorsValues, Values @ posIndex], Catenate];
  colorList = ConstantArray[White, Length @ data];
  (* invert the PositionIndex-like association *)
  KeyValueScan[Set[Part[colorList, #2], Part[#1, 1]]&, colorGroups];
  {colorList, colorGroups, colorFunction}
];

literalColorFunction[colors_] := Scope[
  colorIndex = PositionIndex @ colors;
  colorFunction = System`Private`ConstructNoEntry[
    ColorFunctionObject, "Discrete", Identity
  ];
  {colors, KeyMap[{#, #}&, colorIndex], colorFunction}
];

(**************************************************************************************************)

(* TODO: make this into FlatColor[color], which has a graphics format value to turn it into
the underlying directive! *)

PublicFunction[Color3D]

Color3D[Opacity[o_, color_]] := Color3D @ SetColorOpacity[color, o];
Color3D[Opacity[o_]] := Directive[GrayLevel[0, o], Specularity @ 0];
Color3D[c_] := Directive[Glow @ c, GrayLevel[0, ColorOpacity[c]], Specularity @ 0];

(**************************************************************************************************)

PublicFunction[ComplexHue]

ComplexHue[c_] := OkHue[Arg[c]/Tau, Min[Sqrt[Abs[c]]/1.2,1], .9];

(**************************************************************************************************)

PublicFunction[OkHue]

(* the balance here is between having slightly tweaked saturation and lightness for the primary colors when
the saturation is high, towards a uniform gray/black when saturation and lightness deviate from 1. we use
blendTowardsUniform to do that *)

(* these correspond to choices from $ColorPalette *)
$rainbowLCH = {
  {0/7, {0.609824, 0.198256, 0.086415}},
  {1/7, {0.692448, 0.150783, 0.175395}},
  {2.2/7, {0.651561, 0.181149, 0.384725}},
  {3.4/7, {0.666954, 0.158087, 0.547703}},
  {4.5/7, {0.590379, 0.152313, 0.695439}},
  {5.5/7, {0.629937, 0.155579, 0.813240}},
  {6.5/7, {0.591410, 0.171150, 0.985718}},
  {7/7, {0.609824, 0.198256, 1.086415}}
};

$rainbowLCHFn = Interpolation[$rainbowLCH, InterpolationOrder -> 1];

blendTowardUniform[l_, s_][a_, b_] := Lerp[a, b, Clip[0.7 * ((1 - l)^2/2 + (1 - s)^2), {0, 1}]];

OkHue[h_, s_:1, l_:1] := FromOkLCH[
  {blendTowardUniform[l, s][#1, 0.63] * l+(1-s)/2.75,
   blendTowardUniform[l, s][1.2 * #2 * s, 0.2 * s],
   #3}& @@ $rainbowLCHFn[Mod[h, 1]]
];