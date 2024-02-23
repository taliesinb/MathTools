PublicFunction[HexColorString]

HexColorString[c:$ColorPattern] := HexColorString[c] = toHexColor @ ToRGB @ c;
_HexColorString := BadArguments[];

toHexColor[c_List] := toHexColor[c] = SJoin @ IntStr[Floor[(255 * c) + 1 / 2], 16, 2];
_toHexColor := BadArguments[];

(**************************************************************************************************)

PublicFunction[FromHexColorString]

FromHexColorString[str_Str] /; SLen[str] == 6 := fromHexColor[str];
_FromHexColorString := BadArguments[];

SetListable[fromHexValue];
fromHexColor[s_] := fromHexColor[s] = RGBColor @ N @ fromHexValue @ STake[s, {{1, 2}, {3, 4}, {5, 6}}];
fromHexValue[s_] := FromDigits[s, 16] / 255.;

(**************************************************************************************************)

PublicFunction[HTMLColorString, FromHTMLColorString]

HTMLColorString[None] := "none";
HTMLColorString[Black] := "black";
HTMLColorString[White] := "white";
HTMLColorString[c:$ColorPattern] := HTMLColorString[c] = SJoin["#", HexColorString @ c];
_HTMLColorString := BadArguments[];

FromHTMLColorString["none"] := None;
FromHTMLColorString["black"] := Black;
FromHTMLColorString["white"] := White;
FromHTMLColorString[str_Str] /; SLen[str] == 7 := FromHexColorString @ SDrop[str, 1];
_FromHTMLColorString := BadArguments[];

(**************************************************************************************************)

PublicFunction[SetColorOpacity]

$setColorOpacityRules = Dispatch[{
  RGBColor[r_, g_, b_] :> RGBColor[r, g, b, $opacity],
  RGBColor[{r_, g_, b_}] :> RGBColor[r, g, b, $opacity],
  GrayLevel[g_] :> GrayLevel[g, $opacity],
  Hue[h_] :> Hue[h, 1, 1, $opacity],
  c:(_Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor) /; Len[c] === 3 :> App[c, $opacity]
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
  c:(_Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor) /; Len[c] === 3 :> App[c, 1]
}];

SetOpaque[expr_] := expr /. $opaqueRules;

(**************************************************************************************************)

PublicFunction[SetColorLightness]

SetColorLightness[expr_, lightness_] :=
  expr /. c:$ColorPattern :> RuleEval @ OklabSetLightness[c, lightness];

SetColorLightness[lightness_][expr_] :=
  SetColorLightness[expr, lightness];

(**************************************************************************************************)

PublicFunction[RemoveColorOpacity]

$removeColorOpacityRules = Dispatch[{
  Opacity[_, c_] :> c,
  RGBColor[r_, g_, b_, _] :> RGBColor[r, g, b],
  RGBColor[{r_, g_, b_, _}] :> RGBColor[r, g, b],
  GrayLevel[g_, _] :> GrayLevel[g],
  c:(_Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor) /; Len[c] === 4 :> Take[c, 3]
}];

RemoveColorOpacity[expr_] :=
  expr /. $removeColorOpacityRules;

(**************************************************************************************************)

PublicFunction[ContainsOpacityColorsQ]

$opacityColorsPattern = Alt[
  RGBColor[{_, _, _, _}],
  GrayLevel[_, _],
  _Opacity,
  c:(_RGBColor | _Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor) /; Len[c] === 4
];

ContainsOpacityColorsQ[expr_] := !FreeQ[expr, $opacityColorsPattern];

(**************************************************************************************************)

PublicFunction[ExtractFirstOpacity]

$opacityRule = Alt[
  RGBColor[{_, _, _, o_}], GrayLevel[_, o_], Opacity[o_], Opacity[o_, _],
  RGBColor[_, _, _, o_], Hue[_, _, _, o_], XYZColor[_, _, _, o_], LABColor[_, _, _, o_],
  LCHColor[_, _, _, o_], LUVColor[_, _, _, o_]
] :> o;

ExtractFirstOpacity[expr_] := FirstCase[expr /. $opacityNormalizationRules, $opacityRule, None, {0, Inf}];

(**************************************************************************************************)

PublicFunction[ColorOpacity]

ColorOpacity[color_] := FirstCase[Rep[color, $opacityNormalizationRules], $opacityRule, 1, {0}];

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
  If[MatrixQ[ok], Col1 @ ok, F[ok]]
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

ToRGB[e_] := RepAll[e, $toRGBRules];

$toRGBRules = Dispatch[{
  RGBColor[r_, g_, b_, ___] :> {r, g, b},
  RGBColor[{r_, g_, b_, ___}] :> {r, g, b},
  c:(_GrayLevel | _XYZColor | _CMYKColor | _Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor) :>
    RuleEval[List @@ ColorConvert[c, "RGB"]]
}];

(**************************************************************************************************)

PrivateFunction[FromRGB]

FromRGB[rgb_List] := RGBColor @@ rgb;
FromRGB[rgb_List ? MatrixQ] := RGBColor @@@ rgb;

(**************************************************************************************************)

PublicFunction[NormalizeColorLightness]

NormalizeColorLightness[colors_List, fraction_:1] :=
  normalizeLightness[colors, fraction];

(**************************************************************************************************)

PublicVariable[$ColorPalette, $NormalColorPalette]

PublicGraphicsDirective[$Blue, $Red, $Yellow, $Green, $Pink, $Teal, $Orange, $Purple, $Gray, $White, $Black]

{$Blue, $Red, $Green, $Pink, $Teal, $Yellow, $Orange, $Purple, $Gray, $White, $Black} =
  Map[RGBColor, SSplit @ "#3e81c3 #e1432d #4ea82a #c74883 #47a5a7 #f6e259 #dc841a #8b7ebe #929292 #f3f3f3 #404040"]

$ColorPalette = $NormalColorPalette = {$Red, $Blue, $Green, $Orange, $Purple, $Teal, $Gray, $Pink, $Yellow, $White, $Black};

PublicGraphicsDirective[$DarkColorPalette, $DarkBlue, $DarkRed, $DarkYellow, $DarkGreen, $DarkPink, $DarkTeal, $DarkGray, $DarkOrange, $DarkPurple, $DarkWhite, $DarkBlack]

{$DarkRed, $DarkBlue, $DarkGreen, $DarkOrange, $DarkPurple, $DarkTeal, $DarkGray, $DarkPink, $DarkYellow, $DarkWhite, $DarkBlack} =
  $DarkColorPalette = OklabDarker @ $ColorPalette;

PublicGraphicsDirective[$LightColorPalette, $LightBlue, $LightRed, $LightYellow, $LightGreen, $LightPink, $LightTeal, $LightGray, $LightOrange, $LightPurple, $LightGray, $LightWhite, $LightBlack]

{$LightRed, $LightBlue, $LightGreen, $LightOrange, $LightPurple, $LightTeal, $LightGray, $LightPink, $LightYellow, $LightWhite, $LightBlack} =
  $LightColorPalette = OklabLighter @ $ColorPalette;

(**************************************************************************************************)

PublicHead[Paletted]

PrivateVariable[$paletteUsageString]

$paletteUsageString = STrim @ "
* palette$ can be an one of the following:
| Automatic | the default color palette |
| 'Light', 'Medium', 'Dark' | a lighter or darker version of the default palette |
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

$ecnp = Alt @@ $ExtendedColorNames;

ToColorPalette::invalid = "`` is not a valid color palette.";

ToColorPalette = Case[
  Auto | "Medium"          := $NormalColorPalette;
  "Light"                       := $LightColorPalette;
  "Dark"                        := $DarkColorPalette;
  spec_ -> "Light"              := OklabLighter @ % @ spec;
  spec_ -> "Dark"               := OklabDarker @ % @ spec;
  set:$ecnp                     := getNamedColorSet[set, "Medium"];
  set:$ecnp -> variant_Str      := getNamedColorSet[set, variant];
  list_List ? ColorVectorQ      := list /. $colorNormalizationRules;
  Offset[spec_, n_Int]          := RotateLeft[% @ spec, n];
  Opacity[o_, spec_]            := SetColorOpacity[% @ spec, N @ o];
  spec_                         := (Message[ToColorPalette::invalid, spec]; $Failed)
];

getNamedColorSet[set_, variant_] := getNamedColorSet[set, variant] = Scope[
  colors = $ExtendedColorsGrouped[set, variant];
  If[MissingQ[colors], ReturnFailed[]];
  Values @ colors
];

ToColorPalette[spec_, n_Int] := Scope[
  colors = ToColorPalette[spec];
  If[FailureQ[colors], ReturnFailed[]];
  If[n > 40, Return @ Table[RandomColor[], n]];
  If[Len[colors] < n, colors //= expandColorPalette];
  Take[colors, n]
];

expandColorPalette[colors_] := Join[colors, OklabBlend /@ (UnorderedPairs @ colors)];

(**************************************************************************************************)

PublicFunction[ToRainbowColor]

ToRainbowColor = Case[
  -2                         := $Black;
  -1                         := $White;
  0                          := $LightGray;
  i_Int                      := Part[$NormalColorPalette, i];
  col_ ? ColorQ              := Lookup[$colorAliases, col, col];
  s:(Auto | None)       := s;
  other_                     := $Gray;
  Null                       := Transparent;
  Seq[i_, _]                 := % @ i;
  Seq[s:Auto | None, _] := s;
  Seq[_, 0]                  := $LightGray;
  Seq[name_Str, n_]          := Part[ToColorPalette @ name, n];
];

(**************************************************************************************************)

PrivateVariable[$colorAliases]

$colorAliases = UAssoc[
  Red    -> $Red,    LightRed    -> $LightRed,
  Blue   -> $Blue,   LightBlue   -> $LightBlue,
  Green  -> $Green,  LightGreen  -> $LightGreen,
  Orange -> $Orange, LightOrange -> $LightOrange,
  Purple -> $Purple, LightPurple -> $LightPurple,
  Pink   -> $Pink,   LightPink   -> $LightPink,
  Cyan   -> $Teal,   LightCyan   -> $LightTeal,
  Gray   -> $Gray,   LightGray   -> $LightGray
];

(**************************************************************************************************)

PublicFunction[ToRainbowInteger]

ToRainbowInteger = Case[

  s_Str := Lookup[$romanToInteger, ToLowerCase @ ToSpelledGreek @ ToNonDecoratedRoman @ s, None];

  head_Symbol /; $taggedFormHeadQ[head] := % @ F @ s;

];

$romanToInteger = <|
  "i" -> -1, "1" -> -1,
  "1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5,
  "a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4, "e" -> 5,
  "f" -> 1, "g" -> 2, "h" -> 3,
  "m" -> 1, "n" -> 2, "p" -> 3,
  "x" -> 1, "y" -> 2, "z" -> 3,
  "l" -> 1, "r" -> 2,
  "mu" -> 6, "eps" -> 7, "alpha" -> 8, "rho" -> 1, "lambda" -> 2, "eta" -> 3
|>;

(**************************************************************************************************)

PublicFunction[GrayColorQ]

GrayColorQ[_GrayLevel] := True;
GrayColorQ[r_RGBColor] := SameQ @@ Round[Flatten @ Apply[List, r], .05];
GrayColorQ[_] := False;

(**************************************************************************************************)

PublicFunction[OklabBlend, OklabBlendOperator, BlendOperator]

SetUsage @ "
OklabBlend[colors$] blends a list of ordinary colors, but in OkLAB colorspace.
OklabBlend[colors$, i$] blends part $i between the colors, where $i runs from 0 to 1.
OklabBlend[colors$, {i$1, i$2, $$}] returns a vector of blended colors.
OklabBlend[colors$, Into[n$]] returns a vector of n$ equally spaced blended colors.
"

DeclareArgumentCount[OklabBlend, 1];

OklabBlend[colors_List] := FromOklab @ Mean @ ToOklab[colors];

OklabBlend[colors_List, i_] := oklabInterpolation[colors, i] // FromOklab;

OklabBlend[colors_List, i_Into] := oklabInterpolation[colors, List @ Lerp[0, 1, i]] // FromOklab;

OklabBlend[colors_List, i_List] := oklabInterpolation[colors, {i}] // FromOklab;

oklabInterpolation[colors_, args___] :=
  Interpolation[
    Trans[Lerp[0, 1, Into @ Len @ colors], ToOklab @ colors],
    args,
    InterpolationOrder -> 1
  ];

OklabBlendOperator[colors_List] :=
  Interpolation[
    Trans[Lerp[0, 1, Into @ Len @ colors], ToOklab @ colors],
    InterpolationOrder -> 1
  ] /* FromOklab;

BlendOperator[colors_List] :=
  Interpolation[
    Trans[Lerp[0, 1, Into @ Len @ colors], ToRGB @ colors],
    InterpolationOrder -> 1
  ] /* FromRGB;

(**************************************************************************************************)

PublicFunction[HumanBlend]

HumanBlend[colors_List] := iHumanBlend @ Sort @ VectorReplace[colors, i_Int :> ToRainbowColor[i]];

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
  Ticks -> Auto
};

General::notcolorvec = "Color list contains non-colors.";
General::badcolorvaluevec = "Value and color lists must be lists of the same length.";
checkColArgs[head_, values_, colors_] := (
  If[Len[values] =!= Len[colors], Message[head::badcolorvaluevec]; Return[$Failed, Block]];
  If[!ColorVectorQ[colors], Message[head::notcolorvec]; Return[$Failed, Block]];
)

toColorList[str_Str] := RGBColor /@ SSplit[str];
toColorList[other_] := other;

General::colfuncfirstarg = "First arg should be a rule or list of rules between values and colors."
setupColorRuleDispatch[head_] := (
  head[rules:{__Rule} | rules_Assoc, opts:OptionsPattern[]] := head[Keys @ rules, Values @ rules, opts];
  head[values_List -> colors_, opts:OptionsPattern[]] := head[values, colors, opts];
  head[_, OptionsPattern[]] := (Message[head::colfuncfirstarg]; $Failed);
);

setupColorRuleDispatch[ContinuousColorFunction]

ContinuousColorFunction::interpsize = "Value and color lists must have length at least 2.";
ContinuousColorFunction::badvalues = "Cannot choose an automatic coloring for non-numeric values."

ContinuousColorFunction[values_List, Auto, opts:OptionsPattern[]] := Scope[
  Which[
    VecQ[values, RealValuedNumericQ],
      ChooseContinuousColorFunction[values, opts],
    True,
      Message[ContinuousColorFunction::badvalues]; $Failed
  ]
];

ContinuousColorFunction[values_, colors_, OptionsPattern[]] := Scope[
  colors = toColorList @ colors;
  SetAuto[values, Interval[{0, 1}]];
  values //= Rep[Interval[{a_, b_}] :> Lerp[a, b, Into @ Len @ colors]];
  checkColArgs[ContinuousColorFunction, values, colors];
  If[Len[values] < 2, ReturnFailed["interpsize"]];
  okLabValues = ToOklab[colors];
  values = N @ values;
  interp = Interpolation[Trans[values, okLabValues], InterpolationOrder -> 1];
  UnpackOptions[ticks];
  ConstructNoEntry[
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

DiscreteColorFunction[values_List, Auto] := Scope[
  values = Union @ values; count = Len[values];
  If[MatchQ[values, {_ ? BooleanQ}], values = {False, True}];
  Which[
    values === {False, True},
      colors = $BooleanColors,
    count <= Len[$ColorPalette],
      colors = Take[$ColorPalette, count],
    count <= 2 * Len[$ColorPalette],
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
  ConstructNoEntry[
    ColorFunctionObject, "Discrete", AssocThread[values, colors]
  ]
];

(**************************************************************************************************)

PublicFunction[ColorFunctionCompose]

ColorFunctionCompose[cfunc_ColorFunctionObject ? NoEntryQ, func_] :=
  cfuncCompose[cfunc, func];

(* TODO: update minMax and values by inverting composedFunc where possible *)
cfuncCompose[ColorFunctionObject[type_, minMax_, values_, func_, ticks_], composedFunc_] :=
  ConstructNoEntry[
    ColorFunctionObject, type, {-Inf, Inf}, values, composedFunc /* ClipOperator[minMax] /* func, ticks
  ];

(**************************************************************************************************)

PublicFunction[ColorFunctionObjectQ]

ColorFunctionObjectQ[_ColorFunctionObject ? NoEntryQ] := True;
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

ColorFunctionObject /: Normal[cf_ColorFunctionObject ? NoEntryQ] := getNormalCF[cf];

getNormalCF[ColorFunctionObject[_, minMax_, _, func_, _]] := ClipOperator[minMax] /* func /* RGBColor;
getNormalCF[ColorFunctionObject["Discrete", assoc_]] := assoc;

(**************************************************************************************************)

declareFormatting[
  cf_ColorFunctionObject ? HoldNoEntryQ :> formatColorFunction[cf]
];

makeGradientRaster[values_, func_, size_, transposed_] := Scope[
  {min, max} = MinMax[values]; range = max - min; dx = range / size;
  spaced = N @ Range[min, max, dx]; offsets = (values - min) / dx;
  row = func /@ spaced; array = {row};
  arrayRange = {{min - dx, 0}, {max, 1}};
  If[transposed, array //= Transpose; arrayRange = Rev /@ arrayRange];
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

formatColorFunction[ColorFunctionObject["Discrete", assoc_Assoc]] :=
  Apply[AngleBracket,
    KVMap[{val, color} |-> (val -> simpleColorSquare[color]), assoc]
  ];

formatColorFunction[ColorFunctionObject["Discrete", Id]] :=
  "ColorFunctionObject"["Discrete", Id];

declareFormatting[
  LegendForm[cf_ColorFunctionObject ? HoldNoEntryQ] :>
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
  If[IntQ[ticks] || ticks === Auto, ticks = chooseTicks[ticks, min, max]];
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
    paddingAbove += If[ContainsQ[L @ niceTicksForm, _Subscript], 15, 8];
    paddingBelow += 3;
    maxTickWidth = Max[estimateTickWidth /@ niceTicksForm];
    If[multiplier =!= None,
      AppTo[tickPrimitives, Text[multiplier, {1.3, max}, {-1, -2.4}]];
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
  s_Str                 := SLen[s];
  _                     := 1;
];

chooseTicks[2, min_, max_] :=
  {min, max};

chooseTicks[3, min_, max_] :=
  {min, Avg[min, max], max};

chooseTicks[n_, min_, max_] := Scope[
  dx = max - min;
  If[n === Auto,
    n = 4; scaling = .4; n1 = 2; n2 = 9,
    scaling = 2; n1 = Max[n - 2, 2]; n2 = n + 3
  ];
  ranges = Table[
    Range[min, max, dx / (i - 1)],
    {i, n1, n2}
  ];
  MinimumBy[ranges, {tickListComplexity[n, scaling][#], -Len[#]}&]
];

tickListComplexity[target_, scaling_][ticks_] := Scope[
  tickComplexities = tickComplexity /@ F[niceTickListForm[ticks]];
  targetMismatchPenalty = scaling / (1.0 + Abs[Len[ticks] - target]);
  GeometricMean[tickComplexities] - targetMismatchPenalty
];


tickComplexity = Case[
  Row[{str_Str, ___}] := decimalComplexity[str];
  str_Str := decimalComplexity[str];
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
  If[SFreeQ[str, "."],
    0.5 * SLen[str],
    Dot[{.5, 1}, decimalChunkComplexity /@ SSplit[str, ".", 2]]
  ];

$niceChunks = "5" | "2" | "4" | "6" | "8";
$okayChunks = "25" | "75";

decimalChunkComplexity[str_] := SLen[str] - Switch[str,
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
  base = Min @ Log10Length @ Decases[Abs @ list, 0|0.];
  If[base < 2, base = 0];
  $addPlusSign = Min[list] < 0 && Max[list] > 0;
  If[base === 0, Return[{niceDecimalString /@ list, None}]];
  list = list / Power[10, base];
  {multiplierForm[base] /@ list, None}
];

niceTickListForm[list_List] /; Len[list] > 3 := Scope[
  base = Min @ Log10Length @ Decases[Abs @ list, 0|0.];
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
] // If[$addPlusSign && Positive[n], addPlus, Id];

trimPoint[str_] := STrim[str, "." ~~ EndOfString];
addPlus[str_] := "+" <> str;

(**************************************************************************************************)

colorFunctionLegend[ColorFunctionObject["Discrete", Id]] := "";

colorFunctionLegend[ColorFunctionObject["Discrete", assoc_Assoc]] :=
  DiscreteColorLegend[assoc];

PublicFunction[DiscreteColorLegend]

DiscreteColorLegend[assoc_] :=
  Grid[
    KVMap[{val, color} |-> {"", simpleColorSquare @ color, val}, assoc],
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
  Ticks -> Auto
};

ChooseContinuousColorFunction[ab:{$NumberP, $NumberP}, OptionsPattern[]] := Scope[
  UnpackOptions[ticks];
  {values, colors, newTicks} = pickBiGradient @@ Sort[ab];
  SetAuto[ticks, newTicks];
  ContinuousColorFunction[values, colors, Ticks -> ticks]
];

ChooseContinuousColorFunction[list_List, opts:OptionsPattern[]] := Scope[
  If[!VecQ[list, RealValuedNumericQ] || Len[list] < 2, ReturnFailed[]];
  ChooseContinuousColorFunction[MinMax @ list, opts]
];

$negativePoints = {-1., -0.9, -0.8, -0.6, -0.3, 0.};
$negativeColors := $negativeColors =
  toColorList @ "#31437e #165e9d #3a7dbf #7aacce #ceefef #ffffff";

$positivePoints = {0., 0.3, 0.6, 0.8, 0.9, 1.};
$positiveColors := $positiveColors =
  toColorList @ "#ffffff #efef7b #ff7b4a #d63822 #b50700 #722a40";

$negativePositivePoints = Join[$negativePoints, Rest @ $positivePoints];
SetCached[$negativePositiveColors, Join[$negativeColors, Rest @ $positiveColors]];

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
$rainbowLength = Len @ $rainbowColors;

pickBiGradient[min_ ? Negative, max_ ? Negative] :=
  MapAt[Minus, pickBiGradient[-min, -max], 1];

pickBiGradient[min_ ? Positive, max_ ? Positive] /; min <= max / 10. :=
  pickBiGradient[0, max];

pickBiGradient[min_ ? Positive, max_ ? Positive] /; min <= max / 5. :=
  {Interpolated[pickNice[min, min, Floor], pickNice[max, max, Ceiling], Len @ $positiveColors], $positiveColors, Auto};

pickBiGradient[min_ ? Positive, max_ ? Positive] := Scope[
  dx = max - min;
  min = pickNice[min, dx, Floor];
  max = pickNice[max, dx, Ceiling];
  range = Range[min, max, (max - min) / ($rainbowLength - 1)];
  {range, $rainbowColors, Auto}
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
  Auto := Part[ToColorPalette[Auto], {1, 2, 3, 5, 4, 6}];
  other_ := ToColorPalette[other];
];

$white = GrayLevel[0.98];
$black = GrayLevel[0.1];

ApplyColoring[data_List, palette_:Auto] := Scope[
  If[ColorVectorQ[data], Return @ literalColorFunction[data]];
  $ColorPalette = coloringColorPalette[palette];
  If[FailureQ[$ColorPalette],
    Message[ApplyColoring::badpalette, palette];
    Return[{$Failed, $Failed, $Failed}];
  ];
  posIndex = KSort @ PositionIndex @ data;
  containsInd = KeyQ[posIndex, Indeterminate];
  containsNull = KeyQ[posIndex, Null];
  If[containsInd, indPos = posIndex[Indeterminate]; KDropFrom[posIndex, Indeterminate]];
  If[containsNull, nullPos = posIndex[Null]; KDropFrom[posIndex, Null]];
  uniqueValues = Keys @ posIndex;
  count = Len @ uniqueValues;
  sortedUniqueValues = Sort @ uniqueValues;
  colorFunction = Which[
    ColorVectorQ[uniqueValues],
      Id,
    sortedUniqueValues === {0, 1},
      DiscreteColorFunction[{0, 1}, {$white, $black}],
    sortedUniqueValues === {-1, 1},
      DiscreteColorFunction[{-1, 1}, {$Red, $Blue}],
    sortedUniqueValues === {-1, 0, 1},
      DiscreteColorFunction[{-1, 0, 1}, {$Red, $white, $Blue}],
    Len[uniqueValues] == 1,
      DiscreteColorFunction[uniqueValues, {Gray}],
    (PermutedRangeQ[uniqueValues] || PermutedRangeQ[uniqueValues + 1]) && count <= 12,
      If[palette === Auto, $ColorPalette = discreteColorPalette @ count];
      DiscreteColorFunction[uniqueValues, Auto],
    RealVectorQ[nUniqueValues = N[uniqueValues]],
      ContinuousColorFunction[nUniqueValues, Auto],
    ComplexVectorQ[nUniqueValues],
      nUniqueValues //= Re;
      ComplexHue,
    MatrixQ[nUniqueValues, RealValuedNumericQ],
      norms = Norm /@ nUniqueValues;
      ColorFunctionCompose[ContinuousColorFunction[norms, Auto], Norm],
    True,
      DiscreteColorFunction[uniqueValues, Auto]
  ];
  If[FailureQ[colorFunction], Return @ {$Failed, $Failed, $Failed}];
  normalFunction = Normal @ colorFunction;
  colors = Map[normalFunction, uniqueValues];
  colorsValues = Trans[colors, uniqueValues];
  If[containsInd, AppTo[colorsValues, {White, Indeterminate}]; AppTo[posIndex, Indeterminate -> indPos]];
  If[containsNull, AppTo[colorsValues, {Transparent, Null}]; AppTo[posIndex, Null -> nullPos]];
  colorGroups = Merge[RuleThread[colorsValues, Values @ posIndex], Catenate];
  colorList = Repeat[White, Len @ data];
  (* invert the PositionIndex-like association *)
  KVScan[Set[Part[colorList, #2], F[#1]]&, colorGroups];
  {colorList, colorGroups, colorFunction}
];

literalColorFunction[colors_] := Scope[
  colorIndex = PositionIndex @ colors;
  colorFunction = ConstructNoEntry[
    ColorFunctionObject, "Discrete", Id
  ];
  {colors, KMap[{#, #}&, colorIndex], colorFunction}
];

(**************************************************************************************************)

PublicGraphicsDirective[Color3D]

DeclareGraphicsPrimitive[Color3D, "Color", color3DBoxes, {3}];

color3DBoxes = Case[
  Color3D[Opacity[o_, color_]] := % @ Color3D @ SetColorOpacity[color, o];
  Color3D[Opacity[o_]] := Directive[GrayLevel[0, o], Specularity @ 0];
  Color3D[c_] := Directive[Glow @ c, GrayLevel[0, ColorOpacity[c]], Specularity @ 0];
];

(**************************************************************************************************)

PublicGraphicsDirective[SolidEdgeForm]

SetUsage @ "
SolidEdgeForm[face$, edge$] colors solid primitives to have face color face$ and edge color $edge.
SolidEdgeForm[face$] is equivalent to SolidEdgeForm[face$, Automatic].
* if face$ is automatic, it is a lighter form of edge$.
* if edge$ is Automatic, it is a darker form of face$.
"
DeclareGraphicsPrimitive[SolidEdgeForm, "Color,Color", solidEdgeFormBoxes, {2, 3}];

solidEdgeFormBoxes[se_] :=
  Apply[Directive[FaceForm @ #1, EdgeForm @ #2]&, solidEdgeColors @ se];

PrivateFunction[solidEdgeColors]

solidEdgeColors = Case[
  SolidEdgeForm[f_] := % @ SolidEdgeForm[f, Auto];
  SolidEdgeForm[f_, e_] := {f, e};
  SolidEdgeForm[Auto, e_] := {OklabLighter[e, .1], e};
  SolidEdgeForm[f_, Auto] :=  {f, OklabDarker[f, .1]};
];

(**************************************************************************************************)

PublicFunction[ComplexHue]

ComplexHue[c_] := OkHue[Arg[c]/Tau, Min[Sqrt[Abs[c]]/1.2,1], .9];

ComplexHue /: Normal[ComplexHue] := ComplexHue;

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