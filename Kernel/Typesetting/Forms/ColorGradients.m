(*
A note about a potential ColorGradient as directive.

LinearGradientFilling only works on solid objects, but that's the only directive we could evaluate to.
An 'axis gradient' that tells lines to gradiate between their start and end points could only
ever be a transformer, rather than a directive.
so we could have a transformer called ColorGradiated[primitives, {p1 -> col, p2 -> color}], with
p1 and p2 being positions that could also be symbolic to indicate the bounding box of the contained
primitives, and {c1, c2} being sugar for Horizontal -> {c1, c2} being sugar for {Left -> c1, Right -> c2}.
could also have a Concentration -> 0 (endpoints) and 1 (hard jump in the middle).
if we implement this as VertexColors we'd need to subdivide lines or polygons so that hard jumps
would have an effect, which is pretty complex!

for abstract color specs like to IconColor, we'd need a head like ColorGradient[{c1, c2}] etc.
similarly for ArrowColor, which uses ShaftStyleBoxOperator, this would end up as a composition
of a StyleBoxOperator with a ColorGradientBoxOperator that does the rewriting, and is also what
ColorGradiated would evaluate to.

we could then drop GradientSymbol completely, and instead implement it as the *typeset* form
of ColorGradiated, which would internally call ToGraphics to turn its interior elements into a
single GraphicsBox and then apply ColorGradientBoxOperator to it. this is all fairly composable.

how about axis-aligned gradients? how exactly would we interpret complex specs like
ColorGradient[{Top -> c1, Bottom -> c2}] when applied to shafts? one idea is to construct
coordinate system *for* the line itself, in which Left is the start of the line and right
is the end of the line. that's a lot of work for maybe not much payoff. maybe we just interpret
ColorGradient[{c1, c2}] as the same as ColorGradient[Along -> {c1, c2}], where Along has a
special meaning for lines.

so that's probably a whole day project. what's a simpler stepping tone?

what if we have a simple table mapping one-letter strings (and symbol heads) to
their named icon equivalents. named icon itself could even apply it!

probably i could introduce TextIcon which dispatches to NamedIcon and does this lookup. but it should
have regular, bold, and semibold variants, all as polygons.
it could store all the data on disk and load on demand. and it would ignore alignment.
its typeset form would produce a DynamicBox that picks up the current font color and
font size. later, ToGraphicsBox would be able to skip this dynamic step because it would track font properties
during construction.

initially, only the BoldXXXArrows would work with a color gradient.

*)

(**************************************************************************************************)

PublicTypesettingForm[ColorGradientForm]

DefineStandardTraditionalForm[{
  ColorGradientForm[expr_, colors:{$ColorPattern..}, opts___Rule] :> colorGradientBoxes[expr, colors, opts],
  cg_ColorGradientForm[args___] :> ToBoxes[AppliedForm[cg, args]]
}]

(**************************************************************************************************)

colorGradientBoxes[expr_, colors_, opts___] := ToBoxes @ ColorGradientRasterize[expr, colors, opts]

(**************************************************************************************************)

PublicFunction[ColorGradientRasterize]

Options[ColorGradientRasterize] = {
  "DilationFactor" -> 0,
  "CompressionFactor" -> 0
}

ColorGradientRasterize[expr_, colors_, OptionsPattern[]] := Scope[
  UnpackOptions[dilationFactor, compressionFactor];
  hash = Hash[{expr, colors, dilationFactor, compressionFactor}];
  result = Lookup[QuiverGeometryCaches`$GradientRasterizationCache, hash];
  If[ImageQ[result], Return @ result];
  {raster, boundingBox, regions} = FastRasterizeWithMetadata[expr, Background -> Transparent];
  {bbw, bbh, dh} = boundingBox;
  baselinePos = Scaled[(bbh - dh-0.5) / bbh];
  mask = AlphaChannel @ raster;
  {w, h} = ImageDimensions @ mask;
  totals = Total[ImageData[mask], {1}];
  p = SelectFirstIndex[totals, # > 1&];
  q = w + 1 - SelectFirstIndex[Reverse @ totals, # > 1&];
  cShift = Clip[compressionFactor * (w-1)/2, {0, (q - p)/2 - 1}];
  p += cShift;
  q -= cShift;
  colorFractions = Clip[((N @ Range[1, w]) - p) / (q - p), {0, 1}];
  colors = OklabBlend[colors, colorFractions];
  grad = ImageResize[Image[{colors}], {w, h}, Resampling -> "Nearest"];
  result = SetAlphaChannel[grad, Clip[4 * Blur[mask, dilationFactor]]];
  result = Image[result, BaselinePosition -> baselinePos, ImageSize -> {w, h}/2, Options @ raster];
  If[ImageQ[result], QuiverGeometryCaches`$GradientRasterizationCache[hash] ^= result];
  result
];

(**************************************************************************************************)

PublicTypesettingForm[GradientSymbol, GradientArrowSymbol]

GradientArrowSymbol[args___] := GradientSymbol[RightArrowSymbol, args];

(* TODO: use Dynamic[CurrentValue[ScriptSize]] to make it work properly inside Superscript etc *)

DefineStandardTraditionalForm[{

  (* this is so that we still recognize CategoryArrowSymbol["\[RightArrow]"] etc *)
  GradientSymbol[(_Symbol ? $taggedFormHeadQ)[sub:("\[RightArrow]" | RightArrowSymbol)], args___] :> ToBoxes @ GradientSymbol[sub, args],

  (* TODO: recognize $symbolFormHeadQ and resolve it in case we have a known icon for it! *)
  (* TODO: turn text into a shape via ToGraphicsBox, then use LinearGradientFilling on it *)
  GradientSymbol[sym_, col1_, col2_, sz_:16] :>
    ToBoxes @ ColorGradientForm[
      Style[sym, FontSize -> sz],
      ToRainbowColor /@ {col1, col2},
      "DilationFactor" -> 1, "CompressionFactor" -> 0.5
    ],

  (* TODO: have a registry here, or more precisely move this RightArrow case straight into ToGraphicsBox *)
  GradientSymbol["\[RightArrow]" | RightArrowSymbol, col1_, col2_, sz_:16] :>
    gradientArrowBoxes[col1, col2, sz],

  (* TODO: retire these in favor of having GradientSymbol burrow itself, either via
  ColorRules just doing a BurrowModifiers, or more broadly having all style forms burrow themselves *)
  (g:GradientSymbol[_FunctorSymbol, ___])[args___] :>
    NoSpanBox @ ToBoxes @ FunctorAppliedForm[g, args],

  (h_GradientSymbol)[args___] :>
    NoSpanBox @ ToBoxes @ AppliedForm[h, args]
}];

(* ok, so one way we can do this that will avoid having to hardcode fontsizes etc is by returning a

DynamicBox @ Construct[GraphicsBox, {}, Background -> GrayLevel[0.2], ImageSize -> CurrentValue[FontSize], TrackedSymbols -> {}, DestroyAfterEvaluation -> True]

weird we can't just put the dynamicbox aroudn the CurrentValue
*)

(* have to disable this because otherwise GradientSymbol burrows through tagged forms and they get
rasterized incorrectly *)
(* $styleFormHeadQ[GradientSymbol] = True; *)

(**************************************************************************************************)


$rightArrowPath := $rightArrowPath = Uncompress @ "
1:eJxTTMoPSmViYGDQB2IQLf1oz12BWLYDhiolp+TXrNr/2WIBi5gI2wETYxA4vH9l45H9b7tYDzgn
TPrWmnV2f3HCD1nlSywHVm2Z/uUY16X9DGDAcgCs3PjyfrsdZwNXLL1oD+P3ifyvUlE/vf+OlcNDM7
OX+7n9XsisT7u4/0tpy5T4j6/3/+ZZwVqvcHl/rJ9Q5DnN9/vteSZ93959aX/dgpY8TruP+9uvSs6b
XnJ+v6Xdmu7zBz7tP/IxMbaG7dj+jVaHP09693n/XXavF9MebN4v0Pzk79tFX/anmWy7a3Jslr09c9
ZB+RNf9htpvFvbaHXAvknY6vjqyC8w99hPehTf/yTl8/5H6hM6mZZ9sl8uv++jfP75/YHMnfW3RT/b
u0D9+6zQgVNH4wvUP4f3mx+p3K2464u9KTS8oPbYZ0HtNYPK/5swz/qk21I43x6i3v7+ko25yxZ+sa
99fcTPpWujvdZb1b0bm77YR/Hx7E922WVvN5EjfLnDF/u8Y7n/JCQP2qPZb384deXxHJ4v9qo1/jf3
Wh+xh4aD/WFIuNiHRc3jd636bM9yPjfZYO1JezT/2D/4sC3q3stP9tvck79uMj1n3yXl8ef2uhP2HD
6X9gWs/WwfDXUHzF0MUHD2DAh8sY+EyO+HyXdC9O+H6Z9xnFF1w4IL+9PTQOCT/S2B52/bXlzar7Qw
+F/C1Q/2t6H8vXsjmO1C39nPgqqHpLfX9kL17RccfsLTjz1a+rJHS3/2sPQJ8x8s/RpBwwuWvq2h4Q
8AA9xygg==";

PrivateFunction[gradientArrowBoxes]

gradientArrowBoxes[col1_, col2_, sz_] := TagBox[Construct[
  GraphicsBox,
  {
    ToGraphicsBoxes @ LinearGradientFilling[{0.4 -> ToRainbowColor[col1], 0.7 -> ToRainbowColor[col2]}],
    Construct[PolygonBox, $rightArrowPath]
  },
  PlotRange -> {{-2.8, 1.3}, {-1.3, 1.3}},
  ImagePadding -> {{0, 1}, {0, 0}},
  BaselinePosition -> Scaled[0.05],
  ImageSize -> {Automatic, sz/2+1}
], "ReverseChain"];


AlphaDilation[img_, 0] := img;
AlphaDilation[img_, n_] := SetAlphaChannel[Erosion[RemoveAlphaChannel[img,White], n], Dilation[AlphaChannel @ img, n]];