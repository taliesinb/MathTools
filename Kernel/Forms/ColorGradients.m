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
single GraphicsBox and then apply ColorGradientBoxOperator to it. it could even return a dynamic box
that makes it typeset at the correct font size and script level. this is all fairly composable.

how about axis-aligned gradients? how exactly would we interpret complex specs like
ColorGradient[{Top -> c1, Bottom -> c2}] when applied to shafts? one idea is to construct
coordinate system *for* the line itself, in which Left is the start of the line and right
is the end of the line. that's a lot of work for maybe not much payoff. maybe we just interpret
ColorGradient[{c1, c2}] as the same as ColorGradient[Along -> {c1, c2}], where Along has a
special meaning for lines.

so that's probably a whole day project and maybe overly general. what's a simpler stepping tone?

what if we have a simple table mapping one-letter strings (and symbol heads) to their named
icon equivalents. named icon itself could even apply it!

probably i could introduce TextIcon which dispatches to NamedIcon and does this lookup. but it should
have regular, bold, and semibold variants, all as polygons.
it could compute this on demand from TextToPolygon and it would ignore alignment.
its typeset form would produce a DynamicBox that picks up the current font color and
font size. later, ToGraphicsBox would be able to skip this dynamic step because it would track font properties
during construction.

initially, only the BoldXXXArrows would work with a color gradient.

Internally it can do this by calling EvaluateTemplateBoxes etc and trying to deduce the underyling font weight and font family.

TextIcon itself should return a DynamicBox so that it can be nested in ordinary text boxes as is.
*)

PublicHead[ColorGradient]

PublicOption[CompressionFactor]

SetUsage @ "
ColorGradient[{c$1, c$2}] represents a color gradient between c$1 and c$2, defaulting to left-to-right.
ColorGradient[{c$1, c$2}, dir$] represents an oriented color gradient in the given direction.
* dir$ can be a symbolic direction like %Right, %Top, or a direction vector.
* integers will be interpreted as rainbow colors via %ToRainbowColor.
* the option %CompressionFactor can range from 0 (totally linear) to 1 (hard jump).
"

ColorGradient::badGradientSpec = "`` is not a valid color gradient spec."
parseColorGradient = Case[
  {c1_, c2_} -> dir:$SidePattern                  := % @ ColorGradient[{c1, c2}, dir];
  ColorGradient[{c1_, c2_}] | {c1_, c2_}          := {{1,0}, F, Id, OklabBlendOperator[toRC /@ {c1, c2}]};
  ColorGradient[{c1_, c2_}, dir:$Coord2P]         := {Normalize @ dir, DotOperator[dir], Id, OklabBlendOperator[toRC /@ {c1, c2}]};
  ColorGradient[spec_, side:$SidePattern]         := % @ ColorGradient[spec, $SideToCoords @ side];
  ColorGradient[spec__, CompressionFactor -> c_]  := RepPart[% @ ColorGradient[spec], 3 -> CompressUnit[c]];
  other_ := (
    Message[ColorGradient::badGradientSpec, other];
    {{1,0}, Id, Id, Red&}
  );
]

toRC[e_] := If[ColorQ[e], e, ToRainbowColor[e] /. None -> Black];

(**************************************************************************************************)

PublicFunction[ToColorGradient]

ColorGradient::badSpec = "`` is not a valid color gradient specification."
ToColorGradient = Case[
  {c1_, c2_}          := ColorGradient[{c1, c2}];
  {c1_, c2_} -> side_ := ColorGradient[{c1, c2}, side];
  cg_ColorGradient    := cg;
  other_              := (
    Message[ColorGradient::badSpec, other];
    ColorGradient[{Black, Black}];
  )
];

(**************************************************************************************************)

PublicFunction[CompressUnit]

CompressUnit[0|0.] := Id;
CompressUnit[c_][x_] := Clip[(x - 0.5)/(1 - c + $MachineEpsilon) + 0.5, {0, 1}];

(**************************************************************************************************)

PublicGraphicsPrimitive[ColorGradientStyle]

SetUsage @ "
ColorGradientStyle[prims$, cspec$] draws prims$ with the applied gradient cspec$.
ColorGradientStyle[prims$, cspec$, bounds$] calculates the gradient over the coordinate bounds.
* cspec$ should be either a pair of colors or a %ColorGradient[$$] objects.
"

DeclareGraphicsPrimitive[ColorGradientStyle, "Primitives", colorGradiantFormBoxes];

colorGradiantFormBoxes[ColorGradientStyle[prims_, cspec_, bounds_:Auto]] :=
  ColorGradientBoxes[ToGraphicsBoxes @ prims, cspec, bounds];

(**************************************************************************************************)

PublicTypesettingBoxFunction[ColorGradientBoxes]

SetUsage @ "
ColorGradientBoxes[primitives$, bounds$, cspec$] applies a color gradient given by cspec$ to primitives$ over the given bounds.
* cspec$ should be either a pair of colors or a %ColorGradient[$$] objects.
* the gradient is applied by attaching %VertexColors options to lines and %LinearGradientFilling to polygons and disks.
"

ColorGradientBoxes[primitives_, cspec_, Auto] :=
  ColorGradientBoxes[primitives, cspec, PrimitiveBoxesBounds @ primitives];

ColorGradientBoxes[primitives_, cspec_, {{x1_, x2_}, {y1_, y2_}}] := Scope[
  {dir, toNumber, procNumber, toColor} = parseColorGradient @ cspec;
  points = {{x1, y1}, {x1, y2}, {x2, y1}, {x2, y2}};
  nums = toNumber /@ points;
  minMax = MinMax @ nums;
  $ang = ArcTan2 @@ N[dir];
  $vcfunc = toColor @ procNumber @ Rescale[toNumber @ #, minMax]&;
  attachVertexColors @ primitives
];

toVC = Case[
  m_ ? CoordinateMatrixQ    := VertexColors -> Map[$vcfunc, m];
  ms_ ? CoordinateMatricesQ := VertexColors -> MatrixMap[$vcfunc, ms];
];

(* TODO: handle circles! *)
attachVertexColors = Case[
  list_List                           := Map[%, list];
  StyleBox[p_, o___]                  := Style[% @ p, o];
  s:$shapeP                           := fillShape[s];
  PolygonBox[rule:Rule[array_, _]]    := Construct[PolygonBox, rule, toVC @ array];
  LineBox[array_]                     := Construct[LineBox, array, toVC @ array];
,
  {$shapeP -> _PolygonBox | _DiskBox | _RectangleBox}
]

fillShape[shape_] := Scope[
  {{x1, x2}, {y1, y2}} = PrimitiveBoxesBounds @ shape;
  w = x2 - x1; h = y2 - y1; m = Max[w, h];
  comp = {w / m, h / m};
  lgfNums = DotOperator[dir * comp] /@ {{-1, -1}, {-1, 1}, {1, 1}, {1, -1}};
  {lgfMin, lgfMax} = MinMax @ lgfNums;
  lgRange = Lerp[lgfMin, lgfMax, Into[12]]/2 + 0.5;
  points = {{x1, y1}, {x1, y2}, {x2, y1}, {x2, y2}};
  nums = Rescale[toNumber /@ points, minMax];
  {min, max} = MinMax @ nums;
  range = Lerp[min, max, Into[12]];
  range //= Map[procNumber];
  colors = toColor /@ range;
  lgf = LinearGradientFilling[lgRange -> colors, $ang];
  StyleBox[shape, ToGraphicsBoxes @ lgf]
];

rectRad[w_,h_,ang_] := Norm[{Clip[Tan[Pi/2. - ang]] * w, Clip[Tan[ang]] * h}];

(**************************************************************************************************)

PublicFunction[ImageCropVertical]

ImageCropVertical[img_Image, bt_:Auto] := Scope[
  {w, h} = ImageDimensions @ img;
  If[bt === Auto,
    {{l, r}, {b, t}} = BorderDimensions[img],
    {b, t} = bt
  ];
  baseline = LookupOption[img, BaselinePosition, Auto];
  cropped = ImagePad[img, {{0, 0}, -{b, t}}];
  If[!MatchQ[baseline, _Scaled], Return @ cropped];
  baseline //= F;
  baseline = Clip[Rescale[baseline, {b, h - t} / h], {0, 1}];
  Image[cropped, BaselinePosition -> Scaled[baseline], ImageSize -> {w, h - b - t}/2]
];

(**************************************************************************************************)

PublicFunction[ColorGradientRasterize]

PublicOption[DilationFactor]

CacheVariable[$GradientRasterizationCache]

Options[ColorGradientRasterize] = {
  DilationFactor -> 1,
  ContentPadding -> True
}

ColorGradientRasterize[expr_, colors_, OptionsPattern[]] := Scope[
  UnpackOptions[dilationFactor, contentPadding];
  CachedInto[$GradientRasterizationCache, Hash @ {expr, colors, dilationFactor, contentPadding},

  {raster, rasterSize, tmp, tmp} = MakeImageAndMetadata[expr, Transparent];
  {w, h, dh} = rasterSize;
  baselinePos = Scaled[(h - dh-0.5) / h];

  If[MatchQ[colors, None | Auto | Black],
    result = Image[raster, BaselinePosition -> baselinePos, ImageSize -> {w, h}/2];
  ,
    {dir, toNumber, procNumber, toColor} = parseColorGradient @ colors;
    mask = AlphaChannel @ raster;

    {w, h} = ImageDimensions @ mask;
    pixels = PixelValuePositions[mask, 0];
    {{x1, x2}, {y1, y2}} = CoordinateBounds[pixels];
    points = {{x1, y1}, {x1, y2}, {x2, y1}, {x2, y2}};
    nums = toNumber /@ points;
    minMax = MinMax @ nums;

    (* construct the masking gradient *)
    grid = ToPackedReal @ Table[{x, y}, {y, h}, {x, w}];
    dots = Dot[grid, Normalize @ dir];
    dots = Clip[Rescale[dots, minMax], {0, 1}];
    oklab = F[toColor] /@ procNumber[dots];
    rgb = FromOklab /@ oklab;
    gradImage = Image[rgb];

    result = SetAlphaChannel[gradImage, ImageClip[ImageMultiply[Blur[mask, dilationFactor], 4]]];
    result = Image[result, BaselinePosition -> baselinePos, ImageSize -> {w, h}/2, Options @ raster];
  ];

  If[!ImageQ[result], Return @ $Failed];
  If[!contentPadding, result //= ImageCropVertical];
  result
]];

(**************************************************************************************************)

PublicTypesettingForm[GradientSymbol]

SetUsage @ "
GradientSymbol[symbol$, {c$1, c$2}] displays as symbol$ with a color gradient applied.
GradientSymbol[symbol$, {c$1, c$2} -> side$] specifies a gradient direction.
GradientSymbol[symbol$, ColorGradient[$$]] specifies gradient direction and compression factor.
* symbol$ can be a string or a literal symbol like %BoldRightArrowSymbol.

* internally, %GradientSymbol calls %TextIcon, and so the following options are available:
| %CompressionFactor | the compression factor used for %ColorGradient if one is not specified |
| %ContentPadding | whether to leave vertical padding |
| %Method | 'Raster' or 'Vector' |
| %FontSize | Inherited for dynamic, or a fixed number |
| %FormatType | %MathForm or None |
| %FontWeight | as usual |
| %FontFamily | as usual |
| %FontSlant | as usual |
"

(* TODO: protect these from running on their own FormatValues *)
DefineStandardTraditionalForm[{

  (* TODO: turn text into a shape via ToGraphicsBox, then use LinearGradientFilling on it *)
  GradientSymbol[sym_, cspec_, opts___Rule] :> gradientSymbolBoxes[sym, cspec, opts],

  (* TODO: retire these in favor of having GradientSymbol burrow itself, either via
  ColorRules just doing a BurrowModifiers, or more broadly having all style forms burrow themselves *)
  (g:GradientSymbol[_FunctorSymbol, ___])[args___] :>
    NoSpanBox @ ToBoxes @ FunctorAppliedForm[g, args],

  (h_GradientSymbol)[args___] :>
    NoSpanBox @ ToBoxes @ AppliedForm[h, args]
}];

gradientSymbolBoxes[ReversedChainForm[f_], cspec_, opts___] :=
  ToBoxes @ ReversedChainForm[GradientSymbol[f, cspec, opts]];

gradientSymbolBoxes[sym_Symbol ? $symbolFormHeadQ, cspec_, opts___] :=
  gradientSymbolBoxes[$literalSymbolFormTable @ sym, cspec, opts]

(* trim off things like CategorySymbol for vector method, because we use the right font family *)
gradientSymbolBoxes[(_Symbol ? $taggedFormHeadQ)[inner_], args___] /; FreeQ[{args}, "Raster"] :=
  gradientSymbolBoxes[inner, args];

gradientSymbolBoxes[str_Str, cspec_, opts___] :=
  ToBoxes @ TextIcon[str,
    FilterOptions @ opts,
    FontColor -> cspec,
    FontWeight -> Bold
  ];

(* if we are rasterizing, we don't need to trim off style heads, let them do their thing *)
gradientSymbolBoxes[expr_, cspec_, lopts___, Method -> "Raster", ropts___] :=
  ToBoxes @ TextIcon[expr, FontColor -> cspec, lopts, Method -> "Raster", ropts];

(* TODO: have these foundation styles defined in code, and then automatically set up these
rules. perhaps we can even move them downstream into TextIcon, via FormatType *)

gradientSymbolBoxes[SansSerifForm[inner_], args___] :=
  gradientSymbolBoxes[inner, args, FontFamily -> "KaTeX_SansSerif", FontSlant -> Plain];

gradientSymbolBoxes[ModernForm[inner_], args___] :=
  gradientSymbolBoxes[inner, args, FontFamily -> "KaTeX_SansSerif", FontSlant -> Plain, FontWeight -> Bold];

GradientSymbol::firstArg = "First argument in `` should be a string or literal symbol."
gradientSymbolBoxes[args___] :=
  (Message[GradientSymbol::firstArg, GradientSymbol[args]]; "?");

(* have to disable this because otherwise GradientSymbol burrows through tagged forms and they get
rasterized incorrectly *)
(* $styleFormHeadQ[GradientSymbol] = True; *)

