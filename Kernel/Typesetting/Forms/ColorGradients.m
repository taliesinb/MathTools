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
  {raster, boundingBox} = Rasterize[expr, {"Image", "BoundingBox"}, Background -> Transparent, ImageResolution -> 144];
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

AlphaDilation[img_, 0] := img;
AlphaDilation[img_, n_] := SetAlphaChannel[Erosion[RemoveAlphaChannel[img,White], n], Dilation[AlphaChannel @ img, n]];