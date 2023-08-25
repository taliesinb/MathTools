PublicForm[ColorGradientForm]

DefineStandardTraditionalForm[{
  ColorGradientForm[expr_, colors:{$ColorPattern..}, opts___] :> colorGradientBoxes[expr, colors, opts]
}]

(**************************************************************************************************)

colorGradientBoxes[expr_, colors_, opts___] := ToBoxes @ ColorGradientRasterize[expr, colors, opts]

(**************************************************************************************************)

PublicFunction[ColorGradientRasterize]

$gradientRasterizeCache = Data`UnorderedAssociation[];

ColorGradientRasterize[expr_, colors_, dilation_:0] := Scope[
  hash = Hash[{expr, colors, dilation}];
  result = Lookup[$gradientRasterizeCache, hash];
  If[ImageQ[result], Return @ result];
  raster = Rasterize[expr, Background -> Transparent];
  mask = AlphaChannel @ raster;
  {w, h} = ImageDimensions @ mask;
  totals = Total[ImageData[mask], {1}];
  p = SelectFirstIndex[totals, # > 1&];
  q = w + 1 - SelectFirstIndex[Reverse @ totals, # > 1&];
  colorFractions = Clip[((N @ Range[1, w]) - p) / (q - p), {0, 1}];
  colors = OklabBlend[colors, colorFractions];
  grad = ImageResize[Image[{colors}], {w, h}, Resampling -> "Nearest"];
  result = SetAlphaChannel[grad, Clip[4 * Blur[mask, dilation]]];
  result = Image[result, Options @ raster];
  If[ImageQ[result], $gradientRasterizeCache[hash] ^= result];
  result
];

(*
ColorGradientRasterize[expr_, colors_, dilation_:0] := Scope[
  hash = Hash[{expr, colors, dilation}];
  result = Lookup[$gradientRasterizeCache, hash];
  If[ImageQ[result], Return @ result];
  mask = ColorConvert[Rasterize[expr, Background -> Transparent], "Grayscale"];
  mask = AlphaDilation[mask, dilation];
  {w, h} = ImageDimensions @ mask;
  totals = Total[Part[ImageData[mask], All, All, 2], {1}];
  p = SelectFirstIndex[totals, # > 1&];
  q = w + 1 - SelectFirstIndex[Reverse @ totals, # > 1&];
  colorFractions = Clip[((N @ Range[1, w]) - p) / (q - p), {0, 1}];
  colors = OklabBlend[colors, colorFractions];
  grad = ImageResize[Image[{colors}], {w, h}, Resampling -> "Nearest"];
  result = ImageMultiply[ColorConvert[ColorNegate[mask], "RGB"], grad];
  If[ImageQ[result], $gradientRasterizeCache[hash] ^= result];
  result
];
*)
AlphaDilation[img_, 0] := img;
AlphaDilation[img_, n_] := SetAlphaChannel[Erosion[RemoveAlphaChannel[img,White], n], Dilation[AlphaChannel @ img, n]];