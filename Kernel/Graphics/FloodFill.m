PublicFunction[FloodFill]

CacheVariable[$FloodFillCache]

Options[FloodFill] = {
	"Sensitivity" -> 0.001
}

FloodFill[g_Graphics, rules___] := FloodFill[g -> g, rules];

FloodFill::errimg = "Rasterized graphics contains an error: ``.";
FloodFill[g:(_Graphics -> _Graphics), rules:{__Rule}, OptionsPattern[]] := Scope[
	UnpackOptions[sensitivity];
	CachedInto[$FloodFillCache, Hash[{g, rules, sensitivity}], iFloodFill[g, rules, sensitivity]]
];

iFloodFill[lhs:(Graphics[prims_, opts___] -> g2_Graphics), rules_, sensitivity_] := Scope[
	baseline = Lookup[List @@ Rest[g2], BaselinePosition, Auto];
	len = Len @ rules;
  annos = Style[Annotation[Invisible @ Point[#1], #2, "FillPoint"], Antialiasing->None]& @@@ rules;
  annoGraphics = Graphics[{{AbsolutePointSize[1], annos}, prims}, opts];
  {image, size, bounds, regions} = MakeImageAndMetadata @ annoGraphics;
  image2 = MakeImage[g2, Transparent];
  If[ErrorImageQ[image] || ErrorImageQ[image2], Message[FloodFill::errimg, lhs];
  	Return @ ConstantImage[0.5, {5,5}]];
  fillPoints = Cases[regions, ({color_, "FillPoint"} -> {{x1_, y1_}, {x2_, y2_}}) :> {Round @ {Avg[x1, x2], Avg[y1, y2]}, color}];
  bin = ColorNegate @ Binarize[ColorNegate @ image, sensitivity];
  comps = MorphologicalComponents[bin, 0.001];
  comps = Dilation[comps, 1];
  fillCoords = 2 * Col1[fillPoints];
  fillColors = ToRGB @ Col2[fillPoints];
  compValues = Extract[comps, Rev[fillCoords, 2]];
  compRules = ToPackedReal /@ AssocMap[v |-> (
    i = IndexOf[compValues, v];
    If[MissingQ[i], {0.,0.,0.,0.}, App[Part[fillColors, i], 1.]]
  ), Range[0, Max[comps]]];
  compColors = ToPackedReal @ Map[compRules, comps, {2}];
  compImage = Image[compColors, "Real32", ColorSpace->"RGB"];
  {image, bin, fillPoints, compImage};
  result = ImageCompose[compImage, image2];
  result = Image[result, BaselinePosition -> baseline];
  If[ImageQ[result], result, $Failed]
];