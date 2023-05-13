PublicFunction[SetGraphicsScale]

SetGraphicsScale[g_Graphics, scale_:40, padding_:1] := Scope[
  g = NormalizePlotRange[g];
  plotRange = {{xmin, xmax}, {ymin, ymax}} = GraphicsPlotRange @ g;
  xwidth = xmax - xmin; ywidth = ymax - ymin;
  dims = {xwidth, ywidth};
  padding //= StandardizePadding; SetAll[padding, 1];
  size = (dims * scale) + (2 * Map[Total, padding]); imageWidth = First @ size;
  pointsToScaled = Scaled[# / imageWidth]&;
  g = g /. (FontSize -> p_) :> RuleCondition[FontSize -> pointsToScaled[p]];
  ReplaceOptions[g, {ImageSize -> size, ImagePadding -> padding, PlotRange -> plotRange}]
]

(**************************************************************************************************)

PublicFunction[SetScalableGraphicsFontSize]

SetScalableGraphicsFontSize[g_Graphics, imageSize_:Automatic] := Scope[
  SetAutomatic[imageSize, First @ LookupImageSize @ g];
  imageWidth = First[imageSize, imageSize];
  pointsToScaled = Scaled[# / imageWidth]&;
  g = g /. (FontSize -> p_) :> RuleCondition[FontSize -> pointsToScaled[p]];
  ReplaceOptions[g, ImageSize -> imageSize]
];

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

(* use shortcut for Graphs, otherwise we'd have to call ExtendedGraphPlot to calculate this properly.
this isn't 100% accurate, however, since it can't know how much label padding etc. will be involved *)
LookupImageSize[g_Graph, ___] :=
  Lookup[ComputeExtendedGraphImageSizeData[g], "ImageSize"];

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