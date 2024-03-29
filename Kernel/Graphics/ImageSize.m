PublicFunction[DimensionToAbsoluteThickness, AbsoluteThicknessToDimension]

DimensionToAbsoluteThickness[n_, scale_] := n * scale;
AbsoluteThicknessToDimension[t_, scale_] := t / scale;

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

Options[LookupImageSize] = {AspectRatio -> Auto};

(* use shortcut for Graphs, otherwise we'd have to call ExtendedGraphPlot to calculate this properly.
this isn't 100% accurate, however, since it can't know how much label padding etc. will be involved *)
LookupImageSize[g_Graph, ___] :=
  Lookup[ComputeExtendedGraphImageSizeData[g], "ImageSize"];

LookupImageSize[obj_, OptionsPattern[]] := Scope[
  {imageSize, aspectRatio} = LookupOption[obj, {ImageSize, AspectRatio}];
  SetAuto[aspectRatio, OptionValue @ AspectRatio];
  imageSize = resolveRawImageSize @ imageSize;
  If[NumberQ[aspectRatio] && MatchQ[P2[imageSize], Auto],
    Part[imageSize, 2] = P1[imageSize] * aspectRatio];
  imageSize
];

resolveRawImageSize = Case[
  sz:{_ ? NQ, Auto | (_ ? NQ)} := sz;
  w_ ? NQ                           := {w, Auto};
  s_Symbol                          := {Lookup[$ImageWidthTable, s], Auto};
  _                                 := {720, Auto};
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
  SetAuto[height, width * aspectRatio];
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
  Medium -> 360, Auto -> 360,
  MediumLarge -> 468,
  Large -> 576,
  Huge -> 720
|>;

(**************************************************************************************************)

PrivateFunction[toStandardImageSize]

toStandardImageSize[sym:(MediumSmall|MediumLarge|Huge)] := Lookup[$ImageWidthTable, sym];
toStandardImageSize[other_] := other;


PrivateFunction[toNumericSizeScale]

$sizeScaleAssoc = KDrop[$ImageWidthTable / $ImageWidthTable[Medium], Auto];

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