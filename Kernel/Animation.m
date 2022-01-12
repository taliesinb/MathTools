(**************************************************************************************************)

PackageExport["DiscreteAnimationObject"]

declareBoxFormatting[
  g:DiscreteAnimationObject[_, _Integer] :> makeDiscreteAnimationObjectBoxes[g]
];

makeDiscreteAnimationObjectBoxes[DiscreteAnimationObject[g_, n_]] := Scope[
  ToBoxes @ Framed[
    imgs = FastRasterizeList @ makeDiscreteGraphics[g, n];
    dims = ImageDimensions /@ imgs;
    If[Max[dims] > 64, imgs = ImageResize[#, {64}]& /@ imgs];
    AnimatedImage[
      imgs,
      AnimationRepetitions -> Infinity,
      FrameRate -> 2
    ],
    FrameStyle -> $Teal
  ]
];

g_DiscreteAnimationObject["VideoToClipboard", args___] :=
  CopyFileToClipboard @ VideoFilePath @ g["Video", args]

CopyFileToClipboard[g_DiscreteAnimationObject] :=
  g["VideoToClipboard"];

g_DiscreteAnimationObject["Video", frameTime_:1/2] := Scope[
  numFrames = Last @ g; fps = 1.0 / frameTime;
  If[fps == Round[fps], fps //= Round];
  path = File @ CacheVideoFilePath["ao", Hash[g], numFrames, fps];
  If[FileExistsQ[path], Video @ path,
    VideoRasterizeList[g["Frames", numFrames], path, fps]
  ]
];

g_DiscreteAnimationObject["Manipulate"] :=
  Construct[Animate, First @ g, {\[FormalT], 1, Last @ g, 1}]

g_DiscreteAnimationObject["AnimatedImage"] :=
  AnimatedImage[g["Frames"], FrameRate -> 2];

g_DiscreteAnimationObject["Frames"] :=
  FastRasterizeList @ g["Graphics"];

DiscreteAnimationObject[g_, n_]["Graphics"] :=
  makeDiscreteGraphics[g, n];

makeDiscreteGraphics[g_, n_] :=  Scope[
  f = Construct[Function, \[FormalT], g];
  Map[f, Range @ n]
];

(**************************************************************************************************)

PackageExport["AnimationObject"]

declareBoxFormatting[
  g:AnimationObject[_] :> makeAnimationObjectBoxes[g]
];

makeAnimationObjectBoxes[AnimationObject[g_]] := Scope[
  ToBoxes @ Framed[
    imgs = FastRasterizeList @ makeGraphics[g, 16];
    dims = ImageDimensions /@ imgs;
    If[Max[dims] > 128, imgs = ImageResize[#, {128}]& /@ imgs];
    AnimatedImage[
      imgs,
      AnimationRepetitions -> Infinity,
      DefaultDuration -> 1
    ],
    FrameStyle -> $Purple
  ]
];

g_AnimationObject["VideoToClipboard", args___] :=
  CopyFileToClipboard @ VideoFilePath @ g["Video", args]

CopyFileToClipboard[g_AnimationObject] :=
  g["VideoToClipboard"];

g_AnimationObject["Video", n_:1, fps_Integer:30] := Scope[
  numFrames = Max[n * fps];
  path = File @ CacheVideoFilePath["ao", Hash[g], numFrames, fps];
  If[FileExistsQ[path], Video @ path,
    VideoRasterizeList[g["Frames", numFrames], path, fps]
  ]
];

g_AnimationObject["Manipulate"] :=
  Construct[Animate, First @ g, {\[FormalT], 0, 1}]

g_AnimationObject["AnimatedImage", n_Integer] :=
  AnimatedImage[g["Frames", n * 30], FrameRate -> 30];

g_AnimationObject["Frames", n_Integer] :=
  FastRasterizeList @ g["Graphics", n];

AnimationObject[g_]["Graphics", n_] := makeGraphics[g, n];

makeGraphics[g_, n_] :=  Scope[
  f = Construct[Function, \[FormalT], g];
  Map[f, Interpolated[0., 1., n]]
];

(**************************************************************************************************)

PackageExport["FindAnimationLerp"]
PackageExport["EasingFunction"]

Options[FindAnimationLerp] = {
  ImageSize -> Automatic,
  PlotRange -> Automatic,
  EasingFunction -> "Linear"
}

FindAnimationLerp[a_, b_, OptionsPattern[]] := Scope[
  UnpackOptions[imageSize, plotRange, easingFunction];
  $lerpVariable = toLerpVariable @ easingFunction;
  Block[
    {$headStack = {}},
    res = findLerp[toLerpable @ a, toLerpable @ b];
    If[ContainsQ[res, \[FormalQ]],
      res = res /. \[FormalQ] -> (\[FormalT]^2 * (2 - \[FormalT]^2))
    ];
    AnimationObject @ res
  ]
];

toLerpVariable = Case[
  "Linear"    := \[FormalT];
  "Quadratic" := \[FormalQ];
];

toLerpable = Case[
  g_Graph := ExtendedGraphPlot @ g;
  other_ := other;
];

findLerp[g1:(Graphics|Graphics3D)[a_, oa___], g2:(Graphics|Graphics3D)[b_, ob___]] := Scope[
  g = {g1, g2};
  {is1, is2} = LookupImageSize[#, AspectRatio -> 1]& /@ {g1, g2};
  {pr1, pr2} = GraphicsPlotRange /@ {g1, g2};
  Head[g1][
    findLerp[a, b],
    ImageSize -> customOptLerp[ImageSize, imageSize, is1, is2],
    PlotRange -> customOptLerp[PlotRange, plotRange, pr1, pr2],
    Sequence @@ DeleteOptions[unifyOpts[{oa}, {ob}], {ImageSize, PlotRange}]
  ]
];

customOptLerp[ImageSize, Min, {aw_, ah_}, {bw_, bh_}] := {Min[aw, bw], Min[ah, bh]};
customOptLerp[ImageSize, Max, {aw_, ah_}, {bw_, bh_}] := {Max[aw, bw], Max[ah, bh]};
customOptLerp[PlotRange, Max, a_, b_] := MapThread[List /* MinMax, {a, b}];

customOptLerp[_, type_String, a_, b_] := Block[{$lerpVariable = toLerpVariable @ type}, findLerp[a, b]];
customOptLerp[_, Automatic, a_, b_] := findLerp[a, b];
customOptLerp[_, other_, _, _] := other;

unifyOpts[o1_, o2_] := MergeAssocations[lerpOpt, Association /@ {o1, o2}];
lerpOpt[k_, {a_}] := k -> a;
lerpOpt[k_, {a_, b_}] := k -> findLerp[a, b];

findLerp[Annotation[a_, _, _String], Annotation[b_, _, _String]] :=
  findLerp[a, b];

findLerp[Tooltip[a_, ___], Tooltip[b_, ___]] :=
  findLerp[a, b];

findLerp[a_, a_] := a;

findLerp[a_Symbol, b_Symbol] :=
  binLerp[a, b];

FindAnimationLerp::badassoc = "At stack ``. Cannot unify associations `` and ``. Choosing first."

findLerp[a_Association, b_Association] :=
  If[Keys[a] === Keys[b],
    Message[FindAnimationLerp::badassoc, $headStack, a, b]; a,
    MapThread[findLerp, {a, b}]
  ];

findLerp[a_ ? System`Private`EntryQ, b_ ? System`Private`EntryQ] /; And[!AtomQ[a], !AtomQ[b], SameHeadQ[a, b], SameLengthQ[a, b]] := Block[
  {$headStack = Append[$headStack, Head @ a]},
  Apply[Head @ a, MapThread[findLerp, {List @@ a, List @@ b}]]
];

$fancyCurveP = BezierCurve | BSplineCurve;
findLerp[Line[a_], (h:$fancyCurveP)[b_]] := findLerp[h[a], h[b]];
findLerp[(h:$fancyCurveP)[a_], Line[b_]] := findLerp[h[a], h[b]]

findLerp[a_List ? CoordinateArrayQ, b_List ? CoordinateArrayQ] /; SameLengthQ[a, b] :=
  MapThread[findLerp, {a, b}];

findLerp[a_List ? CoordinateMatrixQ, b_List ? CoordinateMatrixQ] /; SameLengthQ[a, b] :=
  NumericLerp[a, b, $lerpVariable];

findLerp[a_List ? CoordinateVectorQ, b_List ? CoordinateVectorQ] /; SameLengthQ[a, b] :=
  NumericLerp[a, b, $lerpVariable];

findLerp[a_ ? NumericQ, b_ ? NumericQ] :=
  NumericLerp[a, b, $lerpVariable];

findLerp[a_, b_] :=
  binLerp[a, b, \[FormalT]];

findLerp[a_List, b_List] /; !SameLengthQ[a, b] := Scope[
  n = Max[Length @ a, Length @ b];
  NumericLerp[resample[a, n], resample[b, n], $lerpVariable]
]

$interpolatableGraphicsPrimitiveP = Line | Polygon | Arrow;
$inInterpolateGraphicsPrimitiveQ := MemberQ[$headStack, $interpolatableGraphicsPrimitiveP]

resample[a_, n_] /; Length[a] === n := a;
resample[a_, n_] := Interpolation[a, InterpolationOrder -> 1] @ Interpolated[1, Length[a], n];

binLerp[a_, b_, t_] := If[t < 0.5, a, b];
linearLerp[a_, b_, t_] := a * (1 - t) + (b * t);

(**************************************************************************************************)

Graphics3DSpinAnimation[g_Graphics3D, angleDelta_] := Scope[
  viewPoint = LookupOption[g, ViewPoint, Automatic];
  angles0 = ToSpherical @ viewPoint;
  angles1 = applyAngleDelta[angles0, angleDelta];
  viewPoint01 = ComposedNumericLerp[FromSpherical, angles0, angles1, \[FormalT]];
  gSpin = ReplaceOptions[g, ViewPoint -> viewPoint01];
  AnimationObject[gSpin]
];

applyAngleDelta[{r_, a_, b_}, {dr_, da_, db_}] := {r + dr, a + Pi * da, b + Tau * db};
applyAngleDelta[{r_, a_, b_}, {da_, db_}] := {r, a + Pi * da, b + Tau * db};
applyAngleDelta[{r_, a_, b_}, db_] := {r, a, b + Tau * db};

(**************************************************************************************************)

PackageExport["VertexLayoutAnimationLerp"]

Options[VertexLayoutAnimationLerp] = {
  "InitialRotation" -> 0,
  EasingFunction -> "Quadratic",
  PlotRange -> Automatic,
  ImageSize -> Automatic
}

VertexLayoutAnimationLerp[graph_Graph, layout_, OptionsPattern[]] := Scope[
  UnpackOptions[initialRotation, easingFunction, plotRange, imageSize];
  g1 = ExtendedGraphPlot @ ExtendedGraph[graph, If[initialRotation === 0, Sequence @@ {}, CoordinateTransformFunction -> {"Rotate", initialRotation}]];
  g2 = ExtendedGraphPlot @ ExtendedGraph[graph, VertexLayout -> layout];
  FindAnimationLerp[g1, g2, EasingFunction -> easingFunction, ImageSize -> imageSize, PlotRange -> plotRange]
]
(**************************************************************************************************)

PackageExport["NumericLerp"]

NumericLerp[a_, b_, t_ ? NumericQ] := linearLerp[a, b, t];

(**************************************************************************************************)

PackageExport["ComposedNumericLerp"]

ComposedNumericLerp[f_, a_, b_, t_ ? NumericQ] := f @ linearLerp[a, b, t];
