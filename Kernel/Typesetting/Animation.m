PublicOption[MaxFrames]
SetUsage @ "
MaxFrames is an option for animation-related functions that puts an upper bound on the number of frames to render.
"

PublicOption[AnimationLooping]
PublicSymbol[Bidirectional]
SetUsage @ "
AnimationLooping is an option for animation-related functions that has the following settings:
* True (default)
* Bidirectional
* False
"

PublicOption[AnimationType]
SetUsage @ "
AnimationType is an option for animation-related functions that specifies whether an animation is discrete or continuous:
* With the setting AnimationType -> 'Discrete', the animation is discrete.
* With the setting AnimationType -> 'Continuous', the animation is continuous.
"

PublicOption[AnimationDuration]
SetUsage @ "AnimationDuration is an option for animation-related functions that specifies the target length of an animation."

PublicOption[AnimationOrigin]
SetUsage @ "AnimationOrigin is an option for animation-related functions that specifies the origin of an animation."

PublicOption[FrameDuration]
SetUsage @ "FrameDuration is an option for animation-related functions that specifies the length in seconds of a single frame."

PublicOption[FrameCount]
SetUsage @ "FrameCount is an option for animation-related functions that specifies the number of frames in an animation."

(**************************************************************************************************)

PublicFunction[DiscreteAnimation]
PublicFunction[ContinuousAnimation]

Options[DiscreteAnimation] = {
  FrameDuration -> Automatic,
  FrameRate -> Automatic,
  AnimationDuration -> Automatic,
  AnimationLooping -> True,
  AnimationOrigin -> None
};

Options[ContinuousAnimation] = {
  FrameDuration -> Automatic,
  FrameRate -> Automatic,
  AnimationDuration -> 4,
  AnimationLooping -> True,
  AnimationOrigin -> None
}

SetUsage @ "
DiscreteAnimation[expr$, n$] creates a %AnimationObject[$$] in which \[FormalT] runs from 1 to n$.
DiscreteAnimation[expr$] chooses n$ automatically.
* AnimationPart and AnimationRange can be used within the expression.
* The following options are supported:
| %FrameDuration | Automatic | time to devote to each frame |
| %FrameRate | Automatic | rate of frames during playback |
| %AnimationDuration | Automatic | duration of the full animation |
| %AnimationLooping | True | whether to loop the video |
"

SetUsage @ "
ContinuousAnimation[expr$] creates a %AnimationObject[$$] in which \[FormalT] runs from 0 to 1.
ContinuousAnimation[Animate[$$]] turns an ^Animate[$$] object into the equivalent %AnimationObject[$$].
* AnimationPart, AnimationRange, and AnimationBetween can be used within the expression.
* The following options are supported:
| FrameTime | Automatic | time to devote to each frame |
| FrameRate | Automatic | rate of frames during playback |
| AnimationDuration | Automatic | duration of the full animation |
| AnimationLooping | True | whether to loop the video |
"

(* user construction *)
DiscreteAnimation[obj_, nmax:Except[_Rule]:Automatic, opts___Rule] :=
  CatchMessage @ makeDiscreteAnimation[Unevaluated @ obj, nmax, opts];

ContinuousAnimation[obj_, opts___Rule] :=
  CatchMessage @ makeContinuousAnimation[Unevaluated @ obj, opts];

(**************************************************************************************************)

$optsSpec = (_Rule | _RuleDelayed)...;

(* this is what ListAnimate evaluates to *)
DiscreteAnimation[HoldPattern @ Manipulate[PaneSelector[rules:{___Rule}, _Dynamic, ___], {_, __Integer, AnimationRate -> fps_, ___}, ___]] := CatchMessage @ Scope[
  vals = Values[rules];
  makeDiscreteAnimation[
    AnimationPart[vals],
    Automatic,
    FrameRate -> fps, AnimationOrigin -> "ListAnimate"
  ]
]

(**************************************************************************************************)

(* this is what Animate evaluates to *)
ContinuousAnimation[HoldPattern[Manipulate][expr_, {{var_Symbol, __} | var_Symbol, min_, max_, ___}, $optsSpec]] := With[
  {scale = max - min},
  makeContinuousAnimation[
    Hold[expr] /. HoldPattern[var] :> (\[FormalT] * scale) + min
  ]
];

(**************************************************************************************************)

(* list manipulate *)
DiscreteAnimation[HoldPattern[Manipulate][expr_, {{var_Symbol, __} | var_Symbol, l_List}, $optsSpec]] :=
  makeDiscreteAnimation[
    Hold[expr] /. HoldPattern[var] :> AnimationPart[l],
    Automatic,
    AnimationOrigin -> "Manipulate"
  ];

(**************************************************************************************************)

(* stepped manipulate *)
DiscreteAnimation[HoldPattern[Manipulate][expr_, {{var_Symbol, __} | var_Symbol, min_, max_, step_ ? NumericQ, ___}, $optsSpec]] := CatchMessage @ With[
  {scale = max - min, min2 = min, max2 = max, step2 = step},
  makeDiscreteAnimation[
    Hold[expr] /. HoldPattern[var] :> AnimationRange[min2, max2, step2],
    Automatic,
    AnimationOrigin -> "Manipulate"
  ]
];

(**************************************************************************************************)

General::manipanim = "Could not figure out how to convert Manipulate into an animation."

ContinuousAnimation[m_Manipulate] := (Message[ContinuousAnimation::manipanim]; $Failed);
DiscreteAnimation[_Manipulate] := (Message[DiscreteAnimation::manipanim]; $Failed);

(**************************************************************************************************)

makeDiscreteAnimation[Hold[obj_] | obj_, nmax_, opts___Rule] := Scope[
  hobj = Hold[obj] /. $discreteAnimationRules;
  n = DeepCases[hobj, AnimationPart[l_List, _] :> Length[l]];
  If[n === {}, n = {1}];
  SetAutomatic[nmax, Infinity];
  n = Min[Max @ n, nmax];
  hobj //= ReplaceAll[\[FormalN] -> n];
  animationMetadata = optsToAssoc[DiscreteAnimation, opts];
  animationMetadata["FrameCount"] = n;
  animationMetadata["AnimationType"] = "Discrete";
  animationMetadata["ExpressionHash"] = Base36Hash @ hobj;
  solveFrameStuff[animationMetadata];
  Replace[hobj, Hold[e_] :> AnimationObject[e, animationMetadata]]
];

$discreteAnimationRules = {
  AnimationPart[p_] :> RuleCondition @ AnimationPart[p, \[FormalT]],
  AnimationRange[args___] :> RuleCondition @ AnimationPart[Range[args], \[FormalT]],
  AnimationBetween[a_, b_] :> RuleCondition @ NumericLerp[a, b, \[FormalT] / \[FormalN]]
};

(**************************************************************************************************)

makeContinuousAnimation[Hold[obj_] | obj_, opts___Rule] := Scope[
  hobj = Hold[obj] /. $continuousAnimationRules;
  animationMetadata = optsToAssoc[ContinuousAnimation, opts];
  animationMetadata["FrameCount"] = Automatic;
  solveFrameStuff[animationMetadata];
  animationMetadata["AnimationType"] = "Continuous";
  animationMetadata["ExpressionHash"] = Base36Hash @ hobj;
  Replace[hobj, Hold[e_] :> AnimationObject[e, animationMetadata]]
];

$continuousAnimationRules = {
  AnimationPart[p_] :> RuleCondition @ AnimationPart[p, \[FormalT] * Length[p]],
  AnimationRange[args___] :> RuleCondition @ AnimationPart[Range[args], \[FormalT]],
  AnimationBetween[a_, b_] :> RuleCondition @ NumericLerp[a, b, \[FormalT]]
};


(**************************************************************************************************)

optsToAssoc[head_Symbol, opts___] := KeyMap[SymbolName, Association[Options[head], opts]];
optsToAssoc[None, opts___] := KeyMap[SymbolName, Association[opts]];

(**************************************************************************************************)

PublicFunction[ManipulateAnimation]

ManipulateAnimation = Case[

  m:HoldPattern[Manipulate[expr_, Repeated[_List, {2, Infinity}], opts:$optSpec]] :=
    % @ reduceManipulate @ m;

  m:HoldPattern[Manipulate[_, $discSpec, $optSpec]] := DiscreteAnimation[m];

  m:HoldPattern[Manipulate[_PaneSelector, ___]] := DiscreteAnimation[m];

  m:HoldPattern[Manipulate[_, $contSpec, $optSpec]] := ContinuousAnimation[m];

  m_Manipulate := (Message[ManipulateAnimation::manipanim]; $Failed);

  _ := $Failed;
,
  {
    $discSpec -> ({{_Symbol, __} | _Symbol, _List} | {{_Symbol, __} | _Symbol, _, _, _ ? NumericQ, ___Rule}),
    $contSpec -> ({{_Symbol, __} | _Symbol, _, _, ___Rule}),
    $optSpec -> ((_Rule | _RuleDelayed)...)
  }
]

(* TODO: do this left and right, and make a whole complex of it *)

reduceManipulate = Case[

  HoldPattern @ Manipulate[expr_, {{sym_Symbol, init_, ___}, ___}, rest__List, opts___] :=
    %[Manipulate[expr, rest, opts] /. sym -> init];

  HoldPattern @ Manipulate[expr_, {sym_Symbol, list_List, ___}, rest__List, opts___] :=
    %[Manipulate[expr, rest, opts] /. sym -> First[list]];

  HoldPattern @ Manipulate[expr_, {sym_Symbol, min_, ___}, rest__List, opts___] :=
    %[Manipulate[expr, rest, opts] /. sym -> min];

  other_ := other;
]

(**************************************************************************************************)

PublicObject[AnimationObject]

SetHoldFirst[AnimationObject];

declareBoxFormatting[
  g:AnimationObject[_, _Association ? HoldAtomQ] :> With[{res = animatedThumbnailBoxes[g]}, res /; res =!= $Failed]
];

animatedThumbnailBoxes[g_] := Scope[
  res = g["AnimatedThumbnail"];
  If[Head[res] =!= FrameBox, ReturnFailed[], res]
];

(**************************************************************************************************)

obj_AnimationObject["AnimatedThumbnail"] :=
  makeAnimatedThumbnail[
    obj["GraphicsList", MaxFrames -> 8],
    4, 128, Switch[obj["AnimationType"], "Continuous", $Purple, "Discrete", $Teal]
  ]

makeAnimatedThumbnail[frames_, frameRate_, maxSize_, color_] :=
  Framed[
    imgs = FastRasterizeListCenterPadded @ frames;
    dims = ImageDimensions /@ imgs;
    If[Max[dims] > maxSize, imgs = ImageResize[#, {maxSize}]& /@ imgs];
    AnimatedImage[
      imgs,
      AnimationRepetitions -> Infinity,
      FrameRate -> frameRate
    ],
    FrameStyle -> color
  ];

(**************************************************************************************************)

obj_AnimationObject["VideoToClipboard", args___] := CopyFileToClipboard @ VideoFilePath @ obj["Video", args];

(**************************************************************************************************)

CopyFileToClipboard[g_AnimationObject] := g["VideoToClipboard"];

Unprotect[SystemOpen];
SystemOpen[HoldPattern @ Video[File[path_String], ___]] := SystemOpen @ path;
Protect[SystemOpen];

(**************************************************************************************************)

(obj:AnimationObject[g_, _])["Video", opts___Rule] := Scope[
  metadata = obj["ResolvedMetadata", opts];
  UnpackAssociation[metadata, frameRate, frameCount, expressionHash];
  frameCount //= Round;
  If[frameRate == Round[frameRate], frameRate //= Round];
  path = File @ CacheVideoFilePath["ao", expressionHash, frameCount, frameRate];
  If[FileExistsQ[path], Video @ path,
    frames = obj["FrameList", opts];
    VideoRasterizeList[frames, path, frameRate]
  ]
];

(**************************************************************************************************)

AnimationObject[_, assoc_Association]["AnimationType"] = assoc["AnimationType"];

AnimationObject[_, assoc_Association]["Metadata"] := assoc;

AnimationObject[_, assoc_Association]["ResolvedMetadata", opts___] := Scope[
  metadata = solveFrameStuff[assoc, <|AnimationDuration -> 4, FrameRate -> 30|>];
  metadata
];

contQ[meta_] := meta["AnimationType"] === "Continuous";

(**************************************************************************************************)

AnimationObject[_, meta_]["RangeSpec"] :=
  If[contQ[meta],
    {\[FormalT], 0, 1},
    {\[FormalT], 1, meta["FrameCount"]}
  ];

(**************************************************************************************************)

AnimationObject[g_, meta_]["Manipulate"] :=
  Construct[
    Animate, Unevaluated @ g,
    toRangeSpec @ meta
  ];

toRangeSpec[meta_] := If[contQ[meta],
  {\[FormalT], 0, 1},
  {\[FormalT], 1, meta["FrameCount"]}
];

(**************************************************************************************************)

obj_AnimationObject["AnimatedImage", opts___Rule] := Scope[
  meta = obj["ResolvedMetadata", opts];
  frames = obj["FrameList", opts];
  AnimatedImage[frames, FrameRate -> meta["FrameRate"]]
]

(**************************************************************************************************)

obj_AnimationObject["FrameList", opts___Rule] := CatchMessage @
  FastRasterizeListCenterPadded @ obj["GraphicsList", opts];

(**************************************************************************************************)

AnimationObject[g_, _]["GraphicsFunction"] := CatchMessage @
  Construct[Function, \[FormalT], Unevaluated @ g];

(**************************************************************************************************)

obj_AnimationObject["GraphicsList", opts___Rule] := CatchMessage[
  meta = obj["ResolvedMetadata", opts];
  fn = obj["GraphicsFunction"];
  times = getFrameTimes[meta, Lookup[{opts}, MaxFrames, Infinity]];
  Map[fn, times]
];

(**************************************************************************************************)

getFrameTimes[meta_, max_] := Scope[
  num = meta["FrameCount"];
  frameTimes = If[contQ[meta], Lerp[0, 1, Into[num]], Range[1, num]];
  If[IntegerQ[max] && num > max, frameTimes = Part[frameTimes, Round @ Lerp[1, num, Into @ max]]];
  frameTimes
];

(**************************************************************************************************)

PrivateFunction[solveFrameStuff]

solveFrameStuff[assoc_Association, hints:_Association:<||>] := Scope[
  UnpackAssociation[assoc, frameRate, frameDuration, frameCount, animationDuration, "Default" -> Automatic];
  checkDone = Function[
    setInv[frameRate, frameDuration];
    setTimes[animationDuration, frameCount, frameDuration];
    setInv[frameRate, frameDuration];
    res = PackAssociation[frameRate, frameDuration, frameCount, animationDuration];
    If[AllTrue[res, numQ], Return[Join[assoc, res], Block]]
  ];
  checkDone[];
  KeyValueScan[
    {key, val} |-> Switch[key,
      FrameRate,         frameRate = val;         checkDone[],
      FrameCount,        frameCount = val;        checkDone[],
      FrameDuration,     frameDuration = val;     checkDone[],
      AnimationDuration, animationDuration = val; checkDone[]
    ],
    hints
  ];
  $Failed
];

autQ = MatchQ[Automatic];
numQ = NumericQ;

General::inconstf = "Inconsistency between FrameDuration, FrameCount, AnimationDuration, and the number of frames."

SetHoldAll[setInv, setTimes];
setInv[a_ ? numQ, b_ ? autQ] := Set[b, 1 / a];
setInv[a_ ? autQ, b_ ? numQ] := Set[a, 1 / b];
setInv[a_ ? numQ, b_ ? numQ] := If[a != 1 / b, ThrowMessage["inconstf"]];

setTimes[a_ ? autQ, b_ ? numQ, c_ ? numQ] := Set[a, b * c];
setTimes[a_ ? numQ, b_ ? autQ, c_ ? numQ] := Set[b, a / c];
setTimes[a_ ? numQ, b_ ? numQ, c_ ? autQ] := Set[c, a / b];
setTimes[a_ ? autQ, b_ ? autQ, c_ ? numQ] := Null;
setTimes[a_ ? numQ, b_ ? numQ, c_ ? numQ] := If[a != b * c, ThrowMessage["inconstf"]];

(**************************************************************************************************)

DeclareGraphicsPrimitive[AnimationPart, "Primitives", animationBoxes];
DeclareGraphicsPrimitive[AnimationRange, "Opaque", animationBoxes];
DeclareGraphicsPrimitive[AnimationBetween, "Primitives,Primitives", animationBoxes];

animationBoxes[AnimationPart[p_List, ___]] := ToGraphicsBoxes @ First @ p;
animationBoxes[AnimationRange[args___]] := ToGraphicsBoxes @ First @ Range[args];
animationBoxes[AnimationBetween[a_, b_]] := ToGraphicsBoxes @ a;

(**************************************************************************************************)

PublicFunction[AnimationPart, AnimationRange, AnimationBetween]

SetUsage @ "
AnimationPart[list$] will take values from a list during a discrete animation.
"

AnimationPart[e_, i_Integer] := With[{e2 = e}, {n = Length[e2]}, Part[e2, Clip[i, {1, n}]]];

SetUsage @ "
AnimationRange[n$] will take on values 1 through n$ during a discrete animation.
"

SetUsage @ "
AnimationBetween[a$, b$] will interpolate between a$ and b$ during a continuous animation."

(* TODO: Make AnimationBetween *find* a lerp for more complex objections. *)

(**************************************************************************************************)

PublicFunction[FindAnimationLerp]
PublicOption[EasingFunction]

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
    ContinuousAnimation @ res
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
    Sequence @@ DropOptions[unifyOpts[{oa}, {ob}], {ImageSize, PlotRange}]
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

PublicFunction[Graphics3DSpinAnimation]

Graphics3DSpinAnimation[g_Graphics3D, angleDelta_:1] := Scope[
  viewPoint = LookupOption[g, ViewPoint, Automatic];
  angles0 = ToSpherical @ viewPoint;
  angles1 = applyAngleDelta[angles0, angleDelta];
  viewPoint01 = ComposedNumericLerp[FromSpherical, angles0, angles1, \[FormalT]];
  gSpin = ReplaceOptions[g, ViewPoint -> viewPoint01];
  ContinuousAnimation[gSpin]
];

applyAngleDelta[{r_, a_, b_}, {dr_, da_, db_}] := {r + dr, a + Pi * da, b + Tau * db};
applyAngleDelta[{r_, a_, b_}, {da_, db_}] := {r, a + Pi * da, b + Tau * db};
applyAngleDelta[{r_, a_, b_}, db_] := {r, a, b + Tau * db};

(**************************************************************************************************)

PublicFunction[VertexLayoutAnimationLerp]

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

PublicFunction[NumericLerp]

NumericLerp[a_, b_, t_ ? NumericQ] := linearLerp[a, b, t];

(**************************************************************************************************)

PublicFunction[ComposedNumericLerp]

ComposedNumericLerp[f_, a_, b_, t_ ? NumericQ] := f @ linearLerp[a, b, t];
