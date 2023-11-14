PublicTypesettingForm[FixedGraphics]

PublicOption[GraphicsScale, ResolveInsetGraphics, PropogateGraphicsScale]

SetUsage @ "
FixedGraphics[prims$, opts$$] is like %Graphics but applies a fixed scaling from plot range coordinates to pixels.

* to achieve this, bounding boxes of boxified primitives are calculated using %PrimitiveBoxesBounds.

* the following existing %Graphics options are supported:
| %Background |
| %BaselinePosition |
| %Epilog |
| %Prolog |
| %ImagePadding |
| %PlotLabel |
| %Frame |
| %FrameMargins |
| %Ticks |

* frames and %ImagePadding will produce increases in %ImageSize to maintain the scale.

* the %Ticks option can be True, which will produce automatic labeled ticks.

* any expression Style[$$, %ZOrder -> n$] will extract $$ and place it above or below the main primitives.
* currently, ambient styles are not preserved when this is done.
* nested %ZOrder directives will apply further sorting within $$.

* the following novel options are supported:
| %DebugBounds | whether to put a bounding frame around graphics contents |
| %PropogateGraphicsScale | whether to set a global %GraphicsScale option during boxification |
| %ResolveInsetGraphics | whether to convert graphical %Inset primitives to non-inset equivalents |
"

Options[FixedGraphics] = {
  Background -> None,
  BaselinePosition -> Automatic,
  GraphicsScale -> 100,
  Epilog -> {},
  Prolog -> {},
  ImagePadding -> 2,
  PlotLabel -> None,
  Ticks -> False,
  Frame -> False,
  FrameMargins -> 5,
  DebugBounds -> False,
  ResolveInsetGraphics -> True,
  PropogateGraphicsScale -> True
};

DefineStandardTraditionalForm[e_FixedGraphics :> FixedGraphicsBoxes[e]]

FixedGraphicsBoxes[_] := "?";
FixedGraphicsBoxes[FixedGraphics[prims_, opts___]] := Scope[
  UnpackOptionsAs[FixedGraphics, {opts},
    background, baselinePosition, $graphicsScale, epilog, prolog, imagePadding,
    ticks, debugBounds, resolveInsetGraphics, propogateGraphicsScale,
    frame, frameMargins, plotLabel
  ];
  (* FontSize, FontFamily ? *)
  InheritedBlock[{$MakeBoxesStyleData},
    If[TrueQ @ propogateGraphicsScale, $MakeBoxesStyleData[GraphicsScale] = $graphicsScale];
    boxes = ToGraphicsBoxes @ prims;
    epilog //= ToGraphicsBoxes;
    prolog //= ToGraphicsBoxes;
  ];
  boxes //= ReplaceAll[{i_InsetBox :> i, Offset[o_, p_] :> RuleCondition[p + o / $graphicsScale]}];
  If[resolveInsetGraphics, boxes //= ReplaceAll[i_InsetBox :> RuleCondition @ embedInset @ i]];
  bounds = PrimitiveBoxesBounds[boxes, $graphicsScale];
  imagePadding //= StandardizePadding;
  tickPosition = Switch[ticks, True | Above, Above, Below, Below, _, None];
  If[tickPosition =!= None,
    imagePadding += {{15, 30}, {30, 15}};
    tickBoxes = boundingGridBoxes @ BoundingGrid[bounds, $graphicsScale];
    boxes = If[tickPosition === Above, {boxes, tickBoxes}, {tickBoxes, boxes}];
  ];

  boxes //= ZSortBoxes;

  decos = {};
  If[TrueQ @ debugBounds,
    AppendTo[decos, StyleBox[
      RectangleBox @@ Transpose[bounds],
      FaceForm @ None, EdgeForm @ RGBColor[1, 0, 0, .5]
    ]];
  ];

  If[TrueQ @ frame,
    (* TODO: support FrameMargins *)
    padding = StandardizePadding @ frameMargins;
    If[FailureQ @ padding, BadOptionSetting[FixedGraphics, FrameMargins, frameMargins]];
    bounds = EnlargeBounds[bounds, padding / $graphicsScale];
    AppendTo[decos, StyleBox[
      RectangleBox @@ Transpose[bounds],
      FaceForm @ None, EdgeForm @ $Gray]];
    bounds = EnlargeBounds[bounds, 1 / $graphicsScale];
  ];

  If[plotLabel =!= None,
    spacing = 5;
    labelPos = {Mean @ P1 @ bounds, P22[bounds] + spacing / $graphicsScale};
    text = Text[plotLabel, labelPos, {0, -1}, BaseStyle -> {FontFamily -> "Arial", FontSize -> 12}];
    {w, h} = MakeTextImageSize[text] + 1;
    Part[bounds, 2, 2] += (h + spacing) / $graphicsScale;
    AppendTo[decos, ToGraphicsBoxes @ text];
  ];

  If[decos =!= {}, epilog = If[epilog === {}, decos, {decos, epilog}]];

  wh = Dist @@@ bounds;
  imageSize = wh * $graphicsScale + Map[Total, imagePadding];
  Construct[GraphicsBox,
    boxes,
    PlotRange -> bounds,
    PlotRangePadding -> 0,
    ImagePadding -> imagePadding,
    ImageSize -> imageSize,
    If[epilog === {}, Seq[], Epilog -> epilog],
    If[prolog === {}, Seq[], Prolog -> prolog],
    If[background === None, Seq[], Background -> background],
    If[baselinePosition === Automatic, Seq[], BaselinePosition -> baselinePosition]
  ]
];

(**************************************************************************************************)

(* this is recursive, Z-orders within another Z-order will sort within it *)
ZSortBoxes[boxes_] /; FreeQ[boxes, ZOrder] := boxes
ZSortBoxes[boxes_] := Module[
  {zassoc = Assoc[]},
  zassoc[0] = ReplaceAll[boxes,
    StyleBox[b_, l___, ZOrder -> z_, r___] :> (
      KeyAppendTo[zassoc, z, toStyleBox[b, l, r]]; {}
    )
  ];
  Map[ZSortBoxes, Values @ KeySort @ zassoc]
];

toStyleBox[args__] := StyleBox[args];
toStyleBox[b_] := b;

(**************************************************************************************************)

PrivateFunction[embedInsetBoxWithScale]

embedInsetBoxWithScale[i_InsetBox, scale_] := Scope[
  $graphicsScale = scale;
  res = embedInset[i];
  If[H[res] === InsetBox, $Failed, res]
];

FixedGraphics::badInsetDir = "Inset direction `` should be a vector or Automatic.";
FixedGraphics::badInsetSize = "Inset size `` should be a single number or a pair.";
FixedGraphics::badInset = "Unrecognized inset ``."

embedInset = Case[

  i:InsetBox[GraphicsBox[boxes_, opts___Rule], pos_:{0,0}, origin_:Automatic, insetSize_:Automatic, dir_:Automatic] := Scope[
    SetAutomatic[dir, {1, 0}];
    pos //= ResolveOffsets[$graphicsScale];
    If[!MatchQ[dir, $CoordP], Message[FixedGraphics::badInsetDir, dir]; Return @ i];
    UnpackAnonymousOptions[{opts}, Automatic, imageSize, plotRange, plotRangePadding, alignmentPos];
    (* TODO: there are more specs like Offset which would be easy to support *)
    insetSize = If[insetSize =!= Automatic,
      First[insetSize, insetSize] * $graphicsScale,
      imageSize
    ];
    SetAll[plotRange, Automatic];
    SetAutomatic[origin, alignmentPos];
    SetAutomatic[plotRange, PrimitiveBoxesBounds[boxes, None]];
    origin //= resolveOrigin;
    plotRange = PlotRangePad[plotRange, plotRangePadding];
    {plotWidth, plotHeight} = plotSize = Dist @@@ plotRange;
    imageWidth = If[ListQ[insetSize], P1 @ insetSize, insetSize];
    If[!NumberQ[imageWidth], Message[FixedGraphics::badInsetSize, insetSize]; Return @ i];
    scaleFactor = imageWidth / plotWidth / $graphicsScale;
    rotMatrix = RotateToMatrix[Normalize[dir] * scaleFactor];
    If[Norm[origin] != 0., boxes = Construct[GeometricTransformationBox, boxes, -origin]];
    Construct[GeometricTransformationBox, boxes, {rotMatrix, pos}]
  ];

  i:InsetBox[l_, Offset[o_, p_], r___] := With[{p2 = p + o / $graphicsScale},
    InsetBox[l, p2, r]
  ];

  i:InsetBox[_FormBox, ___] := i;

  (* this happpens in the terminal, which doesn't add a FormBox apparently, though
  i have since fixed that by using an internal variable in A0Usage.m *)
  i:InsetBox[_, _, _ImageScaled] := i;

  expr_ := (Message[FixedGraphics::badInset, MsgExpr @ expr]; expr)
];

FixedGraphics::badOrigin = "Inset origin `` should be a coordinate, or a symbolic position.";

resolveOrigin = Case[
  Center | Automatic | Axis :=
    Mean /@ plotRange;

  (* TODO: support a pair e.g. {Left, Top} *)
  symbol_Symbol := Scope[
    {{x1, x2}, {y1, y2}} = plotRange;
    {x, y} = (Lookup[$SideToCoords, symbol] + 1)/2;
    List[
      If[NumberQ[x], Lerp[x1, x2, x], 0 (* catches Axis *)],
      If[NumberQ[y], Lerp[y1, y2, y], 0]
    ]
  ];

  pos:$CoordP := pos;

  other_ := (Message[FixedGraphics::badOrigin, MsgExpr @ other]; {0, 0})
]

(**************************************************************************************************)

PublicFunction[DebugGraphics]

DebugGraphics[g_, opts___] :=
  FixedGraphics[g, Ticks -> True, opts];

