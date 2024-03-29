PublicTypesettingForm[FixedGraphics]

PublicOption[GraphicsScale, ResolveInsetGraphics, PropogateGraphicsScale]

SetUsage @ "
FixedGraphics[prims$] is like %Graphics but applies a fixed scaling from plot range coordinates to screen space.

* %GraphicsScale -> pixels$ specifices that a plot range of 1 corresponds to pixels$. The default is 100.

* to achieve this, bounding boxes of boxified primitives are calculated using %PrimitiveBoxesBounds.

## Options

* the following novel options are supported:

| %DebugBounds | whether to put a bounding frame around graphics contents |
| %PropogateGraphicsScale | whether to set a global %GraphicsScale option during boxification |
| %ResolveInsetGraphics | whether to convert graphical %Inset primitives to non-inset equivalents |

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

## ZOrder

* any expression Style[$$, %ZOrder -> n$] will extract $$ and place it above or below the main primitives.

* currently, ambient styles are not preserved when this is done.

* nested %ZOrder directives will apply further sorting within $$.

"

Options[FixedGraphics] = {
  Background -> None,
  BaselinePosition -> Auto,
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
FixedGraphicsBoxes[FixedGraphics[prims_, opts___]] := Scope @ CatchMessage[FixedGraphicsBoxes,
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
  boxes //= RepAll[{i_InsetBox :> i, Offset[o_, p_] :> RuleEval[p + o / $graphicsScale]}];
  If[resolveInsetGraphics, boxes //= RepAll[i_InsetBox :> RuleEval @ embedInset @ i]];
  bounds = PrimitiveBoxesBounds[boxes, $graphicsScale];
  imagePadding //= StandardizePadding;

  tickPosition = Switch[ticks,
    {False, False},       None,
    True | Above | _List, Above,
    Below,                Below,
    _,                    None
  ];

  If[tickPosition =!= None,
    {tickX, tickY} = If[PairQ[ticks], ticks, {True, True}];
    tickPadding = {{15, If[tickY, 30, 15]}, {If[tickX, 30, 15], 15}};
    imagePadding += tickPadding;
    tickBoxes = GridLineBoxes[bounds, Ticks -> {tickX, tickY}, GraphicsScale -> $graphicsScale];
    boxes = If[tickPosition === Above, {boxes, tickBoxes}, {tickBoxes, boxes}];
  ];

  boxes //= ZSortBoxes;

  decos = {};
  If[TrueQ @ debugBounds,
    AppTo[decos, StyleBox[
      RectangleBox @@ Transpose[bounds],
      FaceForm @ None, EdgeForm @ RGBColor[1, 0, 0, .5]
    ]];
  ];

  If[TrueQ @ frame,
    (* TODO: support FrameMargins *)
    padding = StandardizePadding @ frameMargins;
    If[FailureQ @ padding, OptionMsg[FrameMargins, frameMargins]];
    bounds = EnlargeBounds[bounds, padding / $graphicsScale];
    AppTo[decos, StyleBox[
      RectangleBox @@ Transpose[bounds],
      FaceForm @ None, EdgeForm @ $Gray]];
    bounds = EnlargeBounds[bounds, 1 / $graphicsScale];
  ];

  If[plotLabel =!= None,
    spacing = 5;
    labelPos = {Mean @ F @ bounds, LL[bounds] + spacing / $graphicsScale};
    text = Text[plotLabel, labelPos, {0, -1}, BaseStyle -> {FontFamily -> "Arial", FontSize -> 12}];
    {w, h} = MakeTextImageSize[text] + 1;
    Part[bounds, 2, 2] += (h + spacing) / $graphicsScale;
    AppTo[decos, ToGraphicsBoxes @ text];
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
    If[baselinePosition === Auto, Seq[], BaselinePosition -> baselinePosition]
  ]
];

(**************************************************************************************************)

(* this is recursive, Z-orders within another Z-order will sort within it *)
ZSortBoxes[boxes_] /; FreeQ[boxes, ZOrder -> _] := boxes
ZSortBoxes[boxes_] := Module[
  {zassoc = Assoc[]},
  zassoc[0] = RepAll[boxes, {
    StyleBox[b_, l___, ZOrder -> z_, r___] :> (
      KAppTo[zassoc, z, toStyleBox[b, l, r]]; {}
    ),
    (* Style[foo, ZOrder -> 1] gets an extra list when boxified for some reason *)
    StyleBox[b_, l___, {l2___, ZOrder -> z_, r2___}, r___] :> (
      KAppTo[zassoc, z, toStyleBox[b, l, {l2, r2}, r]]; {}
    )
  }];
  layers = Values @ KSort @ zassoc;
  Map[ZSortBoxes, layers]
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

  i:InsetBox[GraphicsBox[boxes_, opts___Rule], pos_:{0,0}, origin_:Auto, insetSize_:Auto, dir_:Auto] := Scope[
    SetAuto[dir, {1, 0}];
    pos //= ResolveOffsets[$graphicsScale];
    If[!MatchQ[dir, $CoordP], Message[FixedGraphics::badInsetDir, dir]; Return @ i];
    UnpackAnonymousOptions[{opts}, Auto, imageSize, plotRange, plotRangePadding, alignmentPos];
    (* TODO: there are more specs like Offset which would be easy to support *)
    insetSize = If[insetSize =!= Auto,
      F[insetSize, insetSize] * $graphicsScale,
      imageSize
    ];
    SetAll[plotRange, Auto];
    SetAuto[origin, alignmentPos];
    SetAuto[plotRange, PrimitiveBoxesBounds[boxes, None]];
    origin //= resolveOrigin;
    plotRange = PlotRangePad[plotRange, plotRangePadding];
    {plotWidth, plotHeight} = plotSize = Dist @@@ plotRange;
    imageWidth = If[ListQ[insetSize], F @ insetSize, insetSize];
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

  expr_ := (Message[FixedGraphics::badInset, expr]; expr)
];

FixedGraphics::badOrigin = "Inset origin `` should be a coordinate, or a symbolic position.";

resolveOrigin = Case[
  Center | Auto | Axis :=
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

  other_ := (Message[FixedGraphics::badOrigin, other]; {0, 0})
]

(**************************************************************************************************)

PublicFunction[DebugGraphics]

DebugGraphics[g_, opts___] :=
  FixedGraphics[g, Ticks -> True, opts];
