PublicTypesettingForm[FixedGraphics]

PublicOption[ResolveInsetGraphics]

Options[FixedGraphics] = {
  Background -> None,
  BaselinePosition -> Automatic,
  GraphicsScale -> 100,
  Epilog -> {},
  Prolog -> {},
  ImagePadding -> 2,
  PlotLabel -> None,
  Ticks -> False,
  DebugBounds -> False,
  ResolveInsetGraphics -> True,
  "UseGraphicsScale" -> True
};

SetUsage @ "
FixedGraphics[prims$] is like %Graphics but naturally incorporates a scale factor.
* the plot range is computed automically.
"

DefineStandardTraditionalForm[e_FixedGraphics :> FixedGraphicsBoxes[e]]

FixedGraphicsBoxes[_] := "?";
FixedGraphicsBoxes[FixedGraphics[prims_, opts___]] := Scope[
  UnpackOptionsAs[FixedGraphics, {opts},
    background, baselinePosition, $graphicsScale, epilog, prolog, imagePadding,
    ticks, debugBounds, resolveInsetGraphics, useGraphicsScale
  ];
  (* FontSize, FontFamily ? *)
  InheritedBlock[{$MakeBoxesStyleData},
    If[TrueQ @ useGraphicsScale, $MakeBoxesStyleData[GraphicsScale] = $graphicsScale];
    boxes = ToGraphicsBoxes @ prims;
    epilog //= ToGraphicsBoxes;
    prolog //= ToGraphicsBoxes;
  ];
  boxes //= ReplaceAll[{i_InsetBox :> i, Offset[o_, p_] :> RuleCondition[p + o / $graphicsScale]}];
  If[resolveInsetGraphics, boxes //= ReplaceAll[i_InsetBox :> RuleCondition @ embedInset @ i]];
  bounds = PrimitiveBoxesBounds[boxes, $graphicsScale];
  wh = EuclideanDistance @@@ bounds;
  imagePadding //= StandardizePadding;
  If[TrueQ @ ticks,
    imagePadding += {{15, 30}, {30, 15}};
    boxes = {boxes, boundingGridBoxes @ BoundingGrid[bounds, $graphicsScale]}
  ];
  If[TrueQ @ debugBounds,
    boxes = {{FaceForm @ None, EdgeForm @ RGBColor[1, 0, 0, .5], RectangleBox @@ Transpose[bounds]}, boxes};
  ];
  imageSize = Ceiling[wh * $graphicsScale + Total[imagePadding]];
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

PrivateFunction[embedInsetBoxWithScale]

embedInsetBoxWithScale[i_InsetBox, scale_] := Scope[
  $graphicsScale = scale;
  res = embedInset[i];
  If[Head[res] === InsetBox, $Failed, res]
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
    {plotWidth, plotHeight} = plotSize = EuclideanDistance @@@ plotRange;
    imageWidth = If[ListQ[insetSize], First @ insetSize, insetSize];
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

