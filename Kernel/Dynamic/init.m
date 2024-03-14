PrivateVariable[$dPlotRectangleStyle, $dPlotFrameStyle, $dPlotSelectionRectangleStyle, $dPlotLabelStyle]

$dPlotRectangleStyle          = Sequence[EdgeForm @ {GrayLevel[0.5], AbsoluteThickness[1]}, FaceForm @ GrayLevel[0.8]];
$dPlotFrameStyle              = Sequence[FaceForm @ None, EdgeForm @ GrayLevel[0.5]];
$dPlotSelectionRectangleStyle = Sequence[FaceForm @ $dPlotSelectionColor, EdgeForm @ None];
$dPlotLabelStyle              = Sequence[];

(*************************************************************************************************)

PrivateVariable[$dPlotSelectionColor, $dPlotNoneColor]

$dPlotSelectionColor          = RGBColor[0.42, 0.69, 0.96, 0.2];
$dPlotNoneColor               = $DarkRed;

(*************************************************************************************************)

PrivateVariable[$dPlotNoneSymbol]

$dPlotNoneSymbol = "\[LongDash]";

(*************************************************************************************************)

PrivateVariable[$baseDPlotOptions]

$baseDPlotOptions = {
  ImageSize              -> Automatic,
  ImagePadding           -> 10,
  AdditionalImagePadding -> 0,
  ColorRules             -> {},
  DynamicFilter          -> All,
  DynamicSelection       -> All,
  PlotLabel              -> None
};

(*************************************************************************************************)

PrivateFunction[procDPlotOptions]

procDPlotOptions[head_Symbol, opts_List, opts2___Rule] := Scope[

  UnpackOptionsAs[head, opts,
    imageSize, imagePadding, additionalImagePadding,
    colorRules, plotLabel,
    dynamicFilter, dynamicSelection
  ];

  UnpackStringOptions[{opts2}, 1, scaleFactor, heightFactor];
  scaleFactor *= 300;

  SetAutomatic[imageSize, Scaled[1]];
  SetScaledFactor[imageSize, scaleFactor];

  imageSize = Floor @ Switch[imageSize,
    $NumberP,              {1, 1 / heightFactor} * imageSize,
    {$NumberP, $NumberP},  imageSize,
    True,                  checkDPlotSetting[False, opts, ImageSize];
  ];

  imagePadding //= StandardizePadding;
  additionalImagePadding //= StandardizePadding;
  checkDPlotSetting[NumericMatrixQ[imagePadding], opts, ImagePadding];
  checkDPlotSetting[NumericMatrixQ[additionalImagePadding], opts, AdditionalImagePadding];

  scale = {xScale, yScale} = 1.0 / imageSize;

  padding = imagePadding + additionalImagePadding;
  paddedSize = ImageSizePad[imageSize, padding];
  dSelection = DValue[dynamicSelection, "Filter"];
  dFilter = dynamicFilter;

  If[!MatchQ[colorRules, {Rule[_Integer, $ColorPattern]...}], badDPlotSetting[ColorRules, colorRules]];
  otherData = {scale, paddedSize, padding, plotLabel, colorRules};

  PackAssociation[xScale, yScale, dSelection, dFilter, otherData, imageSize]
];

_procDOptions := BadArguments[];

(*************************************************************************************************)

PrivateFunction[badDPlotSetting, checkDPlotSetting]

General::badDynamicPlotOptionSetting = "Bad setting `` -> ``.";

badDPlotSetting[sym_Symbol, value_] := ThrowMessage[sym, value];

checkDPlotSetting[True, _, _] := Null;
checkDPlotSetting[other_, opts_, sym_] := badDPlotSetting[sym, Lookup[opts, sym]];

(*************************************************************************************************)

PrivateFunction[makeDGraphicsBox]

Options[makeDGraphicsBox] = {
  "PlotBoxes"      -> $Failed,
  "LabelBoxes"     -> None,
  "TickBoxes"      -> None,
  "SelectionBoxes" -> $Failed,
  "PlotRange"      -> $Failed,
  "FramePadding"   -> $Failed,
  "Frame"          -> True,
  "Handler"        -> None,
  "OtherData"      -> $Failed
};

makeDGraphicsBox[OptionsPattern[]] := Scope[
  UnpackOptions[
    plotBoxes, selectionBoxes, labelBoxes, tickBoxes,
    plotRange, framePadding, frame,
    handler,
    otherData
  ];
  {scale, paddedSize, padding, plotLabel, colorRules} = otherData;

  (* apply color rules dynamically *)
  plotBoxes //= insertColorRules[colorRules];

  primitiveBoxes = {
    attachStyle[plotBoxes,      $dPlotRectangleStyle],
    attachStyle[selectionBoxes, $dPlotSelectionRectangleStyle],
    attachStyle[labelBoxes,     $dPlotLabelStyle],
    attachStyle[tickBoxes,      $dPlotLabelStyle]
  };

  (* calculate the frame *)
  If[frame =!= False,
    plotSize = PlotRangeSize @ plotRange;
    frameBounds = EnlargeBounds[plotRange, framePadding * (plotSize * scale)];
    {{frameL, frameB}, {frameR, frameT}} = frameCorners = Transpose @ frameBounds;
    frameBoxes = Switch[frame,
      True,      Apply[RectangleBox, frameCorners],
      LeftRight, Cons[LineBox, {{{frameL, frameB}, {frameL, frameT}}, {{frameR, frameB}, {frameR, frameT}}}],
      TopBottom, Cons[LineBox, {{{frameL, frameT}, {frameR, frameT}}, {{frameL, frameB}, {frameR, frameB}}}]
    ];
    primitiveBoxes //= ReplaceAll[{$dFrameTop -> frameT, $dFrameBottom -> frameB, $dFrameLeft -> frameL, $dFrameRight -> frameR}];
    AppendTo[primitiveBoxes, StyleBox[frameBoxes, $dPlotFrameStyle]];
  ];

  graphicsBoxes = Cons[GraphicsBox,
    primitiveBoxes,
    PlotRange -> plotRange,
    ImageSize -> paddedSize,
    ImagePadding -> padding,
    AspectRatio -> Full,
    PlotLabel -> If[plotLabel === None, None, ToBoxes @ plotLabel],
    PlotRangeClipping -> False,
    BaseStyle -> {FontFamily -> "Source Code Pro", FontSize -> 10}
  ];

  (* attach the handler if any *)
  If[handler =!= None, graphicsBoxes = TagBox[graphicsBoxes, handler]];

  graphicsBoxes
];

attachStyle[None, ___] := Nothing;
attachStyle[boxes_, style___] := If[FreeQ[boxes, StyleBox], StyleBox[boxes, style], boxes];

(* TODO: handle PointBox etc *)
insertColorRules[{}] = Id;
insertColorRules[rules_][DBoxes[expr_]] := DBoxes[applyDColorRules[expr, rules]];
insertColorRules[rules_][_] := $Failed;

PrivateSymbol[$dFrameTop, $dFrameBottom, $dFrameLeft, $dFrameRight]

(**************************************************************************************************)

PrivateFunction[applyDColorRules]

applyDColorRules[boxes_, {}] := boxes;

applyDColorRules[boxes_, rules_List] := Scope[
  MapApply[
    {i, c} |-> ApplyTo[boxes, MapAt[StyleBoxOperator[FaceEdgeForm @ c], i]],
    rules
  ];
  boxes
];

(**************************************************************************************************)

PrivateFunction[makeDCoordsHandler]

SetHoldRest[makeDCoordsHandler];

makeDCoordsHandler[coordFn_, down_, up_] :=
  makeDCoordsHandler[coordFn, down, None, up];

makeDCoordsHandler[coordFn_, down_, drag_, up_, dragUp_] :=
  makeDCoordsHandler[coordFn, down, drag; dragUp, up; dragUp];

makeDCoordsHandler[coordFn_, down_, drag_, up_] := EventHandlerTag[{
  makeHandlerTagRule1["MouseDown",    coordFn, down],
  makeHandlerTagRule1["MouseDragged", coordFn, drag],
  makeHandlerTagRule1["MouseUp",      coordFn, up]
}];

SetHoldAll[makeHandlerTagRule1];
makeHandlerTagRule1[_, _, None]              := Nothing;
makeHandlerTagRule1[name_, coordFn_, body_]  := makeHandlerTagRule2[name, coordFn, Function[body] /. $ -> #];
makeHandlerTagRule2[name_, coordFn_, fn_]    := name :> GraphicsXYApply[coordFn, fn];

_makeDCoordsHandler := BadArguments[];

(**************************************************************************************************)

PublicFunction[GraphicsXYApply]

GraphicsXYApply[coordFn_, fn_] := Replace[
  MousePosition["Graphics"],
  xy:{_, _} :> Cons[fn, coordFn @ xy]
];
