PublicFunction[StringDiagram]

PublicOption[WireThickness, NodeEdgeThickness, LabelOffset, DiagramSize, TextModifiers, HalfFrame, BaseThickness, CurveFunction]

PublicOption[NodeSize, NodeLabelPosition, NodeLabelFontWeight, NodeLabelFontFamily, NodeLabelFontSize, NodeLabelSpacing, NodeLabelOffset, NodeLabelBackground, NodeLabelFontColor, NodeFrameColor, NodeBackground, NodeShape]

PublicOption[WireLabelFontSize, WireLabelFontWeight, WireLabelFontFamily, WireLabelPosition, WireLabelSpacing, WireLabelOffset, WireLabelBackground, WireLabelFontColor, WireColor]

PublicOption[RegionLabelFontSize, RegionLabelFontWeight, RegionLabelFontFamily]

PublicOption[TickPosition, TickLabelFontSize, TickLabelFontWeight, TickLabelFontFamily, TickLabelSpacing, TickLabelFontColor, TickLength, RegionFilling]


Options[StringDiagram] = {
  ImageSize             -> Auto,
  ImagePadding          -> Auto,
  Epilog                -> {},

  FontSize              -> 16,
  FontWeight            -> Plain,
  FontFamily            :> $MathFont,

  BaseThickness         -> Auto,
  WireThickness         -> Auto,
  NodeEdgeThickness     -> Auto,

  HalfFrame             -> False,
  FrameThickness        -> Auto,
  FrameColor            -> Auto,
  Background            -> None,

  FlipX                 -> False,
  FlipY                 -> False,
  DiagramSize           -> {12, 12},
  GraphicsScale         -> 5,

  TextModifiers         -> {},
  CurveFunction         -> Auto,
  SplitPosition         -> "Start",
  SegmentPosition       -> 0.5,
  "SplitOrientation"    -> Horizontal,

  NodeSize              -> 25,
  NodeShape             -> NodeDisk,
  NodeLabelFontSize     -> Auto,
  NodeLabelFontWeight   -> Bold,
  NodeLabelFontFamily   -> Auto,
  NodeLabelFontColor    -> Black,
  NodeLabelPosition     -> Center,
  NodeLabelSpacing      -> {5, 0},
  NodeLabelOffset       -> {0, 0},
  NodeLabelBackground   -> None,
  NodeFrameColor        -> None,
  NodeBackground        -> White,

  WireLabelFontSize     -> Auto,
  WireLabelFontWeight   -> Auto,
  WireLabelFontFamily   -> Auto,
  WireLabelFontColor    -> Black,
  WireLabelPosition     -> Vertical,
  WireLabelSpacing      -> {5, 0},
  WireLabelOffset       -> {0, 0},
  WireLabelBackground   -> None,
  WireColor             -> Auto,

  RegionLabelFontSize   -> Auto,
  RegionLabelFontWeight -> Auto,
  RegionLabelFontFamily -> Auto,

  FrameTicks            -> {},
  TickPosition          -> Auto,
  TickLength            -> 5,
  TickLabelFontSize     -> Auto,
  TickLabelFontWeight   -> Plain,
  TickLabelFontFamily   -> Auto,
  TickLabelSpacing      -> 3,
  TickLabelFontColor    -> Black,
  Convention            -> RightToLeft,

  RegionFilling         -> "Explicit",
  ColorRules            -> None,
  GradientSymbolOptions -> {Method -> "Raster"}
}

SetUsage @ "
RegionFilling is an option to %StringDiagram that determines how regions should be filled:
| 'Explicit' | do not fill regions unless explicitly passed a color |
| 'Labeled'  | fill regions if a colored region label is used, retaining the label |
| 'Unlabeled' | fill regions but elide the label |
"

(**************************************************************************************************)

$colorOrInt = $ColorPattern | _Int;

StringDiagram[boxes_List, wires_List, opts___Rule] :=
  StringDiagram[boxes, wires, {}, opts];

StringDiagram[boxes_List, wires_List, regions_List, opts:OptionsPattern[]] := Scope @ CatchMessage[
  $boxSizes = $boxAliases = $wireAliases = UAssoc[];
  $fillRegions = {};

  UnpackOptions[
    flipX, flipY, wireThickness, nodeEdgeThickness, nodeBackground, frameThickness, frameColor, curveFunction,
    segmentPosition, splitPosition, splitOrientation,
    background, baseThickness, halfFrame, imagePadding, epilog, fontSize, fontWeight, fontFamily,
    diagramSize, imageSize, graphicsScale, textModifiers, colorRules, regionFilling, gradientSymbolOptions,
    frameTicks, convention
  ];

  If[convention === LeftToRight, flipX = !flipX];

  $lastInnerLabel = None;
  {$w, $h} = diagramSize;
  SetAuto[imageSize, 2 * graphicsScale * diagramSize];

  SetAuto[baseThickness, 2];
  SetAuto[wireThickness, baseThickness];
  SetAuto[nodeEdgeThickness, baseThickness];

  $colorModifierFn = parseDiagramColorRules[colorRules, gradientSymbolOptions];
  {boxTextModifierFn, wireTextModifierFn, regionTextModifierFn} = Map[
    toModifierFunction,
    If[AssocQ[textModifiers],
      Lookup[textModifiers, {"Boxes", "Wires", "Regions"}, {}],
      {textModifiers, textModifiers, textModifiers}
    ]
  ];

  backgroundColor = If[background === None, None,
    If[ColorQ[background], ToRainbowColor @ background, extractColorFromLabel @ $colorModifierFn @ background]
  ];
  defaultWireColor = If[backgroundColor === None, Black, OklabDarker @ backgroundColor];

  UnpackOptions[nodeFrameColor, nodeSize, $nodeShape, nodeLabelFontColor, nodeLabelPosition, nodeLabelSpacing, nodeLabelOffset, nodeLabelBackground, nodeLabelFontSize, nodeLabelFontWeight, nodeLabelFontFamily];
  $fontColor = nodeLabelFontColor;
  $currentDiagramFontSize = $fontSize = SubAuto[nodeLabelFontSize, fontSize];
  $fontWeight = SubAuto[nodeLabelFontWeight, fontWeight];
  $fontFamily = SubAuto[nodeLabelFontFamily, fontFamily];
  labelSpacing = nodeLabelSpacing; labelPosition = nodeLabelPosition; labelOffset = nodeLabelOffset; labelBackground = nodeLabelBackground;
  boxColor = None; bLabelH = tLabelH = lLabelW = rLabelW = 0;
  $textModifierFn = boxTextModifierFn;
  boxPrims = MapIndex1[$keyOff = 0; parseBox, boxes];

  UnpackOptions[wireColor, wireLabelPosition, wireLabelFontColor, wireLabelSpacing, wireLabelOffset, wireLabelBackground, wireLabelFontSize, wireLabelFontWeight, wireLabelFontFamily];
  SetAuto[wireColor, defaultWireColor];
  $fontColor = wireLabelFontColor;
  $currentDiagramFontSize = $fontSize = SubAuto[wireLabelFontSize, fontSize];
  $fontWeight = SubAuto[wireLabelFontWeight, fontWeight];
  $fontFamily = SubAuto[wireLabelFontFamily, fontFamily];
  labelPosition = wireLabelPosition; labelSpacing = wireLabelSpacing; labelOffset = wireLabelOffset; labelBackground = wireLabelBackground;
  (* SetAuto[curveFunction, CircuitCurve[#, SetbackDistance -> None, SplitPosition -> splitPosition, Orientation -> splitOrientation]&]; *)
  SetAuto[curveFunction, stringDiagramCurve];
  $textModifierFn = wireTextModifierFn;
  wirePrims = MapIndex1[$keyOff = 0; parseWire, wires];

  UnpackOptions[tickPosition, tickLabelFontColor, tickLabelSpacing, tickLabelFontSize, tickLabelFontWeight, tickLabelFontFamily, tickLength];
  $fontColor = tickLabelFontColor;
  $currentDiagramFontSize = $fontSize = SubAuto[tickLabelFontSize, fontSize];
  $fontWeight = SubAuto[tickLabelFontWeight, fontWeight];
  $fontFamily = SubAuto[tickLabelFontFamily, fontFamily];

  SetAuto[tickPosition, If[convention === RightToLeft, Left, Right]];
  OptionMatchMsg[TickPosition, tickPosition, Left | Right];
  labelPosition = tickPosition; labelSpacing = tickLabelSpacing;
  tickPrims = Map[parseFrameTick, frameTicks];

  UnpackOptions[regionLabelFontSize, regionLabelFontWeight, regionLabelFontFamily];
  $currentDiagramFontSize = $fontSize = SubAuto[regionLabelFontSize, fontSize];
  $fontWeight = SubAuto[regionLabelFontWeight, fontWeight];
  $fontFamily = SubAuto[regionLabelFontFamily, fontFamily];
  labelPosition = Center; labelSpacing = 10;
  $textModifierFn = regionTextModifierFn;
  regPrims = Map[parseReg, regions];

  noneFrame = If[$fillRegions =!= {}, GrayLevel[0.99], None];
  SetAuto[frameColor, noneFrame];
  SetAuto[frameThickness, baseThickness];

  rect = Rectangle[Offset[{0, -1}, {-$w, -$h}], Offset[{0, 0}, {$w, $h}]];

  framePrims = If[halfFrame,
    nw = {-$w,  $h};  ne = {$w,  $h};
    sw = {-$w, -$h}; se = {$w, -$h};
    {sw, se} //= Map[Offset[{0, -1}, #]&];
    halfFrameLine = Line @ If[convention === RightToLeft, {ne, nw, sw, se}, {nw, ne, se, sw}];
    Style[halfFrameLine, frameColor, AbsoluteThickness @ frameThickness]
  ,
    Style[rect, FaceForm @ None, EdgeForm @ {frameColor, AbsoluteThickness @ frameThickness}]
  ];
  If[frameColor === None, framePrims = Nothing];

  background = If[backgroundColor === None, Nothing,
    Style[rect, FaceForm @ toRegionColor @ backgroundColor, EdgeForm @ None]
  ];

  SetAuto[imagePadding, 2];

  imagePadding //= StandardizePadding;
  {{padl, padr}, {padb, padt}} = imagePadding;
  If[Max[tLabelH, bLabelH] > 0,
    padb = Max[padb, bLabelH + 2, 22];
    padt = Max[padt, tLabelH + 2, 22];
  ];
  If[lLabelW > 0, padl = Max[padl, lLabelW + 2, 25]];
  If[rLabelW > 0, padr = Max[padr, rLabelW + 2, 25]];
  imagePadding = {{padl, padr}, {padb, padt}};

  totalPadding = Map[Total, imagePadding];
  If[NumericQ[imageSize], imageSize *= {1, $h/$w}];
  origHeight = L @ imageSize;
  imageSize += totalPadding;
  {imageWidth, imageHeight} = imageSize;
  bottomPadding = Part[imagePadding, 2, 1];
  baselinePosition = Scaled[(bottomPadding + origHeight/2 - 8) / imageHeight];

  inlineOptions = SeqDropOptions[{Background, ColorRules}][opts];
  graphics = Graphics[
    {AbsolutePointSize[5 + nodeEdgeThickness],
     inlineOptions, background,
     {AbsoluteThickness[wireThickness], wirePrims}, regPrims,
     framePrims, Annotation[tickPrims, "TransparentToFill"], boxPrims},
    If[epilog =!= {}, Epilog -> epilog, Seq[]],
    ImageSize -> imageSize,
    PlotRange -> {{-$w, $w}, {-$h, $h}},
    PlotRangeClipping -> False,
    PlotRangePadding -> 0,
    ImagePadding -> imagePadding,
    BaselinePosition -> baselinePosition
  ];

  If[$fillRegions =!= {},
    graphics = FloodFill[(graphics /. $fillPreprocessingRules) -> graphics, $fillRegions];
    graphics = Image[graphics, ImageSize -> ImageDimensions[graphics]/2];
  ];

  graphics
];

$fillPreprocessingRules = {
  _Text :> {},
  Annotation[_, "TransparentToFill"] :> {}
}

(**************************************************************************************************)

stringDiagramCurve[pos_, setback_] :=
  NeatCurve[pos,
    JoinStyle -> Map[toJS, pos], Setback -> setback, BendRadius -> 2,
    SegmentPosition -> segmentPosition
  ];

toJS[{x_, y_}] := Which[
  Abs[x] >= $w, Horizontal,
  Abs[y] >= $h, Vertical,
  True, Horizontal
];

(**************************************************************************************************)

doFlip[pos_] := ApplyFlip[pos, {flipX, flipY}];

(**************************************************************************************************)

parseFrameTick = Case[
  pos:$NumberP -> label_ := Scope[
    xs = If[tickPosition === Left, 1, -1];
    p1 = {-$w * xs, pos};
    p0 = Offset[{-tickLength * xs, 0}, p1];
    p2 = {$w * xs, pos};
    {makeLabel[$colorModifierFn @ label, {p0}], Line[{p0, p1}], Opacity[0.1], Line[{p1, p2}]}
  ];
  Interval[{pos1_, pos2_}] -> label_ := Scope[
    foo; (* TODO: figure out what this was supposed to be *)
  ];
  other_ := OptionMsg[FrameTicks, other];
]

(**************************************************************************************************)

parseBox = Case[
  Seq[pos_, key_] := %[pos -> None, key];

  Seq[c_Customized, key_] :=
    customizedBlock[c, $wireCustomizations, %[#, key]&];

  Seq[rule:(opt:(_Symbol | _Str) -> value_) /; MatchQ[opt, $boxCustomizationKeyP], key_] := (
    $keyOff += 1;
    setGlobalsFromRules[rule, $boxCustomizations];
    Nothing
  );

  Seq[pos_ -> label_, key_] := Scope[
    $pos = RepAll[maybe[p_] :> p] @ toPos @ pos;
    ind = key - $keyOff;
    AssociateTo[$boxAliases, ind -> $pos];
    $lastInnerLabel = None;
    {res, size} = makeBox @ label;
    AssociateTo[$boxSizes, ind -> size];
    If[Quiet @ StrQ[label = FormToPlainString[$lastInnerLabel]],
      AssociateTo[$boxAliases, label -> $pos]];
      AssociateTo[$boxSizes, label -> size];
    res
  ];
];

(**************************************************************************************************)

$boxCustomizations = RuleDelayed[
  {NodeFrameColor | FrameColor, NodeLabelFontColor | FontColor, NodeLabelFontWeight | FontWeight,  FontSize, LabelSpacing, NodeLabelOffset | LabelOffset, NodeLabelPosition | LabelPosition, LabelBackground, NodeEdgeThickness, NodeBackground | Background, NodeSize},
  {nodeFrameColor,                                  $fontColor,                      $fontWeight, $fontSize, labelSpacing,                   labelOffset,                     labelPosition, labelBackground, nodeEdgeThickness, nodeBackground,              nodeSize}
];

$boxCustomizationKeyP = Flatten[Alt @@ (F @ $boxCustomizations)];

fsToRel[sz_] := sz / graphicsScale;

makeDiskBox[style_] := {
  Style[Disk[$pos, 0.5], style, EdgeForm[AbsoluteThickness[nodeEdgeThickness]]],
  0.5
};

makeBox = Case[

  "WhiteDisk" := makeDiskBox @ FaceEdgeForm[White, Black];
  "BlackDisk" := makeDiskBox @ FaceEdgeForm[Black];

  (h:"Disk"|NodeDisk|"Box"|NodeBox)[label_, r_:Auto, opts___Rule] := Scope[
    SetAuto[r, nodeSize];
    label //= $colorModifierFn;
    labelColor = extractColorFromLabel @ label;
    fc = Lookup[{opts}, FrameColor, If[ColorQ @ labelColor, OklabDarker[labelColor], SubNone[boxColor, SubNone[nodeFrameColor, defaultWireColor]]]];
    ef = {AbsoluteThickness[nodeEdgeThickness], fc};
    ff = Lookup[{opts}, Background, If[ColorQ @ labelColor, Lighter[labelColor, 0.9], nodeBackground]];
    isDisk = h ~~~ "Disk" | NodeDisk;
    res = List[
      Style[
        If[isDisk,
          Disk[$pos, fsToRel[r]/2],
          CenteredRectangle[$pos, fsToRel @ r, FilterOptions @ opts, RoundingRadius -> fsToRel[5]]
        ],
        FaceForm[ff], EdgeForm[ef]
      ],
      makeLabel[label, List @ $pos]
    ];
    size = If[isDisk, fsToRel[r]/2, Rectangular[fsToRel[r]*{1, 1}/2]];
    {res, size}
  ];

  c_Customized := customizedBlock[c, $boxCustomizations, %];

  ("Point"|Point)[label_]  := Scope[
    labelPosition = doFlip @ Which[F[$pos] <= $w, Right, F[$pos] >= $w, Left, True, Above];
    label //= $colorModifierFn;
    labelColor = extractColorFromLabel @ label;
    res = List[
      Style[Point[$pos], If[ColorQ @ labelColor, OklabDarker @ labelColor, boxColor]],
      makeLabel[label, List @ $pos]
    ];
    {res, 0}
  ];

  None := {Point[$pos], 0};

  label_ := %[$nodeShape[label]];
];


(**************************************************************************************************)

StringDiagram::badwire = "Wire specification `` is invalid."

$wireCustomizations = RuleDelayed[
  {CurveFunction, WireColor, SegmentPosition, SplitPosition, "SplitOrientation", LabelPosition, LabelSpacing, LabelOffset, LabelBackground, FontColor,   FontWeight,  FontSize},
  {curveFunction, wireColor, segmentPosition, splitPosition,  splitOrientation,  labelPosition, labelSpacing, labelOffset, labelBackground, $fontColor, $fontWeight, $fontSize}
];

parseWire = Case[
  Seq[ue:UndirectedEdge[_, UndirectedEdge[_, _UndirectedEdge]], key_] :=
    %[ue -> {None, None, None}];

  Seq[ue:UndirectedEdge[_, UndirectedEdge[_, _]], key_] :=
    %[ue -> {None, None}, key];

  Seq[ue_UndirectedEdge, key_] :=
    %[ue -> None, key];

  Seq[UndirectedEdge[a_, UndirectedEdge[b_, c_]] -> {lbl1_, lbl2_}, key_] := Splice[{
    %[UndirectedEdge[a, b] -> lbl1, key],
    %[UndirectedEdge[b, c] -> lbl2, $keyOff -= 1; key]
  }];

  Seq[UndirectedEdge[a_, UndirectedEdge[b_, UndirectedEdge[c_, d_]]] -> {lbl1_, lbl2_, lbl3_}, key_] := Splice[{
    %[UndirectedEdge[a, b] -> lbl1, key],
    %[UndirectedEdge[b, c] -> lbl2, $keyOff -= 1; key],
    %[UndirectedEdge[c, d] -> lbl3, $keyOff -= 1; key]
  }];

  (* this 'outer' form of coloring is maybe not all that useful? will color the wire but not the label *)
  Seq[(cf:$colorFormP)[spec_], key_] := Scope[
    wireColor = StyleFormData @ cf;
    %[spec, key]
  ];

  Seq[c_Customized, key_] :=
    customizedBlock[c, $wireCustomizations, %[#, key]&];

  Seq[r:Rule[_Str | _Symbol, _], key_] := (
    $keyOff += 1;
    setGlobalsFromRules[r, $wireCustomizations];
    Nothing
  );

  Seq[UndirectedEdge[a_, b_] -> label_, key_] := Scope[
    label //= $colorModifierFn;
    sc = extractColorFromLabel @ label;
    SetNone[sc, wireColor];
    pos = toPos /@ {a, b};
    x = F[Decases[_maybe | -_maybe] @ Col1 @ pos, None];
    pos = pos /. maybe[0] :> SubNone[x, 0];
    size = toSize /@ {a, b};
    curve = Rep[curveFunction, Line -> (SeqFirst /* Line)][pos, size];
    points = DiscretizeCurve @ curve;
    AssociateTo[$wireAliases, (key - $keyOff) -> points];
    StyleOperator[sc] @ {curve, makeLabel[label, points]}
  ];

  Seq[spec_, key_] := (Message[StringDiagram::badwire, spec]; {})
];

(**************************************************************************************************)

extractColorFromLabel = Case[
  Customized[c_, ___]              := % @ c;
  (h:$colorFormP)[_]               := StyleFormData @ h;
  GradientSymbol[_, cspec_, ___]   := OklabDarker @ HumanBlend[getGradColors @ cspec];
  e_                               := findInteriorColor[e];
];

getGradColors = Case[
  c:{_, _}               := c;
  c:{_, _} -> _          := c;
  ColorGradient[c_, ___] := c;
];

(**************************************************************************************************)

StringDiagram::nobox = "Could not find box location for ``, available names include ``, using center coordinate."
toPos = Case[
  Center                   := {0, 0};
  BottomRight              := doFlip @ {$w, -$h};
  TopRight                 := doFlip @ {$w, $h};
  side:Top|Bottom          := %[{side, maybe[0]}];
  side:Left|Right          := %[{side, 0}];
  {Bottom, i_}             := doFlip @ {i, -$h};
  {Top, i_}                := doFlip @ {i, $h};
  {Right, i_}              := doFlip @ {$w, i};
  {Left, i_}               := doFlip @ {-$w, i};
  key:(_Int | _Str)        := Lookup[$boxAliases, key, Message[StringDiagram::nobox, key, Keys @ $boxAliases]; {0, 0}];
  Translate[pos_, off_]    := doFlip[off] + % @ pos;
  other_                   := doFlip @ other;
]

toSize = Case[
  key:(_Int | _Str)        := Lookup[$boxSizes, key];
  _                        := 0;
]

(**************************************************************************************************)

StringDiagram::badregion = "Region specification `` is invalid."

(* confusing where flip is applied here! *)
parseReg = Case[

  UndirectedEdge[a_, b_] -> label_ := Scope[
    wirePos = toRegPos /@ {a, b};
    meanPos = Mean[DeleteNone[#]]& /@ Transpose[wirePos];
    %[meanPos -> label]
  ];

  pos_ -> Placed[c:$colorOrInt, side:($sideP | $Coord2P)] :=
    %[(pos + Lookup[$SideToCoords, Key @ side, side]) -> c];

  pos_List -> c:$colorOrInt :=
    (addFill[pos, c]; Nothing);

  pos_List -> Placed[label_, side_] := Scope[
    label //= $colorModifierFn;
    off = Lookup[$SideToCoords, Key @ side, side];
    labelColor = extractColorFromLabel @ label;
    If[ColorQ @ labelColor, Switch[regionFilling,
      "Explicit", Null,
      "Labeled", addFill[pos + off, labelColor],
      "Unlabeled", addFill[pos + off, labelColor]; Return @ Nothing;
    ]];
    makeLabel[Placed[label, side], {pos}]
  ];

  pos_List -> label_ :=
    %[doFlip[pos] -> Placed[label, Center]];

  side:$sideP -> label_ :=
    With[{coords = $SideToCoords[side]},
      %[doFlip[{$w, $h} * coords] -> Placed[label, doFlip @ Rep[side, $FlipSideRules]]]
    ];

  i_Int -> label_ :=
    %[doFlip[{i, -$h}] -> Placed[label, doFlip @ Above]];

  spec_ :=
    (Message[StringDiagram::badregion, spec]; Nothing)

,
  {$sideP -> ($SidePattern|Above|Below|Center)}
]

(**************************************************************************************************)

addFill[pos_, color_] := AppTo[$fillRegions, pos -> toRegionColor[color]];

toRegionColor[c_] := OklabLighter[ToRainbowColor @ c, .4];

(**************************************************************************************************)

StringDiagram::badregionpos = "Region position specification `` is invalid. Should be wire name or box name."

toRegPos = Case[
  Left    := doFlip @ {-$w, None};
  Right   := doFlip @ { $w, None};
  Top     := doFlip @ {None,  $h};
  Bottom  := doFlip @ {None, -$h};
  side:BottomLeft|BottomRight|TopLeft|TopRight := doFlip[{$w, $h} * Lookup[$SideToCoords, side]];
  k_ /; KeyQ[$wireAliases, k] := Mean @ $wireAliases @ k;
  k_ /; KeyQ[$boxAliases, k] := $boxAliases @ k;
  k_      := (Message[StringDiagram::badregionpos, k]; {0, 0});
];

(**************************************************************************************************)

$labelCustomizations = RuleDelayed[
  {LabelPosition, LabelSpacing, LabelOffset, LabelBackground, FontColor,   FontWeight,  FontSize,  FontFamily},
  {labelPosition, labelSpacing, labelOffset, labelBackground, $fontColor, $fontWeight, $fontSize, $fontFamily}
];

makeLabel[c_Customized, pos_] :=
  customizedBlock[c,$labelCustomizations, makeLabel[#, pos]&];

makeLabel[Placed[label_, side_], pos_] :=
  Scope[labelPosition = side; makeLabel[label, pos]];

makeLabel[label_, pos_] /; labelPosition === Vertical :=
  Scope[
    {miny, maxy} = MinMax @ Col2[pos];
    labelPosition = {If[miny < -$h+0.5, Bottom, Nothing], If[maxy > $h-0.5, Top, Nothing]};
    If[labelPosition === {}, labelPosition = Right];
    makeLabel[label, pos]
  ];

makeLabel[None, _] := {};

makeLabel[label_, pos_] /; labelPosition === Bottom :=
  Scope[labelPosition = Below; makeLabel[label, MinimalBy[pos, L]]];

makeLabel[label_, pos_] /; labelPosition === Top :=
  Scope[labelPosition = Above; makeLabel[label, MaximalBy[pos, L]]];

makeLabel[label_, pos_] /; MatchQ[labelPosition, {__Symbol}] := Block[
  {posList = labelPosition, labelPosition = None},
  makeLabel[labelPosition = #; label, pos]& /@ posList
];

makeLabel[label_, pos_] := With[
  {pos2 = Mean @ pos, labelPos = RemoveOffsets @ labelPosition},
  addLabelPadding @ CenterTextVertical @ Text[
    $lastInnerLabel = label;
    $textModifierFn @ label
  ,
    SimplifyOffsets @ Offset[
      Plus[
        labelSpacing * Rep[labelPosition, $SideToCoords],
        If[MatchQ[labelPosition, _Offset], F @ labelPosition, 0],
        labelOffset
      ],
      pos2
    ]
  ,
    If[ListQ[labelPos], labelPos, -Lookup[$SideToCoords, labelPosition]]
  ,
    Background -> labelBackground,
    BaseStyle -> {
      FontWeight -> $fontWeight,
      FontSize -> $fontSize,
      FontColor -> $fontColor,
      FontFamily -> $fontFamily
    }
  ]
];

addLabelPadding[text:Text[_, pos_, off_, ___]] := Module[{x, y},
  {{x, y}, {ox, oy}} = FromOffsetCoord @ pos;
  Switch[labelPosition,
    Right /; (x >=  $w), rLabelW = Max[rLabelW, ox + F @ MakeTextImageSize @ text],
    Left  /; (x <= -$w), lLabelW = Max[lLabelW, -ox + F @ MakeTextImageSize @ text],
    Above /; (y >=  $h), tLabelH = Max[tLabelH, oy + P2 @ MakeTextImageSize @ text],
    Below /; (y <= -$h), bLabelH = Max[bLabelH, -oy + P2 @ MakeTextImageSize @ text],
    True, Null
  ];
  text
];

_addlabelPadding := BadArguments[];

(**************************************************************************************************)

PublicFunction[FunctorialStringDiagram]

PublicOption[Convention]
PublicSymbol[RightToLeft, LeftToRight, TopToBottom, BottomToTop]

Options[FunctorialStringDiagram] = Options[StringDiagram];

FunctorialStringDiagram[boxes_List, wires_List, rhsSpec_List, opts___Rule] :=
  FunctorialStringDiagram[boxes, wires, rhsSpec, {}, opts];

FunctorialStringDiagram[boxes_List, wires_List, rhsSpec_List, regions_List, opts___Rule] := Scope[
  r2l = LookupOption[{opts}, Convention, RightToLeft] === RightToLeft;
  $boxes = App[boxes, FontWeight -> Plain];
  $wires = App[wires, LabelPosition -> If[r2l, Right, Left]];
  $nboxes = Count[boxes, Except[$boxCustomizationKeyP -> _]];
  $rhsLen = Len[rhsSpec];
  ScanIndex1[procRhsSpec, rhsSpec];
  StringDiagram[$boxes, $wires, regions, HalfFrame -> True, opts]
];

procRhsSpec[arr_, i_] := Block[{a, b, sideArr},
  $head = Id;
  a = If[i == 1, BottomRight, $nboxes];
  b = If[i == $rhsLen, TopRight, $nboxes + 1];
  AppTo[$wires, UndirectedEdge[a, b] -> toArr[arr]];
];

toArr = Case[
  s_Str := % @ CategoryArrowSymbol[s];
  e_       := e;
];

procRhsSpec[pos_ -> obj_, i_] := Block[
  {$head = Id},
  AppTo[$boxes, {Right, pos} -> toObj[obj]];
  $nboxes++;
];

toObj = Case[
  (h_Symbol ? $styleFormHeadQ)[e_] := Block[{$head = h}, % @ e];
  "f"      := % @ Padded[CategoryObjectSymbol["f"], {0.12, 0.12}];
  s_Str := % @ CategoryObjectSymbol[s];
  e_       := "Point"[$head[e]];
]

