PublicFunction[StringDiagram]

PublicOption[WireThickness, NodeEdgeThickness, LabelOffset, DiagramSize, TextModifiers, HalfFrame, BaseThickness, CurveFunction]

PublicOption[NodeDiskSize, NodeLabelPosition, NodeLabelFontWeight, NodeLabelFontFamily, NodeLabelFontSize, NodeLabelSpacing, NodeLabelOffset, NodeLabelBackground, NodeLabelFontColor, NodeFrameColor, NodeBackground]

PublicOption[WireLabelFontSize, WireLabelFontWeight, WireLabelFontFamily, WireLabelPosition, WireLabelSpacing, WireLabelOffset, WireLabelBackground, WireLabelFontColor, WireColor]

PublicOption[TickLabelFontSize, TickLabelFontWeight, TickLabelFontFamily, TickLabelSpacing, TickLabelFontColor, TickLength]

Options[StringDiagram] = {
  ImageSize             -> Automatic,
  ImagePadding          -> Automatic,
  Epilog                -> {},

  FontSize              -> 16,
  FontWeight            -> Plain,
  FontFamily            :> $MathFont,

  BaseThickness         -> Automatic,
  WireThickness         -> Automatic,
  NodeEdgeThickness     -> Automatic,

  HalfFrame             -> False,
  FrameThickness        -> Automatic,
  FrameColor            -> Automatic,
  Background            -> None,

  FlipX                 -> False,
  FlipY                 -> False,
  DiagramSize           -> {12, 12},
  GraphicsScale         -> 5,

  TextModifiers         -> {},
  CurveFunction         -> Automatic,
  SplitPosition         -> "Start",

  NodeDiskSize          -> 22,
  NodeLabelFontSize     -> Automatic,
  NodeLabelFontWeight   -> Bold,
  NodeLabelFontFamily   -> Automatic,
  NodeLabelFontColor    -> Black,
  NodeLabelPosition     -> Center,
  NodeLabelSpacing      -> {5, 0},
  NodeLabelOffset       -> {0, 0},
  NodeLabelBackground   -> None,
  NodeFrameColor        -> None,
  NodeBackground        -> White,

  WireLabelFontSize     -> Automatic,
  WireLabelFontWeight   -> Automatic,
  WireLabelFontFamily   -> Automatic,
  WireLabelFontColor    -> Black,
  WireLabelPosition     -> Vertical,
  WireLabelSpacing      -> {5, 0},
  WireLabelOffset       -> {0, 0},
  WireLabelBackground   -> None,
  WireColor             -> Black,

  FrameTicks            -> {},
  TickLength            -> 5,
  TickLabelFontSize     -> Automatic,
  TickLabelFontWeight   -> Plain,
  TickLabelFontFamily   -> Automatic,
  TickLabelSpacing      -> 3,
  TickLabelFontColor    -> Black
}

(**************************************************************************************************)

StringDiagram[boxes_List, wires_List, opts___Rule] :=
  StringDiagram[boxes, wires, {}, opts];

StringDiagram[boxes_List, wires_List, regions_List, opts:OptionsPattern[]] := Scope[
  $boxAliases = $wireAliases = UAssociation[];
  $fillRegions = {};

  UnpackOptions[
    flipX, flipY, wireThickness, nodeEdgeThickness, nodeBackground, frameThickness, frameColor, curveFunction,
    splitPosition, background, baseThickness, halfFrame, imagePadding, epilog, fontSize, fontWeight, fontFamily,
    diagramSize, imageSize, graphicsScale, textModifiers, frameTicks
  ];

  $lastInnerLabel = None;
  $textModifierFn = Composition @@ textModifiers;
  {$w, $h} = diagramSize;
  SetAutomatic[imageSize, 2 * graphicsScale * diagramSize];

  isColorRegion = MemberQ[regions, _ -> $ColorPattern];
  SetAutomatic[baseThickness, 2];
  SetAutomatic[wireThickness, baseThickness];
  SetAutomatic[nodeEdgeThickness, baseThickness];
  noneFrame = If[isColorRegion, GrayLevel[0.99], None];
  SetAutomatic[frameColor, noneFrame];
  SetAutomatic[frameThickness, baseThickness];

  UnpackOptions[nodeFrameColor, nodeDiskSize, nodeLabelFontColor, nodeLabelPosition, nodeLabelSpacing, nodeLabelOffset, nodeLabelBackground, nodeLabelFontSize, nodeLabelFontWeight, nodeLabelFontFamily];
  $fontColor = nodeLabelFontColor;
  $fontSize = ReplaceAutomatic[nodeLabelFontSize, fontSize];
  $fontWeight = ReplaceAutomatic[nodeLabelFontWeight, fontWeight];
  $fontFamily = ReplaceAutomatic[nodeLabelFontFamily, fontFamily];
  labelSpacing = nodeLabelSpacing; labelPosition = nodeLabelPosition; labelOffset = nodeLabelOffset; labelBackground = nodeLabelBackground;
  boxColor = None; hasBottomLabels = hasTopLabels = hasLeftLabels = hasRightLabels = False;
  $vcenter = False;
  boxPrims = MapIndex1[$keyOff = 0; parseBox, boxes];

  UnpackOptions[wireColor, wireLabelPosition, wireLabelFontColor, wireLabelSpacing, wireLabelOffset, wireLabelBackground, wireLabelFontSize, wireLabelFontWeight, wireLabelFontFamily];
  $fontColor = wireLabelFontColor;
  $fontSize = ReplaceAutomatic[wireLabelFontSize, fontSize];
  $fontWeight = ReplaceAutomatic[wireLabelFontWeight, fontWeight];
  $fontFamily = ReplaceAutomatic[wireLabelFontFamily, fontFamily];
  labelPosition = wireLabelPosition; labelSpacing = wireLabelSpacing; labelOffset = wireLabelOffset; labelBackground = wireLabelBackground;
  SetAutomatic[curveFunction, CircuitCurve[#, SetbackDistance -> None, SplitPosition -> splitPosition]&];
  wirePrims = MapIndex1[$keyOff = 0; parseWire, wires];

  UnpackOptions[tickLabelFontColor, tickLabelSpacing, tickLabelFontSize, tickLabelFontWeight, tickLabelFontFamily, tickLength];
  $fontColor = tickLabelFontColor;
  $fontSize = ReplaceAutomatic[tickLabelFontSize, fontSize];
  $fontWeight = ReplaceAutomatic[tickLabelFontWeight, fontWeight];
  $fontFamily = ReplaceAutomatic[tickLabelFontFamily, fontFamily];
  $vcenter = True;
  labelPosition = Left; labelSpacing = tickLabelSpacing;
  tickPrims = Map[parseFrameTick, frameTicks];

  labelPosition = Center; labelSpacing = 5;
  regPrims = Map[parseReg, regions];

  rect = Rectangle[Offset[{0, -1}, {-$w, -$h}], Offset[{0, 0}, {$w, $h}]];

  framePrims = If[halfFrame,
    nw =                 {-$w,  $h};  ne =                 {$w,  $h};
    sw = Offset[{0, -1}, {-$w, -$h}]; se = Offset[{0, -1}, {$w, -$h}];
    Style[Line[{ne, nw, sw, se}], frameColor, AbsoluteThickness @ frameThickness]
  ,
    Style[rect, FaceForm @ None, EdgeForm @ {frameColor, AbsoluteThickness @ frameThickness}]
  ];
  If[frameColor === None, framePrims = Nothing];

  background = If[background === None, Nothing,
    Style[rect, FaceForm @ Replace[background, $toRegColor], EdgeForm @ None]
  ];

  hasVerticalLabels = hasBottomLabels || hasTopLabels;
  SetAutomatic[imagePadding, 2];

  imagePadding //= StandardizePadding;
  {{padl, padr}, {padb, padt}} = imagePadding;
  If[hasVerticalLabels, padb = Max[padb, 22]; padt = Max[padt, 22]];
  If[hasLeftLabels, padl = Max[padl, 25]];
  If[hasRightLabels, padr = Max[padr, 25]];
  imagePadding = {{padl, padr}, {padb, padt}};

  totalPadding = Map[Total, imagePadding];
  If[NumericQ[imageSize], imageSize *= {1, $h/$w}];
  origHeight = Last @ imageSize;
  imageSize += totalPadding;
  {imageWidth, imageHeight} = imageSize;
  bottomPadding = Part[imagePadding, 2, 1];
  baselinePosition = Scaled[(bottomPadding + origHeight/2 - 8) / imageHeight];

  graphics = Graphics[
    {AbsolutePointSize[5 + nodeEdgeThickness],
     Sequence @@ DeleteOptions[List @ opts, Background],
     background,
     {AbsoluteThickness[wireThickness], wirePrims}, regPrims,
     framePrims, boxPrims, tickPrims},
    If[epilog =!= {}, Epilog -> epilog, Seq[]],
    ImageSize -> imageSize,
    PlotRange -> {{-$w, $w}, {-$h, $h}},
    PlotRangeClipping -> False,
    PlotRangePadding -> 0,
    ImagePadding -> imagePadding,
    BaselinePosition -> baselinePosition
  ];

  If[$fillRegions =!= {},
    graphics = FloodFill[(graphics /. t_Text :> {}) -> graphics, $fillRegions];
    graphics = Image[graphics, ImageSize -> ImageDimensions[graphics]/2];
  ];

  graphics
];

(**************************************************************************************************)

doFlip[pos_] := ApplyFlip[pos, {flipX, flipY}];

(**************************************************************************************************)

parseFrameTick = Case[
  pos:$NumberP -> label_ := Scope[
    p1 = {-$w, pos}; p0 = Offset[{-tickLength, 0}, p1];
    {makeLabel[label, {p0}], Line[{p0, p1}]}
  ];
  Interval[{pos1_, pos2_}] -> label_ := Scope[
    foo;
  ]
]

(**************************************************************************************************)

parseBox = Case[
  Seq[pos_, key_] := %[pos -> None, key];

  Seq[c_Customized, key_] :=
    customizedBlock[c, $wireCustomizations, %[#, key]&];

  Seq[rule:(opt:(_Symbol | String) -> value_) /; MatchQ[opt, $boxCustomizationKeyP], key_] := (
    $keyOff += 1;
    setGlobalsFromRules[rule, $boxCustomizations];
    Nothing
  );

  Seq[pos_ -> lbl_, key_] := Scope[
    $pos = ReplaceAll[maybe[p_] :> p] @ toPos @ pos;
    AssociateTo[$boxAliases, (key - $keyOff) -> $pos];
    $lastInnerLabel = None;
    res = makeBox @ lbl;
    If[Quiet @ StringQ[lbl = FormToPlainString[$lastInnerLabel]],
      AssociateTo[$boxAliases, lbl -> $pos]];
    res
  ];
];

(**************************************************************************************************)

$boxCustomizations = RuleDelayed[
  {NodeFrameColor | FrameColor, NodeLabelFontColor | FontColor, NodeLabelFontWeight | FontWeight,  FontSize, LabelSpacing, NodeLabelOffset | LabelOffset, NodeLabelPosition | LabelPosition, LabelBackground, NodeEdgeThickness, NodeBackground | Background, NodeDiskSize},
  {nodeFrameColor,                                  $fontColor,                      $fontWeight, $fontSize, labelSpacing,                   labelOffset,                     labelPosition, labelBackground, nodeEdgeThickness, nodeBackground,              nodeDiskSize}
];

$boxCustomizationKeyP = Flatten[Alternatives @@ (First @ $boxCustomizations)];

fsToRel[sz_] := sz / graphicsScale;

makeBox = Case[
  (cf:$colorFormP)[spec_] := Scope[
    boxColor = StyleFormData @ cf;
    nodeFrameColor = OklabDarker[boxColor];
    nodeBackground = OklabLighter[boxColor, .45];
    % @ spec
  ];

  "WhiteDisk" := Style[Disk[$pos, 1/2], FaceEdgeForm[White, Black], EdgeForm[AbsoluteThickness[nodeEdgeThickness]]];
  "BlackDisk" := Style[Disk[$pos, 1/2], FaceEdgeForm[Black], EdgeForm[AbsoluteThickness[nodeEdgeThickness]]];

  ("Disk"|NodeDisk)[lbl_, r_:Automatic, opts___Rule] := Scope[
    SetAutomatic[r, nodeDiskSize];
    $vcenter = True;
    {
      Style[Disk[$pos, fsToRel[r]/2], Seq @@ makeBoxEdgeForm[opts]],
      makeLabel[lbl, List @ $pos]
    }
  ];

  ("Box"|NodeBox)[lbl_, r:Except[_Rule]:Automatic, opts___Rule] := Scope[
    SetAutomatic[r, nodeDiskSize];
    $vcenter = True;
    {
      Style[CenteredRectangle[$pos, fsToRel @ r, FilterOptions @ opts, RoundingRadius -> fsToRel[1]], Seq @@ makeBoxEdgeForm[opts]],
      makeLabel[lbl, List @ $pos]
    }
  ];

  c_Customized := customizedBlock[c, $boxCustomizations, %];

  ("Point"|Point)[lbl_]  := Block[
    {labelPosition = Which[First[$pos] <= $w, Right, First[$pos] >= $w, Left, True, Above]},
    {Style[Point[$pos], boxColor], makeLabel[lbl, List @ $pos]}
  ];

  None := Point[$pos];

  lbl_ := %["Disk"[lbl]];
];

(**************************************************************************************************)

makeBoxEdgeForm[opts___Rule] := Scope[
  fc = Lookup[{opts}, FrameColor, ReplaceNone[boxColor, ReplaceNone[nodeFrameColor, Black]]];
  ef = {AbsoluteThickness[nodeEdgeThickness], fc};
  ff = Lookup[{opts}, Background, nodeBackground];
  List[FaceForm[ff] /. $toRegColor, EdgeForm[ef] /. $toRegColor]
]

(**************************************************************************************************)

StringDiagram::badwire = "Wire specification `` is invalid."

$wireCustomizations = RuleDelayed[
  {CurveFunction, WireColor, SplitPosition, LabelPosition, LabelSpacing, LabelOffset, LabelBackground, FontColor,   FontWeight,  FontSize},
  {curveFunction, wireColor, splitPosition, labelPosition, labelSpacing, labelOffset, labelBackground, $fontColor, $fontWeight, $fontSize}
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

  Seq[(cf:$colorFormP)[spec_], key_] := Scope[
    wireColor = StyleFormData @ cf;
    %[spec, key]
  ];

  Seq[c_Customized, key_] :=
    customizedBlock[c, $wireCustomizations, %[#, key]&];

  Seq[r:Rule[_String | _Symbol, _], key_] := (
    $keyOff += 1;
    setGlobalsFromRules[r, $wireCustomizations];
    Nothing
  );

  Seq[UndirectedEdge[a_, b_] -> lbl_, key_] := Scope[
    sc = wireColor;
    If[MatchQ[lbl, $colorFormP[_]], sc = StyleFormData @ Head @ lbl];
    pos = toPos /@ {a, b};
    x = First[DeleteCases[_maybe] @ pos[[All, 1]], None];
    pos = pos /. maybe[0] :> ReplaceNone[x, 0];
    curve = curveFunction[pos];
    points = DiscretizeCurve @ curve;
    AssociateTo[$wireAliases, (key - $keyOff) -> points];
    StyleOperator[sc] @ {curve, makeLabel[lbl, points]}
  ];

  Seq[spec_, key_] := (Message[StringDiagram::badwire, spec]; {})
]

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
  key:(_Integer | _String) := Lookup[$boxAliases, key, Message[StringDiagram::nobox, key, Keys @ $boxAliases]; {0, 0}];
  Translate[pos_, off_]    := doFlip[off] + % @ pos;
  other_                   := doFlip @ other;
]

(**************************************************************************************************)

StringDiagram::badregion = "Region specification `` is invalid."

parseReg = Case[
  UndirectedEdge[a_, b_] -> lbl_ := Scope[
    wirePos = toRegPos /@ {a, b};
    meanPos = Mean[DeleteNone[#]]& /@ Transpose[wirePos];
    %[meanPos -> lbl]
  ];

  pos_ -> Placed[c:$ColorPattern, side:($sideP | $Coord2P)] :=
    %[(pos + Lookup[$SideToCoords, Key @ side, side]) -> c];

  pos_List -> c:$ColorPattern :=
    (AppendTo[$fillRegions, pos -> Replace[c, $toRegColor]]; Nothing);

  pos_List -> p_Placed :=
    makeLabel[p, {pos}];

  pos_List -> lbl_ :=
    makeLabel[Placed[lbl, Center], {pos}];

  side:$sideP -> lbl_ :=
    With[{coords = $SideToCoords[side]},
      %[doFlip[{$w, $h} * coords] -> Placed[lbl, doFlip @ Replace[side, $FlipSideRules]]]
    ];

  i_Integer -> lbl_ :=
    %[doFlip[{i, -$h}] -> Placed[lbl, doFlip @ Above]];

  spec_ :=
    (Message[StringDiagram::badregion, spec]; Nothing)

,
  {$sideP -> ($SidePattern|Above|Below|Center)}
]

$toRegColor = {
  Red    -> OklabLighter[$LightRed, .1],
  Blue   -> OklabLighter[$LightBlue, .1],
  Green  -> OklabLighter[$LightGreen, .1],
  Orange -> OklabLighter[$LightOrange, .1],
  Purple -> OklabLighter[$LightPurple, .1],
  Pink   -> OklabLighter[$LightPink, .1],
  Cyan   -> OklabLighter[$LightTeal, .1],
  Gray   -> OklabLighter[$LightGray, .1]
};

(**************************************************************************************************)

StringDiagram::badregionpos = "Region position specification `` is invalid. Should be wire name or box name."

toRegPos = Case[
  Left    := doFlip @ {-$w, None};
  Right   := doFlip @ { $w, None};
  Top     := doFlip @ {None,  $h};
  Bottom  := doFlip @ {None, -$h};
  side:BottomLeft|BottomRight|TopLeft|TopRight := doFlip[{$w, $h} * Lookup[$SideToCoords, side]];
  k_ /; KeyExistsQ[$wireAliases, k] := Mean @ $wireAliases @ k;
  k_ /; KeyExistsQ[$boxAliases, k] := $boxAliases @ k;
  k_      := (Message[StringDiagram::badregionpos, k]; {0, 0});
];

(**************************************************************************************************)

$labelCustomizations = RuleDelayed[
  {LabelPosition, LabelSpacing, LabelOffset, LabelBackground, FontColor,   FontWeight,  FontSize,  FontFamily},
  {labelPosition, labelSpacing, labelOffset, labelBackground, $fontColor, $fontWeight, $fontSize, $fontFamily}
];

makeLabel[c_Customized, pos_] :=
  customizedBlock[c,$labelCustomizations, makeLabel[#, pos]&];

makeLabel[Placed[lbl_, side_], pos_] :=
  Scope[labelPosition = side; makeLabel[lbl, pos]];

makeLabel[lbl_, pos_] /; labelPosition === Vertical :=
  Scope[
    {miny, maxy} = MinMax @ Part[pos, All, 2];
    labelPosition = {If[miny < -$h+0.5, Bottom, Nothing], If[maxy > $h-0.5, Top, Nothing]};
    If[labelPosition === {}, labelPosition = Right];
    makeLabel[lbl, pos]
  ];

makeLabel[None, _] := {};

makeLabel[lbl_, pos_] /; labelPosition === Bottom :=
  Scope[labelPosition = Below; makeLabel[lbl, MinimalBy[pos, Last]]];

makeLabel[lbl_, pos_] /; labelPosition === Top :=
  Scope[labelPosition = Above; makeLabel[lbl, MaximalBy[pos, Last]]];

makeLabel[lbl_, pos_] /; MatchQ[labelPosition, {__Symbol}] := Block[
  {posList = labelPosition, labelPosition = None},
  makeLabel[labelPosition = #; lbl, pos]& /@ posList
];

makeLabel[lbl_, pos_] := With[{pos2 = Mean @ pos}, applyVCenter @ Text[
  $lastInnerLabel = lbl;
  $textModifierFn @ lbl,
  SimplifyOffsets @ Offset[
    Plus[
      labelSpacing * Replace[labelPosition, $SideToCoords],
      If[MatchQ[labelPosition, _Offset], First @ labelPosition, 0],
      labelOffset
    ],
    pos2
  ],
  With[{labelPos = RemoveOffsets @ labelPosition, pos3 = RemoveOffsets @ pos2},
    Switch[labelPosition,
      Right /; (First[pos3] >=  $w), hasRightLabels = True,
      Left  /; (First[pos3] <= -$w), hasLeftLabels = True,
      Above /; (Last[pos3]  >=  $h), hasTopLabels = True,
      Below /; (Last[pos3]  <= -$h), hasBottomLabels = True,
      True, Null
    ];
    If[ListQ[labelPos], labelPos, -Lookup[$SideToCoords, labelPosition]]
  ],
  Background -> labelBackground,
  BaseStyle -> {FontWeight -> $fontWeight, FontSize -> $fontSize, FontColor -> $fontColor, FontFamily -> $fontFamily}
]];

applyVCenter[txt:Text[lbl_, Offset[off_, pos_], args___]] /; TrueQ[$vcenter] := Scope[
  img = TextRasterize[txt];
  {w, h, bl} = TextRasterSize[txt, True];
  rows = Mean /@ ImageData[Binarize[img]];
  thresh = Lerp[Min @ rows, Max @ rows, 0.7];
  delta1 = SelectFirstIndex[rows, # < thresh&] - 1;
  delta2 = Length[rows] - SelectFirstIndex[Reverse @ rows, # < thresh&];
  delta = -h/2 + Avg[delta1, delta2]/2;
  pos2 = Offset[off + {0, delta}, pos];
  text = Text[lbl, pos2, args];
  text
];

applyVCenter[text_] := text;

(**************************************************************************************************)

PublicFunction[FunctorialStringDiagram]

FunctorialStringDiagram[boxes_List, wires_List, rhsSpec_List, opts___Rule] :=
  FunctorialStringDiagram[boxes, wires, rhsSpec, {}, opts];

FunctorialStringDiagram[boxes_List, wires_List, rhsSpec_List, regions_List, opts___Rule] := Scope[
  $boxes = Append[boxes, Splice[{LabelPosition -> Right, FontWeight -> Plain}]];
  $wires = Append[wires, LabelPosition -> Right];
  $nboxes = Count[boxes, Except[$boxCustomizationKeyP -> _]];
  $rhsLen = Length[rhsSpec];
  ScanIndex1[procRhsSpec, rhsSpec];
  StringDiagram[$boxes, $wires, regions, HalfFrame -> True, opts]
];

procRhsSpec[arr_, i_] := Block[{a, b, sideArr},
  $head = Identity;
  a = If[i == 1, BottomRight, $nboxes];
  b = If[i == $rhsLen, TopRight, $nboxes + 1];
  AppendTo[$wires, UndirectedEdge[a, b] -> toArr[arr]];
];

toArr = Case[
  s_String := % @ CategoryArrowSymbol[s];
  e_       := e;
];

procRhsSpec[pos_ -> obj_, i_] := Block[
  {$head = Identity},
  AppendTo[$boxes, {Right, pos} -> toObj[obj]];
  $nboxes++;
];

toObj = Case[
  (h_Symbol ? $styleFormHeadQ)[e_] := Block[{$head = h}, % @ e];
  "f"      := % @ Padded[CategoryObjectSymbol["f"], Left -> 0.12];
  s_String := % @ CategoryObjectSymbol[s];
  e_       := $head["Point"[$head[e]]];
]



