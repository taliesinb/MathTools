PublicFunction[StringDiagram]

PublicOption[WireThickness, NodeEdgeThickness]

Options[StringDiagram] = {
  ImageSize -> 100,
  ImagePadding -> Automatic,
  Epilog -> {},
  FontSize -> 16,
  FlipX -> False,
  FlipY -> False,
  WireThickness -> Automatic,
  NodeEdgeThickness -> Automatic,
  FrameThickness -> Automatic,
  FrameColor -> Automatic,
  Background -> None,

  "CurveFunction" -> Automatic,
  SplitPosition -> "Start",

  "HalfFrame" -> False,
  "BaseThickness" -> Automatic,
  "NodeDiskSize" -> 2.5,
  "NodeLabelPosition" -> Center,
  "NodeLabelSpacing" -> 4,
  "NodeLabelBackground" -> None,
  "NodeLabelFontColor" -> Black,
  "NodeFrameColor" -> None,
  "NodeBackground" -> White,

  "StringLabelPosition" -> Vertical,
  "StringLabelSpacing" -> 1,
  "StringLabelBackground" -> None,
  "StringLabelFontColor" -> Black,
  "StringColor" -> Black
}

(**************************************************************************************************)

StringDiagram[points_List, strings_List, opts___Rule] :=
  StringDiagram[points, strings, {}, opts];

StringDiagram[points_List, strings_List, regions_List, opts:OptionsPattern[]] := Scope[
  $pntAliases = $strAliases = UAssociation[];
  $fillRegions = {};
  $sz = 12;

  UnpackOptions[flipX, flipY, wireThickness, nodeEdgeThickness, nodeBackground, frameThickness, frameColor, curveFunction,
    splitPosition, background, baseThickness, halfFrame];

  isColorRegion = MemberQ[regions, _ -> $ColorPattern];
  SetAutomatic[baseThickness, If[isColorRegion || background =!= None, 2, 1]];
  SetAutomatic[wireThickness, baseThickness];
  SetAutomatic[nodeEdgeThickness, baseThickness];
  SetAutomatic[frameThickness, baseThickness];
  SetAutomatic[frameColor, If[isColorRegion, $DarkGray, $Gray]];

  UnpackOptions[nodeFrameColor, nodeDiskSize, nodeLabelFontColor, nodeLabelPosition, nodeLabelSpacing, nodeLabelBackground];
  fontColor = nodeLabelFontColor; labelSpacing = nodeLabelSpacing; labelPosition = nodeLabelPosition; labelBackground = nodeLabelBackground;
  pntColor = None; hasBottomLabels = hasTopLabels = False;
  pntPrims = MapIndex1[parsePnt, points];

  UnpackOptions[stringColor, stringLabelPosition, stringLabelFontColor, stringLabelSpacing, stringLabelBackground];
  fontColor = stringLabelFontColor; labelPosition = stringLabelPosition; labelSpacing = stringLabelSpacing; labelBackground = stringLabelBackground;
  SetAutomatic[curveFunction, CircuitCurve[#, SetbackDistance -> None, SplitPosition -> splitPosition]&];
  strPrims = MapIndex1[parseStr, strings];

  labelPosition = Center; labelSpacing = 5;
  regPrims = Map[parseReg, regions];

  UnpackOptions[imagePadding, imageSize, epilog, fontSize];
  rect = Rectangle[Offset[{0, -1}, {-1, -1} * $sz], Offset[{0, 0}, {1, 1} * $sz]];

  frame = If[halfFrame,
    nw = {-1, 1} * $sz; ne = {1, 1} * $sz;
    sw = Offset[{0, -1}, {-1, -1} * $sz]; se = Offset[{0, -1}, {1, -1} * $sz];
    Style[Line[{ne, nw, sw, se}], frameColor, AbsoluteThickness @ frameThickness]
  ,
    Style[rect, FaceForm @ None, EdgeForm @ {frameColor, AbsoluteThickness @ frameThickness}]
  ];

  background = If[background === None, Nothing,
    Style[rect, FaceForm @ Replace[background, $toRegColor], EdgeForm @ None]
  ];

  hasVerticalLabels = hasBottomLabels || hasTopLabels;
  SetAutomatic[imagePadding, {Vertical -> If[hasVerticalLabels, 20, 2], Horizontal -> 2}];

  graphics = Graphics[
    {FontSize -> fontSize, FontFamily -> "KaTeX_Main", AbsolutePointSize[5 + nodeEdgeThickness],
     Sequence @@ DeleteOptions[List @ opts, Background],
     background,
     {AbsoluteThickness[wireThickness], strPrims}, regPrims,
     frame, pntPrims},
    If[epilog =!= {}, Epilog -> epilog, Seq[]],
    ImageSize -> imageSize,
    PlotRange -> {{-1, 1}, {-1, 1}} * $sz,
    PlotRangeClipping -> False,
    PlotRangePadding -> 0,
    ImagePadding -> StandardizePadding[imagePadding]
  ];

  If[$fillRegions =!= {},
    graphics = FloodFill[(graphics /. t_Text :> {}) -> graphics, $fillRegions];
    graphics = Image[graphics, ImageSize -> ImageDimensions[graphics]/2];
  ];

  graphics
];

(**************************************************************************************************)

doFlip[{x_, y_}] := Switch[{flipX, flipY},
  {False, False}, { x,  y},
  {True,  False}, {-x,  y},
  {False, True},  { x, -y},
  {True,  True},  {-x, -y}
];

doFlip[Left]  /; TrueQ[flipX] := Right;
doFlip[Right] /; TrueQ[flipX] := Left;

doFlip[Top]    /; TrueQ[flipY] := Bottom;
doFlip[Bottom] /; TrueQ[flipY] := Top;
doFlip[Above]  /; TrueQ[flipY] := Below;
doFlip[Below]  /; TrueQ[flipY] := Above;

doFlip[sym_Symbol] := sym;

(**************************************************************************************************)

parsePnt = Case[
  Seq[pos_, key_] := %[pos -> None, key];

  Seq[pos_ -> lbl_, key_] := Scope[
    $pos = toPos @ pos;
    AssociateTo[$pntAliases, key -> $pos];
    makePnt @ lbl
  ];
];

(**************************************************************************************************)

makePnt = Case[
  (cf:$colorFormP)[spec_] := Scope[
    pntColor = StyleFormData @ cf;
    nodeFrameColor = OklabDarker[pntColor];
    nodeBackground = OklabLighter[pntColor, .45];
    % @ spec
  ];

  "WhiteDisk" := Style[Disk[$pos, 1/2], FaceEdgeForm[White, Black], EdgeForm[AbsoluteThickness[nodeEdgeThickness]]];
  "BlackDisk" := Style[Disk[$pos, 1/2], FaceEdgeForm[Black], EdgeForm[AbsoluteThickness[nodeEdgeThickness]]];

  "Disk"[lbl_, r_:Automatic, opts___Rule] := Scope[
    SetAutomatic[r, nodeDiskSize];
    {
      Style[Disk[$pos, r], Seq @@ makePntEdgeForm[opts]],
      makeLabel[lbl, List @ $pos]
    }
  ];

  "Box"[lbl_, r:Except[_Rule]:Automatic, opts___Rule] := Scope[
    SetAutomatic[r, nodeDiskSize];
    {
      Style[CenteredRectangle[$pos, r, FilterOptions @ opts, RoundingRadius -> 1], Seq @@ makePntEdgeForm[opts]],
      makeLabel[lbl, List @ $pos]
    }
  ];

  c_Customized := customizedBlock[c,
    {"NodeFrameColor" | FrameColor, FontColor, LabelSpacing, LabelPosition, LabelBackground, "NodeEdgeThickness", Background | "NodeBackground"} :>
    {nodeFrameColor, fontColor, labelSpacing, labelPosition, labelBackground, nodeEdgeThickness, nodeBackground},
    %
  ];

  "Point"[lbl_]  := Block[
    {labelPosition = Which[First[$pos] <= $sz, Right, First[$pos] >= $sz, Left, True, Above]},
    {Style[Point[$pos], pntColor], makeLabel[lbl, List @ $pos]}
  ];

  None := Point[$pos];

  lbl_ := %["Disk"[lbl]];
];

(**************************************************************************************************)

makePntEdgeForm[opts___Rule] := Scope[
  fc = Lookup[{opts}, FrameColor, ReplaceNone[pntColor, ReplaceNone[nodeFrameColor, Black]]];
  ef = {AbsoluteThickness[nodeEdgeThickness], fc};
  ff = Lookup[{opts}, Background, nodeBackground];
  List[FaceForm[ff] /. $toRegColor, EdgeForm[ef] /. $toRegColor]
]

(**************************************************************************************************)

parseStr = Case[
  Seq[ue_UndirectedEdge, key_] := %[ue -> None, key];

  Seq[(cf:$colorFormP)[spec_], key_] := Scope[
    stringColor = StyleFormData @ cf;
    %[spec, key]
  ];

  Seq[c_Customized, key_] :=
    customizedBlock[c, {"CurveFunction", "StringColor", SplitPosition} :> {curveFunction, stringColor, splitPosition}, %[#, key]&];

  Seq[UndirectedEdge[a_, b_] -> lbl_, key_] := Scope[
    sc = stringColor;
    If[MatchQ[lbl, $colorFormP[_]], sc = StyleFormData @ Head @ lbl];
    pos = toPos /@ {a, b};
    x = First[DeleteCases[_maybe] @ pos[[All, 1]], None];
    pos = pos /. maybe[0] :> ReplaceNone[x, 0];
    curve = curveFunction[pos];
    points = DiscretizeCurve @ curve;
    AssociateTo[$strAliases, key -> points];
    StyleOperator[sc] @ {curve, makeLabel[lbl, points]}
  ];

]

toPos = Case[
  BottomRight              := doFlip @ {$sz, -$sz};
  TopRight                 := doFlip @ {$sz, $sz};
  side:Top|Bottom          := %[{side, maybe[0]}];
  {Bottom, i_}             := doFlip @ {i, -$sz};
  {Top, i_}                := doFlip @ {i, $sz};
  {Right, i_}              := doFlip @ {$sz, i};
  {Left, i_}               := doFlip @ {-$sz, i};
  key:(_Integer | _String) := Lookup[$pntAliases, key];
  Translate[pos_, off_]    := doFlip[off] + % @ pos;
  other_                   := doFlip @ other;
]

(**************************************************************************************************)

parseReg = Case[
  UndirectedEdge[a_, b_] -> lbl_ := %[(Mean[DeleteNone[#]]& /@ Transpose[toRegPos /@ {a, b}]) -> lbl];
  pos_ -> Placed[c:$ColorPattern, side_] :=
    %[(pos + Switch[side, Left, {-1, 0}, Right, {1, 0}, Above, {0, 1}, Below, {0, -1}, _, {0, 0}]/2) -> c];
  pos_List -> c:$ColorPattern        := (AppendTo[$fillRegions, pos -> Replace[c, $toRegColor]]; Nothing);
  pos_List -> lbl_   := makeLabel[Placed[lbl, Center], {pos}];
  Left -> lbl_       := %[doFlip[{-$sz, 0}] -> Placed[lbl, doFlip @ Right]];
  Right -> lbl_      := %[doFlip[{$sz, 0}]  -> Placed[lbl, doFlip @ Left]];
  Center -> lbl_     := %[doFlip[{0, 0}]    -> Placed[lbl, doFlip @ Center]];
  Top -> lbl_        := %[doFlip[{0, $sz}]  -> Placed[lbl, doFlip @ Below]];
  Bottom -> lbl_     := %[doFlip[{0, -$sz}] -> Placed[lbl, doFlip @ Above]];
  i_Integer -> lbl_  := %[doFlip[{i, -$sz}] -> Placed[lbl, doFlip @ Above]];
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

toRegPos = Case[
  Left    := doFlip @ {-$sz, None};
  Right   := doFlip @ {$sz, None};
  Top     := doFlip @ {None, $sz};
  Bottom  := doFlip @ {None, -$sz};
  i_      := Mean @ Lookup[$strAliases, i];
]

(**************************************************************************************************)

makeLabel[c_Customized, pos_] :=
  customizedBlock[c,
    {LabelPosition, LabelSpacing, LabelBackground, FontColor} :> {labelPosition, labelSpacing, labelBackground, fontColor},
    makeLabel[#, pos]&
  ];

makeLabel[Placed[lbl_, side_], pos_] :=
  Scope[labelPosition = side; makeLabel[lbl, pos]];

makeLabel[lbl_, pos_] /; labelPosition === Vertical :=
  Scope[
    {miny, maxy} = MinMax @ Part[pos, All, 2];
    labelPosition = {If[miny < -$sz+0.5, Bottom, Nothing], If[maxy > $sz-0.5, Top, Nothing]};
    If[labelPosition === {}, labelPosition = Right];
    makeLabel[lbl, pos]
  ];

makeLabel[None, _] := {};

makeLabel[lbl_, pos_] /; labelPosition === Bottom :=
  Scope[labelPosition = Below; If[lbl =!= None, hasBottomLabels ^= True]; makeLabel[lbl, MinimalBy[pos, Last]]];

makeLabel[lbl_, pos_] /; labelPosition === Top :=
  Scope[labelPosition = Above; If[lbl =!= None, hasTopLabels ^= True]; makeLabel[lbl, MaximalBy[pos, Last]]];

makeLabel[lbl_, pos_] /; MatchQ[labelPosition, {__Symbol}] := Block[
  {posList = labelPosition, labelPosition = None},
  makeLabel[labelPosition = #; lbl, pos]& /@ posList
];

makeLabel[lbl_, pos_] := Text[
  lbl, Mean @ pos,
  If[ListQ[labelPosition], labelPosition,
    Lookup[$sideToLabelOffset, labelPosition] * {1 + .5 * labelSpacing, 1 + .05 * labelSpacing}
  ],
  Background -> labelBackground,
  BaseStyle -> {FontColor -> fontColor}
];

