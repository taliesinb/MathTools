PublicHead[MorphismArrow]

SetUsage @ "
MorphismArrow[path$] renders as a commutative-diagram style arrow following path$.
MorphismArrow[path$, label$] attachs a given label.
* Each label specification can be a single spec or a list of specs of the form pos$ -> label$.
* Each label$ can be a string or Style[$$] expression or Placed[label$, placement$], where placement is one of:
| Above | always vertically above shaft (default) |
| Below | always vertically below shaft |
| Top | above shaft when arrow is left to right |
| Bottom | below shaft when arrow is left to right |
| Center | on top of shaft |
MorphismArrow[path$, label$, decoration$] applies one or more arrowhead decorations.
* Named decorations corresponding to category theory are 'Iso', 'Epi', 'Mono', 'Hook', 'MapsTo', 'Proarrow', 'DoubleArrow', 'Equality'.
* Decorations can also be a list of rules of the form pos$ -> type$.
* pos$ runs from 0 (begining of shaft) to 1 (end of shaft).
* Offset[delta$, pos$] can apply a constant offset to the pos$.
* type$ is one of 'ShortArrow', 'Arrow', 'GentleArrow', 'StraightArrow', 'UpHook', 'DownHook', 'Bar', 'SkewBar', 'Disk'.
* The following options are supported:
| %ArrowPathSetback | how far to set back path from endpoints |
| %ArrowPathReversed | whether to reverse the entire arrow |
| %ArrowShaftThickness | thickness of arrow shaft |
| %ArrowShaftColor | color of arrow shaft |
| %ArrowShaftDashing | dashing to apply to arrow shaft |
| %ArrowShaftHidden | whether to hide the arrow shaft |
| %LabelRectification | whether to ensure labels never appear upside down |
| %LabelOrientation | orientation of label |
| %LabelFontSize | size of label |
| %LabelBackground | background of label |
| %DecorationWidth | size of applied arrowhead decorations |
% The option %Setback is an alias for %ArrowPathSetback.
* %LabelOrientation can be one of the following:
| Horizontal | appear horizontally (automatic) with offset to avoid clipping shaft |
| 'Aligned' | aligned to the shaft |
% The option %LabelPosition can be one of the following:
| side$ | a symbolic side like Top, Left, TopLeft, Center, etc. |
| {dx$, dy$} | fix a particular offset from the arrow's path |
| AwayFrom[coord$] | choose the side furthest from coord$ |
| Towards[coord$] | choose the side closest to coord$ |
"

declareGraphicsFormatting[
  {
    MorphismArrow[path:($CoordMat2P | $GArrowIntP), labelData:Except[_Rule], arrowData:(_List|_String|Automatic|None), opts___Rule] :> morphismArrowBoxes[path, labelData, arrowData, opts],
    MorphismArrow[path:($CoordMat2P | $GArrowIntP), labelData:Except[_Rule], opts___Rule]                            :> morphismArrowBoxes[path, labelData, Automatic, opts],
    MorphismArrow[path:($CoordMat2P | $GArrowIntP), opts___Rule]                                                     :> morphismArrowBoxes[path, {}, Automatic, opts]
  },
  Graphics
];

Options[MorphismArrow] = $morphismArrowOptions;

morphismArrowBoxes[{a_} | {a_, a_}, rest___] /; MatchQ[a, $CoordP] :=
  morphismArrowBoxes[LoopCurve[a, {0, 1} * .25], rest];

morphismArrowBoxes[curve_, labelData_, arrowData_, opts___Rule] := Scope[

  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    arrowPathSetback, arrowPathOffset, arrowPathReversed, decorationWidth,
    arrowShaftColor, arrowShaftOpacity, arrowShaftThickness, arrowShaftDashing, arrowShaftMasking, arrowShaftHidden,
    labelPosition, labelOrientation, labelRectification, labelFontSize, labelBackground, c, labelSpacing, setback
  ];

  arrowPathReversed //= TrueQ;

  If[labelOrientation === "Aligned", labelOrientation = Aligned];

  SetAutomatic[arrowPathSetback, setback];
  SetAutomatic[arrowPathSetback, 0.1];
  shaftExtraThickness = 0;

  If[MatchQ[arrowData, "DoubleArrow" | "Equality" | "DoubleIso"] && arrowPathOffset === None, arrowPathOffset = 2];

  extraSetback = Switch[arrowData,
    "DoubleIso",   {Offset[decorationWidth/2], Offset[decorationWidth/2]},
    "DoubleArrow", {0, Offset[decorationWidth/2]},
    "Hook",        {Offset[decorationWidth/3], 0},
    "Mono",        {Offset[decorationWidth/3], 0},
    _,             None
  ];
  If[arrowPathReversed  && ListQ[extraSetback], extraSetback //= Reverse];
  arrowPathSetback //= ExtendSetback[extraSetback];

  If[MatchQ[arrowData, "DoubleArrow" | "Equality" | "DoubleIso"] && NumberQ[arrowPathOffset],
    shaftExtraThickness += 5;
    arrowPathOffset = "Perpendicular" -> Offset[#]& /@ (arrowPathOffset * {-1, 1});
  ];

  points = toCurvePoints @ curve;
  points = SetbackCoordinates[points, arrowPathSetback];
  If[arrowPathReversed, points //= Reverse];
  $path = points;

  SetAutomatic[labelPosition, Above];
  labelPositionOnShaft = 0.5;

  SetInherited[labelFontSize, decorationWidth];
  $lineStyler = makeShaftStyler[arrowShaftColor, arrowShaftOpacity, arrowShaftThickness, arrowShaftDashing];

  SetAutomatic[labelSpacing, If[labelOrientation === Aligned, arrowShaftThickness, {0.3, 0.1} * labelFontSize] + shaftExtraThickness];
  arrowhead = parseMorphismArrowhead @ arrowData;
  label = Map[parseMorphismLabel, ToList @ labelData];

  points = applyPathOffset[points, arrowPathOffset];
  line = Construct[LineBox, points];
  SetAutomatic[arrowShaftMasking, arrowShaftThickness + 20];
  mask = If[!NumberQ[arrowShaftMasking], Nothing,
    makeShaftStyler[White, None, arrowShaftMasking, None] @ line
  ];
  If[MatchQ[arrowData, "Adjoint" | "Empty"] || arrowShaftHidden, line = {}];

  {mask, $lineStyler @ line, arrowhead, label}
];

PublicHead[Reversed]

MorphismArrow::badarrowspec = "Bad arrowhead spec ``."
parseMorphismArrowhead = Case[
  "Line"             := %[{}];
  "Iso"              := %[{0 -> Reversed["Arrow"], 1 -> "Arrow"}];
  "DoubleIso"        := %[{Offset[.65, 0] -> Reversed["LongArrow"], Offset[.65, 1] -> "LongArrow"}];
  "Epi"              := %[{1 -> "ShortArrow", Offset[-1, 1] -> "ShortArrow"}];
  "Mono"             := %[{Offset[-1, 0] -> "DownHook", 1 -> "GentleArrow"}];
  (* "Mono"             := %[{Offset[-1, 0] -> "GentleArrow", 1 -> "GentleArrow"}]; *)
  "Hook"             := %[{Offset[-1, 0] -> "UpHook", 1 -> "GentleArrow"}];
  "MapsTo"           := %[{0 -> "Bar", 1 -> "Arrow"}];
  "Proarrow"         := %[{0.5 -> "ShortBar", 1 -> "Arrow"}];
  "DoubleArrow"      := %[{Offset[.65, 1] -> "LongArrow"}];
  "ExtendedAdjoint"  := %[{0 -> "ShortBar"}];
  "Adjoint"          := %[{0.5 -> "Tee"}];
  "Equality"         := %[{}];
  "Arrow"|Automatic  := %[{1 -> "Arrow"}];
  "DiskArrow"        := %[{0 -> "SmallDisk", 1 -> "Arrow"}];
  "ReversedArrow"    := %[{0 -> Reversed["Arrow"]}];
  None | "Line" | "Hidden" := {};
  other_String       := (Message[MorphismArrow::badarrowspec, MsgExpr @ other]; {});
  rules:{___Rule}    := MapApply[morphismArrowheadBoxes, rules];
]

morphismArrowheadBoxes[anchor:$NumberP, shape_] := morphismArrowheadBoxes[Offset[0, anchor], shape];

morphismArrowheadBoxes[Offset[offset:$NumberP, anchor:$NumberP], shape_] := Scope[
  {pos, dir} = VectorAlongLine[$path, Scaled[anchor]];
  isReversed = If[MatchQ[shape, Reversed[_]], shape = First @ shape; True, False];
  {width, heightScale, shape2} = Lookup[$morphismArrowheadLines, shape, ReturnFailed[]];
  If[isReversed, dir *= -1; anchor = 1 - anchor];
  toOrientedLine[pos, dir, anchor, offset, width, heightScale, shape2]
];

_morphismArrowheadBoxes := BadArguments[];

(**************************************************************************************************)

toOrientedLine[Offset[off_, pos_], dirx_, anchor_, offset_, width_, heightScale_, DiskBox[coords_, r_]] :=
  {AbsolutePointSize[r * decorationWidth], PointBox[Offset[off, coords + pos]]};

toOrientedLine[pos_, dirx_, anchor_, offset_, width_, heightScale_, DiskBox[coords_, r_]] :=
  {AbsolutePointSize[r * decorationWidth], PointBox[coords + pos]};

toOrientedLine[pos_, dirx_, anchor_, offset_, width_, heightScale_, boxes_] := {
  Construct[
    InsetBox,
    Construct[GraphicsBox,
      DeleteCases[$lineStyler, _Dashing] @ boxes,
      ImageSize -> {Automatic, decorationWidth * heightScale * 1.2},
      PlotRangeClipping -> False, PlotRangePadding -> Scaled[0.1] (* this prevents cutoff arrowheads when rotating *),
      AspectRatio -> Automatic,
      ImagePadding -> None
    ],
    pos,
    {-(1 - anchor + offset) * width, 0},
    Automatic,
    dirx
  ]
};

cr$0 = 0.00000000;
cr$1 = 1.00005519;
cr$2 = 0.55342686;
cr$3 = 0.99873585;

$hookPoints = {{0, 1-cr$1}, {-cr$2, 1-cr$3}, {-cr$3, 1-cr$2}, {-cr$1, 1}, {-cr$3, 1+cr$2}, {-cr$2, 1+cr$3}, {cr$0-0.2, 1+cr$1}}/2;

makeSymmetricArrow[head_[coords_, a___]] := Construct[JoinedCurveBox, {head[coords, a], Construct[head, VectorReflectVertical @ Reverse @ coords, a]}];

$morphismArrowheadLines = <|
  "LongArrow"         -> {1.6, 1.0,  makeSymmetricArrow @ BezierCurve @ {{-1.6, -1.0}, {-1.5, -0.25}, {0, 0}}},
  "ShortArrow"        -> {0.5, 1.0,  makeSymmetricArrow @ BezierCurve @ {{-0.6, -1.0}, {-0.5, -0.25}, {0, 0}}},
  "Arrow"             -> {1.0, 1.0,  makeSymmetricArrow @ BezierCurve @ {{-1.0, -1.0}, {-0.9, -0.25}, {0, 0}}},
  "GentleArrow"       -> {1.0, 1.0,  makeSymmetricArrow @ BezierCurve @ {{-1.0, -1.0}, {-0.5, -0.25}, {0, 0}}},
  "StraightArrow"     -> {1.0, 1.0,  LineBox @ {{-1, -1}, {0, 0}, {-1, 1}}},
  "Triangle"          -> {1.0, 0.7,  Construct[PolygonBox, Threaded[{1, .7}] * {{-1, -1}, {0, 0}, {-1, 1}}]},
  "UpHook"            -> {1.1, 0.85, Construct[BezierCurveBox, $hookPoints]},
  "DownHook"          -> {1.1, 0.85, Construct[BezierCurveBox, VectorReflectVertical @ $hookPoints]},
  "Bar"               -> {0.0, 1.0,  LineBox[{{0, -1/2}, {0, 1/2}}]},
  "ShortBar"          -> {0.0, 0.6,  LineBox[{{0, -1/3}, {0, 1/3}}]},
  "SkewBar"           -> {0.0, 1.0,  LineBox[{{-1, -1}, {0, 1}}/2]},
  "Disk"              -> {0.0, 1.0,  DiskBox[{0, 0}, .5]},
  "SmallDisk"         -> {0.0, 1.0,  DiskBox[{0, 0}, .25]},
  "Tee"               -> {0.0, 0.8,  Construct[LineBox, Threaded[{-0.5, 0}] + {{{0, 1}, {0, -1}}, {{0, 0}, {1.25, 0}}}]}
|>;

(* approximations to circles by bezier, from https://spencermortensen.com/articles/bezier-circle/ *)

(*)
quarter: BezierCurve[{{cr$0, cr$1}, {cr$2, cr$3}, {cr$3, cr$2}, {cr$1, cr$0}}]
half: BezierCurve[{{cr$0, cr$1}, {cr$2,cr$3}, {cr$3,cr$2}, {cr$1, cr$0}, {cr$3, -cr$2}, {cr$2, -cr$3}, {cr$0, -cr$1}}]
full = BezierCurve[{{cr$0, cr$1}, {cr$2,cr$3}, {cr$3,cr$2}, {cr$1, cr$0}, {cr$3, -cr$2}, {cr$2, -cr$3}, {cr$0, -cr$1}, {-cr$2, -cr$3}, {-cr$3, -cr$2}, {-cr$1, cr$0}, {-cr$3,cr$2}, {-cr$2,cr$3}, {cr$0, cr$1}}]
*)

(**************************************************************************************************)

MorphismArrow::badpos = "LabelPosition -> `` is not valid."

parseMorphismLabel = Case[

  None := {};

  x:$NumberP -> item_ :=
    Block[{labelPositionOnShaft = x}, % @ item];

  {x:$NumberP, pos:Above|Below|Center|Top|Bottom|Left|Right} -> item_ :=
    Block[{labelPositionOnShaft = x, labelPosition = pos}, % @ item];

  Placed[item_, pos_] :=
    Block[{labelPosition = pos}, % @ item];

  c_Customized := customizedBlock[c,
    {LabelSpacing, LabelOrientation, LabelFontSize, LabelBackground, LabelPosition} :>
    {labelSpacing, labelOrientation, labelFontSize, labelBackground, labelPosition},
    %
  ];

  item_ := Scope[
    {pos, dir} = RemoveOffsets @ VectorAlongLine[$path, Scaled[labelPositionOnShaft]];
    morphismLabelBoxes[item, {pos, dir}, labelPositionOnShaft, labelPositionToSide[labelPosition, pos, dir]]
  ];
];

(**************************************************************************************************)

PublicHead[AwayFrom, Towards]

(* choose which side of the line the label should go on *)
labelPositionToSide[labelPos_, pos_, dir_] := Scope[
  Switch[labelPos,
    Top,                1,
    Bottom,             -1,
    Center,             0,
    Left | Right,       {dx, dy} = dir; If[dy > 0 || (dy == 0. && dx < 0), 1, -1] * If[labelPosition === Left, 1, -1],
    Above | Below,      {dx, dy} = dir; If[dx > 0 || (dx == 0. && dy < 0), 1, -1] * If[labelPosition === Above, 1, -1],
    Towards[$Coord2P], sideTowards[First @ labelPos, pos, dir],
    AwayFrom[$Coord2P],  sideTowards[pos, First @ labelPos, dir],
    $Coord2P,           0,
    _,                  Message[MorphismArrow::badpos, labelPos]; 0
  ]
];

sideTowards[src_, tgt_, dir_] := Replace[0 -> 1] @ Sign[Dot[tgt - src, VectorRotate90CW @ dir]];

(**************************************************************************************************)

(* figure out how to offset the label so it doesn't intersect the line *)
chooseAngularOffset[vec_] := Scope[
  a = Round[16 * Apply[ArcTan2, vec] / Pi];
  xf = yf = False;
  If[a < 0, a = -a; yf = True];
  If[a > 8, a = 8 - (a - 8); xf = True];
  offset = Switch[a,
    8, {  5,  0},
    7, {  5, -1},
    6, {  5, -2},
    5, {  5, -3},
    4, {  5, -5},
    3, {  3, -5},
    2, {  2, -5},
    1, {  1, -5},
    0, {  0, -5},
    _, {  0, -5}
  ] / 5.;
  If[xf, Part[offset, 2] *= -1];
  If[yf, Part[offset, 1] *= -1];
  offset
];

(**************************************************************************************************)

MorphismArrow::badalign = "Setting LabelOrientation -> `` is not valid."

morphismLabelBoxes[label_, {pos_, dir_}, anchor_, side_] := Scope[
  If[side === 0 && labelOrientation === "Aligned",
    offset = {0, 0};
    Goto[Done];
  ];
  above = side === 1;
  dir2 = If[above, VectorRotate90, VectorRotate90CW] @ dir;
  pos = Offset[Normalize[dir2] * labelSpacing, pos];
  If[MatchQ[labelPosition, $Coord2P],
    offset = labelPosition;
    dir = {1, 0}
  ,
  Switch[labelOrientation,
    Aligned,
      offset = {anchor * 2 - 1, side * -1};
      If[labelRectification && First[dir] < 0, dir *= -1; offset *= -1];,
    Automatic | Horizontal,
      offset = chooseAngularOffset[side * dir];
      If[labelPosition === Center, offset = {0, 0}];
      dir = {1, 0},
    "Manual",
      offset = labelPosition,
    _,
      Message[MorphismArrow::badalign, labelOrientation];
      offset = {0, 0}
  ]];
  Label[Done];
  If[MatchQ[label, _Image],
    {iw, ih} = ImageDimensions @ label;
    text = Inset[label, pos, {iw/4, -3}, Offset[iw / 2], dir];
  ,
    baseStyle = {FontSize -> labelFontSize, FontFamily -> "KaTeX_Main"};
    background = ReplaceAutomatic[labelBackground, If[labelPosition === Center, White, None]];
    text = Text[label, pos, offset, dir, BaseStyle -> baseStyle, Background -> background];
  ];
  Construct[Typeset`MakeBoxes, text, StandardForm, Graphics]
];

(**************************************************************************************************)

PrivateFunction[pathedTextBoxes]

pathedTextBoxes[curve_, labelData_, opts___Rule] := Scope[

  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    labelFontSize, labelBackground, labelOrientation
  ];

  $path = toCurvePoints @ curve;
  labelPosition = Center;
  labelOrientation = Aligned;
  labelPositionOnShaft = 0.5;
  labelSpacing = 0;

  label = Map[parseMorphismLabel, ToList @ labelData];

  label
];

