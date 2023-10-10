PublicHead[MorphismArrow]

SetUsage @ "
MorphismArrow[path$] renders as a commutative-diagram style arrow following path$.
MorphismArrow[path$, label$] attachs a given label.
* label$ can be a single spec or a list of specs of the form pos$ -> label$.
* each label$ can be a string or Style[$$] expression or Placed[label$, placement$], where placement is one of:
| Above | always vertically above shaft (default) |
| Below | always vertically below shaft |
| Top | above shaft when arrow is left to right |
| Bottom | below shaft when arrow is left to right |
| Center | on top of shaft |
MorphismArrow[path$, label$, decoration$] applies one or more arrowhead decorations.
* named decorations corresponding to category theory include:
* 'Iso', 'Epi', 'Mono', 'Hook', 'MapsTo', 'Proarrow', 'DoubleArrow', 'DoubleIso', 'Equality', 'Adjoint', 'LongAdjoint'.
* decorations can also be a list of rules of the form pos$ -> type$.
* pos$ runs from 0 (begining of shaft) to 1 (end of shaft).
* type$ includes the icon names in %NamedIcon as well as 'ShortArrow', 'LongArrow', and 'ShortBar'.
* the following options are supported:
| %Setback | how far to set back path from endpoints |
| %ArrowPathReversed | whether to reverse the entire arrow |
| %ArrowThickness | thickness of arrow shaft |
| %ArrowColor | color of arrow shaft |
| %ArrowDashing | dashing to apply to arrow shaft |
| %ArrowShaftHidden | whether to hide the arrow shaft |
| %ArrowheadSize | size of applied arrowhead decorations |
| %LabelRectification | whether to ensure labels never appear upside down |
| %LabelOrientation | orientation of label |
| %LabelPosition | where the label is positioned |
| %LabelFontSize | size of label |
| %LabelBackground | background of label |
| %LabelOffset | absolute offset applied after label is positioned |
| %DebugLabels | whether to put a dot on the label anchors |
* %LabelOrientation can be one of the following:
| Horizontal | appear horizontally (automatic) with offset to avoid clipping shaft |
| 'Aligned' | aligned to the shaft |
% The option %LabelPosition can be one of the following:
| side$ | a symbolic side like Top, Left, TopLeft, Center, etc. |
| {dx$, dy$} | fix a particular offset from the arrow's path |
| AwayFrom[coord$] | choose the side furthest from coord$ |
| Towards[coord$] | choose the side closest to coord$ |
"

Options[MorphismArrow] = $morphismArrowOptions;

DeclareCurvePrimitive[MorphismArrow, First, morphismArrowBoxes];

SignPrimitive["Curve", MorphismArrow]

(**************************************************************************************************)

morphismArrowBoxes[MorphismArrow[{a_} | {a_, a_}, rest___]] /; MatchQ[a, $CoordP] :=
  morphismArrowBoxes[LoopCurve[a, {0, 1} * .25], rest];

morphismArrowBoxes[MorphismArrow[curve_, opts___Rule]] :=
  morphismArrowBoxes[MorphismArrow[curve, None, "Arrow", opts]];

morphismArrowBoxes[MorphismArrow[curve_, labelData_, opts___Rule]] :=
  morphismArrowBoxes[MorphismArrow[curve, labelData, "Arrow", opts]];

morphismArrowBoxes[MorphismArrow[points_, labelData_, arrowData_, opts___Rule]] := Scope[

  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    arrowPathSetback, arrowPathOffset, arrowPathReversed, arrowheadSize,
    arrowColor, arrowOpacity, arrowThickness, arrowDashing, arrowMasking, arrowShaftHidden,
    labelPosition, labelOrientation, labelRectification, labelFontSize, labelBackground, labelSpacing, setback, labelOffset,
    textModifiers, graphicsScale, $debugLabels
  ];

  SetAutomatic[labelOffset, 0];
  $size = arrowheadSize;
  arrowPathReversed //= TrueQ;
  If[labelOrientation === "Aligned", labelOrientation = Aligned];

  SetAutomatic[setback, arrowPathSetback];
  SetAutomatic[setback, 0.1];
  shaftExtraThickness = 0;

  multiOffset = toMultiOffset @ arrowData;
  If[multiOffset =!= None, arrowPathOffset = multiOffset];

  (* apply setback *)
  Switch[arrowData, "Hook" | "Mono",
    extraSetback = {Offset[$size/3], 0};
    If[arrowPathReversed, extraSetback //= Reverse];
    setback //= ExtendSetback[extraSetback];
  ];
  points = SetbackCoordinates[points, setback];
  If[arrowPathReversed, points //= Reverse];
  $path = points;

  SetAutomatic[labelPosition, Above];
  labelPositionOnShaft = 0.5;

  SetInherited[labelFontSize, $size];
  $lineStyler = ShaftStyleBoxOperator[arrowColor, arrowOpacity, arrowThickness, arrowDashing];

  If[arrowPathOffset =!= None,
    maxOffsetY = Max[0, Abs @ DeepCases[arrowPathOffset, AlignedOffset[c_List] :> LevelPart[c, -1 -> 2]]];
    shaftExtraThickness += maxOffsetY + 2;
  ];

  SetAutomatic[labelSpacing, If[labelOrientation === Aligned, arrowThickness, {0.3, 0.1} * labelFontSize] + shaftExtraThickness];
  arrowhead = parseMorphismArrowhead @ arrowData;
  $textModifierFn = toModifierFunction @ textModifiers;

  (* label should be set back abit from big arrowheads. TODO: make this uniform *)
(*   If[NumberQ[graphicsScale] && arrowData ~~~ "DoubleArrow" | "TripleArrow",
    $path = SetbackCoordinates[$path, {0, $size / graphicsScale}]];
 *)
  label = Map[parseMorphismLabel, ToList @ labelData];

  {points, isMulti} = applyPathOffset[points, arrowPathOffset];
  line = Construct[LineBox, points];
  SetAutomatic[arrowMasking, arrowThickness + 20];
  mask = If[!NumberQ[arrowMasking], Nothing,
    ShaftStyleBoxOperator[White, None, arrowMasking, None] @ line
  ];
  If[MatchQ[arrowData, "Adjoint" | "Empty"] || arrowShaftHidden, line = {}];

  {mask, $lineStyler @ line, arrowhead, label}
];

(**************************************************************************************************)

toMultiOffset = Case[
  "DoubleArrow"    := makeAlignedOff[$doubleY, $doubleArrowX, 0, 1];
  "DoubleIso"      := makeAlignedOff[$doubleY, $doubleArrowX, 1, 1];
  "TripleArrow"    := makeAlignedOff[$tripleY, $tripleArrowY, 0, 1];
  "TripleIso"      := makeAlignedOff[$tripleY, $tripleArrowY, 1, 1];
  "Equality"       := makeAlignedOff[$doubleY, 0, 0, 0];
  "TripleEquality" := makeAlignedOff[$tripleY, 0, 0, 0];
  _                := None;
];

SetListable[makeAlignedOff];
makeAlignedOff[y_, x_, x1_, x2_] := AlignedOffset[{{x * x1, y}, {x * -x2, y}}];
makeAlignedOff[0, _, _, _] := None;

$doubleY := Max[Floor[$size/7], 1] * {1, -1};
$doubleArrowX := $size * 0.6;
$tripleY := Max[Floor[$size/5], 2] * {1, 0, -1};
$tripleArrowY := $size * 0.7;

(**************************************************************************************************)

parseMorphismArrowhead = Case[
  None            := {};
  Automatic       := % @ "Arrow";
  name_String     := % @ LookupOrMessageKeys[$namedMorphismArrowheadSpecs, name, {}, MorphismArrow::badarrownamespec];
  other_          := (Message[MorphismArrow::badarrowspec, MsgExpr @ other]; {});
  rules:{___Rule} := MapApply[morphismArrowheadBoxes, rules];
]

MorphismArrow::badarrowspec = "Bad arrowhead spec ``.."
MorphismArrow::badarrownamespec = "`` is not a valid name. Names include: ``."

morphismArrowheadBoxes[curvePos:$NumberP, shape_] := Scope[
  {pos, dir} = VectorAlongLine[$path, Scaled @ curvePos];
  rawNamedIconBoxes[
    ResolveOffsets[pos, graphicsScale],
    dir, shape /. $arrowAliases, graphicsScale, $size, 1, Automatic, arrowThickness, curvePos]
];

(**************************************************************************************************)

$hookSize = 1.2;

$namedMorphismArrowheadSpecs = <|
  "Line"             -> {},

  "Arrow"            -> {1 -> "Arrow"},
  "Proarrow"         -> {0.5 -> "ShortBar", 1 -> "Arrow"},
  "DoubleArrow"      -> {1 -> "LongArrow"},
  "TripleArrow"      -> {1 -> "LongArrow"},

  "DotDot"           -> {0 -> "Dot", 1 -> "Dot"},
  "DotArrow"         -> {0 -> "Dot", 1 -> "Arrow"},
  "ReversedArrow"    -> {0 -> "ReversedArrow"},

  "Iso"              -> {0 -> "ReversedArrow", 1 -> "Arrow"},
  "DoubleIso"        -> {0 -> Reversed["LongArrow"], 1 -> "LongArrow"},
  "TripleIso"        -> {0 -> Reversed["LongArrow"], 1 -> "LongArrow"},

  "Equality"         -> {},
  "TripleEquality"   -> {},

  "Epi"              -> {1 -> Repeating["ShortArrow", {-0.8, 0}]},
  "Mono"             -> {0 -> Sized["LowerHook", $hookSize], 1 -> "Arrow"},
  "Hook"             -> {0 -> Sized["UpperHook", $hookSize], 1 -> "Arrow"},
  "MapsTo"           -> {0 -> "Bar", 1 -> "Arrow"},

  "BarBar"           -> {0 -> "Bar", 1 -> "Bar"},

  "LongAdjoint"      -> {0 -> "Bar"},
  "Adjoint"          -> {0.5 -> "Tee"}
|>;

$arrowAliases = {
  "Arrow" -> "CurvedArrow",
  "ReversedArrow" -> Reversed @ "CurvedArrow",
  "LongArrow" -> Sized["CurvedArrow", {2, 1}],
  "ShortArrow" -> Sized["CurvedArrow", {2/3, 1}],
  "ShortBar" -> Sized["Bar", {1, 1/2}]
}

_morphismArrowheadBoxes := BadArguments[];

(**************************************************************************************************)

MorphismArrow::badpos = "LabelPosition -> `` is not valid."

parseMorphismLabel = Case[

  None := {};

  x:$NumberP -> item_ :=
    Block[{labelPositionOnShaft = x}, % @ item];

  {x:$NumberP, pos:$SidePattern|Center|Above|Below} -> item_ :=
    Block[{labelPositionOnShaft = x, labelPosition = pos}, % @ item];

  Placed[item_, pos_] :=
    Block[{labelPosition = pos}, % @ item];

  c_Customized := customizedBlock[c,
    {LabelSpacing, LabelOrientation, LabelFontSize, LabelBackground, LabelPosition, LabelOffset} :>
    {labelSpacing, labelOrientation, labelFontSize, labelBackground, labelPosition, labelOffset},
    %
  ];

  item_ := Scope[
    {pos, dir} = RemoveOffsets @ VectorAlongLine[$path, Scaled[labelPositionOnShaft]];
    morphismLabelBoxes[$textModifierFn @ item, {pos, dir}, labelPositionOnShaft, labelPositionToSide[labelPosition, pos, dir]]
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
    $SidePattern,       sideTowards[Lookup[$SideToCoords, labelPos] * 1000, pos, dir],
    Towards[$Coord2P],  sideTowards[First @ labelPos, pos, dir],
    AwayFrom[$Coord2P], sideTowards[pos, First @ labelPos, dir],
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
  If[Not[labelPosition === Center && labelOrientation ~~~ Automatic | Horizontal],
    pos = Offset[Normalize[dir2] * labelSpacing + labelOffset, pos]];
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
    If[$debugLabels, background = RGBColor[1,.9,.9]];
    text = Text[label, pos, offset, dir, BaseStyle -> baseStyle, Background -> background];
  ];
  If[$debugLabels, text = NiceTooltip[{text, Red, Point @ pos}, <|"pos" -> pos, "offset" -> offset, "dir" -> dir|>]];
  ToGraphicsBoxes @ text
];

(**************************************************************************************************)

PrivateFunction[pathedTextBoxes]

pathedTextBoxes[curve_, labelData_, opts___Rule] := Scope[

  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    labelFontSize, labelBackground, labelOrientation
  ];

  $path = CurveToPoints @ curve;
  labelPosition = Center;
  labelOrientation = Aligned;
  labelPositionOnShaft = 0.5;
  labelSpacing = 0;

  label = Map[parseMorphismLabel, ToList @ labelData];

  label
];

