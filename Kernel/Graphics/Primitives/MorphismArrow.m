PublicHead[MorphismArrow]

declareGraphicsFormatting[
  {
    MorphismArrow[path:($CoordMat2P | $GArrowIntP), labelData:Except[_Rule], arrowData:(_List|_String|Automatic), opts___Rule] :> morphismArrowBoxes[path, labelData, arrowData, opts],
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
    arrowPathSetback, decorationWidth,
    arrowShaftColor, arrowShaftOpacity, arrowShaftThickness, arrowShaftDashing, arrowShaftMasking,
    labelOrientation, labelFontSize, labelBackground, labelSpacing
  ];

  If[arrowData === "Mono" && arrowPathSetback =!= None, arrowPathSetback += {.25, 0}];
  points = toCurvePoints @ curve;
  points = SetbackCoordinates[points, arrowPathSetback];
  $path = points;

  SetInherited[labelFontSize, decorationWidth];
  $lineStyler = makeShaftStyler[arrowShaftColor, arrowShaftOpacity, arrowShaftThickness, arrowShaftDashing];

  SetAutomatic[labelSpacing, If[labelOrientation === "Aligned", arrowShaftThickness, {0.3, 0.1} * labelFontSize]];
  arrowhead = parseMorphismArrowhead @ arrowData;
  label = Map[parseMorphismLabel, ToList @ labelData];

  line = Construct[LineBox, points];
  SetAutomatic[arrowShaftMasking, arrowShaftThickness + 20];
  mask = If[!NumberQ[arrowShaftMasking], Nothing,
    makeShaftStyler[White, None, arrowShaftMasking, None] @ line
  ];

  {mask, $lineStyler @ line, arrowhead, label}
];

PublicHead[Reversed]

parseMorphismArrowhead = Case[
  "Iso"           := %[{0 -> Reversed["Arrow"], 1 -> "Arrow"}];
  "Epi"           := %[{1 -> "ShortArrow", Offset[-1, 1] -> "ShortArrow"}];
  "Mono"          := %[{Offset[-1, 0] -> "GentleArrow", 1 -> "GentleArrow"}];
  "Hook"          := %[{Offset[-1, 0] -> "UpHook", 1 -> "GentleArrow"}];
  "MapsTo"        := %[{0 -> "Bar", 1 -> "Arrow"}];
  Automatic       := %[{1 -> "Arrow"}];
  rules:{___Rule} := MapApply[morphismArrowheadBoxes, rules];
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

toOrientedLine[pos_, dirx_, anchor_, offset_, width_, heightScale_, DiskBox[coords_, r_]] :=
  {AbsolutePointSize[r * decorationWidth], PointBox[coords + pos]};

toOrientedLine[pos_, dirx_, anchor_, offset_, width_, heightScale_, boxes_] := {
  Construct[
    InsetBox,
    Construct[GraphicsBox,
      DeleteCases[$lineStyler, _Dashing] @ boxes,
      ImageSize -> {Automatic, decorationWidth * heightScale}
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
  "ShortArrow"        -> {0.6, 1.0,  makeSymmetricArrow @ BezierCurve @ {{-0.6, -1.0}, {-0.5, -0.25}, {0, 0}}},
  "Arrow"             -> {1.0, 1.0,  makeSymmetricArrow @ BezierCurve @ {{-1.0, -1.0}, {-0.9, -0.25}, {0, 0}}},
  "GentleArrow"       -> {1.0, 1.0,  makeSymmetricArrow @ BezierCurve @ {{-1.0, -1.0}, {-0.5, -0.25}, {0, 0}}},
  "StraightArrow"     -> {1.0, 1.0,  LineBox @ {{-1, -1}, {0, 0}, {-1, 1}}},
  "UpHook"            -> {1.0, 0.75, Construct[BezierCurveBox, $hookPoints]},
  "DownHook"          -> {1.0, 0.75, Construct[BezierCurveBox, VectorReflectVertical @ $hookPoints]},
  "Bar"               -> {0.0, 1.0,  LineBox[{{0, -1/2}, {0, 1/2}}]},
  "SkewBar"           -> {0.0, 1.0,  LineBox[{{-1, -1}, {0, 1}}/2]},
  "Disk"              -> {0.0, 1.0,  DiskBox[{0, 0}, .5]}
|>;

(* approximations to circles by bezier, from https://spencermortensen.com/articles/bezier-circle/ *)
(*)
quarter: BezierCurve[{{cr$0, cr$1}, {cr$2, cr$3}, {cr$3, cr$2}, {cr$1, cr$0}}]
half: BezierCurve[{{cr$0, cr$1}, {cr$2,cr$3}, {cr$3,cr$2}, {cr$1, cr$0}, {cr$3, -cr$2}, {cr$2, -cr$3}, {cr$0, -cr$1}}]
full = BezierCurve[{{cr$0, cr$1}, {cr$2,cr$3}, {cr$3,cr$2}, {cr$1, cr$0}, {cr$3, -cr$2}, {cr$2, -cr$3}, {cr$0, -cr$1}, {-cr$2, -cr$3}, {-cr$3, -cr$2}, {-cr$1, cr$0}, {-cr$3,cr$2}, {-cr$2,cr$3}, {cr$0, cr$1}}]
*)

(**************************************************************************************************)

MorphismArrow::badpos = "Label position of `` is not valid."

parseMorphismLabel = Case[

  label:(_String | _Style) := % @ Rule[0.5, label];

  Rule[x:$NumberP, label_] := % @ Rule[{x, Above}, label];

  Rule[{x:$NumberP, labelPos:Above|Below|Center}, label_] := Scope[
    {pos, dir} = VectorAlongLine[$path, Scaled[x]];
    morphismLabelBoxes[label, {pos, dir}, x,
      Switch[labelPos,
        Top,           1,
        Bottom,       -1,
        Center,        0,
        Above | Below, {dx, dy} = dir; If[dx > 0 || dx == 0. && dy > 0, 1, -1] * If[labelPos === Above, 1, -1],
        _,             Message[MorphismArrow::badpos, labelPos]; 0
      ]
    ]
  ]
];

MorphismArrow::badalign = "Setting LabelOrientation -> `` is not valid."

morphismLabelBoxes[label_, {pos_, dir_}, anchor_, ypos_] := Scope[
  If[ypos === 0,
    offset = {0, 0};
    Goto[Done];
  ];
  above = ypos === 1;
  dir2 = If[above, VectorRotate90, VectorRotate90CW] @ dir;
  pos = Offset[Normalize[dir2] * labelSpacing, pos];
  Switch[labelOrientation,
    "Aligned",
      offset = {anchor * 2 - 1, ypos * -1},
    Automatic | Horizontal,
      a = Round[16 * Apply[ArcTan2, ypos * dir] / Pi];
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
      dir = {1, 0},
    _,
      Message[MorphismArrow::badalign, labelOrientation];
      offset = {0, 0}
  ];

  Label[Done];
  baseStyle = {FontSize -> labelFontSize, FontFamily -> "KaTeX_Main"};
  text = Text[label, pos, offset, dir, BaseStyle -> baseStyle, Background -> labelBackground];
  Construct[Typeset`MakeBoxes, text, StandardForm, Graphics]
];

(**************************************************************************************************)

