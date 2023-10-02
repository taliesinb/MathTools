PublicForm[NamedIcon]

PublicOption[IconThickness, IconStyle, IconScaling]

SetUsage @ "
NamedIcon[pos$, dir$, 'name$'] represents a named curve at pos$ with direction dir$.
* the boxified form uses an InsetBox, unless GraphicsScale is set, in which case it produces primitives directly.
* the following options are supported:
| %AlignmentPoint | 0 | value between 0 and 1 specifies which part of the icon is on the position |
| %GraphicsScale | None | how to emit primitives directly to achieve a given pixel size |
| %ImageSize | 20 | size of icon in pixels |
| %IconStyle | None | styles to apply to path |
"

declareGraphicsFormatting[ni:NamedIcon[$Coord2P|_Offset, $Coord2P, _String | _Sized, ___Rule] :> namedIconBoxes[ni], Graphics];

Options[NamedIcon] = {
  AlignmentPoint -> 0,
  GraphicsScale  -> None,
  IconThickness  -> Inherited,
  IconStyle      -> Identity,
  IconScaling    -> 1,
  ImageSize      -> 20
};

AssociateTo[$MakeBoxesStyleData, KeyTake[Options @ NamedIcon, {IconThickness, IconStyle, IconScaling}]];

NamedIcon::unknownIcon = "`` is not a known icon. Known icons include ``.";

namedIconBoxes[NamedIcon[pos_, dir_, name_, opts:OptionsPattern[]]] := Scope[
  UnpackAssociationSymbols[{opts} -> $MakeBoxesStyleData,
    graphicsScale, iconStyle, iconScaling, $iconThickness];
  UnpackOptionsAs[NamedIcon, {opts}, iconThickness, $imageSize, $alignmentPoint];
  $styler = Switch[iconStyle,
    None|Automatic,    Identity,
    _StyleBoxOperator, iconStyle,
    _,                 StyleBoxOperator @@ iconStyle
  ];
  $imageSize *= (iconScaling /. $scalingRules) * {1, 1};
  $aspectRatio = Last[$imageSize] / First[$imageSize];
  If[Head[name] === Sized, {name, iconSize} = List @@ name];
  iconData = LookupOrMessageKeys[$namedIconData, name, $Failed, NamedIcon::unknownIcon];
  {prims, boxes, primSize} = iconData;
  dir //= Normalize;
  makeIcon[ResolveOffsets[pos, graphicsScale], dir, boxes, primSize, graphicsScale]
];

$scalingRules = {Huge -> 1.75, Large -> 1.5, MediumLarge -> 1.25, Medium -> 1, MediumSmall -> .75, Small -> .5, Tiny -> .25};

(**************************************************************************************************)

makeIcon[pos_, dir_, DiskBox[pos2:$CoordP, r_], primsSize_, scale_] :=
  $styler @ Construct[DiskBox,
    coordPlus[pos, pos2],
    ResolveOffsets[Offset[r * $imageSize], scale]
  ];

makeIcon[pos_, dir_, prims_, primsSize_, scale_] :=
  If[NumberQ[scale],
    makeIconInline[pos, dir, prims, primSize, scale],
    makeIconInset[pos, dir, prims, primSize]
  ];

makeIcon[pos_, dir_, LineBox[path:{$Coord2P, $Coord2P}], primSize_, _] := Scope[
  If[$alignmentPoint != 0,
    path //= TranslateVector[{-$alignmentPoint * First[primSize], 0}]];
  path = Dot[path, RotateToMatrix[dir * $imageSize]];
  offsets = Offset[#, pos]& /@ path;
  $styler @ Construct[LineBox, SimplifyOffsets @ offsets]
];

$boundsRect = {FaceForm[None], EdgeForm[Red], RectangleBox[{-1, -1}, {1, 1}]};
makeIconInset[pos_, dir_, prims_, primSize_] := Scope[
  opos = {$alignmentPoint * First[primSize], 0};
  Construct[
    InsetBox,
    Construct[GraphicsBox,
      {If[$iconThickness =!= Inherited, AbsoluteThickness[$iconThickness]],
        $styler @ prims
      },
      ImageSize -> $imageSize * {1.3, 1.3},
      PlotRangeClipping -> False,
      PlotRange -> {{-1, 1}, {-1, 1}} * 1.3,
      AspectRatio -> $aspectRatio,
      ImagePadding -> None
    ],
    pos, opos, Automatic, dir
  ]
];

makeIconInline[pos_, dirx_, prims_, primSize_, graphicsScale_] := Scope[
  If[$alignmentPoint =!= 0,
    prims = Construct[GeometricTransformationBox, prims, ToPackedReal @ {-$alignmentPoint, 0}]];
  rotMatrix = ToPackedReal @ Transpose @ {dirx, VectorRotate90 @ dirx};
  (* prims = {prims, FaceForm[None], EdgeForm[Red], RectangleBox[{-1, -1}, {1, 1}]}; *)
  transform = {rotMatrix * (0.5 * $imageSize / graphicsScale), pos};
  prims = $styler @ Construct[GeometricTransformationBox, prims, transform];
  If[$iconThickness =!= Inherited, {AbsoluteThickness @ $iconThickness, prims}, prims]
];

(**************************************************************************************************)

toIconData[prims_] := Scope[
  prims //= N;
  boxes = ToGraphicsBoxes[prims];
  points = DeepCases[boxes, $Coord2P];
  {min, max} = CoordinateBoundingBox @ Replace[points, {} -> {{0, 0}}];
  primSize = max - min;
  {prims, boxes, primSize}
];

(**************************************************************************************************)

SetListable[makeAsym];
makeAsym[name_ -> head_[upperPath_]] := Scope[
  upperPath = upperPath + Threaded[{1 - Max[Part[upperPath, All, 1]], 0}];
  lowerPath = VectorReflectVertical @ upperPath;
  symmetricCurve = toJoinedCurve[head @ upperPath, head @ Reverse @ Most @ lowerPath];
  List[
    name                       -> symmetricCurve,
    StringJoin["Upper", name]  -> head[upperPath],
    StringJoin["Lower", name]  -> head[lowerPath]
  ]
];

simplifyLine = Case[
  {l___, a_, a_, r___} := % @ {l, a, r};
  other_               := other;
];

toJoinedCurve[Line[c1_], Line[c2_]] := simplifyLine @ Line @ Join[c1, c2];
toJoinedCurve[JoinedCurve[list_], JoinedCurve[list2_]] := JoinedCurve[Join[list1, list2]];
toJoinedCurve[JoinedCurve[list_], elem_] := JoinedCurve[Append[list, elem]];
toJoinedCurve[c1_, c2_] := JoinedCurve @ {c1, c2};

(**************************************************************************************************)

SetListable[makeClosed];
makeClosed[name_ -> curve_] := List[
  name                       -> curve,
  StringJoin["Closed", name] -> toClosedCurve[curve]
];

toClosedCurve[curve_] := Scope[
  p1 = curvePart[curve, 1];
  pn = curvePart[curve, -1];
  Switch[{p1, pn},

    (* already closed *)
    {p_, p_}, None,

    (* symmetric *)
    {{x_, _?Positive}, {x_, _?Negative}},
      If[Head[curve] === JoinedCurve,
        Append[curve, CurveClosed -> True],
        toJoinedCurve[curve, Line @ {p1, pn}]
      ],

    (* open *)
    {_,  {_, 0|0.}},
      toJoinedCurve[curve, Line @ {{First @ p1, 0}, p1}],

    _,
    None
  ]
];

(**************************************************************************************************)

SetListable[makeFilled]
makeFilled[rule_] := rule;

makeFilled[name_ -> curve_ ? isClosedQ] := List[
  name                       -> curve,
  StringJoin["Filled", StringTrimLeft[name, "Closed"]] -> toFilledCurve[curve]
];

toFilledCurve = Case[
  l_Line        := Polygon @@ simplifyLine @ ToPackedReal @ l;
  b_BezierCurve := FilledCurve @ List @ b;
  j_JoinedCurve := FilledCurve @@ j;
];

(**************************************************************************************************)

isClosedQ = Case[
  JoinedCurve[_, CurveClosed -> True] := True;
  curve_ := curvePart[curve, 1] == curvePart[curve, -1];
];

curvePart = Case[
  Seq[JoinedCurve[elems_], part_] := %[Part[elems, part], part];
  Seq[head_[path_], part_]        := Part[path, part]
];

(**************************************************************************************************)

SetListable[procFilledClosedAsym, makeFilledClosed]
procFilledClosedAsym[rule_] := makeFilled @ makeClosed @ makeAsym @ rule;
procFilledClosedAsym[rule_] := makeFilled @ makeClosed @ makeAsym @ rule;

(**************************************************************************************************)

(* primitives are understood to live in bounding box of range -1 to 1, where 0 is treated as the origin point *)
(* iconNameOrder[a_] := StringDelete[a, {"Long","Short","Large","Small", ; *)

$plusPoints = {{{0, -1}, {0, 1}}, {{-1, 0}, {1, 0}}};
$crossPoints = Sqrt[1/2.] * {{{-1, -1}, {1, 1}}, {{-1, 1}, {1, -1}}};

cr$0 = 0.00000000;
cr$1 = 1.00005519;
cr$2 = 0.55342686;
cr$3 = 0.99873585;
$hookPoints = {{0, 1-cr$1}, {-cr$2, 1-cr$3}, {-cr$3, 1-cr$2}, {-cr$1, 1}, {-cr$3, 1+cr$2}, {-cr$2, 1+cr$3}, {cr$0-0.2, 1+cr$1}}/2;

$hline = Line @ {{-1, 0}, {1, 0}};
$tilde = BSplineCurve @ {{-1,0}, {-.5,.5}, {.0,0},{0.5,-.5},{1,0}};

repeatV[head_[curve_], ys_] := Map[head[Threaded[{0, #}] + curve]&, ys];

$leftBar = {{-1, -1/2}, {-1, 1/2}};
$rightBar = {{1, -1/2}, {1, 1/2}};
$leftArrowheadPoints = {{-1/2, 1/2}, {-1, 0}, {-1/2, -1/2}};
$rightArrowheadPoints = {{1/2, 1/2}, {1, 0}, {1/2, -1/2}};

$namedIconData := $namedIconData = Map[toIconData, DeleteNone @ Association[

  makeFilled @ makeClosed @ makeAsym @ {
  "CurvedArrow"   -> BezierCurve @ {{-1.0, 1.0}, {-0.9, 0.25}, {0, 0}},
  "GentleCurveArrow" -> BezierCurve @ {{-1.0, 1.0}, {-0.5, 0.25}, {0, 0}},
  "Triangle"      -> Line @ {{-0.724145, -1.}, {1., 0.}},
  "Arrow"         -> Line @ {{-1, 1}, {0, 0}},
  "Diamond"       -> Line @ {{-1, 0}, {0, 0.6}, {1, 0}},
  "Kite"          -> Line @ {{-0.53, 0}, {-1., 0.89}, {1., 0}}
  },

  makeAsym @ {
  "Hook"          -> BezierCurve @ Reverse @ $hookPoints
  },

  makeClosed @ makeAsym @ {
  "Square"        -> Line @ {{-1, 0}, {-1, 1}, {1, 1}, {1, 0}}
  },

  "FilledSquare"       -> Rectangle[{-1, -1}, {1, 1}],
  "FilledUpperSquare"  -> Rectangle[{-1, 0}, {1, 1}],
  "FilledLowerSquare"  -> Rectangle[{-1, -1}, {1, 0}],

  "Bar"           -> Line @ {{0, -1}, {0, 1}},
  "UpperBar"      -> Line @ {{0, 1}, {0, 0}},
  "LowerBar"      -> Line @ {{0, 0}, {0, -1}},
  "SkewBar"       -> Line @ {{-1/2, -1}, {1/2, 1}},
  "Tee"           -> Line @ {{{-0, 1}, {0, -1}}, {{0, 0}, {1, 0}}},

  "Disk"          -> Disk[{0, 0}, 1],
  "Circle"        -> Circle[{0, 0}, 1],
  "Dot"           -> Disk[{0, 0}, .2],

  "Times"         -> Line @ $crossPoints,
  "Plus"          -> Line @ $plusPoints,
  "CircleTimes"   -> {Line @ $crossPoints, Circle[{0, 0}, 1]},
  "CirclePlus"    -> {Line @ $plusPoints, Circle[{0, 0}, 1]},
  "CircleMinus"   -> {Line[{{-1, 0}, {1, 0}}], Circle[{0, 0}, 1]},
  "CircleDot"     -> {Disk[{0, 0}, .2], Circle[{0, 0}, 1]},

  "Minus"         -> $hline,
  "Equal"         -> repeatV[$hline, {-1, 1}/3],
  "TripleEqual"   -> repeatV[$hline, {-1, 0, 1}/2],

  "Tilde"         -> $tilde,
  "TildeTilde"    -> repeatV[$tilde, {-1, 1}/3],

  "Ellipsis"      -> Disk[{{-1, 0}, {0, 0}, {1, 0}}*0.77, .25],
  "VerticalEllipsis" -> Disk[{{0, -1}, {0, 0}, {0, 1}}*0.77, .25],

  "RightArrow"     -> Line @ {First @ $hline, $rightArrowheadPoints},
  "BarRightArrow"     -> Line @ {$leftBar, First @ $hline, $rightArrowheadPoints},
  "LeftArrow"      -> Line @ {First @ $hline, $leftArrowheadPoints},
  "LeftArrowBar"      -> Line @ {First @ $hline, $leftArrowheadPoints, $rightBar},
  "LeftRightArrow" -> Line @ {First @ $hline, $leftArrowheadPoints, $rightArrowheadPoints}
]];

(**************************************************************************************************)

PublicFunction[NamedIconData]

NamedIconData[] := Keys @ $namedIconData;
NamedIconData[All] := $namedIconData;

NamedIconData[name_String] /; StringContainsQ[name, "*"] := Select[NamedIconData[], StringMatchQ[name]];
NamedIconData[name_String] := LookupOrMessageKeys[$namedIconData, name, $Failed, NamedIcon::unknownIcon];

