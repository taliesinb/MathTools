PublicForm[NamedIcon]

PublicOption[IconThickness, IconColor, IconScaling]

SetUsage @ "
NamedIcon[pos$, dir$, 'name$'] represents a named curve at pos$ with direction dir$.
* the boxified form uses an InsetBox, unless GraphicsScale is set, in which case it produces primitives directly.
* the following options are supported:
| %AlignmentPoint | ranging from 0 to 1, where 0 is the back and 1 is the front (default 0.5) |
| %GraphicsScale | how to emit primitives directly to achieve a given pixel size (default None) |
| %ImageSize | size of icon in pixels (default 20) |
| %IconColor | color of the icon (default None) |
| %IconThickness | thickness of the icon (default 1) |
* for %AlignmentPoint, the back and front are not the bounding box, but rather the part of the icon that overlaps the x-axis.
* %AlignmentPoint -> None leaves the icon centered.
* %DebugBounds -> True will show the icon with bounding box highlighted.
"

Options[NamedIcon] = {
  AlignmentPoint -> None,
  GraphicsScale  -> None,
  IconThickness  -> 1,
  IconColor      -> Automatic,
  IconScaling    -> 1,
  ImageSize      -> 20,
  DebugBounds    -> False
};

AssociateTo[$MakeBoxesStyleData, KeyTake[Options @ NamedIcon, {IconThickness, IconStyle, IconScaling}]];

DeclareGraphicsPrimitive[NamedIcon, "Vector,Delta", namedIconBoxes];

(**************************************************************************************************)

NamedIcon::unknownIcon = "`` is not a known icon. Known icons include ``.";

PublicHead[Reversed, Repeating]

$debugBounds = False;
namedIconBoxes[NamedIcon[pos:$Coord2P|_Offset, dir:$Coord2P, name:(_String | _Sized | _Reversed | _Repeating), opts:OptionsPattern[]]] := Scope[
  UnpackAssociationSymbols[{opts} -> $MakeBoxesStyleData,
    graphicsScale, iconColor, iconScaling, iconThickness];
  UnpackOptionsAs[NamedIcon, {opts}, imageSize, alignmentPoint, $debugBounds];
  rawNamedIconBoxes[pos, dir, name, graphicsScale, imageSize, iconScaling /. $scalingRules, iconColor, iconThickness, alignmentPoint /. $alignmentRules]
];

$alignmentRules = {Left -> 0, Center -> 0.5, Right -> 1};
$scalingRules = {Huge -> 1.75, Large -> 1.5, MediumLarge -> 1.25, Medium -> 1, MediumSmall -> .75, Small -> .5, Tiny -> .25};

(**************************************************************************************************)

PrivateFunction[rawNamedIconBoxes]

rawNamedIconBoxes[pos_, dir_, name_, graphicsScale_, imageSize_, scaling_, color_, thickness_, align_] := Scope[
  If[Head[name] === Reversed, name //= First; dir = dir * -1; If[NumberQ @ align, align //= OneMinus]];
  If[Head[name] === Repeating, {name, reps} = List @@ name, reps = None];
  If[Head[name] === Sized, {name, scaling} = List @@ name];
  $imageSize = imageSize * scaling * {1, 1};
  iconData = LookupOrMessageKeys[$namedIconData, name, $Failed, NamedIcon::unknownIcon];
  If[FailureQ[iconData], Return @ {}];
  {prims, boxes, boxes3D, {{x1, x2}, {y1, y2}, {b1, b2}}, solid} = iconData;
  $styler = SolidEmptyStyleBoxOperator[solid, thickness, None, color];
  $originx = If[NumberQ @ align, Lerp[b1, b2, align], 0];
  $origin = {$originx, 0};
  makeIcon[
    ResolveOffsets[pos, graphicsScale],
    Normalize @ ToPackedReal @ dir,
    applyRep[boxes, reps],
    graphicsScale
  ]
];

applyRep[b_, None] := b;
applyRep[b_, xs_] := Construct[GeometricTransformationBox, b, {{#, 0.}}& /@ xs];

(**************************************************************************************************)

makeIcon[pos_, dir_, DiskBox[pos2:$CoordP, r_], scale_] :=
  $styler @ Construct[DiskBox,
    coordPlus[pos, pos2],
    ResolveOffsets[Offset[r * $imageSize], scale]
  ];

makeIcon[pos_, dir_, prims_, scale_] :=
  If[NumberQ[scale],
    makeIconInline[pos, dir, prims, scale],
    makeIconInset[pos, dir, prims]
  ];

(* this optimization doesn't handle multilines!
makeIconInset[pos_, dirx_, LineBox[path:{_, _}]] := Scope[
  If[$originx =!= 0, path //= TranslateVector[-$origin]];
  path = RotateVectorTo[path, dirx * $imageSize/2];
  offsets = Offset[#, pos]& /@ path;
  $styler @ Construct[LineBox, SimplifyOffsets @ offsets]
];
*)

$boundsRect = {FaceForm[None], EdgeForm[Red], RectangleBox[{-1, -1}, {1, 1}]};
makeIconInset[pos_, dir_, prims_] := Scope[
  boxes = $styler @ prims;
  Construct[
    InsetBox,
    Construct[GraphicsBox,
      If[$debugBounds, addDebugBounds @ boxes, boxes],
      ImageSize -> $imageSize * {1.3, 1.3},
      PlotRangeClipping -> False,
      PlotRange -> {{-1, 1}, {-1, 1}} * 1.3,
      AspectRatio -> (Last[$imageSize] / First[$imageSize]),
      ImagePadding -> None
    ],
    pos, $origin, Automatic, dir
  ]
];

addDebugBounds[boxes_] := List[
  {EdgeForm @ {AbsoluteThickness[2], RGBColor[1, 0, 0, .5]}, FaceForm @ None,
   Construct[RectangleBox, {x1, y1}, {x2, y2}]},
  boxes,
  {RGBColor[0, 0, 1, .5], AbsoluteThickness[2], AbsolutePointSize[5],
    Construct[LineBox, {{b1, 0}, {b2, 0}}],
    Construct[PointBox, {{b1, 0}, {b2, 0}}]}
];

(* this optimization doesn't handle multilines!
makeIconInline[pos_, dirx_, LineBox[path:{_, _}], graphicsScale_] := Scope[
  If[$originx =!= 0, path //= TranslateVector[-$origin]];
  path = RotateVectorTo[path, dirx * $imageSize / graphicsScale / 2] + Threaded[pos];
  $styler @ Construct[LineBox, path]
];
*)

makeIconInline[pos_, dirx_, prims_, graphicsScale_] := Scope[
  If[$debugBounds, prims //= addDebugBounds];
  If[$originx != 0,
    prims = Construct[GeometricTransformationBox, prims, -$origin]];
  rotMatrix = ToPackedReal @ Transpose @ {dirx, VectorRotate90 @ dirx};
  (* prims = {prims, FaceForm[None], EdgeForm[Red], RectangleBox[{-1, -1}, {1, 1}]}; *)
  transform = {rotMatrix * (0.5 * $imageSize / graphicsScale), pos};
  $styler @ Construct[GeometricTransformationBox, prims, transform]
];

(**************************************************************************************************)

makeIconData[bounded[prims_, a_, ___], key_] :=
  makeIconData[prims, key, a];

makeIconData[prims_, key_] :=
  makeIconData[prims, key, {$u, $u, $u}];

makeIconData[prims_, Key[name_], align_] := Scope[
  prims //= N;
  boxes = ToGraphicsBoxes @ prims;
  boxes3D = ToGraphics3DBoxes @ prims;
  solid = solidPrimitiveQ @ prims;
  {prims, boxes, boxes3D, align, solid}
];

(**************************************************************************************************)

solidPrimitiveQ = Case[
  e_List       := % @ First @ e;
  _FilledCurve := True;
  _Polygon     := True;
  _Disk        := True;
  _Rectangle   := True;
  _            := False;
]

(**************************************************************************************************)

SetListable[makeArrowHead];
makeArrowHead[name_ -> head_[upperPath_]] := Scope[
  (* center the primitive *)
  {x1, x2} = MinMax @ Part[upperPath, All, 1];
  dx = -Avg[x1, x2];
  upperPath = TranslateVector[{dx, 0}, upperPath];
  lowerPath = VectorReflectVertical @ upperPath;
  symmetricCurve = toJoinedCurve[head @ upperPath, head @ Reverse @ Most @ lowerPath];
  {x, {y1, y2}} = CoordinateBounds @ upperPath;
  l = If[Part[upperPath, 1, 2] == 0, Part[upperPath, 1, 1], First @ x];
  r = If[Part[upperPath, -1, 2] == 0, Part[upperPath, -1, 1], Last @ x];
  lr = {l, r};
  List[
    name                       -> bounded[symmetricCurve, {x, {-y2, y2}, lr}],
    StringJoin["Upper", name]  -> bounded[head[upperPath], {x, {0, y2}, lr}],
    StringJoin["Lower", name]  -> bounded[head[lowerPath], {x, {-y2, 0}, lr}]
  ]
];

simplifyLine = Case[
  Line[{l___, a_, a_, r___}]     := % @ Line @ {l, a, r};
  Line[{f_, m___, l_}] /; f == l := EmptyPolygon @ {f, m};
  other_                         := other;
];


toJoinedCurve[Line[c1_], Line[c2_]] := simplifyLine @ Line @ Join[c1, c2];
toJoinedCurve[JoinedCurve[list_], JoinedCurve[list2_]] := JoinedCurve[Join[list1, list2]];
toJoinedCurve[JoinedCurve[list_], elem_] := JoinedCurve[Append[list, elem]];
toJoinedCurve[c1_, c2_] := JoinedCurve @ {c1, c2};

(**************************************************************************************************)

SetListable[makeClosed];

makeClosed[name_ -> (curve_ ? isClosedQ)] := name -> curve;

makeClosed[name_ -> bounded[curve_, b_]] := List[
  name                       -> bounded[curve, ReplacePart[b, {3, 1} -> Part[b, 3, 2]]],
  StringJoin["Closed", name] -> bounded[toClosedCurve[curve], b]
];

toClosedCurve[curve_] := Scope[
  p1 = curvePart[curve, 1];
  pn = curvePart[curve, -1];
  Switch[{p1, pn},

    (* symmetric *)
    {{x_, _?Positive}, {x_, _?Negative}},
      If[Head[curve] === JoinedCurve,
        Append[curve, CurveClosed -> True],
        toJoinedCurve[curve, Line @ {p1}]
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
  bounded[c_, a_] := bounded[toFilledCurve @ c, a];
  l_Line          := Polygon @@ simplifyLine @ l;
  p_Polygon       := p;
  e_EmptyPolygon  := Polygon @@ e;
  b_BezierCurve   := FilledCurve @ List @ b;
  j_JoinedCurve   := FilledCurve @@ j;
];

(**************************************************************************************************)

isClosedQ = Case[
  bounded[c_, _]                      := % @ c;
  JoinedCurve[_, CurveClosed -> True] := True;
  _Polygon | _EmptyPolygon            := True;
  curve_                              := curvePart[curve, 1] == curvePart[curve, -1];
];

curvePart = Case[
  Seq[JoinedCurve[elems_], part_] := %[Part[elems, part], part];
  Seq[head_[path_], part_]        := Part[path, part]
];

(**************************************************************************************************)

$arrowIcons := With[
  {cr$0 = 0.00000000, cr$1 = 1.00005519, cr$2 = 0.55342686, cr$3 = 0.99873585},
  {hookPoints = {{0, 1-cr$1}, {-cr$2, 1-cr$3}, {-cr$3, 1-cr$2}, {-cr$1, 1}, {-cr$3, 1+cr$2}, {-cr$2, 1+cr$3}, {cr$0-0.2, 1+cr$1}}/2}, {

  makeFilled @ makeClosed @ makeArrowHead @ {
  "CurvedArrow"        -> BezierCurve @ {{-1.0, 1.0}, {-0.9, 0.25}, {0, 0}},
  "GentleCurveArrow"   -> BezierCurve @ {{-1.0, 1.0}, {-0.5, 0.25}, {0, 0}},
  "Triangle"           -> Line @ {{-0.724145, 1.}, {1., 0.}},
  "Arrow"              -> Line @ {{-1, 1}, {0, 0}},
  "Diamond"            -> Line @ {{-1, 0}, {0, 0.6}, {1, 0}},
  "Kite"               -> Line @ {{-0.53, 0}, {-1., 0.89}, {1., 0}}
  },

  setRightAligned @ makeArrowHead @ {
  "Hook"               -> BezierCurve @ Reverse @ hookPoints
  }
}];

(**************************************************************************************************)

SetListable[makeRotated]
makeRotated[name_ -> bounded[curve_, b_, bt_]] := List[
  name -> bounded[curve, b],
  StringReplace[name, "Right" -> "Left"] -> bounded[ScalePrimitives[curve, {-1, 1}], boundsFlipX @ b],
  StringReplace[name, "Right" -> "Up"]   -> bounded[RotatePrimitives[curve, Pi/2], boundsRot[b, bt]],
  StringReplace[name, "Right" -> "Down"] -> bounded[RotatePrimitives[curve, -Pi/2], boundsRot[boundsFlipX @ b, bt]]
];

flip[{x_, y_}] := -{y, x};
boundsRot[{bx_, by_, _}, bt_] := {by, bx, bt};
boundsFlipX[b_] := flip /@ b;

$hline = Line @ {{-1, 0}, {1, 0}};
$leftBar = {{-1, -1/2}, {-1, 1/2}};
$rightBar = {{1, -1/2}, {1, 1/2}};
$leftArrowheadPoints = {{-1/2, 1/2}, {-1, 0}, {-1/2, -1/2}};
$rightArrowheadPoints = {{1/2, 1/2}, {1, 0}, {1/2, -1/2}};
$leftRightPrim = Line @ {First @ $hline, $leftArrowheadPoints, $rightArrowheadPoints};

$rotatedIcons := makeRotated @ {
  "RightArrow"         -> bound[$u, $h, $u, $z] @ Line @ {First @ $hline, $rightArrowheadPoints},
  "BarRightArrow"      -> bound[$u, $h, $u, $z] @ Line @ {$leftBar, First @ $hline, $rightArrowheadPoints},
  "RightHalfCircle"    -> bound[$r, $u, $r, $u] @ HalfCircle[{0,0}, {1, 0}],
  "RightHalfDisk"      -> bound[$r, $u, $r, $u] @ HalfDisk[{0,0}, {1, 0}]
};

(**************************************************************************************************)

$cs = 1 / 1.5;
$csx = {-$cs, 1}; $csy = {-$cs, $cs};
$rightCup = With[{b = 0.552284749831, r = 1 / $cs},
  {points = {{r, 1}, {r, 1}, {0, 1}, {0,1}, {-b, 1}, {-1, b}, {-1, 0}, {-1, -b}, {-b, -1}, {0, -1}, {0, -1}, {r, -1}}},
  ToPackedReal @ points * $cs
];

$setIcons := {
  "Element"     -> bound[$csx, $csy, $csx] @ {BezierCurve @ $rightCup, Line @ {{1, 0}, {-$cs, 0}}},
  "Subset"      -> bound[$csx, $csy, $csx] @ BezierCurve @ $rightCup,
  "SubsetEqual" -> bound[$csx, {-1, $cs}, $csx] @ {BezierCurve @ $rightCup, Line @ {{-$cs, -1.}, {1, -1.}}}
};

(**************************************************************************************************)

(* primitives are understood to live in bounding box of range -1 to 1, where 0 is treated as the origin point *)
(* iconNameOrder[a_] := StringDelete[a, {"Long","Short","Large","Small", ; *)

$u = {-1, 1};
$h = {-1, 1}/2;
$r = {0, 1};
$l = {-1, 0};
$z = {0, 0};

$sq2 = Sqrt[1/2.];
$plusPoints = {{{0, -1}, {0, 1}}, {{-1, 0}, {1, 0}}};
$crossPoints = $sq2 * {{{-1, -1}, {1, 1}}, {{-1, 1}, {1, -1}}};

$tilde = BSplineCurve @ {{-1,0}, {-.5,.33}, {.0,0},{0.5,-.33},{1,0}};

repeatV[head_[curve_], ys_] := Map[head[Threaded[{0, #}] + curve]&, ys];

$mathIcons := {
  "Dot"                -> bound[$u*.2, $u*.2, $z] @ Disk[{0, 0}, .2],
  "Times"              -> bound[$u*$sq2, $u*$sq2, $z] @ Line @ $crossPoints,
  "Plus"               -> Line @ $plusPoints,
  "CircleTimes"        -> {Line @ $crossPoints, Circle[{0, 0}, 1]},
  "CirclePlus"         -> {Line @ $plusPoints, Circle[{0, 0}, 1]},
  "CircleMinus"        -> {Line[{{-1, 0}, {1, 0}}], Circle[{0, 0}, 1]},
  "CircleDot"          -> {Disk[{0, 0}, .2], Circle[{0, 0}, 1]},

  "Minus"              -> $hline,
  "Equal"              -> repeatV[$hline, {-1, 1}/3],
  "TripleEqual"        -> repeatV[$hline, {-1, 0, 1}/2],

  "Tilde"              -> $tilde,
  "TildeTilde"         -> repeatV[$tilde, {-1, 1}/4]
};

(**************************************************************************************************)

$shapeIcons := {
  makeClosed @ makeArrowHead @ {
  "Square"             -> Line @ {{-1, 0}, {-1, 1}, {1, 1}, {1, 0}}
  },
  "FilledSquare"       -> Rectangle[{-1, -1}, {1, 1}],
  "FilledUpperSquare"  -> Rectangle[{-1, 0}, {1, 1}],
  "FilledLowerSquare"  -> Rectangle[{-1, -1}, {1, 0}],
  "Disk"               -> Disk[{0, 0}, 1],
  "Circle"             -> Circle[{0, 0}, 1]
};

(**************************************************************************************************)

PrivateVariable[$namedIconData]

bound[h_, v_, b_][prims_] := bounded[prims, {h, v, b}];
bound[h_, v_, b_, b2_][prims_] := bounded[prims, {h, v, b}, b2];

SetListable[setRightAligned];
setRightAligned[name_ -> bounded[curve_, {bx_, by_, {_, bb2_}}]] :=
  name -> bounded[curve, {bx, by, {bb2, bb2}}];

$namedIconData := $namedIconData = createIconData[];

createIconData[] := MapIndex1[makeIconData, DeleteCases[None | bounded[None, ___]] @ Association[

  $arrowIcons,

  $shapeIcons,

  "Bar"                -> bound[$z, $u, $z] @ Line @ {{0, -1}, {0, 1}},
  "UpperBar"           -> bound[$z, $r, $z] @ Line @ {{0, 1}, {0, 0}},
  "LowerBar"           -> bound[$z, $l, $z] @ Line @ {{0, 0}, {0, -1}},
  "SkewBar"            -> bound[$h, $u, $z] @ Line @ {{-1/2, -1}, {1/2, 1}},
  "Tee"                -> bound[$r, $u, $r] @ Line @ {{{-0, 1}, {0, -1}}, {{0, 0}, {1, 0}}},

  $mathIcons,

  "Ellipsis"           -> bound[$u, $u*.25, $u] @ Disk[{{-1, 0}, {0, 0}, {1, 0}}*0.77, .25],
  "VerticalEllipsis"   -> bound[$u*.25, $u, $u*.25] @ Disk[{{0, -1}, {0, 0}, {0, 1}}*0.77, .25],

  $rotatedIcons,

  $setIcons,

  "LeftRightArrow"     -> bound[$u, $h, $u] @ $leftRightPrim,
  "UpDownArrow"        -> bound[$h, $u, $z] @ RotatePrimitives[$leftRightPrim, Pi/2]

]];

(**************************************************************************************************)

PublicFunction[NamedIconData]

NamedIconData[] := Keys @ $namedIconData;
NamedIconData[All] := $namedIconData;

NamedIconData[name_String] /; StringContainsQ[name, "*"] := Select[NamedIconData[], StringMatchQ[name]];
NamedIconData[name_String] := LookupOrMessageKeys[$namedIconData, name, $Failed, NamedIcon::unknownIcon];

