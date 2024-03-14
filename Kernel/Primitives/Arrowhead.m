PublicGraphicsPrimitive[Arrowhead]

(* TODO: Arrowhead should really be just a thin veneer on NamedIcon, with its own option namespace etc.
it could do with things like ArrowheadScaling that match IconScaling.
that will reduce duplicated code.
*)

SetUsage @ "
Arrowhead[pos$, dir$] is a graphics primitive that renders as an arrowhead beginning at pos$ and pointing in direction dir$.
* The following options are supported:
| %ArrowheadPlane | specification of the plane of the arrowhead, for 3D arrows |
| %ArrowheadLength | the length of the arrowhead |
| %ArrowheadShape | the shape of the arrowhead, as a string |
| %ArrowheadColor | the color of the arrowhead |
| %ArrowheadAnchor | anchor point of the arrowhead on the position, ranging from 0 (back) to 1 (front) |
| %ArrowheadOpacity | opacity of the arrowhead |
| %ArrowheadThickness | thickness of the edge of the arrowhead |

* see %NamedIcon for valid settings of %ArrowheadShape.

* %ArrowheadAnchor -> Center will use the midpoint of the entire bounding box.

* %ArrowheadLength -> Automatic takes the length from the norm of dir$.

* %ArrowHeadColor -> %SolidEdgeForm[face$, edge$] tints the face and edge of the arrowhead differently.
"

Options[Arrowhead] = $arrowheadOptions;

DeclareGraphicsPrimitive[Arrowhead, "Vector,Delta", arrowheadBoxes, {2, 3}];

(**************************************************************************************************)

PrivateFunction[arrowheadBoxes]

(* shouldn't this just be rawIconBoxes? seems like we are duplicating here *)
arrowheadBoxes[Arrowhead[pos:$ExtCoordP, dir:$CoordP, opts___Rule]] := Scope @ CatchMessage[Arrowhead,

  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    arrowheadPlane, arrowheadLength, arrowheadShape, arrowheadColor, arrowheadAnchor,
    arrowheadThickness, arrowheadOpacity
  ];

  {pos, off} = FromOffsetCoord @ pos;
  is3d = Len[pos] == 3;

  tooltip = Lookup[{opts}, ArrowheadTooltip, None];

  SetAuto[arrowheadLength, Norm @ dir];
  arrowheadLength /= 2;

  iconData = LookupMsg::unknownArrowhead[$namedIconData, arrowheadShape];
  If[FailureQ[iconData], Return @ {}];
  {prims, boxes2D, boxes3D, {boundX, boundY, {l, r}}, solid} = iconData;

  SetAuto[arrowheadColor, Black];
  $styler = SolidEmptyStyleBoxOperator[solid, arrowheadColor, arrowheadOpacity, arrowheadThickness];
  rotMatrix = resolvePlane[pos, dir, arrowheadPlane, arrowheadLength];

  If[is3d && (arrowheadShape === "Cone"),
    Null
(*     pos2 = pos - arrowheadAnchor * dy;
    boxes = Construct[ConeBox, ToPacked @ {pos2, pos2 + dy}, arrowheadLength * 0.333];
 *)
  ,
    dx = SetLengthTo[dir, arrowheadLength];
    pos2 = ToPackedReal @ If[arrowheadAnchor === Center,
      pos - Mean[boundX] * dx,
      pos - Lerp[l, r, arrowheadAnchor] * dx
    ];
    boxes23D = If[is3d, boxes3D, boxes2D];
    If[Norm[off] != 0,
      off = Dot[off, rotMatrix];
      boxes23D = boxes23D /. c:$CoordP :> RuleEval @ Offset[off, c]];
    boxes = If[is3d,
      Construct[GeometricTransformation3DBox, $styler @ boxes23D, {rotMatrix, pos2}],
      Construct[GeometricTransformationBox, $styler @ boxes23D, {rotMatrix, pos2}]
    ];
  ];

  If[tooltip =!= None, boxes = NiceTooltipBoxes[boxes, ToBoxes @ tooltip]];
  boxes
];

Arrowhead::unknownArrowhead = "ArrowheadShape -> `` is not one of ``.";

(**************************************************************************************************)

PublicHead[PlaneNormalPointing, PlaneNormalTowards, PlaneRightPointing, PlaneRightTowards]

SetUsage @ "
ArrowheadPlane is an option for %Arrowhead, %HalfArrowhead that determines the plane in which a 3D arrowhead is drawn.
* It accepts the following settings:
| %PlaneNormalPointing[dir$] | a plane whose rightward axis pointing in direction dir$ |
| %PlaneNormalTowards[pos$] | a plane whose normal points to point pos$ |
| %PlaneRightPointing[dir$] | a plane oriented with rightward pointing in direction dir$ |
| %PlaneRightTowards[pos$] | a plane oriented with rightward pointing towards pos$ |
| None | draw arrowhead as a cone |
* The produced plane is never skew, in other words the right axis is always orthogonal to up axis.
"

(**************************************************************************************************)

PrivateFunction[resolvePlane]

resolvePlane[pos:{_, _}, dx_, _, size_] :=
  RotateToMatrix @ SetLengthTo[dx, size];

resolvePlane[pos_, dx_, plane_, size_] := Scope[
  dy = resolvePlaneDY[pos, dx, plane];
  ToPackedReal @ Transpose @ {SetLengthTo[dx, size], SetLengthTo[dy, size]}
]

resolvePlaneDY[pos_, dx_, None] := pos * 0;
(* TODO: fix these, since these were wrt dy *)
resolvePlaneDY[pos_, dx_, PlaneRightPointing[dir_]] := VectorReject[dir, dy];
resolvePlaneDY[pos_, dx_, PlaneRightTowards[pos2_]] := VectorReject[pos2 - pos, dy];
resolvePlaneDY[pos_, dx_, PlaneNormalPointing[dir_]] := Cross[dy, dir];
resolvePlaneDY[pos_, dx_, PlaneNormalTowards[pos2_]] := Cross[dy, pos2 - pos];