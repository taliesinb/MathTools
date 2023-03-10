PublicHead[Arrowhead]

SetUsage @ "
Arrowhead[pos$, dir$] is a graphics primitive that renders as an arrowhead beginning at pos$ and pointing in direction dir$.
* The following options are supported:
| ArrowheadPlane | specification of the plane of the arrowhead, for 3D arrows |
| ArrowheadLength | the length of the arrowhead |
| ArrowheadShape | the shape of the arrowhead, as a string |
| ArrowheadColor | the color of the arrowhead |
| ArrowheadAnchor | anchor point of the arrowhead on the position, ranging from 0 (back) to 1 (front) |
| ArrowheadOpacity | opacity of the arrowhead |
"

Options[Arrowhead] = $arrowheadOptions;

declareGraphicsFormatting[Arrowhead[pos:$Coord2P, dir:$Coord2P, rest___Rule]  :> arrowheadBoxes[pos, dir, rest], Graphics];
declareGraphicsFormatting[Arrowhead[pos:$Coord3P, dir:$Coord3P, rest___Rule]  :> arrowheadBoxes[pos, dir, rest], Graphics3D];

(**************************************************************************************************)

Arrowhead::badshape = "`` is not a valid setting of ArrowheadShape, which should be one of ``."

PrivateFunction[arrowheadBoxes]

arrowheadBoxes[pos_, dir_, opts___Rule] := Scope[
  UnpackAssociationSymbols[
    {opts} -> $MakeBoxesStyleData,
    arrowheadPlane, arrowheadLength, arrowheadShape, arrowheadColor, arrowheadAnchor,
    arrowheadEdgeThickness, arrowheadEdgeColor, arrowheadOpacity
  ];
  tooltip = Lookup[{opts}, ArrowheadTooltip, None];
  SetAutomatic[arrowheadOpacity, None];
  SetAutomatic[arrowheadLength, Norm @ dir];
  {dx, dy} = resolvePlane[pos, dir, arrowheadPlane, arrowheadLength];
  is3d = InnerDimension[pos] === 3;
  If[is3d && (Max[Abs[dx]] < .0001 || arrowheadShape === "Cone"),
    pos2 = pos - arrowheadAnchor * dy;
    polygon = Construct[ConeBox, ToPacked @ {pos2, pos2 + dy}, arrowheadLength * 0.333];
  ,
    points = Lookup[$arrowheadShapes, arrowheadShape, Message[Arrowhead::badshape, arrowheadShape, StringRiffle[Keys @ $arrowheadShapes, ", "]]; Return[{}]];
    polygon = toOrientedPolygon[pos, dx, dy, points, arrowheadAnchor];
  ];
  edgeColor = Darker[arrowheadColor, .2];
  opacity = If[NumericQ[arrowheadOpacity], Opacity @ arrowheadOpacity, Sequence @@ {}];
  boxes = StyleBox[polygon,
    opacity,
    If[arrowheadColor === None, FaceForm @ None, If[is3d, FaceForm[{Sequence @@ Color3D @ arrowheadColor}], FaceForm @ arrowheadColor]],
    If[arrowheadEdgeThickness == 0, EdgeForm @ None, EdgeForm[{AbsoluteThickness[arrowheadEdgeThickness], opacity, edgeColor}]]
  ];
  If[tooltip =!= None, boxes = NiceTooltipBoxes[boxes, ToBoxes @ tooltip]];
  boxes
]

toOrientedPolygon[pos_, dx_, dy_, points_, anchor_] :=
  Construct[If[Length[dx] == 2, PolygonBox, Polygon3DBox], Threaded[pos] + Dot[points - Threaded[{0, anchor}], {dx, dy}]];

(**************************************************************************************************)

$arrowheadShapes = <|
  "EquilateralTriangle"      -> {{-0.57735, 0}, {0, 1}, {0.57735, 0}},
  "LeftEquilateralTriangle"  -> {{-0.57735, 0}, {0, 1}, {0, 0}},
  "RightEquilateralTriangle" -> {{0, 0}, {0, 1}, {0.57735, 0}},

  "WideTriangle"             -> {{-1, 0}, {0, 1}, {1, 0}},
  "LeftWideTriangle"         -> {{-1, 0}, {0, 1}, {0, 0}},
  "RightWideTriangle"        -> {{0, 0}, {0, 1}, {1, 0}},

  "Triangle"           -> {{-0.75, 0}, {0, 1}, {0.75, 0}},
  "LeftTriangle"       -> {{-0.75, 0}, {0, 1}, {0, 0}},
  "RightTriangle"      -> {{0, 0}, {0, 1}, {0.75, 0}},

  "NarrowTriangle"           -> {{-0.5, 0}, {0, 1}, {0.5, 0}},
  "NarrowLeftTriangle"       -> {{-0.5, 0}, {0, 1}, {0, 0}},
  "NarrowRightTriangle"      -> {{0, 0}, {0, 1}, {0.5, 0}},

  "PointedTriangle"          -> {{-0.57735, -0.30764}, {0., 0.}, {0.57735, -0.30764}, {0., 1.}},
  "LeftPointedTriangle"      -> {{-0.57735, -0.30764}, {0., 0.}, {0., 1.}},
  "RightPointedTriangle"     -> {{0., 0.}, {0.57735, -0.30764}, {0., 1.}}
|>

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

resolvePlane[pos_, dir_, plane_, size_] := Scope[
  dy = dir;
  dx = resolvePlaneDX[pos, dy, plane];
  {SetLengthTo[dx, size], SetLengthTo[dy, size]}
]

resolvePlaneDX[{_, _}, dy_, _] := VectorRotate90CW[dy];
resolvePlaneDX[pos_, dy_, None] := pos * 0;
resolvePlaneDX[pos_, dy_, PlaneRightPointing[dir_]] := VectorReject[dir, dy];
resolvePlaneDX[pos_, dy_, PlaneRightTowards[pos2_]] := VectorReject[pos2 - pos, dy];
resolvePlaneDX[pos_, dy_, PlaneNormalPointing[dir_]] := Cross[dy, dir];
resolvePlaneDX[pos_, dy_, PlaneNormalTowards[pos2_]] := Cross[dy, pos2 - pos];

(**************************************************************************************************)
