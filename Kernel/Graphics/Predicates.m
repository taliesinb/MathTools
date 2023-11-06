PrivateFunction[GraphicsDirectiveQ]

$directiveHeadsP = Alternatives[
  _Thickness, _AbsoluteThickness, _PointSize, _AbsolutePointSize, _Dashing,
  _EdgeForm, _FaceForm, _Directive, _CapForm, _JoinForm, _Arrowheads
];

GraphicsDirectiveQ = Case[
  $directiveHeadsP                     := True;
  $ColorPattern | _Opacity             := True;
  Directive[__ ? GraphicsDirectiveQ]   := True;
  Directive[{__ ? GraphicsDirectiveQ}] := True;
  _                                    := False;
];

(**************************************************************************************************)

PrivateFunction[GraphicsPrimitivesQ]

$arrowInnerP = _Line | _JoinedCurve | _BezierCurve | _BSplineCurve | _Tube;
$wrapperP = Annotation | EventHandler;

(* TODO: distinguish 3D from 2D primitives *)
(* TODO: introduce CoordinateVector3Q, CoordinateVector2Q, etc *)
GraphicsPrimitivesQ = Case[
  list_List                                         := AllTrue[list, GraphicsPrimitivesQ];
  Style[s_, ___ ? styleElementQ]                    := % @ s;
  anno:$AnnotationP                                 := % @ P1 @ anno;
  Rectangle[$Coord2P]                               := True;
  Rectangle[$Coord2P, $Coord2P, ___Rule]            := True;
  Cube[$Coord3P, ___]                               := True;
  Cuboid[$Coord3P]                                  := True;
  Cuboid[$Coord3P, $Coord3P]                        := True;
  CapsuleShape[$Coord3PairP]                        := True;
  StadiumShape[$Coord2PairP]                        := True;
  Line[$CoordMaybeMatsP]                            := True;
  Arrow[$CoordMaybeMatsP, ___]                      := True;
  Arrow[$arrowInnerP ? GraphicsPrimitivesQ, ___]    := True;
  (HalfLine|InfiniteLine)[$CoordPairP]              := True;
  (HalfLine|InfiniteLine)[$CoordP, $CoordP]         := True;
  (BezierCurve|BSplineCurve)[$CoordMaybeMatsP, ___] := True;
  (JoinedCurve|FilledCurve)[list_List, ___]         := % @ list; (* <- TODO: Fix these, these aren't correct! *)
  (Circle|Disk)[$Coord2P, ___]                      := True;
  (Ball|Sphere)[$CoordMaybeMatsP, ___]              := True;
  Annulus[$Coord2P, _]                              := True;
  Cone[$Coord3PairP, ___]                           := True;
  Tube[$CoordMaybeMatsP, __]                        := True;
  CenteredRectangle[$Coord2P, _]                    := True;
  CenteredCuboid[$Coord3P, _]                       := True;
  Point[$CoordMaybeMatP]                            := True;
  (Polygon|Polyhedron)[$CoordMaybeMatsP]            := True;
  (Polygon|Polyhedron)[_, $CoordMaybeMatsP]         := True;
  (Text|Inset)[_, $CoordP, ___]                     := True;
  $ColorPattern | _Opacity                          := True;
  head_Symbol[___] /; TrueQ[$customGraphicsHeadQ @ head] := True;
  e_                                               := GraphicsDirectiveQ[e]
];

styleElementQ = Case[
  None                   := True;
  _ ? GraphicsDirectiveQ := True;
  _                      := False;
]

(**************************************************************************************************)

PrivateFunction[GraphicsQ]

SetUsage @ "
GraphicsQ[object$] returns True if object$ is a %Graphics[$$] or %Graphics3D[$$] expression.
"

GraphicsQ[_Graphics | _Graphics3D] := True;
GraphicsQ[_] := False;
