PrivateVariable[$RulePattern]

$RulePattern = _Rule | _RuleDelayed;

(**************************************************************************************************)

PrivateVariable[$RuleListPattern]

$RuleListPattern = {RepeatedNull[_Rule | _RuleDelayed]};

(**************************************************************************************************)

PrivateVariable[$SymbolicSizePattern]

$SymbolicSizePattern = Tiny | Small | MediumSmall | Medium | MediumLarge | Large | Huge;

SetUsage @ "
$SymbolicSizePattern is a pattern that matches a symbolic size like Small, Medium, etc.
"

(**************************************************************************************************)

PrivateVariable[$SizePattern]

$SizePattern = Tiny | Small | MediumSmall | Medium | MediumLarge | Large | Huge | Scaled[_?NumericQ];

SetUsage @ "
$SizePattern is a pattern that matches a numeric or symbolic size like Small, Medium, Scaled[n$], etc.
"

(**************************************************************************************************)

PrivateVariable[$SidePattern]

$SidePattern = Left | Right | Bottom | Top | BottomLeft | BottomRight | TopLeft | TopRight;

SetUsage @ "
$SizePattern is a pattern that matches a (potentially compound) symbol side, like Left, Top, or TopLeft.
"

(**************************************************************************************************)

PrivateVariable[$ColorPattern]

$ColorPattern = _RGBColor | _GrayLevel | _CMYKColor | _Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor | Opacity[_, _];

SetUsage @ "
$ColorPattern is a pattern that matches a valid color, like %RGBColor[$$] etc.
"

(**************************************************************************************************)

PrivateVariable[$OpacityPattern]

$OpacityPattern = VeryTransparent | HalfTransparent | PartlyTransparent | Opaque | Opacity[_ ? NumericQ];

SetUsage @ "
$OpacityPattern is a pattern that matches an opacity specification.
"

(**************************************************************************************************)

PrivateFunction[ListOrAssociationOf]

ListOrAssociationOf[pattern_] := {Repeated[pattern]} | Association[Repeated[_ -> pattern]];

(**************************************************************************************************)

PrivateVariable[$ModIntP]

$ModIntP = _Integer ? Positive | Modulo[_Integer ? Positive];

(**************************************************************************************************)

PrivateVariable[$PosIntOrInfinityP]

$PosIntOrInfinityP = _Integer ? Positive | Infinity;

(**************************************************************************************************)

PrivateVariable[$NumberP, $CoordP, $Coord2P, $Coord3P, $CoordPairP, $Coord2PairP, $Coord3PairP]

(* TODO: $Coord2VectorP, etc *)

$NumberP = _ ? NumberQ;

$CoordP = {_ ? NumberQ, _ ? NumberQ} | {_ ? NumberQ, _ ? NumberQ, _ ? NumberQ};
$Coord2P = {_ ? NumberQ, _ ? NumberQ};
$Coord3P = {_ ? NumberQ, _ ? NumberQ, _ ? NumberQ};

$CoordPairP = _List ? CoordinatePairQ;
$Coord2PairP = {$Coord2P, $Coord2P};
$Coord3PairP = {$Coord3P, $Coord3P};

(**************************************************************************************************)

PrivateVariable[$CoordMatP, $CoordMaybeMatP, $CoordMatsP, $CoordMaybeMatsP]

$CoordMatP = _List ? CoordinateMatrixQ;
$CoordMaybeMatP = _List ? CoordinateVectorOrMatrixQ;

$CoordMatsP = _List ? CoordinateMatricesQ;
$CoordMaybeMatsP = _List ? CoordinateMatrixOrMatricesQ;

(**************************************************************************************************)

PrivateVariable[$ArcP, $RegionP, $AnnotationP]

headToPattern[e_] := Apply[Alternatives, Blank /@ e];

$ArcP = headToPattern @ {
  Line, InfiniteLine, HalfLine, Arrow, FilledCurve, BezierCurve, BSplineCurve
};

$RegionP = headToPattern @ {
  Cone, FilledTorus, Torus, Cylinder, CapsuleShape,
  Dodecahedron, Icosahedron, Tetrahedron, Octahedron, Hexahedron,
  Parallelepiped, Prism, Pyramid, Simplex,
  Cuboid, Cube
};

(* TODO: handle all wrappers *)
$AnnotationP = headToPattern @ {Annotation, Tooltip, EventHandler, ClickForm};
