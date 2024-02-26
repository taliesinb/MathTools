PrivateExprPattern[$RulePattern]

SetUsage @ "
$RulePattern is a pattern that matches a rule or a delayed rule.
"

DefinePatternMacro[$RulePattern, _Rule | _RuleDelayed];

(**************************************************************************************************)

PrivateExprPattern[$RuleListPattern]

SetUsage @ "
$RuleListPattern is a pattern that matches a possibly empty list of rules or delayed rules.
"

DefinePatternMacro[$RuleListPattern, {RepeatedNull[_Rule | _RuleDelayed]}];

(**************************************************************************************************)

PrivateExprPattern[$SymbolicSizePattern]

SetUsage @ "
$SymbolicSizePattern is a pattern that matches a symbolic size like Small, Medium, etc.
"

DefinePatternMacro[$SymbolicSizePattern, Tiny | Small | MediumSmall | Medium | MediumLarge | Large | Huge];

(**************************************************************************************************)

PrivateExprPattern[$SizePattern]

SetUsage @ "
$SizePattern is a pattern that matches a numeric or symbolic size like Small, Medium, Scaled[n$], etc.
"

DefinePatternMacro[$SizePattern, Tiny | Small | MediumSmall | Medium | MediumLarge | Large | Huge | Scaled[_?NumericQ]];

(**************************************************************************************************)

PrivateExprPattern[$SidePattern]

SetUsage @ "
$SizePattern is a pattern that matches a (potentially compound) symbol side, like Left, Top, or TopLeft.
"

DefinePatternMacro[$SidePattern, Left | Right | Bottom | Top | BottomLeft | BottomRight | TopLeft | TopRight];

(**************************************************************************************************)

PrivateExprPattern[$ColorPattern]

SetUsage @ "
$ColorPattern is a pattern that matches a valid color, like %RGBColor[$$] etc.
"

DefinePatternMacro[$ColorPattern, _RGBColor | _GrayLevel | _CMYKColor | _Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor | Opacity[_, _]];

(**************************************************************************************************)

PrivateExprPattern[$OpacityPattern]

SetUsage @ "
$OpacityPattern is a pattern that matches an opacity specification.
"

DefinePatternMacro[$OpacityPattern, VeryTransparent | HalfTransparent | PartlyTransparent | Opaque | Opacity[_ ? NumericQ]];

(**************************************************************************************************)

PrivateExprPattern[$NotebookOrPathP, $PathP]

DefinePatternMacro[$NotebookOrPathP, _NotebookObject | File[_Str] | _Str];
DefinePatternMacro[$PathP, File[_Str] | _Str];

(**************************************************************************************************)

PrivateExprPattern[SMatchP, SFreeP, SContainsP, SStartsP, SEndsP]

DefineSimpleMacro[   SMatchP,    SMatchP[p_] :> _Str ? (SMatchQ[p])];
DefineSimpleMacro[    SFreeP,     SFreeP[p_] :> _Str ? (SFreeQ[p])];
DefineSimpleMacro[SContainsP, SContainsP[p_] :> _Str ? (SContainsQ[p])];
DefineSimpleMacro[  SStartsP,   SStartsP[p_] :> _Str ? (SStartsQ[p])];
DefineSimpleMacro[    SEndsP,     SEndsP[p_] :> _Str ? (SEndsQ[p])];

(**************************************************************************************************)

PrivateFunction[ListOrAssociationOf]

ListOrAssociationOf[pattern_] := {Repeated[pattern]} | Assoc[Repeated[_ -> pattern]];

(**************************************************************************************************)

PrivateExprPattern[$ModIntP]

DefinePatternMacro[$ModIntP, _Int ? Positive | Modulo[_Int ? Positive]];

(**************************************************************************************************)

PrivateExprPattern[$PosIntOrInfinityP]

DefinePatternMacro[$PosIntOrInfinityP, _Int ? Positive | Inf];

(**************************************************************************************************)

PrivateExprPattern[$NumberP, $ExtCoordP, $CoordP, $Coord2P, $Coord3P, $CoordPairP, $Coord2PairP, $Coord3PairP]

(* TODO: $Coord2VectorP, etc *)

DefinePatternMacro[$NumberP, _ ? NumericQ];

DefinePatternMacro[$CoordP, {_ ? NumericQ, _ ? NumericQ} | {_ ? NumericQ, _ ? NumericQ, _ ? NumericQ}];
DefinePatternMacro[$Coord2P, {_ ? NumericQ, _ ? NumericQ}];
DefinePatternMacro[$Coord3P, {_ ? NumericQ, _ ? NumericQ, _ ? NumericQ}];

DefinePatternMacro[$ExtCoordP, $CoordP | _Offset];

DefinePatternMacro[$CoordPairP, _List ? CoordinatePairQ];
DefinePatternMacro[$Coord2PairP, {$Coord2P, $Coord2P}];
DefinePatternMacro[$Coord3PairP, {$Coord3P, $Coord3P}];

(**************************************************************************************************)

PrivateExprPattern[$CoordMatP, $CoordMat2P, $CoordMat3P, $CoordMaybeMatP, $CoordMatsP, $CoordMaybeMatsP]

DefinePatternMacro[$CoordMat2P, _List ? CoordinateMatrix2DQ];
DefinePatternMacro[$CoordMat3P, _List ? CoordinateMatrix3DQ];

DefinePatternMacro[$CoordMatP, _List ? CoordinateMatrixQ];
DefinePatternMacro[$CoordMaybeMatP, _List ? CoordinateVectorOrMatrixQ];

DefinePatternMacro[$CoordMatsP, _List ? CoordinateMatricesQ];
DefinePatternMacro[$CoordMaybeMatsP, _List ? CoordinateMatrixOrMatricesQ];

(**************************************************************************************************)

PrivateExprPattern[$ArcP, $RegionP, $AnnotationP]

headToPattern[e_] := Apply[Alt, Blank /@ e];

(* use the graphics primitive registry for this! *)
DefinePatternMacro[$ArcP, headToPattern @ {
  Line, InfiniteLine, HalfLine, Arrow, FilledCurve, BezierCurve, BSplineCurve
}];

DefinePatternMacro[$RegionP, headToPattern @ {
  Cone, FilledTorus, Torus, Cylinder, CapsuleShape,
  Dodecahedron, Icosahedron, Tetrahedron, Octahedron, Hexahedron,
  Parallelepiped, Prism, Pyramid, Simplex,
  Cuboid, Cube
}];

(* TODO: handle all wrappers *)
DefinePatternMacro[$AnnotationP, headToPattern @ {Annotation, Tooltip, EventHandler, ClickForm}];