(**************************************************************************************************)

PackageExport["$RulePattern"]

$RulePattern = _Rule | _RuleDelayed;

(**************************************************************************************************)

PackageExport["$RuleListPattern"]

$RuleListPattern = {RepeatedNull[_Rule | _RuleDelayed]};

(**************************************************************************************************)

PackageExport["$SymbolicSizePattern"]

$SymbolicSizePattern = Tiny | Small | MediumSmall | Medium | MediumLarge | Large | Huge;

SetUsage @ "
$SymbolicSizePattern is a pattern that matches a symbolic size like Small, Medium, etc.
"

(**************************************************************************************************)

PackageExport["$SizePattern"]

$SizePattern = Tiny | Small | MediumSmall | Medium | MediumLarge | Large | Huge | Scaled[_?NumericQ];

SetUsage @ "
$SizePattern is a pattern that matches a numeric or symbolic size like Small, Medium, Scaled[n$], etc.
"

(**************************************************************************************************)

PackageExport["$SidePattern"]

$SidePattern = Left | Right | Bottom | Top | BottomLeft | BottomRight | TopLeft | TopRight;

SetUsage @ "
$SizePattern is a pattern that matches a (potentially compound) symbol side, like Left, Top, or TopLeft.
"

(**************************************************************************************************)

PackageExport["$ColorPattern"]

$ColorPattern = _RGBColor | _GrayLevel | _CMYKColor | _Hue | _XYZColor | _LABColor | _LCHColor | _LUVColor | Opacity[_, _];

SetUsage @ "
$ColorPattern is a pattern that matches a valid color, like %RGBColor[$$] etc.
"

(**************************************************************************************************)

PackageExport["$OpacityPattern"]

$OpacityPattern = VeryTransparent | HalfTransparent | PartlyTransparent | Opaque | Opacity[_ ? NumericQ];

SetUsage @ "
$OpacityPattern is a pattern that matches an opacity specification.
"

(**************************************************************************************************)

PackageScope["ListOrAssociationOf"]

ListOrAssociationOf[pattern_] := {Repeated[pattern]} | Association[Repeated[_ -> pattern]];

(**************************************************************************************************)

PackageScope["$ModIntP"]

$ModIntP = _Integer ? Positive | Modulo[_Integer ? Positive];
