LoadShortcuts["Categories"]

(**************************************************************************************************)

NamedDiagram["Naturality/Square"] := CommutativeSquare[
  {$FunF[$Ox], $FunG[$Ox], $FunF[$Oy], $FunG[$Oy]},
  {$NTeta[$Ox], $NTeta[$Oy], $FunF[$Af], $FunG[$Af],
   ArrowDiagram[{0.3, 1} -> $Ox, $Oy, Morphism[$Af, LabelPosition -> Left]]}
]

NamedDiagram["Naturality/SquareGradient"] := CommutativeSquare[
  {$FunF[$Ox], $FunG[$Ox], $FunF[$Oy], $FunG[$Oy]},
  {$NTeta[$Ox], $NTeta[$Oy], $FunF[$Af], $FunG[$Af],
   ArrowDiagram[{0.3, 1} -> $Ox, $Oy, Morphism[$Af, LabelPosition -> Left]]},
   ColorRules -> "ColoringFunctors",
   MorphismColors -> "Gradient"
]

(**************************************************************************************************)

$colorRules = {
  $Oc -> $DarkBlue, $Od -> $DarkRed, $ff -> $DarkRed, $fg -> $DarkBlue,
  $CC -> 1, $CD -> 2, $CE -> 3,
  $FunL -> {1, 2}, $FunR -> {2, 1},   $NTeta -> $Pink,  $NTeps -> $Pink,
  $FunL1 -> {1, 2}, $FunR1 -> {2, 1}, $NTeps1 -> $Pink, $NTeta1 -> $Pink,
  $FunL2 -> {2, 3}, $FunR2 -> {3, 2}, $NTeps2 -> $Teal, $NTeta2 -> $Teal,
  $FunL3 -> {1, 3}, $FunR3 -> {3, 1}, $NTeps3 -> $Orange, $NTeta3 -> $Orange
};

$adjOpts = Sequence[
  RegionFilling -> "Labeled", ColorRules -> $colorRules,
  WireThickness -> 2, NodeEdgeThickness -> 2, NodeLabelFontSize -> 18, NodeSize -> 21, NodeShape -> "Box",
  TextModifiers -> {Subscript[z_, _] :> z}
];

NamedDiagram["Naturality/SlidingImage"] := FunctorialStringDiagram[
  {{-2,#1} -> $NTeta},
  {Bottom <=> 1 -> $FunF, 1 <=> Top -> $FunG},
  {$Ox, 0.5 -> $Af, $Oy},
  DiagramSize -> {6, 12},
  ColorRules -> {$Ox -> RF, $Af -> RBF, $Oy -> BF}, $Opts, $adjOpts
]& /@ {-5, 5};

NamedDiagram["Naturality/Sliding"] := StringDiagram[
  {{-4, #1} -> $NTalpha, {4, #2} -> $NTbeta},
  {Bottom <=> 1, 1 <=> Top, Bottom <=> 2, 2 <=> Top}, {},
  DiagramSize -> {6, 12}, ImagePadding -> 10, $Opts, $adjOpts
]& @@@ {{-4, 4}, {0, 0}, {4, -4}};
