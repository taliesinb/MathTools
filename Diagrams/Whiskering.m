LoadShortcuts["Categories"]

(**************************************************************************************************)

$FunHF = FunctorSymbol[TightCompositionForm[$FunH, $FunF]];
$FunHG = FunctorSymbol[TightCompositionForm[$FunH, $FunG]];

$FunFH = FunctorSymbol[TightCompositionForm[$FunF, $FunH]];
$FunGH = FunctorSymbol[TightCompositionForm[$FunG, $FunH]];

$NTHeta = NaturalTransformationSymbol[CompositionForm[$FunH, $NTeta]];
$NTetaHc = $NTeta[$FunH[$Oc]];

$whiskeringOpts = Sequence[
  ColorRules -> {
    $Oc -> $Black, $NTeta -> RBF,
    $NTHeta -> $Gray, $NTetaHc -> $Gray,
    $FunF -> RF, $FunG -> BF, $FunH -> GF,
    $FunHF -> RGF, $FunHG -> GBF,
    $FunFH -> RGF, $FunGH -> GBF
  }
];

(**************************************************************************************************)

NamedDiagram["Whiskering/Left"] := CommutativeDiagram[{
  {1,1} -> "C" -> $CC, {2,1} -> "D" -> $CD, {3,1} -> "E" -> $CE,
  Morphism[TrapezoidCurve[{"C", "D"}, .2], $FunF],
  Morphism[TrapezoidCurve[{"C", "D"}, -.2], $FunG],
  Morphism[{"D", "E"}, $FunH],
  DoubleMorphism[{1, 2}, $NTeta, Setback -> 5]},
  LabelFontSize -> 18, LabelPosition -> "Outer", LabelPosition -> AwayFrom[{0,-1}],
  $whiskeringOpts,
  MorphismColors -> Inherited
]

NamedDiagram["Whiskering/Right"] := CommutativeDiagram[{
  {1,1} -> "C" -> $CC, {2,1} -> "D" -> $CD, {3,1} -> "E" -> $CE,
  Morphism[TrapezoidCurve[{"D", "E"}, .2], $FunF],
  Morphism[TrapezoidCurve[{"D", "E"}, -.2], $FunG],
  Morphism[{"C", "D"}, $FunH],
  DoubleMorphism[{1, 2}, $NTeta, Setback -> 5]},
  LabelFontSize -> 18, LabelPosition -> "Outer", LabelPosition -> AwayFrom[{0,-1}],
  $whiskeringOpts,
  MorphismColors -> Inherited
]

(**************************************************************************************************)

NamedDiagram["Whiskering/LeftResult"] := CommutativeDiagram[{
  {1,3} -> "C" -> $CC, {3,3} -> "E" -> $CE,
  MorphismArrow[TrapezoidCurve[{"C","E"}, .3], $FunHF, ArrowColor -> $Orange, LabelPosition -> Above],
  MorphismArrow[TrapezoidCurve[{"C","E"}, -.3], $FunHG, ArrowColor -> $Teal, LabelPosition -> Below],
  DoubleMorphism[{1, 2}, $NTHeta, ArrowColor -> $Gray]},
  MorphismColors -> Inherited, $whiskeringOpts
];

NamedDiagram["Whiskering/RightResult"] := CommutativeDiagram[{
  {1,3} -> "C" -> $CC, {3,3} -> "E" -> $CE,
  MorphismArrow[TrapezoidCurve[{"C","E"}, .3], $FunHF, ArrowColor -> $Orange, LabelPosition -> Above],
  MorphismArrow[TrapezoidCurve[{"C","E"}, -.3], $FunHG, ArrowColor -> $Teal, LabelPosition -> Below],
  DoubleMorphism[{1, 2}, $NTHeta, ArrowColor -> $Gray]},
  MorphismColors -> Inherited, $whiskeringOpts
];

(**************************************************************************************************)

NamedDiagram["Whiskering/LeftImage"] := CommutativeDiagram[{
  {1, 0} -> "c" -> Sized[$Oc, {30, Automatic}],
  {2, -.35} -> "Fc"  -> $FunF[$Oc],
  {3, -.35} -> "HFc" -> $FunHF[$Oc],
  {2, .35}  -> "Gc"  -> $FunG[$Oc],
  {3, .35}  -> "HGc" -> $FunHG[$Oc],

  MapsToMorphism[{"c", "Fc"}, ArrowColor -> $Red],
  MapsToMorphism[{"c", "Gc"}, ArrowColor -> $Blue],

  MapsToMorphism[{"Fc", "HFc"}, ArrowColor -> $Green],
  MapsToMorphism[{"Gc", "HGc"}, ArrowColor -> $Green],

  Morphism[{"Fc", "Gc"}, $NTeta[$Oc], ArrowColor -> $Pink],
  Morphism[{"HFc", "HGc"}, $NTHeta[$Oc], ArrowColor -> $Gray]},
  LabelPosition -> Right,
  $whiskeringOpts
];

NamedDiagram["Whiskering/RightImage"] := CommutativeDiagram[{
  {1, 0}    -> "c"   -> Sized[$Oc, {30, Automatic}],
  {2, 0}    -> "Hc"   -> Sized[$FunH[$Oc], {30, Automatic}],
  {3, -.35} -> "FHc" -> $FunFH[$Oc],
  {3, .35} -> "GHc" -> $FunGH[$Oc],

  MapsToMorphism[{"c", "Hc"}, ArrowColor -> $Green],
  MapsToMorphism[{"Hc", "FHc"}, ArrowColor -> $Red],
  MapsToMorphism[{"Hc", "GHc"}, ArrowColor -> $Blue],

  Morphism[{"FHc", "GHc"}, $NTetaHc, ArrowColor -> $Gray]},
  LabelPosition -> Right,
  $whiskeringOpts
];
