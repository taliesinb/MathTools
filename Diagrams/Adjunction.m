LoadShortcuts["Categories"]

(**************************************************************************************************)

NamedDiagram["Adjunction/Pair"] := AdjunctionDiagram[{$CC, $CD}, {$FunL, $FunR}]

(**************************************************************************************************)

NamedDiagram["Adjunction/PairUnitCounit"] := AdjunctionDiagram[
  {{1,1} -> $CC, {3, 1} -> $CD},
  {$FunL, $FunR,
   {2,1} -> "M" -> None,
   LabelPosition -> {0, -0.6},
   Morphism[{1, "M"}, {1.15 -> $NTeta}, "DoubleArrow", Setback -> {15, 50}],
   Morphism[{"M", 2}, {-0.15 -> $NTeps}, "DoubleArrow", Setback -> {50, 15}]}
]

(**************************************************************************************************)

NamedDiagram["Adjunction/LeftUnitCounitFunctorTriangle"] := InTriangleDiagram[
  {$FunL, CompositionForm[$FunL, $FunR, $FunL], $FunL},
  {HC[OneArrow[$FunL], $NTeta], EqualityMorphism[], HC[$NTeps, OneArrow @ $FunL]},
  Right
]

NamedDiagram["Adjunction/RightUnitCounitFunctorTriangle"] := InTriangleDiagram[
  {CompositionForm[$FunR, $FunL, $FunR], $FunR, $FunR},
  {Reversed @ HC[$NTeta, OneArrow[$FunR]], HC[OneArrow @ $FunR, $NTeps], EqualityMorphism[]},
  Left, Origin -> {2, 0}
]

(**************************************************************************************************)

gluedTriangles[a_, b_, f_, g_, x_, y_, revDbl_, opts___Rule] := With[
  {rev = If[revDbl, Reverse, Identity], lpos = If[revDbl, 0.1, 0.5]}, CommutativeDiagram[{
  {1,1} -> "C1" -> a, {2,1} -> "D1" -> b, {2,2} -> "C2" -> a, {3, 2} -> "D2" -> b,
  EqualityMorphism[{1, 3}], EqualityMorphism[{2, 4}],
  Morphism[{1, 2}, f], LabelPosition -> BottomRight,  Morphism[{2, 3}, g], Morphism[{3, 4}, f], LabelSpacing -> 0,
  DoubleMorphism[rev @ {MorphismCoordinates[1], ObjectCoordinates[2]}, {lpos -> x}, Setback -> rev[{8, 40}]],
  DoubleMorphism[rev @ {ObjectCoordinates[3], MorphismCoordinates[2]}, {lpos -> y}, Setback -> rev[{40, 8}]]},
  opts
]];

filledSquare[a_, b_, f_, isRev_, opts___Rule] := CommutativeDiagram[{
  {1,1} -> "C" -> a, {2,1} -> "C2" -> None, {2,2} -> "D2" -> None, {3, 2} -> "D" -> b},
  {Morphism[RollingCurve @ {"C", "C2", "D"}, f],
   Morphism[RollingCurve @ {"C", "D2", "D"}, f],
   DoubleMorphism[HorizontalCurve[ObjectCoordinates["Center"], {-1,1}*.4*If[isRev, -1, 1]], IdentityFunction,
   LabelPosition -> Above]},
  LabelPosition -> Outwards, opts
];

NamedDiagram["Adjunction/LeftGluedTriangles"]  := gluedTriangles[$OC, $OD, $FunL, $FunR, $NTeta, $NTeps, False, $Opts];
NamedDiagram["Adjunction/RightGluedTriangles"] := gluedTriangles[$OD, $OC, $FunR, $FunL, $NTeps, $NTeta, True, $Opts];

NamedDiagram["Adjunction/LeftFilledSquare"]  := filledSquare[$OC, $OD, $FunL, False, Origin -> {3, 0}, $Opts];
NamedDiagram["Adjunction/RightFilledSquare"] := filledSquare[$OD, $OC, $FunR, True, Origin -> {3, 0}, $Opts];

(**************************************************************************************************)

NamedDiagram["Adjunction/LeftTriangleImage"] := OutTriangleDiagram[
  {$FunL[$Oc], $FunL[$FunR[$Od]], $Od},
  {UniqueMorphism @ $FunL[$Ag], $Af, Morphism[$NTeps[$Od], LabelPosition -> Top],
   ArrowDiagram[{0.5,1}-> $Oc, $FunR[$Od], UniqueMorphism[$Ag, LabelPosition -> Left]]}, Left
];

NamedDiagram["Adjunction/RightTriangleImage"] := InTriangleDiagram[
  {$Oc, $FunR[$FunL[$Oc]], $FunR[$Od]},
  {Morphism[$NTeps[$Od], LabelPosition -> Bottom], $Ag, UniqueMorphism @ $FunR[$Af],
  ArrowDiagram[{2.75,1} -> $FunL[$Oc], $Od, $Af]}, Right
]

(**************************************************************************************************)

NamedDiagram["Adjunction/LimitColimit"] := AdjointTripleDiagram[
  {CompactFunctorCategoryForm[$CD,$CC], $CC},
  {Padded[ColimitFunction, {0,1}], LimitFunction, DiagonalFunctorForm[$CD]}
];

(**************************************************************************************************)

NamedDiagram["Adjunction/Composition"] := CommutativeDiagram[{
  AdjunctionDiagram[{$CC, $CD}, {$FunL, $FunR}],
  AdjunctionDiagram[{Invisible @ $CD, $CE}, {$FunLpr, $FunRpr}, Origin -> {1, 0}]
}]

(**************************************************************************************************)

$funL12 = DRGF @ CompositionForm[$FunL1, $FunL2];
$funR21 = DRGF @ CompositionForm[$FunR2, $FunR1];

$colorRules = {
  $Oc -> $DarkBlue, $Od -> $DarkRed, $Af -> $DarkRed, $Ag -> $DarkBlue,
  $CC -> 1, $CD -> 2, $CE -> 3,
  $FunL -> {1, 2}, $FunR -> {2, 1},   $NTeta -> $Pink,  $NTeps -> $Pink,
  $FunL1 -> {1, 2}, $FunR1 -> {2, 1}, $NTeps1 -> $Pink, $NTeta1 -> $Pink,
  $FunL2 -> {2, 3}, $FunR2 -> {3, 2}, $NTeps2 -> $Teal, $NTeta2 -> $Teal,
  $FunL3 -> {1, 3}, $FunR3 -> {3, 1}, $NTeps3 -> $Orange, $NTeta3 -> $Orange
};

$adjOpts := Sequence[
  RegionFilling -> "Labeled", ColorRules -> $colorRules,
  WireThickness -> 2, NodeEdgeThickness -> 2, NodeLabelFontSize -> 18, NodeSize -> 21, NodeShape -> "Box",
  TextModifiers -> {Subscript[z_, _] :> z}
];

stringWire[c_, d_, l_, opts___] := StringDiagram[{}, {Bottom <=> Top -> l}, {{-4.5, 0} -> c, {4.5, 0} -> d}, DiagramSize -> {9, 12}, opts, $adjOpts];
stringEta[c_, d_, l_, r_, eta_, opts___] := StringDiagram[{{0,0} -> eta}, {1 <=> {Top,-6} -> r, 1 <=> {Top,6} -> l}, {Bottom -> d, Top -> c}, opts, $adjOpts];
stringEps[c_, d_, l_, r_, eps_, opts___] := StringDiagram[{{0,0} -> eps}, {1 <=> {Bottom,-6} -> l, 1 <=> {Bottom,6} -> r}, {Bottom -> d, Top -> c}, opts, $adjOpts];

stringMonadPanel[c_, d_, l_, r_, eta_, eps_, opts___] := List[
  stringWire[c, d, l, opts],
  stringWire[d, c, r, opts],
  stringEta[c, d, l, r, eta, opts],
  stringEps[c, d, l, r, eps, opts]
];

(**************************************************************************************************)

NamedDiagram["Adjunction/StringMonadPanel"] :=
  stringMonadPanel[$CC, $CD, $FunL, $FunR, $NTeta, $NTeps];

(**************************************************************************************************)

NamedDiagram["Adjunction/StringL"] := stringWire[$CC, $CD, $FunL];
NamedDiagram["Adjunction/StringR"] := stringWire[$CD, $CC, $FunR];

NamedDiagram["Adjunction/StringLRL"] := StringDiagram[{
  {-4,5} -> $NTeps, {4,-5} -> $NTeta},
  {1 <=> {Bottom,-8} -> $FunL,
   Customized[1 <=> 2 -> $FunR, LabelPosition -> Left],
   2 <=> {Top,8} -> $FunL},
  {{-11,-11} -> Red, {11,11} -> Blue}, $Opts, $adjOpts
];

NamedDiagram["Adjunction/StringRLR"] := StringDiagram[
  {{4,5} -> $NTeps, {-4,-5} -> $NTeta},
  {1 <=> {Bottom,8} -> $FunR,
   Customized[1 <=> 2 -> $FunL, LabelPosition -> Right],
   2 <=> {Top,-8} -> $FunR
  },
  {{11,-11} -> Red, {-11,11} -> Blue}, $Opts, $adjOpts
];

(**************************************************************************************************)

NamedDiagram["Adjunction/StringLImage"] := FunctorialStringDiagram[
  {},
  {{Bottom,0} <=> 1 -> $FunL},
   {$Oc, 0.5 -> $Af, $Od},
  {BottomRight -> $CD, BottomLeft -> $CC},
  $Opts,
  $adjOpts
]

(**************************************************************************************************)

NamedDiagram["Adjunction/StringLRImage"] := FunctorialStringDiagram[
  {{0, 7} -> $NTeps},
  {Bottom <=> 1 -> $FunL,
   1 <=> "g" -> Customized[$FunR, LabelPosition -> Left]},
  {$Oc, 0.5 -> $Ag, $Od},
  {BottomRight -> $CD, BottomLeft -> $CC},
  $Opts, $adjOpts
]

(**************************************************************************************************)

NamedDiagram["Adjunction/StringRImage"] := FunctorialStringDiagram[
  {},
  {{Top, 0} <=> "g" -> $FunR},
   {$Oc, 0.5 -> $Ag, $Od},
  {TopLeft -> $CD, TopRight -> $CC},
  $Opts, $adjOpts
];

(**************************************************************************************************)

NamedDiagram["Adjunction/StringRLImage"] := FunctorialStringDiagram[
  {{0, -7} -> $NTeps},
  {{Top,0} <=> 1 -> $FunR,
   Customized[1 <=> 2 -> $FunL, LabelPosition -> Right]},
  {$Oc, 0.5 -> $Af, $Od},
  {TopLeft -> $CD, TopRight -> $CC},
  $Opts, $adjOpts
];

(**************************************************************************************************)

stringTwoWires[c_, d_, e_, l1_, l2_, opts___] :=
  StringDiagram[{},
    {{Bottom,3} <=> Top -> l2, {Bottom,-3} <=> Top -> l1},
    {Left -> c, Center -> d, Right -> e}, DiagramSize -> {9, 12},
    opts, $adjOpts];

stringNestedMu[c_, d_, e_, l1_, l2_, r1_, r2_, eta1_, eta2_, opts___] :=
  StringDiagram[
    {{0,4.5} -> eta1, {0, -4.5} -> eta2},
    {2 <=> {Top,-8} -> r2,
     1 <=> {Top,-4} -> r1,
     1 <=> {Top,4} -> l1,
     2 <=> {Top,8} -> l2},
    {{0, -9} -> e, Center -> d, {0, 9} -> c},
    opts, $adjOpts];

stringComposedMonadPanel[c_, d_, e_, l1_, l2_, r1_, r2_, eta1_, eta2_, eps1_, eps2_, opts___] := {
  stringTwoWires[c, d, e, l1, l2, opts],
  stringTwoWires[e, d, c, r2, r1, opts],
  stringNestedMu[c, d, e, l1, l2, r1, r2, eta1, eta2, opts],
  stringNestedMu[c, d, e, r2, r1, l2, l1, eps2, eps1, FlipY -> True, opts]
};

NamedDiagram["Adjunction/StringMonadPanel1"] :=
  stringMonadPanel[$CC, $CD, $FunL1, $FunR1, $NTeta1, $NTeps1];

NamedDiagram["Adjunction/StringMonadPanel2"] :=
  stringMonadPanel[$CD, $CE, $FunL2, $FunR2, $NTeta2, $NTeps2];

NamedDiagram["Adjunction/StringMonadPanel12"] :=
  stringComposedMonadPanel[$CC, $CD, $CE, $FunL1, $FunL2, $FunR1, $FunR2, $NTeta1, $NTeta2, $NTeps1, $NTeps2];

NamedDiagram["Adjunction/StringMonadPanel12Result"] :=
  stringMonadPanel[$CC, $CE, $funL12, $funR21, $NTeta3, $NTeps3];

(**************************************************************************************************)

NamedDiagram["Adjunction/ComposedPairs"] := CommutativeDiagram[{
  AdjunctionDiagram[{$CC, $CD}, {$FunL, $FunR}],
  AdjunctionDiagram[{Invisible @ $CD, $CE}, {$FunLpr, $FunRpr}, Origin -> {1, 0}]
}]

(**************************************************************************************************)

NamedDiagram["Adjunction/Correspondence"] := Style[DoubleFractionForm[
  ArrowSignatureForm[$Af,$FunF[$OX], $OY],
  ArrowSignatureForm[$Ag,$OX, $FunG[$OY]]
], 20];
