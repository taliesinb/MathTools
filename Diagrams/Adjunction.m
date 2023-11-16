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
  {HCF[OneArrow[$FunL], $NTeta], EqualityMorphism[], HCF[$NTeps, OneArrow @ $FunL]},
  Right
]

NamedDiagram["Adjunction/RightUnitCounitFunctorTriangle"] := InTriangleDiagram[
  {CompositionForm[$FunR, $FunL, $FunR], $FunR, $FunR},
  {Reversed @ HCF[$NTeta, OneArrow[$FunR]], HCF[OneArrow @ $FunR, $NTeps], EqualityMorphism[]},
  Left, Origin -> {2, 0}
]

(**************************************************************************************************)

gluedTriangles[a_, b_, f_, g_, x_, y_, revDbl_] := With[
  {rev = If[revDbl, Reverse, Identity], lpos = If[revDbl, 0.4, 0.5]}, CommutativeDiagram[{
  {1,1} -> "C1" -> a, {2,1} -> "D1" -> b, {2,2} -> "C2" -> a, {3, 2} -> "D2" -> b,
  EqualityMorphism[{1, 3}], EqualityMorphism[{2, 4}],
  Morphism[{1, 2}, f], LabelPosition -> BottomRight,  Morphism[{2, 3}, g], Morphism[{3, 4}, f], LabelSpacing -> 0,
  DoubleMorphism[rev @ {MorphismCoordinates[1], ObjectCoordinates[2]}, {lpos -> x}, Setback -> rev[{8, 16}]],
  DoubleMorphism[rev @ {ObjectCoordinates[3], MorphismCoordinates[2]}, {lpos -> y}, Setback -> rev[{16, 8}]]}
]];

filledSquare[a_, b_, f_, isRev_, opts___Rule] := CommutativeDiagram[{
  {1,1} -> "C" -> a, {2,1} -> "C2" -> None, {2,2} -> "D2" -> None, {3, 2} -> "D" -> b},
  {Morphism[RollingCurve @ {"C", "C2", "D"}, f],
   Morphism[RollingCurve @ {"C", "D2", "D"}, f],
   DoubleMorphism[HorizontalCurve[ObjectCoordinates["Center"], {-1,1}*.4*If[isRev, -1, 1]], IdentityFunction,
   LabelPosition -> Above]},
  LabelPosition -> Outwards, opts
];

NamedDiagram["Adjunction/LeftGluedTriangles"]  := gluedTriangles[$OC, $OD, $FunL, $FunR, $NTeta, $NTeps, False];
NamedDiagram["Adjunction/RightGluedTriangles"] := gluedTriangles[$OD, $OC, $FunR, $FunL, $NTeps, $NTeta, True];

NamedDiagram["Adjunction/LeftFilledSquare"]  := filledSquare[$OC, $OD, $FunL, False, Origin -> {3, 0}];
NamedDiagram["Adjunction/RightFilledSquare"] := filledSquare[$OD, $OC, $FunR, True, Origin -> {3, 0}];

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