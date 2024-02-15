LoadShortcuts["Categories"]

(**************************************************************************************************)

NamedDiagram["Pullback/PullbackSquare"] :=
  PullbackSquare[{$OP,$OY,$OX,$OZ}, {$Ap2,$Af,$Ap1,$Ag}];

(**************************************************************************************************)

NamedDiagram["Pullback/PullbackSquareUniversalProperty"] := PullbackSquare[
  {$OP,$OY,$OX,$OZ, {1,1}/2 -> $OQ},
  {$Ap2,$Af,$Ap1,$Ag,
   UniqueMorphism[{"Q", "P"}, Setback -> 15],
   Morphism[ElbowCurve[{"Q", "X"}, -.15], $Aq1], Morphism[ElbowCurve[{"Q", "Y"}, .15], $Aq2]}
];

(**************************************************************************************************)

NamedDiagram["Pullback/PushoutSquare"] :=
  PullbackSquare[{$OP,$OY,$OX,$OZ}, {$Ap2,$Af,$Ap1,$Ag}, ArrowPathReversed->True]

(**************************************************************************************************)

NamedDiagram["Pullback/PushoutSquareUniversalProperty"] := PullbackSquare[
  {$OP,$OY,$OX,$OZ,{1,1}/2 -> $OQ},
  {$Ap2,$Af,$Ap1,$Ag, UniqueMorphism[{"Q", "P"}, Setback -> 15],
   Morphism[ElbowCurve[{"Q", "X"}, -.15], $Aq1], Morphism[ElbowCurve[{"Q", "Y"}, .15], $Aq2]},
   ArrowPathReversed->True
];