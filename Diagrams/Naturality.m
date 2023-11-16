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