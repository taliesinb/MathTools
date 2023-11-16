LoadShortcuts["Categories"]

(**************************************************************************************************)

NamedDiagram["MonadAlgebra/Square"] := CommutativeSquare[
  {$FunF[$OX], $FunF[$OY], $OX, $OY},
  {$FunF[$Af], $Af, $FunA, $FunB}
]

(**************************************************************************************************)

NamedDiagram["MonadAlgebra/SquareRainbow"] := CommutativeSquare[
  {$FunT[$Ox], $FunT[$Oy], $Ox, $Oy},
  {$FunT[$Af], $Af, $Ah, $Aj},
  ColorRules -> {$Ox -> RedForm, $Oy -> BlueForm, $Af -> DarkPinkForm}
]

(**************************************************************************************************)

NamedDiagram["MonadAlgebra/MultiplicationSquare"] := CommutativeSquare[
    {FunctorPowerForm[$FunT, 2] @ $Ox, $FunT @ $Ox, $FunT @ $Ox, $Ox},
    {$FunT @ $Ah, $Ah, $NTmu[$Ox], $Ah}, LabelFontSize -> 18
  ]

(**************************************************************************************************)

NamedDiagram["MonadAlgebra/UnitTriangle"] := InTriangleDiagram[
    {$Ox, $FunT[$Ox], $Ox},
    {$NTeta[$Ox], EqualityMorphism[], $Ah},
    Right, Origin -> {1.5,0}, LabelFontSize -> 18
  ]

