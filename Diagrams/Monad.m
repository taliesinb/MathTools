LoadShortcuts["Categories"]

(**************************************************************************************************)

$monadToComonad = {$NTmu -> $NTdelta, $NTeta -> $NTeps};
comonadDiagram[name_, opts___] :=
  NamedDiagram[name, opts, ArrowPathReversed -> True, SymbolReplacements -> $monadToComonad];

(**************************************************************************************************)

NamedDiagram["Monad/MultiplicationSquare"] := CommutativeSquare[
	{HCF[$FunT,$FunT,$FunT], HCF[$FunT,$FunT], HCF[$FunT,$FunT], $FunT},
	{HCF[$FunT,$NTmu], $NTmu, HCF[$NTmu, $FunT], $NTmu}
];

NamedDiagram["Monad/ComultiplicationSquare"] :=
  comonadDiagram["Monad/MultiplicationSquare"];

(**************************************************************************************************)

NamedDiagram["Monad/UnitTriangles"] := DoubleTriangleDiagram[
	{$FunT, HCF[$FunT,$FunT], $FunT, $FunT},
	{HCF[$NTeta,$FunT], HCF[$FunT, $NTeta], {None, "Equality"}, Customized[$NTmu, LabelPosition -> Right], {None, "Equality"}}
]

NamedDiagram["Monad/CounitTriangles"] :=
  comonadDiagram["Monad/UnitTriangles"];

(**************************************************************************************************)

NamedDiagram["Monad/MultiplicationSquareImage"] := CommutativeSquare[
	{FunctorPowerForm[$FunT,3][$OX], FunctorPowerForm[$FunT,2][$OX], FunctorPowerForm[$FunT,2][$OX], $FunT[$OX]},
	{$FunT[$NTmu[$OX]], $NTmu[$OX], $NTmu[$FunT[$OX]], $NTmu[$OX]},
	LabelFontSize -> 15
];

NamedDiagram["Monad/ComultiplicationSquareImage"] :=
  comonadDiagram["Monad/MultiplicationSquareImage"];

(**************************************************************************************************)

NamedDiagram["Monad/UnitTrianglesImage"] := DoubleTriangleDiagram[
	{$FunT[$OX], PowerForm[$FunT,2][$OX], $FunT[$OX], $FunT[$OX]},
	{$NTeta[$FunT[$OX]], $FunT[$NTeta[$OX]], {None, "Equality"}, $NTmu[$OX], {None, "Equality"}},
	LabelFontSize -> 15,
	Origin -> {1.75, 0}
]

NamedDiagram["Monad/CounitTrianglesImage"] :=
  comonadDiagram["Monad/UnitTrianglesImage"];

