LoadShortcuts["Categories"]

(**************************************************************************************************)

NamedDiagram["KanExtension/AdjointTriple"] := AdjointTripleDiagram[
  {CompactFunctorCategoryForm[$CD,$CC], CompactFunctorCategoryForm[$CE,$CC]},
  {LeftKanExtensionForm[$FunF], RightKanExtensionForm[$FunF], CompactFunctorCategoryForm[$FunF,$CC],
  ArrowDiagram[{2,1} -> $CD, $CE, $FunF]
  },LabelFontSize -> 16, "AdjointSetback" -> 8
]

(**************************************************************************************************)

NamedDiagram["KanExtension/DiagramAdjointTriple"] := AdjointTripleDiagram[
  {CompactFunctorCategoryForm[$CD,$CC], CompactFunctorCategoryForm[1,$CC]},
  {LeftKanExtensionForm[$FunP], RightKanExtensionForm[$FunP], CompactFunctorCategoryForm[$FunP,$CC],
  ArrowDiagram[{2,1} -> $CD, 1, $FunP]
  },LabelFontSize -> 16, "AdjointSetback" -> 8
]

(**************************************************************************************************)

NamedDiagram["KanExtension/KanExtension"] := InTriangleDiagram[
  {$CD, $CC, $CE},
  {$FunD, $FunF, Reversed @ UniqueMorphism @ $FunE}, Left
]

(**************************************************************************************************)

NamedDiagram["KanExtension/Left"] := InTriangleDiagram[
  {$CD, $CC, $CDpr},
  {$FunF, $FunP, Reversed @ LeftKanExtensionForm[$FunP][$FunF],
   DoubleMorphism["F" \[DirectedEdge] "D'", $NTeta, Setback -> {15,30}]}
]

(**************************************************************************************************)

NamedDiagram["KanExtension/Right"] := InTriangleDiagram[
  {$CD, $CC, $CDpr},
  {$FunF, $FunP, Reversed @ RightKanExtensionForm[$FunP][$FunF],
   DoubleMorphism["D'" \[DirectedEdge] "F", $NTeps, Setback -> {30,15}]}
]

(**************************************************************************************************)

NamedDiagram["KanExtension/LeftUniversalProperty"] := InTriangleDiagram[
  {$CD, $CC, $CE},
  {$FunF, $FunP, Reversed @ UniqueMorphism[$FunE, LabelPosition -> Above],
   Morphism[ElbowCurve[{"E", "C"}, -.3], $FunEpr],
   DoubleMorphism[2 \[DirectedEdge] 4, Setback -> 5], Text["left", {1.5,-2.25}]},
  Left
]

(**************************************************************************************************)

NamedDiagram["KanExtension/RightUniversalProperty"] := InTriangleDiagram[
  {$CD, $CC, $CE},
  {$FunF, $FunP, Reversed @ UniqueMorphism[$FunE, LabelPosition -> Above],
   Morphism[ElbowCurve[{"E", "C"}, -.3], $FunEpr],
   DoubleMorphism[4 \[DirectedEdge] 2, Setback -> 5], Text["right", {1.5,-2.25}]},
  Left, Origin -> {1.75, 0}
]