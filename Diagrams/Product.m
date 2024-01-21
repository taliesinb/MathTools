LoadShortcuts["Categories"]

(**************************************************************************************************)

NamedDiagram["Product/Product"] := DoubleTriangleDiagram[
  {$OL,CartesianProductForm[$OL,$OR],$OR, $OX},
  {Reversed @ CategoryArrowSymbol[Subscript["p", $sl]],
  Reversed @ CategoryArrowSymbol[Subscript["p", $sr]],
  Subscript[$Af, $sl],
  UniqueMorphism @ $Af,
  Subscript[$Af, $sr]},
  Top
]

(**************************************************************************************************)

NamedDiagram["Product/Coproduct"] := DoubleTriangleDiagram[
  {$OL,CartesianSumForm[$OL,$OR],$OR, $OX},
  {CategoryArrowSymbol[Subscript["i", $sl]],
   CategoryArrowSymbol[Subscript["i", $sr]],
  Reversed @ Subscript[$Af, $sl],
  Reversed @ UniqueMorphism @ $Af,
  Reversed @ Subscript[$Af, $sr]},
  Bottom
]