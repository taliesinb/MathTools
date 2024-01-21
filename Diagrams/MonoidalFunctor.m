LoadShortcuts["Categories"]

(**************************************************************************************************)

$F = $FunF;
MP1 = EmptyCircleMonoidalProductForm;
MP2 = FilledCircleMonoidalProductForm;
$mpmap = MonoidalFunctorMultiplicationForm;
$idmap = MonoidalFunctorUnitSymbol;
$id1 = RelativeToCategoryForm[MonoidalUnitSymbol, $CC]; $id2 = RelativeToCategoryForm[MonoidalUnitSymbol, $CC];
{$o1, $o2, $o3} = {$Oa, $Ob, $Oc};

(**************************************************************************************************)

NamedDiagram["MonoidalFunctor/AssociatorSquare"] := CommutativeDiagram[{
  {1, 1} -> MP2[MP2[$F[$o1],$F[$o2]], $F[$o3]], {3, 1} -> MP2[$F[$o1], MP2[$F[$o2], $F[$o3]]],
  {1, 2} -> MP2[$F @ ParenF @ MP1[$o1, $o2], $F[$o3]], {3, 2} -> MP2[$F[$o1], $F @ ParenF @ MP1[$o2, $o3]],
  {1, 3} -> $F[ParenF @ MP1[MP1[$o1,$o2], $o3]], {3, 3} -> $F[ParenF[MP1[$o1, ParenF @ MP1[$o2,$o3]]]],
  1 \[DirectedEdge] 2 -> ColoredCircleMonoidalProductModifierForm[AssociatorForm[$F[$o1], $F[$o2], $F[$o3]], Black],
  1 \[DirectedEdge] 3 -> MP2[$mpmap[$o1, $o2], OneArrow[$F[$o3]]],
  2 \[DirectedEdge] 4 -> MP2[OneArrow[$F[$o1]], $mpmap[$o2, $o3]],
  3 \[DirectedEdge] 5 -> $mpmap[MP1[$o1, $o2], $o3],
  4 \[DirectedEdge] 6 -> $mpmap[$o1, MP1[$o2, $o3]],
  5 \[DirectedEdge] 6 -> $F[ColoredCircleMonoidalProductModifierForm[AssociatorForm[$o1, $o2, $o3], White]]
}];

(**************************************************************************************************)

NamedDiagram["MonoidalFunctor/RightUnitorSquare"] := CommutativeSquare[
  {
    MP2[$F[$o1], $id2],
    MP2[$F[$o1], $F[$id1]],
    $F[$o1],
    $F[ParenF[MP1[$o1, $id1]]]
  },
  {
    MP2[OneArrow[$F[$o1]], $idmap],
    Reversed @ ColoredCircleMonoidalProductModifierForm[$F[RightUnitorForm[$o1]], White],
    ColoredCircleMonoidalProductModifierForm[RightUnitorForm[$F[$o1]], Black],
    $mpmap[$o1, $id1]
  }, DiagramScaling -> {1.3, .8}, LabelFontSize -> 15
];

(**************************************************************************************************)

NamedDiagram["MonoidalFunctor/LeftUnitorSquare"] := CommutativeSquare[
  {
    MP2[$id2, $F[$o2]],
    MP2[$F[$id1], $F[$o2]],
    $F[$o2],
    $F[ParenF[MP1[$id1, $o2]]]
  },
  {
    MP2[$idmap, OneArrow[$F[$o2]]],
    Reversed @ ColoredCircleMonoidalProductModifierForm[$F[LeftUnitorForm[$o2]], White],
    ColoredCircleMonoidalProductModifierForm[LeftUnitorForm[$F[$o2]], Black],
    $mpmap[$id1, $o2]
  }, DiagramScaling -> {1.3, .8}
];

(**************************************************************************************************)

NamedDiagram["MonoidalFunctor/BraidingSquare"] := CommutativeSquare[
  {
    MP2[$F[$o1], $F[$o2]],
    MP2[$F[$o2], $F[$o1]],
    $F[ParenF @ MP1[$o1, $o2]],
    $F[ParenF @ MP1[$o2, $o1]]
  },
  {
    BraidingForm[$F[$o1], $F[$o2]],
    $F[BraidingForm[$o1, $o2]],
    $mpmap[$o1, $o2],
    $mpmap[$o2, $o1]
  }, DiagramScaling -> {1.3, .8}
];