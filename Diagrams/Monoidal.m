LoadShortcuts["Categories"]

(**************************************************************************************************)

NamedDiagram["Monoidal/Associator"] := CommutativeDiagram[{
  {0, 0} -> "(AB)C" -> MPF[MPF[$OA, $OB], $OC], {1.3,0} -> "A(BC)" -> MPF[$OA, MPF[$OB, $OC]],
  Morphism["(AB)C" \[DirectedEdge] "A(BC)", AssociatorForm[$OA, $OB, $OC]]},
  FontSize -> 14
];

(**************************************************************************************************)

NamedDiagram["Monoidal/Pentagon"] := CommutativePentagon[{
  TMPF[TMPF[$OA, $OB], TMPF[$OC, $OD]],
  TMPF[$OA, TMPF[$OB, TMPF[$OC, $OD]]],
  TMPF[$OA, TMPF[TMPF[$OB, $OC], $OD]],
  TMPF[TMPF[$OA, TMPF[$OB, $OC]], $OD],
  TMPF[TMPF[TMPF[$OA, $OB], $OC], $OD]}, {
  AssociatorForm[$OA, $OB, TMPF[$OC, $OD]],
  Reversed @ MPF[OneArrow[$OA], AssociatorForm[$OB, $OC, $OD]],
  Reversed @ AssociatorForm[$OA, TMPF[$OB, $OC], $OD],
  Reversed @ MPF[AssociatorForm[$OA, $OB, $OC], $1AD],
  AssociatorForm[TMPF[$OA, $OB], $OC, $OD]
}];

(**************************************************************************************************)

NamedDiagram["Monoidal/Hexagon1"] := CommutativeHexagon[{
  MPF[MPF[$OA,$OB],$OC],
  MPF[$OA,MPF[$OB,$OC]],
  MPF[MPF[$OB,$OC],$OA],
  MPF[$OB,MPF[$OC,$OA]],
  MPF[$OB,MPF[$OA,$OC]],
  MPF[MPF[$OB,$OA],$OC]}, {
  AssociatorForm[$OA, $OB, $OC],
  BraidingForm[$OA, TMPF[$OB, $OC]],
  AssociatorForm[$OB, $OC, $OA],
  Reversed @ MPF[OneArrow[$OB], BraidingForm[$OA, $OC]],
  Reversed @ AssociatorForm[$OB, $OA, $OC],
  Reversed @ MPF[BraidingForm[$OA, $OB], OneArrow @ $OC]
}];

NamedDiagram["Monoidal/Hexagon2"] := CommutativeHexagon[{
  MPF[$OA,MPF[$OB,$OC]],
  MPF[MPF[$OA,$OB],$OC],
  MPF[$OC,MPF[$OA,$OB]],
  MPF[MPF[$OC,$OA],$OB],
  MPF[MPF[$OA,$OC],$OB],
  MPF[$OA,MPF[$OC,$OB]]}, {
  InverseForm @ AssociatorForm[$OA, $OB, $OC],
  BraidingForm[TMPF[$OA, $OB], $OC],
  InverseForm @ AssociatorForm[$OA, $OB, $OC],
  Reversed @ MPF[BraidingForm[$OA, $OC], OneArrow[$OB]],
  Reversed @ InverseForm @ AssociatorForm[$OA, $OB, $OC],
  Reversed @ MPF[OneArrow[$OA], BraidingForm[$OB, $OC]]}
];

(**************************************************************************************************)

NamedDiagram["Monoidal/AssociatorIdentity"] := InTriangleDiagram[
  {MPF[MPF[$OA, $OI], $OB], MPF[$OA, MPF[$OI, $OB]], MPF[$OA, $OB]},
  {AssociatorForm[$OA,$OI,$OB], MPF[RightUnitorForm[$OA], $1AB], MPF[$1AA, LeftUnitorForm[$OB]]},
  FontSize -> 18, DiagramScaling -> {1.4, 1}
];

(**************************************************************************************************)

NamedDiagram["Monoidal/BraidingIdentity"] := InTriangleDiagram[
  {MPF[$OA, $OI], MPF[$OI, $OA], $OA},
  {BraidingForm[$OA, $OI], LeftUnitorForm[$OA], RightUnitorForm[$OA]}
];

(**************************************************************************************************)

NamedDiagram["Monoidal/BraidingSelfInverse"] := InTriangleDiagram[
  {MPF[$OA, $OB], MPF[$OA, $OB], MPF[$OB, $OA]},
  {EqualityMorphism[], BraidingForm[$OA, $OB], Reversed @ BraidingForm[$OB, $OA]}
];


