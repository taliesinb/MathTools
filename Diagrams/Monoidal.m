LoadShortcuts["Categories"]

(**************************************************************************************************)

NamedDiagram["Monoidal/Associator"] := CommutativeDiagram[{
  {0, 0} -> "(AB)C" -> MP[MP[$OA, $OB], $OC], {1.3,0} -> "A(BC)" -> MP[$OA, MP[$OB, $OC]],
  Morphism["(AB)C" \[DirectedEdge] "A(BC)", AssociatorForm[$OA, $OB, $OC]]},
  FontSize -> 14
];

(**************************************************************************************************)

NamedDiagram["Monoidal/Unitor"] := CommutativeDiagram[
  {{1,1} -> MP[$OA, $OI], {2, 1} -> $OA, {3,1} -> MP[$OI, $OA]},
  {1 \[DirectedEdge] 2 -> RightUnitorForm[$OA], 3 \[DirectedEdge] 2 -> LeftUnitorForm[$OA]},
  FontSize -> 14
];

(**************************************************************************************************)

NamedDiagram["Monoidal/Braiding"] := CommutativeDiagram[
  {{1,1} -> MP[$OA, $OB], {2, 1} -> MP[$OB, $OA]},
  {1 \[DirectedEdge] 2 -> BraidingForm[$OA, $OB]},
  FontSize -> 14
];

(**************************************************************************************************)

NamedDiagram["Monoidal/AssociatorTimesId"] := CommutativeDiagram[{
  {0, 0} -> "((AB)C)D" -> MP[MP[MP[$OA, $OB], $OC], $OD], {2,0} -> "(A(BC))D" -> MP[MP[$OA, MP[$OB, $OC]], $OD],
  Morphism[{"((AB)C)D", "(A(BC))D"}, MP[AssociatorForm[$OA, $OB, $OC], OneArrow[$OD]]]},
  FontSize -> 14
];

(**************************************************************************************************)

NamedDiagram["Monoidal/Pentagon"] := CommutativePentagon[{
  TMP[TMP[$OA, $OB], TMP[$OC, $OD]],
  TMP[$OA, TMP[$OB, TMP[$OC, $OD]]],
  TMP[$OA, TMP[TMP[$OB, $OC], $OD]],
  TMP[TMP[$OA, TMP[$OB, $OC]], $OD],
  TMP[TMP[TMP[$OA, $OB], $OC], $OD]}, {
  AssociatorForm[$OA, $OB, TMP[$OC, $OD]],
  Reversed @ MP[OneArrow[$OA], AssociatorForm[$OB, $OC, $OD]],
  Reversed @ AssociatorForm[$OA, TMP[$OB, $OC], $OD],
  Reversed @ MP[AssociatorForm[$OA, $OB, $OC], $1AD],
  AssociatorForm[TMP[$OA, $OB], $OC, $OD]
}];

(**************************************************************************************************)

NamedDiagram["Monoidal/Hexagon1"] := CommutativeHexagon[{
  MP[MP[$OA,$OB],$OC],
  MP[$OA,MP[$OB,$OC]],
  MP[MP[$OB,$OC],$OA],
  MP[$OB,MP[$OC,$OA]],
  MP[$OB,MP[$OA,$OC]],
  MP[MP[$OB,$OA],$OC]}, {
  AssociatorForm[$OA, $OB, $OC],
  BraidingForm[$OA, TMP[$OB, $OC]],
  AssociatorForm[$OB, $OC, $OA],
  Reversed @ MP[OneArrow[$OB], BraidingForm[$OA, $OC]],
  Reversed @ AssociatorForm[$OB, $OA, $OC],
  Reversed @ MP[BraidingForm[$OA, $OB], OneArrow @ $OC]
}];

NamedDiagram["Monoidal/Hexagon2"] := CommutativeHexagon[{
  MP[$OA,MP[$OB,$OC]],
  MP[MP[$OA,$OB],$OC],
  MP[$OC,MP[$OA,$OB]],
  MP[MP[$OC,$OA],$OB],
  MP[MP[$OA,$OC],$OB],
  MP[$OA,MP[$OC,$OB]]}, {
  InverseForm @ AssociatorForm[$OA, $OB, $OC],
  BraidingForm[TMP[$OA, $OB], $OC],
  InverseForm @ AssociatorForm[$OA, $OB, $OC],
  Reversed @ MP[BraidingForm[$OA, $OC], OneArrow[$OB]],
  Reversed @ InverseForm @ AssociatorForm[$OA, $OB, $OC],
  Reversed @ MP[OneArrow[$OA], BraidingForm[$OB, $OC]]}
];

(**************************************************************************************************)

NamedDiagram["Monoidal/AssociatorIdentity"] := InTriangleDiagram[
  {MP[MP[$OA, $OI], $OB], MP[$OA, MP[$OI, $OB]], MP[$OA, $OB]},
  {AssociatorForm[$OA,$OI,$OB], MP[RightUnitorForm[$OA], $1AB], MP[$1AA, LeftUnitorForm[$OB]]},
  FontSize -> 18, DiagramScaling -> {1.4, 1}
];

(**************************************************************************************************)

NamedDiagram["Monoidal/BraidingIdentity"] := InTriangleDiagram[
  {MP[$OI, $OA], MP[$OA, $OI], $OA},
  {BraidingForm[$OI, $OA], LeftUnitorForm[$OA], RightUnitorForm[$OA]},
  FontSize -> 18
];

(**************************************************************************************************)

NamedDiagram["Monoidal/BraidingSelfInverse"] := InTriangleDiagram[
  {MP[$OA, $OB], MP[$OA, $OB], MP[$OB, $OA]},
  {EqualityMorphism[], BraidingForm[$OA, $OB], Reversed @ BraidingForm[$OB, $OA]},
  FontSize -> 18
];


