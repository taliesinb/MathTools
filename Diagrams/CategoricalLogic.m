NamedDiagram["CategoricalLogic/ExistentialAdjunctionCorrespondence"] := LargeForm @ DoubleFractionGrid[
  {Padded[$OY, {0.55,0}], SubsetEqualForm, PullbackStarForm[$pi][$OX], "", ParenthesesForm[Row[{"over ", CP[$OI, $OJ]}]]},
  {UsingExplicitAppliedForm @ FunctorSymbol["\[Exists]"][$OY],SubsetEqualForm,  Padded[$OX, {0.8, 0}], "", Padded[ParenthesesForm[Row[{"over ", $OI}]], {0, 1.65}]}
];

(**************************************************************************************************)

NamedDiagram["CategoricalLogic/UniversalAdjunctionCorrespondence"] := LargeForm @ DoubleFractionGrid[
  {PullbackStarForm[$pi][$OX], SubsetEqualForm, Padded[$OY, {0.55,0}], "", ParenthesesForm[Row[{"over ", CP[$OI, $OJ]}]]},
  {Padded[$OX, {0.75, 0}],SubsetEqualForm,  UsingExplicitAppliedForm @ FunctorSymbol["\[ForAll]"][$OY], "", Padded[ParenthesesForm[Row[{"over ", $OI}]], {0, 1.65}]}
];

(**************************************************************************************************)

NamedDiagram["CategoricalLogic/EqualityAdjunctionCorrespondence"] := LargeForm @ DoubleFractionGrid[
  {UsingExplicitAppliedForm@FunctorSymbol["Eq"][$OX], SubsetEqualForm, Padded[$OY, {0.8,0}], "", ParenthesesForm[Row[{"over ", CP[$OI, $OI]}]]},
  {Padded[$OX, {1.2, 0}],SubsetEqualForm,  UsingExplicitAppliedForm @ FunctorAppliedForm[PullbackStarForm[$delta], $OY], "", Padded[ParenthesesForm[Row[{"over ", $OI}]], {0, 1.5}]}
];

(**************************************************************************************************)

NamedDiagram["CategoricalLogic/ComprehensionAdjunctionCorrespondence"] := LargeForm @ DoubleFractionGrid[
  {UsingExplicitAppliedForm@FunctorSymbol["1"][$OI], OverArrowForm[$Au], Padded[ParenthesesForm @ SubsetEqualForm[$OY,$OJ], {0.1,0}], "", ParenthesesForm[Row[{"in ", CatPred}]]},
  {Padded[$OI, {.5, 0}], UnderArrowForm[$Av],  SetForm[ParenthesesForm @ SubsetEqualForm[$OY,$OJ]], "", Padded[ParenthesesForm[Row[{"in ", CatSet}]], {0, 0.6}]}
]