LoadShortcuts["Categories"]

(**************************************************************************************************)

NamedDiagram["DoubleCat/Morphism"] := CommutativeSquare[
  {$Ox0, $Ox1, $Oy0, $Oy1},
  {$Af, $Ag, $Aalpha0, $Aalpha1,
  DoubleMorphism[{"f", "g"}, "\[Phi]", Setback -> 30]}
];

(**************************************************************************************************)

NamedDiagram["DoubleCat/ProArrow"] := CommutativeSquare[
  {$OA, $OB, $OApr, $OBpr},
  {{$AM, "Proarrow"}, {$AN, "Proarrow"}, $Af, $Ag,
   DoubleMorphism[{1, 4}, $Aalpha, Setback -> 30]}
];
