LoadShortcuts["Categories"]

(**************************************************************************************************)

NamedDiagram["Cone/Cone"] := OutTriangleDiagram[
  {OverHat[$OC], $FunD[$OXi], $FunD[$OXj]},
  {$OCi, $OCj, $FunD[Subscript[$Af, $alpha]]}
];

(**************************************************************************************************)

NamedDiagram["Cone/ConeImage"] := OutTriangleDiagram[
  {OverHat[$OC], $FunD[$OXi], $FunD[$OXj],
   {1, 2.5} -> $OXi, {2.1547, 2.5} -> $OXj},
  {$OCi, $OCj, $FunD[Subscript[$Af, $alpha]],
   Morphism[{"X_i", "X_j"}, Subscript[$Af, $alpha]]}
]

(**************************************************************************************************)

NamedDiagram["Cone/Cocone"] := InTriangleDiagram[
  {$FunD[$OXi], $FunD[$OXj], OverHat[$OC]},
  {$FunD[Subscript[$Af, $alpha]], $OCi, $OCj}
]

(**************************************************************************************************)

NamedDiagram["Cone/CoconeImage"] := InTriangleDiagram[
  {$FunD[$OXi], $FunD[$OXj], OverHat[$OC],
   {1, 0.5} -> $OXi, {2.1547, 0.5} -> $OXj},
  {$FunD[Subscript[$Af, $alpha]], $OCi, $OCj,
   Morphism[{"X_i", "X_j"}, Subscript[$Af, $alpha]]}
]