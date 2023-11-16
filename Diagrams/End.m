LoadShortcuts["Categories"]

(**************************************************************************************************)

NamedDiagram["End/Wedge"] := CommutativeSquare[
  {$Ow, $FunF[$Od, $Od], $FunF[$Oc, $Oc], $FunF[$Oc, $Od]},
  {Subscript[$Ae, $Od],
   TightAppliedForm[$FunF, $Oc, $Af],
   Subscript[$Ae, $Oc],
   TightAppliedForm[$FunF, $Af, $Od]},
   LabelFontSize -> 16, GraphicsScale -> 140
];

(**************************************************************************************************)

NamedDiagram["End/Cowedge"] := CommutativeSquare[
  {$Ow, $FunF[$Od, $Od], $FunF[$Oc, $Oc], $FunF[$Oc, $Od]},
  {Subscript[$Ae, $Od],
   TightAppliedForm[$FunF, $Oc, $Af],
   Subscript[$Ae, $Oc],
   TightAppliedForm[$FunF, $Af, $Od]},
  LabelFontSize -> 16,
  FlipX -> True, FlipY -> True,
  ArrowPathReversed -> True,
  GraphicsScale -> 140
];



