(* ::Package:: *)

(* ::Section:: *)
(*Basic*)


LoadShortcuts["Categories"]


(* ::Section:: *)
(*Centering of text within disks*)


toPos[i_] := {Mod[i, 5,1], Ceiling[i/5]-.5}*6 - 18;
plotCharGrid[opts___] := StringDiagram[MapIndex1[toPos[#2] -> #1&,
  Append["[ "] @ Characters["\[Gamma]\[Eta]\[Delta]\[Epsilon]\[Alpha]\[LongRightArrow]\[RightArrow]\[UpDownArrow]\[LeftArrow]\[LongLeftArrow]_*.^\[FilledSquare]\[FilledCircle]\[EmptyCircle]\[CircleDot]\[CirclePlus]\[CircleTimes]01234() ["]],{}, opts, DiagramSize -> {16,20}];
$sizes = {1, 1.25, 1.5};
ZipMap[
  TestRaster @ plotCharGrid[GraphicsScale -> #1, NodeSize -> #2, FontSize -> #3]&,
  5 * $sizes, 25 * $sizes, 20 * $sizes
]


(* ::Section:: *)
(*Functorial string diagram*)


TestRaster @ FunctorialStringDiagram[
  {Center -> $NTeta},
  {},
  {RF @ $Oc, 0.5 -> RBF @ $Af, BF @ $Od}
]
