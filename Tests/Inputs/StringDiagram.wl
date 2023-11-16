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


(* ::Section:: *)
(*Adjunctions*)


TestRaster /@ NamedDiagram["Adjunction/StringMonadPanel"]


TestRaster @ NamedDiagram["Adjunction/StringLRL"]
TestRaster @ NamedDiagram["Adjunction/StringL"]
TestRaster @ NamedDiagram["Adjunction/StringRLR"]
TestRaster @ NamedDiagram["Adjunction/StringR"]


TestRaster @ NamedDiagram["Adjunction/StringLImage"]
TestRaster @ NamedDiagram["Adjunction/StringLRImage"]


TestRaster @ NamedDiagram["Adjunction/StringRImage"]
TestRaster @ NamedDiagram["Adjunction/StringRLImage"]


TestRaster /@ NamedDiagram["Adjunction/StringMonadPanel1"]
TestRaster /@ NamedDiagram["Adjunction/StringMonadPanel2"]
TestRaster /@ NamedDiagram["Adjunction/StringMonadPanel12"]
TestRaster /@ NamedDiagram["Adjunction/StringMonadPanel12Result"]


(* ::Section:: *)
(* Monad and comonad diagrams*)


TestRaster @ NamedDiagram["Monad/Mu"]
TestRaster @ NamedDiagram["Monad/Eta"]
TestRaster @ NamedDiagram["Monad/Delta"]
TestRaster @ NamedDiagram["Monad/Eps"]


(* ::Subsubsection:: *)
(*Monad from adjunction*)


TestRaster @ NamedDiagram["Monad/AdjunctionMuResult"]
TestRaster @ NamedDiagram["Monad/AdjunctionMu"]
TestRaster @ NamedDiagram["Monad/AdjunctionEtaResult"]
TestRaster @ NamedDiagram["Monad/AdjunctionEta"]


TestRaster @ NamedDiagram["Monad/AdjunctionDeltaResult"]
TestRaster @ NamedDiagram["Monad/AdjunctionDelta"]
TestRaster @ NamedDiagram["Monad/AdjunctionEpsResult"]
TestRaster @ NamedDiagram["Monad/AdjunctionEps"]


(* ::Section:: *)
(*Naturality diagrams*)


TestRaster /@ NamedDiagram["Naturality/SlidingImage"]


TestRaster /@ NamedDiagram["Naturality/Sliding"]
