PublicVariable[$MakeBoxesStyleData]

$MakeBoxesStyleData = <||>;

SetHoldAllComplete[patchDownValuesWithInheritedBlock, applyIB];

patchDownValuesWithInheritedBlock[defSym_Symbol, lhsPatt_, blockSym_Symbol] := Block[{dv, pos},
  dv = DownValues[defSym];
  pos = Position[dv, Verbatim[HoldPattern[lhsPatt]] :> rhs_ /; FreeQ[Hold[rhs], Internal`InheritedBlock], 1];
  If[pos === {}, Return[$Failed]];
  DownValues[defSym] = MapIndices[applyIB[blockSym], pos, dv];
]

applyIB[blockSym_][lhs_ :> rhs_] := lhs :> Internal`InheritedBlock[{blockSym}, rhs];

patchDownValuesWithInheritedBlock[
  System`Dump`InheritAmbientSettings,
  System`Dump`InheritAmbientSettings[System`Dump`expr_, System`Dump`head_, System`Dump`opts___],
  $MakeBoxesStyleData
]

(* patchDownValuesWithInheritedBlock[
  Typeset`MakeBoxes,
  Typeset`MakeBoxes[Style[System`Dump`x_, System`Dump`y___], System`Dump`fmt_, System`Dump`head_],
  $MakeBoxesStyleData
];
 *)

(* we do this so styles get made before the styled object, so that changes to $MakeBoxesStyleData will get picked up *)
Typeset`MakeBoxes[Style[System`Dump`x_, System`Dump`y___], System`Dump`fmt_, System`Dump`head_] :=
  Internal`InheritedBlock[{QuiverGeometry`$MakeBoxesStyleData},
    With[{
      System`Dump`h = Map[Function[{System`Dump`z}, Typeset`MakeBoxes[System`Dump`z, System`Dump`fmt, System`Dump`head], HoldAllComplete], Unevaluated @ {System`Dump`y}],
      System`Dump`g = Typeset`MakeBoxes[System`Dump`x, System`Dump`fmt, System`Dump`head]},
        Typeset`Hold[StyleBox[System`Dump`g, System`Dump`h, StripOnInput -> False]]
    ]
  ];

(**************************************************************************************************)

PrivateVariable[$arrowheadOptions]

$arrowheadOptions = {
  ArrowheadPlane -> None,
  ArrowheadLength -> Automatic,
  ArrowheadShape -> "EquilateralTriangle",
  ArrowheadColor -> GrayLevel[0.5],
  ArrowheadAnchor -> 0.5,
  ArrowheadOpacity -> None,
  ArrowheadEdgeThickness -> 1
}

(**************************************************************************************************)

PrivateVariable[$extendedArrowOptions]

$extendedArrowOptions = JoinOptions[
  $arrowheadOptions,
  ArrowheadAnchor -> Automatic,
  ArrowheadPosition -> Automatic,
  ArrowShaftThickness -> 2,
  ArrowShaftColor -> Black,
  ArrowShaftOpacity -> 1,
  ArrowPathShrinking -> None
];

AssociateTo[$MakeBoxesStyleData, $extendedArrowOptions];

(**************************************************************************************************)

PrivateVariable[$bendOptions]

$bendOptions = {
  BendRadius -> 0.1,
  BendShape -> "Arc"
};

(**************************************************************************************************)

AssociateTo[$MakeBoxesStyleData, KeyTake[Options[BendyArrow], {ArrowheadPosition, ArrowShaftThickness, ArrowShaftColor, ArrowShaftOpacity, ArrowPathShrinking}]];
