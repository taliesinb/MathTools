PublicVariable[$MakeBoxesStyleData]

$MakeBoxesStyleData = <||>;

SetHoldAllComplete[patchDownValuesWithInheritedBlock, applyIB];

patchDownValuesWithInheritedBlock[defSym_Symbol, lhsPatt_, blockSym_Symbol] := Block[{dv, pos},
  dv = DownValues[defSym];
  pos = Position[dv, Verbatim[HoldP[lhsPatt]] :> rhs_ /; FreeQ[Hold[rhs], InheritedBlock], 1];
  If[pos === {}, Return[$Failed]];
  DownValues[defSym] = MapIndices[applyIB[blockSym], pos, dv];
]

applyIB[blockSym_][lhs_ :> rhs_] := lhs :> InheritedBlock[{blockSym}, rhs];

patchDownValuesWithInheritedBlock[
  System`Dump`InheritAmbientSettings,
  System`Dump`InheritAmbientSettings[System`Dump`expr_, System`Dump`head_, System`Dump`opts___],
  $MakeBoxesStyleData
]

Typeset`MakeBoxes[ x_List, fmt_, head_, Options] :=
  Map[Fn[{y}, Typeset`MakeBoxes[y, fmt, head, Options], HoldFirst], Uneval[x]]

(* patchDownValuesWithInheritedBlock[
  Typeset`MakeBoxes,
  Typeset`MakeBoxes[Style[System`Dump`x_, System`Dump`y___], System`Dump`fmt_, System`Dump`head_],
  $MakeBoxesStyleData
];
 *)

(* we do this so styles get made before the styled object, so that changes to $MakeBoxesStyleData will get picked up *)
Typeset`MakeBoxes[Style[System`Dump`x_, System`Dump`y___], System`Dump`fmt_, System`Dump`head_] :=
  InheritedBlock[{MathTools`$MakeBoxesStyleData},
    With[{
      System`Dump`h = Map[Fn[{System`Dump`z}, Typeset`MakeBoxes[System`Dump`z, System`Dump`fmt, System`Dump`head], HoldAllComplete], Uneval @ {System`Dump`y}],
      System`Dump`g = Typeset`MakeBoxes[System`Dump`x, System`Dump`fmt, System`Dump`head]},
        Typeset`Hold[StyleBox[System`Dump`g, System`Dump`h, StripOnInput -> False]]
    ]
  ];

(* we do this so nested lists localize ambient styles *)
Typeset`MakeBoxes[ System`Dump`g_List, System`Dump`fmt_, System`Dump`head_] := InheritedBlock[
  {MathTools`$MakeBoxesStyleData},
  Map[ Fn[{System`Dump`x}, Typeset`MakeBoxes[System`Dump`x, System`Dump`fmt, System`Dump`head], HoldAllComplete], Uneval[System`Dump`g]]
];

(**************************************************************************************************)

PublicOption[ArrowheadPlane, ArrowheadLength, ArrowheadShape, ArrowheadColor, ArrowheadAnchor, ArrowheadThickness, ArrowheadOpacity, ArrowheadTooltip]

PrivateVariable[$arrowheadOptions]

$arrowheadOptions = {
  ArrowheadPlane -> None,
  ArrowheadLength -> Auto,
  ArrowheadShape -> "FilledKite",
  ArrowheadColor -> Auto,
  ArrowheadAnchor -> 0.5,
  ArrowheadOpacity -> None,
  ArrowheadThickness -> 1
}

(**************************************************************************************************)

PublicOption[ArrowThickness, ArrowColor, ArrowOpacity, ArrowDashing, ArrowMasking, ArrowShaftHidden, ArrowPathSetback, ArrowPathOffset, ArrowPathReversed]

PublicOption[LabelOrientation, LabelFontSize, LabelBackground, LabelSpacing]

PrivateVariable[$extendedArrowOptions]

$extendedArrowOptions = JoinOptions[
  $arrowheadOptions,
  ArrowheadAnchor -> Auto,
  ArrowheadPosition -> Auto,
  ArrowThickness -> 1,
  ArrowColor -> Auto,
  ArrowOpacity -> 1,
  ArrowDashing -> None,
  ArrowMasking -> None,
  ArrowPathSetback -> Auto, (* legacy *)
  Setback -> Auto,
  ArrowPathOffset -> None
];

AssociateTo[$MakeBoxesStyleData, $extendedArrowOptions];

(**************************************************************************************************)

PrivateVariable[$neatCurveOptions]

$neatCurveOptions = {
  JoinStyle -> Axis,
  SegmentPosition -> 0.5,
  ShortcutRadius -> 0,
  BendRadius -> 0.5,
  Setback -> 0.
};

(**************************************************************************************************)

PublicOption[ArrowheadSize, DebugLabels]

PrivateVariable[$morphismArrowOptions]

$morphismArrowOptions = Normal @ KTake[$extendedArrowOptions, {
  ArrowPathSetback, ArrowColor, ArrowOpacity, ArrowThickness, ArrowDashing, ArrowMasking
}] ~Join~ {
  GraphicsScale -> None,
  ArrowheadSize -> 15,
  LabelPosition -> Auto,
  LabelOrientation -> Auto,
  LabelFontSize -> Inherited,
  LabelBackground -> Auto,
  LabelSpacing -> Auto,
  ArrowPathReversed -> False,
  ArrowShaftHidden -> False,
  LabelRectification -> True,
  DebugLabels -> False,
  TextModifiers -> {}
};

AssociateTo[$MakeBoxesStyleData, $morphismArrowOptions];

(**************************************************************************************************)

AssociateTo[$MakeBoxesStyleData, {
  BendRadius -> 0.1,
  BendShape -> "Arc"
}];

(**************************************************************************************************)

PublicFunction[ReplacePrimitiveCoordinates]

SetUsage @ "
ReplacePrimitiveCoordinates[prims$, rules$] replaces all primitive coordinates present in prims$ via rules$.
"

ReplacePrimitiveCoordinates[prims_, rules_] := Scope[
  $vectorF = $matrixF = RepAll @ Dispatch @ rules;
  RepAll[prims, $rpcDispatch]
]

SetCached[$rpcDispatch, Dispatch @ With[{
  $vecvec    = Alt @@ PrimitiveSignatureLookup["Vector,Vector"],
  $vec       = Alt @@ PrimitiveSignatureLookup["Vector?Radius | Vector,Delta"],
  $matrixy   = Alt @@ PrimitiveSignatureLookup["Pair?Radius | Matrix?Radius | Matrices?Radius | Curve?Radius"],
  $opvec     = Alt @@ PrimitiveSignatureLookup["Opaque,Vector"],
  $primvec   = Alt @@ PrimitiveSignatureLookup["Primitives,Vector | Curve,Vector"],
  $rulesprim = Alt @@ PrimitiveSignatureLookup["Rules,Primitives"],
  $rules     = Alt @@ PrimitiveSignatureLookup["Rules"],
  $op        = Alt @@ PrimitiveSignatureLookup["Opaque"]}, {

  e:($op)[___]                        :> e,

  (* we revert the undocumented behavior that InfiniteLine[{"A", "B"}] evaluates to InfiniteLine[{0, 0}, {"A", "B"}] *)
  i:InfiniteLine[{0, 0}, $Coord2P]    :> i,
  InfiniteLine[{0, 0}, ab:{_, _}]     :> RuleEval @ InfiniteLine[$vectorF /@ ab],

  (h:$vecvec)[v_, w_, a___]           :> RuleEval @ h[$vectorF @ v, $vectorF @ w, a],
  (h:$matrixy)[m_List, a___]          :> RuleEval @ h[$matrixF @ m, a],
  (h:$vec)[v_, a___]                  :> RuleEval @ h[$vectorF @ v, a],
  (h:$opvec)[f_, v_, a___]            :> RuleEval @ h[f, $vectorF @ v, a],
  (h:$primvec)[f_, v_, a___]          :> RuleEval @ h[f /. $rpcDispatch, $vectorF @ v, a],
  (h:$rulesprim)[r_, p_, a___]        :> RuleEval @ h[$ruleF @ r, p /. $rpcDispatch, a],
  (h:$rules)[r_, a___]                :> RuleEval @ h[$ruleF @ r, a]
}]];

$ruleF[e_] := VectorReplace[e, Rule[c_, o_] :> Rule[$vectorF[c], o]];

(**************************************************************************************************)

PrivateFunction[ExpandGraphicsComplex]

ExpandGraphicsComplex[g_] := RepAll[g,
  GraphicsComplex[c_, g2_] :>
    RuleEval @ ReplacePrimitiveCoordinates[
      g2,
      i_Int :> RuleEval[Part[c, i]]
    ]
];

