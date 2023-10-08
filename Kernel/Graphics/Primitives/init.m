PublicVariable[$MakeBoxesStyleData]

$MakeBoxesStyleData = <||>;

SetHoldAllComplete[patchDownValuesWithInheritedBlock, applyIB];

patchDownValuesWithInheritedBlock[defSym_Symbol, lhsPatt_, blockSym_Symbol] := Block[{dv, pos},
  dv = DownValues[defSym];
  pos = Position[dv, Verbatim[HoldPattern[lhsPatt]] :> rhs_ /; FreeQ[Hold[rhs], InheritedBlock], 1];
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
  Map[Function[{y}, Typeset`MakeBoxes[y, fmt, head, Options], HoldFirst], Unevaluated[x]]

(* patchDownValuesWithInheritedBlock[
  Typeset`MakeBoxes,
  Typeset`MakeBoxes[Style[System`Dump`x_, System`Dump`y___], System`Dump`fmt_, System`Dump`head_],
  $MakeBoxesStyleData
];
 *)

(* we do this so styles get made before the styled object, so that changes to $MakeBoxesStyleData will get picked up *)
Typeset`MakeBoxes[Style[System`Dump`x_, System`Dump`y___], System`Dump`fmt_, System`Dump`head_] :=
  InheritedBlock[{QuiverGeometry`$MakeBoxesStyleData},
    With[{
      System`Dump`h = Map[Function[{System`Dump`z}, Typeset`MakeBoxes[System`Dump`z, System`Dump`fmt, System`Dump`head], HoldAllComplete], Unevaluated @ {System`Dump`y}],
      System`Dump`g = Typeset`MakeBoxes[System`Dump`x, System`Dump`fmt, System`Dump`head]},
        Typeset`Hold[StyleBox[System`Dump`g, System`Dump`h, StripOnInput -> False]]
    ]
  ];

(* we do this so nested lists localize ambient styles *)
Typeset`MakeBoxes[ System`Dump`g_List, System`Dump`fmt_, System`Dump`head_] := InheritedBlock[
  {QuiverGeometry`$MakeBoxesStyleData},
  Map[ Function[{System`Dump`x}, Typeset`MakeBoxes[System`Dump`x, System`Dump`fmt, System`Dump`head], HoldAllComplete], Unevaluated[System`Dump`g]]
];

(**************************************************************************************************)

PublicOption[ArrowheadPlane, ArrowheadLength, ArrowheadShape, ArrowheadColor, ArrowheadAnchor, ArrowheadThickness, ArrowheadOpacity, ArrowheadTooltip]

PrivateVariable[$arrowheadOptions]

$arrowheadOptions = {
  ArrowheadPlane -> None,
  ArrowheadLength -> Automatic,
  ArrowheadShape -> "FilledKite",
  ArrowheadColor -> GrayLevel[0],
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
  ArrowheadAnchor -> Automatic,
  ArrowheadPosition -> Automatic,
  ArrowThickness -> 1,
  ArrowColor -> Black,
  ArrowOpacity -> 1,
  ArrowDashing -> None,
  ArrowMasking -> None,
  ArrowPathSetback -> Automatic, (* legacy *)
  Setback -> Automatic,
  ArrowPathOffset -> None
];

AssociateTo[$MakeBoxesStyleData, $extendedArrowOptions];

(**************************************************************************************************)

PublicOption[ArrowheadSize]

PrivateVariable[$morphismArrowOptions]

$morphismArrowOptions = Normal @ KeyTake[$extendedArrowOptions, {
  ArrowPathSetback, ArrowColor, ArrowOpacity, ArrowThickness, ArrowDashing, ArrowMasking
}] ~Join~ {
  GraphicsScale -> None,
  ArrowheadSize -> 15,
  LabelPosition -> Automatic,
  LabelOrientation -> Automatic,
  LabelFontSize -> Inherited,
  LabelBackground -> Automatic,
  LabelSpacing -> Automatic,
  ArrowPathReversed -> False,
  ArrowShaftHidden -> False,
  LabelRectification -> True,
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
  $vectorF = $matrixF = ReplaceAll @ Dispatch @ rules;
  ReplaceAll[prims, $rpcDispatch]
]

$rpcDispatch := $rpcDispatch = Dispatch @ With[{
  $vecvec    = Alternatives @@ PrimitiveSignatureLookup["Vector,Vector"],
  $vec       = Alternatives @@ PrimitiveSignatureLookup["Vector?Radius | Vector,Delta"],
  $matrixy   = Alternatives @@ PrimitiveSignatureLookup["Pair?Radius | Matrix?Radius | Matrices?Radius | Curve?Radius"],
  $opvec     = Alternatives @@ PrimitiveSignatureLookup["Opaque,Vector"],
  $primvec   = Alternatives @@ PrimitiveSignatureLookup["Primitives,Vector | Curve,Vector"],
  $rulesprim = Alternatives @@ PrimitiveSignatureLookup["Rules,Primitives"],
  $rules     = Alternatives @@ PrimitiveSignatureLookup["Rules"],
  $op        = Alternatives @@ PrimitiveSignatureLookup["Opaque"]}, {

  e:($op)[___]                        :> e,

  (* we revert the undocumented behavior that InfiniteLine[{"A", "B"}] evaluates to InfiniteLine[{0, 0}, {"A", "B"}] *)
  i:InfiniteLine[{0, 0}, $Coord2P]    :> i,
  InfiniteLine[{0, 0}, ab:{_, _}]     :> RuleCondition @ InfiniteLine[$vectorF /@ ab],

  (h:$vecvec)[v_, w_, a___]           :> RuleCondition @ h[$vectorF @ v, $vectorF @ w, a],
  (h:$matrixy)[m_List, a___]          :> RuleCondition @ h[$matrixF @ m, a],
  (h:$vec)[v_, a___]                  :> RuleCondition @ h[$vectorF @ v, a],
  (h:$opvec)[f_, v_, a___]            :> RuleCondition @ h[f, $vectorF @ v, a],
  (h:$primvec)[f_, v_, a___]          :> RuleCondition @ h[f /. $rpcDispatch, $vectorF @ v, a],
  (h:$rulesprim)[r_, p_, a___]        :> RuleCondition @ h[$ruleF @ r, p /. $rpcDispatch, a],
  (h:$rules)[r_, a___]                :> RuleCondition @ h[$ruleF @ r, a]
}];

$ruleF[e_] := VectorReplace[e, Rule[c_, o_] :> Rule[$vectorF[c], o]];

(**************************************************************************************************)

PrivateFunction[ExpandGraphicsComplex]

ExpandGraphicsComplex[g_] := ReplaceAll[g,
  GraphicsComplex[c_, g2_] :>
    RuleCondition @ ReplacePrimitiveCoordinates[
      g2,
      i_Integer :> RuleCondition[Part[c, i]]
    ]
];

(**************************************************************************************************)

PrivateFunction[toCurvePoints]

toCurvePoints = Case[
  e_List         := e;
  Tube[e_]       := % @ e;
  other_         := DiscretizeCurve @ other;
];