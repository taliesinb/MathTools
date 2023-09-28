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

PublicOption[ArrowheadPlane, ArrowheadLength, ArrowheadShape, ArrowheadColor, ArrowheadAnchor, ArrowheadEdgeThickness, ArrowheadOpacity, ArrowheadTooltip]

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

PublicOption[ArrowShaftThickness, ArrowShaftColor, ArrowShaftOpacity, ArrowShaftDashing, ArrowShaftMasking, ArrowShaftHidden, ArrowPathShrinking, ArrowPathSetback, ArrowPathOffset, ArrowPathReversed]

PublicOption[LabelOrientation, LabelFontSize, LabelBackground, LabelSpacing]

PrivateVariable[$extendedArrowOptions]

$extendedArrowOptions = JoinOptions[
  $arrowheadOptions,
  ArrowheadAnchor -> Automatic,
  ArrowheadPosition -> Automatic,
  ArrowShaftThickness -> 1,
  ArrowShaftColor -> Black,
  ArrowShaftOpacity -> 1,
  ArrowShaftDashing -> None,
  ArrowShaftMasking -> None,
  ArrowPathShrinking -> None,
  ArrowPathSetback -> Automatic,
  Setback -> Automatic,
  ArrowPathOffset -> None
];

AssociateTo[$MakeBoxesStyleData, $extendedArrowOptions];

(**************************************************************************************************)

PublicOption[DecorationWidth]

PrivateVariable[$morphismArrowOptions]

$morphismArrowOptions = Normal @ KeyTake[$extendedArrowOptions, {
  ArrowPathSetback, ArrowShaftColor, ArrowShaftOpacity, ArrowShaftThickness, ArrowShaftDashing, ArrowShaftMasking
}] ~Join~ {
  DecorationWidth -> 15,
  LabelPosition -> Automatic,
  LabelOrientation -> Automatic,
  LabelFontSize -> Inherited,
  LabelBackground -> Automatic,
  LabelSpacing -> Automatic,
  ArrowPathReversed -> False,
  ArrowShaftHidden -> False,
  LabelRectification -> True
};

AssociateTo[$MakeBoxesStyleData, $morphismArrowOptions];

(**************************************************************************************************)

PrivateFunction[makeShaftStyler]

DefineMacro[makeShaftStyler,
makeShaftStyler[color_, opacity_, thickness_, dashing_] := Quoted @ DeleteNone[StyleBoxOperator[
    If[ColorQ @ color, color, None],
    If[NumberQ @ opacity, Opacity @ opacity, None],
    If[NumberQ @ thickness, AbsoluteThickness @ thickness, None],
    Switch[dashing, _Dashing, dashing, None, None, _, Dashing @ dashing]
  ]]
];

(**************************************************************************************************)

AssociateTo[$MakeBoxesStyleData, {
  BendRadius -> 0.1,
  BendShape -> "Arc"
}];

(**************************************************************************************************)

PrivateFunction[recurseGraphicsCoordinates]

recurseGraphicsCoordinates[f_, primitives_, igc_:False] := Scope[
  $f = f; $igc = igc;
  rgc[primitives]
]

$GPrimVec1H = Flatten @ Alternatives[$GPrimVecH, $GPrimVecRadH, $GPrimVecDeltaH];
$GPrimVec12H = $GPrimVecVecH;
$GPrimVecs1H = Flatten @ Alternatives[$GPrimVecsH, $GPrimVecsRadH, $GPrimPairH, $GPrimPairRadH, $GPrimMatH, $GPrimMatsH, $GPrimMatsRadH];
$GPrimOuterH = Graphics | Graphics3D;
(* ^ these could be more selective but here just rely on mapping at level -1 *)

$GArrowH = Arrow | ExtendedArrow;

PrivateVariable[$GArrowIntP, $GCurveIntP]

$GCurveIntP = _ElbowCurve | _RollingCurve | _VectorCurve | _CompassCurve | _LoopCurve | _SetbackCurve | _SnakeCurve | _HorizontalCurve | _VerticalCurve | _AnchoredCurve | _CircuitCurve | _SmoothedCurve | _Line | _BezierCurve | _BSplineCurve;
$GArrowIntP = Join[_JoinedCurve | _Tube, $GCurveIntP];

rgc = Case[
  (h:$GArrowH)[e:$GArrowIntP, a___] := h[% @ e, a];
  (h:$GPrimVec1H)[v_, a___]         := h[$f[v, h], a];
  (h:$GPrimVec12H)[v_, w_, a___]    := h[Sequence @@ $f[{v, w}, h], a];
  (h:$GPrimVecs1H)[v_, a___]        := h[$f[v, h], a];
  (h:$GTrans1V)[g_, v_, a___]       := h[% @ g, $f[v, h], a];
  (h:$GPrimThruH)[g_, a___]         := h[% @ g, a];
  (h:$GPrimThruVecH)[g_, v_, a___]  := h[% @ g, $f[v, h], a];
  (h:$GPrimAnyVecH)[a_, v_, b___]   := h[a, $f[v, h], b];
  GraphicsComplex[v_, g_]           := GraphicsComplex[$f[v, GraphicsComplex], If[$igc, % @ g, g]];
  GridComplex[v_, g_]               := % @ gridComplexPrimitives @ g;
  GraphicsStyleData[d_, g_]         := GraphicsStyleData[d, % @ g];
  list_List                         := Map[%, list];
  (h:$GPrimOuterH)[g_, a___]        := h[% @ g, a];
  other_                            := other;
]

(**************************************************************************************************)

PrivateFunction[ExpandGraphicsComplex]

ExpandGraphicsComplex[g_] := ReplaceAll[g,
  GraphicsComplex[c_, g2_] :>
    RuleCondition @ recurseGraphicsCoordinates[
      ReplaceAll[#, i_Integer :> RuleCondition[Part[c, i]]]&,
      g2, True
    ]
];

(**************************************************************************************************)

PrivateFunction[toCurvePoints]

toCurvePoints = Case[
  e_List         := e;
  Tube[e_]       := % @ e;
  other_         := DiscretizeCurve @ other;
];