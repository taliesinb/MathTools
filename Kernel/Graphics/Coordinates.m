PublicFunction[MapPrimitiveCoordinates]

SetRelatedSymbolGroup[MapPrimitiveCoordinates, MapPrimitiveBoxCoordinates]

SetUsage @ "
MapPrimitiveCoordinates[fn$, primitives$] transforms all graphics primitives by the \
coordinate transform fn$, which should be a function that operates on a coordinate vector \
or matrix.
MapPrimitiveCoordinates[{fn$, matrixFn$}, $$] applies fn$ to vectors and matrixFn$ to matrices.
* primitives like %Line which can accept a list of coordinate matrices will have \
the function applied to each matrix.
* listability properties of fn$ will be used to ensure it is called the minimum number of times.
* %Threaded[fn$] can be used to indicate that fn$ can be passed matrices.
* %Circle, %Disk etc. do not have proper affine transformations applied to them, or their radii adjusted.
* %Text, etc. do not have their contents rotated if their coordinates are rotated, but if they have a direction it will be changed.
* Objects inside %Translate, %Rotate, etc. are not touched, since they have their own coordinate system.
* Use %MapBoxCoordinates to do the same for boxes.
"

MapPrimitiveCoordinates[{fn_, matrixFn_}, expr_] := Scope[
  $vectorF = fn; $matrixF = matrixFn;
  ReplaceAll[expr, $mpcDispatch]
]

MapPrimitiveCoordinates[Threaded[fn_] | (fn_ ? VectorListableQ), expr_] := Scope[
  $vectorF = $matrixF = fn;
  ReplaceAll[expr, $mpcDispatch]
]

MapPrimitiveCoordinates[fn_, expr_] := Scope[
  $vectorF = fn;
  $matrixF = Map[fn];
  ReplaceAll[expr, $mpcDispatch]
];

MapPrimitiveCoordinates[{x_, y_} ? CoordinateVectorQ, expr_] :=
  MapPrimitiveCoordinates[
    DotRightOperator @ ToPacked @ Transpose @ {{1, 0, x}, {0, 1, y}},
    expr
  ];

$rulesF[e_] := VectorReplace[e, Rule[c:$CoordP, o_] :> Rule[$vectorF[c], o]];
$vecDelta[a_, d_] := With[{a1 = $vectorF[a]}, {a1, $vectorF[a + d] - a1}];

$radius2F = Case[
  r:$NumberP                := radDist2[{r, 0}] * Sign[r]; (* the sign is for ElbowCurve, which can take a negative value *)
  {x:$NumberP, y:$NumberP}  := {radDist2[{x, 0}] * Sign[x], radDist2[{0, y}] * Sign[y]}; (* for {rx, ry} e.g. CenteredRectangle -- but doesn't do the right thing for horizontal stretching on HorizontalCurve *)
  e_                        := e;
];
radDist2[v_] := Dist[$vectorF @ v, $vectorF @ {0, 0}];

$radius3F = Case[
  r:$NumberP                           := radDist3[{r, 0, 0}] * Sign[r];
  {x:$NumberP, y:$NumberP, z:$NumberP} := {radDist3[{x, 0, 0}] * Sign[x], radDist3[{0, y, 0}] * Sign[y], radDist3[{0, 0, z}] * Sign[z]};
  e_                                   := e;
];
radDist3[v_] := Dist[$vectorF @ v, $vectorF @ {0, 0, 0}];


(* we set up this dispatch so that we know (and test) whether to call
$vec or $mat or $matList
for heads that are dual-use, (Line is $mat OR $matList, Point is $vec or $mat), they will
get dispatched to the right case based on a pattern test -- order is important to test
the more specific cases first
*)

(**************************************************************************************************)

$mpcDispatch0 := $mpcDispatch0 = Dispatch @ With[{
  $vecvec    = Alternatives @@ PrimitiveSignatureLookup["Vector,Vector"],
  $vecdelta  = Alternatives @@ PrimitiveSignatureLookup["Vector,Delta"],
  $vecrad    = Alternatives @@ PrimitiveSignatureLookup["Vector,Radius"],
  $vec       = Alternatives @@ PrimitiveSignatureLookup["Vector!Radius"],
  $matrixrad = Alternatives @@ PrimitiveSignatureLookup["Matrix,Radius | Pair,Radius | Curve,Radius"],
  $matrix    = Alternatives @@ PrimitiveSignatureLookup["Matrix!Radius | Pair!Radius | Curve!Radius"],
  $matrices  = Alternatives @@ PrimitiveSignatureLookup["Matrices?Radius"],
  $opvec     = Alternatives @@ PrimitiveSignatureLookup["Opaque,Vector|Primitives,Vector"],
  $op        = Alternatives @@ PrimitiveSignatureLookup["Opaque"],
  $rules     = Alternatives @@ PrimitiveSignatureLookup["Rules,Primitives"],
  $fano      = Alternatives @@ PrimitiveSignatureLookup["FanOut"],
  vecP       = $CoordP,
  matP       = {__List} ? CoordinateMatrixQ,
  matListP   = {__List} ? CoordinateMatricesQ}, {
  e:($op)[___]                            :> e,
  (h:$vecvec)[v:vecP, w:vecP, a___]       :> RuleCondition @ h[$vectorF @ v, $vectorF @ w, a],
  (h:$vecdelta)[v:vecP, d:vecP, a___]     :> RuleCondition @ h[Seq @@ $vecDelta[v, d], a],
  (h:$vecrad)[v:vecP, r_, a___]           :> RuleCondition @ h[$vectorF @ v, If[Len[v] == 2, $radius2F, $radius3F] @ r, a],
  (h:$vec)[v:vecP, a___]                  :> RuleCondition @ h[$vectorF @ v, a],
  (h:$matrixrad)[m:matP, r_, a___]        :> RuleCondition @ h[$matrixF @ m, If[Len[P1 @ m] == 2, $radius2F, $radius3F] @ r, a],
  (h:$matrix)[m:matP, a___]               :> RuleCondition @ h[$matrixF @ m, a],
  (h:$matrices)[v:matListP, a___]         :> RuleCondition @ h[$matrixF /@ v, a],
  (h:$rules)[r_List, p_, a___]            :> RuleCondition @ h[$rulesF @ r, p /. $mpcDispatch, a],
  (h:$fano)[FanOut[s:vecP, t:matP], a___] :> RuleCondition @ h[FanOut[$vectorF, $matrixF @ t], a],
  Text[x_, v:vecP, y_, d:vecP, a___]      :> RuleCondition @ With[{p = $vecDelta[v, d]}, Text[x, P1 @ p, y, PN @ p, a]],
  Inset[x_, v:vecP, y_, z_, d:vecP, a___] :> RuleCondition @ With[{p = $vecDelta[v, d]}, Inset[x, P1 @ p, y, z, PN @ p, a]],
  (h:$opvec)[f_, v:vecP, a___]            :> RuleCondition @ h[f, $vectorF @ v, a]
}];

$mpcDispatch := $mpcDispatch0;

(**************************************************************************************************)

PublicFunction[MapPrimitiveBoxCoordinates]

SetUsage @ "
MapPrimitiveBoxCoordinates[fn$, boxes$] is like %MapPrimitiveCoordinates but operates on primitive boxes.
"

MapPrimitiveBoxCoordinates[args___] := Block[
  {$mpcDispatch = $mpbcDispatch},
  MapPrimitiveCoordinates[args]
];

(**************************************************************************************************)

$mpbcDispatch := $mpbcDispatch = createMpbcDispatch[];

createMpbcDispatch[] := Scope[
  rules = Normal @ $mpcDispatch0;
  nonBoxableHeads = Complement[Keys @ $graphicsHeadQ, Keys @ $primHeadToPrimBoxHead];
  boxifyingRules = Assoc[$primHeadToPrimBoxHead, Thread[nonBoxableHeads :> Seq[]]];
  rules //= ReplaceAll[boxifyingRules];
  rules //= ReplaceAll[Verbatim[Alternatives][a_] :> a];
  rules //= Select[FreeQ[Verbatim[Alternatives[]]]];
  rules //= Map[addBoxConstruct];
  rules
];

addBoxConstruct[other_] := other;
addBoxConstruct[RuleDelayed[lhs_, RuleCondition[h[args___]]]] :=
  RuleDelayed[lhs, RuleCondition @ Construct[h, args]];

(**************************************************************************************************)

PublicFunction[ExtractPrimitiveCoordinates, ExtractPrimitiveBoxCoordinates]

SetUsage @ "ExtractPrimitiveCoordinates[g$] returns a list of primitive coordinates.";
SetUsage @ "ExtractPrimitiveBoxCoordinates[g$] returns a list of primitive box coordinates.";

ExtractPrimitiveCoordinates[prims_] := iExtractCoords[prims, MapPrimitiveCoordinates];
ExtractPrimitiveBoxCoordinates[prims_] := iExtractCoords[prims, MapPrimitiveBoxCoordinates];

iExtractCoords[prims_, fn_] := Scope[
  points = Bag[];
  fn[{StuffBag[points, #]&, StuffBag[points, #, 1]&}, prims];
  BagPart[points, All]
];

