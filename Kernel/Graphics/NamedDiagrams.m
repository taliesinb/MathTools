PublicFunction[CommutativeSquare]

SetUsage @ "
CommutativeSquare[{nw$, ne$, sw$, se$}, {n$, s$, w$, e$}] constructs a %CommutativeDiagram with 4 objects and 4 morphisms.
* additional morphisms can be specified after the four.
* the morphisms n$, s$, w$, e$ can be given as label$ or {label$, type$, opts$$}.
"

CommutativeSquare[{nw_, ne_, sw_, se_}, {n_, s_, w_, e_, extra___}, opts___Rule] :=
  CommutativeDiagram[
    {{1, 1} -> nw, {2, 1} -> ne, {1, 2} -> sw, {2, 2} -> se},
    {toComSugarArrow[1 => 2, n],
     toComSugarArrow[2 => 4, e],
     toComSugarArrow[1 => 3, w],
     toComSugarArrow[3 => 4, s],
     extra},
    LabelPosition -> Outwards,
    opts
  ];

(**************************************************************************************************)

PublicFunction[InTriangleDiagram, OutTriangleDiagram, CompositionTriangleDiagram]

SetUsage @ "
InTriangleDiagram[{l$, r$, b$}, {lr$, lb$, rb$}] constructs a %CommutativeDiagram with 3 objects and 3 morphisms connecting them.
InTriangleDiagram[$$, side$] places b$ below on side side$.
* side$ can be %Left, %Center, or %Right.
* the morphisms can be given as label$ or {label$, type$, opts$$}.
* additional morphisms can be included after the three.
"

SetUsage @ "
OutTriangleDiagram[{t$, l$, r$}, {tl$, tr$, lr$}] constructs a %CommutativeDiagram with 3 objects and 3 morphisms connecting them.
OutTriangleDiagram[$$, side$] places x$ below on side side$.
* side$ can be %Left, %Center, or %Right.
* the morphisms can be given as label$ or {label$, type$, opts$$}.
* additional morphisms can be included after the three.
"

SetUsage @ "
CompositionTriangleDiagram[{l$, t$, r$}, {lt$, tr$, lr$}] constructs a %CommutativeDiagram with 3 objects and 3 morphisms connecting them.
* the morphisms can be given as label$ or {label$, type$, opts$$}.
* additional morphisms can be included after the three.
"

$triCoord = With[{r = 1.1547}, {Left -> {0, r}, Center -> {r/2, r}, Right -> {r, r}}];

InTriangleDiagram[{l_, r_, b_, obs___}, {lr_, lb_, rb_, mors___}, side:(Left|Center|Right):Center, opts___Rule] :=
  commutativeTriangle[{l, r, b, obs}, {lr, lb, rb, mors}, False, side, opts];

OutTriangleDiagram[{t_, l_, r_, obs___}, {tl_, tr_, lr_, mors___}, side:(Left|Center|Right):Center, opts___Rule] :=
  commutativeTriangle[{l, r, t, obs}, {lr, tl, tr, mors}, True, side, opts];

CompositionTriangleDiagram[{l_, t_, r_, obs___}, {l2t_, t2r_, l2r_, mors___}, side:(Top|Bottom):Top, opts___Rule] :=
  commutativeTriangle[{l, r, t, obs}, {l2r, Reversed @ l2t, t2r, mors}, side === Top, Center, opts];

commutativeTriangle[{l_, r_, x_, obs___}, {l2r_, l2x_, r2x_, mors___}, top_, side_, opts___] := Scope[
  {xx, rx} = Lookup[$triCoord, side] + 1;
  {xy, lry} = If[top, {1, 2}, {2, 1}];
  rev = If[top, Id, Rev];
  CommutativeDiagram[Flatten @ {
    {{xx, xy}  -> x, {1, lry}  -> l, {rx, lry} -> r},
    obs,
    toComSugarArrow[rev[1 => 2], l2x],
    toComSugarArrow[rev[1 => 3], r2x],
    toComSugarArrow[2 => 3, l2r],
    mors},
    LabelPosition -> Outwards,
    opts
  ]
];

(**************************************************************************************************)

PublicFunction[DoubleTriangleDiagram]

SetUsage @ "
DoubleTriangleDiagram[{l$, m$, r$, x$}, {lm$, rm$, lx$, m$, rx$}] constructs a %CommutativeDiagram with 4 objects and 5 morphisms connecting them.
DoubleTriangleDiagram[$$, side$] places the bottom object on side$.
* l$ is placed on the left, m$ in the middle, r$ on the right, and x$ below or above.
* the morphisms can be given as label$ or head$[label$, opts$$], where head$ is %Morphism, DoubleMorphism%, etc.
* the direction of the arrows between l$, m, r$ and x$ are always downward.
* additional morphisms can be included after the four.
"

With[{r = 1.125, s = 1.41421}, {t = s/2},
$cdtCoordRules = {
  Top         -> {{0, 1}, {s, 1}, {t, 0}},
  TopRight    -> {{0, 1}, {r, 1}, {r, 0}},
  BottomLeft  -> {{0, 0}, {r, 0}, {0, 1}},
  Bottom      -> {{0, 0}, {s, 0}, {t, 1}},
  BottomRight -> {{0, 0}, {r, 0}, {r, 1}}
}];

DoubleTriangleDiagram[{l_, m_, r_, x_}, {lm_, rm_, lx_, mx_, rx_, extra___}, side:(Bottom|Top):Bottom, opts___Rule] := Scope[
  pos = {{1, 1}, {2, 1}, {3, 1}, {2, 2}};
  rev = Id;
  If[side === Top, rev = Rev; pos //= MapAt[If[# == 2, 1, 2]&, {All, 2}]];
  CommutativeDiagram[{
    Splice @ RuleThread[pos, {l, m, r, x}],
    toComSugarArrow[1 => 2, lm],
    toComSugarArrow[3 => 2, rm],
    toComSugarArrow[rev[1 => 4], lx],
    toComSugarArrow[rev[2 => 4], mx],
    toComSugarArrow[rev[3 => 4], rx],
    extra},
    LabelPosition -> Outwards,
    opts
  ]
];

(**************************************************************************************************)

PublicFunction[ParallelArrowDiagram]

SetUsage @ "
ParallelArrowDiagram[{l$, r$}, {t$, b$, v$}] constructs a %CommutativeDiagram with 2 objects and 2 morphisms connecting them.
ParallelArrowDiagram[$$, {t$, b$, v$}] specifies the double morphism v$ between the two parallel morphisms.
ParallelArrowDiagram[{pos$l -> l$, pos$r -> r$}, $$] specifies positions for l$ and r$.
* l$ is placed on the left, r$ on the right.
* $t connects l$ and r$ above, and b$ connects l$ and r$ below. v$ is a vertical double morphism between them.
"

ParallelArrowDiagram[{lp_ -> l_, rp_ -> r_}, {t_, b_, v_:Null, rest___}, opts___Rule] := Scope[
  rev = If[H[v] === Reversed, v //= P1; Rev, Id];
  curve = Lookup[{opts}, CurveFunction, TrapezoidCurve];
  bend = Lookup[{opts}, BendRadius, .33];
  CommutativeDiagram[{
    lp -> l, rp -> r,
    toComSugarArrow[curve[{OC @ 1, OC @ 2}, bend], t],
    toComSugarArrow[curve[{OC @ 1, OC @ 2}, -bend], b],
    If[v =!= Null, DoubleMorphism[MorphismCoordinates[rev @ {1, 2}], v, Setback -> 10, LabelPosition -> Right]],
    rest
  }, LabelPosition -> Outwards, FilterOptions @ opts]
];

ParallelArrowDiagram[{l:Except[_Rule], r:Except[_Rule]}, morphisms_List, opts___Rule] :=
  ParallelArrowDiagram[{{1, 1} -> l, {2, 1} -> r}, morphisms, opts];

(**************************************************************************************************)

PublicFunction[AdjunctionDiagram]

SetUsage @ "
AdjunctionDiagram[{l$, r$}, {lr$, rl$}] constructs a %CommutativeDiagram representing the adjunction between l$ and r$.
AdjunctionDiagram[{pos$l -> l$, pos$r -> r$}, $$] specifies positions for l$ and r$.
"

AdjunctionDiagram[{lpos_ -> l_, rpos_ -> r_}, {lr_, rl_, extra___}, opts___Rule] := Scope[
  curve = Lookup[{opts}, CurveFunction, TrapezoidCurve];
  bend = Lookup[{opts}, BendRadius, .33];
  CommutativeDiagram[{
    lpos -> l, rpos -> r,
    toComSugarArrow[curve[{OC @ 1, OC @ 2}, bend], lr],
    toComSugarArrow[curve[{OC @ 2, OC @ 1}, bend], rl],
    LongAdjointMorphism[2 => 1, Setback -> 30],
    extra},
    LabelPosition -> Outwards, FilterOptions @ opts
  ]
];

AdjunctionDiagram[{l_, r_}, morphisms_List, opts___Rule] :=
  AdjunctionDiagram[{{1, 1} -> l, {2, 1} -> r}, morphisms, opts]

(**************************************************************************************************)

PublicFunction[AdjointTripleDiagram]

SetUsage @ "
AdjointTripleDiagram[{a$, b$}, {l$, r$, ba$}] constructs a %CommutativeDiagram representing a triple adjunction.
* l$ is the morphism on the left, r$ is the morphism on the right, and ba$ is the reverse morphism.
"

OC = ObjectCoordinates;

AdjointTripleDiagram[{a_, b_}, {abl_, abr_, ba_, extra___}, opts___Rule] := Scope[
  curve = Lookup[{opts}, CurveFunction, TrapezoidCurve];
  bend = Lookup[{opts}, BendRadius, .5];
  setback = Lookup[{opts}, "AdjointSetback", 5];
  CommutativeDiagram[{
    {1, 1} -> a, {1, 2} -> b,
    toComSugarArrow[curve[{OC @ 1, OC @ 2}, -bend], abl],
    toComSugarArrow[curve[{OC @ 1, OC @ 2}, bend], abr],
    toComSugarArrow[ObjectCoordinates @ {2,1}, ba],
    AdjointMorphism[3 => 1, Setback -> {0, setback}], AdjointMorphism[2 => 3, Setback -> {setback, 0}],
    extra},
    FilterOptions @ opts,
    LabelPosition -> Center, LabelOrientation -> Horizontal
  ]
];

(**************************************************************************************************)

PublicFunction[ArrowDiagram]

SetUsage @ "
ArrowDiagram[a$, b$, f$] constructs a %CommutativeDiagram consisting of a single arrow f$ between a% and b%.
* pos$ -> obj$ specifies obj$ should be at position pos$, defaulting to below a$.
"

ArrowDiagram[a_, b_, f_, opts___Rule] := Scope[
  $apos = {1, 1};
  CommutativeDiagram[{
    sadApos[a], sadBpos[b],
    toComSugarArrow[1 => 2, f]
  }, opts]
]

sadApos = Case[
  r_Rule := ($apos = P1 @ r; r);
  obj_   := $apos -> obj;
]

sadBpos = Case[
  Rule[side:$SidePattern|Above|Below, obj_] := Plus[$apos, {1, -1} * Lookup[$SideToCoords, side]] -> obj;
  r_Rule := r;
  obj_ := % @ Rule[Below, obj];
]

(**************************************************************************************************)

PublicFunction[CommutativePentagon, CommutativeHexagon]

$pentagonPoints := $pentagonPoints = VectorReflectVertical @ ClockwiseCirclePoints[5];

CommutativePentagon[objs:{_, _, _, _, _}, arrows:{_, _, _, _, _}, opts___Rule] :=
  commutativeNgon[5, objs, arrows, opts];

CommutativeHexagon[objs:{_, _, _, _, _, _}, arrows:{_, _, _, _, _, _}, opts___Rule] :=
  commutativeNgon[6, objs, arrows, opts];

(**************************************************************************************************)

ngonPoints[n_] := ngonPoints[n] = VectorReflectVertical @ ClockwiseCirclePoints @ n;

commutativeNgon[n_, objects_, arrows_, opts___Rule] := CommutativeDiagram[
  ngonPoints[n] -> objects,
  MapIndex1[toComSugarArrow[#2 => Mod[#2 + 1, n, 1], #1]&, arrows],
  FontSize -> 14, LabelFontSize -> 15, GraphicsScale -> 150,
  LabelPosition -> Outwards, Setback -> 10, opts
]

