PublicFunction[SetbackCoordinates]
PublicHead[Rectangular]

SetUsage @ "
Rectangular[{r$x, r$y}] represents a rectangular setback.
* r$x and r$y are half the size of the entire rectangle, since they are radii.
"

SetUsage @ "
SetbackCoordinates[path$, d$] truncates the endpoints of a path by distance d$ towards its interior.
SetbackCoordinates[path$, {d$1, d$2}] truncates by distance d$1 from start and d$2 from end.
SetbackCoordinates[{path$1, path$2}, $$] truncates several paths at once.
* Line segments will be dropped if the truncated distance exceeds the length of that segment.
* The entire line will become a zero length line if the truncated distance exceeds the entire length.
* Specifications can be a single spec or a pair of:
| d$ | a distance in regular coordinates |
| Scaled[f$] | a fraction of the line length |
| Offset[p$] | a setback in absolute points |
| Offset[p$, d$] | setback distance d$ then absolute points p$ |
| Rectangular[{x$, y$}] | separate radii in the x$ and y$ directions |
* Using Offset[$$] only works for straight lines.
"

$zeroP = 0|0.;
$zeroVP = {$zeroP, $zeroP};
$zeroRP = Rectangular[{$zeroP,$zerop}];
$zeroFP = $zeroP | $zeroRP;
$nullSBSpecP = None | $zeroFP | Offset[$zeroP] | Offset[$zeroP, $zeroFP];

$rectP = Rectangular[$Coord2P];
$numRectP = $NumberP | $rectP;
$sbSpecP = $numRectP | Offset[$NumberP] | Offset[$NumberP, $numRectP];

SetbackCoordinates[spec_, $nullSBSpecP | {$nullSBSpecP, $nullSBSpecP}] :=
  spec;

SetbackCoordinates[spec_ ? CoordinateArrayQ, d_] :=
  SetbackCoordinates[#, d]& /@ spec;

SetbackCoordinates[spec_, Scaled[s_ ? NumberQ]] :=
  SetbackCoordinates[spec, LineLength[spec] * s];

SetbackCoordinates[spec_, d:$sbSpecP] :=
  SetbackCoordinates[spec, {d, d}];

(* we can preserve absolute offsets here for simple lines, but in general we can't walk
a complex line in points since we don't know how it will resolve later when a scale is chosen *)
SetbackCoordinates[{a_, b_}, Offset[d_]] := SetbackCoordinates[{a, b}, {Offset[d], Offset[d]}]

SetbackCoordinates[{a_, b_}, spec:{Repeated[$sbSpecP, 2]} /; ContainsQ[spec, Offset]] := Scope[
  dx = coordNormDelta[a, b];
  {pos, off} = Transpose @ Map[FromOffsetNum, spec];
  {a, b} = SetbackCoordinates[{a, b}, pos];
  {d1, d2} = N @ off;
  {Offset[dx * d1, a], Offset[-dx * d2, b]} // SimplifyOffsets
];

SetbackCoordinates[{a_, b_}, {d1_ ? NumberQ, d2_ ? NumberQ}] := Scope[
  If[Dist[a, b] < d1 + d2, Return @ emptyLine[a, b]];
  dx = N @ Normalize[b - a];
  {a + dx * d1, b - dx * d2}
];

SetbackCoordinates[coords_, {d1:$numRectP, d2:$numRectP}] :=
  setbackHalf[setbackHalf[coords, d1], -d2]

SetbackCoordinates::invalidspec = "Invalid setback specification ``."
SetbackCoordinates[coords_, other_] := (
  Message[SetbackCoordinates::invalidspec, other];
  coords
);

$emptyLineD = 0.0001;
emptyLine[a_, b_] := Scope[
  mid = Avg[a, b];
  d = (b-a) * $emptyLineD;
  {mid - d, mid + d}
];

(**************************************************************************************************)

PublicFunction[ExtendSetback]

ExtendSetback[b_][a_] := ExtendSetback[a, b];
ExtendSetback[a:$NumberP, b:$NumberP] := a + b;

ExtendSetback[$nullSBSpecP, $nullSBSpecP] := 0;
ExtendSetback[$nullSBSpecP, b_] := b;
ExtendSetback[a_, $nullSBSpecP] := a;

ExtendSetback[{a1_, a2_}, {b1_, b2_}] := {extendSB1[a1, b1], extendSB1[a2, b2]};
ExtendSetback[a_, {b1_, b2_}] := {extendSB1[a, b1], extendSB1[a, b2]};
ExtendSetback[{a1_, a2_}, b_] := {extendSB1[a1, b], extendSB1[a2, b]};
ExtendSetback[a_, b_] := extendSB1[a, b];

extendSB1 = Case[
  Seq[$nullSBSpecP, $nullSBSpecP] := 0;
  Seq[$nullSBSpecP, e_] := e;
  Seq[e_, $nullSBSpecP] := e;
  Seq[a_, b_] := ToOffsetNum[Plus @@ Map[FromOffsetNum, {a, b}]];
];

(**************************************************************************************************)

Rectangular /: Plus[Rectangular[p_],  d:$NumberP] := Rectangular[p + d];
Rectangular /: Plus[Rectangular[p1_], Rectangular[p2_]] := Rectangular[p1 + p2];

(**************************************************************************************************)

setbackHalf[{}, _] := {};
setbackHalf[p:{_}, _] := p;
setbackHalf[coords_, 0|0.] := coords;
setbackHalf[coords_, d_ ? Negative] := Rev @ setbackHalf[Rev @ coords, Abs[d]];
setbackHalf[coords_, -r_Rectangular] := Rev @ setbackHalf[Rev @ coords, r];

setbackHalf[coords_, d_] := takeLine[coords, d];

takeLine[{a_, b_}, Rectangular[{d1:$NumberP, d2:$NumberP}]] := Scope[
  sz = {d1,d2}/2;
  c = LineRectangleIntersectionPoint[{b, a}, {a - sz, a + sz}];
  If[Norm[c - b] == 0., c = Lerp[b, a, $emptyLineD]];
  {c, b}
];

takeLine[path_, Rectangular[{d1:$NumberP, d2:$NumberP}]] := Scope[
  a = P1[path];
  sz = {d1,d2}/2; rect = {a-sz, a+sz};
  c = LineRectangleIntersectionPoint[path, rect];
  n = LengthWhile[path, RegionMember[Rectangle @@ rect]];
  DeleteDuplicates @ Prepend[Drop[path, n], c]
];

takeLine[{a_, b_}, d:$NumberP] := (
  If[Dist[a, b] < d, Return @ emptyLine[a, b]];
  dx = N @ Normalize[b - a];
  {a + dx * d, b}
);

takeLine[coords_List, d:$NumberP] := Scope[
  prev = P1 @ coords; total = 0;
  n = LengthWhile[coords, curr |-> (total += Dist[curr, prev]; prev = curr; total < d)];
  If[n == Len[coords], Return @ PN @ coords];
  rem = total - d;
  newCoords = Drop[coords, n];
  If[rem == 0,
    newCoords,
    Prepend[newCoords, PointAlongLine[Part[coords, {n + 1, n}], rem]]
  ]
];

(**************************************************************************************************)

PublicFunction[ContainsOffsetsQ]

ContainsOffsetsQ[points_] := ContainsQ[points, _Offset];

(**************************************************************************************************)

PublicFunction[RemoveOffsets]

RemoveOffsets[points_] := points /. Offset[_, p_] :> p;

(**************************************************************************************************)

PublicFunction[FromOffsetNum, ToOffsetNum]

FromOffsetNum = Case[
  n:$numRectP                     := {n, 0};
  Offset[o:$NumberP]              := {0, o};
  Offset[o:$NumberP, n:$numRectP] := {n, o};
];

ToOffsetNum = Case[
  {$zeroFP, $zeroP} := 0;
  {n_, $zeroP}      := n;
  {$zeroFP, o_}     := Offset[o];
  {n_, o_}          := Offset[o, n];
];

(**************************************************************************************************)

PublicFunction[FromOffsetCoord, ToOffsetCoord]

FromOffsetCoord = Case[
  p:$CoordP                      := {p, {0,0}};
  Offset[o:$CoordP]              := {{0,0}, o};
  Offset[o:$CoordP, p:$CoordP]   := {p, o};
];

ToOffsetCoord = Case[
  {p_, $zeroVP} := p;
  {p_, o_}      := Offset[o, p];
];

(**************************************************************************************************)

PublicFunction[SimplifyOffsets]

SimplifyOffsets[points_] := points //. {
  Offset[d1_, Offset[d2_, p_]] :> Offset[d1 + d2, p],
  Offset[{$zeroP, $zeroP}, p_] :> p
};


PublicFunction[ResolveOffsets]

ResolveOffsets[e_, scale_ ? NumberQ] :=
  ReplaceAll[e, {
    Offset[o_, p_] :> RuleCondition[p + o / scale],
    Offset[o_] :> RuleCondition[o / scale]
  }];

ResolveOffsets[e_, _] := e;

ResolveOffsets[scale_][e_] := ResolveOffsets[e, scale];

(**************************************************************************************************)

(* TODO: expose this and other such Offset-aware functions properly *)
PrivateFunction[coordPlus, coordNormDelta, coordDistance]

coordPlus[Offset[o1_, p1_], Offset[o2_, p2_]] := Offset[o1 + o2, p1 + p2];
coordPlus[Offset[o_, p1_], p2_] := Offset[o, p1 + p2];
coordPlus[p1_, Offset[o_, p2_]] := Offset[o, p1 + p2];
coordPlus[p1_, p2_] := p1 + p2;

coordNormDelta[a_, Offset[_, b_]] := coordNormDelta[a, b];
coordNormDelta[Offset[_, a_], b_] := coordNormDelta[a, b];
coordNormDelta[a_, b_] := N @ Normalize[b - a];

coordDistance[Offset[_, a_], b_] := coordDistance[a, b];
coordDistance[a_, Offset[_, b_]] := coordDistance[a, b];
coordDistance[a_, b_] := Dist[a, b];

