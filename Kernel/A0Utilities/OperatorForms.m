PublicFunction[TakeOperator]

SetUsage @ "TakeOperator[spec$$] is the operator form of %Take."

TakeOperator[spec___][e_] := Take[e, spec];

(**************************************************************************************************)

PublicFunction[DropOperator]

SetUsage @ "DropOperator[spec$$] is the operator form of %Drop."

DropOperator[spec___][e_] := Drop[e, spec];

(**************************************************************************************************)

PublicFunction[ClipOperator]

SetUsage @ "ClipOperator[spec$] is the operator form of %Clip."

ClipOperator[spec_][e_] := Clip[e, spec];

(**************************************************************************************************)

PublicFunction[PartOperator]

SetUsage @ "PartOperator[parts$$] is the operator form of %Part."

PartOperator[spec___][e_] := Part[e, spec];

(**************************************************************************************************)

PublicFunction[PartOfOperator]

SetUsage @ "PartOfOperator[expr$] uses its argument(s) as a %Part spec for expr$."

PartOfOperator[e_][p___] := Part[e, p];

(**************************************************************************************************)

PublicFunction[ContainedInQ]

SetUsage @ "ContainedInQ[expr$] yields true when its argumet is present in expr$ in any position."

ContainedInQ[expr_][patt_] := !FreeQ[expr, patt];

(**************************************************************************************************)

PublicFunction[DotOperator, DotRightOperator]

SetUsage @ "
DotOperator[m$][v$] returns %Dot[m$, v$].
DotOperator[m$, b$][v$] returns %Dot[m$, v$] + b$.
"

SetUsage @ "
DotRightOperator[m$][v$] returns %Dot[v$, m$].
DotRightOperator[m$, b$][v$] returns %Dot[v$, m$] + b$.
"

setVectorListableOperator[DotOperator, DotRightOperator];

DotOperator[matrix_][other_] := Dot[matrix, other]
DotOperator[matrix_, vector_][other_] := Threaded[vector] + Dot[matrix, other];

DotRightOperator[matrix_][other_] := Dot[other, matrix]
DotRightOperator[matrix_, vector_][other_] := Threaded[vector] + Dot[other, matrix];

(**************************************************************************************************)

PublicFunction[AffineOperator]

SetUsage @ "
AffineOperator[m$,b$] applies %Dot[m$, x$] + b$ to a single vector x$ or a list of such vectors.
AffineOperator[m$] just applies %Dot[m$, x$].
* AffineOperator evaluates to DotRightOperator which stores the transposed version of m$."

AffineOperator[matrix_] := DotRightOperator[Transpose @ ToPacked @ matrix];
AffineOperator[matrix_, {(0|0.)..}] := DotRightOperator @ Transpose @ ToPacked @ matrix;
AffineOperator[matrix_, vector_] := DotRightOperator[Transpose @ ToPacked @ matrix, vector];

(**************************************************************************************************)

PublicFunction[ReplaceAllOperator]

SetUsage @ "ReplaceAll[rules$] is the operator form of %ReplaceAll."

ReplaceAllOperator[][other_] := other;
ReplaceAllOperator[r1_][other_] := other /. r1;
ReplaceAllOperator[r1_, r2_][other_] := other /. r1 /. r2;
ReplaceAllOperator[r1_, r2_, r3___][other_] := ReplaceAllOperator[r3][other /. r1 /. r2];

(**************************************************************************************************)

PublicFunction[TimesOperator, ThreadedTimesOperator]

SetUsage @ "TimesOperator[n$] is the operator form of %Times."
SetUsage @ "ThreadedTimesOperator[arr$] multiplies its argument by %Threaded[arr$]."

setVectorListableOperator[TimesOperator, ThreadedTimesOperator];

TimesOperator[a_][b_] := a * b;
ThreadedTimesOperator[a_ ? NumberQ] := TimesOperator @ a;
ThreadedTimesOperator[a_] := TimesOperator[Threaded[a]]

(**************************************************************************************************)

PublicFunction[OffsetOperator]

SetUsage @ "OffsetOperator[off$] is the operator form of %Offset."

OffsetOperator[off_][p_] := Offset[off, p];

(**************************************************************************************************)

PublicFunction[PlusOperator, ThreadedPlusOperator]

SetUsage @ "PlusOperator[n$] is the operator form of %Times."
SetUsage @ "ThreadedPlusOperator[arr$] adds its argument to %Threaded[arr$]."

setVectorListableOperator[PlusOperator, ThreadedPlusOperator];

PlusOperator[a_][b_] := a + b;
ThreadedPlusOperator[a_ ? NumberQ] := PlusOperator @ a;
ThreadedPlusOperator[a_] := PlusOperator[Threaded[a]];

(**************************************************************************************************)

PublicFunction[PlusOne]

PlusOne[a_] := a + 1;

(**************************************************************************************************)

PublicFunction[MinusOne]

MinusOne[a_] := a - 1;


(**************************************************************************************************)

PublicFunction[OneMinus]

OneMinus[a_] := 1 - a;

(**************************************************************************************************)

PublicFunction[ModOperator]

ModOperator[n_][e_] := If[NumericQ[e], Mod[e, n, 0], e];
ModOperator[n_, m_][e_] := If[NumericQ[e], Mod[e, n, m], e];
ModOperator[Infinity] = Identity;
ModOperator[Infinity, _] = Identity;

(**************************************************************************************************)

PublicFunction[PlusOneMod]

PlusOneMod[Infinity] := PlusOne;
PlusOneMod[Infinity, _] := PlusOne;
PlusOneMod[n_][x_] := Mod[x + 1, n];
PlusOneMod[n_, m_][x_] := Mod[x + 1, n, m];

(**************************************************************************************************)

PublicFunction[MinusOneMod]

MinusOneMod[Infinity] := MinusOne;
MinusOneMod[Infinity, _] := MinusOne;
MinusOneMod[n_][x_] := Mod[x - 1, n];
MinusOneMod[n_, m_][x_] := Mod[x - 1, n, m];

(**************************************************************************************************)

PublicFunction[PlusModOperator]

PlusModOperator[n__] := Plus /* ModOperator[n];

(**************************************************************************************************)

PublicFunction[TimesModOperator]

TimesModOperator[n__] := Times /* ModOperator[n];

(**************************************************************************************************)

PublicFunction[MinusModOperator]

MinusModOperator[n__] := Minus /* ModOperator[n];

(**************************************************************************************************)

PublicFunction[SubtractModOperator]

SubtractModOperator[n__] := Subtract /* ModOperator[n];

(**************************************************************************************************)

PublicFunction[OrOperator]

e_OrOperator[arg_] := AnyTrue[e, #[arg]&];

(**************************************************************************************************)

PublicFunction[AndOperator]

e_AndOperator[arg_] := AllTrue[e, #[arg]&];

(**************************************************************************************************)

PublicFunction[NotOperator]

NotOperator[f_][expr_] := Not @ f @ expr;

(**************************************************************************************************)

PublicFunction[SameOperator]

SameOperator = SameAs;

(**************************************************************************************************)

PublicFunction[UnsameOperator]

UnsameOperator[f_][g_] := UnsameQ[f, g];

(**************************************************************************************************)

PublicTypesettingBoxFunction[StyleOperator, StyleBoxOperator]

StyleOperator[] = Identity;
StyleOperator[None] = Identity;
StyleOperator[spec___][Nothing] := Nothing;
StyleOperator[spec___][e_] := Style[e, spec];

StyleBoxOperator[] = Identity;
StyleBoxOperator[None] = Identity;
StyleBoxOperator[spec___][Nothing] := Nothing;
StyleBoxOperator[spec___][e_] := StyleBox[e, spec];

(**************************************************************************************************)

PublicTypesettingBoxFunction[InvisibleOperator]

InvisibleOperator[___] := {};

(**************************************************************************************************)

PrivateTypesettingBoxFunction[SolidEmptyStyleBoxOperator]

SolidEmptyStyleBoxOperator[True, args___] := SolidStyleBoxOperator[args];
SolidEmptyStyleBoxOperator[False, args___] := EmptyStyleBoxOperator[args];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[EmptyStyleBoxOperator]

SetUsage @ "
EmptyStyleBoxOperator[thickness$, opacity$, color$]
* color$ can be %SolidEdgeColor[$$], the edge color will be used.
"

EmptyStyleBoxOperator = Case[
  Seq[t_, o_, s_SolidEdgeForm] := %[t, o, Last @ solidEdgeColors @ s];
  Seq[_, _, None]              := InvisibleOperator;
  Seq[0, _, _]                 := InvisibleOperator;
  Seq[t_, o_, c_]              := StyleBoxOperator[toThick @ t, toOpacity @ o, toColor @ c]
];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[SolidStyleBoxOperator]

SetUsage @ "
SolidStyleBoxOperator[thickness$, opacity$, color$]
* color$ can be %SolidEdgeColor[$$], the face and edge colors will be used.
"

toFaceForm[o_, {c_, _} | c_] :=
  FaceForm[{toOpacity @ o, toColor @ c}];

(* Subtle issue: we must ignore opacity for edges because they are extend both within and without
the underlying polygon, and so produce a weird stripe if they are semiopaque *)
toEdgeForm = Case[
  Seq[0|None, _] := EdgeForm @ None;
  Seq[_, None]   := EdgeForm @ None;
  Seq[t_, {_, c_} | c_]   := EdgeForm @ {toThick @ t, toColor @ c};
];

SolidStyleBoxOperator = Case[
  Seq[t_, o_, s_SolidEdgeForm] := %[t, o, solidEdgeColors @ s];
  Seq[t_, o_, c_]              := StyleBoxOperator[toFaceForm[o, c], toEdgeForm[t, c]];
];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[ShaftStyleBoxOperator]

ShaftStyleBoxOperator[s_SolidEdgeForm, args___] :=
  ShaftStyleBoxOperator[Last @ solidEdgeColors @ s, args];

ShaftStyleBoxOperator[{c1_, c2_}, o_, t_, d_] :=
  LineBoxGradientOperator[{c1, c2}] /* ShaftStyleBoxOperator[None, o, t, d];

ShaftStyleBoxOperator[color_, opacity_, thickness_, dashing_] :=
  StyleBoxOperator[
    If[ColorQ @ color, color, Seq[]],
    If[NumberQ @ opacity, Opacity @ opacity, Seq[]],
    If[NumberQ @ thickness, AbsoluteThickness @ thickness, Seq[]],
    Switch[dashing, _Dashing, dashing, None, Seq[], _, Dashing @ dashing]
  ];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[LineBoxGradientOperator]

LineBoxGradientOperator[cols_][LineBox[points_]] := Scope[
  dists = Prepend[0] @ Accumulate @ MapWindowed[Apply @ EuclideanDistance, N @ RemoveOffsets @ points];
  colors = OklabBlend[cols, dists / Last[dists]];
  Construct[LineBox, points, VertexColors -> colors]
];

l_LineBoxGradientOperator[StyleBox[p_, other___]] := StyleBox[l @ p, other];
_LineBoxGradientOperator[other_] := EchoPF @ other;

(**************************************************************************************************)

toThick[Automatic | None] := Seq[];
toThick[n_] := AbsoluteThickness[n];

toColor[Automatic] := Seq[];
toColor[None] := Opacity[0];
toColor[col_] := col;

toOpacity[Automatic | None] := Seq[];
toOpacity[o_] := Opacity[o];

(**************************************************************************************************)

PublicFunction[SubscriptOperator]

SubscriptOperator[s_][e__] := Subscript[s, e];

(**************************************************************************************************)

PublicFunction[SetOperator]

SetOperator[value_] := Function[var, Set[var, value], {HoldAllComplete}];

(**************************************************************************************************)

PublicFunction[LookupOperator]

LookupOperator[a_][key_] := Lookup[a1, key];
LookupOperator[a_, (Rule|RuleDelayed)["Default", v_]][key_] := Lookup[a, key, v];
LookupOperator[a_, rest__][key_] := Lookup[a, key, LookupOperator[rest] @ key];
