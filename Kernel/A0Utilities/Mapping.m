PublicFunction[PostComposeFunction]

PostComposeFunction[HoldPattern[Fn][body_], fn2_] := Fn[fn2[body]];
PostComposeFunction[HoldPattern[Fn][var_, body_], fn2_] := Fn[var, fn2[body]];
PostComposeFunction[HoldPattern[Fn][var_, body_, attr_], fn2_] := Fn[var, fn2[body], attr];
PostComposeFunction[fn_, fn2_] := fn /* fn2;

(**************************************************************************************************)

PublicFunction[ConstructHoldComplete]

ConstructHoldComplete[fn_Fn, args___] :=
  PostComposeFunction[fn, HoldComplete][args];

ConstructHoldComplete[Apply[fn_Fn], {args___}] :=
  PostComposeFunction[fn, HoldComplete][args];

ConstructHoldComplete[fn_, args___] :=
  HoldComplete[fn[args]];

(**************************************************************************************************)

PublicFunction[CollectWhile]

SetUsage @ "
CollectWhile[body$, test$, item$] runs body$, then test$, and collects item$ into the running list if True.
"

SetHoldAll[CollectWhile];

CollectWhile[body_, test_, collector_] := Module[{bag = Bag[]},
  While[True, body; If[test, StuffBag[bag, collector], Break[]]];
  BagPart[bag, All]
];

_CollectWhile := BadArguments[];

(**************************************************************************************************)

PublicFunction[ZipMap, ZipScan]

ZipMap[f_, args___] := MapThread[f, {args}];
ZipScan[f_, args___] := (ZipMap[f, args];);

(**************************************************************************************************)

PublicFunction[ThreadApply]

SetUsage @ "
ThreadApply[{f$1, f$2, $$}, {e$1, e$2, $$}] returns {f$1[e$1], f$2[e$2], $$}.
ThreadApply[fns$] is the operator form of ThreadApply.
"

ThreadApply[fns_List, items_List] := MapThread[Construct, {fns, items}];
ThreadApply[fns_][items_] := ThreadApply[fns, items];

(**************************************************************************************************)

PublicFunction[Unthread]

SetUsage @ "
Unthread[{e$1, e$2, $$}] turns the outer expression into a list of expressions, one for each e$i.
* not just %List but any head is supported.
"

(* todo: implement these as macros, and better yet as syntax in Loader.m *)

ClearAll[Unthread];

Unthread /: Rule[lhs_, Unthread[rhs_]] :=
  Unthread @ Map[lhs -> #&, rhs];

Unthread /: head_Symbol[l___, Unthread[a_], r___] := With[
  {u = Unique["\[FormalO]"]},
  Map[u |-> head[l, u, r], a]
];

Unthread[a_, 0] := a;

Unthread /: head_Symbol[l___, Unthread[a_, n_Int], r___] := With[
  {u = Unique["\[FormalO]"]},
  Construct[Unthread, Map[u |-> head[l, u, r], a], n-1]
];

(**************************************************************************************************)

PublicFunction[MatrixMap]

MatrixMap[f_, matrix_] := Map[f, matrix, {2}];
MatrixMap[f_][matrix_] := Map[f, matrix, {2}];

(**************************************************************************************************)

PublicFunction[VectorReplace]

VectorReplace[vector_, rule_] := Replace[vector, rule, {1}];
VectorReplace[rule_][vector_] := Replace[vector, rule, {1}];

(**************************************************************************************************)

PublicFunction[MatrixReplace]

MatrixReplace[matrix_, rule_] := Replace[matrix, rule, {2}];
MatrixReplace[rule_][matrix_] := Replace[matrix, rule, {2}];

(**************************************************************************************************)

PublicFunction[VectorApply]

(* this is a named form of @@@, aka MapApply *)
VectorApply[f_, e_] := MapApply[f, e];
VectorApply[f_][e_] := MapApply[f, e];

(**************************************************************************************************)

PublicFunction[MatrixApply]

MatrixApply[f_, e_] := Apply[f, e, {2}];
MatrixApply[f_][e_] := Apply[f, e, {2}];

(**************************************************************************************************)

PublicFunction[ThreadAnd, ThreadOr, ThreadNot]

ThreadAnd[args__] := And @@@ Trans[args];
ThreadOr[args__] := Or @@@ Trans[args];
ThreadNot[arg_List] := Map[Not, arg];

(**************************************************************************************************)

PublicFunction[ThreadMin, ThreadMax]

SetListable[ThreadMin, ThreadMax]

ThreadMin[a__] := Min[a];
ThreadMax[a__] := Max[a];

(**************************************************************************************************)

PublicFunction[ThreadLess, ThreadLessEqual, ThreadGreater, ThreadGreaterEqual, ThreadEqual, ThreadUnequal, ThreadSame, ThreadUnsame]

ThreadLess[lists___]         := MapThread[Less, {lists}];
ThreadLessEqual[lists___]    := MapThread[LessEqual, {lists}];
ThreadGreater[lists___]      := MapThread[Greater, {lists}];
ThreadGreaterEqual[lists___] := MapThread[GreaterEqual, {lists}];
ThreadEqual[lists___]        := MapThread[Equal, {lists}];
ThreadUnequal[lists___]      := MapThread[Unequal, {lists}];
ThreadSame[lists___]         := MapThread[SameQ, {lists}];
ThreadUnsame[lists___]       := MapThread[UnsameQ, {lists}];

(**************************************************************************************************)

PublicFunction[MapIndex1]

SetUsage @ "
MapIndex1[f, arg] is equivalent to MapIndexed[f[#1, First[#2]]&, arg]
"

MapIndex1[f_, list_] := MapIndexed[Fn[{argX, partX}, f[argX, P1 @ partX]], list];
MapIndex1[f_][list_] := MapIndex1[f, list];

(**************************************************************************************************)

PublicFunction[MapIndexStack]

SetUsage @ "
MapIndexStack[f$, stack$, list$] maps f$ over list$, passing the index appended to stack$ as a second argument.
MapIndexStack[f$, stack$] is the operator form of MapIndexStack.
* This is useful to recurse over subexpressions while recording where you are.
"

MapIndexStack[f_, stack_, list_] := MapIndex1[f[#1, Append[stack, #2]]&, list];

(**************************************************************************************************)

PublicFunction[PartValueMap]

PartValueMap[f_, list_List] := MapIndexed[Fn[{argX, partX}, f[P1 @ partX, argX]], list];
PartValueMap[f_, assoc_Assoc] := KeyValueMap[f, assoc];
PartValueMap[f_][e_] := PartValueMap[f, e];

(**************************************************************************************************)

PublicFunction[PartValueScan]

PartValueScan[f_, list_List] := (MapIndexed[Fn[{argX, partX}, f[P1 @ partX, argX];], list];)
PartValueScan[f_, assoc_Assoc] := KeyValueScan[f, assoc];
PartValueScan[f_][e_] := PartValueScan[f, e];

(**************************************************************************************************)

PublicFunction[ScanIndex1]

SetUsage @ "
ScanIndex1[f$, arg$] is equivalent to ScanIndexed[f$[#1, First[#2]]&, arg$]
* ScanIndexed will not evaluate on arg$ if it is has attribute Hold etc.
"

(* TODO: fix the hold limitation *)
ScanIndex1[f_, list_] := (MapIndexed[Fn[{argX, partX}, f[argX, P1 @ partX];], list];)
ScanIndex1[f_][list_] := ScanIndex1[f, list];

(**************************************************************************************************)

PublicFunction[ApplyWindowed]

SetUsage @ "
ApplyWindowed[f$, {e$1, e$2, $$, e$n}] gives {f$[e$1, e$2], f$[e$2, e$3], $$, f$[e$(n-1), e$n]}.
"

ApplyWindowed[f_, list_] := f @@@ Partition[list, 2, 1];
ApplyWindowed[f_, list_, n_] := f @@@ Partition[list, n, 1];

(**************************************************************************************************)

PublicFunction[ApplyWindowedCyclic]

ApplyWindowedCyclic[f_, list_] := f @@@ Partition[list, 2, 1, 1];
ApplyWindowedCyclic[f_, list_, n_] := f @@@ Partition[list, n, 1, 1];

(**************************************************************************************************)

PublicFunction[MapWindowed]

MapWindowed[f_, list_] := f /@ Partition[list, 2, 1];
MapWindowed[f_, list_, n_] := f /@ Partition[list, n, 1];

(**************************************************************************************************)

PublicFunction[MapWindowedCyclic]

MapWindowedCyclic[f_, list_] := f /@ Partition[list, 2, 1, 1];
MapWindowedCyclic[f_, list_, n_] := f /@ Partition[list, n, 1, 1];

(**************************************************************************************************)

PublicFunction[MapTuples]

MapTuples[f_, pairs_] := Map[f, Tuples @ pairs];
MapTuples[f_, pairs_, n_] := Map[f, Tuples[pairs, n]];
MapTuples[f_][pairs_] := MapTuples[f, pairs];

(**************************************************************************************************)

PublicFunction[ApplyTuples]

ApplyTuples[f_, pairs_] := f @@@ Tuples[pairs];
ApplyTuples[f_, pairs_, n_] := f @@@ Tuples[pairs, n];
ApplyTuples[f_][pairs_] := ApplyTuples[f, pairs];

(**************************************************************************************************)

PublicFunction[MapIndices]

SetUsage @ "
MapIndices[f$, {i$1, i$2, $$},  {e$1, e$2, $$}] applies f$ selectively on elements e$(i$1), e$(i$2), $$.
MapIndices[f$, indices$] is the operator form of MapIndices.
* indices that don't exist are skipped.
"

MapIndices[f_, {}, expr_] := expr;

MapIndices[f_, indicesLists:{__List}, expr_] :=
  MapIndices[f, #, expr]& /@ indicesLists;

MapIndices[f_, indices_, expr_] :=
  SafeMapAt[f, expr, List /@ indices];

MapIndices[f_, indices_][expr_] := MapIndices[f, indices, expr];

(**************************************************************************************************)

PublicFunction[MapMost]

MapMost[f_, list_] := SafeMapAt[f, list, 1;;-2]
MapMost[f_][list_] := MapMost[f, list];

(**************************************************************************************************)

PublicFunction[MapRest]

MapRest[f_, list_] := SafeMapAt[f, list, 2;;];
MapRest[f_][list_] := MapRest[f, list];

(**************************************************************************************************)

PublicFunction[MapFirst]

MapFirst[f_, list_] := SafeMapAt[f, list, 1];
MapFirst[f_][list_] := MapFirst[f, list];

(**************************************************************************************************)

PublicFunction[MapFirstLast]

MapFirstLast[{f_, g_}, list_] := SafeMapAt[g, SafeMapAt[f, list, 1], -1];
MapFirstLast[f_, list_] := SafeMapAt[f, SafeMapAt[f, list, 1], -1];
MapFirstLast[f_][list_] := MapFirstLast[f, list];

(**************************************************************************************************)

PublicFunction[MapLast]

MapLast[f_, list_] := SafeMapAt[f, list, -1];
MapLast[f_][list_] := MapLast[f, list];

(**************************************************************************************************)

PublicFunction[SafeMapAt]

SafeMapAt[f_, expr_, parts:{__List}] := Internal`UnsafeQuietCheck[
  MapAt[f, expr, parts],
  Fold[SafeMapAt[f, #1, #2]&, expr, parts]
];

SafeMapAt[f_, expr_, part_] := Internal`UnsafeQuietCheck[
  MapAt[f, expr, part],
  expr
];

SafeMapAt[f_, part_][expr_] := SafeMapAt[f, expr, part];
