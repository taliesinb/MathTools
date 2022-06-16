PackageExport["MatrixMap"]

MatrixMap[f_, matrix_] := Map[f, matrix, {2}];
MatrixMap[f_][matrix_] := Map[f, matrix, {2}];

(**************************************************************************************************)

PackageExport["VectorReplace"]

VectorReplace[vector_, rule_] := Replace[vector, rule, {1}];
VectorReplace[rule_][vector_] := Replace[vector, rule, {1}];

(**************************************************************************************************)

PackageExport["MatrixReplace"]

MatrixReplace[matrix_, rule_] := Replace[matrix, rule, {2}];
MatrixReplace[rule_][matrix_] := Replace[matrix, rule, {2}];

(**************************************************************************************************)

PackageExport["VectorApply"]

(* this is a named form of @@@ *)
VectorApply[f_, e_] := Apply[f, e, {1}];
VectorApply[f_][e_] := Apply[f, e, {1}];

(**************************************************************************************************)

PackageExport["MatrixApply"]

MatrixApply[f_, e_] := Apply[f, e, {2}];
MatrixApply[f_][e_] := Apply[f, e, {2}];

(**************************************************************************************************)

PackageExport["MapUnevaluated"]

SetHoldAllComplete[MapUnevaluated]

MapUnevaluated[f_, args_] :=
  Map[f, Unevaluated[args]];

MapUnevaluated[Function[body_], args_] :=
  Map[Function[Null, body, HoldAllComplete], Unevaluated[args]];

MapUnevaluated[Function[args_, body_], args_] :=
  Map[Function[args, body, HoldAllComplete], Unevaluated[args]];

(**************************************************************************************************)

PackageExport["MapIndex1"]

SetUsage @ "
MapIndex1[f, arg] is equivalent to MapIndexed[f[#1, First[#2]]&, arg]
"

MapIndex1[f_, list_] := MapIndexed[Function[{argX, partX}, f[argX, First @ partX]], list];
MapIndex1[f_][list_] := MapIndex1[f, list];

(**************************************************************************************************)

PackageExport["PartValueMap"]

PartValueMap[f_, list_List] := MapIndexed[Function[{argX, partX}, f[First @ partX, argX]], list];
PartValueMap[f_, assoc_Association] := KeyValueMap[f, assoc];
PartValueMap[f_][e_] := PartValueMap[f, e];

(**************************************************************************************************)

PackageExport["ScanIndex1"]

SetUsage @ "
ScanIndex1[f, arg] is equivalent to ScanIndexed[f[#1, First[#2]]&, arg]
"

ScanIndex1[f_, list_] := (MapIndexed[Function[{argX, partX}, f[argX, First @ partX];], list];)
ScanIndex1[f_][list_] := ScanIndex1[f, list];

(**************************************************************************************************)

PackageExport["ApplyWindowed"]

SetUsage @ "
ApplyWindowed[f$, {e$1, e$2, $$, e$n}] gives {f$[e$1, e$2], f$[e$2, e$3], $$, f$[e$(n-1), e$n]}.
"

ApplyWindowed[f_, list_] := f @@@ Partition[list, 2, 1];
ApplyWindowed[f_, list_, n_] := f @@@ Partition[list, n, 1];

(**************************************************************************************************)

PackageExport["ApplyWindowedCyclic"]

ApplyWindowedCyclic[f_, list_] := f @@@ Partition[list, 2, 1, 1];
ApplyWindowedCyclic[f_, list_, n_] := f @@@ Partition[list, n, 1, 1];

(**************************************************************************************************)

PackageExport["MapWindowed"]

MapWindowed[f_, list_] := f /@ Partition[list, 2, 1];
MapWindowed[f_, list_, n_] := f /@ Partition[list, n, 1];

(**************************************************************************************************)

PackageExport["MapWindowedCyclic"]

MapWindowedCyclic[f_, list_] := f /@ Partition[list, 2, 1, 1];
MapWindowedCyclic[f_, list_, n_] := f /@ Partition[list, n, 1, 1];

(**************************************************************************************************)

PackageExport["MapTuples"]

MapTuples[f_, pairs_] := Map[f, Tuples @ pairs];
MapTuples[f_, pairs_, n_] := Map[f, Tuples[pairs, n]];
MapTuples[f_][pairs_] := MapTuples[f, pairs];

(**************************************************************************************************)

PackageExport["ApplyTuples"]

ApplyTuples[f_, pairs_] := f @@@ Tuples[pairs];
ApplyTuples[f_, pairs_, n_] := f @@@ Tuples[pairs, n];
ApplyTuples[f_][pairs_] := ApplyTuples[f, pairs];

(**************************************************************************************************)

PackageExport["MapIndices"]

SetUsage @ "
MapIndices[f$, {i$1, i$2, $$},  {e$1, e$2, $$}] applies f$ selectively on elements e$(i$1), e$(i$2), $$.
MapIndices[f$, indices$] is the operator form of MapIndices.
"

MapIndices[f_, {}, expr_] := expr;

MapIndices[f_, indicesLists:{__List}, expr_] :=
  MapIndices[f, #, expr]& /@ indicesLists;

MapIndices[f_, indices_, expr_] :=
  SafeMapAt[f, expr, List /@ indices];

MapIndices[f_, indices_][expr_] := MapIndices[f, indices, expr];

(**************************************************************************************************)

PackageExport["MapMost"]

MapMost[f_, list_] := SafeMapAt[f, list, 1;;-2]
MapMost[f_][list_] := MapMost[f, list];

(**************************************************************************************************)

PackageExport["MapRest"]

MapRest[f_, list_] := SafeMapAt[f, list, 2;;];
MapRest[f_][list_] := MapRest[f, list];

(**************************************************************************************************)

PackageExport["MapFirst"]

MapFirst[f_, list_] := SafeMapAt[f, list, 1];
MapFirst[f_][list_] := MapFirst[f, list];

(**************************************************************************************************)

PackageExport["MapLast"]

MapLast[f_, list_] := SafeMapAt[f, list, -1];
MapLast[f_][list_] := MapLast[f, list];

(**************************************************************************************************)

PackageExport["SafeMapAt"]

SafeMapAt[f_, expr_, part_] := Replace[MapAt[f, expr, part], _MapAt -> expr];
SafeMapAt[f_, part_][expr_] := SafeMapAt[f, expr, part];
