PackageExport["MinimumIndexBy"]
PackageExport["MaximumIndexBy"]
PackageExport["MinimumIndices"]
PackageExport["MaximumIndices"]
PackageExport["MinimumIndex"]
PackageExport["MaximumIndex"]
PackageExport["MinimumBy"]
PackageExport["MaximumBy"]
PackageExport["Minimum"]
PackageExport["Maximum"]

(* these represent cliques of functions along abstract dimensions.
there are three abstract dimensions: By-ness, Sign, and Index-ness *)

SetRelatedSymbolGroup @@@ {
  (* Sign symmetry: MinimumFoo <-> MaximumFoo *)
  {Minimum,        Maximum},        {MinimumIndex,   MaximumIndex},   {MinimumIndices, MaximumIndices},
  {MinimumBy,      MaximumBy},      {MinimumIndexBy, MaximumIndexBy},

  (* Bi-ness symmetry: FooBy <-> Foo *)
  {MinimumIndexBy, MinimumIndex},   {MaximumIndexBy, MaximumIndex},
  {MinimumBy,      Minimum},        {MaximumBy,      Maximum},

  (* Index symmetry: FooIndex <-> Foo *)
  {MinimumIndex,   MinimumIndices,   Minimum},      {MinimumIndexBy,   MinimumBy},
  {MaximumIndex,   MaximumIndices,   Maximum},      {MaximumIndexBy,   MaximumBy}
};

(**************************************************************************************************)

SetUsage @ "
MinimumIndexBy[{e$1, e$2, $$}, f$] gives the first index i$ for which f$[e$i] is minimal.
"

SetUsage @ "
MaximumIndexBy[{e$1, e$2, $$}, f$] gives the first index i$ for which f$[e$i] is maximal.
"

MinimumIndexBy[list_, f_] :=
  First @ Ordering[f /@ list, 1];


MaximumIndexBy[list_, f_] :=
  First @ Ordering[f /@ list, -1];

(**************************************************************************************************)

SetUsage @ "
MinimumIndices[{e$1, e$2, $$}] gives the list of indices i$ for which e$i is minimal.
"

SetUsage @ "
MaximumIndices[{e$1, e$2, $$}] gives the list of indices i$ for which e$i is maximal.
"

MinimumIndices[list_] :=
  MinimalBy[Range @ Length @ list, Part[list, #]&];

MaximumIndices[list_] :=
  MaximalBy[Range @ Length @ list, Part[list, #]&];

(**************************************************************************************************)

SetUsage @ "
MinimumIndex[{e$1, e$2, $$}] gives the first index i$ for which e$i is minimal.
"

SetUsage @ "
MaximumIndex[{e$1, e$2, $$}] gives the first index i$ for which e$i is maximal.
"

MinimumIndex[list_] :=
  First @ Ordering[list, 1];

MaximumIndex[list_] :=
  First @ Ordering[list, -1];

(**************************************************************************************************)

SetUsage @ "
MinimumBy[{e$1, e$2, $$}, f$] gives the first index i$ for which f$[e$i] is minimal.
"

SetUsage @ "
MaximumBy[{e$1, e$2, $$}, f$] gives the first index i$ for which f$[e$i] is maximal.
"

MinimumBy[list_, f_] :=
  Part[list, First @ Ordering[f /@ list, 1]];

MaximumBy[list_, f_] :=
  Part[list, First @ Ordering[f /@ list, -1]];

(**************************************************************************************************)

SetUsage @ "
Minimum[{e$1, e$2, $$}] gives the maximal e$i.
"

SetUsage @ "
Maximum[{e$1, e$2, $$}] gives the minimal e$i.
"

Minimum[list_] :=
  Part[list, First @ Ordering[list, 1]];

Maximum[list_] :=
  Part[list, First @ Ordering[list, -1]];

(**************************************************************************************************)

PackageExport["OrderSort"]

OrderSort[list_, None] :=
  Sort @ list;

OrderSort[list_, order_] :=
  Part[list, Ordering[
    FirstPosition[order, #, Null, {1}]& /@ list
  ]];

OrderSort[order_][list_] := OrderSort[list, order];

(**************************************************************************************************)

PackageExport["MostCommon"]

MostCommon[{}, _:First] := None;
MostCommon[list_, f_:First] := f @ MaximumIndices @ Counts @ list;
