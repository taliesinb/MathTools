PublicFunction[MinimumIndexBy, MaximumIndexBy, MinimumIndices, MaximumIndices, MinimumIndex, MaximumIndex, MinimumBy, MaximumBy, Minimum, Maximum]

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
MinimumIndexBy[{e$1, e$2, $$}, f$] gives the first index i$ for which f$[e$i] is minimal as computed by Ordering.
"

SetUsage @ "
MaximumIndexBy[{e$1, e$2, $$}, f$] gives the first index i$ for which f$[e$i] is maximal as computed by Ordering.
"

MinimumIndexBy[list_, f_] :=
  F @ Ordering[f /@ list, 1];

MinimumIndexBy[{}, _] := None;

MaximumIndexBy[list_, f_] :=
  F @ Ordering[f /@ list, -1];

MaximumIndexBy[{}, _] := None;

(**************************************************************************************************)

SetUsage @ "
MinimumIndices[{e$1, e$2, $$}] gives the list of indices i$ for which e$i is minimal.
"

SetUsage @ "
MaximumIndices[{e$1, e$2, $$}] gives the list of indices i$ for which e$i is maximal.
"

MinimumIndices[list_] :=
  MinimalBy[Range @ Len @ list, Part[list, #]&];

MaximumIndices[list_] :=
  MaximalBy[Range @ Len @ list, Part[list, #]&];

(**************************************************************************************************)

SetUsage @ "
MinimumIndex[{e$1, e$2, $$}] gives the first index i$ for which e$i is minimal as computed by Ordering.
"

SetUsage @ "
MaximumIndex[{e$1, e$2, $$}] gives the first index i$ for which e$i is maximal as computed by Ordering.
"

MinimumIndex[list_] :=
  F @ Ordering[list, 1];

MinimumIndex[{}] := None;

MaximumIndex[list_] :=
  F @ Ordering[list, -1];

MaximumIndex[{}] := None;

(**************************************************************************************************)

SetUsage @ "
MinimumBy[{e$1, e$2, $$}, f$] gives the first e$i for which f$[e$i] is minimal as computed by Ordering.
"

SetUsage @ "
MaximumBy[{e$1, e$2, $$}, f$] gives the first e$i for which f$[e$i] is maximal as computed by Ordering.
"

MinimumBy[list_, f_] :=
  Part[list, F @ Ordering[f /@ list, 1]];

MinimumBy[{}, _] := None;

MaximumBy[list_, f_] :=
  Part[list, F @ Ordering[f /@ list, -1]];

MaximumBy[{}, _] := None;

(**************************************************************************************************)

SetUsage @ "
Minimum[{e$1, e$2, $$}] gives the minimal e$i as computed by Ordering.
"

SetUsage @ "
Maximum[{e$1, e$2, $$}] gives the maximal e$i as computed by Ordering.
"

Minimum[list_] :=
  Part[list, F @ Ordering[list, 1]];

Minimum[{}] := None;

Maximum[list_] :=
  Part[list, F @ Ordering[list, -1]];

Maximum[{}] := None;

(**************************************************************************************************)

PublicFunction[OrderSort]

OrderSort[list_, None] :=
  Sort @ list;

OrderSort[list_, order_] :=
  Part[list, Ordering[
    FirstPosition[order, #, Null, {1}]& /@ list
  ]];

OrderSort[order_][list_] := OrderSort[list, order];

(**************************************************************************************************)

PublicFunction[MostCommon]

MostCommon[{}, _:F] := None;
MostCommon[list_, f_:F] := f @ MaximumIndices @ Counts @ list;
