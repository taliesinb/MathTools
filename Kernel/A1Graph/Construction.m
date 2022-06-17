PublicFunction[ExtendedGraph]

ExtendedGraph[args___] :=
  interceptedGraphConstructor[Graph[args, GraphPlottingFunction -> ExtendedGraphPlottingFunction]];

(**************************************************************************************************)

Unprotect[PathGraph];
(* fix a weird oversight in the design of PathGraph *)
PathGraph[n_Integer, opts___] := PathGraph[Range[n], opts];

(**************************************************************************************************)

PublicFunction[DirectedCycle, UndirectedCycle]

cyclicPairs[first_, vertices___] := Partition[{first, vertices, first}, 2, 1];

DirectedCycle[vertices___] := Splice[DirectedEdge @@@ cyclicPairs[vertices]];
UndirectedCycle[vertices___] := Splice[UndirectedEdge @@@ cyclicPairs[vertices]];

(**************************************************************************************************)

PublicFunction[DirectedPath, UndirectedPath]

DirectedPath[vertices___] := Splice[DirectedEdge @@@ Partition[List @ vertices, 2, 1]];
UndirectedPath[vertices___] := Splice[UndirectedEdge @@@ Partition[List @ vertices, 2, 1]];

(**************************************************************************************************)

PublicFunction[Clique]

Clique[vertices___] := Splice[UndirectedEdge @@@ UnorderedPairs[{vertices}]];


