Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


Unprotect[PathGraph];
(* fix a weird oversight in the design of PathGraph *)
PathGraph[n_Integer, opts___] := PathGraph[Range[n], opts];


PackageExport["DirectedCycle"]
PackageExport["UndirectedCycle"]

cyclicPairs[first_, vertices___] := Partition[{first, vertices, first}, 2, 1];

DirectedCycle[vertices___] := Splice[DirectedEdge @@@ cyclicPairs[vertices]];
UndirectedCycle[vertices___] := Splice[UndirectedEdge @@@ cyclicPairs[vertices]];


PackageExport["DirectedPath"]
PackageExport["UndirectedPath"]

DirectedPath[vertices___] := Splice[DirectedEdge @@@ Partition[List @ vertices, 2, 1]];
UndirectedPath[vertices___] := Splice[UndirectedEdge @@@ Partition[List @ vertices, 2, 1]];


PackageExport["Clique"]

Clique[vertices___] := Splice[UndirectedEdge @@@ Subsets[{vertices}, {2}]];


