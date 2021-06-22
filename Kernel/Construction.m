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


(**************************************************************************************************)

PackageExport["EquivalenceGraph"]

EquivalenceGraph[graph_, f_, newOpts___Rule] := Scope[
  vertices = VertexList @ graph;
  vrange = VertexRange @ graph;
  classes = Gather[vrange, f @@ Part[vertices, {#1, #2}]&];
  igraph = IndexGraph[graph];
  contracted = VertexContract[igraph, classes];
  {ivertices, iedges} = VertexEdgeList @ contracted;
  edges = MapAt[Part[vertices, #]&, iedges, {All, 1;;2}];
  vertices = Part[vertices, ivertices];
  opts = Options @ graph;
  ExtendedGraph[vertices, edges, VertexAnnotations -> None, GraphOrigin -> None, newOpts, Sequence @@ opts]
];
