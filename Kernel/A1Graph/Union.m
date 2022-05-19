PackageExport["DisjointUnionLattice"]

Options[DisjointUnionLattice] = Options @ Graph;

DisjointUnionLattice[generators:{__List}, opts:OptionsPattern[]] := Scope[
  graph = MultiwaySystem[
    disjointUnionCayleyFunction[generators],
    {{}},
    "Graph"
  ];
  ExtendedGraph[graph, opts]
];

disjointUnionCayleyFunction[generators_][subsets_] :=
  MapIndexed[{gen, index} |-> If[
    AllTrue[subsets, DisjointQ[gen]],
    Labeled[Sort @ Append[subsets, gen], First @ index],
    Nothing
  ], generators];
