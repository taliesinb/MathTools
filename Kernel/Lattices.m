Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


processEdge[DirectedEdge[a_, b_, tag_ -> delta_]] := (
  KeyAppendTo[$transitions, a, {b, tag, delta}];
  KeyAppendTo[$transitions, b, {a, Negated[tag], -delta}]
)
processEdge[edge_] := (Print["Unknown spec", edge];);
makeLatticeMachineFunction[graph_Graph] := Block[
  {edges, vertexList, $transitions},
  $transitions = <||>;
  Scan[processEdge, EdgeList[graph]];
  rules = KeyValueMap[{oldVertex, newStates} |->
    LatticeVertex[vec_, oldVertex] :> Evaluate[Apply[
      {newVertex, label, delta} |-> Hold[
        Labeled[LatticeVertex[vec + delta, newVertex], label]
      ],
      newStates, {1}
    ]],
    $transitions
  ] // ReleaseHold;
  AppendTo[rules, LatticeVertex[_, _] :> {}];
  rules // Dispatch // Replace
];


PackageExport["MaxNorm"]

SetUsage[MaxNorm,
"MaxNorm is an option to GenerateLattice that sets a maximum norm for lattice vectors."
];

Options[GenerateLattice] = Join[{MaxDepth -> 10, MaxNorm -> 5, NormFunction -> Norm}, Options[Graph]];

Clear[LatticeVertexNorm];
LatticeVertexNorm[LatticeVertex[v_, _], norm_] := N[norm[Abs[v]]];
LatticeVertexNorm[Labeled[s_, _], norm_] := LatticeVertexNorm[s, norm];

PackageExport["PartialMax"]

PartialMax[n_][e_] := PartialMax[e, n];
PartialMax[e_, n_Integer] := Max[Take[e, n]];
PartialMax[e_, n_List] := Dot[e, n];

GenerateLattice[machine_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[maxDepth, maxNorm, imageSize, normFunction];
  graph = ToLatticeMachine[machine];
  edges = EdgeList[graph];
  dim = FirstCase[edges, DirectedEdge[_, _, _ -> vec_] :> Length[vec]];
  If[!IntegerQ[dim], Return[$Failed]];
  function = makeLatticeMachineFunction[graph];
  istate = List @ LatticeVertex[ConstantArray[0, dim], First @ VertexList[graph]];
  If[NumberQ[maxNorm], function = function /* Select[LatticeVertexNorm[#, normFunction] <= maxNorm&]];
  graph = ExploreGraph[function, istate, MaxDepth -> maxDepth, DirectedEdges -> True];
  graph = DeleteDuplicates[EdgeList[graph], isNegatedEqual];
  CardinalGraph[graph, FilterOptions[Graph, opts], VertexLabels -> Placed["Name", Tooltip]]
];

isNegatedEqual[DirectedEdge[a_, b_, c_], DirectedEdge[b_, a_, Negated[c_]]] := True;
isNegatedEqual[DirectedEdge[a_, b_, Negated[c_]], DirectedEdge[b_, a_, c_]] := True;
isNegatedEqual[_, _] := False;

$edgeP = DirectedEdge | UndirectedEdge;


PackageExport["ToLatticeMachine"]

Options[ToLatticeMachine] = Options[CardinalGraph];

$cardVectorRules = <|
  {"x", "y"} -> <|"x" -> {1, 0}, "y" -> {0, 1}|>,
  {"x", "y", "z"} -> <|"x" -> {1, 0, 0}, "y" -> {0, 1, 0}, "z" -> {0, 0, 1}|>,
  {"a", "b", "c"} -> <|"a" -> {1, -1, 0}, "b" -> {1, 0, -1}, "c" -> {0, 1, -1}|>
|>;

ToLatticeMachine[spec_, opts:OptionsPattern[]] := Scope[
  graph = Graph[spec];
  edges = EdgeList[graph];
  cardinals = Union @ Cases[edges, $edgeP[_, _, s_String | Negated[s_String]] :> s];
  rules = Lookup[$cardVectorRules, Key @ cardinals, <||>];
  edges = edges /. {
    (h:$edgeP)[a_, b_, c_String] :> h[a, b, c -> Lookup[rules, c, Return[$Failed]]],
    (h:$edgeP)[a_, b_, Negated[c_String]] :> h[a, b, Negated[c] -> -Lookup[rules, c, Return[$Failed]]]
  };
  CardinalGraph[edges, opts]
];


PackageExport["DisplayLattice"]

Clear[DisplayLattice];

Options[DisplayLattice] = Options[GenerateLattice];

stripLabel[Labeled[e_, _]] := e;
stripLabel[e_] := e;
DisplayLattice[machine_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[imageSize, maxDepth, maxNorm, aspectRatio];
  machine = ToLatticeMachine[machine, ImageSize -> 100];
  latticePlot = GenerateLattice[machine, opts, GraphLegend -> None];
  Grid[{{machine, Spacer[10], stripLabel @ latticePlot}}]
];