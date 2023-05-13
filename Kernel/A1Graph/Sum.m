PublicFunction[GraphDisjointSum]

SetUsage @ "
GraphDisjointSum[{g$1, g$2, $$}] takes the sum of a list of graphs g$i, yielding a single graph with vertices SumVertex[v$, i$], \
where v$ is a vertex from g$i.
GraphDisjointSum[<|k$1 -> g$1, k$2 -> g$2, $$|>] yields a graph with vertices SumVertex[v$, k$i].
* Any of the graphs g$i can also be an expression g$i -> vertex$. All such vertices will be contracted.
"

indexToLabel[{k_} | {Key[k_]}] := k;

relabelSumComponent[graph_, label_] := GraphRelabel[graph, SumVertex[label]];

relabelSumComponent[graph_ -> joinVertex_, index_] := (
    AppendTo[$contractions, SumVertex[joinVertex, label]];
    relabelSumComponent[graph, index]
);

(* TODO: capture and combine the options of all the subgraphs *)
GraphDisjointSum[graphs:ListOrAssociationOf[_Graph | Rule[_Graph, _]], opts:OptionsPattern[Graph]] := Scope[
  $contractions = {};
  union = GraphUnion[Sequence @@ MapIndexed[relabelSumComponent[#1, indexToLabel[#2]], graphs], opts];
  GraphContract[union, $contractions']
];
