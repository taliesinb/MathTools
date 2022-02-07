PackageExport["DiscardVertices"]
PackageExport["SelectVertices"]

DiscardVertices[graph_Graph, filter_] := Subgraph[graph, Discard[VertexList[graph], filter]];
SelectVertices[graph_Graph, filter_] := Subgraph[graph, Select[VertexList[graph], filter]];

(**************************************************************************************************)

PackageExport["GraphRelabel"]

GraphRelabel[graph_Graph, f_] :=
  VertexReplace[graph, Map[# -> f[#]&, VertexList[graph]]];

(**************************************************************************************************)

PackageExport["GraphContract"]

SetUsage @ "
GraphContract[graph$, {v$1, v$2, $$}] contracts a set of vertices into one vertex, yielding ContractedVertex[{v$1, v$2, $$}].
GraphContract[graph$, {{v$11, v$12, $$}, {v$21, v$22, $$}, $$}}] makes multiple contractions, which should be disjoint.
GraphContract[graph$, <|key$1 -> {v$11, v$12, $$}, {v$21, v$22, $$}, $$|>] contracts sets of vertices, naming them \
ContractedVertex[{v$i1, v$i2, $$}, key$].
"

GraphContract[g_, {} | <||>] := g;

GraphContract[g_, contraction_List] :=
  VertexReplace[
    VertexContract[g, vertices],
    Map[
      vertices |-> First[vertices] -> ContractedVertex[vertices],
      toListOfLists @ contraction
    ]
  ];

GraphContract[g_, contraction_Association] :=
  VertexReplace[
    VertexContract[g, Values @ vertices],
    KeyValueMap[
      {key, vertices} |-> First[vertices] -> ContractedVertex[vertices, key],
      contraction
    ]
  ];

(**************************************************************************************************)

PackageExport["GraphContractBy"]

Options[GraphContractBy] = {
  VertexLabels -> Automatic
};

GraphContractBy[graph_Graph, func_] :=
  GraphContract[graph,
    If[OptionValue[VertexLabels] === Automatic,
      GroupBy[VertexList[graph], func],
      GatherBy[VertexList[graph], func]
    ]
  ];

(**************************************************************************************************)

PackageExport["GraphSum"]

SetUsage @ "
GraphSum[{g$1, g$2, $$}] takes the sum of a list of graphs g$i, yielding a single graph with vertices SumVertex[v$, i$], \
where v$ is a vertex from g$i.
GraphSum[<|k$1 -> g$1, k$2 -> g$2, $$|>] yields a graph with vertices SumVertex[v$, k$i].
* Any of the graphs g$i can also be an expression g$i -> vertex$. All such vertices will be contracted.
"

ListOrAssociationOf[pattern_] := {Repeated[pattern]} | Association[Repeated[_ -> pattern]];

indexToLabel[{k_} | {Key[k_]}] := k;

relabelSumComponent[graph_, label_] := GraphRelabel[graph, SumVertex[label]];

relabelSumComponent[graph_ -> joinVertex_, index_] := (
    AppendTo[$contractions, SumVertex[joinVertex, label]];
    relabelSumComponent[graph, index]
);

(* TODO: capture and combine the options of all the subgraphs *)
GraphSum[graphs:ListOrAssociationOf[_Graph | Rule[_Graph, _]], opts:OptionsPattern[Graph]] := Scope[
  $contractions = {};
  union = GraphUnion[Sequence @@ MapIndexed[relabelSumComponent[#1, indexToLabel[#2]], graphs], opts];
  GraphContract[union, $contractions]
];
