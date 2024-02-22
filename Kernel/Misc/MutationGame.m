PublicObject[RootPopulation]

fmtInt[i_] := Which[
  Positive[i], i,
  Negative[i], UnderBar[Abs @ i],
  i == 0, Style[0, LightGray]
];

declareFormatting[
  RootPopulation[f_List] :> popForm[f, $Gray]
];

popForm[f_List, color_] := RawBoxes @ CompactMatrixBox[{f}, FrameStyle -> color, "Factor" -> False];


PublicFunction[MutationGame]

MutationGame[graph_Graph] := Scope[
  vertexCount = VertexCount @ graph;
  outTable = VertexOutTable[graph];
  assoc = <|
    "Type" -> "MutationGame",
    "MirroredGenerators" -> Table[makeMutateGenerator[i, Part[outTable, i]], {i, vertexCount}],
    "InitialStates" -> makeSimpleRoots[vertexCount]
  |>;
  constructGroupoid[assoc]
];

makeSimpleRoots[n_] := Flatten @ Table[
  RootPopulation /@ {UnitVector[n, i], -UnitVector[n, i]},
  {i, n}
]

zmakeSimpleRoots[n_] :=
  Table[RootPopulation @ UnitVector[n, i], {i, n}];


PublicFunction[MutateForward, MutateBackward]

makeMutateGenerator[i_, out_] := GroupoidGenerator[
  MutateForward[i, out],
  MutateBackward[i, out]
];

MutateForward[i_, nbors_][RootPopulation[pop_]] :=
  RootPopulation @ MapAt[Total[Part[pop, nbors]] - #&, pop, i];

MutateBackward[i_, nbors_][RootPopulation[pop_]] :=
  RootPopulation @ MapAt[Total[Part[pop, nbors]] + #&, pop, i];

ToInverseFunction[m_MutateForward] := MutateBackward @@ m;
ToInverseFunction[m_MutateBackward] := MutateForward @@ m;


PublicFunction[EdgeMutationGame]

EdgeMutationGame[graph_Graph] := Scope[
  graph //= ToIndexGraph;
  vertexCount = VertexCount @ graph;
  edges = EdgeList @ graph;
  assoc = <|
    "Type" -> "EdgeMutationGame",
    "MirroredGenerators" -> Map[makeEdgeMutateGenerator, EdgeList @ graph],
    "InitialStates" -> makeSimpleRoots[vertexCount]
  |>;
  constructGroupoid[assoc]
];

makeEdgeMutateGenerator[DirectedEdge[i_, j_]] :=
  GroupoidGenerator[
    EdgeMutateForward[i, j],
    EdgeMutateBackward[i, j]
  ];

makeEdgeMutateGenerator[UndirectedEdge[i_, j_]] := Splice[{
  makeEdgeMutateGenerator @ DirectedEdge[i, j],
  makeEdgeMutateGenerator @ DirectedEdge[j, i]
}];

PublicFunction[EdgeMutateForward, EdgeMutateBackward]

EdgeMutateForward[i_, j_][RootPopulation[pop_]] :=
  RootPopulation @ RepPart[pop, j -> (Part[pop, i] - Part[pop, j])];

EdgeMutateBackward[i_, j_][RootPopulation[pop_]] :=
  RootPopulation @ RepPart[pop, j -> -(Part[pop, j] - Part[pop, i])];

ToInverseFunction[m_EdgeMutateForward] := EdgeMutateBackward @@ m;
ToInverseFunction[m_EdgeMutateBackward] := EdgeMutateForward @@ m;


PublicFunction[MultiMutationGame]

MultiMutationGame[graph_Graph] := Scope[
  graph //= ToIndexGraph;
  vertexCount = VertexCount @ graph;
  outTable = VertexOutTable @ graph;
  outMatrix = AdjacencyMatrix @ graph;
  assoc = <|
    "Type" -> "MultiMutationGame",
    "Generators" -> MapIndex1[makeMultiMutateGenerator[#2, #1, vertexCount]&, outTable],
    "InitialStates" -> Flatten[{
      Table[
        If[ i === j, {
          RepresentationElement @ DiagonalMatrix @ UnitVector[vertexCount, i],
          RepresentationElement @ DiagonalMatrix @ -UnitVector[vertexCount, i]
        },
        RepresentationElement @ DiagonalMatrix @ (UnitVector[vertexCount, i] - UnitVector[vertexCount, j])]
        , {i, vertexCount}, {j, vertexCount}]
    }]
  |>;
  constructGroupoid[assoc]
];

makeMultiMutateGenerator[i_, out_, n_] :=
  RepresentationElement @ RepPart[
    IdentityMatrix @ n,
    Normal @ Merge[Flatten @ {{i, i} -> -1, {#, i} -> 1& /@ out}, Total]
  ]

makeMultiMutateGenerator[i_, out_, n_] := Scope[
  id = IdentityMatrix[n];
  id[[All, i]] = -outMatrix[[i]];
  id[[i, i]] *= -1;
  RepresentationElement @ id
];

(* makeMultiMutateGenerator[i_, out_, n_] :=
  RepresentationElement @ ReplacePart[
    IdentityMatrix @ n,
    KeyValueMap[mergeRule, Echo @ GroupBy[Flatten @ {{i, i} -> -1, {#, i} -> 1& /@ out}, P1 -> PN]]
  ]

mergeRule[p:{i_, i_}, vals_] := p -> (Times @@ vals);
mergeRule[p:{i_, j_}, vals_] := p -> Total[vals];
 *)