Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["RootPopulation"]

fmtInt[i_] := Which[
  Positive[i], i,
  Negative[i], UnderBar[Abs @ i],
  i == 0, Style[0, LightGray]
];

declareFormatting[
  RootPopulation[f_List] :> popForm[f, $Gray],
  NegatedForm[RootPopulation[f_List]] :> popForm[f, $LightRed]
];

popForm[f_List, color_] := RawBoxes @ CompactMatrixBox[{f}, FrameStyle -> color, "Factor" -> False];


PackageExport["MutationGameGroupoid"]

MutationGameGroupoid[graph_Graph] := Scope[
  count = VertexCount @ graph;
  outTable = VertexOutTable[graph];
  assoc = <|
    "Type" -> "MutationGame",
    "MirroredGenerators" -> Table[makeMutateGenerator[i, Part[outTable, i]], {i, count}],
    "InitialStates" -> makeSimpleRoots[count]
  |>;
  constructGroupoid[assoc]
];

makeSimpleRoots[n_] := Flatten @ Table[
  RootPopulation /@ {UnitVector[count, i], -UnitVector[count, i]},
  {i, count}
]

makeSimpleRoots[n_] :=
  Table[RootPopulation @ UnitVector[count, i], {i, count}];

PackageExport["MutateForward"]
PackageExport["MutateBackward"]

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