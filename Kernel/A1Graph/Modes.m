PublicFunction[PlotGraphModes]

complexSortingValue[c_] := {-Round[Abs[c], 0.01], Mod[Arg[c+ $MachineEpsilon * I], 2Pi]}

PlotGraphModes[graph_Graph, k_:4, columns:_Int:4, opts:OptionsPattern[Graph]] := Scope[
  vertices = VertexList[graph];
  trans = AdjacencyMatrix[graph];
  {values, vectors} = Eigensystem[trans, k];
  ordering = OrderingBy[values, complexSortingValue];
  values = Part[values, ordering];
  vectors = Part[vectors, ordering];
  vectors = Map[Normalize[#, Max[Abs[#]]&]&, vectors];
  opts2 = Options[graph];
  If[OptionValue[VertexLabels] === Auto,
    opts = Decases[opts, VertexLabels -> _],
    opts2 = Decases[opts2, VertexLabels -> _]
  ];
  opts2 = Sequence @@ opts2; i = 1;
  plots = MapThread[
    Column[{
      PlotGraphVector[graph, #1, opts, opts2, ImagePadding -> {{5,  5}, {10, 5}}],
      Style[TextString[NumberForm[Chop @ #2, 3]] // SRep[" " -> ""], If[Abs[#2] == 1.0, Bold, {}]],
      #2,
      Row[{"#", i++}, BaseStyle -> Gray]
    }, Alignment->Center, Spacings -> 0]&,
    {vectors, values}
  ];
  Multicolumn[plots, columns, Spacings -> {2,0.5}, Appearance->"Horizontal", Alignment -> Center]
];
