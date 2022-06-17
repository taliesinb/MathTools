PublicObject[SpringElectricalLayout]

Options[SpringElectricalLayout] = JoinOptions[
  "SpringConstant" -> 1,
  "RepulsiveForcePower" -> Automatic,
  SpringLayout
];

SpringElectricalLayout[opts:OptionsPattern[]][data_] :=
  VertexEdgeCoordinateData[data, {"SpringElectricalEmbedding", opts}]
