PublicObject[SpringElectricalLayout]

Options[SpringElectricalLayout] = JoinOptions[
  "SpringConstant" -> 1,
  "RepulsiveForcePower" -> Auto,
  SpringLayout
];

SpringElectricalLayout[opts:OptionsPattern[]][data_] :=
  VertexEdgeCoordinateData[data, {"SpringElectricalEmbedding", opts}]
