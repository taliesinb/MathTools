PublicObject[SpringLayout]
PublicOption[Orientation]

Options[SpringLayout] = {
  "EnergyControl" -> Automatic,
  "StepControl" -> Automatic,
  "StepLength" -> Automatic,
  "Tolerance" -> Automatic
};

SpringLayout[opts:OptionsPattern[]][data_] :=
  VertexEdgeCoordinateData[data, {"SpringEmbedding", opts}]
