PublicObject[SpringLayout]
PublicOption[Orientation]

Options[SpringLayout] = {
  "EnergyControl" -> Auto,
  "StepControl" -> Auto,
  "StepLength" -> Auto,
  "Tolerance" -> Auto
};

SpringLayout[opts:OptionsPattern[]][data_] :=
  VertexEdgeCoordinateData[data, {"SpringEmbedding", opts}]
