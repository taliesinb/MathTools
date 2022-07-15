PublicObject[TutteLayout]

Options[TuttleLayout] = {
};

TutteLayout[opts:OptionsPattern[]][data_] :=
  VertexEdgeCoordinateData[data, {"TutteEmbedding", opts}]
