PublicHead[PathedText]

SetUsage @ "
PathedText[path, text]
"

declareGraphicsFormatting[
  {
    PathedText[path:($CoordMat2P | $GArrowIntP), labelData_, opts___Rule] :> pathedTextBoxes[path, labelData, opts]
  },
  Graphics
];

Options[PathedText] = {
	LabelFontSize -> Inherited,
	LabelBackground -> None
};

