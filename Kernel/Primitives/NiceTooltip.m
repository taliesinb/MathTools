PublicGraphicsPrimitive[NiceTooltip]

DeclareGraphicsPrimitive[NiceTooltip, "Primitives", graphicsTooltipBoxes, {2, 3}];

(* TODO: support 3D *)
graphicsTooltipBoxes[NiceTooltip[a_, b_, size_:{400,300}]] :=
  NiceTooltipBox[
    ToGraphicsBoxes @ a,
    ToBoxes @ NicePane @ b,
    size
  ];

