PublicGraphicsPrimitive[PathedText]

SetUsage @ "
PathedText[curve$, text$] displays text$ along curve$
"

Options[PathedText] = {
  LabelFontSize -> Inherited,
  LabelBackground -> None
};

DeclareGraphicsPrimitive[PathedText, "Curve", pathedTextBoxes0];

(**************************************************************************************************)

pathedTextBoxes0[PathedText[curve_, labelData_, opts___Rule]] :=
  pathedTextBoxes[CurveToPoints @ curve, labelData, opts];