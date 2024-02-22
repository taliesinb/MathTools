PublicGraphicsPrimitive[NiceTooltip]

DeclareGraphicsPrimitive[NiceTooltip, "Primitives", niceTooltipBoxes, {2, 3}];

(* TODO: support 3D *)
niceTooltipBoxes[NiceTooltip[a_, b_, size_:{400,300}]] :=
  NiceTooltipBoxes[
    ToGraphicsBoxes @ a,
    tooltipPrettyBoxes @ b, size
  ];

tooltipPrettyBoxes[e_] :=
  ToPrettifiedString[
    e,
    MaxIndent -> 10, FullSymbolContext -> False, ColorSymbolContext -> True,
    CompactingWidth -> 40, ElideLargeArrays -> True, InlineColors -> True,
    CompactRealNumbers -> True, TabSize -> None
  ];

(**************************************************************************************************)

MakeBoxes[NiceTooltip[a_, b_, sz_], form_] := NiceTooltipBoxes[MakeBoxes[a, form], MakeBoxes @ CompactPrettyForm @ b, sz];
MakeBoxes[NiceTooltip[a_, b_], form_] := NiceTooltipBoxes[MakeBoxes[a, form], MakeBoxes @ CompactPrettyForm @ b];
MakeBoxes[NiceTooltip[a_, None], form_] := MakeBoxes[a, Form];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[NiceTooltipBoxes]

NiceTooltipBoxes[a_, b_, wh_:{300, 300}] := TooltipBox[a,
  PaneBox[
    b,
    BaseStyle -> {FontSize -> 12, FontFamily -> "Fira Code"},
    ImageMargins -> {{5, 5}, {5, 5}},
    ImageSize -> {{30, F[wh, wh]}, {30, L[wh, wh]}}, Alignment -> Center
  ],
  TooltipStyle -> {Background -> GrayLevel[1], CellFrameColor -> None, CellFrame -> 0},
  TooltipDelay -> 0
] ~TagBox~ MouseAppearanceTag["Arrow"];

