PublicFunction[FadeGraphics]

FadeGraphics[g_, 0 | 0.] := g;
FadeGraphics[g_Graphics, opacity_] := ApplyEpilog[g,
  Style[
    Rectangle[{-1000, -1000}, {1000, 1000}],
    EdgeForm[None], FaceForm[GrayLevel[1, opacity]]
  ]
];

(**************************************************************************************************)

PublicFunction[ListShowFaded]

ListShowFaded[list_, i_, opacity_:0.9] := Scope[
  list = VectorReplace[list, g_Graph :> ExtendedGraphPlot[g]];
  {xs, ys} = Transpose[GraphicsPlotRange /@ list];
  {xmin, xmax} = MinMax @ xs;
  {ymin, ymax} = MinMax @ ys;
  faded = Show[
    Sequence @@ Delete[list, i],
    Graphics[Style[Rectangle[{xmin, ymin} - 1, {xmax, ymax} + 1], EdgeForm[None], FaceForm[GrayLevel[1, opacity]]]],
    Part[list, i],
    PlotRange -> {{xmin, xmax}, {ymin, ymax}}
  ]
]

(**************************************************************************************************)

PublicFunction[ShowFaded]

ShowFaded[items__, opacity_?NumericQ] := ListShowFaded[{items}, -1, opacity];
ShowFaded[items__] := ListShowFaded[{items}, -1];
