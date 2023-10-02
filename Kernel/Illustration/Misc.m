PublicFunction[PairwiseTable]

Options[PairwiseTable] = {
  ShowLabels -> False,
  LabelFunction -> BoldForm,
  TableSpacing -> 2
}

PairwiseTable[f_, list_, OptionsPattern[]] := Scope[
  UnpackOptions[showLabels, labelFunction, tableSpacing];
  table = Outer[f, list, list, 1];
  If[showLabels,
    labels = Map[labelFunction, list];
    TableForm[table, TableHeadings -> {labels, labels}, TableSpacing -> tableSpacing]
  ,
    TableForm[table]
  ]
];

(**************************************************************************************************)

PublicFunction[VertexField1DPlot]

VertexField1DPlot[vals_] := ListLinePlot[vals,
  Joined -> False, Filling -> {1 -> Axis},
  FillingStyle -> Directive[Opacity[.5],CapForm[None], AbsoluteThickness[2]], PlotMarkers->{Automatic, 0.12},
  Frame -> True, FrameStyle -> $LightGray, FrameTicks -> None, Axes -> None, GridLines -> {{}, {-0.025}},
  PlotStyle -> $DarkGray,
  PlotRange -> {{1, All}, {-1, 1}}, PlotRangePadding -> 0.4, ImageSize -> 125, AspectRatio -> 1/2.2
]

(**************************************************************************************************)

PublicFunction[GridGraphics]

SetUsage @ "
GridGraphics[g$] displays g$ over plot range {-1, 1}, with some padding.
GridGraphics[g$, n$] displays g$ over plot range {-n$, n$}.
* each unit of plot range corresponds to 100 pixels, e.g. %GraphicsScale -> 100.
* minor gridlines occur at every 0.2, and major at every 1 plotrange.
"

Options[GridGraphics] = {
  GraphicsScale -> 100
};

GridGraphics[g_, opts___Rule] := GridGraphics[g, 1, opts];

GridGraphics[g_, n_Integer, opts___Rule] :=
  GridGraphics[g, {-n, n}, opts];

GridGraphics[g_, {l_, h_}, opts___Rule] := Graphics[g,
  FilterOptions @ opts,
  ImageSize -> (((h - l)+.2)) * Lookup[{opts}, GraphicsScale, 100],
  PlotRange -> {{l-.1, h+.1}, {l-.1, h+.1}},
  Frame -> True, FrameTicks -> None, GridLines -> {
      glPair /@ Range[l, h, .2],
      glPair /@ Range[l, h, .2]
    }, GridLinesStyle -> GrayLevel[0.9]
];

glPair[0|0.] := {0, GrayLevel[0.4]};
glPair[i_] := If[i == Round[i], {i, GrayLevel[0.7]}, i];

(**************************************************************************************************)

PublicFunction[LargeSymbolForm]

LargeSymbolForm[e_, opts___Rule] := inlineSymbol[e, opts];


(**************************************************************************************************)

PublicFunction[ExportNotebookOutputs]

ExportNotebookOutputs[destination_String, prefix_String:"", sz_:3] := Scope[
  EnsureDirectory[destination];
  If[FileType[destination] =!= Directory, ReturnFailed[]];
  outputCells = NotebookImport[EvaluationNotebook[], "Output" -> "Cell"];
  Print["# of cells: ", Length @ outputCells];
  $i = 1;
  Scan[cell |-> (
    path = PathJoin[destination, prefix <> IntegerString[$i++, 10, 3] <> ".png"];
    image = Rasterize[cell, ImageFormattingWidth -> Infinity, ImageResolution -> Ceiling[144 * sz]];
    Print["Rasterizing ", ImageDimensions[image], " to ", path];
    Export[path, image])
  ,
    outputCells
  ];
];


