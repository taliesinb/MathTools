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

PublicFunction[UnitGraphics]

UnitGraphics[g_, n_:1] := Graphics[g,
  ImageSize -> Medium, PlotRange -> {{-n, n}, {-n, n}}, PlotRangePadding -> Scaled[0.1],
  Frame -> True, FrameTicks -> None, GridLines -> {Range[-n, n, n / 5], Range[-n, n, n / 5]}
];

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


