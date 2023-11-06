PublicFunction[PathPlot]

PathPlot[graph_Graph, p_Path -> color_] :=
  PathPlot[graph, p, color];

PathPlot[graph_Graph, path_Path, color_:$Teal] :=
  HighlightGraphRegion[graph, path,
    {color, PathStyle -> "DiskArrow", PathRadius -> 3, DiskRadius -> 4, "Foreground", "SemitransparentArrowheads"},
    GraphLegend -> None
  ];

(**************************************************************************************************)

PublicFunction[PathWordPlot]

$pwpStyle = GrayLevel[0.25];
$pwpLabel = Word;

PathWordPlot[graph_Graph, p:Path[_, _Str, ___, PathCancellation -> False, ___]] := Block[
  {$pathCancellation = False},
  PathWordPlot[graph, MapAt[ToPathWord, p, 2]]
];

PathWordPlot[graph_Graph, path:Path[start_, word_, ___]] :=
  Labeled[
    PathPlot[graph, path, $pwpStyle],
    $pwpLabel /. Word :> PathWordForm[start, ToPathWord @ word, pathEndVertex[graph, path]]
  ]

PathWordPlot[graph_Graph, Labeled[path_, label_]] := Scope[
  $pwpLabel = label; PathWordPlot[graph, path]
];

PathWordPlot[graph_Graph, Style[path_, color_]] := Scope[
  $pwpStyle = color; PathWordPlot[graph, path]
];

PathWordPlot[graph_Graph, None] :=
  inlineSymbol["\[UpTee]", 30];

pathEndVertex[graph_, path_] := Scope[
  end = Part[GraphRegion[graph, Take[path, 2]], 1, 1, -1];
  Part[VertexList @ graph, end]
];

(**************************************************************************************************)

PublicFunction[PathConcatPlot]

PrivateFunction[inlineSymbol]

inlineSymbol[s_, args___] := Style[s, 20, args];

PathConcatPlot[args___, PathStyle -> style_] := Block[{$pwpStyle = style},
  PathConcatPlot[args]
];

PathConcatPlot[graph_, p1_, p2_, p3_] :=
  SpacedRow[
    PathWordPlot[graph, p1],
    inlineSymbol @ "\[Star]",
    PathWordPlot[graph, p2],
    inlineSymbol @ "=",
    PathWordPlot[graph, p3]
  ]

(**************************************************************************************************)

PublicFunction[PathComposePlot]

PathComposePlot[args___, PathStyle -> style_] := Block[{$pwpStyle = style},
  PathComposePlot[args]
];

PathComposePlot[graph_, p1_, p2_, p3_] :=
  SpacedRow[
    PathWordPlot[graph, p1],
    inlineSymbol @ $PathComposeSymbol,
    PathWordPlot[graph, p2],
    inlineSymbol @ "=",
    PathWordPlot[graph, p3]
  ]

PathComposePlot[graph_, p1_, p2_, p3_, p4_] :=
  SpacedRow[
    PathWordPlot[graph, p1],
    inlineSymbol @ $PathComposeSymbol,
    PathWordPlot[graph, p2],
    inlineSymbol @ "=",
    PathWordPlot[graph, p3],
    inlineSymbol @ "=",
    PathWordPlot[graph, p4]
  ]

(**************************************************************************************************)

PublicFunction[PathBuilder]

PathBuilder[vertex_, path_Str, adjustments_:{}] :=
  GraphRegionHighlight -> Style[Arrow[Path[vertex, path, PathAdjustments -> adjustments]], $Purple,
    "Foreground", HighlightRadius->0.3, EdgeSetback -> 2];

PathBuilder[vertex_, {path1_Str, path2_Str}] :=
  GraphRegionHighlight -> Style[
      {Style[Arrow[Path[vertex, path1]], $Purple],
       Style[Arrow[Path[vertex, path2]], $Teal]},
      "Foreground", "SemitransparentArrowheads", PathRadius -> 3, EdgeSetback -> 2];
