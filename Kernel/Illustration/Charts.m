PublicFunction[HighlightChartRegion]

Options[HighlightChartRegion] = {
  "Color" -> Auto,
  "Arrowheads" -> "Cardinals",
  "PreserveColors" -> True,
  "Lighter" -> 0,
  "Label" -> True
}

HighlightChartRegion[graph_, chart_, OptionsPattern[]] := Scope[
  UnpackOptions[color, arrowheads, preserveColors, lighter, label];
  cardinals = ChartSymbolCardinals @ chart;
  SetAutomatic[color, HumanBlend @ Decases[$DarkGray] @ LookupCardinalColors[graph, cardinals]];
  If[lighter != 0, color = ColorConvert[MapAt[# - lighter&, ColorConvert[color, Hue], 2], RGBColor]];
  result = HighlightGraphRegion[
    graph,
    chart, {color,
      If[preserveColors, "ReplaceEdges", "Replace"],
      If[arrowheads === None, "HideArrowheads", Nothing],
      If[arrowheads === All, "FadeEdges", "FadeGraph"],
      Cardinals -> cardinals
    },
    If[arrowheads === None, ArrowheadShape -> None, Sequence @@ {}],
    EdgeThickness -> VeryThick,
    VisibleCardinals -> If[arrowheads === "Cardinals", cardinals, All],
    GraphLegend -> None
  ];
  If[label === True, result = Labeled[result, chart]];
  result
];

(**************************************************************************************************)

PublicFunction[FadePathPlot]

Options[FadePathPlot] = {
  "Labels" -> None,
  "HideArrowheads" -> True
};

FadePathPlot[g_, line_, OptionsPattern[]] := Scope[
  UnpackOptions[labels, hideArrowheads];
  If[labels =!= None,
    Return @ FadePathPlotWithLabels[g, line, labels, hideArrowheads]];
  initialVertices = Map[pathInitialVertex, ToList @ line];
  HighlightGraphRegion[g,
    {line, Point /@ initialVertices},
    {$Teal, RegionStyle -> "Highlight", "Replace", "FadeGraph", If[hideArrowheads, "HideArrowheads", Nothing], PathRadius -> 2, PointSize -> 8},
    GraphLegend -> None, VertexSize -> Map[# -> 8&, initialVertices]
  ]
];

pathInitialVertex[Line[{v1_, ___}, ___]] := v1;

FadePathPlotWithLabels[g_, Line[{v1_, v2_}, dir_], c_List, hideArrowheads_] := Scope[
  mainPath = Line[{v1, v2}, dir];
  cLast = L @ c;
  If[ListQ[cLast], cLast //= F];
  If[RuleQ[cLast], cLast //= F];
  transportPath = Line @ {Offset[v2, Inverted @ cLast], Offset[v2, cLast]};
  mainPathVertices = FF[GraphRegion[g, mainPath]];
  If[Len[c] =!= Len[mainPathVertices], ReturnFailed[]];
  colors = LookupCardinalColors @ g;
  c = RepAll[c, s_Str /; KeyQ[colors, s] :> Style[s, Italic, colors @ s]];
  color = colors @ StripInverted @ cLast;
  c = MapAt[Row[{"      ", #}]&, c, -1];
  HighlightGraphRegion[g,
    {Style[transportPath, color, PathStyle -> "DiskArrow", EdgeSetback -> 0, ArrowheadSize -> 3], mainPath},
    {"Replace", "FadeGraph", $Teal, PathRadius -> 2, If[hideArrowheads, "HideArrowheads", Nothing]},
    GraphLegend -> None, VertexSize -> {v1 -> 8},
    VertexLabels -> AssocThread[IndexedVertex /@ mainPathVertices, c],
    VertexLabelStyle -> {LabelPosition -> Offset[{0, 3}], BaseStyle -> {FontColor -> $Gray, FontWeight -> Bold}}
  ]
];

toCardinalEdgePattern[v2_, c_] := EdgePattern[IndexedVertex @ v2, _, c];
toCardinalEdgePattern[v2_, Inverted[c_]] := EdgePattern[_, IndexedVertex @ v2, c];

(**************************************************************************************************)

PublicFunction[CompassPathPlot]

CompassPathPlot[compass_, path_, color_:$Red] :=
  HighlightGraphRegion[compass,
    {Arrow[path]},
    {color, "Foreground", PathStyle -> "DiskArrow", PathRadius -> 3},
    Epilog -> GraphicsValue[{"CardinalPrimitives", All, _}]
  ];

(**************************************************************************************************)

PublicFunction[MobiusStrip]

MobiusStrip[n_, is3D_:False] := Scope[
  $n = n; tauN = Tau / n; $isA = Table[i <= n/2, {i, 0, n-1}];
  $isC = RotateLeft[$isA, Ceiling[n/3]];
  $isB = RotateRight[$isA, Ceiling[n/3]];
  edges = mobiousPatch /@ Range[0, n-1];
  vertices = Flatten @ Table[LatticeVertex[{x, y}], {x, 0, n-1}, {y, -1, 1}];
  coords = If[is3D,
    Catenate @ Table[TorusVector[{n, y}, {phi, phi/2}], {phi, 0, Tau - tauN, tauN}, {y, -1, 1}],
    F /@ vertices
  ];
  Quiver[vertices, edges,
    VertexCoordinates -> coords,
    EdgeShapeFunction -> If[is3D, Auto, drawMobiusEdge],
    ImagePadding -> {{20, 20}, {20, 0}}, Cardinals -> {"x", "r", "g", "b"},
    ArrowheadPosition -> <|"r" -> 0.33, "g" -> 0.66, "b" -> {0.4, .6}, "x" -> 0.5|>,
    GraphOrigin -> LatticeVertex[{Floor[n/2], 0}], ArrowheadShape -> {"Line", EdgeThickness -> 2},
    EdgeStyle -> GrayLevel[0.5, 0.4]
  ]
];

mlv[x_, y_] := LatticeVertex[{Mod[x, $n], If[x == $n || x == -1, -y, y]}];

mobiousPatch[x_] := <|
  "x" -> Table[mlv[x, y] -> mlv[x + 1, y], {y, -1, 1}],
  toCards[x] -> {DirectedPath[mlv[x, -1], mlv[x, 0], mlv[x, 1]]}
|>;
toCardinalSet[{e_}] := e;
toCardinalSet[e_] := CardinalSet[e];
toCards[n_] := toCardinalSet @ Pick[{"r", "g", If[n < $n/2, Inverted @ "b", "b"]}, {Part[$isA, n+1], Part[$isB, n+1], Part[$isC, n+1]}];

drawMobiusEdge[assoc_] := Scope[
  UnpackAssociation[assoc, coordinates, arrowheads, shape, edgeIndex];
  {a, b} = {{ax, ay}, {bx, by}} = FirstLast[coordinates];
  lines = If[Dist[a, b] > 1,
    ab = Normalize[b - a];
    ab *= If[Abs[F[ab]] > Abs[L[ab]], {1, 0}, {0, 1}] * 0.8;
    {l, r} = {{b + ab, b}, {a, a - ab}};
    counter = assoc["Counter"];
    {shape /@ {l, r}, {Opacity[1, $DarkGray], Text[counter, Mean @ #, {0, 1.8}]& /@ {l, r}}}
  ,
    shape @ {a, b}
  ];
  Style[lines, arrowheads]
];