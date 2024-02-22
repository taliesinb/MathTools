PublicFunction[PathHomomorphimsGrid]

Options[PathHomomorphimsGrid] = {
  HighlightColor -> $DarkGreen
}

PathHomomorphimsGrid[graphsAndPaths_:{Repeated[_-> _]}, OptionsPattern[]] := Scope[
  UnpackOptions[highlightColor];
  {graphs, paths} = KeysValues @ graphsAndPaths;
  If[!MatrixQ[paths], ReturnFailed[]];
  {graphs, labelRow} = Transpose @ Map[toPHGColumnLabel, graphs];
  entries = MapThread[
    MapThread[pathHomomorphismDiagram, {graphs, {##}}]&,
    paths
  ];
  alignment = {{Center}};
  If[F[graphs] === "Paths", PreTo[alignment, Right]];
  If[L[graphs] === "Paths", PreTo[alignment, Left]];
  Grid[
    Pre[labelRow] @ entries,
    Spacings -> {{0, {2}, 0}, {10., 0.5, {0}}}, Alignment -> {alignment, Baseline}
  ]
]

toPHGColumnLabel = Case[
  Labeled[g_Graph, label_]     := {g, Style[label, $LabelStyle, Bold]};
  g_Graph                      := {g, ""};
  s:(_Str | _Form | _Style) := {"Path", s};
];

pathHomomorphismDiagram["Path", path_] :=
  path;

pathHomomorphismDiagram[graph_Graph, path_] := Scope[
  HighlightGraphRegion[graph,
    Style[path, highlightColor, PathRadius->2, DiskRadius -> 4,  ArrowheadSize -> 3.5, "Opaque", "SemitransparentArrowheads", PathStyle -> "DiskArrow"],
    VertexLabels->None,
    ArrowheadShape -> "Line", VertexStyle -> $LightGray, EdgeStyle -> $LightGray,
    ImagePadding -> {{15,15},{10,10}},
    ArrowheadPosition -> 0.55
  ]
];

(**************************************************************************************************)

PublicFunction[IllustrateIsomorphism]

IllustrateIsomorphism[source_, target_] := Scope[
  iso = FindGraphIsomorphism[source, target];
  iso = F[iso, ReturnFailed[]];
  SpacedRow[
    ExtendedGraph[source, VertexLabels -> "Name" -> iso],
    VertexLabelForm @ target
  ]
];
