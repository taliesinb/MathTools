Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


PackageExport["GraphPlottingFunction"]


PackageExport["GraphLegend"]

addGraphOption[symbol_, dvalue_] := (
  Unprotect[Graph];
  Options[Graph] = DeleteDuplicatesBy[Append[Options[Graph], symbol -> dvalue], First];
  (*Graph[graph_Graph ? GraphQ, symbol -> f_] := Annotate[graph, symbol -> f];*)
  Graph[lopts___, symbol -> f_, ropts___] := applyAnnotation[{lopts, ropts}, symbol -> f];
  Protect[Graph];
);

applyAnnotation[opts_, symbol_ -> None] :=
  Graph @@ DeleteCases[opts, symbol -> _];

applyAnnotation[opts_, symbol_ -> value_] :=
  Annotate[Graph @@ DeleteCases[opts, symbol -> _], symbol -> value];

addGraphOption[GraphPlottingFunction, None];
addGraphOption[GraphLegend, None];

Unprotect[Graph];
FormatValues[Graph] = {};

PackageScope["$currentVertexCount"]

$currentVertexCount = 1;

$graphOverrideOuter = True;

Graph /: MakeBoxes[g_Graph ? GraphQ, StandardForm] /;
  (HasAnnotationQ[g, GraphLegend] || HasAnnotationQ[g, GraphPlottingFunction]) && $graphOverrideOuter :=
    customMakeGraphBoxes[g];

Protect[Graph];

customMakeGraphBoxes[graph_Graph] := Scope[
  $currentVertexCount = VertexCount[graph];

  {plotFunc, legend} = Replace[
    AnnotationValue[graph, {GraphPlottingFunction, GraphLegend}],
    $Failed -> None, {1}
  ];

  If[plotFunc =!= None, graph = plotFunc[graph]];

  boxes = Block[{$graphOverrideOuter = False}, RawBoxes @ Construct[MakeBoxes, graph]];
  graph = applyLegend[boxes, legend];
  (* graph = applyLabel[graph, label]; *)

  Construct[MakeBoxes, graph]
];

applyLegend[expr_, None] := expr;
applyLegend[expr_, legend_] := Legended[expr, legend];

applyLabel[expr_, Placed[label_, pos_]] := Labeled[expr, label, pos];
applyLabel[expr_, label_] := Labeled[expr, label];
applyLabel[expr_, None] := expr;


PackageExport["HasAnnotationQ"]

HasAnnotationQ[obj_, key_] :=
  !MatchQ[AnnotationValue[obj, key], None | $Failed];


PackageExport["AttachAnnotation"]

AttachAnnotation[obj_, key_ -> None] :=
  If[AnnotationValue[obj, key] === $Failed, obj,
    AnnotationDelete[obj, key]];

AttachAnnotation[obj_, key_ -> value_] :=
  Annotate[obj, key -> value];

AttachAnnotation[obj_, rules_List] :=
  Fold[AttachAnnotation, obj, rules];


PackageExport["GraphEmbeddingGallery"]

$layouts = {
  "GravityEmbedding", "HighDimensionalEmbedding", "PlanarEmbedding",
  "SpectralEmbedding", "SpringElectricalEmbedding", "SpringEmbedding", "TutteEmbedding"
};

GraphEmbeddingGallery[g_Graph] := Table[Graph[g, GraphLayout -> layout, PlotLabel -> layout], {layout, $layouts}]



PackageExport["FastGraph3D"]

FastGraph3D[g_, opts___] := Graph3D[
  VertexList[g], EdgeList[g],
  EdgeShapeFunction -> "Line",
  VertexShapeFunction -> "Point"
]


PackageExport["ShowLabels"]
PackageExport["VertexLabelForm"]
PackageExport["VertexTooltipForm"]

ShowLabels[e_] := VertexLabelForm[e];
VertexLabelForm[e_] := e /. (g_Graph ? GraphQ) :> RuleCondition @ Graph[g, VertexLabels -> "Name"];
VertexTooltipForm[e_] := e /. (g_Graph ? GraphQ) :> RuleCondition @ Graph[g, VertexLabels -> Placed["Name", Tooltip]];


PackageExport["GraphComponentPlot"]

GraphComponentPlot[graph_] := Map[GraphPlot[#, ImageSize -> 400]&, ConnectedGraphComponents[graph]];


PackageExport["GraphComponentPlot3D"]

GraphComponentPlot3D[graph_] := Map[GraphPlot3D[#, ImageSize -> 400]&, ConnectedGraphComponents[graph]];


PackageExport["PlotGraphVector"]

PlotGraphVector[graph_Graph, opts___Rule][vector_List] :=
  PlotGraphVector[graph, vector, opts];

PlotGraphVector[graph_Graph, vector_, opts___Rule] := GraphPlot[graph,
  EdgeShapeFunction -> "Line", EdgeStyle -> LightGray,
  VertexShape -> MapThread[
    #1 -> ComplexDisk[#2, 20, 1]&,
    {VertexList[graph], vector}
  ],
  opts, VertexLabels -> None
];


PackageExport["TransformGraphCoordinates"]

TransformGraphCoordinates[f_, graph_, method_] :=
  Graph[graph, VertexCoordinates -> Map[f, GraphEmbedding[graph, method]]];


