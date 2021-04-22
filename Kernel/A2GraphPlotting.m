Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


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
addGraphOption[GraphRegionHighlight, None];
addGraphOption[GraphLegend, None];


PackageExport["GraphPlottingFunction"]

SetUsage @ "
GraphPlottingFunction is an option to Graph that specifies a custom function to apply to \
the graph to produce a graphical representation.
* Various global variables are temporarily set during the application that allow properties \
of the graph to be accessed.
"


PackageScope["$GraphRegionTable"]

$GraphRegionTable = StringTrim @ "
| {$$} | a list of edges and/or vertices |
| Line[{v$1, v$2}] | the geodesic between v$1 and v$2 |
| Line[{v$1, v$2}, c$] | start at v$1, moving in cardinal direction c$, and end when v$2 is reached. |
| Line[{v$1, $$, v$n}] | the geodesic path between v$1 and v$2, v$2 and v$3, etc. |
| Path[v$, {c$1, $$, c$n} ] | starting at v$, walking along the path defined by cardinals c$i |
| HalfLine[v$, c$] | a geodesic starting at v$ and continuing in the cardinal direction c$ |
| HalfLine[{v$1, v$2}] | a geodesic starting at v$1 and continuing through v$2 |
| InfiniteLine[v$, c$] | a geodesic with midpoint v$, and continuing in directions c$ and Negated[c$] |
| InfiniteLine[{v$1, v$2}] | a geodesic intersecting v$1 and v$2 |
| Polygon[{v$1, $$, v$n}] | all vertices surrounded by the given vertices, and their mutual edges |
| Disk[v$, r$] | all vertices reachable in up to r$ edges from v$, and their mutual edges |
| Circle[v$, r$] | all vertices exactly r$ edges from v$, and their mutual edges |
* In the specifications above, a cardinal c$ can also be a list {c$1, ..., c$n}, which indicates that the \
geodesic should use the cardinals cyclically: c$1, c$2, $$, c$n, c$1, c$2, $$.
"

PackageScope["$GraphRegionHighlightUsage"]

$GraphRegionHighlightUsage = StringTrim @ "
* Each highlight specification can be one of the following:
| Highlighted[region$] | highlight a set of vertices or edges |
| Foliated[{v$1, $$, v$n}] | draw lines between pages of a foliation of the vertices |
| Outlined[region$] | draw an outline around a set of vertices |
| Styled[region$, opts$$] | style particular vertices or edges differently |
| Line[$$] | equivalent to Highlighted[Line[$$]] |
| Arrow[$$] | like Line, but draw the highlight as an arrow |
In addition, the following wrappers can be used around region$:
| Legended[elems$, legend$] | attach an additional legend |
| Labeled[elems$, label$] | labek the annotation in-place |
Each region$ can be one or more of the following:
<*$GraphRegionTable*>
"

PackageExport["GraphRegionHighlight"]

SetUsage @ "
GraphRegionHighlight is an option to Graph that specifies one or more highlighting directives to highlight \
specific regions of the graph when it is displayed.
<*$GraphRegionHighlightUsage*>
"


PackageExport["GraphLegend"]

SetUsage @ "
GraphLegend is an option to Quiver that creates a legend for the graph.
* GraphLegend -> None specifies no additional legend
* GraphLegend -> Automatic uses a legend for the cardinals
* GraphLegend -> legend$ specifies a particular legend
"

Unprotect[Graph];
FormatValues[Graph] = {};

$graphOverrideOuter = True;

Scan[form |-> (
  MakeBoxes[g_Graph ? GraphQ, form] /;
    Or[
      HasAnnotationQ[g, GraphLegend],
      HasAnnotationQ[g, GraphPlottingFunction],
      HasAnnotationQ[g, GraphRegionHighlight]
    ] := customMakeGraphBoxes[g];
  ),
  {StandardForm, TraditionalForm}
];

Protect[Graph];


PackageScope["$GraphVertexCoordinates"]
PackageScope["$GraphVertexCoordinateListsAssociation"]
PackageScope["$GraphVertexCoordinateDistanceMatrix"]

PackageScope["$GraphEdgeCoordinateLists"]
PackageScope["$GraphEdgeCoordinateListsAssociation"]

PackageScope["$GraphPlotRange"]
PackageScope["$GraphPlotImageSize"]
PackageScope["$GraphMaxSafeVertexSize"]

PackageScope["$GraphMaxSafeVertexSize"]
PackageScope["$GraphMaxSafeVertexSizeScaled"]


PackageExport["ExtendedGraphPlot"]

ExtendedGraphPlot[graph_] := Scope[

  If[!GraphQ[graph], ReturnFailed[]];

  {plottingFunction, graphLegend, graphEpilog} = Replace[
    AnnotationValue[graph, {GraphPlottingFunction, GraphLegend, GraphRegionHighlight}],
    $Failed -> None, {1}
  ];

  SetNone[plottingFunction, GraphComputation`GraphDrawing];

  GraphScope[graph,

    vertexCoordAssoc = edgeCoordAssoc = Data`UnorderedAssociation[];
    GraphComputation`GraphDrawing @ Graph[$IndexGraph,
      VertexShapeFunction -> Function[vertexCoordAssoc[#2] = #1;],
      EdgeShapeFunction -> Function[edgeCoordAssoc[#2] = #1;]
    ];

    $GraphPlotImageSize := $GraphPlotImageSize := LookupImageSize @ graph;

    $GraphVertexCoordinates = Lookup[vertexCoordAssoc, $IndexGraphVertexList, None]; vertexCoordAssoc = None;
    $GraphVertexCoordinatesAssociation := AssociationThread[$GraphVertexList, $GraphVertexCoordinates];
    $GraphVertexCoordinateDistanceMatrix := $GraphVertexCoordinateDistanceMatrix = DistanceMatrix[$GraphVertexCoordinates];
(*     $GraphVertexCoordinateBoundingBox := $GraphVertexCoordinateBoundingBox = CoordinateBoundingBox[$GraphVertexCoordinates];
 *)
    $GraphEdgeCoordinateLists = Lookup[edgeCoordAssoc, $IndexGraphEdgeList, None]; edgeCoordAssoc = None;
    $GraphEdgeCoordinateListsAssociation := $GraphEdgeCoordinateListsAssociation = AssociationThread[$GraphEdgeList, $GraphEdgeCoordinateLists];

    (* before we have called the user function, guess the range based on the vertex and edge coordinates *)
    $GraphPlotRange := $GraphPlotRange = CoordinateBounds[{$GraphVertexCoordinates, $GraphEdgeCoordinateLists}];
    $GraphPlotSize := $GraphPlotSize = rangeSize[$GraphPlotRange];
    $GraphMaxSafeVertexSizeScaled := $GraphMaxSafeVertexSize / First[$GraphPlotSize];
    $GraphMaxSafeVertexSize := $GraphMaxSafeVertexSize = computeMaxSafeVertexSize[];

    graphics = plottingFunction[$Graph];

    $GraphPlotRange := $GraphPlotRange = GraphicsPlotRange @ graphics;
    $GraphPlotSize := $GraphPlotSize = rangeSize[$GraphPlotRange];
    $GraphMaxSafeVertexSize := $GraphMaxSafeVertexSize = computeMaxSafeVertexSize[];

    graphics = applyGraphRegionHighlight[graphics, graphEpilog];
    graphics = applyLegend[graphics, graphLegend];

    graphics
  ]
];

rangeSize[range_] := EuclideanDistance @@@ range;

computeMaxSafeVertexSize[] := Scope[
  minDistance = Min @ DeleteCases[Flatten @ $GraphVertexCoordinateDistanceMatrix, 0|0.];
  (* this might be cheaper via CoordinateBounds *)
  {xAll, yAll} = Transpose @ $GraphVertexCoordinates;
  {{xMin, xMax}, {yMin, yMax}} = $GraphPlotRange;
  borderDistance = Min[xAll - xMin, xMax - xAll, yAll - yMin, yMax - yAll];
  If[borderDistance == 0, borderDistance = Infinity];
  Min[borderDistance, minDistance / 2]
];

customMakeGraphBoxes[graph_Graph] :=
  stripDynamicModule @ ToBoxes @ ExtendedGraphPlot[graph];

(*
customMakeGraphBoxes[graph_Graph] := Scope[

  {plotFunc, legend, graphEpilog} = Replace[
    AnnotationValue[graph, {GraphPlottingFunction, GraphLegend, GraphRegionHighlight}],
    $Failed -> None, {1}
  ];

  {vertexCoordinates, graphLayout} = Lookup[
    Options[graph, {VertexCoordinates, GraphLayout}],
    {VertexCoordinates, GraphLayout}, Automatic
  ];

  If[vertexCoordinates === Automatic,
    vertexCoordinates = GraphEmbedding[graph, graphLayout];
    graph = Graph[graph, VertexCoordinates -> vertexCoordinates];
  ];

  GraphScope[graph,

      $GraphVertexCoordinates = vertexCoordinates;
      $GraphVertexCoordinatesAssociation := AssociationThread[$GraphVertexList, $GraphVertexCoordinates];
      $GraphVertexCoordinateDistanceMatrix = DistanceMatrix[vertexCoordinates];
      $GraphVertexCoordinateBoundingBox := $GraphVertexCoordinateBoundingBox = CoordinateBoundingBox[vertexCoordinates];

      $GraphEdgeCoordinateLists := $GraphEdgeCoordinateLists = captureEdgeCoordinateLists[];
      $GraphEdgeCoordinateListsAssociation := $GraphEdgeCoordinateListsAssociation = AssociationThread[$GraphEdgeList, $GraphEdgeCoordinateLists];

    If[plotFunc =!= None,
      graph = plotFunc[graph]];

    boxes = Block[{$graphOverrideOuter = False}, stripDynamicModule @ RawBoxes @ Construct[MakeBoxes, graph]];

    $GraphPlotRange := $GraphPlotRange = GraphicsPlotRange @ First @ boxes;
    $GraphPlotImageSize := $GraphPlotImageSize := LookupImageSize[boxes];
    $GraphPlotSize := $GraphPlotSize = (#2 - #1& @@@ $GraphPlotRange);

    $GraphMaxSafeVertexSize := $GraphMaxSafeVertexSize = computeMaxSafeVertexSize[];
    $GraphMaxSafeVertexSizeScaled := $GraphMaxSafeVertexSizeScaled = $GraphMaxSafeVertexSize / First[$GraphPlotSize];

    boxes = applyGraphRegionHighlight[boxes, graphEpilog];
    If[FreeQ[boxes, Graphics3DBox], With[{plotRange = $GraphPlotRange},
      boxes = boxes /. GraphicsBox[args___] :> GraphicsBox[args,
        PlotRange -> plotRange, PlotRangePadding -> 0,
        ImagePadding -> 3,
        PlotRangeClipping -> False]
    ]];
    graph = applyLegend[boxes, legend];
  ];

  Construct[MakeBoxes, graph]
];
*)

stripDynamicModule[boxes_] := ReplaceAll[boxes,
  NamespaceBox[
    "NetworkGraphics",
    HoldPattern[DynamicModuleBox[{Typeset`graph = _}, TagBox[subBoxes_, _], ___]]
  ] :> subBoxes
];

applyLegend[expr_, None] := expr;
applyLegend[expr_, legend_] := Legended[expr, legend];

applyLabel[expr_, Placed[label_, pos_]] := Labeled[expr, label, pos];
applyLabel[expr_, label_] := Labeled[expr, label];
applyLabel[expr_, None] := expr;

applyGraphRegionHighlight[graphics_, None | {}] := graphics;
applyGraphRegionHighlight[graphics_, elem_] := applyGraphRegionHighlight[graphics, {elem}];
applyGraphRegionHighlight[graphics_, list_List] := Scope[
  $epilogBag = Internal`Bag[];
  Scan[processHighlightSpec, list];
  epilog = Internal`BagPart[$epilogBag, All];
  AppendEpilog[graphics, epilog]
];

(********************************************)
(** highlight processing code              **)
(********************************************)

sowEpilog[g_] := Internal`StuffBag[$epilogBag, g];

$baseHighlightStyle := $baseHighlightStyle = Opacity[.5, First @ $ColorPalette];

Options[CustomHighlightedOptions] = {
  Background -> Automatic
};

GraphRegionHighlight::badelem = "Unknown highlight element ``.";

processHighlightSpec[other_] := Message[GraphRegionHighlight::badelem, Shallow[other]];

processHighlightSpec[Framed] :=
  sowEpilog @ {EdgeForm[{Red, Dashed}], FaceForm[None], Rectangle @@ (Transpose @ $GraphPlotRange)}

processHighlightSpec[expr_ ? GraphRegionElementQ] :=
  processHighlightSpec @ Highlighted @ expr;

processHighlightSpec[Highlighted[elems_, color:$ColorPattern:Automatic]] := Scope[

  {vertices, edges, negations} = processRegionSpec[elems];
  If[edges =!= {}, vertices = Complement[vertices, AllVertices @ Part[$IndexGraphEdgeList, edges]]];

  r = $GraphMaxSafeVertexSizeScaled * 1.5;

  style = color;
  SetAutomatic[style, $baseHighlightStyle];

  If[vertices =!= {},
    sowEpilog[{style, PointSize[r], Point @ Part[$GraphVertexCoordinates, vertices]}]];

  If[edges =!= {},
    sowEpilog[{style, JoinForm["Round"], CapForm["Round"], Thickness[r],
      edgeCoords = Part[$GraphEdgeCoordinateLists, edges];
      If[False && negations =!= {}, edgeCoords //= MapAt[Reverse, List /@ negations]];
      toJoinedCurve @ edgeCoords
    }]
  ];
];

toJoinedCurve[{a_}] := Line[a];

flipToMatch[a_, b_] := If[First[a] === First[b], Reverse @ a, a];
flipCoordinateLists[line_] := flipToMatch @@@ Partition[line, 2, 1, 1];

toJoinedCurve[list_List] := toSingleLine /@ Split[flipCoordinateLists @ list, Last[#1] == First[#2]&];

toSingleLine[edgeCoords_] := Line[Append[edgeCoords[[All, 1]], edgeCoords[[-1, 2]]]];

(* joinedCurveGroup[{a_}] := Line[a];
joinedCurveGroup[segments_] := JoinedCurve[Line /@ segments];
 *)


PackageExport["AppendEpilog"]

AppendEpilog[graph_Graph, epilog_] := Scope[
  oldEpilog = AnnotationValue[graph, GraphRegionHighlight];
  oldEpilog = If[FailureQ[oldEpilog], {}, Developer`ToList @ oldEpilog];
  Annotate[graph, GraphRegionHighlight -> Join[oldEpilog, Developer`ToList @ epilog]]
];

AppendEpilog[graphics_Graphics, epilog_] :=
  UpdateOptions[graphics, Epilog, Function[Append[Developer`ToList @ #1, epilog]]];


PackageExport["GraphEmbeddingGallery"]

$layouts = {
  "GravityEmbedding", "HighDimensionalEmbedding", "PlanarEmbedding",
  "SpectralEmbedding", "SpringElectricalEmbedding", "SpringEmbedding", "TutteEmbedding"
};

GraphEmbeddingGallery[g_Graph] := Table[Graph[g, GraphLayout -> layout, PlotLabel -> layout], {layout, $layouts}]



PackageExport["FastGraph3D"]

FastGraph3D[g_, opts___] := Graph3D[
  VertexList[g], EdgeList[g],
  VertexStyle -> Directive[Opacity[1], EdgeForm[None], GrayLevel[0]],
  EdgeStyle -> Directive[Opacity[0.5], GrayLevel[0.6]],
  EdgeShapeFunction -> "Line", VertexShapeFunction -> "Point"
]


PackageExport["ShowLabels"]
PackageExport["VertexLabelForm"]
PackageExport["VertexTooltipForm"]

ShowLabels[e_] := VertexLabelForm[e];
VertexLabelForm[e_] := e /. (g_Graph ? GraphQ) :> RuleCondition @ Graph[g, VertexLabels -> "Name", ImagePadding -> 20];
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

