Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


PackageExport["GraphPlottingFunction"]


PackageExport["GraphEpilog"]

SetUsage @ "
GraphEpilog is an option to Graph that specifies a list of additional visual \
elements to put on the graph when plotted.
* Supported annotations include:
| Highlighted[elems$] | highlight a set of vertices or edges |
| Foliated[{v$1, $$, v$n}] | draw lines between pages of a foliation of the vertices |
| Outlined[elems$] | draw an outline around a set of vertices |
| Styled[elems$, opts$$] | style particular vertices or edges differently |
| Line[elems$] | equivalent to Highlighted[Line[elems$]] |
| Arrow[elems$] | like Line, but draw the highlight as an arrow |
In addition, the following wrappers can be used around $elem:
| Legended[elems$, legend$] | attach an additional legend |
| Labeled[elems$, label$] | labek the annotation in-place |
The spec elem$ can be any of the following:
| {$$} | a list of edges and/or vertices |
| Line[{v$1, v$2}] | the geodesic between v$1 and v$2
| Line[{v$1, v$2}, c$] | start at v$1, move in cardinal direction c$, and end when v$2 is reached. |
| Line[{v$1, $$, v$n}] | the geodesic path between v$1 and v$2, v$2 and v$3, etc. |
| HalfLine[v$, c$] | a geodesic starting at v$ and continuing in the cardinal direction c$. |
| HalfLine[{v$1, v$2}] | a geodesic starting at v$1 and continuing through v$2. |
| InfiniteLine[v$, c$] | a geodesic with midpoint v$, and continuing in directions c$ and Negated[c$]. |
| InfiniteLine[{v$1, v$2}] | a geodesic intersecting v$1 and v$2. |
| Polygon[{v$1, $$, v$n}] | all vertices surrounded by the given vertices, and their mutual edges. |
| Disk[v$, r$] | all vertices reachable in up to r$ edges from v$, and their mutual edges. |
| Circle[v$, r$] | all vertices exactly r$ edges from v$, and their mutual edges. |
In the specifications above, a cardinal c$ can also be a list {c$1, ..., c$n}, which indicates that the \
geodesic should take the cardinals c$1, c$2, $$, c$n, c$1, c$2, $$.
"


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
addGraphOption[GraphEpilog, None];
addGraphOption[GraphLegend, None];

PackageScope["$simpleGraphOptions"]
PackageScope["$simpleGraphOptionRules"]

$simpleGraphOptions = {
  ImageSize, ImagePadding, GraphLayout, VertexCoordinates,
  VertexLabels, EdgeLabels,
  GraphLegend, GraphEpilog,
  DirectedEdges
};

$simpleGraphOptionRules = TakeOptions[Graph, $simpleGraphOptions];

Unprotect[Graph];
FormatValues[Graph] = {};

PackageScope["$Graph"]
PackageScope["$GraphVertexCoordinates"]
PackageScope["$GraphVertexCoordinateBoundingBox"]
PackageScope["$GraphVertexList"]
PackageScope["$GraphEdgeList"]

PackageScope["$GraphVertexCoordinateDistanceMatrix"]
PackageScope["$GraphVertexCoordinateMinDistance"]

$graphOverrideOuter = True;

Graph /: MakeBoxes[g_Graph ? GraphQ, StandardForm] /;
  $graphOverrideOuter && Or[
      HasAnnotationQ[g, GraphLegend],
      HasAnnotationQ[g, GraphPlottingFunction],
      HasAnnotationQ[g, GraphEpilog]
    ] := customMakeGraphBoxes[g];

Protect[Graph];



customMakeGraphBoxes[graph_Graph] := Scope[

  {plotFunc, legend, graphEpilog} = Replace[
    AnnotationValue[graph, {GraphPlottingFunction, GraphLegend, GraphEpilog}],
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

      $GraphEdgeCoordinateLists := $GraphEdgeCoordinateLists = obtainEdgeCoordinateLists[];
      $GraphEdgeCoordinateListsAssociation := AssociationThread[$GraphEdgeList, $GraphEdgeCoordinateLists];
      $GraphVertexCoordinates = vertexCoordinates;
      $GraphVertexCoordinatesAssociation := AssociationThread[$GraphVertexList, $GraphVertexCoordinates];
      $GraphVertexCoordinateDistanceMatrix = DistanceMatrix[vertexCoordinates];
      $GraphVertexCoordinateBoundingBox := $GraphVertexCoordinateBoundingBox = CoordinateBoundingBox[vertexCoordinates];

    If[plotFunc =!= None,
      graph = plotFunc[graph]];

    boxes = Block[{$graphOverrideOuter = False}, stripDynamicModule @ RawBoxes @ Construct[MakeBoxes, graph]];

    $GraphPlotRange := $GraphPlotRange = GraphicsPlotRange @ First @ boxes;
    $GraphPlotImageSize := $GraphPlotImageSize := LookupImageSize[boxes];
    $GraphPlotSize := $GraphPlotSize = (#2 - #1& @@@ $GraphPlotRange);

    $GraphMaxSafeVertexSize := $GraphMaxSafeVertexSize = computeMaxSafeVertexSize[];
    $GraphMaxSafeVertexSizeScaled := $GraphMaxSafeVertexSizeScaled = $GraphMaxSafeVertexSize / First[$GraphPlotSize];

    boxes = applyGraphEpilog[boxes, graphEpilog];
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

stripDynamicModule[boxes_] := ReplaceAll[boxes,
  NamespaceBox[
    "NetworkGraphics",
    HoldPattern[DynamicModuleBox[{Typeset`graph = _}, TagBox[subBoxes_, _], ___]]
  ] :> subBoxes
];

obtainEdgeCoordinateLists[] := Scope[
  bag = Internal`Bag[];
  GraphComputation`GraphDrawing[Graph[$Graph,
    VertexShapeFunction -> Function[Null],
    EdgeShapeFunction -> Function[Internal`StuffBag[bag, #1]]]];
  Internal`BagPart[bag, All]
];

computeMaxSafeVertexSize[] := Scope[
  minDistance = Min @ DeleteCases[Flatten @ $GraphVertexCoordinateDistanceMatrix, 0|0.];
  {xAll, yAll} = Transpose @ $GraphVertexCoordinates;
  {{xMin, xMax}, {yMin, yMax}} = $GraphPlotRange;
  borderDistance = Min[xAll - xMin, xMax - xAll, yAll - yMin, yMax - yAll];
  Min[borderDistance, minDistance / 2]
];

computeMinDistance[] := Min @ DeleteCases[Flatten @ $GraphVertexCoordinateDistanceMatrix, 0|0.];

applyLegend[expr_, None] := expr;
applyLegend[expr_, legend_] := Legended[expr, legend];

applyLabel[expr_, Placed[label_, pos_]] := Labeled[expr, label, pos];
applyLabel[expr_, label_] := Labeled[expr, label];
applyLabel[expr_, None] := expr;

applyGraphEpilog[boxes_, None | {}] := boxes;
applyGraphEpilog[boxes_, list_List] := Scope[
  $epilogBag = Internal`Bag[];
  Scan[processEpilogSpec, list];
  epilogBoxes = First @ ToBoxes @ Graphics @ Internal`BagPart[$epilogBag, All];
  gboxPos = FirstPosition[boxes, _GraphicsBox];
  Insert[boxes, Epilog -> epilogBoxes, Join[gboxPos, {2, -1}]]
];

toGraphicsBoxes[g_] := ToBoxes @ Graphics @ g;

sowEpilog[g_] := Internal`StuffBag[$epilogBag, g];

$baseHighlightStyle := $baseHighlightStyle = Opacity[.5, First @ $ColorPalette];

Options[CustomHighlightedOptions] = {
  Background -> Automatic
};

GraphEpilog::badelem = "Unknown epilog element ``.";
processEpilogSpec[other_] := Message[GraphEpilog::badelem, Shallow[other]];

processEpilogSpec[Framed] :=
  sowEpilog @ {EdgeForm[{Red, Dashed}], FaceForm[None], Rectangle @@ (Transpose @ $GraphPlotRange)}

processEpilogSpec[Highlighted[elems_, color_:$ColorPattern:Automatic]] := Scope[

  {vertices, edges, negations} = processRegionSpec[elems];
  If[edges =!= {}, vertices = Complement[vertices, Flatten @ Part[$IndexGraphEdgeList, edges]]];

  r= $GraphMaxSafeVertexSizeScaled * 2;

  style = color;
  SetAutomatic[style, $baseHighlightStyle];

  If[vertices =!= {},
    sowEpilog[{style, PointSize[r], Point @ Part[$GraphVertexCoordinates, vertices]}]];

  If[edges =!= {},
    sowEpilog[{style, JoinForm["Round"], CapForm["Round"], Thickness[r],
      toJoinedCurve @ Part[$GraphEdgeCoordinateLists, edges]}]];
];

toJoinedCurve[{a_}] := Line[a];
toJoinedCurve[list_List] := toSingleLine /@ Split[list, Last[#1] == First[#2]&]

toSingleLine[edgeCoords_] := Line[Append[edgeCoords[[All, 1]], edgeCoords[[-1, 2]]]];

(* joinedCurveGroup[{a_}] := Line[a];
joinedCurveGroup[segments_] := JoinedCurve[Line /@ segments];
 *)


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

