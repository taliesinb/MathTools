Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

$novelGraphOptionsRules = {
  GraphPlottingFunction -> None,
  GraphRegionHighlight -> None,
  GraphLegend -> None,
  ArrowheadSize -> Automatic,
  ArrowheadStyle -> Automatic,
  VertexColorFunction -> None,
  VertexSizeFunction -> None
};

$novelGraphOptionSymbols = Keys @ $novelGraphOptionsRules;

$novelGraphOptionRulePattern = Rule[Alternatives @@ $novelGraphOptionSymbols, _];

$notIntercepted = True;

Unprotect[Graph];
Options[Graph] = JoinOptions[Graph, $novelGraphOptionsRules];
g:Graph[___] /; MemberQ[Unevaluated @ g, $novelGraphOptionRulePattern] && $notIntercepted :=
  Block[{$notIntercepted = False}, interceptedGraphConstructor[g]];
Protect[Graph];

SetHoldAllComplete[interceptedGraphConstructor];

interceptedGraphConstructor[Graph[Shortest[args__], options__Rule]] := Scope[
  annotations = Cases[{options}, $novelGraphOptionRulePattern];
  newOptions = DeleteCases[{options}, $novelGraphOptionRulePattern];
  result = Graph[args, newOptions];
  If[!GraphQ[result], ReturnFailed[]];
  Annotate[result, annotations]
];

interceptedGraphConstructor[e_] := e;

(**************************************************************************************************)

PackageExport["ArrowheadSize"]
PackageExport["ArrowheadStyle"]
PackageExport["ArrowheadSize"]

SetUsage @ "ArrowheadSize is an option to Quiver.";
SetUsage @ "ArrowheadStyle is an option to Quiver.";

(**************************************************************************************************)

PackageExport["GraphPlottingFunction"]

SetUsage @ "
GraphPlottingFunction is an option to Graph that specifies a custom function to apply to \
the graph to produce a graphical representation.
* Various global variables are temporarily set during the application that allow properties \
of the graph to be accessed. See GraphPlotScope for more info.
"

(**************************************************************************************************)

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

(**************************************************************************************************)

PackageExport["GraphRegionHighlight"]

SetUsage @ "
GraphRegionHighlight is an option to Graph that specifies one or more highlighting directives to highlight \
specific regions of the graph when it is displayed.
<*$GraphRegionHighlightUsage*>
"

(**************************************************************************************************)

PackageExport["GraphLegend"]

SetUsage @ "
GraphLegend is an option to Quiver that creates a legend for the graph.
* GraphLegend -> None specifies no additional legend
* GraphLegend -> Automatic uses a legend for the cardinals
* GraphLegend -> legend$ specifies a particular legend
"

(**************************************************************************************************)

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

customMakeGraphBoxes[graph_Graph] :=
  stripDynamicModule @ ToBoxes @ ExtendedGraphPlotDispatch @ graph;

stripDynamicModule[boxes_] := ReplaceAll[boxes,
  NamespaceBox[
    "NetworkGraphics",
    HoldPattern[DynamicModuleBox[{Typeset`graph = _}, TagBox[subBoxes_, _], ___]]
  ] :> subBoxes
];

(**************************************************************************************************)

PackageScope["$GraphVertexCoordinates"]
PackageScope["$GraphVertexCoordinateListsAssociation"]
PackageScope["$GraphVertexCoordinateDistanceMatrix"]
PackageScope["$GraphEdgeCoordinateLists"]
PackageScope["$GraphEdgeCoordinateListsAssociation"]
PackageScope["$GraphIs3D"]
PackageScope["$GraphPlotRange"]
PackageScope["$GraphPlotAspectRatio"]
PackageScope["$GraphPlotImageSize"]
PackageScope["$GraphMaxSafeVertexSize"]
PackageScope["$GraphMaxSafeVertexSize"]
PackageScope["$GraphMaxSafeVertexSizeScaled"]

PackageExport["GraphPlotScope"]

SetHoldRest[GraphPlotScope];

GraphPlotScope[graph_, body_] := Scope[

  If[!GraphQ[graph], ReturnFailed[]];

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
    $GraphIs3D := $GraphIs3D = MatchQ[Dimensions @ $GraphVertexCoordinates, {_, 3}];
    $GraphPlotRange := $GraphPlotRange = CoordinateBounds[{$GraphVertexCoordinates, $GraphEdgeCoordinateLists}];
    $GraphPlotSize := $GraphPlotSize = rangeSize[$GraphPlotRange];
    $GraphPlotAspectRatio := Last[$GraphPlotSize] / First[$GraphPlotSize];
    $GraphMaxSafeVertexSizeScaled := $GraphMaxSafeVertexSize / First[$GraphPlotSize];
    $GraphMaxSafeVertexSize := $GraphMaxSafeVertexSize = computeMaxSafeVertexSize[];

    body
  ]
];

(**************************************************************************************************)

PackageExport["ExtendedGraphPlotDispatch"]

ExtendedGraphPlotDispatch[graph_] := Scope[

  If[!GraphQ[graph], ReturnFailed[]];

  {plottingFunction, graphLegend, graphRegionHighlight} =
    LookupAnnotation[graph, {GraphPlottingFunction, GraphLegend, GraphRegionHighlight}, None];

  SetNone[plottingFunction, GraphComputation`GraphDrawing];

  GraphPlotScope[graph,

    graphics = plottingFunction[$Graph];

    If[MatchQ[graphics, _Legended],
      If[MatchQ[graphLegend, None | Automatic | Placed[Automatic, _]], graphLegend = Last[graphics]];
      graphics = First[graphics];
    ];

    (* recompute these with the results of plottingFunction, for the benefit of GraphRegionHighlight *)
    $GraphPlotRange := $GraphPlotRange = GraphicsPlotRange @ graphics;
    $GraphPlotSize := $GraphPlotSize = rangeSize[$GraphPlotRange];
    $GraphMaxSafeVertexSize := $GraphMaxSafeVertexSize = computeMaxSafeVertexSize[];

    graphics = ApplyEpilog[graphics, graphRegionHighlight];
    graphics = ApplyLegend[graphics, graphLegend];

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

(**************************************************************************************************)

PackageExport["GraphEmbeddingGallery"]

$layouts = {
  "GravityEmbedding", "HighDimensionalEmbedding", "PlanarEmbedding",
  "SpectralEmbedding", "SpringElectricalEmbedding", "SpringEmbedding", "TutteEmbedding"
};

GraphEmbeddingGallery[g_Graph] := Table[Graph[g, GraphLayout -> layout, PlotLabel -> layout], {layout, $layouts}]

(**************************************************************************************************)

PackageExport["ShowLabels"]
PackageExport["VertexLabelForm"]
PackageExport["VertexTooltipForm"]

ShowLabels[e_] := VertexLabelForm[e];
VertexLabelForm[e_] := e /. (g_Graph ? GraphQ) :> RuleCondition @ Graph[g, VertexLabels -> "Name", ImagePadding -> 20];
VertexTooltipForm[e_] := e /. (g_Graph ? GraphQ) :> RuleCondition @ Graph[g, VertexLabels -> Placed["Name", Tooltip]];

(**************************************************************************************************)

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

(**************************************************************************************************)

PackageExport["TransformGraphCoordinates"]

TransformGraphCoordinates[f_, graph_, method_] :=
  Graph[graph, VertexCoordinates -> Map[f, GraphEmbedding[graph, method]]];

