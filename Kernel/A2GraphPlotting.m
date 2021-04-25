Package["GraphTools`"]

PackageImport["GeneralUtilities`"]

(**************************************************************************************************)

PackageExport["ArrowheadSize"]
PackageExport["ArrowheadStyle"]
PackageExport["VertexColorFunction"]

PackageExport["CardinalColors"]

SetUsage @ "VertexColorFunctin is an extended option to Graph."
SetUsage @ "ArrowheadSize is an extended option to Graph.";
SetUsage @ "ArrowheadStyle is an extended option to Graph.";
SetUsage @ "CardinalColors is an extended option to Graph."

(**************************************************************************************************)

PackageExport["GraphPlottingFunction"]

SetUsage @ "
GraphPlottingFunction is an extended option to Graph that specifies a custom function to apply to \
the graph to produce a graphical representation.
* Various global variables are temporarily set during the application that allow properties \
of the graph to be accessed. See GraphPlotScope for more info.
* None indicates the ordinary graph plotting codepath should be used.
* Automatic indicates that the default extended plotting codepath should be used.
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
* The spec Offset[v$, {c1$, $$}] can be used to refer to the vertex obtained by starting at v$ and moving \
along the given cardinals c$i.
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
GraphRegionHighlight is an extended option to Graph that specifies one or more highlighting directives to highlight \
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

Scan[form |-> MakeBoxes[g_Graph /; ExtendedGraphQ[Unevaluated[g]], form] := extendedGraphBoxes[g], {StandardForm, TraditionalForm}];

Protect[Graph];

extendedGraphBoxes[graph_Graph] :=
  stripDynamicModule @ ToBoxes @ ExtendedGraphPlot @ graph;

stripDynamicModule[boxes_] := ReplaceAll[boxes,
  NamespaceBox[
    "NetworkGraphics",
    HoldPattern[DynamicModuleBox[{Typeset`graph = _}, TagBox[subBoxes_, _], ___]]
  ] :> subBoxes
];

(**************************************************************************************************)

GraphPlot;
Unprotect[GraphPlot];

GraphPlot[g_Graph ? ExtendedGraphQ] := ExtendedGraphPlot[g];

Protect[GraphPlot];

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
PackageScope["$GraphMaxSafeVertexSizeScaled"]

PackageExport["GraphPlotScope"]

SetHoldRest[GraphPlotScope];

storeEdgeCoord[coords_, de_DirectedEdge] := $edgeCoordAssoc[de] = coords;
storeEdgeCoord[coords_, ue_UndirectedEdge] := ($edgeCoordAssoc[ue] = coords; $edgeCoordAssoc[Reverse @ ue] = coords);

GraphPlotScope[graph_, body_] := Scope[

  If[!GraphQ[graph], ReturnFailed[]];

  GraphScope[graph,

    (* todo: this will fail for multigraphs with identical edges! *)
    $vertexCoordAssoc = $edgeCoordAssoc = Data`UnorderedAssociation[];
    GraphComputation`GraphDrawing @ Graph[$IndexGraph,
      VertexShapeFunction -> Function[$vertexCoordAssoc[#2] = #1;],
      EdgeShapeFunction -> storeEdgeCoord,
      GraphLayout -> {"MultiEdgeDistance" -> 0.2}
    ];

    $GraphPlotImageSize := $GraphPlotImageSize := LookupImageSize @ graph;

    $GraphVertexCoordinates = Lookup[$vertexCoordAssoc, $IndexGraphVertexList, None]; $vertexCoordAssoc = None;
    $GraphVertexCoordinatesAssociation := AssociationThread[$GraphVertexList, $GraphVertexCoordinates];
    $GraphVertexCoordinateDistanceMatrix := $GraphVertexCoordinateDistanceMatrix = DistanceMatrix[$GraphVertexCoordinates];
(*     $GraphVertexCoordinateBoundingBox := $GraphVertexCoordinateBoundingBox = CoordinateBoundingBox[$GraphVertexCoordinates];
 *)
    $GraphEdgeCoordinateLists = Lookup[$edgeCoordAssoc, $IndexGraphEdgeList, None]; $edgeCoordAssoc = None;
    $GraphEdgeCoordinateListsAssociation := $GraphEdgeCoordinateListsAssociation = AssociationThread[$GraphEdgeList, $GraphEdgeCoordinateLists];

    (* before we have called the user function, guess the range based on the vertex and edge coordinates *)
    $GraphIs3D := $GraphIs3D = MatchQ[Dimensions @ $GraphVertexCoordinates, {_, 3}];
    $GraphPlotRange := $GraphPlotRange = CoordinateBounds[{$GraphVertexCoordinates, $GraphEdgeCoordinateLists}];
    $GraphPlotSize := $GraphPlotSize = rangeSize[$GraphPlotRange];
    $GraphPlotAspectRatio := Last[$GraphPlotSize] / First[$GraphPlotSize];
    $GraphMaxSafeVertexSizeScaled := $GraphMaxSafeVertexSize / First[$GraphPlotSize];
    $GraphMaxSafeVertexSize := $GraphMaxSafeVertexSize = computeMaxSafeVertexSize[];
    $GraphMaxSafeArrowheadSize := $GraphMaxSafeArrowheadSize = computeMaxSafeArrowheadSize[];

    body
  ]
];

(**************************************************************************************************)

PackageExport["ExtendedGraphPlot"]

$autoFilledLegendPattern = (Automatic | _String) | Placed[Automatic | _String, _];

ExtendedGraphPlot[graph_] := Scope[

  If[!GraphQ[graph], ReturnFailed[]];

  {plottingFunction, graphLegend, graphRegionHighlight, vertexColorFunction} =
    LookupAnnotation[graph, {GraphPlottingFunction, GraphLegend, GraphRegionHighlight, VertexColorFunction}, None];

  SetNone[plottingFunction,
    If[vertexColorFunction === None, GraphComputation`GraphDrawing, ExtendedGraphPlottingFunction]];

  SetAutomatic[plottingFunction, ExtendedGraphPlottingFunction];

  GraphPlotScope[graph,

    graphics = plottingFunction[$Graph];

    If[MatchQ[graphics, _Legended],
      If[MatchQ[graphLegend, None | $autoFilledLegendPattern | {$autoFilledLegendPattern..}],
        graphLegend = Last[graphics]];
      graphics = First[graphics];
    ];

    (* recompute these with the results of plottingFunction, for the benefit of GraphRegionHighlight *)
    $GraphPlotRange := $GraphPlotRange = GraphicsPlotRange @ graphics;
    $GraphPlotSize := $GraphPlotSize = rangeSize[$GraphPlotRange];
    $GraphMaxSafeVertexSize := $GraphMaxSafeVertexSize = computeMaxSafeVertexSize[];

    graphics = ApplyEpilog[graphics, resolveGraphRegionHighlightGraphics @ graphRegionHighlight];
    graphics = ApplyLegend[graphics, graphLegend];

    graphics
  ]
];

rangeSize[range_] := EuclideanDistance @@@ range;

computeBorderDistance[{cl_, ch_}, {pl_, ph_}] := {cl - pl, ph - ph};

computeMaxSafeVertexSize[] := Scope[
  minDistance = Min @ DeleteCases[Flatten @ $GraphVertexCoordinateDistanceMatrix, 0|0.];
  coordinateBounds = CoordinateBounds @ $GraphVertexCoordinates;
  (* borderDistance = Min @ MapThread[computeBorderDistance, {coordinateBounds, $GraphPlotRange}]; *)
  (* todo: add padding when this happens *)
  (* If[borderDistance <= $MachineEpsilon, borderDistance = Infinity]; *)
  Min[(* borderDistance,  *)minDistance / 2, Max[$GraphPlotSize] / 3]
];

computeMaxSafeArrowheadSize[] := Scope[
  minDistance = Min @ DeleteCases[Flatten @ DistanceMatrix[Median /@ $GraphEdgeCoordinateLists], 0|0.];
  Min[$GraphMaxSafeVertexSize, minDistance * 1.5, Max[$GraphPlotSize] / 3]
];


(**************************************************************************************************)

PackageExport["ExtendedGraphPlottingFunction"]

Quiver::badcolors = "CardinalColors should be an association from cardinals to colors.";

ExtendedGraphPlottingFunction[___] := $Failed;

failPlot[msgName_String, args___] := (
  Message[MessageName[ExtendedGraphPlot, msgName], args];
  Return[$Failed, Block]
);

ExtendedGraphPlottingFunction[graph_Graph] := Scope[

  cardinalColors = LookupCardinalColors[graph];
  If[!MatchQ[cardinalColors, None | _Association], ReturnFailed["badcolors"]];

  {graphLayout, imageSize, vertexSize, vertexStyle, edgeStyle, vertexLabelStyle, edgeLabelStyle} =
    LookupOption[graph, {GraphLayout, ImageSize, VertexSize, VertexStyle, EdgeStyle, VertexLabelStyle, EdgeLabelStyle}, Automatic];

  {vertexLabels, edgeLabels} =
    LookupOption[graph, {VertexLabels, EdgeLabels}, None];

  {arrowheadStyle, arrowheadSize, graphLegend, vertexColorFunction, vertexAnnotations} =
    LookupExtendedGraphAnnotations[graph, {ArrowheadStyle, ArrowheadSize, GraphLegend, VertexColorFunction, VertexAnnotations}];

  SetNone[vertexAnnotations, <||>];

  automaticLegends = <||>;

  (* choose a size based on the longest path in the graph *)
  (* TODO: switch this to establish a minumum absolute vertex separation *)
  SetAutomatic[imageSize,
    If[Length[$GraphVertexList] > 1000, Large, chooseGraphImageSize @ $SymmetricIndexGraph]];

  (* make sure that the image height isn't too big for large aspect-ratio graphs *)
  basePadding = 5;
  If[$GraphPlotAspectRatio > 1.5,
    imageSize = toNumericImageSize @ imageSize;
    imageSize *= ($GraphPlotAspectRatio - 1.5) / $GraphPlotAspectRatio;
    basePadding = 10;
  ];
  numericImageSize = toNumericImageSize @ imageSize;

  SetAutomatic[edgeStyle, Directive[Opacity[.2], Black]];

  If[arrowheadStyle === None || UndirectedGraphQ[$Graph],
    edgeGraphics = GraphicsGroup @ {edgeStyle, Line[$GraphEdgeCoordinateLists]};
  ,
    If[!AssociationQ[arrowheadStyle], arrowheadStyle = <|All -> arrowheadStyle|>];
    If[cardinalColors === None, cardinalColors = <||>];

    (* create graphics for edges *)
    edgeTagGroups = If[$GraphEdgeTags === None,
      <|All -> Range[$GraphEdgeCount]|>,
      edgeTagGroups = PositionIndex[$GraphEdgeTags]
    ];
    baseArrowheadSize := If[$GraphIs3D, 0.45, 1] * ($GraphMaxSafeArrowheadSize / First[$GraphPlotSize]) * 1;(100 / Sqrt[numericImageSize]);
    SetAutomatic[arrowheadSize, baseArrowheadSize];
    If[MatchQ[arrowheadSize, Scaled[_]], arrowheadSize = First[arrowheadSize] * baseArrowheadSize];
    arrowheadPos = If[$GraphIs3D, 0.65, 0.5];
    edgeGraphics = KeyValueMap[
      {cardinal, edgeIndices} |-> (
        style = Lookup[arrowheadStyle, cardinal, Lookup[arrowheadStyle, All, Automatic]];
        color = Lookup[cardinalColors, cardinal, Automatic];
        SetAutomatic[color, Gray];
        SetAutomatic[style, Arrowheads[{{arrowheadSize, arrowheadPos, makeBaseArrowhead @ color}}]];
        {style, Arrow @ Part[$GraphEdgeCoordinateLists, edgeIndices]}
      ),
      edgeTagGroups
    ];
    edgeGraphics = GraphicsGroup @ {edgeStyle, edgeGraphics};
  ];

  (* create graphics for vertices *)
  (* TODO: implement support for Disk, with darker edgeform etc *)
  (* TODO: implement support for VertexColorFunction, etc *)
  vertexSize //= removeSingleton;
  vertexPointSize = processVertexSize[vertexSize];
  SetAutomatic[vertexStyle, GrayLevel[0.3]];
  vertexStyle //= removeSingleton;

  If[vertexColorFunction =!= None,
    vertexColorFunctionData = toColorFunctionData[vertexColorFunction];
    {colorGroups, colorFunctionObject} = ApplyColoring[vertexColorFunctionData];
    If[FailureQ[colorFunctionObject], failPlot["badcolvals"]];
    automaticLegends["Colors"] := colorFunctionObject;
    aratio = {$GraphPlotAspectRatio, 1};
    diskSize = Match[vertexPointSize, PointSize[sz_] :> Scaled[aratio * sz]];
    vertexPointSize2 = MapAt[# * .05&, vertexPointSize, 1];
    diskFunc = If[$GraphIs3D, {{Black, vertexPointSize2, Point[#]}, {vertexPointSize, Point[#]}}&, Disk];
    vertexItems = KeyValueMap[
      {color, vertexIndices} |-> {
        color, EdgeForm @ Darker[color, .8], diskFunc[#, diskSize]& /@ Part[$GraphVertexCoordinates, vertexIndices]
      },
      colorGroups
    ];
    vertexItems = {EdgeForm @ AbsoluteThickness[.25], vertexItems};
  ,
    vertexItems = Point @ $GraphVertexCoordinates;
  ];
  vertexGraphics = GraphicsGroup @ {vertexPointSize, vertexStyle, vertexItems};

  (* create labels for vertices and edges *)
  labelGraphics = {};

  vertexLabels //= removeSingleton;
  If[vertexLabels =!= None,
    SetAutomatic[vertexLabelStyle, {}];
    plotRangeVertexSize = toPlotRangeSize[vertexPointSize];
    SetNone[plotRangeVertexSize, $GraphMaxSafeVertexSize / 5];
    vertexLabelItems = generateLabelPrimitives[vertexLabels, $GraphVertexList, $GraphVertexCoordinates, plotRangeVertexSize, vertexAnnotations];
    AppendTo[labelGraphics, {vertexLabelStyle, vertexLabelItems}]];

  edgeLabels //= removeSingleton;
  If[edgeLabels =!= None,
    SetAutomatic[edgeLabelStyle, {}];
    edgeLabelItems = generateLabelPrimitives[edgeLabels, $GraphEdgeList, Median /@ $GraphEdgeCoordinateLists, arrowheadSize/2, <||>];
    AppendTo[labelGraphics, {edgeLabelStyle, edgeLabelItems}]];

  labelGraphics = If[labelGraphics === {}, Nothing, GraphicsGroup @ labelGraphics];

  imagePadding = If[labelGraphics =!= Nothing, basePadding * {{1, 1}, {1, 1}} + {{0, 0}, {0, 10}}, basePadding];

  (* for graphs with cardinals, create an automatic legend when asked *)
  If[cardinalColors =!= None,
    automaticLegends["Cardinals"] := ArrowheadLegend[cardinalColors];
  ];

  width = numericImageSize;
  height = width * Clip[$GraphPlotAspectRatio, {.44, 2.222}];

  (* assemble graphics *)
  graphics = If[$GraphIs3D, makeGraphics3D, makeGraphics][
    {edgeGraphics, vertexGraphics, labelGraphics},
    If[$GraphIs3D, imageSize, {width, height}], imagePadding
  ];

  applyAutomaticLegends[graphics, automaticLegends, graphLegend]
];

applyAutomaticLegends[graphics_, <||>, _] := graphics;

applyAutomaticLegends[graphics_, automaticLegends_, Automatic] :=
  ApplyLegend[graphics, Values @ automaticLegends];

applyAutomaticLegends[graphics_, automaticLegends_, Placed[Automatic, place_]] :=
  ApplyLegend[graphics, Map[Placed[#, place]&, Values @ automaticLegends]];

applyAutomaticLegends[graphics_, automaticLegends_, graphLegend_] := Scope[
  keyPattern = Alternatives @@ Keys[automaticLegends];
  graphLegend = Developer`ToList @ graphLegend;
  If[!MatchQ[graphLegend, {Repeated[keyPattern | Placed[keyPattern, _]]}],
    Return @ graphics];
  legends = removeSingleton @ Replace[graphLegend, {
    s_String :> automaticLegends[s],
    Placed[s_String, p_] :> Placed[automaticLegends[s], p]
  }, {1}];
  ApplyLegend[graphics, legends]
];

removeSingleton[{e_}] := e;
removeSingleton[e_] := e;

ExtendedGraphPlot::notvertex = "`` is not a valid vertex of the graph."
ExtendedGraphPlot::badcolfunc = "`` is not a valid color function specification for VertexColorFunction."
ExtendedGraphPlot::msgcolfunc = "Applying the requested color function to property `` gave messages."

toColorFunctionData = MatchValues[
  {"Distance", v_} := Part[$GraphDistanceMatrix, Lookup[$GraphVertexIndices, v, failPlot["notvertex", v]]];
  key_String := Lookup[vertexAnnotations, key, failPlot["badlabelspecanno", key, commaString @ vertexAnnotations]];
  (key_String -> f_) := Replace[Quiet @ Check[Map[toFunc @ f, %[key]], $Failed], $Failed :> failPlot["msgcolfunc", key]];
  spec_ := failPlot["badcolfunc", spec]
];

toFunc[i_Integer] := Extract[i];
toFunc[f_] := f;

ExtendedGraphPlot::badcolvals = "VertexColorFunction did not produce values that could be colored."

(**************************************************************************************************)

ExtendedGraphPlot::badvertexsize = "`` is not a valid setting for VertexSize."

processVertexSize = MatchValues[
  (* Automatic                     := AbsolutePointSize @ N @ Min[0.2 * $GraphMaxSafeVertexSizeScaled * numericImageSize, 5]; *)
  Automatic                     := %[0.3];
  {"Nearest", r_ ? NumericQ}    := PointSize[N[r] * $GraphMaxSafeVertexSizeScaled];
  {"Scaled", r_ ? NumericQ}     := PointSize[N[r] * Norm[$GraphPlotSize] / First[$GraphPlotSize]];
  sym_Symbol /; KeyExistsQ[$fractionSizes, sym] := %[{"Nearest", 3 * $fractionSizes[sym]}];
  r_ ? NumericQ                 := %[{"Nearest", r}];
  other_                        := failPlot["badvertexsize", other];
];

$fractionSizes = <|Tiny -> 0.1, Small -> 0.15, MediumSmall -> 0.175, Medium -> 0.2, MediumLarge -> 0.3, Large -> 0.4, Huge -> 0.6|>;

(* todo: use Offset where appropriate to make labels work correctly under graph resizing *)
toPlotRangeSize = MatchValues[
  AbsolutePointSize[sz_] := sz / numericImageSize * First[$GraphPlotSize];
  PointSize[sz_] := sz * First[$GraphPlotSize];
  _ := None;
];

(**************************************************************************************************)

ExtendedGraphPlot::badlabelspec = "The label specification `` was not one of the recognized forms."

generateLabelPrimitives[spec_, names_, coordinates_, size_, annotations_] := Scope[
  $annotationKeys = Keys[annotations];
  {payloadFunction, placerFunction} = processLabelSpec[spec];
  $labelNames = names; $annotations = annotations; $elemSize = size / 2;
  MapIndexed[placerFunction[payloadFunction @ First @ #2, #1]&, coordinates]
];

processLabelSpec = MatchValues[
  Automatic | All :=            %["Name"];
  Tooltip :=                    %[Placed["Name", Tooltip]];
  p_String :=                   {toPayloadFunction @ p, placeLabelAt};
  Placed[p_String, Tooltip] :=  {toPayloadFunction @ p, placeTooltipAt};
  None :=                       None;
  other_ :=                     failPlot["badlabelspec", other];
];

ExtendedGraphPlot::badlabelspecanno = "The requested annotation `` is not present in the graph. Present annotations are: ``."

toPayloadFunction = MatchValues[
  "Name" := getName;
  "Index" := getIndex;
  "Tag" | "Cardinal" := getCardinal;
  key_ := If[MemberQ[$annotationKeys, key], getAnnotation[key], failPlot["badlabelspecanno", key, commaString @ $annotationKeys]]
];

getIndex[i_] := i;
getName[i_] := Part[$labelNames, i];
getCardinal[i_] := Part[$GraphEdgeTags, i];
getAnnotation[name_][i_] := Part[$annotations, name, i];

placeTooltipAt[label_, pos_] := NiceTooltip[{Transparent, Disk[pos, $elemSize]}, label];
placeTooltipAt[None, _] := Nothing;

placeLabelAt[label_, pos_] := Text[label, pos + {0, $elemSize}, {0, -0.9}, Background -> GrayLevel[1.0, 0.6]];
placeLabelAt[None, _] := Nothing;

(**************************************************************************************************)

ExtendedGraphPlot::badarrowhead = "The arrowhead specification with head `` should be None, Automatic, Graphics[..], or an Image."

processArrowheadSpec = MatchValues[
  Automatic := Automatic;
  None := None;
  g_Graphics := g;
  Placed[img_Image, pos_] := imageToGraphics[img, pos];
  img_Image := imageToGraphics[img, {0, 1}];
  other_ := failPlot["badarrowhead", Head @ other];
];

imageToGraphics[img_, pos_] := ImageToGraphics[ImageResize[img, 50], pos, Replace[arrowheadSize, Tiny -> .1]];

(**************************************************************************************************)

makeGraphics[elements_, imageSize_, padding_] := Graphics[
  elements,
  Frame -> None, Axes -> None,
  ImageSize -> toStandardImageSize @ imageSize,
  ImagePadding -> padding
];

makeGraphics3D[elements_, imageSize_, padding_] := Graphics3D[
  elements,
  Axes -> None, Boxed -> False,
  ImageSize -> toStandardImageSize @ imageSize,
  ImagePadding -> padding,
  ViewProjection -> "Orthographic", Lighting -> "Neutral"
];

(**************************************************************************************************)

PackageScope["chooseArrowheadSize"]

chooseArrowheadSize[imageSize_] := 0.1 * 150 / imageSize;

(**************************************************************************************************)

chooseGraphImageSize[graph_Graph] := Scope[
  If[VertexCount[graph] > 1000, Return @ Large];
  diam = GraphDiameter[graph];
  If[diam === Infinity, diam = Total[GraphDiameter /@ ConnectedGraphComponents[graph]]];
  Which[
    diam < 3, Tiny,
    diam < 8, Small,
    diam < 12, MediumSmall,
    diam < 16, Medium,
    diam < 32, MediumLarge,
    True, Large
  ]
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

