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

GraphPlotScope[graph_, body_] := Scope[

  If[!GraphQ[graph], ReturnFailed[]];

  GraphScope[graph,

    {$GraphVertexCoordinates, $GraphEdgeCoordinateLists} = ExtractGraphPrimitiveCoordinates[$IndexGraph];

    $GraphPlotImageSize := $GraphPlotImageSize := LookupImageSize @ graph;

    $GraphVertexCoordinatesAssociation := AssociationThread[$GraphVertexList, $GraphVertexCoordinates];
    $GraphVertexCoordinateDistanceMatrix := $GraphVertexCoordinateDistanceMatrix = DistanceMatrix[$GraphVertexCoordinates];
(*     $GraphVertexCoordinateBoundingBox := $GraphVertexCoordinateBoundingBox = CoordinateBoundingBox[$GraphVertexCoordinates];
 *)
    $GraphEdgeCoordinateListsAssociation := $GraphEdgeCoordinateListsAssociation = AssociationThread[$GraphEdgeList, $GraphEdgeCoordinateLists];

    (* before we have called the user function, guess the range based on the vertex and edge coordinates *)
    $GraphIs3D := $GraphIs3D = CoordinateMatrixQ[$GraphVertexCoordinates, 3];
    $GraphPlotRange := $GraphPlotRange = CoordinateBounds[{$GraphVertexCoordinates, $GraphEdgeCoordinateLists}];
    $GraphPlotSize := $GraphPlotSize = rangeSize[$GraphPlotRange];
    $GraphPlotSizeX := Part[$GraphPlotSize, 1];
    $GraphPlotSizeY := Part[$GraphPlotSize, 2];
    $GraphPlotScale := $GraphPlotScale = If[$GraphIs3D, $GraphPlotSizeX, Norm @ $GraphPlotSize];
    $GraphPlotAspectRatio := Part[$GraphPlotSize, 2] / $GraphPlotSizeX;

    $GraphPlotScale := $GraphPlotScale = If[$GraphIs3D, $GraphPlotSizeX, Norm @ $GraphPlotSize];
    $GraphPlotAspectRatio := Part[$GraphPlotSize, 2] / $GraphPlotSizeX;
    $GraphMaxSafeVertexSizeScaled := $GraphMaxSafeVertexSize / If[$GraphIs3D, Norm @ $GraphPlotSize, $GraphPlotSizeX];
    $GraphMaxSafeVertexSize := $GraphMaxSafeVertexSize = computeMaxSafeVertexSize[];
    $GraphMaxSafeArrowheadSize := $GraphMaxSafeArrowheadSize = computeMaxSafeArrowheadSize[];

    body
  ]
];

(**************************************************************************************************)

rangeToPointSize[sz_] := sz * $GraphPlotScale;

scaledToRange[sz_] := 5;

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

    graphics = ApplyProlog[graphics, resolveGraphRegionHighlightGraphics @ graphRegionHighlight];
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

  (* process options *)
  FunctionSection[
    {graphLayout, imageSize, vertexSize, vertexStyle, edgeStyle, vertexLabelStyle, edgeLabelStyle, vertexShapeFunction} =
      LookupOption[graph, {GraphLayout, ImageSize, VertexSize, VertexStyle, EdgeStyle, VertexLabelStyle, EdgeLabelStyle, VertexShapeFunction}, Automatic];

    {vertexLabels, edgeLabels} =
      LookupOption[graph, {VertexLabels, EdgeLabels}, None];

    {arrowheadStyle, arrowheadSize, graphLegend, vertexColorFunction, vertexAnnotations, vertexPairAnnotations} =
      LookupExtendedGraphAnnotations[graph, {ArrowheadStyle, ArrowheadSize, GraphLegend, VertexColorFunction, VertexAnnotations, VertexPairAnnotations}];
  ];

  (* initial processing of global options *)
  FunctionSection[
    cardinalColors = LookupCardinalColors[graph];
    If[!MatchQ[cardinalColors, None | _Association], ReturnFailed["badcolors"]];

    SetNone[vertexAnnotations, <||>];

    automaticLegends = <||>;

    (* choose a size based on the longest path in the graph *)
    (* TODO: switch this to establish a minumum absolute vertex separation *)
    SetAutomatic[imageSize,
      If[Length[$GraphVertexList] > 1000, Large,
        graphPlotSizeScalingFunction[If[$GraphIs3D, 30, 15] * ($GraphPlotSizeX / $GraphMaxSafeVertexSize)]]];

    (* make sure that the image height isn't too big for large aspect-ratio graphs *)
    basePadding = 5;
    If[$GraphPlotAspectRatio > 1.5,
      imageSize = toNumericImageSize @ imageSize;
      imageSize *= ($GraphPlotAspectRatio - 1.5) / $GraphPlotAspectRatio;
      basePadding = 10;
    ];
    numericImageSize = toNumericImageSize @ imageSize;
  ];

  (* create graphics for vertices *)
  FunctionSection[
    vertexSize //= removeSingleton;
    vertexPointSize = processVertexSize[vertexSize];

    vertexStyle //= removeSingleton;

    SetAutomatic[vertexShapeFunction, If[vertexColorFunction =!= None, "Disk", "Point"]];

    lighting = None;
    (* todo: GraphPlotAspectRatio should be distinguished from GraphImageAspectRatio  *)
    vertexScaledSize = Scaled[First[vertexPointSize] * {1, 1/$GraphPlotAspectRatio}];
    vertexPlotSize = toPlotRangeSize[vertexPointSize];

    vertexShapeFunction //= removeSingleton;
    {defaultVertexColor, vertexBaseStyle, setbackDistance, vertexDrawFunc} =
      processVertexShapeFunction[vertexShapeFunction];

    SetAutomatic[vertexStyle, defaultVertexColor];
    (* todo: extract a color from vertexStyle and use it for defaultVertexColor *)

    vertexStyle //= removeSingleton /* toDirective;

    If[vertexColorFunction =!= None,
      vertexColorFunctionData = toColorFunctionData[vertexColorFunction];
      If[FailureQ[vertexColorFunctionData], failPlot["badcolvals"]];
      {colorGroups, colorFunctionObject} = ApplyColoring[vertexColorFunctionData];
      If[FailureQ[colorFunctionObject], failPlot["badcolvals"]];
      automaticLegends["Colors"] := colorFunctionObject;
      vertexItems = KeyValueMap[
        vertexDrawFunc[Part[$GraphVertexCoordinates, #2], #1]&,
        colorGroups
      ];
    ,
      vertexItems = vertexDrawFunc[$GraphVertexCoordinates, defaultVertexColor];
    ];
    vertexGraphics = GraphicsGroup @ {vertexBaseStyle /. None -> Nothing, vertexStyle, vertexItems};
  ];

  (* create graphics for edges *)
  FunctionSection[
    SetAutomatic[edgeStyle, Directive[Opacity[.2], Black, If[$GraphIs3D, MediumThick, MediumThin]]];
    defaultArrowheadStyle = If[vertexColorFunction === None, Gray, LightGray];

    edgeStyle //= removeSingleton /* toDirective;
    If[arrowheadStyle === None || UndirectedGraphQ[$Graph],
      edgeGraphics = GraphicsGroup @ {edgeStyle, setback[multiLine, setbackDistance] @ $GraphEdgeCoordinateLists};
    ,
      If[arrowheadStyle === "Plain", arrowheadStyle = Automatic; cardinalColors = None];
      If[!AssociationQ[arrowheadStyle], arrowheadStyle = <|All -> arrowheadStyle|>];
      If[cardinalColors === None, cardinalColors = <||>];

      edgeTagGroups = If[$GraphEdgeTags === None,
        <|All -> Range[$GraphEdgeCount]|>,
        edgeTagGroups = PositionIndex[$GraphEdgeTags]
      ];
      baseArrowheadSize := If[$GraphIs3D, 0.45, 1] * ($GraphMaxSafeArrowheadSize / $GraphPlotSizeX) * 1;
      SetAutomatic[arrowheadSize, baseArrowheadSize];
      If[MatchQ[arrowheadSize, Scaled[_]], arrowheadSize = First[arrowheadSize] * baseArrowheadSize];
      arrowheadPos = If[$GraphIs3D, 0.65, 0.5];
      edgeGraphics = KeyValueMap[
        {cardinal, edgeIndices} |-> (
          style = Lookup[arrowheadStyle, cardinal, Lookup[arrowheadStyle, All, Automatic]];
          color = Lookup[cardinalColors, cardinal, Automatic];
          SetAutomatic[color, defaultArrowheadStyle];
          SetAutomatic[style, Arrowheads[{{arrowheadSize, arrowheadPos, makeBaseArrowhead @ color}}]];
          {style, setback[Arrow, setbackDistance] @ Part[$GraphEdgeCoordinateLists, edgeIndices]}
        ),
        edgeTagGroups
      ];
      edgeGraphics = GraphicsGroup @ {edgeStyle, edgeGraphics};
    ];
  ];

  (* create labels for vertices and edges *)
  FunctionSection[
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
  ];

  (* create the final graphics *)
  FunctionSection[
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
  ]
];

(**************************************************************************************************)

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

(**************************************************************************************************)

removeSingleton[{e_}] := e;
removeSingleton[e_] := e;

(**************************************************************************************************)

ExtendedGraphPlot::badvshapefunc = "`` is not a valid setting for VertexShapeFunction."

processVertexShapeFunction[spec_] := Scope[
  setbackDistance = 0;
  vertexBaseStyle = None;
  Switch[spec,
    "Disk" /; $GraphIs3D,
      defaultVertexColor = $Gray;
      norm = ($GraphPlotSizeX / Norm[$GraphPlotSize]);
      vertexDrawFunc = mapCoordArray @ drawDisk3D[Scaled[{1, 1} * First[vertexPointSize]]];
      setbackDistance = vertexPlotSize/2;
    ,
    "Ball" | "Sphere" /; $GraphIs3D,
      defaultVertexColor = $LightGray;
      vertexDrawFunc = drawSphere[vertexScaledSize];
    ,
    "Disk" | "Ball" | "Sphere",
      defaultVertexColor = $DarkGray;
      vertexBaseStyle = EdgeForm @ AbsoluteThickness[.25];
      vertexDrawFunc = drawDisk[vertexScaledSize];
    ,
    "Point",
      defaultVertexColor = $DarkGray;
      vertexBaseStyle = vertexPointSize;
      setbackDistance = vertexPlotSize / 2;
      vertexDrawFunc = drawPoint;
    ,
    _,
      failPlot["badvshapefunc", spec];
  ];
  setbackDistance = 0;
  {defaultVertexColor, vertexBaseStyle, setbackDistance, vertexDrawFunc}
];

drawPoint[pos_, color_] := {color, Point @ pos};

drawSphere[size_][pos_, color_] :=
  {color, Sphere[pos, vertexScaledSize]};

drawDisk[size_][pos_, color_] := {
  FaceForm[color], EdgeForm[Darker[color, .3]],
  Disk[{0, 0}, 1]
};

drawDisk3D[size_][pos_, color_] := Inset[
  Graphics[drawDisk[1][{0, 0}, color], AspectRatio -> 1],
  pos, {0, 0}, size
];

mapCoordArray[f_][array_, args___] :=
  If[CoordinateMatrixQ[array], Map[f[#, args]&, array], f[array, args]];

(**************************************************************************************************)

multiLine[pos_] := Which[
  ArrayQ[pos, 2|3] || VectorQ[pos, MatrixQ], Line[pos],
  ArrayQ[pos, 2, MatrixQ], Map[Line, pos],
  True, Print[Dimensions /@ pos]
];

setback[head_, 0] := head;
setback[Arrow, dist_] := Arrow[#, dist]&;

setback[Line, dist_] := multiLine[Which[
  CoordinateMatrixQ[#], setbackCoords[dist][#],
  CoordinateArrayQ[#], Map[setbackCoords[dist], #],
  True, #]]&

setbackCoords[dist_][{a_, b_}] /; (EuclideanDistance[a, b] > 2 * dist) := Scope[
  dx = Normalize[b - a] * dist;
  {a + dx, b - dx}; {a, b}
];

setbackCoords[dist_][other_] := other;

insetDisk[size_][pos_] := Inset[Graphics[Disk[{0, 0}, 1], AspectRatio -> 1], pos, {0, 0}, size];
(id:insetDisk[size_])[pos_ ? CoordinateMatrixQ] := Map[id, pos];

(**************************************************************************************************)

ExtendedGraphPlot::notvertex = "`` is not a valid vertex of the graph."
ExtendedGraphPlot::badcolfunc = "`` is not a valid color function specification for VertexColorFunction."
ExtendedGraphPlot::msgcolfunc = "Applying the requested color function to property `` gave messages."

getVertexIndex[v_] := Lookup[$GraphVertexIndices, v, failPlot["notvertex", v]];
getAnnoValue[annos_, key_] := Lookup[annos, key, failPlot["badgraphannokey", key, commaString @ Keys @ annos]];

toColorFunctionData = MatchValues[
  {"Distance", v_} := Part[$GraphDistanceMatrix, getVertexIndex @ v];
  {"Distance", v_, Transpose} := Part[$GraphDistanceMatrix, All, getVertexIndex @ v];
  {"TaggedDistance" -> c_, v_} := TaggedGraphDistance[$Graph, getVertexIndex @ v, All, c];
  {"LatticeDistance", v_} := LatticeDistance[$Graph, getVertexIndex @ v];
  {key_String, v_} := Part[getAnnoValue[vertexPairAnnotations, key], getVertexIndex @ v];
  {key_String, v_, Transpose} := Part[getAnnoValue[vertexPairAnnotations, key], All, getVertexIndex @ v];
  key_String := getAnnoValue[vertexAnnotations, key];
  (key_String -> f_) := Replace[Quiet @ Check[Map[toFunc @ f, %[key]], $Failed], $Failed :> failPlot["msgcolfunc", key]];
  list_List /; Length[list] === $GraphVertexCount := list;
  spec_ := failPlot["badcolfunc", If[Length[spec] > 10 || ByteCount[spec] > 1000, Skeleton[Length[spec]], spec]]
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
  {"Scaled", r_ ? NumericQ}     := PointSize[N[r] * Norm[$GraphPlotSize] / $GraphPlotSizeX];
  sym_Symbol /; KeyExistsQ[$fractionSizes, sym] := %[{"Nearest", 3 * $fractionSizes[sym]}];
  r_ ? NumericQ                 := %[{"Nearest", r}];
  other_                        := failPlot["badvertexsize", other];
];

$fractionSizes = <|Tiny -> 0.1, Small -> 0.15, MediumSmall -> 0.175, Medium -> 0.2, MediumLarge -> 0.3, Large -> 0.4, Huge -> 0.6|>;

(* todo: use Offset where appropriate to make labels work correctly under graph resizing *)
toPlotRangeSize = MatchValues[
  AbsolutePointSize[sz_] := sz / numericImageSize * $GraphPlotSizeX;
  PointSize[sz_] := sz * $GraphPlotSizeX;
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

ExtendedGraphPlot::badgraphannokey = "The requested annotation `` is not present in the graph. Present annotations are: ``."

toPayloadFunction = MatchValues[
  "Name" := getName;
  "Index" := getIndex;
  "Tag" | "Cardinal" := getCardinal;
  key_ := If[MemberQ[$annotationKeys, key], getAnnotation[key], failPlot["badgraphannokey", key, commaString @ $annotationKeys]]
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
  {CapForm[None], elements},
  Axes -> None, Boxed -> False,
  ImageSize -> toStandardImageSize @ imageSize,
  ImagePadding -> padding, PlotRange -> All,
  ViewProjection -> "Orthographic", Lighting -> "Neutral",
  Method -> {"ShrinkWrap" -> True, "EdgeDepthOffset" -> False},
  AspectRatio -> Automatic, ViewPoint -> {Infinity,Infinity,0}
];

(**************************************************************************************************)

PackageScope["chooseArrowheadSize"]

chooseArrowheadSize[imageSize_] := 0.1 * 150 / imageSize;

(**************************************************************************************************)

scalingPower[n_] := Power[n, 1/1.5];
$midSize = 360;
graphPlotSizeScalingFunction[size_] :=
  Clip[
    Floor[scalingPower[size] * $midSize/scalingPower[$midSize], 25],
    Lookup[$ImageWidthTable, {Tiny, Huge}]
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

