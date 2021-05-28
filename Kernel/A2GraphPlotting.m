Package["GraphTools`"]

PackageImport["GeneralUtilities`"]

(**************************************************************************************************)

PackageExport["ArrowheadShape"]
PackageExport["ArrowheadSize"]
PackageExport["ArrowheadStyle"]
PackageExport["ArrowheadPosition"]
PackageExport["VertexColorFunction"]
PackageExport["CardinalColors"]

SetUsage @ "ArrowheadShape is an extended option to Graph.";
SetUsage @ "ArrowheadSize is an extended option to Graph.";
SetUsage @ "ArrowheadStyle is an extended option to Graph.";
SetUsage @ "ArrowheadPosition is an extended option to Graph.";
SetUsage @ "VertexColorFunction is an extended option to Graph."
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

PackageScope["$graphRegionTable"]

$graphRegionTable = StringTrim @ "
The following specifications describe paths in the graph:
| %Path[v$, {c$1, $$, c$n}] | start at v$, move along cardinals c$i |
| %Line[{v$1, v$2}] | the geodesic between v$1 and v$2 |
| %Line[{v$1, v$2}, c$] | start at v$1, moving along c$, and end at v$2 |
| %Line[{v$1, $$, v$n}] | the geodesic path between v$1 and v$2, v$2 and v$3, etc. |
| %Polygon[{v$1, $$, v$n}] | geodesics between the v$i, taken cyclically |
| %HalfLine[v$, c$] | a geodesic starting at v$ and continuing in the cardinal direction c$ |
| %HalfLine[{v$1, v$2}] | a geodesic starting at v$1 and continuing through v$2 |
| %InfiniteLine[v$, c$] | a geodesic with midpoint v$, and continuing in directions c$ and Negated[c$] |
| %InfiniteLine[{v$1, v$2}] | a geodesic intersecting v$1 and v$2 |
| %GraphPathData[$$] | a previously computed path |
* Specifications taking a cardinal direction c$ also take a list {c$1, ..., c$n}, used cyclically.
* %Path[$$, PathAdjustments -> {i$1, i$2, $$}] indicates that the i$th elements of the path should drawn
truncated.

The following specifications describe regions in the graph:
| %Point[v$] | a single vertex v$ |
| %DirectedEdge[v$1, v$2] | a literal edge between v$1 and v$2 |
| %VertexPattern[patt$] | all vertices match patt$ |
| %EdgePattern[v$1, v$2, t$] | all edges matching %DirectedEdge[v$1, v$2, t$] |
| %Disk[v$, r$] | vertices within distance r$ of v$ |
| %Annulus[v$1, {r$1, r$2}] | vertices with distance r$1 \[LessEqual] r$ \[LessEqual] r$2 |
| %Circle[v$, r$] | vertices exactly distance r$ from v$ |
| %Locus[r$1, r$2] | vertices whose distance to regions r$1 and r$2 is equal |
| %Locus[r$1, r$2, 'Polar'] | vertices that straddle the equation d$ (r$1) - d$ (r$2) = 0 |
| %Locus[r$1, r$2, d$] | vertices whose distance to regions r$1 and r$2 differs by less than d$ |
| %RegionBoundary[r$] | the vertices in region r$ adjacent to vertices not in r$ |
| %RegionComplement[r$1, r$2] | the complement of region r$1 with region r$2 |
| %RegionIntersection[r$1, r$2, $$] | the mutual intersection of regions r$i |
| %RegionUnion[r$1, r$2, $$] | the union of regions r$i |
| %GraphRegionData[$$] | a previously computed region |

## Named regions

* A specification of the form <|'name$1' -> spec$1, 'name$2' -> spec$2, $$|> creates named regions.
* Named regions will automatically be given unique colors when highlighted.
* Within a named region, the string 'name$i' can be used to refer to a previously defined region.

## Distances

* All distances are measured relative to the setting of %GraphMetric of the graph.
* Specific regions like %Circle, %Disk etc accept an option %GraphMetric -> m$ to override this default.

## Vertices

* Specifications taking a vertex v$ can also take these symbolic forms:
| %GraphOrigin | the 'origin vertex', if provided |
| %RandomPoint | a randomly chosen vertex |
| %Offset[v$, {c$1, $$}] | start at v$ and move along the cardinals c$i |
"

(**************************************************************************************************)

PackageScope["$GraphRegionHighlightUsage"]

$GraphRegionHighlightUsage = StringTrim @ "

## Highlight specifications

* A single highlight specification, or a list or association of specifications can be given.

* The keys of an association will be used as legend labels.

* Each highlight specification can be one of the following:
| region$ | highlight a set of vertices or edges with a unique color |
| {region$1, region$2, $$} | highlight several regions with the same color |
| %Style[spec$, style$] | specify a color or other options |
| Labeled[region$, label$] | label the annotation in-place |
| %Arrow[spec$] | draw paths as arrows |
| %Axis -> %%All | %InfiniteLine[%GraphOrigin, c$i] for each cardinal c$i |
| %Axis -> {c$1, $$} | %InfiniteLine[%GraphOrigin, c$i] |

## Region specifications

<*$graphRegionTable*>
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

PackageScope["$VertexCoordinates"]
PackageScope["$EdgeCoordinateLists"]
PackageScope["$GraphHighlightStyle"]
PackageScope["$GraphIs3D"]
PackageScope["$GraphPlotRange"]
PackageScope["$GraphPlotSize"]
PackageScope["$GraphPlotAspectRatio"]
PackageScope["$GraphPlotImageSize"]
PackageScope["$GraphPlotImageWidth"]
PackageScope["$GraphMaxSafeVertexSize"]
PackageScope["$GraphPlotGraphics"]

PackageExport["GraphPlotScope"]

SetHoldRest[GraphPlotScope];

GraphPlotScope[graph_, body_] := Scope[

  If[!GraphQ[graph], ReturnFailed[]];

  GraphScope[graph,

    {$VertexCoordinates, $EdgeCoordinateLists} = ExtractGraphPrimitiveCoordinates[$IndexGraph];

    $GraphHighlightStyle := $GraphHighlightStyle = removeSingleton @ LookupOption[$Graph, GraphHighlightStyle];
    $GraphPlotImageSize := $GraphPlotImageSize := LookupImageSize @ $Graph;
    $GraphPlotImageWidth := $GraphPlotImageWidth = First[$GraphPlotImageSize; LookupImageSize @ $Graph];

    (* before we have called the user function, guess the range based on the vertex and edge coordinates *)
    $GraphIs3D := $GraphIs3D = CoordinateMatrixQ[$VertexCoordinates, 3];
    $GraphPlotRange := $GraphPlotRange = computeCoordinateBounds[];
    $GraphPlotSize := $GraphPlotSize = rangeSize[$GraphPlotRange];
    $GraphPlotSizeX := Part[$GraphPlotSize, 1];
    $GraphPlotSizeY := Part[$GraphPlotSize, 2];
    $GraphPlotScale := $GraphPlotScale = If[$GraphIs3D, Norm @ $GraphPlotSize, $GraphPlotSizeX];
    $GraphPlotAspectRatio := $GraphPlotAspectRatio = computeGraphPlotAspectRatio[];

    $GraphMaxSafeVertexSize := $GraphMaxSafeVertexSize = computeMaxSafeVertexSize[];
    $GraphMaxSafeArrowheadSize := $GraphMaxSafeArrowheadSize = computeMaxSafeArrowheadSize[];

    body
  ]
];

$rangeMicroPadding = 1*^-12;
computeCoordinateBounds[] :=
  CoordinateBounds[{$VertexCoordinates, Replace[$EdgeCoordinateLists, {} -> Nothing]}, $rangeMicroPadding];

(**************************************************************************************************)

PackageExport["ExtendedGraphPlot"]

$autoFilledLegendPattern = (Automatic | _String) | Placed[Automatic | _String, _];

ExtendedGraphPlot[graph_] := Block[
  {
   $GraphPlotImageSize, $GraphPlotImageWidth, $GraphPlotRange, $GraphPlotSize, $GraphMaxSafeVertexSize,
   plottingFunction, graphLegend, graphRegionHighlight, vertexColorFunction,
   highlightGraphics, requiredPadding
  },

  If[!GraphQ[graph], ReturnFailed[]];

  {plottingFunction, graphLegend, graphRegionHighlight, vertexColorFunction} =
    LookupAnnotation[graph, {GraphPlottingFunction, GraphLegend, GraphRegionHighlight, VertexColorFunction}, None];

  SetNone[plottingFunction,
    If[vertexColorFunction === None, GraphComputation`GraphDrawing, ExtendedGraphPlottingFunction]];

  SetAutomatic[plottingFunction, ExtendedGraphPlottingFunction];

  GraphPlotScope[graph,

    $GraphPlotGraphics = plottingFunction[$Graph];

    If[MatchQ[$GraphPlotGraphics, _Legended],
      If[MatchQ[graphLegend, None | $autoFilledLegendPattern | {$autoFilledLegendPattern..}],
        graphLegend = Last @ $GraphPlotGraphics];
      $GraphPlotGraphics = First @ $GraphPlotGraphics;
    ];

    (* recompute these with the results of plottingFunction, for the benefit of GraphRegionHighlight *)
    $GraphPlotImageSize := $GraphPlotImageSize = LookupImageSize @ $GraphPlotGraphics;
    $GraphPlotImageWidth := $GraphPlotImageWidth = First[$GraphPlotImageSize] - Total[First @ LookupOption[$GraphPlotGraphics, ImagePadding]];
    $GraphPlotRange := $GraphPlotRange = GraphicsPlotRange @ $GraphPlotGraphics;
    $GraphPlotSize := $GraphPlotSize = rangeSize[$GraphPlotRange];
    $GraphMaxSafeVertexSize := $GraphMaxSafeVertexSize = computeMaxSafeVertexSize[];

    {highlightGraphics, highlightLegends, requiredPadding} = resolveGraphRegionHighlightGraphics @ graphRegionHighlight;
    If[highlightGraphics =!= {},
      $GraphPlotGraphics = GraphicsImageSizePadTo[$GraphPlotGraphics, requiredPadding];
      {negative, positive} = SelectDiscard[highlightGraphics, First /* Negative];
      $GraphPlotGraphics //= ApplyProlog @ Part[negative, All, 2];
      $GraphPlotGraphics //= ApplyEpilog @ Part[positive, All, 2];
      If[highlightLegends =!= None, graphLegend = highlightLegends];
    ];

    ApplyLegend[$GraphPlotGraphics, graphLegend]
  ]
];

GraphicsImageSizePadTo[graphics_, requiredPadding_] := Scope[
  padding = LookupOption[graphics, ImagePadding];
  padding = Map[Max[#, requiredPadding]&, padding, {2}];
  ReplaceOptions[graphics, ImagePadding -> padding]
];

rangeSize[range_] := EuclideanDistance @@@ range;

computeBorderDistance[{cl_, ch_}, {pl_, ph_}] := {cl - pl, ph - ph};

rankedMean[d_] /; Length[d] < 4 := Min @ d;
rankedMean[d_] := HarmonicMean @ Take[Sort @ d, Ceiling @ (2 * Sqrt @ Length[d])];

getRankedMinDistance[coords_] := Scope[
  coords //= ToPackedReal;
  distances = If[Length[coords] > 150,
    DistanceMatrix[RandomChoice[coords, 20], coords],
    DistanceMatrix[coords]
  ];
  distances = DeleteCases[Flatten @ distances, 0|0.];
  If[distances === {}, Infinity, rankedMean @ distances]
];

computeGraphPlotAspectRatio[] := Scope[
  If[CoordinateMatrixQ[$VertexCoordinates, 3],
    viewOptions = LookupExtendedGraphAnnotations[$Graph, ViewOptions];
    SetAutomatic[viewOptions, $automaticViewOptions];
    viewOptions = Association[PlotRange -> CoordinateBounds[$VertexCoordinates], viewOptions];
    viewTransform = ConstructGraphicsViewTransform[viewOptions];
    vertexCoordinates = viewTransform @ $VertexCoordinates;
    {width, height} = PlotRangeSize @ CoordinateBounds[vertexCoordinates];
    height / width
  ,
    $GraphPlotSizeY / $GraphPlotSizeX
  ]
];

computeMaxSafeVertexSize[] := Scope[
  minDistance = getRankedMinDistance[$VertexCoordinates];
  Max[Min[minDistance / 2, Max[$GraphPlotSize] / 3], $MachineEpsilon]
];

computeMaxSafeArrowheadSize[] := Scope[
  minDistance = getRankedMinDistance[lineCenter /@ $EdgeCoordinateLists];
  minDistance = Max[minDistance, $GraphMaxSafeVertexSize/Sqrt[2]];
  Min[$GraphMaxSafeVertexSize, minDistance, Max[$GraphPlotSize] / 3]
];

lineCenter = MatchValues[
  pair:{_, _} := Mean @ pair;
  list_List := Scope[
    n = Length[list]; n2 = (n + 1) / 2;
    If[IntegerQ[n2],
      Part[list, n2],
      Mean @ Part[list, {Floor @ n2, Ceiling @ n2}]
    ]
  ]
];

(**************************************************************************************************)

PackageExport["ExtendedGraphPlottingFunction"]

ExtendedGraphPlot::badcolors = "CardinalColors should be an association from cardinals to colors.";

ExtendedGraphPlottingFunction[___] := $Failed;

failPlot[msgName_String, args___] := (
  Message[MessageName[ExtendedGraphPlot, msgName], args];
  Throw[$Failed, ExtendedGraphPlottingFunction]
);

ExtendedGraphPlottingFunction[graph_Graph] := Scope @ Catch[

  (* process options *)
  FunctionSection[
    {graphLayout,
     imageSize, vertexSize, vertexStyle, edgeStyle, vertexLabelStyle, edgeLabelStyle,
     vertexShapeFunction, edgeShapeFunction} =
      LookupOption[graph, {GraphLayout,
        ImageSize, VertexSize, VertexStyle, EdgeStyle, VertexLabelStyle, EdgeLabelStyle,
        VertexShapeFunction, EdgeShapeFunction}, Automatic];

    {vertexLabels, edgeLabels, plotLabel, epilog, imagePadding} =
      LookupOption[graph, {VertexLabels, EdgeLabels, PlotLabel, Epilog, ImagePadding}, None];

    {arrowheadShape, arrowheadStyle, arrowheadSize, arrowheadPosition, labelCardinals, viewOptions} =
      LookupExtendedGraphAnnotations[graph, {ArrowheadShape, ArrowheadStyle, ArrowheadSize, ArrowheadPosition, LabelCardinals, ViewOptions}];

    {graphLegend, vertexColorFunction, vertexAnnotations} =
      LookupExtendedGraphAnnotations[graph, {GraphLegend, VertexColorFunction, VertexAnnotations}];
  ];

  (* initial processing of global options *)
  FunctionSection[
    cardinalColors = LookupCardinalColors[graph];
    If[!MatchQ[cardinalColors, None | _Association], failPlot["badcolors"]];

    SetNone[vertexAnnotations, <||>];

    automaticLegends = <||>;

    (* choose a size based on vertex seperation *)
    SetAutomatic[imageSize,
      If[Length[$VertexList] > 1000, Large,
        graphPlotSizeScalingFunction[If[$GraphIs3D, 30, 15] * ($GraphPlotSizeX / $GraphMaxSafeVertexSize)]]];

    If[RuleQ[imageSize],
      {imageWidth, imageHeight} = Match[imageSize,
        Rule["LongestEdge", sz_ ? NumericQ]   :> computeEdgeLengthBasedImageSize[1.0, sz],
        Rule["ShortestEdge", sz_ ? NumericQ]  :> computeEdgeLengthBasedImageSize[0.0, sz],
        Rule["MedianEdge", sz_ ? NumericQ]    :> computeEdgeLengthBasedImageSize[0.5, sz]
        _ :> ReturnFailed[]
      ];
    ,
      {imageWidth, imageHeight} = ToNumericImageSize[imageSize, Clip[$GraphPlotAspectRatio, {0.3, 2}]];
    ];
    If[$GraphIs3D,
      (* this makes 3D rotation less zoomy *)
      If[imageHeight > imageWidth * 1.5, imageHeight = imageWidth * 1.5];
      If[imageHeight < imageWidth * 0.5, imageHeight = imageWidth * 0.5];
    ];
    imageSize = {imageWidth, imageHeight};
    {effectiveImageWidth, effectiveImageHeight} = EffectiveImageSize[imageSize, $GraphPlotAspectRatio];

    imagePadding = Which[
      NumericQ[imagePadding], N[imagePadding] * {{1, 1}, {1, 1}},
      RealMatrixQ[imagePadding], N[imagePadding],
      True, {{1, 1}, {1, 1}}
    ];

    edgeCenters = lineCenter /@ $EdgeCoordinateLists;

    edgeStyle //= removeSingleton;
    vertexStyle //= removeSingleton;
  ];

  (* create graphics for vertices *)
  FunctionSection[

    $vertexSizeOverrides = None;
    $defaultVertexSize = Which[edgeStyle === None, 0.6, vertexColorFunction =!= None, 0.5, True, Max[0.3, AbsolutePointSize[6]]];
    vertexSize = processVertexSize @ removeSingleton @ vertexSize;
    vertexSizeImage = plotSizeToImageSize @ vertexSize;

    SetAutomatic[vertexShapeFunction, If[vertexColorFunction =!= None, "Disk", "Point"]];

    lighting = None;

    vertexShapeFunction //= removeSingleton;
    If[vertexShapeFunction === None,
      vertexGraphics = Nothing; setbackDistance = 0;
      Goto[skipVertices];
    ];

    {defaultVertexColor, vertexBaseStyle, setbackDistance, rawVertexDrawFunc, vertexPadding} =
      processVertexShapeFunction[vertexShapeFunction];
    SetNone[vertexBaseStyle, Nothing];
    extendPadding[vertexPadding];

    vertexDrawFunc = If[$vertexSizeOverrides === None,
      vertexDrawFuncWithSize[rawVertexDrawFunc, vertexSize],
      vertexDrawFuncSizeRemapper[rawVertexDrawFunc, $vertexSizeOverrides, vertexSize]
    ];

    SetAutomatic[vertexStyle, defaultVertexColor];
    vertexStyle //= toDirective;

    If[vertexColorFunction =!= None,

      colorGroupFun = If[MatchQ[vertexColorFunction, Tooltip[_]],
        vertexColorFunction = First @ vertexColorFunction;
        toColorVertexDrawFuncWithTooltip[vertexDrawFunc],
        toColorVertexDrawFunc[vertexDrawFunc]
      ];

      palette = Automatic;
      vertexColorFunction //= Replace[Paletted[d_, p_] :> (palette = p; d)];
      vertexColorFunctionData = toColorFunctionData[vertexColorFunction];
      If[FailureQ[vertexColorFunctionData], failPlot["badcolvals"]];
      {colorGroups, colorFunctionObject} = ApplyColoring[vertexColorFunctionData, palette];
      If[FailureQ[colorFunctionObject], failPlot["badcolvals"]];
      If[!MatchQ[colorFunctionObject, ColorFunctionObject["Discrete", Identity]],
        automaticLegends["Colors"] := colorFunctionObject];

      vertexItems = KeyValueMap[colorGroupFun, colorGroups];
    ,
      vertexItems = vertexDrawFunc[Range @ $VertexCount, vertexStyle];

    ];
    vertexGraphics = makeGraphicsGroup @ {vertexBaseStyle, vertexStyle, vertexItems};
  ];
  Label[skipVertices];

  (* create graphics for edges *)
  FunctionSection[
    If[edgeStyle === None,
      edgeGraphics = Nothing;
      Goto[skipEdges];
    ];

    SetAutomatic[edgeStyle, Directive[Opacity[.18], Black, If[$GraphIs3D, MediumThick, SlightlyThick]]];
    edgeStyle //= toDirectiveOptScan[setEdgeStyleGlobals];

    If[arrowheadShape === None || zeroQ[arrowheadSize] || UndirectedGraphQ[$Graph],
      edgePrimitives = annotatedEdgePrimitives[setback[Line, setbackDistance], Range @ $EdgeCount];
      edgeGraphics = makeGraphicsGroup @ {edgeStyle, edgePrimitives};
    ,
      SetAutomatic[arrowheadStyle, Which[
        cardinalColors =!= None, cardinalColors,
        vertexColorFunction =!= None, LightGray,
        True, Gray
      ]];
      baseArrowheadSize := baseArrowheadSize = If[$GraphIs3D, 0.45, 0.8] * ($GraphMaxSafeArrowheadSize / $GraphPlotSizeX);
      arrowheadSize //= processArrowheadSize;

      SetAutomatic[arrowheadShape, If[$GraphIs3D, "Cone", "Arrow"]];
      $twoWayStyle = "ArrowDoubleIn";
      If[ListQ[arrowheadShape],
        {arrowheadShape, arrowheadShapeOpts} = FirstRest @ arrowheadShape;
        Scan[scanArrowheadShapeOpts, arrowheadShapeOpts]];

      SetAutomatic[arrowheadPosition, If[$GraphIs3D && arrowheadShape =!= "Cone", 0.65, 0.502]];

      arrowheadBounds = CoordinateBounds[
        edgeCenters,
        Max[arrowheadSize * $GraphPlotSizeX] * 0.5
      ];
      extendPaddingToInclude[arrowheadBounds];

      edgeTagGroups = If[$EdgeTags === None,
        <|All -> Range[$EdgeCount]|>,
        edgeTagGroups = PositionIndex[$EdgeTags]
      ];

      edgeGraphics = KeyValueMap[drawArrowheadEdges, edgeTagGroups];
      edgeGraphics = makeGraphicsGroup @ {edgeStyle, edgeGraphics};
    ];
  ];
  Label[skipEdges];

  (* create labels for vertices and edges *)
  FunctionSection[
    labelGraphics = {};

    (* for label style, support: Opacity, as well as Tiny, Small, etc. Medium corresponds to ordinary size. *)

    vertexLabels //= removeSingleton;
    If[vertexLabels =!= None,
      SetNone[vertexSize, $GraphMaxSafeVertexSize / 5];
      vertexLabelItems = generateLabelPrimitives[
        vertexLabels, $VertexList, $VertexCoordinates,
        vertexSize, vertexLabelStyle, vertexAnnotations
      ];
      AppendTo[labelGraphics, vertexLabelItems]
    ];

    edgeLabels //= removeSingleton;
    If[edgeLabels =!= None,
      SetAutomatic[arrowheadSize, 0];
      edgeLabelItems = generateLabelPrimitives[
        edgeLabels, $EdgeList, edgeCenters,
        Max[arrowheadSize] * $GraphPlotSizeX, edgeLabelStyle, <||>
      ];
      AppendTo[labelGraphics, edgeLabelItems]];

    labelGraphics = If[labelGraphics === {}, Nothing, GraphicsGroup @ labelGraphics];
  ];

  (* create the final graphics *)
  FunctionSection[
    If[labelGraphics =!= Nothing, extendPadding @ estimateLabelPadding @ labelGraphics];

    (* for graphs with cardinals, create an automatic legend when asked *)
    If[cardinalColors =!= None && arrowheadStyle =!= None,
      automaticLegends["Cardinals"] := ArrowheadLegend[cardinalColors, arrowheadShape];
    ];

    imagePadding //= Ceiling;
    imageSize = Ceiling @ ImageSizePad[imageSize, imagePadding];

    graphicsElements = {edgeGraphics, vertexGraphics, labelGraphics};

    extraOptions = If[!$GraphIs3D, {},
      SetAutomatic[viewOptions, $automaticViewOptions];
      viewOptions
    ];

    (* assemble graphics *)
    graphics = If[$GraphIs3D, makeGraphics3D, makeGraphics][
      graphicsElements,
      imageSize, imagePadding, plotLabel, extraOptions, epilog
    ];

    (* obtain final plotrange *)
    plotRange = $GraphPlotRange;

    (* plotRange += {{-1, 1}, {-1, 1}} * $rangeMicroPadding; *)
    AppendTo[graphics, PlotRange -> plotRange];

    applyAutomaticLegends[graphics, automaticLegends, graphLegend]
  ]
,
  ExtendedGraphPlottingFunction
];

lineDiagonalLength[line_] :=
  EuclideanDistance @@ CoordinateBoundingBox @ line;

lineLength[line_] := EuclideanDistance[First @ line, Last @ line];

computeEdgeLengthBasedImageSize[q_, edgeSize_] := Scope[
  edgeLengths = Chop @ Map[lineLength, $EdgeCoordinateLists];
  edgeLengths = DeleteCases[edgeLengths, 0|0.];
  If[edgeLengths === {},
    edgeLengths = Map[lineDiagonalLength, $EdgeCoordinateLists]];
  quantile = Quantile[edgeLengths, q];
  scaling = edgeSize / quantile;
  Max[#, 10]& /@ N[$GraphPlotSize * scaling]
];

imageSizeToImageFraction[sz_] := sz / effectiveImageWidth;
imageSizeToPlotSize[sz_] := sz / effectiveImageWidth * $GraphPlotSizeX;
imageFractionToPlotSize[sz_] := sz * $GraphPlotSizeX;
plotSizeToImageFraction[sz_] := sz / $GraphPlotSizeX;
plotSizeToImageSize[sz_] := sz / $GraphPlotSizeX * effectiveImageWidth;
plotSizeToDiskSize[sz_] := Scaled[sz * {1, $GraphPlotAspectRatio} / $GraphPlotScale];
plotSizeToPointSize[sz_] := PointSize[sz / $GraphPlotSizeX];

extendPadding[n_] := imagePadding = Map[Max[#, n]&, imagePadding, {2}];
extendPadding[padding_List] := imagePadding = MapThread[Max, {imagePadding, padding}, 2];

extendPaddingToInclude[{{xmin_, xmax_}, {ymin_, ymax_}}] := Scope[
  {{pxmin, pxmax}, {pymin, pymax}} = $GraphPlotRange;
  extra = {{pxmin - xmin, xmax - pxmax}, {pymin - ymin, ymax - pymax}};
  extendPadding @ plotSizeToImageSize @ extra
];

makeGraphicsGroup[g_] := g;

zeroQ[0|0.] := True;
zeroQ[_] := False;

vertexDrawFuncWithSize[rawVertexDrawFunc_, vertexSize_] :=
  ReplaceAll[
    {indices, color} |-> Annotation[
      rawVertexDrawFunc[Part[$VertexCoordinates, indices], color],
      indices, "VertexPrimitives"
    ],
    $vertexSize -> vertexSize
  ];

vertexDrawFuncSizeRemapper[rawVertexDrawFunc_, sizeOverrides_, vertexSize_] :=
  {indices, color} |-> Map[
    index |-> Construct[
      ReplaceAll[
        rawVertexDrawFunc,
        $vertexSize -> Lookup[sizeOverrides, index, vertexSize]
      ],
      Part[$VertexCoordinates, index], color
    ], indices
  ];

toColorVertexDrawFunc[vertexDrawFunc_] :=
  {colorValue, indices} |-> vertexDrawFunc[indices, First @ colorValue];

toColorVertexDrawFuncWithTooltip[vertexDrawFunc_] :=
  {colorValue, indices} |-> NiceTooltip[
    vertexDrawFunc[indices, First @ colorValue],
    Last @ colorValue
  ];

(**************************************************************************************************)

ExtendedGraphPlot::badarrowheadsize = "ArrowheadSize -> `` is not a valid specification."

(* the resulting size is expressed in the coordinate system of the size specification for Arrowheads[...],
which is a fraction of the image width *)
processArrowheadSize = MatchValues[
  s:$SymbolicSizePattern          := imageSizeToImageFraction[4. * Lookup[$SymbolicPointSizes, s]];
  r_ ? NumericQ                   := N[imageSizeToImageFraction[r]];
  PointSize[sz_ ? NumericQ]       := N[sz];
  AbsolutePointSize[sz_ ? NumericQ] := N[imageSizeToImageFraction[sz]];
  Scaled[r_ ? NumericQ]           := baseArrowheadSize * N[r];
  Scaled[s:$SymbolicSizePattern]  := baseArrowheadSize * Lookup[$SymbolicSizeFractions, s];
  assoc_Association               := Map[%, assoc];
  Automatic                       := baseArrowheadSize;
  spec_ := failPlot["badarrowheadsize", spec];
];

PackageExport["TwoWayStyle"]

scanArrowheadShapeOpts = MatchValues[
  TwoWayStyle -> s:("Out"|"In"|"OutClose"|"InClose") := $twoWayStyle ^= arrowheadShape <> "Double" <> s;
  TwoWayStyle -> s:("Square"|"Ball"|"Disk"|"Diamond"|None) := $twoWayStyle ^= s;
  rule_ := failPlot["badsubopt", rule, ArrowheadShape];
];


arrowheadsND[e_] := If[$GraphIs3D, Arrowheads[e, Appearance -> "Projected"], Arrowheads @ e];

drawArrowheadEdges[cardinal_, indices_] := Scope[
  If[
    Or[
      lookupTagSpec[arrowheadShape, cardinal] === None,
      zeroQ @ lookupTagSpec[arrowheadSize, cardinal]
    ],
    func = setback[Line, setbackDistance];
  ,
    arrowheads = arrowheadsND @ List @ makeArrowheadsElement @ cardinal;
    func = setback[Arrow, setbackDistance] /* StyleOperator[arrowheads];
  ];
  annotatedEdgePrimitives[func, indices]
];

annotatedEdgePrimitives[f_, indices_] := Annotation[
  f @ Part[$EdgeCoordinateLists, indices],
  indices, "EdgePrimitives"
];

drawArrowheadEdges[CardinalSet[cardinals_], indices_] := Scope[
  If[$twoWayStyle =!= None,
    cardinals = SortBy[cardinals, NegatedQ];
    cardinals //= ReplaceRepeated[{l___, c_, m___, Negated[c_], r___} :> {l, TwoWay[c], m, r}]];
  cardinals = SortBy[cardinals, {Head[#] === TwoWay, StripNegated @ #}&];
  num = Length[cardinals];
  positions = If[num === 1, {arrowheadPosition}, 0.5 + 0.2 * Standardize[Range[num]]];
  arrowheads = arrowheadsND @ MapThread[
    {card, pos} |-> makeArrowheadsElement[arrowheadPosition = pos; card],
    {cardinals, positions}
  ];
  func = setback[multiArrow, setbackDistance] /* StyleOperator[arrowheads];
  annotatedEdgePrimitives[func, indices]
];

makeArrowheadsElement[cardinal_] := Scope[
  {shape, size, style, position} = lookupTagSpec[cardinal];
  shape = If[shape === "Cardinal",
    SetNone[style, Black];
    makeArrowheadLabelShape[cardinal, style, size]
  ,
    SetNone[style, $Gray];
    makeArrowheadShape[shape, style]
  ];
  If[labelCardinals, shape = attachArrowheadLabel[shape, StripNegated @ cardinal, size]];
  {size, position, shape}
];

lookupTagSpec[other_, cardinal_] := other;
lookupTagSpec[assoc_Association, cardinal_] :=
  Lookup[assoc, cardinal, Lookup[assoc, All, None]];

lookupTagSpec[Negated[cardinal_]] :=
  MapAt[Minus, 2] @ lookupTagSpec[cardinal];

lookupTagSpec[TwoWay[cardinal_]] :=
  ReplacePart[1 -> $twoWayStyle] @ lookupTagSpec[cardinal];

lookupTagSpec[cardinal_] := Map[
  lookupTagSpec[#, cardinal]&,
  {arrowheadShape, arrowheadSize, arrowheadStyle, arrowheadPosition}
];

(**************************************************************************************************)

attachArrowheadLabel[g:Graphics[primitives_, opts___], cardinal_, size_] := Scope[
  cardinal //= Replace[TwoWay[c_] :> c(* Row[{c, Negated[c]}] *)];
  label = makeArrowheadLabel[cardinal, size];
  {{xl, xh}, {yl, yh}} = GraphicsPlotRange[g];
  labelPrimitives = {Opacity[1], Black, Inset[label, {0., xl - 0.2}, {0, 0}, Automatic, None]};
  Graphics[{primitives, labelPrimitives}, opts]
];

attachArrowheadLabel[other_, _, _] := other;

(**************************************************************************************************)

makeArrowheadLabel[cardinal_, size_] := Scope[
  fontSize = Clip[Round[size * imageSize[[1]] / 4], {10, 15}];
  Style[cardinal, FontSize -> fontSize, $LegendLabelStyle, Background -> GrayLevel[1.0, 0.8]]
];

makeArrowheadLabelShape[TwoWay[cardinal_], style_, size_] := Scope[
  label = makeArrowheadLabel[cardinal, size];
  Graphics[{Opacity[1], style,
    Text[label, {0, 0}, {0, -1}],
    Text[label, {0, 0}, {0, -1}, {-1, 0}]
  }]
];

makeArrowheadLabelShape[cardinal_, style_, size_] := Scope[
  label = makeArrowheadLabel[cardinal, size * 1.75];
  Graphics[{Opacity[1], style, Text[label, {0, 0}, {0, -0.9}]}]
];

(**************************************************************************************************)

makeArrowheadGraphic2D[primitives_, style_, opts___] :=
  Graphics[
    {Opacity[1], EdgeForm[None], style, primitives},
    AspectRatio -> 1,
    PlotRangeClipping -> False,
    opts
  ];

makeArrowheadGraphic3D[primitives_, style_, opts___] :=
  Graphics3D[
    {Opacity[1], EdgeForm @ None, FaceForm @ style, primitives},
    opts
  ];

$arrowheads2D = Association[
  "Line" ->
    Line @ ToPacked @ {{-0.2, -0.3}, {0.1, 0.}, {-0.2, 0.3}},
  "LineDoubleOut" ->
    Line @ ToPacked @ {
      {{+0.1, -0.3}, {+0.4, 0.}, {+0.1, 0.3}},
      {{-0.1, -0.3}, {-0.4, 0.}, {-0.1, 0.3}}
    },
  "LineDoubleOutClose" ->
    Line @ ToPacked @ {
      {{0., -0.3}, {+0.3, 0.}, {0., 0.3}},
      {{0., -0.3}, {-0.3, 0.}, {0., 0.3}}
    },
  "LineDoubleIn" ->
    Line @ ToPacked @ {
      {{+0.4, -0.3}, {+0.1, 0.}, {+0.4, 0.3}},
      {{-0.4, -0.3}, {-0.1, 0.}, {-0.4, 0.3}}
    },
  "LineDoubleInClose" ->
    Line @ ToPacked @ {
      {{+0.3, -0.3}, {0., 0.}, {+0.3, 0.3}},
      {{-0.3, -0.3}, {0., 0.}, {-0.3, 0.3}}
    },
  "DoubleLine" ->
    Line @ ToPacked @ {
      {{-0.25, -0.3}, {+0.05, 0.}, {-0.25, 0.3}},
      {{-0.05, -0.3}, {+0.25, 0.}, {-0.05, 0.3}}
    },
  "Triangle" ->
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.1, -0.3}, {-0.1, 0.3}, {0.2, 0.}}}
    ],
  "TriangleDoubleOut" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.1, -0.3}, {-0.1, 0.3}, {-0.4, 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.1, -0.3}, {+0.1, 0.3}, {0.4, 0.}}}
    ]},
  "TriangleDoubleOutClose" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0., -0.3}, {0., 0.3}, {-0.3, 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0., -0.3}, {0., 0.3}, {0.3, 0.}}}
    ]},
  "TriangleDoubleIn" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.4, -0.3}, {-0.4, 0.3}, {-0.1, 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.4, -0.3}, {+0.4, 0.3}, {0.1, 0.}}}
    ]},
  "TriangleDoubleInClose" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.3, -0.3}, {-0.3, 0.3}, {0., 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0.3, -0.3}, {0.3, 0.3}, {0., 0.}}}
    ]},
  "HalfTriangle" ->
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.15, -0.4}, {-0.15, 0.}, {0.25, 0.}}}
    ],
  "HalfTriangleDoubleOut" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.1, -0.4}, {-0.1, 0.}, {-0.5, 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.1, -0.4}, {+0.1, 0.}, {0.5, 0.}}}
    ]},
  "HalfTriangleDoubleOutClose" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0., -0.4}, {0., 0.}, {-0.4, 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0., -0.4}, {0., 0.}, {+0.4, 0.}}}
    ]},
  "HalfTriangleDoubleIn" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.5, -0.4}, {-0.5, 0.}, {-0.1, 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.5, -0.4}, {+0.5, 0.}, {+0.1, 0.}}}
    ]},
  "HalfTriangleDoubleInClose" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.4, -0.4}, {-0.4, 0.}, {0., 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.4, -0.4}, {+0.4, 0.}, {0., 0.}}}
    ]},
  "Arrow" ->
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.3166, -0.3333}, {-0.1833, 0.}, {-0.3166, 0.3333}, {0.25, 0.}}}
    ],
  "ArrowDoubleOut" -> {
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.05, -0.33}, {-0.45, 0.}, {-0.05, 0.33}, {-0.15, 0.}}}
    ],
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0.05, -0.33}, {0.45, 0.}, {0.05, 0.33}, {0.15, 0.}}}
    ]},
  "ArrowDoubleOutClose" -> {
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0.0, -0.33}, {-0.4, 0.}, {0.0, 0.33}, {-0.1, 0.}}}
    ],
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0.0, -0.33}, {0.4, 0.}, {0.0, 0.33}, {0.1, 0.}}}
    ]},
  "ArrowDoubleIn" -> {
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.45, -0.33}, {-0.05, 0.}, {-0.45, 0.33}, {-0.35, 0.}}}
    ],
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.45, -0.33}, {+0.05, 0.}, {+0.45, 0.33}, {+0.35, 0.}}}
    ]},
  "ArrowDoubleInClose" -> {
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.4, -0.33}, {0., 0.}, {-0.4, 0.33}, {-0.30, 0.}}}
    ],
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.4, -0.33}, {0, 0.}, {+0.4, 0.33}, {+0.3, 0.}}}
    ]},
  "Disk" ->
    Disk[ToPacked @ {0., 0.}, .2],
  "Square" ->
    ToPacked /@ Rectangle[-0.2 * {1,1}, 0.2 * {1,1}],
  "Diamond" ->
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.45, 0.}, {0., -0.25}, {0.45, 0.}, {0., 0.25}}}
    ]
];

$coneRadius = 0.15;

$arrowheads3D = Association[
  "Cone" ->
    Cone[ToPacked @ {{-0.2, 0., 0.}, {0.2, 0., 0.}}, $coneRadius],
  "ConeDoubleOut" ->
    Cone[ToPacked @ {{{0.1, 0., 0.}, {0.5, 0., 0.}}, {{-0.1, 0., 0.}, {-0.5, 0., 0.}}}, {$coneRadius, $coneRadius}],
  "ConeDoubleOutClose" ->
    Cone[ToPacked @ {{{0., 0., 0.}, {0.4, 0., 0.}}, {{0., 0., 0.}, {-0.4, 0., 0.}}}, {$coneRadius, $coneRadius}],
  "ConeDoubleIn" ->
    Cone[ToPacked @ {{{0.5, 0., 0.}, {0.1, 0., 0.}}, {{-0.5, 0., 0.}, {-0.1, 0., 0.}}}, {$coneRadius, $coneRadius}],
  "Sphere" ->
    Sphere[ToPacked @ {0., 0., 0.}, .2]
];


PackageScope["abs3DColor"]

abs3DColor[c_] := Directive[Glow @ c, Black, Specularity @ 0];

$namedArrowheads = Union[Keys @ $arrowheads2D, Keys @ $arrowheads3D, {"Cardinal"}];

(**************************************************************************************************)

makeArrowheadShape["Sphere", style_] :=
  makeArrowheadGraphic3D[$arrowheads3D @ name, abs3DColor @ style];

makeArrowheadShape[name_String, style_] /; StringStartsQ[name, {"Line", "DoubleLine"}] :=
  makeArrowheadGraphic2D[
    $arrowheads2D @ name,
    Directive[style, If[$GraphIs3D,
      Thickness @ If[StringContainsQ[name, "Double"], 0.075, 0.15],
      AbsoluteThickness @ 1.2
    ]]
  ];

ExtendedGraphPlot::arrow3din2d = "Cannot use shape `` in a 2D graph."

makeArrowheadShape[name_String, style_] := Which[
  KeyExistsQ[$arrowheads2D, name],
    makeArrowheadGraphic2D[$arrowheads2D @ name, style],
  KeyExistsQ[$arrowheads3D, name],
    If[!$GraphIs3D, failPlot["arrow3din2d", name]];
    makeArrowheadGraphic3D[$arrowheads3D @ name, abs3DColor @ style],
  True,
    badArrowheadShape[name]
];

makeArrowheadShape[Graphics[elems_, opts___], style_] :=
  Graphics[{Opacity[1], style, elems}, opts, AspectRatio -> 1, PlotRangeClipping -> False];

makeArrowheadShape[Placed[img_Image, pos_], style_] :=
  imageToGraphics[img, pos];

makeArrowheadShape[img_Image, style_] :=
  imageToGraphics[img, {0, 1}];

makeArrowheadShape[spec_, _] :=
  badArrowheadShape[spec];

badArrowheadShape[spec_] :=
  failPlot["badarrowhead", spec,
    commaString @ Discard[StringContainsQ["Double"]] @ $namedArrowheads];

imageToGraphics[img_, pos_] := ImageToGraphics[ImageResize[img, 50], pos, 50];

ExtendedGraphPlot::badarrowhead = "ArrowheadShape -> `` should be None, Automatic, Graphics[..], an Image, or one of ``."

(**************************************************************************************************)

PackageExport["ArrowheadLegend"]

to2DShape["Cone"] = "Arrow";
to2DShape["Sphere"] = "Disk";
to2DShape[a_] := a;

ArrowheadLegend[assoc_, "Cardinal"] := "";
ArrowheadLegend[assoc_Association, shape_:"Arrow"] := Scope[
  rows = KeyValueMap[
    {name, color} |-> {makeLegendArrowheadGraphic[color, to2DShape @ shape], name},
    assoc
  ];
  Framed[
    Grid[rows, BaseStyle -> $LegendLabelStyle, Spacings -> {.4, 0.5}],
    FrameMargins -> {{0, 0}, {5, 5}},
    FrameStyle -> None
  ]
]

PackageScope["makeLegendArrowheadGraphic"]

makeLegendArrowheadGraphic[color_, shape_] := makeArrowheadGraphic2D[
  Rotate[$arrowheads2D @ shape, Pi/2], FaceForm @ color,
  BaselinePosition -> Scaled[0.1], ImageSize -> {11, 11}
];

(**************************************************************************************************)

PackageScope["makeHighlightArrowheadShape"]

makeHighlightArrowheadShape[style_, scaling_] :=
  makeArrowheadGraphic2D[
    $arrowheads2D["Line"] /. r_List :> (r * scaling), toDirective @ style, args
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

ExtendedGraphPlot::badvshapefunc = "`` is not a valid setting for VertexShapeFunction. Valid shapes are: ``."

$validVertexShapes = {"Disk", "Ball", "Point", None};

processVertexShapeFunction[spec_] := Scope[
  setbackDistance = 0; vertexPadding = 0;
  vertexBaseStyle = None;
  Switch[spec,
    "Disk" /; $GraphIs3D,
      defaultVertexColor = $Gray;
      vertexDrawFunc = mapCoordArray[drawDisk3D[$vertexSize]];
      vertexPadding = 1 + vertexSizeImage * 2;
      setbackDistance = vertexSize / 2;
    ,
    "Ball" | "Sphere" /; $GraphIs3D,
      defaultVertexColor = $LightGray;
      vertexDrawFunc = drawSphere[$vertexSize / (2 * Sqrt[3.])];
    ,
    "Disk" | "Ball" | "Sphere",
      vertexPadding = 1 + vertexSizeImage/2;
      defaultVertexColor = $DarkGray;
      vertexBaseStyle = EdgeForm @ AbsoluteThickness[.25];
      vertexDrawFunc = drawDisk[$vertexSize / 2];
    ,
    "Point",
      defaultVertexColor = $DarkGray;
      vertexDrawFunc = drawPoint[$vertexSize];
      vertexPadding = 1 + vertexSizeImage / 2;
    ,
    "Square",
      defaultVertexColor = $DarkGray;
      vertexDrawFunc = drawSquare[$vertexSize];
      vertexBaseStyle = EdgeForm @ AbsoluteThickness[1];
      vertexPadding = 1 + vertexSizeImage / 2;
    ,
    "Hexagon",
      defaultVertexColor = $DarkGray;
      vertexDrawFunc = drawHexagon[$vertexSize];
      vertexBaseStyle = EdgeForm @ AbsoluteThickness[1];
      vertexPadding = 1 + vertexSizeImage / 2,
    _,
      failPlot["badvshapefunc", spec, commaString @ $validVertexShapes];
  ];
  {defaultVertexColor, vertexBaseStyle, setbackDistance, vertexDrawFunc, vertexPadding}
];

drawPoint[size_][pos_, color_] :=
  Style[Point @ pos, plotSizeToPointSize @ size, color];

drawSphere[size_][pos_, color_] :=
  Style[Sphere[pos, size], color];

drawDisk[size_][pos_, color_] :=
  edgedStyle[color] @ mapCoordArray[Disk, pos, size];

edgedStyle[color_][primitives_] := Style[
  primitives,
  FaceForm[color], EdgeForm @ Darker[color, .3]
];

drawDisk3D[size_][pos_, color_] := Inset[
  Graphics[drawDisk[1][{0, 0}, color], AspectRatio -> 1],
  pos, {0, 0}, plotSizeToDiskSize @ size
];

drawSquare[size_][pos_, color_] :=
  edgedStyle[color] @ mapCoordArray[Rectangle[# - size / 2, # + size / 2]&, pos];

(* todo: replace this with a single polygon *)
$hexagonPoints = CirclePoints[{1, Tau/12}, 6] / Sqrt[2];
drawHexagon[size_][pos_, color_] :=
  edgedStyle[color] @ mapCoordArray[Polygon[PlusVector[$hexagonPoints * size, #]]&, pos];

mapCoordArray[f_][array_, args___] :=
  mapCoordArray[f, array, args];

mapCoordArray[f_, array_, args___] :=
  If[CoordinateMatrixQ[array], Map[f[#, args]&, array], f[array, args]];

(**************************************************************************************************)

multiLine[pos_] := Which[
  ArrayQ[pos, 2|3] || VectorQ[pos, MatrixQ], Line[pos],
  ArrayQ[pos, 2, MatrixQ], Map[Line, pos],
  True, Print["Invalid multiline dimensions: ", Dimensions /@ pos]
];

multiArrow[pos_, dist_] := Which[
  VectorQ[pos, VectorQ], Arrow[pos, dist],
  VectorQ[pos, MatrixQ], Map[Arrow[#, dist]&, pos],
  True, Print["Invalid multiarrow dimensions: ", Dimensions /@ pos, pos]
];

(* this is needed because when Arrow is given a list of paths, it will interpret the arrowhead
spec as being a list of specs and walk them in parallel *)
multiArrow[pos_] := Which[
  VectorQ[pos, VectorQ], Arrow[pos],
  VectorQ[pos, MatrixQ], Map[Arrow, pos],
  True, Print["Invalid multiarrow dimensions: ", Dimensions /@ pos, pos]
];

setback[head_, 0] := head;

setback[multiArrow, dist_] := multiArrow[#, dist]&;

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

getVertexIndex[v_] := Lookup[$VertexIndex, v, failPlot["notvertex", v]];
getAnnoValue[annos_, key_] := Lookup[annos, key, failPlot["badgraphannokey", key, commaString @ Keys @ annos]];

toColorFunctionData = MatchValues[
  "Name" := $VertexList;
  "Index" := VertexRange[$Graph];
  (* todo, make the distance work on regions as well *)
  "Distance" := MetricDistance[$MetricGraphCache, 1];
  {"Distance", v_} := MetricDistance[$MetricGraphCache, getVertexIndex @ v];
  {key_String, v_} := Part[getAnnoValue[vertexPairAnnotations, key], getVertexIndex @ v];
  {key_String, v_, Transpose} := Part[getAnnoValue[vertexPairAnnotations, key], All, getVertexIndex @ v];
  key_String := getAnnoValue[vertexAnnotations, key];
  (key_String -> f_) := Replace[Quiet @ Check[Map[toFunc @ f, %[key]], $Failed], $Failed :> failPlot["msgcolfunc", key]];
  list_List /; Length[list] === $VertexCount := list;
  assoc_Association := Lookup[assoc, $VertexList, Lookup[assoc, All, $LightGray]];
  spec_ := failPlot["badcolfunc", If[Length[spec] > 10 || ByteCount[spec] > 1000, Skeleton[Length[spec]], spec]]
];

toFunc[i_Integer] := Extract[i];
toFunc[f_] := f;

ExtendedGraphPlot::badcolvals = "VertexColorFunction did not produce values that could be colored."

(**************************************************************************************************)

ExtendedGraphPlot::badvertexsize = "`` is not a valid setting for VertexSize."

$defaultVertexSize = 0.3;
(* this returns a size in plot range coordinates *)

processVertexSize = MatchValues[
  Automatic                     := %[$defaultVertexSize];
  r_ ? NumericQ                 := %[{"Nearest", r}];
  {"Nearest", r_ ? NumericQ}    := N[r] * $GraphMaxSafeVertexSize;
  {"Scaled", r_ ? NumericQ}     := N[r] * Norm[$GraphPlotSize];
  PointSize[sz_ ? NumericQ]     := N[imageFractionToPlotSize[sz]];
  AbsolutePointSize[sz_ ? NumericQ] := N[imageSizeToPlotSize[sz]];
  m_Max | m_Min                 := Map[%, m];
  sym:$SymbolicSizePattern      := N @ imageSizeToPlotSize @ Lookup[$SymbolicPointSizes, sym];
  Scaled[sym:$SymbolicSizePattern] := %[$SymbolicSizeFractions @ sym * 0.5];
  rule_Rule                     := %[{rule}];
  rules_Association             := %[Normal @ rules];
  rules:{__Rule}                := (
    $vertexSizeOverrides ^= Association[processVertexSizeRule /@ rules];
    Lookup[rules, Key @ All, %[$defaultVertexSize]]
  );
  other_                        := failPlot["badvertexsize", other];
];

processVertexSizeRule[All -> _] :=
  Nothing;

processVertexSizeRule[lhs_ -> rhs_] :=
  findVertex[lhs] -> processVertexSize[rhs];

ExtendedGraphPlot::badvertex = "`` is not a valid vertex."

findVertex[v_] := Lookup[$VertexIndex, Key @ v, failPlot["badvertex", v]];
findVertex[GraphOrigin] := findVertex @ LookupAnnotation[$Graph, GraphOrigin];


(**************************************************************************************************)

ExtendedGraphPlot::badlabelspec = "The label specification `` was not one of the recognized forms."

estimateLabelPadding[graphics_] := Scope[
  text = FirstCase[graphics, Text[t_, ___], $Failed, Infinity];
  If[FailureQ[text], Return @ {{0, 0}, {0, 0}}];
  {w, h} = 1 + cachedRasterizeSize[Text @ First @ text] / 2;
  {x, y} = Part[text, 3];
  {
    offsetToPadding[x, w],
    offsetToPadding[y, h]
  }
];

offsetToPadding[o_, s_] := Switch[Sign[o], 1, {s, 0}, 0, {s, s}/2, -1, {0, s}];

cachedRasterizeSize[e_] := cachedRasterizeSize[e] = Rasterize[e, "RasterSize"];

PackageExport["LabelPosition"]

generateLabelPrimitives[spec_, names_, coordinates_, size_, labelStyle_, annotations_] := Scope[
  $annotationKeys = Keys[annotations];
  $labelNames = names; $annotations = annotations; $labeledElemSize = size / 2; $spacings = 0;
  $labelSizeScale = 1; $labelY = -1; $labelX = 0; $labelBackground = GrayLevel[1.0, 0.6];
  $labelBaseStyle = Inherited;
  labelStyle //= removeSingleton;
  labelStyle //= toDirectiveOptScan[setLabelStyleGlobals];
  labelStyle //= DeleteCases[sspec:$sizePattern /; ($labelSizeScale = toNumericSizeScale @ sspec; True)];
  $magnifier = If[$labelSizeScale == 1, Identity, Magnify[#, $labelSizeScale]&];
  {payloadFunction, placerFunction} = processLabelSpec[spec];
  labelElements = MapIndexed[placerFunction[labelForm @ payloadFunction @ First @ #2, #1]&, coordinates];
  {labelStyle, labelElements}
];

toDirectiveOptScan[f_][{Automatic}] :=
  Directive[];

toDirectiveOptScan[f_][e_] :=
  toDirectiveOptScan[f][{e}];

toDirectiveOptScan[f_][e_List | e_Directive] := (
  Scan[f, Cases[e, _Rule]];
  toDirective @ DeleteCases[e, _Rule]
)

ExtendedGraphPlot::badsubopt = "`` is not a recognized option to ``. Recognized options are ItemSize, BaseStyle, Background, LabelPosition, Spacings."

setLabelStyleGlobals = MatchValues[
  ItemSize -> size:$sizePattern := $labelSizeScale ^= toNumericSizeScale @ size;
  Background -> o:$opacityPattern := $labelBackground ^= GrayLevel[1.0, toNumericOpacity @ o];
  Background -> c:$ColorPattern := $labelBackground ^= c;
  BaseStyle -> s_ := $labelBaseStyle ^= s;
  LabelPosition -> Top|Above := $labelY ^= -1;
  LabelPosition -> Bottom|Below := $labelY ^= 1;
  LabelPosition -> Center := $labelX ^= $labelY ^= 0;
  LabelPosition -> Left := $labelX ^= 1;
  LabelPosition -> Right := $labelX ^= -1;
  Spacings -> n_ := $spacings ^= N[n];
  rule_ := failPlot["badsubopt", rule, LabelStyle];
];

$payloadP = _String | _Association;

processLabelSpec = MatchValues[
  Automatic | All :=              %["Name"];
  Tooltip :=                      %[Placed["Name", Tooltip]];
  p:$payloadP :=                  {toPayloadFunction @ p, placeLabelAt};
  Tooltip[p:$payloadP] :=         {toPayloadFunction @ p, placeTooltipAt};
  Placed[p:$payloadP, Tooltip] := {toPayloadFunction @ p, placeTooltipAt};
  None :=                       None;
  other_ :=                     failPlot["badlabelspec", other];
];

ExtendedGraphPlot::badgraphannokey = "The requested annotation `` is not present in the graph. Present annotations are: ``."

toPayloadFunction = MatchValues[
  "Name" :=               getName;
  "Index" :=              getIndex;
  "Tag" | "Cardinal" :=   getCardinal;
  assoc_Association :=    getName /* assoc;
  key_ := If[MemberQ[$annotationKeys, key],
    getAnnotation[key],
    failPlot["badgraphannokey", key, commaString @ $annotationKeys]
  ]
];

myCompactMatrixForm[vec_] :=
  CompactMatrixForm[vec, "Factor" -> False];

myCompactVectorForm[vec_] :=
  CompactMatrixForm[List @ vec,
    "Factor" -> False, "HideZeros" -> False,
    NegationStyle -> UnderBar, InversionStyle -> OverBar];

labelForm[RepresentationElement[matrix_]] :=
  myCompactMatrixForm @ matrix;

labelForm[vec_ ? RealVectorQ] :=
  myCompactVectorForm @ vec;

labelForm[matrix_ ? RealMatrixQ] :=
  myCompactMatrixForm @ matrix;

labelForm[e_] := e;

getIndex[i_] := i;
getName[i_] := Part[$labelNames, i];
getCardinal[i_] := Part[$EdgeTags, i];
getAnnotation[name_][i_] := Part[$annotations, name, i];

placeTooltipAt[label_, pos_] := NiceTooltip[{Transparent, If[$GraphIs3D, Sphere, Disk][pos, 1.1*$labeledElemSize]}, label];
placeTooltipAt[None | _Missing, _] := Nothing;

placeLabelAt[label_, pos_] := Text[
  $magnifier @ label,
  pos  + If[$GraphIs3D, 0, -$labeledElemSize * {$labelX, $labelY} * (1 + $spacings)],
  {$labelX, $labelY} * 0.95,
  Background -> $labelBackground,
  BaseStyle -> $labelBaseStyle
];

placeLabelAt[None | _Missing, _] := Nothing;

makeMagnifier[1|1.0] := Identity;
makeMagnifier[scale_] := Magnify[#, scale]&;

(**************************************************************************************************)

makeGraphics[elements_, imageSize_, padding_, plotLabel_, extraOptions_, epilog_] := Graphics[
  elements,
  Sequence @@ extraOptions,
  Frame -> None, Axes -> None,
  ImageSize -> imageSize,
  ImagePadding -> padding, PlotLabel -> plotLabel,
  AspectRatio -> Automatic, PlotRangePadding -> None,
  PreserveImageOptions -> False,
  If[epilog === None, Sequence @@ {}, Epilog -> epilog]
];

makeGraphics3D[elements_, imageSize_, padding_, plotLabel_, extraOptions_, epilog_] := Graphics3D[
  {CapForm[None], elements, Replace[epilog, None -> Nothing]},
  Sequence @@ extraOptions,
  Axes -> None, Boxed -> False,
  ImageSize -> imageSize,
  ImagePadding -> padding, PlotRange -> All, PlotRangePadding -> None,
  Lighting -> "Neutral",
  Method -> {"ShrinkWrap" -> False, "EdgeDepthOffset" -> False},
  AspectRatio -> Automatic,(* , ViewPoint -> {Infinity, 0, 0}, *)
  PreserveImageOptions -> False,
  PlotLabel -> plotLabel
];

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

(**************************************************************************************************)

PackageExport["TransformGraphPlotPrimitives"]

TransformGraphPlotPrimitives[f_, targetIds_, type_] := Block[
  {$targets = targetIds},
  $GraphPlotGraphics //= ReplaceAll[
    anno:Annotation[_, ids_, type] /; IntersectingQ[ids, $targets] :>
      transformPrimitiveAnno[anno, f]
  ];
];

transformPrimitiveAnno[Annotation[primitives_, ids_, type_], f_] := Block[
  {prim1, prim2, ids1, ids2},
  {prim1, prim2} = splitPrimitives[primitives, ids];
  ids1 = Complement[ids, $targets];
  ids2 =  Intersection[ids, $targets];
  $targets = Complement[$targets, ids];
  f @ List[
    Annotation[prim1, ids1, type],
    Annotation[prim2, ids2, type]
  ]
];

splitPrimitives[Style[prim_, style__], ids_] :=
  Map[Style[#, style]&, splitPrimitives[prim, ids]];

splitPrimitives[(head:Point|Line|Arrow)[coords_], ids_] :=
  Map[head, splitMatchingIds[coords, ids]];

splitPrimitives[Arrow[coords_, d_], ids_] :=
  Map[Arrow[#, d]&, splitMatchingIds[coords, ids]];

splitPrimitives[list_List, ids_] :=
  splitMatchingIds[coords, ids];

splitPrimitives[other_, _] := (
  Panic["UnhandledPrimitive", other];
);

splitMatchingIds[list_, ids_] := Scope[
  indices = Flatten @ Lookup[PositionIndex @ ids, $targets, {}];
  List[
    Part[list, Complement[Range @ Length @ list, indices]],
    Part[list, indices]
  ]
];
