PackageExport["ArrowheadShape"]
PackageExport["ArrowheadSize"]
PackageExport["ArrowheadStyle"]
PackageExport["ArrowheadPosition"]
PackageExport["VertexColorFunction"]
PackageExport["EdgeColorFunction"]
PackageExport["CardinalColors"]
PackageExport["VisibleCardinals"]
PackageExport["ViewRegion"]

SetUsage @ "ArrowheadShape is an extended option to Graph.";
SetUsage @ "ArrowheadSize is an extended option to Graph.";
SetUsage @ "ArrowheadStyle is an extended option to Graph.";
SetUsage @ "ArrowheadPosition is an extended option to Graph.";
SetUsage @ "VertexColorFunction is an extended option to Graph."
SetUsage @ "EdgeColorFunction is an extended option to Graph."
SetUsage @ "CardinalColors is an extended option to Graph."
SetUsage @ "VisibleCardinals is an extended option to Graph."
SetUsage @ "ViewRegion is an extended option to Graph."

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
| %Cycles[word$] | all  disjointclosed paths with given word |
| %GraphPathData[$$] | a previously computed path |
* Specifications taking a cardinal direction c$ also take a list {c$1, ..., c$n}, used cyclically.
* %Path[$$, %PathAdjustments -> {adj$1, adj$2, $$}] gives a series of adjustments to how the path is drawn.

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
| %GraphRegionData[$$] | previously computed region |
| %ConnectedSubgraph[region$] | all edges connecting vertices within region$ |

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

* For an association, the keys will be used as legend labels.

* Each highlight specification can be one of the following:
| region$ | highlight a set of vertices or edges with a unique color |
| {region$1, region$2, $$} | highlight several regions with the same color |
| %Style[spec$, style$$] | specify highlight color or other options |
| %Legended[region$, label$] | label the region as label% in a legend |
| %Arrow[spec$] | draw paths in spec$ as arrows |
| %Axis -> %%All | %InfiniteLine[%GraphOrigin, c$i] for each cardinal c$i |
| %Axis -> {c$1, $$} | %InfiniteLine[%GraphOrigin, c$i] |

* If %GraphHighlightStyle is also provided, elements of it will be applied to \
corresponding highlight specifications:
| <|key$1 -> style$1, $$|> | apply style$i to highlight specification under key$i |
| {style$1, style$2, $$} | apply style$i to highlight specification number i$ |
| style$ | apply style$ to all highlight specifications |
| {$$, opt$ -> value$} | apply additional highlight options globally |

## Highlight styles

* The following options are supported as rules in %Style[spec$, $$] wrapper in a highlight \
specifications, and in %GraphHighlightStyle:

| %SimplifyRegions | True | whether to render disk-like regions with %Disk |
| %PerformanceGoal | 'Quality' | how to prioritize region rendering |
| %PathStyle | 'Line' | how to render paths |
| %ArrowheadPosition | 1.0 | where to place arrowhead on a path |
| %ArrowheadSize | Automatic | arrowhead size |
| %PointSize | Automatic | vertex highlighting radius |
| %HighlightRadius | Automatic | region highlighting radius |
| %PathRadius | Automatic | path highlighting radius |
| %PathOutline | False | whether to outline path highlight |
| %EdgeSetback | 1 | scaled amount by which to set back edges from target |
| %ZOrder | 1 | control order of rendering of highlighted elements |

* %PathStyle controls how paths are highlighted:
| 'Line' | thick overlaid lines |
| 'Arrow' | thick overlaid arrows |
| 'Replace' | replace original edges with new style |
| 'ReplaceEdges' | replaces edges, but preserve original arrowheads |
* 'DiskArrow', 'ArrowDisk', and 'DiskArrowDisk' will begin and/or end the arrow with \
an enlarged disk.

* The default opacity of highlight elements is 0.5, but if style colors are given \
with an explicit opacity this opacity will be used instead.

The following special named style elements control several settings:
| 'Background' | solid elements placed behind target graph |
| 'Foreground' | solid elements placed above target graph |
| 'FadeGraph' | make non-highlighted graph light gray |
| 'FadeEdges' | make non-highlighted edges light gray |
| 'FadeVertices' | make non-highlighted vertices light gray |
| 'HideArrowheads' | hide non-highlighted arrowheads |
| 'HideEdges' | hide non-highlighted edges |
| 'HideVertices' | hide non-highlighted vertices |
| 'Replace' | use %PathStyle -> 'Replace' with solid colors |

* The function %FadeProtected can be used to protect certain primitives from the effects of 'FadeGraph'.
%FadeProtected is typically used as a second argument to %GraphicsValue in an %Epilog or %Prolog.

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

    {$VertexCoordinates, $EdgeCoordinateLists} = ExtractGraphPrimitiveCoordinates @ $IndexGraph;

    viewRegion = LookupExtendedGraphAnnotations[$Graph, ViewRegion];
    If[viewRegion =!= All, applyViewRegion[viewRegion], $VertexParts = $EdgeParts = All];

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

ExtendedGraphPlot::badviewregion = "ViewRegion -> `` is invalid and will be ignored.";

applyViewRegion[regionSpec_] := (
  {$VertexParts, $EdgeParts} = processRegionVerticesEdges @ regionSpec;
  If[FailureQ[$VertexParts],
    $VertexParts = $EdgeParts = All;
    failPlot["badviewregion", regionSpec]
  ];
)

$rangeMicroPadding = 1*^-12;
computeCoordinateBounds[] := Scope[
  plotRange = LookupOption[$Graph, PlotRange];
  If[MatrixQ[plotRange, NumericQ], Return @ plotRange];
  CoordinateBounds[{
    Part[$VertexCoordinates, $VertexParts],
    Replace[Part[$EdgeCoordinateLists, $EdgeParts], {} -> Nothing]},
    $rangeMicroPadding
  ]
];

(**************************************************************************************************)

PackageExport["ExtendedGraphPlot"]

$autoFilledLegendPattern = (Automatic | _String) | Placed[Automatic | _String, _];

ExtendedGraphPlot[graph_] := Block[
  {
   $GraphPlotImageSize, $GraphPlotImageWidth, $GraphPlotRange, $GraphPlotSize, $GraphMaxSafeVertexSize,
   plottingFunction, graphLegend, graphRegionHighlight, vertexColorFunction,
   highlightGraphics, requiredPadding, graphLabel = None
  },

  If[!GraphQ[graph], Return[$Failed, Block]];

  {plottingFunction, graphLegend, graphRegionHighlight, vertexColorFunction} =
    LookupAnnotation[graph, {GraphPlottingFunction, GraphLegend, GraphRegionHighlight, VertexColorFunction}, None];

  SetNone[plottingFunction,
    If[vertexColorFunction === None, GraphComputation`GraphDrawing, ExtendedGraphPlottingFunction]];

  SetAutomatic[plottingFunction, ExtendedGraphPlottingFunction];

  GraphPlotScope[graph,

    $GraphPlotGraphics = plottingFunction[$Graph];

    If[FailureQ[$GraphPlotGraphics], Return[$Failed, Block]];

    If[MatchQ[$GraphPlotGraphics, _Labeled],
      graphLabel = ReplacePart[$GraphPlotGraphics, 1 -> None];
      $GraphPlotGraphics //= First;
    ];

    If[MatchQ[$GraphPlotGraphics, _Legended],
      If[MatchQ[graphLegend, None | $autoFilledLegendPattern | {$autoFilledLegendPattern..}],
        graphLegend = Last @ $GraphPlotGraphics];
      $GraphPlotGraphics //= First;
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

    If[graphLabel =!= None, $GraphPlotGraphics = ReplacePart[graphLabel, 1 -> $GraphPlotGraphics]];

    ApplyLegend[ApplyFinalTransforms @ $GraphPlotGraphics, graphLegend]
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
    vertexCoords = Part[$VertexCoordinates, $VertexParts];
    viewOptions = Association[PlotRange -> CoordinateBounds[vertexCoords], viewOptions];
    viewTransform = ConstructGraphicsViewTransform[viewOptions];
    vertexCoordinates = viewTransform @ vertexCoords;
    {width, height} = PlotRangeSize @ CoordinateBounds[vertexCoordinates];
    height / width
  ,
    $GraphPlotSizeY / $GraphPlotSizeX
  ]
];

computeMaxSafeVertexSize[] := Scope[
  minDistance = getRankedMinDistance @ Part[$VertexCoordinates, $VertexParts];
  Max[Min[minDistance / 2, Max[$GraphPlotSize] / 3], $MachineEpsilon]
];

computeMaxSafeArrowheadSize[] := Scope[
  minDistance = getRankedMinDistance[lineCenter /@ Part[$EdgeCoordinateLists, $EdgeParts]];
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
ExtendedGraphPlot::badpadding = "Padding option `` was invalid."
ExtendedGraphPlot::badthickness = "EdgeThickness -> `` was invalid."

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
     vertexShapeFunction, edgeShapeFunction, frameStyle, baselinePosition} =
      LookupOption[graph, {GraphLayout,
        ImageSize, VertexSize, VertexStyle, EdgeStyle, VertexLabelStyle, EdgeLabelStyle,
        VertexShapeFunction, EdgeShapeFunction, FrameStyle, BaselinePosition}, Automatic];

    {vertexLabels, edgeLabels, plotLabel, prolog, epilog, imagePadding, plotRangePadding, frame, frameLabel} =
      LookupOption[graph, {VertexLabels, EdgeLabels, PlotLabel, Prolog, Epilog, ImagePadding, PlotRangePadding, Frame, FrameLabel}, None];

    {arrowheadShape, arrowheadStyle, arrowheadSize, arrowheadPosition, visibleCardinals, edgeSetback, edgeThickness, labelCardinals, viewOptions, additionalImagePadding} =
      LookupExtendedGraphAnnotations[graph, {ArrowheadShape, ArrowheadStyle, ArrowheadSize, ArrowheadPosition, VisibleCardinals, EdgeSetback, EdgeThickness, LabelCardinals, ViewOptions, AdditionalImagePadding}];

    {graphLegend, vertexColorFunction, vertexAnnotations, edgeColorFunction, colorRules} =
      LookupExtendedGraphAnnotations[graph, {GraphLegend, VertexColorFunction, VertexAnnotations, EdgeColorFunction, ColorRules}];
  ];

  (* initial processing of global options *)
  FunctionSection[
    cardinalColors = LookupCardinalColors[graph];
    If[!MatchQ[cardinalColors, None | _Association], failPlot["badcolors"]];

    SetNone[vertexAnnotations, <||>];
    SetNone[epilog, {}];
    SetNone[prolog, {}];

    $GraphicsBoundingFrame := $GraphicsBoundingFrame = computeBoundingFrame[];

    automaticLegends = <||>;

    (* choose a size based on vertex seperation *)
    SetAutomatic[imageSize,
      If[$VertexParts === All && Length[$VertexList] > 1000, Large,
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

    SetAll[imagePadding, 1];
    imagePadding = Replace[
      StandardizePadding @ imagePadding,
      Except[_ ? MatrixQ] :> failPlot["badpadding", ImagePadding -> imagePadding]
    ];
    imagePadding //= ReplaceAll[0 -> 1];

    additionalImagePadding = Replace[
      StandardizePadding @ additionalImagePadding,
      Except[_ ? MatrixQ] :> failPlot["badpadding", AdditionalImagePadding -> additionalImagePadding]
    ];

    edgeCenters = lineCenter /@ $EdgeCoordinateLists;

    edgeStyle //= removeSingleton;
    vertexStyle //= removeSingleton;
    vertexLabelStyle //= removeSingleton;
    edgeLabelStyle //= removeSingleton;
    edgeShapeFunction //= removeSingleton;
    $esfCounter = 1;

    If[colorRules =!= None, processColorRules[colorRules]];
  ];

  (* create graphics for vertices *)
  FunctionSection[

    $vertexSizeOverrides = None;
    $defaultVertexSize = Which[
      edgeStyle === None,           Scaled @ 0.6,
      vertexColorFunction =!= None, Scaled @ 0.5,
      True,                         Max[0.3, AbsolutePointSize[6]]
    ];
    $inheritedVertexSize = False;
    vertexSize = processVertexSize @ removeSingleton @ vertexSize;
    vertexSizeImage = plotSizeToImageSize @ vertexSize;

    SetAutomatic[vertexShapeFunction, If[vertexColorFunction =!= None, "Disk", "Point"]];

    lighting = None;

    vertexShapeFunction //= removeSingleton;
    If[vertexShapeFunction === None,
      vertexGraphics = Nothing; edgeSetback = 0;
      Goto[skipVertices];
    ];

    {defaultVertexColor, vertexBaseStyle, setbackDistance, rawVertexDrawFunc, vertexPadding} =
      processVertexShapeFunction[vertexShapeFunction];
    SetAutomatic[edgeSetback, setbackDistance];
    extendPadding[vertexPadding];

    vertexDrawFunc = If[$vertexSizeOverrides === None,
      vertexDrawFuncWithSize[rawVertexDrawFunc, vertexSize],
      vertexDrawFuncSizeRemapper[rawVertexDrawFunc, $vertexSizeOverrides, vertexSize]
    ];

    SetAutomatic[vertexStyle, defaultVertexColor];

    vertexItems = drawViaColorFunc[
      vertexColorFunction, vertexDrawFunc, $VertexCount, $VertexParts,
      vertexColorDataProvider, VertexColorFunction
    ];

    vertexBaseStyle = toDirective[{vertexBaseStyle, vertexStyle}];

    maxMertexSize = Max @ vertexSize;
    vertexInfo = <|
      VertexSize -> toSizeInfo[maxMertexSize]
    |>;

    vertexGraphics = Annotation[
      Style[vertexItems, vertexBaseStyle],
      vertexInfo, "VertexPrimitivesRoot"
    ];
  ];
  Label[skipVertices];

  (* create graphics for edges *)
  FunctionSection[
    If[edgeStyle === None,
      edgeGraphics = Nothing;
      Goto[skipEdges];
    ];

    SetAutomatic[edgeStyle, GrayLevel[0, .18]];
    edgeStyle //= toDirectiveOptScan[setEdgeStyleGlobals];

    If[arrowheadShape === None || zeroQ[arrowheadSize] || UndirectedGraphQ[$Graph] || MatchQ[visibleCardinals, {} | None],
      arrowheadDrawFn = drawUndirectedEdges;
      maxArrowheadSize = 0;
    ,
      SetAutomatic[arrowheadStyle, Which[
        cardinalColors =!= None, cardinalColors,
        vertexColorFunction =!= None, LightGray,
        True, Gray
      ]];
      baseArrowheadSize := baseArrowheadSize = If[$GraphIs3D, 0.45, 0.8] * ($GraphMaxSafeArrowheadSize / $GraphPlotSizeX);
      arrowheadSize //= processArrowheadSize;
      maxArrowheadSize = Max[arrowheadSize * $GraphPlotSizeX] / 2;

      SetAutomatic[arrowheadShape, If[$GraphIs3D, "Cone", "Arrow"]];
      $twoWayStyle = Automatic; $pairedDistance = 0.; $negationStyle = "Reverse";
      $lineThickness = If[$GraphIs3D, Thickness @ 0.2, AbsoluteThickness @ 1.2];
      If[ListQ[arrowheadShape],
        {arrowheadShape, arrowheadShapeOpts} = FirstRest @ arrowheadShape;
        Scan[scanArrowheadShapeOpts, arrowheadShapeOpts]];

      SetAutomatic[arrowheadPosition, If[$GraphIs3D && arrowheadShape =!= "Cone", 0.65, 0.502]];

      arrowheadBounds = CoordinateBounds[
        Part[edgeCenters, $EdgeParts],
        maxArrowheadSize
      ];
      extendPaddingToInclude[arrowheadBounds];

      arrowheadDrawFn = drawTagGroupArrowheadEdges;
    ];

    edgeItems = drawViaColorFunc[
      edgeColorFunction, arrowheadDrawFn, $EdgeCount, $EdgeParts,
      edgeColorDataProvider, EdgeColorFunction
    ];

    SetAutomatic[edgeThickness, If[$GraphIs3D, MediumThick, SlightlyThick]];
    edgeThickness = Replace[
      NormalizeThickness @ edgeThickness,
      $Failed :> failPlot["badthickness", edgeThickness]
    ];

    edgeInfo = <|
      EdgeThickness -> edgeThickness, EdgeStyle -> edgeStyle,
      ArrowheadSize -> toSizeInfo[maxArrowheadSize],
      ArrowheadPosition -> arrowheadPosition
    |>;

    edgeGraphics = Annotation[
      Style[edgeItems, edgeStyle, edgeThickness],
      edgeInfo, "EdgePrimitivesRoot"
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
      {vertexLabelItems, zorder} = generateLabelPrimitives[
        vertexLabels, $VertexList, $VertexCoordinates, $VertexParts,
        vertexSize, vertexLabelStyle, vertexAnnotations, True
      ];
      Which[
        zorder == 0, AppendTo[labelGraphics, vertexLabelItems],
        Positive @ zorder, AppendTo[epilog, vertexLabelItems],
        Negative @ zorder, AppendTo[prolog, vertexLabelItems]
      ];
    ];

    edgeLabels //= removeSingleton;
    If[edgeLabels =!= None,
      SetAutomatic[arrowheadSize, 0];
      {edgeLabelItems, zorder} = generateLabelPrimitives[
        edgeLabels, $EdgeList, edgeCenters, $EdgeParts,
        Max[arrowheadSize] * $GraphPlotSizeX, edgeLabelStyle, <||>, False
      ];
      Which[
        zorder == 0, AppendTo[labelGraphics, edgeLabelItems],
        Positive @ zorder, AppendTo[epilog, edgeLabelItems],
        Negative @ zorder, AppendTo[prolog, edgeLabelItems]
      ];
    ];

    labelGraphics = If[labelGraphics === {}, Nothing,
      Annotation[labelGraphics, "LabelPrimitivesRoot"]
    ];
  ];

  (* create the final graphics *)
  FunctionSection[
    If[labelGraphics =!= Nothing, extendPadding @ estimateLabelPadding[labelGraphics, vertexLabelStyle]];

    (* for graphs with cardinals, create an automatic legend when asked *)
    If[cardinalColors =!= None && arrowheadStyle =!= None && arrowheadShape =!= None,
      legendCardinals = If[ListQ[visibleCardinals], KeyTake[cardinalColors, visibleCardinals], cardinalColors];
      automaticLegends["Cardinals"] := ArrowheadLegend[cardinalColors, arrowheadShape];
    ];

    If[colorRules =!= None,
      KeyDropFrom[automaticLegends, "Colors"]];

    interiorImageSize = imageSize;
    imagePadding += additionalImagePadding;
    imagePadding //= Ceiling;
    imageSize = Ceiling @ ImageSizePad[imageSize, imagePadding];

    graphicsElements = {edgeGraphics, vertexGraphics, labelGraphics};

    If[TrueQ @ frame,
      If[MatchQ[frameStyle, {} | Automatic], frameStyle = LightGray];
      frameStyle //= toDirective;
      If[$GraphIs3D,
        cuboid = Cuboid @@ $GraphicsBoundingFrame;
        AppendTo[epilog, Annotation[Style[cuboid, FaceForm @ None, EdgeForm @ frameStyle], "Frame"]];
      ,
        rectangle = Rectangle @@ $GraphicsBoundingFrame;
        AppendTo[epilog, Annotation[Style[rectangle, FaceForm @ None, EdgeForm @ frameStyle], "Frame"]];
        AppendTo[prolog, Annotation[Style[rectangle, FaceForm @ White, EdgeForm @ None], "FrameBackground"]];
      ];
    ];

    externalLabel = None;
    If[frameLabel =!= None,
      If[$GraphIs3D,
        externalLabel = processFrameLabel3D @ frameLabel;
      ,
        frameLabelElements = processFrameLabel @ frameLabel;
        prolog = ToList[prolog, frameLabelElements];
      ];
    ];

    If[ContainsQ[graphicsElements, _UniqueLabel],
      graphicsElements //= processUniqueLabels];

    extraOptions = If[!$GraphIs3D, {},
      SetAutomatic[viewOptions, $automaticViewOptions];
      viewOptions
    ];

    baselinePosition //= processBaselinePosition;

    (* assemble graphics *)
    graphics = If[$GraphIs3D, makeGraphics3D, makeGraphics][
      graphicsElements,
      imageSize, imagePadding, plotRangePadding, plotLabel, extraOptions,
      Replace[prolog, {} -> None], Replace[epilog, {} -> None],
      baselinePosition
    ];

    If[ContainsQ[graphics, _GraphicsValue], Block[{$GraphPlotGraphics = graphics},
      graphics //= ReplaceAll[{
        gv_GraphicsValue :> RuleCondition[evalGraphicsValue @ gv],
        cc_CardinalColor :> evalCardinalColor @ cc
      }];
    ]];

    (* obtain final plotrange *)
    plotRange = $GraphPlotRange;

    (* plotRange += {{-1, 1}, {-1, 1}} * $rangeMicroPadding; *)
    AppendTo[graphics, PlotRange -> plotRange];

    applyExternalLabel[
      applyAutomaticLegends[graphics, automaticLegends, graphLegend],
      externalLabel
    ]
  ]
,
  ExtendedGraphPlottingFunction
];

applyExternalLabel[graphics_, None] := graphics;

applyExternalLabel[graphics_, labeled_Labeled] :=
  ReplacePart[labeled, 1 -> graphics];

(* in plot range *)
toSizeInfo[sz_] := <|
  "PlotRange" -> sz,
  "ImageSize" -> sz * effectiveImageWidth,
  "Fraction" -> sz / $GraphPlotSizeX
|>;

processFrameLabel3D = MatchValues[
  label:Rule[Bottom|Top, _] :=
    % @ List @ label;
  rules:{Rule[Bottom|Top, _]...} := Scope[
    {pos, labels} = KeysValues @ rules;
    Labeled[None, labels, pos]
  ];
  None :=
    None;
  label_ :=
    % @ {Bottom -> label};
];

processFrameLabel = MatchValues[
  label:Rule[Bottom|Top, _] :=
    % @ List @ label;
  rules:{Rule[Bottom|Top, _]...} := Scope[
    {bLabel, tLabel} = Lookup[rules, {Bottom, Top}, None];
    labels = List[
      makeFrameLabelElement[bLabel, Bottom, .85],
      makeFrameLabelElement[tLabel, Top, -.9]
    ];
    labelHeights = Last /@ frameLabelSize /@ labels;
    (* at this point padding has already been done, so we have to update all 3 *)
    padSize = {{0, 0}, labelHeights};
    imageSize += Total /@ padSize;
    imagePadding += padSize;
    additionalImagePadding += padSize;
    Annotation[List[GrayLevel[0, 1], DeleteNone @ labels], "FrameLabel"]
  ];
  None :=
    Nothing;
  label_ :=
    % @ {Bottom -> label};
];

frameLabelSize[None] := {0, 0};
frameLabelSize[label_] := 1 + cachedRasterizeSize[Text @ label] / 2;

makeFrameLabelElement[None, _, _] := None;

makeFrameLabelElement[label_, pos1_, pos2_] :=
  Text[
    LabelForm[label],
    GraphicsValue[{"BoundingFrame", pos1}],
    {0, pos2}, Background -> White
  ];

processBaselinePosition = MatchValues[
  Automatic | GraphOrigin :=
    % @ If[$GraphOrigin =!= None, $GraphOrigin, Center];
  Center  := % @ Scaled[0.5, "Interior"];
  Bottom  := % @ Scaled[0.0, "Interior"];
  Top     := % @ Scaled[1.0, "Interior"];
  "Coordinate" -> (yPos_ ? NumericQ) := Scope[
    yOffset = Part[$GraphPlotRange, 2, 1];
    s = (yPos - yOffset) / $GraphPlotSizeY;
    % @ Scaled[s, "Interior"]
  ];
  Scaled[s_ ? NumericQ, "Interior"] := Scope[
    padOffset = Part[imagePadding, 2, 1];
    interiorImageHeight = Last @ interiorImageSize;
    imageHeight = Last @ imageSize;
    Scaled[Clip[((s * interiorImageHeight) + padOffset) / imageHeight, {0, 1}]]
  ];
  s:Scaled[_ ? NumericQ] := s;
  vertex_ := Scope[
    index = getVertexIndex @ vertex;
    yPos = Part[$VertexCoordinates, index, 2];
    yOffset = Part[$GraphPlotRange, 2, 1];
    s = (yPos - yOffset) / $GraphPlotSizeY;
    % @ Scaled[s, "Interior"]
  ];
];

computeBoundingFrame[] := Scope[
  (* additional image padding goes outside the frame *)
  padding = imagePadding - additionalImagePadding - 1;
  {{l, r}, {b, t}} = padding / effectiveImageWidth * $GraphPlotSizeX;
  If[$GraphIs3D,
    {{xl, xh}, {yl, yh}, {zl, zh}} = $GraphPlotRange;
    {{xl - l, yl - b, zl}, {xh + r, yh + t, zh}}
  ,
    {{xl, xh}, {yl, yh}} = $GraphPlotRange;
    {{xl - l, yl - b}, {xh + r, yh + t}}
  ]
]

filterAssocIndices[All][assoc_] :=
  assoc;

filterAssocIndices[parts_][assoc_] :=
  DeleteCases[Map[Intersection[#, parts]&, assoc], {}];

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

(**************************************************************************************************)

PackageExport["CardinalColor"]

SetUsage @ "CardinalColor[c$] represents the color of cardinal c$."

PackageScope["evalCardinalColor"]

evalCardinalColor[CardinalColor[c_]] :=
  LookupCardinalColors[$Graph, If[ListQ[c], CardinalSet @ DeleteDuplicates @ Map[StripNegated, c], c]];

(**************************************************************************************************)

PackageScope["evalGraphicsValue"]
PackageExport["GraphicsValue"]

SetUsage @ "
GraphicsValue[spec$] represents a computed value for the current graphic.
* GraphicsValue[spec$] will be replaced when present in an Epilog, Prolog, or any other graphics element.
* GraphicsValue[spec$, f$] will apply f$ to the value of spec$.
* The following specifications are supported:
| 'PlotRange' | plot range |
| 'BoundingRectangle' | rectangle bounding the plot range |
| 'BoundingFrame' | bounding frame, which includes effect of %ImagePadding |
| {'BoundingFrame', Left|Right|Bottom|Top} | center of bounding frame on given side |
| {'VertexCoordinates', spec$} | coordinates of vertex or region spec$ |
| {'VertexPrimitives', spec$} | graphics primitives used for vertex or region spec$ |
| {'EdgeCoordinates', spec$} | coordinate of edge or region spec$ |
| {'EdgePrimitives', spec$} | graphics primitives used for edge or region spec$ |
| {'CardinalPrimitives', spec$, c$} | primitives for arrowhead of cardinal c$ on given edges |
| 'ArrowheadSize' | size of arrowheads, in fraction of plot width |
| 'VertexSize' | size of vertices, in fraction of plot width |
| 'PrimitiveSize' | maximum of 'ArrowheadSize' and 'VertexSize' |
* spec$ can also be All, in which case all coordinates or primitives are returned.
"

$validGVSpecs = {
  "PlotRange", "BoundingRectangle", "BoundingFrame",
  "VertexCoordinates", "VertexPrimitives",
  "EdgeCoordinates", "EdgePrimitives", "CardinalPrimitives", "CardinalGraphics"
};

evalGraphicsValue = MatchValues[
  GraphicsValue["PlotRange"] :=
    $GraphPlotRange;
  GraphicsValue["BoundingRectangle"] :=
    Transpose @ $GraphPlotRange;
  GraphicsValue["BoundingFrame"] :=
    $GraphicsBoundingFrame;
  GraphicsValue[{"BoundingFrame", side:Bottom|Top|Left|Right}] :=
    computeCenterPoint[$GraphicsBoundingFrame, side];
  GraphicsValue[{"VertexCoordinates", vertex_}] :=
    Part[$VertexCoordinates, findVertexList @ vertex] //
      postProcPrims[vertex];
  GraphicsValue[{"VertexPrimitives", vertex_}] :=
    ExtractGraphPlotPrimitives[findVertexList @ vertex, "VertexPrimitives"] //
      postProcPrims[vertex];
  GraphicsValue[{"EdgeCoordinates", edge_}] :=
    Part[$edgeCoordinateLists, findEdgeList @ edge] //
      postProcPrims[edge];
  GraphicsValue[{"EdgePrimitives", edge_}] :=
    ExtractGraphPlotPrimitives[findEdgeList @ edge, "EdgePrimitives"] //
      postProcPrims[edge];
  GraphicsValue[{"CardinalPrimitives", edge_, c_}] :=
    extractCardinalArrowhead[c] @ % @ GraphicsValue[{"EdgePrimitives", edge}];
  GraphicsValue[{"CardinalGraphics", c_}] :=
    FirstCase[$GraphPlotGraphics, Graphics[Annotation[_, c, "Cardinal"], ___], None, {1, Infinity}];
  GraphicsValue["EdgeThickness"] :=
    lookupPrimitiveAnnotationData["EdgePrimitivesRoot", EdgeThickness, 0];
  GraphicsValue["ArrowheadSize"] :=
    lookupPrimitiveAnnotationData["EdgePrimitivesRoot", ArrowheadSize, $emptySizeInfo];
  GraphicsValue["VertexSize"] :=
    lookupPrimitiveAnnotationData["VertexPrimitivesRoot", VertexSize, $emptySizeInfo];
  GraphicsValue["PrimitiveSize"] :=
    MapThread[Max, %[GraphicsValue[#]]& /@ {"ArrowheadSize", "VertexSize"}];
  GraphicsValue[spec_, f_] :=
    applyGVFunc[f, % @ GraphicsValue[spec]];
  gv_GraphicsValue := failPlot["badgraphicsvalue", gv, commaString @ $validGVSpecs]
];

lookupPrimitiveAnnotationData[type_, field_, default_] :=
  FirstCase[
    $GraphPlotGraphics,
    Annotation[_, assoc_, type] :> Lookup[assoc, field, default],
    default, {0, Infinity}
  ];

$emptySizeInfo = <|"PlotRange" -> 0, "ImageSize" -> 0, "Fraction" -> 0|>;

findVertexList = MatchValues[
  All := Range @ $VertexCount;
  spec_ := ToList @ findVertex @ spec
];

findEdgeList = MatchValues[
  All := Range @ $EdgeCount;
  spec_ := ToList @ findEdge @ spec
];

applyGVFunc[s_String, a_] := Lookup[a, s];
applyGVFunc[i_Integer, a_] := Part[a, i];
applyGVFunc[f_, a_] := f[a];

postProcPrims[All][vals_] := vals;

postProcPrims[spec_ ? GraphRegionElementQ][vals_] :=
  If[vals === {}, failPlot["gvnoprim", spec], vals];

postProcPrims[spec_][vals_] :=
  First[vals, failPlot["gvnoprim", spec]];

extractCardinalArrowhead[c_][prims_] := ReplaceAll[
  Style[prims, Transparent],
  Annotation[g_, Except[c | Negated[c]], "Cardinal"] :> {}
]

computeCenterPoint[{{xl_, yl_}, {xh_, yh_}}, side_] := Scope[
  xm = (xl + xh)/2; ym = (yl + yh) / 2;
  Match[side,
    Bottom  :> {xm, yl},
    Top     :> {xm, yh},
    Left    :> {xl, ym},
    Right   :> {xh, ym}
  ]
];

computeCenterPoint[{{xl_, yl_, zl_}, {xh_, yh_, zh_}}, side_] :=
  (Insert[
    computeCenterPoint[{{xl, zl}, {xh, zh}}, side],
    yl, 2
  ]; {1, 0, 0})

ExtendedGraphPlot::gvnoprim = "Could not obtain graphics primitives for ``."
ExtendedGraphPlot::badgraphicsvalue = "`` is malformed. Valid specs include ``."

makeGraphicsGroup[g_] := g;

zeroQ[0|0.] := True;
zeroQ[_] := False;

(**************************************************************************************************)

PackageExport["FadeProtected"]

SetUsage @ "
FadeProtected[primitives$] indicates that the given graphics primitives should be protected \
from the effects of the 'FadeGraph' directive in GraphRegionHighlight.
"

FadeProtected[primitives_] := Annotation[primitives, "Protected"]

(**************************************************************************************************)

processUniqueLabels[graphics_] := Scope[
  uniqueLabelMap = <||>;
  DeepCases[graphics, Text[t_, pos_, opts___] :> DeepCases[t,
    UniqueLabel[id_] :> KeyAppendTo[uniqueLabelMap, id, yxCoord @ pos]
  ]];
  uniqueLabelMap //= Map[Minimum];
  orderingMap = AssociationThread[Keys @ uniqueLabelMap, Ordering @ Ordering @ uniqueLabelMap];
  graphics /. UniqueLabel[id_] :> RuleCondition @ orderingMap @ id
];

yxCoord[{x_, y_}] := {-y, x};

(**************************************************************************************************)

vertexDrawFuncWithSize[rawVertexDrawFunc_, vertexSize_] :=
  ReplaceAll[
    {indices, color} |-> Annotation[
      rawVertexDrawFunc[Part[$VertexCoordinates, indices], color],
      indices, "VertexPrimitives"
    ],
    $vertexSize -> vertexSize
  ];

vertexDrawFuncSizeRemapper[rawVertexDrawFunc_, sizeOverrides_, vertexSize_] :=
  {indices, color} |-> Annotation[
    Map[
      index |-> Construct[
        ReplaceAll[
          rawVertexDrawFunc,
          $vertexSize -> Lookup[sizeOverrides, index, vertexSize]
        ],
        Part[$VertexCoordinates, index], color
      ],
      indices],
    indices, "VertexPrimitives"
  ];

(**************************************************************************************************)

processColorRules[rules_] := (
  {vertexColorFunction, edgeColorFunction} = resolveRegionRules[rules, ColorRules];
  If[!ColorVectorQ[vertexColorFunction] || !ColorVectorQ[edgeColorFunction],
    failPlot[GraphHighlight, "badcolvals"]];
  SetAutomatic[arrowheadStyle, Inherited];
);

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

drawUndirectedEdges[indices_, style_] :=
  Style[createEdgePrimitives[indices, Line, None, None], style];

filterCardinals[cards_][CardinalSet[set_List]] := CardinalSet @ Select[set, MemberQ[cards, Negated[#] | #]&];
filterCardinals[cards_][card_] := If[MemberQ[cards, card], card, Null];

drawTagGroupArrowheadEdges[indices_, style_] := Scope[

  filter = If[visibleCardinals === All, Identity, filterCardinals[ToList @ visibleCardinals]];

  edgeTagGroups = If[$EdgeTags === None,
    <|All -> indices|>,
    Map[
      Part[indices, #]&,
      PositionIndex @ Map[filter, Part[$EdgeTags, indices]]
    ]
  ];

  edgeTagGroups //= filterAssocIndices[$EdgeParts];

  edgeStyle = style;
  edgePrimitives = KeyValueMap[drawArrowheadEdges, edgeTagGroups];

  Style[edgePrimitives, style]
];

drawArrowheadEdges[_, {}] := Nothing;

undirectedCardinalQ[CardinalSet[{}] | Null] := True;
undirectedCardinalQ[cardinal:Except[_CardinalSet]] := Or[
  lookupTagSpec[arrowheadShape, cardinal] === None,
  zeroQ @ lookupTagSpec[arrowheadSize, cardinal]
];

drawArrowheadEdges[cardinal_, indices_] /; undirectedCardinalQ[cardinal] :=
  createEdgePrimitives[indices, Line, None, cardinal];

drawArrowheadEdges[cardinal_, indices_] := Scope[
  arrowheads = arrowheadsND @ List @ makeArrowheadsElement @ cardinal;
  createEdgePrimitives[indices, Arrow, arrowheads, cardinal]
];

isPairedShapeQ[shape_String] := StringStartsQ[arrowheadShape, "Paired"];

drawArrowheadEdges[cs:CardinalSet[cardinals_], indices_] /; isPairedShapeQ[arrowheadShape] := Scope[
  shape1 = "Left" <> arrowheadShape; shape2 = "Right" <> arrowheadShape;
  arrowheads = makePairedDiskArrowheads[cardinals, $pairedDistance / 20];
  Part[arrowheads, All, 2] = makeMultiarrowheadPositions[Length[cardinals] / 2, .5];
  createEdgePrimitives[indices, Arrow, Arrowheads @ arrowheads, cs]
];

makePairedDiskArrowheads[{c1_, c2_, rest___}, pdist_] := Scope[
  {shape, size, style1, arrowheadPosition} = lookupTagSpec @ StripNegated @ c1;
  {shape, size, style2, arrowheadPosition} = lookupTagSpec @ StripNegated @ c2;
  shape1o = shape1; shape2o = shape2; negation = Nothing;
  isNegated = OddQ[Count[{c1, c2}, _Negated]];
  If[isNegated && $negationStyle === "Reverse",
      Swap[style1, style2];
      shape1o = shape1o <> "Offset"; shape2o = shape2o <> "Offset"];
  graphic = Graphics @ {Opacity[1],
    {style1, $arrowheads2D @ shape1o},
    {style2, $arrowheads2D @ shape2o}} /. $pairDist -> pdist;
  arrowheadSpec = {size, Null, graphic};
  If[isNegated && MatchQ[$negationStyle, "OverBar" | "UnderBar"],
    arrowheadSpec //= TransformArrowheads[$negationStyle]];
  Prepend[arrowheadSpec] @ makePairedDiskArrowheads[{rest}, pdist]
];

makePairedDiskArrowheads[{}, _] := {};

drawArrowheadEdges[CardinalSet[{c_}], indices_] :=
  drawArrowheadEdges[c, indices];

drawArrowheadEdges[cs:CardinalSet[cardinals_], indices_] := Scope[
  If[$twoWayStyle === Automatic, $twoWayStyle ^= arrowheadShape <> "DoubleIn"];
  If[$twoWayStyle =!= None,
    cardinals = SortBy[cardinals, NegatedQ];
    cardinals //= ReplaceRepeated[{l___, c_, m___, Negated[c_], r___} :> {l, TwoWay[c], m, r}]];
  cardinals = SortBy[cardinals, {Head[#] === TwoWay, StripNegated @ #}&];
  num = Length[cardinals];
  positions = makeMultiarrowheadPositions[num, arrowheadPosition];
  arrowheads = arrowheadsND @ MapThread[
    {card, pos} |-> makeArrowheadsElement[arrowheadPosition = pos; card],
    {cardinals, positions}
  ];
  createEdgePrimitives[indices, multiArrow, arrowheads, cs]
];

makeMultiarrowheadPositions[1, pos_] := {pos};
makeMultiarrowheadPositions[num_, _] := 0.5 + 0.2 * Standardize[Range @ num];

PackageExport["TwoWayStyle"]
PackageExport["PairedDistance"]

scanArrowheadShapeOpts = MatchValues[
  TwoWayStyle -> s:("Out"|"In"|"OutClose"|"InClose") := $twoWayStyle ^= arrowheadShape <> "Double" <> s;
  TwoWayStyle -> s:("Square"|"Ball"|"Disk"|"Diamond"|"CrossLine"|"CrossBar"|"Tube"|None) := $twoWayStyle ^= s;
  EdgeThickness -> thickness_ := $lineThickness ^=  Replace[
    NormalizeThickness @ thickness,
    $Failed :> failPlot["badthickness", thickness]
  ];
  PairedDistance -> n_ ? NumericQ := $pairedDistance ^= N[n];
  NegationStyle -> s:("Reverse"|"OverBar"|"UnderBar") := $negationStyle ^= s;
  rule_ := failPlot["badsubopt", rule, commaString @ {TwoWayStyle, PairedDistance, NegationStyle, EdgeThickness}];
];

(**************************************************************************************************)

PackageExport["CardinalTransition"]

SetUsage @ "
CardinalTransition[a$, b$] represents a transition from cardinal a$ to cardinal b$.
"

declareFormatting[
  ca:CardinalTransition[_, _] :> formatCardinalTransition[ca]
]

formatCardinalTransition[CardinalTransition[a_, b_]] :=
  Row[formatCardinal /@ {"a", "b"}, Style["\[RightArrow]", Gray]]

formatCardinal[c_] := If[!GraphQ[$Graph], c,
  Style[c, LookupCardinalColors[$Graph, c]]
];

(**************************************************************************************************)

drawArrowheadEdges[cs:CardinalSet[trans:{Repeated[_CardinalTransition | Negated[_CardinalTransition]]}], indices_] := Scope[
  {backward, forward} = SelectDiscard[trans, NegatedQ];
  arrowheads = MapThread[makeTransitionArrowhead, {{forward, StripNegated /@ backward}, {True, False}}];
  createEdgePrimitives[indices, Arrow, Arrowheads @ arrowheads, cs]
];

makeTransitionArrowhead[transitions_, isForward_] := Scope[
  {t, size, t, arrowheadPosition} = lookupTagSpec @ StripNegated @ Part[transitions, 1, 1];
  label = Row[formatCardinalTransition /@ transitions, " "];
  label = makeArrowheadLabel[label, arrowheadSize];
  {arrowheadPosition, size, Graphics[{Opacity[1], style, Text[label, {0, 0}, {0, -1}, If[isForward, {1, 0}, {-1, 0}]]}]}
];

(**************************************************************************************************)

createEdgePrimitives[indices_, drawFn_,  arrowheads_, cardinal_] /; TrueQ[edgeShapeFunction === Automatic] := Scope[
  coords = Part[$EdgeCoordinateLists, indices];
  primitives = StyleOperator[arrowheads] @ setback[drawFn, edgeSetback] @ coords;
  Annotation[primitives, indices, "EdgePrimitives"]
];

createEdgePrimitives[indices_, drawFn_, arrowheads_, cardinal_] := Scope[
  primitives = Map[
    index |-> applyDrawFn[edgeShapeFunction, <|
      "Coordinates" -> SetbackCoordinates[Part[$EdgeCoordinateLists, index], edgeSetback],
      "Source" -> Part[$EdgeList, index, 1],
      "Target" -> Part[$EdgeList, index, 2],
      "EdgeIndex" -> index,
      "Counter" :> $esfCounter++,
      "LabelStyle" -> Replace[edgeLabelStyle, Automatic -> $DarkGray],
      "Shape" -> If[drawFn === Line, Line, Arrow],
      "Cardinal" -> cardinal,
      "Arrowheads" -> arrowheads
    |>],
    indices
  ];
  Annotation[primitives, indices, "EdgePrimitives"]
];

ExtendedGraphPlot::edgefnmsg = "EdgeShapeFunction issued messages when applied to ``."
ExtendedGraphPlot::edgefnres = "EdgeShapeFunction produced an expression that does not appear to be valid graphics primitives."

applyDrawFn[f_, assoc_] := Scope[
  Check[
    res = f[assoc],
    failPlot["edgefnmsg", AssociationThread @@ KeysValues @ assoc];
  ];
  If[GraphicsPrimitivesQ[res], res,
    Print[res];
    Message[ExtendedGraphPlot::edgefnres]; {}]
];

(**************************************************************************************************)

arrowheadsND[e_] := If[$GraphIs3D, Arrowheads[e, Appearance -> "Projected"], Arrowheads @ e];

makeArrowheadsElement[cardinal_] := Scope[
  {shape, size, style, position} = lookupTagSpec @ StripNegated @ cardinal;
  SetInherited[style, edgeStyle];
  shape = If[shape === "Cardinal",
    SetNone[style, Black];
    makeArrowheadLabelShape[cardinal, style, size]
  ,
    SetNone[style, $Gray];
    makeArrowheadShape[shape, style]
  ];
  If[shape === None, Return @ Nothing];
  shape = attachCardinalAnnotation[shape, cardinal];
  If[labelCardinals, shape = attachArrowheadLabel[shape, StripNegated @ cardinal, size]];
  element = {size, position, shape};
  If[NegatedQ[cardinal], element //= TransformArrowheads[$negationStyle]];
  element
];

lookupTagSpec[other_, cardinal_] := other;
lookupTagSpec[assoc_Association, cardinal_] :=
  Lookup[assoc, cardinal, Lookup[assoc, All, None]];

lookupTagSpec[TwoWay[cardinal_]] :=
  ReplacePart[1 -> $twoWayStyle] @ lookupTagSpec[cardinal];

lookupTagSpec[cardinal_] := Map[
  lookupTagSpec[#, cardinal]&,
  {arrowheadShape, arrowheadSize, arrowheadStyle, arrowheadPosition}
];

(**************************************************************************************************)

attachCardinalAnnotation[head_[primitives_, opts___], cardinal_] :=
  head[Annotation[primitives, cardinal, "Cardinal"], opts];

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
  LabelForm[cardinal, FontSize -> fontSize, Background -> GrayLevel[1.0, 0.8]]
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
    AspectRatio -> Automatic,
    PlotRangeClipping -> False,
    opts
  ];

makeArrowheadGraphic3D[primitives_, style_, opts___] :=
  Graphics3D[
    {Opacity[1], EdgeForm @ None, FaceForm @ style, primitives},
    opts
  ];

$nudge = 1*^-2;

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
    ],

  "CrossLine" ->
    Line @ ToPacked @ {{0., -0.3}, {0., 0.3}},
  "CrossBar" ->
    ToPacked /@ Rectangle[{-0.05,-0.2}, {0.05,0.2}],

  "RightPairedDisk" ->
    Disk[{$pairDist - $nudge, 0.}, .2, {-Pi/2, Pi/2}],
  "RightPairedDiskOffset" ->
    Disk[{-$pairDist/2 - 0.2, 0.}, .2, {-Pi/2, Pi/2}],
  "LeftPairedDisk" ->
    Disk[{-$pairDist + $nudge, 0.}, .2, {Pi/2, 3*Pi/2}],
  "LeftPairedDiskOffset" ->
    Disk[{$pairDist/2 + 0.2, 0.}, .2, {Pi/2, 3*Pi/2}],

  "RightPairedSquare" ->
    FilledCurve[{{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{$pairDist - $nudge, 0.2}, {$pairDist + 0.2, 0.2}, {$pairDist + 0.2, -0.2}, {$pairDist - $nudge, -0.2}}}],
  "RightPairedSquareOffset" ->
    FilledCurve[{{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{$pairDist - 0.225, 0.2}, {$pairDist - 0.025, 0.2}, {$pairDist - 0.025, -0.2}, {$pairDist - 0.225, -0.2}}}],
  "LeftPairedSquare" ->
    FilledCurve[{{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{-$pairDist + $nudge, 0.2}, {-$pairDist - 0.2, 0.2}, {-$pairDist - 0.2, -0.2}, {-$pairDist + $nudge, -0.2}}}],
  "LeftPairedSquareOffset" ->
    FilledCurve[{{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{$pairDist + 0.225, 0.2}, {$pairDist + 0.025, 0.2}, {$pairDist + 0.025, -0.2}, {$pairDist + 0.225, -0.2}}}],

  "RightPairedDiamond" ->
    FilledCurve[{{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{$pairDist - $nudge, 0.2}, {$pairDist + 0.2, 0.}, {$pairDist - $nudge, -0.2}}}],
  "RightPairedDiamondOffset" ->
    FilledCurve[{{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{-0.2, 0.2}, {0., 0.}, {-0.2, -0.2}}}],
  "LeftPairedDiamond" ->
    FilledCurve[{{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{-$pairDist + $nudge, 0.2}, {-$pairDist - 0.2, 0.}, {-$pairDist + $nudge, -0.2}}}],
  "LeftPairedDiamondOffset" ->
    FilledCurve[{{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{0.2, 0.2}, {0., 0.}, {0.2, -0.2}}}]
];

$coneRadius = 0.15;

$arrowheads3D = Association[
  "Cone" ->
    Cone[ToPacked @ {{-0.2, 0., 0.}, {0.2, 0., 0.}}, $coneRadius],
  "Tube" ->
    Tube[ToPacked @ {{-0.2, 0., 0.}, {0.2, 0., 0.}}, $coneRadius/2],
  "ConeDoubleOut" ->
    Cone[ToPacked @ {{{0.1, 0., 0.}, {0.5, 0., 0.}}, {{-0.1, 0., 0.}, {-0.5, 0., 0.}}}, {$coneRadius, $coneRadius}],
  "ConeDoubleOutClose" ->
    Cone[ToPacked @ {{{0., 0., 0.}, {0.4, 0., 0.}}, {{0., 0., 0.}, {-0.4, 0., 0.}}}, {$coneRadius, $coneRadius}],
  "ConeDoubleIn" ->
    Cone[ToPacked @ {{{0.5, 0., 0.}, {0.1, 0., 0.}}, {{-0.5, 0., 0.}, {-0.1, 0., 0.}}}, {$coneRadius, $coneRadius}],
  "Sphere" ->
    Sphere[ToPacked @ {0., 0., 0.}, .2]
];


PackageExport["Color3D"]

Color3D[Opacity[o_, color_]] := Color3D @ SetColorOpacity[color, o];
Color3D[Opacity[o_]] := Directive[GrayLevel[0, o], Specularity @ 0];
Color3D[c_] := Directive[Glow @ c, GrayLevel[0, ColorOpacity[c]], Specularity @ 0];

$namedArrowheads = Union[
  Discard[Keys @ $arrowheads2D, StringContainsQ["Paired" | "DoubleIn" | "DoubleOut"]],
  Keys @ $arrowheads3D, {"Cardinal", "PairedDisk", "PairedDiamond", "PairedSquare"}
];

(**************************************************************************************************)

PackageExport["ArrowheadData"]

ArrowheadData[name_, style_:{}] :=
  makeArrowheadShape[name, style];

(**************************************************************************************************)

makeArrowheadShape[None, _] := None;

makeArrowheadShape["Sphere", style_] :=
  makeArrowheadGraphic3D[$arrowheads3D @ name, Color3D @ style];

makeArrowheadShape[name_String, style_] /; StringStartsQ[name, {"Line", "DoubleLine", "CrossLine"}] :=
  makeArrowheadGraphic2D[
    $arrowheads2D @ name,
    Directive[style, $lineThickness]
  ];

ExtendedGraphPlot::arrow3din2d = "Cannot use shape `` in a 2D graph."

makeArrowheadShape[name_String, style_] := Which[
  KeyExistsQ[$arrowheads2D, name],
    makeArrowheadGraphic2D[$arrowheads2D @ name, style],
  KeyExistsQ[$arrowheads3D, name],
    If[!$GraphIs3D, failPlot["arrow3din2d", name]];
    makeArrowheadGraphic3D[$arrowheads3D @ name, Color3D @ style],
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
    commaString @ $namedArrowheads];

imageToGraphics[img_, pos_] := ImageToGraphics[ImageResize[img, 50], pos, 50];

ExtendedGraphPlot::badarrowhead = "ArrowheadShape -> `` should be None, Automatic, Graphics[..], an Image, or one of ``."

(**************************************************************************************************)

PackageExport["ArrowheadLegend"]

to2DShape[Automatic] := "Arrow";
to2DShape["Cone"] = "Arrow";
to2DShape["Sphere"] = "Disk";
to2DShape["PairedDisk"] = "Disk";
to2DShape["PairedDiamond"] = "Diamond";
to2DShape["PairedSquare"] = "Square";
to2DShape["PairedLabel"] = "Disk";
to2DShape[a_] := a;

ArrowheadLegend[assoc_, "Cardinal"] := "";
ArrowheadLegend[assoc_Association, shape_:"Arrow"] := Scope[
  shapeIndex = If[AssociationQ[shape], shape, <|All -> shape|>];
  rows = KeyValueMap[
    {name, color} |-> (
      arrowShape = to2DShape @ Lookup[shapeIndex, name, Lookup[shapeIndex, All, "Arrow"]];
      If[MissingQ[arrowShape] || arrowShape === None, Nothing,
        graphic = makeLegendArrowheadGraphic[color, arrowShape];
        {graphic, name}
      ]),
    assoc
  ];
  Framed[
    Grid[rows, BaseStyle -> $LabelStyle, Spacings -> {.4, 0.5}],
    FrameMargins -> {{0, 0}, {5, 5}},
    FrameStyle -> None
  ]
]

PackageScope["makeLegendArrowheadGraphic"]

makeLegendArrowheadGraphic[color_, shape_] := makeArrowheadGraphic2D[
  Rotate[$arrowheads2D @ shape, Pi/2], If[StringContainsQ[shape, "Line"], Identity, FaceForm] @ color,
  BaselinePosition -> Scaled[0.1], ImageSize -> {11, 11}, AspectRatio -> Automatic
];

(**************************************************************************************************)

PackageScope["makeHighlightArrowheadShape"]

makeHighlightArrowheadShape[color_, scaling_, False] :=
  makeArrowheadGraphic2D[
    $arrowheads2D["Line"] /. r_List :> (r * scaling),
    toDirective @ {color, JoinForm @ "Miter", CapForm @ "Round"}
  ];

makeHighlightArrowheadShape[color_, scaling_, True] :=
  makeArrowheadGraphic3D[
    Cone[ToPacked @ ({{-0.2, 0., 0.}, {0.2, 0., 0.}} * 1.75 * scaling), $coneRadius * 1.75 * scaling],
    Color3D @ color
  ];

(**************************************************************************************************)

applyAutomaticLegends[graphics_, <||>, _] := graphics;

applyAutomaticLegends[graphics_, automaticLegends_, Automatic] :=
  ApplyLegend[graphics, Values @ automaticLegends];

applyAutomaticLegends[graphics_, automaticLegends_, Placed[Automatic, place_]] :=
  ApplyLegend[graphics, Map[Placed[#, place]&, Values @ automaticLegends]];

applyAutomaticLegends[graphics_, automaticLegends_, graphLegend_] := Scope[
  keyPattern = Alternatives @@ Keys[automaticLegends];
  graphLegend = ToList @ graphLegend;
  If[!MatchQ[graphLegend, {Repeated[keyPattern | Placed[keyPattern, _]]}],
    Return @ graphics];
  legends = removeSingleton @ Replace[graphLegend, {
    s_String :> automaticLegends[s],
    Placed[s_String, p_] :> Placed[automaticLegends[s], p]
  }, {1}];
  ApplyLegend[graphics, legends]
];

(**************************************************************************************************)

PackageScope["removeSingleton"]

removeSingleton[{e_}] := e;
removeSingleton[e_] := e;

(**************************************************************************************************)

ExtendedGraphPlot::badvshapefunc = "`` is not a valid setting for VertexShapeFunction. Valid shapes are: ``."

$validVertexShapes = {"Disk", "Ball", "Point", None};

processVertexShapeFunction[spec_] := Scope[
  setbackDistance = 0; vertexPadding = 0;
  vertexBaseStyle = None;
  defaultVertexColor = $DarkGray;
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
      vertexBaseStyle = EdgeForm @ AbsoluteThickness[.25];
      vertexDrawFunc = drawDisk[$vertexSize / 2];
    ,
    "Point",
      vertexDrawFunc = drawPoint[$vertexSize];
      vertexPadding = 1 + vertexSizeImage / 2;
    ,
    "Square",
      vertexDrawFunc = drawSquare[$vertexSize];
      vertexBaseStyle = EdgeForm @ AbsoluteThickness[1];
      vertexPadding = 1 + vertexSizeImage / 2;
    ,
    "Hexagon",
      vertexDrawFunc = drawHexagon[$vertexSize];
      vertexBaseStyle = EdgeForm @ AbsoluteThickness[1];
      vertexPadding = 1 + vertexSizeImage / 2,
    _Association,
      vertexDrawFunc = drawCustomShape[spec, $vertexSize];
      If[$inheritedVertexSize,
        vertexSizeImage = Max[Map[cachedRasterizeSize, spec]] / 2];
      vertexPadding = 1 + vertexSizeImage / 2;
    ,
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

drawCustomShape[assoc_, size_][pos_, color_] :=
  Map[drawIndividualCustomShape[assoc, color, size], pos];

drawIndividualCustomShape[assoc_, color_, size_][pos_] := Scope[
  index = IndexOf[$VertexCoordinates, pos];
  shape = If[index === None, None, Part[assoc, index]];
  drawGraphicsWithColor[shape, pos, color, size]
];

drawGraphicsWithColor[None, pos_, color_, size_] :=
  drawPoint[size][pos, color];

drawGraphicsWithColor[other_, pos_, color_, size_] :=
  Text[Style[other, color, FontSize -> size], pos, {0, 0}, Background -> White];

drawGraphicsWithColor[g_Graphics, pos_, color_, size_] :=
  Inset[
    ReplaceOptions[g, FrameStyle -> color],
    pos, {Center, Baseline}, If[$inheritedVertexSize, Automatic, {size, size}]
  ];


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

setbackCoords[0|0.][line_] := line;

setbackCoords[dist_][{a_, b_}] /; (EuclideanDistance[a, b] > 2 * dist) := Scope[
  dx = Normalize[b - a] * dist;
  {a + dx, b - dx}; {a, b}
];

setbackCoords[dist_][other_] := other;

insetDisk[size_][pos_] := Inset[Graphics[Disk[{0, 0}, 1], AspectRatio -> 1], pos, {0, 0}, size];
(id:insetDisk[size_])[pos_ ? CoordinateMatrixQ] := Map[id, pos];

(**************************************************************************************************)

drawViaColorFunc[colorFn_, drawFn_, count_, parts_, dataProviderFn_, optSymbol_] := Scope[

  If[colorFn === None,
    result = drawFn[Part[Range @ count, parts], {}];
    Return @ simplifyPrimitives @ result;
  ];

  colorGroupFn = If[MatchQ[colorFn, Tooltip[_]],
    colorFn = First @ colorFn;
    toColorDrawFuncWithTooltip[drawFn],
    toColorDrawFunc[drawFn]
  ];

  palette = Automatic;
  colorFn //= Replace[Paletted[d_, p_] :> (palette = p; d)];
  colorData = dataProviderFn @ colorFn;
  If[FailureQ[colorData], failPlot[optSymbol, "badcolvals"]];

  {colorGroups, colorFunctionObject} = ApplyColoring[colorData, palette];
  If[FailureQ[colorFunctionObject], failPlot[optSymbol, "badcolvals"]];
  If[!MatchQ[colorFunctionObject, ColorFunctionObject["Discrete", Identity]],
    automaticLegends["Colors"] ^= colorFunctionObject];

  colorGroups //= filterAssocIndices[parts];

  simplifyPrimitives @ KeyValueMap[colorGroupFn, colorGroups]
];

ExtendedGraphPlot::badcolvals = "Setting of `` did not produce values that could be colored."

toColorDrawFunc[drawFn_] :=
  {colorValue, indices} |-> drawFn[indices, First @ colorValue];

toColorDrawFuncWithTooltip[drawFn_] :=
  {colorValue, indices} |-> NiceTooltip[
    drawFn[indices, First @ colorValue],
    Last @ colorValue
  ];

(**************************************************************************************************)

edgeColorDataProvider = MatchValues[
  "Name" := $EdgeList;
  "Index" := Range @ $EdgeCount;
  "Cardinal" := $EdgeTags;
  (* todo, make the distance work on regions as well *)
  rules:{__Rule} := Last @ resolveRegionRules[rules, EdgeColorFunction];
  list_List /; Length[list] === $EdgeCount := list;
  assoc_Association := Lookup[assoc, $EdgeList, Lookup[assoc, All, $LightGray]];
  spec_ := failPlot["badcolfunc", shortMsgForm @ spec, EdgeColorFunction]
];

(**************************************************************************************************)

ExtendedGraphPlot::notvertex = "`` is not a valid vertex of the graph."
ExtendedGraphPlot::badcolfunc = "`` is not a valid color function specification for ``."
ExtendedGraphPlot::msgcolfunc = "Applying the requested color function to property `` gave messages."

getVertexIndex[GraphOrigin] := getVertexIndex @ $GraphOrigin;
getVertexIndex[v_] := Lookup[$VertexIndex, v, failPlot["notvertex", v]];
getAnnoValue[annos_, key_] := Lookup[annos, key, failPlot["badgraphannokey", key, commaString @ Keys @ annos]];

vertexColorDataProvider = MatchValues[
  "Name" := $VertexList;
  "Index" := Range @ $VertexCount;
  (* todo, make the distance work on regions as well *)
  "Distance" := %[{"Distance", GraphOrigin}];
  {"Distance", v_} := MetricDistance[$MetricGraphCache, getVertexIndex @ v];
  key_String := getAnnoValue[vertexAnnotations, key];
  (key_String -> f_) := Replace[Quiet @ Check[Map[toFunc @ f, %[key]], $Failed], $Failed :> failPlot["msgcolfunc", key]];
  rules:{__Rule} := First @ resolveRegionRules[rules, VertexColorFunction];
  list_List /; Length[list] === $VertexCount := list;
  assoc_Association := Lookup[assoc, $VertexList, Lookup[assoc, All, $LightGray]];
  spec_ := failPlot["badcolfunc", shortMsgForm @ spec, VertexColorFunction]
];

toFunc[i_Integer] := Extract[i];
toFunc[f_] := f;

(**************************************************************************************************)

resolveRegionRules[rules_, optSym_] := Scope[
  defaultColor = FirstCase[rules, Rule[All, color:$ColorPattern] :> color, Gray, {1}];
  $vertexValues = ConstantArray[defaultColor, $VertexCount];
  $edgeValues = ConstantArray[defaultColor, $EdgeCount];
  $optSym = optSym;
  $regionColor = $Red;
  Scan[applyRegionRule, rules];
  {$vertexValues, $edgeValues}
];

applyRegionRule = MatchValues[
  All -> $ColorPattern := Null;
  spec_ -> color:$ColorPattern := Scope[
    $regionColor = color /. $colorNormalizationRules;
    % @ spec
  ];
  spec_ ? GraphRegionElementQ := Scope[
    {vertices, edges} = processRegionVerticesEdges @ spec;
    If[FailureQ @ vertices, failPlot["badregion", spec, $optSym]];
    Part[$vertexValues, vertices] = $regionColor;
    Part[$edgeValues, edges] = $regionColor;
  ];
  spec:Except[_List] :=
    % @ List @ spec;
  spec_ := Scope[
    vertices = Lookup[$VertexIndex, spec, failPlot["badregion", spec, $optSym]];
    Part[$vertexValues, vertices] = $regionColor;
  ];
];

ExtendedGraphPlot::badregion = "Specification `` in setting for `` is not a valid region, vertex, or edge."

shortMsgForm[spec_] := If[Length[spec] > 10 || ByteCount[spec] > 1000, Skeleton[Length[spec]], spec];

(**************************************************************************************************)

ExtendedGraphPlot::badvertexsize = "`` is not a valid setting for VertexSize."

(* this returns a size in plot range coordinates *)

processVertexSize = MatchValues[
  Automatic                          := % @ $defaultVertexSize;
  r_ ? NumericQ                      := % @ AbsolutePointSize @ r;
  sym:$SymbolicSizePattern           := % @ AbsolutePointSize @ $SymbolicPointSizes @ sym;
  PointSize[sz_ ? NumericQ]          := N @ imageFractionToPlotSize @ sz;
  AbsolutePointSize[sz_ ? NumericQ]  := N @ imageSizeToPlotSize @ sz;
  Scaled[r_ ? NumericQ]              := N[r * $GraphMaxSafeVertexSize];
  Scaled[sym:$SymbolicSizePattern]   := % @ Scaled @ $SymbolicSizeFractions @ sym;
  m_Max | m_Min                      := Map[%, m];
  rule_Rule                          := % @ {rule};
  rules_Association                  := % @ Normal @ rules;
  rules:{__Rule}                     := (
    $vertexSizeOverrides ^= Association[processVertexSizeRule /@ rules];
    Lookup[rules, Key @ All, %[$defaultVertexSize]]
  );
  Inherited                          := ($inheritedVertexSize ^= True; % @ Automatic);
  other_                             := failPlot["badvertexsize", other];
];

processVertexSizeRule[All -> _] :=
  Nothing;

processVertexSizeRule[lhs_ -> rhs_] :=
  findVertex[lhs] -> processVertexSize[rhs];

(**************************************************************************************************)

findEdge = MatchValues[
  spec_ ? GraphRegionElementQ := Last @ processRegionVerticesEdges @ spec;
  edge_ := IndexOf[$EdgeList, edge /. GraphOrigin :> findVertex[GraphOrigin], failPlot["noedge", edge]];
];

ExtendedGraphPlot::noedge = "Could not find edge ``."

(**************************************************************************************************)

findVertex[IndexedVertex[i_]] := i;
findVertex[v_] := Lookup[$VertexIndex, Key @ v, failPlot["novertex", v]];
findVertex[GraphOrigin] := findVertex @ LookupAnnotation[$Graph, GraphOrigin];

ExtendedGraphPlot::novertex = "`` is not a valid vertex."

(**************************************************************************************************)

ExtendedGraphPlot::badlabelspec = "The label specification `` was not one of the recognized forms."

estimateLabelPadding[graphics_, vertexLabelStyle_] := Scope[
  graphics = graphics /. Text[opts___] :> outerText[opts];
  text = FirstCase[graphics, outerText[t_, ___], $Failed, Infinity];
  If[FailureQ[text], Return @ {{0, 0}, {0, 0}}];
  {w, h} = 1 + cachedRasterizeSize[Text @ First @ text] / 2;
  {x, y} = Part[text, 3];
  N @ If[!FreeQ[vertexLabelStyle, LabelPosition -> Automatic],
    Max[w, h] * 1.1,
    {
      offsetToPadding[x, w],
      offsetToPadding[y, h]
    }
  ]
];

offsetToPadding[o_, s_] := Switch[Sign[o], 1, {s, 0}, 0, {s, s}/2, -1, {0, s}];

cachedRasterizeSize[Null] := {0, 0};
cachedRasterizeSize[e_] := cachedRasterizeSize[e] = Rasterize[e, "RasterSize"];

PackageExport["LabelPosition"]

generateLabelPrimitives[spec_, names_, coordinates_, parts_, size_, labelStyle_, annotations_, isVertices_] := Scope[
  $annotationKeys = Keys @ annotations;
  $labelNames = names;
  coordinates = Part[coordinates, parts];
  $annotations = annotations;
  $labeledElemSize = size / 2;
  $spacings = 0; $labelZOrder = 0;
  $isVertices = isVertices;
  $labelSizeScale = 1; $labelY = -1; $labelX = 0; $labelBackground = GrayLevel[1.0, 0.6];
  $labelBaseStyle = Inherited; $labelOffset = None;
  $adjacencyIndex = None;
  labelStyle //= toDirectiveOptScan[setLabelStyleGlobals];
  labelStyle //= DeleteCases[sspec:$sizePattern /; ($labelSizeScale = toNumericSizeScale @ sspec; True)];
  $magnifier = If[$labelSizeScale == 1, Identity, Magnify[#, $labelSizeScale]&];
  {payloadFunction, placerFunction} = processLabelSpec[spec];
  indices = If[parts === All, Range @ Length @ names, parts];
  labelElements = MapThread[placerFunction[labelForm @ payloadFunction @ #2, #1, #2]&, {coordinates, indices}];
  {Style[labelElements, labelStyle], $labelZOrder}
];

toDirectiveOptScan[f_][{Automatic}] :=
  Directive[];

toDirectiveOptScan[f_][e_] :=
  toDirectiveOptScan[f][{e}];

toDirectiveOptScan[f_][e_List | e_Directive] := (
  Scan[f, Cases[e, _Rule]];
  toDirective @ DeleteCases[e, _Rule]
)

ExtendedGraphPlot::badsubopt = "`` is not a recognized suboption. Recognized options are ``."

setLabelStyleGlobals = MatchValues[
  ItemSize -> size:$sizePattern := $labelSizeScale ^= toNumericSizeScale @ size;
  Background -> o:$opacityPattern := $labelBackground ^= GrayLevel[1.0, toNumericOpacity @ o];
  Background -> None := $labelBackground ^= None;
  Background -> c:$ColorPattern := $labelBackground ^= c;
  BaseStyle -> s_ := $labelBaseStyle ^= s;
  LabelPosition -> Automatic := If[$isVertices,
    $labelX ^= Automatic;
    $labelY ^= Automatic;
    $adjacencyIndex ^= VertexAdjacentEdgeTable[$Graph];
  ,
    %[LabelPosition -> Top]
  ];
  ZOrder -> z_ := ($labelZOrder ^= z);
  LabelPosition -> Top|Above := $labelY ^= -1;
  LabelPosition -> Bottom|Below := $labelY ^= 1;
  LabelPosition -> Center := $labelX ^= $labelY ^= 0;
  LabelPosition -> {x_ ? NumericQ, y_ ? NumericQ} := ($labelX ^= N[x]; $labelY ^= N[y]);
  LabelPosition -> Left := $labelX ^= 1;
  LabelPosition -> Right := $labelX ^= -1;
  LabelPosition -> Offset[{x_ ? NumericQ, y_ ? NumericQ}] := (
    $labelOffset ^= {x, y};
    $labelX ^= Switch[Sign @ x, -1, 1, 0, 0, 1, -1];
    $labelY ^= Switch[Sign @ y, -1, 1, 0, 0, 1, -1];
  );
  Spacings -> n_ := $spacings ^= N[n];
  rule_ := failPlot["badsubopt", rule, commaString @ {ItemSize, BaseStyle, Background, LabelPosition, Spacings}];
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
  assoc_Association :=    lookupPayloadInAssoc[assoc];
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

lookupPayloadInAssoc[assoc_][i_] :=
  Lookup[assoc, IndexedVertex[i], Lookup[assoc, getName @ i]];

getIndex[i_] := i;
getName[i_] := Part[$labelNames, i];
getCardinal[i_] := Part[$EdgeTags, i];
getAnnotation[name_][i_] := Part[$annotations, name, i];

placeTooltipAt[label_, pos_, _] := NiceTooltip[{Transparent, If[$GraphIs3D, Sphere, Disk][pos, 1.1*$labeledElemSize]}, label];
placeTooltipAt[None | _Missing, _, _] := Nothing;

$labelOffsets = N @ CirclePoints[{1, 0}, 8];
placeLabelAt[label_, pos_, index_] /; ($labelX === Automatic) := Scope[
  adjacentEdges = Part[$adjacencyIndex, index];
  edgeCoords = Mean /@ Part[$EdgeCoordinateLists, adjacentEdges];
  bestOffset = MaximumBy[$labelOffsets, sumOfSquaredDistances[#, edgeCoords]&];
  {$labelX, $labelY} = bestOffset * {1.7, 1.1};
  placeLabelAt[label, pos, index]
];

sumOfSquaredDistances[point_, points_] :=
  Total @ Flatten @ SquaredDistanceMatrix[{point}, points];

placeLabelAt[g_Graphics, pos_, _] :=
  Inset[g, pos, {Center, Baseline}];

placeLabelAt[label_, pos_, _] := Text[
  $magnifier @ label,
  If[$labelOffset === None,
    pos + If[$GraphIs3D, 0, -$labeledElemSize * {$labelX, $labelY} * (1 + $spacings)]
  ,
    Offset[$labelOffset, pos]
  ],
  {$labelX, $labelY} * 0.95,
  Background -> $labelBackground,
  BaseStyle -> $labelBaseStyle
];

placeLabelAt[None | _Missing, _, _] := Nothing;

makeMagnifier[1|1.0] := Identity;
makeMagnifier[scale_] := Magnify[#, scale]&;

(**************************************************************************************************)

makeGraphics[elements_, imageSize_, imagePadding_, plotRangePadding_, plotLabel_, extraOptions_, prolog_, epilog_, baseline_] := Graphics[
  elements,
  Sequence @@ extraOptions,
  Frame -> None, Axes -> None,
  ImageSize -> imageSize,
  ImagePadding -> imagePadding, PlotLabel -> plotLabel,
  PlotRangePadding -> plotRangePadding, PlotRangeClipping -> False,
  AspectRatio -> Automatic, PreserveImageOptions -> False,
  If[epilog === None, Sequence @@ {}, Epilog -> epilog],
  If[prolog === None, Sequence @@ {}, Prolog -> prolog],
  BaselinePosition -> baseline
];

makeGraphics3D[elements_, imageSize_, imagePadding_, plotRangePadding_, plotLabel_, extraOptions_, prolog_, epilog_, baseline_] := Graphics3D[
  {CapForm[None],  Replace[prolog, None -> Nothing], elements, Replace[epilog, None -> Nothing]},
  Sequence @@ extraOptions,
  Axes -> None, Boxed -> False,
  ImageSize -> imageSize,
  ImagePadding -> imagePadding, PlotRange -> All,
  PlotRangePadding -> plotRangePadding,
  Lighting -> "Neutral",
  Method -> {"ShrinkWrap" -> False, "EdgeDepthOffset" -> False},
  AspectRatio -> Automatic, PreserveImageOptions -> False,
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
PackageExport["EdgeLabelForm"]

ShowLabels[e_] := VertexLabelForm[e];
VertexLabelForm[e_] := e /. (g_Graph ? GraphQ) :> RuleCondition @ Graph[g, VertexLabels -> "Name", ImagePadding -> 20];
VertexTooltipForm[e_] := e /. (g_Graph ? GraphQ) :> RuleCondition @ Graph[g, VertexLabels -> Placed["Name", Tooltip]];

EdgeLabelForm[e_] := e /. (g_Graph ? GraphQ) :> RuleCondition @ Graph[g, EdgeLabels -> "Index", ImagePadding -> 20];

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

PackageScope["simplifyPrimitives"]

simplifyPrimitives[primitives_] :=
  simplifyPrimitiveAnnotations @ simplifyPrimitiveStyles @ primitives;


$simplifyPrimitiveStyleRules = Dispatch @ {
  Directive[{}] :> {},
  Style[g_] :> g,
  Style[g_, {} | {{}}] :> g,
  Style[Style[g_, a___], b___] :> Style[g, a, b]
};

simplifyPrimitiveStyles[primitives_] :=
  ReplaceRepeated[primitives, $simplifyPrimitiveStyleRules];


$simplifyPrimitiveAnnotationRules = Dispatch @ {

  (* a single annotation with uniform primitives can use a single larger primitive *)
  anno:Annotation[{__Point} | {__Line} | {__Arrow}, __] :> joinAnnotationPrimitives[anno],

  (* fragmented uniform primitives can be combined into a single larger primitive *)
  annos:{Annotation[(head:(Point | Line | Arrow))[_], _List, type_]..} :> joinHeadAnnotations[annos, type],

  (* fragmented list primitives can be joined  *)
  annos:{Annotation[_List, _List, type_]..} :> joinListAnnotations[annos, type],

  (* singleton annos can be joined, even if they are non-uniform / wrapped in style *)
  annos:{Annotation[_, {_}, type_]..} :> joinSingletonAnnotations[annos, type]
};

joinHeadPrimitives[prims_] := Scope[
  head = Part[prims, 1, 0];
  coords = Part[prims, All, 1];
  normFunc = If[head === Point, toCoordinateMatrix, toCoordinateArray];
  coordsArray = ToPackedRealArrays @ Apply[Join] @ Map[normFunc] @ coords;
  head[coordsArray]
];

joinAnnotationPrimitives[Annotation[prims_, args__]] :=
  Annotation[joinHeadPrimitives @ prims, args];

joinHeadAnnotations[annos_, type_] := Scope[
  primitives = joinHeadPrimitives @ Part[annos, All, 1];
  indices = Join @@ Part[annos, All, 2];
  Annotation[primitives, indices, type]
];

toCoordinateArray[e_] := If[CoordinateArrayQ[e], e, List @ e];
toCoordinateMatrix[e_] := If[CoordinateMatrixQ[e], e, List @ e];

joinListAnnotations[annos_, type_] :=
  Annotation[Join @@ Part[annos, All, 1], Join @@ Part[annos, All, 2], type];

joinSingletonAnnotations[annos_, type_] :=
  Annotation[Part[annos, All, 1], Part[annos, All, 2, 1], type];

(* a list of singleton-index annotations can be replaced with a single such primitive annotation *)
simplifyPrimitiveAnnotations[primitives_] :=
  ReplaceRepeated[primitives, $simplifyPrimitiveAnnotationRules];

(**************************************************************************************************)

PackageExport["ExtractGraphPlotPrimitives"]

ExtractGraphPlotPrimitives[targetIds_, type_] := Block[
  {$targets = targetIds},
  DeepCases[
    $GraphPlotGraphics,
    anno:Annotation[_, ids_, type] /; IntersectingQ[ids, $targets] :>
      transformPrimitiveAnno[anno, PartOperator[-1, 1]]
  ]
];

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
    If[ids1 === {}, {}, Annotation[prim1, ids1, type]],
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
  splitMatchingIds[list, ids];

splitPrimitives[other_, _] := (
  Panic["UnhandledPrimitive", other];
);

splitMatchingIds[list_, ids_] := Scope[
  matched = Flatten @ Lookup[PositionIndex @ ids, $targets, {}];
  remaining = Complement[Range @ Length @ list, matched];
  List[
    Part[list, remaining],
    Part[list, matched]
  ]
];

(**************************************************************************************************)

PackageExport["AttachGraphPlotAnnotation"]

AttachGraphPlotAnnotation[name_String] := (
  $GraphPlotGraphics //= MapAt[Annotation[#, name]&, 1]
);

(**************************************************************************************************)

PackageExport["ApplyFinalTransforms"]

ApplyFinalTransforms[g:(_Graphics|_Graphics3D)] :=
  MapAt[ApplyFinalTransforms, g, 1];

ApplyFinalTransforms[primitives_] := primitives //. {
  Annotation[p_, "FadeGraph"] :> fadePrimitives[p],
  Annotation[p_, "FadeEdges"] :> applyToPrimitivesType[p, fadePrimitives, "EdgePrimitives"],
  Annotation[p_, "FadeVertices"] :> applyToPrimitivesType[p, fadePrimitives, "VertexPrimitives"],
  Annotation[p_, "HideArrowheads"] :> applyToPrimitivesType[p, hideArrowheads, "EdgePrimitives"],
  Annotation[p_, "HideVertices"] :> deletePrimitivesType[p, "VertexPrimitives"],
  Annotation[p_, "HideEdges"] :> deletePrimitivesType[p, "EdgePrimitives"]
} // simplifyPrimitives;

deletePrimitivesType[primitives_, type_] := ReplaceAll[primitives,
  Annotation[_, _, type] :> {}
];

applyToPrimitivesType[primitives_, f_, type_] := ReplaceAll[primitives,
  Annotation[p_, indices_, type] :> Annotation[f @ p, indices, type]
];

fadePrimitives[primitives_] := ReplaceAll[primitives, {
  t_Text :> t,
  a:Annotation[_, "FrameLabel" | "Protected"] :> a,
  Directive[Glow[_], opts___] :> Directive[Glow[LightGray], opts],
  _Opacity -> Opacity[1],
  $ColorPattern -> LightGray
}];

hideArrowheads[p_] := ReplaceAll[p, {
  Arrow -> Line,
  _Arrowheads -> {}
}];

(**************************************************************************************************)

PackageExport["AnnotationsToTooltips"]

AnnotationsToTooltips[g_Graphics, name_] :=
  ReplaceAll[g, Annotation[prims_, ids_, name] :> annoWithTooltips[prims, ids]]

annoWithTooltips[prims_List, ids_] :=
  MapThread[Tooltip, {prims, ids}];

annoWithTooltips[Style[prims_, opts___], ids_] :=
  Style[annoWithTooltips[prims, ids], opts];

annoWithTooltips[Arrow[prims_, opts__], ids_] :=
  MapThread[Tooltip[Arrow[#1, opts], #2]&, {prims, ids}];

annoWithTooltips[(head:Point|Line|Arrow)[prims_], ids_] :=
  MapThread[Tooltip[head[#1], #2]&, {prims, ids}];
