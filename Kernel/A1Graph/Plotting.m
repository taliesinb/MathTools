(* TODO: Add AlignmentPoint, much like BaselinePosition *)

PrivateVariable[$graphRegionTable]

$graphRegionTable = StringTrim @ "
The following specifications describe paths in the graph:
| %Path[v$, {c$1, $$, c$n}] | start at v$, move along cardinals c$i |
| %Line[{v$1, v$2}] | the geodesic between v$1 and v$2 |
| %Line[{v$1, v$2}, c$] | start at v$1, moving along c$, and end at v$2 |
| %Line[{v$1, $$, v$n}] | the geodesic path between v$1 and v$2, v$2 and v$3, etc. |
| %Polygon[{v$1, $$, v$n}] | geodesics between the v$i, taken cyclically |
| %HalfLine[v$, c$] | a geodesic starting at v$ and continuing in the cardinal direction c$ |
| %HalfLine[{v$1, v$2}] | a geodesic starting at v$1 and continuing through v$2 |
| %InfiniteLine[v$, c$] | a geodesic with midpoint v$, and continuing in directions c$ and Inverted[c$] |
| %InfiniteLine[{v$1, v$2}] | a geodesic intersecting v$1 and v$2 |
| %Cycles[word$] | all  disjoint closed paths with given word |
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
| %GraphRegionBoundary[r$] | the vertices in region r$ adjacent to vertices not in r$ |
| %GraphRegionComplement[r$1, r$2] | the complement of region r$1 with region r$2 |
| %GraphRegionIntersection[r$1, r$2, $$] | the mutual intersection of regions r$i |
| %GraphRegionUnion[r$1, r$2, $$] | the union of regions r$i |
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

PrivateVariable[$GraphRegionHighlightUsage]

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

* If %HighlightStyle is also provided, elements of it will be applied to \
corresponding highlight specifications:
| <|key$1 -> style$1, $$|> | apply style$i to highlight specification under key$i |
| {style$1, style$2, $$} | apply style$i to highlight specification number i$ |
| style$ | apply style$ to all highlight specifications |
| {$$, opt$ -> value$} | apply additional highlight options globally |

## Highlight styles

* The following options are supported as rules in %Style[spec$, $$] wrapper in a highlight \
specifications, and in %HighlightStyle:

| %SimplifyRegions | True | whether to render disk-like regions with %Disk |
| %PerformanceGoal | 'Quality' | how to prioritize region rendering |
| %PathStyle | 'Line' | how to render paths |
| %ArrowheadPosition | 1.0 | where to place arrowhead on a path |
| %ArrowheadSize | Automatic | arrowhead size |
| %DiskRadius | Automatic | vertex highlighting radius |
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
| {'ReplaceEdges', cards$} | only preserve arrowheads from cards$ |
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

PublicOption[GraphRegionHighlight]

SetUsage @ "
GraphRegionHighlight is an extended option to Graph that specifies one or more highlighting directives to highlight \
specific regions of the graph when it is displayed.
<*$GraphRegionHighlightUsage*>
"

(**************************************************************************************************)

SetUsage @ "
HighlightGraphRegion[graph$, highlights$] highlights regions of graph$ according to highlights$.
* HighlightGraphRegion returns a %Graph in which the option %GraphRegionHighlight has been set to \
highlights$.
* Any existing highlights are preserved.
<*$GraphRegionHighlightUsage*>
"

(**************************************************************************************************)

PublicOption[GraphLegend]

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
  Construct[InterpretationBox,
    stripDynamicModule @ ToBoxes @ ExtendedGraphPlot @ graph,
    graph
  ];

stripDynamicModule[boxes_] := ReplaceAll[boxes,
  NamespaceBox[
    "NetworkGraphics",
    HoldPattern[DynamicModuleBox[{Typeset`graph = _}, TagBox[subBoxes_, _], ___]]
  ] :> subBoxes
];

(**************************************************************************************************)

PrivateVariable[$VertexCoordinates, $EdgeCoordinateLists, $GraphIs3D, $GraphPlotRange, $GraphPlotSize, $GraphPlotAspectRatio, $GraphPlotImageSize, $GraphPlotImageWidth, $GraphPlotEffectiveImageWidth, $GraphMaxSafeVertexSize, $GraphPlotGraphics]

PrivateFunction[GraphPlotScope]

SetHoldRest[GraphPlotScope];

GraphPlotScope[graph_, body_] := Scope[

  If[!GraphQ[graph], ReturnFailed[]];

  UnpackExtendedThemedOptions[graph, collapseMultiedges];

  GPPrint["GraphPlotScope for ", graphSkeleton @ graph];
  GraphScope[
    If[TrueQ @ collapseMultiedges, CombineMultiedges @ graph, graph]
  ,
    {$VertexCoordinates, $EdgeCoordinateLists} = ExtractGraphPrimitiveCoordinates @ $Graph;

    viewRegion = LookupExtendedOption[$Graph, ViewRegion];
    If[viewRegion =!= All, applyViewRegion[viewRegion], $VertexParts = $EdgeParts = All];

    $GraphPlotImageSize := $GraphPlotImageSize := LookupImageSize @ $Graph;
    $GraphPlotImageWidth := $GraphPlotImageWidth = First[$GraphPlotImageSize; LookupImageSize @ $Graph];

    (* before we have called the user function, guess the range based on the vertex and edge coordinates *)
    $GraphIs3D := $GraphIs3D = CoordinateMatrix3DQ[$VertexCoordinates];
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

ExtendedGraphPlot::badopt = "Option setting `` -> `` is invalid.";
ExtendedGraphPlot::badviewregion = "ViewRegion -> `` is invalid and will be ignored.";

applyViewRegion[regionSpec_] := (
  {$VertexParts, $EdgeParts} = processRegionVerticesEdges @ regionSpec;
  If[FailureQ[$VertexParts],
    $VertexParts = $EdgeParts = All;
    failPlot["badviewregion", regionSpec]
  ];
)

$rangeMicroPadding = 1*^-5;
computeCoordinateBounds[] := Scope[
  {plotRange, plotRangePadding} = LookupOption[$Graph, {PlotRange, PlotRangePadding}];
  If[MatrixQ[plotRange, NumericQ], Return @ addPRPadding[plotRange, plotRangePadding]];
  If[NumericQ[plotRange], Return @ addPRPadding[N[{{-1, 1}, {-1, 1}} * plotRange], plotRangePadding]];
  range = CoordinateBounds[{
    Part[$VertexCoordinates, $VertexParts],
    Replace[Part[$EdgeCoordinateLists, $EdgeParts], {} -> Nothing]},
    $rangeMicroPadding
  ];
  range = addPRPadding[range, plotRangePadding];
  If[plotRange === "Square", range //= ToSquarePlotRange];
  range
];

addPRPadding[range_, Automatic | None | 0] := range;
addPRPadding[{{l_, r_}, {b_, t_}}, p_ ? NumericQ] := {{l - p, r + p}, {b - p, t + p}};
addPRPadding[{{l_, r_}, {b_, t_}}, {h_ ? NumericQ, v_ ? NumericQ}] := {{l - h, r + h}, {b - v, t + v}};
addPRPadding[{{l_, r_}, {b_, t_}}, {{pl_, pr_}, {pb_, pt_}}] := {{l - pl, r + pr}, {b - pb, t + pt}};
addPRPadding[range_, _] := (Print["Bad PlotRangePadding"]; range)

(**************************************************************************************************)

PublicFunction[ExtendedGraphPlot]

$autoFilledLegendPattern = (Automatic | _String) | Placed[Automatic | _String | Labeled[Automatic | _String, __], _] | Labeled[Automatic | _String, __];

ExtendedGraphPlot[___] := $Failed;

ExtendedGraphPlot[graph_Graph, opts__Rule] :=
  ExtendedGraphPlot @ ExtendedGraph[graph, opts]

ExtendedGraphPlot[graph_Graph] := Block[
  {
   $GraphPlotImageSize, $GraphPlotImageWidth, $GraphPlotRange, $GraphPlotSize, $GraphMaxSafeVertexSize, $GraphPlotGraphics,
   plottingFunction, graphLegend, graphRegionHighlight, vertexColorFunction,
   highlightGraphics, requiredPadding, graphLabel = None
  },

  If[!GraphQ[graph], Return[$Failed, Block]];

  GPPrint["ExtendedGraphPlot for ", graphSkeleton @ graph];

  {plottingFunction, graphLegend, graphRegionHighlight, vertexColorFunction} =
    LookupAnnotation[graph, {GraphPlottingFunction, GraphLegend, GraphRegionHighlight, VertexColorFunction}, None];

  SetNone[plottingFunction,
    If[vertexColorFunction === None, GraphComputation`GraphDrawing, ExtendedGraphPlottingFunction]];

  SetAutomatic[plottingFunction, ExtendedGraphPlottingFunction];

  GraphPlotScope[graph,

    If[$VertexCount === $EdgeCount === 0,
      (* empty graphs don't accept options for some reason, so we have to pick a size here *)
      Return @ Spacer @ 10];

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

    eventHandler = None;
    If[MatchQ[$GraphPlotGraphics, _EventHandler],
      eventHandler = ReplacePart[$GraphPlotGraphics, 1 -> None];
      $GraphPlotGraphics //= First;
    ];

    (* recompute these with the results of plottingFunction, for the benefit of GraphRegionHighlight *)
    rawGraphics = Replace[$GraphPlotGraphics, EventHandler[e_, ___] :> e];
    $GraphPlotImageSize := $GraphPlotImageSize = LookupImageSize @ $GraphPlotGraphics;
    $GraphPlotImageWidth := $GraphPlotImageWidth = First[$GraphPlotImageSize] - Total[First @ LookupOption[$GraphPlotGraphics, ImagePadding]];
    $GraphPlotEffectiveImageWidth = First[LookupOption[$GraphPlotGraphics, ImageSizeRaw], $GraphPlotImageWidth];

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
    result = ApplyFinalTransforms @ $GraphPlotGraphics;
    
    If[eventHandler =!= None, result = ReplacePart[eventHandler, 1 -> result]];

    ApplyLegend[result, graphLegend]
  ]
];

GraphicsImageSizePadTo[graphics_, requiredPadding_] := Scope[
  padding = LookupOption[graphics, ImagePadding];
  padding = MatrixMap[Max[#, requiredPadding]&, padding];
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
  If[CoordinateMatrix3DQ[$VertexCoordinates],
    viewOptions = LookupExtendedOption[$Graph, ViewOptions];
    SetAutomatic[viewOptions, $automaticViewOptions];
    vertexCoords = Part[$VertexCoordinates, $VertexParts];
    viewOptions //= DeleteOptions["ShrinkWrap"];
    viewOptions = Association[PlotRange -> CoordinateBounds[vertexCoords], viewOptions];
    viewTransform = ConstructGraphicsViewTransform[viewOptions];
    If[ContainsQ[viewTransform, Indeterminate], Return[1, Block]];
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
  Min[minDistance, Max[$GraphPlotSize] / 3]
];

lineCenter = Case[
  pair:{_, _}   := Mean @ pair;
  list_List     := Scope[
    n = Length[list]; n2 = (n + 1) / 2;
    If[IntegerQ[n2],
      Part[list, n2],
      Mean @ Part[list, {Floor @ n2, Ceiling @ n2}]
    ]
  ]
];

(**************************************************************************************************)

PublicFunction[GraphAnnotationData]

GraphAnnotationData[annotation_] :=
  LookupExtendedThemedOption[$Graph, annotation];

(**************************************************************************************************)

PrivateFunction[ComputeExtendedGraphImageSizeData]

(* HACKY: This lets us get info about how big a graph will want to draw
even if it has EdgeLength etc target sizes in it *)
ComputeExtendedGraphImageSizeData[g_Graph] := Scope[
  $returnRawImageSize = True;
  Catch[ExtendedGraphPlot[g], $rawImageSizeDataTag]
];

(**************************************************************************************************)

PublicVariable[$GraphPlotVerboseMode]

SetInitialValue[$GraphPlotVerboseMode, False];

SetHoldAllComplete[GPPrint];
graphSkeleton[g_Graph] := StringJoin["Graph[«", IntegerString @ VertexCount @ g, "», «", IntegerString @ EdgeCount @ g, "»]"];
GPPrint[args___] /; $GraphPlotVerboseMode := Print[args];

PublicFunction[ExtendedGraphPlottingFunction]

ExtendedGraphPlot::badcolors = "CardinalColors -> `` should be an association from cardinals to colors.";
ExtendedGraphPlot::badpadding = "Padding option `` was invalid."
ExtendedGraphPlot::badthickness = "EdgeThickness -> `` was invalid."

ExtendedGraphPlottingFunction[___] := $Failed;

PrivateFunction[failPlot]

failPlot[msgName_String, args___] := (
  Message[MessageName[ExtendedGraphPlot, msgName], args];
  Throw[$Failed, ExtendedGraphPlottingFunction]
);

$numOrNumPairP = _ ? NumericQ | {_ ? NumericQ, _ ? NumericQ};

ExtendedGraphPlottingFunction[graph_Graph] := Scope @ Catch[

  GPPrint["ExtendedGraphPlottingFunction for ", graphSkeleton @ graph];

  (* process options *)
  FunctionSection[

    UnpackAnonymousThemedOptions[graph, Automatic,
      imageSize, vertexSize, vertexStyle, edgeStyle,
      vertexLabelStyle, edgeLabelStyle, vertexFontSize,
      imagePadding, vertexShapeFunction, edgeShapeFunction, frameStyle, baselinePosition
    ];

    UnpackAnonymousThemedOptions[graph, None,
      vertexLabels, edgeLabels, plotLabel, prolog, epilog,
      plotRange, plotRangePadding, frame, frameLabel
    ];

    UnpackExtendedOptions[graph,
      vertexColors
    ];

    UnpackExtendedThemedOptions[graph,
      arrowheadShape, arrowheadStyle, arrowheadSize, arrowheadPosition, twoWayStyle,
      visibleCardinals, labelCardinals, vertexBackground,
      edgeSetback, edgeThickness, edgeLength, edgeOpacity, arrowheadOpacity,

      vertexColorFunction, vertexColorRules, vertexAnnotations,
        edgeColorFunction,   edgeColorRules,   edgeAnnotations,
      regionColorRules,

      vertexTooltips, edgeTooltips,
      vertexClickFunction, vertexOpacity,

      viewOptions, graphLegend,
      additionalImagePadding, aspectRatioClipping, extendImagePadding,
      prologFunction, epilogFunction,
      useAbsoluteSizes,

      vertexLabelPosition,    edgeLabelPosition,
      vertexLabelSpacing,     edgeLabelSpacing,
      vertexLabelBaseStyle,   edgeLabelBaseStyle,
      vertexLabelOrientation, edgeLabelOrientation,
      vertexLabelRules,       edgeLabelRules,
      vertexLabelFunction,    edgeLabelFunction,

      peripheralVertices, frameFade
    ];
  ];

  (* initial processing of global options *)
  GPPrint["Options processing"];
  FunctionSection[
    cardinalColors = LookupCardinalColors[graph];
    If[!AssociationQ[cardinalColors], failPlot["badcolors", MsgExpr @ cardinalColors]];

    SetNone[vertexAnnotations, <||>];
    SetNone[edgeAnnotations, <||>];
    SetNone[epilog, {}];
    SetNone[prolog, {}];

    $GraphicsBoundingFrame := $GraphicsBoundingFrame = computeBoundingFrame[];

    automaticLegends = <||>;

    (* NOTE: themes should always use ImageSize -> "Edge" -> xxx so that user can override *)
    If[edgeLength === None && imageSize === Automatic, edgeLength = 30];
    If[edgeLength =!= None, imageSize = "Edge" -> edgeLength];

    (* choose a size based on vertex seperation *)
    SetAutomatic[imageSize,
      If[$VertexParts === All && Length[$VertexList] > 1000, Large,
        graphPlotSizeScalingFunction[If[$GraphIs3D, 30, 15] * ($GraphPlotSizeX / $GraphMaxSafeVertexSize)]]];

    If[RuleQ[imageSize],
      {imageWidth, imageHeight} = Match[imageSize,
        Rule["LongestEdge", sz:$numOrNumPairP]         :> computeEdgeLengthBasedImageSize[1.0, sz, False],
        Rule["LongestNonLoopEdge", sz:$numOrNumPairP]  :> computeEdgeLengthBasedImageSize[1.0, sz, False],
        Rule["ShortestEdge", sz:$numOrNumPairP]        :> computeEdgeLengthBasedImageSize[0.0, sz, False],
        Rule["ShortestNonLoopEdge", sz:$numOrNumPairP] :> computeEdgeLengthBasedImageSize[0.0, sz, True],
        Rule["ClosestVertices", sz:$numOrNumPairP]     :> computeVertexPairBasedImageSize[sz],
        Rule["Edge", sz:$numOrNumPairP]                :> computeEdgeLengthBasedImageSize[0.1, sz, True],
        Rule["MedianEdge", sz:$numOrNumPairP]          :> computeEdgeLengthBasedImageSize[0.5, sz, False],
        Rule["MedianNonLoopEdge", sz:$numOrNumPairP]   :> computeEdgeLengthBasedImageSize[0.5, sz, True],
        Rule["AverageEdge", sz:$numOrNumPairP]         :> computeEdgeLengthBasedImageSize["Average", sz, False],
        Rule["AverageNonLoopEdge", sz:$numOrNumPairP]  :> computeEdgeLengthBasedImageSize["Average", sz, True],
        _                                              :> failPlot["badopt", ImageSize, imageSize]
      ];
    ,
      aspectRatio = $GraphPlotAspectRatio;
      If[aspectRatioClipping, aspectRatio = Clip[$GraphPlotAspectRatio, {0.3, 2}]];
      {imageWidth, imageHeight} = ToNumericImageSize[imageSize, aspectRatio];
    ];

    If[aspectRatioClipping,
      If[$GraphIs3D,
        (* this makes 3D rotation less zoomy *)
        If[imageHeight > imageWidth * 1.5, imageHeight = imageWidth * 1.5];
        If[imageHeight < imageWidth * 0.5, imageHeight = imageWidth * 0.5];
      ];
    ];
    imageSize = {imageWidth, imageHeight};
    {effectiveImageWidth, effectiveImageHeight} = EffectiveImageSize[imageSize, $GraphPlotAspectRatio];

    SetAutomatic[imagePadding, If[MatrixQ[plotRange], None, All]];
    SetNone[imagePadding, extendImagePadding = False; 1];
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
    GPPrint[{
      "ImageSize" -> imageSize, "EffectiveImageSize" -> {effectiveImageWidth, effectiveImageHeight},
      "ImagePadding" -> imagePadding, "AdditionalImagePadding" -> additionalImagePadding
    }];

    If[TrueQ[$returnRawImageSize], Throw[<|
      "ImageSize" -> imageSize, "EffectiveImageSize" -> {effectiveImageWidth, effectiveImageHeight},
      "ImagePadding" -> imagePadding, "AdditionalImagePadding" -> additionalImagePadding
    |>, $rawImageSizeDataTag]];

    edgeCenters = lineCenter /@ $EdgeCoordinateLists;

    edgeStyle //= removeSingleton;
    vertexStyle //= removeSingleton;
    vertexLabelStyle //= removeSingleton;
    edgeLabelStyle //= removeSingleton;
    edgeShapeFunction //= removeSingleton;
    $esfCounter = 1;

    (* these go to custom shape functions, which have different processing *)
    (* TODO: Strip out custom things that ordinary Style wouldn't understand *)
    simpleVertexLabelStyle = vertexLabelStyle;
    simpleEdgeLabelStyle = edgeLabelStyle;
    SetAutomatic[simpleVertexLabelStyle, $DarkGray];
    SetAutomatic[simpleEdgeLabelStyle, $DarkGray];

    processRegionColorRules[regionColorRules];
    processVertexColorRules[vertexColorRules];
    processEdgeColorRules[edgeColorRules];

    (* this allows GraphVertexData and GraphEdgeData to work *)
    setupGraphVertexData[graph, "Coordinates" -> $VertexCoordinates];
    setupGraphEdgeData[graph, "Coordinates" -> $EdgeCoordinateLists];

    shrinkWrap = False;
    If[ListQ @ viewOptions,
      shrinkWrap = Lookup[viewOptions, "ShrinkWrap", False];
      viewRotation = Lookup[viewOptions, ViewRotation, 0];
      If[viewRotation != 0,
        viewPoint = Lookup[viewOptions, ViewPoint, {1.3, -2.4, 2.}];
        viewPoint //= SphericalRotateVector[viewRotation * Pi];
        viewOptions //= ReplaceOptions[ViewPoint -> viewPoint];
      ];
      viewOptions //= DeleteOptions[{"ShrinkWrap", ViewRotation}];
    ];
  ];

  $fadedEdgeParts = None;

  SetAutomatic[peripheralVertices, guessPeripheralVertexCutoff @ VertexDegree @ $Graph];

  If[peripheralVertices =!= None,
    If[$VertexParts === All, $VertexParts ^= Range @ $VertexCount];
    If[$EdgeParts === All, $EdgeParts ^= Range @ $EdgeCount];
    vertexDegrees = VertexDegree @ $Graph;
    Switch[peripheralVertices,
      _Integer | _Real,
        $VertexParts ^= Select[$VertexParts, Part[vertexDegrees, #] > peripheralVertices&],
      {_, _Integer | _Real},
        {centerVerts, maxDistance} = peripheralVertices;
        centerVerts = findVertexIndices[centerVerts];
        If[!IntegerVectorQ[centerVerts], failPlot["badopt", PeripheralVertices, peripheralVertices]];
        distances = Min /@ Part[GraphDistanceMatrix[UndirectedGraph @ $Graph], All, centerVerts];
        $VertexParts ^= Select[$VertexParts, Part[distances, #] <= maxDistance&],
      _,
        failPlot["badopt", PeripheralVertices, peripheralVertices]
    ];
    isFadedPEdge = Function[{a, b}, Count[{MemberQ[$VertexParts, a], MemberQ[$VertexParts, b]}, True] == 1];
    isNonPEdge = Function[{a, b}, MemberQ[$VertexParts, a] || MemberQ[$VertexParts, b]];
    edgePairs = EdgePairs @ ToIndexGraph @ $Graph;
    $fadedEdgeParts = Select[$EdgeParts, isFadedPEdge @@ Part[edgePairs, #]&];
    {$fadedToEdgeParts, $fadedFromEdgeParts} = SelectDiscard[$fadedEdgeParts, MemberQ[$VertexParts, Part[edgePairs, #, 1]]&];
    $EdgeParts ^= Select[$EdgeParts, isNonPEdge @@ Part[edgePairs, #]&];
  ];

  (* create graphics for vertices *)
  GPPrint["Vertex graphics"];
  FunctionSection[

    hasVertexColors = vertexColors =!= None || vertexColorFunction =!= None || vertexColorRules =!= None;

    $vertexEdgeThickness = 1;
    $vertexSizeOverrides = None;
    $defaultVertexSize = Which[
      edgeStyle === None,           Scaled @ 0.6,
      hasVertexColors,              Scaled @ 0.5,
      True,                         Max[0.3, AbsolutePointSize[6]]
    ];
    $inheritedVertexSize = False;
    vertexSize = processVertexSize @ removeSingleton @ vertexSize;
    vertexSizeImage = plotSizeToImageSize @ vertexSize;

    SetAutomatic[vertexShapeFunction, If[hasVertexColors, "Disk", "Point"]];

    lighting = None;

    vertexShapeFunction //= removeSingleton;
    If[vertexShapeFunction === None,
      vertexGraphics = Nothing;
      SetAutomatic[edgeSetback, 0];
      Goto[skipVertices];
    ];

    {defaultVertexColor, vertexBaseStyle, setbackDistance, rawVertexDrawFunc, vertexPadding} =
      processVertexShapeFunction[vertexShapeFunction];
    SetAutomatic[edgeSetback, setbackDistance];
    If[MatchQ[edgeSetback, PointSize[_]], edgeSetback = imageSizeToPlotSize @@ edgeSetback];
    extendPaddingBy[vertexPadding+1];

    vertexDrawFunc = If[$vertexSizeOverrides === None,
      vertexDrawFuncWithSize[rawVertexDrawFunc, vertexSize],
      vertexDrawFuncSizeRemapper[rawVertexDrawFunc, $vertexSizeOverrides, vertexSize]
    ];

    SetAutomatic[vertexStyle, defaultVertexColor];

    GPPrint["Drawing vertices"];
    vertexItems = drawViaColorFunc[
      vertexColors, vertexColorFunction, vertexDrawFunc, $VertexCount, $VertexParts, None,
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
  GPPrint["Edge graphics"];
  FunctionSection[
    If[edgeStyle === None,
      edgeGraphics = Nothing;
      Goto[skipEdges];
    ];

    SetAutomatic[edgeStyle, If[edgeColorFunction =!= None, {}, GrayLevel[0]]];

    SetAutomatic[edgeShapeFunction, "Arrow"];
    If[Or[
      arrowheadShape === None, zeroQ[arrowheadSize], UndirectedGraphQ[$Graph], MatchQ[visibleCardinals, {} | None],
      And[StringQ[edgeShapeFunction], StringEndsQ[edgeShapeFunction, "Line"]]],
      
      arrowheadDrawFn = drawUndirectedEdges;
      arrowheadSize = maxArrowheadSize = 0;
    ,
      SetAutomatic[arrowheadStyle, Which[
        cardinalColors =!= None, cardinalColors,
        vertexColorFunction =!= None, LightGray,
        True, $Gray
      ]];
      If[AssociationQ[arrowheadStyle],
        arrowheadStyle //= MapIndex1[If[#1 =!= Automatic, #1, cardinalColors @ First @ #2]&]];

      baseArrowheadSize := baseArrowheadSize = If[$GraphIs3D, 0.45, 0.8] * (Min[$GraphMaxSafeArrowheadSize / $GraphPlotSizeX, maxMertexSize * 2]);
      arrowheadSize //= processArrowheadSize;
      maxArrowheadSize = Max[arrowheadSize * $GraphPlotSizeX] / 2;
      SetAutomatic[arrowheadShape, If[$GraphIs3D, "Cone", "Line"]];
      If[!$GraphIs3D, arrowheadShape //= to2DArrowheadShape];
      $twoWayStyle = twoWayStyle; $inversionStyle = "Reverse"; $arrowBorderStyle = None;
      $edgeThickness = If[$GraphIs3D, Thickness @ 0.2,
        AbsoluteThickness @ Max[Round[ImageFractionToImageSize[Max[arrowheadSize]] / 10, .2], .5]];
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
      None, edgeColorFunction, arrowheadDrawFn, $EdgeCount, $EdgeParts, $fadedEdgeParts,
      edgeColorDataProvider, EdgeColorFunction
    ];

    If[$fadedEdgeParts =!= None,
      edgeItems = {
        {
          Line[Part[$EdgeCoordinateLists, $fadedFromEdgeParts], VertexColors -> ConstantArray[{Transparent, Opacity[1]}, Length @ $fadedFromEdgeParts]],
          Line[Part[$EdgeCoordinateLists, $fadedToEdgeParts],   VertexColors -> ConstantArray[{Opacity[1], Transparent}, Length @ $fadedFromEdgeParts]]
        },
        edgeItems
      }
    ];

    SetAutomatic[edgeThickness, If[edgeColorFunction =!= None, Thick, If[$GraphIs3D, MediumThick, SlightlyThick]]];
    edgeThickness = OnFailed[
      NormalizeThickness @ edgeThickness,
      failPlot["badthickness", edgeThickness]
    ];

    edgeInfo = <|
      EdgeThickness -> edgeThickness, EdgeStyle -> edgeStyle,
      ArrowheadSize -> toSizeInfo[maxArrowheadSize],
      ArrowheadPosition -> arrowheadPosition
    |>;

    edgeGraphics = Annotation[
      Style[edgeItems, edgeStyle, edgeThickness, If[edgeOpacity === None, Sequence @@ {}, Opacity @ edgeOpacity]],
      edgeInfo, "EdgePrimitivesRoot"
    ];
  ];
  Label[skipEdges];

  (* create labels for vertices and edges *)
  GPPrint["Label graphics"];
  FunctionSection[
    labelGraphics = {};

    (* for label style, support: Opacity, as well as Tiny, Small, etc. Medium corresponds to ordinary size. *)

    vertexLabels //= removeSingleton;
    If[vertexLabels =!= None || vertexTooltips =!= None || vertexLabelRules =!= None || vertexLabelFunction =!= None,
      SetNone[vertexSize, $GraphMaxSafeVertexSize / 5];
      {vertexLabelItems, zorder} = generateLabelPrimitives[
        vertexLabels, vertexLabelRules, vertexLabelFunction, vertexTooltips,
        $VertexList, $VertexCoordinates, $VertexParts,
        vertexSize,
        {vertexLabelStyle, vertexLabelPosition, vertexLabelSpacing, vertexLabelBaseStyle, vertexLabelOrientation},
        vertexAnnotations, True
      ];
      Which[
        zorder == 0, AppendTo[labelGraphics, vertexLabelItems],
        Positive @ zorder, AppendTo[epilog, vertexLabelItems],
        Negative @ zorder, AppendTo[prolog, vertexLabelItems]
      ];
    ];

    edgeLabels //= removeSingleton;
    If[edgeLabels =!= None || edgeTooltips =!= None || edgeLabelRules =!= None || edgeLabelFunction =!= None,
      SetAutomatic[arrowheadSize, 0];
      {edgeLabelItems, zorder} = generateLabelPrimitives[
        edgeLabels, edgeLabelRules, edgeLabelFunction, edgeTooltips,
        $EdgeList, edgeCenters, $EdgeParts,
        Max[arrowheadSize] * $GraphPlotSizeX,
        {edgeLabelStyle, edgeLabelPosition, edgeLabelSpacing, edgeLabelBaseStyle, edgeLabelOrientation},
        edgeAnnotations, False
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
  GPPrint["Final assembly"];
  FunctionSection[
    
    If[labelGraphics =!= Nothing, applyLabelPadding[labelGraphics, vertexLabelStyle]];
    
    (* for graphs with cardinals, create an automatic legend when asked *)
    If[cardinalColors =!= None && arrowheadShape =!= None,
      legendCardinals = If[ListQ[visibleCardinals], KeyTake[cardinalColors, visibleCardinals], cardinalColors];
      orient = If[FreeQ[graphLegend, Placed[Automatic | "Cardinals", Above|Below]], Vertical, Horizontal];
      automaticLegends["Cardinals"] := ArrowheadLegend[cardinalColors,
        ArrowheadShape -> If[ListQ[arrowheadShape], First @ arrowheadShape, arrowheadShape],
        Orientation -> orient
      ];
    ];

    If[colorRules =!= None,
      KeyDropFrom[automaticLegends, "Colors"]];

    interiorImageSize = imageSize;
    imagePadding += additionalImagePadding;
    imagePadding //= Ceiling;
    imageSize = Ceiling @ ImageSizePad[imageSize, imagePadding];

    graphicsElements = {edgeGraphics, vertexGraphics, labelGraphics};

    If[frameFade =!= None,
      If[!IntegerQ[frameFade], frameFade = 5];
      polygon = FrameFadePolygon[$GraphicsBoundingFrame, frameFade];
      AppendTo[epilog, Annotation[Style[polygon, EdgeForm @ None], "FrameFade"]];
    ];

    If[TrueQ @ frame,
      If[MatchQ[frameStyle, {} | Automatic], frameStyle = LightGray];
      frameStyle //= toDirective;
      If[$GraphIs3D,
        cuboid = Cuboid @@ $GraphicsBoundingFrame;
        AppendTo[epilog, Annotation[Style[cuboid, FaceForm @ None, EdgeForm @ frameStyle], "Frame"]];
      ,
        rectangle = Rectangle @@ $GraphicsBoundingFrame;
        orectangle = Rectangle @@ MapThread[Offset, {{{-1, -1}, {1, 1}}, $GraphicsBoundingFrame}];
        AppendTo[epilog, Annotation[{
          Style[orectangle, FaceForm @ None, EdgeForm @ White, AbsoluteThickness[3]], (* <- prevents sticking through of main graphics *)
          Style[rectangle, FaceForm @ None, EdgeForm @ frameStyle]}, "Frame"]];
        AppendTo[prolog, Annotation[Style[rectangle, FaceForm @ White, EdgeForm @ None], "FrameBackground"]];
      ];
    ];

    externalLabel = None;
    If[frameLabel =!= None,
      If[$GraphIs3D,
        externalLabel = processFrameLabel3D @ frameLabel;
      ,
        frameLabelElements = processFrameLabel @ frameLabel;
        AppendTo[prolog, frameLabelElements];
      ];
    ];

    If[ContainsQ[graphicsElements, _UniqueLabel],
      graphicsElements //= processUniqueLabels];

    extraOptions = If[!$GraphIs3D, {},
      SetAutomatic[viewOptions, $automaticViewOptions];
      viewOptions
    ];

    baselinePosition //= processBaselinePosition;

    (* compute prolog and epilog *)
    If[prologFunction =!= None, AppendTo[prolog, prologFunction[$Graph]]];
    If[epilogFunction =!= None, AppendTo[epilog, epilogFunction[$Graph]]];

    If[shrinkWrap && $GraphIs3D,
      hullIndices = ConvexHullPointIndices[$VertexCoordinates];
      hullPoints = Part[$VertexCoordinates, hullIndices];
      mean = Mean @ hullPoints;
      spherePoints = Tuples[{-1, 0, 1}, 3] * imageSizeToPlotSize[Max[imagePadding]];
      hullPoints = Catenate[PlusVector[spherePoints, #]& /@ hullPoints];
      graphicsElements = {Style[Point @ hullPoints, GrayLevel[1, .99], AbsolutePointSize[0]], graphicsElements};
    ];

    (* assemble graphics *)
    graphics = If[$GraphIs3D, makeGraphics3D, makeGraphics][
      graphicsElements,
      imageSize, {effectiveImageWidth, effectiveImageHeight}, imagePadding, plotLabel, extraOptions,
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

    Switch[useAbsoluteSizes,
      True,
        graphics //= ReplaceAll[PointSize[sz_] :> AbsolutePointSize[sz * effectiveImageWidth]],
      False,
        graphics //= ReplaceAll[AbsolutePointSize[sz_] :> PointSize[sz / effectiveImageWidth]],
      _,
        Null
    ];

    If[vertexClickFunction =!= None && !$GraphIs3D,
      With[
        {nearest = Nearest[$VertexCoordinates -> $VertexList], vsize = vertexSize, vclick = vertexClickFunction},
        graphics = EventHandler[graphics, {"MouseClicked" :> Replace[
            nearest[MousePosition["Graphics"], {1, vsize}],
            {\[FormalV]_} :> vclick[\[FormalV]]
          ]}
        ];
      ];
    ];

    applyExternalLabel[
      applyAutomaticLegends[graphics, automaticLegends, graphLegend],
      externalLabel
    ]
  ]
,
  ExtendedGraphPlottingFunction
];


guessPeripheralVertexCutoff[degrees_] := Scope[
  counts = Counts @ degrees;
  counts = TakeLargest[counts, UpTo @ 2];
  countCutoff = Max[counts] / 3;
  counts = Discard[counts, LessEqualThan[countCutoff]];
  Max[Keys[counts]] - 0.5
]

applyExternalLabel[graphics_, None] := graphics;

applyExternalLabel[graphics_, labeled_Labeled] :=
  ReplacePart[labeled, 1 -> graphics];

(* in plot range *)
toSizeInfo[sz_] := <|
  "PlotRange" -> sz,
  "ImageSize" -> sz * effectiveImageWidth,
  "Fraction" -> sz / $GraphPlotSizeX
|>;

processFrameLabel3D = Case[
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

processFrameLabel = Case[
  label:Rule[Bottom|Top, _] :=
    % @ List @ label;
  rules:{Rule[Bottom|Top, _]...} := Scope[
    {bLabel, tLabel} = Lookup[rules, {Bottom, Top}, None];
    labels = List[
      makeFrameLabelElement[bLabel, Bottom, 1],
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
frameLabelSize[label_] := 1 + CachedRasterSize[Text @ label] / 2;

makeFrameLabelElement[None, _, _] := None;

makeFrameLabelElement[label_, pos1_, pos2_] :=
  Text[
    LabelForm[label],
    GraphicsValue[{"BoundingFrame", pos1}],
    {0, pos2}, Background -> White
  ];

ExtendedGraphPlot::badbaseline = "Could not resolve ``.";

processBaselinePosition = Case[
  GraphOrigin :=
    % @ If[$GraphOrigin =!= None, $GraphOrigin, Center];
  Automatic | None | Center  := % @ Scaled[0.5, "Interior"];
  Bottom  := % @ Scaled[0.0, "Interior"];
  Top     := % @ Scaled[1.0, "Interior"];
  spec_ -> outer:Top|Bottom|Center|Baseline :=
    %[spec] -> outer;
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
  {{l, r}, {b, t}} = padding / imageWidth * $GraphPlotSizeX;
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

computeEdgeLengthBasedImageSize[q_, {edgeSize_, maxWidth_}, ignoreLoops_] := Scope[
  {w, h} = size = computeEdgeLengthBasedImageSize[q, edgeSize, ignoreLoops];
  If[w > maxWidth, size * (maxWidth / w), size]
];

computeEdgeLengthBasedImageSize[q_, edgeSize_, ignoreLoops_] := Scope[
  quantile = EdgeLengthScale[If[ignoreLoops, deleteLoops, Identity] @ $EdgeCoordinateLists, q];
  scaling = edgeSize / quantile;
  If[$GraphIs3D, scaling *= 1.5];
  Max[#, 10]& /@ N[Take[$GraphPlotSize, 2] * scaling]
];

computeVertexPairBasedImageSize[pairSize_] := Scope[
  dists = DistanceMatrix[$VertexCoordinates];
  minDist = Min @ Table[Min @ Drop[Part[dists, n], n], {n, 1, Length[$VertexCoordinates]-1}];
  scaling = pairSize / minDist;
  Max[#, 10]& /@ N[Take[$GraphPlotSize, 2] * scaling]
];

deleteLoops[coords_] := Discard[coords, First[#] == Last[#]&];

imageSizeToImageFraction[sz_] := sz / effectiveImageWidth;
ImageFractionToImageSize[sz_] := sz * effectiveImageWidth;
imageSizeToPlotSize[sz_] := sz / effectiveImageWidth * $GraphPlotSizeX;
imageFractionToPlotSize[sz_] := sz * $GraphPlotSizeX;
plotSizeToImageFraction[sz_] := sz / $GraphPlotSizeX;
plotSizeToImageSize[sz_] := sz / $GraphPlotSizeX * effectiveImageWidth;
plotSizeToDiskSize[sz_] := If[$GraphIs3D, Scaled[sz / $GraphPlotScale], Scaled[sz * {1, $GraphPlotAspectRatio} / $GraphPlotScale]];
plotSizeToPointSize[sz_] := PointSize[sz / $GraphPlotSizeX];

extendPaddingBy[n_] /; TrueQ[extendImagePadding] := imagePadding = MatrixMap[Max[#, n]&, imagePadding];
extendPaddingBy[padding_List] /; TrueQ[extendImagePadding] := imagePadding = MapThread[Max, {imagePadding, padding}, 2];

extendPaddingToInclude[{{xmin_, xmax_}, {ymin_, ymax_}}] /; TrueQ[extendImagePadding] := Scope[
  {{pxmin, pxmax}, {pymin, pymax}} = $GraphPlotRange;
  extra = {{pxmin - xmin, xmax - pxmax}, {pymin - ymin, ymax - pymax}};
  extendPaddingBy @ plotSizeToImageSize @ extra
];

extendPaddingToInclude[{{xmin_, xmax_}, {ymin_, ymax_}, {zmin_, zmax_}}] /; TrueQ[extendImagePadding] := Scope[
  {{pxmin, pxmax}, {pymin, pymax}, {pzmin, pzmax}} = $GraphPlotRange;
  extra = {{pxmin - xmin, xmax - pxmax}, {pymin - ymin, ymax - pymax}, {pzmin - zmin, zmax - pzmax}};
  extendPaddingBy @ Max @ plotSizeToImageSize @ extra
];

(**************************************************************************************************)

PublicHead[CardinalColor]

SetUsage @ "CardinalColor[c$] represents the color of cardinal c$."

PrivateFunction[evalCardinalColor]

evalCardinalColor[CardinalColor[c_]] :=
  LookupCardinalColors[$Graph, If[ListQ[c], CardinalSet @ DeleteDuplicates @ Map[StripInverted, c], c]];

(**************************************************************************************************)

PrivateFunction[evalGraphicsValue]
PublicHead[GraphicsValue]

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

evalGraphicsValue = Case[

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
    Part[$EdgeCoordinateLists, findEdgeList @ edge] //
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

findVertexList = Case[
  All := Range @ $VertexCount;
  spec_List := Map[findVertex, spec];
  spec_ := ToList @ findVertex @ spec;
];

findEdgeList = Case[
  All := Range @ $EdgeCount;
  spec_List := Catenate @ Map[findEdge, spec];
  spec_ := ToList @ findEdge @ spec
];

applyGVFunc[s_String, a_] := Lookup[a, s];
applyGVFunc[i_Integer, a_] := Part[a, i];
applyGVFunc[f_, a_] := f[a];

postProcPrims[All][vals_] := vals;

postProcPrims[spec_ ? GraphRegionElementQ][vals_] :=
  If[vals === {}, failPlot["gvnoprim", spec], vals];

postProcPrims[spec_List][vals_] := vals;

postProcPrims[spec_][vals_] :=
  First[vals, failPlot["gvnoprim", spec]];

extractCardinalArrowhead[c_][prims_] := ReplaceAll[
  Style[prims, Transparent],
  Annotation[g_, Except[c | Inverted[c]], "Cardinal"] :> {}
]

computeCenterPoint[{{xl_, yl_}, {xh_, yh_}}, side_] := Scope[
  xm = Avg[xl, xh]; ym = Avg[yl, yh];
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

PublicFunction[FadeProtected]

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

ExtendedGraphPlot::badcolorruleres = "`` did not resolve to valid colors."
ExtendedGraphPlot::badcolorrules = "`` -> `` is not a list of rules."

processRegionColorRules[None] := Null;
processRegionColorRules[rules:$RuleListPattern] := (
  {vertexColorFunction, edgeColorFunction} = resolveRegionRules[rules, RegionColorRules];
  If[!ColorVectorQ[vertexColorFunction] || !ColorVectorQ[edgeColorFunction],
    failPlot["badcolorruleres", RegionColorRules]];
  SetAutomatic[arrowheadStyle, Inherited];
);
processRegionColorRules[r_] := failPlot["badcolorrules", RegionColorRules, r];

processVertexColorRules[None] := Null;
processVertexColorRules[rules_] :=
  vertexColorFunction = resolveColorRules[VertexColorRules, $VertexList, rules, $Gray];

processEdgeColorRules[None] := Null;
processEdgeColorRules[rules_] := (
  edgeColorFunction = resolveColorRules[EdgeColorRules, $EdgeList, rules, $Gray];
  SetAutomatic[arrowheadStyle, Inherited];
);

resolveColorRules[head_, _, r_, _] := failPlot["badcolorrules", head, r];
resolveColorRules[head_, elements_, rules:$RuleListPattern, default_] := Scope[
  defaultColor = Replace[All, Append[rules, _ -> default]];
  rules = Append[rules, _ -> defaultColor];
  colors = VectorReplace[elements, rules];
  If[!ColorVectorQ[colors], failPlot["badcolorruleres", head]];
  AssociationThread[elements, colors]
];

(**************************************************************************************************)

drawUndirectedEdges[indices_, style_] := Scope[
  edgeStyle = style;
  Style[createEdgePrimitives[indices, multiLine, None, None], style]
];

(**************************************************************************************************)

ExtendedGraphPlot::badarrowheadsize = "ArrowheadSize -> `` is not a valid specification."

(* the resulting size is expressed in the coordinate system of the size specification for Arrowheads[...],
which is a fraction of the image width *)
processArrowheadSize = Case[
  s:$SymbolicSizePattern          := imageSizeToImageFraction[4. * Lookup[$SymbolicPointSizes, s]];
  r_ ? NQ                         := N[imageSizeToImageFraction[r]];
  PointSize[sz_ ? NQ]             := N[sz];
  AbsolutePointSize[sz_ ? NQ]     := N[imageSizeToImageFraction[sz]];
  Scaled[r_ ? NQ]                 := baseArrowheadSize * N[r];
  Scaled[s:$SymbolicSizePattern]  := baseArrowheadSize * Lookup[$SymbolicSizeFractions, s];
  assoc_Association               := Map[%, assoc];
  Automatic                       := baseArrowheadSize;
  spec_                           := failPlot["badarrowheadsize", spec];
  {NQ -> NumericQ, sp -> $SymbolicSizePattern}
];

filterCardinals[cards_][CardinalSet[set_List]] := CardinalSet @ Select[set, MemberQ[cards, Inverted[#] | #]&];
filterCardinals[cards_][card_] := If[MemberQ[cards, card] || AnyTrue[cards, MatchQ[card, #]&], card, Null];

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

  If[style =!= None, Style[edgePrimitives, style], edgePrimitives]
];

(**************************************************************************************************)

drawArrowheadEdges[_, {}, _] := Nothing;

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

drawArrowheadEdges[CardinalSet[{c_}], indices_] :=
  drawArrowheadEdges[c, indices];

drawArrowheadEdges[cs:CardinalSet[cardinals_], indices_] := Scope[
  If[$twoWayStyle === Automatic, $twoWayStyle ^= arrowheadShape <> "DoubleIn"];
  If[$twoWayStyle =!= None,
    cardinals = SortBy[cardinals, InvertedQ];
    cardinals //= ReplaceRepeated[
      {l___, c_, m___, Inverted[c_], r___} :> {l, TwoWay[c], m, r}
    ]
  ];
  cardinals = SortBy[cardinals, {Head[#] === TwoWay, #}&];
  num = Length @ cardinals;
  positions = Map[
    Replace[
      lookupTagSpec[arrowheadPosition, #, 0.5],
      {p1_, p2_} :> If[InvertedQ[#], p2, p1]
    ]&,
    cardinals
  ];
  If[SameQ @@ positions,
    positions = makeMultiarrowheadPositions[num, arrowheadPosition]];
  arrowheads = arrowheadsND @ MapThread[
    {card, pos} |-> ReplacePart[makeArrowheadsElement[card], 2 -> pos],
    {cardinals, positions}
  ];
  createEdgePrimitives[indices, multiArrow, arrowheads, cs]
];

makeMultiarrowheadPositions[num_, Around[mn_, sd_]] := Scope[
  p = mn + sd * Standardize[Range @ num];
  {min, max} = MinMax @ p;
  Which[
    max > 1, p - (max - 1),
    min < 0, p - min,
    True, p
  ]
];

makeMultiarrowheadPositions[1, pos_] := {pos};
makeMultiarrowheadPositions[num_, _] := 0.5 + 0.2 * Standardize[Range @ num];

PublicOption[TwoWayStyle, BorderStyle]

scanArrowheadShapeOpts = Case[

  TwoWayStyle -> s:("Out"|"In"|"OutClose"|"InClose") :=
    $twoWayStyle = arrowheadShape <> "Double" <> s;

  TwoWayStyle -> s:("Square"|"Ball"|"Disk"|"Diamond"|"CrossLine"|"CrossBar"|"Tube"|None) :=
    $twoWayStyle = s;

  EdgeThickness -> thickness_ :=
    $edgeThickness = OnFailed[
      NormalizeThickness @ thickness,
      failPlot["badthickness", thickness]
    ];

  InversionStyle -> s:("Reverse"|"OverBar"|"UnderBar") :=
    $inversionStyle = s;

  BorderStyle -> s_ :=
    $arrowBorderStyle = s;

  rule_ :=
    failPlot["badsubopt", rule, commaString @ {TwoWayStyle, InversionStyle, EdgeThickness}];
];

(**************************************************************************************************)

PrivateVariable[$edgeThickness, $vertexEdgeThickness, $arrowBorderStyle]

$vertexEdgeThickness = 1;
$edgeThickness = AbsoluteThickness[1.2];
$arrowBorderStyle = None;

(**************************************************************************************************)

createEdgePrimitives[indices_, drawFn_, arrowheads_, cardinal_] /; StringQ[edgeShapeFunction] := Scope[
  coords = Part[$EdgeCoordinateLists, indices];
  styleFn = If[StringStartsQ[edgeShapeFunction, "Styled"],
    StyleOperator[edgeStyle, arrowheads],
    StyleOperator[arrowheads]
  ];
  primitives = styleFn @ setback[drawFn, edgeSetback] @ coords;
  Annotation[primitives, indices, "EdgePrimitives"]
];

createEdgePrimitives[indices_, drawFn_, arrowheads_, cardinal_] := Scope[
  edgeWeights = GraphEdgeData[All, EdgeWeight];
  maxEdgeWeight = Max @ edgeWeights;
  primitives = Map[
    index |-> applyDrawFn[edgeShapeFunction, <|
      "Coordinates" -> Part[$EdgeCoordinateLists, index],
      "Setback" -> edgeSetback,
      "Source" -> Part[$EdgeList, index, 1],
      "Target" -> Part[$EdgeList, index, 2],
      "EdgeIndex" -> index,
      "Weight" -> Part[edgeWeights, index],
      "WeightFraction" -> Part[edgeWeights, index] / maxEdgeWeight,
      "Counter" :> $esfCounter++,
      "Style" -> edgeStyle,
      "LabelStyle" -> simpleEdgeLabelStyle,
      "Shape" -> If[MatchQ[drawFn, Line | multiLine], Line, Arrow],
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
  {shape, size, style, position} = lookupTagSpec @ cardinal;
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
  If[labelCardinals =!= False, shape = attachArrowheadLabel[shape, cardinal, size, labelCardinals]];
  element = {size, Replace[position, Around[m_, _] :> m], shape};
  If[InvertedQ[cardinal], element //= TransformArrowheads[$inversionStyle]];
  element
];

lookupTagSpec[other_, cardinal_, default_] :=
  Replace[lookupTagSpec[other, cardinal], None -> default];

lookupTagSpec[other_, cardinal_] := other;

lookupTagSpec[assoc_Association, cardinal_] :=
  Lookup[assoc, StripInverted @ cardinal, Lookup[assoc, All, None]];

lookupTagSpec[TwoWay[cardinal_]] :=
  ReplacePart[1 -> $twoWayStyle] @ lookupTagSpec[cardinal];

lookupTagSpec[cardinal_] := fixInvertedPos[cardinal,
  MapThread[
    lookupTagSpec[#1, cardinal, #2]&,
    {{arrowheadShape, arrowheadSize, arrowheadStyle, arrowheadPosition},
     {"Arrow", baseArrowheadSize, Gray, 0.5}}
  ]
];

fixInvertedPos[_, other_] := other;
fixInvertedPos[card_, {a_, b_, c_, {p1_, p2_}}] :=
  {a, b, c, If[InvertedQ @ card, p2, p1]};

(**************************************************************************************************)

attachCardinalAnnotation[head_[primitives_, opts___], cardinal_] :=
  head[Annotation[primitives, cardinal, "Cardinal"], opts];

(**************************************************************************************************)

attachArrowheadLabel[g:Graphics[primitives_, opts___], cardinal_, size_, type_] := Scope[
  cardinal //= Replace[TwoWay[c_] :> c(* Row[{c, Inverted[c]}] *)];
  orient = If[InvertedQ @ cardinal, -1, 1];
  label = makeArrowheadLabel[StripInverted @ cardinal, size];
  {{xl, xh}, {yl, yh}} = GraphicsPlotRange[g];
  Switch[type,
    Center,
      insetPos = {0, 0},
    Below,
      insetPos = Offset[orient * {0, -8}, {0, 0}],
    Before,
      insetPos = Offset[orient * {-8, 0}, {0, 0}],
    True,
      insetPos = Offset[orient * {3, -2}, {0., If[orient == 1, xl - 0.2, xb + 0.2]}];
  ];
  If[type =!= True, label = label /. (Background -> _) -> (Background -> None)];
  labelPrimitives = {Opacity[1], Black, Inset[label, insetPos, {0, 0}, Automatic, None]};
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

applyAutomaticLegends[graphics_, <||>, _] := graphics;

applyAutomaticLegends[graphics_, automaticLegends_, Automatic] :=
  ApplyLegend[graphics, Values @ automaticLegends];

applyAutomaticLegends[graphics_, automaticLegends_, Placed[Automatic, place_]] :=
  ApplyLegend[graphics, Map[Placed[#, place]&, Values @ automaticLegends]];

applyAutomaticLegends[graphics_, automaticLegends_, graphLegend_] := Scope[
  graphLegend = ToList @ graphLegend;
  $auto = automaticLegends;
  legends = removeSingleton @ Map[assembleLegendItem, graphLegend];
  If[ContainsQ[legends, $Failed], Return @ graphics];
  ApplyLegend[graphics, legends]
];

ExtendedGraphPlot::badlegendkey = "`` is not a valid named legend. Valid names are: ``."
ExtendedGraphPlot::badlegendspec = "`` is not a valid legend spec."

assembleLegendItem = Case[
  Automatic                 := First[$auto, $Failed];
  s_String                  := Lookup[$auto, s, Message[ExtendedGraphPlot::badlegendkey, s, Keys @ $auto]; $Failed];
  Placed[s_String, p_]      := Placed[%[s], p];
  Labeled[s_, l_, args___]  := Labeled[%[s], l, args];
  None                      := None;
  e_                        := e;
]

(**************************************************************************************************)

PrivateFunction[removeSingleton]

(* the hold is because some options have DeleteDuplicates applied to them, like Vertex/EdgeLabels *)
removeSingleton[{Hold[e_]}] := e;
removeSingleton[{e_}] := e;
removeSingleton[e_] := e;

(**************************************************************************************************)

ExtendedGraphPlot::badvshapefuncstr = "`` is not a valid named VertexShapeFunction. Valid shapes are: ``."
ExtendedGraphPlot::badvshapefunc = "`` is not a valid VertexShapeFunction."

$validVertexShapes = {"Disk", "Ball", "Point", None};

processVertexShapeFunction[{spec_, EdgeThickness -> e_}] := (
  $vertexEdgeThickness = e;
  processVertexShapeFunction @ spec
);

(* this awkward thing exists because VertexShapeFunction seems to acquire multiple values
when overriden *)
processVertexShapeFunction[{__, last_}] :=
  processVertexShapeFunction[last];

ExtendedGraphPlot::badvshapefunckey = "`` is not a valid verte."

(* because rule isn't supported by Graph *)
processVertexShapeFunction[key_String /* func_] :=
  processVertexShapeFunction[key -> func];

processVertexShapeFunction[key_String -> func_] := (
  annos = getVertexAnnoValue[vertexAnnotations, key];
  processVertexShapeFunction @ Association @
    MapThread[#1 -> func[#2]&, {$VertexList, annos}]
)

processVertexShapeFunction[spec_] := Scope[
  setbackDistance = 0; vertexPadding = 0;
  vertexBaseStyle = None;
  defaultVertexColor = $DarkGray;
  If[StringQ[spec] && KeyExistsQ[vertexAnnotations, spec],
    spec = Lookup[vertexAnnotations, spec];
    spec = AssociationThread[$VertexList, spec];
  ];
  Switch[spec,
    "Disk" /; $GraphIs3D,
      defaultVertexColor = $Gray;
      vertexDrawFunc = mapCoordArray[drawDisk3D[$vertexSize]];
      setbackDistance = vertexSize / 2;
    ,
    "Ball" | "Sphere" /; $GraphIs3D,
      defaultVertexColor = $LightGray;
      vertexSizeImage = 0;
      vertexDrawFunc = drawSphere[$vertexSize / (2 * Sqrt[3.])];
    ,
    "Disk" | "Ball" | "Sphere",
      vertexBaseStyle = EdgeForm @ AbsoluteThickness[.25];
      vertexDrawFunc = drawDisk[$vertexSize / 2];
    ,
    "Point",
      vertexDrawFunc = drawPoint[$vertexSize];
    ,
    "Square",
      vertexDrawFunc = drawSquare[$vertexSize];
      vertexBaseStyle = EdgeForm @ AbsoluteThickness[1];
    ,
    "Hexagon",
      vertexDrawFunc = drawHexagon[$vertexSize];
      vertexBaseStyle = EdgeForm @ AbsoluteThickness[1];
    ,
    _Association,
      additionalImagePadding += 2;
      vertexDrawFunc = drawCustomShape[spec, $vertexSize];
      If[$inheritedVertexSize,
        (* VertexSize -> Inherited means we obtain vertex size *from* the graphics themselves *)
        vertexSizeImage ^= maxCustomVertexShapeSize[spec];
        GPPrint[{"MaxCustomShapeSize" -> vertexSizeImage}];
      ];
    ,
    "Name" | "Vertex",
      vertexDrawFunc = drawOriginalVertices[$vertexSize];
    ,
    _String,
      failPlot["badvshapefuncstr", spec, commaString @ $validVertexShapes];
    ,
    _,
      If[!System`Private`MightEvaluateWhenAppliedQ[spec],
        failPlot["badvshapefunc"]];
      vertexDrawFunc = drawCustomShapeFunction[spec, $vertexSize];
  ];
  vertexPadding = Ceiling[1 + vertexSizeImage / 2];
  {defaultVertexColor, vertexBaseStyle, setbackDistance, vertexDrawFunc, vertexPadding}
];

drawPoint[size_][pos_, color_] :=
  Style[Point @ pos, plotSizeToPointSize @ size, color];

drawSphere[size_][pos_, color_] :=
  Style[Sphere[pos, size], color];

(* Disk doesn't support coordinate arrays *)
drawDisk[size_][pos_, color_] :=
  edgedStyle[color] @ mapCoordArray[Disk, pos, size];

edgedStyle[{}] := Identity;

edgedStyle[color_][primitives_] := Style[
  primitives,
  FaceForm[color], EdgeForm[{AbsoluteThickness @ $vertexEdgeThickness, Darker[color, .3]}]
];

(* drawDisk3D[size_][pos_, color_] := Inset[
  Graphics[drawDisk[1][{0, 0}, color], AspectRatio -> 1],
  pos, {0, 0}, plotSizeToDiskSize @ size
];
 *)
drawDisk3D[size_][pos_, color_] := drawSphere[size][pos, color];

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

drawCustomShape[assoc_, size_][pos_, color_] :=
  Map[drawIndividualCustomShape[assoc, color, size], pos];

drawIndividualCustomShape[assoc_, color_, size_][pos_] := Scope[
  index = IndexOf[$VertexCoordinates, pos];
  shape = If[index === None, None, Part[assoc, index]];
  drawGraphicsWithColor[shape, pos, color, size]
];

(**************************************************************************************************)

drawOriginalVertices[size_][pos_, color_] :=
  Map[drawOriginalVertexWithSize[color, size], pos];

drawOriginalVertexWithSize[color_, size_][pos_] := Scope[
  index = IndexOf[$VertexCoordinates, pos];
  shape = If[index === None, None, Part[$VertexList, index]];
  drawGraphicsWithColor[shape, pos, color, size]
];

(**************************************************************************************************)

drawCustomShapeFunction[fn_, size_][pos_, color_] := Scope[
  vertexWeights = GraphVertexData[All, VertexWeight];
  maxVertexWeight = If[ListQ[vertexWeights], Max @ weights, None];
  Map[drawIndividualCustomShapeFunction[fn, size, color], pos]
];

drawIndividualCustomShapeFunction[fn_, size_, color_][pos_] := Scope[
  index = IndexOf[$VertexCoordinates, pos];
  vertex = Part[$VertexList, index];
  vertexWeight = If[ListQ[vertexWeights], Part[vertexWeights, index], None];
  $vertexDataInfo = <|
    "Coordinates" -> pos,
    "VertexIndex" -> index,
    "Vertex" -> vertex,
    "Size" -> size,
    "Weight" -> vertexWeight,
    "WeightFraction" -> vertexWeight / maxVertexWeight,
    "PointSize" -> plotSizeToPointSize @ size,
    "Color" -> removeSingleton @ color,
    "LabelStyle" -> simpleVertexLabelStyle,
    "Annotations" -> Part[vertexAnnotations, All, index]
  |>;
  Check[
    res = fn[$vertexDataInfo],
    $Failed
  ];
  res = res /. cc_CardinalColor :> RuleCondition @ evalCardinalColor @ cc;
  If[Head[res] === Form, res = Text[res, pos, {0, 0}, Background -> vertexBackground]];
  If[MatchQ[res, _Graph | _Graphics], res = drawGraphicsWithColor[res, pos, color, size]];
  If[res === None, Return @ {}];
  If[res === $Failed, failPlot["vertexfnmsg", vertex]];
  res
];

ExtendedGraphPlot::vertexfnmsg = "VertexShapeFunction issued messages when applied to ``."

(**************************************************************************************************)

drawGraphicsWithColor[None, pos_, color_, size_] :=
  drawPoint[size][pos, color];

drawGraphicsWithColor[other_, pos_, color_, size_] :=
  Text[
    Style[other, color, FontSize -> Replace[vertexFontSize,
      None|Automatic :> Max[size * effectiveImageWidth * 0.8, 8]]
    ],
    pos, {0, 0}, Background -> vertexBackground
  ];

drawGraphicsWithColor[g_Graph, pos_, color_, size_] :=
  drawGraphicsWithColor[ExtendedGraphPlot[g, BaselinePosition -> Center, Frame -> True, ImagePadding -> 10], pos, color, size];

drawGraphicsWithColor[g_Graphics, pos_, color_, size_] :=
  Inset[
    ReplaceOptions[g, FrameStyle -> color],
    pos, {Center, Baseline},
    If[$inheritedVertexSize,
      Automatic, (* <- user gave VertexSize -> Inherited, so use graphic's natural size *)
      toAspectSize[LookupImageSize @ g, StandardizePadding @ LookupOption[g, ImagePadding], size]
      (* match the vertex size with the graphic's aspect ratio *)
    ]
  ];

toAspectSize[w_, _, max_] :=
  toAspectSize[w, {{0, 0}, {0, 0}}, max];

toAspectSize[{w_, Automatic},  {{l_, r_}, {b_, t_}}, max_] :=
  w * (max / (w - l - r));

toAspectSize[{w_, h_}, {{l_, r_}, {b_, t_}}, max_] :=
  {w, h} * (max / Max[w - l - r, h - b - t])

(**************************************************************************************************)

maxCustomVertexShapeSize[shapes_] := Max[guessCustomShapeSize /@ shapes] / 2;

guessCustomShapeSize[shape_] := CachedRasterSize @ Replace[
  drawGraphicsWithColor[shape, {0, 0}, Black, vertexSize],
  Inset[e_, ___] :> e
];

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

setback[Line|multiLine, dist_] := multiLine[Which[
  CoordinateMatrixQ[#], setbackCoords[dist][#],
  CoordinateArrayQ[#], Map[setbackCoords[dist], #],
  True, #]]&

PrivateFunction[setbackCoords]

setbackCoords[0|0.|{0|0.,0|0.}][line_] := line;

setbackCoords[dist_ ? NumericQ][{a_, b_}] /; (EuclideanDistance[a, b] > 2 * dist) := Scope[
  dx = Normalize[b - a] * dist;
  {a + dx, b - dx}
];

setbackCoords[{da_ ? NumericQ, db_ ? NumericQ}][{a_, b_}] /; (EuclideanDistance[a, b] > (da + db)) := Scope[
  dx = Normalize[b - a];
  {a + dx * da, b - dx * db}
];

setbackCoords[dist_][other_] := SetbackCoordinates[other, dist];

insetDisk[size_][pos_] := Inset[Graphics[Disk[{0, 0}, 1], AspectRatio -> 1], pos, {0, 0}, size];
(id:insetDisk[size_])[pos_ ? CoordinateMatrixQ] := Map[id, pos];

(**************************************************************************************************)

drawViaColorFunc[colors_, colorFn_, drawFn_, count_, parts_, fadedParts_, dataProviderFn_, optSymbol_] := Scope[

  If[colorFn === None && colors === None,

    result = If[fadedParts === None,
      drawFn[Part[Range @ count, parts], {}]
    ,
      range = Range @ count;
      unfadedParts = Complement[parts, fadedParts];
      {
        drawFn[Part[range, unfadedParts], {}],
        semitransparentArrowheads @ drawFn[Part[range, fadedParts], Transparent]
      }
    ];

    Return @ SimplifyGraphicsPrimitives @ result;
  ];

  colorGroupFn = If[MatchQ[colorFn, Tooltip[_]],
    colorFn = First @ colorFn;
    toColorDrawFuncWithTooltip[drawFn],
    toColorDrawFunc[drawFn]
  ];

  If[ListQ[colors], colors = PadRight[colors, count, $Gray]];

  {colorList, colorGroups, colorFunctionObject} = obtainColors[colors, colorFn, dataProviderFn, optSymbol];
  If[!MatchQ[colorFunctionObject, ColorFunctionObject["Discrete", Identity]],
    automaticLegends["Colors"] ^= colorFunctionObject];

  colorGroups //= filterAssocIndices[parts];

  SimplifyGraphicsPrimitives @ KeyValueMap[colorGroupFn, colorGroups]
];

obtainColors[colors_, colorFn_, dataProviderFn_, optSymbol_] := Scope[
  palette = Automatic;
  colorFn //= Replace[Paletted[d_, p_] :> (palette = p; d)];

  If[colors =!= None && colorFn === None,
    colorData = colors;
  ,
    colorData = Check[dataProviderFn @ colorFn, $FailedMessages];
  ];

  If[FailureQ[colorData] || colorData === None, failPlot["badcolfn", optSymbol]];
  If[colorData === $FailedMessages, failPlot["msgcolfn", optSymbol]];

  {colorList, colorGroups, colorFunctionObject} = ApplyColoring[colorData, palette];
  If[FailureQ[colorFunctionObject], failPlot["badcolvals", optSymbol]];

  {colorList, colorGroups, colorFunctionObject}
]

ExtendedGraphPlot::badcolfn = "Setting of `` is not a function that will evaluate to produce colors."
ExtendedGraphPlot::badcolvals = "Setting of `` did not produce values that could be colored."
ExtendedGraphPlot::msgcolfn = "Applying color function specified for `` gave messages."

toColorDrawFunc[drawFn_] :=
  {colorValue, indices} |-> drawFn[indices, First @ colorValue];

toColorDrawFuncWithTooltip[drawFn_] :=
  {colorValue, indices} |-> NiceTooltip[
    drawFn[indices, First @ colorValue],
    Last @ colorValue
  ];

(**************************************************************************************************)

edgeColorDataProvider = Case[

  "Name"                    := $EdgeList;
  
  "Index"                   := Range @ $EdgeCount;
  
  "Cardinal"                := Map[getCardinalEdgeColor, $EdgeTags];

  list_List /; Length[list] === $EdgeCount := list;

  assoc_Association         := Lookup[assoc, $EdgeList, Lookup[assoc, All, $LightGray]];

  "Random"                  := RandomChoice[$ColorPalette, $EdgeCount];

  fn_ ? System`Private`MightEvaluateWhenAppliedQ := Map[fn, $EdgeList];

  _                         := $Failed
];

getCardinalEdgeColor = Case[
  card_ := Lookup[cardinalColors, card, $LightGray];
  CardinalSet[cards_] := OklabBlend @ DeleteDuplicates @ Lookup[cardinalColors, StripInverted @ cards, Nothing];
  Null | None := $LightGray;
];

(**************************************************************************************************)

PublicFunction[LookupVertexColors]

LookupVertexColors[graph_Graph, vertices_:All] := Scope[
  GraphScope[graph,
    UnpackExtendedThemedOptions[graph, vertexColorFunction, vertexColorRules];
    UnpackExtendedOptions[graph, vertexColors];
    processVertexColorRules[vertexColorRules];
    If[vertices =!= All, $VertexList = vertices];
    If[vertexColorFunction === None && vertexColors === None, Return @ None];
    {colorList, colorGroups, colorFunctionObject} = obtainColors[
      vertexColors, vertexColorFunction, vertexColorDataProvider, VertexColorFunction
    ];
    AssociationThread[$VertexList, colorList]
  ]
]

(**************************************************************************************************)

PublicFunction[LookupEdgeColors]

LookupEdgeColors[graph_Graph, edges_:All] := Scope[
  GraphScope[graph,
    UnpackExtendedOptions[graph, edgeColorFunction, edgeColorRules];
    cardinalColors := cardinalColors = LookupCardinalColors @ graph;
    processEdgeColorRules[edgeColorRules];
    If[edges =!= All, $EdgeList = edges];
    If[edgeColorFunction === None, Return @ None];
    {colorList, colorGroups, colorFunctionObject} = obtainColors[
      None, edgeColorFunction, edgeColorDataProvider, EdgeColorFunction
    ];
    AssociationThread[$EdgeList, colorList]
  ]
]

(**************************************************************************************************)

ExtendedGraphPlot::notvertex = "`` is not a valid vertex of the graph."

getVertexIndex[GraphOrigin] := getVertexIndex @ $GraphOrigin;
getVertexIndex[v_] := Lookup[$VertexIndex, Key @ v, failPlot["notvertex", v]];

getVertexAnnoValue[annos_, "Index"] := Range @ $VertexCount;
getVertexAnnoValue[annos_, n:"Name" | "Vertex"] /; !KeyExistsQ[annos, n] := $VertexList;
getVertexAnnoValue[annos_, key_] := Lookup[annos, key, failPlot["badgraphannokey", key, commaString @ Keys @ annos]];

vertexColorDataProvider = Case[

  "Name"                    := $VertexList;

  "Index"                   := Range @ $VertexCount;

  (* todo, make the distance work on regions as well *)
  "Distance"                := %[{"Distance", GraphOrigin}];
  {"Distance", v_}          := MetricDistance[$MetricGraphCache, getVertexIndex @ v];

  key_String                := getVertexAnnoValue[vertexAnnotations, key];
  (key_String -> f_)        := Map[toFunc @ f, %[key]];

  list_List /; Length[list] === $VertexCount := list;

  assoc_Association         := Lookup[assoc, $VertexList, Lookup[assoc, All, $LightGray]];

  "Random"                  := RandomChoice[$ColorPalette, $VertexCount];

  fn_ ? System`Private`MightEvaluateWhenAppliedQ := Map[fn, $VertexList];

  _                         := $Failed;
];

toFunc[i_Integer] := Extract[i];
toFunc[f_] := f;

(**************************************************************************************************)

resolveRegionRules[rules_, optSym_, default_:Gray] := Scope[
  defaultColor = FirstCase[rules, Rule[All, color:$ColorPattern] :> color, default, {1}];
  $vertexValues = ConstantArray[defaultColor, $VertexCount];
  $edgeValues = ConstantArray[defaultColor, $EdgeCount];
  $optSym = optSym;
  $regionColor = $Red;
  Scan[applyRegionRule, rules];
  {$vertexValues, $edgeValues}
];

applyRegionRule = Case[
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

processVertexSize = Case[
  Automatic                          := % @ $defaultVertexSize;
  r_ ? NumericQ                      := % @ AbsolutePointSize @ r;
  sym:$SymbolicSizePattern           := % @ AbsolutePointSize @ $SymbolicPointSizes @ sym;
  PointSize[sz_ ? NumericQ]          := N @ imageFractionToPlotSize @ sz;
  AbsolutePointSize[sz_ ? NumericQ]  := N @ imageSizeToPlotSize @ sz;
  Scaled[r_ ? NumericQ]              := N[r * $GraphMaxSafeVertexSize];
  Scaled[sym:$SymbolicSizePattern]   := % @ Scaled @ $SymbolicSizeFractions @ sym;
  m_Max | m_Min                      := Map[%, m];
  rule:$RulePattern                  := % @ {rule};
  rules_Association                  := % @ Normal @ rules;
  rules:$RuleListPattern             := (
    $vertexSizeOverrides = Association[processVertexSizeRule /@ rules];
    processVertexSize @ Lookup[rules, Key @ All, Automatic]
  );
  Inherited                          := ($inheritedVertexSize = True; % @ Automatic);

  other_                             := failPlot["badvertexsize", other];
];

processVertexSizeRule[All -> _] :=
  Nothing;

processVertexSizeRule[(Rule|RuleDelayed)[VertexPattern[p_], rhs_]] := Scope[
  pos = Flatten @ Position[$VertexList, p, {1}];
  Splice @ RuleThread[pos, VectorReplace[Part[$VertexList, pos], p :> processVertexSize[rhs]]]
];

processVertexSizeRule[lhs_ -> rhs_] :=
  findVertex[lhs] -> processVertexSize[rhs];

(**************************************************************************************************)

findEdge = Case[
  spec_ ? GraphRegionElementQ := Last @ processRegionVerticesEdges @ spec;
  edge_                       := (
    IndexOf[$EdgeList, edge /. GraphOrigin :> findVertex[GraphOrigin], failPlot["noedge", edge]]
  );
];

ExtendedGraphPlot::noedge = "Could not find edge ``."

(**************************************************************************************************)

findVertex[IndexedVertex[i_]] := i;
findVertex[v_] := Lookup[$VertexIndex, Key @ v, failPlot["novertex", v]];
findVertex[GraphOrigin] := findVertex @ LookupAnnotation[$Graph, GraphOrigin];

ExtendedGraphPlot::novertex = "`` is not a valid vertex."

(**************************************************************************************************)

applyLabelPadding[graphics_, vertexLabelStyle_] := Scope[
  graphics = graphics /. Text[opts___] :> outerText[opts];
  texts = Cases[graphics, outerText[t_, ___], Infinity];
  If[texts === {}, Return[Null]];
  bboxIndices = BoundingBoxPointIndices @ ReplaceAll[Part[texts, All, 2], Offset[_, p_] :> p];
  texts = Part[texts, bboxIndices];
  allCorners = Map[textCorners, texts];
  cornerBounds = CoordinateBounds @ allCorners;
  extendPaddingToInclude @ cornerBounds
];

textCorners[text:outerText[content_, pos_, align_:{0,0}, ___]] :=
  offsetCorners[pos, imageSizeToPlotSize @ TextRasterSize @ Apply[Text, text], align]

textCorners[_] := Nothing;

offsetCorners[Offset[_, p_], args__] := offsetCorners[p, args];
offsetCorners[p:{p1_, p2_}, s:{s1_, s2_}, o:{o1_, o2_}] := Scope[
  dx = (-o/2 - 1/2) * s;
  PlusVector[{{0, 0}, {0, s2}, {s1, 0}, {s1, s2}}, p + dx]
]
offsetCorners[p_, _, _] := (* 3D *)
  {p};

offsetToPadding[o_, s_] := Switch[Sign[o], 1, {s, 0}, 0, {s, s}/2, -1, {0, s}];

PublicOption[LabelPosition, LabelOrientation]

SetUsage @ "
LabelOrientation is an option that determines how labels are oriented, and can accept the following settings:
| Vertical | align labels vertically |
| Horizontal | align labels horizontally |
| n$ | align labels at n$ degrees |
| 'Aligned' | align labels with the underlying object |
| 'Upright' | align labels with underlying object, but make sure they are never upside down (not implemented) |
"

$uniformPayloadSizes = False;
generateLabelPrimitives[labelSpec_,  labelRules_, labelFn_, tspec_, names_, coordinates_, parts_, size_, {labelStyle_, labelPosition_, labelSpacing_, labelBaseStyle_, labelOrientation_}, annotations_, isVertices_] := Scope[
  $annotationKeys = Keys @ annotations;
  $labelNames = names;
  $labelWeights := $labelWeights = If[isVertices, LookupVertexAnnotations[$Graph, VertexWeight], LookupEdgeAnnotations[$Graph, EdgeWeight]];
  coordinates = Part[coordinates, parts];
  $annotations = annotations;
  $labeledElemSize = size / 2;
  $spacings = 0; $labelZOrder = 0;
  $isVertices = isVertices; $labelFontSize = None; $labelFontWeight = None; $labelFontFamily = None;
  $labelSizeScale = 1; $labelScaledPos = None; $labelY = None; $labelX = None; $labelBackground = GrayLevel[1.0, 0.6];
  $labelBaseStyle = None; $labelOffset = None;
  $adjacencyIndex = None; $meanCoordinates = Mean @ coordinates;
  $labelOrientation = 0;
  If[!(labelPosition === Automatic && ContainsQ[setLabelStyleGlobals, LabelPosition -> _]),
    setLabelStyleGlobals[LabelPosition -> labelPosition]];
  setLabelStyleGlobals[LabelSpacing -> labelSpacing];
  setLabelStyleGlobals[LabelOrientation -> labelOrientation];
  setLabelStyleGlobals[BaseStyle -> labelBaseStyle];
  labelStyle //= toDirectiveOptScan[setLabelStyleGlobals];
  If[$labelX === $labelY === None && labelPosition === None, $labelY = -1];
  SetNone[$labelX, 0]; SetNone[$labelY, 0];
  labelStyle //= DeleteCases[sspec:$SizePattern /; ($labelSizeScale = toNumericSizeScale @ sspec; True)];
  If[$labelFontSize =!= None, PrependTo[labelStyle, FontSize -> $labelFontSize]];
  If[$labelFontWeight =!= None, PrependTo[labelStyle, FontWeight -> $labelFontWeight]];
  If[$labelFontFamily =!= None, PrependTo[labelStyle, FontFamily -> $labelFontFamily]];
  $magnifier = If[$labelSizeScale == 1, Identity, Magnify[#, $labelSizeScale]&];
  {payloadFunction, placerFunction} = processLabelSpec[labelSpec, labelRules, labelFn];
  indices = If[parts === All, Range @ Length @ names, parts];
  labelElements = tooltipElements = Nothing;
  If[payloadFunction =!= None,
    payloads = Map[payloadFunction, indices];
    $uniformPayloadSizes = ArrayQ[payloads];
    labelElements = MapThread[procPayload, {payloads, coordinates, indices}];
    labelElements = Style[labelElements, labelStyle];
  ];
  If[tspec =!= None,
    tooltipPayloadFunction = processTooltipSpec @ tspec;
    tooltipPayloads = Map[tooltipPayloadFunction, indices];
    $uniformPayloadSizes = ArrayQ[tooltipPayloads];
    tooltipElements = MapThread[
      placeTooltipAt[labelForm @ #3, #1, #2]&,
      {coordinates, indices, tooltipPayloads}
    ];
  ];
  elements = removeSingleton @ {labelElements, tooltipElements};
  {elements, $labelZOrder}
];

procPayload = Case[
  Sequence[Null, _, _] := Nothing;
  Sequence[payload_, coord_, ind_] :=
    placerFunction[labelForm @ payload, coord, ind];
  Sequence[Placed[payload_, labelPos_], coord_, ind_] := Scope[
    $labelY = $labelX = None;
    setLabelStyleGlobals[LabelPosition -> labelPos];
    SetNone[$labelX, 0]; SetNone[$labelY, 0];
    %[payload, coord, ind]
  ];
]

toDirectiveOptScan[f_][{Automatic}] :=
  Directive[];

toDirectiveOptScan[f_][e_] :=
  toDirectiveOptScan[f][{e}];

toDirectiveOptScan[f_][e_List | e_Directive] := (
  Scan[f, Cases[e, _Rule]];
  toDirective @ DeleteCases[e, _Rule]
)

ExtendedGraphPlot::badsubopt = "`` is not a recognized suboption. Recognized options are ``."

setLabelStyleGlobals = Case[
  ItemSize -> size:$SizePattern           := $labelSizeScale = toNumericSizeScale @ size;
  FontSize -> sz_                         := $labelFontSize = sz;
  FontWeight -> w_                        := $labelFontWeight = w;
  FontFamily -> f_                        := $labelFontFamily = f;
  Background -> o:$opacityPattern         := $labelBackground = GrayLevel[1.0, toNumericOpacity @ o];
  Background -> None                      := $labelBackground = None;
  Background -> c:$ColorPattern           := $labelBackground = c;
  BaseStyle -> s_                         := $labelBaseStyle = s;
  LabelPosition -> Automatic              := (
    If[$isVertices,
      $labelX = Automatic;
      $labelY = Automatic;
      $adjacencyIndex = VertexAdjacentEdgeTable[$Graph];
    ,
      %[LabelPosition -> Top]
    ]);
  ZOrder -> z_                            := $labelZOrder = z;
  LabelPosition -> Scaled[s_ ? NQ]        := ($labelScaledPos = s; $labelBackground = None);
  LabelPosition -> TopRight               := (%[LabelPosition -> Top]; %[LabelPosition -> Right]);
  LabelPosition -> TopLeft                := (%[LabelPosition -> Top]; %[LabelPosition -> Left]);
  LabelPosition -> BottomRight            := (%[LabelPosition -> Bottom]; %[LabelPosition -> Right]);
  LabelPosition -> BottomLeft             := (%[LabelPosition -> Bottom]; %[LabelPosition -> Left]);
  LabelPosition -> Top|Above              := SetNone[$labelY, -1];
  LabelPosition -> Bottom|Below           := SetNone[$labelY, 1.25];
  LabelPosition -> Center                 := $labelX = $labelY = 0;
  LabelPosition -> "Radial"               := ($labelX = $labelY = "Radial");
  LabelPosition -> "XSign"                := ($labelX = "Sign"; $labelY = 0);
  LabelPosition -> "YSign"                := ($labelX = 0; $labelY = "Sign");
  LabelPosition -> "Aligned"              := ($labelX = $labelY = "Aligned");
  LabelPosition -> {x_ ? NQ, y_ ? NQ}     := ($labelX = N[x]; $labelY = N[y]);
  LabelPosition -> specs:{__}             := Scan[%[LabelPosition -> #]&, specs];
  LabelPosition -> Left                   := SetNone[$labelX, 1.25];
  LabelPosition -> Right                  := SetNone[$labelX, -1.25];
  LabelPosition -> Offset[{x_ ? NQ, y_ ? NQ}] := (
    $labelOffset = {x, y};
    $labelX = Switch[Sign @ x, -1, 1, 0, 0, 1, -1];
    $labelY = Switch[Sign @ y, -1, 1, 0, 0, 1, -1];
  );
  LabelOrientation -> Automatic           := ($labelOrientation = 0);
  LabelOrientation -> (n_ ? NumericQ)     := ($labelOrientation = N @ n);
  LabelOrientation -> Vertical            := ($labelOrientation = 90);
  LabelOrientation -> Horizontal          := ($labelOrientation = 0);
  LabelOrientation -> "Aligned"           := ($labelOrientation = "Aligned");
  Spacings|LabelSpacing -> n_ := $spacings = N[n];

  rule_ := failPlot["badsubopt", rule, commaString @ {ItemSize, FontSize, FontWeight, FontFamily, BaseStyle, Background, ZOrder, LabelPosition, LabelOrientation, Spacings}];
  {NQ -> NumericQ}
];

$keyP = _String | _Association | (_List ? (SameLengthQ[$labelNames]));

processLabelSpec[spec_, None, None] := processLabelSpec1[spec];

processLabelSpec[spec_, None, fn_] :=
  {getName /* fn, placeLabelAt};

processLabelSpec[spec_, {rules___}, fn_] := With[
  {fallbackFn = Which[
    MatchQ[spec, $payloadP], toPayloadFunction @ spec,
    fn =!= None, getName /* fn,
    True, Null&
  ]},
  {payloadFn = Function[ind, Replace[getName[ind], {rules, _ :> fallbackFn[ind]}]]},
  {payloadFn, placeLabelAt}
];

processLabelSpec1 = Case[
  None                          := {None, None};
  Automatic | All               := %["Name"];
  Tooltip                       := %[Placed["Name", Tooltip]];
  p:$payloadP                   := {toPayloadFunction @ p, placeLabelAt};
  Tooltip[p:$payloadP]          := {toPayloadFunction @ p, placeTooltipAt};
  Placed[p:$payloadP, Tooltip]  := {toPayloadFunction @ p, placeTooltipAt};
  other_                        := failPlot["badlabelspec", other];
  {$payloadP -> $keyP | Rule[$keyP, _]}
];

ExtendedGraphPlot::badlabelspec = "The label specification `` was not one of the recognized forms."

ExtendedGraphPlot::badgraphannokey = "The requested annotation `` is not present in the graph. Present annotations are: ``."

ExtendedGraphPlot::msglabelfn = "The label function specification generated messages."

toPayloadFunction = Case[
  "Name"                := getName;
  "Index"               := getIndex;
  "Tag" | "Cardinal"    := getCardinal;
  assoc_Association     := lookupPayloadInAssoc[assoc];
  "Weight"              := getWeight;
  list_List ? (SameLengthQ[$labelNames])
                        := Part[list, #]&;
  key_ -> f_            := %[key] /* postProcF[toFunc @ f];
  key_ := If[MemberQ[$annotationKeys, key],
    getAnnotation[key],
    failPlot["badgraphannokey", key, commaString @ $annotationKeys]
  ]
];

postProcF[f_][e_] := OnFailed[
    Check[f @ e, $Failed],
    failPlot["msglabelfn"]
  ];

myCompactMatrixForm[vec_] :=
  CompactMatrixForm[vec, "Factor" -> False, NegationStyle -> "Color"];

myCompactVectorForm[vec_] :=
  CompactMatrixForm[List @ vec,
    "Factor" -> False, "HideZeros" -> False,
    InversionStyle -> "Color", NegationStyle -> OverBar];

labelForm[RepresentationElement[matrix_]] :=
  myCompactMatrixForm @ matrix;

labelForm[vec_ ? RealVectorQ] /; $uniformPayloadSizes :=
  myCompactVectorForm @ vec;

labelForm[matrix_ ? RealMatrixQ] /; $uniformPayloadSizes :=
  myCompactMatrixForm @ matrix;

labelForm[e_] := e;

lookupPayloadInAssoc[assoc_][i_] :=
  Lookup[assoc, IndexedVertex[i], Lookup[assoc, getName @ i]];

getIndex[i_] := i;
getName[i_] := Part[$labelNames, i];
getWeight[i_] := Part[$labelWeights, i];
getCardinal[i_] := If[ListQ[$EdgeTags], Part[$EdgeTags, i], None];
getAnnotation[name_][i_] := Part[$annotations, name, i];

placeTooltipAt[label_, pos_, _] := NiceTooltip[{Transparent, If[$GraphIs3D, Sphere, Disk][pos, 1.1*$labeledElemSize]}, label];
placeTooltipAt[None | _Missing, _, _] := Nothing;

$labelScaledPos = None;

placeLabelAt[CardinalSet[labels_List], pos_, index_] := Scope[
  {neg, pos} = SelectDiscard[labels, InvertedQ];
  {
    If[pos === {}, Nothing, placeLabelAt[Row[pos, ","], pos, index]],
    If[neg === {}, Nothing, placeLabelAt[Inverted @ Row[StripInverted /@ neg], pos, index]]
  }
];

placeLabelAt[label_, pos_, index_] /; ($labelScaledPos =!= None) := Scope[
  edgeCoords = Part[$EdgeCoordinateLists, index];
  pos12 = {pos1, pos2} = PointAlongLine[edgeCoords, Scaled @ #]& /@ ($labelScaledPos + {-0.01, 0.01});
  If[InvertedQ[label], label //= StripInverted; Swap[pos2, pos1]];
  text = Block[{$labelScaledPos = None, $labeledElemSize = 0}, placeLabelAt[label, Mean @ pos12,  index]];
  If[Head[text] === Text, Insert[text, dim3to2[pos2 - pos1], 4], text]
];

dim3to2 = Case[
  p:{_, _, _} := {1, 0};
  p:{_, _}    := p
];

$labelOffsets = N @ CirclePoints[{1, 0}, 8];
placeLabelAt[label_, pos_, index_] /; ($labelX === Automatic) := Scope[
  adjacentEdges = Part[$adjacencyIndex, index];
  edgeCoords = Mean /@ Part[$EdgeCoordinateLists, adjacentEdges];
  bestOffset = MaximumBy[$labelOffsets, sumOfDistances[pos - #, edgeCoords]&];
  {$labelX, $labelY} = dim3to2[bestOffset] * {1.7, 1.1};
  placeLabelAt[label, pos, index]
];

placeLabelAt[label_, pos_, index_] /; ($labelX === "Sign") := Scope[
  $labelX = -Sign @ Part[pos - $meanCoordinates, 1];
  placeLabelAt[label, pos, index]
];

placeLabelAt[label_, pos_, index_] /; ($labelY === "Sign") := Scope[
  $labelY = -Sign @ Part[pos - $meanCoordinates, 2];
  placeLabelAt[label, pos, index]
];

placeLabelAt[label_, pos_, index_] /; ($labelX === "Radial") := Scope[
  cpos = pos - $meanCoordinates;
  If[$GraphIs3D,
    {$labelX, $labelY} = {0, Sign[$MachineEpsilon + Last[cpos]] * -1.5};
    cscale = 1 + imageSizeToImageFraction[30] * {1, 1, 0.1};
    placeLabelAt[label, (cscale * cpos) + $meanCoordinates, index]
  ,
    {$labelX, $labelY} = -Sign[cpos];
    placeLabelAt[label, pos, index]
  ]
];

placeLabelAt[label_, pos_, index_] /; (!$GraphIs3D && $labelOrientation === "Aligned" && !$isVertices) := Scope[
  edgeCoords = Part[$EdgeCoordinateLists, index];
  {start, end} = FirstLast @ edgeCoords;
  $labelOrientation = Mod[((ArcTan2 @@ (end - start)) / Degree) + 90, 180] - 90;
  placeLabelAt[label, pos, index]
];


sumOfDistances[point_, points_] :=
  Total @ Flatten @ DistanceMatrix[{point}, points];

placeLabelAt[g_Graphics, pos_, _] :=
  Inset[g, pos, {Center, Baseline}];

$labelFormattingRules = {
  ct_CardinalTransition :> formatCardinalTransition @ ct,
  cs_ChartSymbol :> ChartColorForm[cs, cardinalColors]

};

placeLabelAt[label_, pos_, _] := makeTextLabel[
  label /. $labelFormattingRules,
  If[$labelOffset === None,
    pos + If[$GraphIs3D,
      -$labeledElemSize * {0, 0, $labelY} * (1 + $spacings),
      -$labeledElemSize * {$labelX, $labelY} * (1 + $spacings)],
    Offset[$labelOffset, pos]
  ],
  {$labelX, $labelY} * 0.95
];

makeTextLabel[label_, pos_, offset_] := Text[
  $magnifier @ label, pos, offset,
  If[$labelOrientation == 0, Sequence @@ {}, AngleVector[$labelOrientation * Degree]],
  Background -> $labelBackground,
  If[$labelBaseStyle === None, Sequence @@ {}, BaseStyle -> $labelBaseStyle]
];

placeLabelAt[None | _Missing, _, _] := Nothing;

makeMagnifier[1|1.0] := Identity;
makeMagnifier[scale_] := Magnify[#, scale]&;

(**************************************************************************************************)



(**************************************************************************************************)

makeGraphics[elements_, imageSize_, rawImageSize_, imagePadding_, plotLabel_, extraOptions_, prolog_, epilog_, baseline_] := Graphics[
  elements,
  Sequence @@ extraOptions,
  Frame -> None, Axes -> None,
  ImageSize -> imageSize,
  ImageSizeRaw -> rawImageSize,
  ImagePadding -> imagePadding, PlotLabel -> plotLabel,
  PlotRangePadding -> 0, PlotRangeClipping -> False,
  AspectRatio -> Automatic, PreserveImageOptions -> False,
  If[epilog === None, Sequence @@ {}, Epilog -> epilog],
  If[prolog === None, Sequence @@ {}, Prolog -> prolog],
  BaselinePosition -> baseline
];

makeGraphics3D[elements_, imageSize_, rawImageSize_, imagePadding_, plotLabel_, extraOptions_, prolog_, epilog_, baseline_] := Graphics3D[
  {CapForm[None],  Replace[prolog, None -> Nothing],
  elements /. (a:Arrow[_ ? CoordinateMatricesQ] :> Thread[a]),
  Replace[epilog, None -> Nothing]},
  Sequence @@ extraOptions,
  Axes -> None, Boxed -> False,
  ImageSize -> imageSize,
  ImageSizeRaw -> rawImageSize,
  ImagePadding -> imagePadding, PlotRange -> All,
  PlotRangePadding -> 0,
  Lighting -> "Neutral",
  Method -> {"ShrinkWrap" -> shrinkWrap, "EdgeDepthOffset" -> False},
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

processTooltipSpec = Case[
  None                          := None;
  Automatic | All               := %["Name"];
  p:$payloadP                   := toPayloadFunction @ p;

  other_ :=                     failPlot["badlabelspec", other],
  {$payloadP -> $keyP | Rule[$keyP, _]}
]

ExtendedGraphPlot::badtooltipspec = "The tooltip specification `` was not one of the recognized forms."

(**************************************************************************************************)

PublicFunction[VertexIndexForm, VertexLabelForm]

VertexIndexForm[expr_] := addOptionsToGraphs[expr, VertexLabels -> "Index", ImagePadding -> 20];
VertexLabelForm[expr_] := addOptionsToGraphs[expr, VertexLabels -> "Name", ImagePadding -> 20];

PublicFunction[EdgeIndexForm, EdgeLabelForm]

EdgeIndexForm[expr_] := addOptionsToGraphs[expr, EdgeLabels -> "Index", ImagePadding -> 20];
EdgeLabelForm[expr_] := addOptionsToGraphs[expr, EdgeLabels -> "Name", ImagePadding -> 20];

PublicFunction[GraphIndexForm, GraphLabelForm, GraphTooltipIndexForm, GraphTooltipLabelForm]

GraphIndexForm[expr_] := addOptionsToGraphs[expr, VertexLabels -> "Index", EdgeLabels -> "Index", ImagePadding -> 20];
GraphLabelForm[expr_] := addOptionsToGraphs[expr,  VertexLabels -> "Name", EdgeLabels -> "Name", ImagePadding -> 20];
GraphTooltipIndexForm[expr_] := addOptionsToGraphs[expr,  VertexLabels -> Placed["Index", Tooltip], EdgeLabels -> Placed["Index", Tooltip], ImagePadding -> 20];
GraphTooltipLabelForm[expr_] := addOptionsToGraphs[expr,  VertexLabels -> Placed["Name", Tooltip], EdgeLabels -> Placed["Name", Tooltip], ImagePadding -> 20];

addOptionsToGraphs[expr_, opts___] :=
  ReplaceAll[expr, (g_Graph ? GraphQ) :> RuleCondition @ ExtendedGraph[g, opts]];

(**************************************************************************************************)

PublicFunction[PlotGraphVector]

PlotGraphVector[graph_Graph, opts___Rule][vector_List] :=
  PlotGraphVector[graph, vector, opts];

PlotGraphVector[graph_Graph, vector_, opts___Rule] := ExtendedGraphPlot[graph,
  EdgeShapeFunction -> "Line", EdgeStyle -> LightGray,
  VertexColors -> vector,
  opts, VertexLabels -> None
];

(**************************************************************************************************)

PublicFunction[TransformGraphCoordinates]

TransformGraphCoordinates[f_, graph_, method_] :=
  Graph[graph, VertexCoordinates -> Map[f, GraphEmbedding[graph, method]]];

(**************************************************************************************************)

PublicFunction[ExtractGraphPlotPrimitives]

ExtractGraphPlotPrimitives[targetIds_, type_] := Block[
  {$targets = targetIds},
  DeepCases[
    $GraphPlotGraphics /. i_Inset :> RuleCondition[i /. Annotation[a_, _, type] :> a],
    anno:Annotation[_, ids_, type] /; IntersectingQ[ids, $targets] :>
      transformPrimitiveAnno[anno, PartOperator[-1, 1]]
  ]
];

(**************************************************************************************************)

PublicFunction[TransformGraphPlotPrimitives]

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

PrivateFunction[AttachGraphPlotAnnotation]

AttachGraphPlotAnnotation[name_String] := (
  $GraphPlotGraphics //= MapAt[Annotation[#, name]&, 1]
);

(**************************************************************************************************)

PrivateFunction[ApplyFinalTransforms]

(* do this via a GraphStyleTransform operator graphics primitive *)

ApplyFinalTransforms[g:(_Graphics|_Graphics3D)] :=
  MapAt[ApplyFinalTransforms, g, 1];

ApplyFinalTransforms[primitives_] := primitives //. {
  Annotation[p_, "FadeGraph"] :> fadePrimitives[p],
  Annotation[p_, "FadeEdges"] :> applyToPrimitivesType[p, fadePrimitives, "EdgePrimitives"],
  Annotation[p_, "FadeVertices"] :> applyToPrimitivesType[p, fadePrimitives, "VertexPrimitives"],
  Annotation[p_, "SemitransparentArrowheads"] :> applyToPrimitivesType[p, semitransparentArrowheads, "EdgePrimitives"],
  Annotation[p_, "HideArrowheads"] :> applyToPrimitivesType[p, hideArrowheads, "EdgePrimitives"],
  Annotation[p_, "HideVertices"] :> deletePrimitivesType[p, "VertexPrimitives"],
  Annotation[p_, "HideEdges"] :> deletePrimitivesType[p, "EdgePrimitives"]
} // SimplifyGraphicsPrimitives;

deletePrimitivesType[primitives_, type_] := ReplaceAll[primitives,
  Annotation[_, _, type] :> {}
];

applyToPrimitivesType[primitives_, f_, type_] := ReplaceAll[primitives,
  Annotation[p_, indices_, type] :> Annotation[f @ p, indices, type]
];

$fadePrimitivesRules = {
  t_Text :> t,
  a:Annotation[_, "FrameLabel" | "Protected"] :> a,
  Directive[Glow[_], opts___] :> Directive[Glow[LightGray], opts],
  _Opacity -> Opacity[1],
  (VertexColors -> c_List) :> (VertexColors -> c),
  (* (VertexColors -> c_List) :> (VertexColors -> Map[GrayLevel[0.85, ColorOpacity @ #]&, Echo @ c]), *)
  c:$ColorPattern :> GrayLevel[0.7, Min[ColorOpacity @ c, 0.95]]
};

fadePrimitives[primitives_] := ReplaceAll[primitives, $fadePrimitivesRules];

semitransparentArrowheads[primitives_] := ReplaceAll[primitives,
  a_Arrowheads :>
    ReplaceAll[a, {
      c:$ColorPattern :> SetColorOpacity[c, .4],
      Directive[Glow[c_], opts___] :> Color3D[Opacity[.3, c]]
    }]
];

hideArrowheads[p_] := ReplaceAll[p, {
  Arrow -> Line,
  _Arrowheads -> {}
}];

(**************************************************************************************************)

PrivateFunction[AnnotationsToTooltips]

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

