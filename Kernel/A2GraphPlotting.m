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

PackageScope["$GraphVertexCoordinates"]
PackageScope["$GraphVertexCoordinateDistanceMatrix"]
PackageScope["$GraphEdgeCoordinateLists"]
PackageScope["$GraphHighlightStyle"]
PackageScope["$GraphIs3D"]
PackageScope["$GraphPlotRange"]
PackageScope["$GraphPlotSize"]
PackageScope["$GraphPlotAspectRatio"]
PackageScope["$GraphPlotImageSize"]
PackageScope["$GraphPlotImageWidth"]
PackageScope["$GraphMaxSafeVertexSize"]

PackageExport["GraphPlotScope"]

SetHoldRest[GraphPlotScope];

GraphPlotScope[graph_, body_] := Scope[

  If[!GraphQ[graph], ReturnFailed[]];

  GraphScope[graph,

    {$GraphVertexCoordinates, $GraphEdgeCoordinateLists} = ExtractGraphPrimitiveCoordinates[$IndexGraph];

    $GraphHighlightStyle := $GraphHighlightStyle = LookupOption[$Graph, GraphHighlightStyle];
    $GraphPlotImageSize := $GraphPlotImageSize := LookupImageSize @ $Graph;
    $GraphPlotImageWidth := $GraphPlotImageWidth = First[$GraphPlotImageSize; LookupImageSize @ $Graph];

    $GraphVertexCoordinateDistanceMatrix := $GraphVertexCoordinateDistanceMatrix = DistanceMatrix[$GraphVertexCoordinates];
(*     $GraphVertexCoordinateBoundingBox := $GraphVertexCoordinateBoundingBox = CoordinateBoundingBox[$GraphVertexCoordinates];
 *)

    (* before we have called the user function, guess the range based on the vertex and edge coordinates *)
    $GraphIs3D := $GraphIs3D = CoordinateMatrixQ[$GraphVertexCoordinates, 3];
    $GraphPlotRange := $GraphPlotRange = CoordinateBounds[{$GraphVertexCoordinates, Replace[$GraphEdgeCoordinateLists, {} -> Nothing]}];
    $GraphPlotSize := $GraphPlotSize = rangeSize[$GraphPlotRange];
    $GraphPlotSizeX := Max[Part[$GraphPlotSize, 1], $MachineEpsilon];
    $GraphPlotSizeY := Max[Part[$GraphPlotSize, 2], $MachineEpsilon];
    $GraphPlotScale := $GraphPlotScale = If[$GraphIs3D, $GraphPlotSizeX, Norm @ $GraphPlotSize];
    $GraphPlotAspectRatio := $GraphPlotSizeY / $GraphPlotSizeX;

    $GraphPlotScale := $GraphPlotScale = If[$GraphIs3D, $GraphPlotSizeX, Norm @ $GraphPlotSize];
    $GraphPlotAspectRatio := Part[$GraphPlotSize, 2] / $GraphPlotSizeX;
    $GraphMaxSafeVertexSize := $GraphMaxSafeVertexSize = computeMaxSafeVertexSize[];
    $GraphMaxSafeArrowheadSize := $GraphMaxSafeArrowheadSize = computeMaxSafeArrowheadSize[];

    body
  ]
];

(**************************************************************************************************)

PackageExport["ExtendedGraphPlot"]

$autoFilledLegendPattern = (Automatic | _String) | Placed[Automatic | _String, _];

ExtendedGraphPlot[graph_] := Block[
  {
   $GraphPlotImageSize, $GraphPlotImageWidth, $GraphPlotRange, $GraphPlotSize, $GraphMaxSafeVertexSize,
   plottingFunction, graphLegend, graphRegionHighlight, vertexColorFunction,
   graphics, highlightGraphics, requiredPadding
  },

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
    $GraphPlotImageSize := $GraphPlotImageSize = LookupImageSize @ graphics;
    $GraphPlotImageWidth := $GraphPlotImageWidth = First[$GraphPlotImageSize];
    $GraphPlotRange := $GraphPlotRange = GraphicsPlotRange @ graphics;
    $GraphPlotSize := $GraphPlotSize = rangeSize[$GraphPlotRange];
    $GraphMaxSafeVertexSize := $GraphMaxSafeVertexSize = computeMaxSafeVertexSize[];

    {highlightGraphics, requiredPadding} = resolveGraphRegionHighlightGraphics @ graphRegionHighlight;
    If[highlightGraphics =!= {},
      graphics = GraphicsImageSizePadTo[graphics, requiredPadding];
      graphics = ApplyEpilog[graphics, highlightGraphics];
    ];
    graphics = ApplyLegend[graphics, graphLegend];

    graphics
  ]
];

GraphicsImageSizePadTo[graphics_, requiredPadding_] := Scope[
  padding = LookupOption[graphics, ImagePadding];
  padding = Map[Max[#, requiredPadding]&, padding, {2}];
  ReplaceOptions[graphics, ImagePadding -> padding]
];

rangeSize[range_] := EuclideanDistance @@@ range;

computeBorderDistance[{cl_, ch_}, {pl_, ph_}] := {cl - pl, ph - ph};

rankedMean[d_] := HarmonicMean @ Take[Sort @ d, Ceiling[Length[d] / 4]];

computeMaxSafeVertexSize[] := Scope[
  distances = DeleteCases[Flatten @ $GraphVertexCoordinateDistanceMatrix, 0|0.];
  minDistance = If[distances === {}, Infinity, rankedMean @ distances];
  Max[Min[minDistance / 2, Max[$GraphPlotSize] / 3], $MachineEpsilon]
];

computeMaxSafeArrowheadSize[] := Scope[
  distances = DeleteCases[Flatten @ DistanceMatrix[Median /@ $GraphEdgeCoordinateLists], 0|0.];
  minDistance = If[distances === {}, Infinity, rankedMean @ distances];
  minDistance = Max[minDistance, $GraphMaxSafeVertexSize/Sqrt[2]];
  Min[$GraphMaxSafeVertexSize, minDistance, Max[$GraphPlotSize] / 3]
];


(**************************************************************************************************)

PackageExport["ExtendedGraphPlottingFunction"]

Quiver::badcolors = "CardinalColors should be an association from cardinals to colors.";

ExtendedGraphPlottingFunction[___] := $Failed;

failPlot[msgName_String, args___] := (
  Message[MessageName[ExtendedGraphPlot, msgName], args];
  Throw[$Failed, ExtendedGraphPlottingFunction]
);

ExtendedGraphPlottingFunction[graph_Graph] := Scope @ Catch[

  (* process options *)
  FunctionSection[
    {graphLayout, imageSize, vertexSize, vertexStyle, edgeStyle, vertexLabelStyle, edgeLabelStyle, vertexShapeFunction, edgeShapeFunction} =
      LookupOption[graph, {GraphLayout, ImageSize, VertexSize, VertexStyle, EdgeStyle, VertexLabelStyle, EdgeLabelStyle, VertexShapeFunction, EdgeShapeFunction}, Automatic];

    {vertexLabels, edgeLabels, plotLabel, epilog, imagePadding} =
      LookupOption[graph, {VertexLabels, EdgeLabels, PlotLabel, Epilog, ImagePadding}, None];

    {arrowheadShape, arrowheadStyle, arrowheadSize, arrowheadPosition, viewOptions} =
      LookupExtendedGraphAnnotations[graph, {ArrowheadShape, ArrowheadStyle, ArrowheadSize, ArrowheadPosition, ViewOptions}];

    {graphLegend, vertexColorFunction, vertexAnnotations} =
      LookupExtendedGraphAnnotations[graph, {GraphLegend, VertexColorFunction, VertexAnnotations}];
  ];

  (* initial processing of global options *)
  FunctionSection[
    cardinalColors = LookupCardinalColors[graph];
    If[!MatchQ[cardinalColors, None | _Association], ReturnFailed["badcolors"]];

    SetNone[vertexAnnotations, <||>];

    automaticLegends = <||>;

    (* choose a size based on vertex seperation *)
    SetAutomatic[imageSize,
      If[Length[$VertexList] > 1000, Large,
        graphPlotSizeScalingFunction[If[$GraphIs3D, 30, 15] * ($GraphPlotSizeX / $GraphMaxSafeVertexSize)]]];

    imageSize = ToNumericImageSize[imageSize, Clip[$GraphPlotAspectRatio, {0.2, 1.5}]];

    (* this makes rotation less zoomy *)
    If[$GraphIs3D && First[imageSize] / Last[imageSize] > 1.75,
      Part[imageSize, 2] = First[imageSize] / 1.75];

    {imageWidth, imageHeight} = imageSize;

    (* these are used by toImageCoords and toScaledCoords *)
    imageCoordsFactor = imageWidth / $GraphPlotSizeX;
    scaledCoordsVector = {1, imageWidth / imageHeight} / If[$GraphIs3D, Norm @ $GraphPlotSize, $GraphPlotSizeX];

    imagePadding = Which[
      NumericQ[imagePadding], N[imagePadding] * {{1, 1}, {1, 1}},
      RealMatrixQ[imagePadding], N[imagePadding],
      True, {{1, 1}, {1, 1}}
    ];
  ];

  (* create graphics for vertices *)
  FunctionSection[

    $vertexSizeOverrides = None;
    $defaultVertexSize = Which[edgeStyle === {None}, 0.6, vertexColorFunction =!= None, 0.5, True, 0.3];
    vertexSize = processVertexSize @ removeSingleton @ vertexSize;
    vertexSizeImage = toImageCoords @ vertexSize;

    vertexStyle //= removeSingleton;

    SetAutomatic[vertexShapeFunction, If[vertexColorFunction =!= None, "Disk", "Point"]];

    lighting = None;

    vertexShapeFunction //= removeSingleton;
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

      vertexColorFunctionData = toColorFunctionData[vertexColorFunction];
      If[FailureQ[vertexColorFunctionData], failPlot["badcolvals"]];
      {colorGroups, colorFunctionObject} = ApplyColoring[vertexColorFunctionData];
      If[FailureQ[colorFunctionObject], failPlot["badcolvals"]];
      automaticLegends["Colors"] := colorFunctionObject;

      vertexItems = Capture @ KeyValueMap[colorGroupFun, colorGroups];
    ,
      vertexItems = vertexDrawFunc[Range @ $VertexCount, defaultVertexColor];

    ];
    vertexGraphics = makeGraphicsGroup @ {vertexBaseStyle, vertexStyle, vertexItems};
  ];

  (* create graphics for edges *)
  FunctionSection[
    SetAutomatic[edgeStyle, Directive[Opacity[.18], Black, If[$GraphIs3D, MediumThick, SlightlyThick]]];
    edgeStyle //= toDirectiveOptScan[setEdgeStyleGlobals];

    If[edgeStyle === None,
      edgeGraphics = {}
    ,
      If[arrowheadShape === None || zeroQ[arrowheadSize] || UndirectedGraphQ[$Graph],
        edgeGraphics = makeGraphicsGroup @ {edgeStyle, setback[Line, setbackDistance] @ $GraphEdgeCoordinateLists};
      ,
        SetAutomatic[arrowheadStyle, Which[
          cardinalColors =!= None, cardinalColors,
          vertexColorFunction =!= None, LightGray,
          True, Gray
        ]];
        baseArrowheadSize := baseArrowheadSize = If[$GraphIs3D, 0.45, 0.8] * ($GraphMaxSafeArrowheadSize / $GraphPlotSizeX);
        arrowheadSize //= Replace[$arrowheadSizeRules];
        arrowheadSize //= ReplaceAll[Scaled[r_ ? NumericQ] :> baseArrowheadSize * r];

        SetAutomatic[arrowheadShape, "Arrow"];
        $twoWayStyle = None;
        If[ListQ[arrowheadShape],
          {arrowheadShape, arrowheadShapeOpts} = FirstRest @ arrowheadShape;
          Scan[scanArrowheadShapeOpts, arrowheadShapeOpts]];
        SetAutomatic[arrowheadSize, baseArrowheadSize];
        SetAutomatic[arrowheadPosition, If[$GraphIs3D && arrowheadShape =!= "Cone", 0.65, 0.5]];

        arrowheadBounds = CoordinateBounds[
          Median /@ $GraphEdgeCoordinateLists,
          Max[arrowheadSize * $GraphPlotSizeX] * 0.5
        ];
        extendPaddingToInclude[arrowheadBounds];

        edgeTagGroups = If[$EdgeTags === None,
          <|All -> Range[$EdgeCount]|>,
          edgeTagGroups = PositionIndex[$EdgeTags]
        ];

        edgeGraphics = KeyValueMap[
          {card, indices} |-> drawArrowheadEdges[card, Part[$GraphEdgeCoordinateLists, indices]],
          edgeTagGroups
        ];
        edgeGraphics = makeGraphicsGroup @ {edgeStyle, edgeGraphics};
      ];
    ];
  ];

  (* create labels for vertices and edges *)
  FunctionSection[
    labelGraphics = {};

    (* for label style, support: Opacity, as well as Tiny, Small, etc. Medium corresponds to ordinary size. *)

    vertexLabels //= removeSingleton;
    If[vertexLabels =!= None,
      SetNone[vertexSize, $GraphMaxSafeVertexSize / 5];
      vertexLabelItems = generateLabelPrimitives[
        vertexLabels, $VertexList, $GraphVertexCoordinates,
        vertexSize, vertexLabelStyle, vertexAnnotations
      ];
      AppendTo[labelGraphics, vertexLabelItems]
    ];

    edgeLabels //= removeSingleton;
    If[edgeLabels =!= None,
      edgeLabelItems = generateLabelPrimitives[
        edgeLabels, $EdgeList, Median /@ $GraphEdgeCoordinateLists,
        Max[arrowheadSize]/2, edgeLabelStyle, <||>
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

    If[Max[imagePadding] > 2,
      (* we must scale down pointsize slightly since PointSize and Scaled refer to the full image width,
      which includes the padding we just added! *)
      compensationFactor = 1.0 - ((Total @ First[imagePadding]) / First[imageSize]);
      graphicsElements = graphicsElements /. {
        Scaled[s_] :> Scaled[s * compensationFactor],
        PointSize[s_] :> PointSize[s * compensationFactor]
      };
    ];

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
    plotRange = GraphicsPlotRange[graphics];
    AppendTo[graphics, PlotRange -> plotRange];

    applyAutomaticLegends[graphics, automaticLegends, graphLegend]
  ]
,
  ExtendedGraphPlottingFunction
];

toImageCoords[sz_] := sz * imageCoordsFactor;
toScaledCoords[sz_] := Scaled[sz * scaledCoordsVector];
toScaledPointSize[sz_] := PointSize[sz * First[scaledCoordsVector]];

extendPadding[n_] := imagePadding = Map[Max[#, n]&, imagePadding, {2}];
extendPadding[padding_List] := imagePadding = MapThread[Max, {imagePadding, padding}, 2];

extendPaddingToInclude[{{xmin_, xmax_}, {ymin_, ymax_}}] := Scope[
  {{pxmin, pxmax}, {pymin, pymax}} = $GraphPlotRange;
  extra = {{pxmin - xmin, xmax - pxmax}, {pymin - ymin, ymax - pymax}};
  extendPadding[extra / $GraphPlotSizeX * imageWidth]
];

makeGraphicsGroup[g_] := g;

zeroQ[0|0.] := True;
zeroQ[_] := False;

vertexDrawFuncWithSize[rawVertexDrawFunc_, vertexSize_] :=
  ReplaceAll[
    {indices, color} |->
      rawVertexDrawFunc[Part[$GraphVertexCoordinates, indices], color],
    $vertexSize -> vertexSize
  ];

vertexDrawFuncSizeRemapper[rawVertexDrawFunc_, sizeOverrides_, vertexSize_] :=
  {indices, color} |-> Map[
    index |-> Construct[
      ReplaceAll[
        rawVertexDrawFunc,
        $vertexSize -> Lookup[sizeOverrides, index, vertexSize]
      ],
      Part[$GraphVertexCoordinates, index], color
    ], indices
  ];

toColorVertexDrawFunc[vertexDrawFunc_] :=
  {colorValue, indices} |-> vertexDrawFunc[indices, First @ colorValue];

toColorVertexDrawFuncWithTooltip[vertexDrawFunc_] :=
  {colorValue, indices} |-> NiceTooltip[
    vertexDrawFunc[indices, First @ colorValue],
    Last @ colorAnd
  ];

(**************************************************************************************************)

PackageExport["TwoWayStyle"]

scanArrowheadShapeOpts = MatchValues[
  TwoWayStyle -> s:("Out"|"In"|"OutClose") := $twoWayStyle ^= arrowheadShape <> "Double" <> s;
  TwoWayStyle -> s:("Square"|"Ball"|"Disk"|"Diamond"|None) := $twoWayStyle ^= s;
  rule_ := failPlot["badsubopt", rule, ArrowheadShape];
];

$arrowheadSizeRules = {
  Tiny -> Scaled[0.25],
  Small -> Scaled[0.5],
  MediumSmall -> Scaled[0.75],
  Medium -> Scaled[1.0],
  MediumLarge -> Scaled[1.25],
  Large -> Scaled[1.5],
  Huge -> Scaled[2.0]
};

arrowheadsND[e_] := If[$GraphIs3D, Arrowheads[e, Appearance -> "Projected"], Arrowheads @ e];

drawArrowheadEdges[cardinal_, edgeCoords_] := Scope[
  If[
    Or[
      lookupTagSpec[arrowheadShape, cardinal] === None,
      zeroQ @ lookupTagSpec[arrowheadSize, cardinal]
    ],
    setback[Line, setbackDistance] @ edgeCoords
  ,
    arrowheads = arrowheadsND @ List @ makeArrowheadsElement @ cardinal;
    arrowPrimitives = setback[Arrow, setbackDistance] @ edgeCoords;
    {arrowheads, arrowPrimitives}
  ]
];

drawArrowheadEdges[CardinalSet[cardinals_], edgeCoords_] := Scope[
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
  arrowPrimitives = setback[multiArrow, setbackDistance] @ edgeCoords;
  {arrowheads, arrowPrimitives}
];

makeArrowheadsElement[cardinal_] := Scope[
  {shape, size, style, position} = lookupTagSpec[cardinal];
  size //= Replace[Scaled[s_] :> s * baseArrowheadSize];
  SetNone[style, $Gray];
  {size, position, makeArrowheadShape[shape, style]}
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

makeArrowheadGraphic2D[primitives_, style_, opts___] :=
  Graphics[
    {Opacity[1], EdgeForm[None], style, primitives},
    AspectRatio -> 1,
    PlotRangeClipping -> False,
    opts
  ];

makeArrowheadGraphic3D[primitives_, style_, opts___] :=
  Graphics3D[
    {Opacity[1], EdgeForm @ None, FaceForm @ style, primitives}
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

$namedArrowheads = Union[Keys @ $arrowheads2D, Keys @ $arrowheads3D];

(**************************************************************************************************)

makeArrowheadShape["Sphere", style_] :=
  makeArrowheadGraphic3D[$arrowheads3D @ name, abs3DColor @ style];

makeArrowheadShape[name_String, style_] /; StringStartsQ[name, "Line"] :=
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

ArrowheadLegend[assoc_Association, shape_:"Arrow"] := Scope[
  rows = KeyValueMap[
    {name, color} |-> {makeLegendArrowheadGraphic[color, shape], name},
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
      defaultVertexColor = $DarkGray;
      vertexBaseStyle = EdgeForm @ AbsoluteThickness[.25];
      vertexDrawFunc = drawDisk[$vertexSize / 2];
    ,
    "Point",
      defaultVertexColor = $DarkGray;
      setbackDistance = vertexSize / 2;
      vertexDrawFunc = drawPoint[$vertexSize];
      vertexPadding = 1 + vertexSizeImage / 2;
    ,
    _,
      failPlot["badvshapefunc", spec];
  ];
  {defaultVertexColor, vertexBaseStyle, setbackDistance, vertexDrawFunc, vertexPadding}
];

drawPoint[size_][pos_, color_] :=
  {toScaledPointSize @ size, color, Point @ pos};

drawSphere[size_][pos_, color_] :=
  {color, Sphere[pos, size]};

drawDisk[size_][pos_, color_] := {
  FaceForm[color], EdgeForm[Darker[color, .3]],
  If[CoordinateMatrixQ[pos], Disk[#, size]& /@ pos, Disk[pos, size]]
};

drawDisk3D[size_][pos_, color_] := Inset[
  Graphics[drawDisk[1][{0, 0}, color], AspectRatio -> 1],
  pos, {0, 0}, toScaledCoords @ size
];

mapCoordArray[f_][array_, args___] :=
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
  True, Print["Invalid multiarrow dimensions: ", Dimensions /@ pos]
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
  {"Nearest", r_ ? NumericQ}    := N[r] * $GraphMaxSafeVertexSize;
  {"Scaled", r_ ? NumericQ}     := N[r] * Norm[$GraphPlotSize];
  sym_Symbol /; KeyExistsQ[$fractionSizes, sym] := %[{"Nearest", 3 * $fractionSizes[sym]}];
  r_ ? NumericQ                 := %[{"Nearest", r}];
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

$fractionSizes = <|Tiny -> 0.1, Small -> 0.15, MediumSmall -> 0.175, Medium -> 0.2, MediumLarge -> 0.3, Large -> 0.4, Huge -> 0.6|>;

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
  $labelNames = names; $annotations = annotations; $elemSize = size / 2; $spacings = 0;
  $labelSizeScale = 1; $labelY = -1; $labelX = 0; $labelBackground = GrayLevel[1.0, 0.6];
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

ExtendedGraphPlot::badsubopt = "`` is not a recognized option to ``."

setLabelStyleGlobals = MatchValues[
  ItemSize -> size:$sizePattern := $labelSizeScale ^= toNumericSizeScale @ size;
  Background -> o:$opacityPattern := $labelBackground ^= GrayLevel[1.0, toNumericOpacity @ o];
  Background -> c:$ColorPattern := $labelBackground ^= c;
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

placeTooltipAt[label_, pos_] := NiceTooltip[{Transparent, If[$GraphIs3D, Sphere, Disk][pos, 1.1*$elemSize]}, label];
placeTooltipAt[None | _Missing, _] := Nothing;

placeLabelAt[label_, pos_] := Text[
  $magnifier @ label,
  pos + If[$GraphIs3D, 0, -$elemSize * {$labelX, $labelY} * (1 + $spacings)],
  {$labelX, $labelY} * 0.95,
  Background -> $labelBackground
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

