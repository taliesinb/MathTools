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
   graphics
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
  (* If[borderDistance <= $MachineEpsilon, borderDistance = Infinity]; *)
  Max[Min[(* borderDistance,  *)minDistance / 2, Max[$GraphPlotSize] / 3], $MachineEpsilon]
];

computeMaxSafeArrowheadSize[] := Scope[
  distances = DeleteCases[Flatten @ DistanceMatrix[Median /@ $GraphEdgeCoordinateLists], 0|0.];
  minDistance = If[distances === {}, Infinity, Quantile[distances, 1/6]];
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

    {vertexLabels, edgeLabels, plotLabel} =
      LookupOption[graph, {VertexLabels, EdgeLabels, PlotLabel}, None];

    {arrowheadShape, arrowheadStyle, arrowheadSize, arrowheadPosition} =
      LookupExtendedGraphAnnotations[graph, {ArrowheadShape, ArrowheadStyle, ArrowheadSize, ArrowheadPosition}];

    {graphLegend, vertexColorFunction, vertexAnnotations, vertexPairAnnotations} =
      LookupExtendedGraphAnnotations[graph, {GraphLegend, VertexColorFunction, VertexAnnotations, VertexPairAnnotations}];
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

    {imageWidth, imageHeight} = imageSize = ToNumericImageSize[imageSize, Clip[$GraphPlotAspectRatio, {0.5, 1.5}]];

    (* these are used by toImageCoords and toScaledCoords *)
    imageCoordsFactor = imageWidth / $GraphPlotSizeX;
    scaledCoordsVector = {1, imageHeight / imageWidth} / If[$GraphIs3D, Norm @ $GraphPlotSize, $GraphPlotSizeX];

    imagePadding = {{0, 0}, {0, 0}};
  ];

  (* create graphics for vertices *)
  FunctionSection[

    vertexSize = processVertexSize @ removeSingleton @ vertexSize;
    vertexSizeScaled = toScaledCoords @ vertexSize;
    vertexSizeImage = toImageCoords @ vertexSize;

    vertexStyle //= removeSingleton;

    SetAutomatic[vertexShapeFunction, If[vertexColorFunction =!= None, "Disk", "Point"]];

    lighting = None;

    vertexShapeFunction //= removeSingleton;
    {defaultVertexColor, vertexBaseStyle, setbackDistance, vertexDrawFunc, vertexPadding} =
      processVertexShapeFunction[vertexShapeFunction];

    extendPadding[vertexPadding];
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
    vertexGraphics = makeGraphicsGroup @ {vertexBaseStyle /. None -> Nothing, vertexStyle, vertexItems};
  ];

  (* create graphics for edges *)
  FunctionSection[
    SetAutomatic[edgeStyle, Directive[Opacity[.18], Black, If[$GraphIs3D, MediumThick, SlightlyThick]]];
    edgeStyle //= removeSingleton /* toDirective;

    If[arrowheadShape === None || zeroQ[arrowheadSize] || UndirectedGraphQ[$Graph],
      edgeGraphics = makeGraphicsGroup @ {edgeStyle, setback[Line, setbackDistance] @ $GraphEdgeCoordinateLists};
    ,
      SetAutomatic[arrowheadStyle, Which[
        cardinalColors =!= None, cardinalColors,
        vertexColorFunction =!= None, LightGray,
        True, Gray
      ]];
      baseArrowheadSize := baseArrowheadSize = If[$GraphIs3D, 0.45, 1] * ($GraphMaxSafeArrowheadSize / $GraphPlotSizeX) * 1;
      SetAutomatic[arrowheadSize, baseArrowheadSize];
      SetAutomatic[arrowheadPosition, If[$GraphIs3D, 0.65, 0.5]];
      SetAutomatic[arrowheadShape, "Short"];

      edgeTagGroups = If[$GraphEdgeTags === None,
        <|All -> Range[$GraphEdgeCount]|>,
        edgeTagGroups = PositionIndex[$GraphEdgeTags]
      ];
      edgeGraphics = KeyValueMap[
        {card, indices} |-> drawArrowheadEdges[card, Part[$GraphEdgeCoordinateLists, indices]],
        edgeTagGroups
      ];
      edgeGraphics = makeGraphicsGroup @ {edgeStyle, edgeGraphics};
    ];
  ];

  (* create labels for vertices and edges *)
  FunctionSection[
    labelGraphics = {};

    vertexLabels //= removeSingleton;
    If[vertexLabels =!= None,
      SetAutomatic[vertexLabelStyle, {}];
      SetNone[vertexSize, $GraphMaxSafeVertexSize / 5];
      vertexLabelItems = generateLabelPrimitives[vertexLabels, $GraphVertexList, $GraphVertexCoordinates, vertexSize, vertexAnnotations];
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
    If[labelGraphics =!= Nothing, imagePadding += {{0, 0}, {0, 10}}];

    (* for graphs with cardinals, create an automatic legend when asked *)
    If[cardinalColors =!= None && arrowheadStyle =!= None,
      automaticLegends["Cardinals"] := ArrowheadLegend[cardinalColors];
    ];

    imagePadding //= Ceiling;
    imageSize = Ceiling @ ImageSizePad[imageSize, imagePadding];

    (* assemble graphics *)
    graphics = If[$GraphIs3D, makeGraphics3D, makeGraphics][
      {edgeGraphics, vertexGraphics, labelGraphics},
      imageSize, imagePadding, plotLabel
    ];

    (* obtain final plotrange *)
    plotRange = GraphicsPlotRange[graphics];
    AppendTo[graphics, PlotRange -> plotRange];

    applyAutomaticLegends[graphics, automaticLegends, graphLegend]
  ]
];

toImageCoords[sz_] := sz * imageCoordsFactor;
toScaledCoords[sz_] := Scaled[sz * scaledCoordsVector];

extendPadding[n_] := imagePadding = Map[Max[#, n]&, imagePadding, {2}];
extendPadding[padding_List] := imagePadding = MapThread[Max[#, n]&, {imagePadding, padding}, {2}];

makeGraphicsGroup[g_] := g;

zeroQ[0|0.] := True;
zeroQ[_] := False;

(**************************************************************************************************)

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
  cardinals = SortBy[cardinals, Head[#] =!= Negated&];
  cardinals = cardinals //. {l___, Negated[c_], c_, r___} :> {l, DoubleSided[c], r};
  num = Length[cardinals];
  positions = If[num === 1, {arrowheadPos}, 0.5 + 0.3 * Standardize[Range[num]]];
  arrowheads = arrowheadsND @ MapThread[
    {card, pos} |-> makeArrowheadsElement[arrowheadPosition = pos; card]&,
    {cardinals, positions}
  ];
  arrowPrimitives = setback[multiArrow, setbackDistance] @ edgeCoords;
  {arrowheads, arrowPrimitives}
];

makeArrowheadsElement[cardinal_] := Scope[
  {shape, size, style, position} = loopupTagSpecs[cardinal];
  size //= Replace[Scaled[s_] :> s * baseArrowheadSize];
  {size, position, makeArrowheadShape[shape, style]}
];

loopupTagSpec[other_, cardinal_] := other;
loopupTagSpec[assoc_Association, cardinal_] :=
  Lookup[assoc, cardinal, Lookup[assoc, All, None]];

loopupTagSpecs[Negated[cardinal_]] :=
  MapAt[Minus, 2] @ loopupTagSpecs[cardinal];

loopupTagSpecs[DoubleSided[cardinal_]] :=
  ReplacePart[1 -> "DoubleSided"] @ loopupTagSpecs[cardinal];

loopupTagSpecs[cardinal_] := Map[
  loopupTagSpec[#, cardinal]&,
  {arrowheadShape, arrowheadSize, arrowheadStyle, arrowheadPosition}
];

(**************************************************************************************************)

makeArrowheadShape["Short" | Automatic, style_] :=
  makeArrowheadGraphic[$shortArrowhead, FaceForm @ style];

makeArrowheadShape["DoubleSided", style_] :=
  makeArrowheadGraphic[$doubleSidedArrowhead, FaceForm @ style];

$edgeBasedArrowheadStyle := If[$GraphIs3D, Thickness[0.15], AbsoluteThickness[1.2]];

makeArrowheadShape["Line", style_] :=
  makeArrowheadGraphic[$lineArrowhead, Directive[$edgeBasedArrowheadStyle, style]];

makeArrowheadShape[Graphics[elems_, opts___], style_] :=
  Graphics[{Opacity[1], style, elems}, opts, AspectRatio -> 1, PlotRangeClipping -> False];

makeArrowheadShape[Placed[img_Image, pos_], style_] :=
  imageToGraphics[img, pos];

makeArrowheadShape[img_Image, style_] :=
  imageToGraphics[img, {0, 1}];

imageToGraphics[img_, pos_] := ImageToGraphics[ImageResize[img, 50], pos, 50];

ExtendedGraphPlot::badarrowhead = "ArrowheadShape -> `` should be None, \"Short\", Automatic, Graphics[..], or an Image."
makeArrowheadShape[spec_, _] :=
  failPlot["badarrowhead", spec];

(**************************************************************************************************)

makeArrowheadGraphic[primitives_, style_, opts___] :=
  Graphics[
    {Opacity[1], EdgeForm[None], style, primitives},
    AspectRatio -> 1,
    PlotRangeClipping -> False,
    opts
  ];

$lineArrowhead =
  Line[Developer`ToPackedArray @ {{-0.2, -0.3}, {0.1, 0.}, {-0.2, 0.3}}];

$shortArrowhead = FilledCurve[
  {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
  Developer`ToPackedArray @ {{{-0.3166, -0.3333}, {-0.1833, 0.}, {-0.3166, 0.3333}, {0.25, 0.}}}
];

$doubleSidedArrowhead = {
  FilledCurve[
    {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
    Developer`ToPackedArray @ {{{-0.05, -0.33}, {-0.45, 0.}, {-0.05, 0.33}, {-0.15, 0.}}}
  ],
  FilledCurve[
    {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
    Developer`ToPackedArray @ {{{0.05, -0.33}, {0.45, 0.}, {0.05, 0.33}, {0.15, 0.}}}
  ]
};

(**************************************************************************************************)

PackageExport["ArrowheadLegend"]

ArrowheadLegend[assoc_Association] := Scope[
  rows = KeyValueMap[
    {name, color} |-> {" ", makeLegendArrowheadGraphic @ color, name},
    assoc
  ];
  Grid[rows, BaseStyle -> $LegendLabelStyle, Spacings -> {.4, 0.5}]
]

makeLegendArrowheadGraphic[color_] := makeArrowheadGraphic[
  Rotate[$shortArrowhead, Pi/2], FaceForm @ color,
  BaselinePosition -> Scaled[-0.0], ImageSize -> {11, 11}
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
      vertexDrawFunc = mapCoordArray[drawDisk3D[vertexSizeScaled]];
      vertexPadding = vertexSizeImage * 2;
      setbackDistance = vertexSize / 2;
    ,
    "Ball" | "Sphere" /; $GraphIs3D,
      defaultVertexColor = $LightGray;
      vertexDrawFunc = drawSphere[vertexSize / (2 * Sqrt[3.])];
    ,
    "Disk" | "Ball" | "Sphere",
      defaultVertexColor = $DarkGray;
      vertexBaseStyle = EdgeForm @ AbsoluteThickness[.25];
      vertexDrawFunc = drawDisk[vertexSize / 2];
    ,
    "Point",
      defaultVertexColor = $DarkGray;
      vertexBaseStyle = PointSize @ Part[vertexSizeScaled, 1, 1];
      setbackDistance = vertexSize / 2;
      vertexDrawFunc = drawPoint;
      vertexPadding = vertexSizeImage / 2;
    ,
    _,
      failPlot["badvshapefunc", spec];
  ];
  {defaultVertexColor, vertexBaseStyle, setbackDistance, vertexDrawFunc, vertexPadding}
];

drawPoint[pos_, color_] := {color, Point @ pos};

drawSphere[size_][pos_, color_] :=
  {color, Sphere[pos, size]};

drawDisk[size_][pos_, color_] := {
  FaceForm[color], EdgeForm[Darker[color, .3]],
  If[CoordinateMatrixQ[pos], Disk[#, size]& /@ pos, Disk[pos, size]]
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
  True, Print["Invalid multiline dimensions: ", Dimensions /@ pos]
];

multiArrow[pos_, dist_] := Which[
  ArrayQ[pos, 2], Arrow[pos, dist],
  ArrayQ[pos, 2, MatrixQ], Map[Arrow[#, dist]&, pos],
  True, Print["Invalid multiarrow dimensions: ", Dimensions /@ pos]
];

(* this is needed because when Arrow is given a list of paths, it will interpret the arrowhead
spec as being a list of specs and walk them in parallel *)
multiArrow[pos_] := Which[
  ArrayQ[pos, 2], Arrow[pos],
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

getVertexIndex[v_] := Lookup[$GraphVertexIndices, v, failPlot["notvertex", v]];
getAnnoValue[annos_, key_] := Lookup[annos, key, failPlot["badgraphannokey", key, commaString @ Keys @ annos]];

toColorFunctionData = MatchValues[
  "Name" := $GraphVertexList;
  "Index" := $IndexGraphVertexList;
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

(* this returns a size in plot range coordinates *)
processVertexSize = MatchValues[
  Automatic                     := %[0.3];
  {"Nearest", r_ ? NumericQ}    := N[r] * $GraphMaxSafeVertexSize;
  {"Scaled", r_ ? NumericQ}     := N[r] * Norm[$GraphPlotSize];
  sym_Symbol /; KeyExistsQ[$fractionSizes, sym] := %[{"Nearest", 3 * $fractionSizes[sym]}];
  r_ ? NumericQ                 := %[{"Nearest", r}];
  other_                        := failPlot["badvertexsize", other];
];

$fractionSizes = <|Tiny -> 0.1, Small -> 0.15, MediumSmall -> 0.175, Medium -> 0.2, MediumLarge -> 0.3, Large -> 0.4, Huge -> 0.6|>;

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

makeGraphics[elements_, imageSize_, padding_, plotLabel_] := Graphics[
  elements,
  Frame -> None, Axes -> None,
  ImageSize -> imageSize,
  ImagePadding -> padding, PlotLabel -> plotLabel,
  AspectRatio -> Automatic, PlotRangePadding -> None
];

makeGraphics3D[elements_, imageSize_, padding_, plotLabel_] := Graphics3D[
  {CapForm[None], elements},
  Axes -> None, Boxed -> False,
  ImageSize -> imageSize,
  ImagePadding -> padding, PlotRange -> All, PlotRangePadding -> None,
  ViewProjection -> "Orthographic", Lighting -> "Neutral",
  Method -> {"ShrinkWrap" -> False, "EdgeDepthOffset" -> False},
  AspectRatio -> Automatic, ViewPoint -> {Infinity, 0, 0},
  PlotLabel -> plotLabel
];

(**************************************************************************************************)

PackageScope["chooseArrowheadSize"]

chooseArrowheadSize[imageSize_] := 0.1 * 150 / imageWidth;

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

