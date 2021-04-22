Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["CardinalColors"]

SetUsage @ "
CardinalColors is an option to Graph.
"


PackageExport["QuiverPlot"]

QuiverPlot[graph_Graph] /; (AnnotationValue[graph, GraphPlottingFunction] =!= QuiverPlot || !GraphQ[$Graph]) :=
  ExtendedGraphPlotDispatch @ Annotate[graph, GraphPlottingFunction -> QuiverPlot];

QuiverPlot::badcolors = "CardinalColors should be an association from cardinals to colors.";

QuiverPlot[___] := $Failed;

QuiverPlot[graph_ ? GraphQ] := Scope[

  cardinalColors = LookupCardinalColors[graph];
  If[!MatchQ[cardinalColors, None | _Association], ReturnFailed["badcolors"]];

  {graphLayout, imageSize, vertexStyle, edgeStyle} =
    LookupOption[graph, {GraphLayout, ImageSize, VertexStyle, EdgeStyle}, Automatic];

  {vertexLabels, edgeLabels, vertexLabelStyle, edgeLabelStyle} =
    LookupOption[graph, {VertexLabels, EdgeLabels, VertexLabelStyle, EdgeLabelStyle}, None];

  {arrowheadStyle, arrowheadSize, graphLegend} =
    LookupAnnotation[graph, {ArrowheadStyle, ArrowheadSize, GraphLegend}];

  (* choose a size based on the longest path in the graph *)
  SetAutomatic[imageSize,
    If[Length[$GraphVertexList] > 1000, Large, chooseImageSize @ GraphDiameter @ $SymmetricIndexGraph]];

  (* make sure that the image height isn't too big for large aspect-ratio graphs *)
  If[$GraphPlotAspectRatio > 1.5,
    imageSize = toNumericImageSize @ imageSize;
    imageSize *= ($GraphPlotAspectRatio - 1.5) / $GraphPlotAspectRatio;
  ];
  numericImageSize = toNumericImageSize @ imageSize;

  edgeTags = If[EdgeTaggedGraphQ @ $Graph, EdgeTags @ $GraphEdgeList, None];

  SetAutomatic[edgeStyle, Directive[Opacity[.2], Black]];

  If[arrowheadStyle === None || UndirectedGraphQ[$Graph],
    edgeGraphics = {edgeStyle, Line[$GraphEdgeCoordinateLists]};
  ,
    If[!AssociationQ[arrowheadStyle], arrowheadStyle = <|All -> arrowheadStyle|>];
    If[cardinalColors === None, cardinalColors = <||>];

    (* create graphics for edges *)
    edgeTagGroups = PositionIndex[edgeTags];
    baseArrowheadSize := If[$GraphIs3D, 0.6, 1.0] * (chooseArrowheadSize @ numericImageSize);
    SetAutomatic[arrowheadSize, baseArrowheadSize];
    If[MatchQ[arrowheadSize, Scaled[_]], arrowheadSize = First[arrowheadSize] * baseArrowheadSize];
    arrowheadPos = If[$GraphIs3D, 0.65, 0.5];
    edgeGraphics = KeyValueMap[
      {cardinal, edgeIndices} |-> (
        style = Lookup[arrowheadStyle, cardinal, Lookup[arrowheadStyle, All, Automatic]];
        color = Lookup[cardinalColors, cardinal, Automatic];
        SetAutomatic[color, Black];
        SetAutomatic[style, Arrowheads[{{arrowheadSize, arrowheadPos, makeBaseArrowhead @ color}}]];
        {style, Arrow @ Part[$GraphEdgeCoordinateLists, edgeIndices]}
      ),
      edgeTagGroups
    ];
    edgeGraphics = {edgeStyle, edgeGraphics};
  ];

  (* create graphics for vertices *)
  SetAutomatic[vertexStyle, Directive[GrayLevel[0.3], AbsolutePointSize[5]]];
  pointGraphics = {vertexStyle, Point @ $GraphVertexCoordinates};

  SetAutomatic[vertexLabelStyle, {}];
  SetAutomatic[edgeLabelStyle, {}];

  (* create labels for vertices and edges *)
  labelGraphics = {};
  vertexLabels //= Replace[{a_} :> a];
  edgeLabels //= Replace[{a_} :> a];

  vertexLabels //= processLabelSpec;
  If[vertexLabels =!= None,
    vertexLabelItems = MapThread[
      makeTextLabel[vertexLabels],
      {Range @ $GraphVertexCount, $GraphVertexList, $IndexGraphVertexList}
    ];
    AppendTo[labelGraphics, {vertexLabelStyle, vertexLabelItems}];
  ];

  edgeLabels //= processLabelSpec;
  If[edgeLabels =!= None,
    edgeLabelItems = MapThread[
      makeTextLabel[edgeLabels],
      {Range @ $GraphEdgeCount, edgeTags, Median /@ $GraphEdgeCoordinateLists}
    ];
    AppendTo[labelGraphics, {edgeLabelStyle, edgeLabelItems}];
  ];

  (* for graphs with cardinals, create an automatic legend when asked *)
  addLegend = If[cardinalColors === None, None,
    $legend := ArrowheadLegend[cardinalColors];
    Switch[graphLegend,
      Automatic, $legend,
      Placed[Automatic, _], ReplacePart[graphLegend, 1 -> $legend],
      _, None
    ];
  ];

  (* assemble graphics *)
  graphics = If[$GraphIs3D, makeGraphics3D, makeGraphics][
    {pointGraphics, {edgeStyle, edgeGraphics}, labelGraphics},
    imageSize
  ];

  (* add the automatic legend if there was one. ExtendedGraphPlotDispatch will extract this legend if necessary. *)
  If[addLegend === None, graphics, Legended[graphics, addLegend]]
];

(**************************************************************************************************)

QuiverPlot::badlabelspec = "The label specification `` was not one of the recognized forms."

processLabelSpec = MatchValues[
  Automatic | All | "Name" := "Name";
  Tooltip | Placed["Name", Tooltip] := "NameTooltip";
  "Index" := "Index";
  Placed["Index", Tooltip] := "IndexTooltip";
  None := None;
  {e_} := %[e];
  other_ := (Message[QuiverPlot::badlabelspec, other]; None);
];

QuiverPlot::badarrowhead = "The arrowhead specification with head `` should be None, Automatic, Graphics[..], or an Image."

(**************************************************************************************************)

processArrowheadSpec = MatchValues[
  Automatic := Automatic;
  None := None;
  g_Graphics := g;
  Placed[img_Image, pos_] := imageToGraphics[img, pos];
  img_Image := imageToGraphics[img, {0, 1}];
  other_ := ReturnFailed[QuiverPlot::badarrowhead, Head @ other];
];

imageToGraphics[img_, pos_] := ImageToGraphics[ImageResize[img, 50], pos, Replace[arrowheadSize, Tiny -> .1]];

(**************************************************************************************************)

makeTextLabel = MatchValues[
  "Name" :=         labelAt[#2, #3]&;
  "Index" :=        labelAt[#1, #3]&;
  "NameTooltip" :=  tooltipAt[#2, #3]&;
  "IndexTooltip" := tooltipAt[#1, #3]&;
];

tooltipAt[label_, pos_] := NiceTooltip[{Transparent, Disk[pos, .1]}, label];
labelAt[label_, pos_] := Text[label, pos, {0, -1}];

(**************************************************************************************************)

makeGraphics[elements_, imageSize_] := Graphics[
  elements,
  Frame -> None, Axes -> None,
  ImageSize -> toStandardImageSize @ imageSize,
  ImagePadding -> 5
];

makeGraphics3D[elements_, imageSize_] := Graphics3D[
  elements,
  Axes -> None, Boxed -> False,
  ImageSize -> toStandardImageSize @ imageSize,
  ImagePadding -> 5
];

(**************************************************************************************************)

PackageScope["chooseArrowheadSize"]

chooseArrowheadSize[imageSize_] := 0.1 * 150 / imageSize;

(**************************************************************************************************)

PackageScope["chooseGraphImageSize"]

chooseGraphImageSize[graph_Graph] :=
  If[VertexCount[graph] > 1000, Large,
    chooseGraphImageSize @ EdgeList[graph]];

chooseGraphImageSize[edges_List] :=
  If[Sqrt[Length[edges]] > 2000, Large,
    toStandardImageSize @ chooseImageSize @ GraphDiameter @ ReplaceAll[edges, DirectedEdge -> UndirectedEdge]
  ];

chooseImageSize[diam_] := Which[
  diam < 3, Tiny,
  diam < 8, Small,
  diam < 12, MediumSmall,
  diam < 16, Medium,
  diam < 32, MediumLarge,
  True, Large
];

