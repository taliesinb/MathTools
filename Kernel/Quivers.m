Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["ArrowheadLegend"]

$arrowheadPoints = With[{adx = 4/3, offx = 0}, {{-adx + offx, -1}, {-adx + offx, 1}, {adx + offx, 0}}];
$arrowheadSize = 12 * {1, 1};

ArrowheadLegend[assoc_Association] := Scope[
  $arrowheadSize = 10 * {1, 1};
  rows = KeyValueMap[{name, color} |-> {" ",
    Append[makeBaseArrowhead[color] /. p_Polygon :> Rotate[p, Pi/2], BaselinePosition -> Scaled[0.1]],
    name
  }, assoc];
  Grid[rows, BaseStyle -> {FontFamily -> "Avenir"}, Spacings -> {.5, 0.5}]
]


PackageExport["ColoredArrowhead"]

ColoredArrowhead[color_, sz_:12] := Scope[
  $arrowheadSize = sz * {1, 1};
  Append[
    makeBaseArrowhead[color] /. p_Polygon :> Rotate[p, Pi/2],
    BaselinePosition -> Scaled[0.1]
  ]
];

makeBaseArrowhead[color_] :=
  Graphics[{
    Opacity[1.0], FaceForm[color], EdgeForm[Darker[color, .2]],
    Polygon[$arrowheadPoints]},
    ImageSize -> $arrowheadSize, AspectRatio -> 1,
    PlotRangeClipping -> False
  ];

cardinalEdgePlot[colorMap_, _, _][coords_, DirectedEdge[a_, b_] | UndirectedEdge[a_, b_]] :=
  Line[coords];

cardinalEdgePlot[colorMap_, arrowSize_, arrowGraphic_, gwidth_][coords_, DirectedEdge[a_, b_, c_]] := Scope[
  If[Head[c] === Negated, c = First[c]];
  len = EuclideanDistance[First[coords], Last[coords]] + 0.01;
  color = colorMap[c]; arrowPos = 0.5;
  arrowGraphic = Switch[arrowGraphic,
    Automatic,
      arrowPos = If[NumberQ[arrowSize], Clip[0.5 + (5000 / (200 + gwidth)) * arrowSize / len, {0.5, 0.85}], 0.5];
      makeBaseArrowhead[color],
    _Graphic, arrowGraphic,
    _Association, arrowGraphic[c]
  ];
  arrowheads = Arrowheads[{{arrowSize, arrowPos, arrowGraphic}}];
  {{Opacity[.2], Black, arrowheads, Arrow[coords]}}
];

fastCardinalEdgePlot[colorMap_, arrowSize_, arrowGraphic_, _][coords_, DirectedEdge[a_, b_, c_]] :=
  {colorMap[Replace[c, Negated[z_] :> z]], Line[coords]};

cardinalVertexPlot[pos_, _, _] :=
  {GrayLevel[0.3], AbsolutePointSize[5], Point[pos]};

NiceTooltip[g_, None] := g;
NiceTooltip[g_, e_] := Tooltip[g, Pane[e, BaseStyle -> {FontSize -> 15, "Output"}, ImageMargins -> 5]];

$cardinalNameSets = Characters /@ {"RGB", "AB", "ABC", "ABCD", "ABCDE", "XY", "XYZ", "UV", "UVW"};

cardinalsToColorMap[cardinals_] := Scope[
  Do[
    If[Sort[ToUpperCase @ set] == Sort[cardinals],
      Return @ AssociationThread[set, Take[$ColorPalette, Length[set]]]],
    {set, $cardinalNameSets}
  ];
  AssociationThread[cardinals, Take[$ColorPalette, Length[cardinals]]]
];


PackageExport["Quiver"]

SetUsage @ "
Quiver[graph$] constructs a cardinal quiver from a graph.
Quiver[edges$] constructs a cardinal quiver from a list of edges.
Quiver[vertices$, edges$] constructs a cardinal quiver from a list of vertices and edges.
* The edges of graph$ should be tagged with cardinals.
* The edges incident to one vertex should not be tagged with a cardinal more than once.
* The resulting graph will display with a legend showing the cardinals associated with each edge.
"

PackageExport["ArrowheadSize"]
PackageExport["ArrowheadStyle"]

SetUsage @ "
ArrowheadSize is an option to Quiver.
ArrowheadStyle is an option to Quiver.
"

Options[Quiver] = Join[
  {ArrowheadSize -> Automatic, ArrowheadStyle -> Automatic},
  Options[Graph]
];

Quiver[edges_, opts:OptionsPattern[]] :=
  Quiver[Automatic, edges, opts];

Quiver[graph_Graph, newOpts:OptionsPattern[]] := Scope[
  oldOpts = Options[Graph];
  edges = EdgeList[graph];
  vertices = VertexList[graph];
  makeQuiver[vertices, edges, oldOpts, {newOpts}]
];

Quiver[vertices_, edges_, newOpts:OptionsPattern[]] :=
  makeQuiver[vertices, edges, {}, {newOpts}];

Quiver::invedge = "The edge specification `` is not valid."

processEdge[edge_, _] :=
  (Message[Quiver::invedge, edge]; $Failed);

Quiver::nakededge = "The edge `` is not labeled with a cardinal.";

processEdge[edge:(_Rule | DirectedEdge[_, _]), None] :=
  (Message[Quiver::nakededge, edge]; $Failed);

processEdge[Labeled[edges_, label_], _] :=
  processEdge[edges, label];

processEdge[e_, Verbatim[Alternatives][args__]] := Map[processEdge[e, #]& /@ {args}];
processEdge[l_ -> r_, Negated[c_]] := DirectedEdge[r, l, c];
processEdge[l_ -> r_, label_] := DirectedEdge[l, r, label];

processEdge[DirectedEdge[l_, r_, Verbatim[Alternatives][args__]], z_] :=
  processEdge[DirectedEdge[l, r, #], z]& /@ {args};

processEdge[DirectedEdge[l_, r_], Negated[c_]] := DirectedEdge[r, l, c];
processEdge[DirectedEdge[l_, r_], c_] := DirectedEdge[l, r, c];

processEdge[DirectedEdge[l_, r_, Negated[c_]], _] := DirectedEdge[r, l, c];
processEdge[de:DirectedEdge[_, _, _], _] := de;

processEdge[assoc_Association, _] := KeyValueMap[processEdge[#2, #1]&, assoc];
processEdge[Labeled[e_, label_], _] := processEdge[e, label];

processEdge[list_List, label_] := Map[processEdge[#, label]&, list];

processArrowheadStyle[e_] := e;
processArrowheadStyle[a_Association] := Map[toArrowheadGraphic, a];

toArrowheadGraphic[g_] := None;
toArrowheadGraphic[g_Graphics] := g;
toArrowheadGraphic[Placed[img_Image, pos_]] := toGraphics[img, pos];
toArrowheadGraphic[img_Image] := toGraphics[img, {0, 1}];

toGraphics[img_, pos_] := ImageToGraphics[ImageResize[img, 50], pos, Replace[arrowheadSize, Tiny -> .1]];

chooseImageSize[diam_] := Which[
  diam < 3, Tiny,
  diam < 8, Small,
  diam < 12, MediumSmall,
  diam < 16, Medium,
  diam < 32, MediumLarge,
  True, Large
];

PackageScope["chooseGraphImageSize"]

chooseGraphImageSize[graph_Graph] :=
  If[VertexCount[graph] > 1000, Large,
    chooseGraphImageSize @ EdgeList[graph]];

chooseGraphImageSize[edges_List] :=
  If[Sqrt[Length[edges]] > 2000, Large,
    toStandardImageSize @ chooseImageSize @ GraphDiameter @ ReplaceAll[edges, DirectedEdge -> UndirectedEdge]
  ];

$maxVertexCount = 150;
makeQuiver[vertices_, edges_, oldOpts_, newOpts_] := Scope[

  edges = Flatten @ List @ processEdge[edges, None];
  If[ContainsQ[edges, $Failed], ReturnFailed[]];
  If[!validCardinalEdgesQ[edges],
    reportDuplicateCardinals[edges];
    ReturnFailed[];
  ];

  cardinals = Sort @ UniqueCases[edges, DirectedEdge[_, _, c_] :> c];
  colorMap = cardinalsToColorMap[cardinals];

  If[vertices === Automatic, vertices = Union[edges[[All, 1]], edges[[All, 2]]]];

  is3D = !FreeQ[{oldOpts, newOpts}, "Dimension" -> 3];
  vertexPlotter = cardinalVertexPlot;
  arrowheadSize = Lookup[newOpts, ArrowheadSize, Automatic];
  SetAutomatic[arrowheadSize, If[is3D, 0.01, Tiny]];

  imageSize = Lookup[newOpts, ImageSize, Lookup[oldOpts, ImageSize, Automatic]];
  SetAutomatic[imageSize, If[Length[vertices] > 1000, Large, chooseGraphImageSize @ edges]];

  arrowheadStyle = processArrowheadStyle @ Lookup[newOpts, ArrowheadStyle, Automatic];
  graphicsWidth = toNumericImageSize[imageSize];
  edgePlotter = If[Length[vertices] > $maxVertexCount, fastCardinalEdgePlot,
    cardinalEdgePlot][colorMap, arrowheadSize, arrowheadStyle, graphicsWidth];

  graph = Graph[vertices, edges,
    EdgeShapeFunction -> edgePlotter,
    VertexShapeFunction -> vertexPlotter,
    DeleteOptions[newOpts, {GraphLegend, ArrowheadSize, ArrowheadStyle, ImageSize}],
    VertexStyle -> Directive[FaceForm[GrayLevel[0.4]], EdgeForm[None]],
    GraphLayout -> "SpringElectricalEmbedding",
    EdgeStyle -> Directive[LightGray, Opacity[1], Arrowheads[{{Automatic, .5}}]],
    ContentSelectable -> False, Editable -> False,
    ImagePadding -> 3, ImageSize -> imageSize,
    oldOpts
  ];

  $arrowheadLegend := ArrowheadLegend[colorMap];
  legend = Lookup[newOpts, GraphLegend, Automatic];
  legend //= Replace[{
    Automatic :> $arrowheadLegend,
    Placed[Automatic, pos_] :> Placed[$arrowheadLegend, pos]
  }];

  AttachAnnotation[graph, GraphLegend -> legend]
]

reportDuplicateCardinals[edges_] := (
  KeyValueScan[checkEdgeGroup, GroupBy[edges, Last]];
)

Quiver::dupcardinal = "The cardinal `` is present on the following incident edges: ``."
checkEdgeGroup[tag_, edges_] /; !checkForDuplicateCardinals[edges] := Scope[
  {srcDup, dstDup} = Apply[Alternatives, FindDuplicates[#]]& /@ {InVertices[edges], OutVertices[edges]};
  dupEdges = Cases[edges, DirectedEdge[srcDup, _, _]];
  If[dupEdges === {}, dupEdges = Cases[edges, DirectedEdge[_, dstDup, _]]];
  Message[Quiver::dupcardinal, tag, Take[dupEdges, All, 2]];
];


PackageExport["ToQuiver"]

SetUsage @ "
ToQuiver[obj$] attempts to convert obj$ to a quiver Graph[$$] object.
* If obj$ is already a quiver graph, it is returned unchanged.
* If obj$ is a list of rules, it is converted to a quiver graph.
* Otherwise, $Failed is returned.
"

ToQuiver = MatchValues[
  graph_Graph := If[QuiverQ @ graph, graph, Quiet @ Quiver @ graph];
  edges_List := Quiet @ Quiver @ edges;
  str_String := BouquetQuiver @ str;
  _ := $Failed;
];


PackageExport["BouquetQuiver"]

SetUsage @ "
BouquetQuiver[cardinals$] creates a Bouquet cardinal quiver graph with the given cardinal edges.
BouquetQuiver['string$'] uses the characters of 'string$' as cardinals.
"

Options[BouquetQuiver] = Options[Graph];

BouquetQuiver[str_String, opts:OptionsPattern[]] := BouquetQuiver[Characters[str], opts];

BouquetQuiver[cardinals_List, opts:OptionsPattern[]] :=
  Quiver[Map[c |-> Labeled[1 -> 1, c], cardinals], opts]


PackageExport["QuiverQ"]

SetUsage @ "
QuiverQ[graph$] returns True if graph$ represents a cardinal quiver.
* A cardinal quiver must have a cardinal associated with every edge.
* A cardinal quiver should contain only directed edges.
* A cardinal should not be present on more than one edge incident to a vertex.
"

QuiverQ[g_] := GraphQ[g] && validCardinalEdgesQ[EdgeList[g]];

validCardinalEdgesQ[edges_] := And[
  MatchQ[edges, {DirectedEdge[_, _, _]..}],
  AllTrue[GroupBy[edges, Last], checkForDuplicateCardinals]
];

checkForDuplicateCardinals[edges_] :=
  DuplicateFreeQ[InVertices @ edges] && DuplicateFreeQ[OutVertices @ edges];


PackageExport["FreeQuiver"]

SetUsage @ "
FreeQuiver[graph$] returns a cardinal quiver for graph$, assigning a unique formal symbol \
to each edge in the graph$.
* Undirected edges are transformed into pairs of opposite directed edges.
"

$formalSymbols = Map[letter |-> Symbol["\\" <> "[Formal" <> letter <> "]"], CharacterRange["A", "Z"]];

toQuiverEdge[DirectedEdge[a_, b_]] :=
  DirectedEdge[a, b, $formalSymbols[[$count++]]];

toQuiverEdge[UndirectedEdge[a_, b_]] := Splice[{
  toQuiverEdge[DirectedEdge[a, b]],
  toQuiverEdge[DirectedEdge[b, a]]
}];


FreeQuiver[graph_] := Scope[
  $count = 1;
  makeQuiver[VertexList @ graph, Map[toQuiverEdge, EdgeList @ graph], {}, {}]
];



PackageExport["CardinalList"]

SetUsage @ "
CardinalList[quiver$] returns the list of cardinals in a quiver.
* The cardinals are returned in sorted order.
"

CardinalList[graph_Graph] := CardinalList @ EdgeList @ graph;

CardinalList[edges_List] := Scope[
  cardinals = UniqueCases[edges, DirectedEdge[_, _, c_] :> c];
  cardinals = Union @ Replace[cardinals, Rule[a_, b_] :> a, {1}];
  cardinals
];


PackageExport["CardinalColors"]

CardinalColors[graph_Graph] :=
  FirstCase[Options[graph], cardinalEdgePlot[cmap_, _, _] :> cmap, None, Infinity];

CardinalColors[_] := $Failed;



PackageExport["RemoveCardinals"]

RemoveCardinals[g_Graph] := Scope[
  coords = LookupOption[g, VertexCoordinates];
  Graph[
    VertexList[g], Take[EdgeList[g], All, 2],
    VertexCoordinates -> coords
  ]
];
