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

makeBaseArrowhead[color_] :=
  Graphics[{
    Opacity[1.0], FaceForm[color], EdgeForm[Darker[color, .2]],
    Polygon[$arrowheadPoints]},
    ImageSize -> $arrowheadSize, AspectRatio -> 1,
    PlotRangeClipping -> False
  ];

estimateGraphicsWidth[] := Clip[Sqrt[$currentVertexCount] * 3, {5, 35}];

cardinalEdgePlot[colorMap_, _][coords_, DirectedEdge[a_, b_] | UndirectedEdge[a_, b_]] :=
  Line[coords];

$lastMax = 0;
cardinalEdgePlot[colorMap_, arrowSize_][coords_, DirectedEdge[a_, b_, c_]] := Scope[
  If[Head[c] === Negated, c = First[c]];
  len = EuclideanDistance[First[coords], Last[coords]];
  color = colorMap[c];
  arrowPos = If[NumberQ[arrowSize], Clip[0.5 + estimateGraphicsWidth[] * arrowSize / len, {0.5, 0.85}], 0.5];
  arrowheads = Arrowheads[{{arrowSize, arrowPos, makeBaseArrowhead[color]}}];
  {{Opacity[.2], Black, arrowheads, Arrow[coords]}}
];

fastCardinalEdgePlot[colorMap_, arrowSize_][coords_, DirectedEdge[a_, b_, c_]] :=
  {colorMap[Replace[c, Negated[z_] :> z]], Line[coords]};

cardinalVertexPlot[pos_, _, _] :=
  {GrayLevel[0.3], AbsolutePointSize[5], Point[pos]};

NiceTooltip[g_, None] := g;
NiceTooltip[g_, e_] := Tooltip[g, Pane[e, BaseStyle -> {FontSize -> 15, "Output"}, ImageMargins -> 5]];

$cardinalColors = {
  RGBColor[0.91, 0.23, 0.14], RGBColor[0.24, 0.78, 0.37], RGBColor[0.21, 0.53, 0.86],
  RGBColor[1, 0.59, 0.25], RGBColor[0.87, 0.19, 0.75], RGBColor[0, 0.75, 0.74], GrayLevel[0.53]
};

$cardinalNameSets = Characters /@ {"RGB", "AB", "ABC", "ABCD", "ABCDE", "XY", "XYZ", "UV", "UVW"};

cardinalsToColorMap[cardinals_] := Scope[
  Do[
    If[Sort[ToUpperCase @ set] == Sort[cardinals],
      Return @ AssociationThread[set, Take[$cardinalColors, Length[set]]]],
    {set, $cardinalNameSets}
  ];
  AssociationThread[cardinals, Take[$cardinalColors, Length[cardinals]]]
];


PackageExport["GraphLegend"]

SetUsage @ "
GraphLegend is an option to CardinalQuiver that creates a legend for the graph.
* GraphLegend -> None specifies no additional legend
* GraphLegend -> Automatic uses a legend for the cardinals
* GraphLegend -> legend$ specifies a particular legend
"


PackageExport["CardinalQuiver"]

SetUsage @ "
CardinalQuiver[graph$] constructs a cardinal quiver from a graph.
CardinalQuiver[edges$] constructs a cardinal quiver from a list of edges.
CardinalQuiver[vertices$, edges$] constructs a cardinal quiver from a list of vertices and edges.
* The edges of graph$ should be tagged with cardinals.
* The edges incident to one vertex should not be tagged with a cardinal more than once.
* The resulting graph will display with a legend showing the cardinals associated with each edge.
"

PackageExport["ArrowheadSize"]

SetUsage @ "
ArrowheadSize is an option to CardinalQuiver.
"

Options[CardinalQuiver] = Join[
  {ArrowheadSize -> Automatic, PlotLabel -> None},
  Options[Graph]
];

CardinalQuiver[edges_, opts:OptionsPattern[]] :=
  CardinalQuiver[Automatic, edges, opts];

CardinalQuiver[graph_Graph, newOpts:OptionsPattern[]] := Scope[
  oldOpts = Options[Graph];
  edges = EdgeList[graph];
  vertices = VertexList[graph];
  makeCardinalQuiver[vertices, edges, oldOpts, {newOpts}]
];

CardinalQuiver[vertices_, edges_, newOpts:OptionsPattern[]] :=
  makeCardinalQuiver[vertices, edges, {}, {newOpts}];

CardinalQuiver::invedge = "The edge specification `` is not valid."

processEdge[edge_, _] :=
  (Message[CardinalQuiver::invedge]; $Failed);

CardinalQuiver::nakededge = "The edge `` is not labeled with a cardinal.";

processEdge[edge:(_Rule | DirectedEdge[_, _]), None] :=
  (Message[CardinalQuiver::nakededge, edge]; $Failed);

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

$maxVertexCount = 150;
makeCardinalQuiver[vertices_, edges_, oldOpts_, newOpts_] := Scope[

  edges = Flatten @ processEdge[edges, None];
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
  edgePlotter = If[Length[vertices] > $maxVertexCount, fastCardinalEdgePlot,
    cardinalEdgePlot][colorMap, arrowheadSize];

  graph = Graph[vertices, edges,
    EdgeShapeFunction -> edgePlotter,
    VertexShapeFunction -> vertexPlotter,
    DeleteCases[newOpts, (GraphLegend | ArrowheadSize) -> _],
    VertexStyle -> Directive[FaceForm[GrayLevel[0.4]], EdgeForm[None]],
    GraphLayout -> "SpringElectricalEmbedding",
    EdgeStyle -> Directive[LightGray, Opacity[1], Arrowheads[{{Automatic, .5}}]],
    ContentSelectable -> False, Editable -> False,
    ImagePadding -> 3,
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

PackageScope["toCardinalQuiver"]

toCardinalQuiver[g_Graph] := CardinalQuiver[g];
toCardinalQuiver[edges_List] := CardinalQuiver[edges];
toCardinalQuiver[_] := $Failed;


PackageExport["BouquetQuiver"]

SetUsage @ "
BouquetQuiver[cardinals$] creates a Bouquet cardinal quiver graph with the given cardinal edges.
BouquetQuiver['string$'] uses the characters of 'string$' as cardinals.
"

Options[BouquetQuiver] = Options[Graph];

BouquetQuiver[str_String, opts:OptionsPattern[]] := BouquetQuiver[Characters[str], opts];

BouquetQuiver[cardinals_List, opts:OptionsPattern[]] :=
  CardinalQuiver[Map[c |-> Labeled[1 -> 1, c], cardinals], opts]


PackageExport["CardinalQuiverQ"]

SetUsage @ "
CardinalQuiverQ[graph$] returns True if graph$ represents a cardinal quiver.
* A cardinal quiver must have a cardinal associated with every edge.
* A cardinal quiver should contain only directed edges.
* A cardinal should not be present on more than one edge incident to a vertex.
"

CardinalQuiverQ[g_] := GraphQ[g] && validCardinalEdgesQ[EdgeList[g]];

validCardinalEdgesQ[edges_] := And[
  MatchQ[edges, {DirectedEdge[_, _, _]..}],
  AllTrue[GroupBy[edges, Last], checkForDuplicateCardinals]
];

srcVertices[edges_] := edges[[All, 1]];
dstVertices[edges_] := edges[[All, 2]];

checkForDuplicateCardinals[edges_] :=
  DuplicateFreeQ[srcVertices @ edges] && DuplicateFreeQ[dstVertices @ edges];

reportDuplicateCardinals[edges_] := (
  KeyValueScan[checkEdgeGroup, GroupBy[edges, Last]];
)

CardinalQuiver::dupcardinal = "The cardinal `` is present on the following incident edges: ``."
checkEdgeGroup[tag_, edges_] /; !checkForDuplicateCardinals[edges] := Scope[
  {srcDup, dstDup} = Apply[Alternatives, FindDuplicates[#]]& /@ {srcVertices[edges], dstVertices[edges]};
  dupEdges = Cases[edges, DirectedEdge[srcDup, _, _] | DirectedEdge[_, dstDup, _]];
  Message[CardinalQuiver::dupcardinal, tag, Take[dupEdges, All, 2]];
];

PackageExport["FreeCardinalQuiver"]

SetUsage @ "
FreeCardinalQuiver[graph$] returns a cardinal quiver for graph$, assigning a unique formal symbol \
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


FreeCardinalQuiver[graph_] := Scope[
  $count = 1;
  makeCardinalQuiver[VertexList @ graph, Map[toQuiverEdge, EdgeList @ graph], {}, {}]
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





