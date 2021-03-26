Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["ArrowheadLegend"]

pickPointAlongCurve[{a_, b_}, fraction_] := a + (b - a) * fraction;
pickPointAlongCurve[{a_, b_}, fractions_List] := a + (b - a) * #& /@ fractions;

netCurveLength[coords_] := EuclideanDistance[First[coords], Last[coords]];
pickPointAlongCurve[coords_, fraction_] := Scope[
  len = Length[coords];
  coords = N[coords];
  distances = Accumulate[EuclideanDistance @@@ Partition[coords, 2, 1]];
  PrependTo[distances, 0];
  If[ListQ[fraction],
    interp = Interpolation[Transpose[{distances, coords}], InterpolationOrder -> 1];
    interp /@ (Last[distances] * fraction)
  ,
    Interpolation[Transpose[{distances, coords}], Last[distances] * fraction, InterpolationOrder -> 1]
  ]
]

rotateToVector[{x_, y_}][{a_, b_}] := {a x + y b, a y - x b};

$arrowheadPoints = With[{adx = 2/3}, {{-adx, -1}, {-adx, 1}, {adx, 0}}];

makeArrowhead[pos_, dir_, color_] := Style[
  Polygon[pos + rotateToVector[dir][#]& /@ $arrowheadPoints],
  FaceForm[color], EdgeForm[Darker[color, .2]]
];

ArrowheadLegend[assoc_Association] := Scope[
  rows = KeyValueMap[{name, color} |-> {
    Graphics[makeArrowhead[{0, 0}, {0, 1}, color], ImageSize -> {Automatic, 10}, BaselinePosition -> Bottom],
    name
  }, assoc];
  Grid[rows, BaseStyle -> {FontFamily -> "Avenir"}]
]

logCeiling[r_] := Power[2, Ceiling[Log2[r]]];
logFloor[r_] := Power[2, Floor[Log2[r]]];

cardinalEdgePlot[colorMap_][coords_, DirectedEdge[a_, b_] | UndirectedEdge[a_, b_]] :=
  Line[coords];

cardinalEdgePlot[colorMap_][coords_, DirectedEdge[a_, b_, c_]] := Scope[
  {point, pointPrime} = N @ pickPointAlongCurve[coords, {0.6, 0.61}];
  curveLength = netCurveLength[coords];
  If[curveLength == 0,
    curveLength = EuclideanDistance[First[coords], point] + EuclideanDistance[Last[coords], point]];
  arrowheadSize = GeometricMean[logFloor[curveLength * {0.09, 0.1, 0.11}]];
  arrowheadVector = arrowheadSize * Normalize[pointPrime - point];
  If[RuleQ[c], tooltip = Last[c]; c = First[c];, tooltip = None];
  If[Head[c] === Negated, arrowheadVector = -arrowheadVector; c = First[c]];
  arrowhead = makeArrowhead[point, arrowheadVector, colorMap[c]];
  {Line[coords], NiceTooltip[arrowhead, tooltip]}
];

NiceTooltip[g_, None] := g;
NiceTooltip[g_, e_] := Tooltip[g, Pane[e, BaseStyle -> {FontSize -> 15, "Output"}, ImageMargins -> 5]];


$cardinalColors = {RGBColor[0.91, 0.23, 0.14], RGBColor[0.24, 0.78, 0.37], RGBColor[0.21, 0.53, 0.86], RGBColor[1, 0.59, 0.25], RGBColor[0.87, 0.19, 0.75], RGBColor[0, 0.75, 0.74], GrayLevel[0.53]};
$cardinalNameSets = {{"R", "G", "B"}, {"A", "B"}, {"A", "B", "C"}, {"A", "B", "C", "D"}, {"A", "B", "C", "D", "E"}, {"X", "Y"}, {"X", "Y", "Z"}, {"U", "V"}, {"U", "V", "W"}};

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
GraphLegend is an (extra) option to Graph that creates a legend for the graph.
* GraphLegend -> None specifies no additional legend
* GraphLegend -> legend$ specifies a particular legend.
"


PackageExport["CardinalGraph"]


Options[CardinalGraph] = Options[Graph];

CardinalGraph[list_List, opts:OptionsPattern[]] :=
  CardinalGraph[Graph[list], opts];

CardinalGraph[graph_Graph, newOpts:OptionsPattern[]] := Scope[
  origOpts = Options[Graph];
  opts = Sequence[newOpts, origOpts];
  is3D = ContainsQ[{opts}, "Dimension" -> 3];
  edges = EdgeList[graph]; vertices = VertexList[graph];
  If[MemberQ[edges, _DirectedEdge],
    edges = DeleteDuplicates @ Replace[edges, {
      DirectedEdge[a_, b_, Negated[card_]] :> DirectedEdge[b, a, card],
      UndirectedEdge[a_, b_, Negated[card_]] :> DirectedEdge[b, a, card],
      UndirectedEdge[a_, b_, card_] :> DirectedEdge[a, b, card]
    }, {1}];
  ];
  cardinals = UniqueCases[edges, DirectedEdge[_, _, c_] :> c];
  cardinals = Union @ Replace[cardinals, Rule[a_, b_] :> a, {1}];
  cardinals = cardinals /. Negated[c_] -> c;
  colorMap = cardinalsToColorMap[cardinals];
  edgePlotter = cardinalEdgePlot[colorMap];
  graph = Graph[vertices, edges,
    EdgeShapeFunction -> edgePlotter,
    newOpts,
    VertexStyle -> Directive[FaceForm[GrayLevel[0.7]], EdgeForm[None]],
    EdgeStyle -> Directive[LightGray, Opacity[1]],
    GraphLegend -> ArrowheadLegend[colorMap],
    ContentSelectable -> False, Editable -> False,
    origOpts
  ]
]


PackageExport["CardinalList"]

CardinalList[graph_Graph] := CardinalList @ EdgeList @ graph;

CardinalList[edges_List] := Scope[
  cardinals = UniqueCases[edges, DirectedEdge[_, _, c_] :> c];
  cardinals = Union @ Replace[cardinals, Rule[a_, b_] :> a, {1}];
  cardinals
];

