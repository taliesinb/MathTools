Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

PackageExport["Quiver"]

SetUsage @ "
Quiver[graph$] constructs a cardinal quiver from a graph.
Quiver[edges$] constructs a cardinal quiver from a list of edges.
Quiver[vertices$, edges$] constructs a cardinal quiver from a list of vertices and edges.
* The edges of graph$ should be tagged with cardinals.
* The edges incident to one vertex should not be tagged with a cardinal more than once.
* The resulting graph will display with a legend showing the cardinals associated with each edge.
"

DeclareArgumentCount[Quiver, {1, 2}];

Options[Quiver] = Join[
  {ArrowheadSize -> Automatic, ArrowheadStyle -> Automatic},
  Options[Graph]
];

declareSyntaxInfo[Quiver, {_, _., OptionsPattern[]}];

Quiver[edges_, opts:OptionsPattern[]] :=
  Quiver[Automatic, edges, opts];

Quiver[graph_Graph, newOpts:OptionsPattern[]] := Scope[
  oldOpts = Options[graph];
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


$maxVertexCount = 150;
makeQuiver[vertices_, edges_, oldOpts_, newOpts_] := Scope[

  edges = Flatten @ List @ processEdge[edges, None];
  If[ContainsQ[edges, $Failed], ReturnFailed[]];
  If[!validCardinalEdgesQ[edges],
    reportDuplicateCardinals[edges];
    ReturnFailed[];
  ];

  If[vertices === Automatic, vertices = Union[edges[[All, 1]], edges[[All, 2]]]];

  Graph[
    vertices, edges,
    Sequence @@ DeleteOptions[newOpts, {GraphLegend}],
    GraphLegend -> Lookup[newOpts, GraphLegend, Automatic],
    GraphPlottingFunction -> ExtendedGraphPlottingFunction,
    Sequence @@ oldOpts
  ]
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

(**************************************************************************************************)

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

(**************************************************************************************************)

PackageExport["BouquetQuiver"]

SetUsage @ "
BouquetQuiver[cardinals$] creates a Bouquet cardinal quiver graph with the given cardinal edges.
BouquetQuiver['string$'] uses the characters of 'string$' as cardinals.
"

DeclareArgumentCount[BouquetQuiver, 1];

Options[BouquetQuiver] = Options[Graph];

declareSyntaxInfo[FreeQuiver, {_, OptionsPattern[]}];

BouquetQuiver[str_String, opts:OptionsPattern[]] := BouquetQuiver[Characters[str], opts];

BouquetQuiver[cardinals_List, opts:OptionsPattern[]] :=
  Quiver[Map[c |-> Labeled[1 -> 1, c], cardinals], opts]

(**************************************************************************************************)

PackageExport["QuiverQ"]

SetUsage @ "
QuiverQ[graph$] returns True if graph$ represents a cardinal quiver.
* A cardinal quiver must have a cardinal associated with every edge.
* A cardinal quiver should contain only directed edges.
* A cardinal should not be present on more than one edge incident to a vertex.
"

QuiverQ[g_] := EdgeTaggedGraphQ[g] && validCardinalEdgesQ[EdgeList[g]];

validCardinalEdgesQ[edges_] := And[
  MatchQ[edges, {DirectedEdge[_, _, _]..}],
  AllTrue[GroupBy[edges, Last], checkForDuplicateCardinals]
];

checkForDuplicateCardinals[edges_] :=
  DuplicateFreeQ[InVertices @ edges] && DuplicateFreeQ[OutVertices @ edges];

(**************************************************************************************************)

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

DeclareArgumentCount[FreeQuiver, 1];

declareSyntaxInfo[FreeQuiver, {_}];

FreeQuiver[graph_] := Scope[
  $count = 1;
  makeQuiver[VertexList @ graph, Map[toQuiverEdge, EdgeList @ graph], {}, {}]
];

(**************************************************************************************************)

PackageExport["CardinalList"]

SetUsage @ "
CardinalList[quiver$] returns the list of cardinals in a quiver.
* The cardinals are returned in sorted order.
* If the graph has no tagged edges, None is returned.
"

CardinalList[graph_Graph] := None;

CardinalList[graph_Graph ? EdgeTaggedGraphQ] :=
  DeleteCases[Null] @ Union @ EdgeTags @ graph;

CardinalList[edges_List] :=
  UniqueCases[edges, DirectedEdge[_, _, c_] :> c];

(**************************************************************************************************)

PackageExport["LookupCardinalColors"]

SetUsage @ "
LookupCardinalColors[quiver$] returns the association of cardinals to colors for quiver$.
* The annotation CardinalColors is returned if present.
* The cardinals are given in sorted order.
* If the graph has no tagged edges, None is returned.
"

LookupCardinalColors[graph_Graph] := None;

LookupCardinalColors[graph_Graph ? EdgeTaggedGraphQ] :=
  Replace[
    AnnotationValue[graph, CardinalColors],
    ($Failed | Automatic) :> Module[{cardinals},
      cardinals = CardinalList[graph];
      AssociationThread[Sort @ cardinals, Take[$ColorPalette, Length @ cardinals]]
    ]
  ];

LookupCardinalColors[_] := $Failed;

(**************************************************************************************************)

PackageExport["RemoveCardinals"]

RemoveCardinals[g_Graph] := Scope[
  coords = LookupOption[g, VertexCoordinates];
  Graph[
    VertexList[g], Take[EdgeList[g], All, 2],
    VertexCoordinates -> coords
  ]
];
