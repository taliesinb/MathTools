PublicFunction[Quiver]

SetUsage @ "
Quiver[graph$] constructs a cardinal quiver from a graph.
Quiver[edges$] constructs a cardinal quiver from a list of edges.
Quiver[vertices$, edges$] constructs a cardinal quiver from a list of vertices and edges.
* The edges of graph$ should be tagged with cardinals.
* The edges incident to one vertex should not be tagged with a cardinal more than once.
* The resulting graph will display with a legend showing the cardinals associated with each edge.
"

DeclareArgumentCount[Quiver, {1, 2}];

Options[Quiver] = $ExtendedGraphOptions;

declareSyntaxInfo[Quiver, {_, _., OptionsPattern[]}];

Quiver[vertices_List, {}, opts:OptionsPattern[]] :=
  ExtendedGraph[Graph[vertices, {}], opts];

Quiver[edges_, opts:OptionsPattern[]] :=
  Quiver[Automatic, edges, opts];

Quiver[graph_Graph, opts:OptionsPattern[]] :=
  ExtendedGraph[graph, opts]

Quiver[vertices_, edges_, opts:OptionsPattern[]] :=
  makeQuiver[vertices, edges, {opts}];

Quiver::invedge = "The edge specification `` is not valid."

processEdge[edge_, _] :=
  (Message[Quiver::invedge, edge]; $Failed);

Quiver::nakededge = "The edge `` is not labeled with a cardinal.";

processEdge[Annotation[e_, rule_Rule], tag_] := Block[{$ea = Append[$ea, rule]},
  processEdge[e, tag]
];

processEdge[edge:(_Rule | _TwoWayRule | DirectedEdge[_, _] | UndirectedEdge[_, _]), None] :=
  (Message[Quiver::nakededge, edge]; $Failed);

processEdge[Labeled[edges:{__Rule}, labels_List], _] /; SameLengthQ[edges, labels] :=
  sowAnnos @ MapThread[
    DirectedEdge,
    {Keys @ edges, Values @ edges, SimplifyCardinalSet /@ labels}
  ];

processEdge[Labeled[edges_, label_], _] :=
  processEdge[edges, label];

processEdge[e_, Verbatim[Alternatives][args__]] :=
  Map[processEdge[e, #]&, {args}];

processEdge[l_ <-> r_, label_] := sowAnnos @ {
  DirectedEdge[l, r, label],
  DirectedEdge[r, l, label]
};

processEdge[l_ -> r_, label_] :=
  sowAnnos @ DirectedEdge[l, r, label];

processEdge[DirectedEdge[l_, r_, Verbatim[Alternatives][args__]], z_] :=
  processEdge[DirectedEdge[l, r, #], z]& /@ {args};

processEdge[DirectedEdge[l_, r_], c_] :=
  sowAnnos @ DirectedEdge[l, r, c];

processEdge[UndirectedEdge[a_, b_], c_] :=
  sowAnnos @ {DirectedEdge[a, b, c], DirectedEdge[b, a, c]};

processEdge[UndirectedEdge[a_, b_, c_], _] :=
  sowAnnos @ {DirectedEdge[a, b, c], DirectedEdge[b, a, c]};

processEdge[de:DirectedEdge[_, _, _], _] :=
  sowAnnos @ de;

processEdge[assoc_Association, _] := KeyValueMap[processEdge[#2, #1]&, assoc];
processEdge[Labeled[e_, label_], _] := processEdge[e, label];

processEdge[list_List, label_] := Map[processEdge[#, label]&, list];

sowAnnos[e_] /; $ea === {} := e;

sowAnnos[e_List] := Map[sowAnno, e];
sowAnnos[e_] := (Scan[attachAnno[e], $ea]; e);

attachAnno[e_][key_ -> value_] :=
  KeyAppendTo[$edgeAnnotations, key, e -> value]

Quiver::containfailed = "Edges contain $Failed."

$maxVertexCount = 150;
makeQuiver[vertices_, edges_, newOpts_] := Scope[

  $edgeAnnotations = <||>;

  If[!MatchQ[edges, {DirectedEdge[_, _, Except[_Alternatives]]..}],
    $ea = {};
    edges = Flatten @ List @ processEdge[edges, None];
    If[ContainsQ[edges, $Failed], ReturnFailed[Quiver::containfailed]];
  ];

  If[!validCardinalEdgesQ[edges],
    reportDuplicateCardinals[edges];
    ReturnFailed[];
  ];

  If[$edgeAnnotations =!= <||>,
    index = AssociationRange @ edges;
    $edgeAnnotations = Map[
      KeyMap[index, Association @ #]&,
      $edgeAnnotations
    ];
  ];

  If[vertices === Automatic, vertices = Union[InVertices @ edges, OutVertices @ edges]];

  ExtendedGraph[
    vertices, edges,
    Sequence @@ newOpts,
    GraphLegend -> None,
    If[$edgeAnnotations =!= <||>, EdgeAnnotations -> $edgeAnnotations, Sequence @@ {}]
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

PublicFunction[ToQuiver]

SetUsage @ "
ToQuiver[obj$] attempts to convert obj$ to a quiver Graph[$$] object.
* If obj$ is already a quiver graph, it is returned unchanged.
* If obj$ is a list of rules, it is converted to a quiver graph.
* Otherwise, $Failed is returned.
"

ToQuiver = MatchValues[
  graph_Graph := If[QuiverQ @ graph, graph, Quiet @ Quiver @ graph];
  edges_List  := Quiet @ Quiver @ edges;
  str_String  := BouquetQuiver @ str;
  _           := $Failed;
];

(**************************************************************************************************)

PublicFunction[QuiverQ]

SetUsage @ "
QuiverQ[graph$] returns True if graph$ represents a cardinal quiver.
* A cardinal quiver must have a cardinal associated with every edge.
* A cardinal quiver should contain only directed edges.
* A cardinal should not be present on more than one edge incident to a vertex.
"

QuiverQ[g_] := EdgeTaggedGraphQ[g] && validCardinalEdgesQ[EdgeList[g]];

validCardinalEdgesQ[edges_] := And[
  MatchQ[edges, {DirectedEdge[_, _, _]..}],
  AllTrue[GroupBy[edges // SpliceCardinalSetEdges, Last], checkForDuplicateCardinals]
];

checkForDuplicateCardinals[edges_] :=
  DuplicateFreeQ[InVertices @ edges] && DuplicateFreeQ[OutVertices @ edges];

(**************************************************************************************************)

PublicFunction[FreeQuiver]

SetUsage @ "
FreeQuiver[graph$] returns a cardinal quiver for graph$, assigning a unique formal symbol \
to each edge in the graph$.
* Undirected edges are transformed into pairs of opposite directed edges.
"

$formalSymbols = Map[letter |-> Symbol["\\" <> "[Formal" <> letter <> "]"], CharacterRange["A", "Z"]];

toFreeQuiverEdge[head_[a_, b_]] :=
  head[a, b, $formalSymbols[[$count++]]];

DeclareArgumentCount[FreeQuiver, 1];

declareSyntaxInfo[FreeQuiver, {_}];

Options[FreeQuiver] = $ExtendedGraphOptions;

FreeQuiver[graph_, opts:OptionsPattern[]] := Scope[
  $count = 1;
  makeQuiver[VertexList @ graph, Map[toFreeQuiverEdge, EdgeList @ graph], {opts}]
];

(**************************************************************************************************)

PublicFunction[CardinalEdgeAdd]

CardinalEdgeAdd[quiver_, spec_, userOpts___Rule] := Scope[
  newQuiver = Quiver @ spec;
  If[!QuiverQ[newQuiver], ReturnFailed[]];
  opts = ExtractExtendedGraphOptions[quiver];
  opts //= ReplaceOptions[Cardinals -> Join[CardinalList @ quiver, CardinalList @ newQuiver]];
  ExtendedGraph[
    VertexList @ quiver,
    Join[EdgeList @ quiver, EdgeList @ newQuiver],
    userOpts,
    Sequence @@ opts
  ]
];