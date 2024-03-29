PublicFunction[CardinalList]

SetUsage @ "
CardinalList[quiver$] returns the list of cardinals in a quiver.
* The cardinals are returned in sorted order.
* If the graph has no tagged edges, None is returned.
"

CardinalList[graph_Graph] := None;

CardinalList[graph_Graph ? EdgeTaggedGraphQ] := Rep[
  AnnotationValue[graph, Cardinals],
  ($Failed | Auto) :> extractCardinals[graph]
];

CardinalList[edges_List] :=
  Dedup @ SpliceCardinalSets @ UniqueCases[edges, DirectedEdge[_, _, c_] :> c];

extractCardinals[graph_] := Decases[Union @ SpliceCardinalSets @ EdgeTags @ graph, Null];

(**************************************************************************************************)

PublicFunction[TaggedAdjacencyMatrices]

Options[TaggedAdjacencyMatrices] = {"Antisymmetric" -> False};

TaggedAdjacencyMatrices[graph_ ? EdgeTaggedGraphQ, OptionsPattern[]] := Scope[
  n = VertexCount @ graph;
  assoc = EdgePairsToAdjacencyMatrix[#, n]& /@ TaggedEdgePairs[graph];
  If[OptionValue["Antisymmetric"],
    JoinTo[assoc, Assoc @ KVMap[Inverted[#1] -> Transpose[#2]&, assoc]];
  ];
  assoc
]

(**************************************************************************************************)

PublicFunction[TaggedEdgePairs]

TaggedEdgePairs[graph_ ? EdgeTaggedGraphQ] :=
  GroupBy[
    EdgeList @ ToIndexGraph @ graph,
    L -> Fn[{P1[#], P2[#]}]
  ]

TaggedEdgePairs[graph_, "Directed"] := TaggedEdgePairs[graph];

TaggedEdgePairs[graph_ ? EdgeTaggedGraphQ, "Undirected"] :=
  GroupBy[
    EdgeList @ ToIndexGraph @ graph,
    L -> Fn[Splice[{{P1[#], P2[#]}, {P2[#], P1[#]}}]],
    Id
  ]

(**************************************************************************************************)

PublicFunction[TaggedEdgeLists]

TaggedEdgeLists[graph_ ? EdgeTaggedGraphQ] :=
  GroupBy[EdgeList @ graph, L]

(**************************************************************************************************)

PublicFunction[TagIndices]

SetUsage @ "
TagIndices[graph$] returns an association from cardinals to the indices of edges on which they are present.
"

TagIndices[graph_] := Scope[
  $tagAssoc = <||>;
  ScanIndexed[processTagEntry, EdgeTags @ graph];
  $tagAssoc
];

TagIndices[graph_, Signed -> isSigned_] := Scope[
  $isTISigned = isSigned; TagIndices @ graph
];

$isTISigned = False;

processTagEntry[tag_, {part_}] :=
  KAppTo[$tagAssoc, tag, part];

processTagEntry[CardinalSet[tags_], {part_}] :=
  Scan[KAppTo[$tagAssoc, If[$isTISigned, Id, StripInverted] @ #1, part]&, tags];


(**************************************************************************************************)

PublicFunction[EdgeToTagIndex]

EdgeToTagIndex[graph_ ? EdgeTaggedGraphQ] := EdgeToTagIndex @ EdgeList @ graph;

EdgeToTagIndex[edges_List] := Scope[
  $index = UAssoc[];
  Scan[insertETTI, edges];
  $index
];

insertETTI[head_[a_, b_, card_]] := ($index[head[a, b]] = card; $index[head[b, a]] = Inverted @ card);

(**************************************************************************************************)

PublicFunction[TagVertexOutTable]

SetUsage @ "
TagVertexOutTable[graph$] returns an association from each cardinal to its VertexOutTable.
TagVertexOutTable[graph$, invalid$] uses invalid$ instead of None.
* If a cardinal is not incident to a given vertex, the corresponding entry is None.
* Keys are included for inversions of cardinals.
* As there is a maximum of edge for a given vertex and cardinal, table entries are single integers or None.
"

TagVertexOutTable[graph_, invalid_:None] := Scope[
  cardinals = CardinalList @ graph;
  igraph = ToIndexGraph @ graph;
  cardinals = Join[cardinals, Inverted /@ cardinals];
  outTables = ConstantAssociation[cardinals, Repeat[invalid, VertexCount @ igraph]];
  ({src, dst, tag} |-> (
      Part[outTables, Key @ tag, src] = dst;
      Part[outTables, Key @ Inverted @ tag, dst] = src;
  )) @@@ SpliceCardinalSetEdges @ EdgeList[igraph];
  outTables
];

(**************************************************************************************************)

PublicFunction[VertexTagAdjacencyAssociation]

SetUsage @ "
VertexTagAdjacencyAssociation[graph$] returns an association from vertices to lists of their neighbors.
"

VertexTagAdjacencyAssociation[graph_] := Scope[
  vlist = VertexList @ graph;
  Map[
    pairs |-> GroupPairs[ExtractIndices[vlist, pairs]],
    TaggedEdgePairs[graph, "Undirected"]
  ]
]

(**************************************************************************************************)

PublicFunction[VertexTagTable]

SetUsage @ "
VertexTagTable[graph$] returns a list of lists {tags$1, tags$2, $$} where tag$i is the list of tags \
present on vertex v$i.
"

VertexTagTable[graph_, splice_:True] := Scope[
  rules = {#1 -> #3, #2 -> Inverted[#3]}& @@@ If[splice, SpliceCardinalSetEdges, Id] @ EdgeList[graph];
  Lookup[Merge[Flatten @ rules, Id], VertexList @ graph, {}]
]

(**************************************************************************************************)

PublicFunction[VertexOutTagTable]

SetUsage @ "
VertexOutTagTable[graph$] returns a list of lists {tags$1, tags$2, $$} where tag$i is the list of tags \
present on vertex v$i in the outgoing direction.
"

VertexOutTagTable[graph_, splice_:True] := Scope[
  rules = #1 -> #3& @@@ If[splice, SpliceCardinalSetEdges, Id] @ EdgeList[graph];
  Lookup[Merge[rules, Id], VertexList @ graph, {}]
]

(**************************************************************************************************)

PublicFunction[VertexOutVertexTagTable]

SetUsage @ "
VertexOutVertexTagTable[graph$] returns a list of pairs {pairs$1, pair$2, $$} where pairs$i is the list of pairs
of the form {$vertex$j, tag$j} present on vertex v$i in the outgoing direction.
"

VertexOutVertexTagTable[graph_, splice_:True] := Scope[
  indexGraph = ToIndexGraph @ graph;
  rules = #1 -> {#2, #3}& @@@ If[splice, SpliceCardinalSetEdges, Id] @ EdgeList @ indexGraph;
  Lookup[Merge[rules, Id], VertexList @ indexGraph, {}]
]

(**************************************************************************************************)

PublicFunction[VertexInTagTable]

SetUsage @ "
VertexInTagTable[graph$] returns a list of lists {tags$1, tags$2, $$} where tag$i is the list of tags \
present on vertex v$i in the incoming direction.
* The tags are wrapped with Inverted[$$].
"

VertexInTagTable[graph_, splice_:True] := Scope[
  rules = #2 -> Inverted[#3]& @@@ If[splice, SpliceCardinalSetEdges, Id] @ EdgeList[graph];
  Lookup[Merge[rules, Id], VertexList @ graph, {}]
]

(**************************************************************************************************)

PublicFunction[TagVertexAdjacentEdgeTable]

SetUsage @ "
TagVertexAdjacentEdgeTable[graph$] returns an association from each cardinal to its VertexAdjacentEdgeTable.
TagVertexAdjacentEdgeTable[graph$, invalid$] uses invalid$ instead of None.
* If a cardinal is not incident to a given vertex, the corresponding entry is None.
* Keys are included for inversions of cardinals.
* As there is a maximum of edge for a given vertex and cardinal, table entries are single integers or None.
* If the option %Signed -> True is provided, edges will be wrapped in Inverted if they are traversed in the \
reversed direction.
"

Options[TagVertexAdjacentEdgeTable] = {Signed -> False};

TagVertexAdjacentEdgeTable[graph_, opts:OptionsPattern[]] :=
  TagVertexAdjacentEdgeTable[graph, None, opts];

TagVertexAdjacentEdgeTable[graph_, invalid:Except[_Rule], OptionsPattern[]] :=
  tagVertexAdjacentEdgeTableInternal[graph, invalid, OptionValue[Signed]]

tagVertexAdjacentEdgeTableInternal[graph_, none_, signed_] := Scope[
  cardinals = CardinalList @ graph;
  cardinals = Join[cardinals, Inverted /@ cardinals];
  cardInd = UAssoc @ AssociationRange @ cardinals;
  vectors = Repeat[none, {Len @ cardinals, VertexCount @ graph}];
  tags = EdgeTags @ graph;
  tagInds = Lookup[cardInd, tags];
  invTagInds = Lookup[cardInd, Inverted /@ tags];
  negator = If[signed, Inverted, Id];
  ScanThread[
    i = 1;
    {edge, tagInd, invTagInd} |-> (
      Part[vectors, tagInd,    P1[edge]] = i;
      Part[vectors, invTagInd, P2[edge]] = negator[i++]
    ),
    {EdgePairs @ graph, tagInds, invTagInds}
  ];
  AssocThread[cardinals, ToPacked /@ vectors]
];

(**************************************************************************************************)

PublicFunction[EdgeTagAssociation]

EdgeTagAssociation[graph_] := AssocThread[{#1, #2}& @@@ EdgeList[graph], EdgeTags @ graph];