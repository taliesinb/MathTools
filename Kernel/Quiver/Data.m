PublicFunction[CardinalList]

SetUsage @ "
CardinalList[quiver$] returns the list of cardinals in a quiver.
* The cardinals are returned in sorted order.
* If the graph has no tagged edges, None is returned.
"

CardinalList[graph_Graph] := None;

CardinalList[graph_Graph ? EdgeTaggedGraphQ] := Replace[
  AnnotationValue[graph, Cardinals],
  ($Failed | Automatic) :> extractCardinals[graph]
];

CardinalList[edges_List] :=
  DeleteDuplicates @ SpliceCardinalSets @ UniqueCases[edges, DirectedEdge[_, _, c_] :> c];

extractCardinals[graph_] := DeleteCases[Union @ SpliceCardinalSets @ EdgeTags @ graph, Null];

(**************************************************************************************************)

PublicFunction[TaggedAdjacencyMatrices]

Options[TaggedAdjacencyMatrices] = {"Antisymmetric" -> False};

TaggedAdjacencyMatrices[graph_ ? EdgeTaggedGraphQ, OptionsPattern[]] := Scope[
  n = VertexCount @ graph;
  assoc = EdgePairsToAdjacencyMatrix[#, n]& /@ TaggedEdgePairs[graph];
  If[OptionValue["Antisymmetric"],
    JoinTo[assoc, Association @ KeyValueMap[Inverted[#1] -> Transpose[#2]&, assoc]];
  ];
  assoc
]

(**************************************************************************************************)

PublicFunction[TaggedEdgePairs]

TaggedEdgePairs[graph_ ? EdgeTaggedGraphQ] :=
  GroupBy[
    EdgeList @ ToIndexGraph @ graph,
    Last -> Function[{Part[#, 1], Part[#, 2]}]
  ]

TaggedEdgePairs[graph_, "Directed"] := TaggedEdgePairs[graph];

TaggedEdgePairs[graph_ ? EdgeTaggedGraphQ, "Undirected"] :=
  GroupBy[
    EdgeList @ ToIndexGraph @ graph,
    Last -> Function[Splice[{{Part[#, 1], Part[#, 2]}, {Part[#, 2], Part[#, 1]}}]],
    Identity
  ]

(**************************************************************************************************)

PublicFunction[TaggedEdgeLists]

TaggedEdgeLists[graph_ ? EdgeTaggedGraphQ] :=
  GroupBy[EdgeList @ graph, Last]

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
  KeyAppendTo[$tagAssoc, tag, part];

processTagEntry[CardinalSet[tags_], {part_}] :=
  Scan[KeyAppendTo[$tagAssoc, If[$isTISigned, Identity, StripInverted] @ #1, part]&, tags];

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
  outTables = ConstantAssociation[cardinals, ConstantArray[invalid, VertexCount @ igraph]];
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
    pairs |-> GroupBy[ExtractIndices[vlist, pairs], First -> Last],
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
  rules = {#1 -> #3, #2 -> Inverted[#3]}& @@@ If[splice, SpliceCardinalSetEdges, Identity] @ EdgeList[graph];
  Lookup[Merge[Flatten @ rules, Identity], VertexList @ graph, {}]
]

(**************************************************************************************************)

PublicFunction[VertexOutTagTable]

SetUsage @ "
VertexOutTagTable[graph$] returns a list of lists {tags$1, tags$2, $$} where tag$i is the list of tags \
present on vertex v$i in the outgoing direction.
"

VertexOutTagTable[graph_, splice_:True] := Scope[
  rules = #1 -> #3& @@@ If[splice, SpliceCardinalSetEdges, Identity] @ EdgeList[graph];
  Lookup[Merge[rules, Identity], VertexList @ graph, {}]
]

(**************************************************************************************************)

PublicFunction[VertexOutVertexTagTable]

SetUsage @ "
VertexOutVertexTagTable[graph$] returns a list of pairs {pairs$1, pair$2, $$} where pairs$i is the list of pairs
of the form {$vertex$j, tag$j} present on vertex v$i in the outgoing direction.
"

VertexOutVertexTagTable[graph_, splice_:True] := Scope[
  indexGraph = ToIndexGraph @ graph;
  rules = #1 -> {#2, #3}& @@@ If[splice, SpliceCardinalSetEdges, Identity] @ EdgeList @ indexGraph;
  Lookup[Merge[rules, Identity], VertexList @ indexGraph, {}]
]

(**************************************************************************************************)

PublicFunction[VertexInTagTable]

SetUsage @ "
VertexInTagTable[graph$] returns a list of lists {tags$1, tags$2, $$} where tag$i is the list of tags \
present on vertex v$i in the incoming direction.
* The tags are wrapped with Inverted[$$].
"

VertexInTagTable[graph_, splice_:True] := Scope[
  rules = #2 -> Inverted[#3]& @@@ If[splice, SpliceCardinalSetEdges, Identity] @ EdgeList[graph];
  Lookup[Merge[rules, Identity], VertexList @ graph, {}]
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

TagVertexAdjacentEdgeTable[graph_, invalid_, OptionsPattern[]] := Scope[
  outTable = VertexOutEdgeTable @ graph;
  inTable = VertexInEdgeTable @ graph; $invalid = invalid;
  negator = If[OptionValue[Signed], mapInverted, Identity];
  Merge[mergeNone] @ KeyValueMap[
    {key, edgeIndices} |-> {
      key ->          Map[First[Intersection[#, edgeIndices], invalid]&, outTable],
      Inverted[key] -> negator @ Map[First[Intersection[#, edgeIndices], invalid]&, inTable]
    },
    TagIndices[graph, Signed -> True]
  ]
];

mapInverted[e_] := Map[If[# === $invalid, #, Inverted[#]]&, e];

mergeNone[{a_}] := a;
mergeNone[{a_, b_}] := MapThread[If[#1 === $invalid, #2, #1]&, {a, b}];

(**************************************************************************************************)

PublicFunction[EdgeTagAssociation]

EdgeTagAssociation[graph_] := AssociationThread[{#1, #2}& @@@ EdgeList[graph], EdgeTags @ graph];
