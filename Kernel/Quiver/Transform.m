PublicFunction[MapEdgeTags]

SetUsage @ "
MapEdgeTags[f$, graph$] applies the function f$ to the edge tags of edges of graph$.
MapEdgeTags[{edge$1, edge$2, $$}, f$] applies f$ to tags of a list of edges.
"

MapEdgeTags[f_, list_ ? EdgeListQ] :=
  MapAt[f, list, {All, 3}];

MapEdgeTags[f_, graph_Graph ? EdgeTaggedGraphQ] := Graph[
  VertexList @ graph,
  MapEdgeTags[f, EdgeList @ graph],
  Options @ graph
];

MapEdgeTags[_, graph_Graph] := graph;

MapEdgeTags[_, _] := $Failed

MapEdgeTags[f_][graph_] := MapEdgeTags[f, graph];

(**************************************************************************************************)

PublicFunction[InvertCardinals]

SetUsage @ "
InvertCardinals[graph$] negates all the cardinals of graph$.
InvertCardinals[graph$, {card$i}] negates specific cardinals.
"

(* TODO: handle CardinalSet *)

InvertCardinals[graph_ ? EdgeTaggedGraphQ, cards_List] := Scope[
  cardinals = CardinalList @ graph;
  cardP = Alternatives @@ cards;
  MapEdgeTags[AssociationMap[If[MatchQ[#, cardP], Inverted[#], #]&, cardinals], graph]
];

InvertCardinals[graph_ ? EdgeTaggedGraphQ] :=
  InvertCardinals[graph, CardinalList @ graph];

(**************************************************************************************************)

PublicFunction[RemoveEdgeTags]

SetUsage @ "
RemoveEdgeTags[graph$] removes edge tags from edges of graph$, returning a new graph.
RemoveEdgeTags[{edge$1, edge$2, $$}] removes edge tags from edge$i, returning a new list.
* This turns a cardinal quiver into an unlabelled quiver.
"

RemoveEdgeTags = Case[
  list_ ? EdgeListQ              := Take[If[FreeQ[list, CardinalSet], list, SpliceCardinalSetEdges @ list], All, 2];
  graph_Graph ? EdgeTaggedGraphQ := Graph[
    VertexList @ graph,
    RemoveEdgeTags @ EdgeList @ graph,
    Options @ graph
  ];
  graph_Graph                    := graph;
  _ := $Failed
];

(**************************************************************************************************)

PublicFunction[DeleteCardinal]

DeleteCardinal[graph_, card_] := Scope[
  opts = Options[graph];
  {vertices, edges} = VertexEdgeList[graph];
  edges //= Map[deleteCard[card | Inverted[card]]];
  cardinals = AnnotationValue[graph, Cardinals];
  res = Graph[vertices, edges, opts];
  If[ListQ[cardinals], res = Annotate[res, Cardinals -> DeleteCases[cardinals, card]]];
  res
];

deleteCard[c_][head_[a_, b_, t_]] /; MatchQ[t, c] := Nothing;

deleteCard[c_][head_[a_, b_, CardinalSet[l_List /; MemberQ[l, c]]]] :=
  head[a, b, SimplifyCardinalSet @ CardinalSet @ DeleteCases[l, c]];

deleteCard[c_][other_] := other;

(**************************************************************************************************)

PublicFunction[ExpandCardinalSetEdges]

SetUsage @ "
ExpandCardinalSetEdges[graph$] expands any edges tagged with CardinalSet into multiple edges with \
one cardinal each.
* CombineMultiedges is the inverse of ExpandCardinalSetEdges.
"

ExpandCardinalSetEdges[graph_] := If[
  FreeQ[EdgeTags @ graph, CardinalSet], graph,
  Graph[
    VertexList @ graph,
    SpliceCardinalSetEdges @ EdgeList @ graph,
    Options @ graph
  ]
];

(**************************************************************************************************)

PublicFunction[CombineMultiedges]

SetUsage @ "
CombineMultiedges[graph$] combines edges that share the same endpoints into single edges, combining any cardinals they have.
CombineMultiedges[edges$] combines edges in an edge list directly.
* ExpandCardinalSetEdges is the inverse of CombineMultiedges.
"

CombineMultiedges[graph_Graph] := Graph[
  VertexList[graph],
  combineMultiedgesInternal @ EdgeList @ graph,
  Options @ graph
];

CombineMultiedges[edges_List ? EdgeListQ] :=
  combineMultiedgesInternal[edges];

combineMultiedgesInternal[{}] := {};

combineMultiedgesInternal[edges_] := Scope[
  {plainEdges, tags} = Transpose @ Map[separateTag, edges];
  edgeGroups = PositionIndex[plainEdges];
  If[SameLengthQ[edgeGroups, plainEdges], Return @ edges];
  edges = KeyValueMap[
    {edge, indices} |-> reattachTag[edge, DeleteNone @ Part[tags, indices]],
    edgeGroups
  ]
];

separateTag = Case[
  DirectedEdge[a_, b_, t_] /; Order[a, b] == -1 := {DirectedEdge[b, a], Inverted @ t};
  DirectedEdge[a_, b_, t_]                      := {DirectedEdge[a, b], t};
  UndirectedEdge[a_, b_, t_]                    := {Sort @ UndirectedEdge[a, b], t};
  edge_                                         := {Sort @ edge, None}
];

reattachTag[edge_, {}] := edge;
reattachTag[edge_, {tag_}] := Append[edge, tag];
reattachTag[edge_, tags_List] := reorientCS @ Append[edge, SimplifyCardinalSet @ CardinalSet @ tags];

reorientCS = Case[
  head_[a_, b_, CardinalSet[cs:{__Inverted}]] := head[b, a, CardinalSet[StripInverted /@ cs]];
  other_ := other;
];

(**************************************************************************************************)

PublicFunction[CombineForwardMultiedges]

CombineForwardMultiedges[edges_List] :=
  toCombinedForwardMultiedge /@ GatherBy[edges, TakeOperator[2]]

toCombinedForwardMultiedge[{e_}] := e;
toCombinedForwardMultiedge[list_List] := ReplacePart[Part[list, 1], 3 -> CardinalSet[Sort @ Part[list, All, 3]]];

(**************************************************************************************************)

PublicFunction[AddEdgeTags]

AddEdgeTags[spec_, tag_:None] := Scope[$tag = tag; addEdgeTags @ spec];

addEdgeTags = Case[
  list_ ? EdgeListQ              := addEdgeTag /@ list;
  graph_Graph                    := Graph[
    VertexList @ graph,
    Map[addEdgeTag, EdgeList @ graph],
    Options @ graph
  ];
  _ := $Failed
];

addEdgeTag = Case[
  head_[a_, b_] := head[a, b, $tag];
  other_ := other;
];

(**************************************************************************************************)

PublicFunction[RemoveEdgeTag]

RemoveEdgeTag = Case[
  DirectedEdge[a_, b_, ___] := DirectedEdge[a, b];
  UndirectedEdge[a_, b_, ___] := UndirectedEdge[a, b];
];

(**************************************************************************************************)

PublicFunction[RenameCardinals]

RenameCardinals[graph_Graph, renaming:{__Str}] :=
  RenameCardinals[graph, RuleThread[CardinalList @ graph, renaming]]

RenameCardinals[graph_Graph, {}] := graph;

RenameCardinals[graph_Graph, renaming_List] :=
  RenameCardinals[graph, RuleThread[CardinalList @ graph, renaming]];

RenameCardinals[graph_Graph, renaming:{__Rule}] := Scope[
  {vertices, edges} = VertexEdgeList @ graph;
  replacer = ReplaceAll @ Dispatch @ renaming;
  edges = MapAt[replacer, edges, {All, 3}] /. DirectedEdge[a_, b_, Inverted[c_]] :> DirectedEdge[b, a, c];
  replacer = ReplaceAll @ Dispatch @ (renaming /. Inverted -> Id);
  opts = DropOptions[AnnotationRules] @ Options @ graph;
  annos = Replace[
    ExtendedGraphAnnotations @ graph,
    opt:Rule[(VisibleCardinals | Cardinals | CardinalColors), _] :> replacer[opt],
    {1}
  ];
  Graph[
    vertices, edges,
    Sequence @@ opts,
    AnnotationRules -> {"GraphProperties" -> annos}
  ]
];

RenameCardinals[renaming_][graph_] :=
  RenameCardinals[graph, renaming];

(**************************************************************************************************)

PublicFunction[TruncateQuiver]
PublicHead[TruncatedVertex]

declareFormatting[
  TruncatedVertex[v_, a_] :> Superscript[v, a]
];

SetUsage @ "
TruncatedVertex[vertex$, card$] represents a vertex that has been truncated in the direction card$.
";

Options[TruncateQuiver] = Prepend["AllowSkips" -> True] @ $ExtendedGraphOptions;

TruncateQuiver[quiver_, opts:OptionsPattern[]] :=
  TruncateQuiver[quiver, Automatic, opts];

TruncateQuiver[quiver_, cardinals:Except[_Rule], userOpts:OptionsPattern[]] := Scope[
  UnpackOptions[allowSkips];
  {vertices, edges} = VertexEdgeList @ quiver;
  SetAutomatic[cardinals, t = CardinalList[quiver]; Join[t, Inverted /@ t]];
  If[StringQ[cardinals], cardinals //= ToPathWord];
  ordering = AssociationRange[cardinals]; $n = Len @ cardinals;
  tagTable = Map[SortBy[cardOrder[ordering]], VertexTagTable[quiver, False]];
  tagOutTable = TagVertexOutTable @ quiver;
  vertexCoords = GraphVertexCoordinates @ quiver;
  CollectTo[{truncEdges, truncVertices, truncCoords},
  ScanThread[{v, tags, coord, tagOut} |-> (
    cornerVerts = Map[TruncatedVertex[v, #]&, tags];
    cornerEdges = If[allowSkips, cornerEdge, noskipCornerEdge[ordering]] /@ Partition[cornerVerts, 2, 1, 1];
    cornerCoords = Map[
      PointAlongLine[
        {coord, Part[vertexCoords, Lookup[tagOut, Replace[#, CardinalSet[s_] :> P1[s]]]]},
        Scaled[0.25]]&,
      tags
    ];
    BagInsert[truncEdges, cornerEdges, 1];
    BagInsert[truncVertices, cornerVerts, 1];
    BagInsert[truncCoords, cornerCoords, 1])
  ,
    {vertices, tagTable, vertexCoords, AssociationTranspose @ tagOutTable}
  ]];
  truncEdges = Flatten @ {
    truncEdges, truncatedEdge /@ edges
  };
  opts = Options @ quiver;
  opts = Replace[opts, (AnnotationRules -> annos_) :>
    AnnotationRules -> DropOptions[annos, VertexAnnotations]];
  Graph[
    truncVertices,
    truncEdges,
    VertexCoordinates -> truncCoords,
    Cardinals -> {"f", "r"},
    Sequence @@ opts, FilterOptions @ userOpts
  ]
];

cardOrder[ordering_][CardinalSet[list_]] := Min @ Lookup[ordering, list];
cardOrder[ordering_][e_] := ordering[e];

tqSuccQ[a_, b_] := MatchQ[b - a, 1 | (1 + $n) | (1 - $n)];

noskipCornerEdge[ordering_][{a:TruncatedVertex[v1_, c1_], b:TruncatedVertex[v2_, c2_]}] :=
  If[tqSuccQ[ordering @ c1, ordering @ c2], DirectedEdge[a, b, "r"], Nothing];

cornerEdge[{a_, b_}] := DirectedEdge[a, b, "r"]

truncatedEdge[DirectedEdge[a_, b_, c_]] :=
  DirectedEdge[TruncatedVertex[a, c], TruncatedVertex[b, Inverted @ c], CardinalSet[{"f", Inverted @ "f"}]];
