PublicFunction[WeaklyConnectedSubquiver, ConnectedSubquiver]

WeaklyConnectedSubquiver[g_Graph ? EdgeTaggedGraphQ, cardSpec_, v_:None] :=
  connectedSubquiver[g, toTagPatt @ cardSpec, v, False];

strongEdgeMatchQ[cards_List][c_] := ElementQ[cards, c];

ConnectedSubquiver[g_Graph ? EdgeTaggedGraphQ, cardSpec_, v_] :=
  connectedSubquiver[g, toTagPatt @ cardSpec, v, True];

connectedSubquiver[g_, cpatt_, v_, isStrong_] := Scope[
  vertices = VertexList @ g;
  edges0 = EdgeList[g];
  edges1 = If[isStrong, strongEdgeTrans, weakEdgeTrans][cpatt] /@ edges0;
  filtered = Graph[vertices, DeleteNone @ edges1];
  subGraph = If[isStrong,
    Subgraph[filtered, VertexOutComponent[filtered, v]],
    First[WeaklyConnectedGraphComponents[filtered, {v}], Graph[{}, {}]]
  ];
  ExtendedSubgraph[g, VertexList @ subGraph, LastColumn @ EdgeList @ subGraph]
];

strongEdgeTrans[cp_][e:DirectedEdge[a_, b_, c_]] :=
  Which[MatchQ[c, cp], DirectedEdge[a, b, e], MatchQ[Inverted @ c, cp], DirectedEdge[b, a, e], True, None];

weakEdgeTrans[cp_][e:DirectedEdge[a_, b_, c_]] :=
  If[MatchQ[c, cp] || MatchQ[c, cp], UndirectedEdge[a, b, e], None];

toTagPatt = Case[
  list_List := Alternatives @@ list;
  other_ := other;
];
