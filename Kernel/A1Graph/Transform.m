PublicFunction[DiscardVertices, SelectVertices]

DiscardVertices[graph_Graph, filter_] := Subgraph[graph, Discard[VertexList[graph], filter]];
SelectVertices[graph_Graph, filter_] := Subgraph[graph, Select[VertexList[graph], filter]];

(**************************************************************************************************)

PublicFunction[GraphRelabel]

(* TODO: is this the same as MapVertices? *)

GraphRelabel[graph_Graph, f_] :=
  VertexReplace[graph, Map[# -> f[#]&, VertexList[graph]]];


(**************************************************************************************************)

PublicFunction[InvertGraph]

InvertGraph[g_Graph] := Scope[
  Graph[
    VertexList @ g, ReverseEdges @ EdgeList @ g,
    Options @ g
  ]
]

(**************************************************************************************************)

PublicFunction[ReverseEdges, ReverseEdge]

ReverseEdges[edges_List] := Map[ReverseEdge, edges];

ReverseEdge = Case[
  head_[a_, b_] := head[b, a];
  head_[a_, b_, c_] := head[b, a, c];
];

(**************************************************************************************************)

PublicFunction[MapVertices]

(* TODO: Map on GraphOrigin, etc *)

MapVertices[f_, graph_Graph] := Graph[
  Map[f, VertexList @ graph],
  SafeMapAt[f, EdgeList @ graph, {All, 1;;2}],
  Options @ graph
];

MapVertices[f_, {}] := {};
MapVertices[f_, edges_List] := MapAt[f, edges, {All, 1;;2}];

(**************************************************************************************************)

PublicFunction[MapEdges]

SetUsage @ "
MapEdges[f$, graph$] applies the function f$ to the edges of f$.
"

MapEdges[f_, graph_Graph] := Graph[
  VertexList @ graph,
  Map[f, EdgeList @ graph],
  Options @ graph
];

MapEdges[f_, _] := $Failed;

MapEdges[f_][g_] := MapEdges[f, g];

(**************************************************************************************************)

PublicFunction[VertexSelect]

SetUsage @ "
VertexSelect[graph$, predicate$] gives the subgraph of the vertices sastisfying predicate$.
The predicate can be one of the following:
| f$ | function applied to vertex name |
| v$ -> f$ | function applied to value of vertex value v$ |
| v$ -> n$ | part n$ of a list |
| v$ -> 'key$' | key 'key$' of an assocation |
| v$ -> f$ -> g$ | chained function application |
| {v$1, v$2, $$} -> f$ | f$[v$1, v$2, $$] for each vertex |
The vertex values v$i are values defined for each vertex and can be any of the following:
| 'Name' | vertex name |
| 'Index' | vertex integer index |
| 'Coordinates' | graphical coordinates |
| 'Distance' | distance from %GraphOrigin |
| {'Distance', v$} | distance from vertex v$ |
| 'key$' | vertex annotation 'key$' |
| {v$1, v$2, $$} | a combination of the above |
For graphs created with %LatticeGraph and %LatticeQuiver, the following annotations are available:
| 'AbstractCoordinates' | vector of abstract coordinates |
| 'GeneratingVertex' | corresponding vertex in the fundamental quiver |
| 'Norm' | norm of the abstract coordinates |
"

VertexSelect::notvertex = "`` is not a valid vertex of the graph."
VertexSelect::notboolres = "Predicate did not return True or False for input: ``."
VertexSelect::badgraphannokey = "The requested annotation `` is not present in the graph. Present annotations are: ``."

VertexSelect[graph_, f_] := Scope @ Catch[

  vertices = VertexList @ graph;
  $vertexAnnotations = LookupExtendedOption[graph, VertexAnnotations];
  $vertexCoordinates := $vertexCoordinates = P1 @ ExtractGraphPrimitiveCoordinates[graph];

  GraphScope[graph,
    bools = toVertexResults[f] /. Indeterminate -> True;
    If[!VectorQ[bools, BooleanQ], ReturnFailed[]];
    newVertices = Pick[$VertexList, bools];
  ];

  ExtendedSubgraph[graph, newVertices, Automatic]
,
  VertexSelect
];

toVertexResults = Case[
  data_List -> f_           := MapThread[checkBool @ toFunc @ f, toVertexData @ data];
  data_ -> f_              := Map[checkBool @ toFunc @ f, toVertexData @ data];
  f_                        := Map[checkBool @ f, $VertexList];
];

toVertexData = Case[
  "Name"           := $VertexList;
  "Index"          := Range @ $VertexCount;

  (* todo, make the distance work on regions as well *)
  "Coordinates"    := $vertexCoordinates;
  "Distance"       := %[{"Distance", GraphOrigin}];
  {"Distance", v_} := MetricDistance[$MetricGraphCache, getVertexIndex @ v];
  key_Str          := getAnnoValue[$vertexAnnotations, key];

  list_List        := Map[%, list];
];

toFunc = Case[
  p_Int := PartOperator[p];
  p_Str := PartOperator[p];
  f_ := checkIndet @ f;
  f_ -> g_ := RightComposition[toFunc @ f, toFunc @ g]
];

failSelect[msg_, args___] := (
  Message[MessageName[VertexSelect, msg], args];
  Throw[$Failed, VertexSelect]
);

checkBool[f_][args___] := Catch[
  Replace[
    Check[f[args], $Failed],
    Except[True|False|Indeterminate] :> failSelect["notboolres", SequenceForm[args]]
  ],
  indet
];

checkIndet[f_][args___] :=
  Replace[f[args], Indeterminate :> Throw[Indeterminate, indet]];

getVertexIndex[GraphOrigin] := getVertexIndex @ $GraphOrigin;
getVertexIndex[v_] := Lookup[$VertexIndex, v, failSelect["notvertex", v]];
getAnnoValue[annos_, key_] := Lookup[annos, key, failSelect["badgraphannokey", key, commaString @ Keys @ annos]];

(**************************************************************************************************)

PublicFunction[RemoveSelfLoops]

RemoveSelfLoops[graph_Graph] := Graph[
  VertexList @ graph,
  Select[EdgeList @ graph, P1[#1] =!= P2[#]&],
  Options @ graph
];

(**************************************************************************************************)

PublicFunction[RandomlyPermuteVertices]

SetUsage @ "
RandomlyPermuteVertices[graph$] randomly permutes the %VertexList order of vertices in graph$.
* The option %RandomSeeding controls the pseudorandom permutation.
"

Options[RandomlyPermuteVertices] = {RandomSeeding -> Automatic};

RandomlyPermuteVertices[graph_, OptionsPattern[]] := Scope[

  indices = RandomSeeded[
    RandomSample @ Range @ VertexCount @ graph,
    OptionValue[RandomSeeding]
  ];

  PermuteVertices[graph, indices]
];

(**************************************************************************************************)

PublicFunction[PermuteVertices]

SetUsage @ "
PermuteVertices[graph$, perm$] applies the permutation perm$ to the %VertexList order of vertices in graph$.
"

PermuteVertices[graph_, indices_List] := Scope[

  If[Len[indices] =!= VertexCount[graph] || !PermutedRangeQ[indices], ReturnFailed[]];

  permutor = PartOperator[indices];

  options = Options @ graph;

  coords = LookupOption[options, VertexCoordinates];
  If[ListQ[coords], options //= ReplaceOptions[VertexCoordinates -> permutor[coords]]];

  vertexAnnos = LookupExtendedOption[graph, VertexAnnotations];
  If[AssocQ[vertexAnnos], vertexAnnos //= Map[permutor]];

  result = Graph[permutor @ VertexList @ graph, EdgeList @ graph, Sequence @@ options];
  If[AssocQ[vertexAnnos], result = Annotate[result, VertexAnnotations -> vertexAnnos]];

  result
];