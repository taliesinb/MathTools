Unprotect[PathGraph];
(* fix a weird oversight in the design of PathGraph *)
PathGraph[n_Integer, opts___] := PathGraph[Range[n], opts];


PackageExport["DirectedCycle"]
PackageExport["UndirectedCycle"]

cyclicPairs[first_, vertices___] := Partition[{first, vertices, first}, 2, 1];

DirectedCycle[vertices___] := Splice[DirectedEdge @@@ cyclicPairs[vertices]];
UndirectedCycle[vertices___] := Splice[UndirectedEdge @@@ cyclicPairs[vertices]];


PackageExport["DirectedPath"]
PackageExport["UndirectedPath"]

DirectedPath[vertices___] := Splice[DirectedEdge @@@ Partition[List @ vertices, 2, 1]];
UndirectedPath[vertices___] := Splice[UndirectedEdge @@@ Partition[List @ vertices, 2, 1]];


PackageExport["Clique"]

Clique[vertices___] := Splice[UndirectedEdge @@@ Subsets[{vertices}, {2}]];


(**************************************************************************************************)

PackageExport["EquivalenceGraph"]

EquivalenceGraph[graph_, f_, newOpts___Rule] := Scope[
  vertices = VertexList @ graph;
  vrange = VertexRange @ graph;
  classes = Gather[vrange, f @@ Part[vertices, {#1, #2}]&];
  igraph = IndexGraph[graph];
  contracted = VertexContract[igraph, classes];
  {ivertices, iedges} = VertexEdgeList @ contracted;
  edges = MapAt[Part[vertices, #]&, iedges, {All, 1;;2}];
  vertices = Part[vertices, ivertices];
  opts = Options @ graph;
  ExtendedGraph[vertices, edges, VertexAnnotations -> None, GraphOrigin -> None, newOpts, Sequence @@ opts]
];

(**************************************************************************************************)

PackageExport["GraphAdd"]

SetUsage @ "
GraphAdd[graph$, vertices$, edges$] adds additional vertices and edges to graph$.
* Additional vertices will use vertex coordinates that are given by the mean of their neighbors.
"

GraphAdd[graph_, newVertices_, newEdges_] := Scope[
  CheckIsGraph[1];
  options = Options @ graph;
  {vertices, edges} = VertexEdgeList @ graph;
  newVertices //= ToList;
  newEdges //= ToList;
  newEdges //= Map[toProperEdges];
  If[ContainsQ[newEdges, $Failed], ReturnFailed[]];
  If[IntersectingQ[vertices, newVertices],
    renamedVertices = AdditionalVertex /@ newVertices;
    renamingRules = RuleThread[newVertices, renamedVertices];
    newVertices = renamedVertices;
    newEdges //= ReplaceAll[renamingRules];
  ];
  oldVertexCount = Length @ vertices;
  newVertexCount = Length @ newVertices;
  newGraph = Graph[
    Join[vertices, newVertices],
    Join[edges, newEdges],
    Sequence @@ DeleteOptions[options, VertexCoordinates]
  ];
  If[!GraphQ[newGraph], ReturnFailed[]];
  newGraph //= DeleteVertexAnnotations;
  vertexCoordinates = Lookup[options, VertexCoordinates];
  If[CoordinateMatrixQ[vertexCoordinates],
    vertexCoordinates = Join[vertexCoordinates, ConstantArray[0., {newVertexCount, InnerDimension @ vertexCoordinates}]];
    newVertexIndices = Range[newVertexCount] + oldVertexCount;
    adjTable = Drop[VertexAdjacencyTable @ newGraph, oldVertexCount];
    Do[
      Part[vertexCoordinates, newVertexIndices] = Map[Mean @ Part[vertexCoordinates, #]&, adjTable];
    ,
      {10}
    ];
    newGraph = Graph[newGraph, VertexCoordinates -> vertexCoordinates];
  ];
  newGraph
];

GraphAdd::badedge = "`` is not a valid edge."

toProperEdges = Case[
  a_ -> b_ := DirectedEdge[a, b];
  a_ <-> b_ := UndirectedEdge[a, b];
  d_DirectedEdge := d;
  u_UndirectedEdge := u;
  other_ := (Message[GraphAdd::badedge, other]; $Failed)
];


PackageExport["AdditionalVertex"]

(**************************************************************************************************)

PackageExport["ExtendedSubgraph"]

ExtendedSubgraph[oldGraph_, newVertices_, newEdges_] := Scope[
  options = Options[oldGraph];
  annotations = ExtendedGraphAnnotations[oldGraph];
  vertexCoords = Lookup[options, VertexCoordinates, Automatic];
  oldVertices = VertexList[oldGraph];
  If[newVertices === All,
    newVertexIndices = Range @ Length @ oldVertices;
    newVertices = oldVertices;
  ,
    newVertexIndices = Map[IndexOf[oldVertices, #]&, newVertices];
    newVertexOrdering = Ordering[newVertexIndices];
    newVertices = Part[newVertices, newVertexOrdering];
  ];
  {graphOrigin, vertexAnnotations} = LookupAnnotation[oldGraph, {GraphOrigin, VertexAnnotations}, None];
  If[!MemberQ[newVertices, graphOrigin],
    annotations //= ReplaceOptions[GraphOrigin -> None]];
  sortedNewVertexIndices = Sort @ newVertexIndices;
  If[ListQ[vertexCoords],
    vertexCoords = Part[vertexCoords, sortedNewVertexIndices];
    options //= ReplaceOptions[VertexCoordinates -> vertexCoords];
  ];
  If[AssociationQ[vertexAnnotations],
    vertexAnnotations //= Map[Part[#, sortedNewVertexIndices]&];
    annotations //= ReplaceOptions[VertexAnnotations -> vertexAnnotations];
  ];
  If[newEdges === Automatic,
    newEdges = Select[EdgeList @ oldGraph, MemberQ[newVertices, Part[#, 1]] && MemberQ[newVertices, Part[#, 2]]&]
  ];
  graph = Graph[newVertices, newEdges, options];
  Annotate[graph, annotations]
];

(**************************************************************************************************)

PackageExport["VertexSelect"]

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
  $vertexAnnotations = LookupExtendedGraphAnnotations[graph, VertexAnnotations];
  $vertexCoordinates := $vertexCoordinates = First @ ExtractGraphPrimitiveCoordinates[graph];

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
  "Name"                    := $VertexList;
  "Index"                   := Range @ $VertexCount;

  (* todo, make the distance work on regions as well *)
  "Coordinates"             := $vertexCoordinates;
  "Distance"                := %[{"Distance", GraphOrigin}];
  {"Distance", v_}          := MetricDistance[$MetricGraphCache, getVertexIndex @ v];
  key_String                := getAnnoValue[$vertexAnnotations, key];

  list_List                 := Map[%, list];
];

toFunc = Case[
  p_Integer := PartOperator[p];
  p_String := PartOperator[p];
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

