Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageScope["$EdgeHead"]

$EdgeHead = DirectedEdge | UndirectedEdge;


PackageScope["summaryItem"]

summaryItem[a_, b_] := BoxForm`SummaryItem[{a <> ": ", b}];

PackageExport["SafePrint"]

SafePrint = print;

PackageExport["SafeTap"]
PackageExport["UnsafeTap"]


PackageScope["print"]
PackageScope["echo"]
PackageScope["echoAs"]

$maxPrintRate = 20;
$printCount = 0;
$nextPrintTime = 0;

shouldPrint[type_] := (
    If[SessionTime[] > $nextPrintTime, $nextPrintTime = SessionTime[] + 1; $printCount = 0];
    If[(++$printCount) > $maxPrintRate,
      If[$printCount == $maxPrintRate + 1, Print["Pausing ", type]]; False,
      True
    ]
);

SetHoldAll[print];
print[e___] := If[shouldPrint["print"], Print[e], Null];

SetHoldAll[echo];
echo[e_] := If[shouldPrint["echo"], Echo[e], e];

SetHoldAll[echoAs];
echoAs[label_][e_] := If[shouldPrint["echo"], Echo[e, label], e];

Format[seqForm[a_], StandardForm] := a;
Format[seqForm[args___], StandardForm] := Row[{args}, ","];

UnsafeTap[f_][args___] :=
  Module[{temp = f[args]}, Echo[RightTeeArrow[seqForm[args], temp]]; temp]

SafeTap[f_][args___] := If[shouldPrint["tap"],
  Module[{temp = f[args]}, Echo[RightTeeArrow[seqForm[args], temp]]; temp],
  f[args]
];


PackageExport["EchoIn"]

SetHoldFirst[EchoIn];

EchoIn[f_[args___]] :=
  Module[{temp = f[args]}, Echo[RightTeeArrow[seqForm[args], temp]]; temp];


PackageScope["declareFormatting"]
PackageScope["$isTraditionalForm"]

declareFormatting[rules__RuleDelayed] := Scan[declareFormatting, {rules}];
declareFormatting[lhs_ :> rhs_] :=
  With[{head = First @ PatternHead[lhs]}, {isProtected = ProtectedFunctionQ[head]},
    If[isProtected, Unprotect[head]];
    Format[lhs, StandardForm] := Block[{$isTraditionalForm = False}, rhs];
    Format[lhs, TraditionalForm] := Block[{$isTraditionalForm = True}, rhs];
    If[isProtected, Protect[head]];
  ];

declareFormatting[___] := Panic["BadFormatting"]


PackageExport["EdgePairs"]

SetUsage @ "
EdgePairs[graph$] gives the list of {{u$1, v$1}, {u$2, v$2}, $$}} such that \
vertex with index u$i is connected to vertex with index v$i.
"

EdgePairs[graph_] := AdjacencyMatrix[graph]["NonzeroPositions"];


PackageExport["VertexOutTable"]
PackageExport["VertexInTable"]

SetUsage @ "
VertexOutTable[graph$] returns a list of lists {out$1, out$2, $$} where out$i is a list of the \
indices of the vertices that are have a connection from vertex i$.
"

SetUsage @ "
VertexInTable[graph$] returns a list of lists {in$1, in$2, $$} where in$i consists of the \
indices of the vertices that are have a connection to vertex i$.
"

VertexOutTable[graph_] := AdjacencyMatrix[graph]["AdjacencyLists"];
VertexInTable[graph_] := Transpose[AdjacencyMatrix[graph]]["AdjacencyLists"];


PackageExport["VertexInOutTable"]

SetUsage @ "
VertexInOutTable[graph$] returns a list of pairs of lists {{in$1, out$1}, {in$2, out$2}, $$} where in$i \
is the list of indices of vertices that are have a connection to vertex i$, and out$i is the \
list of indices of vertices that have a connection from vertex i$.
"

VertexInOutTable[graph_] := Scope[
  adj = AdjacencyMatrix[graph];
  Transpose[{adj["AdjacencyLists"], Transpose[adj]["AdjacencyLists"]}]
];


PackageExport["VertexOrientedOutTable"]

SetUsage @ "
VertexOrientedOutTable[graph$] returns a list of pairs of lists {{dout$1, uout$1}, {dout$2, uout$2}, $$} \
where dout$i is the list of indices of vertices that are have a directed edge from vertex i$, and uout$i is \
the list of indices of vertices that have a undirected edge from vertex i$.
"

toOutTable[count_, edges_] := Lookup[GroupBy[edges, First -> Last], Range[count], {}];

VertexOrientedOutTable[graph_] := Scope[
  edges = EdgeList @ IndexGraph @ graph; count = VertexCount[graph];
  dir = Cases[edges, _DirectedEdge];
  undir = Cases[edges, _UndirectedEdge];
  Transpose @ {
    toOutTable[count, dir],
    toOutTable[count, Join[undir, Reverse[undir, 2]], 1]
  }
];

PackageExport["FromVertexOrientedOutTable"]

SetUsage @ "
FromVertexOrientedOutTable[{{dout$1, uout$1}, {dout$2, uout$2}, $$}, {v$1, v$2, $$}] returns a graph in which
vertex v$i is connected to v$j if j is an element of dout$i or uout$i.
* This function is the inverse of VertexOrientedOutTable.
"

PackageExport["VertexOutAssociation"]
PackageExport["VertexInAssociation"]

SetUsage @ "
VertexOutAssociation[graph$] returns an association of lists <|v$1 -> out$1, v$2 -> out$2, $$|> \
where out$i is a list of the vertices that have a connection from v$i.
"

SetUsage @ "
VertexInAssociation[graph$] returns an association of lists <|v$1 -> in$1, v$2 -> in$2, $$|> \
where in$i is a list of the vertices that have a connection to v$i.
"

tableToAssoc[vertices_, table_] := Association @ MapIndexed[
  Part[vertices, First @ #2] -> Part[vertices, #1]&,
  table
];

VertexOutAssociation[graph_] :=
  tableToAssoc[VertexList @ graph, VertexOutTable @ vertices];

VertexInAssociation[graph_] :=
  tableToAssoc[VertexList @ graph, VertexInTable @ vertices];


PackageExport["VertexInOutAssociation"]

SetUsage @ "
VertexInOutAssociation[graph$] returns an association of lists <|v$1 -> {in$1, out$1}, v$2 -> {in$2, out$2}, $$|> \
where in$i is the list of indices of vertices that are have a connection to vertex i$, and out$i is the \
list of indices of vertices that have a connection from vertex i$.
"

VertexInOutAssociation[graph_] := Scope[
  vertices = VertexList[graph];
  Association @ MapIndexed[
    Part[vertices, First @ #2] -> {Part[vertices, First[#1]], Part[vertices, Last[#1]]}&,
    VertexInOutTable[graph]
  ]
];



PackageExport["GraphCorners"]

GraphCorners[graph_] := Scope[
  degree = DegreeCentrality[graph];
  vertices = Pick[VertexList[graph], degree, Min[degree]];
  SortBy[vertices, LatticeVertexAngle]
];


PackageScope["integersToVertices"]

integersToVertices[graph_Graph, expr_] :=
  integersToVertices[VertexList[graph], expr];

integersToVertices[vertices_List, expr_] :=
  expr /. {i:{__Integer} :> Part[vertices, i], i_Integer :> Part[vertices, i]};


PackageScope["toListOfLists"]

toListOfLists = MatchValues[
  list:{__List} := list;
  list_List := {list};
  _ := $Failed;
];


Format[Negated[e_], StandardForm] := OverBar[e];