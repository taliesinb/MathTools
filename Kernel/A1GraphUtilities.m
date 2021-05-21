Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["VertexAnnotations"]
PackageExport["LayoutDimension"]
PackageExport["GraphMetric"]
PackageExport["GraphOrigin"]
PackageExport["Cardinals"]
PackageExport["ViewOptions"]
PackageExport["CoordinateTransformFunction"]
PackageExport["LabelCardinals"]

(**************************************************************************************************)

PackageScope["$extendedGraphUsage"]

$extendedGraphUsage = StringTrim @ "
| %GraphLayout | None | the overall layout method to use for vertices and edges |
| %LayoutDimension | Automatic | number of dimensions of the graph layout |
| %VertexLabels | None | how to label vertices |
| %ImageSize | Automatic | size to plot the graph |
| %GraphRegionHighlight | None | regions of the graph to highlight |
| %GraphLegend | Automatic | legend to attach to the entire graph |
| %ArrowheadSize | Automatic | size of arrowheads |
| %ArrowheadStyle | Automatic | style of arrowheads |
| %ArrowheadPosition | Automatic | position of arrowheads along edges |
| %ArrowheadShape | Automatic | shape of arrowheads |
| %LabelCardinals | False | whether to attach labels to arrowheads |
| %VertexShapeFunction | Automatic | how to draw vertices |
| %EdgeShapeFunction | Automatic | how to draw edges |
| %VertexColorFunction | None | function to obtain colors for vertices |
| %VertexAnnotations | None | association of additional per-vertex data |
| %GraphMetric | Automatic | metric to calculate graph distances |
| %CardinalColors | Automatic | association of cardinal colors |
| %ViewOptions | Automatic | how to project 3D coordinates |
| %CoordinateTransformFunction | None | function to remap coordinates before plotting |

## Arrowheads

* %ArrowheadShape accepts these settings:
| 'Arrow' | solid kinded arrowhead (default) |
| 'Line' | two lines forming a partial triangle |
| 'Disk' | circular disk |
| 'Square' | square |
| 'Diamond' | diamond (rotated square) |
| 'Cone' | thin cone (3D) |
| 'Sphere' | sphere (3D) |
| 'Cardinal' | no arrowhead, use cardinal label |
| None | no arrowheads |
* In addition, %ArrowheadShape supports the suboption {'shape$', %TwoWayStyle -> spec$}.
* %TwoWayStyle -> spec$ determines how to plot a cardinal and its negation together:
| 'Out' | arrowheads facing away from each other |
| 'OutClose' | facing out with backs touching |
| 'In' | arrowheads facing towards each other |
| 'InClose' | facing in with tips touching |
| 'spec$' | one of the regular shapes |

* %ArrowheadSize supports symbol sizes such as Small, Large, etc, or a value Scaled[r$].
These modify scale the automatically chosen size, with Medium leaving it unchanged.

* %ArrowheadStyle can be set to a color or list of directives.

* %CardinalColors -> <|card$1 -> col$1, $$|> determines the colors for arrowheads.

* %ArrowheadPosition -> r$ sets the position of the arrowhead to the fraction r$ along the \
length of the edge.

## Edges

* %EdgeShapeFunction controls how edges are drawn, indepedently of arrowheads, and accepts:
| Automatic | use %Line or %Arrow as appropriate |
| None | do not draw edges |
| f$ | call f$ to obtain graphical primitives |

If a function f$ is given, it is provided with an association containing the following keys:
| 'Coordinates' | the list of {x$, y$} or {x$, y$, z$} coordinates |
| 'Source' | the source vertex |
| 'Target' | the target vertex |
| 'Cardinal' | the cardinal(s) on the edge |

## Vertices

* %VertexShapeFunction controls how vertices are drawn and accepts these settings:
| Automatic | choose a method automatically |
| 'Point' | use %Point[$$] |
| 'Disk' | use %Disk[$$] |
| 'Sphere' | use %Sphere[$$] |
| 'Ball' | use 'Disk' for 2D and 'Sphere' for 3D |
| None | do not draw vertices |

* %VertexSize accepts these settings:
| Automatic | use a safe default size (equivalent to Medium) |
| Small, Medium, $$ | use a symbolic size, with Medium being 0.3 |
| r$ | fraction r$ of the quantiles of inter-vertex distance |
| {v$1 -> s$1, $$, %%All -> s$} | use specific sizes for specific vertices |

* %VertexColorFunction accepts these settings:
| None | color via VertexStyle (default) |
| 'key$' | color with values from VertexAnnotation |
| f$ | apply f$ to vertices to obtain values |
| spec$ -> f$ | apply f$ to result of spec$ |
| %Paletted[spec$, colors$] | use a given named or explicit color palette |
* If a spec produces non-color values, they will colored based on the type of data.
* If %GraphLegend -> Automatic, a color legend will be shown.

## Metrics

* %GraphMetric affects the behavior of %MetricDistance, %MetricDistanceMatrix, and %MetricFindShortestPath.
* The following settings are accepted:
| Automatic | use the default graph distance |
| 'Euclidean' | use root total square of per-cardinal distances |
| %QuadraticForm[$$] | use a quadratic form |
| {s$1, s$2, $$} | use a particular signature |
| n$ | use a homogenous form of degree n$ |
| f$ | apply f$ to association of per-cardinal distances |

## Labeling

* %VertexLabels determines how to label vertices, and accepts these settings:
| None | do not label vertices (default) |
| Automatic | label with vertex names |
| 'Index' | label with vertex indices |
| 'key$' | label with values from VertexAnnotation |
| %Tooltip[spec$] | label vertices via a tooltip |

* %EdgeLabels determines how to label edges, and accepts these settings:
| None | do not label edges |
| 'Index' | label with edge index |
| Automatic | label edges with their cardinals |

* %VertexLabelStyle and EdgeLabelStyle accept these settings:
| Automatic | default |
| styles$ | a list, directive, or individual style |
| {styles$, opt$ -> val$, $$} | provide suboptions |
Suboptions include:
| %ItemSize | a symbolic, numeric, or Scaled[$$] value |
| %Background | additional background to distinguish labels |
| %BaseStyle | extra options to control font, etc. |
| %LabelPosition | one of Above, Below, Left, Right, Center |
| %Spacings | size of offset from the labelled element |

## Annotations

* %VertexAnnotations can be set to an association between named properties \
and lists of values.
* The values should be in the same order and length as given by VertexList.
* These values are accessible via %VertexColorFunction and %VertexLabels.

## Highlights

* %GraphRegionHighlight takes a list of regions to highlight, see %GraphRegion.

## Legends

* %GraphLegend accepts these settings:
| None | no legend |
| Automatic | attach legends for cardinals, colors, highlights, etc |
| expr$ | use a custom legend given by expr$ |
"

(**************************************************************************************************)

$extendedGraphOptionsRules = {
  GraphPlottingFunction -> None,
  GraphRegionHighlight -> None,
  GraphLegend -> None,
  ArrowheadSize -> Automatic,
  ArrowheadStyle -> Automatic,
  ArrowheadShape -> Automatic,
  ArrowheadPosition -> Automatic,
  VertexColorFunction -> None,
  VertexAnnotations -> None,
  LayoutDimension -> Automatic,
  GraphMetric -> Automatic,
  GraphOrigin -> None,
  Cardinals -> Automatic,
  CardinalColors -> Automatic,
  ViewOptions -> Automatic,
  LabelCardinals -> False,
  CoordinateTransformFunction -> None
};

$extendedGraphOptionSymbols = Keys @ $extendedGraphOptionsRules;

$extendedGraphOptionSymbolPattern = Alternatives @@ $extendedGraphOptionSymbols;

$extendedGraphOptionRulePattern = Rule[$extendedGraphOptionSymbolPattern, _];

$notIntercepted = True;

Graph;
SyntaxInformation[Graph];
Options[Graph];

Unprotect[Graph];
Options[Graph] = Sort @ JoinOptions[Graph, $extendedGraphOptionsRules];
SyntaxInformation[Graph] = ReplaceOptions[SyntaxInformation[Graph], "OptionNames" -> Map[SymbolName, Keys[Options[Graph]]]];
g:Graph[___] /; MemberQ[Unevaluated @ g, $extendedGraphOptionRulePattern] && $notIntercepted :=
  Block[{$notIntercepted = False}, interceptedGraphConstructor[g]];
Protect[Graph];

SetHoldAllComplete[interceptedGraphConstructor];

interceptedGraphConstructor[Graph[Shortest[args__], options__Rule]] := Scope[
  annotations = TakeOptions[{options}, $extendedGraphOptionSymbols];
  newOptions = Map[optionFixup] @ DeleteOptions[{options}, $extendedGraphOptionSymbols];
  result = Graph[args, Sequence @@ newOptions];
  If[!GraphQ[result], result = makeNewGraph[args, newOptions]];
  If[!GraphQ[result], ReturnFailed[]];
  Annotate[result, checkGraphAnnotations @ DeleteDuplicatesBy[annotations, First]]
];

makeNewGraph[graph_Graph ? GraphQ, newOptions_List] :=
  Graph[VertexList @ graph, EdgeList @ graph, Sequence @@ newOptions, Sequence @@ Options @ graph];

makeNewGraph[___] := $Failed;

(* these compensate for a weird extra level of list that Graph adds *)
optionFixup = MatchValues[
  Rule[VertexSize, r:{__Rule}] := Rule[VertexSize, Association @ r];
  Rule[sym:(EdgeStyle|VertexStyle), val_] := Rule[sym, toDirective[val]];
  Rule[sym:(GraphHighlightStyle|VertexLabelStyle|EdgeLabelStyle), elem_] := Rule[sym, toDirective[elem]];
  other_ := other;
];

interceptedGraphConstructor[e_] := e;

(**************************************************************************************************)

$arrowheadSizePattern = Alternatives[
  _ ? NumericQ, Scaled[_ ? NumericQ],
  sym_Symbol /; KeyExistsQ[$ImageWidthTable, sym],
  _Association,
  Automatic | None
];

$vertexAnnotationsPattern = Alternatives[
  Association[RepeatedNull[_String -> _List]],
  None
];

$layoutDimensionPattern = Alternatives[
  Automatic, None, 2, 3
];

$graphMetricPattern = Alternatives[
  Automatic, "Euclidean", _QuadraticFormObject, _List, _Integer, _ ? System`Private`MightEvaluateWhenAppliedQ
];

$viewOptionKeysPattern = Alternatives[
  ViewPoint, ViewCenter, ViewVertical, ViewVector, ViewMatrix, ViewProjection, ViewAngle
];

$viewOptionsRulePattern = Automatic | {RepeatedNull[$viewOptionKeysPattern -> _]};

$extendedGraphOptionPatterns = <|
  ArrowheadSize -> $arrowheadSizePattern,
  VertexAnnotations -> $vertexAnnotationsPattern,
  LayoutDimension -> $layoutDimensionPattern,
  GraphMetric -> $graphMetricPattern,
  ViewOptions -> $viewOptionsRulePattern
|>;

checkGraphAnnotations[rules_List] := Map[checkGraphAnnotationRule, rules];

General::badextopt = "The extended option `` -> `` is invalid and will be ignored."

checkGraphAnnotationRule[key_ -> value_] /; And[
  KeyExistsQ[$extendedGraphOptionPatterns, key],
  !MatchQ[value, $extendedGraphOptionPatterns @ key]] := (
    Message[Graph::badextopt, key, value];
    Nothing
  );

checkGraphAnnotationRule[rule_] := rule;

(**************************************************************************************************)

PackageExport["AttachGraphOptions"]

AttachGraphOptions[graph_Graph ? GraphQ, opts___] := Scope[
  result = Graph[graph, opts];
  If[GraphQ[result], result, makeNewGraph[graph, {opts}]]
];

(**************************************************************************************************)

PackageExport["ExtendedGraphQ"]

ExtendedGraphQ[g_Graph ? GraphQ] :=
  Count[AnnotationValue[g, $extendedGraphOptionSymbols], $Failed] =!= Length[$extendedGraphOptionSymbols];

ExtendedGraphQ[_] := False;

(**************************************************************************************************)

PackageScope["LookupExtendedGraphAnnotations"]

LookupExtendedGraphAnnotations[graph_, keys_List] :=
  MapThread[
    If[#1 === $Failed, #2, #1]&,
    {AnnotationValue[graph, keys], Lookup[$extendedGraphOptionsRules, keys]}
  ];

LookupExtendedGraphAnnotations[graph_, key_Symbol] :=
  LookupAnnotation[graph, key, Lookup[$extendedGraphOptionsRules, key]];

(**************************************************************************************************)

PackageScope["ExtendedGraphAnnotations"]

ExtendedGraphAnnotations[graph_] :=
  Normal @ DeleteCases[$Failed] @ AssociationThread[
    $extendedGraphOptionSymbols,
    AnnotationValue[graph, $extendedGraphOptionSymbols]
  ];

(**************************************************************************************************)

PackageScope["$simpleGraphOptions"]
PackageScope["$simpleGraphOptionRules"]

$simpleGraphOptionRules = JoinOptions[{
  EdgeLabels -> None, GraphLayout -> Automatic, ImagePadding -> All,
  ImageSize -> Automatic, VertexCoordinates -> Automatic,
  VertexLabels -> None, VertexSize -> Automatic,
  VertexStyle -> Automatic, EdgeStyle -> Automatic,
  VertexShapeFunction -> Automatic, EdgeShapeFunction -> Automatic, PlotLabel -> None,
  GraphHighlightStyle -> Automatic, VertexLabelStyle -> Automatic,
  Epilog -> {}
  },
  Rest @ $extendedGraphOptionsRules
]

$simpleGraphOptions = Keys @ $simpleGraphOptionRules;

(**************************************************************************************************)

PackageExport["ExtendedGraph"]

SetUsage @ "
ExtendedGraph[args$$] acts like Graph but accepts additional options and overrides how graphs are \
displayed.
* The following options and additional options are supported:
<*$extendedGraphUsage*>
"

Options[ExtendedGraph] = $simpleGraphOptionRules;
ExtendedGraph[args___] :=
  interceptedGraphConstructor[Graph[args, GraphPlottingFunction -> ExtendedGraphPlottingFunction]];

(**************************************************************************************************)

PackageExport["GraphCache"]

SetUsage @ "
GraphCache[sym$] represents a cache of computed properties of a graph that stores cached properties \
in sym$.
";

SetHoldAllComplete[GraphCache];


PackageScope["declareGraphCacheFriendly"]

declareGraphCacheFriendly[sym_] := (
  System`Private`SetValid[sym];
  System`Private`SetNoEntry[sym];
);
declareGraphCacheFriendly[syms__] := Scan[declareGraphCacheFriendly, {syms}];

MakeBoxes[GraphCache[_, sym_Symbol], StandardForm] :=
  RowBox[{"GraphCache", "[", "{", RowBox @ Flatten @ Riffle[ToBoxes /@ Keys @ sym, ","], "}", "]"}];

SetHoldRest[CreateGraphCache];
CreateGraphCache[graph_Graph, symbol_Symbol] := (
  symbol = Data`UnorderedAssociation[];
  GraphCache[graph, symbol]
);

GraphCache /: Print[GraphCache[graph_, sym_]] := Print[Keys @ sym];

(* for ordinary functions, evaluate them on the raw graph *)
GraphCache /: f_Symbol[GraphCache[graph_, sym_], args___] /; System`Private`HasDownEvaluationsQ[f] && System`Private`NotValidQ[f] :=
  f[graph, args];

(* for cache-friendly functions, which have the entryq flag set if they are not in the process of evaluating,
first check the cache, and if not present, mark them as being evaluated, compute the result by passing in the GraphCache,
then cache the result *)
GraphCache /: f_Symbol[gc:GraphCache[_, sym_], args___] /; System`Private`ValidQ[f] && System`Private`NoEntryQ[f] :=
  Lookup[sym, Key @ {f, args}, evaluateWithoutRecursion[sym, f, gc, args]];

SetHoldFirst[evaluateWithoutRecursion];
evaluateWithoutRecursion[sym_, f_, gc_, args___] := Block[{res},
  System`Private`SetNoEntry[f, False];
  sym[{f, args}] = res = f[gc, args];
  System`Private`SetNoEntry[f];
  res
];

$graphCacheEnabled = True;

(**************************************************************************************************)

PackageExport["VertexEdgeList"]

SetUsage @ "
VertexEdgeList[graph$] returns {vertices$, edges$}
"

VertexEdgeList[graph_] := {
  VertexList[graph],
  EdgeList[graph]
}

(**************************************************************************************************)

PackageExport["ToIndexGraph"]

ToIndexGraph[graph_ ? IndexGraphQ] := graph;
ToIndexGraph[graph_] := IndexGraph @ graph;

(**************************************************************************************************)

PackageExport["CombineMultiedges"]

SetUsage @ "
CombineMultiedges[graph$] combines edges that share the same endpoints into
single edges, combining any cardinals they have.
"

CombineMultiedges[graph_] := Scope[
  If[EdgeCount[graph] === 0, Return @ graph];
  {vertices, edges} = VertexEdgeList[graph];
  {edges, tags} = Transpose @ Map[separateTag, edges];
  edgeGroups = PositionIndex[edges];
  If[Length[edgeGroups] === Length[edges], Return @ graph];
  edges = KeyValueMap[
    {edge, indices} |-> reattachTag[edge, DeleteNone @ Part[tags, indices]],
    edgeGroups
  ];
  opts = Options[graph];
  Graph[vertices, edges, opts]
];

separateTag = MatchValues[
  DirectedEdge[a_, b_, t_] /; Order[a, b] == -1 := {DirectedEdge[b, a], Negated @ t};
  DirectedEdge[a_, b_, t_] := {DirectedEdge[a, b], t};
  UndirectedEdge[a_, b_, t_] := {Sort @ UndirectedEdge[a, b], t};
  edge_ := {Sort @ edge, None}
];

reattachTag[edge_, {}] := edge;
reattachTag[edge_, {tag_}] := Append[edge, tag];
reattachTag[edge_, tags_List] := Append[edge, CardinalSet @ tags];

(**************************************************************************************************)

PackageExport["CardinalSet"]

SetUsage @ "
CardinalSet[cardinals$] represents a set of cardinals that is simultaneously present on an edge.
"

MakeBoxes[CardinalSet[set_List], TraditionalForm] :=
  RowBox @ Riffle[MakeBoxes[#, TraditionalForm]& /@ set, " "];

PackageScope["SpliceCardinalSets"]

SpliceCardinalSets[e_] := ReplaceAll[ReplaceAll[e, CardinalSet -> Splice], Negated[z_] :> z];

(**************************************************************************************************)

PackageExport["VertexRange"]

SetUsage @ "
VertexRange[graph$] returns {1, 2, $$, n$} where n$ is the number of vertices in graph.
"

VertexRange[graph_] := Range @ VertexCount @ graph;

(**************************************************************************************************)

PackageExport["AdjacentPairs"]

SetUsage @ "
AdjacentPairs[graph$] gives the list of {{u$1, v$1}, {u$2, v$2}, $$}} such that \
vertex with index u$i is adjacent to vertex with index v$i.
* Note that AdjacentPairs is not given in the same order as EdgeList[graph$], and \
in general might have fewer values when there are multiple edges between the same \
pair of vertices.
* The relation is undirected, so that a$ \[DirectedEdge] b$ generates both {a$, b$} and {b$, a$}.
* Use AdjacentPairs[graph, 'Directed'] to obtain the directed form.
"

AdjacentPairs[graph_] := AdjacencyMatrix[graph]["NonzeroPositions"];

AdjacentPairs[graph_ ? DirectedGraphQ] := Scope[
  adj = AdjacencyMatrix[graph];
  (adj + Transpose[adj])["NonzeroPositions"]
];

AdjacentPairs[graph_, "Undirected"] := AdjacentPairs[graph];
AdjacentPairs[graph_, "Directed"] := AdjacencyMatrix[graph]["NonzeroPositions"];

(**************************************************************************************************)

PackageExport["EdgePairs"]

SetUsage @ "
EdgePairs[graph$] gives the list of {{u$1, v$1}, {u$2, v$2}, $$}} such that \
these is an vertex with index u$i is connected to vertex with index v$i.
* EdgePairs[graph$] has the same length and order as EdgeList[graph$].
* If the correspondence with EdgeList does not matter, consider using AdjacentPairs,
which is faster.
"

(* todo: find a better way of obtaining these than via indexgraph! it seems like
vertex renaming might be expensive, and there is all the option processing that goes along with it.
unfortunately i can't find a way of extracting the list of edges in indexed form directly. *)
EdgePairs[graph_ ? EdgeTaggedGraphQ] := {#1, #2}& @@@ EdgeList @ ToIndexGraph @ graph;
EdgePairs[graph_] := List @@@ EdgeList @ ToIndexGraph @ graph;

(**************************************************************************************************)

PackageExport["VertexOutTable"]
PackageExport["VertexInTable"]

SetUsage @ "
VertexOutTable[graph$] returns a list of lists {out$1, out$2, $$} where out$i is a list of the \
indices of the vertices that are have a connection from vertex v$i.
"

SetUsage @ "
VertexInTable[graph$] returns a list of lists {in$1, in$2, $$} where in$i consists of the \
indices of the vertices that are have a connection to vertex v$i.
"

VertexOutTable[graph_] := AdjacencyMatrix[graph]["AdjacencyLists"];
VertexInTable[graph_] := Transpose[AdjacencyMatrix[graph]]["AdjacencyLists"];

(**************************************************************************************************)

PackageExport["VertexInOutTable"]

SetUsage @ "
VertexInOutTable[graph$] returns a list of pairs of lists {{in$1, out$1}, {in$2, out$2}, $$} where in$i \
is the list of indices of vertices that are have an edge to vertex v$i, and out$i is the \
list of indices of vertices that have a edge from vertex v$i.
"

VertexInOutTable[graph_] := Scope[
  adj = AdjacencyMatrix[graph];
  Transpose[{adj["AdjacencyLists"], Transpose[adj]["AdjacencyLists"]}]
];

(**************************************************************************************************)

PackageExport["VertexAdjacencyTable"]

SetUsage @ "
VertexAdjacencyTable[graph$] returns a list of lists {adj$1, adj$2, $$} where adj$i \
is the list of indices of vertices that are have a connection to vertex v$i.
"

VertexAdjacencyTable[graph_] := Scope[
  adj = AdjacencyMatrix[graph];
  MapThread[Union, {adj["AdjacencyLists"], Transpose[adj]["AdjacencyLists"]}]
];

(**************************************************************************************************)

PackageExport["VertexOutEdgeTable"]
PackageExport["VertexInEdgeTable"]

SetUsage @ "
VertexOutEdgeTable[graph$] returns a list of lists {out$1, out$2, $$} where out$i is a list of the \
indices of edges whose origin is the vertex v$i.
"

SetUsage @ "
VertexInEdgeTable[graph$] returns a list of lists {in$1, in$2, $$} where in$i is a list of the \
indices of edges whose destination is the vertex v$i.
"

VertexOutEdgeTable[graph_] :=
  Lookup[PositionIndex @ FirstColumn @ EdgePairs @ graph, VertexRange @ graph, {}];

VertexInEdgeTable[graph_] :=
  Lookup[PositionIndex @ LastColumn @ EdgePairs @ graph, VertexRange @ graph, {}];

(**************************************************************************************************)

PackageExport["VertexInOutEdgeTable"]

SetUsage @ "
VertexInOutEdgeTable[graph$] returns a list of lists {{in$1, out$1}, {in$2, out$2}, $$}  where in$i \
is a list of the indices of edges whose destination is the vertex v$i, and out$i is a list of the \
indices of edges whose origin is the vertex v$i.
"

VertexInOutEdgeTable[graph_] := Scope[
  pairs = EdgePairs @ graph;
  vertices = VertexRange @ graph;
  Transpose[{
    Lookup[PositionIndex @ FirstColumn @ pairs, vertices, {}],
    Lookup[PositionIndex @ LastColumn @ pairs, vertices, {}]
  }]
];

(**************************************************************************************************)

PackageExport["VertexAdjacentEdgeTable"]

SetUsage @ "
VertexAdjacentEdgeTable[graph$] returns a list of lists {adj$1, adj$2, $$}  where adj$i \
is a list of the indices of edges which begin or end at vertex v$i.
"

VertexAdjacentEdgeTable[graph_] := Scope[
  pairs = EdgePairs @ graph;
  vertices = VertexRange @ graph;
  MapThread[Union, {
    Lookup[PositionIndex @ FirstColumn @ EdgePairs @ graph, vertices, {}],
    Lookup[PositionIndex @ LastColumn @ EdgePairs @ graph, vertices, {}]
  }]
];

(**************************************************************************************************)

PackageExport["TagIndices"]

TagIndices[graph_] := PositionIndex @ EdgeTags @ graph;

(**************************************************************************************************)

PackageExport["TagVertexAdjacentEdgeTable"]

TagVertexAdjacentEdgeTable[graph_] := Scope[
  outTable = VertexOutEdgeTable @ graph;
  inTable = VertexInEdgeTable @ graph;
  Association @ KeyValueMap[
    {key, edgeIndices} |-> {
      key ->          Map[First[Intersection[#, edgeIndices], None]&, outTable],
      Negated[key] -> Map[First[Intersection[#, edgeIndices], None]&, inTable]
    },
    TagIndices @ graph
  ]
];

(**************************************************************************************************)

PackageExport["VertexIndexAssociation"]

VertexIndexAssociation[graph_] := AssociationRange @ VertexList @ graph;

(**************************************************************************************************)

PackageExport["EdgeIndexAssociation"]

EdgeIndexAssociation[graph_] := AssociationRange @ EdgeList @ graph;

(**************************************************************************************************)

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

(**************************************************************************************************)

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

(**************************************************************************************************)

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

(**************************************************************************************************)

PackageExport["InVertices"]
PackageExport["OutVertices"]
PackageExport["AllVertices"]

InVertices[edges_] := edges[[All, 1]];
OutVertices[edges_] := edges[[All, 2]];
AllVertices[edges_] := Join[InVertices @ edges, OutVertices @ edges];

(**************************************************************************************************)

PackageExport["GraphCorners"]

GraphCorners[graph_] := Scope[
  degree = DegreeCentrality[graph];
  vertices = Pick[VertexList[graph], degree, Min[degree]];
  SortBy[vertices, LatticeVertexAngle]
];

(**************************************************************************************************)

PackageExport["GraphVertexCoordinates"]

GraphVertexCoordinates[graph_Graph] :=
  GraphEmbedding[graph];

(**************************************************************************************************)

PackageScope["integersToVertices"]

integersToVertices[graph_Graph, expr_] :=
  integersToVertices[VertexList[graph], expr];

integersToVertices[vertices_List, expr_] :=
  expr /. {i:{__Integer} :> Part[vertices, i], i_Integer :> Part[vertices, i]};

(**************************************************************************************************)

PackageExport["ToGraph"]

SetUsage @ "
ToGraph[obj$] attempts to convert obj$ to a Graph[$$] object.
* If obj$ is already a Graph, it is returned unchanged.
* If obj$ is a list of rules, it is converted to a Graph object.
* Otherwise, $Failed is returned.
"

$edgeP = _DirectedEdge | _UndirectedEdge | _Rule | _TwoWayRule;

ToGraph = MatchValues[
  g_Graph := g;
  list:{Repeated[$edgeP]} := Graph[list];
  _ := $Failed
];

(**************************************************************************************************)

PackageExport["AttachVertexAnnotations"]

AttachVertexAnnotations[graph_, annotations_] := Scope[
  CheckIsGraph[1];
  joinAnnotation[graph, VertexAnnotations, annotations]
];

(**************************************************************************************************)

joinAnnotation[graph_, key_, newAnnotations_] := Scope[
  oldAnnotations = LookupAnnotation[graph, key, None];
  SetNone[oldAnnotations, <||>];
  Annotate[graph, key -> Join[oldAnnotations, newAnnotations]]
];

(**************************************************************************************************)

PackageExport["ExtendedSubgraph"]

ExtendedSubgraph[oldGraph_, newVertices_, newEdges_] := Scope[
  options = Options[oldGraph];
  annotations = ExtendedGraphAnnotations[oldGraph];
  vertexCoords = Lookup[options, VertexCoordinates, Automatic];
  oldVertices = VertexList[oldGraph];
  newVertexIndices = Map[IndexOf[oldVertices, #]&, newVertices];
  newVertexOrdering = Ordering[newVertexIndices];
  newVertices = Part[newVertices, newVertexOrdering];
  vertexAnnotations = LookupAnnotation[oldGraph, VertexAnnotations, None];
  sortedNewVertexIndices = Sort @ newVertexIndices;
  If[ListQ[vertexCoords],
    vertexCoords = Part[vertexCoords, sortedNewVertexIndices];
    options = ReplaceOptions[options, VertexCoordinates -> vertexCoords];
  ];
  If[AssociationQ[vertexAnnotations],
    vertexAnnotations //= Map[Part[#, sortedNewVertexIndices]&];
    annotations = ReplaceOptions[annotations, VertexAnnotations -> vertexAnnotations];
  ];
  If[newEdges === Automatic,
    newEdges = Select[EdgeList @ oldGraph, MemberQ[newVertices, Part[#, 1]] && MemberQ[newVertices, Part[#, 2]]&]
  ];
  graph = Graph[newVertices, newEdges, options];
  Annotate[graph, annotations]
];

(**************************************************************************************************)

PackageExport["IndexGraphQ"]

IndexGraphQ[g_Graph ? GraphQ] :=
  RangeQ @ VertexList @ g;

IndexGraphQ[_] := False;

(**************************************************************************************************)

PackageExport["CanonicalizeEdges"]

CanonicalizeEdges[edges_] := Map[sortUE, edges];
sortUE[UndirectedEdge[a_, b_, tag___]] /; Order[a, b] === 1 := UndirectedEdge[b, a, tag];
sortUE[other_] := other;

(**************************************************************************************************)

PackageExport["ExtractGraphPrimitiveCoordinates"]

SetUsage @ "
ExtractGraphPrimitiveCoordinates[graph$] returns the pair {vcoords$, ecoords$}, where \
vcoords$ is a list of coordinate tuples in the same order as VertexList[graph$], and \
ecoords$ is a list of coordinate matrices in the same order as EdgeList[graph$].
"

ExtractGraphPrimitiveCoordinates[graph_] := Scope[

  If[!GraphQ[graph], ReturnFailed[]];
  igraph = ToIndexGraph[graph];
  If[!GraphQ[igraph], ReturnFailed[]];

  {graphLayout, vertexCoordinates} =
    LookupOption[igraph, {GraphLayout, VertexCoordinates}];

  {layoutDimension, viewOptions, coordinateTransformFunction} =
    LookupExtendedGraphAnnotations[graph, {LayoutDimension, ViewOptions, CoordinateTransformFunction}];

  actualDim = If[ContainsQ[graphLayout, "Dimension" -> 3] || CoordinateMatrixQ[vertexCoordinates, 3], 3, 2];
  SetAutomatic[layoutDimension, actualDim];

  SetAutomatic[graphLayout, {}];

  vertexCoordinates = ConstantArray[0., {VertexCount @ igraph, actualDim}];

  edgeList = EdgeList @ igraph;
  edgeCoordinateLists = ConstantArray[{}, Length @ edgeList];
  If[UndirectedGraphQ[igraph] || MixedGraphQ[igraph],
    edgeList //= CanonicalizeEdges];

  isMulti = MultigraphQ[igraph];
  If[isMulti,
    edgeIndices = PositionIndex[edgeList];
    edgeIndexOffset = ConstantAssociation[edgeList, 1];
    edgeCaptureFunction = storeMultiEdgeCoords;
  ,
    edgeIndices = AssociationRange[edgeList];
    edgeCaptureFunction = storeEdgeCoords;
  ];

  If[isMulti || !DuplicateFreeQ[edgeList],
    graphLayout = Developer`ToList[graphLayout, "MultiEdgeDistance" -> 0.3];
  ];

  newGraph = If[actualDim == 3, Graph3D, Graph][
    VertexList @ igraph, EdgeList @ igraph,
    VertexShapeFunction -> captureVertexCoordinates,
    EdgeShapeFunction -> ({coords, edge} |-> edgeCaptureFunction[coords, sortUE @ edge]),
    GraphLayout -> graphLayout, VertexCoordinates -> LookupOption[igraph, VertexCoordinates]
  ];

  GraphComputation`GraphDrawing @ newGraph;

  vertexCoordinates = ToPackedReal @ vertexCoordinates;

  If[UndirectedGraphQ[graph],
    vertexIndex = AssociationRange @ VertexList @ graph;
    edgeCoordinateLists = MapThread[orientEdgeCoords, {edgeCoordinateLists, edgeList}];
  ];

  applyCoordinateTransform[coordinateTransformFunction];

  If[CoordinateMatrixQ[vertexCoordinates, 3] && layoutDimension == 2,
    SetAutomatic[viewOptions, $automaticViewOptions];
    viewOptions = Association[PlotRange -> CoordinateBounds[vertexCoordinates], viewOptions];
    viewTransform = ConstructGraphicsViewTransform[viewOptions];
    vertexCoordinates //= viewTransform;
    edgeCoordinateLists //= Map[viewTransform];
  ];

  {ToPackedReal @ vertexCoordinates, ToPackedRealArrays @ edgeCoordinateLists}
];

orientEdgeCoords[coords_, _DirectedEdge] := coords;
orientEdgeCoords[coords_, ue:UndirectedEdge[a_, b_, rest___]] := If[
  Norm[First[coords] - Part[vertexCoordinates, vertexIndex @ a]] < 0.001,
  coords, Reverse @ coords
];

captureVertexCoordinates[coords_, vertex_, _] :=
  Part[vertexCoordinates, vertex] = coords;

storeEdgeCoords[coords_, edge_] :=
  Part[edgeCoordinateLists, edgeIndices @ edge] = coords;

storeMultiEdgeCoords[coords_, edge_] :=
  Part[edgeCoordinateLists, Part[edgeIndices @ edge, edgeIndexOffset[edge]++]] = coords;

(**************************************************************************************************)

ExtendedGraphPlot::badwrappedshape = "CoordinateTransformFunction -> ProjectionOnto[...] contains an invalid shape.";
ExtendedGraphPlot::badedgeshape = "CoordinateTransformFunction -> `` is not a valid specification.";

applyCoordinateTransform[Automatic|None] :=
  Null

applyCoordinateTransform[spec_] :=
  Message[ExtendedGraphPlot::badedgeshape, spec];

applyCoordinateTransform[ProjectionOnto[shape_]] := Block[{$rnf},
  $rnf = BoundaryProjection @ shape;
  If[FailureQ[$rnf], Message[ExtendedGraphPlot::badwrappedshape]; Return @ $Failed];
  vertexCoordinates //= $rnf;
  edgeCoordinateLists //= Map[projectLineOntoRNF];
];

projectLineOntoRNF = MatchValues[
  {a_, b_} ? CoordinateMatrixQ /; (Head[$rnf] === RegionNearestFunction) :=
    $rnf @ Range[a, b, Into @ 6];
  points_List ? CoordinateMatrixQ :=
    $rnf @ points;
  points_List := Print[points]; (* projectLineOntoRNF /@ points; *)
];


(**************************************************************************************************)

PackageExport["ToSymmetricGraph"]

ToSymmetricGraph[graph_ ? DirectedGraphQ] :=
  Graph[VertexList @ graph, EdgeList[graph] /. DirectedEdge -> UndirectedEdge];

ToSymmetricGraph[graph_] := graph;

(**************************************************************************************************)

PackageScope["GraphScope"]

PackageScope["NotInGraphScopeOfQ"]

NotInGraphScopeOfQ[graph_] := !GraphQ[$Graph] || (graph =!= $Graph)

PackageScope["$Graph"]
PackageScope["$VertexList"]
PackageScope["$EdgeList"]
PackageScope["$EdgeTags"]
PackageScope["$VertexIndex"]
PackageScope["$VertexCount"]
PackageScope["$EdgeCount"]

PackageScope["$IndexGraph"]
PackageScope["$IndexGraphEdgeList"]

PackageScope["$MetricGraphCache"]
PackageScope["$GraphMetric"]

SetAttributes[GraphScope, HoldRest];

SetUsage @ "
GraphScope[graph$, body$] sets up some dynamically scoped variables that make it easy to
access properties and computed results from a single graph.
The following variables are blocked during the execution of GraphScope:
| $Graph | graph$ |
| $VertexList | VertexList[graph$] |
| $EdgeList | EdgeList[graph$] |
| $EdgeTags | EdgeTags[graph$] |
| $VertexIndex | VertexIndexAssociation[graph$] |
| $VertexCount | VertexCount[$graph] |
| $EdgeCount | EdgeCount[$graph] |
| $IndexGraph | IndexGraph[graph$] |
| $IndexGraphEdgeList | EdgeList[$IndexGraph] |
| $MetricGraphCache | GraphCache[$$] object for a symmetric version of the index graph |
| $GraphMetric | the current graph metric |
* All of the expensive properties are computed (and then cached) on first use.
"

GraphScope[graph_, body_] := Block[
  {
    $Graph = graph,
    $VertexList := $VertexList = VertexList @ $Graph,
    $EdgeList := $EdgeList = EdgeList @ $Graph,
    $EdgeTags := $EdgeTags = Replace[EdgeTags @ $Graph, {} -> None],
    $VertexIndex := $VertexIndex = VertexIndexAssociation @ $Graph,
    $VertexCount = VertexCount @ $Graph,
    $EdgeCount = EdgeCount @ $Graph,

    $IndexGraph := $IndexGraph = ToIndexGraph @ $Graph,
    $IndexGraphEdgeList := $IndexGraphEdgeList = EdgeList @ $IndexGraph,

    $metricGraphCacheSymbol = Null,
    $MetricGraphCache := $MetricGraphCache = createMetricGraphCache[],
    $GraphMetric = Inherited
  },
  body
];

createMetricGraphCache[] := CreateGraphCache[
  Annotate[ToSymmetricGraph @ $IndexGraph, GraphMetric -> LookupAnnotation[$Graph, GraphMetric, Automatic]],
  $metricGraphCacheSymbol
];