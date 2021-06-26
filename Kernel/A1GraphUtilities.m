Package["GraphTools`"]


PackageImport["GeneralUtilities`"]


PackageExport["VertexAnnotations"]
PackageExport["LayoutDimension"]
PackageExport["GraphMetric"]
PackageExport["GraphOrigin"]
PackageExport["Cardinals"]
PackageExport["ViewOptions"]
PackageExport["AdditionalImagePadding"]
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
| %EdgeSetback | Automatic | how far to set back edges from vertices |
| %EdgeThickness | Automatic | thickness of edges |
| %LabelCardinals | False | whether to attach labels to arrowheads |
| %VertexShapeFunction | Automatic | how to draw vertices |
| %EdgeShapeFunction | Automatic | how to draw edges |
| %VertexColorFunction | None | function to obtain colors for vertices |
| %EdgeColorFunction | None | function to obtain colors for edges |
| %ColorRules | None | color vertices and edges by region |
| %VertexAnnotations | None | association of additional per-vertex data |
| %GraphMetric | Automatic | metric to calculate graph distances |
| %CardinalColors | Automatic | association of cardinal colors |
| %VisibleCardinals | All | which cardinals to draw |
| %ViewOptions | Automatic | how to project 3D coordinates |
| %AdditionalImagePadding | None | additional padding to include unconditionally |
| %ViewRegion | All | region of graph to plot |
| %CoordinateTransformFunction | None | function to remap coordinates before plotting |
| %Frame | False | whether to draw a frame |
| %FrameStyle | Automatic | color of frame |

## Arrowheads

* Any of the specifications below can also be given in the form <|card$1 -> spec$1, $$|>.

* %ArrowheadShape accepts these settings:
| 'Arrow' | solid kinded arrowhead (default) |
| 'Line' | partial triangle formed by two lines |
| 'DoubleLine' | two closely spaced partial triangles |
| 'Triangle' | triangle formed by three lines |
| 'HalfTriangle' | half triangle above the edge |
| 'Disk' | circular disk |
| 'Square' | square |
| 'Diamond' | diamond (rotated square) |
| 'Cone' | thin cone (3D) |
| 'Sphere' | sphere (3D) |
| 'Cardinal' | no arrowhead, use cardinal label |
| 'CrossLine' | a horizontal line |
| 'CrossBar' | a thick horizontal line |
| 'Tube' | a tube |
| None | no arrowheads |

* In addition, %ArrowheadShape supports suboptions via {'shape$', subopts$$}:
| %NegationStyle | 'Flip' | how to plot negated cardinals in %CardinalSet[$$] |
| %TwoWayStyle | 'In' | how to plot negated pairs in %CardinalSet[$$] |
| %PairedDistance | 0 | how far away to plot negated pairs |
| %EdgeThickness | 1 | thickness of line-based arrowheads |

* %TwoWayStyle -> spec$ determines how to plot a cardinal and its negation together:
| 'Out' | arrowheads facing away from each other |
| 'OutClose' | facing out with backs touching |
| 'In' | arrowheads facing towards each other |
| 'InClose' | facing in with tips touching |
| 'spec$' | one of the regular shapes |

* %NegationStyle -> spec$ determines how negated cardinals are drawn:
| 'OverBar' | draw a negation bar above arrowhead |
| 'UnderBar' | drwa a negation bar below arrowhead |

* %PairedDistance -> size$ determines the separation of paired cardinals, in points.

* %ArrowheadSize accepts these settings:
| Automatic | use a safe arrowhead size, depending on layout |
| size$ | size$ in points in the final plot |
| Small, Medium, $$ | symbolic size, with Medium being equivalent to 20 |
| %AbsolutePointSize[size$] | equivalent to size$ |
| %PointSize[f$] | a fraction f$ of the width of the final plot |
| %Scaled[r$] | scale the default safe size by r$ |
| %Max[$$], %Min[$$] | max or min of several specifications |

* %ArrowheadStyle can be set to a color or list of directives.

* %CardinalColors -> <|card$1 -> col$1, $$|> determines the colors for arrowheads.

* %ArrowheadPosition -> r$ sets the position of the arrowhead to the fraction r$ along the \
length of the edge.

* %LabelCardinals -> True will add a label to each arrowhead indicating its cardinal.

## Edges

* %EdgeShapeFunction controls how edges are drawn, indepedently of arrowheads, and accepts:
| Automatic | use %Line or %Arrow as appropriate |
| None | do not draw edges |
| f$ | call f$ to obtain graphical primitives |

If a function f$ is given, it is provided with an association containing the following keys:
| 'Coordinates' | the list of {x$, y$} or {x$, y$, z$} coordinates |
| 'Source' | the source vertex |
| 'Target' | the target vertex |
| 'EdgeIndex' | the index of the edge |
| 'Counter' | an integer counter incremented on access |
| 'Shape' | the symbol %Line or %Arrow |
| 'Cardinal' | the cardinal(s) on the edge |
| 'Arrowheads' | the %Arrowheads[$$] expression (or None) |
| 'LabelStyle' | setting of %EdgeLabelStyle |

* The result can contain the expression %UniqueLabel[$$] inside a %Text[$$] primitive.
This will be renumbered so that all labels are ordered according to x$, y$ screen position.

* %EdgeColorFunction accepts these settings:
| None | color via %EdgeStyle (default) |
| 'Cardinal' | color by cardinal present on edge |
| {e$1, e$2, $$} | use values e$i in same order as %VertexList |
| <|e$1 -> val$1, $$, All -> val$|> | assign values to specific edges |
| {region$1 -> val$1, $$, All -> val$} | assign values to edges within specific regions |
| %Paletted[spec$, colors$] | use a given named or explicit color palette |
* If a spec produces non-color values, edges will colored based on the type of data.
* If %GraphLegend -> Automatic, a color legend will be shown.

* %EdgeSetback controls how far an edge should be set back from its endpoints.

* %EdgeThickness controls the thickness of rendered edges, and is given in points.

## Vertices

* %VertexShapeFunction controls how vertices are drawn and accepts these settings:
| Automatic | choose a method automatically |
| 'Point' | use %Point[$$] |
| 'Disk' | use %Disk[$$] |
| 'Sphere' | use %Sphere[$$] |
| 'Ball' | use 'Disk' for 2D and 'Sphere' for 3D |
| 'Square' | square (designed for 'Square' lattice) |
| 'Hexagon' | hexagon (designed for 'Triangular' lattice) |
| None | do not draw vertices |

* %VertexSize accepts these settings:
| Automatic | use a safe default size, depending on layout |
| size$ | size$ in points in the final plot |
| Small, Medium, $$ | symbolic size, with Medium being equivalent to 5 |
| %AbsolutePointSize[size$] | equivalent to size$ |
| %PointSize[f$] | a fraction f$ of the width of the final plot |
| %Scaled[r$] | fraction r$ of the quantiles of inter-vertex distance |
| {v$1 -> s$1, $$, %%All -> s$} | use specific sizes for specific vertices |
| %Max[$$],% Min[$$] | max or min of several specifications |

* %VertexColorFunction accepts these settings:
| None | color via %VertexStyle (default) |
| 'key$' | color with values from setting of %VertexAnnotations |
| {val$1, val$2, $$} | use values val$i in same order as %VertexList |
| <|v$1 -> val$1, $$, All -> val$|> | assign values to specific vertices |
| {region$1 -> val$1, $$, All -> val$} | assign values to vertices within specific regions |
| f$ | apply f$ to vertices to obtain values |
| spec$ -> f$ | apply f$ to result of spec$ |
| %Paletted[spec$, colors$] | use a given named or explicit color palette |
* If a spec produces non-color values, vertices will colored based on the type of data.
* If %GraphLegend -> Automatic, a color legend will be shown.

## Metrics

* %GraphMetric affects the behavior of %MetricDistance, %MetricDistanceMatrix, and %MetricFindShortestPath.
* The following settings are accepted:
| Automatic | the default graph distance |
| 'Euclidean' | root total square of per-cardinal distances |
| 'Chessboard' | maximum of the per-cardinal distances |
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

* %VertexLabelStyle and %EdgeLabelStyle accept these settings:
| Automatic | default |
| styles$ | a list, directive, or individual style |
| {styles$, opt$ -> val$, $$} | provide suboptions |
Supported suboptions are:
| %ItemSize | a symbolic, numeric, or %Scaled[$$] value |
| %Background | additional background to distinguish labels |
| %BaseStyle | extra options to control font, etc. |
| %LabelPosition | one of Above, Below, Left, Right, Center, or Automatic |
| %Spacings | size of offset from the labelled element |
* %VertexLabelStyle -> {%LabelPosition -> Automatic} will maximize the distance to adjacent edges.

## Annotations

* %VertexAnnotations can be set to an association between named properties \
and lists of values.
* The values should be in the same order and length as given by %VertexList.
* These values are accessible via %VertexColorFunction and %VertexLabels.

## Highlights and colors

* %GraphRegionHighlight takes a list of regions to highlight, see %GraphRegion.

* %ColorRules can be a list of rules of the following forms:
| region$ -> color$ | set color of vertices and edges within region, see %GraphRegion |
| vertex$ -> color$ | set color of a specific vertex |
| edge$ -> color$ | set color of a specific edge |
| {spec$1, $$} -> color$ | set color of several elements at once |

## Legends

* %GraphLegend accepts these settings:
| None | no legend |
| Automatic | attach legends for cardinals, colors, highlights, etc |
| expr$ | use a custom legend given by expr$ |

## Misc

* %CoordinateTransformFunction can be a function, which will be applied to each coordinates, or one of:
| {'Rotate', n$} | rotate by n$ degrees |
| 'Rotate90' | rotate 90\[Degree] |
| 'Rotate180' | rotate 180\[Degree] |
| 'Rotate270' | rotate 270\[Degree] |
| 'ReflectHorizontal' | reflect horizontally |
| 'ReflectVertical' | reflect vertically |
| 'BendVertical' | bend vertical edges for layered digraphs |
| {'Snap', n$} | snap vertices to n$ \[Times] n$ grid |
| 'PolarProjection' | spherical polar projection |

* %Padding, whether in %ImagePadding or %AdditionImagePadding, can be specified in these forms:
| None | no padding |
| n$ | pad by n$ on all sides |
| {h$, v$} | pad by h$ horizontally and v$ vertically |
| {{l$, r$}, {b$, t$}} | explicit padding |
| {Left -> l$, $$} | per-side padding |

* %Frame -> True will draw a frame that encompasses the vertices and edges, plus any additional
padding included by %ImagePadding. The effects of %AdditionalImagePadding will not be included.

* The special expression %GraphicsValue[$$] can be used in %Prolog, %Epilog, or by shape functions.
It will be replaced with computed values after plotting is complete. See %GraphicsValue for more information.
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
  EdgeSetback -> Automatic,
  VertexColorFunction -> None,
  EdgeColorFunction -> None,
  VertexAnnotations -> None,
  LayoutDimension -> Automatic,
  GraphMetric -> Automatic,
  GraphOrigin -> None,
  Cardinals -> Automatic,
  CardinalColors -> Automatic,
  VisibleCardinals -> All,
  ViewOptions -> Automatic,
  LabelCardinals -> False,
  CoordinateTransformFunction -> None,
  ColorRules -> None,
  ViewRegion -> All,
  AdditionalImagePadding -> None,
  EdgeThickness -> Automatic
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
optionFixup = Case[
  Rule[VertexSize, r:{__Rule}]                    := Rule[VertexSize, Association @ r];
  Rule[sym:(VertexLabels | EdgeLabels), l_List]   := Rule[sym, Hold[l]];
  Rule[sym:(EdgeStyle|VertexStyle), val_]         := Rule[sym, toDirective[val]];
  Rule[VertexShapeFunction, assoc_Association]    := Rule[VertexShapeFunction, toShape /@ assoc];
  Rule[sym:(GraphHighlightStyle|VertexLabelStyle|EdgeLabelStyle), elem_] := Rule[sym, toDirective[elem]];
  other_                                          := other;
];

(* TODO: compute sizes here so that graph layout knows about them *)
toShape[g_Graph] := ExtendedGraphPlot @ g;
toShape[other_] := other;

interceptedGraphConstructor[e_] := e;

(**************************************************************************************************)

$arrowheadSizePattern = Alternatives[
  _ ? NumericQ,
  $SymbolicSizePattern,
  Scaled[(_ ? NumericQ) | $SymbolicSizePattern],
  PointSize[_ ? NumericQ],
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
  EdgeLabels -> None, GraphLayout -> Automatic, ImagePadding -> None,
  ImageSize -> Automatic, VertexCoordinates -> Automatic,
  VertexLabels -> None, VertexSize -> Automatic,
  VertexStyle -> Automatic, EdgeStyle -> Automatic,
  VertexShapeFunction -> Automatic, EdgeShapeFunction -> Automatic, PlotLabel -> None,
  GraphHighlightStyle -> Automatic, VertexLabelStyle -> Automatic, EdgeLabelStyle -> Automatic,
  Epilog -> {}, Prolog -> {}, Frame -> None, FrameStyle -> Automatic, BaselinePosition -> Automatic,
  FrameLabel -> None
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

PackageExport["ExpandCardinalSetEdges"]

SetUsage @ "
ExpandCardinalSetEdges[graph$] expands any edges tagged with CardinalSet into multiple edges with \
one cardinal each.
* CombineMultiedges is the inverse of ExpandCardinalSetEdges.
"

ExpandCardinalSetEdges[graph_] := Scope[
  If[FreeQ[EdgeTags[graph], CardinalSet], Return @ graph];
  opts = Options[graph];
  Graph[
    VertexList @ graph,
    SpliceCardinalSetEdges @ EdgeList @ graph,
    opts
  ]
];

(**************************************************************************************************)

PackageExport["CombineMultiedges"]

SetUsage @ "
CombineMultiedges[graph$] combines edges that share the same endpoints into \
single edges, combining any cardinals they have.
* ExpandCardinalSetEdges is the inverse of CombineMultiedges.
"

CombineMultiedges[graph_Graph] := iCombineMultiedges[graph];

iCombineMultiedges[graph_] := Scope[
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

separateTag = Case[
  DirectedEdge[a_, b_, t_] /; Order[a, b] == -1 := {DirectedEdge[b, a], Negated @ t};
  DirectedEdge[a_, b_, t_]                      := {DirectedEdge[a, b], t};
  UndirectedEdge[a_, b_, t_]                    := {Sort @ UndirectedEdge[a, b], t};
  edge_                                         := {Sort @ edge, None}
];

reattachTag[edge_, {}] := edge;
reattachTag[edge_, {tag_}] := Append[edge, tag];
reattachTag[edge_, tags_List] := Append[edge, SimplifyCardinalSet @ CardinalSet @ tags];

(**************************************************************************************************)

PackageExport["CardinalSet"]

SetUsage @ "
CardinalSet[cardinals$] represents a set of cardinals that is simultaneously present on an edge.
"

MakeBoxes[CardinalSet[set_List], TraditionalForm] :=
  RowBox @ Riffle[MakeBoxes[#, TraditionalForm]& /@ set, " "];

PackageExport["SimplifyCardinalSet"]

SimplifyCardinalSet = Case[
  CardinalSet[{a_}]                               := % @ a;
  CardinalSet[{l___, CardinalSet[{m___}], r___}]  := % @ CardinalSet[{l, m, r}];
  other_                                          := other;
];

(**************************************************************************************************)

PackageScope["SpliceCardinalSets"]

SpliceCardinalSets[e_] := ReplaceAll[ReplaceAll[e, CardinalSet -> Splice], Negated[z_] :> z];

(**************************************************************************************************)

PackageScope["SpliceCardinalSetEdges"]

SpliceCardinalSetEdges[e_] := ReplaceAll[e, DirectedEdge[a_, b_, CardinalSet[s_]] :> Splice[DirectedEdge[a, b, #]& /@ s]];

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

SetUsage @ "
TagIndices[graph$] returns an association from cardinals to the indices of edges on which they are present.
"

TagIndices[graph_] := Scope[
  $tagAssoc = <||>;
  ScanIndexed[processTagEntry, EdgeTags @ graph];
  $tagAssoc
];

processTagEntry[tag_, {part_}] :=
  KeyAppendTo[$tagAssoc, tag, part];

processTagEntry[CardinalSet[tags_], {part_}] :=
  Scan[KeyAppendTo[$tagAssoc, StripNegated @ #1, part]&, tags];

(**************************************************************************************************)

PackageExport["TagVertexOutTable"]

SetUsage @ "
TagVertexOutTable[graph$] returns an association from each cardinal to its VertexOutTable.
* If a cardinal is not incident to a given vertex, the corresponding entry is None.
* Keys are included for negations of cardinals.
* As there is a maximum of edge for a given vertex and cardinal, table entries are single integers or None.
"

TagVertexOutTable[graph_] := Scope[
  cardinals = CardinalList @ graph;
  igraph = ToIndexGraph @ graph;
  cardinals = Join[cardinals, Negated /@ cardinals];
  outTables = ConstantAssociation[cardinals, ConstantArray[None, VertexCount @ igraph]];
  ({src, dst, tag} |-> (
      Part[outTables, Key @ tag, src] = dst;
      Part[outTables, Key @ Negated @ tag, dst] = src;
  )) @@@ SpliceCardinalSetEdges @ EdgeList[igraph];
  outTables
];

(**************************************************************************************************)

PackageExport["VertexTagTable"]

SetUsage @ "
VertexTagTable[graph$] returns a list of lists {tags$1, tags$2, $$} where tag$i is the list of tags \
present on vertex v$i.
"

VertexTagTable[graph_, splice_:True] := Scope[
  rules = {#1 -> #3, #2 -> Negated[#3]}& @@@ If[splice, SpliceCardinalSetEdges, Identity] @ EdgeList[graph];
  Lookup[Merge[Flatten @ rules, Identity], VertexList @ graph, {}]
]

(**************************************************************************************************)

PackageExport["VertexOutTagTable"]

SetUsage @ "
VertexOutTagTable[graph$] returns a list of lists {tags$1, tags$2, $$} where tag$i is the list of tags \
present on vertex v$i in the outgoing direction.
"

VertexOutTagTable[graph_, splice_:True] := Scope[
  rules = #1 -> #3& @@@ If[splice, SpliceCardinalSetEdges, Identity] @ EdgeList[graph];
  Lookup[Merge[rules, Identity], VertexList @ graph, {}]
]


(**************************************************************************************************)

PackageExport["TagVertexAdjacentEdgeTable"]

SetUsage @ "
TagVertexAdjacentEdgeTable[graph$] returns an association from each cardinal to its VertexAdjacentEdgeTable.
* If a cardinal is not incident to a given vertex, the corresponding entry is None.
* Keys are included for negations of cardinals.
* As there is a maximum of edge for a given vertex and cardinal, table entries are single integers or None.
"

TagVertexAdjacentEdgeTable[graph_] := Scope[
  outTable = VertexOutEdgeTable @ graph;
  inTable = VertexInEdgeTable @ graph;
  Merge[mergeNone] @ KeyValueMap[
    {key, edgeIndices} |-> {
      key ->          Map[First[Intersection[#, edgeIndices], None]&, outTable],
      Negated[key] -> Map[First[Intersection[#, edgeIndices], None]&, inTable]
    },
    TagIndices @ graph
  ]
];

mergeNone[{a_}] := a;
mergeNone[{a_, b_}] := MapThread[If[#1 === None, #2, #1]&, {a, b}];

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

ToGraph = Case[
  g_Graph                 := g;
  list:{Repeated[$edgeP]} := Graph[list];
  _                       := $Failed
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
  If[newVertices === All,
    newVertexIndices = Range @ Length @ oldVertices;
    newVertices = oldVertices;
  ,
    newVertexIndices = Map[IndexOf[oldVertices, #]&, newVertices];
    newVertexOrdering = Ordering[newVertexIndices];
    newVertices = Part[newVertices, newVertexOrdering];
  ];
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

ExtractGraphPrimitiveCoordinates[graph_] := GraphCachedScope[graph,

  If[!GraphQ[graph], ReturnFailed[]];
  igraph = ToIndexGraph[graph];
  If[!GraphQ[igraph], ReturnFailed[]];

  {graphLayout, vertexCoordinates} =
    LookupOption[igraph, {GraphLayout, VertexCoordinates}];

  $egpGraph = graph;

  {layoutDimension, viewOptions, coordinateTransformFunction} =
    LookupExtendedGraphAnnotations[graph, {LayoutDimension, ViewOptions, CoordinateTransformFunction}];

  actualDimension = Which[
    ContainsQ[graphLayout, "Dimension" -> 3] || CoordinateMatrixQ[vertexCoordinates, 3], 3,
    ContainsQ[graphLayout, "Dimension" -> 2] || CoordinateMatrixQ[vertexCoordinates, 2], 2,
    True, Automatic
  ];
  Which[
    actualDimension === layoutDimension === Automatic,
      actualDimension = 2,
    actualDimension === Automatic,
      actualDimension = layoutDimension,
    True,
      Null
  ];

  SetAutomatic[graphLayout, {}];

  vertexCount = VertexCount @ igraph;
  vertexCoordinates = ConstantArray[0., {vertexCount, actualDimension}];

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

  If[(isMulti || !DuplicateFreeQ[edgeList]) && FreeQ[graphLayout, "MultiEdgeDistance" | "SpringElectricalEmbedding"],
    graphLayout = ToList[graphLayout, "MultiEdgeDistance" -> 0.3];
  ];

  newGraph = If[actualDimension == 3, Graph3D, Graph][
    VertexList @ igraph, EdgeList @ igraph,
    VertexShapeFunction -> captureVertexCoordinates,
    EdgeShapeFunction -> ({coords, edge} |-> edgeCaptureFunction[coords, sortUE @ edge]),
    GraphLayout -> graphLayout, VertexCoordinates -> LookupOption[igraph, VertexCoordinates]
  ];

  gdResult = Check[GraphComputation`GraphDrawing @ newGraph, $Failed];
  If[FailureQ[gdResult],
    vertexCoordinates = CirclePoints @ vertexCount;
    If[actualDimension === 3, vertexCoordinates //= AppendColumn @ Zeros @ vertexCount];
    edgeCoordinateLists = Part[vertexCoordinates, #]& /@ EdgePairs @ igraph;
    Goto[end];
  ];

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

  Label[end];
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
ExtendedGraphPlot::badcoordtrans = "CoordinateTransformFunction -> `` issued messages on application.";
ExtendedGraphPlot::badcoordtransname = "CoordinateTransformFunction -> `` is not one of ``."

applyCoordinateTransform[Automatic|None] :=
  Null

applyCoordinateTransform[list_List] :=
  Scan[applyCoordinateTransform, list];

applyCoordinateTransform[f_] := Block[{res},
  res = Check[
    vertexCoordinates = Map[f, vertexCoordinates];
    edgeCoordinateLists = Map[f, edgeCoordinateLists, {-2}];,
    $Failed
  ];
  If[FailureQ[res], Message[ExtendedGraphPlot::badcoordtrans, f]];
];

applyCoordinateTransform["Center"] := Scope[
  center = Mean @ vertexCoordinates;
  applyCoordinateTransform[TranslationTransform[-center]];
];

applyCoordinateTransform["Snap"] :=
  applyCoordinateTransform[{"Snap", 10}];

applyCoordinateTransform[{"Snap", m_, nudge_:0.1}] := Scope[
  applyCoordinateTransform["Center"];
  bounds = CoordinateBounds[edgeCoordinateLists];
  step = (EuclideanDistance @@@ bounds) / m;
  grid = Flatten[CoordinateBoundsArray[bounds, step], 1];
  nearest = Nearest @ grid;
  applyCoordinateTransform[nearest /* First];
  duplicateIndices = DuplicateIndices @ vertexCoordinates;
  newVertexCoordinates = vertexCoordinates;
  adjacencyTable = VertexAdjacencyTable @ $egpGraph;
  $nudge = nudge;
  Scan[index |-> (
    center = Mean @ Part[vertexCoordinates, Part[adjacencyTable, index]];
    Part[newVertexCoordinates, index] //= nudgeDuplicate[center]),
    duplicateIndices, {2}];
  vertexCoordinates ^= newVertexCoordinates;
  edgeCoordinateLists ^= Part[vertexCoordinates, #]& /@ EdgePairs[$egpGraph];
];

nudgeDuplicate[z_][p_] := p + Normalize[Cross[z - p]] * Im[$nudge] + Normalize[z - p] * Re[$nudge];

DuplicateIndices[list_] :=
  Select[Length[#] > 1&] @ Values @ PositionIndex @ vertexCoordinates;

applyCoordinateTransform[{"Rotate", n_}] := Scope[
  applyCoordinateTransform["Center"];
  applyCoordinateTransform[RotationTransform[n * Degree]];
];

applyCoordinateTransform[{"Radial", f_}] := Scope[
  applyCoordinateTransform["Center"];
  applyCoordinateTransform[Normalize[#] * f[Norm[#]]&];
];

applyCoordinateTransform["PolarProjection"] :=
  applyCoordinateTransform[{"PolarProjection", 1}];

applyCoordinateTransform[{"PolarProjection", h_}] := Scope[
  applyCoordinateTransform["Center"];
  applyCoordinateTransform[Apply[{x, y, z} |-> {x / (h-z), y/(h-z)}]];
];

$namedTransforms = <|
  "Rotate90" -> RotationTransform[90 * Degree],
  "Rotate180" -> RotationTransform[180 * Degree],
  "Rotate270" -> RotationTransform[270 * Degree],
  "ReflectHorizontal" -> ReflectionTransform[{1, 0}],
  "ReflectVertical" -> ReflectionTransform[{0, 1}],
  "ShrinkHorizontal" -> ScalingTransform[{0.75, 1}],
  "ShrinkVertical" -> ScalingTransform[{1, 0.75}],
  "ExpandHorizontal" -> ScalingTransform[{1.25, 1}],
  "ExpandVertical" -> ScalingTransform[{1, 1.25}]
|>;

applyCoordinateTransform["BendVertical"] :=
  edgeCoordinateLists //= Map[bendVertical];

bendVertical[{a:{ax_, ay_}, b:{bx_, by_}}] := Scope[
  If[EuclideanDistance[ax, bx] < 0.001, Return @ {a, b}];
  c = {bx, ay};
  ca = along[c, a, .25];
  cb = along[c, b, .25];
  Join[{a}, DiscretizeCurve[{ca, c, cb}], {b}]
];

bendVertical[line_] := line;

applyCoordinateTransform["SquareSelfLoops"] :=
  edgeCoordinateLists //= Map[squareSelfLoop];

squareSelfLoop[list:{a_, Repeated[_, {3, Infinity}], b_}] /; EuclideanDistance[a, b] < 0.01 := Scope[
  c = Mean @ list;
  {p1, p3} = {{xl, yl}, {xh, yh}} = CoordinateBoundingBox @ list;
  p2 = {xh, yl}; p4 = {xl, yh};
  ang = ArcTan @@ (a - c); ang *= 2/Pi;
  (* p4 p3
     p1 p2 *)
  {u, v, w, x} = Which[
    -0.5 <= ang < +0.5, (* E *) {p3, p4, p1, p2},
    +0.5 <= ang < +1.5, (* N *) {p4, p1, p2, p3},
    -1.5 <= ang < -0.5, (* S *) {p2, p3, p4, p1},
    True,               (* W *) {p1, p2, p3, p4}
  ];
  trunc = 0.5;
  v = (v * trunc + u * (1 - trunc));
  w = (w * trunc + x * (1 - trunc));
  DiscretizeCurve[{corner[a, u, v], corner[u, v, w], corner[v, w, x], corner[w, x, a], a}, BSplineCurve]
];

$cr = .1;
corner[a_, b_, c_] :=
  Splice @ {a, along[b, a, $cr], along[b, a, 0.8*$cr], b, along[b, c, 0.8*$cr], along[b, c, $cr]};

along[a_, b_, d_] := PointAlongLine[{a, b}, d];

squareSelfLoop[line_] := line;

applyCoordinateTransform[name_String] := Scope[
  trans = Lookup[$namedTransforms, name,
    Message[ExtendedGraphPlot::badcoordtransname, name, commaString @ Keys @ $namedTransforms];
    $Failed
  ];
  If[FailureQ[trans], ReturnFailed[]];
  applyCoordinateTransform @ trans
];

applyCoordinateTransform[ProjectionOnto[shape_]] := Block[{$rnf},
  $rnf = BoundaryProjection @ shape;
  If[FailureQ[$rnf], Message[ExtendedGraphPlot::badwrappedshape]; Return @ $Failed];
  vertexCoordinates //= $rnf;
  edgeCoordinateLists //= Map[projectLineOntoRNF];
];

projectLineOntoRNF = Case[
  {a_, b_} ? CoordinateMatrixQ /; (Head[$rnf] === RegionNearestFunction) :=
    $rnf @ Interpolated[a, b, 6];
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
PackageScope["$GraphOrigin"]
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
    $GraphOrigin := $GraphOrigin = LookupExtendedGraphAnnotations[$Graph, GraphOrigin],
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