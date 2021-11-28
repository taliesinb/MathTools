PackageExport["Quiver"]

SetUsage @ "
Quiver[graph$] constructs a cardinal quiver from a graph.
Quiver[edges$] constructs a cardinal quiver from a list of edges.
Quiver[vertices$, edges$] constructs a cardinal quiver from a list of vertices and edges.
* The edges of graph$ should be tagged with cardinals.
* The edges incident to one vertex should not be tagged with a cardinal more than once.
* The resulting graph will display with a legend showing the cardinals associated with each edge.
"

DeclareArgumentCount[Quiver, {1, 2}];

Options[Quiver] = $simpleGraphOptionRules;

declareSyntaxInfo[Quiver, {_, _., OptionsPattern[]}];

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

processEdge[Labeled[edges:{__Rule}, labels_List], _] /; Length[edges] === Length[labels] :=
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

$maxVertexCount = 150;
makeQuiver[vertices_, edges_, newOpts_] := Scope[

  $edgeAnnotations = <||>;

  If[!MatchQ[edges, {DirectedEdge[_, _, Except[_Alternatives]]..}],
    $ea = {};
    edges = Flatten @ List @ processEdge[edges, None];
    If[ContainsQ[edges, $Failed], ReturnFailed[]];
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

PackageExport["ToQuiver"]

SetUsage @ "
ToQuiver[obj$] attempts to convert obj$ to a quiver Graph[$$] object.
* If obj$ is already a quiver graph, it is returned unchanged.
* If obj$ is a list of rules, it is converted to a quiver graph.
* Otherwise, $Failed is returned.
"

ToQuiver = MatchValues[
  graph_Graph := If[QuiverQ @ graph, graph, Quiet @ Quiver @ graph];
  edges_List := Quiet @ Quiver @ edges;
  str_String := BouquetQuiver @ str;
  _ := $Failed;
];

(**************************************************************************************************)

PackageExport["BouquetQuiver"]

SetUsage @ "
BouquetQuiver[cardinals$] creates a Bouquet cardinal quiver graph with the given cardinal edges.
BouquetQuiver['string$'] uses the characters of 'string$' as cardinals.
"

DeclareArgumentCount[BouquetQuiver, 1];

Options[BouquetQuiver] = $simpleGraphOptionRules;

declareSyntaxInfo[BouquetQuiver, {_, OptionsPattern[]}];


BouquetQuiver[n_Integer, opts:OptionsPattern[]] := BouquetQuiver[
  Switch[n, 1, "r", 2, "rb", 3, "rgb", 4, "rgbw", _, Take[$alphabet, n]],
  opts
];

BouquetQuiver[str_String, opts:OptionsPattern[]] := BouquetQuiver[Characters[str], opts];

BouquetQuiver[cardinals_List, opts:OptionsPattern[]] :=
  Quiver[Map[c |-> Labeled[1 -> 1, c], cardinals], Cardinals -> cardinals, opts, GraphOrigin -> 1]

(**************************************************************************************************)

PackageExport["QuiverQ"]

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

PackageExport["FreeQuiver"]

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

Options[FreeQuiver] = $simpleGraphOptionRules;

FreeQuiver[graph_, opts:OptionsPattern[]] := Scope[
  $count = 1;
  makeQuiver[VertexList @ graph, Map[toFreeQuiverEdge, EdgeList @ graph], {opts}]
];

(**************************************************************************************************)

PackageExport["CardinalList"]

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

PackageExport["LookupCardinalColors"]

SetUsage @ "
LookupCardinalColors[quiver$] returns the association of cardinals to colors for quiver$.
LookupCardinalColors[quiver$, c$] returns the color for cardinal c$.
* The annotation CardinalColors is returned if present.
* The cardinals are given in sorted order.
* If the graph has no tagged edges, <||> is returned.
* If c$ is an CardinalSet, the corresponding colors will be blended.
"

LookupCardinalColors[graph_Graph] := Scope[
  UnpackExtendedOptions[graph, cardinalColorRules, cardinalColors, cardinalColorFunction];
  cardinals = CardinalList @ graph;
  Which[
    cardinals === None,
      <||>,
    ColorVectorQ[cardinalColors] && SameLengthQ[cardinalColors, cardinals],
      AssociationThread[cardinals, cardinalColors],
    AssociationQ[cardinalColors],
      cardinalColors,
    AssociationQ[cardinalColorFunction],
      AssociationThread[cardinals, Lookup[cardinalColorFunction, cardinals, $Gray]],
    cardinalColorFunction =!= None,
      AssociationMap[cardinalColorFunction, cardinals],
    RuleListQ @ cardinalColorRules,
      AssociationThread[
        cardinals,
        Replace[cardinals, Append[cardinalColorRules, _ -> $Gray], {1}]
      ],
    True,
      ChooseCardinalColors @ cardinals
  ]
];

(* if you look up a programmatically generated color for a cardinal not present in the cardinal list,
we can still make it work properly: used for glued graphs *)
LookupCardinalColors[graph_Graph, card_] /; LookupExtendedOption[graph, CardinalColorFunction] =!= None := Scope[
  UnpackExtendedOptions[graph, cardinalColorFunction];
  If[ListQ[card],
    AssociationMap[cardinalColorFunction, card],
    cardinalColorFunction @ card
  ]
];

LookupCardinalColors[graph_Graph, card_] :=
  Lookup[LookupCardinalColors @ graph, card, Gray];

LookupCardinalColors[graph_Graph, CardinalSet[cards_List]] :=
  HumanBlend @ Sort @ Lookup[LookupCardinalColors @ graph, cards, Gray];

LookupCardinalColors[_] := $Failed;

(**************************************************************************************************)

PackageExport["ChooseCardinalColors"]

ChooseCardinalColors[None, ___] := <||>;

$xyzColors = <|"x" -> $Red, "y" -> $Green, "z" -> $Blue|>;
$xyColors = <|"x" -> $Red, "y" -> $Blue|>;
$rgbwxColors = <|
  "r" -> $Red, "g" -> $Green, "b" -> $Blue,
  "rb" -> $Purple, "rg" -> $Orange, "gb" -> $Teal,
  "w" -> $Gray, "x" -> $DarkGray
|>;

$colorFormed := $colorFormed = Map[Blank, $colorFormP];

ChooseCardinalColors[cardinals_List, palette_:Automatic] := Switch[Sort @ cardinals,
  {___, "f", ___},
    Append[ChooseCardinalColors[DeleteCases[cardinals, "f"]], "f" -> $Orange],
  {"x"},
    <|"x" -> $Red|>,
  {"x", "y"},
    $xyColors,
  {"x", "y", "z"},
    $xyzColors,
  {Repeated[$colorFormed]},
    AssociationThread[cardinals, Lookup[$colorFormAssoc, Part[cardinals, All, 0]]],
  set_ /; SubsetQ[Keys @ $rgbwxColors, set],
    KeyTake[$rgbwxColors, cardinals],
  _,
    AssociationThread[cardinals, ToColorPalette[palette, Length @ cardinals]]
];

(**************************************************************************************************)

PackageExport["RenameCardinals"]

RenameCardinals[graph_Graph, renaming:{__String}] :=
  RenameCardinals[graph, RuleThread[CardinalList @ graph, renaming]]

RenameCardinals[graph_Graph, {}] := graph;

RenameCardinals[graph_Graph, renaming:{__Rule}] := Scope[
  {vertices, edges} = VertexEdgeList @ graph;
  replacer = ReplaceAll @ Dispatch @ renaming;
  edges = MapAt[replacer, edges, {All, 3}];
  opts = DeleteOptions[AnnotationRules] @ Options @ graph;
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

PackageExport["TruncateQuiver"]
PackageExport["TruncatedVertex"]

declareFormatting[
  TruncatedVertex[v_, a_] :> Superscript[v, a]
];

SetUsage @ "
TruncatedVertex[vertex$, card$] represents a vertex that has been truncated in the direction card$.
";

Options[TruncateQuiver] = Prepend["AllowSkips" -> True] @ $simpleGraphOptionRules;

TruncateQuiver[quiver_, opts:OptionsPattern[]] :=
  TruncateQuiver[quiver, Automatic, opts];

TruncateQuiver[quiver_, cardinals:Except[_Rule], userOpts:OptionsPattern[]] := Scope[
  UnpackOptions[allowSkips];
  {vertices, edges} = VertexEdgeList @ quiver;
  SetAutomatic[cardinals, t = CardinalList[quiver]; Join[t, Negated /@ t]];
  If[StringQ[cardinals], cardinals //= ToPathWord];
  ordering = AssociationRange[cardinals]; $n = Length @ cardinals;
  tagTable = Map[SortBy[cardOrder[ordering]], VertexTagTable[quiver, False]];
  tagOutTable = TagVertexOutTable @ quiver;
  vertexCoords = GraphVertexCoordinates @ quiver;
  CollectTo[{truncEdges, truncVertices, truncCoords},
  ScanThread[{v, tags, coord, tagOut} |-> (
    cornerVerts = Map[TruncatedVertex[v, #]&, tags];
    cornerEdges = If[allowSkips, cornerEdge, noskipCornerEdge[ordering]] /@ Partition[cornerVerts, 2, 1, 1];
    cornerCoords = Map[
      PointAlongLine[
        {coord, Part[vertexCoords, Lookup[tagOut, Replace[#, CardinalSet[s_] :> First[s]]]]},
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
    AnnotationRules -> DeleteOptions[annos, VertexAnnotations]];
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
  DirectedEdge[TruncatedVertex[a, c], TruncatedVertex[b, Negated @ c], CardinalSet[{"f", Negated @ "f"}]];

(**************************************************************************************************)

$BigFiveThemeRules = {
  AspectRatioClipping -> False,
  ArrowheadSize -> 12,
  VertexSize -> 5,
  ImageSize -> ("ShortestEdge" -> 30)
};

$GraphThemeData["BigFive"] := $BigFiveThemeRules;

(**************************************************************************************************)

PackageExport["TriangularQuiver"]

Options[TriangularQuiver] = Options[Graph];

TriangularQuiver[n_Integer, opts:OptionsPattern[]] :=
  TriangularQuiver[n, {"r", "g", "b"}, opts];

TriangularQuiver[n_Integer, cards:{x_, y_, z_}, opts:OptionsPattern[]] := Scope[
  vertices = Catenate @ Array[VertexProduct, {n, n}];
  edges = Flatten @ {
    Table[DirectedEdge[VertexProduct[i, j], VertexProduct[i + 1, j], x], {i, n-1}, {j, n}],
    Table[DirectedEdge[VertexProduct[i, j], VertexProduct[i, j + 1], y], {i, n}, {j, n-1}],
    Table[DirectedEdge[VertexProduct[i, j], VertexProduct[i + 1, j + 1], z], {i, n-1}, {j, n-1}]
  };
  vertices //= Select[upperTriProdQ];
  edges //= Select[upperTriProdQ];
  ExtendedGraph[
    vertices, edges,
    opts,
    ImageSize -> ("ShortestEdge" -> 33),
    VertexCoordinates -> N[({#1 - #2/2, #2 * Sqrt[3]/2}& @@@ vertices)],
    GraphTheme -> "BigFive",
    Cardinals -> cards
  ]
]

upperTriProdQ[VertexProduct[a_, b_]] := a <= b;
upperTriProdQ[DirectedEdge[t_, h_, _]] := upperTriProdQ[t] && upperTriProdQ[h];

(**************************************************************************************************)

PackageExport["HexagonalQuiver"]

Options[HexagonalQuiver] = Options[Graph];

HexagonalQuiver[n_Integer, opts:OptionsPattern[]] := Scope[
  n2 = 2 * n;
  z = (3 * n2 + 1)/2;
  cards = {"x", "y", "z"};
  edges = makeHexSkeleton[{{-1,1},{-1,1},{-1,1}}*n2, z, cards];
  vertices = AllUniqueVertices @ edges;
  ExtendedGraph[
    vertices, edges, opts,
    VertexCoordinates -> Map[DotABC, List @@@ vertices],
    Cardinals -> cards,
    ImageSize -> ("ShortestEdge" -> 25),
    GraphTheme -> "BigFive"
  ]
];

$hexNormVecs = {{3, 2, 1}, {1, 3, 2}, {2, 1, 3}};

hexNorm[v_List] := Max[Abs @ Dot[$hexNormVecs, v]];

makeHexSkeleton[{{a1_, a2_}, {b1_, b2_}, {c1_, c2_}}, norm_, {x_, y_, z_}] := Scope[
  ab = Tuples[{Range[a1, a2], Range[b1, b2]}];
  abc = Append[#, -Total[#]]& /@ ab;
  abc //= Select[c1 <= Last[#] <= c2&];
  vertices = VertexProduct @@@ abc;
  edges = Flatten @ {
    hexEdgeList[x, abc, {2, 1, 0}, {1, -1, 0}, 1],
    hexEdgeList[y, abc, {0, 2, 1}, {0, 1, -1}, 1],
    hexEdgeList[z, abc, {1, 0, 2}, {-1, 0, 1}, 1]
    };
  abc //= Select[hexNorm[#] <= norm&];
  vertices = VertexProduct @@@ abc;
  isVertex = ConstantAssociation[vertices, True];
  Select[edges, isVertex[Part[#, 1]] && isVertex[Part[#, 2]]&]
];

hexEdgeList[card_, vertexCoords_, normal_, offset_, mod_] := Map[
  vertex |-> If[Mod[Dot[normal, vertex], 3] == mod,
    DirectedEdge[VertexProduct @@ vertex, VertexProduct @@ Plus[vertex, offset], card],
    Nothing
  ],
  vertexCoords
];

(**************************************************************************************************)

PackageExport["SquareQuiver"]

Options[SquareQuiver] = Options[Graph];

SquareQuiver[m:$ModIntP, opts:OptionsPattern[]] :=
  SquareQuiver[{m, m}, opts];

SquareQuiver[spec_, opts:OptionsPattern[]] :=
  SquareQuiver[spec, {"x", "y"}, opts];

SquareQuiver[spec:{$ModIntP, $ModIntP}, {cx_, cy_}, opts:OptionsPattern[]] := Scope[
  {m, n} = StripModulo @ spec;
  {mp1, np1} = toModPlusOne @ spec;
  vertices = Catenate @ Array[VertexProduct, StripModulo @ {m, n}];
  edges = Flatten @ {
    Table[enrichedEdge[VertexProduct[i, j], VertexProduct[mp1 @ i, j], cx, i], {i, m}, {j, n}],
    Table[enrichedEdge[VertexProduct[i, j], VertexProduct[i, np1 @ j], cy, j], {i, m}, {j, n}]
  };
  edges //= Select[MemberQ[vertices, Part[#, 2]]&];
  basis = {{1, 0}, {0, 1}};
  ExtendedGraph[
    vertices, edges,
    opts,
    VertexCoordinates -> (List @@@ vertices),
    Cardinals -> {cx, cy},
    Sequence @@ modEdgeShapeFunctionSpec[spec, basis],
    GraphTheme -> "BigFive"
  ]
]

(**************************************************************************************************)

PackageExport["CubicQuiver"]

Options[CubicQuiver] = Options[Graph];

CubicQuiver[m:$ModIntP, opts:OptionsPattern[]] :=
  CubicQuiver[{m, m, m}, opts];

CubicQuiver[spec_, opts:OptionsPattern[]] :=
  CubicQuiver[spec, {"x", "y", "z"}, opts];

CubicQuiver[spec:{$ModIntP, $ModIntP, $ModIntP}, {cx_, cy_, cz_}, opts:OptionsPattern[]] := Scope[
  {m, n, p} = StripModulo @ spec;
  {mp1, np1, pp1} = toModPlusOne @ spec;
  vertices = Flatten[Array[VertexProduct, {m, n, p}], 2];
  edges = Flatten @ {
    Table[enrichedEdge[VertexProduct[i, j, k], VertexProduct[mp1 @ i, j, k], cx, i], {i, m}, {j, n}, {k, p}],
    Table[enrichedEdge[VertexProduct[i, j, k], VertexProduct[i, np1 @ j, k], cy, j], {i, m}, {j, n}, {k, p}],
    Table[enrichedEdge[VertexProduct[i, j, k], VertexProduct[i, j, pp1 @ k], cz, j], {i, m}, {j, n}, {k, p}]
  };
  edges //= Select[MemberQ[vertices, Part[#, 2]]&];
  basis = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}};
  ExtendedGraph[
    vertices, edges,
    opts,
    VertexCoordinates -> (List @@@ vertices),
    Cardinals -> {cx, cy, cz},
    LayoutDimension -> 3,
    Sequence @@ modEdgeShapeFunctionSpec[spec, basis],
    GraphTheme -> "BigFive3D"
  ]
]

toModPlusOne[spec_] := PlusOneMod[GetModulus @ #, 1]& /@ spec;

modEdgeShapeFunctionSpec[spec_, basis_] /; ContainsQ[spec, Modulo] := Scope[
  dim = InnerDimension @ basis;
  tuples = Tuples @ MapThread[makeModOffset, {spec, basis}];
  offsets = DeleteCases[{0..}] @ Map[Total] @ tuples;
  {
    EdgeShapeFunction -> ModulusEdgeShapeFunction[offsets],
    ImagePadding -> 25
  }
];

makeModOffset[Modulo[n_], vec_] := vec * #& /@ {-n, 0, n};
makeModOffset[_, _] := ConstantArray[0, {1, dim}];

modEdgeShapeFunctionSpec[spec_, basis_] := {};

(**************************************************************************************************)

$BigFive3DThemeRules = {
  ArrowheadSize -> 15,
  VertexSize -> 4,
  VertexShapeFunction -> "Point",
  ImageSize -> ("ShortestEdge" -> 80),
  ViewOptions->{ViewVector->{-3.333,-10,10},ViewProjection->"Orthographic", "ShrinkWrap" -> True}
};

$GraphThemeData["BigFive3D"] := $BigFive3DThemeRules;

(**************************************************************************************************)

PackageExport["LineQuiver"]

Options[LineQuiver] = Options[Graph];

LineQuiver[n_, opts:OptionsPattern[]] :=
  LineQuiver[n, "x", opts];

LineQuiver[spec_, card_, opts:OptionsPattern[]] := Scope[
  n = spec;
  vertices = Switch[n,
    _Integer, Range @ n,
    _Span, Range @@ n,
    _, ReturnFailed[];
  ];
  edges = MapStaggered[enrichedEdge[#1, #2, card, #1]&, vertices];
  ExtendedGraph[
    vertices, edges,
    opts,
    GraphTheme -> "BigFive",
    ExtendedGraphLayout -> "Linear"
  ]
]

LineQuiver[Modulo[n_Integer], card_, opts:OptionsPattern[]] := Scope[
  np1 = PlusOneMod[n, 1];
  vertices = Range @ n;
  edges = Table[enrichedEdge[i, np1 @ i, cx, i], {i, n}];
  basis = {{1, 0}};
  ExtendedGraph[
    vertices, edges,
    opts,
    VertexCoordinates -> Transpose[{vertices, Zeros @ n}],
    Cardinals -> {cx, cy},
    Sequence @@ modEdgeShapeFunctionSpec[{Modulo @ n}, basis],
    GraphTheme -> "BigFive"
  ]
]

(**************************************************************************************************)

PackageExport["CycleQuiver"]

Options[CycleQuiver] = Options[Graph];

CycleQuiver[n_Integer, opts:OptionsPattern[]] :=
  CycleQuiver[n, "x", opts];

CycleQuiver[n_Integer, card_, opts:OptionsPattern[]] := Scope[
  vertices = Range[1, n];
  edges = enrichedEdge[#1, #2, card, #1]& @@@ Partition[vertices, 2, 1, 1];
  ExtendedGraph[
    vertices, edges,
    opts, VertexLayout -> LinearLayout[],
    GraphTheme -> "BigFive",
    ImageSize -> ("ShortestEdge" -> 35)
  ]
]

(**************************************************************************************************)

enrichedEdge[a_, b_, card_, n_] :=
  DirectedEdge[a, b, card];

enrichedEdge[a_, b_, cs_SerialCardinal, n_] :=
  enrichedEdge[a, b, ModPart[cs, n], n];

enrichedEdge[a_, b_, p_ParallelCardinal, n_] :=
  DirectedEdge[a, b, CardinalSet @ (List @@ p)];

ModPart[seq_, n_] := Part[seq, Mod[n, Length[seq], 1]];

(**************************************************************************************************)

PackageExport["GridQuiver"]

Options[GridQuiver] = Options[Graph];

GridQuiver[k_Integer, n:$ModIntP, opts:OptionsPattern[]] :=
  Switch[k,
    1, LineQuiver[n, opts],
    2, SquareQuiver[n, opts],
    3, CubicQuiver[n, opts],
    _, generalGridQuiver[k, n, opts]
  ];

generalGridQuiver[k_, n_, opts___] := Scope[
  vertices = Flatten @ Array[VertexProduct, ConstantArray[StripModulo @ n, k]];
  edges = Flatten @ Table[Map[generalGridEdge[n, i], vertices], {i, 1, k}];
  ExtendedGraph[
    vertices, edges,
    opts,
    ExtendedGraphLayout -> "SpringElectrical",
    Cardinals -> Range[k],
    LayoutDimension -> 3,
    Sequence @@ modEdgeShapeFunctionSpec[n, IdentityMatrix[k]],
    GraphTheme -> "BigFive3D"
  ]
]

generalGridEdge[Modulo[n_], i_][vertex_] :=
  DirectedEdge[vertex, MapAt[PlusOneMod[n], vertex, i], i];

generalGridEdge[n_, i_][vertex_] :=
  If[Part[vertex, i] < n,
    DirectedEdge[vertex, MapAt[PlusOne, vertex, i], i],
    {}
  ];

(**************************************************************************************************)

PackageExport["TreeQuiver"]
PackageExport["TreeVertex"]

Options[TreeQuiver] = Options[Graph];

TreeQuiver[k_Integer, n_Integer, opts:OptionsPattern[]] := Scope[
  If[!OddQ[n], ReturnFailed[]];
  n = (n - 1) / 2;
  cards = Join[Range[k], Negated /@ Range[k]];
  vertices = Flatten @ Table[TreeVertex @@@ Tuples[cards, i], {i, 0, n}];
  vertices = Discard[vertices, MatchQ[TreeVertex[___, c_, Negated[c_], ___] | TreeVertex[___, Negated[c_], c_, ___]]];
  edges = makeTreeEdge /@ vertices;
  vectorAssoc = AssociationThread[cards, CirclePoints[2 * k]];
  coords = Map[treeVertexCoord, vertices];
  scaling = 1 / k;
  ExtendedGraph[
    vertices, edges,
    opts,
    VertexCoordinates -> coords,
    Cardinals -> Range[k],
    GraphLayout -> {"NudgeDistance" -> 0},
    ImageSize -> "AverageEdge" -> 30,
    GraphTheme -> "BigFive"
  ]
]

treeVertexCoord = Case[
  TreeVertex[] := {0, 0};
  t_TreeVertex := Total[Lookup[vectorAssoc, List @@ t] * Power[scaling, Range @ Length @ t]];
];

makeTreeEdge = Case[
  TreeVertex[] := Nothing;
  t_TreeVertex := DirectedEdge[Most @ t, t, Last @ t];
]

(**************************************************************************************************)

PackageExport["LatticeQuiverCoordinates"]

LatticeQuiverCoordinates[quiver_Graph, Automatic] :=
  LatticeQuiverCoordinates[quiver, chooseLatticeBasisVectors @ Sort @ CardinalList @ quiver];

LatticeQuiverCoordinates[quiver_Graph, "Triangular"] :=
  LatticeQuiverCoordinates[quiver, $abcVectors];

$s32 = Sqrt[3]/2;
$abcVectors = Simplify /@ {{0, 1}, {-$s32, -1/2}, {$s32, -1/2}};

chooseLatticeBasisVectors = Case[
  {"x", "y"} | {"b", "r"}             := {{1,0}, {0, 1}};
  {"x", "y", "z"} | {"b", "g", "r"}   := {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}};
  {"a", "b", "c"}                     := $abcVectors;
  other_ := Take[CirclePoints[Length[other] * 2], Length @ other];
]

LatticeQuiverCoordinates[quiver_Graph, latticeBasis_] := Scope[
  If[ListQ[latticeBasis], latticeBasis = AssociationThread[CardinalList @ quiver, latticeBasis]];
  indexGraph = ToIndexGraph @ quiver;
  outTable = VertexOutVertexTagTable @ indexGraph;
  dims = Rest @ Dimensions @ Values @ latticeBasis;
  coords = ConstantArray[0, Prepend[dims, VertexCount @ indexGraph]];
  edgeBasis = Map[latticeBasis, EdgeTagAssociation @ indexGraph];
  edgeIndex = EdgePairIndexAssociation @ indexGraph;
  visitedEdges = CreateDataStructure["HashSet"];
  initial = MinimumIndex @ VertexInDegree @ indexGraph;
  BreadthFirstScan[indexGraph, initial, {"DiscoverVertex" -> (
    {new, old, d} |-> If[new =!= old,
      Set[Part[coords, new], Part[coords, old] + edgeBasis[{old, new}]];
      visitedEdges["Insert", edgeIndex[{old, new}]];
    ]
  )}];
  coords //= ToPackedReal;
  {coords, visitedEdges["Elements"]}
];


