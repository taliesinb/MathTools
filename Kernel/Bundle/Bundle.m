(**************************************************************************************************)

PublicFunction[BundleGraphQ]

General::notbundle = "Input is not a bundle graph, which is a directed graph with vertices of the form \
BundleVertex[..., ...] and edges tagged with BundleCardinal[_, _]."
BundleGraphQ[g_] := And[
  EdgeTaggedGraphQ[g],
  VertexCount[g, BundleVertex[_, _]] === VertexCount[g],
  EdgeCount[g, DirectedEdge[_, _, BundleCardinal[_, _]]] === EdgeCount[g]
];

(**************************************************************************************************)

PublicHead[BundleVertex, BundleCardinal]

declareBoxFormatting[
  BundleVertex[b_, f_] :> makeColonPair[b, f],
  BundleCardinal[b_, f_] :> makeColonPair[b, f],
  BundleCardinal[None, f_] :> makeColonPair["|", f],
  BundleCardinal[b_, None] :> makeColonPair[b, "-"]
];

BundleCardinal /: Inverted[c_BundleCardinal] := Map[Inverted, c];

PrivateFunction[makeColonPair]

makeColonPair[a_, b_] := ToBoxes @ Row[{a, GrayForm @ ":", b}];

(**************************************************************************************************)

PublicFunction[BundleToFiberGraph]

BundleToFiberGraph[bundle_Graph, baseVertex_:Automatic] := Scope[
  hash = Hash[bundle];
  fiberGraph = bundleHashLookup[hash, "FiberGraph"];
  If[GraphQ[fiberGraph], Return @ fiberGraph];
  SetAutomatic[baseVertex, First @ VertexList @ bundle];
  ExtendedSubgraph[bundle, Cases[VertexList @ bundle, BundleVertex[baseVertex, _]], Automatic]
];

(**************************************************************************************************)

PublicFunction[BundleToBaseGraph]

BundleToBaseGraph::noedges = "The base graph appears to have no edges.";
BundleToBaseGraph[bundle_Graph] := Scope[
  If[!BundleGraphQ[bundle], ReturnFailed["notbundle"]];
  {vertices, edges} = VertexEdgeList[bundle];
  baseVertices = DeleteDuplicates @ FirstColumn @ vertices;
  baseEdges = UniqueCases[edges, head_[BundleVertex[b0_, _],  BundleVertex[b1_, _], BundleCardinal[bt_, _]] /; b0 =!= b1 :> head[b0, b1, bt]];
  If[baseEdges === {}, Message[BundleToBaseGraph::noedges]];
  vertexCoords = None;
  vcf = LookupExtendedOption[bundle, VertexCoordinateFunction];
  baseVCF = If[MatchQ[vcf, BundleVertexCoordinateFunction[_, _, _]], Map[TakeOperator[2], First @ vcf],
    vertexCoords = GroupBy[Normal @ LookupVertexCoordinates[bundle], PartOperator[1, 1] -> Last, Mean];
    None
  ];
  ExtendedGraph[baseVertices, baseEdges,
    VertexCoordinateFunction -> baseVCF,
    VertexCoordinates -> vertexCoords,
    GraphTheme -> LookupExtendedOption[bundle, GraphTheme]
  ]
];

(**************************************************************************************************)

PublicFunction[TrivialBundleGraph]

PublicOption[FiberScale, FiberCoordinateRotation, BaseCoordinateRotation, SectionDisplayMethod]

Options[TrivialBundleGraph] = JoinOptions[
  FiberScale -> 1,
  FiberCoordinateRotation -> Pi/2,
  BaseCoordinateRotation -> 0,
  LayoutDimension -> Automatic,
  SectionDisplayMethod -> Inherited,
  $ExtendedGraphOptions
];

$modIntP = _Integer ? Positive | Modulo[_Integer ? Positive];
$modIntListP = $modIntP | {$modIntP..};

colors1D = <|"b" -> {$Blue}, "f" -> {$Red}|>;
colors2D = <|"b" -> {$Blue, $Teal}, "f" -> {$Red, $Orange}|>
colors3D = <|"b" -> {$Blue, $Teal, $Green}, "f" -> {$Red, $Orange, $Yellow}|>

toQuiver = Case[
  {n_Integer}         := LineQuiver[n, $qname, LayoutDimension -> 1, EdgeLabelStyle -> None, CardinalColors -> colors1D[$qname]];
  {Modulo[n_Integer]} := CycleQuiver[n, $qname, LayoutDimension -> 1, EdgeLabelStyle -> None, CardinalColors -> colors1D[$qname]];
  {m_, n_}            := SquareQuiver[{m, n}, {$qname <> "x", $qname <> "y"}, LayoutDimension -> 2, EdgeLabelStyle -> None, CardinalColors -> colors2D[$qname]];
  {m_, n_, p_}        := CubicQuiver[{m, n, p}, {$qname <> "x", $qname <> "y", $qname <> "z"}, LayoutDimension -> 3, EdgeLabelStyle -> None, CardinalColors -> colors3D[$qname]];
];

TrivialBundleGraph[base:$modIntListP, fiber:$modIntListP, opts:OptionsPattern[]] := Scope[
  {base, fiber} = ToList /@ {base, fiber};
  $qname = "b"; baseQuiver = toQuiver @ base;
  $qname = "f"; fiberQuiber = toQuiver @ fiber;
  baseFiber = Join[base, fiber]; baseDim = Length[base]; fiberDim = Length[fiber];
  If[baseDim + fiberDim >= 4, ReturnFailed[]];
  displayMethod = If[MatchQ[baseFiber, {_, _}], "Total", "Color"];
  esf = MakeModulusEdgeShapeFunction @@ baseFiber;
  TrivialBundleGraph[
    baseQuiver, fiberQuiber, opts,
    SectionDisplayMethod -> displayMethod,
    EdgeShapeFunction -> esf, EdgeLabelStyle -> None,
    ImagePadding -> If[esf =!= Automatic, 30, Automatic],
    If[Length[baseFiber] >= 3, Seq[LayoutDimension -> 3, ImageSize -> "ShortestEdge" -> 45], Seq[]],
    If[Length[fiber] >= 2, FiberCoordinateRotation -> 0, Seq[]]
  ]
];

TrivialBundleGraph::arg12 = "`` argument should be a quiver, or an integer or list of integer (or Modulo of these), giving the `` graph.";
TrivialBundleGraph::baddisplay = "SectionDisplayMethod -> `` is invalid, should be one of ``.";
TrivialBundleGraph[baseGraph_, fiberGraph_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[fiberScale, fiberCoordinateRotation, baseCoordinateRotation, layoutDimension, sectionDisplayMethod];
  If[!MatchQ[sectionDisplayMethod, $bundleSectionDisplayMethodPattern | Inherited],
    ReturnFailed["baddisplay", sectionDisplayMethod, Append[Inherited] @ Cases[$bundleSectionDisplayMethodPattern, _String | _Symbol]]];
  graphs = {baseGraph, fiberGraph};
  If[!EdgeTaggedGraphQ[baseGraph], ReturnFailed["arg12", "First", "base"]];
  If[!EdgeTaggedGraphQ[fiberGraph], ReturnFailed["arg12", "Second", "fiber"]];
  {baseCoords, fiberCoords} = LookupVertexCoordinates /@ graphs;
  If[fiberCoordinateRotation =!= 0, fiberCoords //= RotateVector[fiberCoordinateRotation]];
  If[baseCoordinateRotation =!= 0,  baseCoords //= RotateVector[baseCoordinateRotation]];
  {baseVertices, fiberVertices} = VertexList /@ graphs;
  {baseEdges, fiberEdges} = EdgeList /@ graphs;
  {baseAdj, fiberAdj} = VertexAdjacentVertexEdgeAssociation /@ graphs;
  vertexTuples = ApplyTuples[BundleVertex, {baseVertices, fiberVertices}];
  vertices = BundleVertex @@@ vertexTuples;
  edges = Flatten @ {
    MapIndexed[fiberEdge0, Outer[List, baseEdges, fiberVertices], {2}],
    MapIndexed[fiberEdge1, Outer[List, fiberEdges, baseVertices], {2}],
    MapIndexed[fiberEdge2, Outer[List, baseEdges, fiberEdges], {2}]
  };
  If[layoutDimension === 3,
    If[Length[First[baseCoords]] == 2, baseCoords //= Map[Append[0]]];
    If[Length[First[fiberCoords]] == 2, fiberCoords //= Map[Prepend[0]]];
  ];
  fColors = LookupCardinalColors[fiberGraph];
  bColors = LookupCardinalColors[baseGraph];
  cardinalColorRules = Join[
    KeyValueMap[BundleCardinal[None, #1] -> #2&, fColors],
    KeyValueMap[BundleCardinal[#1, None] -> #2&, bColors],
    {_ -> GrayLevel[0.3, 0.2]}
  ];
  bundleGraph = ExtendedGraph[vertices, edges,
    FilterOptions[opts],
    CardinalColorRules -> cardinalColorRules,
    VertexCoordinateFunction -> BundleVertexCoordinateFunction[baseCoords, fiberCoords, fiberScale],
    VisibleCardinals -> BundleCardinal[None, _] | BundleCardinal[_, None],
    GraphTheme -> "BundleGraph"
  ];
  KeyDropFrom[$BundleGraphCache, Hash @ bundleGraph];
  getBundleGraphData[bundleGraph, baseGraph, fiberGraph, sectionDisplayMethod];
  bundleGraph
];

fiberEdge0[{DirectedEdge[bv1_, bv2_], fv_}, {i_, _}] :=
  DirectedEdge[BundleVertex[bv1, fv], BundleVertex[bv2, fv], BundleCardinal[i, None]];

fiberEdge0[{DirectedEdge[bv1_, bv2_, bt_], fv_}, _] :=
  DirectedEdge[BundleVertex[bv1, fv], BundleVertex[bv2, fv], BundleCardinal[bt, None]];

fiberEdge1[{DirectedEdge[fv1_, fv2_], bv_}, {i_, _}] :=
  DirectedEdge[BundleVertex[bv1, fv], BundleVertex[bv2, fv], BundleCardinal[i, None]];

fiberEdge1[{DirectedEdge[fv1_, fv2_, ft_], bv_}, _] :=
  DirectedEdge[BundleVertex[bv, fv1], BundleVertex[bv, fv2], BundleCardinal[None, ft]];

fiberEdge2[{DirectedEdge[bv1_, bv2_], DirectedEdge[fv1_, fv2_]}, {i_, j_}] := {
  DirectedEdge[BundleVertex[bv1, fv1], BundleVertex[bv2, fv2], BundleCardinal[i, j]],
  DirectedEdge[BundleVertex[bv1, fv2], BundleVertex[bv2, fv1], BundleCardinal[i, Inverted @ j]]
};

fiberEdge2[{DirectedEdge[bv1_, bv2_, bt_], DirectedEdge[fv1_, fv2_, ft_]}, _] := {
  DirectedEdge[BundleVertex[bv1, fv1], BundleVertex[bv2, fv2], BundleCardinal[bt, ft]],
  DirectedEdge[BundleVertex[bv1, fv2], BundleVertex[bv2, fv1], BundleCardinal[bt, Inverted @ ft]]
};

PublicFunction[BundleVertexCoordinateFunction]

BundleVertexCoordinateFunction[bc_, fc_, scale_][BundleVertex[b_, f_]] :=
  Lookup[bc, b] + (scale * Lookup[fc, f])
  
(**************************************************************************************************)

DefineGraphTheme["BundleGraph",
  VertexSize -> 6,
  ImagePadding -> 10,
  ImageSize -> ("Edge" -> 50),
  ArrowheadPosition -> 0.75,
  EdgeSetback -> 0.15,
  ArrowheadShape -> None,
  EdgeColorFunction -> "Cardinal", EdgeStyle -> Opacity[1],
  VertexStyle -> GrayLevel[0.3, 1],
  ViewOptions -> {"ShrinkWrap" -> True}
];

