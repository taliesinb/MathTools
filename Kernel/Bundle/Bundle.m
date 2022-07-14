(**************************************************************************************************)

PublicFunction[BundleGraphQ]

General::notbundle = "Input is not a bundle graph."
BundleGraphQ[g_] := GraphQ[g] && VertexCount[g, BundleVertex[_, _]] === VertexCount[g];

(**************************************************************************************************)

PublicHead[BundleVertex, BundleCardinal]

declareBoxFormatting[
  BundleVertex[b_, f_] :> makeColonPair[b, f],
  BundleCardinal[b_, f_] :> makeColonPair[b, f]
];

BundleCardinal /: Inverted[c_BundleCardinal] := Map[Inverted, c];

PrivateFunction[makeColonPair]

makeColonPair[a_, b_] := ToBoxes @ Row[{a, GrayForm @ ":", b}];

(**************************************************************************************************)

PublicFunction[BundleToBaseGraph]

BundleToBaseGraph[bundle_Graph] := Scope[
  If[!BundleGraphQ[bundle], ReturnFailed["notbundle"]];
  {vertices, edges} = VertexEdgeList[bundle];
  baseVertices = DeleteDuplicates @ FirstColumn @ vertices;
  baseEdges = UniqueCases[edges, head_[BundleVertex[b0_, _],  BundleVertex[b1_, _], BundleCardinal[bt_, _]] /; b0 =!= b1 :> head[b0, b1, bt]];
  vcf = LookupExtendedOption[bundle, VertexCoordinateFunction];
  baseVCF = If[MatchQ[vcf, BundleVertexCoordinateFunction[_, _, _]], Map[TakeOperator[2], First @ vcf], Automatic];
  ExtendedGraph[baseVertices, baseEdges,
    VertexCoordinateFunction -> baseVCF,
    GraphTheme -> LookupExtendedOption[bundle, GraphTheme]
  ]
];

(**************************************************************************************************)

PublicFunction[TrivialBundleGraph]

PublicOption[FiberScale, FiberCoordinateRotation, BaseCoordinateRotation]

Options[TrivialBundleGraph] = JoinOptions[
  FiberScale -> 1,
  FiberCoordinateRotation -> Pi/2,
  BaseCoordinateRotation -> 0,
  LayoutDimension -> Automatic,
  $ExtendedGraphOptions
];

TrivialBundleGraph[baseGraph_, fiberGraph_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[fiberScale, fiberCoordinateRotation, baseCoordinateRotation, layoutDimension];
  graphs = {baseGraph, fiberGraph};
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
  bundleGraph = ExtendedGraph[vertices, edges,
    FilterOptions[opts],
    CardinalColorRules -> KeyValueMap[BundleCardinal[None, #1] -> #2&, fColors],
    VertexCoordinateFunction -> BundleVertexCoordinateFunction[baseCoords, fiberCoords, fiberScale],
    GraphTheme -> "BundleGraph"
  ];
  getBundleGraphData[bundleGraph, baseGraph, fiberGraph];
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

$BundleGraphThemeRules = {
  VertexSize -> 8,
  ImagePadding -> 10,
  ImageSize -> ("AverageEdge" -> 50),
  ArrowheadPosition -> 0.75
};

$GraphThemeData["BundleGraph"] := $BundleGraphThemeRules;
