PublicFunction[BundleSectionRewritingSystem]

BundleSectionRewritingSystem::arg1 = "First argument should be a bundle graph."

BundleSectionRewritingSystem[graph_] := Scope[
  If[!BundleGraphQ[graph], ReturnFailed["arg1"]];
  props = <|"BundleGraph" -> graph|>;
  constructRewritingSystem["BundleSectionRewriting", Null, "CustomProperties" -> props]
]

_BundleSectionRewritingSystem := (Message[BundleSectionRewritingSystem::args]; $Failed);

declareRewritingSystemDispatch["BundleSectionRewriting", BundleSectionRewritingSystemProperty]

BundleSectionRewritingSystemProperty[data_, "CayleyFunction", opts___Rule] :=
  BundleSectionRewritingCayleyFunction; (* why not prefill to make this internal use faster *)

(**************************************************************************************************)

PublicFunction[BundleSectionRewritingCayleyFunction]

(* TODO: Cache this *)
BundleSectionRewritingCayleyFunction[BundleSection[sec_, hash_]] := Scope[
  UnpackAssociation[Lookup[QuiverGeometryLoader`$BundleGraphCache, hash], verticalAdjacency, baseAdjacency, areBundleAdjacent];
  Flatten @ KeyValueMap[
    {b, f} |-> (
      v = BundleVertex[b, f];
      (* get fiber nbors *)
      fnbs = verticalAdjacency[v];
      (* get base nbors *)
      bnbs = baseAdjacency[b];
      fnbs //= Select[fnb |-> AllTrue[bnbs, bnb |-> areBundleAdjacent[{fnb, BundleVertex[bnb, sec @ bnb]}]]];
      BundleSection[ReplacePart[sec, b -> Last[#]], hash]& /@ fnbs
    ),
    sec
  ]
];

(**************************************************************************************************)

PublicFunction[BundleGraphQ]

General::notbundle = "Input is not a bundle graph."
BundleGraphQ[g_] := GraphQ[g] && VertexCount[g, BundleVertex[_, _]] === VertexCount[g];

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

PublicFunction[FindAllBundleSections]

FindAllBundleSections[bundle_Graph] := Scope[
  If[!BundleGraphQ[bundle], ReturnFailed["notbundle"]];
  UnpackAssociation[getBundleGraphData[bundle], bundleVertices, $baseAdjacency, $areBundleAdjacent, hash];
  fiberGroups = GroupBy[bundleVertices, First -> Last];
  {base, fibers} = KeysValues @ fiberGroups;
  allSections = MapTuples[AssociationThread[base, #]&, fibers];
  BundleSection[#, hash]& /@ Select[allSections, validSectionQ]
];

validSectionQ[section_] := Module[{v, bnbs},
  KeyValueScan[
    {b, f} |-> (
      v = BundleVertex[b, f];
      bnbs = $baseAdjacency[b];
      bnbs = Thread @ BundleVertex[bnbs, Lookup[section, bnbs]];
      If[!AllTrue[bnbs, $areBundleAdjacent[{v, #}]&], Return[False, Module]];
    ),
    section
  ];
  True
]

(**************************************************************************************************)

(* PublicHead[ValidBundleSectionQ]

(* TODO: build this into the constructor of BundleSection, when given a second argument that is a graph *)
ValidBundleSectionQ[bundle_Graph, BundleSection[section_Association]] := Scope[
  UnpackAssociation[BundleGraphData[bundle], $baseVertices, $baseAdjacency, $areBundleAdjacent];
  If[!setEqualQ[Keys @ section, $baseVertices], Return[False]];
  validSectionQ @ section
];
 *)
(**************************************************************************************************)


setEqualQ[a_, b_] := Sort[a] === Sort[b];

(**************************************************************************************************)

PublicHead[BundleSection]

BundleSection::usage := "BundleSection[<|b$1 -> f$1, b$2 -> f$2, $$|>, hash$] represents a section of a bundle graph with hash hash$."

declareBoxFormatting[
  bs:BundleSection[_Association, _Integer] :> bundleSectionBoxes[bs],
  BundleVertex[b_, f_] :> SuperscriptBox[ToBoxes @ Style[f, $Red], ToBoxes @ Style[b, $Blue]]
];

bundleSectionBoxes[bs_BundleSection] := With[{
  plots = BundleSectionPlot[bs, Method -> #]& /@ {"Array", "Color", "Total"},
  index = Replace[$BundleSectionDisplayMethod, {"Array" -> 1, "Color" -> 2, "Total" -> 3, _ -> 1}]},
  ToBoxes @ Interpretation[FlipView[plots, index], bs]
];
  
PublicVariable[$BundleSectionDisplayMethod]

$BundleSectionDisplayMethod = "Total";

(**************************************************************************************************)

PublicFunction[BundleSectionPlot]

Options[BundleSectionPlot] = {
  Method -> "Color",
  ImageSize -> 100,
  $ExtendedGraphOptions
};

BundleSectionPlot::method = "Method should be one of ``";

BundleSectionPlot[expr_, OptionsPattern[]] := Scope[

  UnpackOptions[method, $imageSize];
  plotter = Switch[method,
    "Color", BundleSectionPlotColor,
    "Total", BundleSectionPlotTotal,
    "Line",  BundleSectionPlotLine,
    "Array", BundleSectionPlotArray,
    _,       ReturnFailed["method", {"Color","Total","Array","Line"}]
  ];
  ReplaceAll[
    expr,
    sec:BundleSection[_Association, _Integer] :> RuleCondition @ plotter @ sec
  ]
];

BundleSectionPlotColor[BundleSection[sec_Association, hash_]] :=
  ExtendedGraphPlot[
    hashBaseGraph[hash],
    VertexColorFunction -> (sec /* hashColorFunc[hash]),
    GraphTheme -> "BundleIconGraph",
    ImageSize -> $imageSize
  ];

BundleSectionPlotTotal[BundleSection[sec_Association, hash_]] :=
  ExtendedGraphPlot[
    hashBundleGraph[hash],
    RegionColorRules -> {ConnectedSubgraph[Map[Point, bundleSectionVertices[sec]]] -> $Red, All -> $LightGray},
    GraphTheme -> "BundleIconGraph",
    LayoutDimension -> 2,
    ImageSize -> $imageSize
  ];

BundleSectionPlotLine[BundleSection[sec_Association, hash_]] :=
  ExtendedGraphPlot[
    hashBundleGraph[hash],
    RegionColorRules -> {ConnectedSubgraph[Map[Point, bundleSectionVertices[sec]]] -> $Red, All -> Transparent},
    VertexSize -> 5, ImagePadding -> 10,
    GraphTheme -> "BundleIconGraph",
    LayoutDimension -> 2,
    ImageSize -> $imageSize
  ];

BundleSectionPlotArray[BundleSection[sec_Association, hash_]] := Scope[
  bn = VertexCount @ hashBaseGraph[hash]; fn = (VertexCount @ hashBundleGraph[hash]) / bn;
  vals = Lookup[sec, hashBaseVerts[hash]];
  cols = ToRGB @ hashColorFunc[hash] @ vals;
  If[bn > 8 && IntegerQ @ Sqrt[bn],
    FadedMeshImage[Partition[cols, Sqrt[bn]], 4],
    FadedMeshImage[List @ cols, 6]
  ]
];

hashBundleGraph[hash_] := QuiverGeometryLoader`$BundleGraphCache[hash, "BundleGraph"];
hashBaseGraph[hash_] := QuiverGeometryLoader`$BundleGraphCache[hash, "BaseGraph"];
hashBaseVerts[hash_] := QuiverGeometryLoader`$BundleGraphCache[hash, "BaseVertices"];
hashColorFunc[hash_] := QuiverGeometryLoader`$BundleGraphCache[hash, "FiberVertexColorFunction"];

(**************************************************************************************************)

(* BundleSection[sec_Association][prop_String] := BundleSectionProperty[sec, prop];

BundleSectionProperty::noprop = "Unknown property ``.";
BundleSectionProperty[BundleSection[sec_Association] | sec_Association, prop_String] := Switch[
  prop,
  "BaseVertices", bundleSectionBaseVertices[sec],
  "Vertices",     bundleSectionVertices[sec],
  _,              (Message[BundleSectionProperty::noprop, prop]; $Failed)
]; *)

bundleSectionBaseVertices[assoc_] := Keys @ assoc;
bundleSectionVertices[assoc_] := KeyValueMap[BundleVertex, assoc];

(**************************************************************************************************)

$BundleIconGraphThemeRules = {
  AspectRatioClipping -> False,
  ArrowheadSize -> 0,
  VertexSize -> 8,
  Frame -> True,
  ImagePadding -> 10,
  EdgeLength -> 20
};

$GraphThemeData["BundleIconGraph"] := $BundleIconGraphThemeRules;

(**************************************************************************************************)

PublicHead[BundleVertex, BundleCardinal]

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

(**************************************************************************************************)

PublicFunction[ClearBundleData]

ClearBundleData[] := QuiverGeometryLoader`$BundleGraphCache = Data`UnorderedAssociation[];

(**************************************************************************************************)

PublicFunction[BundleData]

BundleData[bundleGraph_Graph, key_:All] := Scope[
  data = getBundleGraphData[bundleGraph];
  If[!AssociationQ[data], ReturnFailed[]];
  If[key === All, data, Lookup[data, key]]
];

(**************************************************************************************************)

getBundleGraphData[bundleGraph_, baseGraph_:Automatic, fiberGraph_:None] := Scope[
  
  hash = Hash[bundleGraph];
  If[AssociationQ[cachedValue = QuiverGeometryLoader`$BundleGraphCache[hash]],
    Return @ cachedValue];

  bundleVertices = VertexList @ bundleGraph;
  bundleCoordinates = LookupVertexCoordinates @ bundleGraph;

  SetAutomatic[baseGraph, BundleToBaseGraph @ bundleGraph];
  baseVertices = VertexList @ baseGraph;
  baseCoordinates = LookupVertexCoordinates @ baseGraph;

  fiberVertices = If[fiberGraph =!= None, VertexList @ fiberGraph, Union @ LastColumn @ VertexList @ bundleGraph];
  fiberVertexColorFunction = DiscreteColorFunction[fiberVertices, Automatic];

  baseAdjacency = VertexAdjacencyAssociation @ baseGraph;
  taggedAdj = VertexTagAdjacencyAssociation @ bundleGraph;
  verticalAdjacency = joinAdjacencyAssociationsMatching[taggedAdj, BundleCardinal[None, _]];
  horizontalAdjacency = joinAdjacencyAssociationsMatching[taggedAdj, BundleCardinal[_, None]];

  areBundleAdjacent = AdjacentVerticesPredicate @ bundleGraph;
  areBaseAdjacent = AdjacentVerticesPredicate @ baseGraph;

  data = Association[
    "Hash" -> hash,
    "BundleGraph" -> bundleGraph,
    "BundleVertices" -> bundleVertices,
    "BaseGraph" -> baseGraph,
    "BaseVertices" -> baseVertices,
    "FiberGraph" -> fiberGraph,
    "FiberVertices" -> fiberVertices,
    "BaseAdjacency" -> baseAdjacency,
    "VerticalAdjacency" -> verticalAdjacency,
    "HorizontalAdjacency" -> horizontalAdjacency,
    "AreBundleAdjacent" -> areBundleAdjacent,
    "AreBaseAdjacent" -> areBaseAdjacent,
    "FiberVertexColorFunction" -> fiberVertexColorFunction
  ];

  AssociateTo[QuiverGeometryLoader`$BundleGraphCache, hash -> data];

  data
];

joinAdjacencyAssociationsMatching[assocs_, pattern_] := Merge[Values @ KeySelect[assocs, MatchQ[pattern]], Catenate];

If[!AssociationQ[QuiverGeometryLoader`$BundleGraphCache], QuiverGeometryLoader`$BundleGraphCache = Data`UnorderedAssociation[]];
