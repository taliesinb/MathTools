PublicFunction[ClearBundleData]

ClearBundleData[] := QuiverGeometryLoader`$BundleGraphCache = Data`UnorderedAssociation[];

If[!AssociationQ[QuiverGeometryLoader`$BundleGraphCache],
QuiverGeometryLoader`$BundleGraphCache = Data`UnorderedAssociation[]
];

(**************************************************************************************************)

PrivateFunction[bundleHashLookup]

bundleHashLookup[hash_, prop___] := QuiverGeometryLoader`$BundleGraphCache[hash, prop];

(**************************************************************************************************)

PublicFunction[BundleData]

BundleData[bundleGraph_Graph, key_:All] := Scope[
  data = getBundleGraphData[bundleGraph];
  If[!AssociationQ[data], ReturnFailed[]];
  If[key === All, data, Lookup[data, key]]
];

(**************************************************************************************************)

PrivateFunction[getBundleGraphData]

getBundleGraphData[bundleGraph_, baseGraph_:Automatic, fiberGraph_:None] := Scope[
  
  hash = Hash[bundleGraph];
  If[AssociationQ[cachedValue = bundleHashLookup[hash]],
    Return @ cachedValue];

  bundleVertices = VertexList @ bundleGraph;
  bundleEdges = EdgeList @ bundleGraph;
  bundleCoordinates = LookupVertexCoordinates @ bundleGraph;

  SetAutomatic[baseGraph, BundleToBaseGraph @ bundleGraph];
  baseVertices = VertexList @ baseGraph;
  baseCoordinates = LookupVertexCoordinates @ baseGraph;

  fiberVertices = If[fiberGraph =!= None, VertexList @ fiberGraph, Union @ LastColumn @ VertexList @ bundleGraph];
  fiberVertexColorFunction = DiscreteColorFunction[fiberVertices, Automatic];
  fiberGroups = GroupBy[bundleVertices, First -> Last];

  baseAdjacency = VertexAdjacencyAssociation @ baseGraph;
  taggedAdj = VertexTagAdjacencyAssociation @ bundleGraph;
  verticalAdjacency = joinAdjacencyAssociationsMatching[taggedAdj, BundleCardinal[None, _]];
  horizontalAdjacency = joinAdjacencyAssociationsMatching[taggedAdj, BundleCardinal[_, None]];

  areBundleAdjacent = AdjacentVerticesPredicate @ bundleGraph;
  areBaseAdjacent = AdjacentVerticesPredicate @ baseGraph;

  horizontalFoliation = CardinalSubquiver[bundleGraph, BundleCardinal[_, None]];
  verticalFoliation = CardinalSubquiver[bundleGraph, BundleCardinal[None, _]];

  data = Association[
    "Hash" -> hash,
    "BundleGraph" -> bundleGraph,
    "BundleVertices" -> bundleVertices,
    "BaseGraph" -> baseGraph,
    "BaseVertices" -> baseVertices,
    "FiberGraph" -> fiberGraph,
    "FiberVertices" -> fiberVertices,
    "FiberGroups" -> fiberGroups,
    "BaseAdjacency" -> baseAdjacency,
    "VerticalAdjacency" -> verticalAdjacency,
    "HorizontalAdjacency" -> horizontalAdjacency,
    "HorizontalFoliation" -> horizontalFoliation,
    "VerticalFoliation" -> verticalFoliation,
    "AreBundleAdjacent" -> areBundleAdjacent,
    "AreBaseAdjacent" -> areBaseAdjacent,
    "FiberVertexColorFunction" -> fiberVertexColorFunction
  ];

  AssociateTo[QuiverGeometryLoader`$BundleGraphCache, hash -> data];

  data
];

joinAdjacencyAssociationsMatching[assocs_, pattern_] := Merge[Values @ KeySelect[assocs, MatchQ[pattern]], Catenate];

