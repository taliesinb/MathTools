DSCreate[partition_] := Scope[
	ds = CreateDataStructure["DisjointSet"];
	Scan[DSAddClique[ds, #]&, partition];
	ds
];

DSAdd[ds_, a_] := ds["Insert", a];
DSAdd[ds_, a_, b_] := (ds["Insert", b]; ds["Unify", a, b]);

DSAddClique[ds_, {a_}] := DSAdd[ds, a];
DSAddClique[ds_, {a_, b_}] := (DSAdd[ds, a]; DSAdd[ds, a, b]);
DSAddClique[ds_, {a_, rest__}] := (DSAdd[ds, a]; Scan[DSAdd[ds, a, #]&, {rest}]);

DSSubsets[ds_] := ds["Subsets"];

DSMerge[ds_, other_] := ds["Merge", other];
DSMergeList[ds_, others_] := Scan[ds["Merge", #]&, others];

DSCopy[ds_] := ds["Copy"];

(**************************************************************************************************)

PackageExport["PartitionNode"]

PartitionNode::usage = "PartitionNode[partition, support]";
zFormat[PartitionNode[support_List, partition_]] :=
	"PartitionNode"[support, Skeleton[Length[partition]]];

getSupport[PartitionNode[support_, _]] := support;
getPartition[PartitionNode[_, partition_]] := partition;

(**************************************************************************************************)

PackageExport["PartitionLattice"]

Options[PartitionLattice] = JoinOptions[
	CombineMultiedges -> True,
	ExtendedGraph
];

PartitionLattice[minimalPartitions_, userOpts:OptionsPattern[]] := Scope[
	mpRange = Range @ Length @ minimalPartitions;
	$dsIndex = $pairIndex = Data`UnorderedAssociation[];
	ScanIndexed[setupPart, minimalPartitions];
	initSupports = List /@ mpRange;
	initNodes = MapThread[PartitionNode, {initSupports, minimalPartitions}];
	rootNode = PartitionNode[{}, {}];
	{vertexList, indexEdgeList} = MultiwaySystem[
		nodeSucessors, initNodes,
		{"VertexList", "IndexEdgeList"},
		PrologVertices -> {rootNode}
	];
	If[OptionValue[CombineMultiedges], indexEdgeList //= CombineForwardMultiedges];
	initialEdges = DirectedEdge[1, # + 1, #]& /@ mpRange;
	indexEdgeList = Join[initialEdges, indexEdgeList];

	IndexedExtendedGraph[
		vertexList, indexEdgeList,
		GraphTheme -> "PartitionLattice",
		FilterOptions @ userOpts
	]
];

setupPart[part_, support_] := (
	$dsIndex[support] = DSCreate[part];
	Scan[addTermToIndex[First @ support], part];
)

addTermToIndex[support_][term_] := Scan[
	pair |-> KeyAppendTo[$pairIndex, pair, support],
	UnorderedPairs @ term
];

$GraphThemeData["PartitionLattice"] = {
	VertexLayout -> TreeLayout[],
	VertexTooltips -> "Name",
	ArrowheadPosition -> Around[0.5,.1],
	ArrowheadSize -> 15
};

nodeSucessors[node_PartitionNode] := Scope[
	support = getSupport @ node;
	$proto = $dsIndex[support]; $node = node;
	remainingMps = Complement[mpRange, support];
	computeSucessor /@ remainingMps
];

computeSucessor[mp_] := Scope[
	$ds = DSCopy @ $proto;
	zPrint["Sucessor of ", $node, " via ", mp];
	newNode = expandStep[$node, List @ mp];
	CacheTo[$dsIndex, getSupport @ newNode, $ds];
	Labeled[newNode, mp]
];

(* expandStep takes a PartitionNode correpsonding to $ds, and a list of supports to add e.g. {{2}, {3}, {9}} *)
expandStep[node_PartitionNode, {}] := node;
expandStep[PartitionNode[oldSupport_, oldPartition_], mpsToAdd_] := Scope[
	DSMergeList[$ds, Lookup[$dsIndex, List /@ mpsToAdd]];
	newPartition = SortContractionSet @ DSSubsets[$ds];
	zPrint["\texpanding by ", mpsToAdd, " yielded partition ", newPartition];
	newPairs = findNewPairs[newPartition, oldPartition];
	zPrint["\tnewPairs = ", newPairs];
	newSupport = Union[oldSupport, mpsToAdd];
	impliedMps = Complement[Union @@ Lookup[$pairIndex, newPairs, Nothing], newSupport];
	expandStep[PartitionNode[newSupport, newPartition], impliedMps]
];

findNewPairs[newPartition_, oldPartition_] := Complement[
	JoinMap[UnorderedPairs, Complement[newPartition, oldPartition]],
	JoinMap[UnorderedPairs, Complement[oldPartition, newPartition]]
];
