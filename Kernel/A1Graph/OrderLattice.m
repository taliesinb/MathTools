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

PublicHead[PartitionNode]

PartitionNode::usage = "PartitionNode[partition, support]";
zFormat[PartitionNode[support_List, partition_]] :=
	"PartitionNode"[support, Skeleton[Len[partition]]];

getSupport[PartitionNode[support_, _]] := support;
getPartition[PartitionNode[_, partition_]] := partition;

(**************************************************************************************************)

PublicFunction[PartitionLattice]

Options[PartitionLattice] = JoinOptions[
	CombineMultiedges -> True,
	ExtendedGraph
];

PartitionLattice[minimalPartitions_, userOpts:OptionsPattern[]] := Scope[
	mpRange = Range @ Len @ minimalPartitions;
	$dsIndex = $pairIndex = UAssoc[];
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
	Scan[addTermToIndex[F @ support], part];
)

addTermToIndex[support_][term_] := Scan[
	pair |-> KAppTo[$pairIndex, pair, support],
	UnorderedPairs @ term
];

DefineGraphTheme["PartitionLattice",
	VertexLayout -> TreeVertexLayout[],
	VertexTooltips -> "Name",
	ArrowheadPosition -> Spread[0.5,.1],
	ArrowheadSize -> 15
];

nodeSucessors[node_PartitionNode] := Scope[
	support = getSupport @ node;
	$proto = $dsIndex[support]; $node = node;
	remainingMps = Comp[mpRange, support];
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
	impliedMps = Comp[Union @@ Lookup[$pairIndex, newPairs, Nothing], newSupport];
	expandStep[PartitionNode[newSupport, newPartition], impliedMps]
];

findNewPairs[newPartition_, oldPartition_] := Comp[
	JoinMap[UnorderedPairs, Comp[newPartition, oldPartition]],
	JoinMap[UnorderedPairs, Comp[oldPartition, newPartition]]
];

(**************************************************************************************************)

PublicFunction[MeetSemilatticeGraph]

Options[MeetSemilatticeGraph] = JoinOptions[
	ExtendedGraph
];

SetsIntersection[{}] := {};
SetsIntersection[list_List] := Inter @@ list;

MeetSemilatticeGraph[elements_, userOpts:OptionsPattern[]] := Scope[
	head = Part[elements, 1, 0];
	subsets = Sort[Sort /@ (List @@@ elements)];
	all = Union @ Map[SetsIntersection] @ Subsets[subsets];
	range = Range @ Len @ all;
	edges = TransitiveReductionGraph @ RelationGraph[SubsetQ[Part[all, #1], Part[all, #2]]&, range];
	IndexedExtendedGraph[
		head @@@ all, EdgeList @ edges,
		GraphTheme -> "Poset",
		FilterOptions @ userOpts
	]
];

DefineGraphTheme["Poset",
	VertexLayout -> TreeVertexLayout[Balanced -> True],
	VertexLabels -> "Name",
	VertexSize -> 5,
	ArrowheadShape -> None
];

(**************************************************************************************************)

PublicObject[Poset]

VertexOutComponentLists[graph_] := AssocMap[
	VertexOutComponent[graph, {#}]&,
	VertexList @ graph
];

EmptyIndex[keys_] := ConstantAssociation[keys, {}];

makePoset[up_Assoc] := makePoset[up, InvertIndex @ up];

makePoset[up_Assoc, dn_Assoc] := Scope[
	set = Union[Keys @ up, Keys @ dn];
	{up, dn} = KUnion[{up, dn}, {}&];
	upRules = FlattenIndex @ up;
	dnRules = FlattenIndex @ dn;
	botSet = Cases[Normal @ dn, Rule[k_, {}] :> k];
	topSet = Cases[Normal @ up, Rule[k_, {}] :> k];
	Poset @ Assoc[
		"Set" -> set,
		"Up" -> up, "Dn" -> dn,
		"UpSet" -> VertexOutComponentLists[Graph @ upRules],
		"DnSet" -> VertexOutComponentLists[Graph @ dnRules],
		"UpRules" -> upRules, "DnRules" -> dnRules,
		"Bot" -> If[SingletonQ[botSet], F @ botSet, None],
		"Top" -> If[SingletonQ[topSet], F @ topSet, None],
		"BotSet" -> botSet,
		"TopSet" -> topSet
	]
];

SingletonQ = Case[
	{e_} := True;
	_ := False
];

(**************************************************************************************************)

PublicFunction[GraphPoset]

GraphPoset[graph_] := makePoset[VertexOutAssociation @ TransitiveReductionGraph @ graph]

(**************************************************************************************************)

PublicFunction[RelationPoset, InverseRelationPoset]

RelationPoset[relation_, list_List] := GraphPoset @ RelationGraph[relation, list];

InverseRelationPoset[relation_, list_List] := ReversePoset @ RelationPoset[relation, list];

(**************************************************************************************************)

PublicFunction[ReversePoset]

ReversePoset[Poset[assoc_]] := Scope[
	UnpackAssociation[assoc, set, up, upSet, dn, dnSet, upRules, dnRules, bot, top, botSet, topSet];
	Poset @ Assoc[
		"Set" -> set,
		"Up" -> dn, "Dn" -> up,
		"UpSet" -> dnSet, "DnSet" -> upSet,
		"UpRules" -> dnRules, "DnRules" -> upRules,
		"Bot" -> top, "Top" -> bot,
		"BotSet" -> topSet,
		"TopSet" -> botSet
	]
];

(**************************************************************************************************)

PublicFunction[SubsetPoset]

SubsetPoset[gens_Str] :=
	SubsetPoset @ SSplit[gens];

SubsetPoset[gens_List] :=
	GraphPoset @ MeetSemilatticeGraph @ Map[parseGen, gens];

parseGen = Case[
	list_List := list;
	s_Str := Chars[s];
]

(**************************************************************************************************)

PublicFunction[DiscretePoset]

DiscretePoset[list_List] := Scope[
	null = EmptyIndex[list];
	Poset[null, null]
];

(**************************************************************************************************)

PublicFunction[PosetGraph]

PosetGraph[Poset[assoc_Assoc]] := Scope[
	ExtendedGraph[
		assoc["Set"],
		Decases[z_ -> z_] @ assoc["DnRules"],
		GraphTheme -> "Poset"
	]
];

(**************************************************************************************************)

PublicFunction[PosetQ]

PosetQ = Case[
	Poset[_Assoc] := True;
	_ := False;
]

declareBoxFormatting[
	p:Poset[_Assoc] :> formatPoset[p]
];

formatPoset[p_Poset ? PosetQ, opts___Rule] := ToBoxes @ ExtendedGraphPlot[
	PosetGraph @ p,
	opts,
	FrameStyle -> LightGray, Frame -> True, ImagePadding -> {Left -> 10, Right -> 10, Top -> 15, Bottom -> 10},
	VertexLabels -> "Name" -> formatPosetElement,
	ImageSize -> ("ShortestEdge" -> 25)
];

formatPoset[_] := $Failed;

formatPosetElement[e_List] := Row[e];
formatPosetElement[e_] := e;

(**************************************************************************************************)

PublicFunction[PosetField]

declareBoxFormatting[
	pf:PosetField[_Poset, _List] :> formatPosetField[pf]
];

formatPosetField[PosetField[p_Poset, v_List]] :=
	formatPoset[p, VertexShapeFunction -> "Index" -> fieldPart[v],
		VertexStyle -> Black, VertexLabels -> None]

fieldPart[field_][i_] := Rep[Part[field, i], 0|0. -> ""];

