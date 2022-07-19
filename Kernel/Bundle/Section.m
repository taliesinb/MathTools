PublicHead[BundleSection]

BundleSection::usage := "BundleSection[<|b$1 -> f$1, b$2 -> f$2, $$|>, hash$] represents a section of a bundle graph with hash hash$."

declareBoxFormatting[
  bs:BundleSection[_Association, _Integer] :> Construct[InterpretationBox, bundleSectionBoxes[bs], bs]
];

bundleSectionBoxes[bs:BundleSection[_, hash_Integer]] := Scope[
  method = bundleHashLookup[hash, "SectionDisplayMethod"];
  If[method === Inherited, method = $BundleSectionDisplayMethod];
  Switch[
    method,
    "Skeleton",                      RowBox[{"BundleSection", "[", "\[LeftAngleBracket]", TextString @ Length @ First @ bs, "\[RightAngleBracket]", "]"}],
    $bundleSectionPlotMethodPattern, ToBoxes @ BundleSectionPlot[bs, Method -> method],
    None | _,                        RowBox[{"BundleSection", "[", ToBoxes @ First @ bs, "]"}]
  ]
];

(**************************************************************************************************)

PublicVariable[$BundleSectionDisplayMethod]
PrivateVariable[$bundleSectionDisplayMethodPattern]

$bundleSectionDisplayMethodPattern = Join[None | "Skeleton", $bundleSectionPlotMethodPattern];

$BundleSectionDisplayMethod = "Base";

$BundleSectionDisplayMethod::badval = "Value should be one of ``."
$outer = True;
$BundleSectionDisplayMethod /: Set[$BundleSectionDisplayMethod, value_] /; $outer === True :=
    If[MatchQ[value, $bundleSectionDisplayMethodPattern],
      Block[{$outer}, Set[$BundleSectionDisplayMethod, value]],
      Message[$BundleSectionDisplayMethod::badval, Cases[$bundleSectionDisplayMethodPattern, _String | _Symbol]]; $Failed
    ];

(**************************************************************************************************)

BundleSection[assoc_, graph_Graph] := constructSectionManually[assoc, graph];

BundleSection::notsection = "Provided association does not represent a section of the bundle graph.";
constructSectionManually[assoc_, bundle_Graph] := Scope[
  If[!BundleGraphQ[bundle], ReturnFailed[BundleSection::notbundle]];
  UnpackAssociation[getBundleGraphData[bundle], baseVertices, $baseAdjacency, $areBundleAdjacent, hash];

  If[assoc === <||> || !SubsetQ[baseVertices, Keys @ assoc] || !validSectionQ[assoc], ReturnFailed[BundleSection::notsection]];

  BundleSection[assoc, hash]
];

(**************************************************************************************************)

(* faster potential method:
'decimate' the total graph into supervertices that consist of little neighborhoods.
solve the problem for each neighborhood, you'll get a set of germs essentially, so a stalk.
then create another graph whose base vertices are the neighborhoods and fiber is the stalk, with
connection being when two neighboring germs are connected on their edge in the total graph.
then solve that problem. a section of this decimated graph is a then a section of original graph,
when we undecimate. so we don't compress in terms of the number of sections.
*)

PublicFunction[FindAllBundleSections]

$AllBundleSectionsCache = UAssociation[];

FindAllBundleSections[bundle_Graph, n_:All] := Scope[
  If[!BundleGraphQ[bundle], ReturnFailed["notbundle"]];

  bundleData = getBundleGraphData[bundle];
  allSections = Lookup[$AllBundleSectionsCache, bundleData["Hash"], None];
  If[allSections =!= None, Goto[Skip]];
  
  UnpackAssociation[getBundleGraphData[bundle], fiberGroups, $baseAdjacency, $areBundleAdjacent, hash];
  {base, fibers} = KeysValues @ fiberGroups;
  
  allSections = MapTuples[AssociationThread[base, #]&, fibers];
  allSections = BundleSection[#, hash]& /@ Select[allSections, validSectionQ];

  $AllBundleSectionsCache[hash] ^= allSections;

  Label[Skip];
  safeRandomSample[n] @ allSections
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

PublicFunction[BundleSectionComponents]

BundleSectionComponents[bundle_Graph, n_:All] := Scope[
  secs = FindAllBundleSections @ bundle;
  graph = RewriteGraph[BundleSectionRewritingSystem @ bundle, secs];
  components = ConnectedComponents[graph];
  Map[safeRandomSample[n], components]
]

safeRandomSample[All] = Identity;
safeRandomSample[n_][list_List] := RandomSample[list, UpTo @ n];

(**************************************************************************************************)

PublicFunction[FindConstantBundleSections]

FindConstantBundleSections[bundle_Graph] := Scope[
  If[!BundleGraphQ[bundle], ReturnFailed["notbundle"]];

  UnpackAssociation[getBundleGraphData[bundle], hash, horizontalFoliation];
  
  components = WeaklyConnectedComponents[horizontalFoliation];

  BundleSection[vertexListToSection[#], hash]& /@ components
];

vertexListToSection[vertexList_List] := Association[Rule @@@ vertexList];
vertexListToSection[subgraph_Graph] := vertexListToSection @ VertexList @ subgraph;
