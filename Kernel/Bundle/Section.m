PublicObject[BundleSection]

BundleSection::usage := "BundleSection[<|b$1 -> f$1, b$2 -> f$2, $$|>, hash$] represents a section of a bundle graph with hash hash$."

declareBoxFormatting[
  bs:BundleSection[_Assoc, _Int] :> Construct[InterpretationBox, bundleSectionBoxes[bs], bs]
];

bundleSectionBoxes[bs:BundleSection[_, hash_Int]] := Scope[
  method = bundleHashLookup[hash, "SectionDisplayMethod"];
  If[method === Inherited, method = $BundleSectionDisplayMethod];
  Switch[
    method,
    "Skeleton",                      RowBox[{"BundleSection", "[", "\[LeftAngleBracket]", TextString @ Len @ P1 @ bs, "\[RightAngleBracket]", "]"}],
    $bundleSectionPlotMethodPattern, ToBoxes @ BundleSectionPlot[bs, Method -> method],
    None | _,                        RowBox[{"BundleSection", "[", ToBoxes @ P1 @ bs, "]"}]
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
      Message[$BundleSectionDisplayMethod::badval, Cases[$bundleSectionDisplayMethodPattern, _Str | _Symbol]]; $Failed
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

CacheVariable[$AllBundleSectionsCache]

Options[BundleSectionComponents] = {
  FiberSymmetries -> None
};

FindAllBundleSections[bundle_Graph, opts:OptionsPattern[]] := FindAllBundleSections[bundle, All, opts];

FindAllBundleSections[bundle_Graph, n:Except[_Rule], opts:OptionsPattern[]] := Scope[
  If[!BundleGraphQ[bundle], ReturnFailed["notbundle"]];

  hash = Hash @ bundle; key = hash[opts];
  allSections = Lookup[$AllBundleSectionsCache, key, None];
  If[allSections =!= None, Goto[Skip]];

  bundleData = getBundleGraphData[bundle];
  UnpackAssociation[bundleData, hash, fiberGroups, baseVertices];

  b = FF @ VertexList @ bundle;
  fs = fiberGroups[b];
  init = Map[f |-> BundleSection[<|b -> f|>, hash], fs];

  allSections = RewriteStates[BundleSectionExtensionSystem[bundle, opts], init];

  bn = Len @ baseVertices;
  allSections //= Select[Len[P1[#]] === bn&];
  allSections //= Sort;

  $AllBundleSectionsCache[key] ^= allSections;

  Label[Skip];
  safeRandomSample[n] @ allSections
];


(**************************************************************************************************)

PublicFunction[BundleSectionComponents]

Options[BundleSectionComponents] = {
  FiberSymmetries -> None
};

BundleSectionComponents[bundle_Graph, opts:OptionsPattern[]] := BundleSectionComponents[bundle, All, opts];

BundleSectionComponents[bundle_Graph, n:Except[_Rule], opts:OptionsPattern[]] := Scope[
  graph = RewriteGraph[BundleSectionHomotopySystem[bundle, opts], All];
  components = WeaklyConnectedComponents[graph];
  Map[safeRandomSample[n], components]
]

safeRandomSample[All] = Id;
safeRandomSample[n_][list_List] := RandomSample[list, UpTo @ n];

(**************************************************************************************************)

PublicFunction[FindConstantBundleSections]

FindConstantBundleSections[bundle_Graph] := Scope[
  If[!BundleGraphQ[bundle], ReturnFailed["notbundle"]];

  UnpackAssociation[getBundleGraphData[bundle], hash, horizontalFoliation];
  
  components = WeaklyConnectedComponents[horizontalFoliation];

  BundleSection[vertexListToSection[#], hash]& /@ components
];

vertexListToSection[vertexList_List] := Assoc[Rule @@@ vertexList];
vertexListToSection[subgraph_Graph] := vertexListToSection @ VertexList @ subgraph;

(**************************************************************************************************)

PublicFunction[SectionOrbit]

SectionOrbit[BundleSection[sec_Assoc, hash_Int], group_] := Scope[
  keys = Keys @ sec;
  sectionLists = toSectionOrbitLists[Values @ sec, hash, group];
  Map[section |-> BundleSection[AssociationThread[keys, section], hash], sectionLists]
];

SectionOrbit[group_][section_] := SectionOrbit[section, group];

toSectionOrbitLists[sectionValues_, hash_, group_] := Scope[
  vertexIndex = bundleHashLookup[hash, "FiberVertexIndex"];
  PermutationReplace[Lookup[vertexIndex, sectionValues], group]
];

(**************************************************************************************************)

PublicFunction[SectionOrbitRepresentative]

SectionOrbitRepresentative[BundleSection[sec_Assoc, hash_Int], group_] :=
  BundleSection[AssociationThread[Keys @ sec, Minimum @ toSectionOrbitLists[Values @ sec, hash, group]], hash]

SectionOrbitRepresentative[group_][sec_] := SectionOrbitRepresentative[sec, group];
