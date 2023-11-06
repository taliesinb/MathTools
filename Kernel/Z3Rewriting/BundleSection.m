PublicFunction[BundleSectionExtensionSystem, BundleSectionHomotopySystem]

PublicOption[FiberSymmetries]

Options[BundleSectionExtensionSystem] = Options[BundleSectionHomotopySystem] = {
  FiberSymmetries -> None
};

BundleSectionExtensionSystem[graph_, opts:OptionsPattern[]] := Scope @ CatchMessage[
  UnpackOptions[fiberSymmetries];
  constructBundleSectionSystem["BundleSectionExtension", graph, fiberSymmetries]
];

BundleSectionHomotopySystem[graph_, opts:OptionsPattern[]] := Scope @ CatchMessage[
  UnpackOptions[fiberSymmetries];
  constructBundleSectionSystem["BundleSectionHomotopy", graph, fiberSymmetries]
]

(**************************************************************************************************)

General::bundlearg1 = "First argument should be a bundle graph."
General::fibersym = "Setting of FiberSymmetries -> `` should be a None, All, or a PermutationGroup[...].";

constructBundleSectionSystem[name_, graph_, fiberSymmetries_] := Scope[
  If[!BundleGraphQ[graph], ThrowMessage["bundlearg1"]];
  props = <|"BundleGraph" -> graph, "FiberSymmetries" -> fiberSymmetries|>;
  SetAll[fiberSymmetries, GraphAutomorphismGroup @ BundleToFiberGraph @ graph];
  canonicalizationFunction = If[fiberSymmetries === None, None,
    If[H[fiberSymmetries] =!= PermutationGroup, ThrowMessage["fibersym", fiberSymmetries]];
    canonicalizationFunction = SectionOrbitRepresentative[fiberSymmetries]
  ];
  constructRewritingSystem[name, Null, "CustomProperties" -> props, CanonicalizationFunction -> canonicalizationFunction]
];

(**************************************************************************************************)

declareRewritingSystemDispatch["BundleSectionExtension", bundleSectionExtensionProperty]

bundleSectionExtensionProperty[data_, "CayleyFunction", opts___Rule] := Scope[
  UnpackStringOptions[{opts}, True, labeled];
  If[labeled,
    LabeledSectionExtensionCayleyFunction,
    SectionExtensionCayleyFunction
  ]
];

bundleSectionExtensionProperty[data_, "AllStates"] :=
  FindAllBundleSections[data["BundleGraph"], FiberSymmetries -> data["FiberSymmetries"]];

PublicFunction[LabeledSectionExtensionCayleyFunction, SectionExtensionCayleyFunction]

SectionExtensionCayleyFunction[bs_] := Block[
  {makeLabeledExtendedSection = makeUnlabeledExtendedSection},
  LabeledSectionExtensionCayleyFunction[bs]
];

LabeledSectionExtensionCayleyFunction[BundleSection[sec_, hash_]] := Scope[
  UnpackAssociation[bundleHashLookup @ hash, fiberGroups, baseAdjacency, areBundleAdjacent, areBaseAdjacent];
  keys = Keys @ sec;
  frontier = Complement[Union @@ Lookup[baseAdjacency, keys], keys];
  Flatten @ Map[
    b |-> (
      (* these are periphery of existing section that touch new base point *)
      bnbs = Select[keys, areBaseAdjacent[{b, #}]&];
      fs = Select[
        fiberGroups[b], (* <- these are candidate fiber vertices above new base point *)
        f |-> (
          v = BundleVertex[b, f]; (* is candidate bundle vertex adjacent to all touching section points? *)
          AllTrue[bnbs, bnb |-> areBundleAdjacent[{v, BundleVertex[bnb, sec @ bnb]}]]
        )
      ];
      Map[makeLabeledExtendedSection[sec, hash, b], fs]
    ),
    frontier
  ]
];

makeLabeledExtendedSection[sec_, hash_, b_][f_] := Labeled[
  BundleSection[KeySort @ Append[sec, b -> f], hash],
  SectionExtensionCardinal[b, v]
];

makeUnlabeledExtendedSection[sec_, hash_, b_][f_] :=
  BundleSection[KeySort @ Append[sec, b -> f], hash];

(**************************************************************************************************)

declareRewritingSystemDispatch["BundleSectionHomotopy", bundleSectionHomotopyProperty]

bundleSectionHomotopyProperty[data_, "CayleyFunction", opts___Rule] := Scope[
  UnpackStringOptions[{opts}, True, labeled];
  If[labeled,
    LabeledSectionHomotopyCayleyFunction,
    SectionHomotopyCayleyFunction
  ]
];

bundleSectionHomotopyProperty[data_, "AllStates"] :=
  FindAllBundleSections[data["BundleGraph"], FiberSymmetries -> data["FiberSymmetries"]];

(**************************************************************************************************)

PublicFunction[LabeledSectionHomotopyCayleyFunction, SectionHomotopyCayleyFunction]

SectionHomotopyCayleyFunction[bs_] := Block[
  {makeLabeledRewrittenSection = makeUnlabeledRewrittenSection},
  LabeledSectionHomotopyCayleyFunction[bs]
];

LabeledSectionHomotopyCayleyFunction[BundleSection[sec_, hash_]] := Scope[
  UnpackAssociation[bundleHashLookup @ hash, verticalAdjacency, baseAdjacency, areBundleAdjacent, cardinalIndex];
  Flatten @ KeyValueMap[
    {b, f} |-> (
      v = BundleVertex[b, f];
      (* get fiber nbors *)
      fnbs = verticalAdjacency[v];
      (* get base nbors *)
      bnbs = baseAdjacency[b];
      fnbs //= Select[fnb |-> AllTrue[bnbs, bnb |-> areBundleAdjacent[{fnb, BundleVertex[bnb, sec @ bnb]}]]];
      makeLabeledRewrittenSection[sec, hash, v] /@ fnbs
    ),
    sec
  ]
];

makeLabeledRewrittenSection[sec_, hash_, v_][v2:BundleVertex[b_, f_]] :=
  Labeled[
    BundleSection[
      ReplacePart[sec, Key[b] -> f],
      hash
    ],
    SectionRewriteCardinal[b, PN @ cardinalIndex[DirectedEdge[v, v2]]]
  ];

makeUnlabeledRewrittenSection[sec_, hash_, v_][v2:BundleVertex[b_, f_]] :=
  BundleSection[
    ReplacePart[sec, Key[b] -> f],
    hash
  ];

(* there is no meaning to inverting the base vertex of a section rewrite *)
SectionRewriteCardinal[a_, Inverted[b_]] := Inverted[SectionRewriteCardinal[a, b]];

(**************************************************************************************************)

PublicHead[SectionRewriteCardinal, SectionExtensionCardinal]

declareBoxFormatting[
  SectionRewriteCardinal[b_, f_] :> makeColonPair[b, f],
  SectionExtensionCardinal[b_, f_] :> makeColonPair[b, f]
];