PublicFunction[BundleSectionRewritingSystem]

BundleSectionRewritingSystem::arg1 = "First argument should be a bundle graph."

BundleSectionRewritingSystem[graph_] := Scope[
  If[!BundleGraphQ[graph], ReturnFailed["arg1"]];
  props = <|"BundleGraph" -> graph|>;
  constructRewritingSystem["BundleSection", Null, "CustomProperties" -> props]
]

_BundleSectionRewritingSystem := (Message[BundleSectionRewritingSystem::args, BundleSectionRewritingSystem]; $Failed);

declareRewritingSystemDispatch["BundleSection", bundleSectionRewritingSystemProperty]

bundleSectionRewritingSystemProperty[data_, "CayleyFunction", opts___Rule] := Scope[
  UnpackStringOptions[{opts}, True, labeled];
  If[labeled,
    LabeledSectionRewritingCayleyFunction,
    SectionRewritingCayleyFunction
  ]
];

bundleSectionRewritingSystemProperty[data_, "AllStates"] :=
  FindAllBundleSections[data["BundleGraph"]];

(**************************************************************************************************)

PublicFunction[LabeledSectionRewritingCayleyFunction, SectionRewritingCayleyFunction]

SectionRewritingCayleyFunction[bs_] := Block[{makeLabeledRewrittenSection = makeUnlabeledRewrittenSection},
  LabeledSectionRewritingCayleyFunction[bs]
];

LabeledSectionRewritingCayleyFunction[BundleSection[sec_, hash_]] := Scope[
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
    SectionRewriteCardinal[b, Last @ cardinalIndex[DirectedEdge[v, v2]]]
  ];

makeUnlabeledRewrittenSection[sec_, hash_, v_][v2:BundleVertex[b_, f_]] :=
  BundleSection[
    ReplacePart[sec, Key[b] -> f],
    hash
  ];

(* there is no meaning to inverting the base vertex of a section rewrite *)
SectionRewriteCardinal[a_, Inverted[b_]] := Inverted[SectionRewriteCardinal[a, b]];

(**************************************************************************************************)

PublicHead[SectionRewriteCardinal]

declareBoxFormatting[
  SectionRewriteCardinal[b_, f_] :> makeColonPair[b, f]
];