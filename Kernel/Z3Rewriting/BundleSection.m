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

