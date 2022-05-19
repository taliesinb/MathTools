(**************************************************************************************************)

PackageExport["HyperstringRewritingSystem"]

DirectedGraphRewritingSystem[rules_] := Scope[
  constructRewritingSystsem["Hyperstring", rules]
]

declareRewritingSystemDispatch["Hyperstring", hyperstringRewritingSystemProperty]

hyperstringRewritingSystemProperty[data_, "CayleyFunction", opts___Rule] := Scope[
  UnpackAssociation[data, rules];
  UnpackStringOptions[{opts}, True, labeled];
  If[!labeled, StripLabel, Identity] /* HyperstringLabeledReplaceList[rules]
];

HyperstringLabeledReplaceList[graph_, rules_][graph_] := "";

