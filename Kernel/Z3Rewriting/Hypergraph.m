(**************************************************************************************************)

PackageExport["DirectedHypergraphRewritingSystem"]

DirectedHypergraphRewritingSystem[rules_] := Scope[
  constructRewritingSystsem["DirectedHypergraph", rules]
]

declareRewritingSystemDispatch["DirectedHypergraph", directedHypergraphRewritingSystemProperty]

directedHypergraphRewritingSystemProperty[data_, "CayleyFunction", opts___Rule] := Scope[
  UnpackAssociation[data, rules];
  UnpackStringOptions[{opts}, True, labeled];
  If[labeled,
    DirectedHypergraphLabeledReplaceList[rules],
    DirectedHypergraphReplaceList[rules]
  ]
];

(**************************************************************************************************)

PackageExport["DirectedUniHypergraphRewritingSystem"]

DirectedUniHypergraphRewritingSystem[rules_] := Scope[
  constructRewritingSystsem["DirectedUniHypergraph", rules]
]

declareRewritingSystemDispatch["DirectedUniHypergraph", directedUniHypergraphRewritingSystemProperty]

directedUniHypergraphRewritingSystemProperty[data_, "CayleyFunction", opts___Rule] := Scope[
  UnpackAssociation[data, rules];
  UnpackStringOptions[{opts}, True, labeled];
  If[labeled,
    DirectedUniHypergraphLabeledReplaceList[rules],
    DirectedUniHypergraphReplaceList[rules]
  ]
];

(**************************************************************************************************)

PackageExport["DirectedUniHypergraphLabeledReplaceList"]

DirectedUniHypergraphLabeledReplaceList[rule_][hyperedges_] :=
  DirectedUniHypergraphLabeledReplaceList[hyperedges, rule];

DirectedUniHypergraphLabeledReplaceList[hyperedges_List, rules_List] :=
  Catenate @ Map[DirectedUniHypergraphLabeledReplaceList[hyperedges, #]&, rules];

DirectedUniHypergraphLabeledReplaceList[hyperedges_List, rule_ | {rule_}] := Scope[
  pos = SubsetPosition[hyperedges, First @ rule];
  removeDupUniH[hyperedges, applyLabeledDUHRewriteChecked[hyperedges, #, rule]& /@ pos]
];

applyLabeledDUHRewrite[edges_, pos_, rule_] :=
  Labeled[
    Union @ Join[Delete[edges, List /@ pos], Replace[Part[edges, pos], Append[rule, _ -> $Failed]]],
    RewriteForm[
      Union @@ Part[edges, pos]
    ]
  ]

(**************************************************************************************************)

PackageExport["DirectedUniHypergraphReplaceList"]

DirectedUniHypergraphReplaceList[rule_][hyperedges_] :=
  DirectedUniHypergraphReplaceList[hyperedges, rule];

DirectedUniHypergraphReplaceList[hyperedges_List, rules_List] :=
  Catenate @ Map[DirectedUniHypergraphReplaceList[hyperedges, #]&, rules];

DirectedUniHypergraphReplaceList[hyperedges_List, rule_ | {rule_}] := Scope[
  pos = SubsetPosition[hyperedges, First @ rule];
  removeDupUniH[hyperedges, applyDUHRewrite[hyperedges, #, rule]& /@ pos]
];

applyDUHRewrite[edges_, pos_, rule_] := Scope[
  old = Part[edges, pos];
  Union @ Join[Delete[edges, List /@ pos], Replace[old, {rule, _ -> $Failed}]]
]

(**************************************************************************************************)

removeDupUniH[hyperedges_, results_] :=
  DeleteCases[results, Labeled[hyperedges, _] | hyperedges];

(**************************************************************************************************)

PackageExport["DirectedHypergraphLabeledReplaceList"]

DirectedHypergraphLabeledReplaceList[rule_][hyperedges_] :=
  DirectedHypergraphLabeledReplaceList[hyperedges, rule];

DirectedHypergraphLabeledReplaceList[hyperedges_List, rules_List] :=
  Catenate @ Map[DirectedHypergraphReplaceList[hyperedges, #]&, rules];

DirectedHypergraphLabeledReplaceList[hyperedges_List, rule_ | {rule_}] := Scope[
  pos = SubsetPosition[hyperedges, First @ rule];
  applyLabeledDHRewrite[hyperedges, #, rule]& /@ pos
];

applyLabeledDHRewrite[edges_, pos_, rule_] :=
  Labeled[
    Join[Delete[edges, List /@ pos], Replace[Part[edges, pos], Append[rule, _ -> $Failed]]],
    RewriteForm[
      Union @@ Part[edges, pos]
    ]
  ]

(**************************************************************************************************)

PackageExport["DirectedHypergraphReplaceList"]

DirectedHypergraphReplaceList[rule_][hyperedges_] :=
  DirectedHypergraphReplaceList[hyperedges, rule];

DirectedHypergraphReplaceList[hyperedges_List, rules_List] :=
  Catenate @ Map[DirectedHypergraphReplaceList[hyperedges, #]&, rules];

DirectedHypergraphReplaceList[hyperedges_List, rule_ | {rule_}] := Scope[
  pos = SubsetPosition[hyperedges, First @ rule];
  applyDHRewrite[hyperedges, #, rule]& /@ pos
];

applyDHRewrite[edges_, pos_, rule_] := Scope[
  old = Part[edges, pos];
  Join[Delete[edges, List /@ pos], Replace[old, {rule, _ -> $Failed}]]
]
