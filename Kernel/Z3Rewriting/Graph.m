(**************************************************************************************************)

PublicFunction[DirectedUniGraphLabeledReplaceList]

DirectedUniGraphLabeledReplaceList[rule_][hyperedges_] :=
  DirectedUniGraphLabeledReplaceList[hyperedges, rule];

DirectedUniGraphLabeledReplaceList[hyperedges_List, rules_List] :=
  Catenate @ Map[DirectedUniGraphLabeledReplaceList[hyperedges, #]&, rules];

DirectedUniGraphLabeledReplaceList[hyperedges_List, rule_ | {rule_}] := Scope[
  pos = SubsetPosition[hyperedges, P1 @ rule];
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

PublicFunction[DirectedUniGraphReplaceList]

DirectedUniGraphReplaceList[rule_][hyperedges_] :=
  DirectedUniGraphReplaceList[hyperedges, rule];

DirectedUniGraphReplaceList[hyperedges_List, rules_List] :=
  Catenate @ Map[DirectedUniGraphReplaceList[hyperedges, #]&, rules];

DirectedUniGraphReplaceList[hyperedges_List, rule_ | {rule_}] := Scope[
  pos = SubsetPosition[hyperedges, P1 @ rule];
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

PublicFunction[DirectedGraphLabeledReplaceList]

DirectedGraphLabeledReplaceList[rule_][hyperedges_] :=
  DirectedGraphLabeledReplaceList[hyperedges, rule];

DirectedGraphLabeledReplaceList[hyperedges_List, rules_List] :=
  Catenate @ Map[DirectedGraphReplaceList[hyperedges, #]&, rules];

DirectedGraphLabeledReplaceList[hyperedges_List, rule_ | {rule_}] := Scope[
  pos = SubsetPosition[hyperedges, P1 @ rule];
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

PublicFunction[DirectedGraphReplaceList]

DirectedGraphReplaceList[rule_][hyperedges_] :=
  DirectedGraphReplaceList[hyperedges, rule];

DirectedGraphReplaceList[hyperedges_List, rules_List] :=
  Catenate @ Map[DirectedGraphReplaceList[hyperedges, #]&, rules];

DirectedGraphReplaceList[hyperedges_List, rule_ | {rule_}] := Scope[
  pos = SubsetPosition[hyperedges, P1 @ rule];
  applyDHRewrite[hyperedges, #, rule]& /@ pos
];

applyDHRewrite[edges_, pos_, rule_] := Scope[
  old = Part[edges, pos];
  Join[Delete[edges, List /@ pos], Replace[old, {rule, _ -> $Failed}]]
]