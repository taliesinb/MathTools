PublicFunction[ExpressionRewritingSystem]

ExpressionRewritingSystem[rules_, opts___] := Scope[
  constructRewritingSystem["Expression", lookupRule @ rules, opts]
]

lookupRule = Case[
  "BinaryPivot" := {\[FormalA]_, {\[FormalB]_, \[FormalC]_}} <-> {{\[FormalA]_, \[FormalB]_}, \[FormalC]_};
  e_ := e;
];

declareRewritingSystemDispatch["Expression", ExpressionRewritingSystemProperty]

ExpressionRewritingSystemProperty[data_, "CayleyFunction", opts___Rule] := Scope[
  UnpackAssociation[data, rules];
  UnpackStringOptions[{opts}, True, labeled];
  rules = Map[procSymRules, ToList @ rules];
  If[labeled,
    LabeledReplaceAllList[rules]
  ,
    plainRules = removeSingleton @ ReplaceAll[PairedRules -> Splice] @ rules;
    ReplaceAllList[plainRules]
  ]
];

(**************************************************************************************************)

PublicFunction[UnorderedExpressionRewritingSystem]

UnorderedExpressionRewritingSystem[rules_] :=
  ExpressionRewritingSystem[rules, CanonicalizationFunction -> SortNestedLists];

PublicFunction[SortNestedLists]

SortNestedLists[e_] := e //. l_List :> Sort[l];

(**************************************************************************************************)

procSymRules = Case[
  lhs_ <-> rhs_ := {
    RuleDelayed @@ Join[Hold[lhs], subPatternSymbols @ Hold[rhs]],
    RuleDelayed @@ Join[Hold[rhs], subPatternSymbols @ Hold[lhs]]
  };
  rule_Rule        := rule;
  rule_RuleDelayed := rule
];


subPatternSymbols[e_] := e /. Verbatim[Pattern][s_Symbol, _] :> s;

(**************************************************************************************************)

PublicFunction[LabeledReplaceAllList]

LabeledReplaceAllList[rules_][expr_] :=
  LabeledReplaceAllList[expr, rules];

LabeledReplaceAllList[expr_, rules_] := Scope[
  $expr = expr;
  Catenate @ MapIndex1[
    iLabeledReplaceAllList,
    rules
  ]
];

LabeledReplaceAllList[expr_, {rule_}] := Scope[
  $expr = expr; iLabeledReplaceAllList[rule, None]
];

iLabeledReplaceAllList[{rule1_RuleDelayed, rule2_RuleDelayed}, i_] := Join[
  iLabeledReplaceAllList[rule1, i],
  invertCard /@ iLabeledReplaceAllList[rule2, i]
];

iLabeledReplaceAllList[rule_, i_] := Scope[
  positions = Position[$expr, toLHS @ rule];
  Map[
    pos |-> Labeled[replaceAt[$expr, rule, pos], ExpressionRewriteMatchForm[pos, i]],
    positions
  ]
];

replaceAt[expr_, rule_, {}] := Replace[expr, rule];
replaceAt[expr_, rule_, pos_] := MapAt[Replace[rule], expr, pos];


invertCard[Labeled[e_, c_]] := Labeled[e, Inverted @ c];

(**************************************************************************************************)

PublicFunction[ReplaceAllList]

ReplaceAllList[expr_, rules_] := Scope[
  positions = Position[expr, toLHS @ rules];
  Switch[positions,
    {},   {},
    {_},  replaceListAt[expr, rules, First @ positions],
    _,    Catenate @ Map[pos |-> replaceListAt[expr, rules, pos], positions]
  ]
];

replaceListAt[expr_, rules_, {}] := ReplaceList[expr, rules];

replaceListAt[expr_, rules_, pos_] := Scope[
  Map[
    result |-> ReplacePart[expr, pos -> result],
    ReplaceList[Extract[expr, pos], rules]
  ]
];

ReplaceAllList[rules_][expr_] := ReplaceAllList[expr, rules];

toLHS := Case[
  rules_List := Alternatives @@ Map[toLHS, rules];
  lhs_ -> _  := lhs;
  lhs_ :> _  := lhs;
];