PublicHead[PCycle]

PCycle[a_] := Cycles[{a}];


PublicFunction[SubstitutionRewritingSystem]

SubstitutionRewritingSystem[rules_, opts___] := Scope @ CatchMessage[
  constructRewritingSystem["Substitution", Map[processSubRuleset, ToList @ rules], opts]
]

processSubRuleset = Case[
  l_List := collapseInvSymmetry @ Flatten @ Map[processSubRule, l];
  rule_  := collapseInvSymmetry @ Flatten @ List @ processSubRule @ rule;
];

SubstitutionRewritingSystem::invrule = "Invalid rule element ``."
processSubRule = Case[
  r:Rule[_, _]             := r;
  r:TwoWayRule[lhs_, rhs_] := {lhs -> rhs, rhs -> lhs};
  Cycles[cycles:{__}]      := Map[processCycle, cycles];
  inv_                     := Msg::invrule[inv];
];

SubstitutionRewritingSystem::invcyc = "Invalid Cycles[...] element ``."
processCycle = Case[
  s_Str     := % @ StringToWord[s];
  {_} | {}  := Nothing;
  {a_, b_}  := processSubRule[TwoWayRule[a, b]];
  list_List := ApplyWindowedCyclic[Rule /* processSubRule, list];
  inv_      := Msg::invcyc[inv];
];

collapseInvSymmetry[list_] := DeleteDuplicates @ Map[normalizeInvRule, list];
  
normalizeInvRule = Case[
  Inverted[lhs_] -> rhs_ := lhs -> Inverted[rhs];
  other_                 := other;
];

SubstitutionRewritingSystem[s_Str, opts___] := Scope[
  chars = Characters[s];
  SubstitutionRewritingSystem[toSignedPermutationRules[chars], opts]
];

toSignedPermutationRules[chars_] :=
  Join[
    ApplyWindowed[{c1, c2} |-> {c1 -> c2, c2 -> c1}, chars],
    Map[c |-> {c -> Inverted[c]}, chars]
  ]

declareRewritingSystemDispatch["Substitution", substitutionRewritingSystemProperty]

substitutionRewritingSystemProperty[data_, "CayleyFunction", opts___Rule] := Scope[
  UnpackAssociation[data, rules];
  UnpackStringOptions[{opts}, True, labeled];
  If[labeled,
    LabeledSubstitutionsList[rules],
    SubstitutionsList[rules]
  ]
];

SubstitutionsList[rules_][expr_] := Map[ReplaceAll[expr, #]&, rules];

LabeledSubstitutionsList[rules_][expr_] := MapIndex1[Labeled[ReplaceAll[expr, #1], #2]&, rules];
