PublicFunction[SubstitutionRewritingSystem]

SubstitutionRewritingSystem[rules_, opts___] := Scope[
  constructRewritingSystem["Substitution", rules, opts]
]

SubstitutionRewritingSystem[s_String, opts___] := Scope[
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
