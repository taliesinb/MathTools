PublicFunction[ReverseRules]

ReverseRules[rules_] := Map[Reverse, rules];

(**************************************************************************************************)

PublicFunction[RuleRange]

SetUsage @ "
RuleRange[{key$1, key$2, $$}] gives the list {$$, key$i -> i$, $$}.
"

RuleRange[labels_] :=
  MapIndexed[#1 -> First[#2]&, labels];

(**************************************************************************************************)

PublicFunction[RuleThread]

SetRelatedSymbolGroup[RuleThread, AssociationThread];

SetUsage @ "
RuleThread[{key$1, key$2, $$}, {val$1, val$2, $$}] gives the list {$$, key$i -> val$i, $$}.
"

RuleThread[keys_List, values_List] /; Length[keys] === Length[values] :=
  MapThread[Rule, {keys, values}];

_RuleThread := BadArguments[];
