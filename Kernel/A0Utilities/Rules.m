PublicFunction[ReverseRules]

ReverseRules[rules_] := Map[Rev, rules];

(**************************************************************************************************)

PublicFunction[RuleRange]

SetUsage @ "
RuleRange[{key$1, key$2, $$}] gives the list {$$, key$i -> i$, $$}.
"

RuleRange[labels_] :=
  MapIndexed[#1 -> P1[#2]&, labels];

(**************************************************************************************************)

PublicFunction[RuleThread]

SetRelatedSymbolGroup[RuleThread, AssociationThread];

SetUsage @ "
RuleThread[{key$1, key$2, $$}, {val$1, val$2, $$}] gives the list {$$, key$i -> val$i, $$}.
"

RuleThread[keys_List, values_List] /; Len[keys] === Len[values] :=
  MapThread[Rule, {keys, values}];

RuleThread::badlen = "Key length `` doesn't match value length ``. First key is ``."
RuleThread[keys_List, values_List] := (
  Message[RuleThread::badlen, Len @ keys, Len @ values, MsgExpr @ P1 @ keys];
  $Failed;
);

_RuleThread := BadArguments[];
