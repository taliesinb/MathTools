PublicFunction[MergeAssocations]

MergeAssocations[f_, assocs_] :=
  KeyValueMap[f, Merge[assocs, Identity]];

(**************************************************************************************************)

PublicFunction[AssociationRange]

SetRelatedSymbolGroup[AssociationRange, RuleRange];

SetUsage @ "
AssociationRange[{key$1, key$2, $$}] gives the association <|$$, key$i -> i$, $$|>.
"

AssociationRange[list_] :=
  AssociationThread[list, Range @ Length @ list];

(**************************************************************************************************)

PublicFunction[ConstantUAssociation]

SetRelatedSymbolGroup[ConstantUAssociation, ConstantAssociation];

SetUsage @ "
ConstantUAssociation[{key$1, key$2, $$}, c_] gives the unordered constant association <|$$, key$i -> c$, $$|>.
"

ConstantUAssociation[keys_List, constant_] := UAssociation @ ConstantAssociation[keys, constant];

(**************************************************************************************************)

PublicFunction[ConstantAssociation]

SetUsage @ "
ConstantAssociation[{key$1, key$2, $$}, c_] gives the constant association <|$$, key$i -> c$, $$|>.
"

ConstantAssociation[keys_List, constant_] := AssociationThread[keys, ConstantArray[constant, Length @ keys]];

(**************************************************************************************************)

PublicFunction[GroupPairs]

SetUsage @ "
GroupPairs[{{key$1, val$1}, {key$2, val$2}, $$}] yields <|key$1 -> {val$1, $$}, $$|> where the val$i are grouped by matching key.
GroupPairs[{expr$1, expr$2, $$}] effectively gives GroupBy[expr$, First -> Last].
"

GroupPairs[list_] := GroupBy[list, First -> Last];
