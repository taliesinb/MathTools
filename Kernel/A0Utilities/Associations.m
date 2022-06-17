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
