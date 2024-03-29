PublicFunction[MergeAssocations]

MergeAssocations[f_, assocs_] :=
  KVMap[f, Merge[assocs, Id]];

(**************************************************************************************************)

PublicFunction[LookupChain]

SetUsage @ "
LookupChain[dict$, key$] looks up key$ in dict$, returning Automatic if not found.
LookupChain[dict$1 -> dict$2 -> $$, key$] looks up key$ in dict$1, then in dict$2 if not found, etc.
LookupChain[spec$, {key$1, key$2, $$}] returns a list of results.
* Any dict$ can be an association or list of rules.
"

LookupChain[dict_, keys_] := Lookup[dict, keys, Auto];
LookupChain[dict1_ -> dict2_, keys_] := Lookup[dict1, keys, Return[iLookup[dict1 -> dict2, keys], Lookup]];

iLookup[dict1_ -> dict2_, key_] := Lookup[dict1, key, iLookup[dict2, key]];
iLookup[dict1_ -> dict2_, keys_List] := Map[key |-> Lookup[dict1, key, iLookup[dict2, key]], keys];
iLookup[dict1_, key_] := Lookup[dict1, key, Auto];

(**************************************************************************************************)

PublicFunction[Associate]

Associate[assoc_, key_ -> value_] := Pre[assoc, key -> value];

(**************************************************************************************************)

PublicFunction[UAssocThread]

General::keysValuesLength = "Key length (``) does not equal value length (``).";
UAssocThread[keys_List, vals_List]  :=
  UAssoc @ Thread[SameLenMsg::keysValuesLength[keys, vals]; keys -> vals];

UAssocThread[keys_][vals_] := UAssocThread[keys, vals];

(**************************************************************************************************)

PublicFunction[AssociationMapThread]

AssociationMapThread[f_, assoc_Assoc] := With[
  {keys = Keys @ assoc},
  Map[f[AssocThread[keys, #]]&, Transpose @ Values @ assoc]
];

(**************************************************************************************************)

PublicFunction[AssociationKeyPattern]

AssociationKeyPattern[assoc_Assoc] := Apply[Alt, Keys @ assoc];

(**************************************************************************************************)

PublicFunction[AssociationRange, RangeAssociation]

SetRelatedSymbolGroup[AssociationRange, RangeAssociation, RuleRange];

SetUsage @ "
AssociationRange[{key$1, key$2, $$}] gives the association <|$$, key$i -> i$, $$|>.
"

AssociationRange[list_] :=
  AssocThread[list, Range @ Len @ list];

RangeAssociation[list_] :=
  AssocThread[Range @ Len @ list, list];

(**************************************************************************************************)

PublicFunction[ConstantUAssociation]

SetRelatedSymbolGroup[ConstantUAssociation, ConstantAssociation];

SetUsage @ "
ConstantUAssociation[{key$1, key$2, $$}, c_] gives the unordered constant association <|$$, key$i -> c$, $$|>.
"

ConstantUAssociation[keys_List, constant_] := UAssoc @ ConstantAssociation[keys, constant];

(**************************************************************************************************)

PublicFunction[ConstantAssociation]

SetUsage @ "
ConstantAssociation[{key$1, key$2, $$}, c_] gives the constant association <|$$, key$i -> c$, $$|>.
"

ConstantAssociation[keys_List, constant_] := AssocThread[keys, Repeat[constant, Len @ keys]];

(**************************************************************************************************)

PublicFunction[GroupPairs]

SetUsage @ "
GroupPairs[{{key$1, val$1}, {key$2, val$2}, $$}] yields <|key$1 -> {val$1, $$}, $$|> where the val$i are grouped by matching key.
GroupPairs[{expr$1, expr$2, $$}] effectively gives GroupBy[expr$, First -> Last].
"

GroupPairs[list_] := GroupBy[list, F -> L];
