PublicFunction[MergeAssocations]

MergeAssocations[f_, assocs_] :=
  KeyValueMap[f, Merge[assocs, Id]];

(**************************************************************************************************)

PublicFunction[LookupChain]

SetUsage @ "
LookupChain[dict$, key$] looks up key$ in dict$, returning Automatic if not found.
LookupChain[dict$1 -> dict$2 -> $$, key$] looks up key$ in dict$1, then in dict$2 if not found, etc.
LookupChain[spec$, {key$1, key$2, $$}] returns a list of results.
* Any dict$ can be an association or list of rules.
"

LookupChain[dict_, keys_] := Lookup[dict, keys, Automatic];
LookupChain[dict1_ -> dict2_, keys_] := Lookup[dict1, keys, Return[iLookup[dict1 -> dict2, keys], Lookup]];

iLookup[dict1_ -> dict2_, key_] := Lookup[dict1, key, iLookup[dict2, key]];
iLookup[dict1_ -> dict2_, keys_List] := Map[key |-> Lookup[dict1, key, iLookup[dict2, key]], keys];
iLookup[dict1_, key_] := Lookup[dict1, key, Automatic];

(**************************************************************************************************)

PublicFunction[Associate]

Associate[assoc_, key_ -> value_] := Prepend[assoc, key -> value];

(**************************************************************************************************)

PublicFunction[AssociationMapThread]

AssociationMapThread[f_, assoc_Assoc] := With[
  {keys = Keys @ assoc},
  Map[f[AssociationThread[keys, #]]&, Transpose @ Values @ assoc]
];

(**************************************************************************************************)

PublicFunction[AssociationKeyPattern]

AssociationKeyPattern[assoc_Assoc] := Apply[Alternatives, Keys @ assoc];

(**************************************************************************************************)

PublicFunction[AssociationRange, RangeAssociation]

SetRelatedSymbolGroup[AssociationRange, RangeAssociation, RuleRange];

SetUsage @ "
AssociationRange[{key$1, key$2, $$}] gives the association <|$$, key$i -> i$, $$|>.
"

AssociationRange[list_] :=
  AssociationThread[list, Range @ Len @ list];

RangeAssociation[list_] :=
  AssociationThread[Range @ Len @ list, list];

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

ConstantAssociation[keys_List, constant_] := AssociationThread[keys, Repeat[constant, Len @ keys]];

(**************************************************************************************************)

PublicFunction[LookupOrMessageKeys]

General::unrecognizedName = "`` is not a recognized name, which include: ``.";

SetUsage @ "
LookupOrMessageKeys[assoc$, key$, default$] returns the value associated with key$, or issues a message if there is none.
LookupOrMessageKeys[$$, msg$] uses a custom message.
* msg$ should be of the form symbol::msgname, whose first slot will be given key$ and second will be given available keys.
"
SetHoldRest[LookupOrMessageKeys];

LookupOrMessageKeys[assoc_, key_, default_] :=
  LookupOrMessageKeys[assoc, key, default, General::unrecognizedName];

LookupOrMessageKeys[assoc_, key_, default_, msg_] := With[{key2 = key},
  Lookup[assoc, Key @ key2,
    Message[msg, MsgExpr @ key, TextString[Row[MsgExpr /@ Keys @ assoc, ", "]]];
    default
  ]
];

(**************************************************************************************************)

PublicFunction[GroupPairs]

SetUsage @ "
GroupPairs[{{key$1, val$1}, {key$2, val$2}, $$}] yields <|key$1 -> {val$1, $$}, $$|> where the val$i are grouped by matching key.
GroupPairs[{expr$1, expr$2, $$}] effectively gives GroupBy[expr$, First -> Last].
"

GroupPairs[list_] := GroupBy[list, P1 -> PN];
