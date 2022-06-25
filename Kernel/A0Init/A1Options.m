PublicFunction[LookupOption]

SetRelatedSymbolGroup[LookupOption, JoinOptions, DeleteOptions, TakeOptions, ReplaceOptions, UpdateOptions];

SetUsage @ "
LookupOption[object$, option$] looks up the value of option$ in object$.
LookupOption[object$, option$, default$] returns default$ if no value is associated with option$.
* object$ can be any expression that %Options evaluates on.
* By default, Automatic is returned if there is no value for option$.
"

LookupOption[obj_, opt_, default_:Automatic] :=
  Quiet @ Lookup[Options[obj, opt], opt, default];

(**************************************************************************************************)

PublicFunction[JoinOptions]

SetUsage @ "
JoinOptions[options$1, options$2, $$] joins together the options$i to form a single list of rules.
* The option$i can be a rule, list of rules, or a symbol with defined %Options.
* If multiple values are given for a given option symbol, the first is taken.
"

JoinOptions[opts___] := DeleteDuplicatesBy[
  Flatten @ Replace[Flatten @ {opts}, s_Symbol :> Options[s], {1}],
  First
];

(**************************************************************************************************)

PublicFunction[DeleteOptions]

SetUsage @ "
DeleteOptions[options$, symbol$] removes rules with LHS symbol$ from the list of rules given by options$.
DeleteOptions[options$, {sym$, sym$2, $$}] removes multiple rules.
DeleteOptions[spec$] is the operator form of DeleteOptions.
"

DeleteOptions[graph_Graph, keys_] :=
   Graph[VertexList @ graph, EdgeList @ graph, DeleteOptions[Options @ graph, keys]];

DeleteOptions[opts_, keys_List] :=
  DeleteCases[opts, (Alternatives @@ keys) -> _];

DeleteOptions[opts_, key_] :=
  DeleteCases[opts, key -> _];

DeleteOptions[key_][opts_] :=
  DeleteOptions[opts, key];

(**************************************************************************************************)

PublicFunction[TakeOptions]

SetUsage @ "
TakeOptions[options$, symbol$] gives only rules with LHS symbol$ from the list of rules given by options$.
TakeOptions[options$, {sym$, sym$2, $$}] gives rules that match any of the sym$i.
TakeOptions[spec$] is the operator form of TakeOptions.
"

TakeOptions[sym_Symbol, spec_] :=
  TakeOptions[Options @ sym, spec];

TakeOptions[opts_List, keys_List] :=
  Cases[opts, Verbatim[Rule][Alternatives @@ keys, _]];

TakeOptions[opts_List, key_] :=
  Cases[opts, Verbatim[Rule][key, _]];

(**************************************************************************************************)

PublicFunction[ReplaceOptions]

SetUsage @ "
ReplaceOptions[object$, symbol$ -> value$] gives a new version of object$ in which the option has been applied.
ReplaceOptions[object$, {rule$1, rule$2, $$}] applies multiple rule changes.
ReplaceOptions[spec$] is the operator form of ReplaceOptions.
* ReplaceOptions can take a %Graph, %Graphics, etc. as an object, returning the same kind of object.
* ReplaceOptions can take a list of rules as an object, returning a new list of rules.
"

ReplaceOptions[obj_, {}] := obj;

ReplaceOptions[g_Graph, rule_Rule | {rules__Rule}] :=
  ExtendedGraph[g, rule];

ReplaceOptions[gs:{__Graph}, spec_] :=
  Map[ReplaceOptions[spec], gs];

ReplaceOptions[obj_, key_ -> value_] := If[
  MemberQ[obj, key -> _],
  Replace[obj, Rule[key, _] :> Rule[key, value], {1}],
  Append[obj, key -> value]
];

ReplaceOptions[obj_, rules_List] :=
  Fold[ReplaceOptions, obj, rules];

ReplaceOptions[rules_][obj_] := ReplaceOptions[obj, rules];

(**************************************************************************************************)

PublicFunction[UpdateOptions]

SetUsage @ "
UpdateOptions[object$, option$, f$] returns a new object with the old value of option$ replaced with f$[value$].
* If the option was not present initially, the value %Automatic is supplied to f$.
"

UpdateOptions[obj_, option_, func_] :=
  ReplaceOptions[obj, option -> func[LookupOption[obj, option]]];