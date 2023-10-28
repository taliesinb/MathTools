PublicHead[Customized]

SetUsage @ "
Customized[item$, opts$$] specifies one or more customizations to be applied to item$.
* The allowed options depend on the context in which customized is used.
"

(**************************************************************************************************)

PrivateFunction[customizedFold]

customizedFold[Customized[item_, opts___], handler_] :=
  Fold[applyCustomizeHandler[handler], item, {opts}]

Customized::badopt = "Unknown custom option ``, ignoring it."
Customized::invalid = "Invalid custom option ``, ignoring it."
Customized::notrule = "Customization rule `` is not a valid rule, ignoring it."

applyCustomizeHandler[_][item_, nonRule_] := (
  Message[Customized::notrule, nonRule];
  item
);

applyCustomizeHandler[handler_][item_, rule_Rule] := Module[
  {result = handler[item, rule]},
  Switch[result,
    _handler,
      Message[Customized::badopt, MsgExpr @ rule];
      item,
    $Failed,
      Message[Customized::invalid, MsgExpr @ rule];
      item,
    _,
      result
  ]
];

(**************************************************************************************************)

PrivateFunction[customizedBlock]

Customized::badspec = "Customization spec `` is not a list of rules, ignoring it."
Customized::badopt2 = "Unknown custom option ``, ignoring it. Available options are: ``."

(* customizedBlock takes the Customized and a rule of option symbols to var symbols (opts :> globals),
and modifies those of them that are customized within an InheritedBlock, then calls innerFn  *)

customizedBlock[Customized[item_], _, innerFn_] := innerFn @ item;

customizedBlock[Customized[item_, rules__], opts_ :> globals_, innerFn_] :=
  InheritedBlock[
    globals,
    Scan[setCustomizedVar[globals, opts], {rules}];
    innerFn @ item
  ];

SetHoldFirst[setCustomizedVar];

setCustomizedVar[globals_, opts_][opt_ -> value_] := With[
  {ind = SelectFirstIndex[opts, MatchQ[opt, #]&]},
  If[MissingQ[ind],
    Message[Customized::badopt2, opt, opts],
    Apply[Set, Append[Extract[Unevaluated @ globals, ind, Hold], value]]
  ]
];

setCustomizedVar[globals_, opts_][bad_] :=
  Message[Customized::notrule, bad];

PrivateFunction[setGlobalsFromRules]

setGlobalsFromRules[ruleOrRules_, opts_ :> globals_] :=
  Scan[setCustomizedVar[globals, opts], ToList @ ruleOrRules];



