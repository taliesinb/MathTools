With[{fmv := GeneralUtilities`Control`PackagePrivate`findMutatedVariables},
  If[FreeQ[DownValues[fmv], ApplyTo],
    DownValues[fmv] = Insert[
      DownValues[fmv],
      Unevaluated @ ApplyTo[GeneralUtilities`Control`PackagePrivate`lhs_Symbol, _],
      {1, 2, 1, 1, 2, 1, 1, 2}
    ]
  ];
];

(**************************************************************************************************)

(* fix a bug in IndexOf, which accidentally didn't limit itself to level one *)
With[{io := GeneralUtilities`IndexOf},
  Unprotect[io];
  If[FreeQ[DownValues[io], {1}],
    DownValues[io] = ReplaceAll[
      DownValues[io],
      HoldPattern[FirstPosition][a_, b_, c_, Heads -> False] :>
        FirstPosition[a, b, c, {1}, Heads -> False]
    ];
  ];
  Protect[io];
];

(**************************************************************************************************)

GeneralUtilities`Control`PackagePrivate`upperCaseString[s_String] /; StringStartsQ[s, "$"] :=
  GeneralUtilities`Control`PackagePrivate`upperCaseString[StringDrop[s, 1]];

(**************************************************************************************************)

Module[{desugaringRules = Normal @ GeneralUtilities`Control`PackagePrivate`$DesugaringRules},
  If[FreeQ[desugaringRules, rewriteDestructuringFunction],
    AppendTo[desugaringRules, HoldPattern[GeneralUtilities`Control`PackagePrivate`e:Function[{___, _List, ___}, _]] :>
      RuleCondition[rewriteDestructuringFunction[GeneralUtilities`Control`PackagePrivate`e]]];
    GeneralUtilities`Control`PackagePrivate`$DesugaringRules = Dispatch @ desugaringRules;
  ];
];

SetHoldAllComplete[rewriteDestructuringFunction, procDestructArg];

rewriteDestructuringFunction[Function[args_, body_]] := Block[
  {$destructAliases = {}, $i = 1},
  ToQuoted[Function,
    Map[procDestructArg, Unevaluated @ args],
    Quoted[body] /. Flatten[$destructAliases]
  ]
];

rewriteDestructuringFunction[e_] := Quoted[e];
 
procDestructArg[e_Symbol] := Quoted[e];

procDestructArg[argSpec_] := With[
  {symbolPos = Position[Unevaluated @ argSpec, _Symbol, {0, Infinity}, Heads -> False]},
  If[symbolPos === {},
    Symbol["QuiverGeometry`Private`$$" <> IntegerString[$i++]]
  ,
    With[{parentSym = Symbol[Extract[Unevaluated @ argSpec, First @ symbolPos, HoldSymbolName] <> "$$"]},
      AppendTo[$destructAliases, Map[
        pos |-> Extract[Unevaluated @ argSpec, pos, HoldPattern] :> Extract[parentSym, pos],
        symbolPos
      ]];
      parentSym
    ]
  ]
]

(**************************************************************************************************)

(* this takes the place of MatchValues in GU *)

PublicMacro[Case]

SetHoldAll[Case, setupCases];

Case /: (Set|SetDelayed)[sym_Symbol, Case[args___]] := setupCases[sym, args];

setupCases[sym_Symbol, CompoundExpression[args__SetDelayed, rewrites_List]] :=
  setupCases[sym, CompoundExpression[args], rewrites];

setupCases[sym_Symbol, CompoundExpression[args__SetDelayed, Null...], rewrites_:{}] := Module[{holds},
  Clear[sym];
  holds = Hold @@@ Hold[args];
  holds = ReplaceAll[holds, procRewrites @ rewrites];
  PrependTo[holds, Hold[case_, UnmatchedCase[sym, case]]];
  holds = ReplaceAll[holds, HoldPattern[Out[]] :> sym];
  Replace[List @@ holds, Hold[a___, b_] :> SetDelayed[sym[a], b], {1}];
];

Case::baddef = "Bad case definition for ``."

setupCases[sym_, args___] := Message[Case::baddef, sym];

SetHoldAllComplete[procRewrites];
procRewrites[l_List] := Map[procRewrites, Unevaluated @ l];
procRewrites[a_ -> b_] := HoldPattern[a] -> b;
procRewrites[a_ :> b_] := HoldPattern[a] :> b;

SetUsage @ "
Case[rules$$] is a macro for defining functions of one variable, specifying LHS and RHS rules for the argument.
Case[rules$$, {alias$1, alias$2, $$}] applies temporary aliases to the rules$ before evaluation.
* Use the form func$ = Case[$$] to attach the rules to the function func$.
* Each of the rules should be of the form patt$ :> body$, and should be seperated by semicolons.
* The aliases can be used to transform the rules before they attached used as definitions.
* Use \[Rule] in an alias to have the RHS of the alias evaluate, and \[RuleDelayed] to perform a literal replacement.
* Aliases can be thought of as 'local macros' that make a particular function definition cleaner or more concise.
"

(**************************************************************************************************)

$numNames = <|1 -> "first", 2 -> "second", 3 -> "third", 4 -> "fourth"|>;

defineCheckArgMacro[checkMacro_Symbol, checker_, msgName_String] := DefineMacro[coerceMacro,
  coerceMacro[n_] := With[
    {nthArg = Part[$LHSPatternSymbols, n], numStr = $numNames[n]},
    Quoted @ Replace[coercer[nthArg], $Failed :> ReturnFailed[MessageName[$LHSHead, msgName], numStr]]
  ]
];

defineCoerceArgMacro[coerceMacro_Symbol, coercer_, msgName_String] := DefineMacro[coerceMacro,
  coerceMacro[n_] := With[
    {nthArg = Part[$LHSPatternSymbols, n], numStr = $numNames[n]},
    Quoted @ Replace[coercer[nthArg], $Failed :> ReturnFailed[MessageName[$LHSHead, msgName], numStr]]
  ]
];

_defineCheckArgMacro := Panic["BadArgMacro"];
_defineCoerceArgMacro := Panic["BadArgMacro"];

(**************************************************************************************************)

PrivateMacro[CheckIsQuiver, CoerceToQuiver]

General::notquiver = "The `` argument should be a quiver.";
General::notquiverc = "The `` argument should be a quiver or list of quiver edges.";
defineCheckArgMacro[CheckIsQuiver, QuiverQ, "notquiver"];
defineCoerceArgMacro[CoerceToQuiver, ToQuiver, "notquiverc"];


PrivateMacro[CheckIsGraph, CoerceToGraph]

General::notgraph = "The `` argument should be a Graph.";
General::notgraphc = "The `` argument should be a Graph or list of edges.";
defineCheckArgMacro[CheckIsGraph, GraphQ, "notgraph"];
defineCoerceArgMacro[CoerceToGraph, ToGraph, "notgraphc"];


PrivateMacro[CheckIsRep, CoerceToRep]

General::notrep = "The `` argument should be a RepresentationObject.";
General::notrepc = "The `` argument should be a group, groupoid, RepresentationObject, PathRepresentationObject, or RootSystem.";
defineCheckArgMacro[CheckIsRep, RepresentationObjectQ, "notrep"];
defineCoerceArgMacro[CoerceToRep, ToRepresentation, "notrepc"];


PrivateMacro[CheckIsGroup]

General::notgroup = "`` is not a valid group.";
defineCheckArgMacro[CheckIsGroup, GroupQ, "notgroup"];


PrivateMacro[CheckIsGraphics]

General::notgraphics = "The `` argument should be a Graphics or Graphics3D expression."
defineCheckArgMacro[CheckIsGraphics, GraphicsQ, "notgraphics"];

(**************************************************************************************************)

PrivateMacro[UnpackOptionsAs]

DefineMacro[UnpackOptionsAs,
UnpackOptionsAs[head_Symbol, opts_, syms__Symbol] :=
  mUnpackOptionsAs[head, opts, {syms}]
];

SetHoldAllComplete[mUnpackOptionsAs];
mUnpackOptionsAs[head_, opts_, syms_] :=
  ToQuoted[Set, Quoted[syms],
    ToQuoted[OptionValue, head, List @ opts, symbolsToCapitalizedStrings @ syms]
  ];

(**************************************************************************************************)

SetHoldAllComplete[symbolsToCapitalizedStrings];

symbolsToCapitalizedStrings[syms_] := Map[
  Function[sym, capitalizeFirstLetter @ HoldSymbolName @ sym, HoldAllComplete],
  Unevaluated @ syms
];

capitalizeFirstLetter[str_String] :=
  If[StringStartsQ[str, "$"], capitalizeFirstLetter @ StringDrop[str, 1],
    ToUpperCase[StringTake[str, 1]] <> StringDrop[str, 1]];

(**************************************************************************************************)

PrivateMacro[UnpackStringOptions]

DefineMacro[UnpackStringOptions,
UnpackStringOptions[options_, default_, syms__Symbol] :=
  mUnpackStringOptions[options, default, {syms}]
];

SetHoldAllComplete[mUnpackStringOptions, uppercaseSymbolName];
mUnpackStringOptions[options_, default_, syms_] :=
  ToQuoted[Set, Quoted[syms],
    ToQuoted[Lookup,
      options,
      symbolsToCapitalizedStrings @ syms,
      Quoted[default]
    ]
  ];

(**************************************************************************************************)

PrivateMacro[UnpackAnonymousOptions]

DefineMacro[UnpackAnonymousOptions,
UnpackAnonymousOptions[object_, default_, syms__Symbol] :=
  mUnpackAnonymousOptions[object, default, {syms}]
];

SetHoldAllComplete[mUnpackAnonymousOptions];
mUnpackAnonymousOptions[object_, default_, syms_] :=
  ToQuoted[Set, Quoted[syms],
    ToQuoted[LookupOption,
      Quoted[object],
      findMatchingSymbols[syms],
      Quoted[default]
    ]
  ];

(**************************************************************************************************)

PrivateMacro[UnpackAnonymousThemedOptions]

DefineMacro[UnpackAnonymousThemedOptions,
UnpackAnonymousThemedOptions[object_, default_, syms__Symbol] :=
  mUnpackAnonymousThemedOptions[object, default, {syms}]
];

SetHoldAllComplete[mUnpackAnonymousThemedOptions];
mUnpackAnonymousThemedOptions[object_, default_, syms_] :=
  ToQuoted[Set, Quoted[syms],
    ToQuoted[LookupThemedOption,
      Quoted[object],
      findMatchingSymbols[syms],
      Quoted[default]
    ]
  ];
(**************************************************************************************************)

PrivateMacro[UnpackExtendedThemedOptions]

DefineMacro[UnpackExtendedThemedOptions,
UnpackExtendedThemedOptions[graph_, syms___Symbol] :=
  mUnpackExtendedThemedOptions[graph, {syms}]
];

SetHoldAllComplete[mUnpackExtendedThemedOptions];
mUnpackExtendedThemedOptions[graph_, syms_] :=
  ToQuoted[Set, Quoted[syms],
    ToQuoted[LookupExtendedThemedOption,
      Quoted[graph],
      findMatchingSymbols[syms]
    ]
  ];

(**************************************************************************************************)

PrivateMacro[UnpackExtendedOptions]

DefineMacro[UnpackExtendedOptions,
UnpackExtendedOptions[graph_, syms___Symbol] :=
  mUnpackExtendedOptions[graph, {syms}]
];

SetHoldAllComplete[mUnpackExtendedOptions];
mUnpackExtendedOptions[graph_, syms_] :=
  ToQuoted[Set, Quoted[syms],
    ToQuoted[LookupExtendedOption,
      Quoted[graph],
      findMatchingSymbols[syms]
    ]
  ];

SetHoldAllComplete[findMatchingSymbols];
$lowerCaseSymbolRegExp = RegularExpression["\\b([a-z])(\\w+)\\b"];
findMatchingSymbols[syms_List] := findMatchingSymbols[syms] = Block[
  {$Context = "QuiverGeometry`Private`Dummy`", $ContextPath = {"System`", "QuiverGeometry`", $Context}, str},
  str = ToString[Unevaluated @ syms, InputForm];
  str = StringReplace[str, $lowerCaseSymbolRegExp :> StringJoin[ToUpperCase["$1"], "$2"]];
  ToExpression[str, InputForm, Quoted]
];

(**************************************************************************************************)

PrivateMacro[GraphCachedScope]
PublicVariable[$GraphCacheStore]

$GraphCacheStore = Language`NewExpressionStore["GraphCache"];

DefineMacro[GraphCachedScope,
GraphCachedScope[graph_, args___, body_] := mGraphCachedScope[graph, {$LHSHead, args}, body]
];

SetHoldAllComplete[mGraphCachedScope];

mGraphCachedScope[graph_, key_, body_] := With[{body2 = MacroExpand @ Scope @ body},
  Quoted @ Module[
    {$cacheTemp$ = $GraphCacheStore["get"[graph, key]]},
    If[$cacheTemp$ === Null,
      $cacheTemp$ = body2;
      If[!FailureQ[$cacheTemp$], $GraphCacheStore["put"[graph, key, $cacheTemp$]]];
    ];
    $cacheTemp$
  ]
];

(**************************************************************************************************)

PrivateFunction[CatchMessage]

DefineMacro[CatchMessage,
CatchMessage[body_] := Quoted[Catch[body, ThrownMessage[_], ThrownMessageHandler[$LHSHead]]],
CatchMessage[head_, body_] := Quoted[Catch[body, ThrownMessage[_], ThrownMessageHandler[head]]]
];

ThrownMessageHandler[msgHead_Symbol][{args___}, ThrownMessage[msgName_String]] :=
  (Message[MessageName[msgHead, msgName], args]; $Failed);

PrivateFunction[ThrowMessage]

ThrowMessage[msgName_String, msgArgs___] :=
  Throw[{msgArgs}, ThrownMessage[msgName]];

(**************************************************************************************************)

PrivateMacro[FunctionSection]

DefineMacro[FunctionSection,
FunctionSection[expr_] := Quoted[expr]
];

(**************************************************************************************************)

PrivateMacro[SetAutomatic, SetMissing, SetNone, SetAll, SetInherited]

defineSetter[symbol_, value_] := (
  DefineLiteralMacro[symbol, symbol[lhs_, rhs_] := If[lhs === value, lhs = rhs, lhs]];
  SetHoldAll @ symbol;
);

defineSetter[SetAutomatic, Automatic];
defineSetter[SetNone, None];
defineSetter[SetAll, All];
defineSetter[SetInherited, Inherited];

DefineLiteralMacro[SetMissing, SetMissing[lhs_, rhs_] := If[MissingQ[lhs], lhs = rhs, lhs]];
SetHoldAll[SetMissing];

(**************************************************************************************************)

PrivateMacro[ReplaceNone, ReplaceMissing, ReplaceAutomatic]

defineReplacer[symbol_, pattern_] := (
  DefineLiteralMacro[symbol,
    symbol[lhs_, rhs_] := Replace[lhs, pattern :> rhs],
    symbol[rhs_]       := Replace[pattern :> rhs]
  ];
  SetHoldAll @ symbol;
);

defineReplacer[ReplaceNone, None];
defineReplacer[ReplaceMissing, _Missing];
defineReplacer[ReplaceAutomatic, Automatic];

(**************************************************************************************************)

PrivateVariable[$NotImplemented]

$NotImplemented := Panic["NotImplemented"];

(**************************************************************************************************)

PrivateFunction[OnFailed]

SetUsage @ "
OnFailed[expr$, body$] evaluates and returns body$ if expr$ is $Failed, otherwise returns expr$.
"

SetHoldRest[OnFailed];

OnFailed[$Failed, e_] := e;
OnFailed[e_, _] := e;

OnFailed[$Failed, e_, _] := e;
OnFailed[e_, _, s_] := s;