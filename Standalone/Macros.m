
(* this ensures we have only one set of rules for each head *)
$MacroRules = Data`UnorderedAssociation[];

$nonterminatingMacroHeadsP = Alternatives[];

invalidateMacroRules[] := (
  Clear[$macroHeadsP, $internalMacroRules];
  $macroHeadsP := (generateMacroRules[]; $macroHeadsP);
  $internalMacroRules := (generateMacroRules[]; $internalMacroRules);
);

invalidateMacroRules[];

generateMacroRules[] := (
  Clear[$macroHeadsP, $internalMacroRules];
  $macroHeadsP = Alternatives @@ Keys @ $MacroRules;
  $terminatingMacroHeadsP = Complement[$macroHeadsP, $nonterminatingMacroHeadsP];
  $internalMacroRules = Catenate @ $MacroRules;
);

General::invalidMacroDefinition = "Not a valid macro definition: ``.";

(*************************************************************************************************)

SetAttributes[MacroHold, HoldAll];

(*************************************************************************************************)

SetAttributes[DefineVariableMacro, HoldAll];

DefineVariableMacro::usage =
"DefineVariableMacro[symbol, value] defines a macro that expands symbol to a value at load time."

DefineVariableMacro[sym_Symbol, value_] := (
  sym = value;
  $MacroRules[HoldPattern[sym]] = {HoldPattern[sym] :> value};
  invalidateMacroRules[];
);

e_DefineVariableMacro := ThrowErrorMessage["invalidMacroDefinition", HoldForm @ e];

(*************************************************************************************************)

SetAttributes[DefinePatternMacro, HoldAll];

DefinePatternMacro::usage =
"DefinePatternMacro[symbol, value] defines a macro that expands symbol to value at load time.
* the symbol is assumed to be a pattern variable."

DefinePatternMacro[sym_Symbol, value_] := (
  $PatternMacros[HoldPattern @ sym] = value;
  DefineVariableMacro[sym, value];
);

e_DefinePatternMacro := ThrowErrorMessage["invalidMacroDefinition", HoldForm @ e];

(*************************************************************************************************)

SetAttributes[{DefineSimpleMacro, defineMacro}, HoldAll];

DefineSimpleMacro::usage =
"DefineSimpleMacro[symbol, lhs :> rhs] defines a simple macro associated with symbol that is expanded at load time.
DefineSimpleMacro[symbol, rules] defines several rules.
* the special symbol $MacroParentSymbol will be substituted with symbol whose top-level definition contains the macro.
* MacroHold can be used to quote code."

DefineSimpleMacro[sym_Symbol, lhs_ :> rhs_] :=
  defineMacro[sym, {HoldPattern[lhs] :> rhs}];

(* since we may have previously set up downvalues for sym, we can't allow the rule LHS to evaluate *)
DefineSimpleMacro[sym_Symbol, rules:{__RuleDelayed}] := With[
  {heldRules = MapAt[HoldPattern, Unevaluated @ rules, {All, 1}]},
  defineMacro[sym, heldRules]
];

defineMacro[sym_Symbol, rules:{__RuleDelayed}] := (
  DownValues[sym]        = rules;
  $MacroRules[HoldPattern[sym]] = rules;
  invalidateMacroRules[];
);

e_DefineSimpleMacro := ThrowErrorMessage["invalidMacroDefinition", HoldForm @ e];

(*************************************************************************************************)

SetAttributes[DefineMessageMacro, HoldAll];

DefineMessageMacro::usage =
"DefineMessageMacro[symbol, defMsgName, defMsgStr, lhs :> rhs] defines a macro associated with throwing messages.
* General::defMsgName = defMsgStr defines the general message that will be thrown by symbol[...].
* symbol::msgName[...] will throw a specific message."

DefineMessageMacro[sym_Symbol, defMsgName_String, defMsgStr_String, RuleDelayed[lhs_, rhs_]] := With[
  {hsym = HoldPattern[sym]},
  If[!KeyExistsQ[$MacroRules, hsym], AppendTo[$nonterminatingMacroHeadsP, hsym]];
  Set[MessageName[General, defMsgName], defMsgStr];
  DownValues[sym] = {HoldPattern[lhs] :> rhs};
  $MacroRules[hsym] = {
    HoldPattern[sym[]]                                                      :> sym[defMsgName],
    HoldPattern[sym[a1:Except[_String], ar___]]                             :> sym[defMsgName, a1, ar],
    HoldPattern[MessageName[sym, msgName_String][args___]]                  :> sym[msgName, args]
  };
  invalidateMacroRules[];
];

e_DefineMessageMacro := ThrowErrorMessage["invalidMacroDefinition", HoldForm @ e];

(*************************************************************************************************)

ContainsMacrosQ::usage = "ContainsMacrosQ[...] returns True if macro symbols are present.";

ContainsMacrosQ[e_] := !FreeQ[Unevaluated @ e, $macroHeadsP];

(*************************************************************************************************)

ExpandMacros::usage = "ExpandsMacros[HoldComplete[...]] expands macros that are present.";

ExpandMacros[hc_ /; FreeQ[hc, $macroHeadsP]] := hc;
ExpandMacros[hc_] := checkDone @ subHolds @ subMps @ ReplaceRepeated[hc, $internalMacroRules];

(* we do this so that macros don't expand before they are defined, e.g. if they are defined in terms of other
macros, or if they have already been defined in a previous load session *)
With[{macroDefineHeads = _DefinePatternMacro | _DefineVariableMacro | _DefineSimpleMacro | _DefineMessageMacro},
  ExpandMacros[h:HoldComplete[macroDefineHeads]] := h;
  ExpandMacros[h:HoldComplete[CompoundExpression[macroDefineHeads, Null]]] := h;
];

(*************************************************************************************************)

General::expansionFailed = "Macros `` failed to expand in ``.";
checkDone[hc_] /; FreeQ[hc, ($terminatingMacroHeadsP)[___]] := hc;
checkDone[hc_] := ThrowErrorMessage["expansionFailed",
  HoldForm @@@ Select[List @@ $terminatingMacroHeadsP, !FreeQ[hc, #]&],
  NicePane @ (Global`$last = Apply[StandaloneHold, hc])
];

subHolds[hc_] := ReplaceRepeated[hc, MacroHold[e_] :> e];

subMps[hc_] /; FreeQ[hc, $MacroParentSymbol] := hc;
subMps[hc_] := subMps2 @ hc;

subMps2[hc:HoldComplete[lhs_SetDelayed | CompoundExpression[lhs_SetDelayed, Null]]] :=
  ReplaceAll[hc, $MacroParentSymbol -> getParentHead[lhs]];

subMps2[hc_] := ReplacePart[hc, Map[
  pos |-> pos -> findParentHead[pos, hc],
  Position[hc, $MacroParentSymbol]
]];

findParentHead[_,        HoldComplete[lhs_SetDelayed]] := getParentHead[lhs];
findParentHead[pos_,     hc_] := findParentHead[Drop[pos, 2], Extract[hc, Take[pos, 2], HoldComplete]];
findParentHead[{} | {_}, hc_] := ThrowErrorMessage["noMacroParent", HoldForm @ hc];
General::noMacroParent = "Could not resolve macro parent in ``.";

SetAttributes[getParentHead, HoldAllComplete];
getParentHead[SetDelayed[lhs_, _]] := MacroHold @@ Replace[PatternHeadSymbol[lhs], $Failed :>
  ThrowErrorMessage["macroParentLHS", HoldForm @ lhs]];
General::macroParentLHS = "Could not find a head symbol for the LHS of SetDelayed, being ``.";

(*************************************************************************************************)

SetAttributes[PatternHeadSymbol, HoldAllComplete];

PatternHeadSymbol::usage =
"PatternHeadSymbol[patt$] gives the symbol head (or just the symbol) which the pattern will match.";

Clear[PatternHeadSymbol];

PatternHeadSymbol[Verbatim[Verbatim][e_]]                            := PatternHeadSymbol[e];
PatternHeadSymbol[Verbatim[HoldPattern][e_]]                         := PatternHeadSymbol[e];
PatternHeadSymbol[Verbatim[Pattern][_, e_]]                          := PatternHeadSymbol[e];
PatternHeadSymbol[Verbatim[PatternTest][e_, _]]                      := PatternHeadSymbol[e];
PatternHeadSymbol[Verbatim[Condition][e_, _]]                        := PatternHeadSymbol[e];
PatternHeadSymbol[Verbatim[Blank][s_Symbol ? Developer`HoldAtomQ]]   := Hold[sym];
PatternHeadSymbol[h_[___]]                                           := PatternHeadSymbol[h];
PatternHeadSymbol[s_Symbol ? Developer`HoldAtomQ]                    := Hold[s];
PatternHeadSymbol[___]                                               := $Failed;
