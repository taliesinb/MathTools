PublicStringPattern[Riffled]

DefineStringPatternMacro[
  Riffled[a_, sep_] :> a ~~ RepeatedNull[sep ~~ a]
];

(**************************************************************************************************)

PublicStringPattern[PatternRecurse]

SetUsage @ "
PatternRecurse[sym$] recurses into the group which contains sym$.
"

SetHoldFirst[PatternRecurse, parseRecursing];

DefineStringPattern[PatternRecurse[sym_Symbol] :> parseRecursing[sym]];

PatternRecurse::symbolNotBound = "`` is not bound at this point in the string expression.";

parseRecursing[var_] := Scope[
  pos = IndexOf[StringPattern`Dump`vars, Hold @ var];
  If[!IntQ[pos], ReturnFailed[PatternRecurse::symbolNotBound, HoldForm @ var]];
  "(?" <> IntStr[pos] <> ")"
];

(**************************************************************************************************)

PublicStringPattern[Avoiding]

SetUsage @ "
Avoiding[patt$, 'chars$'] matches patt$, as long as the match does not contain any of the chars$.
Avoiding[patt$, class$] specifies a character class such as LetterCharacter.
Avoiding[patt$, {'str$1', 'str$2', $$} avoids the literal strings 'str$i.'.
* Avoiding compiles to a simple regular expression where possible.
* Avoiding must validate matches with a callback to when patt$ is complex, or a list of strings to avoid is given.
* Avoiding[_, $$] matches a non-empty string.
"

General::invalidStringPatternUsage = "`` is an invalid usage.";

DefineStringPatternMacro[
  a_Avoiding :> expandAvoiding[a]
];

expandAvoiding = Case[

  (* bind a pattern var *)
  Avoiding[Verbatim[Pattern][s_Symbol, p_], a_] :=
    Construct[Pattern, Uneval @ s, Avoiding[p, a]];

  Avoiding[Verbatim[_]|Verbatim[__], a:(_Str | _Symbol)] := ExceptLetterClass[a]..;
  Avoiding[Verbatim[___], a:(_Str | _Symbol)]            := ExceptLetterClass[a]...;

  (* complex cases: *)

  (* avoid a character class. if we know the interior pattern cannot match the forbidden characters,
  we can avoid introducing a callback to check that fact *)
  Avoiding[p_, a_Str]     := If[StringPatternCanContainCharsQ[p, a], p ? (SFreeQ[a, #]&), p];

  (* avoid specific strings *)
  Avoiding[p_, a:{__Str}] := p ? (SFreeQ[#, a]&);

  a_ := (
    Message[Avoiding::invalidStringPatternUsage, MsgExpr @ a];
    $Failed
  )
];

(**************************************************************************************************)

PublicStringPattern[CaseInsensitive, Maybe, NegativeLookbehind, NegativeLookahead, PositiveLookbehind, PositiveLookahead]

SetUsage @ "CaseInsensitive[p$] matches p$ without case sensitivity.";
SetUsage @ "Maybe[p$] matches 0 or 1 copies of p$.";
SetUsage @ "NegativeLookahead[p$] matches if p$ does not follow the cursor.";
SetUsage @ "NegativeLookbehind[p$] matches if p$ does not precede the cursor.";
SetUsage @ "PositiveLookahead[p$] matches if p$ follows the cursor.";
SetUsage @ "PositiveLookbehind[p$] matches if p$ precedes the cursor.";

DefineStringPattern[
  CaseInsensitive[p_]    :> RawStrExp["(?i)", EvalStrExp @ p, "(?-i)"],
  Maybe[p_]              :> StringPattern`Dump`QuestionMark @ EvalStrExp @ p,
  NegativeLookbehind[p_] :> RawStrExp["(?<!", EvalStrExp @ p, ")"],
  PositiveLookbehind[p_] :> RawStrExp["(?<=", EvalStrExp @ p, ")"],
  NegativeLookahead[p_]  :> RawStrExp["(?!", EvalStrExp @ p, ")"],
  PositiveLookahead[p_]  :> RawStrExp["(?=", EvalStrExp @ lhs, ")"]
];

