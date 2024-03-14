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
Avoiding[patt$, {'str$1', 'str$2', $$}] avoids the literal strings 'str$i'.
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
  Avoiding[V`Pattern[s_Symbol, p_], a_] :=
    Construct[Pattern, Uneval @ s, Avoiding[p, a]];

  (* no binding *)
  Avoiding[Verbatim[_]|Verbatim[__], a:Except[_List]] := ToExceptLetterClass[a]..;
  Avoiding[Verbatim[___], a:Except[_List]]            := ToExceptLetterClass[a]...;

  (* avoid a list of literals *) (* TODO: what about a construct for anonymous tests, like PatternTest but for arb groups? *)
  Avoiding[p_, a:{__Str}]   := Module[{z}, Construct[Condition, Construct[Pattern, z, p], Uneval @ SFreeQ[z, a]]];

  (* avoid a set of characters. if we know the interior pattern cannot match the forbidden characters,
  we can avoid introducing a callback to check that fact. note: the ? causes chars to be checked one
  at a time against the avoid string. *)
  Avoiding[p_, a_]                       := expandAvoiding2[p, a];

  a_ := (
    Message[Avoiding::invalidStringPatternUsage, a];
    $Failed
  )
];

(*
strategy: fully expand the interior, and then augment any character classes inside it as appropriate:
letter classes will have chunks removed (if they are positive) or added (if they are negative).
if we encounter a literal string that can match, the entire Avoid can never match
because we can't change that literal string.
*)

expandAvoiding2[patt_, a_] := Scope[

  $n = ToExceptLetterClass @ a;
  $p = ToLetterClass @ a;
  If[FailureQ[$n], Message[Avoiding::invalidStringPatternUsage, Avoiding[patt, a]]; ReturnFailed[]];

  (* expand the interior pattern so we can try rewrite character groups to avoid the given a *)

  patt = RepRep[patt, $StrExprMacroRules];

  $seenVars = UAssoc[];
  patt = avoidRewrite[patt];

  If[SymbolicLetterClassQ[a], a = SJoin @ ExpandLetterClass @ a];
  If[!StringPatternCanContainQ[patt, a],
    patt,
    (* if we fail, resort to a callback *)
    If[StringQ[a],
      patt ? (SFreeQ[a, #]&),
      patt ? (StringMatchQ[a])
    ]
  ]
];

avoidRewrite = Case[
  s_Str                      := s;

  (* macro rules will expand LetterClass to RawLetterClass *)
  r_RawLetterClass           := % @ Apply[LetterClass, r];
  V`Except[r_RawLetterClass] := % @ Apply[ExceptLetterClass, r];
  n_ExceptLetterClass        := OnFailed[LetterClassUnion[n, $n], n];
  p_ ? SymbolicLetterClassQ  := OnFailed[LetterClassComplement[ToLetterClass @ p, $p], p];

  (* avoid rewriting a backref var *)
  v:(V`Pattern[s_Symbol, V`Blank[]]) /; KeyQ[$seenVars, Hold[s]] := v;
  p:(V`Pattern[s_Symbol, _]) := ($seenVars[Hold[s]] = True; MapAt[%, p, 2]);

  V`Blank[]                  := $n;
  V`BlankSequence[]          := $n..;
  V`BlankNullSequence[]      := $n...;

  e:recurseAll               := Map[%, e];
  e:recurse1                 := MapAt[%, e, 1];
  e:ignore                   := e;
  re:regex                   := With[{res = FromRegexSimple[re]}, % @ res /; res =!= $Failed];

  Whitespace                 := % @ Repeated[WhitespaceCharacter];
  other_                     := other;
,
  {recurseAll -> _Alternatives | _List | _SExpr,
   recurse1   -> _Repeated | _RepeatedNull | _Maybe | _Longest | _Shortest | _Condition | _PatternTest,
   ignore     -> _PatternRecurse | StartOfLine | EndOfLine | StartOfString | EndOfString,
   blanks     -> Verbatim[_] | Verbatim[__] | Verbatim[___]
}];

toExceptClass = Case[
  s_Str                         := ExceptLetterClass[s];
  LetterClass[s__]              := ExceptLetterClass[s];
  ExceptLetterClass[s__]        := LetterClass[s];
  s_ ? LetterClassQ             := Except[s];
  _                             := $Failed;
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
  Maybe[p_]              :> RawMaybe @ EvalStrExp @ p,
  NegativeLookbehind[p_] :> RawStrExp["(?<!", EvalStrExp @ p, ")"],
  PositiveLookbehind[p_] :> RawStrExp["(?<=", EvalStrExp @ p, ")"],
  NegativeLookahead[p_]  :> RawStrExp["(?!", EvalStrExp @ p, ")"],
  PositiveLookahead[p_]  :> RawStrExp["(?=", EvalStrExp @ lhs, ")"]
];

