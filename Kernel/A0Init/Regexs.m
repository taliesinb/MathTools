PublicFunction[ParseStringExpression]

SetUsage @ "
ParseStringExpression[pattern$] converts a string expression into a tuple of data including a regular expression string.
ParseStringExpression[rule$] does the same to a rule.
* this simply calls StringPattern`PatternConvert, the internal function that does this.
* use %DebugStringExpression to see more info.

## Return value

A 4-tuple is returned consisting of:
* the regex string
* pattern symbols, and their capture group indices
* list of conditions and pattern tests, and list of capture group indices involved in each
* unknown, something to do with to do with post conditions of a RuleDelayed maybe?


## Processing

%PatternConvert applies the sequence of replacements ():
| %expandDatePattern | macro-like expansion of %DatePattern |
| %rule0             | process %Condition and %PatternTest, recurse down first |
| %rule1             | capture pattern symbols from %Pattern, process %RegularExpression into RE% head |
| %rule1b            | find alternatives whose arms are all SingleCharacterQ, pop them in a CharacterGroup |
| %rule2             | recurse down %Repeated, %RepeatedNull |
| %rule2b            | escape regexp characters that occur outside a %RE / %CharacterGroup |
| %rule3             | parse the range spec in %Repeated, %RepeatedNull |
| %rules             | turn all remaining expressions into regex strings |

* all of these vars are in StringPattern`Dump`
* %RegularExpression capture group indices are compensated when processed

## Internal heads

* The following internal heads represent chunks of regular expression, and are finally turned into strings by %rules:
| '$$'                         | a literal match |
| %SP[$$]                       | linear chunk of other expressions |
| %RE[$$]                       | a raw regex chunp |
| %QuestionMark[$$]             | an optional chunp |
| %RepeatedObject[$$, {m$, n$}] | a repeated chunp |
| %UnnamedCapGr[$$], %CapGr[$$]  | a capture group |
| %CallOut[n$, m$]              | a callback to a pattern test |
"

ParseStringExpression[patt_] := StringPattern`PatternConvert[patt];

(* fix a bug in kernel, it doesn't recognize HexadecimalCharacter as being able to be placed inline in groups or being a single char *)
If[FreeQ[StringPattern`Dump`SingleCharInGroupRules, HexadecimalCharacter], Module[
  {correctClassRules, currentClassRuleSyms, correctClassRuleSyms},
  correctClassRules = Cases[
    StringPattern`Dump`rules,
    Rule[s_Symbol, z_String] /; StringStartsQ[z, "["] && StringEndsQ[z, "]"] && Context[s] === "System`" :>
      Rule[s, StringTake[z, {2, -2}]]
  ];
  currentClassRuleSyms = Keys @ StringPattern`Dump`SingleCharInGroupRules;
  correctClassRuleSyms = Keys @ correctClassRules;
  JoinTo[StringPattern`Dump`SingleCharInGroupRules, Normal @ KeyDrop[correctClassRules, currentClassRuleSyms]];
  Scan[Set[StringPattern`Dump`SingleCharacterQ[Verbatim[#]], True]&, correctClassRuleSyms];
]];

(* if we reload this file, we need to clear the cache *)
MTLoader`$RegexCacheDirty = True;

(**************************************************************************************************)

PublicFunction[ToRegex]

SetUsage @ "
ToRegex[patt$] converts a string pattern into a regular expression, returning a %Regex['re$'] expression.
"

ToRegex[e_] := Scope[
  re = ParseStringExpression @ e;
  If[ListQ[re], Regex @ F @ re, $Failed]
];

(**************************************************************************************************)

PublicFunction[DebugStringExpression]

SetUsage @ "
DebugStringExpression[patt$] attempts to use StringPattern`PatternConvert to convert patt$ into a regular expression.

* all steps of the conversion process are printed.
* this is otherwise the same as %ParseStringExpression.
"

DebugStringExpression[patt_] := (
  setupDebugStringExpressionDownValue[];
  DebugStringExpression @ patt
);

$debugRuleNames = HoldP[
  StringPattern`Dump`ruleHoldPattern | StringPattern`Dump`rule0 | StringPattern`Dump`expandDatePattern | StringPattern`Dump`rule1 |
  StringPattern`Dump`rule2 | StringPattern`Dump`rule2b | StringPattern`Dump`rule3 | StringPattern`Dump`rules
];

setupDebugStringExpressionDownValue[] := Module[{dv},
  Clear[DebugStringExpression];

  (* rewrite head to ourselves so the DV will work *)
  dv = DownValues[StringPattern`PatternConvert];
  dv = dv /. StringPattern`PatternConvert -> DebugStringExpression;

  (* instrument all replacements with a known rule list *)
  dv = RepRep[dv, HoldP[fn:RepAll|RepRep][in_, rule:$debugRuleNames] :>
    debugReplace[fn, in, rule]];

  (* dv = ReplaceAll[dv, HoldPattern[Set[l_, r_]] :> Set[l, debugSet[l, r]]]; *)

  dv = RepAll[dv, StringPattern`Dump`explosionDetector -> dummyExplosionDetector];

  dv = RepAll[dv, HoldP @ If[StrQ[StringPattern`Dump`res], t_, f_] :>
    If[StrQ[StringPattern`Dump`res], t, indentPrint[Style["res was not a string!", Red]]; f]
  ];

  (* instrument the entire body *)
  dv = Rep[dv, $outerDebugRewrite, {1}];

  DownValues[DebugStringExpression] = dv;
];

SetHoldAll[dummyExplosionDetector];
_dummyExplosionDetector := False;

$outerDebugRewrite =
  HoldP[RuleDelayed[lhs_, rhs_]] :>
  RuleDelayed[lhs, Module[{result},
    indentBody[
      indentPrint["PatternConvert[", MsgForm[StringPattern`Dump`patt, 5, 10], "]"],
      result = rhs,
      indentPrint["PatternConvert[...] = ", result]
    ]]
  ];

SetInitialValue[$pcPatched, False];
If[!$pcPatched,
  (* ensure macro definitions recurse *)
  Unprotect[StringPattern`PatternConvert];
  DownValues[StringPattern`PatternConvert] = RepAll[
    DownValues[StringPattern`PatternConvert],
    HoldP[RepAll[s_, StringPattern`Dump`expandDatePattern]] :>
      RepRep[s, StringPattern`Dump`expandDatePattern]
  ];
  Protect[StringPattern`PatternConvert];
  $pcPatched = True;
];

(**************************************************************************************************)

SetHoldAll[debugSet, debugReplace];

debugSet[sym_, rhs_] := Module[{res = rhs},
  indentPrint[HoldSymbolName @ sym, " = ", res]; res
];

debugReplace[fn_, input_, rule_Symbol] := Module[{evalInput, res},
  evalInput = input;
  res = fn[evalInput, rule];
  indentPrint[
    MsgForm[evalInput, 5, 10], If[fn === RepAll, " /.  ", " //. "], SPadRight[HoldSymbolName @ rule, 17],
    " = ", MsgForm[res, 5, 10]
  ];
  res
];

(**************************************************************************************************)

SetHoldAllComplete[indentBody];

indentBody[pre_, body_, post_] :=
  WithLocalSettings[
    pre,
    Block[{$depth = $depth + 1}, body],
    post
  ];

$depth = 0;
indentPrint[args___] := Print[SRepeat["\t", $depth], args];

(**************************************************************************************************)

(*

these variables contain common rules and things:
  $RegExpSpecialCharactersInGroup  how to escape literal characters in a CharacterGroup that have special meaning in REs
  SingleCharInGroupRules           how to expand symbolic heads into their corresponding lists of characters, which won't be escaped
  $StringPatternObjects            list of literal heads for ruleHoldPattern, seems insignificant
  SingleCharacterQ                 test whether an expression counts as a single character, alternatives of these are gathered by rule1b to form a compound CharacterGroup

PatternTest get turned into CallOut expressions, and corresponding callout var indexes into the conditions list

the following internal heads are used:
  SP                    represents tree of regex chunks
  RE                    a raw regex chunk
  String                a literal match
  QuestionMark          an optional chunk
  RepeatedObject        a repeated chunk
  UnnamedCapGr, CapGr   a capture group
  CallOut[n, m]:        a callback to a pattern test

$AdditionalLetters: private unicode ranges considered to be letters
  StringExpression          same as SP
  PatternSequence           same as SP
  CallOut[n, s]             calls a callback: s(?Cn) to implement a pattern test or condition
  Shortest[s_]              same as SP but with Quantifier set to "?"
  Longest[s_]               same as SP but with Quantifier set to to nothing
  SP                        string joins
  CharacterGroup            apply $RegExpSpecialCharactersInGroup and turn into []
  Except                    positive lookahead that it doesn't match the specified thing
  RE                        applies NCG, which wraps the RE in (?: ) unless it is a "\1" group reference or short just a single character
  UnnamedCapGr, CapGR       wraps in a ()
  AnyOrder[...]             creates alternatives of all the terms
  CaseSensitive[...]        makes the internal chunk case sensitive
  BackRef[n]                turn into a \g{n}
  RepeatedObject[...]       turns into a a "...n" or "...{n,m}"
  FixedOrder[...]           just emit the things in their natural order, dropping empty strings
  Blank[]                   .
  BlankSequence[]           .+ $QUANTIFIER
  BlankNullSequence[]       .* $QUANTIFIER
  s_String                  s
  QuestionMark[...]         (?:...)? $QUANTIFIER

character and span literals include these and their Except equivalents:
  WordBoundary                \w / \W
  DigitCharacter              \d / \D
  LetterCharacter             :alpha: with $AdditionalLetters
  LetterQ                     :alpha: with    $AdditionalLetters
  WordCharacter               :alnum: with $AdditionalLetters
  PunctuationCharacter        :punct:
  HexadecimalCharacter        :xdigit:
  WhitespaceCharacter         \s / \S
  Whitespace                  \s+ $QUANTIFIER
  NumberString                (?:(?:\+|-)?(?:\d+(?:\.\d* )?|\.\d+))
  StartOfLine                 ^  / (?!^)
  EndOfLine                   $  / (?!$)
  StartOfString               \A / (?!\A)
  EndOfString                 \z / (?!\z)

*)

PublicSpecialFunction[EvalStrExp]

PrivateStringPattern[RawStrExp, RawRegex, RawMaybe, RawLetterClass, RawRECall, RawREGroup, RawREAnonGroup, RawREBackRef]

RawRegex       = StringPattern`Dump`RE;
RawStrExp      = StringPattern`Dump`SP;
RawMaybe       = StringPattern`Dump`QuestionMark;
RawLetterClass = StringPattern`Dump`CharacterGroup;
RawRECall      = StringPattern`Dump`CallOut;
RawREGroup     = StringPattern`Dump`CapGr;
RawREAnonGroup = StringPattern`Dump`UnnamedCapGr;
RawREBackRef   = StringPattern`Dump`BackRef;

PrivateVariable[$RawREHeads]

$RawREHeads = {RawRegex, RawStrExp, RawMaybe, RawLetterClass, RawRECall, RawREGroup, RawREAnonGroup, RawREBackRef}

(**************************************************************************************************)

SetInitialValue[$cspRawHeadQ, UAssoc[]];
SetInitialValue[$cspMacroHeadQ, UAssoc[]];

getHead[head_Symbol[___]] := head;
getHead[head_Symbol] := head;

cspRawQ[e_] := Lookup[$cspRawHeadQ, getHead @ e, False];
cspMacroQ[e_] := Lookup[$cspMacroHeadQ, getHead @ e, False];

(**************************************************************************************************)

PrivateVariable[$StrExpInternalRules, $StrExprMacroRules, $StrExprRawRules]

SetInitialValue[$StrExprMacroRules, {}];
SetInitialValue[$StrExprRawRules, {}];

$StrExpInternalRules := StringPattern`Dump`rules;

PrivateFunction[RawSingleLetterQ]

RawSingleLetterQ = StringPattern`Dump`SingleCharacterQ;

(**************************************************************************************************)

PublicFunction[LetterClassQ, SymbolicLetterClassQ]

LetterClassQ[s_Str]        := SingleCharQ[s];
LetterClassQ[e_]           := SymbolicLetterClassQ[e];

SymbolicLetterClassQ[_LetterClass | _ExceptLetterClass] := True;
SymbolicLetterClassQ[_Str]                              := False;
SymbolicLetterClassQ[e_]                                := RawSingleLetterQ[e];

(**************************************************************************************************)

PublicFunction[LetterClassUnion, LetterClassComplement]

SetAttributes[LetterClass, Flat];

SetAttributes[LetterClassUnion, Orderless];

LetterClassUnion[a_, $Failed] := $Failed;

LetterClassUnion[_LetterClass, _ExceptLetterClass] := $Failed;

LetterClassUnion[a_LetterClass, b_LetterClass] := Join[a, b];

LetterClassUnion[a_ExceptLetterClass, b_ExceptLetterClass] := Join[a, b];

LetterClassComplement[a_LetterClass, b_LetterClass] := Scope[
  z = Complement[a, b];
  ToCompactLetterClass @ SJoin @ Complement[ExpandLetterClass @ z, ExpandLetterClass @ b]
];

(**************************************************************************************************)

PublicFunction[ToLetterClass, ToExceptLetterClass, LetterClassNegate]

ToLetterClass = Case[
  s_Str                      := LetterClass @ SRep[s, StringPattern`Dump`$RegExpSpecialCharactersInGroup];
  l_LetterClass              := l;
  l_ExceptLetterClass        := l;
  s_ ? SymbolicLetterClassQ  := LetterClass @ s;
  V`Except[s_ ? SymbolicLetterClassQ] := LetterClassNegate @ % @ s;
];

ToExceptLetterClass[l_] := LetterClassNegate @ ToLetterClass @ l;

LetterClassNegate = Case[
  LetterClass[s__]              := ExceptLetterClass[s];
  ExceptLetterClass[s__]        := LetterClass[s];
  s_ ? SymbolicLetterClassQ     := Except[s];
  _                             := $Failed;
];


(**************************************************************************************************)

PublicFunction[StringPatternCanContainQ]

SetUsage @ "
StringPatternCanContainQ[patt$, 'chars$'] checks if patt$ could match a string that contains one of 'chars$'.
* it will never produce false negatives, only false positives.
* it decompiles regular expressions via %FromRegex to be more accurate. This is cached.
"

(* TODO: i don't think this handles CaseInsensitive properly! *)

StringPatternCanContainQ[_, ""] := False;

StringPatternCanContainQ[patt_, needle_Str] :=
  doRecursivePatternTest[patt, needle, SContainsQ, Scan];

(**************************************************************************************************)

PublicFunction[StringPatternCanStartQ, StringPatternCanEndQ]

SetUsage @ "StringPatternCanStartQ[patt$, 'chars$'] checks if patt$ could match a string starting with any of 'chars$'."
SetUsage @   "StringPatternCanEndQ[patt$, 'chars$'] checks if patt$ could match a string ending with any of 'chars$'."

StringPatternCanStartQ[p_, n_] := doRecursivePatternTest[p, n, SStartsQ, seqCanStartEnd[Id]];
StringPatternCanEndQ[p_, n_]   := doRecursivePatternTest[p, n, SEndsQ,   seqCanStartEnd[Rev]];

seqCanStartEnd[pre_][fn_, seq_] := seqRec[fn, Sequence @@ pre[seq]];

(* check for a nullable patern, which requires us to look further *)
seqRec[fn_, p_ ? NullableStringPatternQ, rest___] := (
  fn @ p;              (* if the nullable thing can match, we have a hit *)
  seqRec[fn, rest]     (* if it doesn't, we might have a match later *)
);

seqRec[f_, p_, ___] := f @ p;

seqRec[_] := Null;

(**************************************************************************************************)

General::notNeedleStr = "Needle `` should be a string.";

(* this is the driver for StringPatternCanContainQ, StringPatternCanStartQ, StringPatternCanEndQ *)
doRecursivePatternTest[patt_, needle_, strFn_, seqFn_] := Scope[
  If[!StrQ[needle], Message[General::notNeedleStr, needle]; ReturnFailed[]];
  StringPattern`Dump`$Quantifier = ""; (* so that when we expand via internal rules we don't get errors *)
  $verbose = False;
  $needle = needle;
  $needleChars = Chars @ needle;
  $strFn = strFn;
  $seqFn = seqFn;
  $seenVars = UAssoc[];
  Catch[pattDispatch @ patt; False, catchYes]
];

throwYes[] := Throw[True, catchYes];

$pattRecurse1 = _Repeated | _RepeatedNull | _Maybe | _Longest | _Shortest | _Condition | _PatternTest;
$pattNullable = _Maybe | _RawMaybe | _RepeatedNull | V`Repeated[_, {0, _}];

pattDispatch = Case[

  c_ExceptLetterClass      := % @ Except @ Apply[LetterClass, c]; (* immediately expand this *)
  l:letter                 := If[classRulesInQ[l], throwYes[]];

  V`Except[e_]             := If[!exceptRulesOutQ[e], throwYes[]];
  V`Except[e_, p_]         := If[!exceptRulesOutQ[e], % @ p];

  Avoiding[p_, a_]         := If[!avoidingRulesOutQ[a], % @ p];

  r:regex                  := % @ CachedFromRegex @ r;

  s_Str                    := If[$strFn[s, $needleChars], throwYes[]];
  s_SExpr                  := $seqFn[pattDispatch, s];

  (* if we see a backref, we've already checked its first occurence *)
  v:(V`Pattern[s_Symbol, V`Blank[]]) /; KeyQ[$seenVars, Hold[s]] := Null;
  p:(V`Pattern[s_Symbol, _]) := ($seenVars[Hold[s]] = True; % @ P2 @ p);

  e:recurseAll             := Scan[%, e];
  e:recurse1               := % @ P1 @ e;
  ignore                   := Null;
  blanks                   := throwYes[];

  e_                       := pattDispatch2 @ e;
,
  {letter     -> _ ? SymbolicLetterClassQ,
   recurseAll -> _Alternatives | _List,
   recurse1   -> $pattRecurse1,
   ignore     -> _PatternRecurse | StartOfLine | EndOfLine | StartOfString | EndOfString,
   blanks     -> Verbatim[_] | Verbatim[__] | Verbatim[___],
   regex      -> _RegularExpression | _Regex | _RawRegex
}];

(* does the Avoiding second arg rule out the needle? if so, we're safe. this will always return false
for a list of avoided strings, for example, for the same reason as exceptRulesOutQ *)
avoidingRulesOutQ = Case[
  a_Str    := SubsetQ[Chars @ a, $needleChars];
  a_       := LetterClassQ[a] && anticlassRulesOutQ[a];
];

(* does the Except first arg rule out the needle? it can only do so if its
a single char-like pattern. *)
exceptRulesOutQ = Case[
  c_Str       := {c} === $needleChars;
  cs:{__Str}  := SubsetQ[cs, $needleChars];
  e_          := avoidingRulesOutQ @ e;
];

anticlassRulesOutQ[p_] := And @@ SMatchQ[$needleChars, p];
classRulesInQ[p_]      := SContainsQ[$needle, p];

(**************************************************************************************************)

PrivateFunction[NullableStringPatternQ, AnchorStringPatternQ]

AnchorStringPatternQ = Case[
  _NegativeLookahead | _NegativeLookbehind | _PositiveLookahead | _PositiveLookbehind     := True;
  WLSymbolBoundary | WordBoundary | StartOfLine | EndOfLine | StartOfString | EndOfString := True;
  _ := False
];

NullableStringPatternQ = Case[
  p:nullable     := True;
  s_SExpr        := AllTrue[s, %];
  a_Alt | a_List := AnyTrue[a, %];
  p:recurse1     := % @ P1 @ p;
  p:recurse2     := % @ P2 @ p;
  p_             := AnchorStringPatternQ @ p;
,
  {nullable -> $pattNullable, recurse1 -> $pattRecurse1, recurse2 -> $pattRecurse2}
];

(**************************************************************************************************)

(* anchors do not contribute to the answer *)
pattDispatch2[e_ ? AnchorStringPatternQ] := Null;

(* macros don't need full expansion to raw elements, which is to be avoided because parsing REs is slow *)
pattDispatch2[e_ ? cspMacroQ] := pattDispatch @ pattApplyRule[e, $StrExprMacroRules];

(* expand the expression one step and recurse, which involves treating strings as containing REs *)
pattDispatch2[e_ ? cspRawQ] := pattDispatchRaw @ pattApplyRule[e, $StrExprRawRules];

(* deal with raw string elements directly *)
With[{rawP = Alt @@ Map[Blank, $RawREHeads]}, pattDispatch2[e:rawP] := pattDispatchRaw @ e];

(* things like Whitespace will make it down to here *)
pattDispatch2[e_Symbol] := pattDispatchRaw @ pattApplyRule[e, $StrExpInternalRules];

(* failure to expand in the above two cases means we must bail out *)
pattApplyRule[e_, rule_] := Module[{res = Rep[e, rule]}, If[res === e, throwYes[], res]];

General::unknownRawStringPatternElement = "Cannot process unknown raw pattern element: ``.";
(* TODO: handle CallOut etc, CpGr, etc. *)
pattDispatchRaw := Case[
  V`Except[e_RawLetterClass]  := If[!anticlassRulesOutQ[e], throwYes[]];
  e_RawLetterClass            := If[classRulesInQ[e], throwYes[]];
  e_RawStrExp                 := $seqFn[pattDispatchRaw, e];
  e_RawMaybe                  := % @ F @ e;
  e_Str | e_RawRegex          := pattDispatch @ CachedFromRegex @ e;
  _RawRECall                  := Null;
  _RawREBackRef               := Null;
  re_RawREGroup               := % @ P2 @ e;
  re_RawREAnonGroup           := % @ P2 @ e;
  raw_                        := (
    Message[General::unknownRawStringPatternElement, raw];
    throwYes[]
  );
];

(**************************************************************************************************)

PublicFunction[CachedFromRegex]

CacheVariable[$FromRegexCache]

CachedFromRegex[re_] := CachedInto[$FromRegexCache, re, FromRegex @ re];

(**************************************************************************************************)

PublicVariable[$RegexMetaCharacters, $RegexCharacterGroupMetaCharacters]

$RegexMetaCharacters = Chars["\\.*+?{}()[]|+^$"];
$RegexCharacterGroupMetaCharacters = Chars["-]\\"];

(**************************************************************************************************)

PublicFunction[ToCompactLetterClass]

ToCompactLetterClass[list_List] :=
  ToCompactLetterClass @ SJoin @ list;

ToCompactLetterClass[str_Str] := Scope[
  codes = Union @ ToCharacterCode @ str;
  runs = Split[codes, #2==#1+1&];
  LetterClass @ StringJoin @ Map[clcSpan, runs]
];

clcSpan[list:({_}|{_, _})] := FromCharacterCode[list];
clcSpan[{first_, __, last_}] := FromCharacterCode[{first, 45, last}];

(**************************************************************************************************)

PublicFunction[FromRegex]

PublicHead[CaptureGroup]

SetUsage @ "
FromRegex['re$'] parses a regular expression back into a StringExpression.

* capture groups are emitted as %CaptureGroup[$$], which otherwise has no meaning.
"

Options[FromRegex] = {Verbose -> False};

FromRegex::parseRegexFail = "Parse failed: ``."
FromRegex::nomatch = "Cannot proceed because no rules matched in state ``. Remaining string: ``."

FromRegex[re_Str | Regex[re_Str] | RegularExpression[re_String], OptionsPattern[]] := Scope @ CatchMessage[
  UnpackOptions[$verbose];
  If[StringStartsQ[re, "(?ms)"], re = StringDrop[re, 5]];
  If[StringFreeQ[re, $RegexMetaCharacters], Return @ re];
  If[StringStartsEndsQ[re, "[", "]"] && StringCount[re, {"[", "]"}] === 2,
     Ret @ parseSimpleCharClass @ StringTake[re, {2, -2}]];
  $result = $seq[];
  $cursor = {1};
  $state = $normalState;
  remaining = re;
  While[remaining =!= "",
    rules = $parsingRules[$state];
    case = F[SCases[remaining, rules, 1], Msg::nomatch[SymbolName @ $state, remaining]];
    {match, action} = case;
    VPrint @ Style[Row[{
      "state = ", SPadRight[SymbolName @ $state, 15],
      "cursor = ", Pane[$cursor, 80],
      "matched = ", Pane[MsgForm @ match, 120],
      "action = ", Pane[MsgForm @ action, 300],
      "result = ", Pane[MsgForm @ $result, 500]}], LineBreakWithin -> False];
    remaining = SDrop[remaining, SLen @ match];
    runAction @ action
  ];
  $result /. ($seq -> SExpr) //. $simplificationRules /. flatAlt -> Alt /. {
    LetterClass[s_ ? SingleCharQ]       :> s,
    ExceptLetterClass[s_ ? SingleCharQ] :> Except[s]
  } /. Verbatim[SExpr][a_] :> a
];

SetAttributes[flatAlt, {Flat, OneIdentity}];

$simplificationRules = {
  (h:LetterClass|ExceptLetterClass)[SExpr[a___]]:> h[a],
  SExpr[l___, $infixAlt, r___] -> flatAlt[SExpr[l], SExpr[r]]
}

runAction = Case[
  Null := Null;
  enterState[state_, head_] := ($state = state; $result //= Insert[head[$seq[]], $cursor]; JoinTo[$cursor, {1, 1}]);
  leaveState[]              := ($cursor = Drop[$cursor, -2]; $cursor[[-1]]++; $state = $normalState);
  switchState[state_]       := ($state = state);
  emitToken[token_]         := ($result //= Insert[token, $cursor]; $cursor[[-1]]++);
  applyToToken[fn_]         := ($result //= MapAt[fn, MapAt[# - 1&, $cursor, -1]]);
  failParse[msg_]           := Msg::parseRegexFail[RawMsgForm @ msg]
];

toLHSPrefixedRule[lhs_ :> rhs_] := $prefix:(StartOfString ~~ lhs) :> {$prefix, rhs};
defineParsingRules[state_, rules___RuleDelayed] := (
  $parsingRules[state] = Map[toLHSPrefixedRule, {rules}];
);
_defineParsingRules := BadArguments[];

$specialChars = {"d", "D", "w", "W", "s", "S", "b", "B", "A", "z"};
$escapedChars = {"(", ")", "[", "]", "{", "}", "\\", ".", "?"};

(**************************************************************************************************)

PublicFunction[FromRegex, FromRegexSimple]

FromRegexSimple[re_] /; StringStartsQ[re, "(?ms)"] := FromRegexSimple @ StringDrop[re, 5];

FromRegexSimple[re_] := Scope[
  If[StringFreeQ[re, $RegexMetaCharacters], Return @ re];
  If[StringCount[re, {"[", "]"}] =!= 2, ReturnFailed[]];
  If[!StringStartsQ[re, "["], ReturnFailed[]];
  Which[
    StringEndsQ[re, "]"],
      parseSimpleCharClass @ StringTake[re, {2, -2}],
    StringEndsQ[re, "]*"],
      RepeatedNull @ parseSimpleCharClass @ StringTake[re, {2, -3}],
    StringEndsQ[re, "]+"],
      Repeated @ parseSimpleCharClass @ StringTake[re, {2, -3}],
    True,
      $Failed
  ]
];

parseSimpleCharClass["[]"] := "";
parseSimpleCharClass[re_] := If[StringTake[re, 1] === "^",
  ExceptLetterClass @ StringDrop[re, 1],
  LetterClass @ re
];

(**************************************************************************************************)

(* TODO: recognize existing symbolic letter classes *)

defineParsingRules[$normalState,

  "(?" ~~ f:{"i","m","s"}.. ~~ ")"       :> If[SContainsQ[f,"i"], enterState[$normalState, CaseInsensitive], Null],
  "(?-i)"                                :> leaveState[],

  "(?:"                                  :> enterState[$normalState, $seq],
  "(?<!"                                 :> enterState[$normalState, NegativeLookbehind],
  "(?<="                                 :> enterState[$normalState, PositiveLookbehind],
  "(?!"                                  :> enterState[$normalState, NegativeLookahead],
  "(?="                                  :> enterState[$normalState, PositiveLookahead],

  "\\" ~~ c:$specialChars                :> emitToken[$specialCharToExpr[c]],
  "\\" ~~ c:$escapedChars                :> emitToken[c],

  "("                                    :> enterState[$normalState, CaptureGroup],

  "[[:" ~~ c:LowercaseLetter.. ~~ ":]]"  :> emitToken[parseCharClass[c]],
  "[^[:" ~~ c:LowercaseLetter.. ~~ ":]]" :> emitToken[Except @ parseCharClass[c]],

  "[^"                                   :> enterState[$classState, ExceptLetterClass],
  "["                                    :> enterState[$classState, LetterClass],

  "{" ~~ m:DigitCharacter.. ~~ "}"                              :> applyToToken[Repeated[#, FromDigits @ m]&],
  "{" ~~ m:DigitCharacter.. ~~ "," ~~ n:DigitCharacter.. ~~ "}" :> applyToToken[Repeated[#, FromDigits /@ {m, n}]&],
  "{"                                    :> failParse["unexpected {"],

  "|"                                    :> emitToken[$infixAlt],

  "??"                                   :> applyToToken[Maybe /* Shortest],
  "*?"                                   :> applyToToken[RepeatedNull /* Shortest],
  "+?"                                   :> applyToToken[Repeated /* Shortest],
  ".*"                                   :> emitToken[___],
  ".+"                                   :> emitToken[__],
  "."                                    :> emitToken[_],
  "?"                                    :> applyToToken[Maybe],
  "*"                                    :> applyToToken[RepeatedNull],
  "+"                                    :> applyToToken[Repeated],
  ")"                                    :> leaveState[],

  "^"                                    :> emitToken[StartOfLine],
  "$"                                    :> emitToken[EndOfLine],

  "\n"                                   :> emitToken[Newline],
  t_                                     :> emitToken[t]
];

$specialCharToExpr = <|
  "d" -> DigitCharacter,      "D" -> Except[DigitCharacter],
  "w" -> WordCharacter,       "W" -> Except[WordCharacter],
  "s" -> WhitespaceCharacter, "S" -> Except[WhitespaceCharacter],
  "A" -> StartOfString,       "z" -> EndOfString,
  "b" -> WordBoundary,        "B" -> Except[WordBoundary]
|>;

$charClassNames = UAssoc[
  "upper" -> UppercaseLetter,
  "lower" -> LowercaseLetter,
  "digit" -> DigitCharacter,
  "alnum" -> AlphanumericCharacter,
  "alpha" -> RomanLetter,
  "blank" -> {" ", "\t"},
  "punct" -> PunctuationCharacter,
  "space" -> WhitespaceCharacter,
  "xdigit"-> HexadecimalCharacter
];

FromRegex::badcharclass = "\"``\" is not a known char class.";
parseCharClass[name_] := LookupMsg[$charClassNames, name];

defineParsingRules[$classState,
  "[:" ~~ c:LowercaseLetter.. ~~ ":]" :> emitToken[parseCharClass @ c],
  "\\^"  :> emitToken["\\^"],
  "\\-"  :> emitToken["\\-"],
  "\\\\" :> emitToken["\\"],
  "\\["  :> emitToken["\\["],
  "\\]"  :> emitToken["\\]"],
  "]"    :> leaveState[],
  o_     :> emitToken[o]
];

(**************************************************************************************************)

PublicFunction[RegexEscape]

SetUsage @ "
RegexEscape['str$'] escapes characters in str$ that have special meaning in regular expressions.
"

RegexEscape[re_] := SRep[re, StringPattern`Dump`$RegExpSpecialCharacters];

(**************************************************************************************************)

PrivateSpecialFunction[DefineStringLetterClass]

SetUsage @ "
DefineStringLetterClass[sym$ -> 'group$'] defines a string letter, effectively as %Regex['[group$]'].
DefineStringLetterClass[expr$ :> rhs$] defines a parameterized letter class, RHS should evaluate to a string.
DefineStringLetterClass[rule$1, rule$2, $$] defines multiple classes at once.
* the special characters `-]^` should be manually escaped if they should appear as literals.
"

(* TODO: why do i have the outer inner distinction it doens't seem to be used anywhere? *)

DefineStringLetterClass = Case[

  sym_Symbol -> str_Str :=
    %[sym -> {If[SingleCharQ[str], str, "[" <> str <> "]"], "[^" <> str <> "]", str}];

  sym_Symbol -> {outer_, outerNeg_, inner_} := (
    StringPattern`Dump`SingleCharInGroupRules //= appUpdateRule[sym -> inner];
    StringPattern`Dump`SingleCharacterQ[Verbatim[sym]] = True;
    StringPattern`Dump`rules //= preUpdateRule[sym -> outer];
    StringPattern`Dump`rules //= preUpdateRule[HoldPattern[Except][sym] -> outerNeg];
    StringPattern`Dump`$StringPatternObjects //= appendIfAbsent[Hold @ sym];
  );

  lhs_ :> rhs_ := With[
    {lhs2 = toSimpleLHS @ HoldPattern @ lhs},
    StringPattern`Dump`SingleCharInGroupRules //= appUpdateRule[HoldPattern[lhs] :> rhs];
    StringPattern`Dump`SingleCharacterQ[lhs2] = True;
    StringPattern`Dump`rules //= preUpdateRule[HoldPattern[lhs] :> "[" <> rhs <> "]"];
    StringPattern`Dump`rules //= preUpdateRule[HoldPattern[Except][HoldPattern[lhs]] :> "[^" <> rhs <> "]"];
  ];

  Sequence[rules__] := Scan[%, {rules}];
];

toSimpleLHS[l_] := l //. Verbatim[Pattern][_, p_] :> p;

(**************************************************************************************************)

PrivateSpecialFunction[DefineStringPattern]

SetUsage @ "
DefineStringPattern[lhs$ :> rhs$] defines a string pattern that applies at the end of pattern-to-regex processing.
DefineStringPattern[rule$1, rule$2, $$] defines several patterns.

* if the rhs$ contains a string that contains the character '□' it is treated as a span of characters except newline.
* the rule is applied at the final stage of StringPattern`PatternConvert, and can emit raw expressions:
| %EvalStrExp[$$]          | rewrite $$ immediately via the same final rules |
| %RawStrExp[e$1, e$2, $$] | a linear chain of sub-results |
| %RawRegex[$$]            | a regex without further interpretation |
| '$$'                     | a string placed into the final regex without further processing |
"

DefineStringPattern = Case[
  rule_RuleDelayed := Module[
    {rule2 = MapAt[HoldP, rule /. $spDeclarationRules, 1], head = F @ DefinitionHead @ rule},
    $cspRawHeadQ[head] = True;
    $StrExprRawRules         //= appUpdateRule[rule2];
    StringPattern`Dump`rules //= appUpdateRule[rule2];
  ];
  Sequence[rules__] := Scan[%, {rules}];
];

$spDeclarationRules = {
  s_Str /; SContainsQ[s, "□"] :> RuleEval @ SRep[s, "□" :> "[^\n]+?"],
  EvalStrExp[s_]                   :> RepRep[s, StringPattern`Dump`rules],
  RawStrExp                        -> StringPattern`Dump`SP,
  RawRegex                         -> StringPattern`Dump`RE
}

(**************************************************************************************************)

PrivateSpecialFunction[DefineStringPatternMacro]

SetUsage @ "
DefineStringPatternMacro[lhs$ :> rhs$] defines a string pattern that applies at the start of pattern-to-regex processing.
DefineStringPattern[rule$1, rule$2, $$] defines several patterns.

* the rule is applied before symbol captures, conditions, etc. are evaluated.
* it is appropriate for meta-patterns that wish to introducing new bindings, or repeat other patterns.
"

DefineStringPatternMacro = Case[
  rule_RuleDelayed := With[
    {rule2 = MapAt[HoldP, rule, 1], head = F @ DefinitionHead @ rule},
    If[!ListQ[StringPattern`Dump`expandDatePattern], StringPattern`Dump`expandDatePattern //= List];
    $cspMacroHeadQ[head] = True;
    $StrExprMacroRules                   //= appUpdateRule[rule2];
    StringPattern`Dump`expandDatePattern //= appUpdateRule[rule2];
  ];
  Sequence[rules__] := Scan[%, {rules}];
]

(**************************************************************************************************)

PublicSpecialFunction[ClearRegexCache]

SetUsage @ "
ClearRegexCache[] clears the kernel's internal regular expression cache, which would otherwise cause the pattern-to-regex pipeline to skip recomputations.
"

ClearRegexCache[] := ClearSystemCache["RegularExpression"];

(**************************************************************************************************)

appendIfAbsent[item_][list_] :=
  If[MemberQ[list, Verbatim @ item], list, App[list, item]];

preUpdateRule[rule_][list_] := updateRule[list, rule, Pre];
appUpdateRule[rule_][list_] := updateRule[list, rule, App];

updateRule[list_, rule:(_[lhs_, rhs_]), fn_] := Module[{pos, newList},
  pos = Position[list, (Rule|RuleDelayed)[Verbatim[lhs], _], {1}];
  If[pos === {},
    fn[list, rule]
  ,
    newList = RepPart[list, pos -> rule];
    If[newList =!= list, MTLoader`$RegexCacheDirty = True];
    newList
  ]
];

(**************************************************************************************************)

PublicFunction[ExpandLetterClass]

ExpandLetterClass[s:(_LetterClass|_ExceptLetterClass)] :=
  Flatten @ Map[ExpandLetterClass, List @@ s];

ExpandLetterClass[s_Str] :=
  Chars @ SRep[a_ ~~ "-" ~~ b_ :> SJoin[CharRange[a, b]]] @ SRep[$classTranslations] @ s;

ExpandLetterClass::expandFailed = "Failed to expand ``.";

ExpandLetterClass[e_] := Scope[
  res = Rep[e, StringPattern`Dump`SingleCharInGroupRules];
  If[!StringQ[res], ReturnFailed["expandFailed", e]];
  ExpandLetterClass @ res
];

(**************************************************************************************************)

PublicFunction[ExpandPosixLetterClasses]

ExpandPosixLetterClasses[expr_] :=
  expr /. str_Str :> RuleEval @ SRep[str, $classTranslations];

$classTranslations = {
  "\\x{" ~~ h:Repeated[HexadecimalCharacter, 4] ~~ "}" :> FromCharCode @ FromHexStr @ h,
  "[:upper:]" -> "A-Z",
  "[:lower:]" -> "a-z",
  "[:digit:]" -> "0-9",
  "[:alnum:]" -> "A-Za-z0-9", (* TODO: RE engine actually matches much more ! *)
  "[:alpha:]" -> "A-Za-z",
  "[:blank:]" -> " \t",
  "[:punct:]" -> "!\"#$%&'()*+,-./:;>=<?@\\\\\\[\\]^_`{|}~",
  "[:space:]" -> "\n\t\r ",
  "[:xdigit:]" -> "0-9A-Fa-f"
}

(**************************************************************************************************)

PublicStringPattern[Regex]

DefineStringPatternMacro[
  Regex[s_Str] :> RegularExpression[s]
]

