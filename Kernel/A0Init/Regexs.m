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
| %rule2             | recurse down %Repeated, %RepeatedNull |
| %rule2b            | escape regexp characters that occur in %RE / %CharacterGroup |
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

(**************************************************************************************************)

PublicFunction[ToRegex]

ToRegex[e_] := Scope[
  re = ParseStringExpression @ e;
  If[ListQ[re], Regex @ P1 @ re, $Failed]
];

(**************************************************************************************************)

PublicFunction[DebugStringExpression]

DebugStringExpression[patt_] := (
  setupDebugStringExpressionDownValue[];
  DebugStringExpression @ patt
);

$debugRuleNames = HoldPattern[
  StringPattern`Dump`ruleHoldPattern | StringPattern`Dump`rule0 | StringPattern`Dump`expandDatePattern | StringPattern`Dump`rule1 |
  StringPattern`Dump`rule2 | StringPattern`Dump`rule2b | StringPattern`Dump`rule3 | StringPattern`Dump`rules
];

setupDebugStringExpressionDownValue[] := Module[{dv},
  Clear[DebugStringExpression];

  (* rewrite head to ourselves so the DV will work *)
  dv = DownValues[StringPattern`PatternConvert];
  dv = dv /. StringPattern`PatternConvert -> DebugStringExpression;

  (* instrument all replacements with a known rule list *)
  dv = ReplaceRepeated[dv, HoldPattern[fn:ReplaceAll|ReplaceRepeated][in_, rule:$debugRuleNames] :>
    debugReplace[fn, in, rule]];

  (* dv = ReplaceAll[dv, HoldPattern[Set[l_, r_]] :> Set[l, debugSet[l, r]]]; *)

  dv = ReplaceAll[dv, StringPattern`Dump`explosionDetector -> dummyExplosionDetector];

  dv = ReplaceAll[dv, HoldPattern @ If[StringQ[StringPattern`Dump`res], t_, f_] :>
    If[StringQ[StringPattern`Dump`res], t, indentPrint[Style["res was not a string!", Red]]; f]
  ];

  (* instrument the entire body *)
  dv = Replace[dv, $outerDebugRewrite, {1}];

  DownValues[DebugStringExpression] = dv;
];

SetHoldAll[dummyExplosionDetector];
_dummyExplosionDetector := False;

$outerDebugRewrite =
  HoldPattern[RuleDelayed[lhs_, rhs_]] :>
  RuleDelayed[lhs, Module[{result},
    indentBody[
      indentPrint["PatternConvert[", MsgExpr[StringPattern`Dump`patt, 5, 10], "]"],
      result = rhs,
      indentPrint["PatternConvert[...] = ", result]
    ]]
  ];

SetInitialValue[$pcPatched, False];
If[!$pcPatched,
  (* ensure macro definitions recurse *)
  Unprotect[StringPattern`PatternConvert];
  DownValues[StringPattern`PatternConvert] = ReplaceAll[
    DownValues[StringPattern`PatternConvert],
    HoldPattern[ReplaceAll[s_, StringPattern`Dump`expandDatePattern]] :>
      ReplaceRepeated[s, StringPattern`Dump`expandDatePattern]
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
    MsgExpr[evalInput, 5, 10], If[fn === ReplaceAll, " /.  ", " //. "], StringPadRight[HoldSymbolName @ rule, 17],
    " = ", MsgExpr[res, 5, 10]
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
indentPrint[args___] := Print[StringRepeat["\t", $depth], args];

(**************************************************************************************************)

(*

these variables contain common rules and things:
  $RegExpSpecialCharactersInGroup  how to escape literal characters in a CharacterGroup that have special meaning in REs
  SingleCharInGroupRules           how to expand symbolic heads into their corresponding lists of characters

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

PrivateStringPattern[RawStrExp, RawRegex, RawMaybe, RawCharGroup]

RawRegex     = StringPattern`Dump`RE;
RawStrExp    = StringPattern`Dump`SP;
RawMaybe     = StringPattern`Dump`QuestionMark;
RawCharGroup = StringPattern`Dump`CharacterGroup;

(**************************************************************************************************)

SetInitialValue[$cspRawHeadQ, UAssoc[]];
SetInitialValue[$cspMacroHeadQ, UAssoc[]];

getHead[head_Symbol[___]] := head;
getHead[head_Symbol] := head;

cspRawQ[e_] := Lookup[$cspRawHeadQ, getHead @ e, False];
cspMacroQ[e_] := Lookup[$cspMacroHeadQ, getHead @ e, False];

(**************************************************************************************************)

PublicVariable[$StrExpInternalRules, $StrExprMacroRules, $StrExprRawRules]

SetInitialValue[$StrExprMacroRules, {}];
SetInitialValue[$StrExprRawRules, {}];

$StrExpInternalRules := StringPattern`Dump`rules;

(**************************************************************************************************)

PublicFunction[LetterClassSymbolQ]

LetterClassSymbolQ = StringPattern`Dump`SingleCharacterQ;

(**************************************************************************************************)

PublicFunction[StringPatternCanContainCharsQ]

SetUsage @ "
StringPatternCanContainCharsQ[patt$, 'chars$'] checks if patt$ could match a string that contains one of 'chars$'.
* it will never produce false negatives, only false positives.
"

StringPatternCanContainCharsQ[_, ""] := False;

StringPatternCanContainCharsQ[patt_, needle_Str] := Scope[
  $needle = needle; $needleChars = Characters @ needle;
  Catch[spcc @ patt; False, spcc]
];

spccYes[] := Throw[True, spcc];

spcc = Case[
  s_Str              := spccLiteral @ s;
  e:recurseAll       := Scan[%, e];
  e:recurse1         := % @ P1 @ e;
  e:recurse2         := % @ P2 @ e;
  ignore             := Null;
  blanks             := spccYes[];

  Verbatim[Except][p_] := spccExcept[p];
  Verbatim[Except][p_, c_] := % @ p;
  ExceptLetterClass[p_] := antipatternRulesOutNeedleQ[LetterClass @ p];
  Avoiding[_, p_]    := spccAvoiding @ p;

  e:regex            := spccRegex @ P1 @ e; (* decompile the regex back to patterns *)

  e_                 := spccExpr @ e;
,
  {recurseAll -> _Alternatives | _StringExpression | _List,
   recurse1   -> _Repeated | _RepeatedNull | _Maybe | _Longest | _Shortest,
   blanks     -> Verbatim[_] | Verbatim[__] | Verbatim[___],
   regex      -> _RegularExpression | _Regex,
   ignore     -> _NegativeLookahead | _NegativeLookbehind | _PositiveLookahead | _PositiveLookbehind}
];

(**************************************************************************************************)

spccLiteral[s_] := If[StringContainsQ[s, $needleChars], spccYes[]];

(**************************************************************************************************)

(* does the Avoiding rule out the needle? if so, we're safe *)
spccAvoiding = Case[
  a_Str    := If[!SubsetQ[Characters @ a, $needleChars], spccYes[]];
  a_Symbol := If[LetterClassSymbolQ[a] && antipatternRulesOutNeedleQ[a], Null, spccYes[]];
  _        := spccYes[];
];

(**************************************************************************************************)

(* similar to Avoiding *)
spccExcept = Case[
  c_Str (* single letter *)      := If[{c} =!= $needleChars, spccYes[]];
  cs:{__Str}                     := If[!SubsetQ[cs, $needleChars], spccYes[]];
  e_LetterClass | e_RawCharGroup := antipatternRulesOutNeedleQ[e];
  e_                             := spccAvoiding @ e;
];

(**************************************************************************************************)

spccExpr[e_] := Which[

  (* are any of the forbidden chars matched by the char class? *)
  LetterClassSymbolQ @ e,      If[StringContainsQ[$needle, e], spccYes[]],

  (* macros don't need full expansion to raw elements, which is to be avoided because parsing REs is slow *)
  cspMacroQ @ e,                  Print["EXPANDING MACRO ", e]; spcc @ Echo @ spccApplyRule[e, $StrExprMacroRules],

  (* expand the expression one step and recurse *)
  cspRawQ @ e,                    Print["EXPANDING RAW ", e]; spccRaw @ Echo @ spccApplyRule[e, $StrExprRawRules],

  (* maybe its a RawCharGroup or similar *)
  True,                          spccRaw[e]
];

(* if it failed to evaluate, bail out *)
spccApplyRule[e_, rule_] := Scope[
  res = Replace[e, rule];
  If[res === e, spccYes[]];
  res
];

(**************************************************************************************************)

CacheVariable[$FromRegexCache]

spccRegex[re_Str] := Scope[
  se = CachedInto[$FromRegexCache, re, FromRegex @ re];
  spcc @ se
];

(**************************************************************************************************)

antipatternRulesOutNeedleQ[p_] := If[And @@ StringMatchQ[$needleChars, p], Null, spccYes[]];

spccRaw = Case[
  Verbatim[Except][e_RawCharGroup] := antipatternRulesOutNeedleQ[e];
  e_RawCharGroup            := If[StringContainsQ[$needle, e], spccYes[]];
  e_RawStrExp               := Scan[%, e];
  e_Str                     := spccRegex @ e; (* this can be a regex *)
  e_RawMaybe                := % @ P1 @ e;
  e_RawRegex                := spccRegex @ P1 @ e;
  raw_                      := (Print["UNKOWN RAW: ", raw]; spccYes[]);
];

(**************************************************************************************************)

PublicFunction[FromRegex]

PublicHead[CaptureGroup]

Options[FromRegex] = {Verbose -> False};

FromRegex::fail = "Parse failed: ``."
FromRegex::nomatch = "Cannot proceed because no rules matched in state ``. Remaining string: ``."

FromRegex[re_Str | Regex[re_Str] | RegularExpression[re_String], OptionsPattern[]] := Scope @ CatchMessage[
  UnpackOptions[$verbose];
  $result = $seq[];
  $cursor = {1};
  $state = $normalState;
  remaining = re;
  While[remaining =!= "",
    rules = $parsingRules[$state];
    case = First[
      StringCases[remaining, rules, 1]
    ,
      ReturnFailed["nomatch", SymbolName @ $state, MsgExpr @ remaining];
    ];
    {match, action} = case;
    VPrint[Style[Row[{"state = ", StringPadRight[SymbolName @ $state, 15], "cursor = ", Pane[$cursor, 80], "matched = ", Pane[MsgExpr @ match, 120], "action = ", Pane[MsgExpr @ action, 300], "result = ", Pane[MsgExpr @ $result, 500]}], LineBreakWithin -> False]];
    remaining = StringDrop[remaining, StrLen @ match];
    runAction @ action
  ];
  $result /. ($seq -> StringExpression) //. $simplificationRules /. flatAlt -> Alternatives /. {
    LetterClass[s_ ? SingleLetterQ] :> s,
    ExceptLetterClass[s_ ? SingleLetterQ] :> Except[s]
  } /. Verbatim[StringExpression][a_] :> a
];

SetAttributes[flatAlt, {Flat, OneIdentity}];

$simplificationRules = {
  (h:LetterClass|ExceptLetterClass)[StringExpression[a___]]:> h[a],
  StringExpression[l___, $infixAlt, r___] -> flatAlt[StringExpression[l], StringExpression[r]]
}

runAction = Case[
  Null := Null;
  enterState[state_, head_] := ($state = state; $result //= Insert[head[$seq[]], $cursor]; JoinTo[$cursor, {1, 1}]);
  leaveState[]              := ($cursor = Drop[$cursor, -2]; $cursor[[-1]]++; $state = $normalState);
  switchState[state_]       := ($state = state);
  emitToken[token_]         := ($result //= Insert[token, $cursor]; $cursor[[-1]]++);
  applyToToken[fn_]         := ($result //= MapAt[fn, MapAt[# - 1&, $cursor, -1]]);
  failParse[msg_]           := ThrowMessage["fail", msg]
];

toLHSPrefixedRule[lhs_ :> rhs_] := $prefix:(StartOfString ~~ lhs) :> {$prefix, rhs};
defineParsingRules[state_, rules___RuleDelayed] := (
  $parsingRules[state] = Map[toLHSPrefixedRule, {rules}];
);
_defineParsingRules := BadArguments[];

$specialChars = {"d", "D", "w", "W", "s", "S", "b", "B", "A", "z"};
$escapedChars = {"(", ")", "[", "]", "{", "}", "\\", ".", "?"};

(**************************************************************************************************)

defineParsingRules[$normalState,

  "(?" ~~ f:{"i","m","s"}.. ~~ ")" :> If[StringContainsQ[f,"i"], enterState[$normalState, CaseInsensitive], Null],
  "(?-i)"                          :> leaveState[],

  "(?:"          :> enterState[$normalState, $seq],
  "(?<!"         :> enterState[$normalState, NegativeLookbehind],
  "(?<="         :> enterState[$normalState, PositiveLookbehind],
  "(?!"          :> enterState[$normalState, NegativeLookahead],
  "(?="          :> enterState[$normalState, PositiveLookahead],

  "\\" ~~ c:$specialChars :> emitToken[$specialCharToExpr[c]],
  "\\" ~~ c:$escapedChars :> emitToken[c],

  "("            :> enterState[$normalState, CaptureGroup],

  "[[:" ~~ c:LowercaseLetter.. ~~ ":]]"  :> emitToken[parseCharClass[c]],
  "[^[:" ~~ c:LowercaseLetter.. ~~ ":]]" :> emitToken[Except @ parseCharClass[c]],

  "[^"           :> enterState[$classState, ExceptLetterClass],
  "["            :> enterState[$classState, LetterClass],

  "{" ~~ m:DigitCharacter.. ~~ "}"                              :> applyToToken[Repeated[#, FromDigits @ m]&],
  "{" ~~ m:DigitCharacter.. ~~ "," ~~ n:DigitCharacter.. ~~ "}" :> applyToToken[Repeated[#, FromDigits /@ {m, n}]&],
  "{"            :> failParse["unexpected {"],

  "|"            :> emitToken[$infixAlt],

  "??"           :> applyToToken[Maybe /* Shortest],
  "*?"           :> applyToToken[RepeatedNull /* Shortest],
  "+?"           :> applyToToken[Repeated /* Shortest],
  ".*"           :> emitToken[___],
  ".+"           :> emitToken[__],
  "."            :> emitToken[_],
  "?"            :> applyToToken[Maybe],
  "*"            :> applyToToken[RepeatedNull],
  "+"            :> applyToToken[Repeated],
  ")"            :> leaveState[],

  "^"            :> emitToken[StartOfLine],
  "$"            :> emitToken[EndOfLine],

  "\n"           :> emitToken[Newline],
  t_             :> emitToken[t]
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
  "alnum" -> RomanCharacter,
  "alpha" -> RomanLetter,
  "blank" -> {" ", "\t"},
  "punct" -> PunctuationCharacter,
  "space" -> WhitespaceCharacter,
  "xdigit"-> HexadecimalCharacter
];

FromRegex::badcharclass = "\"``\" is not a known char class.";
parseCharClass[name_] := Lookup[$charClassNames, name, ThrowMessage["badCharClass", name]];

defineParsingRules[$classState,
  "[:" ~~ c:LowercaseLetter.. ~~ ":]" :> emitToken[parseCharClass @ c],
  "\\^"  :> emitToken["^"],
  "\\-"  :> emitToken["-"],
  "\\\\" :> emitToken["\\"],
  "\\["  :> emitToken["["],
  "\\]"  :> emitToken["]"],
  "]"    :> leaveState[],
  o_     :> emitToken[o]
];

(**************************************************************************************************)

PublicFunction[RegexEscape]

RegexEscape[re_] := StringReplace[re, StringPattern`Dump`$RegExpSpecialCharacters];

(**************************************************************************************************)

PrivateSpecialFunction[DefineStringLetterClass]

DefineStringLetterClass = Case[
  sym_Symbol -> str_Str := %[sym -> {If[StrLen[str] == 1, str, "[" <> str <> "]"], str}];
  sym_Symbol -> {outer_, inner_} := Module[{},
    StringPattern`Dump`SingleCharInGroupRules //= addOrUpdateRule[sym -> inner];
    StringPattern`Dump`SingleCharacterQ[Verbatim[sym]] := True;
    StringPattern`Dump`rules //= addOrUpdateRule[sym -> outer];
    StringPattern`Dump`$StringPatternObjects //= appendIfAbsent[Hold @ sym];
  ];
  Sequence[rules__] := Scan[%, {rules}];
];

(**************************************************************************************************)

PrivateSpecialFunction[DefineStringPattern]

DefineStringPattern = Case[
  rule_RuleDelayed := Module[
    {rule2 = MapAt[HoldPattern, rule /. $spDeclarationRules, 1], head = First @ DefinitionHead @ rule},
    $cspRawHeadQ[head] = True;
    $StrExprRawRules         //= addOrUpdateRule[rule2];
    StringPattern`Dump`rules //= addOrUpdateRule[rule2];
  ];
  Sequence[rules__] := Scan[%, {rules}];
];

$spDeclarationRules = {
  s_Str /; StringContainsQ[s, "□"] :> RuleCondition @ StringReplace[s, "□" :> "[^\n]+?"],
  EvalStrExp[s_]                   :> ReplaceRepeated[s, StringPattern`Dump`rules],
  RawStrExp                        -> StringPattern`Dump`SP,
  RawRegex                         -> StringPattern`Dump`RE
}

(**************************************************************************************************)

PrivateSpecialFunction[DefineStringPatternMacro]

DefineStringPatternMacro = Case[
  rule_RuleDelayed := With[
    {rule2 = MapAt[HoldPattern, rule, 1], head = First @ DefinitionHead @ rule},
    If[!ListQ[StringPattern`Dump`expandDatePattern], StringPattern`Dump`expandDatePattern //= List];
    $cspMacroHeadQ[head] = True;
    $StrExprMacroRules                   //= addOrUpdateRule[rule2];
    StringPattern`Dump`expandDatePattern //= addOrUpdateRule[rule2];
  ];
  Sequence[rules__] := Scan[%, {rules}];
]

(**************************************************************************************************)

PublicSpecialFunction[ClearRegexCache]

ClearRegexCache[] := ClearSystemCache["RegularExpression"];

(**************************************************************************************************)

appendIfAbsent[item_][list_] :=
  If[MemberQ[list, Verbatim @ item], list, Append[list, item]];

addOrUpdateRule[list_List, rule:(_[lhs_, rhs_])] := Module[{pos, newList},
  pos = Position[list, (Rule|RuleDelayed)[Verbatim[lhs], _], {1}];
  If[pos === {},
    Append[list, rule]
  ,
    newList = ReplacePart[list, pos -> rule];
    If[newList =!= list, QuiverGeometryLoader`$RegexCacheDirty = True];
    newList
  ]
]

addOrUpdateRule[rule_][list_] := addOrUpdateRule[list, rule];

(**************************************************************************************************)

PublicFunction[ExpandPosixLetterClasses]

ExpandPosixLetterClasses[expr_] :=
  expr /. str_Str :> RuleCondition @ StringReplace[str, $classTranslations];

$classTranslations = {
  "[:upper:]" -> "A-Z",
  "[:lower:]" -> "a-z"
  "[:digit:]" -> "0-9"
  "[:alnum:]" -> "0-9A-Za-z",
  "[:alpha:]" -> "a-zA-Z",
  "[:blank:]" -> " \t",
  "[:punct:]" -> "!\"#$%&'()*+,-./:;>=<?@\\\\\\[\\]^_`{|}~",
  "[:space:]" -> "\n\t\r ",
  "[:xdigit:]" -> "0-9A-Fa-f"
}
