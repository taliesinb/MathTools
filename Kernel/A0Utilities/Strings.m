PublicFunction[LowerCaseFirst, UpperCaseFirst]

LowerCaseFirst[str_Str] := SJoin[ToLowerCase @ STake[str, 1], SDrop[str, 1]];
UpperCaseFirst[str_Str] := SJoin[ToUpperCase @ STake[str, 1], SDrop[str, 1]];

(**************************************************************************************************)

PublicFunction[UpperCaseFirstQ, LowerCaseFirstQ]

SetListable[UpperCaseFirstQ, LowerCaseFirstQ]

UpperCaseFirstQ[""] = False;
UpperCaseFirstQ[str_Str] := UpperCaseQ @ STake[str, 1];

LowerCaseFirstQ[""] = False;
LowerCaseFirstQ[str_Str] := LowerCaseQ @ STake[str, 1];

(**************************************************************************************************)

PublicFunction[LinearSyntaxQ, ContainsLinearSyntaxQ]

LinearSyntaxQ[s_Str] := StringStartsEndsQ[s, "\!\(\*", "\)"] && StringBalancedQ[s, "\(", "\)"];
ContainsLinearSyntaxQ[s_Str] := SContainsQ[s, "\!\(\*"] && SContainsQ[s, "\)"];

(**************************************************************************************************)

PublicFunction[StringBalancedQ]

StringBalancedQ[str_Str, l_, r_] := balancedQ @ Accumulate @ SCases[str, {l -> 1, r -> -1}];
StringBalancedQ[l_, r_][str_] := StringBalancedQ[str, l, r];
balancedQ[{}] := True;
balancedQ[ints_] := Min[ints] >= 0 && L[ints] == 0;

(**************************************************************************************************)

PublicFunction[ToLinearSyntax, FromLinearSyntax]

ToLinearSyntax[e_] := SJoin["\!\(\*", ToString[ToBoxes @ e, InputForm], "\)"];

FromLinearSyntax[str_Str] := If[StringStartsEndsQ[str, "\!\(\*", "\)"], tryEvalStr @ STake[str, {4, -2}], $Failed];
FromLinearSyntax[_] := $Failed;

tryEvalStr[""] := $Failed;
tryEvalStr[str_] := Quiet @ Check[ToExpression[str, InputForm, Hold], $Failed];

(**************************************************************************************************)

PublicFunction[UpperCaseLast, LowerCaseLast]

UpperCaseLast[str_Str] := SJoin[SDrop[str, -1], ToUpperCase @ STake[str, -1]];
LowerCaseLast[str_Str] := SJoin[SDrop[str, -1], ToLowerCase @ STake[str, -1]];

(**************************************************************************************************)

PublicFunction[ToTitleString]

ToTitleString[s_Str] :=
  ToLowerCase @ SRep[s, RegularExpression["([a-z])([A-Z])"] :> "$1 $2"];

(**************************************************************************************************)

PublicFunction[CamelCaseSplit]

CamelCaseSplit[s_Str] := SSplit[s, RegularExpression["(?<=[a-z])(?=[A-Z])"]];

(**************************************************************************************************)

PrivateFunction[commaString]

qs[s_Str] := PrefixSlash[s];
qs[e_] := e;
commaString[list_List] := TextString[Row[Map[qs, list], ", "]];

(**************************************************************************************************)

PublicFunction[StringReplaceRepeated]

StringReplaceRepeated[str_Str, rules_] := FixedPoint[SRep[rules], str];
StringReplaceRepeated[rules_][str_] := StringReplaceRepeated[str, rules];

(**************************************************************************************************)

PublicFunction[StringFindDelimitedPosition]

StringFindDelimitedPosition[str_, {start_, mid_, stop_}] := Scope[
  pos = F[SFind[str, start ~~ mid ~~ stop, 1], None];
  If[!ListQ[pos], ReturnFailed[]];
  lens = F @ SCases[
    STake[str, pos],
    a:start ~~ mid ~~ z:stop :> {+SLen[a], -SLen[z]},
    1
  ];
  pos + lens
]

(**************************************************************************************************)

PublicFunction[StringDeepCases]

StringDeepCases[expr_, pattern_] := Catenate @ DeepCases[expr, s_Str :> SCases[s, pattern]];
StringDeepCases[pattern_][expr_] := StringDeepCases[expr, pattern];

(**************************************************************************************************)

PublicFunction[FirstStringCase]

SetHoldRest[FirstStringCase];

FirstStringCase[string_, pattern_, else_:None] :=
  F[
    SCases[string, pattern, 1],
    else
  ]

(**************************************************************************************************)

PublicFunction[CommonStringPrefix, CommonStringPrefixLength]

CommonStringPrefix[{}] := None;
CommonStringPrefix[strings_ ? StrVecQ] :=
  STake[F @ strings, CommonStringPrefixLength @ strings];

CommonStringPrefixLength[strings_ ? StrVecQ] :=
  CommonPrefixLength @ Chars @ strings;

PublicFunction[CommonStringSuffix, CommonStringSuffixLength]

CommonStringSuffix[{}] := None;
CommonStringSuffix[strings_ ? StrVecQ] :=
  STake[F @ strings, Minus @ CommonStringSuffixLength @ strings];

CommonStringSuffixLength[strings_ ? StrVecQ] :=
  CommonSuffixLength @ Chars @ strings;

(**************************************************************************************************)

PublicFunction[StringJoinLeft, StringJoinRight]

StringJoinLeft[prefix_Str, other_Str] := SJoin[prefix, other];
StringJoinLeft[prefix_Str, other_List] := Map[StringJoinLeft[prefix], other];
StringJoinLeft[prefix_Str][other_] := StringJoinLeft[prefix, other];

StringJoinRight[other_Str, suffix_Str] := SJoin[other, suffix];
StringJoinRight[other_List, suffix_Str] := Map[StringJoinRight[suffix], other];
StringJoinRight[suffix_Str][other_] := StringJoinRight[other, suffix];
  
(**************************************************************************************************)

PublicFunction[StringFunction]

StringFunction[template_Str] :=
  Construct[
    Fn,
    SRep[template, $stringFunctionSlotRules]
  ] /. {SExpr -> SJoin, s_Slot :> TextString[s]};

$stringFunctionSlotRules = {
  "##" -> "#",
  "#" ~~ i:DigitCharacter :> Slot[FromDigits[i]],
  "#" ~~ w:LetterCharacter.. :> Slot[w]
};

(**************************************************************************************************)

PrivateFunction[PrefixSlash]

PrefixSlash[s_] := SJoin["\\", s];

(**************************************************************************************************)

PrivateFunction[StringStartsEndsQ]

StringStartsEndsQ[str_Str, a_, b_] := SStartsQ[str, a] && SEndsQ[str, b];
StringStartsEndsQ[str_List, a_, b_] := Map[StringStartsEndsQ[#, a, b]&, str];
StringStartsEndsQ[a_, b_][str_] := StringStartsEndsQ[str, a, b];

(**************************************************************************************************)

PublicFunction[SingleLetterQ]

SingleLetterQ[s_Str] := SLen[s] == 1;
SingleLetterQ[_] := False;

(**************************************************************************************************)

PublicFunction[HexIntegerString]

SetListable[HexIntegerString];
HexIntegerString[e_Int] := IntStr[e, 16];

(**************************************************************************************************)

PublicFunction[RandomString]

$base36Chars = Chars @ "abcdefghijklmnopqrstuvwxyz0123456789";
RandomString[n_Int] := SJoin @ Part[$base36Chars, RandomInteger[{1, 36}, n]];

(**************************************************************************************************)

PublicFunction[StringLineCases]

Options[StringLineCases] = {IgnoreCase -> False};

StringLineCases[text_, patt_, None, OptionsPattern[]] :=
  SCases[text, patt, IgnoreCase -> OptionValue[IgnoreCase]];

StringLineCases[text_, patt_, o:OptionsPattern[]] :=
  iStringLineCases[text, patt, 0, OptionValue[IgnoreCase]];

StringLineCases[text_, patt_, n_Int, o:OptionsPattern[]] :=
  iStringLineCases[text, patt, n, OptionValue[IgnoreCase]];

iStringLineCases[texts_List, patt_, n_, ic_] :=
  iStringLineCases[#, patt, n, ic]& /@ texts;

iStringLineCases[text_Str, pattern_, n_, ic_] := Scope[
  spans = SFind[text, pattern, IgnoreCase -> ic];
  If[!ListQ[spans], ReturnFailed[]];
  If[spans === {}, Return @ {}];
  linePos = getStringLinePositions @ text;
  lineSpans = toContextLineSpan[#, n]& /@ spans;
  STake[text, lineSpans]
];

iStringLineCases[___] := $Failed;

toContextLineSpan[{a_, b_}, n_] := {
  contextLimitedPart[n+1] @ Rev @ Select[linePos, LessEqualThan[a]],
  contextLimitedPart[n+1] @ Select[linePos, GreaterEqualThan[b]]
} + {1, -1};

contextLimitedPart[n_][list_] := Part[list, Min[n, Len @ list]];

(**************************************************************************************************)

getStringLinePositions[text_] := Pre[0] @ Part[SFind[text, EndOfLine], All, 1];

(**************************************************************************************************)

PublicFunction[ToStringLinePositions]

ToStringLinePositions[text_, spans_] :=
  stringPosToLinePos[getStringLinePositions @ text, Part[spans, All, 1]];

stringPosToLinePos[linePos_, pos_List] := Map[stringPosToLinePos[linePos, #]&, pos];
stringPosToLinePos[linePos_, p_Int] := Count[linePos, _Int ? (LessThan[p])];

(**************************************************************************************************)

PublicFunction[StringLineFind]

Options[StringLineFind] = {IgnoreCase -> False};

StringLineFind[text_Str, pattern_, opts:OptionsPattern[]] := Scope[
  spans = SFind[text, pattern, opts];
  If[spans === {}, Return @ {}];
  ToStringLinePositions[text, spans]
];

