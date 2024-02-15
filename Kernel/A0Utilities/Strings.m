PublicFunction[LowerCaseFirst, UpperCaseFirst]

LowerCaseFirst[str_Str] := StringJoin[ToLowerCase @ StringTake[str, 1], StringDrop[str, 1]];
UpperCaseFirst[str_Str] := StringJoin[ToUpperCase @ StringTake[str, 1], StringDrop[str, 1]];

(**************************************************************************************************)

PublicFunction[UpperCaseFirstQ, LowerCaseFirstQ]

SetListable[UpperCaseFirstQ, LowerCaseFirstQ]

UpperCaseFirstQ[""] = False;
UpperCaseFirstQ[str_Str] := UpperCaseQ @ StringTake[str, 1];

LowerCaseFirstQ[""] = False;
LowerCaseFirstQ[str_Str] := LowerCaseQ @ StringTake[str, 1];

(**************************************************************************************************)

PublicFunction[UpperCaseLast, LowerCaseLast]

UpperCaseLast[str_Str] := StringJoin[StringDrop[str, -1], ToUpperCase @ StringTake[str, -1]];
LowerCaseLast[str_Str] := StringJoin[StringDrop[str, -1], ToLowerCase @ StringTake[str, -1]];

(**************************************************************************************************)

PublicFunction[ToTitleString]

ToTitleString[s_Str] :=
  ToLowerCase @ StringReplace[s, RegularExpression["([a-z])([A-Z])"] :> "$1 $2"];

(**************************************************************************************************)

PublicFunction[CamelCaseSplit]

CamelCaseSplit[s_Str] := StringSplit[s, RegularExpression["(?<=[a-z])(?=[A-Z])"]];

(**************************************************************************************************)

PrivateFunction[commaString]

qs[s_Str] := PrefixSlash[s];
qs[e_] := e;
commaString[list_List] := TextString[Row[Map[qs, list], ", "]];

(**************************************************************************************************)

PublicFunction[StringReplaceRepeated]

StringReplaceRepeated[str_Str, rules_] := FixedPoint[StringReplace[rules], str];
StringReplaceRepeated[rules_][str_] := StringReplaceRepeated[str, rules];

(**************************************************************************************************)

PublicFunction[StringFindDelimitedPosition]

StringFindDelimitedPosition[str_, {start_, mid_, stop_}] := Scope[
  pos = First[StringPosition[str, start ~~ mid ~~ stop, 1], None];
  If[!ListQ[pos], ReturnFailed[]];
  lens = P1 @ StringCases[
    StringTake[str, pos],
    a:start ~~ mid ~~ z:stop :> {+StringLength[a], -StringLength[z]},
    1
  ];
  pos + lens
]

(**************************************************************************************************)

PublicFunction[StringDeepCases]

StringDeepCases[expr_, pattern_] := Catenate @ DeepCases[expr, s_Str :> StringCases[s, pattern]];
StringDeepCases[pattern_][expr_] := StringDeepCases[expr, pattern];

(**************************************************************************************************)

PublicFunction[FirstStringCase]

SetHoldRest[FirstStringCase];

FirstStringCase[string_, pattern_, else_:None] :=
  P1[
    StringCases[string, pattern, 1],
    else
  ]

(**************************************************************************************************)

PublicFunction[CommonStringPrefix, CommonStringPrefixLength]

CommonStringPrefix[{}] := None;
CommonStringPrefix[strings_ ? StringVectorQ] :=
  StringTake[P1 @ strings, CommonStringPrefixLength @ strings];

CommonStringPrefixLength[strings_ ? StringVectorQ] :=
  CommonPrefixLength @ Characters @ strings;

PublicFunction[CommonStringSuffix, CommonStringSuffixLength]

CommonStringSuffix[{}] := None;
CommonStringSuffix[strings_ ? StringVectorQ] :=
  StringTake[P1 @ strings, Minus @ CommonStringSuffixLength @ strings];

CommonStringSuffixLength[strings_ ? StringVectorQ] :=
  CommonSuffixLength @ Characters @ strings;

(**************************************************************************************************)

PublicFunction[StringJoinLeft, StringJoinRight]

StringJoinLeft[prefix_Str, other_Str] := StringJoin[prefix, other];
StringJoinLeft[prefix_Str, other_List] := Map[StringJoinLeft[prefix], other];
StringJoinLeft[prefix_Str][other_] := StringJoinLeft[prefix, other];

StringJoinRight[other_Str, suffix_Str] := StringJoin[other, suffix];
StringJoinRight[other_List, suffix_Str] := Map[StringJoinRight[suffix], other];
StringJoinRight[suffix_Str][other_] := StringJoinRight[other, suffix];
  
(**************************************************************************************************)

PublicFunction[StringFunction]

StringFunction[template_Str] :=
  Construct[
    Fn,
    StringReplace[template, $stringFunctionSlotRules]
  ] /. {StringExpression -> StringJoin, s_Slot :> TextString[s]};

$stringFunctionSlotRules = {
  "##" -> "#",
  "#" ~~ i:DigitCharacter :> Slot[FromDigits[i]],
  "#" ~~ w:LetterCharacter.. :> Slot[w]
};

(**************************************************************************************************)

PrivateFunction[PrefixSlash]

PrefixSlash[s_] := StringJoin["\\", s];

(**************************************************************************************************)

PrivateFunction[StringStartsEndsQ]

StringStartsEndsQ[str_Str, a_, b_] := StringStartsQ[str, a] && StringEndsQ[str, b];
StringStartsEndsQ[str_List, a_, b_] := Map[StringStartsEndsQ[#, a, b]&, str];
StringStartsEndsQ[a_, b_][str_] := StringStartsEndsQ[str, a, b];

(**************************************************************************************************)

PublicFunction[SingleLetterQ]

SingleLetterQ[s_Str] := StringLength[s] == 1;
SingleLetterQ[_] := False;

(**************************************************************************************************)

PublicFunction[HexIntegerString]

SetListable[HexIntegerString];
HexIntegerString[e_Int] := IntegerString[e, 16];

(**************************************************************************************************)

PublicFunction[RandomString]

$base36Chars = Characters @ "abcdefghijklmnopqrstuvwxyz0123456789";
RandomString[n_Int] := StringJoin @ Part[$base36Chars, RandomInteger[{1, 36}, n]];

(**************************************************************************************************)

PublicFunction[StringLineCases]

Options[StringLineCases] = {IgnoreCase -> False};

StringLineCases[text_, patt_, None, OptionsPattern[]] :=
  StringCases[text, patt, IgnoreCase -> OptionValue[IgnoreCase]];

StringLineCases[text_, patt_, o:OptionsPattern[]] :=
  iStringLineCases[text, patt, 0, OptionValue[IgnoreCase]];

StringLineCases[text_, patt_, n_Int, o:OptionsPattern[]] :=
  iStringLineCases[text, patt, n, OptionValue[IgnoreCase]];

iStringLineCases[texts_List, patt_, n_, ic_] :=
  iStringLineCases[#, patt, n, ic]& /@ texts;

iStringLineCases[text_Str, pattern_, n_, ic_] := Scope[
  linePos = Prepend[0] @ PA1 @ StringPosition[text, EndOfLine];
  charSpans = StringPosition[text, pattern, IgnoreCase -> ic];
  If[charSpans === {}, Return @ {}];
  lineSpans = toLineSpan[#, n]& /@ charSpans;
  StringTake[text, lineSpans]
];

iStringLineCases[___] := $Failed;

toLineSpan[{a_, b_}, n_] := {
  limitedPart[n+1] @ Reverse @ Select[linePos, LessEqualThan[a]],
  limitedPart[n+1] @ Select[linePos, GreaterEqualThan[b]]
} + {1, -1};

limitedPart[n_][list_] := Part[list, Min[n, Len @ list]];
