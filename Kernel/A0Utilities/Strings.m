PublicFunction[LowerCaseFirst, UpperCaseFirst]

LowerCaseFirst[str_String] := StringJoin[ToLowerCase @ StringTake[str, 1], StringDrop[str, 1]];
UpperCaseFirst[str_String] := StringJoin[ToUpperCase @ StringTake[str, 1], StringDrop[str, 1]];

PublicFunction[UpperCaseLast, LowerCaseLast]

UpperCaseLast[str_String] := StringJoin[StringDrop[str, -1], ToUpperCase @ StringTake[str, -1]];
LowerCaseLast[str_String] := StringJoin[StringDrop[str, -1], ToLowerCase @ StringTake[str, -1]];

(**************************************************************************************************)

PublicFunction[ToTitleString]

ToTitleString[s_String] :=
  ToLowerCase @ StringReplace[s, RegularExpression["([a-z])([A-Z])"] :> "$1 $2"];

(**************************************************************************************************)

PrivateFunction[commaString]

qs[s_String] := PrefixSlash[s];
qs[e_] := e;
commaString[list_List] := TextString[Row[Map[qs, list], ", "]];

(**************************************************************************************************)

PublicFunction[StringReplaceRepeated]

StringReplaceRepeated[str_String, rules_] := FixedPoint[StringReplace[rules], str];
StringReplaceRepeated[rules_][str_] := StringReplaceRepeated[str, rules];

(**************************************************************************************************)

PublicFunction[StringFindDelimitedPosition]

StringFindDelimitedPosition[str_, {start_, mid_, stop_}] := Scope[
  pos = First[StringPosition[str, start ~~ mid ~~ stop, 1], None];
  If[!ListQ[pos], ReturnFailed[]];
  lens = First @ StringCases[
    StringTake[str, pos],
    a:start ~~ mid ~~ z:stop :> {+StringLength[a], -StringLength[z]},
    1
  ];
  pos + lens
]

(**************************************************************************************************)

PublicFunction[FirstStringCase]

SetHoldRest[FirstStringCase];

FirstStringCase[string_, pattern_, else_:None] :=
  First[
    StringCases[string, pattern, 1],
    else
  ]

(**************************************************************************************************)

PublicFunction[CommonStringPrefix, CommonStringPrefixLength]

CommonStringPrefix[{}] := None;
CommonStringPrefix[strings_ ? StringVectorQ] :=
  StringTake[First @ strings, CommonStringPrefixLength @ strings];

CommonStringPrefixLength[strings_ ? StringVectorQ] :=
  CommonPrefixLength @ Characters @ strings;

PublicFunction[CommonStringSuffix, CommonStringSuffixLength]

CommonStringSuffix[{}] := None;
CommonStringSuffix[strings_ ? StringVectorQ] :=
  StringTake[First @ strings, Minus @ CommonStringSuffixLength @ strings];

CommonStringSuffixLength[strings_ ? StringVectorQ] :=
  CommonSuffixLength @ Characters @ strings;

(**************************************************************************************************)

PrivateVariable[$Alphabet]

$Alphabet = Join[Alphabet["English"], Alphabet["Greek"]];
$Alphabet = Join[$Alphabet, ToUpperCase[$Alphabet]];

(**************************************************************************************************)

PublicFunction[StringJoinLeft, StringJoinRight]

StringJoinLeft[prefix_String, other_String] := StringJoin[prefix, other];
StringJoinLeft[prefix_String, other_List] := Map[StringJoinLeft[prefix], other];
StringJoinLeft[prefix_String][other_] := StringJoinLeft[prefix, other];

StringJoinRight[other_String, suffix_String] := StringJoin[other, suffix];
StringJoinRight[other_List, suffix_String] := Map[StringJoinRight[suffix], other];
StringJoinRight[suffix_String][other_] := StringJoinRight[other, suffix];
  
(**************************************************************************************************)

PublicFunction[StringFunction]

StringFunction[template_String] :=
  Construct[
    Function,
    StringReplace[template, $stringFunctionSlotRules]
  ] /. {StringExpression -> StringJoin, s_Slot :> TextString[s]};

$stringFunctionSlotRules = {
  "#" ~~ i:DigitCharacter :> Slot[FromDigits[i]],
  "#" ~~ w:LetterCharacter.. :> Slot[w]
};


(**************************************************************************************************)

PrivateFunction[PrefixSlash]

PrefixSlash[s_] := StringJoin["\\", s];

(**************************************************************************************************)

PrivateFunction[WrapQuotes]

WrapQuotes[s_] := StringJoin["\"", s, "\""];