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

PublicFunction[SelectStrings, DiscardStrings]

SetUsage @ "
SelectStrings[{str$1, str$2, $$}, patt$] gives a list of the str$i that match patt$.
SelectStrings[patt$] is the operator form of SelectStrings.
"

SetUsage @ "
DiscardStrings[{str$1, str$2, $$}, patt$] gives a list of the str$i that do not match patt$.
DiscardStrings[patt$] is the operator form of DiscardStrings.
"

SelectStrings[strings_List, patt_] := CheckMsg Pick[strings, StringMatchQ[strings, patt]];
DiscardStrings[strings_List, patt_] := Pick[strings, Not /@ StringMatchQ[strings, patt]];

SelectStrings[patt_][strings_] := SelectStrings[strings, patt];
DiscardStrings[patt_][strings_] := DiscardStrings[strings, patt];

(**************************************************************************************************)

PublicFunction[SelectStringsContaining, DiscardStringsContaining]

SetUsage @ "
SelectStringsContaining[{str$1, str$2, $$}, patt$] gives a list of the str$i that contain patt$.
SelectStringsContaining[patt$] is the operator form of SelectStringsContaining.
"

SetUsage @ "
DiscardStringsContaining[{str$1, str$2, $$}, patt$] gives a list of the str$i that do not contain patt$.
DiscardStringsContaining[patt$] is the operator form of DiscardStringsContaining.
"

SelectStringsContaining[strings_List, patt_] := Pick[strings, StringContainsQ[strings, patt]];
DiscardStringsContaining[strings_List, patt_] := Pick[strings, StringFreeQ[strings, patt]];

SelectStringsContaining[patt_][strings_] := SelectStringsContaining[strings, patt];
DiscardStringsContaining[patt_][strings_] := DiscardStringsContaining[strings, patt];

(**************************************************************************************************)

PublicFunction[CommonStringPrefix, CommonStringSuffix]

SetUsage @ "CommonStringPrefix[{str$1, str$2, $$}] gives the string that is the longest prefix of all the str$i."
SetUsage @ "CommonStringSuffix[{str$1, str$2, $$}] gives the string that is the longest suffix of all the str$i."

CommonStringPrefix[{}] := "";
CommonStringPrefix[strings_ ? StrVecQ] := STake[F @ strings, CommonStringPrefixLength @ strings];

CommonStringSuffix[{}] := "";
CommonStringSuffix[strings_ ? StrVecQ] := STake[F @ strings, Minus @ CommonStringSuffixLength @ strings];

PublicFunction[CommonStringPrefixLength, CommonStringSuffixLength]

SetUsage @ "CommonStringPrefixLength[{str$1, str$2, $$}] gives the length of the string that is the longest prefix of all the str$i."
SetUsage @ "CommonStringSuffixLength[{str$1, str$2, $$}] gives the length of the string that is the longest suffix of all the str$i."

CommonStringPrefixLength[strings_ ? StrVecQ] := CommonPrefixLength @ Chars @ strings;
CommonStringSuffixLength[strings_ ? StrVecQ] := CommonSuffixLength @ Chars @ strings;

(**************************************************************************************************)

PublicFunction[StringLineCases]

SetUsage @ "
StringLineCases[text$, patt$] returns the lines of a string that contain a match to patt$.
StringLineCases[$$, n$] gives n$ additional surrounding lines.
* the option %IgnoreCase is supported.
"

Options[StringLineCases] = {IgnoreCase -> False};

StringLineCases[text_, patt_, None, OptionsPattern[]] :=
  SCases[text, patt, IgnoreCase -> OptionValue[IgnoreCase]];

StringLineCases[text_, patt_, o:OptionsPattern[]] :=
  iStringLineCases[text, patt, 0, OptionValue[IgnoreCase]];

StringLineCases[text_, patt_, n_Int, o:OptionsPattern[]] :=
  iStringLineCases[text, patt, n, OptionValue[IgnoreCase]];

_StringLineCases := BadArguments[];

iStringLineCases[texts_List, patt_, n_, ic_] :=
  iStringLineCases[#, patt, n, ic]& /@ texts;

iStringLineCases[text_Str, pattern_, n_, ic_] := Scope[
  spans = CheckMsg @ SFind[text, pattern, IgnoreCase -> ic];
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

getStringLinePositions[text_] := Pre[0] @ Col1 @ SFind[text, EndOfLine];

(**************************************************************************************************)

PublicFunction[ToStringLinePositions]

ToStringLinePositions[text_, spans_] :=
  stringPosToLinePos[getStringLinePositions @ text, Col1[spans]];

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
