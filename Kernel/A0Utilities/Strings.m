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
