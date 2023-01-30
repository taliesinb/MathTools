tokenizeUsage[str_String] :=
  Developer`ToList[
    TOpen[TLineGroup], TOpen[TLine],
    DeleteCases[ ""|"\n"] @ StringSplit[str, Append[$tokenRules, wholeWord[word:$currentMainSymbol] :> TMain[word]]],
    TClose[TLine], TClose[TLineGroup]
  ];

wholeWord[e_] := (StartOfLine|WordBoundary) ~~ e ~~ WordBoundary;
$literalSymbols = $literalSymbolRegex;

wholeInfixSyntax[e_] := Whitespace ~~ Except[StartOfLine] ~~ e ~~ Except[EndOfLine] ~~ Whitespace;
$infixSyntaxStrings = Alternatives @@ StringSplit["-> :> === =!= > < >= == <= != = /@ @@@ @@ @ && || + - / * |"];

$varTokenRules = {
  "$$" -> TSeq[],
  var:LetterCharacter.. ~~ "$$" :> TVarSeq[var],
  var:LetterCharacter.. ~~ "$" ~~ ind:(LetterCharacter | DigitCharacter).. :> TVarIndexed[var, parseSubscript @ ind],
  var:LetterCharacter.. ~~ "$(" ~~ Shortest[ind__] ~~ ")" :> TVarIndexed[var, parseSubscript @ ind],
  var:LetterCharacter.. ~~ "$" :> TVar[var]
};

$tokenRules = {
  (* line-oriented formatting *)
  StartOfLine ~~ "* " -> TBullet[],
  StartOfLine ~~ "| " -> TOpen[TTableRow],
  " |\n" -> TClose[TTableRow],

  (* line *)
  StartOfLine ~~ "```\n" ~~ block:Except["`"]... ~~ "\n```\n" :> TCodeBlock[block],

  (* brackets *)
  "[" -> TOpen[TBracket], "(" -> TOpen[TParen], "\[LeftAssociation]"|"<|" -> TOpen[TAssoc], "{" -> TOpen[TBrace],
  "]" -> TClose[TBracket], ")" -> TClose[TParen], "\[RightAssociation]"|"|>" -> TClose[TAssoc], "}" -> TClose[TBrace],

  (* infix syntax *)
  wholeInfixSyntax[t:$infixSyntaxStrings] -> TInfixSyntax[t],

  (* literals *)
  "'" :> TQuote[],
  d:DigitCharacter.. :> TInteger[d],
  "$Failed" -> TLiteralSymbol["$Failed"],
  wholeWord[word:$literalSymbols] :> TLiteralSymbol[word],
  "%%" ~~ w:LetterCharacter.. :> TOptionSymbol[w],
  "%" ~~ w:LetterCharacter.. :> TSymbol[w],

  (* vars *)
  Splice @ $varTokenRules,

  "\n" -> Splice[{TClose[TLine], TOpen[TLine]}]
};

parseSubscript[s_String] := StringSplit[s, $subscriptTokenRules];

$subscriptTokenRules = {
  Splice @ $varTokenRules,
  d:DigitCharacter.. :> TInteger[d],
  v:WordCharacter.. :> TVar[v]
}

(**************************************************************************************************)

groupTokens[tokens_List] := Scope[
  $i = 1; $n = Length[tokens]; $tokens = tokens;
  firstToken = First @ tokens;
  If[!MatchQ[firstToken, _TOpen], ReturnFailed[]];
  grouped = groupSegment[First @ firstToken] //. $postGroupRules;
  spanned = findMathSpan[grouped];
  spanned
];

$postGroupRules = {
  TLine[t__TTableRow] :> groupTableRows[{t}]
};

WhitespaceOrComma = (WhitespaceCharacter | ",")..;
$whitespaceP = _String ? (StringMatchQ[WhitespaceOrComma]);
$mathTokenP = Alternatives[
  _TParen, _TAssoc, _TBracket, _TBrace,
  _TInfixSyntax, _TQuote,
  _TSeq, _TVar, _TVarSeq, _TVarIndexed, _TInteger,
  _TLiteralSymbol, _TMain, _TSymbol, _TOptionSymbol
];

$mathSpanRule = {
  span:{$mathTokenP, RepeatedNull[$mathTokenP | $whitespaceP], $mathTokenP} :> Apply[TMathSpan, span],
  span:{$mathTokenP} :> Apply[TMathSpan, span]
};

findMathSpansIn[head_[args___]] :=
  Apply[head, SequenceReplace[{args}, $mathSpanRule]];

findMathSpan = Case[
  t_TLineGroup := Map[%, t];
  t_TLine | t_List := findMathSpansIn[t];
  t_TableRow := Map[%, t, {3}];
  other_ := other;
];

groupTableRows[rows_] := Apply[TTable, splitRow /@ rows];
splitRow[t_TTableRow] := TTableRow @@ SequenceSplit[List @@ t, {TInfixSyntax["|"]}];

groupSegment[nodeType_] := Block[
  {result = nodeType[]},
  While[$i++ < $n,
    item = Match[
      Part[$tokens, $i],
      TOpen[type_] :> groupSegment @ type,
      TQuote[] :> groupString[],
      TClose[type_] :> Which[
        type === nodeType, Break[],
        type === TLineGroup && nodeType === TLine, Break[],
        True, Print["unexpected closing ", type]
      ],
      other_ :> other
    ];
    AppendTo[result, item];
  ];
  result
];

groupString[] := Block[{start = ++$i},
  While[$i < $n && Part[$tokens, $i] =!= TQuote[], $i++];
  TString @@ Part[$tokens, start ;; $i-1]
]

(**************************************************************************************************)

PublicFunction[ParseUsageString]

ParseUsageString[str_] :=
  groupTokens @ tokenizeUsage @ str

ParseUsageString[n_Integer] :=
  ParseUsageString @ $RawUsageStringTable[[n]];

(**************************************************************************************************)

PublicFunction[TokensToMarkdown]

TokensToMarkdown[tokens_] :=
  StringJoin @ outerMarkdown @ tokens;

mapSeq[f_, args___] := Map[f, {args}];

outerMarkdown = Case[
  TLineGroup[args___] := Riffle[mapSeq[lineMarkdown, args], "\n\n"];
];

lineMarkdown[args___] :=
  Catch[iLineMarkdown[args], _$dispatchError, handleDispatchError[args]];

handleDispatchError[args___][badtoken_, $dispatchError[fn_]] := (
  Print["unexpected token ", badtoken, " while parsing in ", fn, ":\n", NF @ args, "\n"];
  " ERROR "
)

iLineMarkdown = Case[
  TLine[args___] := mapSeq[lineInnerMarkdown, args];
  t_TTable := tableMarkdown[t];
  other_ := Throw[other, $dispatchError[lineMarkdown]];
];

tableMarkdown[TTable[args___]] := {"| -- |\n", Riffle[mapSeq[rowMarkdown, args], "\n"]};
rowMarkdown[TTableRow[args___]] := {"| ", Riffle[mapSeq[lineInnerMarkdown, args], " | "], " |"};

lineInnerMarkdown = Case[
  TBullet[] := "* ";
  s_TMathSpan := mathMarkdown[s];
  other_ := markdownDispatch[lineInnerMarkdown, other];
];

TSymbolP = (TLiteralSymbol|TMain|TSymbol|TOptionSymbol);

markdownDispatch = Case[
  s_String := wlToUnicode[s];
  l_List := Map[$f, l];
  TAssoc[args___] := {"<|", mapSeq[$f, args], "|>"};
  TBrace[args___] := {"{", mapSeq[$f, args], "}"};
  TBracket[args___] := {"[", mapSeq[$f, args], "]"};
  TParen[args___] := {"(", mapSeq[$f, args], ")"};
  TInfixSyntax[s_] := s;
  TInteger[s_] := s;
  TQuote[] := "\"";
  TString[args___] := {"\"", args, "\""};
  TSymbolP[t_] := katexEscape[t];
  other_ := Throw[other, $dispatchError[$f]];
];
markdownDispatch[f_, s_] := Block[{$f = f}, markdownDispatch[s]];

mathMarkdown[TMathSpan[args___]] := {"$",
   StringTrim @ StringJoin @ markdownDispatch[innerMathMarkdown, {args}],
   "$"
};

wlToUnicode[s_] := StringReplace[s, $WLSymbolToUnicode];

$infixTranslation = StringJoin["\,{", #, "}\,"]& /@ Take[SymbolTranslationData[<|"InputForm" -> "Katex"|>], 12];
$infixTranslation["=="] = "\,⩵\,";
$infixTranslation["==="] = "\,⩶\,";

katexEscape[s_] := StringReplace[s, $WLSymbolToKatexRegex];

dollarEscape[s_] := StringReplace[s, "$" -> "{\\mathdollar}"];
makeKatexSymbol[s_, type_] := {"\\", type, "{", dollarEscape @ s, "}"};

varMarkdown[s_String] /; StringLength[s] === 1 := s;
varMarkdown[s_] := {"\\textit{", s, "}"};
varMarkdown[s_] := s;

innerMathMarkdown = Case[
  TLiteralSymbol[t_] := makeKatexSymbol[t, "lsymbol"];
  TMain[t_] := makeKatexSymbol[t, "msymbol"];
  TSymbol[t_] := makeKatexSymbol[t, "rawsymbol"];
  TOptionSymbol[t_] := makeKatexSymbol[t, "osymbol"];
  TString[args___] := makeKatexSymbol[t, "lstring"];
  TVar[v_] := varMarkdown @ v;
  TVarIndexed[v_, ind_] := {varMarkdown @ v, "_", "{", % @ ind, "}"};
  TVarSeq[v_] := {varMarkdown @ v, "... "};
  TSeq[] := {" ... "};
  TInfixSyntax[s_] := Lookup[$infixTranslation, s, s];
  TAssoc[args___] := {" \\langle \\lvert ", mapSeq[%, args], " \\rvert \\rangle "};
  TBrace[args___] := {" \\lbrace ", mapSeq[%, args], " \\rbrace "};
  TParen[args___] := {"(", mapSeq[%, args], ")"};
  other_ := markdownDispatch[other]
];

(**************************************************************************************************)

PublicFunction[UsageToMarkdown]

$currentMainSymbol = "FooBar";

UsageToMarkdown[usage_String] :=
  UsageToMarkdown @ Rule[
    First[StringCases[usage, $mainSymbolRegex, 1], ""],
    usage
  ];


UsageToMarkdown[mainSymbol_String -> usage_String] := Scope[
  (* $currentMainSymbol will be picked up later by ParseUsageString, it's PackageScope *)
  $currentMainSymbol = mainSymbol;
  TokensToMarkdown @ ParseUsageString @ usage
];

(**************************************************************************************************)

PublicFunction[DumpUsagesToString]

DumpUsagesToString[n_] := Scope[
  rules = Normal @ Take[$RawUsageStringTable, n];
  StringRiffle[UsageToMarkdown /@ rules, "\n\n"]
];

(**************************************************************************************************)

PublicFunction[DumpUsagesToClipboard]

DumpUsagesToClipboard[n_] :=
  CopyToClipboard @ DumpUsagesToString[n]

(**************************************************************************************************)

PublicFunction[ExportUsages]

ExportUsages[n_] := Scope[
  usageString = DumpUsagesToString[n];
  (* fullString = StringJoin["\\\\[", $KatexPrelude, "\n\\\\]\n\n", usageString]; *)
  fullString = processForFranklin @ StringJoin["$$\n", $KatexPrelude, "\n$$\n\n", usageString];
  exportPath = PathJoin[$MarkdownExportDirectory, "Usages.md"];
  Export[exportPath, fullString, "Text", CharacterEncoding -> "UTF-8"]
];