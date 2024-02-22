PublicStringPattern[Nullspace, ShortBlank, ASCIIWord, Base64Pattern, LinearSyntaxPattern]

DefineStringPattern[
  Nullspace            :> """\s*""",
  ShortBlank           :> """.+?""",
  Number               :> """(?!<\d)\d+(?!\d)""",
  Base64Pattern        :> """[[:alnum:]+/]+={0,2}""",
  ASCIIWord            :> """\b[[:alnum:]]+\b"""
];

DefineStringPattern[
  LinearSyntaxPattern  :> "\!\(\*" ~~ Shortest[___] ~~ "\)"
]

PublicStringPattern[EnglishlikeWord, LowercaseWord, UppercaseWord]

DefineStringPattern[
  EnglishlikeWord      :> """\b(?:[[:alpha:]][:lower:]*?)(?:'s|n't)?\b""",
  LowercaseWord        :> """\b[[:lower:]]+\b""",
  UppercaseWord        :> """\b[[:upper:]]+\b"""
]

PublicStringPattern[CamelCaseWord, LowerCamelCaseWord, UpperCamelCaseWord]

DefineStringPattern[
  CamelCaseWord        :> """\b[[:alpha:]][[:alnum:]]*\b""",
  LowerCamelCaseWord   :> """\b[[:lower:]][[:alnum:]]*\b""",
  UpperCamelCaseWord   :> """\b[[:upper:]][[:alnum:]]*\b"""
]

PublicStringPattern[TitleCaseWord, TitleCasePhrase, FullNamePhrase]

DefineStringPattern[
  TitleCaseWord        :> """\b[[:upper:]][[:lower:]]+\b""",
  TitleCasePhrase      :> """\b[[:upper:]][[:lower:]]+(?: (?:of |or |and |in |on |the |by |a |the )?[[:upper:]][[:lower:]]+)*\b""",
  FullNamePhrase       :> """\b[[:upper:]][[:lower:]]+(?: [[:upper:]]\.?)*(?: van| der| de| von| st| del)*(?: [[:upper:]][[:lower:]]+\b)+"""
]

(**************************************************************************************************)

PublicStringPattern[Riffled]

DefineStringPatternMacro[
  Riffled[a_, sep_] :> a ~~ RepeatedNull[sep ~~ a]
];

(**************************************************************************************************)

PublicStringPattern[WLSpan, WLSymbolSpan, WLStrSpan]

(* TODO: Handle comments! *)

DefineStringPattern[
  WLSymbolSpan :> "(?:[$a-zA-Z][`a-zA-Z0-9]+)"
];

DefineStringPatternMacro[
  WLStrSpan :> makeUniqueWLStrSpan[],
  WLSpan :> makeUniqueWLSpan[]
];

makeUniqueWLStrSpan[] := Module[{z}, Construct[Condition, z:("\"" ~~ Shortest[___] ~~ "\""), Uneval @ notPartialStrQ[z]]];

notPartialStrQ[s_] := EvenQ @ StringLength @ First[StringCases[s, "\\".. ~~ "\"" ~~ EndOfString], ""];

(* this matches up anything up to a top-level comma *)
makeUniqueWLSpan[] := Module[{z, q, rep, repIn},
  rep = Riffled[PatternRecurse @ z, ","];
  repIn = Function[#1 ~~ rep ~~ #2];
  Nullspace ~~ z:SExpr[
    Alt[
      "{}", "<||>", "[]", WLSymbolSpan,
      "(" ~~ PatternRecurse[z] ~~ ")",
      Construct[Condition, q:("\"" ~~ ShortBlank ~~ "\""), Uneval @ notPartialStrQ @ q],
      repIn["[", "]"], repIn["{", "}"], repIn["<|", "|>"], repIn["\[LeftAssociation]", "\[RightAssociation]"],
      ExceptLetterClass[",()[]{}\"\[LeftAssociation]\[RightAssociation]"]..
    ],
    Maybe @ PatternRecurse[z]
  ] ~~ Nullspace
];

(* TODO: make a delimiter class *)

assocStrFreeQ[q_] := StringFreeQ[q, {"<|", "|>", "\[LeftAssociation]", "\[RightAssociation]"}]

(**************************************************************************************************)

PublicStringPattern[Regex]

DefineStringPatternMacro[
  Regex[s_Str] :> RegularExpression[s]
]

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

PublicStringPattern[StartOfParagraph, EndOfParagraph]

$ParagraphSRE = """(?:\A|(?<=\A\n)|(?<=\n\n))(?!$)""";
$ParagraphERE = """(?<!\n)(?:\z|(?=\n\z)|(?=\n\n))""";

(*
StartOfParagraph = (StartOfString | PositiveLookbehind[StartOfString ~~ "\n"] | PositiveLookbehind["\n\n"]) ~~ NegativeLookahead[EndOfLine];
EndOfParagraph = NegativeLookbehind["\n"] ~~ (EndOfString | PositiveLookahead["\n" ~~ EndOfString] | PositiveLookahead["\n\n"]);
*)

DefineStringPattern[
  StartOfParagraph :> RawRegex[$ParagraphSRE],
  EndOfParagraph   :> RawRegex[$ParagraphERE]
];

(**************************************************************************************************)

PublicStringPattern[SQuoteSpan, DQuoteSpan, ParenSpan, BraceSpan, BracketSpan, DoubleBracketSpan]

defineSpanStringPattern[head_, str_] := Scope[
  pair = StringPart[str, {1, -1}];
  {rl, rr} = SSplit @ RegexEscape @ str;
  regex = Regex @ SJoin[rl, "[^", rr, "]+", rr];
  defineSpanStringPattern[head, regex, pair];
];

defineSpanStringPattern[head_, regex_, {lchar_, rchar_}] :=
  DefineStringPatternMacro[
    head | head[] :> regex,
    head[p_]      :> lchar ~~ Avoiding[p, rchar] ~~ rchar
  ];

SetUsage @ "SQuoteSpan matches a span delimited by '.";
SetUsage @ "DQuoteSpan matches a double quote.";
SetUsage @ "DQuoteSpan matches a double quote.";

KVScan[defineSpanStringPattern, UAssoc[
  SQuoteSpan         -> "' '",
  DQuoteSpan         -> "\" \"",
  ParenSpan          -> "( )",
  BraceSpan          -> "{ }",
  BracketSpan        -> "[ ]",
  DoubleBracketSpan  -> "[[ ]]"
]];

(**************************************************************************************************)

PublicStringPattern[XMLSpan]

DefineStringPatternMacro[
  XMLSpan[tag_Str]        :> xmlSpan[tag],
  XMLSpan[tag_Str, patt_] :> xmlSpan[tag, patt]
];

xmlSpan[tag_] := F @ Module[{z}, List[SJoin["<", tag, ">"] ~~ z:Shortest[___] ~~ SJoin["</", tag, ">"] /; StringBalancedQ[z, "<", ">"]]];
xmlSpan[tag_, lit_Str] := SJoin["<", tag, ">", lit, "</", tag, ">"];
xmlSpan[tag_, patt_] := SJoin["<", tag, ">"] ~~ patt ~~ SJoin["</", tag, ">"];

(**************************************************************************************************)

PublicStringPattern[LineSpan, ParagraphSpan]

SetUsage @ "LineSpan matches an entire line.\nLineSpan[p$] matches p$ if it constitutes an entire line."
SetUsage @ "ParagraphSpan matches an entire paragraph, separated from others by two newlines.\nParagraphSpan[p$] matches p$ if it constitutes an entire paragraph."

DefineStringPatternMacro[
  LineSpan | LineSpan[]             :> Regex["^[^\n]+$"],
  LineSpan[p_]                      :> StartOfLine ~~ Avoiding[p, "\n"] ~~ EndOfLine,
  ParagraphSpan | ParagraphSpan[]   :> RawRegex[$ParagraphSRE <> ".*?" <> $ParagraphERE],
  ParagraphSpan[p_]                 :> StartOfParagraph ~~ Avoiding[p, {"\n\n"}] ~~ EndOfParagraph
];

(**************************************************************************************************)

PublicStringPattern[LineFragment, ParentheticalPhrase, SingleQuotedPhrase, DoubleQuotedPhrase, HyperlinkPattern]

(* tested on https://regex101.com, modified to include : *)
(* $hyperlinkRegexp = """(?i)\b((?:https?://|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))"""; *)

(* $textFragment = "(?:(?:[[:alnum:]][[:alnum:] ?!,;()'\"-]*[[:alnum:]])|[[:alnum:]])"; *)

SetUsage @ "LineFragment matches part of a line."
SetUsage @ "HyperlinkPattern matches a hyperlink, which must start with 'http:', and not occur as part of longer string of alphanumeric characters."

(* TODO: retire these *)
DefineStringPattern[
  LineFragment             :> RawStrExp["[^\n]*"],
  (* TextFragment          :> RawStrExp[$textFragment], *)
  ParentheticalPhrase      :> RawStrExp["""(?<!\S)\(□\)"""],
  SingleQuotedPhrase       :> RawStrExp["""(?<!\S)(?:'□')|(?:‘□’)"""],
  DoubleQuotedPhrase       :> RawStrExp["""(?<!\S)(?:"□")|(?:“□”)"""],
  HyperlinkPattern         :> RawStrExp["""\bhttps?://[[:alnum:]-]+(?:\.[[:alnum:]\-]+)*(?:/[[:alnum:]~\-_.]*)*(?:\?[[:alnum:]:=&+%\-._]+)?"""]
];

(**************************************************************************************************)

PublicStringPattern[WholeWord, WithinLine]

SetUsage @ "WholeWord[p$] matches p$ if it has a %WordBoundary on either side."
SetUsage @ "WithinLine[p$] matches p$ if occurs within a single line."

DefineStringPattern[
  WholeWord[p_]   :> RawStrExp["\\b", EvalStrExp @ p, "\\b"],
  WithinLine[p_]  :> parseWithinLine[p]
];

parseWithinLine[p_] := RawStrExp["(?-s)", EvalStrExp @ p, "(?s)"]

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

