PublicStringPattern[Nullspace, ShortBlank, ASCIIWord, Base64Pattern, LinearSyntaxPattern]

DefineStringPatternMacro[
  Nullspace            :> RepeatedNull[WhitespaceCharacter],
  ShortBlank           :> Shortest[__]
];

DefineStringPattern[
  Number               :> RawRegex["""(?!<\d)\d+(?!\d)"""],
  Base64Pattern        :> RawRegex["""[[:alnum:]+/]+={0,2}"""],
  ASCIIWord            :> RawRegex["""\b[[:alnum:]]+\b"""]
];

DefineStringPattern[
  LinearSyntaxPattern  :> RawRegex["\!\(\*" ~~ Shortest[___] ~~ "\)"]
]

PublicStringPattern[EnglishlikeWord, LowercaseWord, UppercaseWord]

DefineStringPattern[
  EnglishlikeWord      :> RawRegex["""\b(?:[[:alpha:]][[:lower:]]*?)(?:'s|n't)?\b"""],
  LowercaseWord        :> RawRegex["""\b[[:lower:]]+\b"""],
  UppercaseWord        :> RawRegex["""\b[[:upper:]]+\b"""]
]

PublicStringPattern[CamelCaseWord, LowerCamelCaseWord, UpperCamelCaseWord]

DefineStringPattern[
  CamelCaseWord        :> RawRegex["""\b[[:alpha:]][[:alnum:]]*\b"""],
  LowerCamelCaseWord   :> RawRegex["""\b[[:lower:]][[:alnum:]]*\b"""],
  UpperCamelCaseWord   :> RawRegex["""\b[[:upper:]][[:alnum:]]*\b"""]
]

PublicStringPattern[TitleCaseWord, TitleCasePhrase, FullNamePhrase]

DefineStringPattern[
  TitleCaseWord        :> RawRegex["""\b[[:upper:]][[:lower:]]+\b"""],
  TitleCasePhrase      :> RawRegex["""\b[[:upper:]][[:lower:]]+(?: (?:of |or |and |in |on |the |by |a |the )?[[:upper:]][[:lower:]]+)*\b"""],
  FullNamePhrase       :> RawRegex["""\b[[:upper:]][[:lower:]]+(?: [[:upper:]]\.?)*(?: van| der| de| von| st| del)*(?: [[:upper:]][[:lower:]]+\b)+"""]
]
(**************************************************************************************************)

PublicStringPattern[StartOfParagraph, EndOfParagraph]

(* TODO: replace all these patterns with their high-level equivalents, easier to debug and other macros can introspect them without
decompilation *)
(*
StartOfParagraph = (StartOfString | PositiveLookbehind[StartOfString ~~ "\n"] | PositiveLookbehind["\n\n"]) ~~ NegativeLookahead[EndOfLine];
EndOfParagraph = NegativeLookbehind["\n"] ~~ (EndOfString | PositiveLookahead["\n" ~~ EndOfString] | PositiveLookahead["\n\n"]);
*)

DefineStringPattern[
  StartOfParagraph :> RawRegex["""(?:\A|(?<=\A\n)|(?<=\n\n))(?!$)"""],
  EndOfParagraph   :> RawRegex["""(?<!\n)(?:\z|(?=\n\z)|(?=\n\n))"""]
];

(**************************************************************************************************)

PublicStringPattern[SQuoteSpan, DQuoteSpan, ParenSpan, BraceSpan, BracketSpan, DoubleBracketSpan]

defineSpanStringPattern[head_, {lchar_, rchar_}] :=
  DefineStringPatternMacro[
    head | head[] :> lchar ~~ Avoiding[___, rchar] ~~ rchar,
    head[p_]      :> lchar ~~ Avoiding[p,   rchar] ~~ rchar
  ];

SetUsage @ "SQuoteSpan matches a span delimited by '.";
SetUsage @ "DQuoteSpan matches a double quote.";
SetUsage @ "DQuoteSpan matches a double quote.";

KVScan[defineSpanStringPattern[#1, StringSplit[#2]]&, UAssoc[
  SQuoteSpan         -> "' '",
  DQuoteSpan         -> "\" \"",
  ParenSpan          -> "( )",
  BraceSpan          -> "{ }",
  BracketSpan        -> "[ ]",
  DoubleBracketSpan  -> "[[ ]]"
]];

(**************************************************************************************************)

PublicStringPattern[XMLSpan]

(* TODO: introduce some other mechanism of doing backrefs so we don't end up with temp vars *)
DefineStringPatternMacro[
  XMLSpan | XMLSpan[]  :> Module[{tag}, SExpr["<", tag:ASCIIWord, ">", ShortBlank, "</", tag_, ">"]],
  XMLSpan[tag_]        :> xmlSpan[tag],
  XMLSpan[tag_, patt_] :> xmlSpan[tag, patt]
];

xmlSpan[tag_] := F @ Module[{z}, List[SJoin["<", tag, ">"] ~~ z:Shortest[___] ~~ SJoin["</", tag, ">"] /; StringBalancedQ[z, "<", ">"]]];
xmlSpan[tag_, lit_Str] := SJoin["<", tag, ">", lit, "</", tag, ">"];
xmlSpan[tag_, patt_] := SJoin["<", tag, ">"] ~~ patt ~~ SJoin["</", tag, ">"];

(**************************************************************************************************)

PublicStringPattern[LineSpan, ParagraphSpan]

SetUsage @ "LineSpan matches an entire line.\nLineSpan[p$] matches p$ if it constitutes an entire line."
SetUsage @ "ParagraphSpan matches an entire paragraph, separated from others by two newlines.\nParagraphSpan[p$] matches p$ if it constitutes an entire paragraph."

DefineStringPatternMacro[
  LineSpan | LineSpan[]             :> StartOfLine ~~ Avoiding[__, "\n"] ~~ EndOfLine,
  LineSpan[p_]                      :> StartOfLine ~~ Avoiding[p,  "\n"] ~~ EndOfLine,
  ParagraphSpan | ParagraphSpan[]   :> StartOfParagraph ~~ Avoiding[__, {"\n\n"}] ~~ EndOfParagraph,
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

PublicStringPattern[WholeWord]

SetUsage @ "WholeWord[p$] matches p$ if it has a %WordBoundary on either side."

DefineStringPattern[
  WholeWord[p_]   :> RawStrExp["\\b", EvalStrExp @ p, "\\b"]
];

(**************************************************************************************************)

PublicStringPattern[WithinLine]

SetUsage @ "WithinLine[p$] matches p$ if occurs within a single line."

(* TODO: maybe WithinWord, WithinParagraph? *)
DefineStringPatternMacro[
  WithinLine[p_]  :> Avoiding[p, "\n"]
];
