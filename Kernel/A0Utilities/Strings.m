PublicFunction[LowerCaseFirst, UpperCaseFirst]

LowerCaseFirst[str_String] := StringJoin[ToLowerCase @ StringTake[str, 1], StringDrop[str, 1]];
UpperCaseFirst[str_String] := StringJoin[ToUpperCase @ StringTake[str, 1], StringDrop[str, 1]];

(**************************************************************************************************)

PublicFunction[UpperCaseFirstQ, LowerCaseFirstQ]

SetListable[UpperCaseFirstQ, LowerCaseFirstQ]

UpperCaseFirstQ[""] = False;
UpperCaseFirstQ[str_String] := UpperCaseQ @ StringTake[str, 1];

LowerCaseFirstQ[""] = False;
LowerCaseFirstQ[str_String] := LowerCaseQ @ StringTake[str, 1];

(**************************************************************************************************)

PublicFunction[UpperCaseLast, LowerCaseLast]

UpperCaseLast[str_String] := StringJoin[StringDrop[str, -1], ToUpperCase @ StringTake[str, -1]];
LowerCaseLast[str_String] := StringJoin[StringDrop[str, -1], ToLowerCase @ StringTake[str, -1]];

(**************************************************************************************************)

PublicFunction[ToTitleString]

ToTitleString[s_String] :=
  ToLowerCase @ StringReplace[s, RegularExpression["([a-z])([A-Z])"] :> "$1 $2"];

(**************************************************************************************************)

PublicFunction[StringCaseSplit]

StringCaseSplit[s_String] := StringSplit[s, RegularExpression["(?<=[a-z])(?=[A-Z])"]];

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

PublicFunction[StringDeepCases]

StringDeepCases[expr_, pattern_] := Catenate @ DeepCases[expr, s_String :> StringCases[s, pattern]];
StringDeepCases[pattern_][expr_] := StringDeepCases[expr, pattern];

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

(**************************************************************************************************)

PrivateFunction[StringStartsEndsQ]

StringStartsEndsQ[str_String, a_, b_] := StringStartsQ[str, a] && StringEndsQ[str, b];
StringStartsEndsQ[str_List, a_, b_] := Map[StringStartsEndsQ[#, a, b]&, str];
StringStartsEndsQ[a_, b_][str_] := StringStartsEndsQ[str, a, b];

(**************************************************************************************************)

PrivateFunction[StringTrimLeft, StringTrimRight, StringTrimLeftRight]

StringTrimLeft[str_String, left_] := StringDelete[str, StartOfString ~~ left];
StringTrimLeft[list_List, left_] := Map[StringTrimLeft[#, left]&, list];
StringTrimLeft[left_][str_] := StringTrimLeft[str, left];

StringTrimRight[str_String, right_] := StringDelete[str, right ~~ EndOfString];
StringTrimRight[list_List, right_] := Map[StringTrimRight[#, right]&, list];
StringTrimRight[right_][str_] := StringTrimRight[str, right];

StringTrimLeftRight[str_String, left_, right_] := StringDelete[StringDelete[str, StartOfString ~~ left], right ~~ EndOfString];
StringTrimLeftRight[list_List, left_, right_] := Map[StringTrimLeftRight[#, left, right]&, list];
StringTrimLeftRight[left_, right_][str_] := StringTrimLeftRight[str, left, right];

(**************************************************************************************************)

PublicVariable[$LowercaseRomanLetters, $UppercaseRomanLetters, $RomanLetters]

$LowercaseRomanLetters = "abcdefghijklmnopqrstuvwxyz";
$UppercaseRomanLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
$RomanLetters = StringJoin[$LowercaseRomanLetters, $UppercaseRomanLetters];

(**************************************************************************************************)

PublicVariable[$LowercaseGreekLetters, $UppercaseGreekLetters, $GreekLetters]

$LowercaseGreekLetters = "\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Epsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\[Lambda]\[Mu]\[Nu]\[Xi]\[Pi]\[Rho]\[Sigma]\[Tau]\[CurlyPhi]\[Phi]\[Chi]\[Psi]\[Omega]";
$UppercaseGreekLetters = "\[CapitalGamma]\[CapitalPi]\[CapitalSigma]\[CapitalOmega]\[CapitalPhi]";
$GreekLetters = StringJoin[$LowercaseGreekLetters, $UppercaseGreekLetters];

(**************************************************************************************************)

PublicVariable[$LowercaseDoubleStruckLetters, $UppercaseDoubleStruckLetters, $DoubleStruckLetters, $DoubleStruckDigits, $DoubleStruckCharacters]

$LowercaseDoubleStruckLetters = "𝕒𝕓𝕔𝕕𝕖𝕗𝕘𝕙𝕚𝕛𝕜𝕝𝕞𝕟𝕠𝕡𝕢𝕣𝕤𝕥𝕦𝕧𝕨𝕩𝕪𝕫";
$UppercaseDoubleStruckLetters = "𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ";
$DoubleStruckLetters = StringJoin[$LowercaseDoubleStruckLetters, $UppercaseDoubleStruckLetters];
$DoubleStruckDigits = "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡";
$DoubleStruckCharacters = StringJoin[$DoubleStruckLetters, $DoubleStruckDigits];

(**************************************************************************************************)

PublicSymbol[ASCIILetter, LowercaseLetter, UppercaseLetter, AlphanumericCharacter]
PublicSymbol[LowercaseGreekLetter, UppercaseGreekLetter, GreekLetter]
PublicSymbol[DoubleQuote]
PublicSymbol[LowercaseDoubleStruckLetter, UppercaseDoubleStruckLetter, DoubleStruckLetter, DoubleStruckDigit, DoubleStruckCharacter]

declareStringLetterPattern[
  ASCIILetter -> "a-zA-Z0-9",
  LowercaseLetter -> "[:lower:]",
  UppercaseLetter -> "[:upper:]",
  AlphanumericCharacter -> "[:alnum:]",
  LowercaseGreekLetter -> $LowercaseGreekLetters,
  UppercaseGreekLetter -> $UppercaseGreekLetters,
  GreekLetter -> $GreekLetters,
  LowercaseDoubleStruckLetter -> $LowercaseDoubleStruckLetters,
  UppercaseDoubleStruckLetter -> $UppercaseDoubleStruckLetters,
  DoubleStruckLetter -> $DoubleStruckLetters,
  DoubleStruckDigit -> $DoubleStruckDigits,
  DoubleStruckCharacter -> $DoubleStruckCharacters,
  DoubleQuote -> "\""
];

(**************************************************************************************************)

PublicHead[ExceptLetterClass, LetterClass]

$letterClassExceptRules = {
  "^" -> "\\^"
};

declareStringPattern[
  LetterClass[class_String] :> StringJoin["[", StringReplace[class, $letterClassExceptRules], "]"],
  ExceptLetterClass[class_String] :> StringJoin["[^", StringReplace[class, $letterClassExceptRules], "]"]
]

(**************************************************************************************************)

PublicFunction[ExpandPosixCharacterClasses]

ExpandPosixCharacterClasses[expr_] := expr /. str_String :> RuleCondition @ StringReplace[str, $classTranslations];

$classTranslations = {
  "[:upper:]" -> "A-Z",
  "[:lower:]" -> "a-z"
  "[:digit:]" -> "0-9"
  "[:alnum:]" -> "0-9A-Za-z",
  "[:alpha:]" -> "a-zA-Z",
  "[:blank:]" -> " \t",
  "[:punct:]" -> "!\"#$%&'()*+,-./:;<=>?@\\\\\\[\\]^_`{|}~",
  "[:space:]" -> "\n\t\r ",
  "[:xdigit:]" -> "0-9A-Fa-f"
}

(**************************************************************************************************)

PublicSymbol[ASCIIWord, LowercaseWord, TitlecaseWord, TitlecasePhrase, FullNamePhrase]

declareStringPattern[
  Word            :> """\b(?:[[:alpha:]]+?)(?:'s|n't)?\b""",
  ASCIIWord       :> """\b(?:[a-zA-Z0-9]+?)\b""",
  LowercaseWord   :> "\\b[[:lower:]]+\\b",
  TitlecaseWord   :> "\\b[[:upper:]][[:lower:]]+\\b",
  TitlecasePhrase :> """\b[[:upper:]][[:lower:]]+(?: (?:of |or |and |in |on |the |by |a |the )?[[:upper:]][[:lower:]]+)*\b""",
  FullNamePhrase  :> """\b[[:upper:]][[:lower:]]+(?: [[:upper:]]\.?)*(?: van| der| de| von| st| del)*(?: [[:upper:]][[:lower:]]+\b)+"""
];

(**************************************************************************************************)

PublicHead[Maybe, NegativeLookbehind, NegativeLookahead, PositiveLookbehind, PositiveLookahead]

declareStringPattern[
  Maybe[lhs_]              :> spBlob @ StringPattern`Dump`QuestionMark @ spRecurse @ lhs,
  NegativeLookbehind[lhs_] :> spBlob["(?<!", spRecurse @ lhs, ")"],
  PositiveLookbehind[lhs_] :> spBlob["(?<=", spRecurse @ lhs, ")"],
  NegativeLookahead[lhs_]  :> spBlob["(?!", spRecurse @ lhs, ")"],
  PositiveLookahead[lhs_]  :> spBlob["(?=", spRecurse @ lhs, ")"]
];

(**************************************************************************************************)

PublicHead[LineFragment, DelineatedPhrase, ParentheticalPhrase, SingleQuotedPhrase, DoubleQuotedPhrase, HyperlinkPattern]

(* tested on https://regex101.com *)
(* $hyperlinkRegexp = """(?i)\b((?:https?://|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))"""; *)

(* $textFragment = "(?:(?:[[:alnum:]][[:alnum:] ?!,;()'\"-]*[[:alnum:]])|[[:alnum:]])"; *)

declareStringPattern[
  LineFragment             :> spBlob["[^\n]*"],
  (* TextFragment             :> spBlob[$textFragment], *)
  DelineatedPhrase[l_, r_] :> spBlob[spRecurse @ l, "[^\n]*?", spRecurse @ r],
  ParentheticalPhrase      :> spBlob["""(?<!\S)\(□\)"""],
  SingleQuotedPhrase       :> spBlob["""(?<!\S)(?:'□')|(?:‘□’)"""],
  DoubleQuotedPhrase       :> spBlob["""(?<!\S)(?:"□")|(?:“□”)"""],
  HyperlinkPattern         :> spBlob["""(?<!\S)https?://[[:alnum:]-]+(?:\.[[:alnum:]-]+)*(?:/[[:alnum:]-_]*)*(?:\?[[:alnum:]=&+%-]+)?"""]
];

(**************************************************************************************************)

PublicHead[MarkdownHeadingPattern, MarkdownNoteLinkPattern, MarkdownHyperlinkPattern, MarkdownInlineCodePattern, MarkdownBlockCodePattern, MarkdownEmphasisPattern]

$emphasisFragment = StringReplace["""(?:(?:\*Z\*)|(?:/Z/)|(?:(?<!\S)_Z_(?!\S)))""", "Z" -> """(?:(?:\S[^\n]*?\S)|\S)"""];

declareStringPattern[
  MarkdownHeadingPattern    :> "^#{1,4} (?:[^\n])+$",
  MarkdownNoteLinkPattern   :> "\\[\\[□\\]\\]",
  MarkdownHyperlinkPattern  :> "\\[□\\]\\(http[^ \n]+\\)",
  MarkdownInlineCodePattern :> "`□`",
  MarkdownBlockCodePattern  :> "^```[^`]+\n```",
  MarkdownEmphasisPattern   :> $emphasisFragment
];

(**************************************************************************************************)

PublicFunction[SingleLetterQ]

SingleLetterQ[s_String] := StringLength[s] == 1;
SingleLetterQ[_] := False;

(**************************************************************************************************)

PublicFunction[HexIntegerString]

SetListable[HexIntegerString];
HexIntegerString[e_Integer] := IntegerString[e, 16];

(**************************************************************************************************)

PublicFunction[RandomString]

$base36Chars = Characters @ "abcdefghijklmnopqrstuvwxyz0123456789";
RandomString[n_Integer] := StringJoin @ Part[$base36Chars, RandomInteger[{1, 36}, n]];