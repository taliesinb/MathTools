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

PrivateVariable[$Alphabet]

$Alphabet = Characters @ "abcdefghijklmnopqrstuvwxyz\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\[Lambda]\[Mu]\[Nu]\[Xi]\[Omicron]\[Pi]\[Rho]\[Sigma]\[Tau]\[Upsilon]\[CurlyPhi]\[Chi]\[Psi]\[Omega]";
$Alphabet = Join[$Alphabet, ToUpperCase[$Alphabet]];

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

PrivateFunction[QuotedStringQ]

QuotedStringQ[s_] := StringMatchQ[s, "\"*\""];

(**************************************************************************************************)

PrivateFunction[StringStartsEndsQ]

StringStartsEndsQ[str_Str, a_, b_] := StringStartsQ[str, a] && StringEndsQ[str, b];
StringStartsEndsQ[str_List, a_, b_] := Map[StringStartsEndsQ[#, a, b]&, str];
StringStartsEndsQ[a_, b_][str_] := StringStartsEndsQ[str, a, b];

(**************************************************************************************************)

PrivateFunction[StringTrimLeft, StringTrimRight, StringTrimLeftRight]

StringTrimLeft[str_Str, left_] := StringDelete[str, StartOfString ~~ left];
StringTrimLeft[list_List, left_] := Map[StringTrimLeft[#, left]&, list];
StringTrimLeft[left_][str_] := StringTrimLeft[str, left];

StringTrimRight[str_Str, right_] := StringDelete[str, right ~~ EndOfString];
StringTrimRight[list_List, right_] := Map[StringTrimRight[#, right]&, list];
StringTrimRight[right_][str_] := StringTrimRight[str, right];

StringTrimLeftRight[str_Str, left_, right_] := StringDelete[StringDelete[str, StartOfString ~~ left], right ~~ EndOfString];
StringTrimLeftRight[list_List, left_, right_] := Map[StringTrimLeftRight[#, left, right]&, list];
StringTrimLeftRight[left_, right_][str_] := StringTrimLeftRight[str, left, right];

(**************************************************************************************************)

PublicVariable[$LowercaseRomanLetters, $UppercaseRomanLetters, $RomanLetters, $RomanDigits, $RomanCharacters]

$LowercaseRomanLetters = "abcdefghijklmnopqrstuvwxyz";
$UppercaseRomanLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
$RomanLetters = StringJoin[$LowercaseRomanLetters, $UppercaseRomanLetters];
$RomanDigits = "0123456789";
$RomanCharacters = StringJoin[$RomanLetters, $RomanDigits];

(**************************************************************************************************)

PublicVariable[$LowercaseUnicodeScriptLetters, $UppercaseUnicodeScriptLetters, $UnicodeScriptLetters]

$LowercaseUnicodeScriptLetters = "𝒶𝒷𝒸𝒹ℯ𝒻ℊ𝒽𝒾𝒿𝓀𝓁𝓂𝓃ℴ𝓅𝓆𝓇𝓈𝓉𝓊𝓋𝓌𝓍𝓎𝓏";
$UppercaseUnicodeScriptLetters = "𝒜ℬ𝒞𝒟ℰℱ𝒢ℋℐ𝒥𝒦ℒℳ𝒩𝒪𝒫𝒬ℛ𝒮𝒯𝒰𝒱𝒲𝒳𝒴𝒵";
$UnicodeScriptLetters = StringJoin[$LowercaseUnicodeScriptLetters, $UppercaseUnicodeScriptLetters];

(**************************************************************************************************)

PublicVariable[$LowercaseScriptLetters, $UppercaseScriptLetters, $ScriptLetters]

$LowercaseScriptLetters = "\[ScriptA]\[ScriptB]\[ScriptC]\[ScriptD]\[ScriptE]\[ScriptF]\[ScriptG]\[ScriptH]\[ScriptI]\[ScriptJ]\[ScriptK]\[ScriptL]\[ScriptM]\[ScriptN]\[ScriptO]\[ScriptP]\[ScriptQ]\[ScriptR]\[ScriptS]\[ScriptT]\[ScriptU]\[ScriptV]\[ScriptW]\[ScriptX]\[ScriptY]\[ScriptZ]";
$UppercaseScriptLetters = "\[ScriptCapitalA]\[ScriptCapitalB]\[ScriptCapitalC]\[ScriptCapitalD]\[ScriptCapitalE]\[ScriptCapitalF]\[ScriptCapitalG]\[ScriptCapitalH]\[ScriptCapitalI]\[ScriptCapitalJ]\[ScriptCapitalK]\[ScriptCapitalL]\[ScriptCapitalM]\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalP]\[ScriptCapitalQ]\[ScriptCapitalR]\[ScriptCapitalS]\[ScriptCapitalT]\[ScriptCapitalU]\[ScriptCapitalV]\[ScriptCapitalW]\[ScriptCapitalX]\[ScriptCapitalY]\[ScriptCapitalZ]";
$ScriptLetters = StringJoin[$LowercaseScriptLetters, $UppercaseScriptLetters];

(**************************************************************************************************)

PublicVariable[$LowercaseSanSerifLetters, $UppercaseSanSerifLetters, $SanSerifLetters]

$LowercaseSanSerifLetters = "𝖺𝖻𝖼𝖽𝖾𝖿𝗀𝗁𝗂𝗃𝗄𝗅𝗆𝗇𝗈𝗉𝗊𝗋𝗌𝗍𝗎𝗏𝗐𝗑𝗒𝗓";
$UppercaseSanSerifLetters = "𝖠𝖡𝖢𝖣𝖤𝖥𝖦𝖧𝖨𝖩𝖪𝖫𝖬𝖭𝖮𝖯𝖰𝖱𝖲𝖳𝖴𝖵𝖶𝖷𝖸𝖹";
$SanSerifLetters = StringJoin[$LowercaseSanSerifLetters, $UppercaseSanSerifLetters];

(**************************************************************************************************)

PublicVariable[$LowercaseMonospaceLetters, $UppercaseMonospaceLetters, $MonospaceLetters]

$LowercaseMonospaceLetters = "𝚊𝚋𝚌𝚍𝚎𝚏𝚐𝚑𝚒𝚓𝚔𝚕𝚖𝚗𝚘𝚙𝚚𝚛𝚜𝚝𝚞𝚟𝚠𝚡𝚢𝚣";
$UppercaseMonospaceLetters = "𝙰𝙱𝙲𝙳𝙴𝙵𝙶𝙷𝙸𝙹𝙺𝙻𝙼𝙽𝙾𝙿𝚀𝚁𝚂𝚃𝚄𝚅𝚆𝚇𝚈𝚉";
$MonospaceLetters = StringJoin[$LowercaseMonospaceLetters, $UppercaseMonospaceLetters];

(**************************************************************************************************)

PublicVariable[$LowercaseFrakturLetters, $UppercaseFrakturLetters, $FrakturLetters]

$LowercaseFrakturLetters = "𝔞𝔟𝔠𝔡𝔢𝔣𝔤𝔥𝔦𝔧𝔨𝔩𝔪𝔫𝔬𝔭𝔮𝔯𝔰𝔱𝔲𝔳𝔴𝔵𝔶𝔷";
$UppercaseFrakturLetters = "𝔄𝔅ℭ𝔇𝔈𝔉𝔊ℌℑ𝔍𝔎𝔏𝔐𝔑𝔒𝔓𝔔ℜ𝔖𝔗𝔘𝔙𝔚𝔛𝔜ℨ";
$FrakturLetters = StringJoin[$LowercaseFrakturLetters, $UppercaseFrakturLetters];

(**************************************************************************************************)

PublicVariable[$LowercaseDoubleStruckLetters, $UppercaseDoubleStruckLetters, $DoubleStruckLetters, $DoubleStruckDigits, $DoubleStruckCharacters]

$LowercaseDoubleStruckLetters = "𝕒𝕓𝕔𝕕𝕖𝕗𝕘𝕙𝕚𝕛𝕜𝕝𝕞𝕟𝕠𝕡𝕢𝕣𝕤𝕥𝕦𝕧𝕨𝕩𝕪𝕫";
$UppercaseDoubleStruckLetters = "𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ";
$DoubleStruckLetters = StringJoin[$LowercaseDoubleStruckLetters, $UppercaseDoubleStruckLetters];
$DoubleStruckDigits = "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡";
$DoubleStruckCharacters = StringJoin[$DoubleStruckLetters, $DoubleStruckDigits];

(**************************************************************************************************)

PublicFunction[ToNonDecoratedRoman]

toStringRules[str1_, str2_] := RuleThread[Characters @ str1, Characters @ str2];

$toNonDecoratedRoman := $toNonDecoratedRoman = Join[
  toStringRules[$UnicodeScriptLetters, $RomanLetters],
  toStringRules[$ScriptLetters, $RomanLetters],
  toStringRules[$MonospaceLetters, $RomanLetters],
  toStringRules[$FrakturLetters, $RomanLetters],
  toStringRules[$DoubleStruckCharacters, $RomanCharacters]
];

ToNonDecoratedRoman[str_Str] := StringReplace[str, $toNonDecoratedRoman];

(**************************************************************************************************)

PublicVariable[$LowercaseGreekLetters, $UppercaseGreekLetters, $GreekLetters]

$LowercaseGreekLetters = "\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Epsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\[Lambda]\[Mu]\[Nu]\[Xi]\[Pi]\[Rho]\[Sigma]\[Tau]\[CurlyPhi]\[Phi]\[Chi]\[Psi]\[Omega]";
$UppercaseGreekLetters = "\[CapitalGamma]\[CapitalDelta]\[CapitalTheta]\[CapitalLambda]\[CapitalXi]\[CapitalPi]\[CapitalSigma]\[CapitalPhi]\[CapitalPsi]\[CapitalOmega]";
$GreekLetters = StringJoin[$LowercaseGreekLetters, $UppercaseGreekLetters];

(**************************************************************************************************)

PublicFunction[ToSpelledGreek]

$spelledGreek = "alpha beta gamma delta curlyepsilon epsilon zeta eta theta iota kappa lambda mu nu xi pi rho sigma tau curlyphi phi chi psi omega Gamma Delta Theta Lambda Xi Pi Sigma Phi Psi Omega";
$toSpelledGreek := $toSpelledGreek = RuleThread[Characters @ $GreekLetters, StringSplit @ $spelledGreek];

ToSpelledGreek[str_Str] := StringReplace[str, $toSpelledGreek];

(**************************************************************************************************)

PublicSymbol[ASCIILetter, LowercaseLetter, UppercaseLetter, AlphanumericCharacter]
PublicSymbol[LowercaseRomanLetter, UppercaseRomanLetter, RomanLetter, RomanCharacter]
PublicSymbol[LowercaseGreekLetter, UppercaseGreekLetter, GreekLetter]
PublicSymbol[DoubleQuote]
PublicSymbol[LowercaseDoubleStruckLetter, UppercaseDoubleStruckLetter, DoubleStruckLetter, DoubleStruckDigit, DoubleStruckCharacter]
PublicSymbol[LowercaseUnicodeScriptLetter, UppercaseUnicodeScriptLetter, UnicodeScriptLetter]
PublicSymbol[LowercaseScriptLetter, UppercaseScriptLetter, ScriptLetter]
PublicSymbol[LowercaseSanSerifLetter, UppercaseSanSerifLetter, SanSerifLetter]
PublicSymbol[LowercaseMonospaceLetter, UppercaseMonospaceLetter, MonospaceLetter]
PublicSymbol[LowercaseFrakturLetter, UppercaseFrakturLetter, FrakturLetter]

declareStringLetterPattern[
  ASCIILetter -> "a-zA-Z0-9",
  LowercaseLetter -> "[:lower:]",
  UppercaseLetter -> "[:upper:]",
  AlphanumericCharacter -> "[:alnum:]",
  LowercaseRomanLetter -> $LowercaseRomanLetters,
  UppercaseRomanLetter -> $UppercaseRomanLetters,
  RomanLetter -> $RomanLetters,
  RomanCharacter -> $RomanCharacters,
  LowercaseGreekLetter -> $LowercaseGreekLetters,
  UppercaseGreekLetter -> $UppercaseGreekLetters,
  GreekLetter -> $GreekLetters,
  LowercaseDoubleStruckLetter -> $LowercaseDoubleStruckLetters,
  UppercaseDoubleStruckLetter -> $UppercaseDoubleStruckLetters,
  DoubleStruckLetter -> $DoubleStruckLetters,
  DoubleStruckDigit -> $DoubleStruckDigits,
  DoubleStruckCharacter -> $DoubleStruckCharacters,
  LowercaseUnicodeScriptLetter -> $LowercaseUnicodeScriptLetters, UppercaseUnicodeScriptLetter -> $UppercaseUnicodeScriptLetters, UnicodeScriptLetter -> $UnicodeScriptLetters,
  LowercaseScriptLetter -> $LowercaseScriptLetters, UppercaseScriptLetter -> $UppercaseScriptLetters, ScriptLetter -> $ScriptLetters,
  LowercaseSanSerifLetter -> $LowercaseSanSerifLetters, UppercaseSanSerifLetter -> $UppercaseSanSerifLetters, SanSerifLetter -> $SanSerifLetters,
  LowercaseMonospaceLetter -> $LowercaseMonospaceLetters, UppercaseMonospaceLetter -> $UppercaseMonospaceLetters, MonospaceLetter -> $MonospaceLetters,
  LowercaseFrakturLetter -> $LowercaseFrakturLetters, UppercaseFrakturLetter -> $UppercaseFrakturLetters, FrakturLetter -> $FrakturLetters,
  DoubleQuote -> "\""
];

(**************************************************************************************************)

PublicHead[ExceptLetterClass, LetterClass]

$letterClassExceptRules = {
  "^" -> "\\^"
};

declareStringPattern[
  LetterClass[class_Str] :> StringJoin["[", StringReplace[class, $letterClassExceptRules], "]"],
  ExceptLetterClass[class_Str] :> StringJoin["[^", StringReplace[class, $letterClassExceptRules], "]"]
]

(**************************************************************************************************)

PublicFunction[ExpandPosixCharacterClasses]

ExpandPosixCharacterClasses[expr_] := expr /. str_Str :> RuleCondition @ StringReplace[str, $classTranslations];

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
  HyperlinkPattern         :> spBlob["""(?<!\S)https?://[[:alnum:]-]+(?:\.[[:alnum:]-]+)*(?:/[[:alnum:]-_.]*)*(?:\?[[:alnum:]=&+%-.]+)?"""]
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