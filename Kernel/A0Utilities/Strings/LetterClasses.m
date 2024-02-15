PrivateVariable[$Alphabet]

$Alphabet = Characters @ "abcdefghijklmnopqrstuvwxyz\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\[Lambda]\[Mu]\[Nu]\[Xi]\[Omicron]\[Pi]\[Rho]\[Sigma]\[Tau]\[Upsilon]\[CurlyPhi]\[Chi]\[Psi]\[Omega]";
$Alphabet = Join[$Alphabet, ToUpperCase[$Alphabet]];

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

PublicFunction[ScriptToRoman]

$scriptToRoman := $scrToRoman = toStringRules[$UnicodeScriptLetters, $RomanLetters];

ScriptToRoman[str_Str] := StringReplace[str, $scriptToRoman];

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

PublicSymbol[DQuote, SQuote, Newline]
PublicSymbol[Letter, LowercaseLetter, UppercaseLetter, AlphanumericCharacter, Digit]
PublicSymbol[RomanLetter, LowercaseRomanLetter, UppercaseRomanLetter, RomanCharacter, RomanDigit]

DefineStringLetterClass[
  DQuote      -> "\"",
  SQuote      -> "'",
  Newline     -> "\n",
  Letter      -> "[:alpha:]", LowercaseLetter      -> "[:lower:]", UppercaseLetter      -> "[:upper:]", AlphanumericCharacter -> "[:alnum:]", Digit      -> "[:digit:]",
  RomanLetter -> "[:alpha:]", LowercaseRomanLetter -> "[:lower:]", UppercaseRomanLetter -> "[:upper:]", RomanCharacter        -> "[:alnum:]", RomanDigit -> "[:digit:]"
  (* ^ these are redundant but included for symmetry with the below *)
];

(**************************************************************************************************)

(* TODO: recognize spans of characters and use them instead for speed *)
PublicSymbol[LowercaseGreekLetter, UppercaseGreekLetter, GreekLetter]
PublicSymbol[LowercaseDoubleStruckLetter, UppercaseDoubleStruckLetter, DoubleStruckLetter, DoubleStruckDigit, DoubleStruckCharacter]
PublicSymbol[LowercaseUnicodeScriptLetter, UppercaseUnicodeScriptLetter, UnicodeScriptLetter]
PublicSymbol[LowercaseScriptLetter, UppercaseScriptLetter, ScriptLetter]
PublicSymbol[LowercaseSanSerifLetter, UppercaseSanSerifLetter, SanSerifLetter]
PublicSymbol[LowercaseMonospaceLetter, UppercaseMonospaceLetter, MonospaceLetter]
PublicSymbol[LowercaseFrakturLetter, UppercaseFrakturLetter, FrakturLetter]

DefineStringLetterClass[
  LowercaseGreekLetter         -> $LowercaseGreekLetters         , UppercaseGreekLetter         -> $UppercaseGreekLetters         , GreekLetter         -> $GreekLetters,
  LowercaseDoubleStruckLetter  -> $LowercaseDoubleStruckLetters  , UppercaseDoubleStruckLetter  -> $UppercaseDoubleStruckLetters  , DoubleStruckLetter  -> $DoubleStruckLetters, DoubleStruckDigit -> $DoubleStruckDigits , DoubleStruckCharacter -> $DoubleStruckCharacters,
  LowercaseUnicodeScriptLetter -> $LowercaseUnicodeScriptLetters , UppercaseUnicodeScriptLetter -> $UppercaseUnicodeScriptLetters , UnicodeScriptLetter -> $UnicodeScriptLetters,
  LowercaseScriptLetter        -> $LowercaseScriptLetters        , UppercaseScriptLetter        -> $UppercaseScriptLetters        , ScriptLetter        -> $ScriptLetters,
  LowercaseSanSerifLetter      -> $LowercaseSanSerifLetters      , UppercaseSanSerifLetter      -> $UppercaseSanSerifLetters      , SanSerifLetter      -> $SanSerifLetters,
  LowercaseMonospaceLetter     -> $LowercaseMonospaceLetters     , UppercaseMonospaceLetter     -> $UppercaseMonospaceLetters     , MonospaceLetter     -> $MonospaceLetters,
  LowercaseFrakturLetter       -> $LowercaseFrakturLetters       , UppercaseFrakturLetter       -> $UppercaseFrakturLetters       , FrakturLetter       -> $FrakturLetters
];

(**************************************************************************************************)

PublicHead[LetterClass, ExceptLetterClass]

DefineStringPattern[
  LetterClass[args___]       :> StringPattern`Dump`CharacterGroup[args],
  ExceptLetterClass[args___] :> Except[StringPattern`Dump`CharacterGroup[args]]
];

