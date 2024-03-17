PublicStringPattern[DQuote, SQuote, Newline]
PublicStringPattern[Letter, LowercaseLetter, UppercaseLetter, AlphanumericCharacter, Digit]
PublicStringPattern[RomanLetter, LowercaseRomanLetter, UppercaseRomanLetter, RomanCharacter, RomanDigit]

DefineStringLetterClass[
  DQuote      -> "\"",
  SQuote      -> "'",
  Newline     -> "\n",
  Letter      -> "[:alpha:]", LowercaseLetter      -> "[:lower:]", UppercaseLetter      -> "[:upper:]", AlphanumericCharacter -> "[:alnum:]",   Digit      -> "[:digit:]",
  RomanLetter -> "a-zA-Z",    LowercaseRomanLetter -> "a-z",       UppercaseRomanLetter -> "A-Z",       RomanCharacter        -> "a-zA-Z0-9",   RomanDigit -> "0-9"
];

(* NOTE: ordinary Letter matches e.g. greek letters *)

(**************************************************************************************************)

PublicStringPattern[CombiningMark, AccentMark, AccentLetter]

DefineStringLetterClass[
  CombiningMark  -> "\:0300-\:036f",
  AccentMark  -> "\:0301-\:0304\:0306-\:0308\:030a-\:030c\:0326-\:0328",
  AccentLetter -> "ÁÂÄ-ÇÉÍÎÑÓ-ÖØÚÜÝáâä-çéíîñó-öøúüýĂ-ćČ-đĘęĞğİıĹĺĽľŁ-ńŇňŐőŔŕŚśŞ-šŤťŰűŹ-žƵƶǢǣǼ-ǿȘ-țЁЇЎЙйёїў"
]

(**************************************************************************************************)

PublicStringPattern[LetterClass, ExceptLetterClass]

SetUsage @ "
LetterClass['str$'] represents a character class in a regex.
LetterClass[e$1, e$2] is the union of multiple such classes.
* individual elements can be other constructs like %LetterCharacter, %AlphabetCharacter[$$], etc.
"

SetUsage @ "
ExceptLetterClass['str$'] represents the negation of a character class in a regex.
LetterClass[e$1, e$2] is the union of multiple such classes.
* individual elements can be other constructs like %LetterCharacter, %AlphabetCharacter[$$], etc.
"

DefineStringPatternMacro[
  LetterClass[args___]       :> RawLetterClass[args],
  ExceptLetterClass[args___] :> Except[RawLetterClass[args]]
];

