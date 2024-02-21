PublicFunction[PossibleFirstNameQ, PossibleFullNameQ]

$firstNamesURL = "https://www.usna.edu/Users/cs/roche/courses/s15si335/proj1/files.php%3Ff=names.txt&downloadcode=yes";

CacheVariable[$BloomFilters]

LoadFirstNamesBloomFilter[] := Scope[
  namesBloomFile = DataPath["Text", "FirstNames.mx"];
  If[!FileExistsQ[namesBloomFile],
    PrintTemporary["Populating Bloom filter for first names."];

    standardFirstNamesPath = DataPath["Text", "StandardFirstNames.txt"];
    If[!FileExistsQ[standardFirstNamesPath] && FailureQ[
      PrintTemporary["Downloading names.txt"];
      SafeURLDownload[$firstNamesURL, standardFirstNamesPath]], ReturnFailed[]];
    standardFirstNames = StringSplit[ImportUTF8 @ standardFirstNamesPath, "\n"];

    localFirstNamesPath = DataPath["Text", "LocalFirstNames.txt"];
    If[!FileExistsQ[localFirstNamesPath],
      localFirstNames = Part[StringSplit[BearPeople[], " ", 2], All, 1];
      localFirstNames = Union @ Select[localFirstNames, StringLength[#] > 1 && UpperCaseFirstQ[#]&];
      localFirstNames = DeleteCases[localFirstNames, "Diff" | "St"];
      ExportUTF8[localFirstNamesPath, StringRiffle[localFirstNames, "\n"]];
    ,
      localFirstNames = StringSplit[ImportUTF8 @ localFirstNamesPath, "\n"]
    ];

    firstNames = Union[ToLowerCase @ standardFirstNames, ToLowerCase @ localFirstNames];
    bloomFilter = CreateDataStructure["BloomFilter", 3000];
    Scan[bloomFilter["Insert", #]&, firstNames];
    ExportMX[namesBloomFile, bloomFilter];
  ,
    primeBloom[];
    bloomFilter = ImportMX @ namesBloomFile;
    If[H[bloomFilter] =!= DataStructure,
      Print["Bloom filter file is corrupt."];
      TrashFile[namesBloomFile];
      Return @ LoadFirstNamesBloomFilter[];
    ];
  ];
  If[H[bloomFilter] =!= DataStructure, ReturnFailed[]];
  bloomFilter
];

primeBloom[] := (CreateDataStructure["BloomFilter", 20]; Clear[primeBloom]);

$FirstNamesBloomFilter := $FirstNamesBloomFilter = CachedInto[$BloomFilters, "FirstNames", LoadFirstNamesBloomFilter[]];

PossibleFirstNameQ[str_Str] := $FirstNamesBloomFilter["CouldContain", ToLowerCase @ str];

PossibleFullNameQ[str_Str] := Or[
  And[
    StringMatchQ[str, FullNamePhrase],
    PossibleFirstNameQ @ P1 @ StringSplit[str, " ", 2]
  ],
  MemberQ[$KnownAuthors, str]
];

(**************************************************************************************************)

PublicFunction[SplitFirstLastName]

SplitFirstLastName[str_String] := StringTrim @ splitName @ str;
SplitFirstLastName["Aleph 0"] = {"Aleph 0"};

splitName = StringCase[
  first:NameWord ~~ " " ~~ mid:NameWord ~~ " " ~~ last:NameWord := If[PossibleFirstNameQ[mid], {first <> " " <> mid, last}, {first, mid <> " " <> last}];
  first:NameWord ~~ last:(SurnamePrefix.. ~~ " " ~~ NameWord)   := {first, last};
  last__ ~~ ", " ~~ first__                                     := {first, last};
  init:UppercaseLetter ~~ Maybe["."] ~~ " " ~~ last:NameWord    := resolveInitial[init, last];
  Shortest[first__] ~~ " " ~~ last__                            := {first, trimInitials @ last};
  str__                                                         := {str};
,
  {NameWord -> TitleCaseWord, SurnamePrefix -> " " ~~ LowercaseWord|"Van"|"Der"|"De"|"Von"|"St"|"Del"}
];

trimInitials[s_Str] := StringTrimLeft[s, (UppercaseLetter ~~ " " | ". ")..];

SplitFirstLastName::multiple = "Multiple candidates match ``: ``.";
resolveInitial[init_, last_] := Scope[
  cands = Pick[$KnownAuthors, StringMatchQ[$KnownAuthors, init ~~ LetterCharacter.. ~~ " " ~~ last]];
  If[Length[cands] == 1, Return @ splitName @ First @ cands];
  If[Length[cands] > 1, Message[SplitFirstLastName::multiple, init <> " " <> last, cands]];
  {init, last}
];

(**************************************************************************************************)

PublicFunction[ExtractFirstName, ExtractLastName]

ExtractFirstName[str_Str] := P1 @ SplitFirstLastName @ str;
ExtractLastName[str_Str] := PN @ SplitFirstLastName @ str;

(**************************************************************************************************)

PublicFunction[DeleteMiddleInitials]

DeleteMiddleInitials[str_] := StringReplaceRepeated[str, (" " ~~ UppercaseLetter ~~ " " | ". ") -> " "];

(**************************************************************************************************)

(* not sure why this is necessary, message happens in PublicVariable on reload *)
Clear[$englishWordsData, $EnglishWords, $LowercaseEnglishWords, $TitleCaseEnglishWords, $ProperNames];

PublicFunction[EnglishWordQ, LowercaseEnglishWordQ, TitleCaseEnglishWordQ, ProperNameQ]
PublicVariable[$EnglishWords, $LowercaseEnglishWords, $TitleCaseEnglishWords, $ProperNames]

EnglishWordQ[str:(_Str | {___Str})]          := Lookup[$assocEnglishWords, str, False];
LowercaseEnglishWordQ[str:(_Str | {___Str})] := Lookup[$assocLowercaseEnglishWords, str, False];
TitleCaseEnglishWordQ[str:(_Str | {___Str})] := Lookup[$assocTitleCaseEnglishWords, str, False];
ProperNameQ[str:(_Str | {___Str})]           := Lookup[$assocProperNames, str, False];

$englishWordsPath = DataPath["Text", "EnglishWords.mx"];
$systemWordsFile = "/usr/share/dict/words";
$systemProperNamesFile = "/usr/share/dict/propernames";

loadEnglishWordsData[] := Scope[
  If[FileExistsQ[$englishWordsPath], Return @ ImportMX[$englishWordsPath]];
  words = DictionaryLookup[];
  If[FileExistsQ[$systemWordsFile],
    systemWords = StringTrim @ StringSplit[ImportUTF8 @ $systemWordsFile, "\n"];
    words = DeleteCases[""] @ Union[words, systemWords];
  ];
  properNames = If[FileExistsQ[$systemProperNamesFile],
    StringTrim @ StringSplit[ImportUTF8 @ $systemProperNamesFile, "\n"],
    {}
  ];
  lowerWords = Select[words, LowerCaseFirstQ];
  upperWords = Select[words, UpperCaseFirstQ];
  assocs = ConstantUAssociation[#, True]& /@ {words, lowerWords, upperWords, properNames};
  ExportMX[$englishWordsPath, assocs];
  assocs
];

$englishWordsData := $englishWordsData = loadEnglishWordsData[];

$EnglishWords := $EnglishWords                   = Keys @ Part[$englishWordsData, 1];
$LowercaseEnglishWords := $LowercaseEnglishWords = Keys @ Part[$englishWordsData, 2];
$TitleCaseEnglishWords := $TitleCaseEnglishWords = Keys @ Part[$englishWordsData, 3];
$ProperNames := $ProperNames                     = Keys @ Part[$englishWordsData, 4];

$assocEnglishWords := $assocEnglishWords                   = Part[$englishWordsData, 1];
$assocLowercaseEnglishWords := $assocLowercaseEnglishWords = Part[$englishWordsData, 2];
$assocTitleCaseEnglishWords := $assocTitleCaseEnglishWords = Part[$englishWordsData, 3];
$assocProperNames := $assocProperNames                     = Part[$englishWordsData, 4];

(**************************************************************************************************)

PublicVariable[$MathWords]

$MathWords := $MathWords = StringSplit[ImportUTF8 @ DataPath["Text", "MathWords.txt"], "\n"];

(*************************************************************************************************)

PublicVariable[$TitleNormalizationRules, $AbstractNormalizationRules]

SetInitialValue[$TitleNormalizationRules, {
  "\[InvisibleSpace]" -> "",
  "``" -> "\"", "''" -> "\"",
  "\[OpenCurlyDoubleQuote]" -> "\"",
  "\[CloseCurlyDoubleQuote]" -> "\"",
  "\[OpenCurlyQuote]" -> "'",
  "\[CloseCurlyQuote]" -> "'",
  "\[Dash]" -> "-"
}];


SetInitialValue[$AbstractNormalizationRules, {
  "\[InvisibleSpace]" -> "",
  "``" -> "\"", "''" -> "\"",
  "\n" ~~ Repeated[" "... ~~ "\n"] :> "\n",
  "\[OpenCurlyDoubleQuote]" -> "\"",
  "\[CloseCurlyDoubleQuote]" -> "\"",
  "\[OpenCurlyQuote]" -> "'",
  "\[CloseCurlyQuote]" -> "'",
  "`" -> "'"
}];

(**************************************************************************************************)

PublicFunction[Depluralize]

SetListable[Depluralize];

Depluralize = Case[
  word_Str ? UpperCaseQ := word;
  word_Str              := tryDepluralRules[word];
];

tryDepluralRules[str_Str] := Scope[
  Scan[{{lhs, rhs}} |-> If[
    StringEndsQ[str, lhs] &&
    $validDepluralQ[cand = StringDrop[str, -StrLen[lhs]] <> rhs],
      Return[cand, Block]],
    $depluralTuples
  ];
  str
];

loadTextTable[filename_] := Scope[
  fileStr = StringTrim @ ImportUTF8 @ DataPath["Text", filename];
  table = StringTrim /@ StringExtract[fileStr, "\n" -> All, " ".. -> All];
  ReplaceAll[table, "_" -> ""]
]

$depluralTuples := $depluralTuples = loadTextTable["DepluralRules.txt"];

$depluralBlacklist := $depluralBlacklist = StringSplit @ ImportUTF8 @ DataPath["Text", "DepluralBlacklist.txt"];

$validDepluralQ := $validDepluralQ = ConstantUAssociation[Complement[$EnglishWords, $depluralBlacklist], True];

(**************************************************************************************************)

PublicFunction[WordFrequencySort]

WordFrequencySort[words_] := Scope[
  freq = WordFrequencyData[words];
  freq = ReplaceAll[freq, _Missing -> 0];
  ReverseSortBy[words, freq]
];

