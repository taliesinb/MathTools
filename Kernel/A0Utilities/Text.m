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
    standardFirstNames = SSplit[ImportUTF8 @ standardFirstNamesPath, "\n"];

    localFirstNamesPath = DataPath["Text", "LocalFirstNames.txt"];
    If[!FileExistsQ[localFirstNamesPath],
      localFirstNames = Part[SSplit[BearPeople[], " ", 2], All, 1];
      localFirstNames = Union @ Select[localFirstNames, SLen[#] > 1 && UpperCaseFirstQ[#]&];
      localFirstNames = Decases[localFirstNames, "Diff" | "St"];
      ExportUTF8[localFirstNamesPath, SRiffle[localFirstNames, "\n"]];
    ,
      localFirstNames = SSplit[ImportUTF8 @ localFirstNamesPath, "\n"]
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

SetCached[$FirstNamesBloomFilter, CachedInto[$BloomFilters, "FirstNames", LoadFirstNamesBloomFilter[]]];

PossibleFirstNameQ[str_Str] := $FirstNamesBloomFilter["CouldContain", ToLowerCase @ str];

PossibleFullNameQ[str_Str] := Or[
  And[
    SMatchQ[str, FullNamePhrase],
    PossibleFirstNameQ @ F @ SSplit[str, " ", 2]
  ],
  MemberQ[$KnownAuthors, str]
];

(**************************************************************************************************)

PublicFunction[SplitFirstLastName]

SplitFirstLastName[str_String] := STrim @ splitName @ str;
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
  cands = Pick[$KnownAuthors, SMatchQ[$KnownAuthors, init ~~ LetterCharacter.. ~~ " " ~~ last]];
  If[Len[cands] == 1, Return @ splitName @ F @ cands];
  If[Len[cands] > 1, Message[SplitFirstLastName::multiple, init <> " " <> last, cands]];
  {init, last}
];

(**************************************************************************************************)

PublicFunction[ExtractFirstName, ExtractLastName]

ExtractFirstName[str_Str] := F @ SplitFirstLastName @ str;
ExtractLastName[str_Str] := L @ SplitFirstLastName @ str;

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
    systemWords = STrim @ SSplit[ImportUTF8 @ $systemWordsFile, "\n"];
    words = Decases[""] @ Union[words, systemWords];
  ];
  properNames = If[FileExistsQ[$systemProperNamesFile],
    STrim @ SSplit[ImportUTF8 @ $systemProperNamesFile, "\n"],
    {}
  ];
  lowerWords = Select[words, LowerCaseFirstQ];
  upperWords = Select[words, UpperCaseFirstQ];
  assocs = ConstantUAssociation[#, True]& /@ {words, lowerWords, upperWords, properNames};
  ExportMX[$englishWordsPath, assocs];
  assocs
];

SetCached[$englishWordsData, loadEnglishWordsData[]];

SetCached[$EnglishWords,          Keys @ P1[$englishWordsData]];
SetCached[$LowercaseEnglishWords, Keys @ P2[$englishWordsData]];
SetCached[$TitleCaseEnglishWords, Keys @ P3[$englishWordsData]];
SetCached[$ProperNames,           Keys @ P4[$englishWordsData]];

SetCached[$assocEnglishWords,          P1[$englishWordsData]];
SetCached[$assocLowercaseEnglishWords, P2[$englishWordsData]];
SetCached[$assocTitleCaseEnglishWords, P3[$englishWordsData]];
SetCached[$assocProperNames,           P4[$englishWordsData]];

(**************************************************************************************************)

PublicVariable[$MathWords]

SetCached[$MathWords, SSplit[ImportUTF8 @ DataPath["Text", "MathWords.txt"], "\n"]];

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
    SEndsQ[str, lhs] &&
    $validDepluralQ[cand = SDrop[str, -SLen[lhs]] <> rhs],
      Return[cand, Block]],
    $depluralTuples
  ];
  str
];

loadTextTable[filename_] := Scope[
  fileStr = STrim @ ImportUTF8 @ DataPath["Text", filename];
  table = STrim /@ SExtract[fileStr, "\n" -> All, " ".. -> All];
  RepAll[table, "_" -> ""]
]

SetCached[$depluralTuples, loadTextTable["DepluralRules.txt"]];

SetCached[$depluralBlacklist, SSplit @ ImportUTF8 @ DataPath["Text", "DepluralBlacklist.txt"]];

SetCached[$validDepluralQ, ConstantUAssociation[Comp[$EnglishWords, $depluralBlacklist], True]];

(**************************************************************************************************)

PublicFunction[WordFrequencySort]

WordFrequencySort[words_] := Scope[
  freq = WordFrequencyData[words];
  freq = RepAll[freq, _Missing -> 0];
  ReverseSortBy[words, freq]
];