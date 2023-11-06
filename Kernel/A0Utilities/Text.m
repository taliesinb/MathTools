PublicFunction[PossibleFirstNameQ, PossibleFullNameQ]

(*
created with:
$AuthorFirstNames = Part[SplitFirstLastName /@ BearPeople[], All, 1];
$EnglishFirstNames = StringSplit[ImportUTF8["~/Downloads/FirstNames.txt"],"\n"];
$AllFirstNames = Union[ToLowerCase @ $EnglishFirstNames, ToLowerCase @ $AuthorFirstNames];
$ds = CreateDataStructure["BloomFilter", 2000]
Scan[$ds["Insert",#]&, $AllFirstNames];
Export["~/git/qg/Data/Text/FirstNames.mx", $ds]
*)

$firstNamesURL = "https://www.usna.edu/Users/cs/roche/courses/s15si335/proj1/files.php%3Ff=names.txt&downloadcode=yes";

LoadFirstNamesBloomFilter[] := Scope[
  namesBloomFile = LocalPath["Data", "Text", "FirstNames.mx"];
  If[!FileExistsQ[namesDSFile],
    namesTextFile = LocalPath["Data", "Text", "FirstNames.txt"];
    If[!FileExistsQ[namesTextFile] && FailureQ[SafeURLDownload[$firstNamesURL, namesTextFile]], ReturnFailed[]];
    authorFirstNames = Part[SplitFirstLastName /@ BearPeople[], All, 1];
    englishFirstNames = StringSplit[ImportUTF8 @ namesTextFile, "\n"];
    firstNames = Union[ToLowerCase @ englishFirstNames, ToLowerCase @ authorFirstNames];
    bloomFilter = CreateDataStructure["BloomFilter", 3000];
    Scan[bloomFilter["Insert",#]&, firstNames];
    ExportMX[namesBloomFile, bloomFilter];
  ,
    bloomFilter = ImportMX @ namesBloomFile;
  ];
  If[H[bloomFilter] =!= DataStructure, ReturnFailed[]];
  bloomFilter
];

$FirstNamesBloomFilter := $FirstNamesBloomFilter = LoadFirstNamesBloomFilter[];

PossibleFirstNameQ[str_Str] := $FirstNamesBloomFilter["CouldContain", ToLowerCase @ str];

PossibleFullNameQ[str_Str] := Or[
  And[
    StringMatchQ[str, FullNamePhrase],
    PossibleFirstNameQ[P1 @ StringSplit[str, " ", 2]]
  ],
  MemberQ[$KnownAuthors, str]
];

(**************************************************************************************************)

PublicFunction[ExtractTitleAuthor]

ExtractTitleAuthor[title_Str] := Scope[
  person = None;
  Scan[FirstStringCase[title, #]&, {
    StartOfString ~~ t___ ~~ "(" ~~ Shortest[p___] ~~ ")" ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = t; Goto[Done]],
    StartOfString ~~ Shortest[p___] ~~ (": " | " - " | " -- " | " --- " | " | ") ~~ t___ ~~ "" ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = t; Goto[Done]],
    StartOfString ~~ t___ ~~ " - " ~~ p___ ~~ "" ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = t; Goto[Done]],
    StartOfString ~~ p___ ~~ " \"" ~~ t___ ~~ "\"" ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = t; Goto[Done]],
    StartOfString ~~ t___ ~~ p:(TitlecaseWord ~~ " " ~~ TitlecaseWord) ~~ " (" ~~ u___ ~~ ")" ~~ EndOfString :>
      If[StringContainsQ[u, "university", IgnoreCase -> True] && PossibleFullNameQ[p], person = p; title = t; Goto[Done]],
    StartOfString ~~ t1___ ~~ " - " ~~ p:FullNamePhrase ~~ " - " ~~ t2___ ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = StringJoin[t1, " - ", t2]; Goto[Done]]
  }];
  Return @ {None, None};
  Label[Done];
  title = StringTrim @ StringTrim[StringTrim @ title, "\"" | "\[OpenCurlyDoubleQuote]" | "\[CloseCurlyDoubleQuote]"];
  {person, title}
]

(**************************************************************************************************)

PublicFunction[SplitFirstLastName]

$chineseName = "[A-Z][a-z]{1,4}";
$chineseNamePattern = RegularExpression[$chineseName <> " " <> $chineseName <> " " <> $chineseName];

$surnamePrefix = LowercaseWord|"Van"|"Der"|"De"|"Von"|"St"|"Del";

SplitFirstLastName[str_Str] := Which[
  StringMatchQ[str, TitlecaseWord ~~ (" " ~~ $surnamePrefix).. ~~ " " ~~ TitlecaseWord], StringSplit[str, " ", 2],
  StringMatchQ[str, $chineseNamePattern],                                                StringReverse @ Rev @ StringSplit[StringReverse @ str, " ", 2],
  StringContainsQ[str, ", "],                                                            Rev @ StringSplit[str, ", ", 2],
  StringMatchQ[str, TitlecaseWord ~~ (" " ~~ TitlecaseWord)..],                          StringReverse @ Rev @ StringSplit[StringReverse @ str, " ", 2],
  StringContainsQ[str, " "],                                                             MapLast[trimInitials] @ StringSplit[str, " ", 2],
  True,                                                                                  {str}
];

SplitFirstLastName["Aleph 0"] = {"Aleph 0"};

trimInitials[s_Str] := StringTrimLeft[s, (UppercaseLetter ~~ " " | ". ")..];

(**************************************************************************************************)

PublicFunction[ExtractFirstName, ExtractLastName]

ExtractFirstName[str_Str] := P1 @ SplitFirstLastName @ str;
ExtractLastName[str_Str] := PN @ SplitFirstLastName @ str;

(**************************************************************************************************)

PublicFunction[DeleteMiddleInitials]

DeleteMiddleInitials[str_] := StringReplaceRepeated[str, (" " ~~ UppercaseLetter ~~ " " | ". ") -> " "];

(**************************************************************************************************)

(* not sure why this is necessary, message happens in PublicVariable on reload *)
Clear[$englishWordsData, $EnglishWords, $LowercaseEnglishWords, $TitlecaseEnglishWords, $ProperNames];

PublicFunction[EnglishWordQ, LowercaseEnglishWordQ, TitlecaseEnglishWordQ, ProperNameQ]
PublicVariable[$EnglishWords, $LowercaseEnglishWords, $TitlecaseEnglishWords, $ProperNames]

EnglishWordQ[str:(_Str | {___Str})]          := Lookup[$assocEnglishWords, str, False];
LowercaseEnglishWordQ[str:(_Str | {___Str})] := Lookup[$assocLowercaseEnglishWords, str, False];
TitlecaseEnglishWordQ[str:(_Str | {___Str})] := Lookup[$assocTitlecaseEnglishWords, str, False];
ProperNameQ[str:(_Str | {___Str})]           := Lookup[$assocProperNames, str, False];

$englishWordsPath = LocalPath["Data", "Text", "EnglishWords.mx"];
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
$TitlecaseEnglishWords := $TitlecaseEnglishWords = Keys @ Part[$englishWordsData, 3];
$ProperNames := $ProperNames                     = Keys @ Part[$englishWordsData, 4];

$assocEnglishWords := $assocEnglishWords                   = Part[$englishWordsData, 1];
$assocLowercaseEnglishWords := $assocLowercaseEnglishWords = Part[$englishWordsData, 2];
$assocTitlecaseEnglishWords := $assocTitlecaseEnglishWords = Part[$englishWordsData, 3];
$assocProperNames := $assocProperNames                     = Part[$englishWordsData, 4];

(**************************************************************************************************)

PublicVariable[$MathWords]

$MathWords := $MathWords = StringSplit[ImportUTF8 @ LocalPath["Data", "Text", "MathWords.txt"], "\n"];

(*************************************************************************************************)

PublicVariable[$TitleNormalizationRules]

SetInitialValue[$TitleNormalizationRules, {
  "\[OpenCurlyDoubleQuote]" -> "\"",
  "\[CloseCurlyDoubleQuote]" -> "\"",
  "\[OpenCurlyQuote]" -> "'",
  "\[CloseCurlyQuote]" -> "'",
  "\[Dash]" -> "-"
}];
