PublicFunction[MarkdownFrontMatter]

MarkdownFrontMatter[path_String | File[path_String]] := Scope[
  
  path //= NormalizePath;

  str = ReadString[path];

  If[StringStartsQ[str, "{"],
    jsonStr = FirstStringCase[str, json:(StartOfString ~~ "{\n" ~~ Shortest[___] ~~ "\n}\n") :> json];
    If[StringQ[jsonStr],
      res = Developer`ReadRawJSONString @ jsonStr;
      If[AssociationQ[res], Return @ res]
    ];
  ];

  None
];

(**************************************************************************************************)

PrivateFunction[getMarkdownUnixTime]

getMarkdownUnixTime[path_String] := Scope[
  Quiet[
    stream = OpenRead[path];
    line = Last @ ReadList[stream, "String", 2];
    Close[stream];
  ];
  If[!StringQ[line], Return[None]];
  matches = StringCases[line, "unixtime\":" ~~ d:DigitCharacter.. :> d, 1];
  If[matches === {}, Return[None]];
  FromDigits @ First @ matches
];

(**************************************************************************************************)

PublicFunction[NotebookFrontMatter]

NotebookFrontMatter[nb_NotebookObject] :=
  NotebookFrontMatter @ NotebookFileName @ nb;

$frontMatterMetadataCache = UAssociation[];

NotebookFrontMatter[path_String | File[path_String]] := Scope[
  
  path //= NormalizePath;
  fileDate = FileDate @ path;
  
  result = Lookup[$frontMatterMetadataCache, path, None];
  If[AssociationQ[result],
    cachedDate = Lookup[result, "unixtime", 0];
    If[cachedDate === UnixTime[fileDate], Goto[Done]];
  ];

  filebase = FileBaseName @ path;

  numbering = StringTrim @ FirstStringCase[filebase, DigitCharacter.. ~~ " ", ""];
  weight = If[numbering === "", 999, FromDigits @ numbering];

  {title, taggingRules} = getNotebookData[path];
  SetNone[title, trimNumberPrefix @ filebase];
  fileDate = DatePlus[fileDate, -1]; (* to force Hugo to render the page *)
  dateString = DateString[fileDate, {"Year", "-", "Month", "-", "Day"}];

  result = Association[
    "unixtime" -> UnixTime @ fileDate,
    "date" -> dateString,
    "weight" -> weight,
    "title" -> title,
    "notebookpath" -> path,
    KeySelect[taggingRules, StringQ[#] && LowerCaseQ[StringTake[#, 1]]&]
  ];

  $frontMatterMetadataCache[path] ^= result;

  Label[Done];

  result["relativepath"] = RelativePath[$baseImportPath, FileNameDrop @ path];

  If[$frontMatterFunction =!= None,
    result //= $frontMatterFunction;
  ];

  result
];

getNotebookData[path_String] := Scope[
  nb = Get @ path;
  title = FirstCase[nb, Cell[title_String, "Title"|"Chapter"|"Section", ___] :> title, None, Infinity];
  taggingRules = LookupOption[nb, TaggingRules];
  If[RuleListQ[taggingRules], taggingRules //= Association];
  SetAutomatic[taggingRules, <||>];
  KeyDropFrom[taggingRules, "TryRealOnly"];
  {title, taggingRules}
];
