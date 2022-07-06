PublicFunction[NotebookFrontMatter]

NotebookFrontMatter[nb_NotebookObject] :=
  NotebookFrontMatter @ NotebookFileName @ nb;

$frontMatterMetadataCache = Data`UnorderedAssociation[];

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
  dateString = DateString[fileDate, {"Year", "-", "Day", "-", "Month"}];

  result = Association[
    "date" -> dateString,
    "weight" -> weight,
    "title" -> title,
    "unixtime" -> UnixTime @ fileDate,
    "notebookpath" -> path,
    taggingRules
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
