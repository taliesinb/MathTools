PublicFunction[MarkdownFrontMatter]

MarkdownFrontMatter[path_Str | File[path_Str]] := Scope[
  
  path //= NormalizePath;

  If[!FileExistsQ[path], ReturnFailed[]];

  str = ReadString[path];

  If[SStartsQ[str, "{"],
    jsonStr = FirstStringCase[str, json:(StartOfString ~~ "{\n" ~~ Shortest[___] ~~ "\n}\n") :> json];
    If[StrQ[jsonStr],
      res = ReadRawJSONString @ jsonStr;
      res = res /. Null -> None;
      If[AssocQ[res], Return @ res]
    ];
  ];

  None
];

(**************************************************************************************************)

PrivateFunction[getMarkdownUnixTime]

getMarkdownUnixTime[path_Str] := Scope[
  Quiet[
    stream = OpenRead[path];
    line = L @ ReadList[stream, "String", 2];
    Close[stream];
  ];
  If[!StrQ[line], Return[None]];
  matches = SCases[line, "unixtime\":" ~~ d:DigitCharacter.. :> d, 1];
  If[matches === {}, Return[None]];
  FromDigits @ F @ matches
];

(**************************************************************************************************)

PublicFunction[NotebookFrontMatter]

NotebookFrontMatter[_] := $Failed;

NotebookFrontMatter[nb_NotebookObject] :=
  NotebookFrontMatter @ NotebookFileName @ nb;

$frontMatterMetadataCache = UAssoc[];

(* TODO: deal with notebooks that have changed in FE but haven't been saved yet! *)
NotebookFrontMatter[path_Str | File[path_Str]] := Scope[
  
  path //= NormalizePath;
  fileDate = FileDate @ path;
  
  result = Lookup[$frontMatterMetadataCache, path, None];
  If[AssocQ[result],
    cachedDate = Lookup[result, "unixtime", 0];
    If[cachedDate === UnixTime[fileDate], Goto[Done]];
  ];

  filebase = FileBaseName @ path;

  numbering = STrim @ FirstStringCase[filebase, DigitCharacter.. ~~ " ", ""];
  weight = If[numbering === "", 999, FromDigits @ numbering];

  {title, subTitle, taggingRules} = getNotebookData[path];
  SetNone[title, trimNumberPrefix @ filebase];
  fileDate = DatePlus[fileDate, -1]; (* to force Hugo to render the page *)
  dateString = DateString[fileDate, {"Year", "-", "Month", "-", "Day"}];

  result = Assoc[
    "unixtime" -> UnixTime @ fileDate,
    "date" -> dateString,
    "weight" -> weight,
    "title" -> title,
    "summary" -> subTitle,
    "notebookpath" -> path,
    KSelect[taggingRules, StrQ[#] && LowerCaseQ[STake[#, 1]]&]
  ];

  $frontMatterMetadataCache[path] ^= result;

  Label[Done];

  result["relativepath"] = SubNone[RelativePath[$notebookPath, FileNameDrop @ path], ""];

  If[$frontMatterFunction =!= None,
    result //= $frontMatterFunction;
  ];

  result
];

getNotebookData[path_Str] := Scope[
  nb = Get @ path;
  title = FirstCase[nb, Cell[title_Str, "Title"|"Chapter"|"Section", ___] :> title, None, Inf];
  subTitle = FirstCase[nb, Cell[subtitle_Str, "Subtitle", ___] :> subtitle, None, Inf];
  SetNone[subTitle, notebookFirstLine @ nb];
  taggingRules = LookupOption[nb, TaggingRules];
  If[RuleListQ[taggingRules], taggingRules //= Assoc];
  SetAuto[taggingRules, <||>];
  KDropFrom[taggingRules, "TryRealOnly"];
  {title, subTitle, taggingRules}
];

notebookFirstLine[nb_] :=
  FirstCase[nb,
    Cell[b_ /; FreeQ[b, _GraphicsBox], "Text", ___] :>
      With[{res = boxesFirstLine @ b}, res /; SLen[res] > 5],
    None, Inf
  ];

boxesFirstLine[b_] := Scope[
  str = Quiet @ CatchMessage @ textCellToMarkdown @ b;
  If[!StrQ[str], Return[None]];
  split = SSplit[str, w:("." | "?" | "!" | "...") ~~ (EndOfString | EndOfLine | WhitespaceCharacter) :> w, 2];
  STrim @ SJoin @ Take[split, UpTo[2]]
];