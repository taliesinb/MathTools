PublicIOFunction[ExportBearNoteToObsidian]

PublicOption[CompressImages]

(* this assumes use of Custom Attachment Location, specified to use ./assets, image-${date}, YYYYMMDDHHmmssSSS *)

General::noBearNote = "No note found with title ``."
General::missingBearImage = "Could not find image at ``.";
General::badBearImage = "Could not read image at ``.";
General::missingAttachments = "Could not read attachments for ``.";
General::unknownAttachment = "Could not find attachment data for file ``.";

Options[ExportBearNoteToObsidian] = {
  ReplaceExisting -> False,
  ObsidianVault -> Auto,
  CompressImages -> True,
  PDFPath -> Auto,
  DryRun -> False,
  Verbose -> Auto
}

(* TODO: avoid creating alias pages *)
ExportBearNoteToObsidian[title_Str, OptionsPattern[]] := Scope @ CatchMessage[

  UnpackOptions[obsidianVault, compressImages, replaceExisting, $verbose, $dryRun];
  $pdfPath = OptionValue[PDFPath];
  SetAuto[$verbose, $dryRun];
  SetAuto[obsidianVault, $DefaultObsidianVault];

  results = BearNoteData["Title" -> title, {"ID", "HasFilesQ", "HasImagesQ", "Title", "CreationDate", "ModificationDate", "Text", "UUID"}];

  If[Len[results] =!= 1, ReturnFailed["noBearNote", MsgExpr @ title]];
  data = F @ results;

  UnpackAssociation[data, ID, hasFilesQ, hasImagesQ, title, creationDate, modificationDate, text, UUID];
  VPrint["Obtained data for note with ID ", ID, ", UUID ", UUID, " title \"", title, "\""];

  hasAttachments = Max[hasFilesQ, hasImagesQ] == 1;
  If[hasAttachments,
    $attachments = BearNoteAttachmentData[ID];
    If[FailureQ[$attachments], ReturnFailed["missingAttachments", MsgExpr @ title]];
    VPrint["Found ", Len @ $attachments, " attachments in database."];
  ,
    $attachments = Assoc[];
  ];

  text //= STrim;
  text = STrim @ StringTrimLeft[text, "# " ~~ " "... ~~ title <> "\n"];

  frontMatter = Assoc[];
  akas = getTextAkas @ text;
  If[StrVecQ[akas] && akas =!= {},
    frontMatter["aliases"] = Decases[akas, title];
    text = STrim @ SDelete[text, "#meta/aka " ~~ Shortest[__] ~~ EndOfLine];
  ];

  tags = {}; noteFolder = None; mainTag = None;
  If[SStartsQ[text, "#" ~~ LetterCharacter],

    {tagLine, text} = SSplit[text, EndOfLine, 2];
    text // STrim;
    VPrint["Found tag line: ", tagLine];

    extraMetaTag = Nothing;
    If[SStartsQ[tagLine, "#meta/master for "],
      tagLine //= StringTrimLeft["#meta/master for "];
      extraMetaTag = "#meta/master";
    ];
    mainTag = F @ SSplit[tagLine, " ", 2];
    VPrint["Main tag: ", mainTag];
    fieldTags = STrim @ SCases[tagLine, field:(" #field/" ~~ (LetterCharacter | "/")..)];

    tags = Flatten @ {mainTag, extraMetaTag, fieldTags};
    tagPath = SSplit[StringTrimLeft[mainTag, "#"], "/"];
    noteFolder = F[tagPath, None];

    Label[retry];
    proc = Lookup[$tagProcessors, tagStr = SRiffle[tagPath, "/"], None];
    If[proc === None && Len[tagPath] > 1,
      tagPath //= Most; Goto[retry]];

    If[proc =!= None,
      VPrint["Applying tag processor for ", tagStr, "."];
      extra = proc[title, tagLine, text]];

    If[AssocQ[extra],
      frontMatter = Join[frontMatter, extra];
      If[!MissingQ[newText = frontMatter[".contents"]],
        text = newText;
        frontMatter[".contents"] =. ;
      ];
    ];
  ,
    VPrint["No tagline present: ", STake[text, UpTo[20]]];
  ];
  text = SRep[text, "![](" ~~ imageName:ShortBlank ~~ ")" :> copyImageToObsidian[imageName]];

  metaTags = SCases[text, "#meta/" ~~ Except[WhitespaceCharacter]..];
  If[metaTags =!= {},
    JoinTo[tags, metaTags];
    text //= SDelete[StartOfLine ~~ "#meta/" ~~ LineFragment ~~ "\n"];
  ];
  text //= SRep[Repeated["\n", {3, Inf}] -> "\n\n"] /* STrim;

  isMaster = ContainsQ[tags, "#meta/master"];
  mainTag //= StringTrimLeft["#"];
  If[isMaster, mainTag = SRiffle[Most @ SSplit[mainTag, "/"], "/"]];

  PreTo[frontMatter, "type" -> mainTag];
  frontMatter["tags"] = Dedup @ Join[tags, Lookup[frontMatter, "tags", {}]];
  frontMatter["bear"] = $OpenBearNoteIDTemplateSimple @ UUID;

  VPrint["Frontmatter: "];
  If[$verbose, VPrint[ExportYAMLDict @ frontMatter]];
  mdPath = CreateObsidianNote[title, text,
    NoteFolder -> noteFolder,
    CreationTime -> FromBearTime[creationDate], ModificationTime -> FromBearTime[modificationDate],
    ObsidianVault -> obsidianVault, FrontMatter -> frontMatter,
    DryRun -> $dryRun, ReplaceExisting -> replaceExisting
  ];

  If[FailureQ[mdPath], ReturnFailed[]];

  mdPath
];

(* TODO: replace this wiith Bear's aka mechanism *)
getTextAkas[text_] := Scope[
  If[SFreeQ[text, StartOfLine ~~ "#meta/aka "], Return[{}]];
  akaStr = FirstStringCase[text, "#meta/aka " ~~ akas:Shortest[__] ~~ EndOfLine :> akas];
  akaStr = SDelete[akaStr, ParentheticalPhrase];
  If[akaStr === None, Return[{}]];
  akaStr = SRep[akaStr, inner:("\"" ~~ ShortBlank ~~ "\"") /; SContainsQ[inner, ","] :> SRep[inner, "," -> "COMMA"]];
  akas = STrim @ SSplit[akaStr, ","];
  akas = StringTrimLeft[akas, "or "];
  akas = StringTrimRight[akas, " in " ~~ ___];
  akas = StringTrimLeftRight[akas, "\"", "\""];
  akas = StringTrimLeftRight[akas, "[[", "]]"];
  akas = SRep[akas, "COMMA" -> ","];
  akas
];

(* we need to make a black list, based on common false positives *)

(*************************************************************************************************)

$OpenBearNoteIDTemplateSimple = StringFunction @ "bear://x-callback-url/open-note?id=#1";

(*************************************************************************************************)

$tagProcessors = <|
  "doc" -> docMetadata,
  "person" -> personMetadata,
  "org/company" -> companyMetadata
|>;

(*************************************************************************************************)

findTypedLinksAfter[contents_Str, pattern_, type_] := Scope[
  Flatten @ SCases[contents, pattern ~~ lf:LineFragment :>
    SCases[lf, link:MarkdownHyperlinkPattern /; linkHasTypeQ[link, type]],
    IgnoreCase -> True
  ]
];

linkHasTypeQ[link_Str, type_] := Scope[
  text = Quiet @ BearNoteText @ STake[link, {3, -3}];
  StrQ[text] && SContainsQ[text, type]
];

(*************************************************************************************************)

companyMetadata[title_Str, tagLine_Str, contents_Str] := Scope[
  body = tagLine <> " " <> contents;
  founders = findTypedLinksAfter[body, "founded by " | "co-founded by ", "#person"];
  Assoc[
    "founders" -> founders
  ];
];

  Assoc[
    "title" -> trimmedTitle,
    "authors" -> authors,
    "tags" -> tags,
    "abstract" -> abstract,
    "concepts" -> concepts,
    "date" -> date,
    "path" -> localPath,
    "url" -> url,
    "journal" -> journal,
    ".contents" -> newContents
  ]

(*************************************************************************************************)

personMetadata[title_Str, tagLine_Str, contents_Str] := Scope[
  Assoc[];
];

(*************************************************************************************************)

$pdfPath = Auto;

General::badBearDocTitle = "Bad title `` for a document.";
docMetadata[title_Str, tagLine_Str, contents_Str] := Scope[

  {by, venue, in, about} = stringSectionSplit[tagLine, {" by ", " in " ~~ PositiveLookahead["#journal"], " in ", " about " | " defining " | " connecting "}];
  authors = SCases[by, {link:MarkdownNoteLinkPattern :> link, name:FullNamePhrase :> simplyFullName @ name}];
  tags = SCases[in, MarkdownTagPattern];
  journal = FirstStringCase[venue, MarkdownTagPattern];

  {min, max} = MinMax @ SFind[title, "\""];
  If[max <= min, ThrowMessage["badBearDocTitle", MsgExpr @ title]];
  trimmedTitle = STake[title, {min + 1, max - 1}];
  path = toPaperPDFPath[$pdfPath, title];
  url = FirstStringCase[contents, HyperlinkPattern];
  newContents = SDelete[contents, url];

  If[!FileExistsQ[path],
    glob = "*" <> ToLowerCase[trimmedTitle] <> "*.pdf";
    candidates = FileNames[glob, SubAuto[$pdfPath, $PDFPath], IgnoreCase -> True];
    If[Len[candidates] === 1,
      candidate = F @ candidates;
      VPrint["Found candidate ", MsgPath @ candidate, " for missing path ", MsgPath @ path];
      path = candidate;
    ,
      If[SContainsQ[contents, "#meta/downloaded"], ThrowMessage["missingLocalPath", MsgExpr @ title]];
    ];
  ];
  localPath = If[FileExistsQ[path], toLocalPath @ path, None];

  abstract = FirstStringCase[contents,
    StartOfLine ~~ "> " ~~ abstract:LineFragment :> abstract
  ];

  tagsStr = SRiffle[tags, " "];

  conceptExtractor = createLocalConceptExtractor[tagsStr];
  abstractStr = SubNone[abstract, ""];
  concepts = Dedup @ Join[
    SCases[about, MarkdownNoteLinkPattern ? conceptLinkQ],
    SCases[abstractStr, MarkdownNoteLinkPattern ? conceptLinkQ],
    SCases[abstractStr, conceptExtractor]
  ];

  date = None;
  If[SContainsQ[url, "arxiv"],
    arxivData = ImportArxivPage[url];
    If[AssocQ[arxivData],
      date = arxivData["Date"];
      If[StrQ[date],
        date = FromDateString[date, {"Year", "/", "Month", "/", "Day"}];
      ];
    ];
  ,
    If[StrQ @ localPath, date = guessPDFdate @ path];
  ];

  abstract //= SRep[link:MarkdownNoteLinkPattern :> STake[link, {3, -3}]];

  conceptPattern = DeepFirstCase[conceptExtractor, _Alternatives];
  $isLinked = UAssoc[]; linkedQ = Fn[Lookup[$isLinked, #, $isLinked[#] = True; False]];
  enrichmentRule = {
    link:MarkdownNoteLinkPattern :> link,
    WordBoundary ~~ lhs:conceptPattern ~~ plural:Maybe["s" | "es"] ~~ WordBoundary :>
      If[linkedQ[lhs], lhs <> plural, "[[" <> lhs <> "]]" <> plural]
  };
  newContents = SRep[
    newContents,
    StartOfLine ~~ abstractLHS:("> " ~~ LineFragment) :> SRep[abstractLHS, enrichmentRule]
  ];

  Assoc[
    "title" -> trimmedTitle,
    "authors" -> authors,
    "tags" -> tags,
    "abstract" -> abstract,
    "concepts" -> concepts,
    "date" -> date,
    "path" -> localPath,
    "url" -> url,
    "journal" -> journal,
    ".contents" -> newContents
  ]
];

enrichWithConceptExtractorLinks[extractor_][text_] := Scope[

  pattern = DeepFirstCase[extractor, _Alternatives];

  rule = {
    link:MarkdownNoteLinkPattern :> link,
    WordBoundary ~~ lhs:pattern ~~ plural:Maybe["s"] ~~ WordBoundary :>
      "[[" <> lhs <> "]]" <> plural
  };

  SRep[text, StartOfLine ~~ "> " ~~ abstract:LineFragment :> SRep[abstract, rule], 1]

];

simplyFullName[str_String] := SJoin["[[",
  SRep[str, (" " ~~ LetterCharacter ~~ Maybe["."] ~~ " ") -> " "],
  "]]"
];

(*************************************************************************************************)

stringSectionSplit[s_, rules_] := Scope[
  sections = SSplit[s, RuleRange @ rules];
  Map[
    Rep[sections, {{___, #, sec_Str, ___} :> sec, _ -> ""}]&,
    Range @ Len @ rules
  ]
];

toLocalPath[path_] := "file://" <> SRep[path, " " -> "%20"];

(*************************************************************************************************)

CacheVariable[$PDFDateCache]

guessPDFdate[path_] := Scope[
  CachedInto[$PDFDateCache, path,
    firstPage = Quiet @ Import[path, {"PagePlaintext", 1}];
    dateStr = FirstStringCase[firstPage, SpelledDatePattern];
    If[!StrQ[dateStr], dateStr = FirstStringCase[firstPage, NumericDatePattern]];
    date = If[StrQ[dateStr], FromDateString @ dateStr, None];
    date
  ]
];

(*************************************************************************************************)

computeBearAkaRules[type_] := computeBearAkaRules[type] = Scope[
  data = BearNoteData[{"Tag" -> type}, {"Title", "Text"}];
  data = Select[data, SFreeQ[#Title, {":", "\""}] && StringCount[#Title, " "] <= 4&];
  akaGraph = Graph @ Flatten @ Map[Thread[App[getTextAkas[#Text], #Title] -> #Title]&, data];
  titleToText = Assoc[#Title -> #Text& /@ data];
  akaComps = WeaklyConnectedComponents[akaGraph];
  primaries = componentToPrimary /@ akaComps;
  Flatten @ MapThread[Thread[#1 -> #2]&, {akaComps, primaries}]
];

componentToPrimary[{a_}] := a;
componentToPrimary[list_] := L @ SortBy[list, SLen @ Lookup[titleToText, #, ""]&];

(*************************************************************************************************)

$bearConceptTitles := $bearConceptTitles =
  ConstantUAssociation[Flatten[List @@@ computeBearAkaRules["concept"]], True];

conceptLinkQ[link_] := Lookup[SDrop[link, {3, -3}], $bearConceptTitles, False];

(*************************************************************************************************)

SetCached[$linkBlacklist, SSplit[ImportUTF8 @ DataPath["Text", "LinkBlacklist.txt"], "\n"]];

(* computed with:
$concepts = BearNoteData[{"Tag" -> "concept"}, {"Title", "Text"}];
$concepts = Select[$concepts, StringFreeQ[#Title, {":", "\""}]&];
$allConceptPhrases = DeleteNone @ Flatten[{#Title, MathTools`External`Bear`getTextAkas[#Text]}& /@ $concepts];
$singleWordConceptPhrases = Select[$allConceptPhrases, StringFreeQ[#, " "] && ASCIIQ[#]&];
$frequencies = WordFrequencyData[$singleWordConceptPhrases];
$frequencies2 = Log @ Select[$frequencies,NumberQ];
CopyToClipboard @ StringRiffle[Keys@Sort@Select[$frequencies2, GreaterThan[-14]], "\n"]
*)

createLocalConceptExtractor[noteTagsStr_Str] := Scope[

  baseRules = computeBearAkaRules["concept"];

  blacklist = $linkBlacklist;
  whitelist = Catenate @ KSelect[$linkWhitelists, SContainsQ[noteTagsStr, #]&];
  blacklist = Comp[blacklist, whitelist];
  blacklistPattern = Alt @@ blacklist;
  filteredRules = Select[baseRules, FreeQ[F[#], blacklistPattern]&];

  titles = Union @ Flatten @ (List @@@ filteredRules);
  titles //= Select[SLen[#] > 2&];
  titles //= ReverseSortBy[SLen];

  (WordBoundary ~~ lhs:(Alt @@ titles) ~~ Maybe["s"] ~~ WordBoundary) :> "[[" <> lhs <> "]]"
];

SetCached[$linkWhitelists, createLinkWhitelists[]];

createLinkWhitelists[] := Scope[
  text = ImportUTF8 @ DataPath["Text", "LinkWhitelists.txt"];
  table = SSplit[SSplit[text, "\n"], " "];
  Assoc @ MapApply[toWLentry, table]
];

toWLentry[key_, vals__] := If[SContainsQ[key, ","],
  # -> {vals}& /@ SSplit[key, ","],
  key -> {vals}
];

(*************************************************************************************************)

copyImageToObsidian[imageName_] := Scope[

  imageName //= URLDecode;
  attachment = Lookup[$attachments, imageName, ThrowMessage["unknownAttachment", MsgPath @ imageName]];
  UnpackAssociation[attachment, fileName, creationDate, insertionDate, uploadDate, extension, UUID];

  filePath = PathJoin[$bearImagePath, UUID, fileName];
  If[!FileExistsQ[filePath], ThrowMessage["missingBearImage", MsgPath @ filePath]];

  shouldCompress = compressImages && extension == "png";
  If[shouldCompress, extension = "jpg"];

  creationDate = FromBearTime @ creationDate;
  dateStr = DateString[creationDate, {"Year", "Month", "Day", "Hour", "Minute", "Second", "Millisecond"}];

  outputFileName = "image-" <> dateStr <> "." <> extension;
  outputPath = PathJoin[obsidianVault, "assets", outputFileName];

  If[shouldCompress,
    img = Import[filePath];
    If[!ImageQ[img], ThrowMessage["badBearImage", MsgPath @ filePath]];
    whenWhet @ Export[outputPath, img, CompressionLevel -> 0.1];
  ,
    whenWhet @ CopyFile[filePath, outputPath, OverwriteTarget -> True];
  ];

  whenWhet @ SetFileTime[outputPath, creationDate];

  "![[" <> outputFileName <> "]]"
];

(*************************************************************************************************)

PublicFunction[FromBearTime]

FromBearTime[t_] := FromUnixTime[978300000 + t];