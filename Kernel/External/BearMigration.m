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
  ObsidianVault -> Automatic,
  CompressImages -> True,
  PDFPath -> Automatic,
  DryRun -> False,
  Verbose -> Automatic
}

(* TODO: avoid creating alias pages *)
ExportBearNoteToObsidian[title_Str, OptionsPattern[]] := Scope @ CatchMessage[

  UnpackOptions[obsidianVault, compressImages, replaceExisting, $verbose, $dryRun];
  $pdfPath = OptionValue[PDFPath];
  SetAutomatic[$verbose, $dryRun];
  SetAutomatic[obsidianVault, $DefaultObsidianVault];

  results = BearNoteData["Title" -> title, {"ID", "HasFilesQ", "HasImagesQ", "Title", "CreationDate", "ModificationDate", "Text", "UUID"}];

  If[Length[results] =!= 1, ReturnFailed["noBearNote", MsgExpr @ title]];
  data = First @ results;

  UnpackAssociation[data, ID, hasFilesQ, hasImagesQ, title, creationDate, modificationDate, text, UUID];
  VPrint["Obtained data for note with ID ", ID, ", UUID ", UUID, " title \"", title, "\""];

  hasAttachments = Max[hasFilesQ, hasImagesQ] == 1;
  If[hasAttachments,
    $attachments = BearNoteAttachmentData[ID];
    If[FailureQ[$attachments], ReturnFailed["missingAttachments", MsgExpr @ title]];
    VPrint["Found ", Length @ $attachments, " attachments in database."];
  ,
    $attachments = Assoc[];
  ];

  text //= StringTrim;
  text = StringTrim @ StringTrimLeft[text, "# " ~~ " "... ~~ title <> "\n"];

  frontMatter = Assoc[];
  akas = getTextAkas @ text;
  If[StringVectorQ[akas] && akas =!= {},
    frontMatter["aliases"] = DeleteCases[akas, title];
    text = StringTrim @ StringDelete[text, "#meta/aka " ~~ Shortest[__] ~~ EndOfLine];
  ];

  tags = {}; noteFolder = None; mainTag = None;
  If[StringStartsQ[text, "#" ~~ LetterCharacter],

    {tagLine, text} = StringSplit[text, EndOfLine, 2];
    text // StringTrim;
    VPrint["Found tag line: ", tagLine];

    extraMetaTag = Nothing;
    If[StringStartsQ[tagLine, "#meta/master for "],
      tagLine //= StringTrimLeft["#meta/master for "];
      extraMetaTag = "#meta/master";
    ];
    mainTag = First @ StringSplit[tagLine, " ", 2];
    VPrint["Main tag: ", mainTag];
    fieldTags = StringTrim @ StringCases[tagLine, field:(" #field/" ~~ (LetterCharacter | "/")..)];

    tags = Flatten @ {mainTag, extraMetaTag, fieldTags};
    tagPath = StringSplit[StringTrimLeft[mainTag, "#"], "/"];
    noteFolder = First[tagPath, None];

    Label[retry];
    proc = Lookup[$tagProcessors, tagStr = StringRiffle[tagPath, "/"], None];
    If[proc === None && Length[tagPath] > 1,
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
    VPrint["No tagline present: ", StringTake[text, UpTo[20]]];
  ];
  text = StringReplace[text, "![](" ~~ imageName:ShortBlank ~~ ")" :> copyImageToObsidian[imageName]];

  metaTags = StringCases[text, "#meta/" ~~ Except[WhitespaceCharacter]..];
  If[metaTags =!= {},
    JoinTo[tags, metaTags];
    text //= StringDelete[StartOfLine ~~ "#meta/" ~~ LineFragment ~~ "\n"];
  ];
  text //= StringReplace[Repeated["\n", {3, Infinity}] -> "\n\n"] /* StringTrim;

  isMaster = ContainsQ[tags, "#meta/master"];
  mainTag //= StringTrimLeft["#"];
  If[isMaster, mainTag = StringRiffle[Most @ StringSplit[mainTag, "/"], "/"]];

  PrependTo[frontMatter, "type" -> mainTag];
  frontMatter["tags"] = DeleteDuplicates @ Join[tags, Lookup[frontMatter, "tags", {}]];
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
  If[StringFreeQ[text, StartOfLine ~~ "#meta/aka "], Return[{}]];
  akaStr = FirstStringCase[text, "#meta/aka " ~~ akas:Shortest[__] ~~ EndOfLine :> akas];
  akaStr = StringDelete[akaStr, ParentheticalPhrase];
  If[akaStr === None, Return[{}]];
  akaStr = StringReplace[akaStr, inner:("\"" ~~ ShortBlank ~~ "\"") /; StringContainsQ[inner, ","] :> StringReplace[inner, "," -> "COMMA"]];
  akas = StringTrim @ StringSplit[akaStr, ","];
  akas = StringTrimLeft[akas, "or "];
  akas = StringTrimRight[akas, " in " ~~ ___];
  akas = StringTrimLeftRight[akas, "\"", "\""];
  akas = StringTrimLeftRight[akas, "[[", "]]"];
  akas = StringReplace[akas, "COMMA" -> ","];
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
  Flatten @ StringCases[contents, pattern ~~ lf:LineFragment :>
    StringCases[lf, link:MarkdownHyperlinkPattern /; linkHasTypeQ[link, type]],
    IgnoreCase -> True
  ]
];

linkHasTypeQ[link_Str, type_] := Scope[
  text = Quiet @ BearNoteText @ StringTake[link, {3, -3}];
  StringQ[text] && StringContainsQ[text, type]
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

$pdfPath = Automatic;

General::badBearDocTitle = "Bad title `` for a document.";
docMetadata[title_Str, tagLine_Str, contents_Str] := Scope[

  {by, venue, in, about} = stringSectionSplit[tagLine, {" by ", " in " ~~ PositiveLookahead["#journal"], " in ", " about " | " defining " | " connecting "}];
  authors = StringCases[by, {link:MarkdownNoteLinkPattern :> link, name:FullNamePhrase :> simplyFullName @ name}];
  tags = StringCases[in, MarkdownTagPattern];
  journal = FirstStringCase[venue, MarkdownTagPattern];

  {min, max} = MinMax @ StringPosition[title, "\""];
  If[max <= min, ThrowMessage["badBearDocTitle", MsgExpr @ title]];
  trimmedTitle = StringTake[title, {min + 1, max - 1}];
  path = toPaperPDFPath[$pdfPath, title];
  url = FirstStringCase[contents, HyperlinkPattern];
  newContents = StringDelete[contents, url];

  If[!FileExistsQ[path],
    glob = "*" <> ToLowerCase[trimmedTitle] <> "*.pdf";
    candidates = FileNames[glob, ReplaceAutomatic[$pdfPath, $PDFPath], IgnoreCase -> True];
    If[Length[candidates] === 1,
      candidate = First @ candidates;
      VPrint["Found candidate ", MsgPath @ candidate, " for missing path ", MsgPath @ path];
      path = candidate;
    ,
      If[StringContainsQ[contents, "#meta/downloaded"], ThrowMessage["missingLocalPath", MsgExpr @ title]];
    ];
  ];
  localPath = If[FileExistsQ[path], toLocalPath @ path, None];

  abstract = FirstStringCase[contents,
    StartOfLine ~~ "> " ~~ abstract:LineFragment :> abstract
  ];

  tagsStr = StringRiffle[tags, " "];

  conceptExtractor = createLocalConceptExtractor[tagsStr];
  abstractStr = ReplaceNone[abstract, ""];
  concepts = DeleteDuplicates @ Join[
    StringCases[about, MarkdownNoteLinkPattern ? conceptLinkQ],
    StringCases[abstractStr, MarkdownNoteLinkPattern ? conceptLinkQ],
    StringCases[abstractStr, conceptExtractor]
  ];

  date = None;
  If[StringContainsQ[url, "arxiv"],
    arxivData = ImportArxivPage[url];
    If[AssociationQ[arxivData],
      date = arxivData["Date"];
      If[StringQ[date],
        date = FromDateString[date, {"Year", "/", "Month", "/", "Day"}];
      ];
    ];
  ,
    If[StringQ @ localPath, date = guessPDFdate @ path];
  ];

  abstract //= StringReplace[link:MarkdownNoteLinkPattern :> StringTake[link, {3, -3}]];

  conceptPattern = DeepFirstCase[conceptExtractor, _Alternatives];
  $isLinked = UAssoc[]; linkedQ = Function[Lookup[$isLinked, #, $isLinked[#] = True; False]];
  enrichmentRule = {
    link:MarkdownNoteLinkPattern :> link,
    WordBoundary ~~ lhs:conceptPattern ~~ plural:Maybe["s" | "es"] ~~ WordBoundary :>
      If[linkedQ[lhs], lhs <> plural, "[[" <> lhs <> "]]" <> plural]
  };
  newContents = StringReplace[
    newContents,
    StartOfLine ~~ abstractLHS:("> " ~~ LineFragment) :> StringReplace[abstractLHS, enrichmentRule]
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

  StringReplace[text, StartOfLine ~~ "> " ~~ abstract:LineFragment :> StringReplace[abstract, rule], 1]

];

simplyFullName[str_String] := StringJoin["[[",
  StringReplace[str, (" " ~~ LetterCharacter ~~ Maybe["."] ~~ " ") -> " "],
  "]]"
];

(*************************************************************************************************)

stringSectionSplit[s_, rules_] := Scope[
  sections = StringSplit[s, RuleRange @ rules];
  Map[
    Replace[sections, {{___, #, sec_Str, ___} :> sec, _ -> ""}]&,
    Range @ Length @ rules
  ]
];

toLocalPath[path_] := "file://" <> StringReplace[path, " " -> "%20"];

(*************************************************************************************************)

CacheVariable[$PDFDateCache]

guessPDFdate[path_] := Scope[
  CachedInto[$PDFDateCache, path,
    firstPage = Quiet @ Import[path, {"PagePlaintext", 1}];
    dateStr = FirstStringCase[firstPage, SpelledDatePattern];
    If[!StringQ[dateStr], dateStr = FirstStringCase[firstPage, NumericDatePattern]];
    date = If[StringQ[dateStr], FromDateString @ dateStr, None];
    date
  ]
];

(*************************************************************************************************)

computeBearAkaRules[type_] := computeBearAkaRules[type] = Scope[
  data = BearNoteData[{"Tag" -> type}, {"Title", "Text"}];
  data = Select[data, StringFreeQ[#Title, {":", "\""}] && StringCount[#Title, " "] <= 4&];
  akaGraph = Graph @ Flatten @ Map[Thread[Append[getTextAkas[#Text], #Title] -> #Title]&, data];
  titleToText = Assoc[#Title -> #Text& /@ data];
  akaComps = WeaklyConnectedComponents[akaGraph];
  primaries = componentToPrimary /@ akaComps;
  Flatten @ MapThread[Thread[#1 -> #2]&, {akaComps, primaries}]
];

componentToPrimary[{a_}] := a;
componentToPrimary[list_] := Last @ SortBy[list, StringLength @ Lookup[titleToText, #, ""]&];

(*************************************************************************************************)

$bearConceptTitles := $bearConceptTitles =
  ConstantUAssociation[Flatten[List @@@ computeBearAkaRules["concept"]], True];

conceptLinkQ[link_] := Lookup[StringDrop[link, {3, -3}], $bearConceptTitles, False];

(*************************************************************************************************)

$linkBlacklist := $linkBlacklist = StringSplit[ImportUTF8 @ DataPath["Text", "LinkBlacklist.txt"], "\n"];

(* computed with:
$concepts = BearNoteData[{"Tag" -> "concept"}, {"Title", "Text"}];
$concepts = Select[$concepts, StringFreeQ[#Title, {":", "\""}]&];
$allConceptPhrases = DeleteNone @ Flatten[{#Title, QuiverGeometry`External`Bear`getTextAkas[#Text]}& /@ $concepts];
$singleWordConceptPhrases = Select[$allConceptPhrases, StringFreeQ[#, " "] && ASCIIQ[#]&];
$frequencies = WordFrequencyData[$singleWordConceptPhrases];
$frequencies2 = Log @ Select[$frequencies,NumberQ];
CopyToClipboard @ StringRiffle[Keys@Sort@Select[$frequencies2, GreaterThan[-14]], "\n"]
*)

createLocalConceptExtractor[noteTagsStr_Str] := Scope[

  baseRules = computeBearAkaRules["concept"];

  blacklist = $linkBlacklist;
  whitelist = Catenate @ KeySelect[$linkWhitelists, StringContainsQ[noteTagsStr, #]&];
  blacklist = Complement[blacklist, whitelist];
  blacklistPattern = Alternatives @@ blacklist;
  filteredRules = Select[baseRules, FreeQ[First[#], blacklistPattern]&];

  titles = Union @ Flatten @ (List @@@ filteredRules);
  titles //= Select[StringLength[#] > 2&];
  titles //= ReverseSortBy[StringLength];

  (WordBoundary ~~ lhs:(Alternatives @@ titles) ~~ Maybe["s"] ~~ WordBoundary) :> "[[" <> lhs <> "]]"
];

$linkWhitelists := $linkWhitelists = createLinkWhitelists[];

createLinkWhitelists[] := Scope[
  text = ImportUTF8 @ DataPath["Text", "LinkWhitelists.txt"];
  table = StringSplit[StringSplit[text, "\n"], " "];
  Assoc @ MapApply[toWLentry, table]
];

toWLentry[key_, vals__] := If[StringContainsQ[key, ","],
  # -> {vals}& /@ StringSplit[key, ","],
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
