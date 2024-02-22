PublicIOFunction[ImportURLToMarkdown]

ImportURLToMarkdown::badurl = "Not url:``."
ImportURLToMarkdown::baddomain = "Unrecognized domain ``."

Options[ImportURLToMarkdown] = {
  Verbose -> False,
  DownloadPDF -> True
}

ImportURLToMarkdown[url_Str, opts:OptionsPattern[]] := Scope[
  domain = ToLowerCase @ URLParse[url, "Domain"];
  If[!StrQ[domain], ReturnFailed["badurl", MsgExpr @ url]];
  Switch[
    StringTrimLeft[domain, "www."],
    "scholar.google.com",       ImportScholarPageToMarkdown[url, opts],
    "arxiv.org",                ImportArxivPageToMarkdown[url, opts],
    "youtube.com" | "youtu.be", ImportYoutubeVideoToMarkdown[url, FilterOptions @ opts],
    "openreview.net",           ImportOpenReviewPageToMarkdown[url],
    _,                          ReturnFailed["baddomain", domain]
  ]
]

_ImportURLToMarkdown := BadArguments[];

(**************************************************************************************************)

PublicVariable[$KnownNoteURLPatterns]

$KnownNoteURLPatterns = "scholar.google.com" | "arxiv.org" | "youtube.com" | "youtu.be" | "openreview.net";

(**************************************************************************************************)

PublicIOFunction[CreateBearArxivPages]

Options[CreateBearArxivPages] = {
  Verbose -> False
};

CreateBearArxivPages::badmd = "Could not create markdown for data ``.";
CreateBearArxivPages[assocs:{__Assoc}, opts:OptionsPattern[]] := Scope[
  assocs = ReverseSortBy[assocs, Key["Date"]];
  list = Map[assoc |-> (
    res = PaperToMarkdown[assoc, opts];
    If[!StrQ[res], ReturnFailed["badmd", MsgExpr @ assoc]];
    CreateBearNote[res]
  ),
    assocs
  ];
  SRiffle[list, "\n"]
];

(**************************************************************************************************)

PublicIOFunction[PopulateOrphanBearLinkNotes]

SetUsage @ "
PopulateOrphanBearLinkNotes[] finds all pages that correspond to bare links to known sites and populates the corresponding notes.
* Youtube links will populate the page with video metadata.
* Arxiv links will populate with authors, abstract, and download the corresponding paper.
"

Options[PopulateOrphanBearLinkNotes] = {
  DryRun -> False
};

PopulateOrphanBearLinkNotes[OptionsPattern[]] := Scope[
  UnpackOptions[$dryRun];
  {uuids, titles, extraText} = gatherLinkNoteData[$KnownNoteURLPatterns];
  Print["Found ", Len @ uuids, " links."];
  MapThread[$i = 1; processOrphanLinkNote, {uuids, titles, extraText}]
];

processOrphanLinkNote[uuid_, title_, extraText_] := Scope[
  Print[$i++, ": Processing note with title \"", title, "\""];
  link = FirstStringCase[title, HyperlinkPattern];
  If[!StrQ[link], Print["Could not find link in title \"", title, "\""]; ReturnFailed[]];
  markdown = ImportURLToMarkdown[link, AdditionalText -> extraText];
  If[!StrQ[markdown], Print["Failed to create markdown."]; ReturnFailed[]];
  If[$dryRun, Return @ markdown];
  res = ReplaceBearNote[uuid, markdown];
  If[FailureQ[res], Print["Failed to replace note."]; ReturnFailed[]];
  noteTitle = StringTrimLeft[SExtract[markdown, "\n" -> 1], "# "];
  noteTitle
];

gatherLinkNoteData[pattern_] := Scope[
  noteData = Rev @ BearNoteData["Title" -> SContainsQ[pattern], {"UUID", "Title", "Text", "CreationDate"}];
  noteUUIDs = Part[noteData, All, "UUID"];
  noteTitles = Part[noteData, All, "Title"];
  extraText = Map[
    If[SContainsQ[#Text, "\n"], SDrop[#Text, F @ F @ SFind[#Text, "\n"]], ""]&,
    noteData
  ];
  {noteUUIDs, noteTitles, extraText}
];

(**************************************************************************************************)

PublicIOFunction[CreateNoteFromURL]

Options[CreateNoteFromURL] = JoinOptions[ImportURLToMarkdown, DuplicateTarget -> False];

CreateNoteFromURL[url_Str, opts:OptionsPattern[]] := Scope[
  UnpackOptions[duplicateTarget];
  res = ImportURLToMarkdown[url, FilterOptions @ opts];
  If[!StrQ[res], ReturnFailed[]];
  CreateBearNote[res, DuplicateTarget -> duplicateTarget]
];

(**************************************************************************************************)

PublicIOFunction[CreateNotesFromURLList]

Options[CreateNotesFromURLList] = Options[CreateNoteFromURL];

CreateNotesFromURLList[urls:{__Str}, opts:OptionsPattern[]] := Scope[
  results = Map[
    Quiet[CreateNoteFromURL[#, opts], {CreateBearNote::exists}]&,
    urls
  ];
  If[!StrVecQ[results], ReturnFailed[]];
  SRiffle[results, "\n"]
]

PublicIOFunction[CreateNotesFromClipboardList]

CreateNotesFromClipboardList::skippedURLs = "Skipping unknown URLs: ``.";
CreateNotesFromClipboardList[] := Scope[
  clipboard = PasteFromClipboard[];
  urls = SCases[clipboard, HyperlinkPattern];
  If[!StrVecQ[urls], ReturnFailed[]];
  {urls, unknownURLs} = SelectDiscard[urls, SContainsQ[$KnownNoteURLPatterns]];
  If[unknownURLs =!= {}, Message[CreateNotesFromClipboardList::skippedURLs, MsgExpr @ unknownURLs]];
  urls = Dedup @ urls;
  If[urls === {}, Return @ ""];
  CreateNotesFromURLList[urls]
];

(**************************************************************************************************)

PrivateFunction[IntStr2]

IntStr2[n_] := IntStr[n, 10, 2];

(**************************************************************************************************)

PublicFunction[ToNoteDateString]

ToNoteDateString = Case[
  s_Str /; SStartsQ[s, "#"]                                              := % @ StringTrimLeft[s, "#"];
  s_Str /; SMatchQ[s, DigitCharacter.. ~~ ("/" ~~ DigitCharacter...)...] := SJoin["Y", intListToDateString @ SSplit[s, "/"]];
  DateObject[{y_}, "Year", ___]                          := SJoin["Y", IntStr @ y];
  DateObject[{y_, m_}, "Month", ___]                     := SJoin["Y", IntStr @ y, "M", IntStr2 @ m];
  DateObject[{y_, m_, d_}, "Day", ___]                   := SJoin["Y", IntStr @ y, "M", IntStr2 @ m, "D", IntStr2 @ d];
  DateObject[{y_, m_, d_, h_, min_, s_}, "Instant", ___] := SJoin["Y", IntStr @ y, "M", IntStr2 @ m, "D", IntStr2 @ d, "H", IntStr2 @ h, "M", IntStr2 @ min, "S", IntStr2 @ Floor @ s];
];

numberListToDateString[tag_] := SJoin["Y", tagListToDate @  SSplit[StringTrimLeft[tag, "#"], "/"]];

$cenP = ("18" | "19" | "20");
fullYearStrQ[s_] := SMatchQ[s, $cenP ~~ DigitCharacter ~~ DigitCharacter];
monthStrQ[s_] := SMatchQ[s, DigitCharacter | ("0" ~~ DigitCharacter) | "10" | "11" | "12"];
dayStrQ[s_] := SMatchQ[s, DigitCharacter | ("0"|"1"|"2" ~~ DigitCharacter) | "30" | "31"];
pad2[s_] := SPadLeft[s, 2, "0"];

intListToDateString = Case[
  {y_ ? fullYearStrQ}                               := y;
  {y_ ? fullYearStrQ, m_ ? monthStrQ}               := {y, "M", pad2 @ m};
  {y_ ? fullYearStrQ, m_ ? monthStrQ, d_ ? dayStrQ} := {y, "M", pad2 @ m, "D", pad2 @ d};
];

(**************************************************************************************************)

PublicFunction[ParseNoteAliases]

$allowedParenPattern = "(" ~~ (DigitCharacter | " " | ",").. ~~ ")";

encodeHunk[s_] :=  "@" <> URLEncode[s] <> "@";
decodeHunks[s_] := SRep[s, "@" ~~ encoded:ShortBlank ~~ "@" :> URLDecode[encoded]];

ParseNoteAliases[str_Str] := Scope[
  escaped = SRep[str, dq:DoubleQuotedPhrase :> encodeHunk[STake[dq, {2, -2}]]];
  noComments = SRep[escaped, pp:ParentheticalPhrase :> If[SMatchQ[pp, $allowedParenPattern], encodeHunk @ pp, ""]];
  split = STrim @ SSplit[noComments, ","];
  split = SRep[split, "@" ~~ encoded:ShortBlank ~~ "@" :> URLDecode[encoded]];
  split
];

(**************************************************************************************************)

PublicFunction[NoteFullNameToShortName]

NoteFullNameToShortName[name_String] :=
  Scope[items = SSplit[name, " "]; SJoin[F @ items, STake[L @ items, 1]]];

NoteFullNameToShortName[name_List] :=
  SRiffle[NoteFullNameToShortName /@ name, ", "];

(**************************************************************************************************)

PublicFunction[CreateMeetingNote]

(* TODO: make this production-grade *)

(* toFinalChatTitle[id_, title_, text_, creationDate_] := Scope[
  subtitle = Part[StringSplit[StringTrim @ text, "\n".., 3], 2];
  people = TaggedTextCases[First @ StringSplit[title <> "\n" <> subtitle, " about ", 2], "#person"];
  shortPeople = NoteFullNameToShortName @ people;
  If[people === {}, Print["no people for ", ClickForm[title, OpenBearNote[title]]]];
  titleDate = FirstStringCase[title, NoteDatePattern];
  title2 = StringReplace[title, WordBoundary ~~ Alternatives["chat", "chat with", "with", "on", "meeting"] ~~ WordBoundary -> " ", IgnoreCase -> True];
  title2 = StringReplace[title2, NoteDatePattern -> " "];
  title2 = StringTrim @ StringReplace[title2, " ".. -> " "];
  subtitleDate = FirstStringCase[subtitle, NoteDatePattern];
  If[StringQ[titleDate] && StringQ[subtitleDate] && titleDate =!= subtitleDate, Print["Date error: ",title, " ", titleDate, " ", subtitleDate]];
  date = If[StringQ[titleDate], titleDate, subtitleDate];
  If[!StringQ[date],
    date = ToNoteDateString @ DateObject[FromBearTime[creationDate], "Day"]; Print["publ: ", date]];
  lowerAll = ToLowerCase[title <> " " <> subtitle];
  {isMeeting, isChat} = StringContainsQ[lowerAll, WordBoundary ~~ # ~~ WordBoundary]& /@ {"meeting", "chat"};
  If[isMeeting && isChat, Print["type error: ", title]; OpenBearNote[title]];
  type = If[isMeeting, "meeting with", "chat with"];
  StringJoin[date, " ", type, " ", shortPeople]
];  *)