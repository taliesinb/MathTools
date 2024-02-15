PublicFunction[URLToMarkdown]

URLToMarkdown::badurl = "Not url:``."
URLToMarkdown::baddomain = "Unrecognized domain ``."

Options[URLToMarkdown] = {
  Verbose -> False,
  DownloadPDF -> True
}

URLToMarkdown[url_Str, opts:OptionsPattern[]] := Scope[
  domain = ToLowerCase @ URLParse[url, "Domain"];
  If[!StringQ[domain], ReturnFailed["badurl", MsgExpr @ url]];
  Switch[
    StringTrimLeft[domain, "www."],
    "scholar.google.com",       ScholarPageToMarkdown[url, opts],
    "arxiv.org",                ArxivPageToMarkdown[url, opts],
    "youtube.com" | "youtu.be", YoutubeVideoToMarkdown[url, FilterOptions @ opts],
    "openreview.net",           OpenReviewPageToMarkdown[url],
    _,                          ReturnFailed["baddomain", domain]
  ]
]

_URLToMarkdown := BadArguments[];

(**************************************************************************************************)

PublicVariable[$KnownNoteURLPatterns]

$KnownNoteURLPatterns = "scholar.google.com" | "arxiv.org" | "youtube.com" | "youtu.be" | "openreview.net";

(**************************************************************************************************)

PublicFunction[CreateBearArxivPages]

Options[CreateBearArxivPages] = {
  Verbose -> False
};

CreateBearArxivPages::badmd = "Could not create markdown for data ``.";
CreateBearArxivPages[assocs:{__Assoc}, opts:OptionsPattern[]] := Scope[
  assocs = ReverseSortBy[assocs, Key["Date"]];
  list = Map[assoc |-> (
    res = PaperToMarkdown[assoc, opts];
    If[!StringQ[res], ReturnFailed["badmd", MsgExpr @ assoc]];
    CreateBearNote[res]
  ),
    assocs
  ];
  StringRiffle[list, "\n"]
];

(**************************************************************************************************)

PublicFunction[PopulateOrphanBearLinkNotes]

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
  Print["Found ", Length @ uuids, " links."];
  MapThread[$i = 1; processOrphanLinkNote, {uuids, titles, extraText}]
];

processOrphanLinkNote[uuid_, title_, extraText_] := Scope[
  Print[$i++, ": Processing note with title \"", title, "\""];
  link = FirstStringCase[title, HyperlinkPattern];
  If[!StringQ[link], Print["Could not find link in title \"", title, "\""]; ReturnFailed[]];
  markdown = URLToMarkdown[link, AdditionalText -> extraText];
  If[!StringQ[markdown], Print["Failed to create markdown."]; ReturnFailed[]];
  If[$dryRun, Return @ markdown];
  res = ReplaceBearNote[uuid, markdown];
  If[FailureQ[res], Print["Failed to replace note."]; ReturnFailed[]];
  noteTitle = StringTrimLeft[StringExtract[markdown, "\n" -> 1], "# "];
  noteTitle
];

gatherLinkNoteData[pattern_] := Scope[
  noteData = Reverse @ BearNoteData["Title" -> StringContainsQ[pattern], {"UUID", "Title", "Text", "CreationDate"}];
  noteUUIDs = Part[noteData, All, "UUID"];
  noteTitles = Part[noteData, All, "Title"];
  extraText = Map[
    If[StringContainsQ[#Text, "\n"], StringDrop[#Text, First @ First @ StringPosition[#Text, "\n"]], ""]&,
    noteData
  ];
  {noteUUIDs, noteTitles, extraText}
];

(**************************************************************************************************)

PublicFunction[CreateNoteFromURL]

Options[CreateNoteFromURL] = JoinOptions[URLToMarkdown, DuplicateTarget -> False];

CreateNoteFromURL[url_Str, opts:OptionsPattern[]] := Scope[
  UnpackOptions[duplicateTarget];
  res = URLToMarkdown[url, FilterOptions @ opts];
  If[!StringQ[res], ReturnFailed[]];
  CreateBearNote[res, DuplicateTarget -> duplicateTarget]
];

(**************************************************************************************************)

PublicFunction[CreateNotesFromURLList]

Options[CreateNotesFromURLList] = Options[CreateNoteFromURL];

CreateNotesFromURLList[urls:{__Str}, opts:OptionsPattern[]] := Scope[
  results = Map[
    Quiet[CreateNoteFromURL[#, opts], {CreateBearNote::exists}]&,
    urls
  ];
  If[!StringVectorQ[results], ReturnFailed[]];
  StringRiffle[results, "\n"]
]

PublicFunction[CreateNotesFromClipboardList]

CreateNotesFromClipboardList::skippedURLs = "Skipping unknown URLs: ``.";
CreateNotesFromClipboardList[] := Scope[
  clipboard = PasteFromClipboard[];
  urls = StringCases[clipboard, HyperlinkPattern];
  If[!StringVectorQ[urls], ReturnFailed[]];
  {urls, unknownURLs} = SelectDiscard[urls, StringContainsQ[$KnownNoteURLPatterns]];
  If[unknownURLs =!= {}, Message[CreateNotesFromClipboardList::skippedURLs, MsgExpr @ unknownURLs]];
  urls = DeleteDuplicates @ urls;
  If[urls === {}, Return @ ""];
  CreateNotesFromURLList[urls]
];

(**************************************************************************************************)

PrivateFunction[IntStr2]

IntStr2[n_] := IntegerString[n, 10, 2];

(**************************************************************************************************)

PublicFunction[ToNoteDateString]

ToNoteDateString = Case[
  s_Str /; StringStartsQ[s, "#"]                                              := % @ StringTrimLeft[s, "#"];
  s_Str /; StringMatchQ[s, DigitCharacter.. ~~ ("/" ~~ DigitCharacter...)...] := StrJoin["Y", intListToDateString @ StringSplit[s, "/"]];
  DateObject[{y_}, "Year", ___]                          := StrJoin["Y", IntStr @ y];
  DateObject[{y_, m_}, "Month", ___]                     := StrJoin["Y", IntStr @ y, "M", IntStr2 @ m];
  DateObject[{y_, m_, d_}, "Day", ___]                   := StrJoin["Y", IntStr @ y, "M", IntStr2 @ m, "D", IntStr2 @ d];
  DateObject[{y_, m_, d_, h_, min_, s_}, "Instant", ___] := StrJoin["Y", IntStr @ y, "M", IntStr2 @ m, "D", IntStr2 @ d, "H", IntStr2 @ h, "M", IntStr2 @ min, "S", IntStr2 @ Floor @ s];
];

numberListToDateString[tag_] := StringJoin["Y", tagListToDate @  StringSplit[StringTrimLeft[tag, "#"], "/"]];

$cenP = ("18" | "19" | "20");
fullYearStrQ[s_] := StringMatchQ[s, $cenP ~~ DigitCharacter ~~ DigitCharacter];
monthStrQ[s_] := StringMatchQ[s, DigitCharacter | ("0" ~~ DigitCharacter) | "10" | "11" | "12"];
dayStrQ[s_] := StringMatchQ[s, DigitCharacter | ("0"|"1"|"2" ~~ DigitCharacter) | "30" | "31"];
pad2[s_] := StringPadLeft[s, 2, "0"];

intListToDateString = Case[
  {y_ ? fullYearStrQ}                               := y;
  {y_ ? fullYearStrQ, m_ ? monthStrQ}               := {y, "M", pad2 @ m};
  {y_ ? fullYearStrQ, m_ ? monthStrQ, d_ ? dayStrQ} := {y, "M", pad2 @ m, "D", pad2 @ d};
];

(**************************************************************************************************)

PublicFunction[ParseNoteAliases]

$allowedParenPattern = "(" ~~ (DigitCharacter | " " | ",").. ~~ ")";

encodeHunk[s_] :=  "@" <> URLEncode[s] <> "@";
decodeHunks[s_] := StringReplace[s, "@" ~~ encoded:ShortBlank ~~ "@" :> URLDecode[encoded]];

ParseNoteAliases[str_Str] := Scope[
  escaped = StringReplace[str, dq:DoubleQuotedPhrase :> encodeHunk[StringTake[dq, {2, -2}]]];
  noComments = StringReplace[escaped, pp:ParentheticalPhrase :> If[StringMatchQ[pp, $allowedParenPattern], encodeHunk @ pp, ""]];
  split = StringTrim @ StringSplit[noComments, ","];
  split = StringReplace[split, "@" ~~ encoded:ShortBlank ~~ "@" :> URLDecode[encoded]];
  split
];

(**************************************************************************************************)

PublicFunction[NoteFullNameToShortName]

NoteFullNameToShortName[name_String] :=
  Scope[items = StringSplit[name, " "]; StringJoin[First @ items, StringTake[Last @ items, 1]]];

NoteFullNameToShortName[name_List] :=
  StringRiffle[NoteFullNameToShortName /@ name, ", "];

(**************************************************************************************************)

PublicFunction[CreateMeatingNote]

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