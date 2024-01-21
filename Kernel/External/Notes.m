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
    _,                          ReturnFailed["baddomain", domain]
  ]
]

_URLToMarkdown := BadArguments[];

(**************************************************************************************************)

PublicVariable[$KnownNoteURLPatterns]

$KnownNoteURLPatterns = "scholar.google.com" | "arxiv.org" | "youtube.com" | "youtu.be";

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

$validNotePagesP = "scholar.google.com" | "arxiv.org" | "youtube.com" | "youtu.be";

CreateNotesFromClipboardList[] := (
  clipboard = PasteFromClipboard[];
  urls = StringCases[clipboard, HyperlinkPattern];
  If[!StringVectorQ[urls], ReturnFailed[]];
  urls = DeleteDuplicates @ Select[urls, StringContainsQ[$validNotePagesP]];
  If[urls === {}, Return @ ""];
  CreateNotesFromURLList[urls]
);