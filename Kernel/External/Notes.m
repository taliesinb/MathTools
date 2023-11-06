PublicFunction[URLToMarkdown]

URLToMarkdown::badurl = "Not url:``."
URLToMarkdown::baddomain = "Unrecognized domain ``."

Options[URLToMarkdown] = {
  Verbose -> False,
  DownloadPDF -> True
}

URLToMarkdown[url_Str, opts:OptionsPattern[]] := Scope[
  domain = ToLowerCase @ URLParse[url, "Domain"];
  If[!StringQ[domain], ReturnFailed["noturl", url]];
  Switch[
    StringTrimLeft[domain, "www."],
    "scholar.google.com",       ScholarPageToMarkdown[url, opts],
    "arxiv.org",                ArxivPageToMarkdown[url, opts],
    "youtube.com" | "youtu.be", YoutubeToMarkdown[url, FilterOptions @ opts],
    _,                          ReturnFailed["baddomain", domain]
  ]
]

_URLToMarkdown := BadArguments[];

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





