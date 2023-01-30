PublicFunction[URLToMarkdown]

URLToMarkdown::badurl = "Not url:``."
URLToMarkdown::baddomain = "Unrecognized domain ``."

Options[URLToMarkdown] = {
  Verbose -> False,
  DownloadPDF -> True
}

URLToMarkdown[url_String, opts:OptionsPattern[]] := Scope[
  domain = ToLowerCase @ URLParse[url, "Domain"];
  If[!StringQ[domain], ReturnFailed["noturl", url]];
  Switch[domain,
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

CreateNoteFromURL[url_String, opts:OptionsPattern[]] := Scope[
  UnpackOptions[duplicateTarget];
  res = URLToMarkdown[url, FilterOptions @ opts];
  If[!StringQ[res], ReturnFailed[]];
  CreateBearNote[res, DuplicateTarget -> duplicateTarget]
];

(**************************************************************************************************)

PublicFunction[CreateNotesFromURLList]

Options[CreateNotesFromURLList] = Options[CreateNoteFromURL];

CreateNotesFromURLList[urls:{__String}, opts:OptionsPattern[]] := Scope[
  results = Map[
    Quiet[CreateNoteFromURL[#, opts], {CreateBearNote::exists}]&,
    urls
  ];
  If[!StringVectorQ[results], ReturnFailed[]];
  StringRiffle[results, "\n"]
]
