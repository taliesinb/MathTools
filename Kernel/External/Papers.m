PrivateFunction[paperKeyOrder]

paperKeyOrder = Case[

  "Title"             := {1, 0};
  "Date"              := {2, 0};
  "Authors"           := {3, 0};
  "URL"               := {4, 0};
  "PDFURL"            := {5, 0};
  "PrimarySubjects"   := {8, 0};    (* Arxiv *)
  "SecondarySubjects" := {9, 0};    (* Arxiv *)
  "Subjects"          := {10, 0};   (* Arxiv, OpenReview *)
  "Origin"            := {11, 0};
  "Event"             := {12, 0};
  "Abstract"          := {100, 0};
  "CustomMetadata"    := {200, 0};  (* OpenReview *)

  other_              := {1000, other};
];

(**************************************************************************************************)

PrivateFunction[postProcessPaperPageData]

postProcessPaperPageData[data_, downloadPDF_, pdfPath_] := Scope[

  If[downloadPDF, data["PDFFilePath"] = DownloadPaper[data, PDFPath -> pdfPath, Verbose -> $verbose]];

  KSortBy[data, paperKeyOrder]
];

(**************************************************************************************************)

PublicFunction[PaperToMarkdown]

PublicOption[AdditionalText]

Options[PaperToMarkdown] = {
  PDFPath :> $PDFPath,
  DownloadPDF -> True,
  Verbose -> False,
  AdditionalText -> None
}

PaperToMarkdown[$Failed, ___] := $Failed;

PaperToMarkdown::addtextabs = "AdditionalText seems to contain abstract for article \"``\".";
PaperToMarkdown[data_Assoc, OptionsPattern[]] := Scope[
  UnpackOptions[pDFPath, downloadPDF, $verbose, additionalText];
  UnpackAssociation[data, authors, title, origin, fieldTags, abstract, url:"URL"];
  title //= sanitizeTitle;
  authors //= sanitizeAuthors;
  title = PaperPageTitle[authors, title];
  If[Len[authors] > 8,
    authors = SortBy[authors, {!MemberQ[$KnownAuthors, If[ListQ[#], SRiffle[#, " "], #]], Position[authors, #]}&];
    authorLinks = SRiffle[App["et al"] @ Take[Map[toAuthorLink2, authors], UpTo @ 12], ", "]
  ,
    authorLinks = SRiffle[toAuthorLink /@ authors, ", "]
  ];
  abstract //= sanitizeAbstract;

  fieldString = If[fieldTags === None, "", {" in ", SRiffle[fieldTags, ", "]}];

  localPDFPath = Lookup[data, "PDFFilePath", Auto];
  SetAuto[localPDFPath, toPaperPDFPath[pDFPath, title]];
  If[!FileExistsQ[localPDFPath] && downloadPDF && MatchQ[origin, "Arxiv" | "OpenReview"],
    DownloadPaper[data, PDFPath -> pDFPath, Verbose -> $verbose]
  ];
  downloadTag = If[FileExistsQ[localPDFPath], "#meta/downloaded", "#todo/download"];

  If[StrQ[additionalText] && STrim[additionalText] =!= "",
    additionalText //= STrim /* SDelete[("> " <> abstract) | abstract] /* STrim;
    If[EditDistance[additionalText, abstract] < SLen[abstract] * 0.05,
      Message[PaperToMarkdown::addtexttabs, title]];
  ];
  SJoin[
    "# ", title, "\n",
    "\n",
    "#doc/paper", fieldString, " by ", authorLinks, "\n",
    "\n",
    downloadTag, "\n",
    url,
    "\n\n",
    "> ", abstract,
    If[StrQ[additionalText] && additionalText =!= "", {"\n\n", additionalText}, ""]
  ]
];

(**************************************************************************************************)

toAuthorLink = Case[
  str_Str /; SContainsQ[str, "."] := str;
  str_Str         := SJoin["[[", DeleteMiddleInitials @ str, "]]"];
  {first_, last_} := SJoin["[[", first, " ", last, "]]"];
  {last_}         := last;
];

toAuthorLink2 = Case[
  str_Str         := If[MemberQ[$KnownAuthors, str], SJoin["[[", str, "]]"], str];
  {first_, last_} := toAuthorLink2 @ SJoin[first, " ", last];
  {last_}         := last;
]

(**************************************************************************************************)

PrivateFunction[sanitizeAuthors]

sanitizeAuthors[authors_] := RepAll[authors, s_String :> SRep[s, $authorFixups]];

$authorFixups = {
  "Antoran" -> "Antorán",
  "Urquhart Allingham" -> "Allingham",
  "Von Glehn" -> "von Glehn"
};

(**************************************************************************************************)

PrivateFunction[sanitizeAbstract, sanitizeTitle]

sanitizeAbstract[str_Str] := STrim @ SRep[str, $AbstractNormalizationRules];
sanitizeTitle[str_Str] := STrim @ SRep[str, $TitleNormalizationRules]

(**************************************************************************************************)

PublicFunction[PaperPageTitle]
PublicVariable[$KnownAuthors]

SetCached[$KnownAuthors, BearPeople[]];

PaperPageTitle[authors_, title_] := Scope[
  authors //= VectorReplace[str_Str :> SplitFirstLastName[str]];
  If[Len[authors] > 4,
    knownAuthorAssoc = ConstantAssociation[VectorReplace[$KnownAuthors, str_Str :> SplitFirstLastName[str]], 0];
    authors2 = SortBy[authors, Lookup[knownAuthorAssoc, Key @ #, 1]&];
    authors2 = Take[authors2, UpTo[4]];
    lastAuthor = L @ authors;
    If[MemberQ[authors2, lastAuthor], authors2 = App[lastAuthor] @ Decases[authors2, lastAuthor]];
    authorSurnames = authors2[[All, -1]];
    If[Len[authors] > Len[authors2], AppTo[authorSurnames, "et al"]];
    VPrint["Chosen authors: ", authorSurnames];
  ,
    authorSurnames = authors[[All, -1]];
  ];
  authorPrefix = SRiffle[authorSurnames, ", "];
  title //= SRep[{"$" -> "", "\\'e" -> "é", "\\\"o" -> "ö"}];
  heading = SJoin[authorPrefix, ": \"", title, "\""];
  heading
]

(**************************************************************************************************)

PublicIOFunction[DownloadPaper]

PublicOption[PDFPath, AllowRename]

Options[DownloadPaper] = {
  PDFPath -> Auto,
  Verbose -> Auto,
  DryRun -> False,
  AllowRename -> True,
  OverwriteTarget -> False
};

DownloadPaper::norename = "Existing candidate `` found, but renames are forbidden."
DownloadPaper::badrename = "Could not rename `` to ``."
DownloadPaper::nopdfurl = "No PDF URL could be found for paper with title \"``\"."

$minPaperSize = 5000;

DownloadPaper[assoc_Assoc, OptionsPattern[]] := Scope[
  UnpackAssociation[assoc, authors, title, pdfUrl:"PDFURL", url:"URL"];
  UnpackOptions[$verbose, $dryRun, pDFPath, allowRename, overwriteTarget];
  SetAuto[$verbose, $dryRun];

  title = PaperPageTitle[authors, title];
  VPrint["Paper prefixed title: ", title];

  localPdfPath = toPaperPDFPath[pDFPath, title];
  pdfFileName = FileNameTake @ localPdfPath;
  pdfDir = FileNameDrop @ localPdfPath;

  If[FileExistsQ[localPdfPath],
    If[!overwriteTarget,
      If[FileByteCount[localPdfPath] < $minPaperSize,
        VPrint["Paper with file name ", MsgPath @ localPdfPath, " already exists, but is too small. Downloading again."]
      ,
        VPrint["Paper with file name ", MsgPath @ localPdfPath, " already exists, skipping download."];
        Return[localPdfPath]
      ];
    ,
      VPrint["Paper with file name ", MsgPath @ localPdfPath, " already exists, overwriting."];
    ];
    Goto[SkipRename];
  ];

  partialTitle = Part[SSplit[pdfFileName, "\""], 2];
  If[SLen[partialTitle] > 8,
    titleGlob = SJoin["*", SRep[partialTitle, "'"|":" -> "*"], "*.pdf"];
    targets = FileNames[titleGlob, pdfDir, IgnoreCase -> True];
    If[Len[targets] === 1,
      target = F @ targets;
      VPrint["Found existing candidate ", MsgPath @ target, ", renaming to ", MsgPath @ localPdfPath, "."];
      If[!TrueQ[allowRename], VPrint["Renames forbidden, skipping"];
        ReturnFailed["norename", MsgPath @ target]];
      whenWet @ MoveFile[target, localPdfPath];
      Return @ localPdfPath
    ];
  ];
  Label[SkipRename];

  If[!StrQ[pdfUrl] && StrQ[url],
    VPrint["Searching for mirror URL for ", MsgPath @ url];
    pdfUrl = FindPDFMirrorURL[url];
    If[StrQ[pdfUrl], VPrint["Found mirror at ", MsgPath @ pdfUrl]];
  ];

  If[!StrQ[pdfUrl], ReturnFailed["nopdfurl", title]];

  res = SafeURLDownload[pdfUrl, localPdfPath, OverwriteTarget -> True];
  If[!StrQ[res] || !FileExistsQ[res], ReturnFailed[]];

  If[FileByteCount[res] < $minPaperSize,
    VPrint["Downloaded paper ", MsgPath @ res, " is too small, deleting."];
    TrashFile[res];
    ReturnFailed[];
  ];

  res
]

(**************************************************************************************************)

PrivateFunction[toPaperPDFPath]

toPaperPDFPath[Auto, authorsAndTitle_] := toPaperPDFPath[$PDFPath, authorsAndTitle];

toPaperPDFPath[pdfPath_, authorsAndTitle_] := Scope[
  fileName = SRep[SRep[authorsAndTitle, ":" -> " - "], Repeated[" ", {2, Inf}] -> " "];
  PathJoin[pdfPath, fileName <> ".pdf"]
];

(**************************************************************************************************)

PublicIOFunction[DownloadPDFMirror, FindPDFMirrorURL]

PublicVariable[$SciHubServers]

SetInitialValue[$SciHubServers, {
  "https://sci.hubbza.co.za/",
  "https://www.libgen.tw/",
  "https://sci.hubg.org/",
  "https://sci-hub.st/",
  "https://sci-hub.wf/",
  "https://sci-hub.it.nf/",
  "https://sci-hub.do/",
  "https://sci.hubg.org/",
  "https://sci-hub.ee/"
}];

General::nomirror = "Could not query any SciHub servers from `` for mirror of ``."
General::badmirror = "Could not obtain mirroring URL for `` via ``."

DownloadPDFMirror[url_Str, localPdfPath_Str] := Scope[
  pdfUrl = FindPDFMirrorURL[url];
  If[!StrQ[pdfUrl], ReturnFailed[]];
  SafeURLDownload[pdfUrl, localPdfPath]
];

$containsRawPDFUrlP = "https://journals.plos.org" | "https://direct.mit.edu/";
$rawPDFP = ("\"https://" ~~ Except["\""].. ~~ ".pdf\"") | (PositiveLookbehind["href="] ~~ "\"" ~~ Except["\""].. ~~ "type=printable\"")

FindPDFMirrorURL[url_Str] := Scope[
  domain = URLParse[url, "AbsoluteDomain"];
  If[SMatchQ[url, "https://biorxiv.org/" ~~ ___ ~~ ".pdf"], Return @ url];
  If[SMatchQ[url, $containsRawPDFUrlP ~~ ___],
    res = CachedURLFetch[url];
    If[StrQ[res],
      pdfUrl = SCases[res, z:$rawPDFP :> STrim[z, "\""]];
      pdfUrl = F[pdfUrl, None];
      If[StrQ[pdfUrl],
        If[SStartsQ[pdfUrl, "/"], pdfUrl = SJoin[domain, StringTrimLeft[pdfUrl, "/"]]];
        Goto[Done]
      ];
    ];
  ];
  If[SMatchQ[url, "https://www.tandfonline.com/doi/full/" ~~ ___],
    pdfUrl = SRep[F @ SSplit[url, "?", 2], "/full/" -> "/pdf/"] <> "?download=true";
    Goto[Done];
  ];
  Do[
    mirrorUrl = mirror <> url;
    VPrint["Trying ", MsgPath @ mirrorUrl];
    res = CachedURLFetch[mirrorUrl];
    If[StrQ[res],
      pdfUrl = SCases[res, "embed type=\"application/pdf\" src=\"" ~~ u:Except["\""].. ~~ "\"" :> u];
      pdfUrl = F[pdfUrl, None];
      If[StrQ[pdfUrl],
        If[SStartsQ[pdfUrl, "/"],
          pdfUrl = SJoin[mirror, STrim[pdfUrl, "/"]]];
        Goto[Done];
      ];
    ,
      VPrint["No result from ", MsgPath @ mirrorUrl];
    ];
  ,
    {mirror, $SciHubServers}
  ];
  ReturnFailed["nomirror", MsgPath /@ $SciHubServers, MsgPath @ url];
  Label[Done];
  VPrint["Found PDF at ", MsgPath @ pdfUrl];
  pdfUrl
];