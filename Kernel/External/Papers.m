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

  KeySortBy[data, paperKeyOrder]
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
    authors = SortBy[authors, {!MemberQ[$KnownAuthors, If[ListQ[#], StringRiffle[#, " "], #]], Position[authors, #]}&];
    authorLinks = StringRiffle[Append["et al"] @ Take[Map[toAuthorLink2, authors], UpTo @ 12], ", "]
  ,
    authorLinks = StringRiffle[toAuthorLink /@ authors, ", "]
  ];
  abstract //= sanitizeAbstract;

  fieldString = If[fieldTags === None, "", {" in ", StringRiffle[fieldTags, ", "]}];

  localPDFPath = Lookup[data, "PDFFilePath", Automatic];
  SetAutomatic[localPDFPath, toPaperPDFPath[pDFPath, title]];
  If[!FileExistsQ[localPDFPath] && downloadPDF && MatchQ[origin, "Arxiv" | "OpenReview"],
    DownloadPaper[data, PDFPath -> pDFPath, Verbose -> $verbose]
  ];
  downloadTag = If[FileExistsQ[localPDFPath], "#meta/downloaded", "#todo/download"];

  If[StringQ[additionalText] && StringTrim[additionalText] =!= "",
    additionalText //= StringTrim /* StringDelete[("> " <> abstract) | abstract] /* StringTrim;
    If[EditDistance[additionalText, abstract] < StringLength[abstract] * 0.05,
      Message[PaperToMarkdown::addtexttabs, title]];
  ];
  StringJoin[
    "# ", title, "\n",
    "\n",
    "#doc/paper", fieldString, " by ", authorLinks, "\n",
    "\n",
    downloadTag, "\n",
    url,
    "\n\n",
    "> ", abstract,
    If[StringQ[additionalText] && additionalText =!= "", {"\n\n", additionalText}, ""]
  ]
];

(**************************************************************************************************)

toAuthorLink = Case[
  str_Str /; StringContainsQ[str, "."] := str;
  str_Str         := StringJoin["[[", DeleteMiddleInitials @ str, "]]"];
  {first_, last_} := StringJoin["[[", first, " ", last, "]]"];
  {last_}         := last;
];

toAuthorLink2 = Case[
  str_Str         := If[MemberQ[$KnownAuthors, str], StringJoin["[[", str, "]]"], str];
  {first_, last_} := toAuthorLink2 @ StringJoin[first, " ", last];
  {last_}         := last;
]

(**************************************************************************************************)

PrivateFunction[sanitizeAuthors]

sanitizeAuthors[authors_] := ReplaceAll[authors, s_String :> StringReplace[s, $authorFixups]];

$authorFixups = {
  "Antoran" -> "Antorán",
  "Urquhart Allingham" -> "Allingham",
  "Von Glehn" -> "von Glehn"
};

(**************************************************************************************************)

PrivateFunction[sanitizeAbstract, sanitizeTitle]

sanitizeAbstract[str_Str] := StringTrim @ StringReplace[str, $AbstractNormalizationRules];
sanitizeTitle[str_Str] := StringTrim @ StringReplace[str, $TitleNormalizationRules]

(**************************************************************************************************)

PublicFunction[PaperPageTitle]
PublicVariable[$KnownAuthors]

$KnownAuthors := $KnownAuthors = BearPeople[];

PaperPageTitle[authors_, title_] := Scope[
  authors //= VectorReplace[str_Str :> SplitFirstLastName[str]];
  If[Len[authors] > 4,
    knownAuthorAssoc = ConstantAssociation[VectorReplace[$KnownAuthors, str_Str :> SplitFirstLastName[str]], 0];
    authors2 = SortBy[authors, Lookup[knownAuthorAssoc, Key @ #, 1]&];
    authors2 = Take[authors2, UpTo[4]];
    lastAuthor = PN @ authors;
    If[MemberQ[authors2, lastAuthor], authors2 = Append[lastAuthor] @ DeleteCases[authors2, lastAuthor]];
    authorSurnames = authors2[[All, -1]];
    If[Len[authors] > Len[authors2], AppendTo[authorSurnames, "et al"]];
    VPrint["Chosen authors: ", authorSurnames];
  ,
    authorSurnames = authors[[All, -1]];
  ];
  authorPrefix = StringRiffle[authorSurnames, ", "];
  title //= StringReplace[{"$" -> "", "\\'e" -> "é", "\\\"o" -> "ö"}];
  heading = StringJoin[authorPrefix, ": \"", title, "\""];
  heading
]

(**************************************************************************************************)

PublicIOFunction[DownloadPaper]

PublicVariable[$PDFPath]

PublicOption[PDFPath, AllowRename]

SetInitialValue[$PDFPath, With[{path = NormalizePath @ "~/Dropbox/doc/paper"}, If[FileExistsQ[path], path, DataPath["Papers"]]]];

Options[DownloadPaper] = {
  PDFPath -> Automatic,
  Verbose -> Automatic,
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
  SetAutomatic[$verbose, $dryRun];

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

  partialTitle = Part[StringSplit[pdfFileName, "\""], 2];
  If[StringLength[partialTitle] > 8,
    titleGlob = StringJoin["*", StringReplace[partialTitle, "'"|":" -> "*"], "*.pdf"];
    targets = FileNames[titleGlob, pdfDir, IgnoreCase -> True];
    If[Len[targets] === 1,
      target = P1 @ targets;
      VPrint["Found existing candidate ", MsgPath @ target, ", renaming to ", MsgPath @ localPdfPath, "."];
      If[!TrueQ[allowRename], VPrint["Renames forbidden, skipping"];
        ReturnFailed["norename", MsgPath @ target]];
      whenWet @ MoveFile[target, localPdfPath];
      Return @ localPdfPath
    ];
  ];
  Label[SkipRename];

  If[!StringQ[pdfUrl] && StringQ[url],
    VPrint["Searching for mirror URL for ", MsgPath @ url];
    pdfUrl = FindPDFMirrorURL[url];
    If[StringQ[pdfUrl], VPrint["Found mirror at ", MsgPath @ pdfUrl]];
  ];

  If[!StringQ[pdfUrl], ReturnFailed["nopdfurl", title]];

  res = SafeURLDownload[pdfUrl, localPdfPath, OverwriteTarget -> True];
  If[!StringQ[res] || !FileExistsQ[res], ReturnFailed[]];

  If[FileByteCount[res] < $minPaperSize,
    VPrint["Downloaded paper ", MsgPath @ res, " is too small, deleting."];
    TrashFile[res];
    ReturnFailed[];
  ];

  res
]

(**************************************************************************************************)

PrivateFunction[toPaperPDFPath]

toPaperPDFPath[Automatic, authorsAndTitle_] := toPaperPDFPath[$PDFPath, authorsAndTitle];

toPaperPDFPath[pdfPath_, authorsAndTitle_] := Scope[
  fileName = StringReplace[StringReplace[authorsAndTitle, ":" -> " - "], Repeated[" ", {2, Infinity}] -> " "];
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
  If[!StringQ[pdfUrl], ReturnFailed[]];
  SafeURLDownload[pdfUrl, localPdfPath]
];

$containsRawPDFUrlP = "https://journals.plos.org" | "https://direct.mit.edu/";
$rawPDFP = ("\"https://" ~~ Except["\""].. ~~ ".pdf\"") | (PositiveLookbehind["href="] ~~ "\"" ~~ Except["\""].. ~~ "type=printable\"")

FindPDFMirrorURL[url_Str] := Scope[
  domain = URLParse[url, "AbsoluteDomain"];
  If[StringMatchQ[url, "https://biorxiv.org/" ~~ ___ ~~ ".pdf"], Return @ url];
  If[StringMatchQ[url, $containsRawPDFUrlP ~~ ___],
    res = CachedURLFetch[url];
    If[StringQ[res],
      pdfUrl = StringCases[res, z:$rawPDFP :> StringTrim[z, "\""]];
      pdfUrl = First[pdfUrl, None];
      If[StringQ[pdfUrl],
        If[StringStartsQ[pdfUrl, "/"], pdfUrl = StringJoin[domain, StringTrimLeft[pdfUrl, "/"]]];
        Goto[Done]
      ];
    ];
  ];
  If[StringMatchQ[url, "https://www.tandfonline.com/doi/full/" ~~ ___],
    pdfUrl = StringReplace[P1 @ StringSplit[url, "?", 2], "/full/" -> "/pdf/"] <> "?download=true";
    Goto[Done];
  ];
  Do[
    mirrorUrl = mirror <> url;
    VPrint["Trying ", MsgPath @ mirrorUrl];
    res = CachedURLFetch[mirrorUrl];
    If[StringQ[res],
      pdfUrl = StringCases[res, "embed type=\"application/pdf\" src=\"" ~~ u:Except["\""].. ~~ "\"" :> u];
      pdfUrl = First[pdfUrl, None];
      If[StringQ[pdfUrl],
        If[StringStartsQ[pdfUrl, "/"],
          pdfUrl = StringJoin[mirror, StringTrim[pdfUrl, "/"]]];
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

