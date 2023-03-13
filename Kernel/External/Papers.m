$ArxivTaxonomyDictionary := $ArxivTaxonomyDictionary = Association[
  Rule[ToLowerCase[#1], #2]& @@@ StringExtract[ImportUTF8 @ LocalPath["Kernel", "External", "ArxivTaxonomy.txt"], "\n" -> All, "\t" -> All]
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

PaperToMarkdown::addtextabs = "AdditionalText seems to contain abstract for article \"``\"."
PaperToMarkdown[data_Association, OptionsPattern[]] := Scope[
  UnpackOptions[pDFPath, downloadPDF, $verbose, additionalText];
  UnpackAssociation[data, authors, title, origin, subjects, abstract, url:"URL"];
  title //= sanitizeTitle;
  title = PaperPageTitle[authors, title];
  If[Length[authors] > 8,
    authors = SortBy[authors, {!MemberQ[$KnownAuthors, If[ListQ[#], StringRiffle[#, " "], #]], Position[authors, #]}&];
    authorLinks = StringRiffle[Append["et al"] @ Take[Map[toAuthorLink2, authors], 12], ", "]
  ,
    authorLinks = StringRiffle[toAuthorLink /@ authors, ", "]
  ];
  abstract //= sanitizeAbstract;
  subjectString = If[
    subjects =!= None && origin === "Arxiv",
    StringRiffle[removeDupPrefix @ Lookup[$ArxivTaxonomyDictionary, ToLowerCase @ StripLabel @ subjects, Nothing], ", "],
    None
  ];
  localPDFPath = toPDFPath[pDFPath, title];
  If[!FileExistsQ[localPDFPath] && downloadPDF && origin === "Arxiv",
    ArxivDownloadPDF[data, PDFPath -> pDFPath, Verbose -> $verbose]
  ];
  downloadTag = If[FileExistsQ[localPDFPath], "#meta/downloaded", "#todo/download"];
  If[StringQ[additionalText],
    additionalText //= StringTrim /* StringDelete[("> " <> abstract) | abstract] /* StringTrim;
    If[EditDistance[additionalText, abstract] < StringLength[abstract] * 0.05,
      Message[PaperToMarkdown::addtexttabs, title]];
  ];
  StringJoin[
    "# ", title, "\n",
    "\n",
    "#doc/paper", If[StringQ[subjectString], {" in ", subjectString}, ""], " by ", authorLinks, "\n",
    "\n",
    downloadTag, "\n",
    url,
    "\n\n",
    "> ", abstract,
    If[StringQ[additionalText] && additionalText =!= "", {"\n\n", additionalText}, ""]
  ]
];

PrivateFunction[sanitizeAbstract, sanitizeTitle]

sanitizeAbstract[str_String] := StringReplace[str, {
  "\n" ~~ Repeated[" "... ~~ "\n"] :> "\n", "`" -> "'",
  "\[OpenCurlyDoubleQuote]" -> "\"", "\[CloseCurlyDoubleQuote]" -> "\"",
  "\[OpenCurlyQuote]" -> "'", "\[CloseCurlyQuote]" -> "'"
}];

sanitizeTitle[str_String] := StringReplace[str, $TitleNormalizationRules]

toAuthorLink = Case[
  str_String /; StringContainsQ[str, "."] := str;
  str_String      := StringJoin["[[", DeleteMiddleInitials @ str, "]]"];
  {first_, last_} := StringJoin["[[", first, " ", last, "]]"];
  {last_}         := last;
];

toAuthorLink2 = Case[
  str_String      := If[MemberQ[$KnownAuthors, str], StringJoin["[[", str, "]]"], str];
  {first_, last_} := toAuthorLink2 @ StringJoin[first, " ", last];
  {last_}         := last;
]

removeDupPrefix[list_] := Select[DeleteDuplicates @ list, elem |-> NoneTrue[DeleteCases[elem] @ list, other |-> StringStartsQ[other, elem]]];

(**************************************************************************************************)

PublicVariable[$KnownAuthors]

SetInitialValue[$KnownAuthors, {}];

PaperPageTitle[authors_, title_] := Scope[
  authors //= VectorReplace[str_String :> SplitFirstLastName[str]];
  If[Length[authors] > 4,
    knownAuthorAssoc = ConstantAssociation[VectorReplace[$KnownAuthors, str_String :> SplitFirstLastName[str]], 0];
    authors2 = SortBy[authors, Lookup[knownAuthorAssoc, #, 1]&];
    authors2 = Take[authors2, UpTo[4]];
    lastAuthor = Last @ authors;
    If[MemberQ[authors2, lastAuthor], authors2 = Append[lastAuthor] @ DeleteCases[authors2, lastAuthor]];
    authorSurnames = authors2[[All, -1]];
    If[Length[authors] > Length[authors2], AppendTo[authorSurnames, "et al"]];
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

PublicFunction[DownloadPaper]

PublicVariable[$PDFPath]

PublicOption[PDFPath, AllowRename]

SetInitialValue[$PDFPath, With[{path = NormalizePath @ "~/Dropbox/doc/paper"}, If[FileExistsQ[path], path, Automatic]]];

Options[DownloadPaper] = {
  PDFPath :> $PDFPath,
  Verbose -> Automatic,
  DryRun -> False,
  AllowRename -> True,
  OverwriteTarget -> False
};

DownloadPaper::norename = "Existing candidate `` found, but renames are forbidden."
DownloadPaper::badrename = "Could not rename `` to ``."
DownloadPaper::nopdfurl = "No PDF URL could be found for paper with title \"``\"."

toPDFPath[Automatic, title_] := toPDFPath[LocalPath["Data", "Papers"], title];

toPDFPath[pdfPath_, title_] := Scope[
  fileName = StringReplace[StringReplace[title, ":" -> " - "], Repeated[" ", {2, Infinity}] -> " "];
  PathJoin[pdfPath, fileName <> ".pdf"]
]

$minPaperSize = 5000;

DownloadPaper[assoc_Association, OptionsPattern[]] := Scope[
  UnpackAssociation[assoc, authors, title, pdfUrl:"PDFURL", url:"URL"];
  UnpackOptions[$verbose, $dryRun, pDFPath, allowRename, overwriteTarget];
  SetAutomatic[$verbose, $dryRun];
  title = PaperPageTitle[authors, title];
  VPrint[title];

  localPdfPath = toPDFPath[pDFPath, title];
  pdfFileName = FileNameTake @ localPdfPath;
  pdfDir = FileNameDrop @ localPdfPath;

  If[FileExistsQ[localPdfPath],
    If[!overwriteTarget,
      If[FileByteCount[localPdfPath] < $minPaperSize,
        VPrint["File ", MsgPath @ localPdfPath, " already exists, but is too small. Downloading again."]
      ,
        VPrint["File ", MsgPath @ localPdfPath, " already exists, skipping download."];
        Return[localPdfPath]
      ];
    ,
      VPrint["File ", MsgPath @ localPdfPath, " already exists, overwriting."];
    ];
    Goto[SkipRename];
  ];

  partialTitle = Part[StringSplit[pdfFileName, "\""], 2];
  If[StringLength[partialTitle] > 8,
    titleGlob = StringJoin["*", StringReplace[partialTitle, "'"|":" -> "*"], "*.pdf"];
    targets = FileNames[titleGlob, pdfDir, IgnoreCase -> True];
    If[Length[targets] === 1,
      target = First @ targets;
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
    VPrint["Downloaded file ", MsgPath @ res, " is too small, deleting."];
    TrashFile[res];
    ReturnFailed[];
  ];

  res
]


(**************************************************************************************************)

PublicFunction[DownloadPDFMirror, FindPDFMirrorURL]

PublicVariable[$SciHubServers]

Clear[$SciHubServers];
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

DownloadPDFMirror[url_String, localPdfPath_String] := Scope[
  pdfUrl = FindPDFMirrorURL[url];
  If[!StringQ[pdfUrl], ReturnFailed[]];
  SafeURLDownload[pdfUrl, localPdfPath]
];

$containsRawPDFUrlP = "https://journals.plos.org" | "https://direct.mit.edu/";
$rawPDFP = ("\"https://" ~~ Except["\""].. ~~ ".pdf\"") | (PositiveLookbehind["href="] ~~ "\"" ~~ Except["\""].. ~~ "type=printable\"")

FindPDFMirrorURL[url_String] := Scope[
  domain = URLParse[url, "AbsoluteDomain"];
  If[StringMatchQ[url, "https://biorxiv.org/" ~~ ___ ~~ ".pdf"], Return @ url];
  If[StringMatchQ[url, $containsRawPDFUrlP ~~ ___],
    res = CachedURLFetch[url];
    If[StringQ[res],
      pdfUrl = StringCases[res, z:$rawPDFP :> StringTrim[z, "\""]];
      pdfUrl = First[pdfUrl, None];
      If[StringQ[pdfUrl],
        If[StringStartsQ[pdfUrl, "/"], pdfUrl '= StringJoin[domain, StringTrimLeft[pdfUrl, "/"]]];
        Goto[Done]
      ];
    ];
  ];
  If[StringMatchQ[url, "https://www.tandfonline.com/doi/full/" ~~ ___],
    pdfUrl = StringReplace[First @ StringSplit[url, "?", 2], "/full/" -> "/pdf/"] <> "?download=true";
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

