xmlFilePath[id_] := LocalPath["Data", "Arxiv", "XML", id <> ".m"]
pdfFilePath[id_] := LocalPath["Data", "Arxiv", "PDF", id <> ".pdf"]

(**************************************************************************************************)

PublicFunction[ImportArxivPage]

ImportArxivPage::badurl = "URL `` does not contain a valid id."
ImportArxivPage::baddl = "URL `` could not be imported as XML."
ImportArxivPage::badxml = "URL `` was downloaded as XML that did not contain a title."

ImportArxivPage[str_String] := Scope[
	id = FirstStringCase[str, num:(Repeated[DigitCharacter, 4] ~~ "." ~~ Repeated[DigitCharacter, {4, 7}]) :> num];
	If[!StringQ[id], ReturnFailed["badurl", str]];
	xmlPath = xmlFilePath[id];
	If[FileExistsQ[xmlPath],
		xml = Get[xmlPath]
	,
    url = $pageTemplate[id];
		xml = Block[{$AllowInternet = True}, Import[url, "XMLObject"]];
    If[Head[xml] =!= XMLObject["Document"], ReturnFailed["baddl", url]];
    If[!ContainsQ[xml, XMLMetaPattern["citation_title", _]], ReturnFailed["badxml", url]];
    Export[xmlPath, xml];
  ];
  subjectSpan = DeepFirstCase[xml, XMLElement["td", {___, "class" -> "tablecell subjects"}, content___] :> content];
  primarySubjects = DeepCases[subjectSpan, XMLElement["span", {"class" -> "primary-subject"}, content___] :> content];
  secondarySubjects = subjectSpan /. XMLElement["span", {"class" -> "primary-subject"}, ___] :> None;
  primarySubjects //= extractSubjects;
  secondarySubjects //= extractSubjects;
	data = Association[
    VectorApply[
      #1 -> DeepFirstCase[xml, XMLMetaPattern[#2, content_] :> StringTrim[content]]&,
      {"Title" -> "citation_title", "Abstract" -> "citation_abstract", "URL" -> "og:url", "PDFUrl" -> "citation_pdf_url", "Date" -> "citation_date"}
    ],
    "URL" -> DeepFirstCase[xml, XMLElement["meta", {"property" -> "og:url", "content" -> content_}, _] :> content],
    "Authors" -> DeepCases[xml, XMLMetaPattern["citation_author", content_] :> fromLastFirstName[content]],
    "PrimarySubjects" -> primarySubjects,
    "SecondarySubjects" -> secondarySubjects,
    "Subjects" -> Join[primarySubjects, secondarySubjects]
  ];
  data
]

$pageTemplate = StringFunction @ "https://arxiv.org/abs/#1"

XMLMetaPattern[name_, patt_] := XMLElement["meta", {"name" -> name, "content" -> patt}, _];

fromLastFirstName[str_] := Scope[
  If[StringFreeQ[str, ","], Return @ List @ str];
  MapFirst[stripInitials] @ Reverse @ StringTrim @ StringSplit[str, ",", 2]
];

stripInitials = StringReplaceRepeated[(" ".. ~~ LetterCharacter ~~ ".") -> " "] /* StringTrim;

extractSubjects[e_] := Flatten @ StringCases[
  Flatten @ DeepCases[e, _String],
  human:(LetterCharacter.. ~~ (" " | " - " ~~ LetterCharacter..)...) ~~ " (" ~~ superfield:((LetterCharacter|"-")..) ~~ Repeated["." ~~ subfield:LetterCharacter.., {0,1}] ~~ ")" :>
    Labeled[DeleteCases[""] @ {ToLowerCase @ superfield, ToLowerCase @ subfield}, human]
];

(**************************************************************************************************)

PublicVariable[$KnownAuthors]

SetInitialValue[$KnownAuthors, {}];

PaperPageTitle[authors_, title_] := Scope[
  If[Length[authors] > 3,
    authors2 = Cases[authors, {_, Alternatives @@ $KnownAuthors}];
    If[Length[authors2] <= 1, authors2 = Part[authors, {1, 2, -1}]];
    authorSurnames = authors2[[All, -1]];
    AppendTo[authorSurnames, "et al"];
  ,
    authorSurnames = authors[[All, -1]];
  ];
  authorPrefix = StringRiffle[authorSurnames, ", "];
  heading = StringJoin[authorPrefix, ": \"", title, "\""];
  heading
]

(**************************************************************************************************)

PublicFunction[DownloadArxivPDF]

PublicOption[PDFPath, AllowRename]

Options[DownloadArxivPDF] = {
  PDFPath -> Automatic,
  Verbose -> Automatic,
  DryRun -> False,
  AllowRename -> True
};

DownloadArxivPDF::baddl = "Failed to download `` to ``."
DownloadArxivPDF::rename = "Existing candidate `` found, but renames are forbidden."

DownloadArxivPDF[assoc_Association, OptionsPattern[]] := Scope[
  UnpackAssociation[assoc, authors, title, pdfUrl:"PDFUrl"];
  UnpackOptions[$verbose, $dryRun, pDFPath, allowRename];
  SetAutomatic[$verbose, $dryRun];
  title = PaperPageTitle[authors, title];
  localFileName = StringReplace[StringReplace[title, ":" -> " - "], Repeated[" ", {2, Infinity}] -> " "];
  If[pDFPath === Automatic,
    localPath = pdfFilePath[localFileName],
    localPath = FileNameJoin[{pDFPath, localFileName <> ".pdf"}];
  ];
  If[FileExistsQ[localPath],
    VPrint["File ", MsgPath @ localPath, " already exists, skipping download."];
    Return[localPath]];
  If[pDFPath =!= Automatic,
    partialTitle = Part[StringSplit[localFileName, "\""], 2];
    titleGlob = StringJoin["*", StringReplace[partialTitle, "'"|":" -> "*"], "*.pdf"];
    targets = FileNames[titleGlob, pDFPath, IgnoreCase -> True];
    If[Length[targets] === 1,
      target = First @ targets;
      VPrint["Found existing candidate ", MsgPath @ target, ", renaming to ", MsgPath @ localPath, "."];
      If[!TrueQ[allowRename], VPrint["Renames forbidden, skipping"];
        ReturnFailed["rename", MsgPath @ target]];
      whenWet @ RenameFile[target, localPath];
      Return @ localPath
    ];
  ];
  VPrint["Downloading ", MsgPath @ pdfUrl, " to ", MsgPath @ localPath];
  whenWet[
    tmpPath = FileNameJoin[{$TemporaryDirectory, localFileName <> ".pdf"}];
    result = Block[{$AllowInternet = True}, Check[URLDownload[pdfUrl, tmpPath], $Failed]];
    If[!MatchQ[result, File[_]], ReturnFailed["baddl", MsgPath @ pdfUrl, MsgPath @ localPath]];
    RenameFile[tmpPath, localPath];
  ];
  localPath
]

(**************************************************************************************************)

PublicFunction[ArxivDataToMarkdown]

ArxivDataToMarkdown[data_Association] := Scope[
  UnpackAssociation[data, authors, subjects, title, abstract, url:"URL"];
  title = PaperPageTitle[authors, title];
  authorLinks = StringRiffle[StringJoin["[[", #1, " ", #2, "]]"]& @@@ authors, ", "];
  authorPrefix = StringRiffle[authorSurnames, ", "];
  abstract //= StringReplace["\n" ~~ Repeated[" "... ~~ "\n"] :> "\n"];
  subjectString = StringRiffle["#field/" <> #1& @@@ subjects, ", "];
  StringJoin[
    "# ", title, "\n",
    "\n",
    "#doc/paper in ", subjectString, " by ", authorLinks, "\n",
    "\n",
    url,
    "\n\n",
    "> ", abstract
  ]
];