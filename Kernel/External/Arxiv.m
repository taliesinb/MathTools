xmlFilePath[id_] := LocalPath["Data", "Arxiv", "XML", id <> ".m"]

(**************************************************************************************************)

PublicFunction[ImportArxivPage]

PublicOption[DownloadPDF, PDFPath]

Options[ImportArxivPage] = {
  DownloadPDF -> True,
  PDFPath -> Automatic
}

ImportArxivPage::badurl = "URL `` does not contain a valid id."
ImportArxivPage::baddl = "URL `` could not be imported as XML."
ImportArxivPage::badxml = "URL `` was downloaded as XML that did not contain a title."

ImportArxivPage[str_String, opts:OptionsPattern[]] := Scope[
	id = FirstStringCase[str, num:$arixvIDRegexp :> num];
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
      {"Title" -> "citation_title", "Abstract" -> "citation_abstract", "URL" -> "og:url", "PDFURL" -> "citation_pdf_url", "Date" -> "citation_date"}
    ],
    "URL" -> DeepFirstCase[xml, XMLElement["meta", {"property" -> "og:url", "content" -> content_}, _] :> content],
    "Authors" -> DeepCases[xml, XMLMetaPattern["citation_author", content_] :> fromLastFirstName[content]],
    "PrimarySubjects" -> primarySubjects,
    "SecondarySubjects" -> secondarySubjects,
    "Subjects" -> Join[primarySubjects, secondarySubjects]
  ];
  UnpackOptions[downloadPDF, pDFPath];
  localPdfPath = toPDFPath[pDFPath, data["Title"]];
  data["PDFFilePath"] = Which[
    FileExistsQ[localPdfPath], localPdfPath,
    downloadPDF,               ArxivDownloadPDF[data, FilterOptions @ opts],
    True,                      None
  ];
  data
];

$arixvIDRegexp = ((Repeated[DigitCharacter, 4] ~~ ".") | ("hep-th" ~~ ("/" | "."))) ~~ Repeated[DigitCharacter, {4, 7}];

$pageTemplate = StringFunction @ "https://arxiv.org/abs/#1"

XMLMetaPattern[name_, patt_] := XMLElement["meta", {"name" -> name, "content" -> patt}, _];

fromLastFirstName["Hooft, Gerard 't"] := {"Gerard", "'t Hooft"};
fromLastFirstName[str_] := Scope[
  If[StringFreeQ[str, ","], Return @ List @ str];
  MapFirst[stripInitials] @ Reverse @ StringTrim @ StringSplit[str, ",", 2]
];

stripInitials = StringReplaceRepeated[(" ".. ~~ LetterCharacter ~~ ".") -> " "] /* StringTrim;

extractSubjects[e_] := Flatten @ StringCases[
  Flatten @ DeepCases[e, _String],
  human:(LetterCharacter.. ~~ (" " | " - " ~~ LetterCharacter..)...) ~~ " (" ~~ field:((LetterCharacter|"-").. ~~ Repeated["." ~~ LetterCharacter.., {0,1}]) ~~ ")" :>
    Labeled[ToLowerCase @ field, human]
];

(**************************************************************************************************)

PublicVariable[$KnownAuthors]

SetInitialValue[$KnownAuthors, {}];

PaperPageTitle[authors_, title_] := Scope[
  authors //= VectorReplace[str_String :> SplitFirstLastName[str]];
  If[Length[authors] > 3,
    authors2 = Cases[authors, {_, Alternatives @@ $KnownAuthors}];
    If[Length[authors2] <= 1, authors2 = Part[authors, {1, 2, -1}]];
    authorSurnames = authors2[[All, -1]];
    AppendTo[authorSurnames, "et al"];
  ,
    authorSurnames = authors[[All, -1]];
  ];
  authorPrefix = StringRiffle[authorSurnames, ", "];
  title //= StringReplace[{"$" -> "", "\\'e" -> "é", "\\\"o" -> "ö"}];
  heading = StringJoin[authorPrefix, ": \"", title, "\""];
  heading
]

(**************************************************************************************************)

PublicFunction[ArxivDownloadPDF]

PublicVariable[$PDFPath]

PublicOption[PDFPath, AllowRename]

SetInitialValue[$PDFPath, With[{path = NormalizePath @ "~/Dropbox/doc/paper"}, If[FileExistsQ[path], path, Automatic]]];

Options[ArxivDownloadPDF] = {
  PDFPath :> $PDFPath,
  Verbose -> Automatic,
  DryRun -> False,
  AllowRename -> True
};

ArxivDownloadPDF::baddl = "Failed to download `` to ``."
ArxivDownloadPDF::norename = "Existing candidate `` found, but renames are forbidden."
ArxivDownloadPDF::badrename = "Could not rename `` to ``."

toPDFPath[Automatic, title_] := toPDFPath[LocalPath["Data", "Arxiv", "PDF"], title];
toPDFPath[pdfPath_, title_] := Scope[
  fileName = StringReplace[StringReplace[title, ":" -> " - "], Repeated[" ", {2, Infinity}] -> " "];
  FileNameJoin[{pdfPath, fileName <> ".pdf"}]
]

ArxivDownloadPDF[id_String, opts:OptionsPattern[]] := Scope[
  result = ImportArxivPage[id, DownloadPDF -> False, FilterOptions @ opts];
  If[AssociationQ[result], ArxivDownloadPDF[result, opts], $Failed]
];

ArxivDownloadPDF[assoc_Association, OptionsPattern[]] := Scope[
  UnpackAssociation[assoc, authors, title, pdfUrl:"PDFURL"];
  UnpackOptions[$verbose, $dryRun, pDFPath, allowRename];
  SetAutomatic[$verbose, $dryRun];
  title = PaperPageTitle[authors, title];

  localPdfPath = toPDFPath[pDFPath, title];
  If[FileExistsQ[localPdfPath],
    VPrint["File ", MsgPath @ localPdfPath, " already exists, skipping download."];
    Return[localPdfPath]];

  pdfFileName = FileNameTake @ localPdfPath;
  pdfDir = FileNameDrop @ localPdfPath;

  partialTitle = Part[StringSplit[pdfFileName, "\""], 2];
  If[StringLength[partialTitle] > 8,
    titleGlob = StringJoin["*", StringReplace[partialTitle, "'"|":" -> "*"], "*.pdf"];
    targets = FileNames[titleGlob, pdfDir, IgnoreCase -> True];
    If[Length[targets] === 1,
      target = First @ targets;
      VPrint["Found existing candidate ", MsgPath @ target, ", renaming to ", MsgPath @ localPdfPath, "."];
      If[!TrueQ[allowRename], VPrint["Renames forbidden, skipping"];
        ReturnFailed["norename", MsgPath @ target]];
      whenWet @ MoveFile[target, localPdfPath]
      Return @ localPdfPath
    ];
  ];

  VPrint["Downloading ", MsgPath @ pdfUrl, " to ", MsgPath @ localPdfPath];
  whenWet[
    tmpPath = FileNameJoin[{$TemporaryDirectory, pdfFileName}];
    result = Block[{$AllowInternet = True}, Check[URLDownload[pdfUrl, tmpPath], $Failed]];
    If[!MatchQ[result, File[_]], ReturnFailed["baddl", MsgPath @ pdfUrl, MsgPath @ localPdfPath]];
    MoveFile[tmpPath, localPdfPath];
  ];
  localPdfPath
]

(**************************************************************************************************)

$ArxivTaxonomyDictionary := $ArxivTaxonomyDictionary = Association[
  Rule[ToLowerCase[#1], #2]& @@@ StringExtract[ImportUTF8 @ LocalPath["Kernel", "External", "ArxivTaxonomy.txt"], "\n" -> All, "\t" -> All]
];

(**************************************************************************************************)

PublicFunction[ArxivPageToMarkdown]

Options[ArxivPageToMarkdown] = {
  PDFPath :> $PDFPath,
  DownloadPDF -> True
}

ArxivPageToMarkdown[id_String, opts:OptionsPattern[]] :=
  ArxivPageToMarkdown[ImportArxivPage[id, opts], opts];

ArxivPageToMarkdown[data_Association, OptionsPattern[]] := Scope[
  UnpackOptions[pDFPath];
  UnpackAssociation[data, authors, subjects, title, abstract, url:"URL"];
  title = PaperPageTitle[authors, title];
  authorLinks = StringRiffle[toAuthorLink /@ authors, ", "];
  abstract //= StringReplace["\n" ~~ Repeated[" "... ~~ "\n"] :> "\n"];
  subjectString = StringRiffle[removeDupPrefix @ Lookup[$ArxivTaxonomyDictionary, ToLowerCase @ StripLabel @ subjects, Nothing], ", "];
  localPDFPath = toPDFPath[pDFPath, title];
  downloadTag = If[FileExistsQ[localPDFPath], "#meta/downloaded", "#todo/download"];
  StringJoin[
    "# ", title, "\n",
    "\n",
    "#doc/paper in ", subjectString, " by ", authorLinks, "\n",
    "\n",
    downloadTag, "\n",
    url,
    "\n\n",
    "> ", abstract
  ]
];

toAuthorLink = Case[
  str_String      := StringJoin["[[", str, "]]"];
  {first_, last_} := StringJoin["[[", first, " ", last, "]]"];
  {last_}         := last;
];

removeDupPrefix[list_] := Select[DeleteDuplicates @ list, elem |-> NoneTrue[DeleteCases[elem] @ list, other |-> StringStartsQ[other, elem]]];

(**************************************************************************************************)

PublicFunction[ArxivSearch]

PublicOption[PaperAuthor, PaperTitle, PaperAbstract, PaperID]

Options[ArxivSearch] = {
  PaperAuthor -> None,
  PaperTitle -> None,
  PaperAbstract -> None,
  PaperID -> None
};

ArxivSearch::badhtml = "Did not obtain a valid HTML result."

ArxivSearch[opts___Rule] := Scope[
  url = toSearchString[{opts}];
  hash = Base36Hash[url];
  htmlPath = xmlFilePath @ hash;
  If[FileExistsQ[htmlPath] && (UnixTime[] - UnixTime[FileDate[htmlPath]]) < 60 * 60 * 24,
    VPrint["Getting cached data from ", MsgPath @ htmlPath];
    searchResults = ImportUTF8[htmlPath];
  ,
    searchResults = Block[{$AllowInternet = True}, Import[url, "Text"]];
    If[!StringQ[searchResults] || !StringContainsQ[searchResults, "Advanced Search"], ReturnFailed["badhtml", url]];
    ExportUTF8[htmlPath, searchResults];
  ];
  ids = StringCases[searchResults, "<a href=\"https://arxiv.org/abs/" ~~ id:Except["\""].. ~~ "\"" :> id];
  If[ids === {}, Return @ {}];
  If[Length[ids] > 32, ids = Take[ids, 32]];
  ArxivAPISearch[PaperID -> ids]
];

$searchSpecTemplate = StringFunction @ "&terms-#1-operator=AND&terms-#1-term=#3&terms-#1-field=#2";

$arxivSearchTemplate = StringFunction @ "https://arxiv.org/search/advanced?advanced=#1&abstracts=show&size=50&order=-announced_date_first";

toSearchString[rules_List] := Scope[
  $index = 0;
  $arxivSearchTemplate @ StringJoin @ Map[toSearchStringFragment, Sort @ rules]
];

toSearchStringFragment = Case[
  (_Symbol -> None)            := Nothing;
  (field_Symbol -> q_String)   := $searchSpecTemplate[$index++, fieldToType @ field, StringReplace["%20" -> "+"] @ URLEncode @ q];
];

fieldToType = <|PaperAuthor -> "author", PaperTitle -> "title", PaperAbstract -> "abstract", PaperID -> "id"|>;

(**************************************************************************************************)

PrivateFunction[ArxivAPISearch]

Options[ArxivAPISearch] = Options[ArxivSearch];

ArxivAPISearch::badxml = "Did not obtain a valid XML result."

ArxivAPISearch[opts___Rule] := Scope[
  url = $arxivAPISearchURL;
  searchQuery = StringRiffle[Map[toAPISearchString, Sort @ {opts}], "+AND+"];
  url //= addQueryElem["search_query", searchQuery];

  idList = ToList @ ReplaceNone[{}] @ Lookup[{opts}, PaperID, {}];
  idQuery = encodeAPIQuery @ StringRiffle[idList, ","];
  url //= addQueryElem["id_list", idQuery];

  hash = Base36Hash[url];
  xmlPath = xmlFilePath @ hash;
  If[FileExistsQ[xmlPath] && (UnixTime[] - UnixTime[FileDate[xmlPath]]) < 60 * 60 * 24,
    VPrint["Getting cached data from ", MsgPath @ xmlPath];
    xml = Get[xmlPath];
  ,
    xml = Block[{$AllowInternet = True}, Import[url, "XML"]];
    If[!ContainsQ[xml, XMLElement["id", ___]], ReturnFailed["badxml", url]];
    Export[xmlPath, xml];
  ];
  entries = DeepCases[xml, XMLElement["entry", {}, entryData_] :> extractAPIEntryData[entryData]];
  entries
];

addQueryElem[name_, ""][url_] := url;
addQueryElem[name_, val_][url_] := StringJoin[url, "&", name, "=", val];

toAPISearchString = Case[
  (_ -> None) := Nothing;
  (PaperID -> _) := Nothing;
  (PaperAuthor -> q_) := "au:" <> encodeAPIQuery[q];
  (PaperTitle -> q_) := "ti:" <> encodeAPIQuery[q];
  (PaperAbstract -> q_) := "abs:" <> encodeAPIQuery[q];
];

encodeAPIQuery[a_] := URLEncode @ If[StringContainsQ[a, " "], "\"" <> a <> "\"", a];

$arxivAPISearchURL = "http://export.arxiv.org/api/query?start=0&max_results=100";

extractAPIEntryData[xml_] := Scope[
  assoc = Association[
   "Title" -> DeepFirstCase[xml, XMLElement["title",{},{title_}] :> title],
   "URL" -> DeepFirstCase[xml, XMLElement["id", {}, {id_}] :> StringReplace[id, "http://" -> "https://"]],
   "Abstract" -> DeepFirstCase[xml, XMLElement["summary", _, {data_}] :> data],
   "Authors" -> DeepCases[xml, XMLElement["name", _, {data_}] :> data],
   "PublishDate" -> DeepFirstCase[xml, XMLElement["published", _, {date_}] :> DateObject[date]],
   "Subjects" -> DeleteDuplicates @ DeepCases[xml, ("term" -> term_String) :> term]
  ];
  assoc["PDFURL"] = StringReplace[assoc["URL"], "/abs/" -> "/pdf/"] <> ".pdf";
  assoc
];