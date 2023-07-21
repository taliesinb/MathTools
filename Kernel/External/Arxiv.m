xmlFilePath[id_] := LocalPath["Data", "Arxiv", id <> ".m"]

(**************************************************************************************************)

PublicSymbol[ArxivPaperIDPattern]

declareStringPattern[ArxivPaperIDPattern :> "(?ms)(?:\\d{1,4}\\.|hep-th[/.]|gr-qc[/.]|q-alg[/.])\\d{4,7}(?:v\\d)?"]

(**************************************************************************************************)

PublicFunction[ArxivPageToMarkdown]

ArxivPageToMarkdown[url_String, opts___Rule] := PaperToMarkdown[ImportArxivPage[url, FilterOptions @ opts], FilterOptions @ opts];

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
	id = FirstStringCase[str, num:ArxivPaperIDPattern :> num];
	If[!StringQ[id], ReturnFailed["badurl", str]];
	xmlPath = xmlFilePath[id];
	If[FileExistsQ[xmlPath],
		xml = Get[xmlPath]
	,
    url = $pageTemplate[id];
		xml = WithInternet @ Import[url, "XMLObject"];
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
    "Authors" -> DeepCases[xml, XMLMetaPattern["citation_author", content_] :> fromLastFirstName[StringReplace[content, $authorNormalizationRules]]],
    "URL" -> DeepFirstCase[xml, XMLElement["meta", {"property" -> "og:url", "content" -> content_}, _] :> content],
    "PrimarySubjects" -> primarySubjects,
    "SecondarySubjects" -> secondarySubjects,
    "Subjects" -> Join[primarySubjects, secondarySubjects],
    "Origin" -> "Arxiv"
  ];
  data //= KeySortBy[$paperKeyOrder];
  UnpackOptions[downloadPDF, pDFPath];
  localPdfPath = toPDFPath[pDFPath, data["Title"]];
  data["PDFFilePath"] = Which[
    FileExistsQ[localPdfPath], localPdfPath,
    downloadPDF,               DownloadPaper[data, FilterOptions @ opts],
    True,                      None
  ];
  data
];

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

PublicFunction[ArxivSearch]

PublicOption[PaperAuthor, PaperTitle, PaperAbstract, PaperSubject, PaperID]

Options[ArxivSearch] = {
  PaperAuthor -> None,
  PaperTitle -> None,
  PaperAbstract -> None,
  PaperSubject -> None,
  PaperID -> None
};

ArxivSearch::badhtml = "Did not obtain a valid HTML result."
ArxivSearch::toomany = "Search returned at least 200 results, some might be ignored.";

ArxivSearch[opts___Rule] := Scope[
  url = toSearchString[{opts}];
  hash = Base36Hash[url];
  htmlPath = xmlFilePath @ hash;
  If[FileExistsQ[htmlPath] && FileAge[htmlPath] < 60 * 60 * 24,
    VPrint["Getting cached data from ", MsgPath @ htmlPath];
    searchResults = ImportUTF8[htmlPath];
  ,
    searchResults = WithInternet @ Import[url, "Text"];
    If[!StringQ[searchResults] || !StringContainsQ[searchResults, "Advanced Search"], ReturnFailed["badhtml", url]];
    ExportUTF8[htmlPath, searchResults];
  ];
  ids = StringCases[searchResults, "<a href=\"https://arxiv.org/abs/" ~~ id:Except["\""].. ~~ "\"" :> id];
  If[ids === {}, Return @ {}];
  If[Length[ids] >= 200, Message[ArxivSearch::toomany]; ids = Take[ids, 200]];
  ArxivAPISearch[PaperID -> ids]
];

$searchSpecTemplate = StringFunction @ "&terms-#1-operator=AND&terms-#1-term=#3&terms-#1-field=#2";

$arxivSearchTemplate = StringFunction @ "https://arxiv.org/search/advanced?advanced=#1&abstracts=show&size=200&order=-announced_date_first";

toSearchString[rules_List] := Scope[
  $index = 0;
  $arxivSearchTemplate @ StringJoin @ Map[toSearchStringFragment, Sort @ rules]
];

toSearchStringFragment = Case[
  (_Symbol -> None)            := Nothing;
  (field_Symbol -> q_String)   := $searchSpecTemplate[$index++, fieldToType @ field, StringReplace["%20" -> "+"] @ URLEncode @ q];
];

fieldToType = <|PaperAuthor -> "author", PaperTitle -> "title", PaperAbstract -> "abstract", PaperID -> "id", PaperSubject -> "cross_list_category"|>;

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
  If[FileExistsQ[xmlPath] && FileAge[xmlPath] < 60 * 60 * 24,
    VPrint["Getting cached data from ", MsgPath @ xmlPath];
    xml = Get[xmlPath];
  ,
    xml = WithInternet @ Import[url, "XML"];
    If[!ContainsQ[xml, XMLElement["id", ___]], ReturnFailed["badxml", url]];
    Export[xmlPath, xml];
  ];
  entries = DeepCases[xml, XMLElement["entry", {}, entryData_] :> extractAPIEntryData[entryData]];
  Reverse @ SortBy[entries, Key["Date"]]
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
   "Authors" -> DeepCases[xml, XMLElement["name", _, {data_}] :> StringReplace[data, $authorNormalizationRules]],
   "Date" -> DeepFirstCase[xml, XMLElement["published", _, {date_}] :> DateObject[date]],
   "URL" -> DeepFirstCase[xml, XMLElement["id", {}, {id_}] :> StringReplace[id, "http://" -> "https://"]],
   "Abstract" -> DeepFirstCase[xml, XMLElement["summary", _, {data_}] :> data],
   "Subjects" -> DeleteDuplicates @ DeepCases[xml, ("term" -> term_String) :> term],
   "Origin" -> "Arxiv"
  ];
  assoc["PDFURL"] = StringReplace[assoc["URL"], "/abs/" -> "/pdf/"] <> ".pdf";
  assoc //= KeySortBy[$paperKeyOrder];
  assoc
];

PrivateVariable[$paperKeyOrder]
$paperKeyOrder = <|"Title" -> 1, "Date" -> 2, "Authors" -> 3, "URL" -> 4, "PDFURL" -> 5, "PrimarySubjects" -> 8, "SecondarySubjects" -> 9, "Subjects" -> 10, "Origin" -> "Arxiv", "Abstract" -> 100|>;

$authorNormalizationRules = {" St. " -> " St ", "T. St " -> "Toby St ", "Toby B. St " -> "Toby St "};