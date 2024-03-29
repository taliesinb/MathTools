xmlFilePath[id_] := DataPath["Arxiv", SRep[id, "/"|"." -> "_"] <> ".m"]

(**************************************************************************************************)

PublicStringPattern[ArxivPaperIDPattern]

DefineStringPattern[ArxivPaperIDPattern :> RawRegex["(?:\\d{1,4}\\.|hep-th[/.]|gr-qc[/.]|q-alg[/.])\\d{4,7}(?:v\\d)?"]]

(**************************************************************************************************)

PublicIOFunction[ImportArxivPageToMarkdown]

ImportArxivPageToMarkdown[url_Str, opts___Rule] :=
  PaperToMarkdown[ImportArxivPage[url, FilterOptions @ opts], FilterOptions @ opts];

(**************************************************************************************************)

PublicIOFunction[ImportArxivPage]

PublicOption[DownloadPDF, PDFPath]

Options[ImportArxivPage] = {
  DownloadPDF -> True,
  PDFPath -> Auto,
  Verbose -> False
}

ImportArxivPage::badurl = "URL `` does not contain a valid id."
ImportArxivPage::baddl = "URL `` could not be imported as XML."
ImportArxivPage::badxml = "URL `` was downloaded as XML that did not contain a title."

ImportArxivPage[str_Str, opts:OptionsPattern[]] := Scope[
  UnpackOptions[$verbose];

	id = FirstStringCase[str, num:ArxivPaperIDPattern :> num];
  VPrint["Arxiv paper ID: ", id];
	If[!StrQ[id], ReturnFailed["badurl", str]];

  xmlPath = xmlFilePath[id];
	If[FileExistsQ[xmlPath],
    VPrint["XML file exists at ", MsgPath @ xmlPath];
		xml = Get[xmlPath]
	,
    url = $pageTemplate[id];
    VPrint["Downloading XML to ", MsgPath @ xmlPath, " from ", MsgPath @ url];
		xml = WithInternet @ Import[url, "XMLObject"];
    If[H[xml] =!= XMLObject["Document"], ReturnFailed["baddl", url]];
    If[!ContainsQ[xml, xmlMetaPattern["citation_title", _]], ReturnFailed["badxml", url]];
    Export[xmlPath, xml];
  ];

  subjectSpan = DeepFirstCase[xml, XMLElement["td", {___, "class" -> "tablecell subjects"}, content___] :> content];
  primarySubjects = DeepCases[subjectSpan, XMLElement["span", {"class" -> "primary-subject"}, content___] :> content];
  secondarySubjects = subjectSpan /. XMLElement["span", {"class" -> "primary-subject"}, ___] :> None;
  primarySubjects //= extractSubjects;
  secondarySubjects //= extractSubjects;

  (* since we want to preserve the original order of primary / secondary, use removeDupPrefix instead of DeleteRedundantTags *)
  allSubjects = Join[primarySubjects, secondarySubjects];
  fieldTags = Lookup[$ArxivTaxonomyDictionary, ToLowerCase @ StripLabel @ allSubjects, Nothing];

  customMetadata = Assoc[
    "PrimaryArxivSubjects" -> primarySubjects,
    "SecondaryArxivSubjects" -> secondarySubjects
  ];

  metadata = VectorApply[
    #1 -> DeepFirstCase[xml, xmlMetaPattern[#2, content_] :> STrim[content]]&,
    {"Title" -> "citation_title", "Abstract" -> "citation_abstract", "URL" -> "og:url", "PDFURL" -> "citation_pdf_url", "Date" -> "citation_date"}
  ];

  authors = DeepCases[xml, xmlMetaPattern["citation_author", content_] :> fromLastFirstName[SRep[content, $authorNormalizationRules]]];
  authors //= sanitizeAuthors;
  url = DeepFirstCase[xml, XMLElement["meta", {"property" -> "og:url", "content" -> content_}, _] :> content];

  data = Assoc[
    metadata,
    "Authors" -> authors,
    "URL" -> url,
    "FieldTags" -> fieldTags,
    "Origin" -> "Arxiv",
    "CustomMetadata" -> customMetadata
  ];

  UnpackOptions[downloadPDF, pDFPath];
  postProcessPaperPageData[data, downloadPDF, pDFPath]
];

removeDupPrefix[list_] := Select[Dedup @ list, elem |-> NoneTrue[Decases[elem] @ list, other |-> SStartsQ[other, elem]]];

(**************************************************************************************************)

$ArxivTaxonomyDictionary := $ArxivTaxonomyDictionary = Assoc[
  Rule[ToLowerCase[#1], #2]& @@@ SExtract[ImportUTF8 @ DataPath["Text", "ArxivTaxonomy.txt"], "\n" -> All, "\t" -> All]
];

(**************************************************************************************************)

$pageTemplate = StringFunction @ "https://arxiv.org/abs/#1"

xmlMetaPattern[name_, patt_] := XMLElement["meta", {"name" -> name, "content" -> patt}, _];

fromLastFirstName["Hooft, Gerard 't"] := {"Gerard", "'t Hooft"};

fromLastFirstName[str_] := Scope[
  If[SFreeQ[str, ","], Return @ List @ str];
  MapFirst[stripInitials] @ Rev @ STrim @ SSplit[str, ",", 2]
];

stripInitials = StringReplaceRepeated[(" ".. ~~ LetterCharacter ~~ ".") -> " "] /* STrim;

extractSubjects[e_] := Flatten @ SCases[
  Flatten @ DeepCases[e, _Str],
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
    If[!StrQ[searchResults] || !SContainsQ[searchResults, "Advanced Search"], ReturnFailed["badhtml", url]];
    ExportUTF8[htmlPath, searchResults];
  ];
  ids = SCases[searchResults, "<a href=\"https://arxiv.org/abs/" ~~ id:Except["\""].. ~~ "\"" :> id];
  If[ids === {}, Return @ {}];
  If[Len[ids] >= 200, Message[ArxivSearch::toomany]; ids = Take[ids, 200]];
  ArxivAPISearch[PaperID -> ids]
];

$searchSpecTemplate = StringFunction @ "&terms-#1-operator=AND&terms-#1-term=#3&terms-#1-field=#2";

$arxivSearchTemplate = StringFunction @ "https://arxiv.org/search/advanced?advanced=#1&abstracts=show&size=200&order=-announced_date_first";

toSearchString[rules_List] := Scope[
  $index = 0;
  $arxivSearchTemplate @ SJoin @ Map[toSearchStringFragment, Sort @ rules]
];

toSearchStringFragment = Case[
  (_Symbol -> None)       := Nothing;
  (field_Symbol -> q_Str) := $searchSpecTemplate[$index++, fieldToType @ field, SRep["%20" -> "+"] @ URLEncode @ q];
];

fieldToType = <|PaperAuthor -> "author", PaperTitle -> "title", PaperAbstract -> "abstract", PaperID -> "id", PaperSubject -> "cross_list_category"|>;

(**************************************************************************************************)

PrivateFunction[ArxivAPISearch]

Options[ArxivAPISearch] = Options[ArxivSearch];

ArxivAPISearch::badxml = "Did not obtain a valid XML result."

ArxivAPISearch[opts___Rule] := Scope[
  url = $arxivAPISearchURL;
  searchQuery = SRiffle[Map[toAPISearchString, Sort @ {opts}], "+AND+"];
  url //= addQueryElem["search_query", searchQuery];

  idList = ToList @ SubNone[{}] @ Lookup[{opts}, PaperID, {}];
  idQuery = encodeAPIQuery @ SRiffle[idList, ","];
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
  Rev @ SortBy[entries, Key["Date"]]
];

addQueryElem[name_, ""][url_] := url;
addQueryElem[name_, val_][url_] := SJoin[url, "&", name, "=", val];

toAPISearchString = Case[
  (_ -> None) := Nothing;
  (PaperID -> _) := Nothing;
  (PaperAuthor -> q_) := "au:" <> encodeAPIQuery[q];
  (PaperTitle -> q_) := "ti:" <> encodeAPIQuery[q];
  (PaperAbstract -> q_) := "abs:" <> encodeAPIQuery[q];
];

encodeAPIQuery[a_] := URLEncode @ If[SContainsQ[a, " "], "\"" <> a <> "\"", a];

$arxivAPISearchURL = "http://export.arxiv.org/api/query?start=0&max_results=100";

extractAPIEntryData[xml_] := Scope[
  assoc = Assoc[
   "Title" -> DeepFirstCase[xml, XMLElement["title",{},{title_}] :> title],
   "Authors" -> DeepCases[xml, XMLElement["name", _, {data_}] :> SRep[data, $authorNormalizationRules]],
   "Date" -> DeepFirstCase[xml, XMLElement["published", _, {date_}] :> DateObject[date]],
   "URL" -> DeepFirstCase[xml, XMLElement["id", {}, {id_}] :> SRep[id, "http://" -> "https://"]],
   "Abstract" -> DeepFirstCase[xml, XMLElement["summary", _, {data_}] :> data],
   "Subjects" -> Dedup @ DeepCases[xml, ("term" -> term_Str) :> term],
   "Origin" -> "Arxiv"
  ];
  assoc["PDFURL"] = SRep[assoc["URL"], "/abs/" -> "/pdf/"] <> ".pdf";
  assoc //= KSortBy[paperKeyOrder];
  assoc
];

$authorNormalizationRules = {" St. " -> " St ", "T. St " -> "Toby St ", "Toby B. St " -> "Toby St "};