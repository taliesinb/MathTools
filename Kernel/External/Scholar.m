PublicVariable[$ScholarUserID]

SetInitialValue[$ScholarUserID, None];

(**************************************************************************************************)

PublicIOFunction[ImportScholarPageToMarkdown]

ImportScholarPageToMarkdown[url_Str, opts___Rule] :=
  PaperToMarkdown[ImportScholarPage[url, FilterOptions @ opts], FilterOptions @ opts];

(**************************************************************************************************)

PublicIOFunction[ImportScholarPage]

PublicOption[DownloadPDF, PDFPath]

Options[ImportScholarPage] = {
  DownloadPDF -> True,
  PDFPath -> Automatic,
  Verbose -> False
}

ImportScholarPage::userid = "Must set a user ID."
ImportScholarPage::badurl = "URL `` does not contain a valid id."
ImportScholarPage::baddl = "URL `` could not be imported as XML."
ImportScholarPage::badxml = "URL `` was downloaded as XML that did not contain a title."
ImportScholarPage::badxml2 = "XML for paper with id `` was missing a field for ``."

xmlFilePath[id_] := DataPath["Scholar", id <> ".mx"];

ImportScholarPage[url_Str, opts:OptionsPattern[]] := Scope[
  UnpackOptions[$verbose];

  If[!StringQ[$ScholarUserID], ReturnFailed["userid"]];
  id = If[
    StringMatchQ[url, Repeated[AlphanumericCharacter | "_" | "-", 12]], url,
    FirstStringCase[url, ":" ~~ num:Repeated[AlphanumericCharacter | "_" | "-", 12] :> num]
  ];
  If[!StringQ[id], ReturnFailed["badurl", url]];

  xmlPath = xmlFilePath[id];
  If[FileExistsQ[xmlPath],
    VPrint["Found cached Google Scholar result at ", MsgPath @ xmlPath];
    xml = ImportMX[xmlPath]
  ,
    url = $paperPageTemplate[$ScholarUserID, id];

    VPrint["Querying Google Scholar with ", MsgPath @ url];
    xml = WithInternet @ Import[url, "XMLObject"];
    If[H[xml] =!= XMLObject["Document"], ReturnFailed["baddl", url]];
    If[!ContainsQ[xml, XMLElement["meta", {"property" -> "og:title", "content" -> _}, _]],
      VPrint[xml];
      ReturnFailed["badxml", url]];
    ExportMX[xmlPath, xml];
  ];

  arxivURL = DeepFirstCase[xml,
    XMLElement["a", {___, "class" -> "gsc_oci_title_link", ___, "href" -> href_Str /; StringStartsQ[href, "https://arxiv.org"], ___}, _] :> href
  ];
  If[StringQ[arxivURL] && StringContainsQ[arxivURL, ArxivPaperIDPattern],
    VPrint["Delegating to ImportArxivPage[\"", arxivURL, "\"]"];
    Return @ ImportArxivPage[arxivURL, FilterOptions @ opts];
  ];

  paperXML = DeepFirstCase[xml, XMLElement["div", {___, "id" -> "gsc_vcpb", ___}, _], ReturnFailed["badxml2", id, "Document"]];
  pdfURL = DeepFirstCase[paperXML,
    XMLElement["div",
      {___, "class" -> "gsc_oci_title_ggi", ___},
      {___, XMLElement["a", {___, "href" -> url_Str, ___}, {___, XMLElement["span", {___, "class" -> "gsc_vcd_title_ggt", ___}, {"[PDF]"}], ___}]}
    ] :> url,
    None
  ];
  If[StringQ[pdfURL] && StringContainsQ[pdfURL, "%"], pdfURL = First @ StringSplit[pdfURL, "%", 2]];

  articleURL = DeepFirstCase[paperXML,
    XMLElement["a", {___, "class"->"gsc_oci_title_link", ___, "href" -> url_, ___}, _] :> url,
    None
  ];

  metadata = Assoc @ DeepCases[paperXML, XMLElement["div", {"class" -> "gs_scl"}, {
    XMLElement["div", {___, "class"->"gsc_oci_field", ___}, {field_}],
    XMLElement["div", {___, "class"->"gsc_oci_value", ___}, {value_}]}] :> Rule[field, value]
  ];

  authors = Lookup[metadata, "Authors", ReturnFailed["badxml2", id, "Authors"]];
  authors = Map[DeleteMiddleInitials, StringTrim @ StringSplit[authors, ","]];
  authors = DeleteCases[authors, ""];
  authors = Map[If[UpperCaseQ[StringDelete[#, " " | "."]], ToTitleCase @ ToLowerCase @ #, #]&, authors];
  authors //= sanitizeAuthors;
  VPrint["Authors: ", authors];

  title = DeepFirstCase[xml,
    XMLElement["meta", {"property" -> "og:title", "content" -> t_}, _] :> t,
    ReturnFailed["badxml2", id, "Title"]
  ];
  title //= sanitizeTitle;

  publicationDate = Lookup[metadata, "Publication date", None];
  If[StringQ[publicationDate], publicationDate //= DateObject];

  abstract = Lookup[metadata, "Description", ReturnFailed["badxml2", id, "Abstract"]];
  abstract //= XMLToText /* sanitizeAbstract;

  data = Assoc[
    "Title" -> title,
    "Authors" -> authors,
    "Date" -> publicationDate,
    "URL" -> articleURL,
    "PDFURL" -> pdfURL,
    "FieldTags" -> None,
    "Abstract" -> abstract,
    "Origin" -> "GoogleScholar"
  ];

  UnpackOptions[downloadPDF, pDFPath];
  postProcessPaperPageData[data, downloadPDF, pDFPath]
];

$paperPageTemplate = StringFunction["https://scholar.google.com/citations?view_op=view_citation&hl=en&user=#1&sortby=pubdate&citation_for_view=#1:#2"];

