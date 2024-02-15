xmlFilePath[id_] := LocalPath["Data", "OpenReview", id <> ".m"]

(**************************************************************************************************)

PublicSymbol[OpenReviewPaperIDPattern]

DefineStringPattern[OpenReviewPaperIDPattern :> "(?<!\\w)(?:[[:alnum:]-_]{9,11})(?!\\w)"]

(**************************************************************************************************)

PublicFunction[OpenReviewPageToMarkdown]

OpenReviewPageToMarkdown[url_Str, opts___Rule] :=
  PaperToMarkdown[ImportOpenReviewPage[url, FilterOptions @ opts], FilterOptions @ opts];

(**************************************************************************************************)

PublicFunction[ImportOpenReviewPage]

PublicOption[DownloadPDF, PDFPath]

Options[ImportOpenReviewPage] = {
  DownloadPDF -> True,
  PDFPath :> $PDFPath,
  Verbose -> False
}

ImportOpenReviewPage::badurl = "URL `` does not contain a valid id."
ImportOpenReviewPage::baddl = "URL `` could not be imported as XML."
ImportOpenReviewPage::badxml = "URL `` was downloaded as XML that did not contain a title."
ImportOpenReviewPage::badfield = "Could not extract field `` for article at ``."

ImportOpenReviewPage[str_Str, opts:OptionsPattern[]] := Scope[
  UnpackOptions[$verbose];

  id = If[StringMatchQ[str, ArxivPaperIDPattern], str,
    FirstStringCase[str, "id=" ~~ num:OpenReviewPaperIDPattern :> num, ReturnFailed["badurl", str]]
  ];
  VPrint["OpenReview paper ID: ", id];

  xmlPath = xmlFilePath[id];
  url = $pageTemplate[id];
  If[FileExistsQ[xmlPath],
    xml = Get[xmlPath]
  ,
    xml = WithInternet @ Import[url, "XMLObject"];
    If[H[xml] =!= XMLObject["Document"], ReturnFailed["baddl", url]];
    If[!ContainsQ[xml, XMLElement["meta", {"property" -> "og:title", "content" -> _}, _]], ReturnFailed["badxml", url]];
    Export[xmlPath, xml];
  ];

  title = DeepFirstCase[xml, XMLElement["title", {}, {title_Str}] :> StringTrimRight[sanitizeTitle @ postProcessXMLValue[title], " | OpenReview"]];
  If[!StringQ[title], ReturnFailed["badfield", "Title", url]];
  VPrint["Extracted title: ", title];

  pdfURL = DeepFirstCase[xml, XMLElement["meta", {"name" -> "citation_pdf_url", "content" -> url_}, _] :> url];
  If[!StringQ[pdfURL], pdfURL = DeepFirstCase[xml, XMLElement["a", { ___, "href" -> url_, ___, "title" -> "Download PDF", ___}, _] :> url]];
  If[!StringQ[pdfURL], ReturnFailed["badfield", "PDFURL", url]];

  date = DeepFirstCase[xml, XMLElement["span", {"class" -> "date item"}, inner_] :> canonDateItem[inner]];
  If[!StringQ[date], ReturnFailed["badfield", "Date", url]];

  authors = DeepCases[xml, XMLElement["meta", {"name" -> "citation_author", "content" -> author_}, _] :> author];
  VPrint["Extracted authors: ", authors];
  If[Not @ StringVectorQ @ authors, ReturnFailed["badfield", "Authors", url]];
  authors //= sanitizeAuthors;

  xmlFreeFormFields = Association @ DeepCases[xml,
    {XMLElement["strong", {___, "class" -> _String ? (StringContainsQ["note-content-field"]), ___}, {field_}], ___,
     XMLElement["span", {___, "class" -> _String ? (StringContainsQ["note-content-value"]), ___}, {value_}]} :>
    (StringTrim[field, (WhitespaceCharacter | ":")..] -> postProcessXMLValue[value])
  ];
  VPrint["Read free-form fields: ", Keys @ xmlFreeFormFields];

  jsonMetadata = DeepFirstCase[xml, XMLElement["script", {"id" -> "__NEXT_DATA__", "type" -> "application/json"}, {data_String}] :> data];
  event = None;
  If[StringQ[jsonMetadata],
    json = ImportJSONString[jsonMetadata];
    venue = DeepFirstCase[json, a_Association /; KeyExistsQ[a, "venue"] :> a["venue"]];
    event = extractEvent @ venue;
    If[!StringQ[event],
      invitation = DeepFirstCase[json, a_Association /; KeyExistsQ[a, "invitation"] :> a["invitation"]];
      event = extractEvent @ invitation
    ];
    If[StringQ[event], VPrint["Extracted event: ", event],
      VPrint[json]];
  ];

  {abstract, keywords, tldr} = Lookup[xmlFreeFormFields, {"Abstract", "Keywords", "TL;DR"}, None];
  If[StringQ[tldr], abstract = StringJoin["TLDR: ", sanitizeAbstract @ tldr, "\n", abstract]];
  If[!StringQ[abstract], ReturnFailed["badfield", "Abstract", url]];
  If[StringQ[keywords],
    keywords = StringTrim @ StringSplit[keywords, ","];
    fieldTags = extractFieldTags @ ToLowerCase @ keywords;
  ,
    fieldTags = None
  ];

  customMetadata = KeyDrop[xmlFreeFormFields, {"Title", "Abstract"}];
  customMetadata["Keywords"] = keywords;

  data = Association[
    "Title" -> title,
    "Date" -> date,
    "Authors" -> authors,
    "URL" -> url,
    "PDFURL" -> pdfURL,
    "FieldTags" -> fieldTags,
    "Origin"  -> "OpenReview",
    "Abstract" -> abstract,
    "Event" -> event,
    "CustomMetadata" -> customMetadata
  ];

  UnpackOptions[downloadPDF, pDFPath];
  postProcessPaperPageData[data, downloadPDF, pDFPath]
];

(**************************************************************************************************)

extractFieldTags[keywords_List] := Scope[
  text = Join[
    BearNoteData[{"Title" -> StringMatchQ[keywords, IgnoreCase -> True]}, "Text"],
    BearNoteData[{"Text" -> StringContainsQ["#meta/aka " ~~ Alternatives[keywords], IgnoreCase -> True]}, "Text"]
  ];
  fieldTags = Flatten @ StringCases[text, "#field/" ~~ LetterCharacter.. ~~ Repeated[LetterCharacter.. ~~ "/"]..];
  DeleteRedundantTags @ DeleteDuplicates @ fieldTags
];

(**************************************************************************************************)

extractEvent = Case[
  <|"value" -> v_|> := % @ v;
  str_String        := FirstStringCase[str,
    StartOfString ~~ event:RomanLetter.. ~~ (" " | ".cc/") ~~ year:RecentYearPattern :> StringJoin[event, " ", year]
  ];
  _                 := None;
]

(**************************************************************************************************)

$pageTemplate = StringFunction @ "https://openreview.net/forum?id=#1";

$standardFieldRules = {
  "Title"        -> XMLElement["title", {}, {title_Str}] :> StringTrimRight[sanitizeTitle @ postProcessXMLValue[title], " | OpenReview"],
  "PDFURL"       -> XMLElement["meta", {"name" -> "citation_pdf_url", "content" -> url_}, _] :> url,
  "Date"         -> XMLElement["span", {"class" -> "date item"}, {dates_String}] :> canonDateString[dates]
};

(**************************************************************************************************)

postProcessXMLValue[val_] :=
  applyLatexMarkup @ ReplaceAll[val, XMLElement["a", {___, "href" -> urlFragment_, ___}, ___] :> urlFragment];

applyLatexMarkup = Case[
  s_String := StringReplace[s, $latexMarkupRules];
  e_ := e
];

$latexMarkupRules = {
  "``" -> "\"", "''" -> "\"",
  "\\emph{" ~~ Shortest[a___] ~~ "}" :> "_" <> a <> "_",
  "[![" ~~ Shortest[__] ~~ "]" ~~ Maybe["(" ~~ Shortest[__] ~~ ") " ~~ Shortest[__]] ~~ "](" ~~ Shortest[link__] ~~ ")" :> link
};

(**************************************************************************************************)

canonDateItem := Case[
  other_ := % @ StringRiffle[DeepCases[other, _Str]];
  str_Str := FirstStringCase[str,
    (day:Repeated[DigitCharacter, {1,2}] ~~ " " ~~ month:Repeated[RomanLetter, {3, 10}] ~~ " " ~~ year:("20" ~~ Repeated[DigitCharacter, 2])) :>
      year <> "/" <> $monthNameToNumber[StringTake[ToLowerCase @ month, 3]] <> "/" <> StringPadLeft[day, 2, "0"]
  ];
];

$monthNameToNumber = <|"jan" -> "01", "feb" -> "02", "mar" -> "03", "apr" -> "04", "may" -> "05", "jun" -> "06", "jul" -> "07", "aug" -> "08", "sep" -> "09", "oct" -> "10", "nov" -> "11", "dec" -> "12"|>;
