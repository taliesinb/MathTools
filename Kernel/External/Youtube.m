jsonFilePath[id_] := DataPath["Youtube", id <> ".json"];

(**************************************************************************************************)

PublicStringPattern[YoutubeVideoIDPattern]

DefineStringPattern[YoutubeVideoIDPattern :> "[a-zA-Z0-9_-]{9,12}"]

(**************************************************************************************************)

PublicIOFunction[ImportYoutubeVideoMetadata]

General::noVideoID = "No video ID found in ``.";
ImportYoutubeVideoMetadata::badinfo = "Could not obtain JSON info for video with ID ``."

Options[ImportYoutubeVideoMetadata] = {
  Verbose -> False
};

ImportYoutubeVideoMetadata[url_Str, OptionsPattern[]] := Scope[
  UnpackOptions[$verbose];
  vid = getVideoID @ url;
  If[!StringQ[vid], ReturnFailed["noVideoID", url]];
  jsonPath = jsonFilePath @ vid;
  VPrint["Trying to obtain metadata for \"", url, "\"."];
  If[FileExistsQ[jsonPath],
    VPrint["Existing JSON found at ", MsgPath @ jsonPath];
    data = Quiet @ ImportJSON @ jsonPath;
    If[!FailureQ[data], Return @ data];
    If[FileAge[jsonPath] < 60 * 60 * 24, ReturnFailed["badinfo", vid]]; (* retry after 1 day *)
    VPrint["Invalid JSON, retrying."];
  ];
  json = RunToolOutput["youtube-dl", "https://youtube.com/watch?v=" <> vid, "--dump-json", Verbose -> $verbose];
  If[!StringQ[json] || !StringStartsQ[json, "{"],
    ExportUTF8[jsonPath, ""];
    ReturnFailed["badinfo", vid]];
  data = ImportJSONString @ json;
  VPrint["Saving data to ", MsgPath @ jsonPath];
  ExportUTF8[jsonPath, json];
  data
]

getVideoID[url_Str] := Which[
  StringMatchQ[url, YoutubeVideoIDPattern], url,
  StringContainsQ[url, "watch?v="], FirstStringCase[url, "watch?v=" ~~ id:YoutubeVideoIDPattern :> id],
  StringContainsQ[url, ".be/"],     FirstStringCase[url, ".be/" ~~ id:YoutubeVideoIDPattern :> id],
  StringContainsQ[url, "/live/"],   FirstStringCase[url, "/live/" ~~ id:YoutubeVideoIDPattern :> id],
  True, None
];

(**************************************************************************************************)

PublicStringPattern[YoutubePlaylistIDPattern]

DefineStringPattern[YoutubePlaylistIDPattern :> "[a-zA-Z0-9_-]{34}"]

(**************************************************************************************************)

PublicIOFunction[ImportYoutubePlaylistMetadata]

General::noPlaylistID = "No playlist ID found in ``.";
ImportYoutubePlaylistMetadata::badinfo = "Could not obtain JSON info for playlist with ID ``.";x

Options[ImportYoutubePlaylistMetadata] = {
  Verbose -> False
};

ImportYoutubePlaylistMetadata[url_Str, OptionsPattern[]] := Scope[
  UnpackOptions[$verbose];
  pid = getPlaylistID @ url;
  If[!StringQ[pid], ReturnFailed["noPlaylistID", url]];
  jsonPath = jsonFilePath @ pid;
  VPrint["Trying to obtain metadata for \"", url, "\"."];
  If[FileExistsQ[jsonPath],
    VPrint["Existing JSON found at ", MsgPath @ jsonPath];
    data = Quiet @ ImportJSON @ jsonPath;
    If[!FailureQ[data], Return @ data];
    If[FileAge[jsonPath] < 60 * 60 * 24, ReturnFailed["badinfo", pid]]; (* retry after 1 day *)
    VPrint["Invalid JSON, retrying."];
  ];
  json = RunToolOutput["youtube-dl", "--flat-playlist", "-J", pid, Verbose -> $verbose];
  If[!StringQ[json] || !StringStartsQ[json, "{"],
    ExportUTF8[jsonPath, ""];
    ReturnFailed["badinfo", pid]];
  data = ImportJSONString @ json;
  If[!AssociationQ[data],
    VPrint["Did not receive valid JSON object."];
    VPrint[data];
    ReturnFailed["badinfo", url];
  ];
  VPrint["Saving data to ", MsgPath @ jsonPath];
  ExportUTF8[jsonPath, json];
  data
];

getPlaylistID[url_Str] := Which[
  StringMatchQ[url, YoutubePlaylistIDPattern], url,
  StringContainsQ[url, "list="], FirstStringCase[url, "list=" ~~ id:YoutubePlaylistIDPattern :> id],
  True, None
];

(**************************************************************************************************)

PublicIOFunction[ImportYoutubeVideoToMarkdown]

Options[ImportYoutubeVideoToMarkdown] = {
  Verbose -> False
};

ImportYoutubeVideoToMarkdown[url_Str, opts:OptionsPattern[]] := Scope[
  vid = getVideoID @ url;
  If[!StringQ[vid], ReturnFailed["noVideoID", url]];
  data = ImportYoutubeVideoMetadata[vid, opts];
  If[!AssocQ[data], ReturnFailed[]];
  ImportYoutubeVideoToMarkdown[data, opts]
];

ImportYoutubeVideoToMarkdown[data_Assoc, OptionsPattern[]] := Scope[
  UnpackOptions[$verbose];
  If[!KeyExistsQ[data, "channel"], data["channel"] = data["uploader"]];
  UnpackAssociation[data, uploader:"channel", title:"title", description:"description", url:"webpage_url", date:"upload_date"];
  If[StringMatchQ[date, RegularExpression["20[0-9]{6}"]], date = StringInsert[date, "/", {5, 7}]];

  VPrint["Creating markdown for ", MsgExpr @ title, " uploaded by ", MsgExpr @ uploader];

  title //= canonicalizeText;
  description //= canonicalizeText;
  matches = resolveYoutubeUploader[uploader];
  {matches, title} = customUploaderProcessing[matches, title];
  title = StringReplace[title, {"/" -> "-", " :" -> ":"}];

  title //= trimTitle;
  {matches, title, type1} = resolvePerson[matches, title, description];
  {authorBlurb, title, type2} = resolveAuthorBlurb[matches, title, description];

  description //= trimDescription;
  title //= trimTitle;
  title = trimAuthorFromTitle[title, matches["Person"], matches["Author"]];

  description = StringTrimLeft[description, Maybe["\""] ~~ title ~~ Maybe["\""] ~~ Maybe["." | "\n"]..];
  tag = "#video" <> ReplaceNone[ReplaceNone[type1, type2], ""];

  UnpackAssociation[matches, author];
  $youtubeVideoTemplate @ PackAssociation[
    author, title, tag, date, authorBlurb, url, description, uploader, matches
  ]
];

$youtubeVideoTemplate = StringFunction @ StringTrim @ """
# #Author: "#Title"

#Tag #AuthorBlurb

##meta/uploaded on #Date to "#Uploader"

#Url

> #Description
""";

(**************************************************************************************************)

PublicIOFunction[YoutubeVideoToStructuredData]

Options[YoutubeVideoToStructuredData] = Options[ImportYoutubeVideoToMarkdown];

YoutubeVideoToStructuredData[url_Str, opts:OptionsPattern[]] := Scope[
  $youtubeVideoTemplate = Identity;
  ImportYoutubeVideoToMarkdown[url, opts]
]

(**************************************************************************************************)

PublicIOFunction[YoutubeBearNoteToStructuredData]

YoutubeBearNoteToStructuredData[title_Str] := Scope[
  text = BearNoteText[title];
  If[!StringQ[text], ReturnFailed[]];
  lines = StringSplit[text, "\n"..];
  {title, lines} = FirstRest @ lines;
  If[!StringMatchQ[title, "# " ~~ ___ ~~ ": \"" ~~ ___ ~~ "\""], ReturnFailed[]];
  {author, title} = StringSplit[StringDrop[title, 2], ": ", 2];
  title = StringTrim[title, "\""];
  $fields = Assoc[
    "Author" -> author, "Title" -> title, "Date" -> None, "AuthorBlurb" -> None, "URL" -> None, "Description" -> None, "Uploader" -> None,
    "Matches" -> <|"Person" -> None, "Institute" -> None, "Event" -> None, "Podcast" -> None, "Uploader" -> None|>,
    "Notes" -> {}, "Fields" -> {}
  ];
  Scan[parseYTBNoteLine, lines];
  parseYTBNoteBlurb @ $fields["AuthorBlurb"];
  If[$fields["Type"] === "", $fields["Type"] = None];
  $fields
];

$hierarchicalTagSuffix = ("/" ~~ LetterCharacter..)... ~~ WordBoundary;

parseYTBNoteLine = StringCase[
  {"#video", type:$hierarchicalTagSuffix, " uploaded on ", date:(DigitCharacter | "/").., " ", blurb___} :=
    AssociateTo[$fields, {"Type" -> type, "Date" -> date, "AuthorBlurb" -> blurb}];
  {"#video", type:$hierarchicalTagSuffix, " ", blurb___} :=
    AssociateTo[$fields, {"Type" -> type, "AuthorBlurb" -> blurb}];
  {"#meta/uploaded on ", date:(DigitCharacter | "/").., " to \"", uploader__, "\""} :=
    AssociateTo[$fields, "Date" -> date, "Uploader" -> uploader];
  {"#meta/uploaded on ", date:(DigitCharacter | "/")..} :=
    AssociateTo[$fields, "Date" -> date];
  url:{("https://www.youtube.com" | "https://youtu.be/") ~~ ___} :=
    AssociateTo[$fields, "URL" -> url];
  {"> " ~~ desc___} :=
    If[$fields["Description"] === None,
      AssociateTo[$fields, "Description" -> desc],
      KeyAppendTo[$fields, "Notes", desc]
    ];
  "- - -" :=
    Null;
  Whitespace :=
    Null;
  note___ :=
    KeyAppendTo[$fields, "Notes", note]
];

parseYTBNoteBlurb[blurb_Str] := StringCases[blurb, {
  link:MarkdownNoteLinkPattern               :> parseYTBLink[StringTake[link, {3, -3}]],
  date:NumericDatePattern                    :> If[$fields["Date"] === None, AssociateTo[$fields, "Date" -> date]],
  field:("#field" ~~ $hierarchicalTagSuffix) :> KeyUnionTo[$fields, "Fields", List @ field],
  org:("#org"     ~~ $hierarchicalTagSuffix) :> resolveYTBNoteMatch[org, "Institute"],
  org:("#event"   ~~ $hierarchicalTagSuffix) :> resolveYTBNoteMatch[org, "Event"]
}];

resolveYTBNoteMatch[tag_, field_] := Scope[
  master = BearNoteData["Tag" -> tag, "Title"];
  If[Length[master] === 1, $fields["Matches", field] = First @ master];
];

parseYTBLink[title_] := Scope[
  text = BearNoteText[title];
  If[!StringQ[text],
    If[StringEndsQ[title, " " ~~ RecentYearPattern], $fields["Matches", "Event"] = title];
    Return[Null]
  ];
  MapApply[
    If[StringContainsQ[text, StartOfLine ~~ #1], $fields["Matches", #2] = title; Return[Null, Block]]&,
    {"#person"          -> "Person",
     "#meta/instance"   -> "Event",
     "#org/institute"   -> "Institute",
     "#org/lab"         -> "Institute",
     "#event"           -> "Event",
     "#channel/podcast" -> "Podcast",
     "#channel/video"   -> "Channel"}
  ];
];

(**************************************************************************************************)

canonicalizeText[text_] := StringReplace[text,
  {"\[OpenCurlyQuote]" -> "'", "\[CloseCurlyQuote]" -> "'",
   "\[OpenCurlyDoubleQuote]" -> "\"", "\[CloseCurlyDoubleQuote]" -> "\""}];

(**************************************************************************************************)

PublicVariable[$UploaderToPerson, $UploaderToChannel, $UploaderToInstitute, $UploaderToPodcast]

$knownLiveAuthors := $knownLiveAuthors =
  Complement[$KnownAuthors, $DeadPeople, {"Peter M Neumann", "St John", "3Blue1Brown"}];

lowerAssoc[list_] := UAssoc @ Map[ToLowerCase[#] -> #&, list];

$reuploaders = "Samuel Mimram";
$UploaderToPerson    := $UploaderToPerson    = computeUploaderToPerson[];
$UploaderToChannel   := $UploaderToChannel   = handleToTitle["#channel/video"];
$UploaderToPodcast   := $UploaderToPodcast   = handleToTitle["#channel/podcast"];
$UploaderToInstitute := $UploaderToInstitute = Join[handleToTitle["#org/institute"], handleToTitle["#org/lab"]];
$UploaderToEvent     := $UploaderToEvent     = handleToTitle["#event"];
$eventNamesToTitle   := $eventNamesToTitle   = akaToTitle["#event"];

computeUploaderToPerson[] := DeleteCases[$reuploaders] @ Join[lowerAssoc @ $knownLiveAuthors, handleToTitle["#person"]];

handleToTitle[tag_] := UAssoc[procHandleTitle /@ BearNoteData[{"Tag" -> tag}, {"Title", "Text"}]];

procHandleTitle[assoc_] := Scope[
  UnpackAssociation[assoc, title, text];
  handle = title;
  FirstStringCase[text, "#handle/youtube is " ~~ h:DoubleQuotedPhrase :> (handle = h)];
  ToLowerCase[StringTrim[handle, "\""]] -> title
];

eventToType[title_] := Scope[
  text = BearNoteText @ title;
  If[!StringQ[text],
    If[StringEndsQ[title, " " ~~ RecentYearPattern],
      Return @ eventToType @ StringTrimRight[title, " " ~~ RecentYearPattern],
      Return @ "event"
    ];
  ];
  If[StringContainsQ[text, "#meta/instance"],
    Return @ eventToType @ FirstStringCase[text, "#meta/instance of [[" ~~ link___ ~~ "]]" :> eventToType[link]]];
  FirstStringCase[text, "#event/" ~~ t:WordCharacter.. :> t, "event"]
];

(**************************************************************************************************)

bearNoteFields[title_] := bearNoteFields[title] = iBearNoteFields @ title;

iBearNoteFields[title_] := Scope[
  text = BearNoteText @ title;
  If[!StringQ[text],
    If[StringEndsQ[title, " " ~~ RecentYearPattern],
      Return @ bearNoteFields @ StringTrimRight[title, " " ~~ RecentYearPattern],
      Return @ {}
    ]
  ];
  StringTrim @ StringCases[text, field:("#field/" ~~ (LetterCharacter | "/")..)]
];

(**************************************************************************************************)

akaToTitle[tag_] := UAssoc[procAkaTitle /@ BearNoteData[{"Tag" -> tag}, {"Title", "Text"}]];

$eventTitleSuffix = CaseInsensitive[" conference" | " summer school" | " winter school"];

procAkaTitle[assoc_] := Scope[
  UnpackAssociation[assoc, title, text];
  If[StringContainsQ[text, "#meta/ontology"], Return @ {}];
  aka = StringCases[text, "#meta/aka " ~~ h:DoubleQuotedPhrase :> StringDelete[StringTrim[h, "\""], "\\"]];
  If[StringEndsQ[title, $eventTitleSuffix], AppendTo[aka, StringTrimRight[title, $eventTitleSuffix]]];
  Splice @ Flatten @ {
    title -> title,
    # -> title& /@ aka
  }
];

(**************************************************************************************************)

resolveYoutubeUploader[uploader_] := Scope[
  uploaderLower = ToLowerCase @ uploader;
  person    = Lookup[$UploaderToPerson,    uploaderLower, None];
  channel   = Lookup[$UploaderToChannel,   uploaderLower, None];
  institute = Lookup[$UploaderToInstitute, uploaderLower, None];
  podcast   = Lookup[$UploaderToPodcast,   uploaderLower, None];
  event     = Lookup[$UploaderToEvent,     uploaderLower, None];
  matches = PackAssociation[person, channel, institute, podcast, event];
  If[Count[matches, _Str] > 0, VPrint["Uploader matches: ", DeleteNone @ matches]];
  matches["Uploader"] = uploader;
  matches
];

(**************************************************************************************************)

customUploaderProcessing[matches_, title_] := Scope[
  If[StringStartsQ[title, "Mindscape " ~~ DigitCharacter..~~ " | "],
    matches["Podcast"] = "Mindscape";
    matches["Person"]  = None;
    title //= StringTrimLeft["Mindscape " ~~ DigitCharacter.. ~~ " | "];
  ];
  If[StringEndsQ[title, " | The Cartesian Cafe with Timothy Nguyen"],
    matches["Podcast"] = "The Cartesian Cafe";
    matches["Person"]  = None;
    title //= StringTrimRight[" | The Cartesian Cafe with Timothy Nguyen"];
  ];
  If[StringEndsQ[title, " [UNPLUGGED]"],
    title //= StringTrimLeft[Except["-"].. ~~ " - "];
    title //= StringTrimRight[" [UNPLUGGED]"];
  ];
  If[StringEndsQ[title, " - Computerphile"],
    matches["Channel"] = "Computerphile";
    matches["Person"]  = None;
    title //= StringTrimRight[" - Computerphile"];
  ];
  {matches, title}
];

(**************************************************************************************************)

resolvePerson[matches_, title_, description_] := Scope[

  matches = matches;
  UnpackAssociation[matches, person, channel, uploader, event];

  (* this might overwrite an event extracted from the channel; that's ok *)
  Which[
    StringQ[rawEvent = FirstStringCase[title, $eventNameDateP]],
      title = trimTitle @ StringDelete[title, Maybe["@ " | "at " | "at the "] ~~ rawEvent ~~ Repeated[".", {0, 3}]];
      event = canonicalizeEvent @ rawEvent;
      VPrint["Recognized event in title: ", MsgExpr @ event];
    ,
    (* we have a higher bar for finding events in the description, since they have more opportunity for false positives *)
    StringQ[eventChunk = FirstStringCase[description, $introducedEventPattern]] &&
    StringQ[rawEvent = FirstStringCase[eventChunk, $eventNameDateP]],
      event = canonicalizeEvent @ rawEvent;
      VPrint["Recognized event in description: ", MsgExpr @ event],
    True,
      Null
  ];

  If[!StringQ[person],
    VPrint["Attempting extracting of person from title."];
    {person2, title2} = ExtractTitleAuthor[title, description];
    If[StringQ[person2],
      VPrint["Found person in title: ", MsgExpr @ person2];
      person = person2; title = title2];
  ];

  blob = StringJoin[uploader, "\n", title, "\n", description];
  If[!StringQ[person],
    VPrint["Attempting extracting of person from description."];
    person = findIntroducedPerson @ blob;
    If[StringQ[person], VPrint["Found person in description: ", MsgExpr @ person]];
  ];

  If[!StringQ[person] && !StringQ[channel] && PossibleFullNameQ[uploader],
    VPrint["Obtaining person from uploader: ", uploader];
    person = uploader;
  ];

  If[!StringQ[person],
    VPrint["Attempting fallback extraction of person from description."];
    person = findPersonSpeculative @ blob;
    If[StringQ[person], VPrint["Found person in description: ", MsgExpr @ person]];
  ];

  (* author will form the prefix of the title, so shouldn't be an institute.
     we fall back to the uploader or video channel as the prefix *)
  author = Which[
    StringQ[person] && PossibleFullNameQ[person], ExtractLastName @ person,
    StringQ[channel],                             channel,
    True,                                         uploader
  ];
  VPrint["Using author name: ", MsgExpr @ author];

  (* remove redundancy from the title *)
  title = trimAuthorFromTitle[title, person, author];

  type = Which[
    StringContainsQ[blob, "tutorial" | "Tutorial"], "/tutorial",
    StringContainsQ[blob, "lecture" | "Lecture"],   "/lecture",
    StringQ[event],                                 "/talk",
    True,                                           None
  ];

  matches["Person"] = person;
  matches["Author"] = author;
  matches["Event"] = event;

  {matches, title, type}
];

(**************************************************************************************************)

trimAuthorFromTitle[title_, person_, author_] := Scope[
  paPatt = Select[Alternatives[person, author], StringQ];
  If[Length[paPatt] > 0,
    If[StringStartsQ[title, paPatt],
      title = StringTrim @ StringTrimLeft[title, paPatt ~~ WhitespaceCharacter... ~~ Maybe["," | "-" | "--" | ":"]];
      title //= trimTitle;
      VPrint["Trimming person/author from start of title: ", MsgExpr @ title];
    ];
    If[StringEndsQ[title, paPatt],
      title = StringTrim @ StringTrimRight[title, "," | "-" | "--" | ":" ~~ WhitespaceCharacter... ~~ paPatt];
      title //= trimTitle;
      VPrint["Trimming person/author from start of title: ", MsgExpr @ title];
    ];
  ];
  title
];

(**************************************************************************************************)

(* this looks for contextual strings that indicate someone is the primary person in the video *)
findIntroducedPerson[str2_] := Block[{str},
  str = StringDelete[str2, $personPrefixes, IgnoreCase -> True];
  StringCases[str,
    $introductionPrefixes ~~ name:FullNamePhrase :>
      If[PossibleFullNameQ[name], Return[name, Block]],
    IgnoreCase -> True
  ];
  StringCases[str,
    name:FullNamePhrase ~~ $introductionSuffixes :>
      If[PossibleFullNameQ[name], Return[name, Block]],
    IgnoreCase -> True
  ];
  StringCases[str,
    name:FullNamePhrase ~~ (", " | " (" | " at the ") ~~ "University" | "College" | "Institute" :>
      If[PossibleFullNameQ[name], Return[name, Block]]
  ];
  None
];

$honorifics = Alternatives @@ StringSplit["sir lord professor prof. prof doctor dr. dr"];
$jobQualifiers = Alternatives @@ StringSplit["researcher author scientist mathematician physicist historian philosopher"];
$personPrefixes = Alternatives[$honorifics, $jobQualifiers] ~~ " ";

$introductionPrefixes = RegularExpression @ ToRegularExpression[Alternatives[
  "speaker:", "lecturer:", "joined by", "presented by", "lecture by", "tutorial by", "talk by",
 "intervews", "interview with", "speaks with", "speak with", "speaking with", "chat with", "conversation with", "conversation between",
 "chats with", "chatting with", "talk by", "guest speaker", "guest lecturer"
] ~~ " "];

$introductionSuffixes = RegularExpression @ ToRegularExpression[" " ~~ Alternatives[
  "delivers", "speaks on", "introduces", "lectures on", "lectures about ", "discusses", "debates", "explains"
]];

(**************************************************************************************************)

(* this function is more speculative *)
findPersonSpeculative[str_] := Block[
  {trimmedStr},

  (* if there is a match to a full name, bingo *)
  Scan[person |-> (
    If[StringContainsQ[str, person, IgnoreCase -> True], Return[person, Block]]
    ),
    First @ $knownSafeLiveAuthors
  ];

  (* find last names that aren't part of a longer full name that we didn't recognize above *)
  trimmedStr = StringDelete[str, TitleCaseWord ~~ " " ~~ TitleCaseWord ~~ Maybe["-" ~~ TitleCaseWord]];
  ScanThread[{fullName, lastName} |->
    If[StringContainsQ[trimmedStr, WordBoundary ~~ lastName ~~ WordBoundary, IgnoreCase -> True],
      Return[fullName, Block]],
    Rest @ $knownSafeLiveAuthors
  ];

  (* find english names we didn't recognize in the known authors list *)
  StringCases[
    StringDelete[str, "music" ~~ ___, IgnoreCase -> True],
    ("by ") ~~ name:FullNamePhrase :> If[PossibleFullNameQ[name], Return[name, Block]]
  ];

  None
];

$knownSafeLiveAuthors := $knownSafeLiveAuthors = uniqueProperNamesAndLastNames[$knownLiveAuthors];

(* this is a 3-tuple, first is the list of full names, second is a list of authors that are matched
to the third which is a list of last names. the latter two are filtered so that last names are not
english non-proper words (which would yield false positives) and to not be ambigious based on last name. *)
uniqueProperNamesAndLastNames[fullNames_] := Scope[
  fullNames = Select[fullNames, StringContainsQ[" "]];
  pairs = {#, ExtractLastName @ #}& /@ fullNames;
  pairs = Select[pairs, P2 /* lastNameSuitableForMatchingQ];
  pairs = Catenate @ Select[Length[#] === 1&] @ GroupBy[pairs, Last];
  Prepend[fullNames] @ Transpose @ pairs
];

lastNameSuitableForMatchingQ[last_] :=
  StringLength[last] > 3 && !LowercaseEnglishWordQ[ToLowerCase @ last];

(**************************************************************************************************)

resolveAuthorBlurb[matches_, title_, description_] := Scope[

  UnpackAssociation[matches, person, channel, uploader, event, institute, podcast];

  (* this doesn't use the author anywhere, weirdly *)

  authorBlurb = StringJoin @ Which[
    StringQ[person] && StringQ[channel] && person =!= channel,
      {"by [[", person, "]] on [[", channel, "]]"},
    StringQ[person],
      {"by [[", person, "]]"},
    StringQ[channel] && channel =!= event && channel =!= institute,
      {"on [[", channel, "]]"},
    True,
      ""
  ];
  VPrint["Final author blurb: ", MsgExpr @ authorBlurb];

  type = None;
  If[StringQ[event],
    title = StringTrim @ StringDelete["[]"|"()"] @ StringDelete[title, event ~~ Repeated[".", {0, 3}]];
    eventType = eventToType @ event;
    authorBlurb = StringJoin[authorBlurb, " from ", eventType, " [[", event, "]]"];
    VPrint["Recognized matched event in title: ", MsgExpr @ event];
    type = "/talk";
  ];

  If[StringQ[institute],
    authorBlurb = StringJoin[authorBlurb, " at [[", institute, "]]"];
    type = "/talk";
  ];
  If[StringQ[podcast],
    authorBlurb = StringReplace[authorBlurb, "by [[" -> "on [[" <> podcast <> "]] with [["];
    type = "/interview";
  ];
  If[!StringQ[type] && StringContainsQ[title, "panel", IgnoreCase -> True],
    type = "/panel";
  ];

  links = StringCases[authorBlurb, "[[" ~~ text:Shortest[___] ~~ "]]" :> text];
  fields = DeleteRedundantTags @ Flatten[bearNoteFields /@ links];
  If[fields =!= {}, authorBlurb = authorBlurb <> " in " <> StringRiffle[fields, ", "]];

  authorBlurb = StringTrim @ StringReplaceRepeated[authorBlurb, "  " -> " "];
  {authorBlurb, title, type}
]

(**************************************************************************************************)

$eventNameP := $eventNameP = Alternatives @@ Keys @ $eventNamesToTitle;

$eventNameDateP := $eventNameDateP = conf:$eventNameP ~~ Maybe[" " | " '" | "'"] ~~ year:DigitCharacter..;

(**************************************************************************************************)

$eventRuleP := $eventRuleP = RuleDelayed[
  $eventNameDateP,
  StringJoin[Lookup[$eventNamesToTitle, conf, event], " ", canonicalizeYear @ year]
];

canonicalizeEvent[rawEvent_] := FirstStringCase[rawEvent, $eventRuleP];

canonicalizeYear = Case[
  s_String /; StringMatchQ[s, ("0"|"1"|"2") ~~ DigitCharacter] := "20" <> s;
  other_ := other;
]

(**************************************************************************************************)

$eventPrefix = CaseInsensitive["@" | "keynote" | "keynote talk" | "talk" | "presentation" | "presented"] ~~ " at " ~~ Maybe["the "];
$eventSuffix = CaseInsensitive[" conference." | " conference"] | " " | "." | "";

computeIntroducedEventPattern[] := RegularExpression @ ToRegularExpression @ StringExpression[
  Maybe[$eventPrefix],
  $eventNameDateP /. Verbatim[Pattern][_, rhs_] :> rhs,
  $eventSuffix
];

$introducedEventPattern := $introducedEventPattern = computeIntroducedEventPattern[];

(**************************************************************************************************)

trimTitle[title_] := Scope[
  title = StringTrimLeft[title, "#" ~~ DigitCharacter..];
  title = StringReplace[title, Repeated[" ", {2, Infinity}] -> " "];
  title = StringTrim @ StringDelete[title, $titleFluff, IgnoreCase -> True];
  title = StringTrim[title, (" " | "|" | "." | "-" | "\[Dash]")..];
  title = StringReplace[title, " " ~~ Repeated["-", {2, Infinity}] ~~ " " -> " - "];
  title
];

$titleFluff = "()" | "[]" | "[" | "]" | "(full)" | "(video)" | "(playlist)" | "full movie" | "full video";

(**************************************************************************************************)

trimDescription[s_] := StringTrim @ StringTrimLeft["."] @ StringReplaceRepeated[s, {
  "#" -> "", (" "... ~~ "\n\n") -> ". ", ("\n"|"\r") -> " ", "  " -> " ",
  link:HyperlinkPattern :> link,
  ("Go to https://curiositystream.com" ~~ __ ~~ "annual subscription.") -> "",
  ("To try everything Brilliant has to offer" ~~ ___ ~~ " premium subscription." ~~ ("."..))  -> "",
  a:LetterCharacter ~~ m:("/"|"*"|"_") ~~ b:LetterCharacter :> a <> " " <> m <> " " <> b}];

(**************************************************************************************************)

PublicVariable[$DeadPeople]

(* these people can't be youtube authors!
generated by:
$shortNames = StringDelete[#, " "]& /@ $KnownAuthors;
$entities = Entity["Person", #]& /@ $shortNames;
WithInternet[$deathDates = EntityValue[$entities,"DeathDate"]];
$DeadPeople = Pick[$KnownAuthors,$deathDates,_DateObject]
CopyToClipboard@InsertLinebreaks[StringRiffle[$DeadPeople,","],60]
*)

$DeadPeople = "Arend Heyting,Arthur Schopenhauer,Arthur Cayley,Charles Hermite,
Bernard Riemann, Ludwig Boltzmann,Euclid,Francis Bacon,J Willard Gibbs,
George Boole,Norbert Wiener,Ludwig Wittgenstein,Boris
Podolsky,Nathan Rosen,Niels Bohr,Alonzo Church,Elwin Christoffel,Richard
Dedekind,Paul Taylor,Carl Friedrich Gauss,B F Skinner,Ivan
Pavlov,Samuel Eilenberg,Melvil Dewey,Aristid
Lindenmayer,Carl Sagan,Emil Artin,Julian Schwinger,Luigi
Bianchi,Sophus Lie,Frigyes Riesz,Eugene Wigner,Claude
Chevalley,Wolfgang Krull,Serge Lang,Mark Kac,Charles
Ehresmann,Issai Schur,Nathan Jacobson,Jean Leray,Wolfgang
Pauli,Alan Turing,Ernst Witt,Edsger Dijkstra,L E J
Brouwer,Gerhard Gentzen,Alfred North Whitehead,Richard
Harris,Jacques Tits,David Hilbert,Hermann Minkowski,Paul
Dirac,Freeman Dyson,Richard Feynman,Heinz Hopf,Leopold
Kronecker,Ed Nelson,Camille Jordan,Andre Weil,Saunders
MacLane,Claude Shannon,Alexander Grothendieck,Albert
Einstein,Emmy Noether,Saul Kripke,Kazimierz Kuratowski,Felix
Hausdorff,Marvin Minsky,Henri Bergson,Ernst Mach,Felix Klein,
John von Neumann,Leonard Euler,Murray Gell-Mann"

$DeadPeople = StringTrim @ StringSplit[StringReplace[$DeadPeople, "\n" -> " "], ","];

(**************************************************************************************************)

PrivateIOFunction[ImportYoutubePlaylistToMarkdown]

Options[ImportYoutubePlaylistToMarkdown] = {
  DryRun -> False,
  DuplicateTarget -> False,
  Verbose -> False
};

ImportYoutubePlaylistToMarkdown::nometadata = "Failed to obtain metadata for playlist ``.";
ImportYoutubePlaylistToMarkdown::noEntryMetadata = "Failed to obtain metadata for all entries in playlist ``.";

ImportYoutubePlaylistToMarkdown[url_Str, opts:OptionsPattern[]] := Scope[
  UnpackOptions[$verbose, $dryRun, duplicateTarget];
  SetAutomatic[$verbose, $dryRun];

  VPrint["Generating markdown for Youtube playlist ", MsgExpr @ url];

  metadata = ImportYoutubePlaylistMetadata[url, Verbose -> $verbose];
  If[!AssociationQ[metadata], ReturnFailed["nometadata", MsgExpr @ url]];
  UnpackAssociation[metadata, uploader:"uploader", title:"title", url:"webpage_url", entries:"entries"];

  VPrint["Playlist title ", MsgExpr @ title, " has ", Length @ entries, " videos"];
  VPrint["Obtaining individual video metadata"];
  videoMarkdowns = Map[ImportYoutubeVideoToMarkdown[#id, Verbose -> $verbose]&, entries];
  If[!StringVectorQ[videoMarkdowns],
    ReturnFailed["nometadata", MsgExpr @ url];
  ];

  VPrint["Creating individual video pages"];
  links = Map[CreateBearNote[#, DryRun -> True, opts]&, videoMarkdowns];
  entries = StringRiffle["* " <> #& /@ links, "\n"];

  title //= trimTitle;
  tag = "#playlist/video";

  authorBlurb =

  $youtubePlaylistTemplate @ PackAssociation[
    author, title, tag, authorBlurb, url, entries
  ]
];

$youtubePlaylistTemplate = StringFunction @ StringTrim @ """
# #Author: "#Title"

#Tag by #AuthorBlurb

#Url

### Table of videos

#Entries
"""

(**************************************************************************************************)

PrivateFunction[ExtractTitleAuthor]

ExtractTitleAuthor[title_Str, desc_:None] := Scope[
  person = None;
  title //= StringDelete[NumericDatePattern | SpelledDatePattern];
  title //= StringDelete[" ()"];
  title = StringTrim[title, Repeated[LetterClass[" ,:-"]]];
  (* so it doesn't show up as part of a full name phrase *)
  title2 = StringReplace[title, "Interview" -> "interview"];
  Scan[FirstStringCase[title2, #]&, {

    (* special case for Topos institute *)
    StartOfString ~~ "Berkeley Seminar: " ~~ Shortest[p__] ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = StringExtract[desc, "\n" -> 1]; Goto[Done]],

    (* EPIT Spring School on HoTT: Paige North (Directed Homotopy Type Theory) *)
    StartOfString ~~ event:(___ ~~ "School" ~~ ___) ~~ ": " ~~ Shortest[p__] ~~ " (" ~~ t___ ~~ ")" ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = event <> " - " <> t; Goto[Done]],

    (* Group Theory (Pierre Cagne) *)
    StartOfString ~~ t___ ~~ "(" ~~ Shortest[p___] ~~ ")" ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = t; Goto[Done]],

    (* Pierre Cagne: Group Theory *)
    StartOfString ~~ Shortest[p___] ~~ $authorTitleSeparator ~~ t___ ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = t; Goto[Done]],

    (* Group Theory - Pierre Cagne *)
    StartOfString ~~ t___ ~~ (" - " | " \[Dash] ") ~~ p___ ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = t; Goto[Done]],

    (* Group Theory - Pierre Cagne - Random other stuff *)
    StartOfString ~~ t___ ~~ (" - " | " \[Dash] ") ~~ p___ ~~ (" - " | " \[Dash] ") ~~ o___ ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = t <> " - " <> o; Goto[Done]],

    (* Pierre Cagne "Group Theory" *)
    StartOfString ~~ p___ ~~ " \"" ~~ t___ ~~ "\"" ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = t; Goto[Done]],

    (* "Group Theory" by Pierre Cagne *)
    StartOfString ~~ "\"" ~~ t___ ~~ "\" by " ~~ p__ ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = t; Goto[Done]],

    (* Group Theory Pierre Cagne (Foo University) *)
    StartOfString ~~ t___ ~~ p:(TitleCaseWord ~~ " " ~~ TitleCaseWord) ~~ " (" ~~ u___ ~~ ")" ~~ EndOfString :>
      If[StringContainsQ[u, "university" | "institute", IgnoreCase -> True] && PossibleFullNameQ[p], person = p; title = t; Goto[Done]],

    (* Group Theory - Pierre Cagne - Applications *)
    StartOfString ~~ t1___ ~~ " - " ~~ p:FullNamePhrase ~~ " - " ~~ t2___ ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = StringJoin[t1, " - ", t2]; Goto[Done]],

    (* Pierre Cagne on Group Theory *)
    (* Richard Borcherds (Fields Medalist) on the Monster Group, String Theory, Self Studying and Moonshine *)
    StartOfString ~~ p:FullNamePhrase ~~ Maybe[" (" ~~ __ ~~ ")"] ~~ " on " ~~ t___ ~~ EndOfString :>
      If[PossibleFullNameQ[p], person = p; title = ToTitleCase[t]; Goto[Done]]

  }];
  Return @ {None, None};
  Label[Done];
  person = StringTrim @ StringDelete[person, StartOfString ~~ $honorifics ~~ " ", IgnoreCase -> True];
  title = trimTitle @ title;
  title = StringTrim @ StringTrim[title, "\""];
  {person, title}
]

$authorTitleSeparator = (": " | " | " | ", " | "; " | " - " | " -- " | " --- ");
