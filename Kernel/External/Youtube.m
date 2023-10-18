jsonFilePath[id_] := LocalPath["Data", "Youtube", id <> ".json"];

(**************************************************************************************************)

PublicSymbol[YoutubeIDPattern]

declareStringPattern[YoutubeIDPattern :> "[a-zA-Z0-9_-]{9,12}"]

(**************************************************************************************************)

PublicFunction[YoutubeVideoMetadata]

General::noytid = "No video ID found in ``.";
YoutubeVideoMetadata::badinfo = "Could not obtain JSON info for video with ID ``."

Options[YoutubeVideoMetadata] = {
  Verbose -> False
};

YoutubeVideoMetadata[url_String, OptionsPattern[]] := Scope[
  UnpackOptions[$verbose];
  yid = getYTID @ url;
  If[!StringQ[yid], ReturnFailed["noytid", url]];
  jsonPath = jsonFilePath @ yid;
  VPrint["Trying to obtain metadata for \"", url, "\"."];
  If[FileExistsQ[jsonPath],
    VPrint["Existing JSON found at ", MsgPath @ jsonPath];
    data = Quiet @ ImportJSON @ jsonPath;
    If[!FailureQ[data], Return @ data];
    If[FileAge[jsonPath] < 60 * 60 * 24, ReturnFailed["badinfo", yid]]; (* retry after 1 day *)
    VPrint["Invalid JSON, retrying."];
  ];
  json = RunToolOutput["youtube-dl", "https://youtube.com/watch?v=" <> yid, "--dump-json", Verbose -> $verbose];
  If[!StringQ[json] || !StringStartsQ[json, "{"],
    ExportUTF8[jsonPath, ""];
    ReturnFailed["badinfo", yid]];
  data = ImportJSONString @ json;
  VPrint["Saving data to ", MsgPath @ jsonPath];
  ExportUTF8[jsonPath, json];
  data
]

getYTID[url_String] := Which[
  StringMatchQ[url, YoutubeIDPattern], url,
  StringContainsQ[url, "watch?v="], FirstStringCase[url, "watch?v=" ~~ id:YoutubeIDPattern :> id],
  StringContainsQ[url, ".be/"],     FirstStringCase[url, ".be/" ~~ id:YoutubeIDPattern :> id],
  StringContainsQ[url, "/live/"],   FirstStringCase[url, "/live/" ~~ id:YoutubeIDPattern :> id],
  True, None
];

(**************************************************************************************************)

PublicFunction[$UploaderToPerson, $UploaderToChannel]

SetInitialValue[$UploaderToPerson, $UploaderToChannel, <||>];

(**************************************************************************************************)

PublicFunction[YoutubeToMarkdown]

YoutubeToMarkdown[url_String] := Scope[
  yid = getYTID @ url;
  If[!StringQ[yid], ReturnFailed["noytid", url]];
  data = YoutubeVideoMetadata @ yid;
  If[!AssociationQ[data], ReturnFailed[]];
  YoutubeToMarkdown @ data
];

YoutubeToMarkdown[data_Association] := Scope[
  If[!KeyExistsQ[data, "channel"], data["channel"] = "anonymous"];
  UnpackAssociation[data, channel:"channel", title:"title", description:"description", url:"webpage_url", date:"upload_date"];
  If[StringMatchQ[date, RegularExpression["20[0-9]{6}"]], date = StringInsert[date, "/", {5, 7}]];
  rawChannel = channel;
  person = Lookup[$UploaderToPerson, channel];
  channel = Lookup[$UploaderToChannel, rawChannel];
  title = StringReplace[title, "/" -> "-"];
  If[!StringQ[person],
    {person2, title2} = ExtractTitleAuthor[title];
    If[StringQ[person2], person = person2; title = title2]];
  blob = StringJoin[rawChannel, "\n", title, "\n", description];
  If[!StringQ[person], person = findPerson1 @ blob];
  If[!StringQ[person] && !StringQ[channel] && PossibleFullNameQ[rawChannel], person = rawChannel];
  If[!StringQ[person], person = findPerson2 @ blob];
  author = Which[
    StringQ[person] && PossibleFullNameQ[person], ExtractLastName @ person,
    StringQ[channel], channel,
    True, rawChannel
  ];
  authorLink = StringJoin @ Which[
    StringQ[person] && StringQ[channel],
      If[person =!= channel, {"by [[", person, "]] on [[", channel, "]]"}, {"by [[", person, "]]"}],
    StringQ[person],
      {"by [[", person, "]]"},
    StringQ[channel],
      {"to [[", channel, "]]"},
    True,
      {"to ", rawChannel}
  ];
  StringJoin[
    "# ", author, ": \"", StringDelete[title, {"[", "]"}], "\"\n",
    "\n",
    "#video uploaded on ", date, " ", authorLink, "\n\n",
    url,
    "\n\n",
    "> ", trimDescription @ description
  ]
]

trimDescription[s_] := StringTrim @ StringReplaceRepeated[s, {"#" -> "", (" "... ~~ "\n\n") -> ". ", ("\n"|"\r") -> " ", "  " -> " ",
  link:HyperlinkPattern :> link,
  ("To try everything Brilliant has to offer" ~~ ___ ~~ " premium subscription." ~~ ("."..))  -> "",
  a:LetterCharacter ~~ m:("/"|"*"|"_") ~~ b:LetterCharacter :> a <> " " <> m <> " " <> b}];

findPerson1[e_] := Block[{},
  StringCases[e,
    ("speaker: "|"joined by"|"lecture by "|"talk by"|"intervews "|"interview with"|"speaks with"|"speak with"|"speaking with "|"chat with"|"chats with"|"chatting with"|"talk by"|"guest speaker "|"guest lecturer ") ~~
      Maybe["dr " | "dr. " | "prof " | "professor "|"researcher "|"author "|"scientist "|"mathematician "] ~~ name:FullNamePhrase :> If[PossibleFullNameQ[name], Return[name, Block]],
    IgnoreCase -> True
  ];
  None;
]

findPerson2[e_] := Block[
  {$authors = Complement[$KnownAuthors, $DeadPeople, {"Peter M Neumann", "St John", "3Blue1Brown"}],
   authorLastNames, trimFullNames},
  Scan[author |-> (
    If[StringContainsQ[e, author, IgnoreCase -> True], Return[author, Block]]
    ),
    $authors
  ];
  authorLastNames = ExtractLastName /@ $authors;
  trimFullNames = StringDelete[e, TitlecaseWord ~~ " " ~~ TitlecaseWord];
  ScanThread[{author, lastName} |-> (
    If[StringLength[lastName] > 3 && Count[authorLastNames, lastName] == 1 && StringContainsQ[trimFullNames, WordBoundary ~~ lastName ~~ WordBoundary, IgnoreCase -> True],
      Return[author, Block]]
    ),
    {$authors, authorLastNames}
  ];
  StringCases[
    StringDelete[e, "music" ~~ ___, IgnoreCase -> True],
    ("by ") ~~ name:FullNamePhrase :> If[PossibleFullNameQ[name], Return[name, Block]]
  ];
  None
];

(* generated by:
$shortNames = StringDelete[#, " "]& /@ $KnownAuthors;
$entities = Entity["Person", #]& /@ $shortNames;
WithInternet[$deathDates = EntityValue[$entities,"DeathDate"]];
$DeadPeople = Pick[$KnownAuthors,$deathDates,_DateObject]
CopyToClipboard@InsertLinebreaks[StringRiffle[$DeadPeople,","],60]
*)

(**************************************************************************************************)

PublicVariable[$DeadPeople]

$DeadPeople = "Arend Heyting,Arthur Schopenhauer,Charles Hermite,
Bernard Riemann, Ludwig Boltzmann,Euclid,Francis Bacon,J Willard Gibbs,
George Boole,Norbert Wiener,Ludwig Wittgenstein,Boris
Podolsky,Nathan Rosen,Niels Bohr,Alonzo Church,Richard
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
John von Neumann,Leonard Euler"

$DeadPeople = StringTrim @ StringSplit[StringReplace[$DeadPeople, "\n" -> " "], ","];
