PublicFunction[SaveBearData, LoadBearData]

toBearDataPath[name_] := (
  EnsureDirectory[LocalPath["Data", "Bear"]];
  LocalPath["Data", "Bear", name <> ".mx"]
);

SaveBearData[name_Str] := ExportMX[toBearDataPath[name], BearNoteData[All, All]];

LoadBearData[name_Str] := ImportMX @ toBearDataPath[name];

(*************************************************************************************************)

PublicFunction[CompareBearData]

CompareBearData[a_, b_] := Scope[
  a //= indexByUUID;
  b //= indexByUUID;
  DeleteNone @ Merge[{a, b}, compareBearRows]
]

compareBearRows[{a_, b_}] := If[a === b, None, DeleteNone @ MapThread[combineBearFields, {a, b}]];
combineBearFields[a_, b_] := If[a === b, None, Flipper[a, b]];

indexByUUID[a_] := Assoc[#UUID -> #& /@ a];

(*************************************************************************************************)

PublicFunction[CreateBearNote]

PublicOption[DuplicateTarget]

Options[CreateBearNote] = {
  DuplicateTarget -> False
};

CreateBearNote::exists = "Will not create note with title \"``\" because one already exists, and DuplicateTarget -> False.";

CreateBearNote[title_Str, contents_Str, OptionsPattern[]] := Scope[
  UnpackOptions[duplicateTarget];
  title //= sanitizeTitle;
  If[!duplicateTarget && BearNoteExistsQ[title, IgnoreCase -> True],
    Message[CreateBearNote::exists, title],
    SystemOpen @ $CreateBearNoteTemplate[URLEncode @ title, URLEncode @ contents];
  ];
  StringJoin["[[", title, "]]"]
];

CreateBearNote::badtitle = "Note should start with a H1-style title."
CreateBearNote[contents_Str, opts:OptionsPattern[]] := Scope[
  If[!StringStartsQ[contents, "# "], ReturnFailed["badtitle"]];
  {title, contents} = StringSplit[contents, "\n", 2];
  title = StringTrim @ StringTrimLeft[title, "# "];
  contents //= StringTrim;
  CreateBearNote[title, contents, opts]
]

_CreateBearNote := BadArguments[];

$CreateBearNoteTemplate = StringFunction @ "bear://x-callback-url/create?title=#1&text=#2";

(*************************************************************************************************)

PublicFunction[ReplaceBearNote]

ReplaceBearNote[id_Str, title_Str, contents_Str] :=
  ReplaceBearNote[id, StringJoin["# ", StringTrim @ title, "\n", StringTrim @ contents]];

ReplaceBearNote::badtitle = "Note replacement should start with a H1-style title."
ReplaceBearNote::noexisting = "Cannot replace Bear note with existing title `` since it does not exist.";
ReplaceBearNote[id_Str, contents_Str] := Scope[
  If[!StringStartsQ[contents, "# "], ReturnFailed["badtitle"]];
  If[!BearNoteUUIDQ[id],
    {oldTitle, id} = {id, FindBearNote @ id};
    If[!StringQ[id], ReturnFailed["noexisting", oldTitle]];
  ];
  SystemOpen @ $ReplaceBearNoteTemplate[id, URLEncode @ contractTextHeader @ StringTrim @ contents];
]

_ReplaceBearNote := BadArguments[];

$ReplaceBearNoteTemplate = StringFunction @ "bear://x-callback-url/add-text?id=#1&mode=replace_all&exclude_trashed=true&show_window=no&open_note=no&text=#2";

(*************************************************************************************************)

contractTextHeader[str_Str] := If[StringStartsQ[str, LineFragment ~~ "\n\n"], StringReplace[str, "\n\n" -> "\n", 1], str];

(*************************************************************************************************)

toTitleOrIDQuery[titleOrID_Str] := FilteredEntityClass[
  $BearNote,
  If[BearNoteUUIDQ[titleOrID],
    EntityFunction[z, z["ZUNIQUEIDENTIFIER"] == titleOrID],
    EntityFunction[z, z["ZTITLE"] == titleOrID]
  ]
];

(*************************************************************************************************)

PublicFunction[TextReplaceBearNote]

TextReplaceBearNote[titleOrId_Str, replacements_] :=
  TransformBearNote[titleOrId, StringReplace[replacements]];

(*************************************************************************************************)

PublicFunction[TransformBearNote]

PublicOption[VerifyResult]

Options[TransformBearNote] = {
  Verbose -> Automatic,
  DryRun -> False,
  VerifyResult -> False
}

TransformBearNote::notfound = "Cannot find existing note with ID or title \"``\".";
TransformBearNote::badtresult = "Result of transform on \"``\" was not a string or issued messages.";
TransformBearNote::corrupt = "Apparently corrupt result when transforming \"``\".";

TransformBearNote[titleOrId_Str, fn_, OptionsPattern[]] := Scope[
  UnpackOptions[$verbose, $dryRun, verifyResult];
  SetAutomatic[$verbose, $dryRun];
  VPrint["Looking up contents of article with title or ID: ", titleOrId];
  res = EntityValue[toTitleOrIDQuery @ titleOrId, {"ZUNIQUEIDENTIFIER", "ZTEXT"}];
  If[!MatchQ[res, {{_Str, _Str}}], ReturnFailed["notfound", titleOrId]];
  {uuid, text} = P1 @ res;
  text //= trimFirstLine;
  VPrint["Applying fn to string of length ", StringLength @ text];
  newText = Check[fn[text], $Failed];
  If[!StringQ[newText], ReturnFailed["badtresult", titleOrId]];
  If[newText =!= text,
    VPrint["Text changed (len ", StringLength @ text, "); edit distance ", EditDistance[newText, text]];
    VPrint["Calling bear"];
    whenWet[
      SystemOpen @ $TextReplaceBearNoteTemplate[uuid, URLEncode @ newText];
      Pause[0.2];
    ];
    If[verifyResult,
      VPrint["Obtaining new note text"];
      newText2 = BearNoteText[uuid];
      newText2 //= trimFirstLine;
      If[newText2 =!= newText && !$dryRun,
        VPrint["New note text didn't match"];
        Message[TransformBearNote::corrupt, titleOrId];
        Print[Flipper["NEW\n\n" <> newText2, "OLD\n\n" <> newText]];
        Abort[]
      ];
    ];
  ,
    VPrint["Text unchanged"];
  ];
];

trimFirstLine[str_Str] := PN @ StringSplit[str, "\n", 2];

$TextReplaceBearNoteTemplate = StringFunction @ "bear://x-callback-url/add-text?id=#1&mode=replace&exclude_trashed=true&show_window=no&open_note=no&text=#2";

(*************************************************************************************************)

PublicFunction[RenameBearNote]

RenameBearNote::notfound = "Cannot find existing note with ID or title \"``\".";
RenameBearNote::badtresult = "Result of transform on \"``\" was not a string or issued messages.";

RenameBearNote[old_Str, old_Str] := None;

RenameBearNote[old_Str, newTitle_Str, fn_:None] := Scope[
  res = EntityValue[toTitleOrIDQuery @ old, {"ZUNIQUEIDENTIFIER", "ZTITLE", "ZTEXT"}];
  If[!MatchQ[res, {{_Str, _Str, _Str}}], ReturnFailed["notfound", old]];
  {uuid, oldTitle, text} = P1 @ res;
  newText = StringTrimLeft[text, ("# " <> oldTitle) | oldTitle];
  If[fn =!= None,
    newText //= fn;
    If[!StringQ[newText], ReturnFailed["badtresult", old]];
  ];
  newContents = StringJoin[
    StringTrim @ StringJoin["# ", newTitle], "\n",
    StringTrim @ newText
  ];
  Pause[0.15];
  SystemOpen @ $ReplaceBearNoteTemplate[uuid, URLEncode @ newContents];
]

_RenameBearNote := BadArguments[];

(*************************************************************************************************)

PublicFunction[GlobalBearStringReplace]

Options[GlobalBearStringReplace] = {
  Verbose -> Automatic,
  DryRun -> False,
  VerifyResult -> True,
  IgnoreCase -> False
};

GlobalBearStringReplace::badpatt = "Not a valid pattern: ``."
GlobalBearStringReplace::notfound = "No notes containing regex pattern \"``\"."
GlobalBearStringReplace[rules_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[$verbose, $dryRun, verifyResult, ignoreCase];
  SetAutomatic[$verbose, $dryRun];
  rules //= ToList;
  lhsPattern = P1 /@ rules;
  lhsPattern = If[Len[lhsPattern] === 1, P1 @ lhsPattern, Alternatives @@ lhsPattern];
  regex = ToRegularExpression @ lhsPattern;
  If[TrueQ @ ignoreCase, regex = StringInsert[regex, "i", 3]];
  If[!StringQ[regex], ReturnFailed["badpatt", regex]];
  VPrint["Searching against regexp \"", regex, "\""];
  With[{re = regex}, query = EntityFunction[z, StringContainsQ[z["ZTEXT"], RegularExpression @ re]]];
  res = EntityValue[FilteredEntityClass[$BearNote, query], "ZUNIQUEIDENTIFIER"];
  If[!MatchQ[res, {__Str}], ReturnFailed["notfound", regex]];
  VPrint["Obtained ", Len @ res, " results."];
  Scan[
    TransformBearNote[
      #,
      StringReplace[rules, IgnoreCase -> ignoreCase],
      Verbose -> $verbose, DryRun -> $dryRun, VerifyResult -> verifyResult
    ]&,
    res
  ];
]

(*************************************************************************************************)

PublicFunction[AppendToBearNote]

AppendToBearNote::notfound = "Cannot find existing note with title \"``\"."
AppendToBearNote[titleOrID_Str, text_Str] := Scope[
  id = If[BearNoteUUIDQ[titleOrID], titleOrID, FindBearNote @ titleOrID];
  If[FailureQ[id], ReturnFailed["notfound", titleOrID]];
  SystemOpen @ $AppendToBearNoteTemplate[id, URLEncode @ StringJoin["\n\n", StringTrim @ text]];
]

AppendToBearNote[titleOrID_Str, images:_Image | {__Image}] := Scope[
  id = If[BearNoteUUIDQ[titleOrID], titleOrID, FindBearNote @ titleOrID];
  If[FailureQ[id], ReturnFailed["notfound", titleOrID]];
  Scan[
    image |-> SystemOpen @ $AppendImageToBearNoteTemplate[id, CreateUUID[] <> ".png", URLEncode @ imagePNGBase64 @ image],
    ToList @ images
  ];
]

imagePNGBase64[img_] := Base64String @ Normal @ ExportByteArray[img, "PNG"];

_AppendToBearNote := BadArguments[];

$AppendToBearNoteTemplate = StringFunction @ "bear://x-callback-url/add-text?id=#1&mode=append&exclude_trashed=true&new_line=true&text=#2";
$AppendImageToBearNoteTemplate = StringFunction @ "bear://x-callback-url/add-file?id=#1&mode=append&filename=#2&file=#3";

(*************************************************************************************************)

$bearImagePath = NormalizePath @ "~/Library/Group Containers/9K33E3U3T4.net.shinyfrog.bear/Application Data/Local Files/Note Images";
bearImagePath[path_] := Import @ PathJoin[$bearImagePath, path];

(*************************************************************************************************)

$BearDBFile = NormalizePath @ "~/Library/Group Containers/9K33E3U3T4.net.shinyfrog.bear/Application Data/database.sqlite";
$BearDBRef := $BearDBRef = DatabaseReference[File[$BearDBFile]];
$BearDB := $BearDB = RelationalDatabase[$BearDBRef];

PublicFunction[RegisterBearEntities]

$BearDBMapping = {"ZSFNOTE" -> {
  "Z_PK", "ZCREATIONDATE", "ZTITLE", "ZCREATIONDATE", "ZMODIFICATIONDATE", "ZHASFILES", "ZHASIMAGES", "ZARCHIVED", "ZTRASHED", "ZPINNED",
  "ZUNIQUEIDENTIFIER", "ZTEXT", "ZLASTEDITINGDEVICE"
}};

RegisterBearEntities::fail = "Failed to register BearNote.";
RegisterBearEntities[] := Scope[
  VPrint["Registering BearNote entities"];
  es = EntityStore[$BearDBMapping, $BearDB];
  EntityUnregister["BearNote"];
  res = EntityRegister[es];
  If[res =!= {"BearNote"}, ReturnFailed["fail"]];
  es
];

(*************************************************************************************************)

PublicVariable[$BearNote]

SetDelayedInitialValue[$BearNote, $BearNote2];

$BearNote2 := $BearNote2 = (Quiet @ RegisterBearEntities[]; "ZSFNOTE");

(*************************************************************************************************)

PublicFunction[BearNoteExistsQ]

BearNoteExistsQ[title_Str] := FilteredEntityClass[$BearNote, EntityFunction[z, z["ZTITLE"] == title]]["EntityCount"] > 0;
BearNoteExistsQ[title_Str, IgnoreCase -> True] := FilteredEntityClass[$BearNote, EntityFunction[z, StringMatchQ[z["ZTITLE"], title, IgnoreCase -> True]]]["EntityCount"] > 0;

BearNoteExistsQ[{}] := {};
BearNoteExistsQ[titles:{___Str}] := Scope[
  class = FilteredEntityClass[$BearNote, EntityFunction[z, MemberQ[titles, z["ZTITLE"]]]];
  foundTitles = EntityValue[class, "ZTITLE"];
  MemberQ[foundTitles, #]& /@ titles
];

(*************************************************************************************************)

PublicFunction[FindBearNote]

Options[FindBearNote] = {IgnoreCase -> True};

FindBearNote[title_Str, OptionsPattern[]] := Scope[
  UnpackOptions[ignoreCase];
  query = If[ignoreCase,
    EntityFunction[z, StringMatchQ[z["ZTITLE"], title, IgnoreCase -> True]],
    EntityFunction[z, z["ZTITLE"] == title]
  ];
  res = EntityValue[FilteredEntityClass[$BearNote, query], "ZUNIQUEIDENTIFIER"];
  If[MatchQ[res, {_Str}], P1 @ res, $Failed]
];

(*************************************************************************************************)

PublicFunction[GotoBearNote, OpenBearNote]

GotoBearNote[titleOrID_Str] :=
  SystemOpen @ If[BearNoteUUIDQ[titleOrID], $OpenBearNoteIDTemplate, $OpenBearNoteTitleTemplate][URLEncode @ titleOrID, "show"];

OpenBearNote[list_List] := Scan[OpenBearNote, list];
OpenBearNote[titleOrID_Str] :=
  SystemOpen @ If[BearNoteUUIDQ[titleOrID], $OpenBearNoteIDTemplate, $OpenBearNoteTitleTemplate][URLEncode @ titleOrID, "new"];

$OpenBearNoteIDTemplate = StringFunction @ "bear://x-callback-url/open-note?id=#1&exclude_trashed=true&#2_window=yes";
$OpenBearNoteTitleTemplate = StringFunction @ "bear://x-callback-url/open-note?title=#1&exclude_trashed=true&#2_window=yes";

(*************************************************************************************************)

PublicFunction[BearNoteText]

BearNoteText[titleOrID_Str] := Scope[
  res = EntityValue[toTitleOrIDQuery @ titleOrID, "ZTEXT"];
  If[MatchQ[res, {_Str}], P1 @ res, $Failed]
];

(*************************************************************************************************)

PublicFunction[BearNoteUUIDQ]

"30A395FF-FF7D-4B9D-97B5-FB29E3C9A8B2"
$BearNoteUUIDRegEx = RegularExpression["[A-Z0-9]{8}(?:-[A-Z0-9]{4}){3}-[A-Z0-9]{12}(?:-[A-Z0-9]{3,6}-[A-Z0-9]{16})?"];
BearNoteUUIDQ[s_Str] := StringMatchQ[s, $BearNoteUUIDRegEx];

(*************************************************************************************************)

PublicFunction[BearNoteData]

Options[BearNoteData] = {
  MaxItems -> Infinity
};

BearNoteData::badfield = "Unrecognized field ``."
BearNoteData[part_, field_, OptionsPattern[]] := Scope @ CatchMessage[
  UnpackOptions[maxItems];
  class = toBearClass @ part;
  If[maxItems =!= Infinity, class = SampledEntityClass[class, maxItems]];
  isAll = field === All;
  If[isAll, field = Keys @ $bearFieldDict];
  isKeyed = MatchQ[field, _Rule];
  If[isKeyed,
    isList = ListQ[Values @ field];
    field = ToList[Keys @ field, Values @ field]];
  result = EntityValue[class, Lookup[$bearFieldDict, field]];
  If[isKeyed,
    keys = Part[result, All, 1];
    field = Part[field, If[isList, 2;;, 2]];
    result = Part[result, All, If[isList, 2;;, 2]];
  ];
  If[ListQ[field],
    result = If[H[class] === Entity,
      AssociationThread[field, result],
      AssociationThread[field, #]& /@ result
    ]
  ];
  If[isKeyed,
    result = AssociationThread[keys, result]
  ];
  result
]

toBearClass = Case[
  list:{__Rule}                 := combineEntityFunctions @ Part[Map[toBearClass, list], All, 2];
  All                           := $BearNote;
  part:(_Int | _Span)           := Part[EntityList[$BearNote], part];
  "Text" -> (p:(_Str | _RegularExpression)) := %["Text" -> StringContainsQ[p]];
  "Tag" -> tag_Str              := With[{re = "\n(?:#meta/master(?: for)? )?#" ~~ StringTrimLeft[tag, "#"]}, %["Text" -> StringContainsQ[RegularExpression[re]]]];
  field_ -> True                := %[field -> 1];
  field_ -> False               := %[field -> 0];
  field_ -> p_StringExpression  := %[field -> RegularExpression[ToRegularExpression @ p]];
  field_ -> r_RegularExpression := %[field -> StringMatchQ[r]];
  field_ -> fn_Symbol           := makeFEC[field, fn[$Z]];
  field_ -> (fn_Symbol[value_]) := makeFEC[field, fn[value][$Z]];
  field_ -> value_              := makeFEC[field, $Z == value];
]

SetHoldRest[makeFEC];
makeFEC[field_, body_] := With[
  {prop = Lookup[$bearFieldDict, field, ThrowMessage["badfield", field]]},
  FilteredEntityClass[$BearNote, EntityFunction[z, body]] /. ($Z :> z[prop]) /. (re_RegularExpression) :> ExpandPosixCharacterClasses[re]
];

combineEntityFunctions[fns:{HoldPattern[__EntityFunction]}] :=
  FilteredEntityClass[$BearNote, Construct[EntityFunction, z, (Hold @@ fns)[[All, 2]]] /. Hold -> And];

$bearFieldDict = Assoc[
  "ID"                  -> "Z_PK",
  "Title"               -> "ZTITLE",
  "CreationDate"        -> "ZCREATIONDATE",
  "ModificationDate"    -> "ZMODIFICATIONDATE",
  "HasFilesQ"           -> "ZHASFILES",
  "HasImagesQ"          -> "ZHASIMAGES",
  "ArchivedQ"           -> "ZARCHIVED",
  "TrashedQ"            -> "ZTRASHED",
  "PinnedQ"             -> "ZPINNED",
  "UUID"                -> "ZUNIQUEIDENTIFIER",
  "Text"                -> "ZTEXT",
  "LastEditingDevice"   -> "ZLASTEDITINGDEVICE"
]

(*************************************************************************************************)

PublicFunction[BearPeople]

BearPeople[] := BearNoteData[{"Tag" -> "person"}, "Title"];

(*************************************************************************************************)

PublicFunction[BearConcepts]

BearConcepts[] := BearNoteData[{"Tag" -> "concept"}, "Title"];

(*************************************************************************************************)

PublicFunction[DeleteBearNote]

DeleteBearNote[uuid_Str] := SystemOpen @ $BearDeleteTemplate @ uuid;
DeleteBearNote[list_List] := Scan[DeleteBearNote, list];

$BearDeleteTemplate = StringFunction @ "bear://x-callback-url/trash?id=#1";

(*************************************************************************************************)

PublicFunction[BearNoteTextCases]

BearNoteTextCases[pattern_] := Scope[
  regex = ToRegularExpression @ toLHSPattern @ pattern;
  If[!StringQ[regex], ReturnFailed[]];
  allText = BearNoteData["Text" -> StringContainsQ[RegularExpression @ regex], "Text"];
  Catenate @ StringCases[allText, pattern]
]

toLHSPattern = Case[
  l_List                      := Alternatives @@ Map[%, l];
  r_Rule | r_RuleDelayed      := P1 @ r;
  other_                      := other;
]

(**************************************************************************************************)

PublicFunction[CreateBearArxivPages]

Options[CreateBearArxivPages] = {
  Verbose -> False
};

CreateBearArxivPages::badmd = "Could not create markdown for data ``.";
CreateBearArxivPages[assocs:{__Assoc}, opts:OptionsPattern[]] := Scope[
  assocs = ReverseSortBy[assocs, Key["Date"]];
  list = Map[assoc |-> (
    res = PaperToMarkdown[assoc, opts];
    If[!StringQ[res], ReturnFailed["badmd", MsgExpr @ assoc]];
    CreateBearNote[res]
  ),
    assocs
  ];
  StringRiffle[list, "\n"]
];
