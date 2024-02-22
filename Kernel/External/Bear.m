PublicIOFunction[SaveBearData, LoadBearData]

toBearDataPath[name_] := DataPath["Bear", name <> ".mx"];

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

PublicIOFunction[CreateBearNote]

PublicOption[DuplicateTarget]

Options[CreateBearNote] = {
  DuplicateTarget -> False,
  DryRun -> False
};

CreateBearNote[title_Str, contents_Str, OptionsPattern[]] := Scope[
  UnpackOptions[duplicateTarget, $dryRun];
  title //= sanitizeTitle;
  whenWet @ If[!duplicateTarget && BearNoteExistsQ[title, IgnoreCase -> True],
    Message[CreateBearNote::noteExists, title],
    SystemOpen @ $CreateBearNoteTemplate[URLEncode @ title, URLEncode @ contents];
  ];
  SJoin["[[", title, "]]"]
];

General::badBearTitle = "Note should start with a H1-style title."
CreateBearNote[contents_Str, opts:OptionsPattern[]] := Scope[
  If[!SStartsQ[contents, "# "], ReturnFailed["badBearTitle"]];
  If[SContainsQ[contents, "\n"],
    {title, contents} = SSplit[contents, "\n", 2],
    title = contents; contents = "";
  ];
  title = STrim @ StringTrimLeft[title, "# "];
  contents //= STrim;
  CreateBearNote[title, contents, opts]
]

_CreateBearNote := BadArguments[];

$CreateBearNoteTemplate = StringFunction @ "bear://x-callback-url/create?title=#1&text=#2";

(*************************************************************************************************)

PublicIOFunction[ReplaceBearNote]

ReplaceBearNote[id_Str, title_Str, contents_Str] :=
  ReplaceBearNote[id, SJoin["# ", STrim @ title, "\n", STrim @ contents]];

ReplaceBearNote::noNoteExisting = "Cannot replace note with existing title `` since it does not exist.";
ReplaceBearNote[id_Str, contents_Str] := Scope[
  If[!SStartsQ[contents, "# "], ReturnFailed["badBearTitle"]];
  If[!BearNoteUUIDQ[id],
    {oldTitle, id} = {id, FindBearNote @ id};
    If[!StrQ[id], ReturnFailed["noNoteExisting", oldTitle]];
  ];
  SystemOpen @ $ReplaceBearNoteTemplate[id, URLEncode @ contractTextHeader @ STrim @ contents];
]

_ReplaceBearNote := BadArguments[];

$ReplaceBearNoteTemplate = StringFunction @ "bear://x-callback-url/add-text?id=#1&mode=replace_all&exclude_trashed=true&show_window=no&open_note=no&text=#2";

(*************************************************************************************************)

contractTextHeader[str_Str] := If[SStartsQ[str, LineFragment ~~ "\n\n"], SRep[str, "\n\n" -> "\n", 1], str];

(*************************************************************************************************)

toTitleOrIDQuery[titleOrID_Str] := FilteredEntityClass[
  $BearNoteEntity,
  With[{field = If[BearNoteUUIDQ[titleOrID], "ZUNIQUEIDENTIFIER", "ZTITLE"]},
    EntityFunction[z, (z[field] == titleOrID) && (z["ZTRASHED"] == 0)]
  ]
];

General::bearUUIDTitleMix = "Cannot lookup a mix of UUIDs and titles: ``.";

toTitleOrIDQuery[titlesOrIDs:{__Str}] := FilteredEntityClass[
  $BearNoteEntity,
  With[{field = If[VecQ[titlesOrIDs, BearNoteUUIDQ], "ZUNIQUEIDENTIFIER",
    If[AnyTrue[titlesOrIDs, BearNoteUUIDQ], ThrowMessage["bearUUIDTitleMix", titlesOrIDs]]; "ZTITLE"]},
    EntityFunction[z, MemberQ[titlesOrIDs, z[field]] && (z["ZTRASHED"] == 0)]
  ]
];

(*************************************************************************************************)

PublicIOFunction[TextReplaceBearNote]

Options[TextReplaceBearNote] = {
  Verbose -> Auto,
  DryRun -> False,
  VerifyResult -> False
}

TextReplaceBearNote[titleOrID_Str, replacements_, opts:OptionsPattern[]] :=
  TransformBearNote[titleOrID, SRep[replacements], opts];

(*************************************************************************************************)

PublicIOFunction[TransformBearNote]

PublicOption[VerifyResult, TransformTitle]

Options[TransformBearNote] = {
  Verbose -> Auto,
  DryRun -> False,
  VerifyResult -> False,
  TransformTitle -> False
}

TransformBearNote::notfound = "Cannot find existing note with ID or title \"``\".";
TransformBearNote::badtresult = "Result of transform on \"``\" was not a string or issued messages.";
TransformBearNote::corrupt = "Apparently corrupt result when transforming \"``\".";
TransformBearNote::multiple = "Multiple notes with ID or title \"``\".";

TransformBearNote[titleOrID_Str, fn_, OptionsPattern[]] := Scope[
  UnpackOptions[$verbose, $dryRun, verifyResult, transformTitle];
  SetAutomatic[$verbose, $dryRun];
  If[$verbose,
    {title, uuid} = F[EntityValue[toTitleOrIDQuery @ titleOrID, {"ZTITLE", "ZUNIQUEIDENTIFIER"}], {None, None}];
    If[BearNoteUUIDQ @ titleOrID, SetNone[uuid, titleOrID], SetNone[title, titleOrID]];
    VPrint["Transforming Bear note with title \"", title, "\" and UUID \"", uuid, "\""];
  ];

  res = EntityValue[toTitleOrIDQuery @ titleOrID, {"ZUNIQUEIDENTIFIER", "ZTEXT"}];
  If[!MatchQ[res, {{_Str, _Str}}],
    If[Len[res] > 1, ReturnFailed["multiple", titleOrID]];
    ReturnFailed["notfound", titleOrID]];
  {uuid, oldText} = F @ res;

  {oldTitle, oldBody} = splitTitleBody @ oldText;

  input = If[transformTitle, oldText, oldBody];
  result = Check[fn[input], $Failed];
  If[!StrQ[result], ReturnFailed["badtresult", titleOrID]];

  VBlock @ If[input === result,
    VPrint["Text unchanged."];
  ,
    If[transformTitle,
      {newTitle, newBody} = splitTitleBody @ result,
      newTitle = oldTitle; newBody = result;
    ];
    If[$verbose,
      If[newTitle =!= oldTitle,
        VPrint["Title changed: ", ShowSequenceAlignment[oldTitle, newTitle]];
      ];
      If[newBody =!= oldBody,
        VPrint["Text changed:"];
        VPrint @ ShowSequenceAlignment[oldBody, newBody, ElideUnchanged -> True];
      ];
    ];
    whenWet[
      If[newTitle =!= oldTitle,
        SystemOpen @ $ReplaceBearNoteTemplate[uuid, URLEncode @ result],
        SystemOpen @ $TextReplaceBearNoteTemplate[uuid, URLEncode @ newBody]
      ];
      Pause[0.2];
    ];

    If[verifyResult && !$dryRun,
      retries = 0;
      Label[retry];
      result2 = BearNoteText[uuid];
      If[!transformTitle, {tmp, result2} = splitTitleBody @ result2];
      If[result2 =!= result,
        If[retries < 3, Pause[0.5]; retries++; Goto[retry]];
        VPrint["Retrieved result didn't match stored result:"];
        If[$verbose, VPrint, Print] @ ShowSequenceAlignment[result, result2, ElideUnchanged -> True];
        ReturnFailed["corrupt", titleOrID];
      ];
    ];
  ];

  uuid
];

splitTitleBody[str_Str] := Scope[
  {title, text} = SSplit[str, "\n", 2];
  title = StringTrimLeft[title, "# "];
  {title, text}
];

$TextReplaceBearNoteTemplate = StringFunction @ "bear://x-callback-url/add-text?id=#1&mode=replace&exclude_trashed=true&show_window=no&open_note=no&text=#2";

(*************************************************************************************************)

PublicIOFunction[FindDuplicateBearNotes]

FindDuplicateBearNotes[] := Scope[
  data = BearNoteData[All, {"Title", "UUID"}];
  groups = GroupBy[data, Key["Title"] -> Key["UUID"]];
  Select[groups, Len[#] > 1&]
];

(*************************************************************************************************)

PublicIOFunction[RenameBearNote]

RenameBearNote::notfound = "Cannot find existing note with ID or title \"``\".";
RenameBearNote::badtresult = "Result of transform on \"``\" was not a string or issued messages.";
RenameBearNote::trimTitleFail = "Could not trim title from body for \"``\".";

RenameBearNote[old_Str, old_Str] := None;

RenameBearNote[old_Str, newTitle_Str, fn_:None] := Scope[
  res = EntityValue[toTitleOrIDQuery @ old, {"ZUNIQUEIDENTIFIER", "ZTITLE", "ZTEXT"}];
  If[!MatchQ[res, {{_Str, _Str, _Str}}], ReturnFailed["notfound", old]];
  {uuid, oldTitle, text} = F @ res;
  newText = StringTrimLeft[text, ("# " <> oldTitle) | oldTitle];
  If[SLen[text] == SLen[newText], ReturnFailed["trimTitleFail", old]];
  If[fn =!= None,
    newText //= fn;
    If[!StrQ[newText], ReturnFailed["badtresult", old]];
  ];
  newContents = SJoin[
    STrim @ SJoin["# ", newTitle], "\n",
    STrim @ newText
  ];
  Pause[0.15];
  SystemOpen @ $ReplaceBearNoteTemplate[uuid, URLEncode @ newContents];
  uuid
]

_RenameBearNote := BadArguments[];

(*************************************************************************************************)

PublicIOFunction[GlobalBearStringReplace]

PublicOption[MaxFailures]

Options[GlobalBearStringReplace] = {
  Verbose -> Auto,
  DryRun -> False,
  VerifyResult -> True,
  IgnoreCase -> False,
  TransformTitle -> True,
  MaxFailures -> Inf
};

GlobalBearStringReplace::badpatt = "Not a valid pattern: ``."
GlobalBearStringReplace::notfound = "No notes containing regex pattern \"``\".";
GlobalBearStringReplace::failures = "The following `` notes failed the transform: ``.";

GlobalBearStringReplace[rules_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[$verbose, $dryRun, verifyResult, ignoreCase, transformTitle, maxFailures];
  SetAutomatic[$verbose, $dryRun];

  rules //= ToList;
  lhsPattern = F /@ rules;
  lhsPattern = If[Len[lhsPattern] === 1, F @ lhsPattern, Alt @@ lhsPattern];
  regex = ToRegularExpression @ lhsPattern;
  If[TrueQ @ ignoreCase, regex = SInsert[regex, "i", 3]];
  If[!StrQ[regex], ReturnFailed["badpatt", regex]];

  VPrint["Searching against regexp \"", regex, "\""];
  With[{re = regex}, query = EntityFunction[z, SContainsQ[z["ZTEXT"], RegularExpression @ re] && (z["ZTRASHED"] == 0)]];
  uuids = EntityValue[FilteredEntityClass[$BearNoteEntity, query], "ZUNIQUEIDENTIFIER"];
  If[!MatchQ[uuids, {__Str}], ReturnFailed["notfound", regex]];
  VPrint["Obtained ", Len @ uuids, " results."];

  fn = SRep[rules, IgnoreCase -> ignoreCase];
  opts = Seq[Verbose -> $verbose, DryRun -> $dryRun, VerifyResult -> verifyResult, TransformTitle -> transformTitle];
  numFailures = 0;
  results = Map[
    uuid |-> If[numFailures > maxFailures, uuid,
      tres = TransformBearNote[uuid, fn, opts];
      If[FailureQ[tres], numFailures++];
      tres
    ],
    uuids
  ];

  failures = Pick[uuids, results, $Failed];
  If[failures =!= {}, Message[GlobalBearStringReplace::failures, numFailures, failures]];

  Pause[0.25];
  BearUUIDToTitle @ uuids
];

(*************************************************************************************************)

PublicIOFunction[BearUUIDToTitle]

BearUUIDToTitle[{}] := {};

BearUUIDToTitle[$Failed] := $Failed;

BearUUIDToTitle[id_Str] := Scope[
  query = FilteredEntityClass[$BearNoteEntity, EntityFunction[z, (z["ZUNIQUEIDENTIFIER"] == id) && (z["ZTRASHED"] == 0)]];
  res = EntityValue[query, "ZTITLE"];
  F[res, $Failed]
];

BearUUIDToTitle[ids_List] := Scope[
  query = With[{ids2 = Select[ids, StrQ]}, FilteredEntityClass[$BearNoteEntity, EntityFunction[z, MemberQ[ids2, z["ZUNIQUEIDENTIFIER"]] && (z["ZTRASHED"] == 0)]]];
  pairs = EntityValue[query, {"ZUNIQUEIDENTIFIER", "ZTITLE"}];
  rules = MapApply[#1 -> #2&, pairs];
  Lookup[rules, ids, $Failed]
];

(*************************************************************************************************)

PublicIOFunction[AppendToBearNote]

AppendToBearNote::notfound = "Cannot find existing note with title \"``\"."
AppendToBearNote[titleOrID_Str, text_Str] := Scope[
  id = If[BearNoteUUIDQ[titleOrID], titleOrID, FindBearNote @ titleOrID];
  If[FailureQ[id], ReturnFailed["notfound", titleOrID]];
  SystemOpen @ $AppendToBearNoteTemplate[id, URLEncode @ SJoin["\n\n", STrim @ text]];
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

PrivateVariable[$bearImagePath]

$bearImagePath = NormalizePath @ "~/Library/Group Containers/9K33E3U3T4.net.shinyfrog.bear/Application Data/Local Files/Note Images";

(*************************************************************************************************)

$BearDBFile = NormalizePath @ "~/Library/Group Containers/9K33E3U3T4.net.shinyfrog.bear/Application Data/database.sqlite";
$BearDBRef := $BearDBRef = DatabaseReference[File[$BearDBFile]];
$BearDB := $BearDB = RelationalDatabase[$BearDBRef];

PublicFunction[RegisterBearEntities]

$BearDBMapping = {
  "ZSFNOTE" -> {
    "Z_PK", "ZCREATIONDATE", "ZTITLE", "ZSUBTITLE", "ZCREATIONDATE", "ZMODIFICATIONDATE", "ZHASFILES", "ZHASIMAGES", "ZARCHIVED", "ZTRASHED", "ZPINNED",
    "ZUNIQUEIDENTIFIER", "ZTEXT", "ZLASTEDITINGDEVICE"
  },
  "ZSFNOTEFILE" -> {
    "Z_PK", "ZNOTE", "ZFILESIZE", "ZHEIGHT", "ZWIDTH", "ZCREATIONDATE", "ZINSERTIONDATE", "ZUPLOADEDDATE", "ZFILENAME", "ZNORMALIZEDFILEEXTENSION", "ZUNIQUEIDENTIFIER"
  },
  "ZSFNOTETAG" -> {
    "Z_PK", "ZTAGCON", "ZTITLE", "ZUNIQUEIDENTIFIER"
  }
};

RegisterBearEntities::fail = "Failed to register Bear entities.";
RegisterBearEntities[] := Module[{es, res},
  VPrint["Registering ZSFNOTE, ZSFNOTEFILE, ZSFNOTETAG entities."];
  es = EntityStore[$BearDBMapping, $BearDB];
  res = Quiet @ EntityRegister[es];
  If[res =!= {"ZSFNOTE", "ZSFNOTEFILE", "ZSFNOTETAG"},
    VPrint["Registering failed"];
    Return[$Failed]];
  RegisterBearEntities[] := es;
  es
];

(*************************************************************************************************)

$BearNoteEntity := $BearNoteEntity = (Quiet @ RegisterBearEntities[]; "ZSFNOTE");
$BearFileEntity := $BearFileEntity = (Quiet @ RegisterBearEntities[]; "ZSFNOTEFILE");
$BearTagEntity := $BearTagEntity = (Quiet @ RegisterBearEntities[]; "ZSFNOTETAG");

(*************************************************************************************************)

PublicIOFunction[BearNoteExistsQ]

entitiesExist[entity_, entityFn_] := FilteredEntityClass[entity, entityFn]["EntityCount"] > 0;

BearNoteExistsQ[title_Str] :=
  entitiesExist[$BearNoteEntity, EntityFunction[z, (z["ZTITLE"] == title) && (z["ZTRASHED"] == 0)]];

BearNoteExistsQ[title_Str, IgnoreCase -> True] :=
  entitiesExist[$BearNoteEntity, EntityFunction[z, SMatchQ[z["ZTITLE"], title, IgnoreCase -> True] && (z["ZTRASHED"] == 0)]];

BearNoteExistsQ[{}] := {};
BearNoteExistsQ[titles:{___Str}] := Scope[
  class = FilteredEntityClass[$BearNoteEntity, EntityFunction[z, MemberQ[titles, z["ZTITLE"]] && (z["ZTRASHED"] == 0)]];
  foundTitles = EntityValue[class, "ZTITLE"];
  MemberQ[foundTitles, #]& /@ titles
];

(*************************************************************************************************)

PublicIOFunction[FindBearNote]

Options[FindBearNote] = {IgnoreCase -> True};

FindBearNote[title_Str, OptionsPattern[]] := Scope[
  UnpackOptions[ignoreCase];
  query = If[ignoreCase,
    EntityFunction[z, SMatchQ[z["ZTITLE"], title, IgnoreCase -> True] && (z["ZTRASHED"] == 0)],
    EntityFunction[z, (z["ZTITLE"] == title) && (z["ZTRASHED"] == 0)]
  ];
  res = EntityValue[FilteredEntityClass[$BearNoteEntity, query], "ZUNIQUEIDENTIFIER"];
  If[MatchQ[res, {_Str}], F @ res, $Failed]
];

(*************************************************************************************************)

PublicIOFunction[BearNoteTitles]

Options[BearNoteTitles] = {
  IgnoreCase -> True
}

globToPattern := Case[
  glob_Str    := SRep[glob, "*" -> ___];
  globs_List  := Map[%, globs];
];

$strOrListP = _Str | {__Str};

toGlobMatcher[patt_, False] := SMatchQ[globToPattern @ patt];
toGlobMatcher[patt_, True] := SMatchQ[globToPattern @ patt, IgnoreCase -> True];

BearNoteTitles[title:$strOrListP, OptionsPattern[]] :=
  BearNoteData["Title" -> toGlobMatcher[title, OptionValue[IgnoreCase]], "Title"];

BearNoteTitles[title:$strOrListP, subtitle:$strOrListP, OptionsPattern[]] :=
  BearNoteData[{"Title" -> toGlobMatcher[title, OptionValue[IgnoreCase]], "Subtitle" -> toGlobMatcher[subtitle, OptionValue[IgnoreCase]]}, "Title"];

(*************************************************************************************************)

PublicIOFunction[GotoBearNote, OpenBearNote]

GotoBearNote[titleOrID_Str] :=
  SystemOpen @ If[BearNoteUUIDQ[titleOrID], $OpenBearNoteIDTemplate, $OpenBearNoteTitleTemplate][URLEncode @ titleOrID, "show"];

OpenBearNote[list_List] := Scan[OpenBearNote, list];
OpenBearNote[titleOrID_Str] :=
  SystemOpen @ If[BearNoteUUIDQ[titleOrID], $OpenBearNoteIDTemplate, $OpenBearNoteTitleTemplate][URLEncode @ titleOrID, "new"];

$OpenBearNoteIDTemplate = StringFunction @ "bear://x-callback-url/open-note?id=#1&exclude_trashed=true&#2_window=yes";
$OpenBearNoteTitleTemplate = StringFunction @ "bear://x-callback-url/open-note?title=#1&exclude_trashed=true&#2_window=yes";

(*************************************************************************************************)

PublicIOFunction[BearNoteText]

BearNoteText[titleOrID_Str] := Scope[
  res = EntityValue[toTitleOrIDQuery @ titleOrID, "ZTEXT"];
  If[!MatchQ[res, {_Str}], ReturnFailed[]];
  F @ res
];

(*************************************************************************************************)

PublicIOFunction[BearNoteUUIDQ]

"30A395FF-FF7D-4B9D-97B5-FB29E3C9A8B2"
$BearNoteUUIDRegEx = RegularExpression["[A-Z0-9]{8}(?:-[A-Z0-9]{4}){3}-[A-Z0-9]{12}(?:-[A-Z0-9]{3,6}-[A-Z0-9]{16})?"];
BearNoteUUIDQ[s_Str] := SMatchQ[s, $BearNoteUUIDRegEx];
BearNoteUUIDQ[_] := False;

(*************************************************************************************************)

PublicIOFunction[BearNoteData]

SetUsage @ "
BearNoteData[query$, 'field$'] returns a list of the fields for notes matching query$.
BearNoteData[query$, {'field$1', 'field$2', $$}] returns a list of associations.
BearNoteData[query$, All] returns an association of all fields.

## Queries
* a query can consist of a rule 'field$' -> spec$, or a list of these, or %%All, or a %%Span.
* each spec$ can be a literal value or a predicate like %StringMatchQ, %StringStartsQ, %ContainsQ.

## Options
* the option %%MaxItems specifies how many items to return.
* the option %%IncludeTrashed specifies whether to include items in the trash.

* also see %BearNoteLookup, which is designed to look up a single note's fields by UUID or title.
"

PublicOption[IncludeTrashed]

Options[BearNoteData] = {
  MaxItems -> Inf,
  IncludeTrashed -> False
};

BearNoteData[part_, field_, OptionsPattern[]] := Scope @ CatchMessage[
  UnpackOptions[maxItems, includeTrashed];
  If[!includeTrashed,
    part = Switch[part,
      _Rule | {__Rule}, ToList[part, "TrashedQ" -> 0],
      All,              "TrashedQ" -> 0,
      _,                part
    ];
  ];
  class = toBearClass @ part;
  If[maxItems =!= Inf, class = SampledEntityClass[class, maxItems]];
  isAll = field === All;
  If[isAll, field = $knownBearFields];
  isKeyed = MatchQ[field, _Rule];
  If[isKeyed,
    isList = ListQ[Values @ field];
    field = ToList[Keys @ field, Values @ field]];
  Block[{Databases`Database`$DBQueryLogger = If[TrueQ @ $verbose, (VPrint["SQL Query: ", Pane @ #]; #)&, Id]},
    result = EntityValue[class, Lookup[$bearFieldDict, field]]
  ];
  If[result === {}, Return @ {}];
  If[isKeyed,
    keys = Part[result, All, 1];
    field = Part[field, If[isList, 2;;, 2]];
    result = Part[result, All, If[isList, 2;;, 2]];
  ];
  If[ListQ[field],
    result = If[H[class] === Entity,
      AssocThread[field, result],
      AssocThread[field, #]& /@ result
    ];
  ];
  If[isKeyed,
    result = AssocThread[keys, result]
  ];
  result
]

$masterTagPrefix = Maybe["#meta/master" ~~ Maybe[" for"] ~~ WhitespaceCharacter..];

toBearClass = Case[
  list:{__Rule}                 := combineEntityFunctions @ Part[Map[toBearClass, list], All, 2];
  All                           := $BearNoteEntity;
  part:(_Int | _Span)           := Part[EntityList[$BearNoteEntity], part];
  "Text" -> (p:(_Str | _RegularExpression)) := %["Text" -> SContainsQ[p]];
  "Tag" -> tag_                 := %["Subtitle" -> SContainsQ[toRegexp @ toStrPatt @ tag]];
  "PrimaryTag" -> tag_          := %["Subtitle" -> SStartsQ[toRegexp[$masterTagPrefix ~~ toStrPatt[tag]]]];
  field_ -> True                := %[field -> 1];
  field_ -> False               := %[field -> 0];
  field_ -> p_StringExpression  := %[field -> checkLineRE @ RegularExpression[ToRegularExpression @ p]];
  field_ -> r_RegularExpression := %[field -> SMatchQ[checkLineRE @ r]];
  field_ -> fn_Symbol           := makeFEC[field, fn[$Z]];
  field_ -> (fn_Symbol[args__]) := checkLineExp @ makeFEC[field, fn[args][$Z]];
  field_ -> value_              := makeFEC[field, $Z == value];
];

toRegexp = Case[
  s_Str                := s;
  re_RegularExpression := re;
  exp_                 := RegularExpression @ ToRegularExpression @ exp;
];

toStrPatt = Case[
  strs:{__Str} := Apply[Alt, strs];
  str_Str      := str;
  other_       := other;
];

badReQ[re_Str] := SContainsQ[SRep[re, "[^" -> "["], "^"|"$"];

BearNoteData::badRegexp = "Regular expressions containing ^ and $ are not supported by SQLite: \"``\". Consider rewriting to newlines, which will not match exactly the same cases.";
checkLineRE[RegularExpression[re_Str]] := (
  If[badReQ[re], Message[BearNoteData::badRegexp, re]];
  re
);

BearNoteData::badStringExp = "String expressions containing StartOfLine and EndOfLine are not supported by SQLite: ``. Consider rewriting to newlines, which will not match exactly the same cases.";
checkLineExp[e_] := (
  If[ContainsQ[e, StartOfLine | EndOfLine | RegularExpression[re_Str ? badReQ]],
    Message[BearNoteData::badStringExp, e]];
  e
);

checkLineRE[e_] := e;

SetHoldRest[makeFEC];

makeFEC[field_, body_] := With[
  {prop = lookupBearField @ field},
  FilteredEntityClass[$BearNoteEntity, EntityFunction[z, body]] /. ($Z :> z[prop]) /. (re_RegularExpression) :> ExpandPosixLetterClasses[re]
];

combineEntityFunctions[fns:{HoldP[__EntityFunction]}] :=
  FilteredEntityClass[$BearNoteEntity, Construct[EntityFunction, z, (Hold @@ fns)[[All, 2]]] /. Hold -> And];

(*************************************************************************************************)

$bearFieldDict = Assoc[
  "ID"                  -> "Z_PK",
  "Title"               -> "ZTITLE",
  "Subtitle"            -> "ZSUBTITLE",
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
];

$knownBearFields = Keys @ $bearFieldDict;

General::unknownBearField = "Unrecognized field ``, should be one of ``."

lookupBearField = Case[
  field_Str   := Lookup[$bearFieldDict, field, ThrowMessage["unknownBearField", field, MsgExpr @ $knownBearFields]];
  fields_List := Lookup[$bearFieldDict, fields, ThrowMessage["unknownBearField", F @ Comp[fields, $knownBearFields], MsgExpr @ $knownBearFields]];
];

(*************************************************************************************************)

PublicIOFunction[BearNoteLookup]

SetUsage @ "
BearNoteLookup['id$', 'field$'] gives the value of field$ for the note with given UUID or title.
BearNoteLookup['id$', {'field$1', 'field$2', $$}] gives a tuple of field values.
BearNoteLookup[{'id$1', 'id$2', $$}, spec$] gives a list of tuples.

* if an id$ is not found, $Failed or a tuple of $Failed will be returned.
* if more than one note has a matching title, failure will also be returned and a message issued.

* also see %BearNoteData, which is designed to look up fields by general query.
"

BearNoteLookup::multipleResults = "ID `` yielded multiple results."
BearNoteLookup::firstArg = "First argument `` should be a string or list of strings."


BearNoteLookup[{}, _] := {};

BearNoteLookup[id_Str, fieldSpec_] := Scope @ CatchMessage[

  query = toTitleOrIDQuery @ id;
  keyField = If[BearNoteUUIDQ @ id, "ZUNIQUEIDENTIFIER", "ZTITLE"];

  fieldTuple = lookupBearField @ fieldSpec;
  res = EntityValue[query, fieldTuple];

  If[!MatchQ[res, {_}],
    If[Len[res] > 1, Message[BearNoteLookup::multipleResults, id]];
    Return @ If[ListQ @ fieldSpec, Repeat[$Failed, Len @ fieldSpec], $Failed]
  ];

  F @ res
];

BearNoteLookup[idList:{__Str}, fieldSpec_] := Scope @ CatchMessage[

  query = toTitleOrIDQuery @ idList;
  If[FailureQ[query], ReturnFailed[]];
  keyField = If[BearNoteUUIDQ @ F @ idList, "ZUNIQUEIDENTIFIER", "ZTITLE"];

  fieldTuple = ToList[keyField, lookupBearField @ fieldSpec];
  resTuples = Quiet @ Check[EntityValue[query, fieldTuple], None];
  If[resTuples === None, Return @ Repeat[$Failed, {Len @ idList, Len @ fieldTuple}]];
  (* ^ work around bug in EntityValue when no results and repeated fields *)

  If[!ListVectorQ[resTuples], ReturnFailed[]];
  resAssoc = Assoc @ Map[F[#] -> Rest[#]&, resTuples];

  If[Len[resAssoc] > Len[idList], Message[BearNoteLookup::multipleResults, idList]];
  res = Lookup[resAssoc, idList, Repeat[$Failed, Len @ fieldTuple]];

  If[StrQ[fieldSpec], PA1 @ res, res]
];

BearNoteLookup[other_, _] := (Message[BearNoteLookup::firstArg, MsgExpr @ other]; $Failed)

(*************************************************************************************************)

PublicIOFunction[BearNoteAttachmentData]

BearNoteAttachmentData[id_Int] := Scope[
  class = FilteredEntityClass[$BearFileEntity, EntityFunction[z, z["ZNOTE"] == id]];
  attachments = EntityValue[class, {"ZFILENAME", "ZCREATIONDATE", "ZINSERTIONDATE", "ZUPLOADEDDATE", "ZNORMALIZEDFILEEXTENSION", "ZUNIQUEIDENTIFIER"}];
  If[!ListQ[attachments] || Len[attachments] == 0, ReturnFailed[]];
  Assoc @ MapApply[
    #1 -> Assoc["FileName" -> #1, "CreationDate" -> #2, "InsertionDate" -> #3, "UploadDate" -> #4, "Extension" -> #5, "UUID" -> #6]&,
    attachments
  ]
];

(*************************************************************************************************)

PublicIOFunction[BearPeople]

BearPeople[] := BearNoteTaggedTitles["#person"];

(*************************************************************************************************)

PublicIOFunction[DeleteBearNote]

DeleteBearNote[uuid_Str] := SystemOpen @ $BearDeleteTemplate @ uuid;
DeleteBearNote[list_List] := Scan[DeleteBearNote, list];

$BearDeleteTemplate = StringFunction @ "bear://x-callback-url/trash?id=#1";

(*************************************************************************************************)

PublicIOFunction[BearNoteTextCases]

PublicOption[SurroundingContextLines]

Options[BearNoteTextCases] = {
  SurroundingContextLines -> None,
  IgnoreCase -> False,
  "PostFilter" -> None
}

BearNoteTextCases[pattern_, OptionsPattern[]] := Scope[
  UnpackOptions[surroundingContextLines, ignoreCase, postFilter];
  regex = checkLineRE @ ToRegularExpression @ toLHSPattern @ pattern;
  If[!StrQ[regex], ReturnFailed[]];
  allText = BearNoteData["Text" -> SContainsQ[RegularExpression @ regex], "Text"];
  Catenate @ doPostFilter[postFilter] @ StringLineCases[allText, pattern, surroundingContextLines, IgnoreCase -> ignoreCase]
];

BearNoteTextCases[pattern_, prop_Str, OptionsPattern[]] := Scope[
  UnpackOptions[surroundingContextLines, ignoreCase, postFilter];
  regex = checkLineRE @ ToRegularExpression @ toLHSPattern @ pattern;
  If[!StrQ[regex], ReturnFailed[]];
  data = BearNoteData["Text" -> SContainsQ[RegularExpression @ regex], {"Text", prop}];
  If[!ListQ[data], ReturnFailed[]];
  If[data === {}, Return[Assoc[]]];
  Decases[Assoc @ Map[#[prop] -> doPostFilter[postFilter][StringLineCases[#Text, pattern, surroundingContextLines, IgnoreCase -> ignoreCase]]&, data], {}]
];

doPostFilter[None] := Id;
doPostFilter[patt_][res_List] := res /. s_Str /; SFreeQ[s, patt] -> Nothing;

toLHSPattern = Case[
  l_List                      := Alt @@ Map[%, l];
  r_Rule | r_RuleDelayed      := F @ r;
  other_                      := other;
];

(*************************************************************************************************)

PublicIOFunction[CreateBlankBearNote]

CreateBlankBearNote[list_List, tagLine_String] :=
  Map[CreateBlankBearNote[#, tagLine]&, list];

CreateBlankBearNote[title_String, tagLine_Str] := Scope[
  alias = FirstStringCase[title, "(" ~~ a___ ~~ ")" :> a];
  If[StrQ[alias], title = STrim @ SDelete[title, "(" ~~ ___ ~~ ")"]];
  lines = {
    tagLine,
    If[StrQ[alias], "#alias " <> alias, Nothing],
    "#meta/blank"
  };
  If[StrQ[alias], CreateBearNote[alias, "#alias"]];
  content = SRiffle[lines, "\n\n"];
  CreateBearNote[title, content, DryRun -> False]
]

(*************************************************************************************************)

PublicIOFunction[BearFindMasterNote]

BearFindMasterNote::multipleMasters = "Multiple master pages exist for tag ``: ``."
BearFindMasterNote[tag_Str] := Scope[
  If[!SStartsQ[tag, "#"], tag = "#" <> tag];
  (* because SQLite doesn't support multiline regexp apparently? *)
  pattern = RegularExpression["(?i)#meta/master for " <> tag];
  noteData = BearNoteData["Subtitle" -> SStartsQ[pattern], {"Title", "Subtitle"}];
  If[noteData === {}, Return @ None];
  pattern = "#meta/master for " ~~ tag ~~ ("\n" | " " | "." | ",");
  noteData = Select[noteData, SContainsQ[#Subtitle, pattern, IgnoreCase -> True]&];
  notes = Key["Title"] /@ noteData;
  If[notes === {}, None,
    If[Len[notes] > 1, Message[BearFindMasterNote::multipleMasters, tag, notes]];
    F @ notes
  ]
];

(*************************************************************************************************)

PublicIOFunction[BearTags]

BearTags[] := EntityValue[$BearTagEntity, "ZTITLE"];
BearTags[pattern_] := Select[BearTags[], SMatchQ[pattern]];

(*************************************************************************************************)

PublicFunction[DeleteRedundantTags]

DeleteRedundantTags[tags_List] := Map[
  SRiffle[#, "/"]&,
  Dedup[ReverseSortBy[Len] @ SSplit[tags, "/"], PrefixQ]
];

(* this seems to work as well as the old code, though the order is different:
removeDupPrefix[list_] := Select[DeleteDuplicates @ list, elem |-> NoneTrue[DeleteCases[elem] @ list, other |-> StringStartsQ[other, elem]]];
*)

(*************************************************************************************************)

PublicIOFunction[BearSubstituteMasterPages]

Options[BearSubstituteMasterPages] = {
  Verbose -> Auto,
  DryRun -> True,
  MaxItems -> Inf,
  "IgnoreOrphanTags" -> False
};

BearSubstituteMasterPages::orphaned = "The following tags are orphaned: ``.";

BearSubstituteMasterPages[tag_Str, OptionsPattern[]] := Scope[
  UnpackOptions[$verbose, $dryRun, maxItems, ignoreOrphanTags];
  SetAutomatic[$verbose, $dryRun];
  tag = StringTrimLeft[tag, "#"];
  tags = BearTags[tag];
  tags = Take[tags, UpTo[maxItems]];
  tags = "#" <> #& /@ tags;
  VPrint["Found tags: ", tags];
  masters = BearFindMasterNote /@ tags;
  VPrint["Found master notes: ", masters];
  paired = AssocThread[tags, masters];
  orphaned = Keys @ Select[paired, EqualTo[None]];
  If[ignoreOrphanTags,
    VPrint["Ignoring orphaned tags: ", orphaned];
    paired //= Select[StrQ];
  ,
    If[Len[orphaned] > 0, ReturnFailed["orphaned", orphaned]];
  ];
  VPrint["Updating master pages."];
  rules = KVMap[updateMasterPage, paired];
  VPrint["Rules: ", rules];
  VPrint["Replacing all tag instances."];
  GlobalBearStringReplace[rules, Verbose -> $verbose, DryRun -> $dryRun];
  paired
];

parentTag[tag_] := SRiffle[Most @ SSplit[tag, "/"], "/"];

updateMasterPage[tag_, master_] := (
  VPrint["Rewriting page \"", master, "\""];
  TextReplaceBearNote[master, ("#meta/master for " <> tag) -> parentTag[tag],
    Verbose -> $verbose, DryRun -> $dryRun
  ];
  tag -> ("[[" <> master <> "]]")
);

(**************************************************************************************************)

PublicIOFunction[ReplaceBareBareLinks]

ReplaceBareBareLinks[domain_Str] := Scope[
  data = BearNoteData["Text" -> SStartsQ["[" ~~ ___ ~~ "](https://" <> domain], {"UUID", "Text"}];
  Map[updateBareNote[#UUID, #Text]&, data]
];

updateBareNote[uuid_, text_] := Scope[
  url = FirstStringCase[text, "(https://" ~~ ShortBlank ~~ ")"];
  If[!StrQ[url], ReturnFailed[]];
  url = STake[url, {2, -2}];
  md = ImportHTMLToMarkdown @ url;
  If[!StrQ[md], ReturnFailed[]];
  ReplaceBearNote[uuid, md]
];

(**************************************************************************************************)

PublicTypesettingForm[BearTitleForm]

DefineStandardTraditionalForm[{
  BearTitleForm[s_Str]        :> ToBoxes @ Style[ClickForm[s, OpenBearNote @ s], Background -> GrayLevel[0.95]],
  BearTitleForm[list:{__Str}] :> ToBoxes @ Multicolumn[BearTitleForm /@ list, 3]
}]

(**************************************************************************************************)

PublicIOFunction[BearNoteAliasRules]

(* note: this only creates rules for actually existing alias pages. make this an option? *)
BearNoteAliasRules[] := Scope[
  aliasTitles = BearNoteData["Subtitle" -> "#alias", "Title"];
  titleToAliases = F /@ BearNoteTextCases["aliases:: ", "Title", SurroundingContextLines -> 0];
  alts = Alt @@ aliasTitles;
  results = Map[SCases[alts], titleToAliases];
  Flatten @ KVMap[Thread[#2 -> #1]&, Decases[results, {}]]
];

(*************************************************************************************************)

PublicIOFunction[BearNoteTaggedTitles, CachedBearNoteTaggedTitles]

CacheVariable[$BearNoteTaggedTitlesCache]

BearNoteTaggedTitles[tag_Str] := Scope[
  data = BearNoteData["PrimaryTag" -> tag, {"Title", "Text"}];
  If[data === {}, Return @ {}];
  titles = Lookup[data, "Title"];
  aliases = Flatten @ Map[
    FirstStringCase[#Text, StartOfLine ~~ "aliases:: " ~~ lf:LineFragment :> ParseNoteAliases[lf], Nothing]&,
    data
  ];
  result = Dedup @ Join[titles, aliases];
  $BearNoteTaggedTitlesCache[tag] ^= result;
  result
];

CachedBearNoteTaggedTitles[tag_Str] :=
  CachedInto[$BearNoteTaggedTitlesCache, tag, BearNoteTaggedTitles[tag]];

(*************************************************************************************************)

PublicIOFunction[FilterBearNotesByTag]

$BearNoteTagCache = UAssoc[];

FilterBearNotesByTag[{}, _] := {};

FilterBearNotesByTag[titles_List, tag_Str] := Scope[
  knownTitles = CachedBearNoteTaggedTitles @ tag;
  found = Inter[knownTitles, titles];
  doesMatch = ConstantAssociation[found, True];
  remaining = Discard[titles, doesMatch];
  If[remaining =!= {},
    results = BearNoteData[{"Title" -> SMatchQ[remaining], "PrimaryTag" -> tag}, "Title"];
    If[results =!= {},
      $BearNoteTaggedTitlesCache[tag] ^= Union[knownTitles, results];
      AssociateTo[doesMatch, Thread[results -> True]];
    ];
  ];
  Select[titles, doesMatch]
];

(*************************************************************************************************)

PublicIOFunction[TaggedTextCases]

(* this preserves the order that entities occur in text *)
TaggedTextCases[text_Str, tag_Str] := Scope[
  knownTitles = CachedBearNoteTaggedTitles @ tag;
  patt = link:MarkdownNoteLinkPattern :> $link[STake[link, {3, -3}]];
  If[Len[knownTitles] > 0,
    patt = {patt, WordBoundary ~~ (Alt @@ knownTitles) ~~ WordBoundary}];
  matches = SCases[text, patt];
  linkMatches = Cases[matches, $link[s_] :> s];
  validatedLinkMatches = FilterBearNotesByTag[linkMatches, tag];
  matches = matches /. $link[s_] :> If[MemberQ[validatedLinkMatches, s], s, Nothing];
  Dedup @ matches
];
