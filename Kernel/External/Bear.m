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
  StringJoin["[[", title, "]]"]
];

General::badBearTitle = "Note should start with a H1-style title."
CreateBearNote[contents_Str, opts:OptionsPattern[]] := Scope[
  If[!StringStartsQ[contents, "# "], ReturnFailed["badBearTitle"]];
  If[StringContainsQ[contents, "\n"],
    {title, contents} = StringSplit[contents, "\n", 2],
    title = contents; contents = "";
  ];
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

ReplaceBearNote::noNoteExisting = "Cannot replace note with existing title `` since it does not exist.";
ReplaceBearNote[id_Str, contents_Str] := Scope[
  If[!StringStartsQ[contents, "# "], ReturnFailed["badBearTitle"]];
  If[!BearNoteUUIDQ[id],
    {oldTitle, id} = {id, FindBearNote @ id};
    If[!StringQ[id], ReturnFailed["noNoteExisting", oldTitle]];
  ];
  SystemOpen @ $ReplaceBearNoteTemplate[id, URLEncode @ contractTextHeader @ StringTrim @ contents];
]

_ReplaceBearNote := BadArguments[];

$ReplaceBearNoteTemplate = StringFunction @ "bear://x-callback-url/add-text?id=#1&mode=replace_all&exclude_trashed=true&show_window=no&open_note=no&text=#2";

(*************************************************************************************************)

contractTextHeader[str_Str] := If[StringStartsQ[str, LineFragment ~~ "\n\n"], StringReplace[str, "\n\n" -> "\n", 1], str];

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
  With[{field = If[VectorQ[titlesOrIDs, BearNoteUUIDQ], "ZUNIQUEIDENTIFIER",
    If[AnyTrue[titlesOrIDs, BearNoteUUIDQ], ThrowMessage["bearUUIDTitleMix", titlesOrIDs]]; "ZTITLE"]},
    EntityFunction[z, MemberQ[titlesOrIDs, z[field]] && (z["ZTRASHED"] == 0)]
  ]
];

(*************************************************************************************************)

PublicFunction[TextReplaceBearNote]

Options[TextReplaceBearNote] = {
  Verbose -> Automatic,
  DryRun -> False,
  VerifyResult -> False
}

TextReplaceBearNote[titleOrID_Str, replacements_, opts:OptionsPattern[]] :=
  TransformBearNote[titleOrID, StringReplace[replacements], opts];

(*************************************************************************************************)

PublicFunction[TransformBearNote]

PublicOption[VerifyResult, TransformTitle]

Options[TransformBearNote] = {
  Verbose -> Automatic,
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
    {title, uuid} = First[EntityValue[toTitleOrIDQuery @ titleOrID, {"ZTITLE", "ZUNIQUEIDENTIFIER"}], {None, None}];
    If[BearNoteUUIDQ @ titleOrID, SetNone[uuid, titleOrID], SetNone[title, titleOrID]];
    VPrint["Transforming Bear note with title \"", title, "\" and UUID \"", uuid, "\""];
  ];

  res = EntityValue[toTitleOrIDQuery @ titleOrID, {"ZUNIQUEIDENTIFIER", "ZTEXT"}];
  If[!MatchQ[res, {{_Str, _Str}}],
    If[Length[res] > 1, ReturnFailed["multiple", titleOrID]];
    ReturnFailed["notfound", titleOrID]];
  {uuid, oldText} = P1 @ res;

  {oldTitle, oldBody} = splitTitleBody @ oldText;

  input = If[transformTitle, oldText, oldBody];
  result = Check[fn[input], $Failed];
  If[!StringQ[result], ReturnFailed["badtresult", titleOrID]];

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
  {title, text} = StringSplit[str, "\n", 2];
  title = StringTrimLeft[title, "# "];
  {title, text}
];

$TextReplaceBearNoteTemplate = StringFunction @ "bear://x-callback-url/add-text?id=#1&mode=replace&exclude_trashed=true&show_window=no&open_note=no&text=#2";

(*************************************************************************************************)

PublicFunction[FindDuplicateBearNotes]

FindDuplicateBearNotes[] := Scope[
  data = BearNoteData[All, {"Title", "UUID"}];
  groups = GroupBy[data, Key["Title"] -> Key["UUID"]];
  Select[groups, Length[#] > 1&]
];

(*************************************************************************************************)

PublicFunction[RenameBearNote]

RenameBearNote::notfound = "Cannot find existing note with ID or title \"``\".";
RenameBearNote::badtresult = "Result of transform on \"``\" was not a string or issued messages.";
RenameBearNote::trimTitleFail = "Could not trim title from body for \"``\".";

RenameBearNote[old_Str, old_Str] := None;

RenameBearNote[old_Str, newTitle_Str, fn_:None] := Scope[
  res = EntityValue[toTitleOrIDQuery @ old, {"ZUNIQUEIDENTIFIER", "ZTITLE", "ZTEXT"}];
  If[!MatchQ[res, {{_Str, _Str, _Str}}], ReturnFailed["notfound", old]];
  {uuid, oldTitle, text} = P1 @ res;
  newText = StringTrimLeft[text, ("# " <> oldTitle) | oldTitle];
  If[StringLength[text] == StringLength[newText], ReturnFailed["trimTitleFail", old]];
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
  uuid
]

_RenameBearNote := BadArguments[];

(*************************************************************************************************)

PublicFunction[GlobalBearStringReplace]

PublicOption[MaxFailures]

Options[GlobalBearStringReplace] = {
  Verbose -> Automatic,
  DryRun -> False,
  VerifyResult -> True,
  IgnoreCase -> False,
  TransformTitle -> True,
  MaxFailures -> Infinity
};

GlobalBearStringReplace::badpatt = "Not a valid pattern: ``."
GlobalBearStringReplace::notfound = "No notes containing regex pattern \"``\".";
GlobalBearStringReplace::failures = "The following `` notes failed the transform: ``.";

GlobalBearStringReplace[rules_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[$verbose, $dryRun, verifyResult, ignoreCase, transformTitle, maxFailures];
  SetAutomatic[$verbose, $dryRun];

  rules //= ToList;
  lhsPattern = P1 /@ rules;
  lhsPattern = If[Len[lhsPattern] === 1, P1 @ lhsPattern, Alternatives @@ lhsPattern];
  regex = ToRegularExpression @ lhsPattern;
  If[TrueQ @ ignoreCase, regex = StringInsert[regex, "i", 3]];
  If[!StringQ[regex], ReturnFailed["badpatt", regex]];

  VPrint["Searching against regexp \"", regex, "\""];
  With[{re = regex}, query = EntityFunction[z, StringContainsQ[z["ZTEXT"], RegularExpression @ re] && (z["ZTRASHED"] == 0)]];
  uuids = EntityValue[FilteredEntityClass[$BearNoteEntity, query], "ZUNIQUEIDENTIFIER"];
  If[!MatchQ[uuids, {__Str}], ReturnFailed["notfound", regex]];
  VPrint["Obtained ", Len @ uuids, " results."];

  fn = StringReplace[rules, IgnoreCase -> ignoreCase];
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

PublicFunction[BearUUIDToTitle]

BearUUIDToTitle[{}] := {};

BearUUIDToTitle[$Failed] := $Failed;

BearUUIDToTitle[id_Str] := Scope[
  query = FilteredEntityClass[$BearNoteEntity, EntityFunction[z, (z["ZUNIQUEIDENTIFIER"] == id) && (z["ZTRASHED"] == 0)]];
  res = EntityValue[query, "ZTITLE"];
  First[res, $Failed]
];

BearUUIDToTitle[ids_List] := Scope[
  query = With[{ids2 = Select[ids, StringQ]}, FilteredEntityClass[$BearNoteEntity, EntityFunction[z, MemberQ[ids2, z["ZUNIQUEIDENTIFIER"]] && (z["ZTRASHED"] == 0)]]];
  pairs = EntityValue[query, {"ZUNIQUEIDENTIFIER", "ZTITLE"}];
  rules = MapApply[#1 -> #2&, pairs];
  Lookup[rules, ids, $Failed]
];

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

PublicFunction[BearNoteExistsQ]

entitiesExist[entity_, entityFn_] := FilteredEntityClass[entity, entityFn]["EntityCount"] > 0;

BearNoteExistsQ[title_Str] :=
  entitiesExist[$BearNoteEntity, EntityFunction[z, (z["ZTITLE"] == title) && (z["ZTRASHED"] == 0)]];

BearNoteExistsQ[title_Str, IgnoreCase -> True] :=
  entitiesExist[$BearNoteEntity, EntityFunction[z, StringMatchQ[z["ZTITLE"], title, IgnoreCase -> True] && (z["ZTRASHED"] == 0)]];

BearNoteExistsQ[{}] := {};
BearNoteExistsQ[titles:{___Str}] := Scope[
  class = FilteredEntityClass[$BearNoteEntity, EntityFunction[z, MemberQ[titles, z["ZTITLE"]] && (z["ZTRASHED"] == 0)]];
  foundTitles = EntityValue[class, "ZTITLE"];
  MemberQ[foundTitles, #]& /@ titles
];

(*************************************************************************************************)

PublicFunction[FindBearNote]

Options[FindBearNote] = {IgnoreCase -> True};

FindBearNote[title_Str, OptionsPattern[]] := Scope[
  UnpackOptions[ignoreCase];
  query = If[ignoreCase,
    EntityFunction[z, StringMatchQ[z["ZTITLE"], title, IgnoreCase -> True] && (z["ZTRASHED"] == 0)],
    EntityFunction[z, (z["ZTITLE"] == title) && (z["ZTRASHED"] == 0)]
  ];
  res = EntityValue[FilteredEntityClass[$BearNoteEntity, query], "ZUNIQUEIDENTIFIER"];
  If[MatchQ[res, {_Str}], P1 @ res, $Failed]
];

(*************************************************************************************************)

PublicFunction[BearNoteTitles]

Options[BearNoteTitles] = {
  IgnoreCase -> True
}

globToPattern := Case[
  glob_Str    := StringReplace[glob, "*" -> ___];
  globs_List  := Map[%, globs];
];

$strOrListP = _Str | {__Str};

toGlobMatcher[patt_, False] := StringMatchQ[globToPattern @ patt];
toGlobMatcher[patt_, True] := StringMatchQ[globToPattern @ patt, IgnoreCase -> True];

BearNoteTitles[title:$strOrListP, OptionsPattern[]] :=
  BearNoteData["Title" -> toGlobMatcher[title, OptionValue[IgnoreCase]], "Title"];

BearNoteTitles[title:$strOrListP, subtitle:$strOrListP, OptionsPattern[]] :=
  BearNoteData[{"Title" -> toGlobMatcher[title, OptionValue[IgnoreCase]], "Subtitle" -> toGlobMatcher[subtitle, OptionValue[IgnoreCase]]}, "Title"];

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
  If[!MatchQ[res, {_Str}], ReturnFailed[]];
  P1 @ res
];

(*************************************************************************************************)

PublicFunction[BearNoteUUIDQ]

"30A395FF-FF7D-4B9D-97B5-FB29E3C9A8B2"
$BearNoteUUIDRegEx = RegularExpression["[A-Z0-9]{8}(?:-[A-Z0-9]{4}){3}-[A-Z0-9]{12}(?:-[A-Z0-9]{3,6}-[A-Z0-9]{16})?"];
BearNoteUUIDQ[s_Str] := StringMatchQ[s, $BearNoteUUIDRegEx];
BearNoteUUIDQ[_] := False;

(*************************************************************************************************)

PublicFunction[BearNoteData]

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
  MaxItems -> Infinity,
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
  If[maxItems =!= Infinity, class = SampledEntityClass[class, maxItems]];
  isAll = field === All;
  If[isAll, field = $knownBearFields];
  isKeyed = MatchQ[field, _Rule];
  If[isKeyed,
    isList = ListQ[Values @ field];
    field = ToList[Keys @ field, Values @ field]];
  Block[{Databases`Database`$DBQueryLogger = If[TrueQ @ $verbose, (VPrint["SQL Query: ", Pane @ #]; #)&, Identity]},
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
      AssociationThread[field, result],
      AssociationThread[field, #]& /@ result
    ];
  ];
  If[isKeyed,
    result = AssociationThread[keys, result]
  ];
  result
]

$masterTagPrefix = Maybe["#meta/master" ~~ Maybe[" for"] ~~ WhitespaceCharacter..];

toBearClass = Case[
  list:{__Rule}                 := combineEntityFunctions @ Part[Map[toBearClass, list], All, 2];
  All                           := $BearNoteEntity;
  part:(_Int | _Span)           := Part[EntityList[$BearNoteEntity], part];
  "Text" -> (p:(_Str | _RegularExpression)) := %["Text" -> StringContainsQ[p]];
  "Tag" -> tag_                 := %["Subtitle" -> StringContainsQ[toRegexp @ toStrPatt @ tag]];
  "PrimaryTag" -> tag_          := %["Subtitle" -> StringStartsQ[toRegexp[$masterTagPrefix ~~ toStrPatt[tag]]]];
  field_ -> True                := %[field -> 1];
  field_ -> False               := %[field -> 0];
  field_ -> p_StringExpression  := %[field -> checkLineRE @ RegularExpression[ToRegularExpression @ p]];
  field_ -> r_RegularExpression := %[field -> StringMatchQ[checkLineRE @ r]];
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
  strs:{__Str} := Apply[Alternatives, strs];
  str_Str      := str;
  other_       := other;
];

badReQ[re_Str] := StringContainsQ[StringReplace[re, "[^" -> "["], "^"|"$"];

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

combineEntityFunctions[fns:{HoldPattern[__EntityFunction]}] :=
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
  fields_List := Lookup[$bearFieldDict, fields, ThrowMessage["unknownBearField", First @ Complement[fields, $knownBearFields], MsgExpr @ $knownBearFields]];
];

(*************************************************************************************************)

PublicFunction[BearNoteLookup]

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

  First @ res
];

BearNoteLookup[idList:{__Str}, fieldSpec_] := Scope @ CatchMessage[

  query = toTitleOrIDQuery @ idList;
  If[FailureQ[query], ReturnFailed[]];
  keyField = If[BearNoteUUIDQ @ First @ idList, "ZUNIQUEIDENTIFIER", "ZTITLE"];

  fieldTuple = ToList[keyField, lookupBearField @ fieldSpec];
  resTuples = Quiet @ Check[EntityValue[query, fieldTuple], None];
  If[resTuples === None, Return @ ConstantArray[$Failed, {Len @ idList, Len @ fieldTuple}]];
  (* ^ work around bug in EntityValue when no results and repeated fields *)

  If[!ListVectorQ[resTuples], ReturnFailed[]];
  resAssoc = Assoc @ Map[First[#] -> Rest[#]&, resTuples];

  If[Len[resAssoc] > Len[idList], Message[BearNoteLookup::multipleResults, idList]];
  res = Lookup[resAssoc, idList, Repeat[$Failed, Len @ fieldTuple]];

  If[StringQ[fieldSpec], PA1 @ res, res]
];

BearNoteLookup[other_, _] := (Message[BearNoteLookup::firstArg, MsgExpr @ other]; $Failed)

(*************************************************************************************************)

PublicFunction[BearNoteAttachmentData]

BearNoteAttachmentData[id_Int] := Scope[
  class = FilteredEntityClass[$BearFileEntity, EntityFunction[z, z["ZNOTE"] == id]];
  attachments = EntityValue[class, {"ZFILENAME", "ZCREATIONDATE", "ZINSERTIONDATE", "ZUPLOADEDDATE", "ZNORMALIZEDFILEEXTENSION", "ZUNIQUEIDENTIFIER"}];
  If[!ListQ[attachments] || Length[attachments] == 0, ReturnFailed[]];
  Assoc @ MapApply[
    #1 -> Assoc["FileName" -> #1, "CreationDate" -> #2, "InsertionDate" -> #3, "UploadDate" -> #4, "Extension" -> #5, "UUID" -> #6]&,
    attachments
  ]
];

(*************************************************************************************************)

PublicFunction[BearPeople]

BearPeople[] := BearNoteTaggedTitles["#person"];

(*************************************************************************************************)

PublicFunction[DeleteBearNote]

DeleteBearNote[uuid_Str] := SystemOpen @ $BearDeleteTemplate @ uuid;
DeleteBearNote[list_List] := Scan[DeleteBearNote, list];

$BearDeleteTemplate = StringFunction @ "bear://x-callback-url/trash?id=#1";

(*************************************************************************************************)

PublicFunction[BearNoteTextCases]

PublicOption[SurroundingContextLines]

Options[BearNoteTextCases] = {
  SurroundingContextLines -> None,
  IgnoreCase -> False,
  "PostFilter" -> None
}

BearNoteTextCases[pattern_, OptionsPattern[]] := Scope[
  UnpackOptions[surroundingContextLines, ignoreCase, postFilter];
  regex = checkLineRE @ ToRegularExpression @ toLHSPattern @ pattern;
  If[!StringQ[regex], ReturnFailed[]];
  allText = BearNoteData["Text" -> StringContainsQ[RegularExpression @ regex], "Text"];
  Catenate @ doPostFilter[postFilter] @ StringLineCases[allText, pattern, surroundingContextLines, IgnoreCase -> ignoreCase]
];

BearNoteTextCases[pattern_, prop_Str, OptionsPattern[]] := Scope[
  UnpackOptions[surroundingContextLines, ignoreCase, postFilter];
  regex = checkLineRE @ ToRegularExpression @ toLHSPattern @ pattern;
  If[!StringQ[regex], ReturnFailed[]];
  data = BearNoteData["Text" -> StringContainsQ[RegularExpression @ regex], {"Text", prop}];
  If[!ListQ[data], ReturnFailed[]];
  If[data === {}, Return[Assoc[]]];
  DeleteCases[Assoc @ Map[#[prop] -> doPostFilter[postFilter][StringLineCases[#Text, pattern, surroundingContextLines, IgnoreCase -> ignoreCase]]&, data], {}]
];

doPostFilter[None] := Identity;
doPostFilter[patt_][res_List] := res /. s_Str /; StringFreeQ[s, patt] -> Nothing;

toLHSPattern = Case[
  l_List                      := Alternatives @@ Map[%, l];
  r_Rule | r_RuleDelayed      := P1 @ r;
  other_                      := other;
];

(*************************************************************************************************)

PublicFunction[CreateBlankBearNote]

CreateBlankBearNote[list_List, tagLine_String] :=
  Map[CreateBlankBearNote[#, tagLine]&, list];

CreateBlankBearNote[title_String, tagLine_Str] := Scope[
  alias = FirstStringCase[title, "(" ~~ a___ ~~ ")" :> a];
  If[StringQ[alias], title = StringTrim @ StringDelete[title, "(" ~~ ___ ~~ ")"]];
  lines = {
    tagLine,
    If[StringQ[alias], "#alias " <> alias, Nothing],
    "#meta/blank"
  };
  If[StringQ[alias], CreateBearNote[alias, "#alias"]];
  content = StringRiffle[lines, "\n\n"];
  CreateBearNote[title, content, DryRun -> False]
]

(*************************************************************************************************)

PublicFunction[BearFindMasterNote]

BearFindMasterNote::multipleMasters = "Multiple master pages exist for tag ``: ``."
BearFindMasterNote[tag_Str] := Scope[
  If[!StringStartsQ[tag, "#"], tag = "#" <> tag];
  (* because SQLite doesn't support multiline regexp apparently? *)
  pattern = RegularExpression["(?i)#meta/master for " <> tag];
  noteData = BearNoteData["Subtitle" -> StringStartsQ[pattern], {"Title", "Subtitle"}];
  If[noteData === {}, Return @ None];
  pattern = "#meta/master for " ~~ tag ~~ ("\n" | " " | "." | ",");
  noteData = Select[noteData, StringContainsQ[#Subtitle, pattern, IgnoreCase -> True]&];
  notes = Key["Title"] /@ noteData;
  If[notes === {}, None,
    If[Len[notes] > 1, Message[BearFindMasterNote::multipleMasters, tag, notes]];
    First @ notes
  ]
];

(*************************************************************************************************)

PublicFunction[BearTags]

BearTags[] := EntityValue[$BearTagEntity, "ZTITLE"];
BearTags[pattern_] := Select[BearTags[], StringMatchQ[pattern]];

(*************************************************************************************************)

PublicFunction[DeleteRedundantTags]

DeleteRedundantTags[tags_List] := Map[
  StringRiffle[#, "/"]&,
  DeleteDuplicates[ReverseSortBy[Length] @ StringSplit[tags, "/"], PrefixQ]
];

(* this seems to work as well as the old code, though the order is different:
removeDupPrefix[list_] := Select[DeleteDuplicates @ list, elem |-> NoneTrue[DeleteCases[elem] @ list, other |-> StringStartsQ[other, elem]]];
*)

(*************************************************************************************************)

PublicFunction[BearSubstituteMasterPages]

Options[BearSubstituteMasterPages] = {
  Verbose -> Automatic,
  DryRun -> True,
  MaxItems -> Infinity,
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
  paired = AssociationThread[tags, masters];
  orphaned = Keys @ Select[paired, EqualTo[None]];
  If[ignoreOrphanTags,
    VPrint["Ignoring orphaned tags: ", orphaned];
    paired //= Select[StringQ];
  ,
    If[Length[orphaned] > 0, ReturnFailed["orphaned", orphaned]];
  ];
  VPrint["Updating master pages."];
  rules = KeyValueMap[updateMasterPage, paired];
  VPrint["Rules: ", rules];
  VPrint["Replacing all tag instances."];
  GlobalBearStringReplace[rules, Verbose -> $verbose, DryRun -> $dryRun];
  paired
];

parentTag[tag_] := StringRiffle[Most @ StringSplit[tag, "/"], "/"];

updateMasterPage[tag_, master_] := (
  VPrint["Rewriting page \"", master, "\""];
  TextReplaceBearNote[master, ("#meta/master for " <> tag) -> parentTag[tag],
    Verbose -> $verbose, DryRun -> $dryRun
  ];
  tag -> ("[[" <> master <> "]]")
);

(**************************************************************************************************)

PublicFunction[ReplaceBareBareLinks]

ReplaceBareBareLinks[domain_Str] := Scope[
  data = BearNoteData["Text" -> StringStartsQ["[" ~~ ___ ~~ "](https://" <> domain], {"UUID", "Text"}];
  Map[updateBareNote[#UUID, #Text]&, data]
];

updateBareNote[uuid_, text_] := Scope[
  url = FirstStringCase[text, "(https://" ~~ ShortBlank ~~ ")"];
  If[!StringQ[url], ReturnFailed[]];
  url = StringTake[url, {2, -2}];
  md = HTMLToMarkdown @ url;
  If[!StringQ[md], ReturnFailed[]];
  ReplaceBearNote[uuid, md]
];

(**************************************************************************************************)

PublicTypesettingForm[BearTitleForm]

DefineStandardTraditionalForm[{
  BearTitleForm[s_Str]        :> ToBoxes @ Style[ClickForm[s, OpenBearNote @ s], Background -> GrayLevel[0.95]],
  BearTitleForm[list:{__Str}] :> ToBoxes @ Multicolumn[BearTitleForm /@ list, 3]
}]

(**************************************************************************************************)

PublicFunction[BearNoteAliasRules]

(* note: this only creates rules for actually existing alias pages. make this an option? *)
BearNoteAliasRules[] := Scope[
  aliasTitles = BearNoteData["Subtitle" -> "#alias", "Title"];
  titleToAliases = First /@ BearNoteTextCases["aliases:: ", "Title", SurroundingContextLines -> 0];
  alts = Alternatives @@ aliasTitles;
  results = Map[StringCases[alts], titleToAliases];
  Flatten @ KeyValueMap[Thread[#2 -> #1]&, DeleteCases[results, {}]]
];

(*************************************************************************************************)

PublicFunction[BearNoteTaggedTitles, CachedBearNoteTaggedTitles]

CacheSymbol[$BearNoteTaggedTitlesCache]

BearNoteTaggedTitles[tag_Str] := Scope[
  data = BearNoteData["PrimaryTag" -> tag, {"Title", "Text"}];
  If[data === {}, Return @ {}];
  titles = Lookup[data, "Title"];
  aliases = Flatten @ Map[
    FirstStringCase[#Text, StartOfLine ~~ "aliases:: " ~~ lf:LineFragment :> ParseNoteAliases[lf], Nothing]&,
    data
  ];
  result = DeleteDuplicates @ Join[titles, aliases];
  $BearNoteTaggedTitlesCache[tag] ^= result;
  result
];

CachedBearNoteTaggedTitles[tag_Str] :=
  CachedInto[$BearNoteTaggedTitlesCache, tag, BearNoteTaggedTitles[tag]];

(*************************************************************************************************)

PublicFunction[FilterBearNotesByTag]

$BearNoteTagCache = UAssoc[];

FilterBearNotesByTag[{}, _] := {};

FilterBearNotesByTag[titles_List, tag_Str] := Scope[
  knownTitles = CachedBearNoteTaggedTitles @ tag;
  found = Intersection[knownTitles, titles];
  doesMatch = ConstantAssociation[found, True];
  remaining = Discard[titles, doesMatch];
  If[remaining =!= {},
    results = BearNoteData[{"Title" -> StringMatchQ[remaining], "PrimaryTag" -> tag}, "Title"];
    If[results =!= {},
      $BearNoteTaggedTitlesCache[tag] ^= Union[knownTitles, results];
      AssociateTo[doesMatch, Thread[results -> True]];
    ];
  ];
  Select[titles, doesMatch]
];

(*************************************************************************************************)

PublicFunction[TaggedTextCases]

(* this preserves the order that entities occur in text *)
TaggedTextCases[text_Str, tag_Str] := Scope[
  knownTitles = CachedBearNoteTaggedTitles @ tag;
  patt = link:MarkdownNoteLinkPattern :> $link[StringTake[link, {3, -3}]];
  If[Length[knownTitles] > 0,
    patt = {patt, WordBoundary ~~ (Alternatives @@ knownTitles) ~~ WordBoundary}];
  matches = StringCases[text, patt];
  linkMatches = Cases[matches, $link[s_] :> s];
  validatedLinkMatches = FilterBearNotesByTag[linkMatches, tag];
  matches = matches /. $link[s_] :> If[MemberQ[validatedLinkMatches, s], s, Nothing];
  DeleteDuplicates @ matches
];
