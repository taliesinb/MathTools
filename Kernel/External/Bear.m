PublicFunction[CreateBearNote]

Options[CreateBearNote] = {
  OverwriteTarget -> False
};

CreateBearNote::exists = "Will not create note with title \"``\" because one already exists, and OverwriteTarget -> False.";

CreateBearNote[title_String, contents_String, OptionsPattern[]] := Scope[
  UnpackOptions[overwriteTarget];
  If[!overwriteTarget && BearNoteExistsQ[title], ReturnFailed["exists", title]];
  SystemOpen @ $BearCreateTemplate[URLEncode @ title, URLEncode @ contents]
];

CreateBearNote::badtitle = "Note should start with a H1-style title."
CreateBearNote[contents_String, opts:OptionsPattern[]] := Scope[
  If[!StringStartsQ[contents, "# "], ReturnFailed["badtitle"]];
  {title, contents} = StringSplit[contents, "\n", 2];
  title = StringTrim @ StringTrimLeft[title, "# "];
  contents //= StringTrim;
  CreateBearNote[title, contents, opts]
]

$BearCreateTemplate = StringFunction @ "bear://x-callback-url/create?title=#1&text=#2";

(*************************************************************************************************)

$bearImagePath = NormalizePath @ "~/Library/Group Containers/9K33E3U3T4.net.shinyfrog.bear/Application Data/Local Files/Note Images";
bearImagePath[path_] := Import[FileNameJoin[{$bearImagePath, path}]];

(*************************************************************************************************)

$BearDBFile = NormalizePath @ "~/Library/Group Containers/9K33E3U3T4.net.shinyfrog.bear/Application Data/database.sqlite";
$BearDBRef := $BearDBRef = DatabaseReference[File[$BearDBFile]];
$BearDB := $BearDB = RelationalDatabase[$BearDBRef];

PublicFunction[RegisterBearEntities]

(*

(* this is disabled because entity seems to be basically non-functional as soon as you rename any columns. they just stop working!
even the examples in EntityStore under Scope don't work if you try EntityRegister and then EntityList on them! *)

PublicVariable[$BearDBMapping]

$bearDBColSpecs = {
  (* Bug in entity framework: Cannot use ID here, otherwise CanonicalNameProperties stops working and the whole thing fails *)
  "Z_PK"                -> "Z_PK",
  "ZCREATIONDATE"       -> "ZCREATIONDATE",
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
};

(*
  (* Bug in entity framework; emits e.g. EntityFunction::invprop: Unknown property ZCREATIONDATE in EntityFunction. *)
  "CreationDate"        -> "ZCREATIONDATE"      -> FromUnixTime,
  "ModificationDate"    -> "ZMODIFICATIONDATE"  -> FromUnixTime,
  "HasFilesQ"           -> "ZHASFILES"          -> EqualTo[1],
  "HasImagesQ"          -> "ZHASIMAGES"         -> EqualTo[1],
  "ArchivedQ"           -> "ZARCHIVED"          -> EqualTo[1],
  "TrashedQ"            -> "ZTRASHED"           -> EqualTo[1],
  "PinnedQ"             -> "ZPINNED"            -> EqualTo[1],
 *)

toDBPropSpec = Case[
  col_String        := <|"ColumnPrefix" -> "ZSFNOTE", "ColumnName" -> col|>;
  col_String -> fn_ := <|"Function" -> EntityFunction[x, fn[x[col]]]|>;
]

$BearDBMapping := $BearDBMapping = <|"Types" -> <|"BearNote" -> <|
  "EntityTypeExtractor" -> "ZSFNOTE",
  "Properties" -> Association @ VectorApply[#1 -> toDBPropSpec[#2]&, $bearDBColSpecs],
  "CanonicalNameProperties" -> {"Z_PK"}
|>|>|>;
 *)

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

SetInitialValue[$BearNote, $BearNote2];

$BearNote2 := $BearNote2 = (Quiet @ RegisterBearEntities[]; "ZSFNOTE");

(*************************************************************************************************)

PublicFunction[BearNoteExistsQ]

BearNoteExistsQ[title_String] := FilteredEntityClass[$BearNote, EntityFunction[z, z["ZTITLE"] == title]]["EntityCount"] > 0;

BearNoteExistsQ[{}] := {};
BearNoteExistsQ[titles:{___String}] := Scope[
  class = FilteredEntityClass[$BearNote, EntityFunction[z, MemberQ[titles, z["ZTITLE"]]]];
  foundTitles = EntityValue[class, "ZTITLE"];
  MemberQ[foundTitles, #]& /@ titles
];

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
  result = EntityValue[class, Lookup[$bearFieldDict, field]];
  If[ListQ[field],
    result = If[Head[class] === Entity,
      AssociationThread[field, result],
      AssociationThread[field, #]& /@ result
    ]
  ];
  result
]

toBearClass = Case[
  list:{__Rule}                 := combineEntityFunctions @ Part[Map[toBearClass, list], All, 2];
  All                           := $BearNote;
  part:(_Integer | _Span)       := Part[EntityList[$BearNote], part];
  "Text" -> pattern_String      := %["Text" -> StringContainsQ[pattern]];
  field_ -> True                := %[field -> 1];
  field_ -> False               := %[field -> 0];
  field_ -> fn_Symbol           := makeFEC[field, fn[$Z]];
  field_ -> (fn_Symbol[value_]) := makeFEC[field, fn[value][$Z]];
  field_ -> value_              := makeFEC[field, $Z == value];
]

SetHoldRest[makeFEC];
makeFEC[field_, body_] := With[
  {prop = Lookup[$bearFieldDict, field, ThrowMessage["badfield", field]]},
  FilteredEntityClass[$BearNote, EntityFunction[z, body]] /. $Z :> z[prop]
];

combineEntityFunctions[fns:{__EntityFunction}] :=
  FilteredEntityClass[$BearNote, Construct[EntityFunction, z, (Hold @@ fns)[[All, 2]]] /. Hold -> And];

$bearFieldDict = Association[
  "ID"                  -> "Z_PK",
  "Title"               -> "ZTITLE",
  "CreationDate"        -> "ZCREATIONDATE",
  "OrderDate"           -> "ZORDERDATE",
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

PublicFunction[OpenBearNote]

OpenBearNote[uuid_String] := SystemOpen @ $BearOpenTemplate @ uuid;
OpenBearNote[list_List] := Scan[OpenBearNote, list];

$BearOpenTemplate = StringFunction @ "bear://x-callback-url/open-note?id=#1&new_window=true";

(*************************************************************************************************)

PublicFunction[DeleteBearNote]

DeleteBearNote[uuid_String] := SystemOpen @ $BearDeleteTemplate @ uuid;
DeleteBearNote[list_List] := Scan[DeleteBearNote, list];

$BearDeleteTemplate = StringFunction @ "bear://x-callback-url/trash?id=#1";
