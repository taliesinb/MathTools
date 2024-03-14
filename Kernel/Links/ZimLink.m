CacheVariable[$ZimLinkFunctions]

General::libraryLinkFunctionCall = "Library link function call `` failed.";

zimCall[name_String, args___] := Rep[
  VPrint["Calling ", name[args]];
  CacheTo[$ZimLinkFunctions, name, loadFunc[name]][args],
  _LibraryFunction[___] | _LibraryFunctionError :> Msg::libraryLinkFunctionCall[name[args]]
];

General::libraryLinkFunctionLoad = "Could not load function `` from ``.";
loadFunc[name_String] := OnFailed[
  If[!FileExistsQ[$zimLinkPath], iBuildZimLink[]];
  VPrint["Loading ", name -> Lookup[$zimLinkFunctionSignatures, name]];
  LibraryFunctionLoad[$zimLinkPath, name, tospec @ LookupMsg[$zimLinkFunctionSignatures, name]]
,
  Import["!nm -gU " <> $zimLinkPath, "Text"];
  Msg::libraryLinkFunctionLoad[name, MsgPath @ $zimLinkPath]
];

$argRules = {ByteArray -> {LibraryDataType[ByteArray], "Constant"}, Str -> "UTF8String"};
$retRules = ByteArray -> LibraryDataType[ByteArray];
tospec[args_ -> ret_] := Seq[args /. $argRules, ret /. $retRules];

$zimLinkFunctionSignatures = Assoc[
  "zimGetLinkVersion"           -> {}                -> Int,
  "zimLoadedArchiveCount"       -> {}                -> Int,
  "zimOpenArchive"              -> {Int, Str}        -> "Void",
  "zimArchiveEntryData"         -> {Int, Int, Int}   -> ByteArray,
  "zimArchiveEntryRedirects"    -> {Int}             -> ByteArray,
  "zimEntryContentsByTitle"     -> {Int, Str}        -> ByteArray,
  "zimEntryContentsByTitleList" -> {Int, ByteArray}  -> ByteArray,
  "zimSearch"                   -> {Int, Str, Int}   -> ByteArray
];


(*************************************************************************************************)

$zeroSep = "\:0000";

General::zimInternalError = "Internal error."
ByteArrayToStringList[ba_ByteArray] := SSplit[ByteArrayToString @ ba, $zeroSep];
ByteArrayToStringList[_] := ThrowMessage["zimInternalError"];

(* the final "" is to ensure we have a null-terminated string at the end, so C++ can parse it easily *)
StringListToByteArray[strs_List] := StringToByteArray @ SRiffle[App[strs, ""], $zeroSep];
StringListToByteArray[_] := ThrowMessage["zimInternalError"];

(*************************************************************************************************)

PrivateFunction[ZimLoadedArchiveCount]

ZimLoadedArchiveCount[] := CatchMessage @ zimCall["zimLoadedArchiveCount"];

(*************************************************************************************************)

PrivateFunction[UnloadZimLink]

UnloadZimLink[] := If[Len[$ZimLinkFunctions] > 0,
  VPrint["Unloading ZimLink."];
  KVScan[LibraryFunctionUnload[Print["Unloading ", #1]; #2]&, $ZimLinkFunctions];
  LibraryUnload @ $zimLinkPath;
  $ZimLinkFunctions = UAssoc[];
];

(*************************************************************************************************)

PrivateIOFunction[BuildZimLink]

General::libraryLinkBuildError = "Could not load build library ``.";
BuildZimLink[] := CatchMessage @ iBuildZimLink[];

iBuildZimLink[] := Scope[
  UnloadZimLink[];
  Print["Building ZimLink."];
  If[FileExistsQ[$zimLinkPath], DeleteFile @ $zimLinkPath];
  res = Run["/usr/local/bin/wolframscript " <> $zimLinkBuilderPath];
  If[res =!= 0 || !FileExistsQ[$zimLinkPath],
    If[FileExistsQ[$zimLinkBuilderOutput], FilePrint @ $zimLinkBuilderOutput];
    Msg::libraryLinkBuildError[$zimLinkPath]];
];

$zimLinkBuilderOutput = LocalPath["LibraryLink", "ZimLink", "BuildOutput.txt"];
$zimLinkBuilderPath = LocalPath["LibraryLink", "ZimLink", "Build.wls"]
$zimLinkPath = LocalPath["LibraryLink", "ZimLink", "ZimLink.dylib"];

(*************************************************************************************************)

SystemHead[ZimArchive]

CacheVariable[$ZimArchiveIDToName]

DefineStandardTraditionalForm[
  (ZimArchive[id_Int] ? MEQ) /; KeyQ[$ZimArchiveIDToName, id] :> RowBox[{
    "\"ZimArchive\"", RowBox[{"[", tightColoredBoxes[$ZimArchiveIDToName @ id, $Yellow, 12], "]"}]
  }]
];

MEQ = ManagedLibraryExpressionQ;

(*************************************************************************************************)

PublicIOFunction[OpenZimArchive]

OpenZimArchive::zimLinkVersion = "Wrong version `` of ZimLink!";
OpenZimArchive[path_String] := Scope @ CatchMessage[
  path //= NormalizePath;
  baseName = FileBaseName @ path;
  If[!FileExistsQ[path], ReturnFailed[]];
  ZimLoadedArchiveCount[]; (* force ZimLink to load so we can call CreateManagedLibraryExpression *)
  obj = CreateManagedLibraryExpression["ZimArchive", ZimArchive];
  id = F @ obj;
  zimCall["zimOpenArchive", id, path];
  $ZimArchiveIDToName[id] ^= baseName;
  obj
];

(*************************************************************************************************)

PublicFunction[ZimEntryRedirects]

ZimEntryRedirects[ZimArchive[id_Int] ? MEQ, n_Integer:Inf] := Scope @ CatchMessage[
  bytes = zimCall["zimArchiveEntryRedirects", id];
  strs = ByteArrayToStringList @ bytes;
  If[!Divisible[Len[strs], 2], ReturnFailed[]];
  Rule @@@ Partition[strs, 2]
];

(*************************************************************************************************)

PublicFunction[ZimEntryData]

ZimEntryData[ZimArchive[id_Int] ? MEQ, allowRedirs_:False, n_Integer:Inf] := Scope @ CatchMessage[
  bytes = zimCall["zimArchiveEntryData", id, If[n === Inf, 0, n], If[allowRedirs, 0, 1]];
  strs = ByteArrayToStringList @ bytes;
  If[!Divisible[Len[strs], 2], ReturnFailed[]];
  columns = Transpose @ Partition[strs, 2];
  DataFrame[{"Path", "Title"} -> columns]
];

(*************************************************************************************************)

PublicFunction[ZimEntryContents]

ZimEntryContents[ZimArchive[id_Int] ? MEQ, title_Str] := Scope @ CatchMessage[
  bytes = zimCall["zimEntryContentsByTitle", id, title];
  ByteArrayToString @ bytes
];

ZimEntryContents[ZimArchive[id_Int] ? MEQ, titles_List] := Scope @ CatchMessage[
  bytes = StringListToByteArray @ titles;
  result = zimCall["zimEntryContentsByTitleList", id, bytes];
  ByteArrayToStringList @ result
];

(*************************************************************************************************)

PublicFunction[ZimSearch]

ZimSearch[ZimArchive[id_Int] ? MEQ, query_Str, limit_Integer:100] := Scope @ CatchMessage[
  bytes = zimCall["zimSearch", id, query, limit];
  ByteArrayToStringList @ bytes
];
