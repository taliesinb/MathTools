PrivateVariable[$QGCacheDirectory]

$QGCacheDirectory = LocalPath["Data"];

(**************************************************************************************************)

PublicFunction[EnsureDirectoryShallow]

General::filenotdir = "Provided path `` should be to a directory, not a file."
General::deepnotexists = "Provided path `` does not exist and neither does its parent, so won't create."

EnsureDirectoryShallow[_] := $Failed;
EnsureDirectoryShallow[path_String | File[path_String]] := Scope[
  path //= NormalizePath;
  Which[
    FileExistsQ[path] && DirectoryQ[path],                  path,
    FileQ[path],                                            ThrowMessage["filenotdir", MsgPath @ path],
    !FileExistsQ[path] && !DirectoryQ[FileNameDrop[path]],  ThrowMessage["deepnotexists", MsgPath @ path],
    True,                                                   VPrint["Creating directory ", MsgPath @ path]; If[!$dryRun, CreateDirectory[path]]
  ]
];

(**************************************************************************************************)

PrivateFunction[CacheFilePath]

CacheFilePath[name_, args___] :=
  CacheFilePath[name, args, FileExtension -> "mx"]

CacheFilePath[name_, args___, FileExtension -> ext_] :=
  PathJoin[$QGCacheDirectory, name, StringJoin[Riffle[toCacheArgString /@ {args}, "_"], ".", ext]];

$simpleArgP = _Integer | _String | None | Infinity | False | True;

toCacheArgString = Case[
  tuple:{Repeated[$simpleArgP, {1, 3}]} := StringRiffle[tuple, {"(", "-", ")"}];
  {} := "";
  data:(_List | _Association | _SparseArray) := Base36Hash @ data;
  graph_Graph := Base36Hash @ VertexEdgeList @ graph;
  other_ := TextString @ other;
];

(**************************************************************************************************)

PrivateFunction[EnsureExport]

EnsureExport[filepath_, expr_] := Scope[
  If[!FileExistsQ[filepath],
    dir = DirectoryName @ filepath;
    If[!FileExistsQ[dir], EnsureDirectoryShallow @ dir];
    Export[filepath, expr];
  ];
  expr
];

(**************************************************************************************************)

PrivateFunction[ToNotebookPath]

ToNotebookPath = Case[
  nb_NotebookObject          := Quiet @ Check[NotebookFileName @ nb, $Failed];
  co_CellObject              := % @ ParentNotebook @ co;
  File[file_] | file_String  := If[ToLowerCase[FileExtension[file]] == "nb", NormalizePath @ file, $Failed];
  _                          := $Failed;
];

(**************************************************************************************************)

PublicSpecialFunction[CopyUnicodeToClipboard]

CopyUnicodeToClipboard[text_] := Scope[
  out = TemporaryPath["clipboard.txt"];
  Export[out, text, CharacterEncoding -> "UTF-8"];
  RunAppleScript["set the clipboard to ( do shell script \"cat " <> out <> "\" )"];
  DeleteFile[out];
];

(**************************************************************************************************)

PublicSpecialFunction[ImportJSONString]

ImportJSONString::badjson = "String `` does not appear to be valid JSON.";

ImportJSONString[str_String] := Scope[
  json = Quiet @ Check[ReadRawJSONString @ str, $Failed];
  If[FailureQ[json], ReturnFailed["badjson", MsgExpr @ str]];
  json /. Null -> None
];

_ImportJSONString := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[ImportJSON]

ImportJSON::badjson = "File `` does not appear to be valid JSON."

ImportJSON[path_String] := Scope[
  path //= NormalizePath;
  If[!FileExistsQ[path], ReturnFailed[]];
  json = Quiet @ Check[ReadRawJSONFile @ path, $Failed];
  If[FailureQ[json], Message[ImportJSON::badjson, MsgPath @ path]];
  json /. Null -> None
];

_ImportJSON := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[ExportJSON]

ExportJSON[path_String, expr_] := Scope[
  path //= NormalizePath;
  outStr = WriteRawJSONString[expr, Compact -> 4];
  outStr = StringReplace[outStr, "\\/" -> "/"];
  ExportUTF8[path, outStr]
];

_ExportJSON := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[ImportMX]

ImportMX::nefile = "File `` does not exist.";
ImportMX::fail = "File `` is corrupt.";

ImportMX[path_String] := Block[
  {System`Private`ConvertersPrivateDumpSymbol, path2 = NormalizePath @ path},
  If[FailureQ[Quiet @ Check[Get[path2], $Failed]] || Head[System`Private`ConvertersPrivateDumpSymbol] =!= HoldComplete,
    If[!FileExistsQ[path2],
      Message[ImportMX::nefile, MsgPath @ path2],
      Message[ImportMX::fail, MsgPath @ path2]
    ];
    $Failed
  ,
    First @ System`Private`ConvertersPrivateDumpSymbol
  ]
];

(**************************************************************************************************)

PublicSpecialFunction[ExportMX]

ExportMX::fail = "Could not write expression to ``.";
ExportMX[path_String, expr_] := Block[
  {System`Private`ConvertersPrivateDumpSymbol = HoldComplete[expr], path2 = NormalizePath @ path},
  If[FailureQ @ Quiet @ Check[DumpSave[path2, System`Private`ConvertersPrivateDumpSymbol], $Failed],
    Message[ExportMX::fail, MsgPath @ path2]; $Failed,
    path2
  ]
];


(**************************************************************************************************)

PublicSpecialFunction[ExportUTF8]

ExportUTF8[path_String, string_String] :=
  Export[path, string, "Text", CharacterEncoding -> "UTF-8"];

_ExportUTF8 := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[ExportUTF8Dated]

ExportUTF8Dated::badtime = "Creation and modification times should be DateObjects."

ExportUTF8Dated[path_String, string_String, creation_, modification_:Automatic] := Scope[
  path //= NormalizePath;
  Export[path, string, "Text", CharacterEncoding -> "UTF-8"];
  creation //= toDateObject;
  modification //= toDateObject;
  If[!DateDateObjectQ[creation], BadArguments[]];
  If[modification === Automatic,
    SetFileDate[path, creation]
  ,
    If[!DateDateObjectQ[modification], BadArguments[]];
    SetFileDate[path, creation];
    SetFileDate[path, modification, "Modification"];
  ];
  path
];

toDateObject = Case[
  n_ ? NumericQ := DateObject[n];
  other_        := other;
];

_ExportUTF8Dated := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[ImportUTF8]

ImportUTF8[path_String] :=
  Import[path, "Text", CharacterEncoding -> "UTF8"];

_ImportUTF8 := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[PrettyPut]

PrettyPut[expr_, path_String] := ExportUTF8[path, ToPrettifiedString @ expr];

_PrettyPut := BadArguments[];

(**************************************************************************************************)

PrivateSpecialFunction[LocalPath]

LocalPath[args___] := PathJoin[$PackageDirectory, args];

(**************************************************************************************************)

PublicSpecialFunction[ExportUTF8WithBackup]

ExportUTF8WithBackup[path_, contents_, currentContents_:Automatic] := Scope[
  If[!StringQ[contents], ReturnFailed[]];
  If[!FileExistsQ[path],
    ExportUTF8[path, contents];
  ,
    SetAutomatic[currentContents, ImportUTF8 @ path];
    If[contents =!= currentContents,
      hash = Base36Hash @ currentContents;
      cachePath = StringJoin[path, ".", hash, ".backup"];
      If[!FileExistsQ[cachePath], CopyFile[path, cachePath]];
      ExportUTF8[path, contents];
    ];
  ];
  path
];

(**************************************************************************************************)

PublicFunction[AbsolutePathQ]

AbsolutePathQ = Case[
  s_String /; $WindowsQ := StringStartsQ[s, LetterCharacter ~~ ":\\"];
  s_String := StringStartsQ[s, $PathnameSeparator | "~"];
  _ := False;
];

(**************************************************************************************************)

PublicFunction[RelativePathQ]

RelativePathQ = Case[
  s_String := !AbsolutePathQ[s];
  _ := False;
];

(**************************************************************************************************)

PublicFunction[ToAbsolutePath]

SetUsage @ "
ToAbsolutePath[path$, baseDir$] ensures path$ is an absolute path, resolving it relative to baseDir$ if necessary.
ToAbsolutePath[baseDir$] is an operator form of ToAbsolutePath.
"

ToAbsolutePath[path_String ? AbsolutePathQ, base_] := NormalizePath @ path;
ToAbsolutePath[path_String, base_String] := NormalizePath @ PathJoin[base, path];
ToAbsolutePath[None, _] := None;
ToAbsolutePath[_, _] := $Failed;
ToAbsolutePath[base_][spec_] := ToAbsolutePath[spec, base];

(**************************************************************************************************)

PublicFunction[RelativePath]

RelativePath[_, path_] := None;
RelativePath[base_String, path_] := If[StringStartsQ[path, base], StringTrim[StringDrop[path, StringLength @ base], $PathnameSeparator], None];

(**************************************************************************************************)

PublicFunction[NormalizePath]

NormalizePath = Case[
  ""            := "";
  None          := None;
  path_String   := StringReplaceRepeated[path, $pathNormalizationRules];
];

$pathNormalizationRules = {
  StartOfString ~~ "~" :> $HomeDirectory,
  $PathnameSeparator ~~ Except[$PathnameSeparator].. ~~ $PathnameSeparator ~~ ".." ~~ $PathnameSeparator :> $PathnameSeparator,
  $PathnameSeparator ~~ EndOfString :> "",
  $PathnameSeparator ~~ "." :> ""
}

(**************************************************************************************************)

PrivateFunction[ReplaceFileExtension]

ReplaceFileExtension[path_, None] :=
  PathJoin[FileNameDrop @ path, FileBaseName @ path];

ReplaceFileExtension[path_, ext_] :=
  PathJoin[FileNameDrop @ path, FileBaseName[path] <> "." <> ext];

(**************************************************************************************************)

PublicFunction[FileAge]

FileAge[path_] := UnixTime[] - UnixTime[FileDate[path]];