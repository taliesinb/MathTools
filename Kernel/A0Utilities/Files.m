PublicIOFunction[EnsureDirectoryShallow]

General::filenotdir = "Provided path `` should be to a directory, not a file."
General::deepnotexists = "Provided path `` does not exist and neither does its parent, so won't create."

EnsureDirectoryShallow[_] := $Failed;
EnsureDirectoryShallow[path_Str | File[path_Str]] := Scope[
  path //= NormalizePath;
  Which[
    FileExistsQ[path] && DirectoryQ[path],                  path,
    FileQ[path],                                            ThrowMessage["filenotdir", MsgPath @ path],
    !FileExistsQ[path] && !DirectoryQ[FileNameDrop[path]],  ThrowMessage["deepnotexists", MsgPath @ path],
    True,                                                   VPrint["Creating directory ", MsgPath @ path]; If[!$dryRun, CreateDirectory[path]]
  ]
];

(**************************************************************************************************)

PrivateCacheFunction[CacheFilePath]

CacheFilePath[name_, args___] :=
  CacheFilePath[name, args, FileExtension -> "mx"]

CacheFilePath[name_, args___, FileExtension -> ext_] :=
  DataPath[name, SJoin[Riffle[toCacheArgString /@ {args}, "_"], ".", ext]];

$simpleArgP = _Int | _Str | None | Inf | False | True;

toCacheArgString = Case[
  tuple:{Repeated[$simpleArgP, {1, 3}]} := SRiffle[tuple, {"(", "-", ")"}];
  {} := "";
  data:(_List | _Assoc | _SparseArray) := Base36Hash @ data;
  graph_Graph := Base36Hash @ VertexEdgeList @ graph;
  other_ := TextString @ other;
];

(**************************************************************************************************)

PublicIOFunction[EnsureExport]

EnsureExport[filepath_, expr_] := Scope[
  If[!FileExistsQ[filepath],
    dir = DirectoryName @ filepath;
    If[!FileExistsQ[dir], EnsureDirectoryShallow @ dir];
    Export[filepath, expr];
  ];
  expr
];

(**************************************************************************************************)

PrivateSpecialFunction[ToNotebookPath]

ToNotebookPath = Case[
  nb_NotebookObject      := Quiet @ Check[NotebookFileName @ nb, $Failed];
  co_CellObject          := % @ ParentNotebook @ co;
  File[file_] | file_Str := If[ToLowerCase[FileExtension[file]] == "nb", NormalizePath @ file, $Failed];
  _                      := $Failed;
];

(**************************************************************************************************)

PublicIOFunction[CopyUnicodeToClipboard]

(* TODO: undertake replacements of $ScriptLetters codepoints which mathematica substitutes for private use ones *)
CopyUnicodeToClipboard[text_] := Scope[
  out = TemporaryPath["clipboard.txt"];
  Export[out, text, CharacterEncoding -> "UTF-8"];
  RunAppleScript["set the clipboard to ( do shell script \"cat " <> out <> "\" )"];
  DeleteFile[out];
];

(**************************************************************************************************)

PublicIOFunction[ImportJSONString]

ImportJSONString::badjson = "Str `` does not appear to be valid JSON.";

ImportJSONString[str_Str] := Scope[
  json = Quiet @ Check[ReadRawJSONString @ str, $Failed];
  If[FailureQ[json], ReturnFailed["badjson", MsgExpr @ str]];
  json /. Null -> None
];

_ImportJSONString := BadArguments[];

(**************************************************************************************************)

PublicIOFunction[ImportJSON]

ImportJSON::badjson = "File `` does not appear to be valid JSON."

ImportJSON[path_Str] := Scope[
  path //= NormalizePath;
  If[!FileExistsQ[path], ReturnFailed[]];
  json = Quiet @ Check[ReadRawJSONFile @ path, $Failed];
  If[FailureQ[json], Message[ImportJSON::badjson, MsgPath @ path]];
  json /. Null -> None
];

_ImportJSON := BadArguments[];

(**************************************************************************************************)

PublicIOFunction[ExportJSON]

ExportJSON[path_Str, expr_] := Scope[
  path //= NormalizePath;
  outStr = WriteRawJSONString[expr, Compact -> 4];
  outStr = SRep[outStr, "\\/" -> "/"];
  ExportUTF8[path, outStr]
];

_ExportJSON := BadArguments[];

(**************************************************************************************************)

PublicIOFunction[ImportMX]

ImportMX::nefile = "File `` does not exist.";
ImportMX::fail = "File `` is corrupt.";

ImportMX[path_Str] := Block[
  {System`Private`ConvertersPrivateDumpSymbol, path2 = NormalizePath @ path},
  If[FailureQ[Quiet @ Check[Get[path2], $Failed]] || H[System`Private`ConvertersPrivateDumpSymbol] =!= HoldC,
    If[!FileExistsQ[path2],
      Message[ImportMX::nefile, MsgPath @ path2],
      Message[ImportMX::fail, MsgPath @ path2]
    ];
    $Failed
  ,
    F @ System`Private`ConvertersPrivateDumpSymbol
  ]
];

(**************************************************************************************************)

PublicIOFunction[ExportMX]

ExportMX::fail = "Could not write expression to ``.";
ExportMX[path_Str, expr_] := Block[
  {System`Private`ConvertersPrivateDumpSymbol = HoldC[expr], path2 = NormalizePath @ path},
  If[FailureQ @ Quiet @ Check[DumpSave[path2, System`Private`ConvertersPrivateDumpSymbol], $Failed],
    Message[ExportMX::fail, MsgPath @ path2]; $Failed,
    path2
  ]
];


(**************************************************************************************************)

PublicIOFunction[ExportUTF8]

ExportUTF8[path_Str ? ASCIIQ, string_Str] :=
  Export[path, string, "Text", CharacterEncoding -> "UTF-8"];

SetCached[$tempExportFile, TemporaryPath["utf_export"]];

(* works around failure on e.g. ExportUTF8["~/𝖢𝖺𝗍.txt", "Hello world this is 𝖢𝖺𝗍."] *)
ExportUTF8[path_Str, string_Str] := Scope[
  ExportUTF8[$tempExportFile, string];
  RunTool["mv", $tempExportFile, NormalizePath @ path];
  path
];

_ExportUTF8 := BadArguments[];

(**************************************************************************************************)

PublicIOFunction[ExportUTF8Dated]

ExportUTF8Dated::badtime = "Creation and modification times should be DateObjects."

ExportUTF8Dated[path_Str, string_Str, creation_, modification_:Auto] := Scope[
  path //= NormalizePath;
  Export[path, string, "Text", CharacterEncoding -> "UTF-8"];
  creation //= toDateObject;
  modification //= toDateObject;
  If[!DateObjectQ[creation], BadArguments[]];
  If[modification === Auto,
    SetFileTime[path, {creation, creation}]
  ,
    If[!DateObjectQ[modification], BadArguments[]];
    SetFileTime[path, {creation, modification}];
  ];
  path
];

toDateObject = Case[
  n_ ? NumericQ := DateObject[n];
  other_        := other;
];

_ExportUTF8Dated := BadArguments[];

(**************************************************************************************************)

PublicIOFunction[SetFileTime]

(* TODO: make the creation part work on Windows / Linux *)

SetFileTime::modificationFailed = "Modification of path `` failed.";

SetFileTime[path_, c:(_Int | _Real | _DateObject)] := SetFileTime[path, {c, None, None}];
SetFileTime[path_, {c_, m_}] := SetFileTime[path, {c, m, None}];
SetFileTime[path_Str, {None, None, None}] := Null;
SetFileTime[path_Str, cma:{_, _, _}] :=
  setFileTime[path, toCmaDate /@ cma];

setFileTime[path_] := Case[
  {None, None, None} := Null;
  {None, m_, None}   := SetFileDate[path, m, "Modification"];
  {None, None, a_}   := SetFileDate[path, m, "Access"];
  {c_, m_, None}     := setCreateModify[path, c, m];
  {c_, m_, a_}       := (setCreateModify[path, c, m]; SetFileDate[path, a, "Access"]);
];

setCreateModify[path_, c_, m_] := Module[{code},
  code = RunUTF8["SetFile ", toFTarg["-d", c], " ", toFTarg["-m", m], " ", BashEscape @ NormalizePath @ path];
  If[code =!= 0, Message[SetFileTime::modificationFailed, MsgPath @ path]];
];

toFTarg[flag_, None] := Nothing;
toFTarg[flag_, date_] := SJoin[flag, " '", sftStr[date], "'"];

sftStr = Case[
  DateObject[{y_, m_, d_}, ___] :=
    SJoin[intStr2 @ m, "/", intStr2 @ d, "/", IntStr @ y];
  DateObject[{y_, m_, d_, h_, m2_, s_, ___}, ___] :=
    SJoin[intStr2 @ m, "/", intStr2 @ d, "/", IntStr @ y, " ", intStr2 @ h, ":", intStr2 @ m2, ":", intStr2 @ Floor @ s];
  u:(_Integer | _Real) := % @ FromUnixTime @ u;
]

toCmaDate = Case[
  d_DateObject         := d;
  u:(_Integer | _Real) := FromUnixTime @ u;
  None                 := None;
]

intStr2[e_] :=  IntStr[e, 10, 2];

(**************************************************************************************************)

PublicIOFunction[ImportUTF8]

ImportUTF8[path_Str] :=
  Import[path, "Text", CharacterEncoding -> "UTF8"];

_ImportUTF8 := BadArguments[];

(**************************************************************************************************)

PublicIOFunction[PrettyPut]

PrettyPut[expr_, path_Str] := ExportUTF8[path, ToPrettifiedString @ expr];

_PrettyPut := BadArguments[];

(**************************************************************************************************)

PublicIOFunction[ExportUTF8WithBackup]

ExportUTF8WithBackup[path_, contents_, currentContents_:Auto] := Scope[
  If[!StrQ[contents], ReturnFailed[]];
  If[!FileExistsQ[path],
    ExportUTF8[path, contents];
  ,
    SetAuto[currentContents, ImportUTF8 @ path];
    If[contents =!= currentContents,
      hash = Base36Hash @ currentContents;
      cachePath = SJoin[path, ".", hash, ".backup"];
      If[!FileExistsQ[cachePath], CopyFile[path, cachePath]];
      ExportUTF8[path, contents];
    ];
  ];
  path
];

(**************************************************************************************************)

PublicFunction[AbsolutePathQ]

AbsolutePathQ = Case[
  s_Str /; $WindowsQ := SStartsQ[s, LetterCharacter ~~ ":\\"];
  s_Str := SStartsQ[s, $PathnameSeparator | "~"];
  _ := False;
];

(**************************************************************************************************)

PublicFunction[RelativePathQ]

RelativePathQ = Case[
  s_Str := !AbsolutePathQ[s];
  _ := False;
];

(**************************************************************************************************)

PublicFunction[ToAbsolutePath]

SetUsage @ "
ToAbsolutePath[path$, baseDir$] ensures path$ is an absolute path, resolving it relative to baseDir$ if necessary.
ToAbsolutePath[baseDir$] is an operator form of ToAbsolutePath.
"

ToAbsolutePath[path_Str ? AbsolutePathQ, base_] := NormalizePath @ path;
ToAbsolutePath[path_Str, base_Str] := NormalizePath @ PathJoin[base, path];
ToAbsolutePath[None, _] := None;
ToAbsolutePath[_, _] := $Failed;
ToAbsolutePath[base_][spec_] := ToAbsolutePath[spec, base];

(**************************************************************************************************)

PublicFunction[RelativePath]

RelativePath[_, path_] := None;
RelativePath[base_Str, path_] := If[SStartsQ[path, base], STrim[SDrop[path, SLen @ base], $PathnameSeparator], None];

(**************************************************************************************************)

PublicFunction[NormalizePath]

NormalizePath = Case[
  ""       := "";
  None     := None;
  path_Str := StringReplaceRepeated[path, $pathNormalizationRules];
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

(**************************************************************************************************)

PublicFunction[FileLineFind]

PublicOption[WholeWords]

General::fileNotFound = "File `` does not exist.";

Options[FileLineFind] = {
  IgnoreCase -> False,
  WholeWords -> True
};

FileLineFind[files:(_Str | {__Str}), patt_, OptionsPattern[]] := Scope[
  UnpackOptions[$ignoreCase, wholeWords];
  If[TrueQ[wholeWords], patt //= WLWholeWord];
  patt = ToRegex @ patt;
  If[FailureQ[patt], ReturnFailed[]];
  Flatten @ Map[searchFileLines[#, patt]&, ToList @ files]
];

searchFileLines[path_, patt_] := Scope[
  If[!FileExistsQ[path], Message[FileLineFind::fileNotFound, MsgPath @ path]; Return @ Nothing];
  str = ImportUTF8 @ path;
  FileLine[path, #]& /@ StringLineFind[str, patt, IgnoreCase -> $ignoreCase]
];

(**************************************************************************************************)

PublicFunction[FileStringCases]

Options[FileStringCases] = JoinOptions[
  FileLineFind,
  SurroundingContextLines -> None
];

FileStringCases[files_, patt_, OptionsPattern[]] := Scope[
  UnpackOptions[$ignoreCase, $surroundingContextLines, wholeWords];
  If[TrueQ[wholeWords], patt //= WLWholeWord];
  If[!ListQ[StringCases["", patt]], ReturnFailed[]];
  iFileStringCases[files, patt]
];

iFileStringCases[files:{__Str}, patt_] :=
  Decases[{}] @ AssocMap[iFileStringCases[#, patt]&, files]

iFileStringCases[path_Str, patt_] := Scope[
  If[!FileExistsQ[path], Message[FileCases::fileNotFound, MsgPath @ path]; Return @ {}];
  str = ImportUTF8 @ path;
  StringLineCases[str, patt, $surroundingContextLines, IgnoreCase -> $ignoreCase]
];

(**************************************************************************************************)

PublicFunction[FileStringReplace]

Options[FileStringReplace] = {
  Verbose -> Automatic,
  DryRun -> False,
  IgnoreCase -> False,
  WholeWords -> True
};

toStrPattLHS = Case[
  Rule[a_, b_]        := a;
  RuleDelayed[a_, b_] := a;
  {r_}                := % @ r;
  list_List           := Alt @@ Map[%, list];
];

FileStringReplace[files_, rewrite:($RulePattern | $RuleListPattern), OptionsPattern[]] := Scope[
  UnpackOptions[$verbose, $dryRun, $ignoreCase, wholeWords];
  SetAuto[$verbose, $dryRun];
  If[TrueQ[wholeWords], rewrite //= MapColumn[WLWholeWord, 1]];
  If[!ListQ[StringCases["", rewrite]], ReturnFailed[]];
  $lhs = ToRegex @ toStrPattLHS @ ReplaceAll[(Condition|PatternTest)[p_, _] :> p] @ rewrite;
  $rewrite = rewrite;
  Flatten @ Map[iFileStringReplace, ToList @ files]
];

FileStringReplace::fileReplaceMatchFailed = "Replacement failed on match ``.";
FileStringReplace::fileReplaceFailed = "Replacement failed on entire file ``.";

iFileStringReplace[file_] := Scope[
  If[!FileExistsQ[file], Message[FileStringReplace::fileNotFound, MsgPath @ file]; Return @ Nothing];
  text = ImportUTF8 @ file;
  spans = SFind[text, $lhs, IgnoreCase -> $ignoreCase];
  If[spans === {},
    Return @ Nothing];
  lines = ToStringLinePositions[text, spans];
  res = ZipMap[
    {span, line} |-> (
      fileLine = FileLine[file, line];
      chunk = STake[text, span];
      newChunk = StringReplace[chunk, $rewrite];
      Which[
        !StrQ[newChunk],
          Message[FileStringReplace::fileReplaceMatchFailed, MsgExpr @ chunk];
          Nothing,
        newChunk =!= chunk,
          VPrint[Pane[fileLine, {250, Automatic}], ShowSequenceAlignment[chunk, newChunk]];
          {fileLine, span, newChunk},
        True,
          VPrint[Pane[fileLine, {250, Automatic}], Style[chunk, $DarkPurple]];
          Nothing
      ]),
    spans, lines];
  If[res === {}, Return @ Nothing];
  {fileLines, spans, newChunks} = Transpose @ res;
  newText = StringReplacePart[text, newChunks, spans];
  If[!StrQ[newText],
    Message[FileStringReplace::fileReplaceMatchFailed, MsgPath @ file];
    Return[Nothing];
  ];
  whenWet[
    ExportUTF8[file, newText];
  ];
  fileLines
];