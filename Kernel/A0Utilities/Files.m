PrivateVariable[$CacheDirectory]

$CacheDirectory = FileNameJoin[{ParentDirectory @ $PackageDirectory, "Data"}];

(**************************************************************************************************)

PrivateFunction[CacheFilePath]

CacheFilePath[name_, args___] :=
  CacheFilePath[name, args, FileExtension -> "mx"]

CacheFilePath[name_, args___, FileExtension -> ext_] :=
  FileNameJoin[{$CacheDirectory, name, StringJoin[Riffle[toCacheArgString /@ {args}, "_"], ".", ext]}];

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
    If[!FileExistsQ[dir], EnsureDirectory @ dir];
    Export[filepath, expr];
  ];
  expr
];

(**************************************************************************************************)

PublicFunction[CopyUnicodeToClipboard]

CopyUnicodeToClipboard[text_] := Scope[
  out = FileNameJoin[{$TemporaryDirectory, "temp_unicode.txt"}];
  Export[out, text, CharacterEncoding -> "UTF-8"];
  Run["osascript -e 'set the clipboard to ( do shell script \"cat " <> out <> "\" )'"];
  DeleteFile[out];
];

(**************************************************************************************************)

PublicFunction[ExportUTF8]

ExportUTF8[path_, string_] :=
  Export[path, string, "Text", CharacterEncoding -> "UTF-8"];

(**************************************************************************************************)

PublicFunction[ImportUTF8]

ImportUTF8[path_] :=
  Import[path, "Text", CharacterEncoding -> "UTF8"];

(**************************************************************************************************)

PrivateFunction[LocalPath]

LocalPath[args___] := FileNameJoin[{$PackageDirectory, args}];

(**************************************************************************************************)

PublicFunction[ExportUTF8WithBackup]

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
  s_String /; $SystemID === "Windows" := StringStartsQ[s, LetterCharacter ~~ ":\\"];
  s_String := StringStartsQ[s, $PathnameSeparator | "~"];
  _ := False;
];

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
  $PathnameSeparator ~~ "." :> ""
}

(**************************************************************************************************)

PrivateFunction[ToFileName]

ToFileName[""|None, ""|None] :=
  $Failed;

ToFileName[""|None, file_String] :=
  NormalizePath @ file;

ToFileName[base_String, file_String] :=
  NormalizePath @ FileNameJoin[{base, file}];