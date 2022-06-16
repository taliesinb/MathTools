PackageScope["$CacheDirectory"]

$CacheDirectory = FileNameJoin[{ParentDirectory @ QuiverGeometryPackageLoader`$Directory, "Data"}];

(**************************************************************************************************)

PackageScope["CacheFilePath"]

CacheFilePath[name_, args___] :=
  CacheFilePath[name, args, FileExtension -> "mx"]

CacheFilePath[name_, args___, FileExtension -> ext_] :=
  FileNameJoin[{$CacheDirectory, name, StringJoin[Riffle[toCacheArgString /@ Flatten[{args}], "_"], ".", ext]}];

toCacheArgString = Case[
  data:(_List | _Association | _SparseArray) := Base36Hash @ data;
  graph_Graph := Base36Hash @ VertexEdgeList @ graph;
  other_ := TextString @ other;
];

(**************************************************************************************************)

PackageScope["EnsureExport"]

EnsureExport[filepath_, expr_] := Scope[
  If[!FileExistsQ[filepath],
    dir = DirectoryName @ filepath;
    If[!FileExistsQ[dir], EnsureDirectory @ dir];
    Export[filepath, expr];
  ];
  expr
];

(**************************************************************************************************)

PackageExport["CopyUnicodeToClipboard"]

CopyUnicodeToClipboard[text_] := Scope[
  out = FileNameJoin[{$TemporaryDirectory, "temp_unicode.txt"}];
  Export[out, text, CharacterEncoding -> "UTF-8"];
  Run["osascript -e 'set the clipboard to ( do shell script \"cat " <> out <> "\" )'"];
  DeleteFile[out];
];

(**************************************************************************************************)

PackageExport["ExportUTF8WithBackup"]

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

PackageExport["AbsolutePathQ"]

AbsolutePathQ = Case[
  s_String /; $SystemID === "Windows" := StringStartsQ[s, LetterCharacter ~~ ":\\"];
  s_String := StringStartsQ[s, $PathnameSeparator | "~"];
  _ := False;
];

(**************************************************************************************************)

PackageExport["NormalizePath"]

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

PackageScope["ToFileName"]

ToFileName[""|None, ""|None] :=
  $Failed;

ToFileName[""|None, file_String] :=
  NormalizePath @ file;

ToFileName[base_String, file_String] :=
  NormalizePath @ FileNameJoin[{base, file}];