#!/usr/bin/env wolframscript

If[$OperatingSystem =!= "MacOSX", Print["Only MacOS supported."]; Exit[1]];

dir = DirectoryName @ $InputFileName;

Needs["CCompilerDriver`"];

source = FileNameJoin[{dir, "ZimLink.cpp"}];
If[!FileExistsQ[source], Print["Can't locate source file."]; Exit[2]];

buildOutput = FileNameJoin[{dir, "BuildOutput.txt"}];

If[FileExistsQ[buildOutput], DeleteFile @ buildOutput];

$brewPath = "/opt/homebrew";
If[!FileExistsQ[$brewPath], Print["Can't find homebrew path."]; Exit[3]];

$brewInc = $brewPath <> "/include";
$brewLib = $brewPath <> "/lib";

If[!FileExistsQ[$brewInc <> "/zim"], Print["Can't locate libzim files in homebrew."]; Exit[4]];

shellOutput = None;
result = CreateLibrary[{source},"ZimLink",
  "Language" -> "C++",
  "CompileOptions" -> {"-std=c++20", "-g"},
  "Libraries" -> {"zim"},
  "IncludeDirectories" ->  {$brewInc},
  "LibraryDirectories" ->  {$brewLib},
  "CleanIntermediate" -> True,
  "ShellOutputFunction" -> (Export[buildOutput, Echo @ #]&)
];

If[!StringQ[result] && !FileExistsQ[result],
  Print["Build failed, output written to BuildOutput.txt"];
  Exit[5];
];

target = FileNameJoin[{dir, FileNameTake @ result}];
Print["Copying ", result, " to ", target];
CopyFile[result, target, OverwriteTarget -> True];
Exit[0];