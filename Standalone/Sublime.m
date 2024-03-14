Begin["`Sublime`"];

(*************************************************************************************************)

SublimeOpen[e_String] :=
  runSubl["-n", e];

SublimeOpen[list:{__String}] :=
  runSubl["-n", Sequence @@ list];

(*************************************************************************************************)

SublimeOpenProject[path_String] /; StringEndsQ[path, ".sublime-project"] :=
  runSubl["--project", ExpandFileName @ path];

SublimeOpenProject[path2_String] := Module[{path = path2},
  If[FileType[path] === File, path = FileNameDrop[path]];
  projFiles = FileNames["*.sublime-project", path, Infinity];
  If[projFiles === {}, $Failed, SublimeOpenProject @ First @ projFiles]
];

(*************************************************************************************************)

$SublimePath = "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl";

(* TODO: why do i have to do this if there is no shell? *)
$subpath2 = StringReplace[$SublimePath, " " -> "\\ "];

runSubl[args___] := If[Experimental`NewRun[$subpath2, args, Experimental`Shell -> None] =!= 0, $Failed, Null];

(*************************************************************************************************)

End[];