PublicFunction[SymLink]

SymLink[source_String, target_String] /; $PosixQ :=
  RunTool["ln", "-s", source, target];

SymLink[source_String, target_String] :=
  If[FileQ[source], CopyFile[source, target], CopyDirectory[source, target]];
