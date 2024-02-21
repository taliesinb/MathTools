PublicIOFunction[SymLink]

SymLink[source_Str, target_Str] /; $PosixQ :=
  RunTool["ln", "-s", source, target];

SymLink[source_Str, target_Str] :=
  If[FileQ[source], CopyFile[source, target], CopyDirectory[source, target]];
