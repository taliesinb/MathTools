If[!MemberQ[$ContextPath, "QuiverGeometry`"],
  QuiverGeometryLoader`$FastLoad = True;
  Get[FileNameJoin[{FileNameDrop @ $InputFileName, "init.m"}]]
,
  QuiverGeometryLoader`LoadSource[False, True, True];
];

BeginPackage["QuiverGeometryWatcher`"];

InitWatcher
PollWatcher
$WatcherStream

Begin["`Private`"];

(* we can sometimes trigger inside the Initialization of a spurious notebook... so detect this and do nothing.
but maybe that is solved now... i was accidentally evaluating the original notebook sometimes *)
InitWatcher[] := If[MemberQ[$Packages, "QuiverGeometryLoader`"],
  If[Head[$WatcherStream] =!= InputStream, QuiverGeometry`FSWatch[All]];
  SetOptions[EvaluationNotebook[], NotebookDynamicExpression :> Refresh[PollWatcher[], UpdateInterval -> 0.5]];
];

PollWatcher[] := Block[
  {event, nb},
  If[Head[$WatcherStream] =!= InputStream, $WatcherStream = QuiverGeometry`FSWatch[All]];
  If[QuiverGeometryLoader`$CurrentlyLoading, Return[]];
  event = ReadString[$WatcherStream, TimeConstraint -> 0];
  If[!StringQ[event], Return[]];
  event //= StringTrim;
  If[MatchQ[FileNameTake @ event, "Loader.m" | "Watcher.m"], Return[]];
  If[!QuiverGeometryLoader`LoadSource[False, True, True], Return[]];
  FrontEndExecute[FrontEndToken[EvaluationNotebook[], "EvaluateInitialization"]];
  goodBeep[];
];

If[$OperatingSystem === "MacOSX",
goodBeep[] := Run["afplay /System/Library/Sounds/Morse.aiff&"],
goodBeep[] := Beep[];
];

End[];
End[];
