If[!MemberQ[$ContextPath, "MathTools`"],
  MTLoader`$FastLoad = True;
  Get[FileNameJoin[{FileNameDrop @ $InputFileName, "init.m"}]]
,
  MTLoader`LoadSource[False, True, True];
];

BeginPackage["MathToolsWatcher`"];

InitWatcher
PollWatcher
$WatcherStream

Begin["`Private`"];

(* we can sometimes trigger inside the Initialization of a spurious notebook... so detect this and do nothing.
but maybe that is solved now... i was accidentally evaluating the original notebook sometimes *)
InitWatcher[] := If[MemberQ[$Packages, "MTLoader`"],
  If[Head[$WatcherStream] =!= InputStream, MathTools`FSWatch[All]];
  SetOptions[EvaluationNotebook[], NotebookDynamicExpression :> Refresh[PollWatcher[], UpdateInterval -> 0.5]];
];

$evalCount = 0;
PollWatcher[] := Block[
  {event, nb = EvaluationNotebook[]},
  If[Head[$WatcherStream] =!= InputStream, beep[3]; $WatcherStream = MathTools`FSWatch[All]];
  If[MTLoader`$CurrentlyLoading, Return[]];
  event = ReadString[$WatcherStream, TimeConstraint -> 0];
  If[!StringQ[event], Return[]];
  event //= StringTrim;
  If[!IntegerQ[$evalCount], $evalCount = 0]; $evalCount++;
  If[MatchQ[FileNameTake @ event, "Loader.m" | "Watcher.m"], Return[]];
  If[!MTLoader`LoadSource[False, True, True], beep[2]; Return[]];
  SetOptions[nb, Background -> If[OddQ[$evalCount], GrayLevel[0.98], GrayLevel[1]]];
  FrontEndExecute[FrontEndToken[nb, "EvaluateInitialization"]];
  (* goodBeep[]; *)
];

beep[n_] := (Beep[]; Do[Pause[0.1]; Beep[], {n-1}];)
If[$OperatingSystem === "MacOSX",
goodBeep[] := Run["afplay /System/Library/Sounds/Morse.aiff&"],
goodBeep[] := Beep[];
];

End[];
End[];
