PublicVariable[$PosixQ, $WindowsQ]

$PosixQ :=   $OperatingSystem =!= "Windows";
$WindowsQ := $OperatingSystem === "Windows"

(**************************************************************************************************)

PrivateFunction[ToolAvailableQ]

ToolAvailableQ[name_] := StringQ @ findTool1[name];

(**************************************************************************************************)

PrivateFunction[toolKeyTranslationRules, toolKeyTranslationRules2]

toolKeyTranslationRules[rules_] := Append[_ -> Nothing][(#1 -> v_) :> (#2 -> v)& @@@ rules];

(**************************************************************************************************)

PrivateFunction[findTool]

$binaryPaths = {"/usr/local/bin", "/usr/bin", "/usr/sbin", "/sbin", "/bin", $HomeDirectory, "/Applications", PathJoin[$HomeDirectory, "/Applications"]};

General::toolnp = "Tool `` is not present in any of the normal binary paths. Please install it.";

SetHoldRest[findTool];

findTool[name_String] := findTool[name, ThrowMessage["toolnp", name]];
findTool[name_String, else_] := ReplaceNone[findTool1[name], else];

findTool1[name_] := findTool1[name] = SelectFirst[PathJoin[#, name]& /@ $binaryPaths, FileExistsQ, None];

(**************************************************************************************************)

PrivateFunction[falseIsFail]

falseIsFail[False] := $Failed;
falseIsFail[True] := Null;
falseIsFail[e_] := e;

(**************************************************************************************************)

PublicFunction[RunTool]

PublicOption[WorkingDirectory, StandaloneTerminal, OpenToolOutput]

Options[RunTool] = {
	StandaloneTerminal -> False,
	WorkingDirectory -> Automatic,
	OpenToolOutput -> Automatic,
	Verbose -> False,
	DryRun -> False
};

PrivateVariable[$genericToolOpts]

$genericToolOpts = {
	Verbose -> Automatic,
	DryRun -> False,
	StandaloneTerminal -> False
}

$dryRun = True;
RunTool[args1___, DryRun -> bool_, args2___] := Block[{$dryRun = bool}, RunTool[args1, args2]];

$tverbose = Automatic;
RunTool[args1___, Verbose -> bool_, args2___] := Block[{$tverbose = bool}, RunTool[args1, args2]];

$inTerm = False;
RunTool[args1___, StandaloneTerminal -> bool_, args2___] := Block[{$inTerm = TrueQ[bool]}, RunTool[args1, args2]];

General::tooldir = "WorkingDirectory -> `` does not exist."
$wdir = None;
RunTool[args1___, WorkingDirectory -> dir_, args2___] := Block[{$wdir = NormalizePath @ dir},
	If[!DirectoryQ[$wdir], ThrowMessage["tooldir", $wdir]];
	RunTool[args1, args2]
];

$oto = Automatic;
RunTool[args1___, OpenToolOutput -> oto_, args2___] := Block[{$oto = oto}, RunTool[args1, args2]];

$oto = Automatic;
RunTool[args1___, OpenToolOutput -> oto_, args2___] := Block[{$oto = oto}, RunTool[args1, args2]];

RunTool::badrp = "Run failed to return an association, returned: ``"

RunTool[cmd_, args___] := Scope @ Block[{$verbose = ReplaceAutomatic[$tverbose, $dryRun]},
	cmdPath = findTool[cmd];
	args = procArg /@ {args};
	args2 = BashEscape /@ args;
	argStr = StringRiffle[Flatten[{cmdPath, args2}], " "];
	If[$inTerm,
		RunInTerminalWindow[If[StringQ[$wdir], $wdir, "~"], argStr];
		Return @ True
	];
	tmpOut = MakeTemporaryFile["tool", cmd <> ".#.out"];
	argStr2 = argStr <> " &>" <> tmpOut;
	If[$wdir =!= None,
		VPrint["Running \"", argStr, "\" in ", MsgPath @ $wdir];
		If[$dryRun, exitCode = 0, WithLocalSettings[SetDirectory[$wdir], exitCode = RunUTF8 @ argStr2, ResetDirectory[]]];
	,
		VPrint["Running \"", argStr, "\""];
		exitCode = If[$dryRun, 0, RunUTF8 @ argStr2];
	];
	success = exitCode === 0;
	showOut = $oto;
	SetAutomatic[showOut, !success];
	If[TrueQ[showOut],
		stream = OpenAppend[tmpOut, CharacterEncoding -> "UTF8"];
		WriteLine[stream, "#### tool input follows ####"];
		If[$wdir =!= None, WriteLine[stream, "cd \"" <> $wdir <> "\""]];
		WriteLine[stream, argStr];
		Close[stream];
		TextFileOpen[tmpOut];
	];
	success
]

procArg[_ -> (Automatic|None)] := Nothing;
procArg[k_String -> v_] := k <> "=" <> procArg[v];
procArg[e_] := procArg @ TextString[e];
procArg[e_String] := e;

(**************************************************************************************************)

PublicFunction[RunUTF8]

RunUTF8[str_String] /; ASCIIQ[str] := Run[str];
RunUTF8[str_String] := Scope[
	tmpFile = MakeTemporaryFile["tool", Base36Hash[str] <> ".sh"];
	ExportUTF8[tmpFile, str];
	Run["/bin/bash -e " <> tmpFile]
];

(**************************************************************************************************)

PublicFunction[RunToolOutput]

RunToolOutput[args__, Verbose -> t_] := Block[{$tverbose = t}, RunToolOutput[args]];

RunToolOutput[cmd_, args___] := Scope[
	cmdPath = findTool[cmd];
	args = procArg /@ {args};
	args2 = BashEscape /@ args;
	inFile = MakeTemporaryFile["tool", cmd <> ".#.sh"];
	outFile = inFile <> ".out"; errFile = inFile <> ".err";
	argStr = StringRiffle[Flatten[{cmdPath, args2, "1>" <> outFile, " 2>" <> errFile}], " "];
	VPrint["Running \"", cmdPath, " ", StringRiffle[args2, " "]	, "\"."];
	ExportUTF8[inFile, argStr];
	If[Run["/bin/bash -e " <> inFile] =!= 0,
		VPrint["Tool failed, returned error:", ImportUTF8 @ errFile];
		ReturnFailed[]
	];
	ImportUTF8[outFile]
]

(**************************************************************************************************)

PublicFunction[BashEscape]

BashEscape[s_String] := If[StringMatchQ[s, RegularExpression["[a-zA-Z_-]+"]], s, StringJoin["'", StringReplace[s, {"'" -> "\\'", "\\" -> "\\\\"}], "'"]];

(**************************************************************************************************)

PublicFunction[SystemOpenWith]

SystemOpenWith::failopen = "Failed to open path `` with tool \"``\"."
SystemOpenWith[path_, tool_] /; $OperatingSystem === "MacOSX" := Scope[
	res = Run["open -a \"" <> tool <> "\" \"" <> procArg[NormalizePath @ path] <> "\""];
	If[res === 0, path,
		Message[SystemOpenWith::failopen, MsgPath @ path, tool];
		$Failed
	]
]

SystemOpenWith[path_, _] := SystemOpen[path];

(**************************************************************************************************)

PublicFunction[WebpageOpen]

(* works best if you have "Fast Duplicate Tab Closer" extension installed *)
WebpageOpen[s_String] := SystemOpenWith[s, "Google Chrome"];

(**************************************************************************************************)

PublicFunction[TextFileOpen]

TextFileOpen[s_String] := SystemOpenWith[s, "Sublime Text"];
