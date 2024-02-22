PrivateIOFunction[WithLocalDirectory]

SetHoldRest[WithLocalDirectory];

WithLocalDirectory[None|Auto|Inherited, body_] :=
	body;

WithLocalDirectory[dir_Str, body_] :=
	WithLocalSettings[
		VPrint["Setting working directory: ", MsgPath @ dir];
		SetDirectory[dir];
	,
		body
	,
		ResetDirectory[];
	];

_WithLocalDirectory := BadArguments[];

(**************************************************************************************************)

PrivateIOFunction[ToolAvailableQ]

ToolAvailableQ[name_] := StrQ @ iFindTool[name];

(**************************************************************************************************)

PrivateFunction[toolKeyTranslationRules, toolKeyTranslationRules2]

toolKeyTranslationRules[rules_] := App[_ -> Nothing][(#1 -> v_) :> (#2 -> v)& @@@ rules];

(**************************************************************************************************)

PublicVariable[$BinaryPaths]

SetInitialValue[$BinaryPaths, {"/opt/homebrew/bin", "/usr/local/bin", "/usr/bin", "/usr/sbin", "/sbin", "/bin", $HomeDirectory, "/Applications", PathJoin[$HomeDirectory, "/Applications"]}];

(**************************************************************************************************)

PublicIOFunction[FindTool]

General::toolNotFound = "Tool `` is not present in any of the normal binary paths. Please install it.";

SetHoldRest[FindTool];

FindTool[name_Str] := FindTool[name, Message[General::toolNotFound, name]; $Failed];
FindTool[name_Str, else_] := ReplaceNone[iFindTool[name], else];

iFindTool[name_] := iFindTool[name] = SelectFirst[PathJoin[#, name]& /@ $BinaryPaths, FileExistsQ, None];

(**************************************************************************************************)

PrivateIOFunction[RunUTF8]

RunUTF8[str_Str, args__Str] := RunUTF8 @ SJoin[str, args];

RunUTF8[str_Str] := Module[{code},
	VPrint["Running \"", str, "\""];
	code = iRunUTF8[str];
	VPrint["Exit code = ", code];
	code
];

iRunUTF8[str_Str ? ASCIIQ] := Run[str];

iRunUTF8[str_Str] := Module[{tmpFile},
	tmpFile = MakeTemporaryFile["tool", Base36Hash[str] <> ".sh"];
	ExportUTF8[tmpFile, str];
	Run["/bin/bash -e " <> tmpFile]
];

_RunUTF8 := BadArguments[];

(**************************************************************************************************)

PrivateFunction[toolCommandString]

toolCommandString[tool_, args___] := Scope[
	toolPath = FindTool[tool, None];
	If[!StrQ[toolPath], ThrowMessage["toolNotFound", tool]];
	args = procToolArg /@ {args};
	SRiffle[Flatten[{toolPath, args}], " "]
];

(**************************************************************************************************)

PrivateFunction[falseIsFail]

falseIsFail[False] := $Failed;
falseIsFail[True] := Null;
falseIsFail[e_] := e;

(**************************************************************************************************)

(* inherited by things like GitClone and passed on to their calls to RunTool *)
PrivateVariable[$genericToolOpts]

$genericToolOpts = {
	Verbose            -> Auto,
	DryRun             -> False,
	StandaloneTerminal -> False
}

PublicIOFunction[RunTool]

PublicOption[WorkingDirectory, StandaloneTerminal, OpenToolOutput]

Options[RunTool] = {
	WorkingDirectory   -> Auto,
	StandaloneTerminal -> False,
	OpenToolOutput     -> Auto,
	Verbose            -> False,
	DryRun             -> False
};

SetUsage @ "
RunTool['tool$', args$$] run a command line tool, returning $Failed if it gave a non-zero exit code.

* the tool will be found via %FindTool, since Mathematica has no access to common interactive shell path setup.

# Argument processing

* arguments are processed individually as follows:
| 'str$'         | emit unchanged |
| File[$$]       | emit normalized path |
| True, False    | emit 'true' or 'false' |
| int$, real$    | emit stringified form |
| 'key$' -> val$ | emit as 'key$=val$' |

* values of Automatic and None are not emitted
* all arguments are escaped via %BashEscape, so no shell substitution will occur.

# Options

| %Verbose | whether to print all steps involved |
| %DryRun | whether to pretend to run the tool, and print all steps |
| %StandaloneTerminal | whether to spawn a user-visible terminal to show the tool output |
| %WorkingDirectory | working directory of the run |
| %OpenToolOutput | whether to show the output of the tool via %TextFileOpen |

* the default value of %OpenToolOutput -> Automatic will show the output only if the run fails.
"

General::toolWorkingDirectory = "WorkingDirectory -> `` does not exist.";
General::toolNoOutput = "Tool `` succeeded but did not produce expected output.";
General::toolBadArgument = "Argument `` could not be stringified.";

$openOutput = $tverbose = $runInTerminal = False; $workingDir = None;
DefineOptionToVariableBlocking[RunTool, {DryRun :> $dryRun, Verbose :> $tverbose, StandaloneTerminal :> $runInTerminal, WorkingDirectory :> $workingDir, OpenToolOutput :> $openOutput}];

RunTool[cmd_Str, args___] := Scope @ TrueQ @ CatchMessage[

	$verbose = ReplaceAutomatic[$tverbose, $dryRun];

	cmdStr = toolCommandString[cmd, args];

	If[StrQ[$workingDir] && !DirectoryQ[$workingDir], ThrowMessage["toolWorkingDirectory", $workingDir]];
	If[$runInTerminal,
		RunInTerminalWindow[If[StrQ[$workingDir], $workingDir, "~"], cmdStr];
		Return @ True
	];
	outputPath = MakeTemporaryFile["tool", cmd <> ".#.out"];
	cmdStrRedir = cmdStr <> " &>" <> outputPath;

	exitCode = If[$dryRun, 0, WithLocalDirectory[$workingDir, RunUTF8 @ cmdStrRedir]];
	success = exitCode === 0;

	openOutput = $openOutput;
	SetAutomatic[openOutput, !success];
	If[TrueQ[openOutput],
		VPrint["Opening tool output."];
		If[!FileExistsQ[outputPath], Message[RunTool::toolNoOutput, cmd]];
		stream = OpenAppend[outputPath, CharacterEncoding -> "UTF8"];
		WriteLine[stream, "\n\n#### tool input follows ####"];
		If[$workingDir =!= None, WriteLine[stream, "cd \"" <> $workingDir <> "\""]];
		WriteLine[stream, cmdStr];
		Close[stream];
		TextFileOpen[outputPath];
	];

	success
];

_RunTool := BadArguments[];

procToolArg = Case[
	ignored               := Nothing;
	_ -> ignored          := Nothing;
	k_Str -> v_           := k <> "=" <> %[v];
	v_Str                 := BashEscape @ v;
	v_Int                 := IntStr[v];
	r_Real                := TextString @ r;
	r_Rational            := % @ N @ r;
	False                 := "false";
	True                  := "true";
	File[f_]              := % @ f;
	v_					          := ThrowMessage["toolBadArgument", MsgExpr @ v];
,
	{ignored -> Auto | None}
];

(**************************************************************************************************)

PublicIOFunction[RunToolOutput]

SetUsage @ "
RunToolTooloutput['tool$', args$$] run a command line tool, returning the output of the run as a string.
* the option %Verbose -> True prints all steps.
* the option %WorkingDirectory -> 'dir$' specifies a working directory.
"

Options[RunToolOutput] = {
	Verbose -> True,
	WorkingDirectory -> Auto
}

DefineOptionToVariableBlocking[RunToolOutput, {WorkingDirectory :> $workingDir, Verbose :> $tverbose}];

RunToolOutput[cmd_String, args___] := Scope @ CatchMessage[

	$verbose = ReplaceAutomatic[$tverbose, $dryRun];

	cmdStr = toolCommandString[cmd, args];

	cmdFile = MakeTemporaryFile["tool", cmd <> ".#.sh"];
	outFile = cmdFile <> ".out.txt";
	errFile = cmdFile <> ".err.txt";
	argStr = SJoin[cmdStr, " 1>", outFile, " 2>", errFile];
	ExportUTF8[cmdFile, argStr];

	If[StrQ[$workingDir] && !DirectoryQ[$workingDir], ReturnFailed["toolWorkingDirectory", $workingDir]];
	VPrint["Running base command \"", cmdStr, "\" via a script at ", MsgPath @ cmdFile];
	exitCode = WithLocalDirectory[$workingDir, RunUTF8["/bin/bash -e ", cmdFile]];

	If[exitCode != 0,
		If[FileExistsQ @ outFile, VPrint["Tool failed with output:\n", ImportUTF8 @ outFile]];
		If[FileExistsQ @ errFile, VPrint["Tool failed with error:\n", ImportUTF8 @ errFile]];
		ReturnFailed[]
	];

	If[!FileExistsQ[outFile],
		ReturnFailed["toolNoOutput", cmd],
		ImportUTF8[outFile]
	]
];

_RunToolOutput := BadArguments[];

(**************************************************************************************************)

PublicFunction[BashEscape]

BashEscape[s_Str] := If[SMatchQ[s, RegularExpression["[a-zA-Z_-]+"]], s, SJoin["'", SRep[s, {"'" -> "\\'", "\\" -> "\\\\"}], "'"]];

(**************************************************************************************************)

PublicIOFunction[SystemOpenWith]

SystemOpenWith::failopen = "Failed to open path `` with tool \"``\"."
SystemOpenWith[path_, tool_] /; $MacOSQ := Scope[
	res = RunUTF8["open -a \"", tool, "\" ", procToolArg @ NormalizePath @ path];
	If[res === 0, path,
		Message[SystemOpenWith::failopen, MsgPath @ path, tool];
		$Failed
	]
]

SystemOpenWith[path_, _] := SystemOpen[path];

(**************************************************************************************************)

PublicIOFunction[WebpageOpen]

(* works best if you have "Fast Duplicate Tab Closer" extension installed *)
WebpageOpen[s_Str] := SystemOpenWith[s, "Google Chrome"];

(**************************************************************************************************)

PublicIOFunction[TextFileOpen]

TextFileOpen[s_Str] := SystemOpenWith[s, "Sublime Text"];
