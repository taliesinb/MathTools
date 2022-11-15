PublicFunction[RunInTerminalWindow]

$runInTerminalTemplate = StringFunction @ StringTrim @ """
tell application "Terminal"
    do script "cd '#1'; #2"
    activate
end tell
"""

RunInTerminalWindow[directory_String, command_String] := Scope[
    cmd = $runInTerminalTemplate[NormalizePath @ directory, command];
    RunAppleScript[cmd]
]

(**************************************************************************************************)

PublicFunction[RunAppleScript]

$scriptFile = FileNameJoin[{$TemporaryDirectory, "qg_temp_applescript.scpt"}];

RunAppleScript[cmd_] := (
    ExportUTF8[$scriptFile, cmd];
    res = Run["osascript " <> $scriptFile];
    If[res === 0, Null, $Failed]
);
