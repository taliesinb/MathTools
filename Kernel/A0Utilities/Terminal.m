PublicFunction[RunInTerminalWindow]

$runInTerminalTemplate = StringFunction @ STrim @ """
tell application "Terminal"
  do script "cd '#1'; #2"
  activate
end tell
"""

(* TODO: do we have to escape single quotes here? *)
RunInTerminalWindow[directory_Str, command_Str] := Scope[
  cmd = $runInTerminalTemplate[NormalizePath @ directory, command];
  RunAppleScript[cmd]
]

(**************************************************************************************************)

PublicFunction[RunAppleScript]

RunAppleScript[cmd_] := (
  $scriptFile = MakeTemporaryFile["applescript.#.scpt"];
  ExportUTF8[$scriptFile, cmd];
  res = Run["osascript " <> $scriptFile];
  If[res === 0, Null, $Failed]
);
