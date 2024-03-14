BeginPackage["MTLoader`Standalone`"];

(*
this prevents the FE from slowing down all kernel restarts by loading WolframAlpha
(which goes on to load other packages!). unfortunately the definitions are only attached after startup at some unknown
time, and then later loaded by a scheduled task, so we prevent the definitions getting attached here *)
Quiet[
  ClearAll[FrontEnd`Private`PrimeControlEqualPump];
  Protect[FrontEnd`Private`PrimeControlEqualPump];
];

(* Misc.m *)
System`RealString;
System`SWith;

(* Messages.m *)
StandaloneErrorHandler;
StandaloneErrorMessage;
$StandaloneErrorTag;
System`TopLevelEvaluationFunction;
System`ThrowErrorMessage;
System`ThrowError;
System`CatchError;
System`CatchErrorAsFailure;

(* Macros.m *)
System`$MacroRules;
System`$MacroParentSymbol;
System`DefineVariableMacro;
System`DefinePatternMacro;
System`DefineSimpleMacro;
System`DefineMessageMacro;
System`ExpandMacros;
System`ContainsMacrosQ;
System`MacroHold;
System`PatternHeadSymbol;

(* NiceEcho.m *)
System`EchoLen;
System`EchoDims;
System`EchoKeys;
System`EchoSet;
System`EchoH;
System`EchoH0;
System`EchoH1;
System`EchoF;
System`EchoFL;
System`EchoFH;
System`EchoFLH;
System`NicePaster;

(* Sublime.m *)
System`SublimeOpen;
System`SublimeOpenProject;
System`$SublimePath;

(* FindSymbol.md *)
System`NameHasDefinitionsQ;
System`FindSymbol;
System`FindNames;
System`SymbolGrid;

(* ConstrainedBoxes.m*)
StandaloneHold;
StandaloneSequence;
System`InputFormLength;
System`ConstrainedMakeBoxes;

(* NiceLayout.m *)
System`MaxRows; System`MaxColumns;
System`ItemLabels; System`ItemFunction; System`TooltipFunction;
System`MaxWidth; System`MaxHeight;
System`NiceMulticolumn;

System`NiceGrid;
System`NiceGridAll;
System`NicePane;

System`NiceTooltip;
System`NiceTooltipBoxes;
System`PlainGrid;
system`NiceErrorBox;

(* FrontendCursor.m *)
System`SaveCursorPosition;
System`RestoreCursorPosition;

SetDirectory[FileNameDrop[$InputFileName]];

Get["Misc.m"];
Get["Messages.m"];
Get["Macros.m"];
Get["NiceEcho.m"];
Get["FindSymbol.m"];
Get["FrontendCursor.m"];
Get["ConstrainedBoxes.m"];
Get["NiceLayout.m"];
Get["Sublime.m"];

ResetDirectory[];

$StandaloneLoaded = True;

EndPackage[];
