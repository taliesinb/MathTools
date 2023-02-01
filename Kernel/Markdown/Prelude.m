PublicVariable[$KatexPrelude, $KatexPreludeFile]

SetInitialValue[$KatexPreludeFile, "KatexPrelude.tex"];

$KatexPrelude := computeKatexPrelude[$KatexPreludeFile];

computeKatexPrelude[file_] := computeKatexPrelude[file] = Scope[
  base = ImportUTF8 @ LocalPath["Kernel", "Markdown", file];
  add = EmitKatexFunctionDefinitions[];
  StringJoin[base, "\n\n", add]
];

(**************************************************************************************************)

PublicFunction[EmitKatexFunctionDefinitions]

EmitKatexFunctionDefinitions[] := Scope[
  defs = KeyValueMap[toKatexFunctionDef, $katexDisplayFunction];
  StringRiffle[defs, "\n"]
];

toSlotStr = <|1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#5"|>

EmitKatexFunctionDefinitions::badfunc = "Could not evaluate function `` to produce definition for macro ``."

toKatexFunctionDef[name_, func_] := Scope[
  maxSlot = Max[0, DeepCases[func, Slot[i_Integer] :> i]];
  slotStrs = toSlotStr /@ Range[maxSlot];
  eval = Check[boxesToKatexString[func @@ slotStrs], $Failed];
  If[!StringQ[eval],
    Message[EmitKatexFunctionDefinitions::badfunc, func, name],
    $katexFnDefTemplate[name, StringJoin @ slotStrs, eval]
  ]
]
  
$katexFnDefTemplate = StringFunction["\\gdef\\#1#2{#3}"]

(**************************************************************************************************)

PublicFunction[KatexFunction]

(* registers a katex function to go in the prelude *)
KatexFunction /: Set[KatexFunction[name_String], func_] :=
  $katexDisplayFunction[name] ^= func;

