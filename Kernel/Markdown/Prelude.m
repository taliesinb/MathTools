PublicVariable[$KatexPrelude]

$KatexPrelude := $KatexPrelude = computeKatexPrelude[];

computeKatexPrelude[] := Scope[
  base = ImportUTF8 @ LocalPath["Markdown", "KatexPrelude.tex"];
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

toKatexFunctionDef[name_, func_] := Scope[
  maxSlot = Max[0, DeepCases[func, Slot[i_Integer] :> i]];
  slotStrs = toSlotStr /@ Range[maxSlot];
  eval = boxesToKatexString[func @@ slotStrs];
  $katexFnDefTemplate[name, StringJoin @ slotStrs, eval]
]
  
$katexFnDefTemplate = StringFunction["\\gdef\\#1#2{#3}"]

(**************************************************************************************************)

PublicFunction[KatexFunction]

(* registers a katex function to go in the prelude *)
KatexFunction /: Set[KatexFunction[name_String], func_] :=
  $katexDisplayFunction[name] ^= func;

