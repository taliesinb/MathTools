PublicVariable[$KatexPrelude, $KatexPreludeFile]

SetInitialValue[$KatexPreludeFile, "KatexPrelude.tex"];

$KatexPrelude := computeKatexPrelude[$KatexPreludeFile];

computeKatexPrelude[file_] := computeKatexPrelude[file] = Scope[
  base = ImportUTF8 @ LocalPath["Kernel", "Markdown", file];
  add = EmitKatexMacroDefinitions[];
  StringJoin[base, "\n\n", add]
];

(**************************************************************************************************)

PublicFunction[EmitKatexMacroDefinitions]

EmitKatexMacroDefinitions[] := Scope[
  defs = KeyValueMap[toKatexMacroDefinition, $katexMacros];
  StringRiffle[defs, "\n"]
];

toSlotStr = <|1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#5"|>

EmitKatexFunctionDefinitions::badfunc = "Could not evaluate function `` to produce definition for macro ``."

toKatexMacroDefinition[name_, None] := None;
toKatexMacroDefinition[name_, func_] := Scope[
  maxSlot = Max[0, DeepCases[func, Slot[i_Integer] :> i]];
  slotStrs = toSlotStr /@ Range[maxSlot];
  eval = Check[boxesToKatexString[func @@ slotStrs], $Failed];
  If[!StringQ[eval],
    Message[EmitKatexFunctionDefinitions::badfunc, func, name],
    $katexMacroDefinitionTemplate[name, StringJoin @ slotStrs, eval]
  ]
]
  
$katexMacroDefinitionTemplate = StringFunction["\\gdef\\#1#2{#3}"]

(**************************************************************************************************)

PublicFunction[LookupKatexMacro, LookupKatexDisplayFunction, LookupNotebookDisplayFunction]

LookupKatexMacro := Case[
  None        := None;
  sym_Symbol  := Replace["" -> None] @ StringRiffle[DeleteNone @ Map[%, Lookup[$symbolToKatexMacroName, sym, {}]], "\n"];
  name_String := toKatexMacroDefinition[name, Lookup[$katexMacros, name, None]];
];

LookupKatexDisplayFunction := Case[
  None        := None;
  sym_Symbol  := DeleteNone @ AssociationMap[%, Lookup[$symbolToTemplateName, sym, {}]];
  name_String := Lookup[$katexDisplayFunction, name, None];
];

LookupNotebookDisplayFunction := Case[
  None        := None;
  sym_Symbol  := DeleteNone @ AssociationMap[%, Lookup[$symbolToTemplateName, sym, {}]];
  name_String := Lookup[$notebookDisplayFunction, name, None];
];
