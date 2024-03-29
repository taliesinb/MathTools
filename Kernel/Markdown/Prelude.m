PublicVariable[$KatexPrelude]

$KatexPrelude := computeKatexPrelude[];

computeKatexPrelude[] := Scope[
  defs = KVMap[toKatexMacroDefinition, $katexMacros];
  SRiffle[defs, "  "]
];

toSlotStr = <|1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#5"|>

EmitKatexFunctionDefinitions::badfunc = "Could not evaluate function `` to produce definition for macro ``."

toKatexMacroDefinition[name_, None] := None;
toKatexMacroDefinition[name_, func_] := Scope[
  maxSlot = Max[0, DeepCases[func, Slot[i_Int] :> i]];
  slotStrs = toSlotStr /@ Range[maxSlot];
  eval = Check[boxesToKatexString[func @@ slotStrs], $Failed];
  If[!StrQ[eval],
    Message[EmitKatexFunctionDefinitions::badfunc, func, name],
    $katexMacroDefinitionTemplate[name, SJoin @ slotStrs, eval]
  ]
]
  
$katexMacroDefinitionTemplate = StringFunction["\\gdef\\#1#2{#3}"]

(**************************************************************************************************)

PublicFunction[LookupKatexMacro, LookupKatexDisplayFunction, LookupNotebookDisplayFunction]

LookupKatexMacro := Case[
  None        := None;
  sym_Symbol  := Rep["" -> None] @ SRiffle[DeleteNone @ Map[%, Lookup[$symbolToKatexMacroName, sym, {}]], "\n"];
  name_Str := toKatexMacroDefinition[name, Lookup[$katexMacros, name, None]];
];

LookupKatexDisplayFunction := Case[
  None        := None;
  sym_Symbol  := DeleteNone @ AssocMap[%, Lookup[$symbolToTemplateName, sym, {}]];
  name_Str := Lookup[$katexDisplayFunction, name, None];
];

LookupNotebookDisplayFunction := Case[
  None        := None;
  sym_Symbol  := DeleteNone @ AssocMap[%, Lookup[$symbolToTemplateName, sym, {}]];
  name_Str := Lookup[$notebookDisplayFunction, name, None];
];
