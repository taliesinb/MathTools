PublicVariable[$expressionToTemplateBoxRules, $templateBoxFunctions, $katexDefinitions, $templateBoxToKatexFunctions]

PublicFunction[ClearTemplateBoxDefinitions]

ClearTemplateBoxDefinitions[] := (
  $expressionToTemplateBoxRules = <||>;
  $templateBoxFunctions = $katexDefinitions = <||>;
  $templateBoxNameCounts = <||>;
  $templateBoxToKatexFunctions = <||>;
);

ClearTemplateBoxDefinitions[];

PublicFunction[TemplateBoxForm]
PublicHead[KatexBox]

ClearAll[TemplateBoxForm, KatexForm, setTemplateBoxForm, setKatexBoxForm, setTemplateBoxRules0, setTemplateBoxRules1];

SetHoldAll[setTemplateBoxForm, setKatexBoxForm, getPatternSymbols];

TemplateBoxForm /: SetDelayed[TemplateBoxForm[lhs_], rhs_] := CatchMessage @ setTemplateBoxForm[lhs, rhs];
KatexForm /: SetDelayed[KatexForm[lhs_], rhs_] := setKatexBoxForm[lhs, rhs];

TemplateBoxForm::varlenriffle = "Use of variable-length sequence on RHS for head `` is not a SequenceRiffle.";
TemplateBoxForm::toomanyrules = "Too many rules for head ``."

toTName[base_, 1] := base;
toTName[base_, 0] := LowerCaseFirst @ base;
toTName[base_, n_] := StringJoin[base, IntegerString @ n];

PrivateFunction[toKName]

toKSuffix[n_] := Switch[n, 0, "Ⅹ", 1, "", 2, "Ⅱ", 3, "Ⅲ", 4, "Ⅳ", 5, "Ⅴ"];
toKName[base_, 0] := LowerCaseFirst @ toKName[base, 1];
toKName[base_, n_] := StringJoin[StringCases[base, {"Gray" -> "G", RegularExpression @ "[A-Z][A-Za-z]"}], toKSuffix @ n];

setTemplateBoxForm[lhs:(head_Symbol[___]), rhs_] := Scope[
  
  $head = head;
  headName = SymbolName @ head;

  n = KeyIncrement[$templateBoxNameCounts, head];
  If[n >= 5, ThrowMessage["toomanyrules", $head]];

  (* extract pattern variables, but put sequence one at the end
      head[a_, b_]    ==>  {a, b}
      head[a_, b__]   ==>  {a, b}
      head[a__, b_]   ==>  {b, a}
  *)
  {syms, usyms, vsym} = getPatternSymbols[lhs];
  
  (* tell MakeBoxes how to convert head[a_, b_] into TBox[{a, b}, "head"] *)
  tName = toTName[headName, n];
  setTemplateBoxRules0[lhs, syms, tName];
  nsyms = Length @ syms;

  (* convert the RHS to be a pure function that contains only #1, #2, ...
      head[a_, b_]  :> foo[b, a]                        ==>  foo[#2, #1]&
      head[a_, b__] :> foo[b, a]                        ==>  foo[##2, #1]&
      head[a_, b__] :> foo[SequenceRiffle[b, "+"], a]   ==>  foo[TemplateSlotSequence[2, "+"], #1]&
   *)
  slotTuple = Slot /@ Range[nsyms];
  If[vsym =!= None, slotTuple //= ReplacePart[{nsyms, 0} -> SlotSequence]];
  slotFn = Function[rhs] /. RuleThread[syms, slotTuple];
  slotFn = slotFn /. HoldPattern[SequenceRiffle[SlotSequence[i_], r_]] :> TemplateSlotSequence[i, r];
  riffleElem = FirstCase[slotFn, TemplateSlotSequence[_, r_] :> r, None, Infinity];
  
  (* set up katexBoxRules to convert from TBox[{a, b}, "head"] to "head"[a, b]
     if there is a SlotSequence, we turn it into a tuple
     if there is a TemplateSlotSequence, we turn into a riffled tuple
  *)
  kName = toKName[headName, n];
  kFn = Construct[Function, kName @@ slotTuple];
  If[vsym =!= None,
    seqRule = If[riffleElem =!= None,
      With[{r = riffleElem}, s_SlotSequence :> Riffle[List[s], r]],
      s_SlotSequence :> List[s]
    ];
    kFn = kFn /. seqRule;
  ];
  $templateBoxToKatexFunctions[tName] ^= kFn;

  (* set up $templateBoxFunctions, which defines for MMA frontend will display TBox[{a, b}, "head"] at runtime.
     will later produce a style definition in a notebook stylesheet. *)
  $templateBoxFunctions[tName] ^= slotFn //. KatexBox[a_, b_] :> a;
  
  (* set up $katexDefinitions, which defines how Katex will display "head"[a, b] at runtime.
     will later produce a \gdef expression that goes in the Katex prelude *)
  katexFn = slotFn //. {
    KatexBox[a_, b_] :> b,
    SlotSequence -> Slot,
    TemplateSlotSequence[i_, r_] :> Slot[i],
    RowBox[e_List] :> e
  };
  $katexDefinitions[kName] ^= katexFn;
];

$varPatternP = _BlankSequence | _BlankNullSequence | _Repeated;

TemplateBoxForm::multivar = "Multiple variable-length pattern symbols in `` are not supported.";
getPatternSymbols[e_] := Scope[
  syms = Cases[Unevaluated[e], Verbatim[Pattern][sym_Symbol, Except[$varPatternP]] :> sym, Infinity, Heads -> True];
  varSyms = Cases[Unevaluated[e], Verbatim[Pattern][sym_Symbol, $varPatternP] :> sym, Infinity, Heads -> True];
  If[Length[var] > 1, ThrowMessage["multivar", $head]];
  {DeleteDuplicates @ Join[syms, varSyms], DeleteDuplicates @ syms, First[varSyms, None]}
];

(**************************************************************************************************)

setTemplateBoxForm[head_Symbol, rhs_] := Scope[
  $head = head;
  headName = SymbolName @ head;
  
  tName = toTName[headName, 0];
  setTemplateBoxRules0[head, {}, tName];

  rhsFn = Function[rhs];
  kName = toKName[headName, 0];
  $templateBoxToKatexFunctions[tName] ^= Construct[Function, StringJoin["\\", kName]];

  $templateBoxFunctions[tName] ^= rhsFn //. KatexBox[a_, b_] :> a;

  $katexDefinitions[kName] ^= rhsFn //. KatexBox[a_, b_] :> b;
];

(**************************************************************************************************)

(* this takes a head that should apply with one unrestricted argument, a corresponding
box  *)

setSimpleTemplateBoxForm[head_, box_] := Foo;

(**************************************************************************************************)

SetHoldFirst[setTemplateBoxRules0];
setTemplateBoxRules0[lhs_, syms_, tName_] :=
  setTemplateBoxRules1[lhs, TemplateBox[makeQGBoxes /@ syms, tName]];

setTemplateBoxRules0[lhs_, {}, tName_] :=
  setTemplateBoxRules1[lhs, TemplateBox[{}, tName]];

SetHoldAll[setTemplateBoxRules1];
setTemplateBoxRules1[lhs_, rhs_] := (
  KeyAppendTo[$expressionToTemplateBoxRules, $head, HoldPattern[lhs] :> rhs];
  $BoxFormattingHeadQ[$head] = True;
  MakeBoxes[lhs, StandardForm] := rhs;
  MakeBoxes[lhs, TraditionalForm] := rhs;
)

(**************************************************************************************************)

PublicFunction[KatexFunction]

KatexFunction /: Set[KatexFunction[name_String], func_] :=
  $katexDefinitions[name] ^= func;

(**************************************************************************************************)

PrivateFunction[declareTemplateBoxRules]

declareTemplateBoxRules[rules__] := Scan[declareTemplateBoxRule, {rules}];

TemplateBoxForm::badrule = "Expresion `` is not a Rule or RuleDelayed.";

declareTemplateBoxRule = Case[
  (Rule|RuleDelayed)[lhs_, rhs_]  := setTemplateBoxForm[lhs, rhs];
  Null                            := Null;
  other_                          := Message[TemplateBoxForm::badrule, InputForm @ other];
];

(**************************************************************************************************)

PublicFunction[EmitKatexFunctionDefinitions]

EmitKatexFunctionDefinitions[] := Scope[
  defs = KeyValueMap[toKatexFunctionDef, $katexDefinitions];
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
