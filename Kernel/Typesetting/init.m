PublicVariable[$expressionToTemplateBoxRules, $templateBoxDisplayFunction, $katexDisplayFunction, $templateToKatexFunction]

PublicFunction[ClearTemplateBoxDefinitions]

ClearTemplateBoxDefinitions[] := (
  $expressionToTemplateBoxRules = <||>;
  $templateBoxDisplayFunction = $katexDisplayFunction = <||>;
  $templateBoxNameCounts = <||>;
  $templateToKatexFunction = <||>;
);

ClearTemplateBoxDefinitions[];

(**************************************************************************************************)

PublicFunction[TemplateBoxNameQ]

TemplateBoxNameQ[str_] :=
  KeyExistsQ[$TemplateKatexFunction, str] || KeyExistsQ[$localTemplateToKatexFunctions, str] || (
    AssociationQ[$localTemplateToKatexFunctions] && KeyExistsQ[$localTemplateToKatexFunctions, str]
  );

(**************************************************************************************************)

PublicFunction[PrintTemplateBoxDefinitions]

PrintTemplateBoxDefinitions[] := Scope[
  Print @ SpacedColumn[
    "Display functions" -> SpacedRow[
      "Template" -> Grid[$templateBoxDisplayFunction],
      "Katex" -> Grid[$katexDisplayFunction]
    ],
    "TemplateToKatexFunction" -> Grid[$templateToKatexFunction],
    "KatexDefinitions" -> Pane @ EmitKatexFunctionDefinitions[],
    "NameCounts" -> Grid[$templateBoxNameCounts],
    Spacings -> 100
  ];
]

(**************************************************************************************************)

PublicHead[KatexBox]

SetHoldAll[setTemplateBoxForm, setKatexBoxForm, getPatternSymbols];

toTName[base_, 1] := base;
toTName[base_, 0] := LowerCaseFirst @ base;
toTName[base_, n_] := StringJoin[base, IntegerString @ n];

PrivateFunction[toKName]

toKSuffix[n_] := Switch[n, 0, "Ⅹ", 1, "", 2, "Ⅱ", 3, "Ⅲ", 4, "Ⅳ", 5, "Ⅴ"];
toKName[base_, 0] := LowerCaseFirst @ toKName[base, 1];
toKName[base_, n_] := StringJoin[StringCases[base, {"Gray" -> "G", RegularExpression @ "[A-Z][A-Za-z]"}], toKSuffix @ n];

General::tboxrulecount = "Too many rules for head ``."

DefineLiteralMacro[registerTemplateBoxName,
  registerTemplateBoxName[head_, n_] := Quoted[
    $head = head;
    headName = SymbolName @ head;
    nameCount = ReplaceAutomatic[n, KeyIncrement[$templateBoxNameCounts, head]];
    If[nameCount >= 5, ThrowMessage["tboxrulecount", $head]];
    $tName = toTName[headName, nameCount];
    $kName = toKName[headName, nameCount];
  ]
];

(**************************************************************************************************)

(* this is the most general case, which handles var arg patterns etc.
we have various special purpose declaration functions to avoid engaging with this *)

setTemplateBoxForm[lhs_ :> rhs_] :=
  setTemplateBoxForm[lhs, rhs];

setTemplateBoxForm[lhs:(head_Symbol[___]), rhs_] := Scope[
  
  registerTemplateBoxName[head, Automatic];

  (* extract pattern variables, but put sequence one at the end
      head[a_, b_]    ==>  {a, b}
      head[a_, b__]   ==>  {a, b}
      head[a__, b_]   ==>  {b, a}
  *)
  {syms, usyms, vsym} = getPatternSymbols[lhs];
  
  (* tell MakeBoxes how to convert head[a_, b_] into TBox[{a, b}, "head"] *)
  registerTemplateBoxRules[lhs, syms];
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
  slotFn = slotFn //. $displayFunctionReplacements;
  riffleElem = FirstCase[slotFn, TemplateSlotSequence[_, r_] :> r, None, Infinity];

  (* set up $templateBoxDisplayFunction and $katexDisplayFunction *)
  registerDisplayFunction[slotFn];
  
  (* set up katexBoxRules to convert from TBox[{a, b}, "head"] to "head"[a, b]
     if there is a SlotSequence, we turn it into a tuple
     if there is a TemplateSlotSequence, we turn into a riffled tuple
  *)
  templateToKatexFn = Construct[Function, $kName @@ slotTuple];
  If[vsym =!= None,
    seqRule = If[riffleElem =!= None,
      With[{r = riffleElem}, s_SlotSequence :> Riffle[List[s], r]],
      s_SlotSequence :> List[s]
    ];
    templateToKatexFn = templateToKatexFn /. seqRule;
  ];
  registerTemplateToKatexFunction[templateToKatexFn];

  {$tName, $kName}
];

$displayFunctionReplacements = {
  TEval[e_]                         :> RuleCondition @ e,
  HoldPattern[RBox[r___]]           :> RowBox[{r}],
  HoldPattern[SBox[s_]]             :> TemplateBox[{}, s],
  HoldPattern[TBox[form_][args___]] :> TemplateBox[{args}, form]
};

$varPatternP = _BlankSequence | _BlankNullSequence | _Repeated;

TemplateBoxForm::multivar = "Multiple variable-length pattern symbols in `` are not supported.";

getPatternSymbols[e_] := Scope[
  syms = Cases[Unevaluated[e], Verbatim[Pattern][sym_Symbol, Except[$varPatternP]] :> sym, Infinity, Heads -> True];
  varSyms = Cases[Unevaluated[e], Verbatim[Pattern][sym_Symbol, $varPatternP] :> sym, Infinity, Heads -> True];
  If[Length[var] > 1, ThrowMessage["multivar", $head]];
  {DeleteDuplicates @ Join[syms, varSyms], DeleteDuplicates @ syms, First[varSyms, None]}
];

(**************************************************************************************************)

(* this is a case in which user declared boxes for a lone symbol, so there are no arguments
to worry about *)

setTemplateBoxForm[head_Symbol, rhs_] := Scope[
  
  registerTemplateBoxName[head, 0];
  
  registerTemplateBoxRules[head, {}];

  fn = Function[rhs] //. $displayFunctionReplacements;
  registerDisplayFunction[fn];

  registerTemplateToKatexFunction[Construct[Function, PrefixSlash @ $kName]];

  {$tName, $kName} (* used by other code sometimes *)
];

TemplateBoxForm::badspec = "Unknown call setTemplateBoxForm[``]";

setTemplateBoxForm[args___] := (Message[TemplateBoxForm::badspec, SequenceForm[args]]; $Failed);

(**************************************************************************************************)

registerTemplateToKatexFunction[fn_] :=
  $templateToKatexFunction[$tName] = fn //. KatexBox[a_, b_] :> b;

registerTemplateToKatexFunction[] :=
  $templateToKatexFunction[$tName] = PrefixSlash @ $kName;

(**************************************************************************************************)

(* this takes an arbtirary box function and then processes it to turn it into a templateBoxDisplayFunction
and a katexDefinition, that will live in stylesheet and prelude respectively *)

registerDisplayFunction[displayFn_] := (

  (* set up $templateBoxDisplayFunction, which defines for MMA frontend will display TBox[{a, b}, "head"] at runtime.
     will later produce a style definition in a notebook stylesheet. *)
  $templateBoxDisplayFunction[$tName] = displayFn //. KatexBox[a_, b_] :> a;

  (* set up $katexDisplayFunction, which defines how Katex will display "head"[a, b] at runtime.
     will later produce a \gdef expression that goes in the Katex prelude *)
  $katexDisplayFunction[$kName] ^= displayFn //. {
    KatexBox[a_, b_] :> b,
    SlotSequence -> Slot,
    TemplateSlotSequence[i_, r_] :> Slot[i],
    RowBox[e_List] :> e
  };
);

(**************************************************************************************************)

SetHoldFirst[registerTemplateBoxRules];

registerTemplateBoxRules[lhs_, rhs_] :=
  registerTemplateBoxRules[lhs, rhs, $tName];

registerTemplateBoxRules[lhs_, syms_, tName_] :=
  registerTemplateBoxRules2[lhs, TemplateBox[makeQGBoxes /@ syms, tName]];

registerTemplateBoxRules[lhs_, {}, tName_] :=
  registerTemplateBoxRules2[lhs, TemplateBox[{}, tName]];

SetHoldAll[registerTemplateBoxRules2];

registerTemplateBoxRules2[lhs_, rhs_] := (
  KeyAppendTo[$expressionToTemplateBoxRules, $head, HoldPattern[lhs] :> rhs];
  $BoxFormattingHeadQ[$head] = True;
  MakeBoxes[lhs, StandardForm] := rhs;
  MakeBoxes[lhs, TraditionalForm] := rhs;
)

(**************************************************************************************************)

(* this handle both registering a display function and the template to katex conversion, which is
trivial *)

registerOneArgTemplate[displayFn_] := (

  registerDisplayFunction[displayFn];

  registerTemplateToKatexFunction[];

);

(* this optimizes the case where the katex does nothing, which is most kinds of symbols.
here we don't bother to register a $katexDisplayFunction, to avoid bloat *)

registerOneArgTemplate[Identity | (#&)] := (

  $templateBoxDisplayFunction[$tName] = (#&);

  registerTemplateToKatexFunction[(#&)];
);

(**************************************************************************************************)
(** EXPOSED INTERFACE TO USERS                                                                   **)
(**************************************************************************************************)

PublicFunction[TemplateBoxForm]

TemplateBoxForm /: SetDelayed[TemplateBoxForm[lhs_], rhs_] :=
  CatchMessage @ setTemplateBoxForm[lhs, rhs];

(**************************************************************************************************)

(* this takes a head that should act like e.g. GraphSymbol, VertexSymbol, etc. *)

PublicFunction[DeclareUnaryTemplateBox]

DeclareUnaryTemplateBox[head_, style_:None] := CatchMessage @ Scope[

  registerTemplateBoxName[head, 1];
  
  registerTemplateBoxRules[head[s_], {s}];

  registerOneArgTemplate @ toStyleBoxFn @ style;
];

toStyleBoxFn = Case[
  s_String     := StyleBox[#, s]&;
  fn_Function  := fn;
  None         := #&;
];

(**************************************************************************************************)

(* this takes a constant head that should act like e.g. PiSymbol *)

PublicFunction[DeclareConstantSymbolTemplateBox]

DeclareConstantSymbolTemplateBox[head_, rhs_] :=
  setTemplateBoxForm[head, rhs];

(**************************************************************************************************)

(* this takes a constant head that should act like e.g. PiSymbol *)

PublicFunction[DeclareTypedSymbolTemplateBox]

DeclareTypedSymbolTemplateBox[head_, rhs_:None] :=
  DeclareUnaryTemplateBox[head, rhs];

(**************************************************************************************************)

(* this takes a head that should act like e.g. GraphSymbol, VertexSymbol, etc. *)

PublicFunction[DeclareFunctionTemplateBox]

DeclareFunctionTemplateBox[head_, rhs_] := Scope[

  $head = head;

  (* Evaluate forces the FunctionBox to evaluate now, since KatexBox needs to be literally present *)
  {$tName, $kName} = setTemplateBoxForm[head, Evaluate @ FunctionBox @ rhs];

  applyRule = HoldPattern[head[args___]] :> TemplateBox[Prepend[makeQGBoxes /@ {args}, $headBox], "AppliedForm"];
  applyRule = applyRule /. $headBox -> SBox[$tName];
  
  registerTemplateBoxRules2 @@ applyRule;
];

(**************************************************************************************************)

PublicFunction[DeclareTemplateBoxRules]

(* this is used to define many simultaneous tepmlates via the same codepath as
TemplateBoxForm[lhs] := rhs *)

DeclareTemplateBoxRules[rules__] :=
  CatchMessage @ Scan[declareTemplateBoxRule, {rules}];

DeclareTemplateBoxRules::badrule = "Expresion `` is not a Rule or RuleDelayed.";

declareTemplateBoxRule = Case[
  (Rule|RuleDelayed)[lhs_, rhs_]  := setTemplateBoxForm[lhs, rhs];
  Null                            := Null;
  other_                          := ThrowMessage["badrule", InputForm @ other];
];

(**************************************************************************************************)

(* this takes a constant head that should act like e.g. PiSymbol *)

PublicFunction[DeclareRelationTemplateBox]

DeclareRelationTemplateBox[head_, symbol_] := Scope[

  {tName, dummy} = setTemplateBoxForm[head, symbol];

  riff = KatexBox[SBox @ tName, PrefixSlash @ dummy];
  setTemplateBoxForm[head[args___], RBox[SequenceRiffle[args, TEval @ riff]]];
];

(**************************************************************************************************)

PublicFunction[DeclareLocalStyles]

DeclareLocalStyles::taggingrules = "Could not update tagging rules.";

SetHoldAll[DeclareLocalStyles];
DeclareLocalStyles[e___] := Scope[
  $expressionToTemplateBoxRules = <||>;
  $templateBoxDisplayFunction = $katexDisplayFunction = <||>;
  $templateBoxNameCounts = <||>;
  $templateToKatexFunction = <||>;
  (e);
  privateStylesheet = GeneratePrivateQuiverGeometryStylesheet[];
  SetOptions[EvaluationNotebook[], StyleDefinitions -> privateStylesheet];
  katex = EmitKatexFunctionDefinitions[];
  currentRules = Lookup[Options[EvaluationNotebook[], TaggingRules], TaggingRules, <||>];
  If[!AssociationQ[currentRules], ReturnFailed["taggingrules"]];
  taggingRules = Join[currentRules, <|"KatexDefinitions" -> katex, "TemplateToKatexFunctions" -> $templateToKatexFunction|>];
  SetOptions[EvaluationNotebook[], TaggingRules -> taggingRules];
];


