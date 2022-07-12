
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
  KeyExistsQ[$TemplateKatexFunction, str] || KeyExistsQ[$templateToKatexFunction, str] || (
    AssociationQ[$localTemplateToKatexFunctions] && KeyExistsQ[$localTemplateToKatexFunctions, str]
  );

(**************************************************************************************************)

PublicFunction[PrintTemplateBoxDefinitions]

PrintTemplateBoxDefinitions[] := Scope[
  Print @ SpacedColumn[
    "Formatting rules" -> Grid[InputForm /@ $expressionToTemplateBoxRules],
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

SetHoldAll[setTemplateBoxForm];

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
  {syms, vsym} = getPatternSymbols[lhs, rhs];
  
  (* tell MakeBoxes how to convert head[a_, b_] into TBox[{a, b}, "head"] *)
  registerTemplateBoxRules[lhs, syms];

  registerSlotFn[rhs, syms, vsym]
]

SetHoldAll[getPatternSymbols];

$varPatternP = _BlankSequence | _BlankNullSequence | _Repeated;

TemplateBoxForm::multivar = "Multiple variable-length pattern symbols in `` are not supported.";

getPatternSymbols[lhs_, rhs_] := Scope[
  syms = Cases[Unevaluated @ lhs, Verbatim[Pattern][sym_Symbol, Except[$varPatternP]] :> sym, Infinity, Heads -> True];
  varSyms = Cases[Unevaluated @ lhs, Verbatim[Pattern][sym_Symbol, $varPatternP] :> sym, Infinity, Heads -> True];
  syms = Select[syms, ContainsQ[Unevaluated @ rhs, #]&]; (* drop unused symbols *)
  If[Length[var] > 1, ThrowMessage["multivar", $head]];
  {DeleteDuplicates @ Join[syms, varSyms], First[varSyms, None]}
];


(**************************************************************************************************)

SetHoldFirst[registerSlotFn];

registerSlotFn[rhs_, syms_, vsym_] := Scope[

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
  TEval[e_]                            :> RuleCondition @ e,
  (* HoldPattern[TBox[s_Slot][arg_]]      :> TemplateBox[{arg, s}, "ParameterizedTemplateBox"], *)
  HoldPattern[RBox[r___]]              :> RowBox[{r}],
  HoldPattern[SBox[s_]]                :> TemplateBox[{}, s],
  HoldPattern[TBox[s_String][args___]] :> TemplateBox[{args}, s]
};

(**************************************************************************************************)

(* this is the case in which the template wants to processes arguments at boxification time
before storing them in the TemplateBox. *)

setTemplateBoxForm[lhs:(head_Symbol[___]), With[withClause_, rhs_]] := Scope[

  registerTemplateBoxName[head, Automatic];

  (* get all symbols used in RHS, and injected by With *)
  {syms, vsym} = getPatternSymbols[lhs, rhs];
  withSyms = getWithSymbols[withClause];

  (* have MakeBoxes evaluate the With, yielding the right tuple to put in TBox  *)
  registerStagedTemplateBoxRules[lhs, Evaluate @ syms, Evaluate @ $tName, Evaluate @ withSyms, withClause];
  
  (* we continue as the normal setTemplateBoxForm does, where our tuple will now
  have some additional variables at the beginning *)
  registerSlotFn[rhs, Join[withSyms, syms], vsym];
];

SetHoldAll[getWithSymbols];

getWithSymbols[vars__] := Cases[Unevaluated @ {vars}, HoldPattern @ Set[v_Symbol, _] :> v, 2];

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

registerTemplateBoxRules[lhs_, syms_] :=
  registerTemplateBoxRules[lhs, syms, $tName];

registerTemplateBoxRules[lhs_, syms_, tName_] :=
  registerTemplateBoxRules2[lhs, TemplateBox[makeQGBoxes /@ syms, tName]];

registerTemplateBoxRules[lhs_, {}, tName_] :=
  registerTemplateBoxRules2[lhs, TemplateBox[{}, tName]];

SetHoldAll[registerStagedTemplateBoxRules];

registerStagedTemplateBoxRules[lhs_, syms_, tName_, withSyms_, withClause__] :=
  registerTemplateBoxRules2[lhs, With[withClause, TemplateBox[Join[withSyms, makeQGBoxes /@ syms], tName]]];

registerStagedTemplateBoxRules[lhs_, {}, tName_, withSyms_, withClause__] :=
  registerTemplateBoxRules2[lhs, With[withClause, TemplateBox[withSyms, tName]]];

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

(* this takes a constant head that should act like e.g. PiSymbol *)

PublicFunction[DeclareConstantSymbolTemplateBox]

DeclareConstantSymbolTemplateBox[head_ -> rhs_] :=
  setTemplateBoxForm[head, rhs];

DeclareConstantSymbolTemplateBox[head_, rhs_] :=
  setTemplateBoxForm[head, rhs];

(**************************************************************************************************)

(* this takes a head that should apply a style *)

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

(* this takes a head that should act like e.g. GraphSymbol, VertexSymbol, etc. *)

PublicFunction[DeclareTypedSymbolTemplateBox]

DeclareTypedSymbolTemplateBox[head_, fn_:None] :=
  DeclareUnaryTemplateBox[head, fn];

  (**************************************************************************************************)

(* this takes a head that should act like e.g. PathQuiverSymbol *)

PublicFunction[DeclareDerivedSymbolTemplateBox]

DeclareDerivedSymbolTemplateBox[head_, fn_:None] :=
  DeclareUnaryTemplateBox[head, fn];

(**************************************************************************************************)

(* this takes a head that should act like e.g. VertexListFunction *)

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

DeclareTemplateBoxRules::badrule = "Expression `` is not a Rule or RuleDelayed.";

declareTemplateBoxRule = Case[
  (Rule|RuleDelayed)[lhs_, rhs_]  := setTemplateBoxForm[lhs, rhs];
  Null                            := Null;
  other_                          := ThrowMessage["badrule", InputForm @ other];
];

(**************************************************************************************************)

declareNullaryTemplateBox[head_, boxes_] := Scope[
  {tName, kName} = setTemplateBoxForm[head, boxes];
  KatexBox[SBox @ tName, PrefixSlash @ kName]
];

(**************************************************************************************************)

PublicFunction[DeclareNAryRelationTemplateBox]

DeclareNAryRelationTemplateBox[head_, boxes_] := Scope[
  riff = declareNullaryTemplateBox[head, boxes];
  setTemplateBoxForm[head[args___], RBox[SequenceRiffle[args, RBox[" ", TEval @ riff, " "]]]];
];

(**************************************************************************************************)

PublicFunction[DeclareNAryOperatorTemplateBox]

DeclareNAryOperatorTemplateBox[head_, boxes_] :=
  DeclareNAryRelationTemplateBox[head, boxes];

(**************************************************************************************************)

PublicFunction[DeclareBinaryRelationTemplateBox]

DeclareBinaryRelationTemplateBox[head_, boxes_] := Scope[
  riff = declareNullaryTemplateBox[head, boxes];
  setTemplateBoxForm[head[a_, b_], RBox[a, " ", TEval @ riff, " ", b]]
];

(**************************************************************************************************)

PublicFunction[DeclareIndexedBinaryRelationTemplateBox]

DeclareIndexedBinaryRelationTemplateBox[head_, boxes_] := Scope[
  riff = declareNullaryTemplateBox[head, boxes];
  setTemplateBoxForm[head[a_, b_], RBox[a, " ", TEval @ riff, " ", b]];
  setTemplateBoxForm[head[a_, b_, l_], RBox[a, " ", SubscriptBox[TEval @ riff, l], " ", b]]
];

(**************************************************************************************************)

PublicFunction[DeclareInfixTemplateBox]

DeclareInfixTemplateBox[head_, boxes_] :=
  DeclareBinaryRelationTemplateBox[head, boxes];

(**************************************************************************************************)

PublicFunction[DeclareSequenceTemplateBox, DeclareStyledSequenceTemplateBox]

DeclareSequenceTemplateBox[head_, left_, right_] :=
  setTemplateBoxForm[head[args___], RBox[left, SequenceRiffle[args, ", "], right]];

DeclareStyledSequenceTemplateBox[head_, left_, right_] :=
  setTemplateBoxForm[
    head[style_, args___] :> With[
      {styledL = RuntimeTBox[style, left],
       styledR = RuntimeTBox[style, left]},
      RBox[
        styledL,
        SequenceRiffle[args, ", "],
        styledR
      ]
    ]
  ];

(**************************************************************************************************)

PrivateFunction[RuntimeTBox]

(* this isn't right, since KatexBox has to evaluate ahead of time *)

RuntimeTBox[param_Symbol, arg_] := With[
  {name = SymbolName @ param},
  {kName = toKName[name, 1], tName = toTName[name, 1]},
  KatexBox[TemplateBox[{arg, tName}, "ParameterizedTemplateBox"], kName[arg]]
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


