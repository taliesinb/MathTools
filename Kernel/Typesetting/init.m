PublicFunction[TBox, TBoxOp, SBox, RBox, GBox]

TBox[args___, form_] := TemplateBox[{args}, form];
TBoxOp[form_][args___] := TemplateBox[{args}, form];

SBox[form_] := TemplateBox[{}, form];

RBox[args___] := RowBox[{args}];

GBox[entries_, alignments_, rowSpacings_, colSpacings_] :=
  GridBox[
    entries,
    GridBoxAlignment -> {"Columns" -> alignments},
    GridBoxSpacings -> {"Rows" -> prepend0 @ rowSpacings, "Columns" -> prepend0 @ colSpacings}
  ];

prepend0[list_List] := Prepend[list, 0];
prepend0[e_] := e;

(**************************************************************************************************)

PublicSymbol[$0, $1, $2, $3, $4, $5]

PublicFunction[DefineStandardTraditionalForm]

DefineStandardTraditionalForm[list_] := Scan[DefineStandardTraditionalForm, list];

DefineStandardTraditionalForm[lhs_ :> rhs_] := (
  MakeBoxes[lhs, StandardForm] := rhs;
  MakeBoxes[l:lhs, TraditionalForm] := MakeBoxes @ l;
)

(**************************************************************************************************)

PublicFormBox[Raise]

DefineStandardTraditionalForm[RaiseForm[e_, n_ ? NumericQ] :> RaiseBox[MakeBoxes @ e, n]];

RaiseBox[e_, n_] := AdjustmentBox[e, BoxBaselineShift -> -n];

(**************************************************************************************************)

tagAsMath[t_] := TagBox[t /. TagBox[e_, "QG"] :> e, "QG"];

(**************************************************************************************************)

PublicFunction[DefineTemplateBox]

SetUsage @ "
DefineTemplateBox['tname$' -> boxes$] defines the template %DisplayFunction for %TemplateBox[$$, 'tname$'] to be the \
literal value of boxes$, where $1, $2, etc are the slots. Evaluation is not allowed because DisplayFunctions \
cannot do anything other than simple substitution anyway.
DefineTemplateBox[templateSpec$, katexSpec$] defines the Katex display function definition that will be embedded into prelude.
DefineTemplateBox[templateSpec$, katexSpec$, conversionSpec$] defines how to convert a %TemplateBox[$$] into a call to Katex display function.
* The Katex display function spec katexSpec$ can be one of:
| Automatic | use katexified version of boxes$ present in the %TemplateBox definition (the first argument) |
| kboxes$ | use katexified version of kboxes$ |
| 'kname$' -> spec$ | names the katex display function 'kname$', rather than shortening 'tname$' |
* The conversion from a %TemplateBox into Katex display template call is described by conversionSpec$:
| Automatic | convert %TemplateBox[args$$, 'tname$'] into \\kname${args$$} |
| 'tname$' -> Function[$$] | convert %TemplateBox[args$$, 'tname$'] by applying function to args$$ |
| 'tname$' \[RuleDelayed] body | do a literal replacement of $1, $2, etc in body$  |
* The name of Automatic will use the same 'tname$' as the first argument.
* Any of the specs can be lists, these will be scanned over.
"

DefineTemplateBox = Case[
  boxSpec_List := Scan[%, boxSpec];
  boxSpec_ := $[boxSpec, Automatic, Automatic];
  Seq[tSpec_, kSpec_] := $[tSpec, kSpec, Automatic];
  Seq[tSpec_, kSpec_, cSpec_] := Scope @ QuiverGeometry`PackageScope`CatchMessage[
    $tname = $tboxes = $kname = None;
    procTSpec[tSpec]; procKSpec[kSpec]; procCSpec[cSpec /. $0 -> $kname];
    {$tname, $kname}
  ];
];

procTSpec = Case[
  name_String -> boxes:Except[_Function] := (
    $tname = name; $tboxes = boxes;
    $templateBoxDisplayFunction[name] = toSlotFn[boxes]  //. KatexSwitch[b_, _] :> b;
  );
  spec_ := ThrowMessage["arg1", spec];
];
DefineTemplateBox::arg1 = "First argument `` is invalid.";

procKSpec = Case[
  Automatic                              := $[Automatic -> Automatic];
  Automatic -> boxes_                    := $[toKname[$tname] -> boxes];
  name_String -> Automatic               := $[name -> $tboxes];
  specs:{___Rule}                        := Last[Map[procKSpec, specs], None];
  name_String -> boxes:Except[_Function] := (
    $kname = name;
    $katexDisplayFunction[name] = toSlotFn[boxes] //. KatexSwitch[_, b_] :> b;
  );
  boxes:Except[_Rule]                    := $[Automatic -> boxes];
  spec_                                  := ThrowMessage["arg2", spec, $tname];
];
DefineTemplateBox::arg2 = "Second argument `` for TBox `` is invalid.";

procCSpec = Case[
  Automatic := $[$tname :> Automatic];
  name_ :> Automatic := $[name -> Construct[Function, $kname[##]]];
  specs:{___RuleDelayed} := Last[Map[procCSpec, specs]];
  fn_Function := $[$tname -> fn];
  name_String :> body_ := $[name -> toSlotFn @ Unevaluated @ body];
  name_String -> fn_Function := (
    $templateToKatexFunction[name] = fn;
  );
  spec_ := ThrowMessage["arg3", spec, $tname];
];
DefineTemplateBox::arg3 = "Third argument `` for TBox `` is invalid.";

(**************************************************************************************************)

PrivateFunction[toKname]

$kNameRules = {RegularExpression @ "^[a-z][a-z]?", "Gray" -> "G", RegularExpression @ "[A-Z][A-Za-z]", "1" -> "Ⅰ", "2" -> "Ⅱ", "3" -> "Ⅲ", "4" -> "Ⅳ"};
toKname = Case[
  str_String ? LowerCaseQ := StringTake[str, 2];
  str_String := StringJoin @ StringCases[str, $kNameRules];
];

$slotRules = {$1 -> Slot[1], $2 -> Slot[2], $3 -> Slot[3], $4 -> Slot[4], $5 -> Slot[5]};
toSlotFn[None] := None;
toSlotFn[body_] := Function[body] /. $slotRules;

(**************************************************************************************************)

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

PublicFunction[DefineTaggedForm]

SetRelatedSymbolGroup[DefineTaggedForm, DefineUnaryForm]
SetRelatedSymbolGroup[DefineUnaryForm, DefineBinaryForm, DefineTernaryForm]
SetRelatedSymbolGroup[DefineBinaryForm, DefineLiteralInfixBinaryForm]
SetRelatedSymbolGroup[DefineInfixForm, DefineLiteralInfixForm]
SetRelatedSymbolGroup[DefineRiffledForm, DefineLiteralRiffledForm]
SetRelatedSymbolGroup[DefineInfixForm, DefineRiffledForm, DefineLeftRightForm]
SetRelatedSymbolGroup[DefineLiteralInfixForm, DefineLiteralRiffledForm, DefineLiteralLeftRightForm]
SetRelatedSymbolGroup[DefineLeftRightForm, DefineLiteralLeftRightForm]
SetRelatedSymbolGroup[DefineUnaryForm, DefineUnaryStyleForm]

SetUsage @ "
DefineTaggedForm[symbol$] defines symbol$[arg$1] to boxify to %TemplateBox[{arg$1}, 'symbol$'], \
which displays as arg$1.
* A shortened katex macro is set up, which looks like \\sym{arg$1}.
"

SetListable[DefineTaggedForm];

DefineTaggedForm[formSym_Symbol] := With[
  {name = SymbolName @ formSym},
  DefineStandardTraditionalForm[formSym[e_] :> TBox[MakeQGBoxes @ e, name]];
  DefineTemplateBox[name -> $1]
];

_DefineTaggedForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineUnaryForm]

SetUsage @ "
DefineUnaryForm[symbol$, boxes$] defines symbol$[arg$1] to boxify to %TemplateBox[{arg$1}, 'symbol$'], \
which displays as boxes$ where $1 is substituted.
* A shortened katex macro is set up, which looks like \\sym{arg$1}.
* A third argument specifies a box function which can be called to construct the underlying boxes directly.
"

DefineUnaryForm[formSym_Symbol, boxes_, boxFn_:None] := With[
  {name = SymbolName @ formSym},
  If[boxFn =!= None,
    boxFn[e_] := TBox[e, name];
    DefineStandardTraditionalForm[formSym[e_] :> boxFn[MakeQGBoxes @ e]],
    DefineStandardTraditionalForm[formSym[e_] :> TBox[MakeQGBoxes @ e, name]]
  ];
  DefineTemplateBox[name -> boxes]
];

_DefineUnaryForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineBinaryForm]

SetUsage @ "
DefineBinaryForm[symbol$, boxes$] defines symbol$[arg$1, arg$2] to boxify to %TemplateBox[{arg$1, arg$2}, 'symbol$'], \
which displays as boxes$ where $1, $2 are substituted.
* A shortened katex macro is set up, which looks like \\sym{arg$1, arg$2}.
* A third argument specifies a box function which can be called to construct the underlying boxes directly.
"

DefineBinaryForm[formSym_Symbol, boxes_, boxFn_:None] := With[
  {name = SymbolName @ formSym},
  If[boxFn =!= None,
    boxFn[e_, f_] := TBox[e, f, name];
    DefineStandardTraditionalForm[formSym[e_, f_] :> boxFn[MakeQGBoxes @ e, MakeQGBoxes @ f]],
    DefineStandardTraditionalForm[formSym[e_, f_] :> TBox[MakeQGBoxes @ e, MakeQGBoxes @ f, name]]
  ];
  DefineTemplateBox[name -> boxes]
];

_DefineBinaryForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineTernaryForm]

SetUsage @ "
DefineTernaryForm[symbol$, boxes$] defines symbol$[arg$1, arg$2, arg$3] to boxify to %TemplateBox[{arg$1, arg$2, arg$3}, 'symbol$'], \
which displays as boxes$ where $1, $2, $3 are substituted.
* A shortened katex macro is set up, which looks like \\sym{arg$1, arg$2, arg$3}.
* A third argument specifies a box function which can be called to construct the underlying boxes directly.
"

DefineTernaryForm[formSym_Symbol, boxes_, boxFn_:None] := With[
  {name = SymbolName @ formSym},
  If[boxFn =!= None,
    boxFn[e_, f_] := TBox[e, f, name];
    DefineStandardTraditionalForm[formSym[e_, f_, g_] :> boxFn[MakeQGBoxes @ e, MakeQGBoxes @ f, MakeQGBoxes @ g]],
    DefineStandardTraditionalForm[formSym[e_, f_, g_] :> TBox[MakeQGBoxes @ e, MakeQGBoxes @ f, MakeQGBoxes @ g, name]]
  ];
  DefineTemplateBox[name -> boxes]
];

_DefineTernaryForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineLiteralInfixBinaryForm]

SetUsage @ "
DefineLiteralInfixBinaryForm[symbol$, infixBox$] defines symbol$[arg$1, arg$2] to boxify to %RowBox[{arg$1, infixBox$, arg$2}].
* No katex macro is set up.
* The bare head symbol$ just displays as infixBox$.
"

DefineLiteralInfixBinaryForm[formSym_Symbol, infixBox_] :=
  DefineStandardTraditionalForm[{
    formSym :> tagAsMath @ infixBox,
    formSym[a_, b_] :> tagAsMath @ RBox[MakeQGBoxes @ a, RBox[" ", infixBox, " "], MakeQGBoxes @ b]
  }];

_DefineLiteralInfixBinaryForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineLiteralInfixForm]

SetUsage @ "
DefineLiteralInfixForm[symbol$, infixBox$] defines symbol$[arg$1, arg$2, $$] to boxify to %RowBox[{arg$1, infixBox$, arg$2, infixBox$, $$}].
* No katex macro is set up.
* The bare head symbol$ just displays as infixBox$.
* symbol$[] display as nothing.
* symbol$[arg$1] displays as arg$1.
* DefineLiteralInfixForm uses only %DefineStandardTraditionalForm internally.
"

DefineLiteralInfixForm[formSym_Symbol, infixBox_] :=
  DefineStandardTraditionalForm[{
    formSym :> tagAsMath @ infixBox,
    formSym[] :> "",
    formSym[e_] :> tagAsMath @ MakeQGBoxes[e],
    formSym[seq__] :> tagAsMath @ RowBox[Riffle[MakeQGBoxes /@ {seq}, RBox[" ", infixBox, " "]]]
  }];

_DefineLiteralInfixForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineInfixForm]

SetUsage @ "
DefineInfixForm[symbol$, infixBox$] defines symbol$[arg$1, arg$2, $$] to boxify to %TemplateBox[{arg$1, infixBox$, arg$2, infixBox$, $$}, 'symbol$'], \
which displays as RowBox[{arg$1, infixBox$, arg$2, infixBox$, $$}].
* A shortened katex macro is set up, which looks like \\sym{arg$1, arg$2, $$}.
* The bare head symbol$ just displays as infixBox$.
* symbol$[] display as nothing.
* symbol$[arg$1] displays as arg$1.
"

DefineInfixForm[formSym_Symbol, riffledBox_] := With[
  {name = SymbolName @ formSym},
  DefineStandardTraditionalForm[{
    formSym :> riffledBox,
    formSym[] :> "",
    formSym[e_] :> MakeQGBoxes[e],
    formSym[seq__] :> TBox[Riffle[MakeQGBoxes /@ {seq}, RBox[" ", riffledBox, " "]], name]
  }];
  DefineTemplateBox[name -> DBox[RowBox[$1]], {}, #&]
];

_DefineInfixForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineLeftRightForm]

SetUsage @ "
DefineLeftRightForm[symbol$, leftBox$, rightBox$] defines symbol$[arg$] to boxify to %TemplateBox[{arg$}, 'symbol'], \
which displays as %RowBox[{leftBox$, arg$, rightBox$}].
* A shortened Katex macro is also set up.
* If they are single characters, Katex \\left and \\right is used for the left and right boxes.
"

DefineLeftRightForm[formSym_Symbol, leftBox_, rightBox_] := With[
  {name = SymbolName @ formSym, lbox = toLrBox[leftBox, "\\left"], rbox = toLrBox[rightBox, "\\right"]},
  DefineStandardTraditionalForm[
    formSym[arg_] :> TBox[MakeQGBoxes @ arg, name]
  ];
  DefineTemplateBox[name -> RBox[lbox, " ", $1, " ", rbox]]
];

toLrBox[s_String ? SingleLetterQ, wrapper_] := KatexSwitch[s, wrapper[s]];
toLrBox[other_, _] := other;

_DefineLeftRightForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineLiteralLeftRightForm]

SetUsage @ "
DefineLiteralLeftRightForm[symbol$, leftBox$, rightBox$] defines symbol$[arg$] to boxify to %RowBox[{leftBox$, arg$, rightBox$}].
* No katex macro is set up.
* If they are single characters, Katex \\left and \\right is used for the left and right boxes via the 'katexSwitch' template box.
* DefineLiteralInfixForm uses only %DefineStandardTraditionalForm internally.
"

DefineLiteralLeftRightForm[formSym_Symbol, lbox_, rbox_] := With[
  {name = SymbolName @ formSym},
  If[SingleLetterQ[lbox],
    DefineStandardTraditionalForm[
      formSym[arg_] :> With[{box = MakeQGBoxes @ arg},
        TBox[
          RBox[lbox, box, rbox],
          RBox["left"[lbox], " ", box, " ", "right"[rbox]],
          "katexSwitch"
        ]
      ]
    ],
    DefineStandardTraditionalForm[
      formSym[arg_] :> tagAsMath @ RBox[lbox, MakeQGBoxes @ arg, rbox]
    ]
  ];
];

_DefineLiteralLeftRightForm := BadArguments[];

DefineTemplateBox["katexSwitch" -> $1, {}, "katexSwitch" :> $2];

(**************************************************************************************************)

PublicFunction[DefineRiffledForm]

SetUsage @ "
DefineRiffledForm[symbol$, containerBoxes$, riffleBox$] defines symbol$[$$] to boxify to %TemplateBox[{{$$}}, 'symbol'], \
which displays as containerBoxes$ with $1 substituted to be a list of boxes riffled with riffleBox$.
* A shortened Katex macro is also set up.
* DBox should be used in container$ to ensure the single template argument, a list, is processed correctly.
* A fourth argument specifies a box function which can be called to construct the underlying boxes directly.
"

DefineRiffledForm[formSym_Symbol, boxes_, riffledBox_, boxFn_:None] := With[
  {name = SymbolName @ formSym, isSimple = SameQ[boxes, RowBox[$1]]},
  If[boxFn =!= None,
    If[isSimple,
      boxFn[e_] := e; boxFn[] := "",
      boxFn[e_] := TBox[e, name]
    ];
    boxFn[seq___] := TBox[Riffle[{seq}, riffledBox], name];
    DefineStandardTraditionalForm[formSym[seq___] :> boxFn[MakeQGBoxSequence[seq]]]
  ,
    If[isSimple,
      DefineStandardTraditionalForm[{formSym[] :> "", formSym[e_] :> MakeQGBoxes[e]}],
      DefineStandardTraditionalForm[{formSym[] :> TBox[{}, name], formSym[e_] :> TBox[List @ MakeQGBoxes[e], name]}]
    ];
    DefineStandardTraditionalForm[formSym[seq___] :> TBox[Riffle[MakeQGBoxes /@ {seq}, riffledBox], name]]
  ];
  If[isSimple,
    DefineTemplateBox[name -> DBox[boxes], {}, #&],
    DefineTemplateBox[name -> DBox[boxes]]
  ];
];

_DefineRiffledForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineLiteralRiffledForm]

SetUsage @ "
DefineLiteralRiffledForm[symbol$, containerBoxes$, riffleBox$] defines symbol$[$$] to boxify to containerBoxes$ \
with $1 substituted to be a list of boxes riffled with riffleBox$.
* No shortened Katex macro is set up.
* If container$ is RowBox[$1], a %TemplateBox will not be created for zero and single arg cases.
* A fourth argument specifies a box function which can be called to construct the underlying boxes directly.
* DefineLiteralRiffledForm uses only %DefineStandardTraditionalForm internally.
"

DefineLiteralRiffledForm[formSym_Symbol, containerBoxes_, riffledBox_, boxFn_:None] := With[
  {containerFn = Construct[Function, containerBoxes] /. $1 :> Riffle[#1, riffledBox]},
  If[boxFn =!= None,
    boxFn[seq___] := containerFn @ {seq};
    DefineStandardTraditionalForm[formSym[seq___] :> tagAsMath @ boxFn[MakeQGBoxSequence[seq]]]
  ,
    DefineStandardTraditionalForm[formSym[seq___] :> tagAsMath @ containerFn @ Map[MakeQGBoxes, {seq}]]
  ];
];

_DefineLiteralRiffledForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineSymbolForm]

SetUsage @ "
DefineSymbolForm[symbol$ -> boxes$] defines symbol$ to boxify to %TemplateBox[{}, 'symbol'].
* A shortened Katex macro is also set up.
"

SetListable[DefineSymbolForm];

DefineSymbolForm[sym_Symbol -> boxes_] := With[
  {lname = LowerCaseFirst @ SymbolName @ sym},
  DefineStandardTraditionalForm[sym :> SBox[lname]];
  DefineTemplateBox[lname -> boxes]
]

_DefineSymbolForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineLiteralSymbolForm]

SetUsage @ "
DefineLiteralSymbolForm[symbol$ -> boxes$] defines symbol$ to boxify to boxes$.
* No shortened Katex macro is set up.
* DefineLiteralSymbolForm uses only %DefineStandardTraditionalForm internally.
"

SetListable[DefineLiteralSymbolForm];

DefineLiteralSymbolForm[sym_Symbol -> boxes_] :=
  DefineStandardTraditionalForm[sym :> tagAsMath @ boxes];

_DefineLiteralSymbolForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineUnaryStyleForm]

SetUsage @ "
DefineUnaryStyleForm[symbol$, style$] defines symbol$[$$] to boxify to %StyleBox[$$, style$].
* DefineUnaryStyleForm uses %DefineUnaryForm internally.
* A third argument specifies a box function which can be called to construct the underlying boxes directly.
"

DefineUnaryStyleForm[formSym_, style_, boxSym_:None] :=
  DefineUnaryForm[formSym, StyleBox[$1, style], boxSym];

(**************************************************************************************************)

PublicHead[KatexSwitch]

SetUsage @ "
KatexSwitch[wlForm$, kForm$] displays as wlForm$ but converts to Katex as kForm$.
"

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
  HoldPattern[SBox[s_]]                  :> TemplateBox[{}, s],
  HoldPattern[TBox[args___, s_]]         :> TemplateBox[{args}, s],
  HoldPattern[TBoxOp[s_String][args___]] :> TemplateBox[{args}, s]
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
  $templateToKatexFunction[$tName] = fn //. KatexSwitch[a_, b_] :> b;

registerTemplateToKatexFunction[] :=
  $templateToKatexFunction[$tName] = PrefixSlash @ $kName;

(**************************************************************************************************)

(* this takes an arbtirary box function and then processes it to turn it into a templateBoxDisplayFunction
and a katexDefinition, that will live in stylesheet and prelude respectively *)

registerDisplayFunction[displayFn_] := (

  (* set up $templateBoxDisplayFunction, which defines for MMA frontend will display TBox[{a, b}, "head"] at runtime.
     will later produce a style definition in a notebook stylesheet. *)
  $templateBoxDisplayFunction[$tName] = displayFn //. KatexSwitch[a_, b_] :> a;

  (* set up $katexDisplayFunction, which defines how Katex will display "head"[a, b] at runtime.
     will later produce a \gdef expression that goes in the Katex prelude *)
  $katexDisplayFunction[$kName] ^= displayFn //. {
    KatexSwitch[a_, b_] :> b,
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
  registerTemplateBoxRules2[lhs, TemplateBox[MakeQGBoxes /@ syms, tName]];

registerTemplateBoxRules[lhs_, {}, tName_] :=
  registerTemplateBoxRules2[lhs, TemplateBox[{}, tName]];

SetHoldAll[registerStagedTemplateBoxRules];

registerStagedTemplateBoxRules[lhs_, syms_, tName_, withSyms_, withClause__] :=
  registerTemplateBoxRules2[lhs, With[withClause, TemplateBox[Join[withSyms, MakeQGBoxes /@ syms], tName]]];

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

  (* Evaluate forces the FunctionBox to evaluate now, since KatexSwitch needs to be literally present *)
  {$tName, $kName} = setTemplateBoxForm[head, Evaluate @ FunctionBox @ rhs];

  applyRule = HoldPattern[head[args___]] :> TemplateBox[Prepend[MakeQGBoxes /@ {args}, $headBox], "AppliedForm"];
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
  KatexSwitch[SBox @ tName, PrefixSlash @ kName]
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

PublicFunction[DeclareLocalStyles]

DeclareLocalStyles::taggingrules = "Could not update tagging rules.";

SetHoldAll[DeclareLocalStyles];
DeclareLocalStyles[e___] := Scope @ Internal`InheritedBlock[{$templateToKatexFunction},
  $expressionToTemplateBoxRules = <||>;
  $templateBoxDisplayFunction = $katexDisplayFunction = <||>;
  $templateBoxNameCounts = <||>;
  $templateToKatexFunction0 = $templateToKatexFunction;
  (e);
  privateStylesheet = GeneratePrivateQuiverGeometryStylesheet[];
  SetOptions[EvaluationNotebook[], StyleDefinitions -> privateStylesheet];
  katex = EmitKatexFunctionDefinitions[];
  currentRules = Lookup[Options[EvaluationNotebook[], TaggingRules], TaggingRules, <||>];
  If[!AssociationQ[currentRules], ReturnFailed["taggingrules"]];
  $newKatexFuncs = KeyDrop[$templateToKatexFunction, Keys @ $templateToKatexFunction0];
  taggingRules = Join[currentRules, <|"KatexDefinitions" -> katex, "TemplateToKatexFunctions" -> $newKatexFuncs|>];
  SetOptions[EvaluationNotebook[], TaggingRules -> taggingRules];
];

(**************************************************************************************************)

PublicFunction[DBox]

(* this is needed because DisplayFunction -> (RowBox[#]&) where # is a list does not work! *)
DBox[e_] := DynamicBox[e, DestroyAfterEvaluation -> True, TrackedSymbols -> {}];

(**************************************************************************************************)

PublicFunction[RedForm, GreenForm, BlueForm, OrangeForm, PinkForm, TealForm, GrayForm, PurpleForm]
PublicFunction[LightRedForm, LightGreenForm, LightBlueForm, LightOrangeForm, LightPinkForm, LightTealForm, LightGrayForm, LightPurpleForm]
PublicFunction[DarkRedForm, DarkGreenForm, DarkBlueForm, DarkOrangeForm, DarkPinkForm, DarkTealForm, DarkGrayForm, DarkPurpleForm]
PublicFunction[BoldForm, ItalicForm, UnderlinedForm, StruckthroughForm, PlainTextForm, MathTextForm]

PrivateFunction[RedBox, GreenBox, BlueBox, OrangeBox, PinkBox, TealBox, GrayBox, PurpleBox]
PrivateFunction[LightRedBox, LightGreenBox, LightBlueBox, LightOrangeBox, LightPinkBox, LightTealBox, LightGrayBox, LightPurpleBox]
PrivateFunction[DarkRedBox, DarkGreenBox, DarkBlueBox, DarkOrangeBox, DarkPinkBox, DarkTealBox, DarkGrayBox, DarkPurpleBox]
PrivateFunction[BoldBox, ItalicBox, UnderlinedBox, StruckthroughBox, PlainTextBox, MathTextBox]

defineStyleFormAndBox[form_, box_, style_] := With[
  {tName = SymbolName[form]},
  {kName = toKName[tName, 0]},
  MakeBoxes[form[e_], StandardForm]     := box[MakeBoxes[e, StandardForm]];
  MakeBoxes[form[e_], TraditionalForm]  := box[MakeBoxes[e, TraditionalForm]];
  box[e_]                               := TemplateBox[{e}, tName];
  (* TODO: replace this with single call to registerSingleArgTemplateBox *)
  $templateBoxDisplayFunction[tName]    = toFrontendStyleFunction[style];
  $templateToKatexFunction[tName]       = Function[kName[#]];
  $katexDisplayFunction[kName]          = toKatexStyleFunction[style];
];

toFrontendStyleFunction = Case[
  style_                := StyleBox[#1, style]&;
  KatexForm[style_, _]  := % @ style;
];

toKatexStyleFunction = Case[
  color_ ? ColorQ       := % @ StringJoin["textcolor{", ColorHexString @ color, "}"];
  str_String            := Function[str[#]];
  KatexForm[_, style_]  := % @ style;
];

MapApply[
  defineStyleFormAndBox,
  {
    {RedForm,            RedBox,             $Red},
    {GreenForm,          GreenBox,           $Green},
    {BlueForm,           BlueBox,            $Blue},
    {OrangeForm,         OrangeBox,          $Orange},
    {PinkForm,           PinkBox,            $Pink},
    {TealForm,           TealBox,            $Teal},
    {GrayForm,           GrayBox,            $Gray},
    {PurpleForm,         PurpleBox,          $Purple},
    {LightRedForm,       LightRedBox,        $LightRed},
    {LightGreenForm,     LightGreenBox,      $LightGreen},
    {LightBlueForm,      LightBlueBox,       $LightBlue},
    {LightOrangeForm,    LightOrangeBox,     $LightOrange},
    {LightPinkForm,      LightPinkBox,       $LightPink},
    {LightTealForm,      LightTealBox,       $LightTeal},
    {LightGrayForm,      LightGrayBox,       $LightGray},
    {LightPurpleForm,    LightPurpleBox,     $LightPurple},
    {DarkRedForm,        DarkRedBox,         $DarkRed},
    {DarkGreenForm,      DarkGreenBox,       $DarkGreen},
    {DarkBlueForm,       DarkBlueBox,        $DarkBlue},
    {DarkOrangeForm,     DarkOrangeBox,      $DarkOrange},
    {DarkPinkForm,       DarkPinkBox,        $DarkPink},
    {DarkTealForm,       DarkTealBox,        $DarkTeal},
    {DarkGrayForm,       DarkGrayBox,        $DarkGray},
    {DarkPurpleForm,     DarkPurpleBox,      $DarkPurple},
    {BoldForm,           BoldBox,            KatexForm[Bold, "mathbf"]},
    {ItalicForm,         ItalicBox,          KatexForm[Italic, "mathit"]},
    {UnderlinedForm,     UnderlinedBox,      KatexForm[Underlined, "underline"]},
    {StruckthroughForm,  StruckthroughBox,   KatexForm[Struckthrough, "struckthrough"]},
    {PlainTextForm,      PlainTextBox,       KatexForm["MathText", "textrm"]},
    {MathTextForm,       MathTextBox,        KatexForm["MathTextFont", "textrm"]}
  }
];

PrivateSymbol["$colorNameP"]

$colorNameP = Alternatives[
  "RedForm", "GreenForm", "BlueForm", "OrangeForm", "PinkForm", "TealForm", "GrayForm", "PurpleForm",
  "LightRedForm", "LightGreenForm", "LightBlueForm", "LightOrangeForm", "LightPinkForm", "LightTealForm", "LightGrayForm", "LightPurpleForm",
  "DarkRedForm", "DarkGreenForm", "DarkBlueForm", "DarkOrangeForm", "DarkPinkForm", "DarkTealForm", "DarkGrayForm", "DarkPurpleForm"
];

MakeBoxes[PlainTextForm[e_], StandardForm]     := PlainTextBox[MakeQGBoxes @ e];
MakeBoxes[PlainTextForm[e_], TraditionalForm]  := PlainTextBox[MakeQGBoxes @ e];
MakeBoxes[MathTextForm[e_], StandardForm]      := MathTextBox[MakeQGBoxes @ e];
MakeBoxes[MathTextForm[e_], TraditionalForm]   := MathTextBox[MakeQGBoxes @ e];
  
(**************************************************************************************************)

PrivateFunction[FunctionBox]

FunctionBox[e_String] := KatexSwitch[e, StringJoin["\\operatorname{", e, "}"]];

(**************************************************************************************************)

PublicFormBox[ZNamedFunction]

DefineUnaryForm[ZNamedFunctionForm, KatexSwitch[$1, "operatorname"[$1]], ZNamedFunctionBox]

(**************************************************************************************************)

PublicFormBox[ZSpaceRiffled, ZCommaRiffled, ZRiffled]

DefineStandardTraditionalForm[
  ZRiffledForm[seq___, r_] :> ZRiffledBox[MakeQGBoxSequence @ seq, MakeQGBoxes @ r]
];

ZRiffledBox[r_] := "";
ZRiffledBox[e_, r_] := e;
ZRiffledBox[seq__, r_] := RowBox @ Riffle[{seq}, r];

DefineRiffledForm[ZCommaRiffledForm, RowBox[$1], ", ", ZCommaRiffledBox];
DefineRiffledForm[ZSpaceRiffledForm, RowBox[$1], " ", ZSpaceRiffledBox];

(**************************************************************************************************)

PublicFormBox[ZApplied]

DefineStandardTraditionalForm[
  ZAppliedForm[f_, args___] :> ZAppliedBox[MakeQGBoxes @ f, MakeQGBoxSequence @ args]
];

DefineTemplateBox[
  "ZAppliedForm" -> RBox[$1, "(", $2, ")"]
];

ZAppliedBox[f_, args___] := TBox[f, ZCommaRiffledBox @ args, "ZAppliedForm"]

(**************************************************************************************************)

PublicFunction[EvaluateTemplateBox]

EvaluateTemplateBox = BoxForm`TemplateBoxToDisplayBoxes

(**************************************************************************************************)

PrivateFunction[RuntimeTBox]

(* this isn't right, since KatexSwitch has to evaluate ahead of time *)

RuntimeTBox[param_Symbol, arg_] := With[
  {name = SymbolName @ param},
  {kName = toKName[name, 1], tName = toTName[name, 1]},
  KatexSwitch[TemplateBox[{arg, tName}, "ParameterizedTemplateBox"], kName[arg]]
];

(**************************************************************************************************)

PublicOption[NegationStyle, InversionStyle]

SetUsage @ "NegationStyle is an option to CompactNumberForm and other functions."
SetUsage @ "InversionStyle is an option to CompactNumberForm and other functions."

PrivateVariable[$compactNumberOptions]

$compactNumberOptions = {
  NegationStyle -> "Color",
  InversionStyle -> UnderBar
};