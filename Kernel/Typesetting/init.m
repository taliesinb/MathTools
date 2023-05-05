PublicFunction[TBox, TBoxOp, SBox, RBox, GBox, KBox]

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

KBox[wl_, kb_] := TemplateBox[{wl, kb}, "katexSwitch"];

(**************************************************************************************************)

PublicFunction[OpBox, WideOpBox, VeryWideOpBox]

OpBox[b_] := KBox[RBox["\[ThinSpace]", b, "\[ThinSpace]"], KBin @ b];
OpBox["/"] := "/";

WideOpBox[b_] := KBox[RBox[" ", b, " "], KBin @ b];
VeryWideOpBox[b_] := KBox[RBox["  ", b, "  "], KBin @ b];

(**************************************************************************************************)

PublicFunction[OverdotBox, OverdoubledotBox, UnderdotBox]

OverdoubledotBox[b_] := KBox[OverscriptBox[b, ".."], "ddot"[b]];

OverdotBox[b_] := KBox[OverscriptBox[b, "."], "dot"[b]];
OverdotBox["="] := KBox["≐", "\\doteq"];

UnderdotBox[b_] := KBox[UnderscriptBox[b, LowerBox[".", .1]], {"""\underset{\raisebox{0.3em}{.}}{""", b, "}"}];
UnderdotBox["="] := KBox[UnderscriptBox[b, LowerBox[".", .1]], {"""\underset{\raisebox{0.3em}{.}}{""", b, "}"}];

(**************************************************************************************************)

PublicFunction[MarginBox]

MarginBox[boxes_, {left_, right_}] := MarginBox[boxes, {left, right}, {0, 0}];
MarginBox[boxes_, {left_, right_}, {bottom_, top_}] := AdjustmentBox[boxes, BoxMargins -> {{left, right}, {bottom, top}}];

(**************************************************************************************************)

PublicFunction[HatBox]

HatBox[box_] := KBox[OverscriptBox[box, "^"], "hat" @ box];

(**************************************************************************************************)

PublicFunction[UnlimitedSpanBox, ForceKatexCharBox]

UnlimitedSpanBox[e_] := StyleBox[e, SpanAdjustments -> {{0, 0}, {0, 0}}, SpanMaxSize->Infinity, SpanMinSize -> 1.5];

ForceKatexCharBox[e_] := StyleBox[e, FontFamily -> "KaTeX_Main", PrivateFontOptions -> {"OperatorSubstitution" -> False}];

(**************************************************************************************************)

PublicFormBox[Raise, Lower]

DefineStandardTraditionalForm[RaiseForm[e_, n_ ? NumericQ] :> RaiseBox[MakeBoxes @ e, n]];
DefineStandardTraditionalForm[LowerForm[e_, n_ ? NumericQ] :> LowerBox[MakeBoxes @ e, n]];

RaiseBox[e_, n_] := AdjustmentBox[e, BoxBaselineShift -> -n];
LowerBox[e_, n_] := AdjustmentBox[e, BoxBaselineShift -> n];

(**************************************************************************************************)

PublicFunction[KOrd, KBin]

KOrd[k_] := "mathord"[k];
KBin[k_] := "mathbin"[k];

(**************************************************************************************************)

PublicVariable[$PipeBox]

$PipeBox = KBox["|", "\\middle|"];

(**************************************************************************************************)

PublicFunction[FunctionBox]

FunctionBox[e_] := KBox[e, "op"[e]];

(**************************************************************************************************)

PublicFunction[InverseBox]

InverseBox[b_] := SuperscriptBox[b, RBox["-", "1"]];

(**************************************************************************************************)

PublicFunction[Overbracketbox, UnderbracketBox, UnderbraceBox, OverbraceBox, UnderparenthesisBox, OverparenthesisBox]

UnderbraceBox[a_, b_] := KBox[UnderscriptBox[UnderscriptBox[a, "\[UnderBrace]"], b], SubscriptBox["underbrace"[a], b]];
OverbraceBox[a_, b_] := KBox[OverscriptBox[OverscriptBox[a, "\[OverBrace]"], b], SuperscriptBox["overbrace"[a], b]];

UnderbracketBox[a_, b_] := KBox[UnderscriptBox[UnderscriptBox[a, "\[UnderBracket]"], b], SubscriptBox["underbracket"[a], b]];
OverbracketBox[a_, b_] := KBox[OverscriptBox[OverscriptBox[a, "\[OverBracket]"], b], SuperscriptBox["overbracket"[a], b]];

UnderparenthesisBox[a_, b_] := KBox[UnderscriptBox[UnderscriptBox[a, "\[UnderParenthesis]"], b], SubscriptBox["undergroup"[a], b]];
OverparenthesisBox[a_, b_] := KBox[OverscriptBox[OverscriptBox[a, "\[OverParenthesis]"], b], SuperscriptBox["overgroup"[a], b]];

(**************************************************************************************************)

PrivateFunction[LeftBox, RightBox, DelimiterBox, LeftRightBox]

DelimiterBox[e_] := StyleBox[e, "DelimiterFont"];

LeftBox[d_String] := KBox[DelimiterBox @ d, "\\left" <> d];
LeftBox["{"] := KBox[DelimiterBox @ "{", "\\left\\{"];
LeftBox["|"] := KBox["\[LeftBracketingBar]", "\\left\\lvert"];
LeftBox["||"] := KBox["\[LeftDoubleBracketingBar]", "\\left\\lVert"];

RightBox[d_String] := KBox[DelimiterBox @ d, "\\right" <> d];
RightBox["}"] := KBox[DelimiterBox @ "}", "\\right\\}"];
RightBox["|"] := KBox["\[RightBracketingBar]", "\\right\\rvert"];
RightBox["||"] := KBox["\[RightDoubleBracketingBar]", "\\right\\rVert"];

LeftRightBox[l_, inner___, r_] := RBox[LeftBox @ l, inner, RightBox @ r];

_LeftBox := BadArguments[];
_RightBox := BadArguments[];
_LeftRightBox := BadArguments[];

(**************************************************************************************************)

PublicFunction[BracesBox, AngleBracketBox, ParenthesesBox, SquareBracketBox, DoubleSquareBracketBox, BarBox, DoubleBarBox]

BracesBox[inner___]              := LeftRightBox["{", inner, "}"];
AngleBracketBox[inner___]        := LeftRightBox["⟨", inner, "⟩"];
ParenthesesBox[inner___]         := LeftRightBox["(", inner, ")"];
SquareBracketBox[inner___]       := LeftRightBox["[", inner, "]"];
DoubleSquareBracketBox[inner___] := LeftRightBox["⟦", inner, "⟧"];
BarBox[inner___]                 := LeftRightBox["|", inner, "|"];
DoubleBarBox[inner___]           := LeftRightBox["||", inner, "||"];

(**************************************************************************************************)

PublicSymbol[$0, $1, $2, $3, $4, $5, $$1, $$2, $$3]

$slotRules = {$1 -> Slot[1], $2 -> Slot[2], $3 -> Slot[3], $4 -> Slot[4], $5 -> Slot[5], $$1 -> SlotSequence[1], $$2 -> SlotSequence[2], $$3 -> SlotSequence[3]};

toSlotFn[None] := None;

toSlotFn[body_] := Function[body] /. $slotRules;

(**************************************************************************************************)

PublicFunction[SpecializeToNotebookBoxes, SpecializeToKatexBoxes]

SpecializeToNotebookBoxes[e_] := e //. $toWLBoxes;
SpecializeToKatexBoxes[e_] := e //. $toKBoxes;

$toWLBoxes = Dispatch @ {
  HoldPattern[RBox[args___]] :> RowBox[{args}],
  KBox[wl_, _] :> wl, HoldPattern[KBox[wl_, _]] :> wl,
  $wlThinSpace -> "\[ThinSpace]", $kSpace :> Sequence[]
};

$toKBoxes = Dispatch @ {
  HoldPattern[RBox[args___]] :> RowBox[{args}],
  KBox[_, kb_] :> kb, HoldPattern[KBox[_, kb_]] :> kb,
  $wlThinSpace -> Sequence[], $kSpace -> " ",
  TagBox[b_, "QG"] :> b
};

PrivateSymbol[$wlThinSpace, $kSpace]

(**************************************************************************************************)

PublicOption[BoxFunction]

SetUsage @ "
BoxFunction is an option to various DefineXXX functions that specifies a symbol to attach box-construction code to.
"

attachBoxFunctionDefs[None, _] := Null;
attachBoxFunctionDefs[sym_Symbol, fn_] := SetDelayed[sym[args___], fn[args]];

(**************************************************************************************************)

PublicOption[HeadBoxes]

SetUsage @ "
HeadBoxes is an option to various DefineXXX functions that specifies boxes to associate with the pure head.
"

attachHeadBoxes[None, _] := Null;
attachHeadBoxes[boxes_, sym_Symbol] := DefineStandardTraditionalForm[sym :> boxes];

(**************************************************************************************************)

PublicOption[KatexMacroName]

SetUsage @ "
KatexMacroName is an option to various DefineXXX functions that specifies the name of the Katex global macro to set up.
* The following settings can be used:
| None | the macro will be used and literal expansion will be used instead |
| Automatic | a name will be derived from the template box name |
| 'name$' | the given name will be used |
"

$kNameRules = {RegularExpression @ "^[a-z][a-z]?", "Gray" -> "G", RegularExpression @ "[A-Z][A-Za-z]", "1" -> "Ⅰ", "2" -> "Ⅱ", "3" -> "Ⅲ", "4" -> "Ⅳ"};
templateNameToMacroName = Case[
  str_String ? LowerCaseQ := StringTake[str, 2];
  str_String := StringJoin @ StringCases[str, $kNameRules];
];

(**************************************************************************************************)

PublicOption[TemplateName]

SetUsage @ "
TemplateName is an option to various DefineXXX functions that specifies the name of the template box.
* The following settings can be used:
| 'name$' | use the given name |
| Automatic | use the name of the associated symbol |
"

makeTemplateName[symbol_Symbol, Automatic] := SymbolName @ symbol;
makeTemplateName[symbol_, name_String] := name;

_makeTemplateName := BadArguments[];

(**************************************************************************************************)

PublicOption[Boxification]

SetUsage @ "
Boxification is an option to various DefineXXX functions that specifies the function to boxify arguments.
"

toSequenceBoxifyFn[MakeQGBoxes] = MakeQGBoxSequence;
toSequenceBoxifyFn[fn_] := Function[Null, Sequence @@ MapUnevaluated[fn, {##}]];

(**************************************************************************************************)

$defineOpts = {
  BoxFunction -> None,
  Boxification -> MakeQGBoxes,
  KatexMacroName -> None,
  TemplateName -> Automatic,
  HeadBoxes -> None
};

(**************************************************************************************************)

PrivateFunction[tagAsMath]

tagAsMath[t_] := TagBox[t /. TagBox[e_, "QG"] :> e, "QG"];

(**************************************************************************************************)

PublicVariable[$notebookDisplayFunction, $katexDisplayFunction, $katexMacros, $symbolToTemplateName, $symbolToKatexMacroName]

PublicFunction[ClearTemplateBoxDefinitions]

ClearTemplateBoxDefinitions[] := (
  $notebookDisplayFunction = $katexDisplayFunction = $katexMacros = $symbolToTemplateName = $symbolToKatexMacroName = <||>;
);

ClearTemplateBoxDefinitions[];

(**************************************************************************************************)

PrivateFunction[DefineNotebookDisplayFunction, DefineKatexDisplayFunction, DefineKatexMacro]

DefineNotebookDisplayFunction[templateName_String, fn_Function] := (
  $notebookDisplayFunction[templateName] = SpecializeToNotebookBoxes[fn];
);

DefineKatexDisplayFunction[templateName_String, fn_Function] := (
  $katexDisplayFunction[templateName] = SpecializeToKatexBoxes[fn];
);

DefineKatexDisplayFunction[templateName_, fn_, None] :=
  DefineKatexDisplayFunction[templateName, fn];

DefineKatexDisplayFunction[templateName_, fn_, macroName_String] := (
  $katexDisplayFunction[templateName] = macroName;
  DefineKatexMacro[macroName, fn];
);

DefineKatexMacro[name_String, fn_Function] := (
  $katexMacros[name] = SpecializeToKatexBoxes[fn];
);

(**************************************************************************************************)

AssociateSymbolToTemplateName[sym_Symbol, name_String] := KeyAppendTo[$symbolToTemplateName, sym, name];
AssociateSymbolToKatexMacro[sym_Symbol, name_String]   := KeyAppendTo[$symbolToKatexMacroName, sym, name];
AssociateSymbolToKatexMacro[sym_Symbol, None]          := Null;

(**************************************************************************************************)

_DefineNotebookDisplayFunction = BadArguments[];
_DefineKatexDisplayFunction = BadArguments[];
_DefineKatexMacro = BadArguments[];
_AssociateSymbolToTemplateName = BadArguments[];
_AssociateSymbolToKatexMacro = BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineTemplateBox]

(* kmacro: If Automatic, base name on template box name, if None, don't set up a macro *)
DefineTemplateBox[symbol_Symbol, templateName_String, boxes_, katexMacroName_] := Scope[
  SetAutomatic[katexMacroName, templateNameToMacroName @ templateName];
  AssociateSymbolToTemplateName[symbol, templateName];
  AssociateSymbolToKatexMacro[symbol, katexMacroName];
  fn = toSlotFn @ boxes;
  DefineNotebookDisplayFunction[templateName, fn];
  DefineKatexDisplayFunction[templateName, fn, katexMacroName];
  fn
];

_DefineTemplateBox = BadArguments[];

(**************************************************************************************************)

DefineKatexMacro["op", "operatorname"[#1]&];

(**************************************************************************************************)

PublicForm[KatexSwitch]

SetUsage @ "
KatexSwitch[wlForm$, kForm$] displays as wlForm$ but converts to Katex as kForm$.
"

DefineStandardTraditionalForm[KatexSwitch[wl_, k_] :> KBox[MakeQGBoxes @ wl, MakeQGBoxes @ k]];
DefineTemplateBox[KatexSwitch, "katexSwitch", KBox[$1, $2], None]

(**************************************************************************************************)

PublicFunction[TemplateNameQ]

TemplateNameQ[name_] := Or[
  KeyExistsQ[$katexDisplayFunction, name],
  AssociationQ[$localKatexDisplayFunction] && KeyExistsQ[$localKatexDisplayFunction, name]
];

(**************************************************************************************************)

PublicFunction[PrintTemplateBoxDefinitions]

PrintTemplateBoxDefinitions[] := Scope[
  Print @ SpacedColumn[
    "Display functions" -> SpacedRow[
      "Template" -> Grid[$notebookDisplayFunction],
      "Katex" -> Grid[$katexDisplayFunction]
    ],
    "KatexDefinitions" -> Pane @ EmitKatexFunctionDefinitions[],
    Spacings -> 100
  ];
]

(**************************************************************************************************)

PublicFormBox[Riffled]

DefineStandardTraditionalForm[
  RiffledForm[head_][args___] :> RiffledBox[MakeQGBoxes @ head][MakeQGBoxSequence @ args]
]

DefineNotebookDisplayFunction["RiffledForm", Function[RowBox[{TemplateSlotSequence[2, #1]}]]];
DefineKatexDisplayFunction["RiffledForm", Function[Riffle[{##2}, #1]]];
AssociateSymbolToTemplateName[RiffledForm, "RiffledForm"]

RiffledBox[rif_][args___] := TemplateBox[{rif, args}, "RiffledForm"];

(**************************************************************************************************)

PublicFormBox[CommaRow]

DefineStandardTraditionalForm[
  CommaRowForm[a___] :> CommaRowBox[MakeQGBoxSequence[a]]
];

CommaRowBox[] := ""
CommaRowBox[a_] := a;
CommaRowBox[a__] := TBox[a, "CommaRowForm"];

DefineNotebookDisplayFunction["CommaRowForm", Function[RowBox[{TemplateSlotSequence[1, ", "]}]]];
DefineKatexDisplayFunction["CommaRowForm", Riffle[{##}, ","]&];
AssociateSymbolToTemplateName[CommaRowForm, "CommaRowForm"];

(**************************************************************************************************)

PublicFormBox[TightRow]

DefineStandardTraditionalForm[
  TightRowForm[a___] :> TightRowBox[MakeQGBoxSequence[a]]
];

TightRowBox[] := RBox[];
TightRowBox[a_] := a;
TightRowBox[a__] := TBox[a, "TightRowForm"];

DefineNotebookDisplayFunction["TightRowForm", Function[RowBox[{TemplateSlotSequence[1]}]]];
DefineKatexDisplayFunction["TightRowForm", {##}&];
AssociateSymbolToTemplateName[TightRowForm, "TightRowForm"];

(**************************************************************************************************)

SetRelatedSymbolGroup[DefineTaggedForm, DefineUnaryForm]
SetRelatedSymbolGroup[DefineUnaryForm, DefineBinaryForm, DefineTernaryForm, DefineNAryForm]
SetRelatedSymbolGroup[DefineBinaryForm, DefineIndexedBinaryForm]
SetRelatedSymbolGroup[DefineInfixForm]
SetRelatedSymbolGroup[DefineUnaryForm, DefineStyleForm]

(**************************************************************************************************)

PublicFunction[DefineTaggedForm]

PublicOption[Aliases]

SetUsage @ "
DefineTaggedForm[symbol$] defines symbol$[arg$1] to boxify to %TemplateBox[{arg$1}, 'symbol$'], \
which displays as arg$1.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1}.
* The option Aliases specifies what string arguments should be rewritten to arbitrary expressions.
"

SetListable[DefineTaggedForm];

Options[DefineTaggedForm] = JoinOptions[$defineOpts, Aliases -> None];

DefineTaggedForm[formSym_Symbol, OptionsPattern[]] := With[
  {name = makeTemplateName[formSym, OptionValue[TemplateName]]},
  {boxify = OptionValue[Boxification], aliases = OptionValue[Aliases]},
  DefineStandardTraditionalForm[formSym[e_] :> TBox[boxify @ e, name]];
  DefineTemplateBox[formSym, name, $1, OptionValue[KatexMacroName]];
  If[AssociationQ[aliases], DefineStandardTraditionalForm[
      formSym[alias_String /; KeyExistsQ[aliases, alias]] :> ToBoxes @ formSym[Lookup[aliases, alias]]
  ]];
];

_DefineTaggedForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineUnaryForm]

SetUsage @ "
DefineUnaryForm[symbol$, boxes$] defines symbol$[arg$1] to boxify to %TemplateBox[{arg$1}, 'symbol$'], \
which displays as boxes$ where $1 is substituted.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
"

Options[DefineUnaryForm] = $defineOpts;

DefineUnaryForm[formSym_Symbol, boxes_, OptionsPattern[]] := With[
  {name = makeTemplateName[formSym, OptionValue[TemplateName]]},
  {boxify = OptionValue[Boxification]},
  {fn = TBox[#1, name]&},
  attachBoxFunctionDefs[OptionValue[BoxFunction], fn];
  attachHeadBoxes[OptionValue[HeadBoxes], formSym];
  DefineStandardTraditionalForm[formSym[e_] :> fn[boxify @ e]];
  DefineTemplateBox[formSym, name, boxes, OptionValue[KatexMacroName]]
];

_DefineUnaryForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineBinaryForm]

SetUsage @ "
DefineBinaryForm[symbol$, boxes$] defines symbol$[arg$1, arg$2] to boxify to %TemplateBox[{arg$1, arg$2}, 'symbol$'], \
which displays as boxes$ where $1, $2 are substituted.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1, arg$2}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
"

Options[DefineBinaryForm] = $defineOpts;

DefineBinaryForm[formSym_Symbol, boxes_, OptionsPattern[]] := With[
  {name = makeTemplateName[formSym, OptionValue[TemplateName]]},
  {boxify = OptionValue[Boxification]},
  {fn = TBox[#1, #2, name]&},
  attachBoxFunctionDefs[OptionValue[BoxFunction], fn];
  attachHeadBoxes[OptionValue[HeadBoxes], formSym];
  DefineStandardTraditionalForm[formSym[a_, b_] :> fn[boxify @ a, boxify @ b]];
  DefineTemplateBox[formSym, name, boxes, OptionValue[KatexMacroName]]
];

_DefineBinaryForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineTernaryForm]

Options[DefineTernaryForm] = $defineOpts;

SetUsage @ "
DefineTernaryForm[symbol$, boxes$] defines symbol$[arg$1, arg$2, arg$3] to boxify to %TemplateBox[{arg$1, arg$2, arg$3}, 'symbol$'], \
which displays as boxes$ where $1, $2, $3 are substituted.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1, arg$2}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
"

DefineTernaryForm[formSym_Symbol, boxes_, OptionsPattern[]] := With[
  {name = makeTemplateName[formSym, OptionValue[TemplateName]]},
  {boxify = OptionValue[Boxification]},
  {fn = TBox[#1, #2, #3, name]&},
  attachBoxFunctionDefs[OptionValue[BoxFunction], fn];
  attachHeadBoxes[OptionValue[HeadBoxes], formSym];
  DefineStandardTraditionalForm[formSym[a_, b_, c_] :> fn[boxify @ a, boxify @ b, boxify @ c]];
  DefineTemplateBox[formSym, name, boxes, OptionValue[KatexMacroName]]
];

_DefineTernaryForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineNAryForm]

Options[DefineNAryForm] = $defineOpts;

SetUsage @ "
DefineNAryForm[symbol$, boxes$] defines symbol$[arg$1, $$, arg$n] to boxify to %TemplateBox[{arg$1, $$, arg$n}, 'symbol$'], \
which displays as boxes$ where $1, $2, $3 are substituted.
* No %KatexMacroName option is available, because katex macros do not support variable arity.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
"

DefineNAryForm[formSym_Symbol, boxes_, OptionsPattern[]] := With[
  {name = makeTemplateName[formSym, OptionValue[TemplateName]]},
  {boxify = toSequenceBoxifyFn[OptionValue[Boxification]]},
  {fn = TBox[##, name]&},
  attachBoxFunctionDefs[OptionValue[BoxFunction], fn];
  attachHeadBoxes[OptionValue[HeadBoxes], formSym];
  DefineStandardTraditionalForm[formSym[seq___] :> fn @ boxify @ seq];
  DefineTemplateBox[formSym, name, boxes, OptionValue[KatexMacroName]]
];

_DefineNAryForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineInfixForm]

Options[DefineInfixForm] = JoinOptions[$defineOpts, HeadBoxes -> Automatic];

SetUsage @ "
DefineInfixForm[symbol$, infixBox$] defines symbol$[arg$1, arg$2, $$] to boxify to %TemplateBox[{arg$1, infixBox$, arg$2, infixBox$, $$}, 'symbol$'], \
which displays as RowBox[{arg$1, infixBox$, arg$2, infixBox$, $$}].
* The bare head symbol$ just displays as infixBox$.
* symbol$[] display as nothing.
* symbol$[arg$1] displays as arg$1.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1, arg$2, $$}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
"

(* TODO: replace Padding mechanism by unifying mathbin / mathop on the K side with thin space padding on the M side *)

DefineInfixForm[formSym_Symbol, infixBox_, opts:OptionsPattern[]] :=
  DefineNAryForm[formSym, RiffledBox[infixBox][$$1], HeadBoxes -> infixBox, FilterOptions @ opts];

_DefineInfixForm := BadArguments[];

(**************************************************************************************************)

PublicFormBox[Applied]

DefineStandardTraditionalForm[{
  AppliedForm[fn_, args___] :> AppliedBox[MakeQGBoxes @ fn, MakeQGBoxSequence @ args]
}];

AppliedBox[fn_, args__] := TBox[fn, CommaRowBox[args], "appliedForm"];
AppliedBox[fn_] := TBox[fn, "emptyAppliedForm"];

DefineTemplateBox[AppliedForm, "appliedForm", RBox[$1, KBox["(", "\\lparen "], $2, KBox[")", "\\rparen "]], None];
DefineTemplateBox[AppliedForm, "emptyAppliedForm", RBox[$1, KBox["(", "\\lparen "], KBox[")", "\\rparen "]], None];

(**************************************************************************************************)

PublicFunction[DefineInfixBinaryForm]

Options[DefineInfixBinaryForm] = Options @ DefineInfixForm;

SetUsage @ "
DefineInfixBinaryForm[symbol$, infixBox$] defines symbol$[arg$1, arg$2] to boxify to %TemplateBox[{arg$1, arg$2}, 'symbol$'], \
which displays as RowBox[{arg$1, infixBox$, arg$2}].
* The bare head symbol$ just displays as infixBox$.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1, arg$2, $$}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
"

DefineInfixBinaryForm[formSym_Symbol, infixBox_, OptionsPattern[]] :=
  DefineBinaryForm[formSym, RBox[$1, infixBox, $2], HeadBoxes -> infixBox, FilterOptions @ opts];

_DefineInfixBinaryForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineIndexedInfixBinaryForm]

SetUsage @ "
DefineIndexedInfixBinaryForm[symbol$, boxes$] defines symbol$[arg$1, arg$2] to boxify to %TemplateBox[{arg$1, arg$2}, 'symbol$'], \
which displays as RowBox[{arg$1, infixBox$, arg$2}], and defines symbol$[arg$1, arg$2, arg$3] to boxify to \
%TemplateBox[{arg$1, arg$2, arg$3, 'symbol$indexed'], which displays as RowBox[{arg$1, SubscriptBox[infixBox$, arg$3], arg$2}].
"

Options[DefineIndexedInfixBinaryForm] = Options @ DefineInfixForm;

DefineIndexedInfixBinaryForm[formSym_Symbol, infixBox_, opts:OptionsPattern[]] := (
  DefineUnaryForm[formSym, OpBox @ SubscriptBox[infixBox, $1], TemplateName -> StringJoin[SymbolName @ formSym, "Head"]];
  DefineTernaryForm[formSym, RBox[$1, OpBox @ SubscriptBox[infixBox, $3], $2], FilterOptions @ opts];
);

_DefineIndexedInfixBinaryForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineCommaForm]

PublicOption[HeadBoxes]

Options[DefineCommaForm] = $defineOpts;

SetUsage @ "
DefineCommaForm[symbol$, boxes$] defines symbol$[arg$1, arg$2, $$] to boxify to %TemplateBox[{arg$1, arg$2, $$}, 'symbol$'], \
which displays as boxes$ with $1 substituted with CommaRowBox[arg$1, arg$2, $$].
* The option %HeadBoxes can be used to define how symbol% itself should boxify.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1, arg$2}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
";

DefineCommaForm[formSym_Symbol, boxes_, OptionsPattern[]] := With[
  {name = makeTemplateName[formSym, OptionValue[TemplateName]]},
  {boxify = toSequenceBoxifyFn[OptionValue[Boxification]]},
  {fn = TBox[CommaRowBox[##], name]&},
  attachBoxFunctionDefs[OptionValue[BoxFunction], fn];
  defineHeadboxes[formSymbol, OptionValue[HeadBoxes]];
  DefineStandardTraditionalForm[formSym[seq___] :> fn @ boxify @ seq];
  DefineTemplateBox[formSym, name, boxes, OptionValue[KatexMacroName]]
];

_DefineCommaForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineRestCommaForm]

Options[DefineRestCommaForm] = $defineOpts;

SetUsage @ "
DefineRestCommaForm[symbol$, boxes$] defines symbol$[arg$1, arg$2, $$] to boxify to %TemplateBox[{arg$1, arg$2}, 'symbol$'], \
which displays as boxes$ with $2 substituted with CommaRowBox[arg$1, arg$2, $$].
* The option %HeadBoxes can be used to define how symbol% itself should boxify.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1, arg$2}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
";

DefineRestCommaForm[formSym_Symbol, boxes_, OptionsPattern[]] := With[
  {name = makeTemplateName[formSym, OptionValue[TemplateName]]},
  {boxify = toSequenceBoxifyFn[OptionValue[Boxification]]},
  {fn = TBox[#1, CommaRowBox[##2], name]&},
  attachBoxFunctionDefs[OptionValue[BoxFunction], fn];
  defineHeadboxes[formSymbol, OptionValue[HeadBoxes]];
  DefineStandardTraditionalForm[formSym[seq__] :> fn @ boxify @ seq];
  DefineTemplateBox[formSym, name, boxes, OptionValue[KatexMacroName]]
];

_DefineRestCommaForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineRuleAsMapsTo]

DefineRuleAsMapsTo[head_] := DefineStandardTraditionalForm[
  head[l___, Rule[lhs_, rhs_], r___] :> MakeBoxes[head[l, MapsToForm[lhs, rhs], r]]
]

(**************************************************************************************************)

PublicFunction[DefineSymbolForm]

Options[DefineSymbolForm] = $defineOpts

SetUsage @ "
DefineSymbolForm[symbol$ -> boxes$] defines symbol$ to boxify to %TemplateBox[{}, 'symbol$'].
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym.
"

SetListable[DefineSymbolForm];

DefineSymbolForm[sym_Symbol -> boxes_, OptionsPattern[]] := With[
  {name = makeTemplateName[sym, OptionValue[TemplateName]]},
  DefineStandardTraditionalForm[sym :> SBox[name]];
  DefineTemplateBox[sym, name, boxes, OptionValue[KatexMacroName]]
]

_DefineSymbolForm := BadArguments[];

(**************************************************************************************************)

PublicFunction[DefineNamedFunctionSymbolForm]

DefineNamedFunctionSymbolForm = Case[
  list_List                 := Map[%, list];
  sym_Symbol                := %[sym -> ToLowerCase @ StringTrimRight[SymbolName @ sym, "Function"]];
  sym_Symbol -> name_       := DefineStandardTraditionalForm[{
    sym          :> FunctionBox @ name,
    sym[args___] :> ToBoxes @ AppliedForm[sym, args]
  }];
];

_DefineNamedFunctionSymbolForm := BadArguments[];

(**************************************************************************************************)

PrivateFunction[KConstruct]

KConstruct[str_String] := StringJoin["\\", str, " "];
KConstruct[str_String, args__] := str[args];

(**************************************************************************************************)

PublicFunction[DefineStyleForm]

Options[DefineStyleForm] = $defineOpts;

SetUsage @ "
DefineStyleForm[symbol$, style$] defines symbol$[$$] to boxify to %StyleBox[$$, style$].
* DefineStyleForm uses %DefineUnaryForm internally.
"

DefineStyleForm[formSym_, style_, opts:OptionsPattern[]] := (
  $styleFormData[formSym] = style;
  DefineUnaryForm[formSym, StyleBox[$1, style], opts];
);

(**************************************************************************************************)

PrivateFunction[StyleFormHeadQ, StyleFormData]

$styleFormData = UAssociation[];

StyleFormHeadQ[s_Symbol] := StyleFormData[s] =!= None;
StyleFormHeadQ[_] := False;

StyleFormData[s_Symbol] := Lookup[$styleFormData, s, None];

(**************************************************************************************************)

PublicForm[IndexedForm]

DefineStandardTraditionalForm[{
  IndexedForm[head_]                  :> MakeQGBoxes @ head,
  IndexedForm[head_, Null|None]       :> MakeQGBoxes @ IndexedForm[head],
  IndexedForm[head_, sub_]            :> SubscriptBox[MakeQGBoxes @ head, makeSubSupBoxes @ sub],
  IndexedForm[head_, sub_, Null|None] :> MakeBoxes @ IndexedForm[head, sub],
  IndexedForm[head_, sub_, sup_]      :> SubsuperscriptBox[MakeQGBoxes @ head, makeSubSupBoxes @ sub, makeSubSupBoxes @ sup],
  IndexedForm[head_, Null|None, sup_] :> SuperscriptBox[MakeQGBoxes @ head, makeSubSupBoxes @ sup],
  (i_IndexedForm)[arg_]               :> RBox[MakeBoxes @ i, " ", MakeQGBoxes @ arg],
  (i_IndexedForm)[arg_, cond_]        :> RBox[MakeBoxes @ i, " ", MakeQGBoxes @ SuchThatForm[arg, cond]]
}]

PublicFunction[DefineLegacyIndexedForm]

DefineLegacyIndexedForm[head_Symbol, boxes_] := DefineStandardTraditionalForm[{
  head                          :> boxes,
  head[arg_, rest___]           :> MakeBoxes @ IndexedForm[RawBoxes @ boxes, rest][arg],
  head[arg_, a_, b_, cond_] :> MakeBoxes @ IndexedForm[RawBoxes @ boxes, a, b][arg, cond]
}]

(**************************************************************************************************)

SetHoldAllComplete[makeSubSupBoxes]
makeSubSupBoxes = Case[
  list_List := MakeBoxes @ SubstackForm @ list;
  e_        := MakeQGBoxes @ e;
];

(**************************************************************************************************)

PublicFunction[DefineLocalTemplates]

DefineLocalTemplates::taggingrules = "Could not update tagging rules.";

SetHoldAll[DefineLocalTemplates];
DefineLocalTemplates[e___] := Scope @ Internal`InheritedBlock[{$katexMacros, $katexDisplayFunction},
  $notebookDisplayFunction = <||>;
  $katexMacros0 = $katexMacros;
  $katexDisplayFunction0 = $katexDisplayFunction;
  (e);
  privateStylesheet = GeneratePrivateQuiverGeometryStylesheet[];
  SetOptions[EvaluationNotebook[], StyleDefinitions -> privateStylesheet];
  currentRules = Lookup[Options[EvaluationNotebook[], TaggingRules], TaggingRules, <||>];
  If[!AssociationQ[currentRules], ReturnFailed["taggingrules"]];
  $katexDisplayFunction1 = KeyDrop[$katexDisplayFunction, Keys @ $katexDisplayFunction0];
  $katexMacros1 = KeyDrop[$katexMacros, Keys @ $katexMacros0];
  taggingRules = Join[currentRules, <|"KatexDisplayFunctions" -> $katexDisplayFunction1, "KatexMacros" -> $katexMacros1|>];
  SetOptions[EvaluationNotebook[], TaggingRules -> taggingRules];
];

(**************************************************************************************************)

PublicFunction[DBox]

(* this is needed because DisplayFunction -> (RowBox[#]&) where # is a list does not work! *)
DBox[e_] := DynamicBox[e, DestroyAfterEvaluation -> True, TrackedSymbols -> {}];

(**************************************************************************************************)

PublicFormBox[Red, Green, Blue, Orange, Pink, Teal, Gray, Purple]
PublicFormBox[LightRed, LightGreen, LightBlue, LightOrange, LightPink, LightTeal, LightGray, LightPurple]
PublicFormBox[DarkRed, DarkGreen, DarkBlue, DarkOrange, DarkPink, DarkTeal, DarkGray, DarkPurple, MultisetColor]

PublicFormBox[Bold, Italic, Underlined, Struckthrough, PlainText, MathText, Roman, Fraktur, Caligraphic, SansSerif, Typewriter]

SystemSymbol[ScriptForm]
PublicSymbol[ScriptBox]

Unprotect[ScriptForm]; (* it's an undocumented system symbol! *)

DefineStyleForm[#1, #3, BoxFunction -> #2]& @@@ ExpressionTable[
  RedForm             RedBox             $Red
  GreenForm           GreenBox           $Green
  BlueForm            BlueBox            $Blue
  OrangeForm          OrangeBox          $Orange
  PinkForm            PinkBox            $Pink
  TealForm            TealBox            $Teal
  GrayForm            GrayBox            $Gray
  PurpleForm          PurpleBox          $Purple
  LightRedForm        LightRedBox        $LightRed
  LightGreenForm      LightGreenBox      $LightGreen
  LightBlueForm       LightBlueBox       $LightBlue
  LightOrangeForm     LightOrangeBox     $LightOrange
  LightPinkForm       LightPinkBox       $LightPink
  LightTealForm       LightTealBox       $LightTeal
  LightGrayForm       LightGrayBox       $LightGray
  LightPurpleForm     LightPurpleBox     $LightPurple
  DarkRedForm         DarkRedBox         $DarkRed
  DarkGreenForm       DarkGreenBox       $DarkGreen
  DarkBlueForm        DarkBlueBox        $DarkBlue
  DarkOrangeForm      DarkOrangeBox      $DarkOrange
  DarkPinkForm        DarkPinkBox        $DarkPink
  DarkTealForm        DarkTealBox        $DarkTeal
  DarkGrayForm        DarkGrayBox        $DarkGray
  DarkPurpleForm      DarkPurpleBox      $DarkPurple
  MultisetColorForm   MultisetColorBox   RGBColor[{0.73, 0.27, 0.27}]
  BoldForm            BoldBox            Bold
  UnderlinedForm      Underlinedbox      Underlined
  ItalicForm          ItalicBox          Italic
  UnderlinedForm      UnderlinedBox      Underlined
  StruckthroughForm   StruckthroughBox   Struckthrough
  PlainTextForm       PlainTextBox       "MathText"
  MathTextForm        MathTextBox        "MathTextFont"
  RomanForm           RomanBox           "RomanMathFont"
  ScriptForm          ScriptBox          "ScriptMathFont"
  FrakturForm         FrakturBox         "FrakturMathFont"
  CaligraphicForm     CaligraphicBox     "CaligraphicMathFont"
  SansSerifForm       SansSerifBox       "SansSerifMathFont"
  TypewriterForm      TypewriterBox      "TypewriterMathFont"
];

(**************************************************************************************************)

PublicForm[Color1Form, Color2Form, Color3Form, Color4Form, Color5Form, Color6Form, Color7Form, Color8Form, ColorNForm]

PublicForm[Background1Form, Background2Form, Background3Form, Background4Form, Background5Form, Background6Form, Background7Form, Background8Form, BackgroundNForm]

ColorNForm[n_Integer] := Part[{Color1Form, Color2Form, Color3Form, Color4Form, Color5Form, Color6Form, Color7Form, Color8Form}, n];

DefineStyleForm[#1, currentStyleSetting[FontColor, #2]]& @@@ ExpressionTable[
  Color1Form  "Color1"
  Color2Form  "Color2"
  Color3Form  "Color3"
  Color4Form  "Color4"
  Color5Form  "Color5"
  Color6Form  "Color6"
  Color7Form  "Color7"
  Color8Form  "Color8"
]

BackgroundNForm[n_Integer] := Part[{Background1Form, Background2Form, Background3Form, Background4Form, Background5Form, Background6Form, Background7Form, Background8Form}, n];

DefineStyleForm[#1, currentStyleSetting[Background, #2]]& @@@ ExpressionTable[
  Background1Form  "Background1"
  Background2Form  "Background2"
  Background3Form  "Background3"
  Background4Form  "Background4"
  Background5Form  "Background5"
  Background6Form  "Background6"
  Background7Form  "Background7"
  Background8Form  "Background8"
]

(**************************************************************************************************)

PublicFunction[EvaluateTemplateBox, EvaluateTemplateBoxFull]

EvaluateTemplateBox[expr_] := ReplaceAll[expr, tb:TemplateBox[_List, _String] :> RuleCondition @ evalTB[tb]];
EvaluateTemplateBoxFull[expr_] := ReplaceRepeated[expr, tb:TemplateBox[_List, _String] :> RuleCondition @ evalTB[tb]];

evalTB := Case[
  TemplateBox[args_List, name_String] /; KeyExistsQ[$notebookDisplayFunction, name] := Apply[$notebookDisplayFunction @ name, args];
  tb:TemplateBox[_List, _String]                                                    := BoxForm`TemplateBoxToDisplayBoxes[tb];
];

PublicFunction[EvaluateTemplateBoxAsKatex, EvaluateTemplateBoxAsKatexFull]

EvaluateTemplateBoxAsKatex[expr_] := ReplaceAll[expr, tb:TemplateBox[_List, _String] :> RuleCondition @ evalTBK[tb]];
EvaluateTemplateBoxAsKatexFull[expr_] := ReplaceRepeated[expr, tb:TemplateBox[_List, _String] :> RuleCondition @ evalTBK[tb]];

evalTBK := Case[
  TemplateBox[args_List, name_String] /; KeyExistsQ[$katexDisplayFunction, name] := Apply[$katexDisplayFunction @ name, args];
  other_ := other;
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
