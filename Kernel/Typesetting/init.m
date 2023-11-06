PrivateTypesettingBoxFunction[TBox, TBoxOp, SBox, RBox, GBox, KBox]

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

PrivateVariable[$unaryFormHeadQ, $styleFormHeadQ, $modifierFormHeadQ, $taggedFormHeadQ, $infixFormHeadQ, $binaryFormHeadQ, $naryFormHeadQ, $symbolFormHeadQ]

SetInitialValue[$unaryFormHeadQ, $styleFormHeadQ, $modifierFormHeadQ, $taggedFormHeadQ, $infixFormHeadQ, $binaryFormHeadQ, $naryFormHeadQ, $symbolFormHeadQ, UAssoc[]];

$styleFormHeadQ[Style] = True;

(**************************************************************************************************)

(* this is so that ApplyScriptScaling has a registry to work with *)

PrivateSpecialFunction[registerFormScriptingArgPositions]
PrivateVariable[$formScriptingArgumentPositions]

SetInitialValue[$formScriptingArgumentPositions, UAssoc[]]

registerFormScriptingArgPositions[heads_List, arg_] := Scan[registerFormScriptingArgPositions[#, arg]&, heads];
registerFormScriptingArgPositions[head_Symbol, pos_] := (
  KeyUnionTo[$formScriptingArgumentPositions, head, ToList[pos]];
);
registerFormScriptingArgPositions[_Symbol, {}] := Null;

_registerFormScriptingArgPositions := BadArguments[];

registerFormScriptingArgPositions[{Subscript, Superscript}, 2];
registerFormScriptingArgPositions[Subsuperscript, {2, 3}]

(**************************************************************************************************)

PrivateSpecialFunction[setupFormDefinitionCaching, clearFormDefinitionCache]

CacheSymbol[$FormDefinitionCache]

setupFormDefinitionCaching[fn_Symbol] := (
  expr_fn /; TrueQ[$fdCacheEnabled] := Block[
    {key = getFDCacheKey[expr], hash = Hash[Unevaluated @ expr], res, pair, $fdCacheEnabled = False},
    pair = Lookup[$FormDefinitionCache, key];
    If[ListQ[pair] && PN[pair] === hash,
      res = P1 @ pair
    ,
      res = expr;
      AssociateTo[$FormDefinitionCache, key -> {res, hash}];
    ];
    res
  ];
);

clearFormDefinitionCache[] := (
  $formDefinitionCache = UAssoc[];
)

$fdCacheEnabled = True;

SetHoldAllComplete[getFDCacheKey];
getFDCacheKey[e:(fn_[sym_, ___])] := Hold[fn, sym];
_getFDCacheKey := BadArguments[];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[OpBox, WideOpBox, VeryWideOpBox]

OpBox[b_] := KBox[RBox["\[ThinSpace]", b, "\[ThinSpace]"], KBin @ b];
OpBox["/"] := "/";

WideOpBox[b_] := KBox[RBox[" ", b, " "], KBin @ b];
VeryWideOpBox[b_] := KBox[RBox["  ", b, "  "], KBin @ b];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[OverdotBox, OverdoubledotBox, UnderdotBox]

OverdoubledotBox[b_] := KBox[OverscriptBox[b, ".."], "ddot"[b]];

OverdotBox[b_] := KBox[OverscriptBox[b, "."], "dot"[b]];
OverdotBox["="] := KBox["≐", "\\doteq"];

UnderdotBox[b_] := KBox[UnderscriptBox[b, LowerBox[".", .1]], {"""\underset{\raisebox{0.3em}{.}}{""", b, "}"}];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[MarginBox]

MarginBox[boxes_, {left_, right_}] := MarginBox[boxes, {left, right}, {0, 0}];
MarginBox[boxes_, {left_, right_}, {bottom_, top_}] := AdjustmentBox[boxes, BoxMargins -> {{left, right}, {bottom, top}}];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[HatBox]

HatBox[box_] := KBox[OverscriptBox[box, "^"], "hat" @ box];

(**************************************************************************************************)

PublicTypesettingFormBox[NoSpanForm, UnlimitedSpanForm]

DefineStandardTraditionalForm[{
  NoSpanForm[e_]        :> NoSpanBox @ MakeBoxes @ e,
  UnlimitedSpanForm[e_] :> UnlimitedSpanBox @ MakeBoxes @ e
}]

PrivateTypesettingBoxFunction[ForceKatexCharBox]

NoSpanBox[e_] := StyleBox[e, SpanMaxSize -> 1];

UnlimitedSpanBox[e_] := StyleBox[e, SpanAdjustments -> {{0, 0}, {0, 0}}, SpanMaxSize->Infinity, SpanMinSize -> 1.5];

ForceKatexCharBox[e_] := StyleBox[e, FontFamily -> "KaTeX_Main", PrivateFontOptions -> {"OperatorSubstitution" -> False}];

(**************************************************************************************************)

PublicTypesettingFormBox[RaiseForm, LowerForm]

DefineStandardTraditionalForm[RaiseForm[e_, n_ ? NumericQ] :> RaiseBox[MakeBoxes @ e, n]];
DefineStandardTraditionalForm[LowerForm[e_, n_ ? NumericQ] :> LowerBox[MakeBoxes @ e, n]];

RaiseBox[e_, n_] := AdjustmentBox[e, BoxBaselineShift -> -n];
LowerBox[e_, n_] := AdjustmentBox[e, BoxBaselineShift -> n];

(**************************************************************************************************)

PublicTypesettingForm[AdjustmentForm]

DefineStandardTraditionalForm[
  AdjustmentForm[e_, b_] :> AdjustmentBox[MakeBoxes @ e, BoxMargins -> b]
];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[KOrd, KBin]

KOrd[k_] := "mathord"[k];
KBin[k_] := "mathbin"[k];

(**************************************************************************************************)

PrivateVariable[$PipeBox]

$PipeBox = KBox["|", "\\middle|"];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[FunctionBox]

FunctionBox[e_] := KBox[e, "op"[e]];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[InverseBox]

InverseBox[b_] := SuperscriptBox[b, RBox["-", "1"]];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[Overbracketbox, UnderbracketBox, UnderbraceBox, OverbraceBox, UnderparenthesisBox, OverparenthesisBox]

UnderbraceBox[a_, b_] := KBox[UnderscriptBox[UnderscriptBox[a, "\[UnderBrace]"], b], SubscriptBox["underbrace"[a], b]];
OverbraceBox[a_, b_] := KBox[OverscriptBox[OverscriptBox[a, "\[OverBrace]"], b], SuperscriptBox["overbrace"[a], b]];

UnderbracketBox[a_, b_] := KBox[UnderscriptBox[UnderscriptBox[a, "\[UnderBracket]"], b], SubscriptBox["underbracket"[a], b]];
OverbracketBox[a_, b_] := KBox[OverscriptBox[OverscriptBox[a, "\[OverBracket]"], b], SuperscriptBox["overbracket"[a], b]];

UnderparenthesisBox[a_, b_] := KBox[UnderscriptBox[UnderscriptBox[a, "\[UnderParenthesis]"], b], SubscriptBox["undergroup"[a], b]];
OverparenthesisBox[a_, b_] := KBox[OverscriptBox[OverscriptBox[a, "\[OverParenthesis]"], b], SuperscriptBox["overgroup"[a], b]];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[LeftBox, RightBox, DelimiterBox, LeftRightBox]

DelimiterBox[e_] := StyleBox[e, "DelimiterFont"];

LeftBox[d_Str] := KBox[DelimiterBox @ d, "\\left" <> d];
LeftBox["{"] := KBox[DelimiterBox @ "{", "\\left\\{"];
LeftBox["|"] := KBox["\[LeftBracketingBar]", "\\left\\lvert"];
LeftBox["||"] := KBox["\[LeftDoubleBracketingBar]", "\\left\\lVert"];

RightBox[d_Str] := KBox[DelimiterBox @ d, "\\right" <> d];
RightBox["}"] := KBox[DelimiterBox @ "}", "\\right\\}"];
RightBox["|"] := KBox["\[RightBracketingBar]", "\\right\\rvert"];
RightBox["||"] := KBox["\[RightDoubleBracketingBar]", "\\right\\rVert"];

LeftRightBox[l_, inner___, r_] := RBox[LeftBox @ l, inner, RightBox @ r];

_LeftBox := BadArguments[];
_RightBox := BadArguments[];
_LeftRightBox := BadArguments[];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[BracesBox, AngleBracketBox, ParenthesesBox, SquareBracketBox, DoubleSquareBracketBox, BarBox, DoubleBarBox]

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

toSlotFn[body_] := Fn[body] /. $slotRules;

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
attachBoxFunctionDefs[sym_Symbol, fn_] := (
  QuiverGeometryLoader`DeclarePreservedFunction[sym]; (* <- this ensures that after caching and reloading, box function definitions aren't wiped *)
  SetDelayed[sym[args___], fn[args]]
);

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
  str_Str ? LowerCaseQ := StringTake[str, 2];
  str_Str := StringJoin @ StringCases[str, $kNameRules];
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
makeTemplateName[symbol_, name_Str] := name;

_makeTemplateName := BadArguments[];

(**************************************************************************************************)

PublicOption[Boxification]

SetUsage @ "
Boxification is an option to various DefineXXX functions that specifies the function to boxify arguments.
"

toSequenceBoxifyFn[MakeQGBoxes] = MakeQGBoxSequence;
toSequenceBoxifyFn[fn_] := Fn[Null, Sequence @@ MapUnevaluated[fn, {##}]];

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

PublicFunction[ClearTemplateBoxDefinitions]

PrivateVariable[$notebookDisplayFunction, $katexDisplayFunction, $katexMacros, $symbolToTemplateName, $symbolToKatexMacroName, $notebookDisplayFunctionBases]

SetInitialValue[$notebookDisplayFunction, $katexDisplayFunction, $katexMacros, $symbolToTemplateName, $symbolToKatexMacroName, $notebookDisplayFunctionBases, <||>];

ClearTemplateBoxDefinitions[] := (
  $notebookDisplayFunction =
  $katexDisplayFunction =
  $katexMacros =
  $symbolToTemplateName =
  $symbolToKatexMacroName =
  $notebookDisplayFunctionBases =
  <||>;
);

(**************************************************************************************************)

PrivateSpecialFunction[DefineNotebookDisplayFunction, DefineKatexDisplayFunction, DefineKatexMacro]

SetInitialValue[$qgTemplateBoxNameQ, UAssoc[]];

DefineNotebookDisplayFunction[templateName_Str, fn_Fn] := (
  $qgTemplateBoxNameQ[templateName] = True;
  $notebookDisplayFunction[templateName] = SpecializeToNotebookBoxes[fn];
);

DefineKatexDisplayFunction[templateName_Str, fn_Fn] := (
  $katexDisplayFunction[templateName] = SpecializeToKatexBoxes[fn];
);

DefineKatexDisplayFunction[templateName_, fn_, None] :=
  DefineKatexDisplayFunction[templateName, fn];

DefineKatexDisplayFunction[templateName_, fn_, macroName_Str] := (
  $katexDisplayFunction[templateName] = macroName;
  DefineKatexMacro[macroName, fn];
);

DefineKatexMacro[name_Str, fn_Fn] := (
  $katexMacros[name] = SpecializeToKatexBoxes[fn];
);

(**************************************************************************************************)

PrivateSpecialFunction[AssociateSymbolToTemplateName, AssociateSymbolToKatexMacro]

AssociateSymbolToTemplateName[sym_Symbol, name_Str] := KeyAppendTo[$symbolToTemplateName, sym, name];
AssociateSymbolToKatexMacro[sym_Symbol, name_Str]   := KeyAppendTo[$symbolToKatexMacroName, sym, name];
AssociateSymbolToKatexMacro[sym_Symbol, None]       := Null;

(**************************************************************************************************)

_DefineNotebookDisplayFunction = BadArguments[];
_DefineKatexDisplayFunction = BadArguments[];
_DefineKatexMacro = BadArguments[];
_AssociateSymbolToTemplateName = BadArguments[];
_AssociateSymbolToKatexMacro = BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[DefineTemplateBox]

(* kmacro: If Automatic, base name on template box name, if None, don't set up a macro *)
DefineTemplateBox[symbol_Symbol, templateName_Str, boxes_, katexMacroName_] := Scope[
  SetAutomatic[katexMacroName, templateNameToMacroName @ templateName];
  AssociateSymbolToTemplateName[symbol, templateName];
  AssociateSymbolToKatexMacro[symbol, katexMacroName];
  fn = toSlotFn @ boxes;
  registerFormScriptingArgPositions[symbol, findScriptingArgPositions[fn /. AdjustmentBox[b_, _] :> b]];
  DefineNotebookDisplayFunction[templateName, fn];
  DefineKatexDisplayFunction[templateName, fn, katexMacroName];
  fn
];

_DefineTemplateBox = BadArguments[];

(**************************************************************************************************)

findScriptingArgPositions[fn_] :=
  Catenate @ Map[patt |-> DeepCases[fn, patt], $scriptPositionPatterns];

$scriptPositionPatterns = {
  (SubscriptBox|SuperscriptBox)[_, Slot[n_]] :> n,
  SubsuperscriptBox[_, _, Slot[n_]] :> n,
  SubsuperscriptBox[_, Slot[n_], _] :> n,
  StyleBox[Slot[n_], Smaller] :> n
};

(**************************************************************************************************)

DefineKatexMacro["op", "operatorname"[#1]&];

(**************************************************************************************************)

PublicTypesettingForm[KatexSwitch]

SetUsage @ "
KatexSwitch[wlForm$, kForm$] displays as wlForm$ but converts to Katex as kForm$.
"

DefineStandardTraditionalForm[KatexSwitch[wl_, k_] :> KBox[MakeQGBoxes @ wl, MakeQGBoxes @ k]];
DefineTemplateBox[KatexSwitch, "katexSwitch", KBox[$1, $2], None]

(**************************************************************************************************)

PublicFunction[QGTemplateNameQ]

QGTemplateNameQ[name_] := Or[
  $qgTemplateBoxNameQ @ name,
  AssocQ[$localKatexDisplayFunction] && KeyExistsQ[$localKatexDisplayFunction, name]
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

PublicTypesettingFormBox[RiffledForm]

DefineStandardTraditionalForm[
  RiffledForm[head_][args___] :> RiffledBox[MakeQGBoxes @ head][MakeQGBoxSequence @ args]
]

DefineNotebookDisplayFunction["RiffledForm", Fn[RowBox[{TemplateSlotSequence[2, #1]}]]];
DefineKatexDisplayFunction["RiffledForm", Fn[Riffle[{##2}, #1]]];
AssociateSymbolToTemplateName[RiffledForm, "RiffledForm"]

RiffledBox[rif_][args___] := TemplateBox[{rif, args}, "RiffledForm"];

(**************************************************************************************************)

PublicTypesettingFormBox[CommaRowForm, TightCommaRowForm]

DefineStandardTraditionalForm[{
  CommaRowForm[a___] :> CommaRowBox[MakeQGBoxSequence[a]],
  TightCommaRowForm[a___] :> TightCommaRowBox[MakeQGBoxSequence[a]]
}];

CommaRowBox[] := ""
CommaRowBox[a_] := a;
CommaRowBox[a__] := TBox[a, "CommaRowForm"];

TightCommaRowBox[] := ""
TightCommaRowBox[a_] := a;
TightCommaRowBox[a__] := TBox[a, "TightCommaRowForm"];

DefineNotebookDisplayFunction["CommaRowForm", Fn[RowBox[{TemplateSlotSequence[1, ", "]}]]];
DefineKatexDisplayFunction["CommaRowForm", Riffle[{##}, ","]&];
AssociateSymbolToTemplateName[CommaRowForm, "CommaRowForm"];

DefineNotebookDisplayFunction["TightCommaRowForm", Fn[RowBox[{TemplateSlotSequence[1, ","]}]]];
DefineKatexDisplayFunction["TightCommaRowForm", Riffle[{##}, ","]&];
AssociateSymbolToTemplateName[TightCommaRowForm, "TightCommaRowForm"];

(**************************************************************************************************)

PublicTypesettingFormBox[TightRowForm]

DefineStandardTraditionalForm[
  TightRowForm[a___] :> TightRowBox[MakeQGBoxSequence[a]]
];

TightRowBox[] := RBox[];
TightRowBox[a_] := a;
TightRowBox[a__] := TBox[a, "TightRowForm"];

DefineNotebookDisplayFunction["TightRowForm", Fn[RowBox[{TemplateSlotSequence[1]}]]];
DefineKatexDisplayFunction["TightRowForm", {##}&];
AssociateSymbolToTemplateName[TightRowForm, "TightRowForm"];

(**************************************************************************************************)

SetRelatedSymbolGroup[DefineTaggedForm, DefineUnaryForm, DefineUnaryModifierForm]
SetRelatedSymbolGroup[DefineUnaryForm, DefineBinaryForm, DefineTernaryForm, DefineNAryForm]
SetRelatedSymbolGroup[DefineBinaryForm, DefineIndexedBinaryForm]
SetRelatedSymbolGroup[DefineInfixForm]
SetRelatedSymbolGroup[DefineUnaryForm, DefineStyleForm]

(**************************************************************************************************)

PublicSpecialFunction[DefineTaggedForm]

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
  $unaryFormHeadQ[formSym] = True;
  $taggedFormHeadQ[formSym] = True;
  DefineStandardTraditionalForm[formSym[e_] :> TBox[boxify @ e, name]];
  DefineTemplateBox[formSym, name, $1, OptionValue[KatexMacroName]];
  If[AssocQ[aliases], DefineStandardTraditionalForm[
      formSym[alias_Str /; KeyExistsQ[aliases, alias]] :> ToBoxes @ formSym[Lookup[aliases, alias]]
  ]];
];

_DefineTaggedForm := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[DefineUnaryForm]

SetUsage @ "
DefineUnaryForm[symbol$, boxes$] defines symbol$[arg$1] to boxify to %TemplateBox[{arg$1}, 'symbol$'], \
which displays as boxes$ where $1 is substituted.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
"

Options[DefineUnaryForm] = $defineOpts;

setupFormDefinitionCaching[DefineUnaryForm];

DefineUnaryForm[formSym_Symbol, boxes_, OptionsPattern[]] := With[
  {name = makeTemplateName[formSym, OptionValue[TemplateName]]},
  {boxify = OptionValue[Boxification]},
  {fn = TBox[#1, name]&},
  $unaryFormHeadQ[formSym] = True;
  attachBoxFunctionDefs[OptionValue[BoxFunction], fn];
  attachHeadBoxes[OptionValue[HeadBoxes], formSym];
  DefineStandardTraditionalForm[formSym[e_] :> fn[boxify @ e]];
  DefineTemplateBox[formSym, name, boxes, OptionValue[KatexMacroName]]
];

_DefineUnaryForm := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[DefineUnaryModifierForm]

SetUsage @ "
DefineUnaryModifierForm[$$] is like DefineUnaryForm[$$] but registers the form as a modifier.
* modifiers have the property that BurrowModifiers and UnburrowModifiers will act on them.
"

DefineUnaryModifierForm[formSym_Symbol, args___] := (
  $modifierFormHeadQ[formSym] = True;
  DefineUnaryForm[formSym, args];
)

_DefineUnaryModifierForm := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[DefineBinaryForm]

SetUsage @ "
DefineBinaryForm[symbol$, boxes$] defines symbol$[arg$1, arg$2] to boxify to %TemplateBox[{arg$1, arg$2}, 'symbol$'], \
which displays as boxes$ where $1, $2 are substituted.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1, arg$2}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
"

Options[DefineBinaryForm] = $defineOpts;

setupFormDefinitionCaching[DefineBinaryForm];

DefineBinaryForm[formSym_Symbol, boxes_, OptionsPattern[]] := With[
  {name = makeTemplateName[formSym, OptionValue[TemplateName]]},
  {boxify = OptionValue[Boxification]},
  {fn = TBox[#1, #2, name]&},
  $binaryFormHeadQ[formSym] = True;
  attachBoxFunctionDefs[OptionValue[BoxFunction], fn];
  attachHeadBoxes[OptionValue[HeadBoxes], formSym];
  DefineStandardTraditionalForm[formSym[a_, b_] :> fn[boxify @ a, boxify @ b]];
  DefineTemplateBox[formSym, name, boxes, OptionValue[KatexMacroName]]
];

_DefineBinaryForm := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[DefineTernaryForm]

Options[DefineTernaryForm] = $defineOpts;

SetUsage @ "
DefineTernaryForm[symbol$, boxes$] defines symbol$[arg$1, arg$2, arg$3] to boxify to %TemplateBox[{arg$1, arg$2, arg$3}, 'symbol$'], \
which displays as boxes$ where $1, $2, $3 are substituted.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1, arg$2}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
"

setupFormDefinitionCaching[DefineTernaryForm];

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

PublicSpecialFunction[DefineNAryForm]

Options[DefineNAryForm] = $defineOpts;

SetUsage @ "
DefineNAryForm[symbol$, boxes$] defines symbol$[arg$1, $$, arg$n] to boxify to %TemplateBox[{arg$1, $$, arg$n}, 'symbol$'], \
which displays as boxes$ where $1, $2, $3 are substituted.
* No %KatexMacroName option is available, because katex macros do not support variable arity.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
"

setupFormDefinitionCaching[DefineNAryForm];

DefineNAryForm[formSym_Symbol, boxes_, OptionsPattern[]] := With[
  {name = makeTemplateName[formSym, OptionValue[TemplateName]]},
  {boxify = toSequenceBoxifyFn[OptionValue[Boxification]]},
  {fn = TBox[##, name]&},
  $naryFormHeadQ[formSym] = True;
  attachBoxFunctionDefs[OptionValue[BoxFunction], fn];
  attachHeadBoxes[OptionValue[HeadBoxes], formSym];
  DefineStandardTraditionalForm[formSym[seq___] :> applyReverseChain @ fn @ boxify @ seq];
  DefineTemplateBox[formSym, name, boxes, OptionValue[KatexMacroName]]
];

(* this is used so that GradientSymbol["\[RightArrow]", ...] appears reversed in composition order! *)
applyReverseChain[TemplateBox[targs:{TagBox[_, "ReverseChain"]..}, tname_]] := TemplateBox[Rev @ Part[targs, All, 1], tname];
applyReverseChain[e_] := e;

_DefineNAryForm := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[DefineInfixForm]

Options[DefineInfixForm] = JoinOptions[$defineOpts, HeadBoxes -> Automatic];

SetUsage @ "
DefineInfixForm[symbol$, infixBox$] defines symbol$[arg$1, arg$2, $$] to boxify to %TemplateBox[{arg$1, infixBox$, arg$2, infixBox$, $$}, 'symbol$'], \
which displays as RowBox[{arg$1, infixBox$, arg$2, infixBox$, $$}].
* The bare head symbol$ just displays as infixBox$.
* symbol$[] display as nothing.
* symbol$[arg$1] displays as arg$1.
* symbol$[symbol$[arg$1, arg$2], arg$3], etc. introduces brackets as necessary.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1, arg$2, $$}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
"

(* TODO: replace Padding mechanism by unifying mathbin / mathop on the K side with thin space padding on the M side *)

setupFormDefinitionCaching[DefineInfixForm];

DefineInfixForm[formSym_Symbol, infixBox_, opts:OptionsPattern[]] := P1 @ {
  $infixFormHeadQ[formSym] = True;
  DefineNAryForm[formSym, RiffledBox[infixBox][$$1], HeadBoxes -> infixBox, FilterOptions @ opts],
  DefineStandardTraditionalForm[
    f:formSym[___, _formSym, ___] :> ToBoxes @ VectorReplace[f, z_formSym :> ParenthesesForm @ z]
  ]
};

_DefineInfixForm := BadArguments[];

(**************************************************************************************************)

PublicTypesettingFormBox[AppliedForm, TightAppliedForm]

DefineStandardTraditionalForm[{
  AppliedForm[fn_, args___] :> AppliedBox[MakeQGBoxes @ fn, MakeQGBoxSequence @ args],
  TightAppliedForm[fn_, args___] :> TightAppliedBox[MakeQGBoxes @ fn, MakeQGBoxSequence @ args]
}];

AppliedBox[fn_, args__] := TBox[fn, CommaRowBox[args], "appliedForm"];
AppliedBox[fn_] := TBox[fn, "emptyAppliedForm"];

TightAppliedBox[fn_, args__] := TBox[fn, TightCommaRowBox[args], "appliedForm"];
TightAppliedBox[fn_] := TBox[fn, "emptyAppliedForm"];

DefineTemplateBox[AppliedForm, "appliedForm", RBox[$1, KBox["(", "\\lparen "], $2, KBox[")", "\\rparen "]], None];
DefineTemplateBox[AppliedForm, "emptyAppliedForm", RBox[$1, KBox["(", "\\lparen "], KBox[")", "\\rparen "]], None];


(**************************************************************************************************)

PublicSpecialFunction[DefineInfixBinaryForm]

Options[DefineInfixBinaryForm] = Options @ DefineInfixForm;

SetUsage @ "
DefineInfixBinaryForm[symbol$, infixBox$] defines symbol$[arg$1, arg$2] to boxify to %TemplateBox[{arg$1, arg$2}, 'symbol$'], \
which displays as RowBox[{arg$1, infixBox$, arg$2}].
* The bare head symbol$ just displays as infixBox$.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1, arg$2, $$}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
"

setupFormDefinitionCaching[DefineInfixBinaryForm];

DefineInfixBinaryForm[formSym_Symbol, infixBox_, OptionsPattern[]] :=
  DefineBinaryForm[formSym, RBox[$1, infixBox, $2], HeadBoxes -> infixBox, FilterOptions @ opts];

_DefineInfixBinaryForm := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[DefineIndexedInfixBinaryForm]

SetUsage @ "
DefineIndexedInfixBinaryForm[symbol$, boxes$] defines symbol$[arg$1, arg$2] to boxify to %TemplateBox[{arg$1, arg$2}, 'symbol$'], \
which displays as RowBox[{arg$1, infixBox$, arg$2}], and defines symbol$[arg$1, arg$2, arg$3] to boxify to \
%TemplateBox[{arg$1, arg$2, arg$3, 'symbol$indexed'], which displays as RowBox[{arg$1, SubscriptBox[infixBox$, arg$3], arg$2}].
"

Options[DefineIndexedInfixBinaryForm] = Options @ DefineInfixForm;

setupFormDefinitionCaching[DefineIndexedInfixBinaryForm];

DefineIndexedInfixBinaryForm[formSym_Symbol, infixBox_, opts:OptionsPattern[]] := (
  DefineUnaryForm[formSym, OpBox @ SubscriptBox[infixBox, $1], TemplateName -> StringJoin[SymbolName @ formSym, "Head"]];
  DefineTernaryForm[formSym, RBox[$1, OpBox @ SubscriptBox[infixBox, $3], $2], FilterOptions @ opts];
);

_DefineIndexedInfixBinaryForm := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[DefineCommaForm]

PublicOption[HeadBoxes]

Options[DefineCommaForm] = $defineOpts;

SetUsage @ "
DefineCommaForm[symbol$, boxes$] defines symbol$[arg$1, arg$2, $$] to boxify to %TemplateBox[{arg$1, arg$2, $$}, 'symbol$'], \
which displays as boxes$ with $1 substituted with CommaRowBox[arg$1, arg$2, $$].
* The option %HeadBoxes can be used to define how symbol% itself should boxify.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1, arg$2}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
";

setupFormDefinitionCaching[DefineCommaForm];

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

PublicSpecialFunction[DefineRestCommaForm]

Options[DefineRestCommaForm] = $defineOpts;

SetUsage @ "
DefineRestCommaForm[symbol$, boxes$] defines symbol$[arg$1, arg$2, $$] to boxify to %TemplateBox[{arg$1, arg$2}, 'symbol$'], \
which displays as boxes$ with $2 substituted with CommaRowBox[arg$1, arg$2, $$].
* The option %HeadBoxes can be used to define how symbol% itself should boxify.
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym{arg$1, arg$2}.
* If the %BoxFunction option is not None, the symbol provided will be set up so it can be called to construct the underlying boxes directly.
";

setupFormDefinitionCaching[DefineRestCommaForm];

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

PublicSpecialFunction[DefineRuleAsMapsTo]

setupFormDefinitionCaching[DefineRuleAsMapsTo];

DefineRuleAsMapsTo[head_] := DefineStandardTraditionalForm[
  head[l___, Rule[lhs_, rhs_], r___] :> MakeBoxes[head[l, MapsToForm[lhs, rhs], r]]
]

(**************************************************************************************************)

PrivateVariable[$literalSymbolFormTable]

SetInitialValue[$literalSymbolFormTable, UAssoc[]];

(**************************************************************************************************)

PublicSpecialFunction[DefineSymbolForm]

Options[DefineSymbolForm] = $defineOpts

SetUsage @ "
DefineSymbolForm[symbol$ -> boxes$] defines symbol$ to boxify to %TemplateBox[{}, 'symbol$'].
* If the %KatexMacroName option is not None, a shortened katex macro is set up, which looks like \\sym.
"

SetListable[DefineSymbolForm];

setupFormDefinitionCaching[DefineSymbolForm];

DefineSymbolForm[sym_Symbol -> boxes_, OptionsPattern[]] := With[
  {name = makeTemplateName[sym, OptionValue[TemplateName]]},
  $symbolFormHeadQ[sym] = True;
  $literalSymbolFormTable[sym] = boxes;
  DefineStandardTraditionalForm[sym :> SBox[name]];
  DefineTemplateBox[sym, name, boxes, OptionValue[KatexMacroName]]
]

_DefineSymbolForm := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[DefineIconSymbolForm]

SetUsage @ "
DefineIconSymbolForm[symbol$ -> 'str$'] defines symbol$ to format as TextIcon['str$'].
"

SetListable[DefineIconSymbolForm]

setupFormDefinitionCaching[DefineIconSymbolForm];

DefineIconSymbolForm[sym_Symbol -> str_Str] := (
  $symbolFormHeadQ[sym] = True;
  $literalSymbolFormTable[sym] = str;
  DefineStandardTraditionalForm[sym :> ToBoxes @ TextIcon[str]]
);

_DefineIconSymbolForm := BadArguments[];

(**************************************************************************************************)

PublicSpecialFunction[DefineNamedFunctionSymbolForm]

setupFormDefinitionCaching[DefineNamedFunctionSymbolForm];

DefineNamedFunctionSymbolForm[e_] := iDefineNamedFunctionSymbolForm[e];

iDefineNamedFunctionSymbolForm = Case[
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

KConstruct[str_Str] := StringJoin["\\", str, " "];
KConstruct[str_Str, args__] := str[args];

(**************************************************************************************************)

PublicSpecialFunction[DefineStyleForm]

Options[DefineStyleForm] = $defineOpts;

SetUsage @ "
DefineStyleForm[symbol$, style$] defines symbol$[$$] to boxify to %StyleBox[$$, style$].
* DefineStyleForm uses %DefineUnaryForm internally.
* the resulting form has the property that BurrowModifiers and UnburrowModifiers will act on it.
"

setupFormDefinitionCaching[DefineStyleForm];

DefineStyleForm[formSym_Symbol, style_, opts:OptionsPattern[]] := (
  $styleFormData[formSym] = style;
  $styleFormHeadQ[formSym] = True;
  $modifierFormHeadQ[formSym] = True;
  DefineUnaryForm[formSym, StyleBox[$1, style]];
);

(**************************************************************************************************)

PrivateFunction[StyleFormData]

SetInitialValue[$styleFormData, UAssoc[]];

StyleFormData[s_Symbol] := Lookup[$styleFormData, s, None];
StyleFormData[] := $styleFormData;

(**************************************************************************************************)

PublicTypesettingForm[IndexedForm]

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

(**************************************************************************************************)

PrivateSpecialFunction[DefineLegacyIndexedForm]

setupFormDefinitionCaching[DefineLegacyIndexedForm];

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

PublicSpecialFunction[DefineLocalTemplates]

DefineLocalTemplates::taggingrules = "Could not update tagging rules.";

SetHoldAll[DefineLocalTemplates];
DefineLocalTemplates[e___] := Scope @ InheritedBlock[{$katexMacros, $katexDisplayFunction},
  $notebookDisplayFunction = <||>;
  $katexMacros0 = $katexMacros;
  $katexDisplayFunction0 = $katexDisplayFunction;
  (e);
  privateStylesheet = GeneratePrivateQuiverGeometryStylesheet[];
  SetOptions[EvaluationNotebook[], StyleDefinitions -> privateStylesheet];
  currentRules = Lookup[Options[EvaluationNotebook[], TaggingRules], TaggingRules, <||>];
  If[!AssocQ[currentRules], ReturnFailed["taggingrules"]];
  $katexDisplayFunction1 = KeyDrop[$katexDisplayFunction, Keys @ $katexDisplayFunction0];
  $katexMacros1 = KeyDrop[$katexMacros, Keys @ $katexMacros0];
  taggingRules = Join[currentRules, <|"KatexDisplayFunctions" -> $katexDisplayFunction1, "KatexMacros" -> $katexMacros1|>];
  SetOptions[EvaluationNotebook[], TaggingRules -> taggingRules];
];

(**************************************************************************************************)

PrivateTypesettingBoxFunction[DBox]

(* this is needed because DisplayFunction -> (RowBox[#]&) where # is a list does not work! *)
DBox[e_] := DynamicBox[e, DestroyAfterEvaluation -> True, TrackedSymbols -> {}];

(**************************************************************************************************)

PublicTypesettingFormBox[RedForm, GreenForm, BlueForm, OrangeForm, PinkForm, TealForm, GrayForm, PurpleForm]
PublicTypesettingFormBox[LightRedForm, LightGreenForm, LightBlueForm, LightOrangeForm, LightPinkForm, LightTealForm, LightGrayForm, LightPurpleForm]
PublicTypesettingFormBox[DarkRedForm, DarkGreenForm, DarkBlueForm, DarkOrangeForm, DarkPinkForm, DarkTealForm, DarkGrayForm, DarkPurpleForm, MultisetColorForm]

PublicTypesettingFormBox[BoldForm, ItalicForm, UnderlinedForm, StruckthroughForm, LargerForm, SmallerForm, PlainTextForm, MathTextForm, RomanForm, FrakturForm, CaligraphicForm, SansSerifForm, TypewriterForm]

SystemSymbol[ScriptForm]
PrivateTypesettingBoxFunction[ScriptBox]

Unprotect[ScriptForm]; (* it's an undocumented system symbol! *)

(* BoxFunction will make a templatebox, which is an unnecessary level of indirection *)
makeInlineStyleForm[formSym_, boxSym_, style_] := (
  DefineStyleForm[formSym, style];
  boxSym[z_] := StyleBox[z, style]
);

makeInlineStyleForm @@@ ExpressionTable[
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
  UnderlinedForm      UnderlinedBox      Underlined
  ItalicForm          ItalicBox          Italic
  StruckthroughForm   StruckthroughBox   Struckthrough
  LargerForm          LargerBox          Larger
  SmallerForm         SmallerBox         Smaller
  PlainTextForm       PlainTextBox       "MathText"
  MathTextForm        MathTextBox        "MathTextFont"
  RomanForm           RomanBox           "RomanMathFont"
  ScriptForm          ScriptBox          "ScriptMathFont"
  FrakturForm         FrakturBox         "FrakturMathFont"
  CaligraphicForm     CaligraphicBox     "CaligraphicMathFont"
  SansSerifForm       SansSerifBox       "SansSerifMathFont"
  TypewriterForm      TypewriterBox      "TypewriterMathFont"
];

registerFormScriptingArgPositions[SmallerForm, 1];

(**************************************************************************************************)

PublicTypesettingForm[Color1Form, Color2Form, Color3Form, Color4Form, Color5Form, Color6Form, Color7Form, Color8Form, ColorNForm]

PublicTypesettingForm[Background1Form, Background2Form, Background3Form, Background4Form, Background5Form, Background6Form, Background7Form, Background8Form, BackgroundNForm]

ColorNForm[n_Int] := Part[{Color1Form, Color2Form, Color3Form, Color4Form, Color5Form, Color6Form, Color7Form, Color8Form}, n];

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

BackgroundNForm[n_Int] := Part[{Background1Form, Background2Form, Background3Form, Background4Form, Background5Form, Background6Form, Background7Form, Background8Form}, n];

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

PublicTypesettingForm[StyledForm]

SetUsage @ "
StyledForm[expr$, dirs$$] effectively applies Style[$$, dirs$$] to the innermost part of expr$ that is not a unary form.
* This is useful because wrapping a form like %FunctorSymbol['X'] with Style% will interference with its normal typesetting.
* This is subtly different from doing BurrowModifiers @ Style[...] because the burrowed Style will not use MakeQGBoxes, and hence not show single-letter symbols as italics.
";

(* TODO: shouldn't all style heads work this way? *)
StyledForm[(head_Symbol ? $unaryFormHeadQ)[arg_], dirs__] := head[StyledForm[arg, dirs]];
StyledForm[Style[arg_, s___], dirs__] := Style[StyledForm[arg, dirs], s];

$styleFormHeadQ[StyledForm] = True;

(* does this work with Katex? *)
DefineStandardTraditionalForm[StyledForm[e_, s___] :> StyleBox[MakeQGBoxes @ e, s]];

(**************************************************************************************************)

PrivateFunction[BurrowModifiers, UnburrowModifiers]

SetUsage @ "BurrowModifiers[expr$] pushes all modifier-like heads (%Style, %PrimedForm%, %RedForm, $$) through tagged forms (%FunctionSymbol, %CategoryObjectSymbol, $$), making them innermost."
SetUsage @ "UnburrowModifiers[expr$] pulls all modifier-like heads (%Style, %PrimedForm%, %RedForm, $$) through tagged forms (%FunctionSymbol, %CategoryObjectSymbol, $$), making them outermost."

$modifierOrStyleFormHeadQ := $modifierOrStyleFormHeadQ = Join[$modifierFormHeadQ, $styleFormHeadQ];

BurrowModifiers[e_]   := ReplaceRepeated[e, (mod_Symbol ? $modifierOrStyleFormHeadQ)[(tag_Symbol ? $taggedFormHeadQ)[sub_], s___] :> tag[mod[sub, s]]];
UnburrowModifiers[e_] := ReplaceRepeated[e, (tag_Symbol ? $taggedFormHeadQ)[(mod_Symbol ? $modifierOrStyleFormHeadQ)[sub_, s___]] :> mod[tag[sub], s]];

(**************************************************************************************************)

PrivateFunction[ExpandTemplateBox]

ExpandTemplateBox = Case[
  TemplateBox[{a_, b_}, "katexSwitch"]      := a;
  tb:TemplateBox[_, _Str ? QGTemplateNameQ] := ExpandQGTemplateBox @ tb;
  tb:TemplateBox[_List, _Str]               := ExpandNotebookTemplateBox @ tb;
];

(**************************************************************************************************)

PrivateFunction[ExpandQGTemplateBox, QGTemplateBoxQ]

ExpandQGTemplateBox = Case[
  TemplateBox[args_List, name_Str] := Apply[replaceTemplateSlots @ $notebookDisplayFunction @ name, args];
]

QGTemplateBoxQ[TemplateBox[_List, _Str ? QGTemplateNameQ]] := True;
QGTemplateBoxQ[_] := False;

replaceTemplateSlots[e_] := ReplaceRepeated[e, {
  TemplateSlot[n_Int] :> Slot[n],
  TemplateSlotSequence[n_Int] :> SlotSequence[n],
  TemplateSlotSequence[n_Int, riff_] :> SequenceRiffle[SlotSequence[n], riff]
}];

(**************************************************************************************************)

PublicFunction[ExpandNotebookTemplateBox]

ExpandNotebookTemplateBox = Case[
  tb:TemplateBox[_List, _Str] := BoxForm`TemplateBoxToDisplayBoxes[tb];
];

(**************************************************************************************************)

PublicFunction[EvaluateTemplateBox, EvaluateTemplateBoxFull]

EvaluateTemplateBox[expr_] := ReplaceAll[expr, tb:TemplateBox[_List, _Str] :> RuleCondition @ evalTB[tb]];
EvaluateTemplateBoxFull[expr_] := ReplaceRepeated[expr, tb:TemplateBox[_List, _Str] :> RuleCondition @ evalTB[tb]];

evalTB := Case[
  TemplateBox[{a_, b_}, "katexSwitch"] := a;
  tb:TemplateBox[_, name_Str] /; KeyExistsQ[$notebookDisplayFunction, name] := ExpandQGTemplateBox @ tb;
  tb:TemplateBox[_List, _Str]                                               := ExpandNotebookTemplateBox @ tb;
];

PublicFunction[EvaluateTemplateBoxAsKatex, EvaluateTemplateBoxAsKatexFull]

EvaluateTemplateBoxAsKatex[expr_] := ReplaceAll[expr, tb:TemplateBox[_List, _Str] :> RuleCondition @ evalTBK[tb]];
EvaluateTemplateBoxAsKatexFull[expr_] := ReplaceRepeated[expr, tb:TemplateBox[_List, _Str] :> RuleCondition @ evalTBK[tb]];

evalTBK := Case[
  TemplateBox[args_List, name_Str] /; KeyExistsQ[$katexDisplayFunction, name] := Apply[$katexDisplayFunction @ name, args];
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

(**************************************************************************************************)

(* these mirror SuperStar and SuperDagger in System` *)

PublicTypesettingBoxFunction[SuperStarBox, SuperDaggerBox]

SuperStarBox[e_] := SuperscriptBox[e, "*"];
SuperDaggerBox[e_] := SuperscriptBox[e, "\[Dagger]"];

(**************************************************************************************************)

PublicFunction[FormToPlainString]

FormToPlainString::noformstr = "Cannot convert form `` to a plain string. Resulting boxes were ``.";

FormToPlainString = Case[
  name_Str              := ToNonDecoratedRoman @ name;
  Sized[obj_, _]        := % @ obj;
  Customized[obj_, ___] := % @ obj;
  g_Graph               := "graph";
  form_                 := Scope[
    boxes = EvaluateTemplateBoxFull @ ToBoxes @ ReplaceRepeated[form, $plainStrNormalizationRules];
    str = boxToString @ boxes;
    If[!StringQ[str], ReturnFailed["noformstr", MsgExpr @ form, InputForm @ boxes]];
    StringReplace[ToSpelledGreek @ ToNonDecoratedRoman @ str, $plainStringReplacements]
  ];
]

$plainStrNormalizationRules = {
  "\[RightArrow]"                       -> "->",
  (head_Symbol ? $styleFormHeadQ)[arg_] :> arg;
  GradientSymbol[sym_, ___]             :> sym,
  ColorGradientForm[sym_, ___]          :> sym,
  CompactHomForm[a_,b_]                 :> HomForm[a, b]
};

boxToString = Case[
  StyleBox[e_, ___]              := % @ e;
  AdjustmentBox[e_, ___]         := % @ e;
  FrameBox[e_, ___]              := StringJoin["[", % @ e, "]"];
  RowBox[e_]                     := StringJoin @ Map[%, e];
  SubsuperscriptBox[e_, a_, b_]  := StringJoin[% @ e, "^", % @ b, "_", % @ a];
  SuperscriptBox[e_, "\[Prime]"] := StringJoin[% @ e, "'"];
  SubscriptBox[e_, s_]           := StringJoin[% @ e, "_", % @ s];
  SuperscriptBox[e_, s_]         := StringJoin[% @ e, "^", % @ s];
  e_Str                          := If[StringMatchQ[e, "\"*\""], StringTake[e, {2, -2}], e];
  other_                         := $Failed;
];

$plainStringReplacements = {"\[FilledCircle]" -> "@", "\[FilledSmallCircle]" -> "@", "\[CircleTimes]" -> "*", "\[CirclePlus]" -> "+"}

(**************************************************************************************************)

PublicTypesettingForm[MathForm]

DefineTaggedForm[MathForm]

