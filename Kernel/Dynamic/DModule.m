PublicTypesettingBoxFunction[DValue]

PrivateVariable[$dValuePrefix]

$dValuePrefix = "Global";

$dInitialValues = UAssoc[];

ClearAll[DValue];


DValue[dv_DValue, _] := dv;

DValue[init_, name_Str:"$"] := With[
  {id = Len[$dInitialValues] + 1},
  {fullname = $dValuePrefix <> name <> IntStr[id]},
  {inContext = "$CellContext`" <> fullname <> "$$"},
  {symbol = (Clear[inContext]; Symbol[inContext])},
  $dInitialValues[symbol] = ToPacked @ init;
  ConstructNoEntry[DValue, symbol, fullname, id]
];

MakeBoxes[DValue[_Symbol, fullname_Str, id_Int], StandardForm] := dValueBoxes[fullname, id];

dValueBoxes[fullname_, id_] := StyleBox[
  fullname, FontWeight -> Bold,
  FontFamily -> "Source Code Pro",
  FontColor -> ToRainbowColor[Mod[id, 10, 1]]
];

(**************************************************************************************************)

PublicTypesettingBoxFunction[DGetInitialValue]

DGetInitialValue[DValue[s_Symbol, __]] := Lookup[$dInitialValues, s, Message[DSetInitialValue::unknownDValue, s]];
DGetInitialValue[other_] := other;

DSetInitialValue::unknownDValue = "Cannot find an initial value for ``.";

_DGetInitialValue := BadArguments[];

(**************************************************************************************************)

PublicTypesettingBoxFunction[DBoxes]

DefineSyntaxInformation[DBoxes, {"LocalVariables" -> {"VariableList", 1}}];

SetHoldAll[DBoxes, dBoxTemp, makeDBox];

makeDBox[e_] := Block[{$dDeps = {}}, Append[
  rewriteDThings @ dBoxTemp[e],
  Construct[RuleDelayed, TrackedSymbols, DeleteDuplicates @ $dDeps]
]];

rewriteDThings[e_] := e /. $rewriteDBoxes /. $subLiteralSymbols /. $rewriteDValues;

$rewriteDBoxes     = d_DBoxes                      :> RuleEval[rewriteDBoxes @ d];
$subLiteralSymbols = s_Symbol ? HasImmediateValueQ :> RuleEval[s];
$rewriteDValues    = DValue[s_Symbol, _, _]        :> RuleEval[AppendTo[$dDeps, s]; s];

rewriteDBoxes = Case[
  DBoxes[locals_List, body_] := makeDBox @ Block[locals, body];
  DBoxes[body_]              := makeDBox @ body;
  e_                         := (Message[DBoxes::malformed, e]; $Failed)
];

DBoxes::malformed = "`` is malformed.";

(**************************************************************************************************)

PublicTypesettingBoxFunction[DModule, DHold]

PublicOption[DynamicSelection, DynamicFilter]

PublicMutatingFunction[DSet]

SetUsage @ "
DModule[vars$, body$] runs body$ to assemble a %DynamicModuleBox[$$] expression.
* vars$ are local variables, with no special meaning.

## Dynamic variables
* DValue[init$] can be used to create dynamic variables that will be handled by the %DynamicModuleBox.
* SetInitialValue[DValue[$$], value] can be used to set existing variables.
* GetInitialValue[DValue[$$]] can be used to get existing variables, literals are returned as is.

## DynamicBox
* DBoxes[$$] defines a %DynamicBox[$$] whose %TrackedSymbols are set automatically.
* DBoxes[{s$1, s$2}, $$] sets up local variables inside the %DynamicBox.
* any symbols with literal values are automatically substituted into the interior of the %DynamicBox.
* therefore use ModuleScope to ensure that local variables live long enough to be rewritten.

## Composition
* If a DModule is constructed within another DModule, the inner DModule is simply folded into the outer one.

## Final subsitutions
* DHold[expr$] will be released at the end of the build phase.
* DSet[lhs$, rhs$] will be replaced with %Set at the end of the build phase.
";

DefineSyntaxInformation[DModule, {"LocalVariables" -> {"VariableList", 1}}];

SetHoldAll[DModule, DSet, DHold];

DModule::messagesDuringBuild = "Messages occurred during build phase.";
DModule::unresolvedDValue = "A DValue was not resolved.";

DModule[vars_List, body_] := Scope @ CatchMessage[
  $dDeps = {}; $dValuePrefix = "Module";
  DModule = Module;
  $dInitialValues = Association[];
  result = CheckMsg::messagesDuringBuild @ Module[vars, body];
  boxes = rewriteDThings @ result;
  setup = rewriteDThings @ KeyValueMap[DSet, $dInitialValues];
  dModuleBox = Construct[DynamicModuleBox, setup, boxes];
  If[!FreeQ[dModuleBox, DValue], Msg::unresolvedDValue[]];
  dModuleBox /. $finalDModuleRules
];

_DModule := BadArguments[];

$finalDModuleRules = {DSet -> Set, DHold[h_] :> h, dBoxTemp -> DynamicBox};

(**************************************************************************************************)

PublicTypesettingBoxFunction[DebugDynamic]

DebugDynamic[RawBoxes @ b_] := DebugDynamic @ b;

DebugDynamic[dmb:DynamicModuleBox[vars_List, body_, opts___]] := Scope[
  syms = Cases[Unevaluated @ vars, s_Symbol | HoldPattern[Set][s_Symbol, _] :> Hold[s]];
  syms = Sort @ syms;
  cols = {HoldSymbolName @@@ syms, DynamicBox @@@ syms};
  rows = Transpose @ {toDVarName /@ syms, toDVarDisplay /@ syms};
  PrependTo[rows, {StandaloneHold[body], "\[SpanFromLeft]"}];
  grid = GridBox[rows, ColumnAlignments -> Left, ColumnWidths -> {{10, 50}}];
  boxes = ReplacePart[dmb, 2 -> grid] /. StandaloneHold[e_] :> e;
  RawBoxes @ boxes
];

toDVarName[Hold[s_]] := colorDynamicVar @ HoldSymbolName @ s;
toDVarDisplay[Hold[s_]] := DynamicBox[ToBoxes @ NicePane[s, {UpTo[500], UpTo[50]}]];

colorDynamicVar[s_Str] := Scope[
  s = StringTrim[s, "$"..];
  id = FromDigits @ SJoin @ StringCases[s, Digit];
  dValueBoxes[s, id]
];

(**************************************************************************************************)

PublicVariable[$CustomDynamicModuleBoxTypesetting]

SetInitialValue[$CustomDynamicModuleBoxTypesetting, True];

Unprotect[DynamicModuleBox];
MakeBoxes[DynamicModuleBox[vars_List, body_], StandardForm] /; $CustomDynamicModuleBoxTypesetting :=
  makeDynamicModuleBoxBoxes[vars, body];
Protect[DynamicModuleBox];

SetHoldAll[makeDynamicModuleBoxBoxes];

makeDynamicModuleBoxBoxes[vars_, body_] := Scope[
  held = InternalHoldForm[DynamicModuleBox[vars, body]];
  str = MakeBoxes @ CompactPrettyFullForm[held,
    CompactingWidth -> 200, FullSymbolContext -> False, MaxIndent -> 10,
    TabSize -> 1];
  str2 = SRep[str, $highlightDVars];
  StyleBox[str2, StripOnInput -> True, "Code"]
];

$highlightDVars = sym:(Maybe["$CellContext`"] ~~ name:("$" | RomanLetter).. ~~ digits:RomanDigit... ~~ "$$") :>
   ToLinearSyntax @ RawBoxes @ Cons[
    InterpretationBox,
    dValueBoxes[name, FromDigits @ digits],
    ToExpression @ sym
  ];

