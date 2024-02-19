PublicFunction[CreateSymbol]

CreateSymbol[name_Str, value_] :=
  ToExpression[name, InputForm, SetOperator[value]];

(**************************************************************************************************)

PublicFunction[FindFunctionSymbols]
PublicFunction[FindInertSymbols]
PublicFunction[FindDownEvaluationSymbols]
PublicFunction[FindSubEvaluationSymbols]
PublicFunction[FindUpEvaluationSymbols]
PublicFunction[FindOwnEvaluationSymbols]
PublicFunction[FindOperationFormSymbols]

FindFunctionSymbols[glob_String]           := ToExpression[Names @ glob, InputForm, toFunctionSymbol];
FindInertSymbols[glob_String]              := ToExpression[Names @ glob, InputForm, toInertSymbol];
FindDownEvaluationSymbols[glob_String]     := ToExpression[Names @ glob, InputForm, toDownEvaluationSymbol];
FindSubEvaluationSymbols[glob_String]      := ToExpression[Names @ glob, InputForm, toSubEvaluationSymbol];
FindUpEvaluationSymbols[glob_String]       := ToExpression[Names @ glob, InputForm, toUpEvaluationSymbol];
FindOwnEvaluationSymbols[glob_String]      := ToExpression[Names @ glob, InputForm, toOwnEvaluationSymbol];
FindOperationFormSymbols[glob_String]      := ToExpression[Names @ glob, InputForm, toOperatorFormSymbol];

SetHoldAllComplete[toFunctionSymbol, toInertSymbol, toDownEvaluationSymbol, toSubEvaluationSymbol, toUpEvaluationSymbol, toOwnEvaluationSymbol, toOperatorFormSymbol, hasSubUsageQ];

toFunctionSymbol[s_Symbol]       := If[HasOwnEvaluationsQ[s] || !HasDownEvaluationsQ[s], Nothing, s];
toInertSymbol[s_Symbol]          := If[HasAnyEvaluationsQ[s], Nothing, s];
toDownEvaluationSymbol[s_Symbol] := If[HasDownEvaluationsQ[s], Hold @ s, Nothing];
toSubEvaluationSymbol[s_Symbol]  := If[HasSubEvaluationsQ[s],  Hold @ s, Nothing];
toUpEvaluationSymbol[s_Symbol]   := If[HasUpEvaluationsQ[s],   Hold @ s, Nothing];
toOwnEvaluationSymbol[s_Symbol]  := If[HasOwnEvaluationsQ[s],  Hold @ s, Nothing];
toOperatorFormSymbol[s_Symbol]   := Which[
  HasOwnEvaluationsQ[s] || HasNoEvaluationsQ[s], Nothing,
  HasSubEvaluationsQ[s] && HasDownEvaluationsQ[s] && hasSubUsageQ[s], s,
  True, Nothing
];

hasSubUsageQ[s_] := !StringQ[MessageName[s, "usage"]] || StringContainsQ[MessageName[s, "usage"], " operator "];

(**************************************************************************************************)

PublicFunction[UsageString]

SetHoldAll[UsageString]

UsageString[s_Symbol | Hold[s_Symbol]] := Replace[MessageName[s, "usage"], _MessageName :> ""]
UsageString[_] := "";

(**************************************************************************************************)

PublicFunction[ExtractUsageLHS, ParseUsageLHS, ParseUsageBoxes]

PrivateHead[$UsagePatt, $UsageStr, $UsageEtc, $UsageRuleD]

ExtractUsageLHS[usage_Str] := Flatten @ StringCases[StringSplit[usage, "\n"], {
  StartOfLine ~~ (LinearSyntaxPattern ~~ " or ").. ~~ s:LinearSyntaxPattern :> FromLinearSyntax[s],
  StartOfLine ~~ s:LinearSyntaxPattern :> FromLinearSyntax[s]
}];

ParseUsageLHS[usage_Str] := Scope[
  boxes = ExtractUsageLHS[usage];
  $unknown = Assoc[];
  parsed = Map[ParseUsageBoxes, boxes];
  If[Len[$unknown] > 0, Print[Grid @ $unknown]];
  parsed
];

ParseUsageBoxes[boxes_] := Scope[
  $boxes = boxes;
  parseUsageBoxes[boxes] //. $usageTransforms
]

$usageTypeHints = Assoc[
$UsageAny              -> "e expr elem a b c s x new snew val value",
$UsageKey              -> "k key",
$UsageAssoc            -> "assoc",
$UsageBody             -> "body",
$UsageColor            -> "col color",
$UsageDefault          -> "def default",
$UsageFn               -> "f fs fn red test crit",
$UsageImage            -> "img image",
$UsageInt              -> "di m n i j width depth height level max",
$UsageIntOrIntPair     -> "seq",
$UsageIntPair          -> "range",
$UsageLen              -> "len",
$UsageLevel            -> "levelspec",
$UsagePatt             -> "pattern patt",
$UsagePos              -> "pos",
$UsageSpec             -> "spec nspec nlist dlist klist padlist type codes",
$UsageStr              -> "str string path",
$UsageSymbol           -> "h sym symbol",
$UsageArray            -> "array data",
$UsageStrPat           -> "forms",
$UsageVars             -> "params",
$UsageMList[$UsageStr] -> "dirs",
$UsageList[$UsageAny]  -> "list vals",
$UsageList[$UsageRule] -> "rules srules",
$UsageList[$UsageStr]  -> "strings strs",
$UsageList[$UsageKey]  -> "keys",
$UsageList[$UsageRule] -> "opts rules"
];

(* use contextual hints:
"p Function Sort"
"p, patt StringPatt StringCount"
"s String StringCount"
*)

pattNameToType[e_] := (KeyAppendTo[$unknown, e, Apply[RawBoxes, $boxes]]; $UsageAny);
KeyValueScan[{type, str} |-> With[{patt = Alternatives @@ StringSplit[str, " "]}, pattNameToType[patt] = type], $usageTypeHints];

$usageTransforms = {
  {Subscript[p_, _].., $UsageEtc}                                      :> $UsageList[p],
  Subscript[p_, _]                                                     :> p,
  {$UsagePatt @ "min", $UsagePatt @ "max"}                             :> $UsageIntPair,
  $UsagePatt["f"][$UsagePatt["args"]]                                  :> $UsageAny,
  $UsagePatt[name_Str]                                                 :> pattNameToType @ name,
  $UsageStr[_]                                                         :> $UsageStr,
  "Rule"[_, _]                                                         :> $UsageRule,
  "RuleDelayed"[_, _]                                                  :> $UsageRuleD,
  "Minus"[_]                                                           :> $UsageNegativeInt,
  "Key"[_]                                                             :> $UsageHeaded[Key],
  "Span"[__]                                                           :> $UsageSpan,
  {p_, p_, $UsageEtc}                                                  :> $UsageList[p],
  {p_, p_, p_, $UsageEtc}                                              :> $UsageList[p]
};

SetHoldAll[parseUsageBoxes, parseCommaOrBox];

parseUsageBoxes = Case[
  Hold[e_]                          := % @ e;
  "\[Ellipsis]"                     := $UsageEtc;
  StyleBox[a_, "TI"]                := $UsagePatt[% @ a];
  StyleBox[a_, ___]                 := % @ a;

  R[e_, "[", R["[", p_, "]"], "]"]  := Construct["Part", % @ e, % @ p];

  R[h_, "[", "]"]            := Construct[% @ h];
  R[h_, "[", a_, "]"]        := Construct[% @ h, % @ a];

  R["{", "}"]                := List[];
  R["{", b_, "}"]            := List[% @ b];

  R[$lassoc, $rassoc]                          := "Association"[];
  R[$lassoc, a_, $rassoc]                      := "Association"[% @ a];
  R[$lassoc, a_, ",", b_, $rassoc]             := "Association"[% @ a, % @ b];
  R[$lassoc, a_, ",", b_, ",", c_, $rassoc]    := "Association"[% @ a, % @ b, % @ c];
(*
  R[a_, ";;", b_]            := Span[% @ a, % @ b];
  R[a_, ";;", b_, ";;", c_]  := Span[% @ a, % @ b, % @ c];
  R[a_, $rule, b_]           := Rule[% @ a, % @ b];
  R[a_, $ruleD, b_]          := RuleDelayed[% @ a, % @ b];

  R[body_, "&"]              := Construct["Function", % @ body];
  R[a_, $fn, body_]          := Construct["Function", % @ a, % @ body];
 *)

  R[op_Str ? prefixOpQ, b_]                := Construct[$prefixMap @ op, % @ b];
  R[a_, op_Str ? postfixOpQ]               := Construct[$postfixMap @ op, % @ a];
  R[a_, op_Str ? infixOpQ, b_]             := Construct[$infixMap @ op, % @ a, % @ b];
  R[a_, op_Str, b_, op_Str, c_] /; naryInfixOpQ[op] := Construct[$naryInfixMap @ op, % @ a, % @ b, % @ c];

  R[a_]                             := % @ a;
  RowBox[list_List ? commaListQ]    := Seq @@ MapUnevaluated[parseUsageCommaOrBox, list];

  SubscriptBox[a_, b_]              := Subscript[% @ a, % @ b];
  SuperscriptBox[a_, b_]            := Subscript[% @ a, % @ b];

  s_Str ? DQuotedStringQ            := $UsageStr @ Construct[%, StripDQuotes @ s];
  s_Str ? LinearSyntaxQ             := % @@ FromLinearSyntax @ s;
  $Failed                           := $Failed;

  e_Symbol                          := HoldSymbolName @ e; (* occurs only in CoordinateBoundingBox ? *)
  e_                                := e;
,
  {$lassoc           :> "<|" | "\[LeftAssociation]",
   $rassoc           :> "|>" | "\[RightAssociation]",
   $rule             :> "\[Rule]" | "->",
   $fn               :> "\[Function]" | "|->",
   $ruleD            :> "\[RuleDelayed]" | ":>",
   R[a$___]          :> RowBox[{a$}]
  }
];

parseUsageCommaOrBox = Case[
  "," := Nothing;
  e_  := parseUsageBoxes @ e;
];

commaListQ = Case[
  {}                             := True;
  {PatternSequence[_, ","].., _} := True;
  _                              := False;
];

(**************************************************************************************************)

PublicFunction[$WolframCharacterExpansions, $WolframCharacterContractions]

$WolframCharacterExpansions := $WolframCharacterExpansions = UAssoc @ MathCharacterData["Symbol" -> "InputForm"];
$WolframCharacterContractions := $WolframCharacterContractions = UAssoc @ MathCharacterData["InputForm" -> "Symbol"];

(**************************************************************************************************)

infixOpQ[s_]       := KeyExistsQ[$infixMap, s];
naryInfixOpQ[s_]   := KeyExistsQ[$naryInfixMap, s];
binaryInfixOpQ[s_] := KeyExistsQ[$binaryInfixMap, s];
prefixOpQ[s_]      := KeyExistsQ[$prefixMap, s];
postfixOpQ[s_]     := KeyExistsQ[$postfixMap, s];

$infixMap       := $infixMap         = Join[$binaryInfixMap, $naryInfixMap];
$naryInfixMap   := $naryInfixMap     = loadSyntaxMap["NAryInfix.txt"];
$binaryInfixMap := $binaryInfixMap   = loadSyntaxMap["BinaryInfix.txt"];
$prefixMap      := $prefixMap        = loadSyntaxMap["Prefix.txt"];
$postfixMap     := $postfixMap       = loadSyntaxMap["Postfix.txt"];

loadSyntaxMap[name_] := Scope[
  path = LocalPath["Data", "Wolfram", name];
  table = ImportUTF8 @ path;
  entries = StringTrim /@ StringSplit[StringSplit[table, "\n"], Whitespace];
  rules = charHeadRule @@@ entries;
  Association @ rules
];

charHeadRule[k_, v_] /; KeyExistsQ[$WolframCharacterContractions, k] := {k -> v, $WolframCharacterContractions[k] -> v};
charHeadRule[k_, v_] := k -> v;

(**************************************************************************************************)

PublicFunction[CreateMultipleSymbols]

CreateMultipleSymbols[context_, names:{___Str}, values_List] := Block[
  {$Context = context, $ContextPath = {"System`", "Global`"}},
  If[Length[names] =!= Length[values], Message[CreateMultipleSymbols::badlen, Length @ names, Length @ values, Short @ names]];
  ToExpression[StringJoin["{", Riffle[names, ","], "}"], InputForm, SetOperator[values]]
]

CreateMultipleSymbols::badlen = "Was provided `` names and `` values: ``."
CreateMultipleSymbols[___] := $Failed;

(**************************************************************************************************)

PublicFunction[UpdateSublimeSyntaxFiles]

$sublimeSyntaxPath = "~/git/Sublime-WolframLanguage";

groupSortIndex[_] := 0;
groupSortIndex["Option"] = 1;
groupSortIndex["Function"] = 2;

UpdateSublimeSyntaxFiles[] := Scope[
  inFile = PathJoin[$sublimeSyntaxPath, "WolframLanguage.sublime-syntax.template"];
  outSyntaxFile = PathJoin[$sublimeSyntaxPath, "WolframLanguage.sublime-syntax"];
  outCompletionsFile = PathJoin[$sublimeSyntaxPath, "WolframLanguage.sublime-completions"];
  template = ImportUTF8 @ inFile;
  groups = ImportMX @ LocalPath["Data", "Syntax", "SystemSymbols.mx"];
  groups //= KeySortBy[groupSortIndex];
  localGroups = QuiverGeometryLoader`$SymbolGroups;
  KeyValueMap[addToGroup, localGroups];
  guSymbols = Names["GeneralUtilities`*"];
  guSymbols //= Select[StringLength[#] > 2 && StringStartsQ[#, UppercaseLetter] &];
  addToGroup["Function", guSymbols];
  (* add each alias to the group of its target *)
  MapApply[
    {alias, target} |-> (
      group = Part[SelectFirstIndex[QuiverGeometryLoader`$SymbolTable, MemberQ[#, target]&, Print["ALIAS NOT FOUND: ", target]], 1, 2];
      addToGroup[group, {alias}]
    ),
    QuiverGeometryLoader`$SymbolAliases
  ];
  addToGroup["SpecialFunction", {"ExpressionTable"}];
  {regexpSections, symbolSections} = Transpose @ KeyValueMap[makeSplitDefs, groups];
  completionLists = KeyValueMap[makeCompletionDefs, groups];
  regexpDefs = StringRiffle[regexpSections, "\n"];
  symbolDefs = StringRiffle[symbolSections, "\n"];
  completionDefs = $completionListTemplate @ StringTrimRight[",\n"] @ StringRiffle[Catenate @ completionLists, ",\n"];
  filledTemplate = StringReplace[template, {"$REGEXPS$" -> regexpDefs, "$SYMBOLS$" -> symbolDefs}];
  {
    ExportUTF8[outCompletionsFile, completionDefs],
    ExportUTF8[outSyntaxFile, filledTemplate]
  }
]

addToGroup[group_, syms_] := KeyUnionTo[groups, group, Complement[syms, Catenate @ groups]];

$completionListTemplate = StringFunction @
"""{
  "scope": "source.wolfram",
  "completions": [
#1
  ]
}""";

groupToSyntaxScope = Case[
  "Symbol" := "constant.language.symbol.wolfram";
  "Function" := % @ "builtin";
  "Option" := "constant.language.option.wolfram";
  other_ := StringJoin["support.function.",
    ToLowerCase @ StringRiffle[CamelCaseSplit @ StringDelete[other, "Function"], "."],
    ".wolfram"
  ];
];

$groupToSymbol = Assoc[
  "Package"                -> "p",
  "Symbol"                 -> "s",
  "Head"                   -> "f",
  "Object"                 -> "f",
  "Function"               -> "f",
  "MutatingFunction"       -> "f",
  "SpecialFunction"        -> "f",
  "DebuggingFunction"      -> "f",
  "Option"                 -> "\[RightArrow]",
  "TypesettingForm"        -> "■",
  "TypesettingBoxFunction" -> "□",
  "GraphicsDirective"      -> "꠵",
  "GraphicsPrimitive"      -> "△",
  "GraphicsBoxFunction"    -> "▲",
  "ScopingFunction"        -> "f",
  "Variable"               -> "$"
];

groupToKindColorProxy = Case[
  "Symbol"                                                          := "symbol";
  "Object"                                                          := "function";
  "Function" | "MutationFunction" | "ScopingFunction"               := "function";
  "Package" | "DebuggingFunction" | "SpecialFunction"               := "function";
  "Variable"                                                        := "variable";
  "GraphicsBoxFunction" | "GraphicsPrimitive" | "GraphicsDirective" := "navigation";
  "TypesettingBoxFunction" | "TypesettingForm"                      := "snippet";
  _                                                                 := "symbol";
];

makeSplitDefs["Variable", names_] :=
  MapLast[
    StringReplace["- match: '{{" -> "- match: '\\${{"],
    makeSplitDefs2["Variable", StringDrop[names, 1]]
  ];

makeSplitDefs[group_, names_] :=
  makeSplitDefs2[group, names];

makeSplitDefs2[group_, names_] := Scope[
  scope = groupToSyntaxScope[group];
  names //= DeleteDuplicates;
  If[Length[names] >= 16,
    grouped = KeySort @ GroupBy[names, StringJoin[group, "_", StringTake[#, 1]]&];
    grouped //= KeyMap[StringReplace["$" -> "DOLLAR"]];
    defItems = StringJoin @ KeyValueMap[makeLetterSubDef, grouped] // escapeDollars;
    parseItems = StringJoin @ Map[makeParseItem[scope]] @ Keys[grouped];
  ,
    defItems = StringJoin["  ", group, ": (?:", StringRiffle[names, "|"], ")\n"];
    parseItems = makeParseItem[scope][group];
  ];
  {defItems, parseItems}
];

makeCompletionDefs[group_, names_] := Scope[
  kindString = $kindTemplate[groupToKindColorProxy @ group, $groupToSymbol @ group, group];
  completions = Map[makeCompletion[kindString], names];
  completions
];

escapeDollars[e_] := StringReplace[e, "$" -> "\\$"];
doubleEscapeDollars[e_] := StringReplace[e, "$" -> "\\\\$"];

$kindTemplate = StringFunction @ """["#1", "#2", "#3"]""";
$completionTemplate = StringFunction @ """    {"trigger": "#1", "contents": "#2", "kind": #3}"""

makeCompletion[kindString_][name_] := If[StringLength[name] <= 3, Nothing,
  $completionTemplate[name, doubleEscapeDollars @ name, kindString]
];

makeParseItem[scope_][defName_] :=
  StringJoin["    - match: '{{", defName, "}}{{symbolEndBoundary}}'\n      scope: ", scope, "\n"];

makeShortDef[defName_, strings_] :=
  StringJoin["  ", defName, ": (?:", StringRiffle[strings, "|"], ")\n"];

makeLetterSubDef[subDefName_, strings_] :=
  StringJoin[
    "  ", subDefName, ": (?:",
    StringTake[P1 @ strings, 1],
    "(?:", StringRiffle[StringDrop[strings, 1], "|"],
    "))\n"
];