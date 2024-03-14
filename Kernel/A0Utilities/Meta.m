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

hasSubUsageQ[s_] := !StrQ[MessageName[s, "usage"]] || SContainsQ[MessageName[s, "usage"], " operator "];

(**************************************************************************************************)

PublicFunction[UsageString]

SetHoldAll[UsageString]

UsageString[s_Symbol | Hold[s_Symbol]] := Rep[MessageName[s, "usage"], _MessageName :> ""]
UsageString[_] := "";

(**************************************************************************************************)

PublicFunction[ExtractUsageLHS, ParseUsageLHS, ParseUsageBoxes]

PrivateHead[$UsagePatt, $UsageStr, $UsageEtc, $UsageRuleD]

ExtractUsageLHS[usage_Str] := Flatten @ SCases[SSplit[usage, "\n"], {
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

pattNameToType[e_] := (KAppTo[$unknown, e, Apply[RawBoxes, $boxes]]; $UsageAny);
KVScan[{type, str} |-> With[{patt = Alt @@ SSplit[str, " "]}, pattNameToType[patt] = type], $usageTypeHints];

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

SetCached[$WolframCharacterExpansions,   UAssoc @ MathCharacterData["Symbol" -> "InputForm"]];
SetCached[$WolframCharacterContractions, UAssoc @ MathCharacterData["InputForm" -> "Symbol"]];

(**************************************************************************************************)

infixOpQ[s_]       := KeyQ[$infixMap, s];
naryInfixOpQ[s_]   := KeyQ[$naryInfixMap, s];
binaryInfixOpQ[s_] := KeyQ[$binaryInfixMap, s];
prefixOpQ[s_]      := KeyQ[$prefixMap, s];
postfixOpQ[s_]     := KeyQ[$postfixMap, s];

SetCached[$infixMap,       Join[$binaryInfixMap, $naryInfixMap]];
SetCached[$naryInfixMap,   loadSyntaxMap["NAryInfix.txt"]];
SetCached[$binaryInfixMap, loadSyntaxMap["BinaryInfix.txt"]];
SetCached[$prefixMap,      loadSyntaxMap["Prefix.txt"]];
SetCached[$postfixMap,     loadSyntaxMap["Postfix.txt"]];

loadSyntaxMap[name_] := Scope[
  path = DataPath["Wolfram", name];
  table = ImportUTF8 @ path;
  entries = STrim /@ SSplit[SSplit[table, "\n"], Whitespace];
  rules = charHeadRule @@@ entries;
  Assoc @ rules
];

charHeadRule[k_, v_] /; KeyQ[$WolframCharacterContractions, k] := {k -> v, $WolframCharacterContractions[k] -> v};
charHeadRule[k_, v_] := k -> v;

(**************************************************************************************************)

PublicFunction[CreateMultipleSymbols]

CreateMultipleSymbols[context_, names:{___Str}, values_List] := Block[
  {$Context = context, $ContextPath = {"System`", "Global`"}},
  If[Len[names] =!= Len[values], Message[CreateMultipleSymbols::badlen, Len @ names, Len @ values, Short @ names]];
  ToExpression[SJoin["{", Riffle[names, ","], "}"], InputForm, SetOperator[values]]
]

CreateMultipleSymbols::badlen = "Was provided `` names and `` values: ``."
CreateMultipleSymbols[___] := $Failed;

(**************************************************************************************************)

PublicIOFunction[SourceLineFind, SourceStringCases, SourceStringReplace]

$kernelPathLen = SLen[LocalPath["Kernel"]] + 1;

SourceLineFind[patt_, opts___Rule]     := FileLineFind[MTLoader`$SourceFiles, patt, opts];
SourceStringCases[patt_, opts___Rule]   := FileStringCases[MTLoader`$SourceFiles, patt, opts] // Rep[a_Assoc :> KMap[SDrop[#, $kernelPathLen]&, a]];
SourceStringReplace[patt_, opts___Rule] := FileStringReplace[MTLoader`$SourceFiles, patt, opts];

(**************************************************************************************************)

PublicIOFunction[RevertSublimeSyntaxFiles]

RevertSublimeSyntaxFiles[] :=
  RunTool["git", "checkout", "--", "WolframLanguage.sublime-syntax", "WolframLanguage.sublime-completions", WorkingDirectory -> $sublimeSyntaxPath];

(**************************************************************************************************)

PublicIOFunction[UpdateSublimeSyntaxFiles]

PublicVariable[$LastSublimeSyntaxGroups]

$LastSublimeSyntaxGroups = None;

Options[UpdateSublimeSyntaxFiles] = {Verbose -> True};

$sublimeSyntaxPath = "~/git/Sublime-WolframLanguage";

groupSortIndex[_] := 0;
groupSortIndex["Option"] = 1;
groupSortIndex["Function"] = 2;

UpdateSublimeSyntaxFiles::messageOccurred = "Message occurred during computation, aborting.";
UpdateSublimeSyntaxFiles::invalidResult = "Did not produce valid results, aborting.";
UpdateSublimeSyntaxFiles::badAlias = "Cannot resolve alias `` to a core symbol.";
UpdateSublimeSyntaxFiles::noSystemSymbols = "Cannot load system symbol groups from ``. Have you run generate_syntax_table.wls?";

UpdateSublimeSyntaxFiles[OptionsPattern[]] := Scope @ CatchMessage[

  UnpackOptions[$verbose];
  inFile             = PathJoin[$sublimeSyntaxPath, "WolframLanguage.sublime-syntax.template"];
  outSyntaxFile      = PathJoin[$sublimeSyntaxPath, "WolframLanguage.sublime-syntax"];
  outCompletionsFile = PathJoin[$sublimeSyntaxPath, "WolframLanguage.sublime-completions"];

  res = Check[
    template = ImportUTF8 @ inFile;
    loaderSymbolTable = Map[symsToNames, MTLoader`$SymbolTable];

    (* system groups *)
    systemSymbolPath = DataPath["Wolfram", "SystemSymbolTable.mx"];
    groups = ImportMX @ systemSymbolPath;
    If[!AssocQ[groups], Msg::noSystemSymbols[MsgPath @ systemSymbolPath]];

    groups = Union /@ groups;
    (* allow changes to Data/Wolfram/SymbolTable.m to override the symbol group assignments,
       since its expensive to run the generate_syntax_table.wls script *)
    loaderSymbolGroups = Merge[KVMap[L[#1] -> #2&, loaderSymbolTable], Catenate];
    loaderSymbols = Union @ Catenate @ loaderSymbolTable;
    groups = AssocMap[
      Union[Lookup[loaderSymbolGroups, #, {}], Comp[Lookup[groups, #, {}], loaderSymbols]]&,
      Union[Keys @ groups, Keys @ loaderSymbolGroups]
    ];
    If[!AssocQ[groups], ReturnFailed[]];

    groups //= KSortBy[groupSortIndex];
    localGroups = MTLoader`$SymbolGroups;

    KVMap[addToGroup, localGroups];
    addToGroup["SpecialFunction", {"ExpressionTable"}];

    VPrint["Extracting GU symbols."];
    guSymbols = Complement[Names["GeneralUtilities`*"], loaderSymbols];
    guSymbols //= Select[SLen[#] > 2 && SStartsQ[#, UppercaseLetter] &];
    addToGroup["Function", guSymbols];

    (* add each alias to the group of its target *)
    coreNameToGroup = Assoc @ KVMap[Thread @ Rule[#2, #1]&, loaderSymbolGroups];
    aliasGroups = KVMap[
      {alias, target} |-> (
        group = Lookup[coreNameToGroup, target, Msg::badAlias[alias -> target]];
        addToGroup[group, {alias}];
        alias -> group
      ),
      MTLoader`$FromSymbolAlias
    ];
    VPrint["Resolved aliases groups."];

    VPrint["Processing symbol groups. Groups available at $LastSublimeSyntaxGroups."];
    (* groups //= KeySort; *)
    $LastSublimeSyntaxGroups ^= groups;
    {regexpSections, symbolSections} = VBlock @ Transpose @ KVMap[makeSplitDefs, groups];

    VPrint["Processing completions."];
    completionLists = KVMap[makeCompletionDefs, groups];
    regexpDefs = SRiffle[regexpSections, "\n"];
    symbolDefs = SRiffle[symbolSections, "\n"];
    completionDefs = $completionListTemplate @ StringTrimRight[",\n"] @ SRiffle[Catenate @ completionLists, ",\n"];
    filledTemplate = SRep[template, {"$REGEXPS$" -> regexpDefs, "$SYMBOLS$" -> symbolDefs}];
  ,
    $Failed
  ];
  If[res === $Failed, ReturnFailed["messageOccurred"]];
  If[!StrQ[completionDefs] || !StrQ[filledTemplate], ReturnFailed["invalidResult"]];
  VPrint["Writing files."];
  {
    ExportUTF8[outCompletionsFile, completionDefs],
    ExportUTF8[outSyntaxFile, filledTemplate]
  }
]

symsToNames = Case[
  l_List      := Map[symsToNames, l];
  Hold[a___]  := Splice @ MapUnevaluated[HoldSymbolName, {a}];
  s_Symbol    := SymbolName[s];
];

addToGroup[group_, {}] := Null;
addToGroup[group_, syms_] := KUnionTo[groups, group,
  unassigned = Comp[syms, Catenate @ KDrop[groups, group]];
  If[Len[unassigned] < Len[syms],
    VPrint[group, ": following syms already assigned: ", Comp[syms, unassigned]]];
  unassigned
];

$completionListTemplate = StringFunction @
"""{
  "scope": "source.wolfram",
  "completions": [
#1
  ]
}""";

groupToSyntaxScope = Case[
  "Symbol"   := "constant.language.symbol.wolfram";
  "Function" := % @ "builtin";
  "Option"   := "constant.language.option.wolfram";
  other_     := igroupToSyntaxScope @ other;
];

igroupToSyntaxScope[group_] := igroupToSyntaxScope[str] =
  SJoin["support.function.",
    ToLowerCase @ SRiffle[CamelCaseSplit @ SRep[group, {"Function" -> "", "IO" -> "Io"}], "."],
    ".wolfram"
  ];

$groupToSymbol = Assoc[
  "Package"                -> "p",
  "Symbol"                 -> "s",
  "Head"                   -> "f",
  "Object"                 -> "f",
  "Function"               -> "f",
  "MutatingFunction"       -> "f",
  "SpecialFunction"        -> "f",
  "StringPattern"          -> "f",
  "DebuggingFunction"      -> "f",
  "Option"                 -> "\[RightArrow]",
  "TypesettingForm"        -> "■",
  "TypesettingBoxFunction" -> "□",
  "GraphicsDirective"      -> "꠵",
  "GraphicsPrimitive"      -> "△",
  "GraphicsBoxFunction"    -> "▲",
  "ScopingFunction"        -> "f",
  "Variable"               -> "$",
  "SpecialVariable"        -> "$",
  "CacheVariable"          -> "$"
];

groupToKindColorProxy = Case[
  "Symbol"                                                          := "symbol";
  "Object"                                                          := "function";
  "Function" | "MutationFunction" | "ScopingFunction"               := "function";
  "Package" | "DebuggingFunction" | "SpecialFunction"               := "function";
  "Variable" | "SpecialVariable" | "CacheVariable"                  := "variable";
  "GraphicsBoxFunction" | "GraphicsPrimitive" | "GraphicsDirective" := "navigation";
  "TypesettingBoxFunction" | "TypesettingForm"                      := "snippet";
  _                                                                 := "symbol";
];

UpdateSublimeSyntaxFiles::badVariableSymbolNames = "Some symbols in group `` did not start with $: ``.";

makeSplitDefs[group:"Variable" | "SpecialVariable" | "CacheVariable", names_] :=
  MapLast[
    SRep["- match: '{{" -> "- match: '\\${{"],
    If[!AllTrue[names, SStartsQ["$"]],
      Message[UpdateSublimeSyntaxFiles::badVariableSymbolNames, group, Discard[names, SStartsQ["$"]]]];
    makeSplitDefs2[group, SDrop[names, 1]]
  ];

makeSplitDefs[group_, names_] :=
  makeSplitDefs2[group, names];

makeSplitDefs2[group_, names_] := Scope[
  scope = groupToSyntaxScope[group];
  names //= Dedup;
  VPrint[
    SPadRight[group, 30], " -> ", SPadRight[scope, 50],
    If[Len[names] < 200, Tooltip[#, Multicolumn[names, 3]]&, Id] @ SJoin[" (", IntStr @ Len @ names, " symbols)"]
  ];
  If[Len[names] >= 16,
    grouped = KSort @ GroupBy[names, SJoin[group, "_", STake[#, 1]]&];
    grouped //= KMap[SRep["$" -> "DOLLAR"]];
    defItems = SJoin @ KVMap[makeLetterSubDef, grouped] // escapeDollars;
    parseItems = SJoin @ Map[makeParseItem[scope]] @ Keys[grouped];
  ,
    defItems = SJoin["  ", group, ": (?:", SRiffle[names, "|"], ")\n"];
    parseItems = makeParseItem[scope][group];
  ];
  {defItems, parseItems}
];

makeCompletionDefs[group_, names_] := Scope[
  kindString = $kindTemplate[groupToKindColorProxy @ group, $groupToSymbol @ group, group];
  completions = Map[makeCompletion[kindString], names];
  completions
];

escapeDollars[e_] := SRep[e, "$" -> "\\$"];
doubleEscapeDollars[e_] := SRep[e, "$" -> "\\\\$"];

$kindTemplate = StringFunction @ """["#1", "#2", "#3"]""";
$completionTemplate = StringFunction @ """    {"trigger": "#1", "contents": "#2", "kind": #3}"""

makeCompletion[kindString_][name_] := If[SLen[name] <= 3, Nothing,
  $completionTemplate[name, doubleEscapeDollars @ name, kindString]
];

makeParseItem[scope_][defName_] :=
  SJoin["    - match: '{{", defName, "}}{{symbolEndBoundary}}'\n      scope: ", scope, "\n"];

makeShortDef[defName_, strings_] :=
  SJoin["  ", defName, ": (?:", SRiffle[strings, "|"], ")\n"];

makeLetterSubDef[subDefName_, strings_] :=
  SJoin[
    "  ", subDefName, ": (?:",
    STake[F @ strings, 1],
    "(?:", SRiffle[SDrop[strings, 1], "|"],
    "))\n"
];