PublicFunction[CreateSymbol]

CreateSymbol[name_String, value_] :=
  ToExpression[name, InputForm, SetOperator[value]];

(**************************************************************************************************)

PublicFunction[CreateMultipleSymbols]

CreateMultipleSymbols[context_, names:{___String}, values_List] := Block[
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
      group = Part[SelectFirstIndex[QuiverGeometryLoader`$SymbolTable, MemberQ[#, target]&], 1, 2];
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

$groupToSymbol = Association[
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
    StringTake[First @ strings, 1],
    "(?:", StringRiffle[StringDrop[strings, 1], "|"],
    "))\n"
];