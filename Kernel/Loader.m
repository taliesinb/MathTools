Begin["QuiverGeometryPackageLoader`Private`"];

Needs["GeneralUtilities`"];

Unprotect[EpilogFunction];
ClearAll[EpilogFunction];

(* this can trigger paclet downloads, which if $AllowInternet = False will cause a message
here, so we Quiet it *)
Quiet[
  Style;
  SyntaxInformation[Style];
  Options[Style];
];

(* moved from A0Usage.m because this runs slowly:
the default behavior of System`InformationDump` will introduce LineSpacing that messes up
my SetUsage inline tables, so remove it. *)

dummy::usage = "Dummy";
ToBoxes[Information[dummy]];
System`InformationDump`subtitleStyled[sub_] := Style[sub, "InformationUsageText"];

(*************************************************************************************************)

QuiverGeometryPackageLoader`$LoaderFileName = $InputFileName;

QuiverGeometryPackageLoader`$SourceDirectory = DirectoryName @ $InputFileName;

QuiverGeometryPackageLoader`$PackageDirectory = ParentDirectory @ QuiverGeometryPackageLoader`$SourceDirectory;

QuiverGeometryPackageLoader`$CurrentFile = None;

QuiverGeometryPackageLoader`$PreservedVariables = Data`UnorderedAssociation[];
QuiverGeometryPackageLoader`$PreservedFunctions = Data`UnorderedAssociation[];

Attributes[QuiverGeometryPackageLoader`DeclarePreservedVariable] = {HoldAll};
QuiverGeometryPackageLoader`DeclarePreservedVariable[var_] :=
  AssociateTo[QuiverGeometryPackageLoader`$PreservedVariables, Hold[var] -> True];

Attributes[QuiverGeometryPackageLoader`DeclarePreservedFunction] = {HoldAll};
QuiverGeometryPackageLoader`DeclarePreservedFunction[fn_] :=
  AssociateTo[QuiverGeometryPackageLoader`$PreservedFunctions, Hold[fn] -> True];


(*************************************************************************************************)

(* Fix GU's ability to open files in subl on MacOS *)
If[$OperatingSystem === "MacOSX",
  GeneralUtilities`Packages`PackagePrivate`$sublimePath = "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl";
];

(* we will immediately resolve these system symbols, which will take care of the vast majority of Package`PackageSymbol cases *)
$coreSymbols = Get @ FileNameJoin[{QuiverGeometryPackageLoader`$SourceDirectory, "SymbolTable.m"}];

$coreSymbols = Sort @ DeleteDuplicates @ $coreSymbols;

$coreSymbolNames = SymbolName /@ $coreSymbols;
$coreSymbolContexts = Context /@ $coreSymbols;

$corePackageSymbols = DeleteCases[Package`PublicScopedOption | Package`PublicTypesettingFormBox] @ Pick[$coreSymbols, $coreSymbolContexts, "Package`"];
$corePackageSymbolNames = SymbolName /@ $corePackageSymbols;

makePackageSymbolHeadP[s_] := Apply[Alternatives, Pick[$corePackageSymbols, StringStartsQ[$corePackageSymbolNames, s]]];
$systemPackageDeclarationHeadP = makePackageSymbolHeadP["System"];
$publicPackageDeclarationHeadP = makePackageSymbolHeadP["Public"];
$privatePackageDeclarationHeadP = makePackageSymbolHeadP["Private"];

toRegexPattern["$Failed"] := "(\\$Failed)";
toRegexPattern[str_] := "(" <> str <> ")";
$coreSymbolRegex = RegularExpression @ StringJoin @ Riffle[Map[toRegexPattern, Sort @ $coreSymbolNames], "|"];

$coreSymbolAssociation = AssociationThread[$coreSymbolNames, $coreSymbols];

(*************************************************************************************************)

SetAttributes[ResolvedSymbol, HoldAllComplete];
makeResolvedSymbol[name_String] := ToExpression[name, InputForm, ResolvedSymbol];

$lowerCaseSymbolRegex = RegularExpression["[$]?[a-z]"];

$initialSymbolResolutionDispatch = With[{kernelInit = FileNameJoin[{QuiverGeometryPackageLoader`$PackageDirectory, "Kernel", "init.m"}]}, Dispatch[{
  (* Package`PackageSymbol[name_String] /; StringStartsQ[name, $lowerCaseSymbolRegex] :> RuleCondition[makeResolvedSymbol[name]], *)
  Package`PackageSymbol["SetUsage"][usageString_String]                      :> RuleCondition[rewriteSetUsage[usageString]],
  Package`PackageSymbol[name_String] /; StringMatchQ[name, $coreSymbolRegex] :> RuleCondition[$coreSymbolAssociation[name]],
  Package`PackageSymbol[name_String] /; StringContainsQ[name, "`"]           :> RuleCondition[makeResolvedSymbol[name]],
  Package`PackageSymbol["UAssociation"]                                      :> Data`UnorderedAssociation,
  p:Package`PackageSymbol["$PackageFileName"]                                :> RuleCondition[QuiverGeometryPackageLoader`$CurrentFile],
  p:Package`PackageSymbol["$PackageDirectory"]                               :> RuleCondition[QuiverGeometryPackageLoader`$PackageDirectory],
  p:Package`PackageSymbol["$PackageInitializer"]                             :> If[DownValues[QuiverGeometryPackageLoader`Load] === {}, Get @ kernelInit],
  p:Package`PackageSymbol["PublicScopedOption"][___]                         :> RuleCondition @ processScopedOption[p],
  p:Package`PackageSymbol["PublicTypesettingFormBox"][___]                   :> RuleCondition @ processTypesettingFormBox[p]
}]];

(* turns PublicScopedOption[Foo] into PublicOption[Foo], PrivateVariable[$foo] *)
processScopedOption[p_] :=
   Package`PackageSplice @@ Cases[p, Package`PackageSymbol[name_String] :> {
    Package`PublicOption[Package`PackageSymbol @ name],
    Package`PrivateVariables[Package`PackageSymbol @ StringJoin["$", ToLowerCase @ StringTake[name, 1], StringDrop[name, 1]]]
  }];

(* turns PublicTypesettingFormBox[AppliedForm] into PublicTypesettingForm[AppliedForm], PrivateBoxFunction[AppliedBox] *)
processTypesettingFormBox[p_] :=
   Package`PackageSplice @@ Cases[p, Package`PackageSymbol[name_String] :> {
    Package`PublicTypesettingForm[Package`PackageSymbol[name]],
    If[!StringEndsQ[name, "Form"], Print["ERROR: PublicTypesettingFormBox ", name]];
    Package`PrivateBoxFunction[Package`PackageSymbol[StringDrop[name, -4] <> "Box"]]
  }];

(* this means SetUsage doesn't have to resolve the symbol later, which is expensive. *)
rewriteSetUsage[usageString_String] := Scope[
  symbolName = StringCases[usageString, StartOfString ~~ WhitespaceCharacter... ~~ name:(Repeated["$", {0, 1}] ~~ WordCharacter..) :> name, 1];
  symbolName = First[symbolName, None];
  If[symbolName === None,
    Package`PackageSymbol["SetUsage"][usageString],
    Package`PackageSymbol["SetUsage"][Package`PackageSymbol[symbolName], usageString]
  ]
];

fileStringUTF8[path_] := Block[{bytes},
  bytes = ReadByteArray @ path;
  If[bytes === EndOfFile, "", ByteArrayToString @ bytes]
];

failRead[] := Throw[$Failed, failRead];

$fileContentCache = Data`UnorderedAssociation[];

observeTextFile[path_] := Module[{fileModTime},
  cachedModTime = Lookup[$fileContentCache, path, $Failed];
  fileModTime = UnixTime @ FileDate[path, "Modification"];
  If[cachedModTime =!= fileModTime,
    $changedTextFileCount++;
    $fileContentCache[path] = fileModTime;
  ];
];

LVPrint[args___] := If[QuiverGeometryPackageLoader`$Verbose, Print[args]];

readPackageFile[path_, context_] := Module[{cacheEntry, fileModTime, contents},
  {cachedModTime, cachedContents} = Lookup[$fileContentCache, path, {$Failed, $Failed}];
  fileModTime = UnixTime @ FileDate[path, "Modification"];
  isDirty = FailureQ[cachedContents] || cachedModTime =!= fileModTime;
  If[isDirty,
    zLVPrint["Reading \"" <> path <> "\""];
    contents = loadFileContents[path, context];
    $fileContentCache[path] = {fileModTime, contents};
  ,
    contents = cachedContents;
  ];
  {contents, isDirty}
];

loadFileContents[path_, context_] := Module[{str, contents}, Block[{$currentContext = context},
  $loadedFileCount++; QuiverGeometryPackageLoader`$CurrentFile = path;
  str = StringReplace[fileStringUTF8 @ path, $stringProcessingRules];
  If[MatchQ[str, Whitespace] || str === "", Return @ Package`PackageData[]];
  contents = TimeConstrained[Check[Package`ToPackageExpression @ str, $Failed], 1];
  If[Head[contents] =!= Package`PackageData, contents === $Failed];
  If[FailureQ[contents], handleSyntaxError[path, str]];
  Block[{$Context = context}, contents = contents /. $initialSymbolResolutionDispatch /. ResolvedSymbol[sym_] :> sym];
  contents
]];F

$stringProcessingRules = {
  "=>" -> "\[DirectedEdge]",
  "<=>" -> "\[UndirectedEdge]",
  RegularExpression["(?s)\"\"\"(.*?)\"\"\""] :>
    StringJoin["\"", StringReplace["$1", {"\\" -> "\\\\", "\"" -> "\\\""}], "\""],
  RegularExpression["(?s)ExpressionTable\\[\n(.*?)\n\\]"] :>
    StringJoin["QuiverGeometryLoader`ExpressionTable[\"", $currentContext, "\", \"", StringTrim @ StringReplace["$1", {"\\" -> "\\\\", "\"" -> "\\\""}], "\"]"],
(*   RegularExpression["(?s)(?<=\\s)`(.*?)`(?=\\s)"] :>
    StringJoin["\"", StringReplace["$1", {"\\" -> "\\\\", "\"" -> "\\\""}], "\""],
 *)
  RegularExpression[" ~!~ ([^\n]+)"] :> " ~NotMatchQ~ " <> bracketRHS["$1"],
  RegularExpression[" ~~~ ([^\n]+)"] :> " ~MatchQ~ " <> bracketRHS["$1"]
}

QuiverGeometryLoader`ExpressionTable[context_String, str_String] := Block[
  {stream = StringToStream[str], $ContextPath = Append[$globalImports, "QuiverGeometry`"], $Context = context},
  Replace[ReadList[stream, Hold[Expression]], Hold[Times[a___]] :> {a}, {1}]
];

(**************************************************************************************************)

bracketRHS[s_] := Block[{$Context = "QuiverGeometryPackageLoader`Scratch`", len},
  len = SyntaxLength[s];
  "(" <> StringInsert[s, ")", len+1]
];

If[!ValueQ[QuiverGeometryPackageLoader`$SystemOpenEnabled], QuiverGeometryPackageLoader`$SystemOpenEnabled = True];
DoSystemOpen[s_] := If[QuiverGeometryPackageLoader`$SystemOpenEnabled, SystemOpen[s]];

handleSyntaxError[path_, str_] := Scope[
  Print["Syntax error in ", path];
  tmpPath = FileNameJoin[{$TemporaryDirectory, "syntax_error_file.m"}];
  Export[tmpPath, str, "Text", CharacterEncoding -> "UTF8"];
  errors = TimeConstrained[GeneralUtilities`FindSyntaxErrors[tmpPath], 2, {}];
  errors = errors /. tmpPath -> path;
  Beep[];
  If[errors =!= {},
    Print["Aborting; syntax errors:"];
    Scan[Print, Take[errors, UpTo[5]]];
    DoSystemOpen @ Part[errors, 1, 1];
  ];
  failRead[];
];

initPathQ[path_] := StringContainsQ[FileNameTake @ path, "init.m" | ("A" ~~ DigitCharacter)];

filePathToContext[path_] := Block[{str, subContext, contextList},
  str = StringTrim[StringDrop[path, $mainPathLength], ("init.m" | "init.wl" | ".m" | ".wl") ~~ EndOfString];
  str = StringTrim[str, $PathnameSeparator];
  str = StringDelete[str, (WordBoundary ~~ "A"|"Z" ~~ DigitCharacter)];
  If[StringEndsQ[str, "Main"], str = StringDrop[str, -4]];
  contextList = Developer`ToList[$trimmedMainContext, FileNameSplit @ str];
  StringJoin[{#, "`"}& /@ contextList]
];

toSymbolReplacementRule[name_, ResolvedSymbol[sym_]] :=
  Package`PackageSymbol[name] :> sym;

(* not sure why, but Complement::heads is sometimes issued for context QuiverGeometry`PackageScope` *)
createSymbolsInContextAndDispatchTable[names_, context_, contextPath_] := Block[
  {$Context = context, $ContextPath = contextPath, rules, resolvedSyms},
  resolvedSyms = ToExpression[names, InputForm, ResolvedSymbol];
  rules = MapThread[toSymbolReplacementRule, {names, resolvedSyms}];
  Dispatch @ rules
];

addPackageSymbolsToBag[bag_, expr_, head_] := (
  Internal`StuffBag[bag, Cases[expr, e:head[Package`PackageSymbol[_String]..] :> Part[List @@ e, All, 1], {2}], 2];
  Cases[expr, splice_Package`PackageSplice :> addPackageSymbolsToBag[bag, splice, head], {2}];
  (* ^= pick up PublicTypesettingFormBox etc *)
);

addPackageCasesToBag[bag_, expr_, rule_] :=
  Internal`StuffBag[bag, Cases[expr, rule, {2}], 1];

resolveRemainingSymbols[{path_, context_, packageData_Package`PackageData, _}] := Scope[
  unresolvedNames = DeepUniqueCases[packageData, Package`PackageSymbol[name_] :> name];
  dispatch = createSymbolsInContextAndDispatchTable[unresolvedNames, context, $globalImports];
  {path, context, packageData /. dispatch}
];

fileSortingTuple[path_] := {
    StringFreeQ[path, "A0Init" | "A0Utilities"],
    Which[StringEndsQ[path, "init.m"], -1, StringEndsQ[path, "final.m"], 1, True, 0],
    path
  };

(* because $ContextPath is not allowed to be empty -- this results in weird internal messages *)
$dummyContextPath = {"QuiverGeometryLoader`DummyContext`"};

QuiverGeometryPackageLoader`ReadPackages[mainContext_, mainPath_, cachingEnabled_:True, fullReload_:True] := Block[
  {$directory, $files, $textFiles, $privateSymbols, $systemSymbols, $publicSymbols, $packageExpressions, $packageRules,
   $mainContext, $trimmedMainContext, $mainPathLength, $exportRules, $scopeRules, result, requiresFullReload,
   $preservedValues, $preservedDownValues, $ignoreFiles
  },

  Off[General::shdw]; (* because things like SetUsage, SetAutomatic, etc have QG local definitions *)

  $directory = AbsoluteFileName @ ExpandFileName @ mainPath;
  $mainContext = mainContext;
  $mainPathLength = StringLength[$directory];
  $trimmedMainContext = StringTrim[mainContext, "`"];

  $filesToSkip = FileNames[{"Loader.m", "init.m", "*.old.m", "SymbolTable.m"}, $directory];
  $ignoreFiles = FileNames["user_ignore.m", $directory];

  If[Length[$ignoreFiles] === 1,
    $ignoreFiles = StringTrim @ StringSplit[ReadString @ First @ $ignoreFiles, "\n"];
    $filesToSkip = Join[$filesToSkip, FileNames[$ignoreFiles, $directory, Infinity]];
  ];

  $userFiles = FileNames["user_*.m", $directory];
  $files = Sort @ Complement[FileNames["*.m", $directory, Infinity], Join[$filesToSkip, $userFiles]];
  $files = SortBy[$files, fileSortingTuple];

  $textFiles = FileNames[{"*.txt", "*.tex"}, $directory, Infinity];

  $globalImports = {"System`", "GeneralUtilities`"};

  $systemSymbols = Internal`Bag[];
  $publicSymbols = Internal`Bag[];
  $privateSymbols = Internal`Bag[];
  $loadedFileCount = $changedTextFileCount = 0;

  dirtyCount = 0;
  requiresFullReload = fullReload;
  result = Catch[
    $packageExpressions = Map[
      path |-> Block[{expr, context, isDirty},
        context = filePathToContext @ path;
        {expr, isDirty} = readPackageFile[path, context];
        If[isDirty, dirtyCount++];
        addPackageSymbolsToBag[$systemSymbols,  expr, $systemPackageDeclarationHeadP];
        addPackageSymbolsToBag[$publicSymbols,  expr, $publicPackageDeclarationHeadP];
        addPackageSymbolsToBag[$privateSymbols, expr, $privatePackageDeclarationHeadP];
        If[!requiresFullReload && isDirty && initPathQ[path],
          LVPrint["Dirty package \"", path, "\" is forcing a full reload."];
          requiresFullReload = True;
        ];
        {path, context, expr, isDirty}
      ],
      $files
    ];
  ,
    failRead
  ];
  
  Quiet @ Remove["QuiverGeometryPackageLoader`Scratch`*"]; (* <- dumping ground for SyntaxLength *)
  If[result === $Failed,
    LVPrint["Reading failed."];
    Return[$Failed]];

  Scan[observeTextFile, $textFiles];
  Scan[observeTextFile, $userFiles];

  If[!requiresFullReload,
    $packageExpressions = DeleteCases[$packageExpressions, {_, _, _, False}];
  ];

  If[cachingEnabled && ($loadedFileCount == 0 || $packageExpressions === {}) && $changedTextFileCount == 0,
    LVPrint["No contents changed, skipping evaluation."];
    Return[{}];
  ];

  $preservedValues = Replace[
    Keys @ QuiverGeometryPackageLoader`$PreservedVariables,
    Hold[sym_] :> With[{val = sym}, Hold[sym = val]],
    {1}
  ];

  (* for things like RedBox that would otherwise we wiped out due to form definition caching mechanism *)
  $preservedDownValues = Replace[
    Keys @ QuiverGeometryPackageLoader`$PreservedFunctions,
    Hold[sym_] :> With[{dv = DownValues[sym]}, Hold[DownValues[sym] = dv]],
    {1}
  ];

  If[requiresFullReload,
    LVPrint["Clearing all symbols."];
    Construct[ClearAll, mainContext <> "*", mainContext <> "**`*"];
  ,
    dirtyContexts = Part[$packageExpressions, All, 2];
    LVPrint["Clearing dirty contexts: ", dirtyContexts];
    Apply[ClearAll, Map[# <> "*"&, dirtyContexts]];
  ];

  LVPrint["Copying preserved values."];
  ReleaseHold[$preservedValues];
  ReleaseHold[$preservedDownValues];

  $systemSymbols = DeleteDuplicates @ Internal`BagPart[$systemSymbols, All];
  $publicSymbols = DeleteDuplicates @ Internal`BagPart[$publicSymbols, All];
  $privateSymbols = DeleteDuplicates @ Internal`BagPart[$privateSymbols, All];

  LVPrint["Creating symbols (", Length @ $systemSymbols, " system, ", Length @ $publicSymbols, " public, ", Length @ $privateSymbols, " private)."];
  $systemDispatch = createSymbolsInContextAndDispatchTable[$systemSymbols, "System`", $dummyContextPath];
  $publicDispatch = createSymbolsInContextAndDispatchTable[$publicSymbols, $mainContext, $dummyContextPath];
  $privateDispatch = createSymbolsInContextAndDispatchTable[$privateSymbols, $mainContext <> "PackageScope`", $dummyContextPath];

  LVPrint["Recognizing symbols."];
  $packageExpressions = $packageExpressions /. $systemDispatch /. $publicDispatch /. $privateDispatch;

  LVPrint["Resolving unrecognized symbols."];
  $packageExpressions //= Map[resolveRemainingSymbols];

  On[General::shdw];

  LVPrint["Finding suspicious lines."];
  QuiverGeometryPackageLoader`$SuspiciousPackageLines = findSuspiciousPackageLines[$packageExpressions];

  LVPrint["Read ", Length[$packageExpressions], " packages."];
  $packageExpressions
];

QuiverGeometryPackageLoader`$SuspiciousPackageLines = {};

$badControlStatementPatterns = Alternatives[
  w_Switch /; EvenQ[Length[Unevaluated @ w]],
  w_Which /; OddQ[Length[Unevaluated @ w]]
];

positionToFileLine[{fileNum_, 3, line_, rest___}] :=
  Part[$packageExpressions, fileNum, 3, line, rest, 0] -> FileLine[Part[$packageExpressions, fileNum, 1], Part[$packageExpressions, fileNum, 3, line, 1]];

positionToFileLine[_, _] := Nothing;

findSuspiciousPackageLines[pdata_] :=
  positionToFileLine /@ Position[$packageExpressions, $badControlStatementPatterns];

QuiverGeometryPackageLoader`EvaluatePackages[packagesList_List] := Block[
  {$currentPath, $currentLineNumber, $formsChanged, result, initialFile, finalFile},
  $currentPath = ""; $currentLineNumber = 0;
  QuiverGeometryPackageLoader`$FileTimings = <||>;
  QuiverGeometryPackageLoader`$FileLineTimings  = <||>;
  $formsChanged = $failEval = False;
  LVPrint["Evaluating packages."];
  loadUserFile["user_init.m"];
  result = GeneralUtilities`WithMessageHandler[
    Scan[evaluatePackage, packagesList],
    handleMessage
  ];
  If[$failEval, Return[$Failed, Block]];
  If[userFileChangedQ["user_final.m"], loadUserFile["user_final.m"]];
  If[(Length[packagesList] > 10) || formsChanged || userFileChangedQ["user_shortcuts.m"], loadUserFile["user_shortcuts.m"]];
  result
];

QuiverGeometryPackageLoader`DirectoryTimings[] := Block[{pathLen},
  pathLen = StringLength @ AbsoluteFileName @ ExpandFileName @ QuiverGeometryPackageLoader`$SourceDirectory;
  fileTimings = Normal @ QuiverGeometryPackageLoader`$FileTimings;
  groupTimings = MapAt[First @ StringSplit[StringDrop[#, pathLen], $PathnameSeparator]&, fileTimings, {All, 1}];
  ReverseSort @ Merge[groupTimings, Total]
];

If[!AssociationQ[$userFileModTimes], $userFileModTimes = Association[]];

userFileChangedQ[name_] := Block[{path, modTime, lastModTime},
  path = toUserFilePath @ name;
  If[!FileExistsQ[path], Return[False]];
  modTime = UnixTime @ FileDate[path, "Modification"];
  lastModTime = Lookup[$userFileModTimes, path, 0];
  $userFileModTimes[path] = modTime;
  lastModTime != modTime
];

QuiverGeometryPackageLoader`ReloadUserFile[] := QuiverGeometryPackageLoader`ReloadUserFile["user_final.m"];

QuiverGeometryPackageLoader`ReloadUserFile[name_] := loadUserFile @ name;
  
$userContext = "QuiverGeometryPackageLoader`Private`User`";
$userContextPath = {"System`", "GeneralUtilities`", "QuiverGeometry`", "QuiverGeometry`PackageScope`"};
$fileContextPath = {"System`", "QuiverGeometry`", "QuiverGeometry`PackageScope`"};

toUserFilePath[name_] := FileNameJoin[{QuiverGeometryPackageLoader`$SourceDirectory, name}];

loadUserFile[name_] := Block[{path},
  path = toUserFilePath[name];
  If[!FileExistsQ[path], Return[]];
  Block[{$Context = $userContext, $ContextPath = $userContextPath},
    LVPrint["Loading \"", name, "\""];
    Get @ path;
  ];
];

SetAttributes[evaluateExpression, HoldAllComplete];

MakeBoxes[pd_Package`PackageData, StandardForm] :=
  RowBox[{"PackageData", StyleBox[RowBox[{"[", Length[pd], "]"}], Background -> LightRed]}];

evaluatePackage[{path_, context_, packageData_Package`PackageData}] := Catch[
  $currentPath = path; $currentFileLineTimings = <||>; $failCount = 0;
  If[$failEval, Return[$Failed, Catch]];
  LVPrint["Evaluating \"", path, "\""];
  $formsChanged = Or[$formsChanged, StringContainsQ[context, "`Typesetting`Forms`"]]; (* to avoid expensive symbol enum *)
  QuiverGeometryPackageLoader`$FileTimings[path] = First @ AbsoluteTiming[
    Block[{$Context = context, $ContextPath = $fileContextPath}, Catch[Scan[evaluateExpression, packageData], $evaluateExpressionTag]];
  ];
  QuiverGeometryPackageLoader`$FileLineTimings[path] = $currentFileLineTimings;
,
  MacroEvaluate, catchMacroFailure
];

evaluatePackage[spec_] := (
  LVPrint["Invalid package data."];
  LVPrint[Shallow @ spec];
  Abort[];
);

MacroEvaluate::macrofail = "Macro failed.";

catchMacroFailure[$Failed, _] := handleMessage @
  Failure["MacroEvaluate", <|"MessageTemplate" :> MacroEvaluate::macrofail, "MessageParameters" -> {}|>];

catchMacroFailure[f_Failure, _] := handleMessage @ f;

evaluateExpression[{lineNumber_, expr_}] := If[$failEval, $Failed,
  $currentLineNumber = lineNumber;
  $currentFileLineTimings[lineNumber] = First @ AbsoluteTiming[{expr}];
];

handleMessage[f_Failure] := Block[{fileLine},
  $failEval = True;
  If[$failCount++ > 5, Print["Emergency abort!"]; Abort[]];
  (* ^ this is an emergency measure: it shouldn't happen but when we do get a long list of errors the
  OS can lock up for a while *)
  Beep[];
  fileLine = GeneralUtilities`FileLine[$currentPath, $currentLineNumber];
  Print["Aborting; message ", HoldForm @@ f["HeldMessageTemplate"], " occurred at ", fileLine];
  Print[FailureString @ f];
  Throw[$Failed, $evaluateExpressionTag];
  DoSystemOpen[fileLine];

];

(*************************************************************************************************)

$lastLoadSuccessful = False;

QuiverGeometryPackageLoader`Read[cachingEnabled_:True, fullReload_:True] :=
  QuiverGeometryPackageLoader`ReadPackages["QuiverGeometry`", QuiverGeometryPackageLoader`$SourceDirectory, cachingEnabled, fullReload];

QuiverGeometryPackageLoader`Load[fullReload_:True, fullRead_:False] := Block[
  {$AllowInternet = False, URLSubmit = Print["URLSubmit[", Row[{##}, " "], "]"]&},
  FinishDynamic[];
  Block[{packages},
  packages = QuiverGeometryPackageLoader`Read[!fullRead, fullReload];
  If[FailureQ[packages], ReturnFailed[]];
  QuiverGeometryPackageLoader`$LoadCount++;
  If[!FailureQ[QuiverGeometryPackageLoader`EvaluatePackages @ packages],
    $lastLoadSuccessful = True];
  ]
];

QuiverGeometryPackageLoader`$LoadCount = 0;

selfModTime[] := UnixTime @ FileDate[QuiverGeometryPackageLoader`$LoaderFileName, "Modification"];

QuiverGeometryPackageLoader`$SelfModTime = selfModTime[];

QuiverGeometryPackageLoader`NeedsSelfLoad[] := selfModTime[] =!= QuiverGeometryPackageLoader`$SelfModTime;

End[];
