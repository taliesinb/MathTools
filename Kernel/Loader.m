BeginPackage["QuiverGeometryLoader`", {"GeneralUtilities`"}];

$PublicContext = "QuiverGeometry`";
$PrivateContext = "QuiverGeometry`Private`";
$CacheContext = "QuiverGeometryCaches`";
$LoaderContext = "QuiverGeometryLoader`";
$ShortcutsContext = "QuiverGeometryShortcuts`";

$LoaderFileName = $InputFileName;
$SourceDirectory = DirectoryName @ $InputFileName;
$PackageDirectory = ParentDirectory @ $SourceDirectory;
$SymbolTable = None;
$CurrentFile = None;
$CurrentFileName = None;

DeclarePreservedVariable
DeclarePreservedFunction
$PreservedVariables = Data`UnorderedAssociation[];
$PreservedFunctions = Data`UnorderedAssociation[];

$LoadCount = 0;
$Verbose
$SymbolAliases
$SymbolGroups
$SystemOpenEnabled
$SuspiciousPackageLines
$AttachSyntaxInformation

LoadSource
ReadSource
NeedsSelfLoad

FileTimings
FileLineTimings
ExpensiveFileTimings
ExpensiveFileLineTimings
DirectoryTimings

ExpressionTable
ResolvedSymbol

(*************************************************************************************************)

Begin["`Private`"];

LVPrint[args___] := If[$Verbose, Print[args]];

LVPrint["Prelude."];

(* the first few symbols cause expensive packages to load, and appear as options to Style. RowLabels is put in System by GraphicsGrid. *)
Unprotect[System`EdgeThickness, System`EpilogFunction, System`LLMEvaluator, System`LLMEvaluatorNames, System`RowLabels, System`ColumnLabels, System`EdgeOpacity];
ClearAll[System`EdgeThickness, System`EpilogFunction, System`LLMEvaluator, System`LLMEvaluatorNames, System`RowLabels, System`ColumnLabels, System`EdgeOpacity];
Quiet[Style; Options[Style]];

(* moved from A0Usage.m because this runs slowly:
the default behavior of System`InformationDump` will introduce LineSpacing that messes up
my SetUsage inline tables, so remove it. *)

dummy::usage = "Dummy";
ToBoxes[Information[dummy]];
System`InformationDump`subtitleStyled[sub_] := Style[sub, "InformationUsageText"];

$baseContextPath = {"System`", "GeneralUtilities`", $PublicContext, $PrivateContext};

(*************************************************************************************************)

Attributes[DeclarePreservedVariable] = {HoldAll};
DeclarePreservedVariable[var_] := If[
  StringStartsQ[Context @ var, $PublicContext],
  AssociateTo[$PreservedVariables, Hold[var] -> True]
];

Attributes[DeclarePreservedFunction] = {HoldAll};
DeclarePreservedFunction[fn_] :=
  AssociateTo[$PreservedFunctions, Hold[fn] -> True];

(*************************************************************************************************)

(* we set $CurrentFileName where necessary so TraceLoading will say the right location *)
GeneralUtilities`$CurrentFileName = $LoaderFileName;

(* Fix GU's ability to open files in subl on MacOS *)
If[$OperatingSystem === "MacOSX",
  GeneralUtilities`Packages`PackagePrivate`$sublimePath = "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl";
];

LVPrint["Reading core symbols from SymbolTable.m."];

(* we will immediately resolve these system symbols, which will take care of the vast majority of Package`PackageSymbol cases *)
$SymbolTable = Get @ FileNameJoin[{$SourceDirectory, "SymbolTable.m"}];
$SymbolAliases = Lookup[$SymbolTable, "SymbolAliases"];
$SymbolTable = DeleteCases[$SymbolTable, "SymbolAliases" -> _];

$coreSymbols = DeleteCases[_Hold] @ Catenate @ Values @ $SymbolTable;

$SymbolTable = Association @ $SymbolTable;

$coreSymbols = Sort @ DeleteDuplicates @ $coreSymbols;

$coreSymbolNames = SymbolName /@ $coreSymbols;
$coreSymbolContexts = Context /@ $coreSymbols;

$corePackageSymbols = Pick[$coreSymbols, $coreSymbolContexts, "Package`"];
$corePackageSymbols //= DeleteCases[Package`PublicScopedOption | Package`PublicTypesettingFormBox];
$corePackageSymbolNames = SymbolName /@ $corePackageSymbols;
$corePackageSymbolGroups = StringTrim[$corePackageSymbolNames, {"System", "Public", "Private"}];
$legacyPackageDirs = {"Package", "PackageExport", "PackageImport", "PackageScope"};

(* rule that will scan all packages and add symbols to their groups,  headPattern_[sym_] -> (group -> sym) *)
toAlt[{a_}] := a;
toAlt[a_List] := Alternatives @@ a;
$symbolGroupRules = KeyValueMap[
  #2[z___] :> (#1 -> extractStrings[z])&,
  KeyDrop[$legacyPackageDirs] @
    Merge[Thread[$corePackageSymbolGroups -> $corePackageSymbols], toAlt]
];

extractStrings[z___] := DeepCases[{z}, _String];

(* association from symbol group, like Symbol, TypesettingForm, etc to list of these symbols.
we don't care about System/Private/Public at all *)
$SymbolGroups = $initSymGroups = Association["Package" -> $corePackageSymbolNames];

makePackageSymbolHeadP[s_] := Apply[Alternatives, Pick[$corePackageSymbols, StringStartsQ[$corePackageSymbolNames, s]]];
$systemPackageDeclarationHeadP = makePackageSymbolHeadP["System"];
$publicPackageDeclarationHeadP = makePackageSymbolHeadP["Public"];
$privatePackageDeclarationHeadP = makePackageSymbolHeadP["Private"];
$cachePackageDeclarationHeadP = makePackageSymbolHeadP["Cache"];

toRegexPattern["$Failed"] := "(\\$Failed)";
toRegexPattern[str_] := "(" <> str <> ")";

LVPrint["Creating core symbols regexp."];
$coreSymbolRegex = RegularExpression @ StringJoin @ Riffle[Map[toRegexPattern, Sort @ $coreSymbolNames], "|"];

$coreSymbolAssociation = AssociationThread[$coreSymbolNames, $coreSymbols];

GeneralUtilities`$CurrentFileName =.;

(*************************************************************************************************)

SetAttributes[ResolvedSymbol, HoldAllComplete];
makeResolvedSymbol[name_String] := ToExpression[name, InputForm, ResolvedSymbol];

$lowerCaseSymbolRegex = RegularExpression["[$]?[a-z]"];

$initialSymbolResolutionDispatch = With[
  {kernelInit = FileNameJoin[{$PackageDirectory, "Kernel", "init.m"}]},
  Dispatch[{
  (* Package`PackageSymbol[name_String] /; StringStartsQ[name, $lowerCaseSymbolRegex] :> RuleCondition[makeResolvedSymbol[name]], *)
  Package`PackageSymbol["SetUsage"][usageString_String]                      :> RuleCondition[rewriteSetUsage[usageString]],
  Package`PackageSymbol[name_String] /; StringMatchQ[name, $coreSymbolRegex] :> RuleCondition[$coreSymbolAssociation[name]],
  Package`PackageSymbol[name_String] /; StringContainsQ[name, "`"]           :> RuleCondition[makeResolvedSymbol[name]],
  Splice @ MapApply[Package`PackageSymbol[#1] -> #2&, $SymbolAliases],
  Package`PackageSymbol["$PackageFileName"]                                  :> RuleCondition[$CurrentFile],
  Package`PackageSymbol["$PackageDirectory"]                                 :> RuleCondition[$PackageDirectory],
  Package`PackageSymbol["$PackageInitializer"]                               :> If[DownValues[QuiverGeometryLoader`LoadSource] === {}, Get @ kernelInit],
  p:Package`PackageSymbol["PublicScopedOption"][___]                         :> RuleCondition @ processScopedOption[p],
  p:Package`PackageSymbol["PublicTypesettingFormBox"][___]                   :> RuleCondition @ processTypesettingFormBox[p]
  }]
];

(* turns PublicScopedOption[Foo] into PublicOption[Foo], PrivateVariable[$foo] *)
processScopedOption[p_] :=
   Package`PackageSplice @@ Cases[p, Package`PackageSymbol[name_String] :> {
    Package`PublicOption[Package`PackageSymbol @ name],
    Package`PrivateVariable[Package`PackageSymbol @ StringJoin["$", ToLowerCase @ StringTake[name, 1], StringDrop[name, 1]]]
  }];

(* turns PublicTypesettingFormBox[AppliedForm] into PublicTypesettingForm[AppliedForm], PrivateTypesettingBoxFunction[AppliedBox] *)
processTypesettingFormBox[p_] :=
   Package`PackageSplice @@ Cases[p, Package`PackageSymbol[name_String] :> {
    Package`PublicTypesettingForm[Package`PackageSymbol[name]],
    If[!StringEndsQ[name, "Form"], Print["ERROR: PublicTypesettingFormBox ", name]];
    Package`PrivateTypesettingBoxFunction[Package`PackageSymbol[StringDrop[name, -4] <> "Box"]]
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

(**************************************************************************************************)

readPackageFile[path_, context_] := Module[{cacheEntry, fileModTime, contents},
  {cachedModTime, cachedContents} = Lookup[$fileContentCache, path, {$Failed, $Failed}];
  fileModTime = UnixTime @ FileDate[path, "Modification"];
  isDirty = FailureQ[cachedContents] || cachedModTime =!= fileModTime;
  If[isDirty,
    LVPrint["Reading \"" <> path <> "\""];
    contents = loadFileContents[path, context];
    $fileContentCache[path] = {fileModTime, contents};
  ,
    contents = cachedContents;
  ];
  {contents, isDirty}
];

(**************************************************************************************************)

loadFileContents[path_, context_] := Module[{str, contents}, Block[{$currentContext = context},
  $loadedFileCount++; $CurrentFile = path;
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
  RegularExpression[" ~!~ ([^\n;&]+)"] :> " ~NotMatchQ~ " <> bracketRHS["$1"],
  RegularExpression[" ~~~ ([^\n;&]+)"] :> " ~MatchQ~ " <> bracketRHS["$1"]
}

bracketRHS[s_] := bracketRHS[s] = Block[{$Context = "QuiverGeometryLoader`Scratch`", len},
  len = SyntaxLength @ StringDelete[s, "||" ~~ ___ ~~ EndOfString];
  "(" <> StringInsert[s, ")", len+1]
];

If[!ValueQ[$SystemOpenEnabled], $SystemOpenEnabled = True];
DoSystemOpen[s_] := If[$SystemOpenEnabled, SystemOpen[s]];

handleSyntaxError[path_, str_] := Scope[
  Print["Syntax error in ", path];
  tmpPath = FileNameJoin[{$TemporaryDirectory, "syntax_error_file.m"}];
  Export[tmpPath, str, "Text", CharacterEncoding -> "UTF8"];
  errors = TimeConstrained[FindSyntaxErrors[tmpPath], 2, {}];
  errors = errors /. tmpPath -> path;
  Beep[];
  If[errors =!= {},
    Print["Aborting; syntax errors:"];
    Scan[Print, Take[errors, UpTo[5]]];
    DoSystemOpen @ Part[errors, 1, 1];
  ];
  failRead[];
];

(**************************************************************************************************)

(* this won't handle aliases and the like, so eventually we should migrate this to use
full package parsing *)
ExpressionTable[context_String, str_String] := Block[
  {stream = StringToStream[str], $ContextPath = $baseContextPath, $Context = context, result},
  result = Replace[ReadList[stream, Hold[Expression]], Hold[Times[a___]] :> {a}, {1}];
  Close[stream];
  result
];

(**************************************************************************************************)

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

(* because $ContextPath is not allowed to be empty -- this results in weird internal messages.
also, we need System` in there because otherwise General` shows up in other contexts, must
be some quited message that gets generated *)
$dummyContextPath = {"QuiverGeometryLoader`DummyContext`"};

(* not sure why, but Complement::heads is sometimes issued for context QuiverGeometry`Private` *)
createSymbolsInContextAndDispatchTable[names_, context_, contextPath_] := Block[
  {$Context = context, $ContextPath = contextPath, $NewSymbol, rules, resolvedSyms},
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

ReadSource[cachingEnabled_:True, fullReload_:True] := Block[
  {$directory, $files, $textFiles, $packageExpressions,
   $privateSymbols, $systemSymbols, $publicSymbols, $cacheSymbols,
   $trimmedMainContext, $mainPathLength, $exportRules, $scopeRules, result, requiresFullReload,
   $preservedValues, $preservedDownValues, $ignoreFiles, $symGroups,
   GeneralUtilities`$CurrentFileName = $LoaderFileName
  },

  Off[General::shdw]; (* because things like SetUsage, SetAutomatic, etc have QG local definitions *)

  $symGroups = If[fullReload, $initSymGroups, $SymbolGroups];
  $directory = AbsoluteFileName @ ExpandFileName @ $SourceDirectory;
  $mainPathLength = StringLength[$SourceDirectory];
  $trimmedMainContext = StringTrim[$PublicContext, "`"];

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
  $cacheSymbols = Internal`Bag[];
  $loadedFileCount = $changedTextFileCount = 0;

  dirtyCount = 0;
  requiresFullReload = fullReload;
  result = Catch[
    $packageExpressions = Map[
      path |-> Block[{expr, context, isDirty, GeneralUtilities`$CurrentFileName = path},
        context = filePathToContext @ path;
        {expr, isDirty} = readPackageFile[path, context];
        If[isDirty, dirtyCount++];
        addPackageSymbolsToBag[$systemSymbols,  expr, $systemPackageDeclarationHeadP];
        addPackageSymbolsToBag[$publicSymbols,  expr, $publicPackageDeclarationHeadP];
        addPackageSymbolsToBag[$privateSymbols, expr, $privatePackageDeclarationHeadP];
        addPackageSymbolsToBag[$cacheSymbols,   expr, $cachePackageDeclarationHeadP];
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

  If[dirtyCount > 0,
    LVPrint["Populating symbol group table."];
    $symGroups = Union /@ Merge[{
      $symGroups,
      DeleteDuplicates @ Merge[
        Map[rule |-> DeepCases[$packageExpressions, rule], $symbolGroupRules],
        Apply[Union]
      ]},
      Apply[Union]
    ];
    $SymbolGroups = $symGroups;
  ];

  Quiet @ Remove["QuiverGeometryLoader`Scratch`*"]; (* <- dumping ground for SyntaxLength *)
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

  LVPrint["Updating preserved values."];
  $preservedValues = Replace[
    Keys @ $PreservedVariables,
    Hold[sym_] :> If[System`Private`HasImmediateValueQ[sym],
      With[{val = sym}, Hold[sym = val]],
      With[{ov = OwnValues[sym]}, Hold[OwnValues[sym] = ov]]
    ],
    {1}
  ];

  LVPrint["Updating preserved functions."];
  (* for things like RedBox that would otherwise be wiped out due to form definition caching mechanism *)
  $preservedDownValues = Replace[
    Keys @ $PreservedFunctions,
    Hold[sym_] :> With[{dv = DownValues[sym]}, Hold[DownValues[sym] = dv]],
    {1}
  ];

  If[requiresFullReload,
    LVPrint["Clearing all symbols."];
    Construct[ClearAll, $PublicContext <> "*", $PublicContext <> "**`*"];
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
  $cacheSymbols = DeleteDuplicates @ Internal`BagPart[$cacheSymbols, All];

  LVPrint["Creating symbols (", Length @ $systemSymbols, " system, ", Length @ $publicSymbols, " public, ", Length @ $privateSymbols, " private)."];
  $systemDispatch = createSymbolsInContextAndDispatchTable[$systemSymbols, "System`", $dummyContextPath];
  $publicDispatch = createSymbolsInContextAndDispatchTable[$publicSymbols, $PublicContext, $dummyContextPath];
  $privateDispatch = createSymbolsInContextAndDispatchTable[$privateSymbols, $PrivateContext, $dummyContextPath];
  $cacheDispatch = createSymbolsInContextAndDispatchTable[$cacheSymbols, $CacheContext, $dummyContextPath];
  (* this probably happens because of a Quieted message somewhere during symbol creation *)
  If[NameQ[$PrivateContext <> "General"], Remove[$PrivateContext <> "General"]];

  LVPrint["Initializing cache symbols."];
  Scan[initCacheSymbol, Normal @ $cacheDispatch];

  LVPrint["Recognizing symbols."];
  (* TODO: unify these Dispatch heads for speed *)
  $packageExpressions = $packageExpressions /. $systemDispatch /. $publicDispatch /. $privateDispatch /. $cacheDispatch;

  LVPrint["Resolving unrecognized symbols."];
  $packageExpressions //= Map[resolveRemainingSymbols];

  On[General::shdw];

  LVPrint["Finding suspicious lines."];
  $SuspiciousPackageLines = findSuspiciousPackageLines[$packageExpressions];

  LVPrint["Read ", Length[$packageExpressions], " packages."];
  $packageExpressions
];

(*************************************************************************************************)

initCacheSymbol[_ :> sym_] :=
  If[!System`Private`HasImmediateValueQ[sym], sym = Data`UnorderedAssociation[]];

(*************************************************************************************************)

MakeBoxes[pd_Package`PackageData, StandardForm] :=
  RowBox[{"PackageData", StyleBox[RowBox[{"[", Length[pd], "]"}], Background -> LightRed]}];

(*************************************************************************************************)

$SuspiciousPackageLines = {};

$badControlStatementPatterns = Alternatives[
  w_Switch /; EvenQ[Length[Unevaluated @ w]],
  w_Which /; OddQ[Length[Unevaluated @ w]]
];

positionToFileLine[{fileNum_, 3, line_, rest___}] :=
  Part[$packageExpressions, fileNum, 3, line, rest, 0] -> FileLine[Part[$packageExpressions, fileNum, 1], Part[$packageExpressions, fileNum, 3, line, 1]];

positionToFileLine[_, _] := Nothing;

findSuspiciousPackageLines[pdata_] :=
  positionToFileLine /@ Position[$packageExpressions, $badControlStatementPatterns];

(*************************************************************************************************)

evaluatePackageData[packagesList_List] := Block[
  {$currentPath, $currentLineNumber, $formsChanged, $failEval,
    result, initialFile, finalFile, extraContexts,
   GeneralUtilities`$CurrentFileName = $LoaderFileName, $Line = 0},
  $currentPath = ""; $currentLineNumber = 0;
  $formsChanged = $failEval = False;
  $fileTimings = $fileLineTimings = Association[];
  LVPrint["Evaluating packages."];
  loadUserFile["user_init.m"];
  result = WithMessageHandler[
    Scan[evaluatePackage, packagesList],
    handleMessage
  ];
  If[!MemberQ[$ContextPath, $PublicContext], AppendTo[$ContextPath, $PublicContext]];
  If[$failEval, Return[$Failed, Block]];
  If[userFileChangedQ["user_final.m"],
    extraContexts = loadUserFile["user_final.m"];
    If[VectorQ[extraContexts, StringQ],
      LVPrint["Adding additional contexts: ", extraContexts];
      $ContextPath = Join[$ContextPath, Complement[extraContexts, $ContextPath]]
    ];
  ];
  result
];

(*************************************************************************************************)


FileTimings[] :=
  trimFiles @ ReverseSort @ $fileTimings;

FileLineTimings[] :=
  assocToFileLines[ReverseSortBy[Total] @ $fileLineTimings];

ExpensiveFileTimings[ms_:5] :=
  trimFiles @ ReverseSort @ Select[GreaterThan[ms]] @ $fileTimings;

ExpensiveFileLineTimings[ms_:5] :=
  assocToFileLines[DeleteCases[<||>] @ Map[Select[GreaterThan[ms]]] @ $fileLineTimings];

trimFiles[assoc_] := Block[{pathLen},
  pathLen = StringLength @ AbsoluteFileName @ ExpandFileName @ $SourceDirectory;
  KeyMap[StringDrop[#, pathLen + 1]&, assoc]
];

assocToFileLines[files_] :=
  Flatten @ KeyValueMap[
    {file, lines} |-> KeyValueMap[
      {line, time} |-> (FileLine[file, line] -> time),
      lines
    ],
    files
  ];

DirectoryTimings[] := Block[
  {pathLen, fileTimings, groupTimings},
  pathLen = StringLength @ AbsoluteFileName @ ExpandFileName @ $SourceDirectory;
  fileTimings = Normal @ $fileTimings;
  groupTimings = MapAt[First @ StringSplit[StringDrop[#, pathLen], $PathnameSeparator]&, fileTimings, {All, 1}];
  ReverseSort @ Merge[groupTimings, Total]
];

(*************************************************************************************************)

If[!AssociationQ[$userFileModTimes], $userFileModTimes = Association[]];

userFileChangedQ[name_] := Block[
  {path, modTime, lastModTime},
  path = toUserFilePath @ name;
  If[!FileExistsQ[path], Return[False]];
  modTime = UnixTime @ FileDate[path, "Modification"];
  lastModTime = Lookup[$userFileModTimes, path, 0];
  $userFileModTimes[path] = modTime;
  lastModTime != modTime
];

toUserFilePath[name_] := FileNameJoin[{$SourceDirectory, name}];

$userFileContext = "QuiverGeometryLoader`Private`User`";
$userFileContextPath = Append[$baseContextPath, $LoaderContext];

loadUserFile[name_] := Block[{path, result},
  path = toUserFilePath[name];
  If[!FileExistsQ[path], Return[]];
  Block[{$Context = $userFileContext, $ContextPath = $userFileContextPath},
    LVPrint["Loading \"", name, "\""];
    Get @ path
  ]
];

(*************************************************************************************************)

Attributes[msTiming] = {HoldAllComplete};
msTiming[e_] := 1000 * First[AbsoluteTiming @ e];

(* this mostly has no effect, but can change dynamic symbol resolution of ToExpression, Symbol, etc.
this doesn't include GU because we shadow some GU symbols *)
$packageFileContextPath = {"System`", $PublicContext, $PrivateContext};

evaluatePackage[{path_, context_, packageData_Package`PackageData}] := Catch[
  $currentPath = path; $currentFileLineTimings = <||>; $failCount = 0;
  If[$failEval, Return[$Failed, Catch]];
  LVPrint["Evaluating \"", path, "\""];
  $formsChanged = Or[$formsChanged, StringContainsQ[context, "`Typesetting`Forms`"]]; (* to avoid expensive symbol enum *)
  $fileTimings[path] = msTiming[
    Block[{$Context = context, $ContextPath = $packageFileContextPath, GeneralUtilities`$CurrentFileName = path},
      Catch[Scan[evaluateExpression, packageData], $evaluateExpressionTag]
    ];
  ];
  $fileLineTimings[path] = $currentFileLineTimings;
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

SetAttributes[evaluateExpression, HoldAllComplete];

evaluateExpression[{lineNumber_, expr_}] := If[$failEval, $Failed,
  $Line = $currentLineNumber = lineNumber;
  $currentFileLineTimings[lineNumber] = msTiming[{expr}];
];

(*************************************************************************************************)

handleMessage[f_Failure] := Block[{fileLine},
  $failEval = True;
  If[$failCount++ > 5, Print["Emergency abort!"]; Abort[]];
  (* ^ this is an emergency measure: it shouldn't happen but when we do get a long list of errors the
  OS can lock up for a while *)
  Beep[];
  fileLine = FileLine[$currentPath, $currentLineNumber];
  Print["Aborting; message ", HoldForm @@ f["HeldMessageTemplate"], " occurred at ", fileLine];
  Print[FailureString @ f];
  Throw[$Failed, $evaluateExpressionTag];
  DoSystemOpen[fileLine];

];

(*************************************************************************************************)

LoadSource[fullReload_:True, fullRead_:False] := Block[
  {$AllowInternet = False, URLSubmit = Print["URLSubmit[", Row[{##}, " "], "]"]&},
  FinishDynamic[];
  Block[{packages = ReadSource[!fullRead, fullReload]},
    If[FailureQ[packages], ReturnFailed[]];
    $LoadCount++;
    If[!FailureQ[evaluatePackageData @ packages], $lastLoadSuccessful = True];
  ]
];
$lastLoadSuccessful = False;

(*************************************************************************************************)

selfModTime[] := UnixTime @ FileDate[$LoaderFileName, "Modification"];

$lastSelfModTime = selfModTime[];

NeedsSelfLoad[] := selfModTime[] =!= $lastSelfModTime;

(*************************************************************************************************)

End[];

EndPackage[];