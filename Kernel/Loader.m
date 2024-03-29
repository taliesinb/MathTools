BeginPackage["MTLoader`", {"GeneralUtilities`"}];

(* contexts *)
$PublicContext = "MathTools`";
$PrivateContext = "MathTools`Private`";
$CacheContext = "MathToolsCaches`";
$LoaderContext = "MTLoader`";
$ShortcutsContext = "MathToolsShortcuts`";

(* file paths *)
$LoaderFileName = AbsoluteFileName @ ExpandFileName @ $InputFileName;
$SourceDirectory = DirectoryName @ $InputFileName;
$PackageDirectory = ParentDirectory @ $SourceDirectory;

If[!TrueQ[MTLoader`Standalone`$StandaloneLoaded], Get @ FileNameJoin[{$PackageDirectory, "Standalone", "init.m"}]];

DeclarePreservedVariable
DeclarePreservedFunction
$PreservedVariables = Data`UnorderedAssociation[];
$PreservedFunctions = Data`UnorderedAssociation[];

(* these are changed livee during loading for various purposes *)
$CurrentlyLoading = False;
$CurrentFile = None;
$CurrentFileName = None;
$RegexCacheDirty = False;
$LastFailure = None;

$LoadCount;
$SourceFiles;

(* these are populated by loading and used for syntax generation later *)
$FromSymbolAlias
$ToSymbolAlias
$SymbolGroups
$SymbolTable
$CoreNameToSymbol

(* this needs to be set before calling the loader *)
$LoadVerbose

(* these can be customized in user_init.m *)
$DisableSyntaxInformation
$DisableMenuItems
$DisableSetUsage
$FastLoad (* <- does all the above *)

(* these are used by the entrypoint init.m *)
SourceFiles
ReadSource
LoadSource
NeedsSelfLoad
LoadSingleFile

(* these are for optimizing load time and other debugging *)
FileTimings
FileLineTimings
ExpensiveFileTimings
ExpensiveFileLineTimings
DirectoryTimings
FindSuspiciousCodebaseLines
FindCodebaseLines
ComputeSourceExpressionHashes
ApplySourceSymbolRewrites
ReadAliasRules

ExpressionTable

WatchPrint
WatchCurrentCell
WatchCurrentCellAdd

(*************************************************************************************************)

Begin["`Private`"];

$mainPathLength = StringLength @ $SourceDirectory;
$trimmedMainContext = StringTrim[$PublicContext, "`"];

LVPrint[args___] := If[$LoadVerbose, Print[args]];

LVPrint["Prelude."];

(*************************************************************************************************)

If[$firstTime =!= True,

(* the first few symbols cause expensive packages to load, and appear as options to Style. RowLabels is put in System by GraphicsGrid. *)
Unprotect[System`TemplateSlot, System`EdgeThickness, System`EpilogFunction, System`LLMEvaluator, System`LLMEvaluatorNames, System`RowLabels, System`ColumnLabels, System`EdgeOpacity];
ClearAll[System`TemplateSlot, System`EdgeThickness, System`EpilogFunction, System`LLMEvaluator, System`LLMEvaluatorNames, System`RowLabels, System`ColumnLabels, System`EdgeOpacity];
Quiet[Style; Options[Style]];

(* moved from A0Usage.m because this runs slowly:
the default behavior of System`InformationDump` will introduce LineSpacing that messes up
my SetUsage inline tables, so remove it. *)
dummy::usage = "Dummy";
ToBoxes[Information[dummy]];
System`InformationDump`subtitleStyled[sub_] := Style[sub, "InformationUsageText"];

];
$firstTime = True;

(*************************************************************************************************)

$baseContextPath = {"System`", "GeneralUtilities`", $PublicContext, $PrivateContext};

(*************************************************************************************************)

EPrint[args___] := If[!$silent, Print[args]];

$silent = False;

(*************************************************************************************************)

ReadAliasRules[path_] := Module[{symbolAliasStr},
  symbolAliasStr = ByteArrayToString @ ReadByteArray @ path;
  Cases[
    StringSplit[Discard[StringStartsQ["#"]] @ StringSplit[symbolAliasStr, "\n"], Whitespace],
    {k_String, v_String} :> Rule[k, v]
  ]
];

(*************************************************************************************************)
(*************************************************************************************************)
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

LVPrint["Reading core symbols from Data/Wolfram/SymbolTable.m."];

(* we will immediately resolve these system symbols, which will take care of the vast majority of Package`PackageSymbol cases *)
symbolTablePath = FileNameJoin[{$PackageDirectory, "Data", "Wolfram", "SymbolTable.m"}];
$SymbolTable = Check[Get @ symbolTablePath, $Failed];
If[!MatchQ[$SymbolTable, {Rule[_, _List]..}],
  Print["Error in SymbolTable, aborting."];
  End[]; EndPackage[]; Abort[];
];

symbolAliasPath = FileNameJoin[{$PackageDirectory, "Data", "Wolfram", "SymbolAliases.txt"}];
aliasRules = ReadAliasRules[symbolAliasPath];
$FromSymbolAlias = Association @ aliasRules;
$ToSymbolAlias = Association @ Reverse[aliasRules, 2];

$coreSymbols = DeleteCases[_Hold] @ Catenate @ Values @ $SymbolTable;

$SymbolTable = Association @ $SymbolTable;

$coreSymbols = Sort @ DeleteDuplicates @ $coreSymbols;

$coreSymbolNames = SymbolName /@ $coreSymbols;
$coreSymbolContexts = Context /@ $coreSymbols;

(*************************************************************************************************)

$corePackageSymbols = Pick[$coreSymbols, $coreSymbolContexts, "Package`"];
$corePackageSymbolNames = SymbolName /@ $corePackageSymbols;
$corePackageSymbolGroups = StringTrim[$corePackageSymbolNames, {"System", "Public", "Private"}];
$legacyPackageDirs = {"Package", "PackageExport", "PackageImport", "PackageScope"};

$cpsPatt = Alternatives @@ $corePackageSymbols;
collectSymbolGroups[packageExpr_] := Module[{positions},
  positions = Position[packageExpressions, $cpsPatt];
  Merge[Flatten @ Extract[packageExpressions, Most /@ positions, toSymbolGroupRules], Identity]
];

SetAttributes[toSymbolGroupRules, HoldAllComplete];
$packageSymbolToGroup = AssociationThread[$corePackageSymbols, $corePackageSymbolGroups];
toSymbolGroupRules[head_[args__]] := Thread[$packageSymbolToGroup[head] -> DeepCases[Hold[args], _String]]

(*************************************************************************************************)

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

$coreSymbolNames //= Sort;
$coreSymbolAssociation = AssociationThread[$coreSymbolNames, $coreSymbols];

$shadowedSymbolNames = {"DistanceMatrix", "Torus"};
KeyDropFrom[$coreSymbolAssociation, $shadowedSymbolNames];
$coreSymbolNames = Keys @ $coreSymbolAssociation;

$CoreNameToSymbol = $coreSymbolAssociation;

GeneralUtilities`$CurrentFileName =.;

(*************************************************************************************************)

$initialSymbolResolutionDispatch1 = (q_Package`PackageSymbol :> RuleCondition[resolvePackageSymbol @@ q]);

Clear[resolvePackageSymbol];

resolvePackageSymbol["$PackageFileName"]  := $CurrentFile;
resolvePackageSymbol["$PackageDirectory"] := $PackageDirectory;

Scan[
  Set[resolvePackageSymbol["V`" <> SymbolName[#]], Verbatim[#]]&,
  {Blank, BlankSequence, BlankNullSequence, Alternatives, Repeated, RepeatedNull,
   Pattern, Except, Longest, Shortest, Verbatim, HoldPattern, KeyValuePattern,
   Condition, PatternTest, Optional}
];

KeyValueMap[Set[resolvePackageSymbol[#1], #2]&, $coreSymbolAssociation];

resolvePackageSymbol["UpSetDelayed"]      := resolvePackageSymbol["SetCached"];

resolvePackageSymbol[o_] := Package`PackageSymbol[o];

KeyValueMap[Set[resolvePackageSymbol[#1], resolvePackageSymbol[#2]]&, $FromSymbolAlias];

$initialSymbolResolutionDispatch2 = Dispatch @ {
  Package`PackageSymbol["SetUsage"][usageString_String]  :> RuleCondition[rewriteSetUsage[usageString]],
  Package`PackageSymbol["$PackageInitializer"]           :> If[DownValues[MTLoader`LoadSource] === {}, Get @ kernelInit],
  p:Package`PublicScopedOption[___]                      :> RuleCondition @ processScopedOption[p],
  p:Package`PublicTypesettingFormBox[___]                :> RuleCondition @ processTypesettingFormBox[p]
};

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
rewriteSetUsage[usageString_String] := Block[
  {symbolName},
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

failRead[] := Throw[$Failed, $readPackageTag];

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

$loadedFileCount = 0;
loadFileContents[path_, context_] := Module[{str, contents}, Block[{$currentContext = context},
  $loadedFileCount++; $CurrentFile = path;
  str = StringReplace[fileStringUTF8 @ path, $stringProcessingRules];
  If[MatchQ[str, Whitespace] || str === "", Return @ Package`PackageData[]];
  contents = TimeConstrained[Check[Package`ToPackageExpression @ str, $Failed], 1];
  If[Head[contents] =!= Package`PackageData, contents === $Failed];
  If[ContainsQ[contents, $missingCommaP], handleMissingComma[path, contents]];
  If[FailureQ[contents], handleSyntaxError[path, str]];
  Block[{$Context = context}, contents = contents /. $initialSymbolResolutionDispatch1 /. $initialSymbolResolutionDispatch2];
  contents
]];

$stringProcessingRules = {
  "=>" -> "\[DirectedEdge]",
  "<=>" -> "\[UndirectedEdge]",
  RegularExpression["(?s)\"\"\"(.*?)\"\"\""] :>
    StringJoin["\"", StringReplace["$1", {"\\" -> "\\\\", "\"" -> "\\\""}], "\""],
  RegularExpression["(?s)ExpressionTable\\[\n(.*?)\n\\]"] :>
    StringJoin["MTLoader`ExpressionTable[\"", $currentContext, "\", \"\n", StringTrim @ StringReplace["$1", {"\\" -> "\\\\", "\"" -> "\\\""}], "\"\n]"],
(*   RegularExpression["(?s)(?<=\\s)`(.*?)`(?=\\s)"] :>
    StringJoin["\"", StringReplace["$1", {"\\" -> "\\\\", "\"" -> "\\\""}], "\""],
 *)
  RegularExpression[" ~!~ ([^\n;&]+)"] :> " ~NotMatchQ~ " <> bracketRHS["$1"],
  RegularExpression[" ~~~ ([^\n;&]+)"] :> " ~MatchQ~ " <> bracketRHS["$1"]
};

bracketRHS[s_] := bracketRHS[s] = Block[{$Context = "MTLoader`Scratch`", len},
  len = SyntaxLength @ StringDelete[s, "||" ~~ ___ ~~ EndOfString];
  "(" <> StringInsert[s, ")", len+1]
];

$missingCommaP = HoldPattern[Rule[_Times, _] | RuleDelayed[_Times, _] | SetDelayed[_Times, _] | Set[_Times, _]];

handleMissingComma[path_, expr_] := Block[
  {pos, expr2},
  $fileContentCache[path] =.;
  expr2 = expr2 /. HoldPattern[Times[-1, z_]] :> z;
  pos = Position[expr2, $missingCommaP];
  If[pos === {}, Return[]];
  pos = Part[pos, 1, 1];
  line = Extract[expr, {pos, 1}];
  error = FileLine[path, line];
  EPrint["Missing comma near ", error];
  SystemOpen @ error;
  failRead[];
];

handleSyntaxError[path_, str_] := Block[
  {tmpPath, errors},
  $fileContentCache[path] =.;
  If[$silent, failRead[]];
  EPrint["Syntax error in ", path];
  badBeep[];
  tmpPath = FileNameJoin[{$TemporaryDirectory, "syntax_error_file.m"}];
  Export[tmpPath, str, "Text", CharacterEncoding -> "UTF8"];
  errors = TimeConstrained[Quiet @ FindSyntaxErrors[tmpPath], 2, {}];
  errors = errors /. {tmpPath -> path, HoldPattern[StringTake[s_, _]] :> s}; (* work around bug in FSE if error is at last char *)
  If[errors =!= {},
    EPrint["Aborting; syntax errors:"];
    Scan[EPrint, Take[errors, UpTo[5]]];
    SystemOpen @ Part[errors, 1, 1];
  ];
  failRead[];
];

If[$OperatingSystem === "MacOSX",
badBeep[] := Run["afplay /System/Library/Sounds/Sosumi.aiff&"],
badBeep[] := (Beep[]; Pause[0.1]; Beep[]);
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

(* because $ContextPath is not allowed to be empty -- this results in weird internal messages.
also, we need System` in there because otherwise General` shows up in other contexts, must
be some quited message that gets generated *)
$dummyContextPath = {"MTLoader`DummyContext`"};

createSymbolsInContextAndRules[names_, context_, contextPath_] := Block[
  {$Context = context, $ContextPath = contextPath, $NewSymbol, rules, resolvedSyms},
  resolvedSyms = ToExpression[names, InputForm, Hold];
  rules = MapThread[toSymbolReplacementRule, {names, resolvedSyms}];
  rules
];

toSymbolReplacementRule[name_, Hold[sym_]] :=
  Package`PackageSymbol[name] :> sym;

addPackageSymbolsToBag[bag_, expr_, head_] := (
  Internal`StuffBag[bag, Cases[expr, e:head[Package`PackageSymbol[_String]..] :> Part[List @@ e, All, 1], {2}], 2];
  Cases[expr, splice_Package`PackageSplice :> addPackageSymbolsToBag[bag, splice, head], {2}];
  (* ^= pick up PublicTypesettingFormBox etc *)
);

addPackageCasesToBag[bag_, expr_, rule_] :=
  Internal`StuffBag[bag, Cases[expr, rule, {2}], 1];

$globalImports = {"System`", "GeneralUtilities`"};

resolveRemainingSymbols[{path_, context_, packageData_Package`PackageData, _}] := Module[
  {unresolvedNames, dispatch},
  unresolvedNames = DeepUniqueCases[packageData, Package`PackageSymbol[name_] :> name];
  dispatch = Dispatch @ createSymbolsInContextAndRules[unresolvedNames, context, $globalImports];
  {path, context, packageData /. dispatch}
];

(*************************************************************************************************)

$skippedFilesPattern = {"Loader.m", "Watcher.m", "init.m", "*.old.m", "user_*.m"};

SourceFiles[] := Block[
  {skippedFiles, userIgnoreFile, ignoredFiles, sourceFiles},

  skippedFiles = FileNames[$skippedFilesPattern, $SourceDirectory];

  userIgnoreFile = FileNames["user_ignore.m", $SourceDirectory];

  If[Length[userIgnoreFile] === 1,
    ignoredFiles = StringTrim @ StringSplit[FileString @ First @ userIgnoreFile, "\n"];
    skippedFiles = Join[skippedFiles, FileNames[ignoredFiles, $SourceDirectory, Infinity]];
  ];

  sourceFiles = FileNames["*.m", $SourceDirectory, Infinity];
  sourceFiles = Sort @ Complement[sourceFiles, skippedFiles];

  SortBy[sourceFiles, fileSortingTuple]
];

(* TODO: have a FilePriority.m file that lists patterns, the position of the first match in this list
determines when the thing executes, or maybe it's a table from pattern to priority *)

$macroPath = $PathnameSeparator <> "Macros" <> $PathnameSeparator;
$initUtilsPath = $PathnameSeparator ~~ ("A0Init" | "A0Utilities") ~~ $PathnameSeparator;

fileSortingTuple[path_] := {
  StringFreeQ[path, $macroPath],
  StringFreeQ[path, $initUtilsPath],
  Which[StringEndsQ[path, "init.m"], -1, StringEndsQ[path, "final.m"], 1, True, 0],
  path
};

(*************************************************************************************************)

(*
fullReload=False means drop packages from the returned list that were unchanged since their last read.
even if set to true, it is overridden if any init files were changed, since they cause many previous
evaluation results to be incorrect.
*)

ReadSource[fullReload_:True, clear_:True] := Block[
  {sourceFiles, userFiles,
   packageExpressions,
   systemSymbols, publicSymbols, privateSymbols, cacheSymbols,
   cacheSymbolRules, symbolDispatch,
   result, requiresFullReload,
   preservedValues, preservedDownValues,
   $loadedFileCount = 0, $changedTextFileCount = 0,
   dirtyCount, symbolGroups,
   GeneralUtilities`$CurrentFileName = $LoaderFileName,
   $CurrentlyLoading = True
  },

  Off[General::shdw]; (* because things like SetUsage, SetAutomatic, etc have MT local definitions *)

  symbolGroups = If[fullReload, $initSymGroups, $SymbolGroups];

  $SourceFiles = sourceFiles = SourceFiles[];
  userFiles = FileNames["user_*.m", $SourceDirectory];

  If[!VectorQ[sourceFiles, StringQ],
    LVPrint["Could not obtain source files."];
    ReturnFailed[]];

  systemSymbols = Internal`Bag[];
  publicSymbols = Internal`Bag[];
  privateSymbols = Internal`Bag[];
  cacheSymbols = Internal`Bag[];

  dirtyCount = 0;
  requiresFullReload = fullReload;
  result = Catch[
    packageExpressions = Map[
      path |-> Block[{expr, context, isDirty, GeneralUtilities`$CurrentFileName = path},
        context = filePathToContext @ path;
        {expr, isDirty} = readPackageFile[path, context];
        If[isDirty, dirtyCount++];
        addPackageSymbolsToBag[systemSymbols,  expr, $systemPackageDeclarationHeadP];
        addPackageSymbolsToBag[publicSymbols,  expr, $publicPackageDeclarationHeadP];
        addPackageSymbolsToBag[privateSymbols, expr, $privatePackageDeclarationHeadP];
        addPackageSymbolsToBag[cacheSymbols,   expr, $cachePackageDeclarationHeadP];
        (* If[!requiresFullReload && isDirty && initPathQ[path],
          LVPrint["Dirty package \"", path, "\" is forcing a full reload."];
          requiresFullReload = True;
        ]; *)
        {path, context, expr, isDirty}
      ],
      sourceFiles
    ];
  ,
    $readPackageTag
  ];

  Quiet @ Remove["MTLoader`Scratch`*"]; (* <- dumping ground for SyntaxLength *)
  If[result === $Failed,
    LVPrint["Reading failed."];
    Return[$Failed]];

  Scan[observeTextFile, userFiles];

  If[!requiresFullReload,
    packageExpressions = DeleteCases[packageExpressions, {_, _, _, False}];
  ];

  If[requiresFullReload, Block[{newSymbolGroups},
    LVPrint["Populating symbol group table."];
    pos = Position[packageExpressions, Package`PrivateExprPattern];
    newSymbolGroups = collectSymbolGroups @ packageExpressions;
    symbolGroups = Union /@ Merge[{symbolGroups, newSymbolGroups}, Apply[Union]];
    $SymbolGroups = symbolGroups;
  ]];

  If[packageExpressions === {} && $changedTextFileCount == 0,
    LVPrint["No contents changed, skipping evaluation."];
    Return[{}];
  ];

  If[clear,
    LVPrint["Updating preserved values."];
    preservedValues = Replace[
      Keys @ $PreservedVariables,
      Hold[sym_] :> If[System`Private`HasImmediateValueQ[sym],
        With[{val = sym}, Hold[sym = val]],
        With[{ov = OwnValues[sym]}, Hold[OwnValues[sym] = ov]]
      ],
      {1}
    ];

    LVPrint["Updating preserved functions."];
    (* for things like RedBox that would otherwise be wiped out due to form definition caching mechanism *)
    preservedDownValues = Replace[
      Keys @ $PreservedFunctions,
      Hold[sym_] :> With[{dv = DownValues[sym]}, Hold[DownValues[sym] = dv]],
      {1}
    ];

    If[requiresFullReload,
      LVPrint["Clearing all symbols."];
      Construct[ClearAll, $PublicContext <> "*", $PublicContext <> "**`*"];
    ,
      dirtyContexts = Part[packageExpressions, All, 2];
      LVPrint["Clearing dirty contexts: ", dirtyContexts];
      Apply[ClearAll, Map[# <> "*"&, dirtyContexts]];
    ];

    LVPrint["Copying preserved values."];
    ReleaseHold[preservedValues];
    ReleaseHold[preservedDownValues];
  ];

  systemSymbols = DeleteDuplicates @ Internal`BagPart[systemSymbols, All];
  publicSymbols = DeleteDuplicates @ Internal`BagPart[publicSymbols, All];
  privateSymbols = DeleteDuplicates @ Internal`BagPart[privateSymbols, All];
  cacheSymbols = DeleteDuplicates @ Internal`BagPart[cacheSymbols, All];

  LVPrint["Creating symbols (", Length @ systemSymbols, " system, ", Length @ publicSymbols, " public, ", Length @ privateSymbols, " private)."];

  symbolDispatch = Dispatch @ Flatten @ List[
    createSymbolsInContextAndRules[systemSymbols, "System`", $dummyContextPath],
    createSymbolsInContextAndRules[publicSymbols, $PublicContext, $dummyContextPath],
    createSymbolsInContextAndRules[privateSymbols, $PrivateContext, $dummyContextPath],
    cacheSymbolRules = createSymbolsInContextAndRules[cacheSymbols, $CacheContext, $dummyContextPath]
  ];

  (* this probably happens because of a Quieted message somewhere during symbol creation *)
  If[NameQ[$PrivateContext <> "General"], Construct[Remove, $PrivateContext <> "General"]];

  LVPrint["Initializing cache symbols."];
  Scan[initCacheSymbol, cacheSymbolRules];

  LVPrint["Resolve known symbols."];
  packageExpressions = packageExpressions /. symbolDispatch;

  LVPrint["Resolving unknown symbols."];
  packageExpressions //= Map[resolveRemainingSymbols];

  On[General::shdw];

  LVPrint["Read ", Length[packageExpressions], " packages."];

  packageExpressions
];

(*************************************************************************************************)

ComputeSourceExpressionHashes[] := Module[{exprs},
  source = ReadSource[True, False];
  If[!ListQ[source], Return[$Failed]];
  Association @ Map[computeFileHash, source]
];

computeFileHash[_] := Nothing;

computeFileHash[{path_, context_, Package`PackageData[lines___List]}] :=
  StringDrop[path, $mainPathLength] -> Association[Map[computeLineHash, Unevaluated @ {lines}]];

SetAttributes[computeLineHash, HoldAllComplete];

computeLineHash[{line_, expr_}] := line -> Hash[HoldComplete @ expr];

(*************************************************************************************************)

(* this will load path, but only execute it if it is dirty since the last load, or
if any reloads of MT have happened since last load (which would invalid it) *)

LoadSingleFile[path_, context_, contextPath_] := Block[
  {dispatch, contents},
  Block[{$fileTimings, $fileLineTimings, $currentPath = path, $currentLineNumber = 1, $formsChanged, $failEval},
    $fileTimings = $fileLineTimings = Association[];
    contents = Catch[readPackageFile[path, context], $readPackageTag];
  ];
  If[FailureQ[contents], ReturnFailed[]];
  {contents, isDirty} = contents;
  Block[
    {$globalImports = Join[{"System`", "GeneralUtilities`", $PublicContext, $PrivateContext}, contextPath]},
    packageData = resolveRemainingSymbols[{path, context, contents, True}];
  ];
  Block[{$currentPath = path, $currentLineNumber = 1, $formsChanged = False, $failEval = False},
    evaluatePackage @ packageData
  ]
];

(*************************************************************************************************)

initCacheSymbol[_ :> sym_] :=
  If[!System`Private`HasImmediateValueQ[sym], sym = Data`UnorderedAssociation[]];

(*************************************************************************************************)

MakeBoxes[pd_Package`PackageData, StandardForm] :=
  RowBox[{"PackageData", StyleBox[RowBox[{"[", Length[pd], "]"}], Background -> LightRed]}];

(*************************************************************************************************)

Attributes[FindCodebaseLines] = {HoldFirst};

FindCodebaseLines[pattern_] := Block[
  {$packageExpressions = ReadSource[True, False]},
  positionToFileLine /@ Position[$packageExpressions, HoldPattern @ pattern]
];

(*************************************************************************************************)

FindSuspiciousCodebaseLines[] :=
  Construct[FindCodebaseLines, $badControlStatementPatterns];

$badControlStatementPatterns = Alternatives[
  w_Switch /; EvenQ[Length[Unevaluated @ w]],
  w_Which /; OddQ[Length[Unevaluated @ w]]
];

positionToFileLine[pos:{fileNum_, 3, line_, rest___}] :=
  FileLine[Part[$packageExpressions, fileNum, 1], Part[$packageExpressions, fileNum, 3, line, 1]];

positionToFileLine[_, _] := Nothing;

(*************************************************************************************************)

evaluatePackageData[packagesList_List] := Block[
  {$currentPath, $currentLineNumber, $formsChanged, $failEval,
    result, initialFile, finalFile, extraContexts,
   GeneralUtilities`$CurrentFileName = $LoaderFileName, $Line = 0, $ClearRegexCache = False},
  $LastFailure = None;
  $MessagePrePrint = Automatic;
  $currentPath = ""; $currentLineNumber = 0;
  $formsChanged = $failEval = False;
  $fileTimings = $fileLineTimings = Association[];
  LVPrint["Evaluating packages."];
  loadUserFile["user_init.m"];
  result = Catch[catchAllErrors @ Scan[evaluatePackage, packagesList], $evaluationTerminator];
  If[$RegexCacheDirty,
    VPrint["Clearing RegularExpression cache."];
    ClearSystemCache["RegularExpression"];
    $RegexCacheDirty = False];
  If[!MemberQ[$ContextPath, $PublicContext], AppendTo[$ContextPath, $PublicContext]];
  If[$failEval, Return[$Failed, Block]];
  ToExpression["$MessagePrePrint = MathTools`MsgPrePrint"];
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

(* this mostly has no effect, but can change dynamic symbol resolution of ToExpression, Symbol, etc.
this doesn't include GU because we shadow some GU symbols *)
$packageFileContextPath = {"System`", $PublicContext, $PrivateContext};

evaluatePackage[{path_, context_, packageData_Package`PackageData}] := If[$failEval, $Failed,
  $currentPath = path; $currentFileLineTimings = <||>; $failCount = 0;
  LVPrint["Evaluating \"", path, "\""];
  $formsChanged = Or[$formsChanged, StringContainsQ[context, "`Forms`"]]; (* to avoid expensive symbol enum *)
  TimeConstrained[
  $fileTimings[path] = msTiming @ Block[
    {$Context = context, $ContextPath = $packageFileContextPath, GeneralUtilities`$CurrentFileName = path},
    Scan[evaluateExpression, packageData]
  ], 2, Message[LoadSource::timeout]];
  $fileLineTimings[path] = $currentFileLineTimings;
];

LoadSource::timeout = "Evaluation timed out.";

evaluatePackage[spec_] := (
  EPrint["Invalid package data."];
  Print @ InputForm @ spec;
  Abort[];
);

SetAttributes[evaluateExpression, HoldAllComplete];

evaluateExpression[{lineNumber_, expr_}] := If[$failEval, $Failed,
  $Line = $currentLineNumber = lineNumber;
  $currentFileLineTimings[lineNumber] = msTiming[
    List @@ ExpandMacros @ HoldComplete @ expr
  ];
];

(*************************************************************************************************)

SetHoldAllComplete[catchAllErrors];
catchAllErrors[body_] :=
  Catch[
    CatchErrorAsFailure[
      "UncaughtError",
      WithMessageHandler[body, handleMessage],
      handleMessage
    ],
    MacroEvaluate,
    catchMacroFailure
  ];

(*************************************************************************************************)

MacroEvaluate::macrofail = "Macro failed.";

catchMacroFailure[$Failed, _] := handleMessage @
  Failure["MacroEvaluate", <|"MessageTemplate" :> MacroEvaluate::macrofail, "MessageParameters" -> {}|>];

catchMacroFailure[f_Failure, _] := handleMessage @ f;

(*************************************************************************************************)

handleMessage[f_Failure] := Block[{fileLine},
  $failEval = True;
  $LastFailure = f;
  If[$failCount++ > 5, EPrint["Emergency abort!"]; Abort[]];
  (* ^ this is an emergency measure: it shouldn't happen but when we do get a long list of errors the
  OS can lock up for a while *)
  If[!$silent, Beep[]];
  fileLine = FileLine[$currentPath, $currentLineNumber];
  EPrint["Aborting; message ", getHeldTemplate @ f, " occurred at ", fileLine];
  EPrint["Failure available at MTLoader`$LastFailure."];
  EPrint[FailureString @ f];
  Throw[$Failed, $evaluationTerminator];
  SystemOpen[fileLine];
];

getHeldTemplate[Failure[_, <|___, "MessageTemplate" :> f_, ___|>]] := HoldForm[f];
getHeldTemplate[_] := "???";

(*************************************************************************************************)

Attributes[msTiming] = {HoldAllComplete};
msTiming[e_] := 1000 * First[AbsoluteTiming @ e];

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

$userFileContext = "MTLoader`Private`User`";
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

LoadSource[fullReload_:True, silent_:False] := Block[
  {$AllowInternet = False, URLSubmit = Print["URLSubmit[", Row[{##}, " "], "]"]&, $silent = silent, $CurrentlyLoading = True},
  FinishDynamic[];
  $lastLoadSuccessful = False;
  Block[{packages = ReadSource[fullReload, True]},
    If[FailureQ[packages], Return[False]];
    If[!IntegerQ[$LoadCount], $LoadCount = 0]; $LoadCount++;
    If[!FailureQ[evaluatePackageData @ packages],
      $lastLoadSuccessful = True
    ];
  ];
  $lastLoadSuccessful
];
$lastLoadSuccessful = False;

(*************************************************************************************************)

selfModTime[] := UnixTime @ FileDate[$LoaderFileName, "Modification"];

$lastSelfModTime = selfModTime[];

NeedsSelfLoad[] := selfModTime[] =!= $lastSelfModTime;

(*************************************************************************************************)

(* mostly inherited from the Palette.nb stylesheet *)
With[{watcherInitFile = FileNameJoin[{$SourceDirectory, "Watcher.m"}]},
  $watcherNBexpr = Notebook[
    {},
    Evaluator -> "Watcher", WindowTitle -> "WatchPrint",
    InitializationCellWarning -> False, Editable -> True, Saveable -> False,
    WindowElements -> {"StatusArea", "HorizontalScrollBar", "VerticalScrollBar"},
    WindowMargins -> {{Automatic, 0}, {Automatic, 0}},
    WindowSize -> {1000, Scaled[0.8]},
    WindowToolbars -> {}, Background -> White,
    ShowGroupOpener -> False,
    StyleDefinitions -> MathTools`$LightStylesheetPath,
    WholeCellGroupOpener -> False, CellMargins -> 0,
    PrivateCellOptions -> {"EvaluationUnmatchedStyle" -> {}},
    CellOpen -> True, ShowCellLabel -> False, ShowCellTags -> False,
    Initialization :> (
      Get[watcherInitFile];
      $ContextPath = $currentContextPath;
      MathToolsWatcher`InitWatcher[]
    )
  ];
];

ensureWatcherNb[] := Module[{nbs, nb},
  nbs = Notebooks["WatchPrint"];
  If[nbs === {},
    nb = NotebookPut @ ReplaceAll[$watcherNBexpr, $currentContextPath -> $ContextPath];
    FrontEndExecute[FrontEndToken[nb, "EvaluateInitialization"]];
  ,
    nb = First @ nbs;
  ];
  nb
];

selectedCellsAsCode[] := ReplaceAll[NotebookRead @ SelectedCells[], Cell[b_, "Input", r___] :> Cell[b, "Code", r]];

WatchCurrentCell[] := WatchCellPrint[selectedCellsAsCode[], All];
WatchCurrentCellAdd[] := WatchCellPrint[selectedCellsAsCode[], After];

Attributes[WatchPrint] = {HoldFirst};
WatchPrint[expr_] := Module[{code,
  tpf = Symbol["MathTools`ToPrettifiedString"],
  ihf = Symbol["MathTools`InternalHoldForm"]},
  code = tpf @ ihf @ expr;
  WatchCellPrint[Cell[BoxData @ code, "Code"], After];
];

WatchCellPrint[cell_, pos_] := Block[
  {nb = ensureWatcherNb[]},
  SelectionMove[nb, pos, Notebook, AutoScroll -> False];
  NotebookWrite[nb, cell, All];
  FrontEndExecute[FrontEndToken[nb, "Evaluate"]]
];

(*************************************************************************************************)

ApplySourceSymbolRewrites::notCoreSymbol = "`` is not a core symbol.";
ApplySourceSymbolRewrites[rewrites:{(_String -> _String)..}, n_:Infinity] := Block[
  {$rewrites, $strPatt, $exprPatt, lhs, lhsSyms, packages},
  (* make sure we look for both the full name and the name under any existing aliases *)
  $rewrites = MapApply[
    (WordBoundary ~~ If[!MissingQ[alias = $ToSymbolAlias[#1]] && alias =!= #1, Alternatives[#1, alias], #1] ~~ WordBoundary) -> #2&,
    rewrites
  ];
  lhs = Keys @ $rewrites;
  $strPatt = WordBoundary ~~ (Alternatives @@ Part[lhs, All, 2]) ~~ WordBoundary;
  lhsSyms = Map[lookupAliasTargetSymbolLookup, lhs];
  If[MemberQ[lhsSyms, $Failed], Return @ $Failed];
  $exprPatt = Alternatives @@ lhsSyms;
  packages = ReadSource[True, False];
  DeleteCases[Map[rewritePackage, Take[packages, UpTo[n]]], {}]
];

lookupAliasTargetSymbolLookup[StringExpression[WordBoundary, s_, WordBoundary]] := lookupAliasTargetSymbolLookup @ s;
lookupAliasTargetSymbolLookup["Inf"|"Infinity"] := HoldPattern @ Infinity;
lookupAliasTargetSymbolLookup[sym_String] := Lookup[$coreSymbolAssociation, sym, Message[ApplySourceSymbolRewrites::notCoreSymbol, sym]; $Failed];
lookupAliasTargetSymbolLookup[alts_Alternatives] := FirstCase[Lookup[$coreSymbolAssociation, List @@ alts], _Symbol, Message[ApplySourceSymbolRewrites::notCoreSymbol, alts]; $Failed];
lookupAliasTargetSymbolLookup[e_] := (Message[ApplySourceSymbolRewrites::notCoreSymbol, e]; $Failed);

(* to prevent false positives searching for HoldComplete *)
SetAttributes[myHoldComplete, HoldAllComplete];

rewritePackage[{path_, context_, packageExpr_}] := Block[
  {$fileStr, $filePath = path, $currentContext = context, linePos, exprPairs, exprLines, exprs, exprSpansL, exprSpansR, exprSpans, spanReplacements, relPath, filestr2},

  $fileStr = fileStringUTF8 @ path;
  If[StringFreeQ[$fileStr, $strPatt], Return[{}]];

  (* we assume that $stringProcessingRules does not introduce any additional newlines, so that we
  can convert the PackageData line numbers into spans in the unexpanded string *)
  linePos = First /@ StringPosition[$fileStr, StartOfLine ~~ Shortest[___] ~~ EndOfLine];
  If[$fileStr === "", Return[{}]];

  exprPairs = List @@ (myHoldComplete @@@ packageExpr);
  exprLines = Part[exprPairs, All, 1];
  exprs = Part[exprPairs, All, 2;;];

  exprSpansL = Part[linePos, exprLines];
  exprSpansR = Append[Rest[exprSpansL] - 1, StringLength @ $fileStr];
  exprSpans = Transpose[{exprSpansL, exprSpansR}];

  spanReplacements = MapThread[rewriteExpr, {exprLines, exprSpans, exprs}];
  If[spanReplacements === {},
    Nothing
  ,
    filestr2 = StringReplacePart[$fileStr, Values @ spanReplacements, Keys @ spanReplacements];
    If[StringQ[filestr2], Export[path, filestr2, "Text", CharacterEncoding -> "UTF-8"]];
    relPath = StringDrop[path, $mainPathLength];
    relPath -> spanReplacements
  ]
];

deepCount[h_, p_] := Count[h, p, {0, Infinity}, Heads -> True];

rewriteExpr[line_, span_, h_] /; FreeQ[h, $exprPatt] := Nothing;
rewriteExpr[line_, span_, h_] := Module[{sold, snew, scount, ecount},
  sorig = StringTake[$fileStr, span];
  sold = escapeThings @ sorig;
  scount = StringCount[sold, $strPatt];
  ecount = deepCount[h, $exprPatt];
  If[scount > ecount,
    Print["Mismatched count in: ", FileLine[$filePath, line], " (", scount, " strings versus ", ecount, " expressions)"];
    Print["String matches:      ", KeyMap[Part[#,2]&] @ DeleteCases[0] @ AssociationMap[StringCount[sold, #]&, Keys[$rewrites]]];
    Print["Expression matches:  ", DeleteCases[0] @ AssociationMap[deepCount[h, #]&, lhsSyms]];
    printMatches[sold];
    Return[Nothing]
  ];
  snew = unescapeThings @ StringReplace[sold, $rewrites];
  If[snew === sorig, Nothing, span -> snew]
];

printMatches[sold_] := Module[{spans, results, shighlight, snew},
  spans = StringPosition[sold, $strPatt];
  results = highlightStr[StringReplace[StringTake[sold, #], $rewrites]]& /@ spans;
  shighlight = StringReplacePart[sold, results, spans];
  snew = unescapeThings @ shighlight;
  Print @ Framed[snew, Background -> GrayLevel[0.96]];
];

highlightStr[s_] := StringJoin["\!\(\*", ToString[ToBoxes @ Style[s, RGBColor[1, .2, .2]], InputForm], "\)"];

commentBalancedQ[s_String] := StringCount[s, "(*"] === StringCount[s, "*)"];

escapeThings[s_String] := StringReplace[s, {
      "(*" ~~ z:Shortest[___] ~~ "*)" /; commentBalancedQ[z] :> "<***" <> Compress[z] <> "***>",
  "\"\"\"" ~~ z:Shortest[___] ~~ "\"\"\""                    :> "<'''" <> Compress[z] <> "'''>",
      "\"" ~~ z:Shortest[___] ~~ "\"" /; notPartialStrQ[z]   :> "!'''" <> Compress[z] <> "'''!"
}];

notPartialStrQ[s_] := EvenQ @ StringLength @ First[StringCases[s, "\\".. ~~ EndOfString], ""];

unescapeThings[s_String] := StringReplace[s, {
  "<***" ~~ z:Shortest[___] ~~ "***>"                        :> "(*" <> Uncompress[z] <> "*)",
  "<'''" ~~ z:Shortest[___] ~~ "'''>"                        :> "\"\"\"" <> Uncompress[z] <> "\"\"\"",
  "!'''" ~~ z:Shortest[___] ~~ "'''!"                        :> "\"" <> Uncompress[z] <> "\""
}];

(*************************************************************************************************)

End[];

EndPackage[];