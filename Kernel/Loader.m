Begin["QuiverGeometryPackageLoader`Private`"];

Get["GeneralUtilities`"];

Unprotect[EpilogFunction];
ClearAll[EpilogFunction];

(*************************************************************************************************)

(* we will immediately resolve these system symbols, which will take care of the vast majority of Package`PackageSymbol cases *)
$coreSymbols = {
  (* package symbols: *) Package`Package,
     Package`PackageExport, Package`PackageScope, Package`PackageImport,
     Package`SystemMacro,   Package`SystemVariable,  Package`SystemFunction,  Package`SystemOption,  Package`SystemHead,  Package`SystemSymbol,  Package`SystemForm,  Package`SystemObject,
     Package`PublicMacro,   Package`PublicVariable,  Package`PublicFunction,  Package`PublicOption,  Package`PublicHead,  Package`PublicSymbol,  Package`PublicForm,  Package`PublicObject,
     Package`PrivateMacro,  Package`PrivateVariable, Package`PrivateFunction, Package`PrivateOption, Package`PrivateHead, Package`PrivateSymbol, Package`PrivateForm, Package`PrivateObject,
  (* system symbols: *) True, False, None, Automatic, Inherited, All, Full, Indeterminate, Null, $Failed, Span, UpTo,
  (* object heads: *)
    Symbol, Integer, String, Complex, Real, Rational,
    List, Association, Image, SparseArray, Rasterize,
    Graph, Rule, RuleDelayed, TwoWayRule, DirectedEdge, UndirectedEdge,
  (* system function: *)
    Map, Scan, MapAt, MapIndexed, MapThread, Apply, Fold, FoldList, FixedPoint, Riffle,
    Composition, Function, Identity, Construct, Slot,
    Tuples, Subsets, Permutations,
    AppendTo, PrependTo, AssociateTo, ApplyTo, Normal,
    Sort, Reverse, SortBy, GroupBy, GatherBy, Ordering, Count, Counts, CountsBy, DeleteDuplicates, DeleteDuplicatesBy,
    Head, First, Last, Rest, Most, Part, Extract, Select, SelectFirst, Cases, FirstCase, Pick, Gather, Split, Partition, DeleteCases, Transpose, RotateLeft, RotateRight,
    Position, FirstPosition,
    Length, Dimensions, Prepend, Append, Take, Drop, Join, Catenate, Flatten, Union, Intersection, Complement, Range, Insert, Delete,
    Replace, ReplacePart, ReplaceAll, ReplaceRepeated,
    StringJoin, StringTake, StringDrop, StringCases, StringLength, TextString, StringTrim, StringReplace, StringRiffle, Characters, StringSplit, StringInsert, StringDelete, StringPadLeft, StringPadRight,
    RegularExpression, StringExpression, StartOfString, EndOfString, NumberString,
    Keys, KeyTake, KeyDrop, KeySort, Values, Key, AssociationMap, AssociationThread, AssociationQ, Lookup, KeyMap, KeyValueMap, Thread, Dispatch,
    PositionIndex, Merge,
    Options, OptionsPattern, OptionValue, SetOptions,
    AllTrue, AnyTrue, NoneTrue,
    StringQ, AssociationQ, ListQ, IntegerQ, FailureQ, VectorQ, MatrixQ, ArrayQ, NumberQ, GraphQ, NumericQ, BooleanQ,
    FreeQ, MemberQ, MatchQ, SameQ, UnsameQ, Equal, TrueQ, MissingQ,
    StringMatchQ, StringFreeQ, StringContainsQ, StringStartsQ, StringEndsQ, DuplicateFreeQ,
    And, Or, Not, EqualTo, Greater, GreaterThan, Less, LessThan, LessEqual, LessEqualThan, GreaterEqual, GreaterEqualThan, Between, Positive, Negative, NonNegative,
    If, While, Which, Do, Switch, Return, Throw, Catch, Break, Continue, Table, ConstantArray,
    IdentityMatrix, UnitVector, Transpose, ArrayFlatten, ArrayReduce, ArrayReshape, Inverse, RotationMatrix,
    Repeated, Verbatim, HoldPattern, Condition, RuleCondition, Except, PatternTest, Alternatives,
    Hold, HoldComplete, Sequence, Splice,
    Message, MessageName, Echo, EchoTiming, EchoFunction, Quiet, Check, General, Print, Assert,
    Sqrt, Power, Abs, Dot, Cross, Times, Plus, Minus, Subtract, Divide, Min, Max, Mod, MinMax, Floor, Ceiling, Round,
    N, Pi, Sin, Cos, Tan, Tanh, ArcTan, Re, Im, Exp, Log, Log10,
    Total, Mean, Median, Norm, Normalize, Clip, EuclideanDistance,
    Interpolation, InterpolationOrder,
    TranslationTransform, ScalingTransform, RotationTransform, TransformationMatrix, AffineTransform, GeometricTransformation,
    ImageSize, ImagePadding, ImageMargins, ContentPadding, FrameMargins, PlotRange, PlotRangePadding, PlotRangeClipping, BaseStyle, ColorFunction, ColorFunctionScaling,
    Frame, FrameTicks, Ticks, FrameStyle, FontFamily, FontWeight, FontSize, ItemSize,
    EdgeStyle, VertexStyle, EdgeShapeFunction, VertexShapeFunction, GraphLayout, DirectedEdges,
    EdgeList, VertexList, IndexGraph, VertexCount, EdgeCount, AdjacencyMatrix, Subgraph, PathGraph, GraphPlot, Graph3D,
    Style, Labeled, Tooltip, Framed, Legended, Placed,
    StyleBox, TooltipBox, FrameBox,
    Row, Column, Grid, SpanFromLeft, SpanFromAbove, SpanFromBoth, TextAlignment, Baseline, BaselinePosition, Alignment, AlignmentPoint, Spacings, Dividers,
    RowBox, GridBox,
    Graphics, Graphics3D, GraphicsGroup, GraphicsComplex, Raster,
    GraphicsBox, Graphics3DBox, GraphicsGroupBox, GraphicsComplexBox, RasterBox,
    Subscript, Superscript, Subsuperscript, UnderBar, OverBar,
    SubscriptBox, Superscript, SubsuperscriptBox,
    RGBColor, GrayLevel, Hue, CMYKColor, XYZColor, LABColor, LCHColor, LUVColor, ColorConvert, Lighter, Darker,
    (* Red, Green, Blue, Yellow, Orange, Brown, Purple, Pink, Cyan, Magenta, Black, White, LightGray, Gray,
    LightRed, LightGreen, LightBlue, LightYellow, LightOrange, LightBrown, LightPurple, LightPink, LightCyan, LightMagenta, *)
    Opacity, Directive, Thickness, AbsoluteThickness, PointSize, AbsolutePointSize, Offset, Scaled, EdgeForm, FaceForm,
    AspectRatio,
    Inset, Translate, Rotate, Annotate, Annotation, Text,
    InsetBox, RotationBox, GeometricTransformationBox, GeometricTransformation3DBox, TextBox, Text3DBox,
    Left, Right, Above, Below, Before, After, Center, Top, Bottom,
    Tiny, Small, Medium, Large,
    Line, Circle, Rectangle, Triangle, Disk, Point, Polygon, Arrow, Arrowheads, BezierCurve, BSplineCurve, JoinedCurve, FilledCurve,
    LineBox, CircleBox, RectangleBox, DiskBox, PointBox, PolygonBox, ArrowBox, BezierCurveBox, BSplineCurveBox, JoinedCurveBox, FilledCurveBox,
    HalfLine, InfiniteLine, InfinitePlane,
    Sphere, Tube, Cuboid, Cylinder, Cone, Polyhedron,
    SphereBox, TubeBox, CuboidBox, CylinderBox, ConeBox,
    ViewCenter, ViewVector, ViewPoint, ViewMatrix, ViewProjection, ViewAngle,
    MakeBoxes, ToBoxes, Format, TraditionalForm, StandardForm, RawBoxes, NumberForm, EngineeringForm, InputForm,
    SymbolName, Names,
    Attributes, SetAttributes, Protect, Unprotect, Clear, ClearAll,
    DownValues, UpValues, SubValues, Set, SetDelayed,
    Flat, OneIdentity, HoldFirst, HoldRest, HoldAll, HoldAllComplete,
  (* system scopes: *) With, Block, Module,
  (* general utilities; *)
  GeneralUtilities`Scope, GeneralUtilities`ContainsQ, GeneralUtilities`ScanIndexed,
  GeneralUtilities`DeclareArgumentCount, GeneralUtilities`Match, GeneralUtilities`MatchValues,
  GeneralUtilities`ReturnFailed, GeneralUtilities`UnpackOptions
};

$coreSymbols = Sort @ DeleteDuplicates @ $coreSymbols;

$coreSymbolNames = SymbolName /@ $coreSymbols;

toRegexPattern["$Failed"] := "(\\$Failed)";
toRegexPattern[str_] := "(" <> str <> ")";
$coreSymbolRegex = RegularExpression @ StringJoin @ Riffle[Map[toRegexPattern, Sort @ $coreSymbolNames], "|"];

$coreSymbolAssociation = AssociationThread[$coreSymbolNames, $coreSymbols];

(*************************************************************************************************)

SetAttributes[ResolvedSymbol, HoldAllComplete];
makeResolvedSymbol[name_String] := ToExpression[name, InputForm, ResolvedSymbol];

$lowerCaseSymbolRegex = RegularExpression["[$]?[a-z]"];

$initialSymbolResolutionDispatch = Dispatch[{
  (* Package`PackageSymbol[name_String] /; StringStartsQ[name, $lowerCaseSymbolRegex] :> RuleCondition[makeResolvedSymbol[name]], *)
  Package`PackageSymbol["SetUsage"][usageString_String] :> RuleCondition[rewriteSetUsage[usageString]],
  Package`PackageSymbol[name_String] /; StringMatchQ[name, $coreSymbolRegex] :> RuleCondition[$coreSymbolAssociation[name]],
  Package`PackageSymbol[name_String] /; StringContainsQ[name, "`"] :> RuleCondition[makeResolvedSymbol[name]],
  Package`PackageSymbol["$PackageFileName"] :> RuleCondition[QuiverGeometryPackageLoader`$CurrentFile],
  Package`PackageSymbol["$PackageDirectory"] :> RuleCondition[QuiverGeometryPackageLoader`$Directory]
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

VPrint[args___] := If[QuiverGeometryPackageLoader`$Verbose, Print[args]];

readPackageFile[path_, context_] := Module[{cacheEntry, fileModTime, contents},
  {cachedModTime, cachedContents} = Lookup[$fileContentCache, path, {$Failed, $Failed}];
  fileModTime = UnixTime @ FileDate[path, "Modification"];
  isDirty = FailureQ[cachedContents] || cachedModTime =!= fileModTime;
  If[isDirty,
    VPrint["Reading \"" <> path <> "\""];
    contents = loadFileContents[path, context];
    $fileContentCache[path] = {fileModTime, contents};
  ,
    contents = cachedContents;
  ];
  {contents, isDirty}
];

loadFileContents[path_, context_] := Module[{str, contents},
  $loadedFileCount++; QuiverGeometryPackageLoader`$CurrentFile = path;
  str = StringReplace[fileStringUTF8 @ path, $stringProcessingRules];
  If[MatchQ[str, Whitespace] || str === "", Return @ Package`PackageData[]];
  contents = Check[Package`ToPackageExpression @ str, $Failed];
  If[FailureQ[contents], handleSyntaxError[path]];
  Block[{$Context = context}, contents = contents /. $initialSymbolResolutionDispatch /. ResolvedSymbol[sym_] :> sym];
  contents
];

$stringProcessingRules = {
  RegularExpression["(?s)\"\"\"(.*?)\"\"\""] :>
    StringJoin["\"", StringReplace["$1", {"\\" -> "\\\\", "\"" -> "\\\""}], "\""],
  RegularExpression[" ~!~ ([^\n]+)"] :> " ~NotMatchQ~ " <> bracketRHS["$1"],
  RegularExpression[" ~~~ ([^\n]+)"] :> " ~MatchQ~ " <> bracketRHS["$1"]
}

bracketRHS[s_] := Block[{$Context = "QuiverGeometryPackageLoader`Scratch`", len},
  len = SyntaxLength[s];
  "(" <> StringInsert[s, ")", len+1]
];

If[!ValueQ[QuiverGeometryPackageLoader`$SystemOpenEnabled], QuiverGeometryPackageLoader`$SystemOpenEnabled = True];
DoSystemOpen[s_] := If[QuiverGeometryPackageLoader`$SystemOpenEnabled, SystemOpen[s]];

handleSyntaxError[path_] := Scope[
  errors = GeneralUtilities`FindSyntaxErrors[path];
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

createSymbolsInContextAndDispatchTable[names_, context_, contextPath_] := Block[
  {$Context = context, $ContextPath = contextPath, rules},
  Dispatch @ MapThread[toSymbolReplacementRule, {names, ToExpression[names, InputForm, ResolvedSymbol]}]
];

addPackageSymbolsToBag[bag_, expr_, head_] :=
  Internal`StuffBag[bag, Cases[expr, e:head[Package`PackageSymbol[_String]..] :> Part[List @@ e, All, 1], {2}], 2];

addPackageCasesToBag[bag_, expr_, rule_] :=
  Internal`StuffBag[bag, Cases[expr, rule, {2}], 1];

resolveRemainingSymbols[{path_, context_, packageData_Package`PackageData, _}] := Scope[
  unresolvedNames = DeepUniqueCases[packageData, Package`PackageSymbol[name_] :> name];
  dispatch = createSymbolsInContextAndDispatchTable[unresolvedNames, context, $globalImports];
  {path, context, packageData /. dispatch}
];

QuiverGeometryPackageLoader`ReadPackages[mainContext_, mainPath_, cachingEnabled_:True, fullReload_:True] := Block[
  {$directory, $files, $textFiles, $privateSymbols, $systemSymbols, $publicSymbols, $packageExpressions, $packageRules,
   $mainContext, $trimmedMainContext, $mainPathLength, $exportRules, $scopeRules, result, requiresFullReload
  },

  Off[General::shdw];

  $directory = AbsoluteFileName @ ExpandFileName @ mainPath;
  $mainContext = mainContext;
  $mainPathLength = StringLength[$directory];
  $trimmedMainContext = StringTrim[mainContext, "`"];

  $filesToSkip = FileNames[{"Loader.m", "init.m", "*.old.m"}, $directory];
  $userFiles = FileNames["user_*.m", $directory];
  $files = Sort @ Complement[FileNames["*.m", $directory, Infinity], Join[$filesToSkip, $userFiles]];
  $files = SortBy[$files, {StringFreeQ[#, "A0Init" | "A0Utilities"], StringSplit[StringDelete[#, ("init.m" ~~ EndOfString)], "/"]}&];

  $textFiles = FileNames[{"*.txt", "*.tex"}, $directory, Infinity];

  $globalImports = {"System`", "GeneralUtilities`", "Developer`"};

  $systemSymbols = Internal`Bag[];
  $publicSymbols = Internal`Bag[];
  $privateSymbols = Internal`Bag[];
  $loadedFileCount = $changedTextFileCount = 0;

  requiresFullReload = fullReload;
  result = Catch[
    $packageExpressions = Map[
      path |-> Block[{expr, context, isDirty},
        context = filePathToContext @ path;
        {expr, isDirty} = readPackageFile[path, context];
        addPackageSymbolsToBag[$systemSymbols,  expr, Package`SystemMacro  | Package`SystemVariable  | Package`SystemFunction  | Package`SystemHead  | Package`SystemSymbol  | Package`SystemForm  | Package`SystemObject | Package`PublicOption | Package`SystemOption];
        addPackageSymbolsToBag[$publicSymbols,  expr, Package`PublicMacro  | Package`PublicVariable  | Package`PublicFunction  | Package`PublicHead  | Package`PublicSymbol  | Package`PublicForm  | Package`PublicObject];
        addPackageSymbolsToBag[$privateSymbols, expr, Package`PrivateMacro | Package`PrivateVariable | Package`PrivateFunction | Package`PrivateHead | Package`PrivateSymbol | Package`PrivateForm | Package`PrivateObject | Package`PrivateOption];
        If[!requiresFullReload && isDirty && initPathQ[path],
          VPrint["Dirty package \"", path, "\" is forcing a full reload."];
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
    VPrint["Reading failed."];
    Return[$Failed]];

  Scan[observeTextFile, $textFiles];
  Scan[observeTextFile, $userFiles];

  If[!requiresFullReload,
    $packageExpressions = DeleteCases[$packageExpressions, {_, _, _, False}];
  ];

  If[cachingEnabled && ($loadedFileCount == 0 || $packageExpressions === {}) && $changedTextFileCount == 0,
    VPrint["No contents changed, skipping evaluation."];
    Return[{}];
  ];

  $PreviousPathAlgebra = If[
    System`Private`HasImmediateValueQ[QuiverGeometry`$PathAlgebra],
    QuiverGeometry`$PathAlgebra, None];

  If[requiresFullReload,
    VPrint["Clearing all symbols."];
    Construct[ClearAll, mainContext <> "*", mainContext <> "**`*"];
  ,
    dirtyContexts = Part[$packageExpressions, All, 2];
    VPrint["Clearing dirty contexts: ", dirtyContexts];
    Apply[ClearAll, Map[# <> "*"&, dirtyContexts]];
  ];

  $systemSymbols = DeleteDuplicates @ Internal`BagPart[$systemSymbols, All];
  $publicSymbols = DeleteDuplicates @ Internal`BagPart[$publicSymbols, All];
  $privateSymbols = DeleteDuplicates @ Internal`BagPart[$privateSymbols, All];

  $systemDispatch = createSymbolsInContextAndDispatchTable[$systemSymbols, "System`", {}];
  $publicDispatch = createSymbolsInContextAndDispatchTable[$publicSymbols, $mainContext, {}];
  $privateDispatch = createSymbolsInContextAndDispatchTable[$privateSymbols, $mainContext <> "PackageScope`", {}];

  $packageExpressions = $packageExpressions /. $systemDispatch /. $publicDispatch /. $privateDispatch;

  $packageExpressions //= Map[resolveRemainingSymbols];

  On[General::shdw];

  VPrint["Finding suspicious lines."];
  QuiverGeometryPackageLoader`$SuspiciousPackageLines = findSuspiciousPackageLines[$packageExpressions];

  VPrint["Read ", Length[$packageExpressions], " packages."];
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
  loadUserFile["user_init.m"];
  result = GeneralUtilities`WithMessageHandler[
    Scan[evaluatePackage, packagesList],
    handleMessage
  ];
  If[$failEval, Return[$Failed, Block]];
  If[$formsChanged, loadUserFile["user_final.m"]];
  result
];

QuiverGeometryPackageLoader`ReloadUserFile[name_] := loadUserFile @ name;
  
$userContext = "QuiverGeometryPackageLoader`Private`User`";
$userContextPath = {"System`", "GeneralUtilities`", "QuiverGeometry`", "QuiverGeometry`PackageScope`"};

loadUserFile[name_] := Block[{path},
  path = FileNameJoin[{QuiverGeometryPackageLoader`$Directory, name}];
  If[!FileExistsQ[path], Return[]];
  Block[{$Context = $userContext, $ContextPath = $userContextPath},
    VPrint["Loading \"", name, "\""];
    Get @ path;
  ];
];

SetAttributes[evaluateExpression, HoldAllComplete];

MakeBoxes[pd_Package`PackageData, StandardForm] :=
  RowBox[{"PackageData", StyleBox[RowBox[{"[", Length[pd], "]"}], Background -> LightRed]}];

evaluatePackage[{path_, context_, packageData_Package`PackageData}] := Catch[
  $currentPath = path; $currentFileLineTimings = <||>; $failCount = 0;
  If[$failEval, Return[$Failed, Catch]];
  VPrint["Evaluating \"", path, "\""];
  $formsChanged = Or[$formsChanged, StringContainsQ[context, "StylesheetForms"]]; (* to avoid expensive symbol enum *)
  QuiverGeometryPackageLoader`$FileTimings[path] = First @ AbsoluteTiming[
    Block[{$Context = context}, Catch[Scan[evaluateExpression, packageData], evaluateExpression]];
  ];
  QuiverGeometryPackageLoader`$FileLineTimings[path] = $currentFileLineTimings;
,
  MacroEvaluate, catchMacroFailure
];

evaluatePackage[spec_] := (
  VPrint["Invalid package data."];
  VPrint[Shallow @ spec];
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
  Throw[$Failed, evaluateExpression];
  DoSystemOpen[fileLine];

];

(*************************************************************************************************)

QuiverGeometryPackageLoader`$Directory = DirectoryName[$InputFileName];
QuiverGeometryPackageLoader`$LoaderFileName = $InputFileName;

QuiverGeometryPackageLoader`$CurrentFile = None;

$PreviousPathAlgebra = None;

$lastLoadSuccessful = False;

QuiverGeometryPackageLoader`Read[cachingEnabled_:True, fullReload_:True] :=
  QuiverGeometryPackageLoader`ReadPackages["QuiverGeometry`", QuiverGeometryPackageLoader`$Directory, cachingEnabled, fullReload];

QuiverGeometryPackageLoader`Load[fullReload_:True] := PreemptProtect @ Block[{packages},
  packages = QuiverGeometryPackageLoader`Read[True, fullReload];
  If[FailureQ[packages], ReturnFailed[]];
  If[packages === {} && $lastLoadSuccessful, Return[Null]];
  QuiverGeometryPackageLoader`$LoadCount++;
  VPrint["Evaluating packages."];
  If[!FailureQ[QuiverGeometryPackageLoader`EvaluatePackages @ packages],
    $lastLoadSuccessful = True];
  If[$PreviousPathAlgebra =!= None,
    QuiverGeometry`$PathAlgebra = $PreviousPathAlgebra];
];

QuiverGeometryPackageLoader`$LoadCount = 0;


selfModTime[] := UnixTime @ FileDate[QuiverGeometryPackageLoader`$LoaderFileName, "Modification"];

QuiverGeometryPackageLoader`$SelfModTime = selfModTime[];

QuiverGeometryPackageLoader`NeedsSelfLoad[] := selfModTime[] =!= QuiverGeometryPackageLoader`$SelfModTime;

End[];
