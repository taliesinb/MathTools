PublicFunction[RunTests]

SetUsage @ "
RunTests[] runs all tests in the \"Tests/Inputs/\" directory.
RunTests['patt$'] runs tests that match the string pattern patt$.
* each test file is an ordinary '.m' file, containing lines that will be run in order.
* lines ending in ; will be run but will not produce tests.
* lines not ending in ; will produce results that constitute tests.
* tests will be matched against the contents of a corresponding file in the \"Tests/Outputs/\" directory.
* all matching is via %Same.
* any match failures will print out the failing results such that expected and actual can be flipped by clicking.
* buttons will also be included to update the test in-line, or jump to the corresponding input or expected result.
* if any lines produce messages or aborts, these will be reported and result in test failures.
* if more than 2 messages are produced, the remaining lines will be skipped.
* the special functions %TestRaster, %TestBoxes, %TestData can be used to avoid storing complex data in-line in output files.
* these objects represent results that are stored in the \"Tests/Objects\" directory and referenced by content hash.
* these objects will format in the frontend appropriate for debugging and diagnosis.
* if results are removed or added, but the remainder match the expected results, the output file will be automatically updated.
* the following options are supported:
| %Verbose | print every action performed |
| %DryRun | do not actually write any changes to disk |
| %TestContentsPattern | skip files whose inputs do not contain a set of patterns |
| %DisableCaching | whether to disable all caching for purposes of reproducibility (default True) |
"


PublicOption[TestContentsPattern, DisableCaching]

Options[RunTests] = {
  Verbose -> False,
  DryRun -> False,
  TestContentsPattern -> None,
  DisableCaching -> False
};

RunTests::noindir = "Input directory not found at ``.";
RunTests::notests = "No tests present in ``.";

RunTests[filePattern_String:All, OptionsPattern[]] := Scope[

  UnpackOptions[$verbose, $dryRun, $testContentsPattern, disableCaching];
  $CachingEnabled = !TrueQ[disableCaching];

  testFiles = findTestFiles[filePattern];
  If[testFiles === {}, ReturnFailed["notests", inDir]];

  EnsureDirectory[LocalPath["Tests", "Objects", #]]& /@ {"MX", "PNG", "WL"};

  outDir = LocalPath["Tests", "Outputs"];
  EnsureDirectoryShallow[outDir];

  fileResults = Map[runFileTest[#, outDir]&, testFiles];
  shortFiles = StringTrimLeft[testFiles, LocalPath["Tests", "Inputs"] <> $PathnameSeparator];
  AssociationThread[shortFiles, fileResults]
];

(**************************************************************************************************)

findTestFiles[pattern_] := Scope[
  inDir = LocalPath["Tests", "Inputs"];
  If[!DirectoryQ[inDir], ReturnFailed["noindir", inDir]];
  FileNames[If[pattern === All, "*.m", pattern], inDir]
];

(**************************************************************************************************)

RunTests::cantAutoAdd = "Expected outputs `` =!= actual outputs ``, and overlap does not match.";
RunTests::noOutputs = "Output file `` does not exist or is empty, creating one with `` outputs."
RunTests::inputMessages = "Some `` inputs produced messages, aborting.";
RunTests::outputMessages = "Some `` outputs produced messages, which shoudn't happen.";
RunTests::someFailed = "Some `` tests failed, `` tests passed."

$numStatements = $numMessages = $numFailed = $numPassed = 0;
$skipRemaining = False;

runFileTest[inputPath_, outDir_] := Scope[

  $fileContents = Association[];

  inputFileName = FileNameTake @ inputPath;
  VPrint["Reading inputs from ", MsgPath @ inputPath];
  inputs = readExpressionList @ inputPath;
  If[FailureQ[inputs], ReturnFailed[]];
  If[!contentsMatchContentsPattern[inputs],
    VPrint["Skipping, ", MsgExpr @ $testContentsPattern, " not found."];
    Return[];
  ];

  VPrint["Evaluating ", Length @ inputs, " expressions."];
  $numStatements = $numMessages = 0; $skipRemaining = False;
  newOutputs = evalExpressions @ inputs;
  newCount = Length[newOutputs];
  If[$numStatements > 0, VPrint["Ran ", $numStatements, " preparation statements."]];
  If[$numMessages > 0, VMessage[RunTests::inputMessages, $numMessages]; Return[$Failed]];

  $outputPath = outputPath = PathJoin[outDir, inputFileName];
  If[!FileExistsQ[outputPath] || FileByteCount[outputPath] == 0,
    VMessage[RunTests::noOutputs, outputPath, Length @ newOutputs];
    If[(gallery = TestOutputGallery[newOutputs]) =!= None, Print[gallery]];
    writeExpressionList[outputPath, newOutputs];
    VPrint["Vacuously done."]; Return[{newCount, 0}];
  ];

  VPrint["Reading expected outputs from ", MsgPath @ outputPath];
  oldOutputs = readExpressionList @ outputPath;
  If[FailureQ[oldOutputs], ReturnFailed[]];
  oldOutputs //= evalExpressions;
  If[$numMessages > 0, VMessage[RunTests::outputMessages, $numMessages]; Return[$Failed]];

  oldCount = Length[oldOutputs];
  minCount = Min[newCount, oldCount];

  If[newCount =!= oldCount,
    If[Part[newOutputs, 1;;minCount, 3] === Part[oldOutputs, 1;;minCount, 3],
      VPrint["Expected outputs ", newCount, " =!= actual outputs ", oldCount, ", but overlap matches; updating outputs."];
      If[newCount > oldCount && (gallery = TestOutputGallery[Drop[newOutputs, oldCount]]) =!= None, Print[gallery]];
      writeExpressionList[outputPath, newOutputs];
      VPrint["Vacuously done."]; Return[{newCount, 0}];
    ];
    VMessage[RunTests::cantAutoAdd, oldCount, newCount]
  ];

  VPrint["Comparing ", newCount " expressions."];
  $numFailed = $numPassed = 0;
  ScanThread[compareOutputs, {Range @ newCount, newOutputs, oldOutputs}];

  If[$numFailed === 0,
    VPrint["All ", $numPassed, " tests passed!"],
    VMessage[RunTests::someFailed, $numPassed, $numFailed];
  ];

  {$numPassed, $numFailed}
];

(**************************************************************************************************)

PublicHead[ExpressionAt]

evalExpressions[exprs_List] := Map[evalExpr, exprs];

ExpressionAt::exprMsg = "Message occurred at ``.";
ExpressionAt::exprAbort = "Abort occured at ``."
ExpressionAt::overload = "Too many messages occurred, skipping remaining evaluations."
ExpressionAt::exprBad = "Bad expression ``."

evalExpr = Case[

  e_ExpressionAt /; $skipRemaining := e;

  ExpressionAt[f_, p_, Hold[CompoundExpression[e___, Null]]] := (
    $numStatements += 1;
    % @ ExpressionAt[f, p, Hold[CompoundExpression[e]]];
    Nothing
  );

  ExpressionAt[f_, p_, Hold[e_]] := With[
    {res = CheckAbort[Check[e, $HadMessage], $HadAbort]},
    If[res === $HadMessage, $numMessages++; VMessage[ExpressionAt::exprMsg, calcFileLine[f, p]]];
    If[res === $HadAbort, $numMessages++; VMessage[ExpressionAt::exprAbort, calcFileLine[f, p]]];
    If[$numMessages >= 3, $skipRemaining = True; VMessage[ExpressionAt::overload]];
    ExpressionAt[f, p, Construct[Hold, res]]
  ];

  other_ := (VMessage[ExpressionAt::exprBad, MsgExpr @ other]; Nothing)
];

DefineStandardTraditionalForm[e:ExpressionAt[_String, _Integer, _Hold] :> makeExpressionAtBoxes[e]]

makeExpressionAtBoxes[ExpressionAt[path_, pos_, h_Hold]] :=
  ClickBox[ToBoxes @ toValueIcon @ h, SystemOpen @ calcFileLine[path, pos]];

(**************************************************************************************************)

contentsMatchContentsPattern[e_] := Or[
  $testContentsPattern === None,
  AllTrue[ToList @ $testContentsPattern, ContainsQ[e, #]&]
];

(**************************************************************************************************)

PublicFunction[TestOutputGallery]

SetUsage @ "
TestOutputGallery['patt$'] finds and prints are objects like %TestRasterObject etc that correspond to a test file matching patt$.
* these are printed in a gallery, and clicking on any such object will jump to its definition in an input test file.
"

$testHeads = {TestRasterObject, TestBoxesObject, TestDataObject};
$testObjectP = Alternatives @@ Map[Blank, $testHeads];

TestOutputGallery::lenMismatch = "Cannot match inputs from file(s) `` with expected outputs from (``)."
TestOutputGallery[string_String] := Scope[
  inputFiles = findTestFiles[string];
  If[inputFiles === {}, ReturnFailed[]];
  outputFiles = StringReplace[inputFiles, LocalPath["Tests", "Inputs"] -> LocalPath["Tests", "Outputs"]];
  inputExprs = Flatten[readExpressionList /@ inputFiles];
  inputExprs //= DeleteCases[ExpressionAt[_, _, Hold[CompoundExpression[___, Null]]]];
  outputExprs = Flatten[readExpressionList /@ outputFiles];
  If[MemberQ[inputExprs, $Failed] || MemberQ[outputExprs, $Failed], ReturnFailed[]];
  If[Length[inputExprs] =!= Length[outputExprs],
    Message[TestOutputGallery::lenMismatch, MsgPath /@ inputFiles, MsgPath /@ outputFiles];
    ReturnFailed[];
  ];
  exprs = MapThread[ReplacePart[#1, 3 -> Part[#2, 3]]&, {inputExprs, outputExprs}];
  TestOutputGallery @ exprs
];

TestOutputGallery[data_] := Scope[
  outputs = DeepCases[data, $testObjectP];
  If[outputs === {}, Return @ None];
  SpacedRow[outputs, Spacings -> 20, MaxWidth -> 8]
];

(**************************************************************************************************)

$testContext = "QuiverGeometry`TestHarness`";
$testContextPath = {"System`", "GeneralUtilities`", "QuiverGeometry`", "QuiverGeometry`PackageScope`", "QuiverGeometry`Shortcuts`"};

RunTests::badTestFile = "Could not read list of expressions from test file ``.";
RunTests::syntaxError = "Syntax error at ``.";
readExpressionList[path_] := Block[
  {$Context = $testContext, $ContextPath = $testContextPath, stream, exprs, pos},
  stream = OpenRead[path];
  If[FailureQ[stream],
    VMessage[RunTests::badtestfile, path];
    Return @ $Failed];
  exprs = Bag[];
  pos = 0;
  While[(expr = Check[Quiet @ Read[stream, Hold[Expression]], $Failed]) =!= EndOfFile,
    If[expr === $Failed,
      VMessage[RunTests::syntaxError, streamPosToFileLine[path, StreamPosition @ stream]];
      Close[stream];
      Return @ $Failed
    ];
    If[expr =!= Hold[Null], StuffBag[exprs, ExpressionAt[path, pos, expr]]];
    pos = StreamPosition[stream];
  ];
  Close[stream];
  exprs = BagPart[exprs, All];
  If[!ListQ[exprs],
    VMessage[RunTests::badTestFile, path];
    Return @ $Failed];
  exprs
];

(**************************************************************************************************)

RunTests::writeFail = "Could not write `` expressions to ``.";
writeExpressionList[path_, list_] := Block[
  {$Context = $testContext, $ContextPath = $testContextPath},
  VPrint["Writing ", Length @ list, " expressions to ", MsgPath @ path];
  If[$dryRun, Return[]];
  str = StringRiffle[Map[toOutString, list], "\n\n"];
  If[FailureQ @ ExportUTF8[path, str], VMessage[RunTests::writeFail, Length @ list, MsgPath @ path]];
];

toOutString = Case[
  ExpressionAt[_, _, Hold[s_String]] := StringReplace[ToString[s, InputForm], "\\n" -> "\n"];
  ExpressionAt[_, _, Hold[e_]]       := ToPrettifiedString[e, MaxIndent -> 4, CompactingWidth -> Infinity];
];

(**************************************************************************************************)

updateExpression[outPath_, outPos_, n_, newExpr_] := Scope[
  oldOutputs = ReadList[outPath, Hold[Expression]];
  newOutputs = ReplacePart[oldOutputs, n -> newExpr];
  fileLine = calcFileLine[outPath, outPos];
  Print["Updating expression #", n, " at ", fileLine];
  newExprs = ExpressionAt[0, 0, #]& /@ newOutputs;
  writeExpressionList[outPath, newExprs];
];

(**************************************************************************************************)

calcFileLine[file_String, pos_Integer] := Scope[
  (* a global cache can get stale, so we only cache if we're in RunTests *)
  contents = If[AssociationQ[$fileContents],
    CacheTo[$fileContents, file, ReadString @ file],
    ReadString @ file
  ];
  line = StringCount[StringTake[contents, UpTo[pos + 1]], "\n"] + 1;
  MsgPath[file, line]
]

(**************************************************************************************************)

RunTests::testFail = "Result #`` generated at `` disagreed with output at ``.";

compareOutputs[part_, ExpressionAt[inFile_, inPos_, newOutput_Hold], ExpressionAt[outFile_, outPos_, oldOutput_Hold]] /; newOutput =!= oldOutput := (
  $numFailed += 1;
  VMessage[RunTests::testFail, part, calcFileLine[inFile, inPos], calcFileLine[outFile, outPos]];
  Print @ Grid[{{
    Column[{
      smallButton["replace", $LightRed, updateExpression[outFile, outPos, part, newOutput]],
      smallButton["see in", $White, SystemOpen @ calcFileLine[inFile, inPos]],
      smallButton["see out", $White, SystemOpen @ calcFileLine[outFile, outPos]]
    }, Spacings -> 0.1], "  ",
    LabeledFlipView[{"new" -> toValueIcon[newOutput], "old" -> toValueIcon[oldOutput]}]
  }}, Alignment -> {Left, Top}];
)

SetHoldRest[smallButton];
smallButton[e_, c_, b_] := RawBoxes @ ClickBox[FrameBox[
  StyleBox[DeployBox @ e, 10],
  Background -> c, FrameStyle -> OklabDarker[c],
  Alignment -> Baseline, FrameMargins -> {{0, 0}, {0, 0}},
  ImageSize -> {60, 15}
], b];

compareOutputs[___] := $numPassed += 1;

(**************************************************************************************************)

(* just make a single icon form, and the objects have a single dispatch pathway that checks hash
exists, etc *)

toValueIcon = Case[
  Hold[e_Image]  := thumbnailize @ e;
  Hold[o:objP]   := o; (* has its own icon form *)
  Hold[e_]       := PrettyCodeForm[e, CompactingWidth -> 50, ElideLargeArrays -> True];
,
  {objP -> $testObjectP}
];

thumbnailize[e_] := ReplaceAll[e, i_Image ? HoldAtomQ :> RuleCondition @ ImageResize[i, UpTo @ 250]];

(**************************************************************************************************)

objectPath[name_] := LocalPath["Tests", "Objects", ToUpperCase @ FileExtension @ name, name];

writeObject[data_, ext_] := Scope[
  name = Base36Hash[data] <> "." <> ext;
  path = objectPath @ name;
  If[!FileExistsQ[path] || FileByteCount[path] === 0,
    VPrint["Writing object to ", path];
    whenWet @ If[ext === "mx", ExportMX, Export][path, data]
  ];
  name
];

readObject[name_] := Quiet @ Check[If[StringEndsQ[name, ".mx"], ImportMX, Import] @ objectPath[name], $Failed];

SetHoldFirst[testObjNameQ];
testObjNameQ[name_String] := FileExistsQ[objectPath @ name];
testObjNameQ[_] := False;

testObjFrame[col_, b_] := FrameBox[b, FrameStyle -> col, RoundingRadius -> 3, FrameMargins -> 5];

(**************************************************************************************************)

PrivateFunction[ImportTestObject]

ImportTestObject[_Symbol[name_String, ___]] := readObject @ name;

(**************************************************************************************************)

PublicSpecialFunction[TestRaster]
PublicObject[TestRasterObject]

TestRaster[expr_] := TestRasterObject[
  writeObject[FastRasterize @ expr, "png"],
  writeObject[ToBoxes @ expr, "mx"]
];

declareBoxFormatting[
  TestRasterObject[name_ ? testObjNameQ, _] :>
    testObjFrame[$Orange, ToBoxes @ thumbnailize @ readObject @ name]
];

(**************************************************************************************************)

PublicSpecialFunction[TestBoxes]
PublicObject[TestBoxesObject]

TestBoxes[expr_] := TestBoxesObject[writeObject[ToBoxes @ expr, "wl"]];

declareBoxFormatting[
  TestBoxesObject[name_ ? testObjNameQ] :>
    testObjFrame[$Yellow, ToBoxes @ boxesFlipper @ readObject @ name]
];

boxesFlipper[boxes_] := FlipView[{RawBoxes[boxes], PrettyCodeForm[boxes]}];

(**************************************************************************************************)

PublicSpecialFunction[TestData]
PublicObject[TestDataObject]

TestData[expr_] := TestDataObject[writeObject[expr, "mx"]];

declareBoxFormatting[
  TestDataObject[name_ ? testObjNameQ] :>
    testObjFrame[$Red, ToBoxes @ PrettyCodeForm[readObject @ name, ElideLargeArrays -> True]]
];

