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
RunTests::notests = "No test files present in `` match ``.";

$testsInPath := $testsInPath = LocalPath["Tests", "Inputs"];
$testsOutPath := $testsOutPath = LocalPath["Tests", "Outputs"];

declareFunctionAutocomplete[RunTests, {FileNameTake /@ FileNames["*.wl", $testsInPath]}];

RunTests[filePattern_String:All, OptionsPattern[]] := Scope[

  UnpackOptions[$verbose, $dryRun, $testContentsPattern, disableCaching];
  $CachingEnabled = !TrueQ[disableCaching];

  testFiles = findTestFiles[filePattern];
  If[testFiles === {}, ReturnFailed["notests", $testsInPath, filePattern]];

  EnsureDirectory[LocalPath["Tests", "Objects", #]]& /@ {"MX", "PNG", "WL"};

  outDir = LocalPath["Tests", "Outputs"];
  EnsureDirectoryShallow[outDir];

  fileResults = Map[runFileTest[#, outDir]&, testFiles];
  shortFiles = StringTrimLeft[testFiles, $testsInPath <> $PathnameSeparator];

  If[!$Notebooks, Print[]; Print["Results:"]; MapThread[printFileInfo, {StringPadRight[shortFiles, 25], fileResults}]];
  AssociationThread[shortFiles, fileResults]
];

printFileInfo[file_, {i_, n_}] := Print[file, IntegerString[i, 10, 3], " passed\t", IntegerString[n, 10, 3], " failed"];
printFileInfo[file_, {i_, 0}] := Print[file, IntegerString[i, 10, 3], " passed"];
printFileInfo[file_, $Failed] := Print[file, "encountered errors"];
printFileInfo[file_, None] := Print[file, "skipped"];

(**************************************************************************************************)

findTestFiles[pattern_] := Scope[
  If[!DirectoryQ[$testsInPath], ReturnFailed["noindir", $testsInPath]];
  FileNames[If[pattern === All, "*.wl", pattern], $testsInPath]
];

(**************************************************************************************************)

RunTests::cantAutoAdd = "``: expected outputs `` =!= actual outputs ``, and overlap does not match.";
RunTests::someMessages = "``: some `` inputs produced messages, aborting.";
RunTests::someFailed = "``: `` tests failed, `` tests passed."

countRealTests[inputs_] := Count[inputs, Except @ ExpressionAt[_, _, Hold[CompoundExpression[___, Null]]]];

runFileTest[inputPath_, outDir_] := Scope[

  msgPath = MsgPath @ inputPath;
  $fileContents = Association[];

  inputFileName = FileNameTake @ inputPath;
  VPrint["Reading inputs from ", msgPath];
  inputs = readExpressionList @ inputPath;
  If[FailureQ[inputs], ReturnFailed[]];
  If[!contentsMatchContentsPattern[inputs],
    VPrint["Skipping, ", MsgExpr @ $testContentsPattern, " not found."];
    Return @ None;
  ];
  numInputs = countRealTests[inputs];

  $outputPath = outputPath = PathJoin[outDir, inputFileName];

  VPrint["Reading expected outputs from ", MsgPath @ outputPath];
  oldOutputs = readExpressionList @ outputPath;
  oldCount = Length @ oldOutputs;
  If[FailureQ[oldOutputs], ReturnFailed[]];

  VPrint["Evaluating ", numInputs, " inputs and comparing with ", oldCount, " outputs."];
  newOutputs = evalExpressionsJoint[inputs, oldOutputs];
  newCount = Length @ newOutputs;
  If[$numMessages > 0, VMessage[RunTests::someMessages, msgPath, $numMessages]; ReturnFailed[]];

  If[newCount =!= oldCount,
    minCount = Min[newCount, oldCount];
    If[Part[newOutputs, 1;;minCount, 3] === Part[oldOutputs, 1;;minCount, 3],
      VPrint["Expected outputs ", newCount, " =!= actual outputs ", oldCount, ", but overlap matches; updating outputs."];
      If[newCount > oldCount && (gallery = TestOutputGallery[Drop[newOutputs, oldCount]]) =!= None, Print[gallery]];
      writeExpressionList[outputPath, newOutputs];
      VPrint["Vacuously done."];
      Return @ {newCount, 0};
    ];
    VMessage[RunTests::cantAutoAdd, msgPath, oldCount, newCount];
    ReturnFailed[];
  ];

  If[$numFailed === 0,
    VPrint[msgPath, ": all ", $numPassed, " tests passed!"],
    VMessage[RunTests::someFailed, msgPath, $numFailed, $numPassed];
  ];

  {$numPassed, $numFailed}
];

(**************************************************************************************************)

contentsMatchContentsPattern[e_] := Or[
  $testContentsPattern === None,
  AllTrue[ToList @ $testContentsPattern, ContainsQ[e, #]&]
];

(**************************************************************************************************)

$fileProgress = 0;
printTemp[label_] := If[$Notebooks,
  $tmpProg = PrintTemporary[
    Style[label, FontWeight -> Bold, FontFamily -> "Arial"], "  ",
    RawBoxes @ ProgressIndicatorBox[Dynamic[$fileProgress]]
  ],
  If[!$verbose, Print[]; Print[label, ":"]];
];

updateProg[i_, n_] := If[$Notebooks,
  $fileProgress = i / ilen,
  If[!$verbose, Print[i, "/", ilen]];
]

unprintTemp[] := If[$Notebooks,
  Quiet @ NotebookDelete[$tmpProg]
];

evalExpressionsJoint[inputs_, outputs_] := Block[
  {newOutputs = {}, i = 0, ilen = countRealTests[inputs], o = 0, count = 0, iExpr, oExpr},
  printTemp[inputFileName];
  $numSkipped = $numRun = $numMessages = $numFailed = $numPassed = 0;
  $skipRemaining = False;
  While[True,
    While[(iExpr = evalExpr @ SafePart[inputs, ++i]) === Nothing, Null];
    While[(oExpr = evalExpr @ SafePart[outputs, ++o]) === Nothing, Null];
    If[iExpr === oExpr === EndOfFile, Break[]];
    If[iExpr =!= EndOfFile && oExpr =!= EndOfFile,
      count++;
      updateProg[count, ilen];
      If[compareOutputs[count, iExpr, oExpr],
        If[$verbose, VPrint["Test passed."]]]];
    AppendTo[newOutputs, iExpr];
  ];
  unprintTemp[];
  DeleteCases[newOutputs, EndOfFile]
];

(**************************************************************************************************)

PublicHead[ExpressionAt]

evalExpressions[exprs_List] := (
  $numSkipped = $numRun = $numMessages = 0;
  $skipRemaining = False;
  Map[evalExpr, exprs]
)

ExpressionAt::exprMsg = "Message occurred at ``.";
ExpressionAt::exprAbort = "Abort occured at ``."
ExpressionAt::overload = "Too many messages occurred, skipping remaining evaluations."
ExpressionAt::exprBad = "Bad expression ``."

$inCE = False;

evalExpr = Case[

  _Missing := EndOfFile; (* this happens when joint expression walking hits the end of one file *)

  e_ExpressionAt /; $skipRemaining := ($numSkipped++; e);

  ExpressionAt[f_, p_, Hold[CompoundExpression[e___, Null]]] := Block[
    {$inCE = True},
    % @ ExpressionAt[f, p, Hold[CompoundExpression[e]]];
    Nothing
  ];

  ExpressionAt[f_, p_, Hold[expr_]] := With[
    {res = CheckAbort[
      Catch[
        WithMessageHandler[Construct[Hold, expr], throwMessage],
        $throwMessageTag
      ],
      $Aborted
    ]},
    $numRun++;
    handleExceptionalEvals[f, p, res];
    If[$numMessages >= 3, $skipRemaining = True; VMessage[ExpressionAt::overload]];
    ExpressionAt[f, p, res]
  ];

  other_ := (VMessage[ExpressionAt::exprBad, MsgExpr @ other]; Nothing)
];

handleExceptionalEvals[f_, p_, res_Failure] := (
  $numMessages++;
  VMessage[ExpressionAt::exprMsg, calcFileLine[f, p]];
  (* force the display of the message if we're in a CompoundExpression, which won't happen otherwise *)
  If[$inCE, compareOutputs[$numRun, ExpressionAt[f, p, res], ExpressionAt[None, None, None]]]
);

handleExceptionalEvals[f_, p_, $Aborted] := (
  $numMessages++;
  $skipRemaining = True;
  VMessage[ExpressionAt::exprAbort, calcFileLine[f, p]]
);

throwMessage[f_Failure] := Throw[f, $throwMessageTag];

(**************************************************************************************************)

DefineStandardTraditionalForm[e:ExpressionAt[_String, _Integer, _Hold] :> makeExpressionAtBoxes[e]]

makeExpressionAtBoxes[ExpressionAt[path_, pos_, h_Hold]] :=
  ClickBox[ToBoxes @ toValueIcon @ h, SystemOpen @ calcFileLine[path, pos]];

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
  outputFiles = StringReplace[inputFiles, $testsInPath -> $testsOutPath];
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
  SpacedRow[outputs, Spacings -> 20, MaxWidth -> 6]
];

(**************************************************************************************************)

SetHoldFirst[withTestContext];
withTestContext[body_] := Block[{$Context = $testContext, $ContextPath = $testContextPath}, body];

$testContext = "QuiverGeometry`TestHarness`";
$testContextPath = {"System`", "GeneralUtilities`", "QuiverGeometry`", "QuiverGeometry`Private`"};

(**************************************************************************************************)

RunTests::badTestFile = "Could not read list of expressions from test file ``.";
RunTests::syntaxError = "Syntax error at ``.";
RunTests::emptyTestFile = "File `` does not exist or is empty."

readExpressionList[path_] := withTestContext @ Block[
  {stream, exprs, pos},
  If[!FileExistsQ[path] || FileByteCount[path] == 0,
    VMessage[RunTests::emptyTestFile, MsgPath @ path];
    Return @ {}
  ];
  stream = OpenRead[path];
  If[FailureQ[stream],
    VMessage[RunTests::badtestfile, path];
    Return @ $Failed];
  exprs = Bag[];
  pos = 0;
  While[(expr = Check[Quiet @ Read[stream, Hold[Expression]], $Failed]) =!= EndOfFile,
    (* this is a symptom that we should be executing as we go along ! *)
    If[MatchQ[expr, Hold[_LoadShortcuts]], ReleaseHold @ expr; expr = Hold[Null]];
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
writeExpressionList[path_, list_] := withTestContext @ Block[
  {str},
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
  oldOutputs = withTestContext @ ReadList[outPath, Hold[Expression]];
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

RunTests::testFail = "Test #`` failed; `` versus ``.";

compareOutputs[_, ExpressionAt[_, _, new_Hold], ExpressionAt[_, _, new_]] := (
  $numPassed += 1;
  True
);

compareOutputs[part_, ExpressionAt[inFile_, inPos_, f_Failure], ExpressionAt[outFile_, outPos_, oldOutput_]] := Scope[
  fstr = FailureString @ f;
  If[$Notebooks,
    Print @ Grid[{{
      Column[{
        smallButton["see in", $White, SystemOpen @ calcFileLine[inFile, inPos]],
        smallButton["more", $Green,   printMore[inFile, inPos, oldOutput, None]]
      }, Spacings -> 0.4],
      FramedForm[fstr, $LightOrange]
    }}, Alignment -> {Left, Center}];
  ,
    Print["\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"];
    Print[fstr];
    Print["\n"];
  ];
  False
];

compareOutputs[part_, ExpressionAt[inFile_, inPos_, newOutput_Hold], ExpressionAt[outFile_, outPos_, oldOutput_Hold]] /; newOutput =!= oldOutput := (
  $numFailed += 1;
  If[$Notebooks,
    Print @ Grid[{{
      Column[{
        smallButton["replace", $LightOrange, updateExpression[outFile, outPos, part, newOutput]],
        smallButton["see in", $White, SystemOpen @ calcFileLine[inFile, inPos]],
        smallButton["see out", $White, SystemOpen @ calcFileLine[outFile, outPos]],
        smallButton["more", $Green,  printMore[inFile, inPos, oldOutput, newOutput]]
      }, Spacings -> 0.4], "  ",
      LabeledFlipView[{"new" -> toValueIcon[newOutput], "old" -> toValueIcon[oldOutput]}, LabelPosition -> Left]
    }}, Alignment -> {Left, Center}]
  ,
    VMessage[RunTests::testFail, part, calcFileLine[inFile, inPos], calcFileLine[outFile, outPos]];
    printRepStr["="];
    Print["Expected:"];
    Print[CompactPrettyForm @@ oldOutput];
    Print["\nOutput:"];
    Print[CompactPrettyForm @@ newOutput];
    Print["\nDiff:"];
    printDiff[oldOutput, newOutput];
  ];
  False
)

printRepStr[char_] := Print @ StringRepeat[char, 60];

printDiff[Hold[a_], Hold[b_]] :=
  Scan[printDiffItem[a, b], FindExpressionDifferences[a, b, MaxItems -> 5]];

printDiffItem[a_, b_][diff:ExpressionDifference[pos_, ___]] := Scope[
  {chunk1, chunk2} = extractDiffChunk[a, b, pos];
  printRepStr["-"];
  Print[diff];
  If[chunk1 =!= a || chunk2 =!= b,
    Print[diffPrettyForm @ chunk1, " =!= ", diffPrettyForm @ chunk2];
  ];
];

_compareOutputs = BadArguments[];

(**************************************************************************************************)

SetHoldRest[smallButton];
smallButton[e_, c_, b_] := RawBoxes @ ClickBox[FrameBox[
  StyleBox[DeployBox @ e, 10, FontColor -> Black, FontFamily -> "Arial", FontWeight -> Bold],
  Background -> c, FrameStyle -> OklabDarker[c],
  Alignment -> Baseline, FrameMargins -> {{0, 0}, {0, 0}},
  ImageSize -> {50, 15}
], b];


(**************************************************************************************************)

printMore[inFile_, inPos_, a_, b_] := CreateDebuggingWindow[
  Flatten @ {
    makeDiffCells[a, b],
    Cell["code", "Subsubsection"],
    makeInputCodeCell[inFile, inPos]
  },
  1200,
  StyleDefinitions -> $LightStylesheetPath
]

(**************************************************************************************************)

makeInputCodeCell[file_, pos_] := Scope[
  withTestContext[
    stream = OpenRead[file];
    SetStreamPosition[stream, pos];
    testExpr = Read[stream, Hold[Expression]];
    pos2 = StreamPosition[stream];
    depSpans = findDependencySpans[stream, testExpr, pos];
    Close[stream];
  ];
  $contents = ReadString[file];
  chunk = contentChunk @ {pos + 1, pos2};
  If[depSpans =!= {},
    depChunks = contentChunk /@ depSpans;
    chunk = StringJoin @ Riffle[Append[depChunks, chunk], "\n\n"];
  ];
  chunk //= StringReplace[WordBoundary ~~ ("TestRaster"|"TestBoxes") ~~ WordBoundary -> "Identity"];
  Cell[BoxData @ chunk, "Code"]
];

contentChunk[span_] := StringTrim @ StringTake[$contents, span];

findLocalSymbols[expr_] := Union @ DeepCases[expr, s_Symbol ? testSymbolQ :> HoldPattern[s]];

findDependencySpans[stream_, expr_, stopPos_] := Scope[

  locals = findLocalSymbols[expr];
  If[locals === {}, Return @ {}];

  Label[retry];
  localSymP = Alternatives @@ locals;
  newLocals = {}; spans = {};
  pos1 = pos2 = 0;
  SetStreamPosition[stream, 0];
  While[pos2 < stopPos,
    expr = Read[stream, Hold[Expression]];
    If[expr === EndOfFile, Break[]];
    pos1 = pos2; pos2 = StreamPosition @ stream;
    expr = expr /. _Pattern :> Null; (* don't want to pick up pattern symbols as locals *)
    If[ContainsQ[expr, (Set|SetDelayed)[lhs_, rhs_] /; !FreeQ[Unevaluated @ lhs, localSymP]],
      UnionTo[newLocals, Complement[findLocalSymbols @ expr, locals]];
      AppendTo[spans, {pos1, pos2}]];
  ];
  (* because we go from top to bottom, we might find a definition that depends on an earlier definition,
  so we expand the locals we depend on and try again *)
  If[newLocals =!= {},
    locals = Union[locals, newLocals];
    Goto[retry];
  ];

  spans
];

(* we don't end execute LoadShortcuts so we'll get some spurious local symbols, but that's ok. *)
SetHoldAllComplete[testSymbolQ];
testSymbolQ[s_ ? HoldSymbolQ] := Context[s] === $testContext && StringLength[HoldSymbolName[s]] > 1;

(**************************************************************************************************)

(* these three cases happen for messages, where we only have the old form or even neither (if the message is in a CompoundExpression *)

makeDiffCells[None, None] := Nothing;

makeDiffCells[Hold[TestRasterObject[p_String]], None] := {
  Cell["expected", "Subsubsection"],
  Cell[BoxData @ ToBoxes @ importObject @ p, "Output"];
}

makeDiffCells[Hold[other_], None] := Scope[
  code = PrettyCodeForm[other, CompactingWidth -> 50, ElideLargeArrays -> True];
  {
    Cell["expected", "Subsubsection"],
    Cell[BoxData @ ToBoxes @ code, "Output"]
  }
];

makeDiffCells[Hold[a:{__TestRasterObject}], Hold[b:{__TestRasterObject}]] /; Length[a] === Length[b] :=
  MapThread[makeDiffCells[Hold[#1], Hold[#2]]&, {a, b}];

makeDiffCells[Hold @ TestRasterObject[img1_, box1_], Hold @ TestRasterObject[img2_, box2_]] := Scope[
  imgs = Map[Image[Import @ objectPath @ #, Magnification -> 1]&, {img1, img2}];
  boxes = Map[ImportMX @ objectPath @ #&, {box1, box2}];
  List[
    Cell["expected rasters", "Subsubsection"],
    makeImageDiffCell @@ imgs,
    Cell["box diffs", "Subsubsection"],
    makeExprDiffCells @@ boxes
  ]
];

makeDiffCells[Hold[a_], Hold[b_]] := makeExprDiffCells[a, b];

niceLabel[a_] := Style[a, FontFamily -> "Arial", Bold];

makeImageDiffCell[a_, b_] :=
  Cell[BoxData @ ToBoxes @ FlipView[{Labeled[a, niceLabel @ "old"], Labeled[b, niceLabel @ "new"]}], "Output"];

(**************************************************************************************************)

makeExprDiffCells[a_, b_] := makeDiffItemCell[a, b] /@ FindExpressionDifferences[a, b, MaxItems -> 5];

makeDiffItemCell[a_, b_][diff:ExpressionDifference[pos_, ___]] := With[
  {chunks = extractDiffChunk[a, b, pos]},
  {Cell[BoxData @ ToBoxes @ diff, "Output", Background -> RGBColor[1, 0.95, 0.95]],
   Cell[BoxData @ ToBoxes @ Row[diffPrettyForm /@ chunks, "  \[NotEqual]  "], "Output"]}
];

diffPrettyForm[e_] := CompactPrettyFullForm[e, CompactRealNumbers -> 5, CompactingWidth -> 100];

extractDiffChunk[a_, b_, {}] := {a, b};

extractDiffChunk[a_, b_, pos_] := Scope[
  opos = pos;
  While[pos =!= {},
    chunk1 = Extract[a, pos, InternalHoldForm];
    chunk2 = Extract[b, pos, InternalHoldForm];
    leafCount = Max[LeafCount @ chunk1, LeafCount @ chunk2];
    If[leafCount > 8,
      If[leafCount > 12,
        chunk1 = Extract[a, opos, InternalHoldForm];
        chunk2 = Extract[b, opos, InternalHoldForm];
      ];
      Break[]
    ];
    opos = pos; pos //= Most;
  ];
  {chunk1, chunk2}
];

(**************************************************************************************************)

(* just make a single icon form, and the objects have a single dispatch pathway that checks hash
exists, etc *)

toValueIcon = Case[
  Hold[e_Image]    := thumbnailize @ e;
  Hold[o:objP]     := o; (* has its own icon form *)
  Hold[e_]         := PrettyCodeForm[e, CompactingWidth -> 50, ElideLargeArrays -> True];
  Hold[o:{objP..}] := Row[o, Spacer[5]];
,
  {objP -> $testObjectP}
];

thumbnailize[e_] := ReplaceAll[e, i_Image ? HoldAtomQ :> RuleCondition @ toThumbnail[i]];

toThumbnail[i_] := Scope[
  {w, h} = ImageDimensions[i];
  Image[If[w < 300 && h < 300, i, ImageResize[i, {300}]], Magnification -> 1]
];

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
  TestRasterObject[name_ ? testObjNameQ, mxName_String] :>
    ClickBox[
      testObjFrame[$Orange, ToBoxes @ thumbnailize @ readObject @ name],
      SetSelectedNotebook @ CreateDocument @ TextCell[ImportMX @ objectPath @ mxName, "Input"]
    ]
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

