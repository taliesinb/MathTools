PrivateFunction[RunImageTests]

PublicFunction[RunInputOutputTests]

Options[RunInputOutputTests] = {
  Verbose -> True,
  DryRun -> False
};

RunInputOutputTests::noindir = "Input directory not found at ``.";
RunInputOutputTests::notests = "No tests present in ``.";

RunInputOutputTests[OptionsPattern[]] := Scope[
  UnpackOptions[$verbose, $dryRun];
  inDir = LocalPath["Tests", "Inputs"];
  If[!DirectoryQ[inDir], ReturnFailed["noindir", inDir]];
  outDir = LocalPath["Tests", "Outputs"];
  testFiles = FileNames["*.m", inDir];
  If[testFiles === {}, ReturnFailed["notests", inDir]];
  EnsureDirectoryShallow[outDir];
  Scan[runFileTest[#, outDir]&, testFiles];
];

RunInputOutputTests::badtestfile = "Could not read list of expressions from test file ``.";
RunInputOutputTests::lendifferbad = "Number of outputs differs (ground truth ``, ran ``), and overlap doesn't match.";
RunInputOutputTests::lendiffergood = "Number of outputs differs (ground truth ``, ran ``), and overlap matches; modifying output file to match.";
RunInputOutputTests::makeoutput = "Output file `` does not exist or is empty, creating now."
RunInputOutputTests::badinputs = "Could not obtain a list of input expressions from ``."

runFileTest[inputPath_, outDir_] := Scope[
  inputFileName = FileNameTake @ inputPath;
  VPrint["Reading inputs from ", MsgPath @ inputPath];
  inputs = readExpressionList @ inputPath;
  If[FailureQ[inputs], ReturnFailed[]];
  VPrint["Evaluating ", Length @ inputs, " expressions."];
  $numStatements = 0;
  newOutputs = Replace[inputs, {
    Hold[ce:CompoundExpression[___, Null]] :> (ce; $numStatements++; Nothing),
    Hold[e_] :> e
    }, {1}];
  If[$numStatements > 0, VPrint["Encountered ", $numStatements, " preparation statements."]];
  outputPath = FileNameJoin[{outDir, inputFileName}];
  If[!FileExistsQ[outputPath] || FileByteCount[outputPath] == 0,
    Message[RunInputOutputTests::makeoutput, outputPath];s
    writeExpressionList[outputPath, newOutputs];
    Return[Null]
  ];
  VPrint["Reading expected outputs from ", MsgPath @ outputPath];
  oldOutputs = readExpressionList @ outputPath;
  If[FailureQ[oldOutputs], ReturnFailed[]];
  oldOutputs = Replace[oldOutputs, Hold[e_] :> e, {1}];
  newCount = Length[newOutputs];
  oldCount = Length[oldOutputs];
  minCount = Min[newCount, oldCount];
  If[newCount =!= oldCount,
    If[Take[newOutputs, minCount] === Take[oldOutputs, minCount],
      Message[RunInputOutputTests::lendiffergood, oldCount, newCount];
      writeExpressionList[outputPath, newOutputs];
      Return[Null]
    ];
    ReturnFailed[RunInputOutputTests::lendifferbad, oldCount, newCount]
  ];
  $inputPath = inputPath;
  VPrint["Comparing ", newCount " expressions."];
  ScanThread[compareOutputs, {Range @ newCount, newOutputs, oldOutputs}];
];

RunInputOutputTests::ioexprfail = "Result #`` from `` changed.";

compareOutputs[part_, newOutput_, oldOutput_] := Scope[
  If[newOutput =!= oldOutput,
    Message[RunInputOutputTests::ioexprfail, part, $inputPath];
    VPrint[Framed @ FlipView[flipViewForm /@ {newOutput, oldOutput}, ImageSize -> All]];
  ];
];

flipViewForm = Case[
  e_TextRasterFile  := e;
  e_Image           := Thumbnail[e];
  e_                := ToPrettifiedString[e];
];

$testContext = "QuiverGeometry`TestHarness`";
$testContextPath = {"System`", "QuiverGeometry`", "QuiverGeometry`PackageScope`", "QuiverGeometry`Shortcuts`"};

readExpressionList[path_] := Block[
  {$Context = $testContext, $ContextPath = $testContextPath, result},
  result = ReadList[path, Hold[Expression]];
  If[!ListQ[result],
    Message[RunInputOutputTests::badtestfile, path];
    ReturnFailed[];
  ];
  result //= ReplaceAll[$decompressExpressionRules];
  result //= DeleteCases[Hold[Null]];
  result
];

writeExpressionList[path_, list_] := Block[
  {$Context = $testContext, $ContextPath = $testContextPath},
  VPrint["Writing ", Length @ list, " expressions to ", MsgPath @ path];
  If[$dryRun, Return[]];
  str = StringRiffle[Map[toOutString, list /. $compressExpressionRules], "\n\n"];
  ExportUTF8[path, str];
];

toOutString = Case[
  s_String    := StringReplace[ToString[s, InputForm], "\\n" -> "\n"];
  e_          := ToPrettifiedString[e, MaxIndent -> 4];
];

$compressExpressionRules = l_List /; LeafCount[Unevaluated @ l] > 8 && ArrayQ[Unevaluated @ l] && PackedArrayQ[ToPackedArray[l]] :>
  RuleCondition[CompressedArray[Compress[ToPackedArray[l]]]];

$decompressExpressionRules = CompressedArray[c_] :> RuleCondition[Uncompress[c]];

SystemHead[CompressedArray]

(**************************************************************************************************)

(* this evaluates to a file containing a raster of the thing, with path being a
content-based hash.
TODO: TestExpressionOutline, which should use a compact format to represent the entire tree of stuff.
*)

PublicFunction[TestRaster]
PublicObject[TextRasterFile]

toRasterPath[hash_] := LocalPath["Tests", "Rasters", hash <> ".png"];

TestRaster[expr_] := Scope[
  img = FastRasterize[expr];
  hash = Base36Hash[img];
  rasterPath = toRasterPath[hash];
  If[!FileExistsQ[rasterPath] || FileByteCount[rasterPath] === 0,
    VPrint["Writing image of dimensions ", ImageDimensions @ img, " to ", MsgPath @ rasterPath];
    whenWet @ Export[rasterPath, img]];
  TextRasterFile[hash]
];

declareBoxFormatting[
  TextRasterFile[str_String] :> makeTextRasterFileboxes[str]
]

makeTextRasterFileboxes[str_String] := Scope[
  rasterPath = toRasterPath[str];
  If[!FileExistsQ[rasterPath] || !ImageQ[Quiet[img = Import @ rasterPath]],
    RowBox[{"TextRasterFile", "[", ToBoxes @ tightColoredBoxes[rasterPath, $LightRed], "]"}],
    RowBox[{"TextRasterFile", "[", ToBoxes @ ImageResize[img, 200], "]"}]
  ]
]