PrivateFunction[RunImageTests]

$TestsDirectory = FileNameJoin[{FileNameDrop[QuiverGeometryPackageLoader`$Directory], "Tests"}];

RunInputOutputTests::noindir = "Input directory not found at ``.";
RunInputOutputTests::notests = "No tests present in ``.";

RunInputOutputTests[] := Scope[
  inDir = FileNameJoin[{$TestsDirectory, "Inputs"}];
  If[!FileExistsQ[inDir], ReturnFailed["noindir", inDir]];
  outDir = FileNameJoin[{$TestsDirectory, "Outputs"}];
  testFiles = FileNames["*.m", inDir];
  If[testFiles === {}, ReturnFailed["notests", inDir]];
  EnsureDirectory[outDir];
  Scan[runFileTest[#, outDir]&, testFiles];
];

RunInputOutputTests::badtestfile = "Could not read list of expressions from test file ``.";
RunInputOutputTests::badlen = "Number of outputs changed (`` to ``).";
RunInputOutputTests::makeoutput = "Output file `` does not exist, creating now."

runFileTest[inputPath_, outDir_] := Scope[
  inputFileName = FileNameTake @ inputPath;
  Print["Running ", inputFileName];
  inputs = readExpressionList @ inputPath;
  newOutputs = Replace[inputs, Hold[e_] :> e, {1}];
  If[FailureQ[newOutputs], ReturnFailed[]];
  outputPath = FileNameJoin[{outDir, inputFileName}];
  If[!FileExistsQ[outputPath],
    Message[RunInputOutputTests::makeoutput, outputPath];
    makeOutputFiles[inputs, outputPath];
    Return[Null]
  ];
  oldOutputs = readExpressionList @ outputPath;
  If[Length[newOutputs] =!= Length[oldOutputs],
    ReturnFailed[RunInputOutputTests::badlen, Length[oldOutputs], Length[newOutputs]]];
  $inputPath = inputPath; $outputPath = outputPath;
  ScanThread[compareOutputs, {Range @ Length @ newOutputs, newOutputs, oldOutputs}]
];

RunInputOutputTests::ioexprfail = "Result #`` from \"``\" changed. Input form shown below.";
RunInputOutputTests::iorastmissing = "Result #`` from \"``\" requires rasterization, but no raster file exists at \"``\"."
RunInputOutputTests::iorastextra = "Result #`` from \"``\" does not require rasterization, but a raster file exists at \"``\"."
RunInputOutputTests::iorastfail = "Result #`` from \"``\" does not match previous rasterization stored in \"``\". Images shown below."

compareOutputs[part_, newOutput_, oldOutput_] := Scope[
  If[newOutput =!= oldOutput,
    Message[RunInputOutputTests::ioexprfail, part, $inputPath];
    Print[Framed @ FlipView[Shallow[InputForm @ #, {4, 50}, _NumericArray]& /@ {newOutput, oldOutput}, ImageSize -> All]];
  ];
  rasterQ = requiresRasterizationQ[newOutput];
  rasterPath = makeRasterPath[part];
  rasterExists = FileExistsQ[rasterPath];
  Switch[
    {rasterQ, rasterExists},
    {True, False}, Message[RunInputOutputTests::iorastmissing, part, $inputPath, rasterPath],
    {False, True}, Message[RunInputOutputTests::iorastextra, part, $inputPath, rasterPath],
    {True, True},
      thisImage = rasterizeExpression[newOutput];
      prevImage = Import[rasterPath, "PNG", IncludeMetaInformation -> False];
      If[ImageData[thisImage] != ImageData[prevImage],
        Message[RunInputOutputTests::iorastfail, part, $inputPath, rasterPath];
        Print[Framed @ FlipView[{thisImage, prevImage}, ImageSize -> All]]
      ]
  ];
];

makeOutputFiles[inputs_List, outputPath_] := Scope[
  writeExpressionList[outputPath, inputs];
  $outputPath = outputPath;
  PartValueScan[writeAuxFile, inputs];
];

writeAuxFile[part_, output_] := Scope[
  If[!requiresRasterizationQ[output], Return[]];
  path = makeRasterPath[part];
  If[FileExistsQ[path], Return[]];
  img = rasterizeExpression[output];
  Export[path, img, "PNG", IncludeMetaInformation -> False]
];

requiresRasterizationQ = Case[
  _Graph | _Labeled | _Legended | _Framed | _Graphics | _Graphics3D | _Image | _Grid | _Row | _Column | _RasterizedForm := True;
  head_Symbol[___] := FormatValues[head] =!= {};
  head_[___] := requiresRasterizationQ[head];
  _List | Association := False;
  atom_Symbol := FormatValues[atom] =!= {};
  _ := False;
];

makeRasterPath[part_] := $outputPath <> "." <> IntegerString[part, 10, 3] <> ".png";

rasterizeExpression[expr_] := Rasterize[expr, ImageFormattingWidth -> Infinity, ImageResolution -> 144];


(*
idea: always expect a .m file as output. if it is not present, create it from scratch with the given outputs.
if it is present, compare the fresh outputs with previous outputs.
in addition, if the output would be rasterized by the markdown code, then check if there is a corresponding NAME.000.png
file. If not, create it. If so, compare it, and report discrepancies.
we can ALSO produce KaTeX, and store that in a NAME.000.tex file.

when saving and loading expressions, we should try typeset them nicely. We should compress arrays nicely. Anything with more than like 8 entries.
*)

readExpressionList[path_] := Block[
  {$Context = "QuiverGeometry`TestHarness`", $ContextPath = {"System`", "QuiverGeometry`", "QuiverGeometry`PackageScope`", "QuiverGeometry`Shortcuts`"}, result},
  result = ReadList[path];
  If[ListQ[result], result /. $decompressExpressionRules,
    Message[RunInputOutputTests::badtestfile, path]; $Failed]
];

writeExpressionList[path_, list_] := Block[
  {$Context = "QuiverGeometry`TestHarness`", $ContextPath = {"System`", "QuiverGeometry`", "QuiverGeometry`PackageScope`", "QuiverGeometry`Shortcuts`"}},
  Scan[Write[path, #]&, list /. $compressExpressionRules];
  Close[path];
];

$compressExpressionRules = l_List /; LeafCount[Unevaluated @ l] > 8 && ArrayQ[Unevaluated @ l] && PackedArrayQ[ToPackedArray[l]] :>
  RuleCondition[CompressedArray[Compress[ToPackedArray[l]]]];

$decompressExpressionRules = CompressedArray[c_] :> RuleCondition[Uncompress[c]];


SystemHead["CompressedArray"]


