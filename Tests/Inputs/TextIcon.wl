TestRaster @ MathForm @ Row[{CategoryObjectSymbol["a"], BoldRightArrowSymbol, "\[CenterDot]"}, BaseStyle -> {FontSize -> 100}]

makeTable[char_, ff_] := Block[
  {pairs},
  pairs = Map[
    Style[
      Row[{Style[char], TextIcon[char, FontFamily -> ff, FormatType -> None], Spacer[10]}],
      FontSize->#, FontFamily -> ff]&,
    {5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50,55}
  ];
  Column[Row /@ TakeList[pairs, {12, 7, 4}]]
];
TestRaster @ makeTable["X", "Arial"]
TestRaster @ makeTable["g", "Arial"]
TestRaster @ makeTable["X", $MathFont]
TestRaster @ makeTable["g", $MathFont]


$styleOpts = Sequence[FontSize -> 30, FontFamily -> $MathFont];
TestRaster @ Style[Row[{
  "F",
  Framed["F", BaselinePosition-> Baseline],
  TextIcon["F", ContentPadding->True, $styleOpts, Background -> $Red, FormatType -> None],
  Framed["F", BaselinePosition-> Baseline, ContentPadding -> False],
  TextIcon["F", $styleOpts, Background -> $Red, FormatType -> None, ContentPadding -> False]}], $styleOpts]


makeIconRow[a_, b_, c_, fn_] :=  {fn @ a, Superscript[fn @ a, fn @ b], Subscript[fn @ a, fn @ c], Subsuperscript[fn @ a, fn @ c, fn @ b]};
$sz = 50;
TestRaster @ Grid[{
  makeIconRow["A", "B", "C", Identity],
  makeIconRow["A", "B", "C", TextIcon[#, FormatType -> None]&],
  makeIconRow["A", "B", "C", MathForm],
  makeIconRow["A", "B", "C", TextIcon[#, Method -> "Raster"]&],
  makeIconRow["a", "b", "c", Identity],
  makeIconRow["a", "b", "c", TextIcon[#, FormatType -> None]&],
  makeIconRow["a", "b", "c", MathForm],
  makeIconRow["a", "b", "c", TextIcon[#, Method -> "Raster"]&],
  makeIconRow[BoldRightArrowSymbol, BoldRightArrowSymbol, BoldRightArrowSymbol, Identity]
}, BaseStyle -> {FontSize -> $sz, FontFamily -> $MathFont}, Background->{{}, {{White, GrayLevel[0.9]}}}]