TestRaster @ Graphics[ColorGradientStyle[Rectangle[{0,0},{1,1}], {1,2}], ImageSize -> 50]


TestRaster @ Graphics[{
  ColorGradientStyle[Rectangle[{0,0},{1,1}], {1,2}],
  ColorGradientStyle[Rectangle[{1.5,0},{3.5,1}], {1, 2}]}, ImageSize -> 200]


TestRaster @ Graphics[
  ColorGradientStyle[
    {Rectangle[{0,0},{1,1}], Rectangle[{1.5,0},{3.5,1}]},
    {1,2}
  ], ImageSize -> 200]


TestRaster @ Graphics[
  ColorGradientStyle[
    {Rectangle[{0,0},{1,1}], Rectangle[{1.5,0},{3.5,1}]},
    {1,2} -> Top
  ], ImageSize -> 200]


TestRaster @ Row @ Table[
  Graphics[ColorGradientStyle[Rectangle[{0,0},{1,1}], ColorGradient[{Black, White}, CompressionFactor -> f]], ImageSize -> 50],
  {f, 0, 1, .1}
]


r = 1;
TestRaster @ Row @ Table[
  Graphics[ColorGradientStyle[Disk[{0,0},r], {Black,White}->side], ImageSize -> 50],
  {side, {Left, TopLeft, Top, TopRight, Right, BottomRight, Bottom, BottomLeft}}
]


TestRaster @ Graphics[PrimitivesGrid[Partition[Table[
  PrimitivesGrid[Table[ColorGradientStyle[Rectangle[{0,0}, {w, h}], {Black,White}->side],
    {w, {1, 2}}, {h, {1, 2}}], {0, 0}, Spacings -> .1, RowAlignments -> {Bottom, Top}, ColumnAlignments -> {Right, Left}],
  {side, {Left, TopLeft, Top, TopRight, Right, BottomRight, Bottom, BottomLeft}}
], UpTo @ 4], {0, 0}, Spacings -> .2], ImageSize -> 800]


TestRaster @ TextIcon["\[FilledSquare]", FontColor -> ColorGradient[{1, 2}, CompressionFactor -> 0.9], FontSize -> 100]


TestRaster @ Row[Join[
  GradientSymbol[#, {1, 2}]& /@ {RightArrowSymbol, BoldRightArrowSymbol},
  GradientSymbol[#, {1, 2} -> Top]& /@ {UpArrowSymbol, BoldUpArrowSymbol},
  {Superscript[BoldRightArrowSymbol, BoldRightArrowSymbol],
   Superscript[GradientSymbol[BoldRightArrowSymbol, {1, 2}], GradientSymbol[BoldRightArrowSymbol, {3, 4}]],
  }
], " ", BaseStyle -> {FontSize -> 100}]