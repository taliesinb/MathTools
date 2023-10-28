PublicFunction[FadedMeshImage]

FadedMeshImage[array_, blockSize_, fadeFactor_:0.8, frame_:None] := Scope[
  dims = Dimensions @ array;
  If[!ArrayQ[array, _, MachineRealQ] || !MatchQ[dims, {_, _, 3} | {_, _}],
    ReturnFailed["baddata"]];
  {h, w} = Take[dims, 2];
  {b1, b2} = blockSize + {If[True, -1, 0], 1}; d = 0;
  {h2, w2} = 1 + {h, w} * b2 - 2d;
  pixels = Repeat[0., {3, h2, w2}]; fade = Repeat[1., {3, h2, w2}];
  ScanIndexed[paintBlockAdditive, ToPackedArray @ array, {2}];
  Do[multRow[0.5, r * b2 + 1], {r, 1, h - 1}];
  Do[multCol[0.5, c * b2 + 1], {c, 1, w - 1}];
  Do[Part[fade, All, r * b2 + 1, All] = fadeFactor, {r, 0, h}];
  Do[Part[fade, All, All, c * b2 + 1] = fadeFactor, {c, 0, w}];
  pixels *= fade;
  If[frame =!= None,
    frameStyle = getFrameMeshColor @ frame;
    Part[pixels, All, All, 1] = frameStyle;
    Part[pixels, All, All, w2] = frameStyle;
    Part[pixels, All, 1, All] = frameStyle;
    Part[pixels, All, h2, All] = frameStyle;
  ];
  Image[pixels, Interleaving -> False, ImageSize -> {w2, h2}]
]

paintBlockAdditive[v_, {r_, c_}] := Module[
  {r1 = (r * b2) - d, c1 = (c * b2) - d},
  Part[pixels, All, (r1 - b1)-1 ;; r1+1, (c1 - b1)-1 ;; c1+1] += v;
];

multRow[val_, row_] := Part[pixels, All, row, All] *= val;
multCol[val_, col_] := Part[pixels, All, All, col] *= val;

(**************************************************************************************************)

PublicFunction[MeshImage]

FadedMeshImage::baddata = MeshImage::baddata = "Data should be an w * h * d array of real numbers."

Options[MeshImage] = {
  Frame -> True, Mesh -> True,
  FrameStyle -> GrayLevel[0.4],
  MeshStyle -> GrayLevel[0.4]
};

MeshImage[array_, blockSize_, OptionsPattern[]] := Scope[
  UnpackOptions[frame, mesh, frameStyle, meshStyle];
  If[MatchQ[meshStyle, Opacity[_]] && TrueQ[mesh],
    Return @ FadedMeshImage[array, blockSize, 1 - First[meshStyle], If[TrueQ @ frame, frameStyle, None]]];
  frameStyle //= getFrameMeshColor;
  meshStyle //= getFrameMeshColor;
  dims = Dimensions @ array;
  If[!ArrayQ[array, _, MachineRealQ] || !MatchQ[dims, {_, _, 3} | {_, _}],
    ReturnFailed["baddata"]];
  {h, w} = Take[dims, 2];
  If[!mesh,
    image = ImageResize[Image[array], blockSize * w];
    If[frame, image = ImagePad[image, 1, frameStyle]];
    Return @ Image[image, ImageSize -> {{w, h} * blockSize + If[frame, 2, 0]}];
  ];
  {b1, b2} = blockSize + {-1, 1};
  hasColor = Length[dims] == 3;
  d = If[frame, 0, 1];
  {h2, w2} = 1 + {h, w} * b2 - 2d;
  pixels = ToPackedReal @ Repeat[N @ meshStyle, If[hasColor, {h2, w2, 3}, {h2, w2}]];
  If[frame, If[hasColor, paintFrame[All], paintFrame[]]];
  pixels //= ToPackedReal;
  ScanIndexed[If[hasColor && b1 == 2, paintBlockSafe, paintBlock], ToPackedArray @ array, {2}];
  Image[pixels, ImageSize -> {w2, h2}]
];

getFrameMeshColor = Case[
  GrayLevel[n_]   := N @ n;
  Opacity[_]      := 0;
  _               := Return[$Failed, Block];
]

paintFrame[cspec___] := (
  Part[pixels, All, 1, cspec] = frameStyle;
  Part[pixels, All, w2, cspec] = frameStyle;
  Part[pixels, 1, All, cspec] = frameStyle;
  Part[pixels, h2, All, cspec] = frameStyle;
);

(* because otherwise the slice of size 3 gets matched with the size 3 of v *)
paintBlockSafe[v_, {r_, c_}] := Module[
  {r1 = (r * b2) - d, c1 = (c * b2) - d, p1, p2},
  p1 = (r1 - b1) ;; r1; p2 = (c1 - b1) ;; c1;
  Part[pixels, p1, p2, 1] = Part[v, 1];
  Part[pixels, p1, p2, 2] = Part[v, 2];
  Part[pixels, p1, p2, 3] = Part[v, 3];
];

paintBlock[v_, {r_, c_}] := Module[
  {r1 = (r * b2) - d, c1 = (c * b2) - d},
  Part[pixels, (r1 - b1) ;; r1, (c1 - b1) ;; c1] = v;
];

(**************************************************************************************************)

PublicFunction[CompactArrayPlot, ColorLegend]

Options[CompactArrayPlot] = {
  PixelConstrained -> 4,
  ColorFunction -> Automatic,
  ColorLegend -> None,
  Frame -> True,
  Mesh -> True,
  MeshStyle -> GrayLevel[0.8]
};

CompactArrayPlot::badrank = "Array should be of rank 2 or 3, but had rank ``.";
CompactArrayPlot::rank3chans = "Rank 3 array should have 3 channels.";
CompactArrayPlot::rank3vals = "Rank 3 array should be numeric in interval [0, 1].";
CompactArrayPlot::interr = "Internal error while processing array.";
CompactArrayPlot::badcvals = "ColorFunction produced non-RGB values, first was: ``.";

CompactArrayPlot[array_, OptionsPattern[]] := Scope[
  UnpackOptions[pixelConstrained, colorFunction, colorLegend, frame, mesh, meshStyle];
  array //= ToPacked;
  dims = Dimensions @ array; ndims = Length @ dims;
  If[array === {} || MemberQ[dims, 0], Return[Spacer[1]]];
  If[ndims < 2 || ndims > 3, ReturnFailed["badrank", ndims]];
  isRGB = ndims === 3;
  If[isRGB,
    If[Last[dims] =!= 3, ReturnFailed["badchans"]];
    If[!PackedArrayQ[array] || !UnitIntervalArrayQ[array], ReturnFailed["rank3vals"]];
  ];
  SetAutomatic[colorFunction, Which[
    isRGB,
      None,
    PackedArrayQ[array, Real] && UnitIntervalArrayQ[array],
      None,
    PackedArrayQ[array, Integer] && UnitIntervalArrayQ[array],
      None,
    PackedArrayQ[array, Complex] || ContainsComplexQ[array],
      ComplexHue,
    ArrayQ[array, 2, ColorQ],
      RGBColor,
    True,
      $BooleanColors = {White, Black};
      Last @ ApplyColoring @ Catenate @ array
  ]];
  If[colorFunction =!= None,
    cfunc = colorFunction;
    If[ColorFunctionObjectQ @ cfunc, cfunc //= Normal];
    cfunc //= stripFinalRGB;
    array = ToPackedReal @ MatrixMap[cfunc, array];
    If[ArrayQ[array, 2, ColorQ],
      array = ToPackedReal @ ToRGB @ array];
    If[!PackedArrayQ[array], ReturnFailed["badcvals", MsgExpr[SelectFirst[array, !MatchQ[#, $Coord3P]&], 20, 200]]];
  ];
  If[!PackedArrayQ[array], ReturnFailed["interr"]];
  graphics = MeshImage[array, pixelConstrained, Frame -> frame, Mesh -> mesh, MeshStyle -> meshStyle];
  SetAutomatic[colorLegend, colorFunction];
  If[colorLegend =!= None, graphics //= ApplyLegend[colorLegend]];
  graphics
];

stripFinalRGB[RightComposition[fns___, RGBColor]] := RightComposition[fns];
stripFinalRGB[other_] := other;

(**************************************************************************************************)

PublicFunction[BinaryArrayPlot]

Options[BinaryArrayPlot] = {
  PixelConstrained -> 4
}

BinaryArrayPlot[array_, opts:OptionsPattern[]] :=
  BinaryArrayPlot[array, Automatic, opts];

BinaryArrayPlot[array_, digits:(_Integer|Automatic), OptionsPattern[]] := Scope[
  UnpackOptions[pixelConstrained];
  {min, max} = MinMax @ array;
  Which[
    VectorQ[array, NonNegativeIntegerQ],
      SetAutomatic[digits, If[max == 0, 0, Floor[1 + Log2 @ max]]];
      array = BinaryDigits[array, digits];
    ,
    MatrixQ[array, NonNegativeIntegerQ],
      If[IntegerQ[digits] && InnerDimension[array] > digits,
        array = Take[array, All, digits]];
      If[max > 1, ReturnFailed[]];
    ,
    True,
      ReturnFailed[];
  ];
  CompactArrayPlot[1 - array, PixelConstrained -> pixelConstrained]
];
