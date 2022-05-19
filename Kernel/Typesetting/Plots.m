PackageExport["FadedMeshImage"]

FadedMeshImage[array_, blockSize_] := Scope[
  fadeFactor = 0.8;
  dims = Dimensions @ array;
  If[!ArrayQ[array, _, Developer`MachineRealQ] || !MatchQ[dims, {_, _, 3} | {_, _}],
    ReturnFailed["baddata"]];
  {h, w} = Take[dims, 2];
  {b1, b2} = blockSize + {If[True, -1, 0], 1}; d = 0;
  {h2, w2} = 1 + {h, w} * b2 - 2d;
  pixels = ConstantArray[0., {3, h2, w2}]; fade = ConstantArray[1., {3, h2, w2}];
  ScanIndexed[paintBlockAdditive, Developer`ToPackedArray @ array, {2}];
  Do[multRow[0.5, r * b2 + 1], {r, 1, h - 1}];
  Do[multCol[0.5, c * b2 + 1], {c, 1, w - 1}];
  Do[Part[fade, All, r * b2 + 1, All] = fadeFactor, {r, 0, h}];
  Do[Part[fade, All, All, c * b2 + 1] = fadeFactor, {c, 0, w}];
  pixels *= fade;
  Image[pixels, Interleaving -> False, ImageSize -> {w2, h2}]
]

paintBlockAdditive[v_, {r_, c_}] := Module[
  {r1 = (r * b2) - d, c1 = (c * b2) - d},
  Part[pixels, All, (r1 - b1)-1 ;; r1+1, (c1 - b1)-1 ;; c1+1] += v;
];

multRow[val_, row_] := Part[pixels, All, row, All] *= val;
multCol[val_, col_] := Part[pixels, All, All, col] *= val;

multFrame[val_, cspec___] := (
  Part[pixels, All, 1, cspec] *= val;
  Part[pixels, All, w2, cspec] *= val;
  Part[pixels, 1, All, cspec] *= val;
  Part[pixels, h2, All, cspec] *= val;
);

(**************************************************************************************************)

PackageExport["MeshImage"]

FadedMeshImage::baddata = MeshImage::baddata = "Data should be an w * h * d array of real numbers."

Options[MeshImage] = {
  Frame -> True, Mesh -> True,
  FrameStyle -> GrayLevel[0.4],
  MeshStyle -> GrayLevel[0.4]
};

MeshImage[array_, blockSize_, OptionsPattern[]] := Scope[
  UnpackOptions[frame, mesh, frameStyle, meshStyle];
  frameStyle //= getFrameMeshColor;
  meshStyle //= getFrameMeshColor;
  {b1, b2} = blockSize + {If[mesh, -1, 0], 1};
  dims = Dimensions @ array;
  If[!ArrayQ[array, _, Developer`MachineRealQ] || !MatchQ[dims, {_, _, 3} | {_, _}],
    ReturnFailed["baddata"]];
  hasColor = Length[dims] == 3;
  {h, w} = Take[dims, 2];
  d = If[frame, 0, 1];
  {h2, w2} = 1 + {h, w} * b2 - 2d;
  pixels = ConstantArray[frameStyle, If[hasColor, {h2, w2, 3}, {h2, w2}]];
  If[frame, If[hasColor, paintFrame[All], paintFrame[]]];
  ScanIndexed[paintBlock, Developer`ToPackedArray @ array, {2}];
  Image[pixels, ImageSize -> {w2, h2}]
];

getFrameMeshColor = Case[
  GrayLevel[n_] :=  N @ n;
  _ :=              Return[$Failed, Block];
]

paintFrame[cspec___] := (
  Part[pixels, All, 1, cspec] = meshStyle;
  Part[pixels, All, w2, cspec] = meshStyle;
  Part[pixels, 1, All, cspec] = meshStyle;
  Part[pixels, h2, All, cspec] = meshStyle;
);

paintBlock[v_, {r_, c_}] := Module[
  {r1 = (r * b2) - d, c1 = (c * b2) - d},
  Part[pixels, (r1 - b1) ;; r1, (c1 - b1) ;; c1] = v;
];

(**************************************************************************************************)

PackageExport["CompactArrayPlot"]
PackageExport["ColorLegend"]

Options[CompactArrayPlot] = {
  PixelConstrained -> 4,
  ColorFunction -> Automatic,
  ColorLegend -> None,
  Frame -> True,
  Mesh -> True
};

CompactArrayPlot::badrank = "Array should be of rank 2 or 3, but had rank ``.";
CompactArrayPlot::rank3chans = "Rank 3 array should have 3 channels.";
CompactArrayPlot::rank3vals = "Rank 3 array should be numeric in interval [0, 1].";
CompactArrayPlot::interr = "Internal error while processing array.";
CompactArrayPlot::badcvals = "ColorFunction produced non-RGB values, first was: ``.";

CompactArrayPlot[array_, OptionsPattern[]] := Scope[
  UnpackOptions[pixelConstrained, colorFunction, colorLegend, frame, mesh];
  array //= ToPackedReal;
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
    Developer`PackedArrayQ[array, Real] && UnitIntervalArrayQ[array],
      None,
    Developer`PackedArrayQ[array, Integer] && UnitIntervalArrayQ[array],
      None,
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
    If[!PackedArrayQ[array], ReturnFailed["badcvals"]];
  ];
  If[!PackedArrayQ[array], ReturnFailed["interr"]];
  graphics = MeshImage[array, pixelConstrained, Frame -> frame, Mesh -> mesh];
  SetAutomatic[colorLegend, colorFunction];
  If[colorLegend =!= None, graphics //= ApplyLegend[colorLegend]];
  graphics
];

stripFinalRGB[RightComposition[fns___, RGBColor]] := RightComposition[fns];
stripFinalRGB[other_] := other;

(**************************************************************************************************)

PackageExport["BinaryArrayPlot"]

Options[BinaryArrayPlot] = {
  PixelConstrained -> 4
}

BinaryArrayPlot[array_, opts:OptionsPattern[]] :=
  BinaryArrayPlot[array, Automatic, opts];

BinaryArrayPlot[array_, digits:(_Integer|Automatic), OptionsPattern[]] := Scope[
  UnpackOptions[pixelConstrained];
  {min, max} = MinMax @ array;
  Which[
    VectorQ[array, Internal`NonNegativeIntegerQ],
      SetAutomatic[digits, If[max == 0, 0, Floor[1 + Log2 @ max]]];
      array = BinaryDigits[array, digits];
    ,
    MatrixQ[array, Internal`NonNegativeIntegerQ],
      If[IntegerQ[digits] && InnerDimension[array] > digits,
        array = Take[array, All, digits]];
      If[max > 1, ReturnFailed[]];
    ,
    True,
      ReturnFailed[];
  ];
  CompactArrayPlot[1 - array, PixelConstrained -> pixelConstrained]
];
