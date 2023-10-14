PublicFunction[CompactMatrixBox]

$compactMatrixOptions = JoinOptions[
  $compactNumberOptions,
  ItemSize -> Automatic, FrameStyle -> GrayLevel[0.85], "Factor" -> True, "HideZeros" -> True
];

Options[CompactMatrixBox] = $compactMatrixOptions;

expandItemSize[Automatic, matrix_] := {0.65, 0.2};

expandItemSize[num_ ? NumericQ, _] := {N @ num, 0.3};
expandItemSize[num:{_ ? NumericQ, _ ? NumericQ}, _] := {num, num};
expandItemSize[_, _] := {0.6, 0.3};

CompactMatrixBox[{{}}, ___] := "";

CompactMatrixBox[matrix_, OptionsPattern[]] := Scope[
  UnpackOptions[negationStyle, inversionStyle, itemSize, frameStyle, factor, hideZeros];

  blockNumberFormatting[CompactMatrixBox, {negationStyle, inversionStyle},
    $zero = If[hideZeros, $grayDot, "0"];
    iCompactMatrixBox[matrix, itemSize, frameStyle, factor]
  ]
];

iCompactMatrixBox[matrix_, itemSize_, frameStyle_, shouldFactor_] := Scope[
  {matrix, factor} = If[shouldFactor && Dimensions[matrix] =!= {1, 1} &&
    Min[Abs[ExpandUnitRoots[matrix] /. ModForm[m_, _] :> m]] > 0,
    MatrixSimplify[matrix], {matrix, 1}];
  entries = MatrixMap[numBox, matrix] // simplifyNumBoxes;
  matrixBoxes = matrixGridBoxes[entries, expandItemSize[itemSize, matrix], frameStyle];
  If[factor === 1, matrixBoxes,
    RowBox[{matrixBoxes, numBox[factor] // simplifyNumBoxes}]
  ]
];

matrixGridBoxes[entries_, {w_, h_}, frameStyle_] := GridBox[entries,
  GridBoxFrame -> {"ColumnsIndexed" -> {{{1, -1}, {1, -1}} -> True}},
  GridBoxAlignment -> {"Columns" -> {{Center}}},
  GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}},
  GridBoxSpacings -> {"Columns" -> {{w}}, "Rows" -> {0.7, {h}, 0.1}},
  BaseStyle -> {FontFamily -> "Source Code Pro", FontSize -> 12, TextAlignment -> Left},
  FrameStyle -> frameStyle
];

(**************************************************************************************************)

PublicTypesettingForm[CompactMatrixForm]

SetUsage @ "
CompactMatrixForm
";

Options[CompactMatrixForm] = Options[makeCompactMatrixFormBoxes] = $compactMatrixOptions;

DefineStandardTraditionalForm[
  CompactMatrixForm[e_, opts___Rule] :> makeCompactMatrixFormBoxes[e, opts]
];

makeCompactMatrixFormBoxes[e_, opts:OptionsPattern[]] := Scope[

  {negationStyle, inversionStyle, itemSize, frameStyle, factor, hideZeros} =
    OptionValue[CompactMatrixForm, {opts}, {NegationStyle, InversionStyle, ItemSize, FrameStyle, "Factor", "HideZeros"}];

  blockNumberFormatting[CompactMatrixForm, {negationStyle, inversionStyle},
    $zero = If[hideZeros, $grayDot, "0"];
    If[MatrixQ[Unevaluated[e]],
      held = List @ RawBoxes @ iCompactMatrixBox[e, itemSize, frameStyle, factor];
    ,
      held = Hold[e] /. m_List /; MatrixQ[Unevaluated[m]] :>
        RuleCondition[RawBoxes @ iCompactMatrixBox[m, itemSize, frameStyle, factor]];
    ]
  ];

  MakeBoxes @@ held
];

(**************************************************************************************************)

PrivateFunction[renderRepresentationMatrix]

renderRepresentationMatrix[matrix_, isTraditional_:False, opts___] :=
  RawBoxes @ CompactMatrixBox[matrix, opts, NegationStyle -> "Color", InversionStyle -> None];

(**************************************************************************************************)

PublicTypesettingForm[LabeledMatrixForm]

declareFormatting[
  LabeledMatrixForm[expr_] :> formatLabeledMatrices[expr]
];

formatLabeledMatrices[expr_] := ReplaceAll[expr,
  matrix_ /; MatrixQ[Unevaluated @ matrix] /; Length[Unevaluated @ matrix] =!= 1 :> RuleCondition @ formatLabeledMatrix @ matrix
]

formatLabeledMatrix[matrix_] := Scope[
  tooltips = MapIndexed[Tooltip, matrix, {2}];
  MatrixForm[tooltips, TableHeadings -> Automatic]
];
