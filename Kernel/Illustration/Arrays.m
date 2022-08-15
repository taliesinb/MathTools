PublicFunction[NeutralGraphics3D]

NeutralGraphics3D[prims_, opts___] := Graphics3D[
  {EdgeForm @ AbsoluteThickness[3], FaceForm @ GrayLevel[0.9], EdgeForm @ GrayLevel[0.5], prims},
  opts, Boxed -> False, Lighting -> AmbientLight[White], ImageSize -> 300,
  ViewProjection -> "Orthographic",
  ViewPoint -> {-2, -1.5, 2.5},
  ViewVertical -> {0, 0, 1}

]

(**************************************************************************************************)

$lhsP = _Integer | _Span | All;
$lhsSpecP = {$lhsP..} | {Rule[_Integer, $lhsP]..} | Rule[_Integer, $lhsP];
$lhsSpecP2 = $lhsSpecP | {___, _ -> Each, ___} | {___, Each, ___};

procHighlightSpec = Case[
  lhs:$lhsSpecP2 := % @ List @ lhs;
  other_ := Map[procHighlightRule, Developer`ToList @ other];
];

procHighlightRule = Case[
  lhs:{___, _ -> Each, ___} := MapTuples[procHighlightRule, VectorReplace[lhs, {(i_Integer -> Each) :> Thread[i -> Range[Part[$dims, i]]], other_ :> List[other]}]];
  lhs:{___, Each, ___} := MapTuples[procHighlightRule, MapIndex1[If[#1 === Each, Range @ Part[$dims, #2], List @ #1]&, lhs]];
  lhs:$lhsSpecP := % @ Rule[lhs, Automatic];
  Rule[lhs_, col_ /; ColorQ[col] || col === Automatic] := procHighlightLHS[Developer`ToList @ lhs] -> ReplaceAutomatic[col, indexedColor[$count++]];
];

procHighlightLHS = Case[
  specList:{$lhsP..} := If[Length[specList] =!= Length @ $dims,
    QuiverGeometry`PackageScope`ThrowMessage["speccount"],
    Map[procAxisSpec, specList]
  ];
  specRules:{Rule[_Integer, $lhsP]..} := Scope[
    spec = ConstantArray[_, Length @ $dims];
    Set[spec[[#1]], procAxisSpec[#2]]& @@@ specRules;
    spec
  ]
]

procAxisSpec = Case[
  i_Integer := i;
  spec_Span := Alternatives @@ Range @@ spec;
  All := _;
];

$pallete = $ColorPalette;
highlightSpecToRules[dims_, spec_] := Scope[
  $dims = dims; $count = 0;
  rules = Flatten @ procHighlightSpec @ spec;
  Which[
    $count == 0,        Null,
    1 <= $count <= 8,   rules = rules /. indexedColor[i_] :> Part[$ColorPalette, i + 1],
    True,               rules = rules /. indexedColor[i_] :> OklabHue[0.6 * i / $count]
  ];
  rules
]

(**************************************************************************************************)

makeCube[pos_] := Annotation[CenteredCuboid[PadRight[pos, 3, 1], 1-$cubeGap], pos];

toCubePos[e_] := ReplaceAll[e, c:(_Cuboid | _CenteredCuboid) :> ReplaceAll[c, {x_ ? NumberQ, y_ ? NumberQ, z_ ? NumberQ} :> {-x, y, -z}]];

(**************************************************************************************************)

PublicFunction[CubeArray]

PublicOption[CubeGap, CubeStyle]

Options[CubeArray] = {CubeGap -> 0.2, CubeStyle -> Automatic};

$defaultCubeStyle = Directive[FaceForm[GrayLevel[0.9,.3]], EdgeForm[{AbsoluteThickness[1], GrayLevel[0.5,.3]}]];

CubeArray[dims_, OptionsPattern[]] := Scope[
  UnpackOptions[$cubeGap, cubeStyle];
  SetAutomatic[cubeStyle, $defaultCubeStyle];
  Style[
    toCubePos @ Array[List /* makeCube, dims],
    FaceEdgeForm[cubeStyle]
  ]
];

(**************************************************************************************************)

PublicFunction[ColoredCubeArray]

Options[ColoredCubeArray] = Options[CubeArray];

ColoredCubeArray[cfunc_, dims_, OptionsPattern[]] := Scope[
  UnpackOptions[$cubeGap, cubeStyle];
  SetAutomatic[cubeStyle, $defaultCubeStyle];
  toCubePos @ Array[
    Function[
      c = Replace[cfunc[{##}], Automatic | None -> cubeStyle];
      Style[makeCube[{##}], FaceEdgeForm @ c]
    ],
    dims
  ]
];
  
ColoredCubeArray[spec:(_List | _Rule), dims_, opts:OptionsPattern[]] :=
  ColoredCubeArray[
    Replace[Append[_ -> None] @ highlightSpecToRules[dims, spec]],
    dims,
    opts
  ];

(**************************************************************************************************)

PublicFunction[SpannedCubeArray]

PublicOption[SpanGap, HideHighlighted]

Options[SpannedCubeArray] = {
  CubeGap -> 0.2, SpanGap -> 0.2, CubeStyle -> Automatic,
  HideHighlighted -> True
};

SpannedCubeArray[spec:(_List | _Rule), dims_, OptionsPattern[]] := Scope[
  UnpackOptions[cubeGap, spanGap, cubeStyle, hideHighlighted];
  array = CubeArray[dims, CubeGap -> cubeGap, CubeStyle -> cubeStyle];
  rules = highlightSpecToRules[dims, spec];
  spans = makeSpanningCuboid @@@ rules;
  If[hideHighlighted, array = array /. Annotation[_, Alternatives @@ Keys[rules]] -> {}];
  {array, spans}
,
  makeSpanningCuboid[pattern_, color_] := Scope[
    matches = DeepCases[array, Annotation[CenteredCuboid[pos_, sz_], pattern] :> pos];
    bounds = CoordinateBoundingBox[matches, 0.5 - spanGap/2];
    Style[Cuboid @@ bounds, FaceEdgeForm @ color]
  ]
];

(**************************************************************************************************)

PublicFunction[CubeArrayAxes]

PublicOption[FlagOrientation]

CubeArrayAxes::badlabelpos = "Unrecognized LabelPosition ``.";

Options[CubeArrayAxes] = {
  LabelPosition -> "Near",
  InsetScale -> 1/144,
  BaseStyle -> {},
  FontSize -> 16
};

CubeArrayAxes[origin_, dims_, {lx_, ly_, lz_}, OptionsPattern[]] := Scope[
  UnpackOptions[labelPosition, insetScale, baseStyle, fontSize];
  AppendTo[baseStyle, FontSize -> fontSize];
  spec = Lookup[cubeArrayAxesOrient, labelPosition, ReturnFailed["badlabelpos", orient]];
  MapThread[
    CubeEdgeText[#1, origin, dims, Seq @@ #2, InsetScale -> insetScale, BaseStyle -> baseStyle]&,
    {{lx, ly, lz}, spec}
  ]
];

cubeArrayAxesOrient = <|
  "XZY"    -> {{{3, 1, 2}, {0,0,0}}, {{1, 2, 3}, {1,0,1}}, {{1, 3, 2}, {1,0,0}}},
  "YZX"    -> {{{3, 1, 2}, {1,0,1}}, {{3, 2, 1}, {0,0,0}}, {{1, 3, 2}, {0,0,1}}},
  "ZXY"    -> {{{3, 1, 2}, {1,0,1}}, {{3, 2, 1}, {1,1,1}}, {{1, 3, 2}, {0,0,1}}},
  "XYZ"    -> {{{3, 1, 2}, {1,0,1}}, {{3, 2, 1}, {1,1,1}}, {{1, 3, 2}, {1,1,0}}},
  "ZYX"    -> {{{3, 1, 2}, {1,1,1}}, {{3, 2, 1}, {1,0,1}}, {{1, 3, 2}, {1,0,0}}},
  "Near"   -> {{{3, 1, 2}, {0,0,0}}, {{3, 2, 1}, {0,0,0}}, {{1, 3, 2}, {0, 0, {Above, 0}}}},
  "Far"    -> {{{3, 1, 2}, {1,1,1}}, {{3, 2, 1}, {1,1,1}}, {{1, 3, 2}, {1, 1, {Below, 1.02}}}}
|>;

$CubeAxesStyle = FontWeight -> Bold;

(**************************************************************************************************)

PublicFunction[CubeEdgeText]

faceNameToIndex = Case[
  {"YZ", Top}    := {{1, 2, 3}, {0, 1, 1}};
  {"YZ", Bottom} := {{1, 2, 3}, {0, 1, 0}};
  {"YZ", Left}   := {{1, 3, 2}, {0, 0, 1}};
  {"YZ", Right}  := {{1, 3, 2}, {0, 0, 0}};

  {"XY", Top}    := {{3, 1, 2}, {1, 0, 1}};
  {"XY", Bottom} := {{3, 1, 2}, {1, 0, 0}};
  {"XY", Left}   := {{3, 2, 1}, {1, 1, 0}};
  {"XY", Right}  := {{3, 2, 1}, {1, 1, 1}};

  {"XZ", Top}    := {{2, 1, 3}, {0, 0, 1}};
  {"XZ", Bottom} := {{2, 1, 3}, {0, 0, 0}};
  {"XZ", Left}   := {{2, 3, 1}, {0, 1, 0}};
  {"XZ", Right}  := {{2, 3, 1}, {0, 1, 1}};
];

Options[CubeEdgeText] = {FlipX -> Automatic, FlipY -> False, InsetScale -> 1/144, BaseStyle -> {}};

CubeEdgeText[text_, origin_, dims_, faceName_String, pos_Symbol] :=
  CubeEdgeText[text, origin, dims, Seq @@ faceNameToIndex[{faceName, pos}]];

CubeEdgeText[text_, origin_, dims_, indices_, positions_:{0,0,0}, opts:OptionsPattern[]] := Scope[
  origin2 = origin - dims * 0.001; dims2 = dims * 1.002;
  cubeText0[text, origin2, Threaded[origin] + DiagonalMatrix[dims2], indices, positions, opts]
];
  
cubeText0[text_, origin_, axisVectors:{_, _, _}, {i_, j_, k_}, {s_, t_, p_}, opts___] :=
  cubeText1[text, origin + Part[axisVectors, i] * s, Part[axisVectors, {j, k}], {t, p}, opts]
  
cubeText1[text_, origin_, {dx_, dy_}, {fx_, fy_}, opts___] :=
  PlaneInset[text, origin + dx * toRat[fx] + dy * toRat[fy], {dx, dy}, {1, -1} * Map[unit2biunit, {fx, fy}], opts];
  
toRat[{Above|Below, x_}] := x;
toRat[x_] := x;

unit2biunit[{Above, _}] := 1;
unit2biunit[{Below, _}] := -1;
unit2biunit[x_] := 2x - 1;

