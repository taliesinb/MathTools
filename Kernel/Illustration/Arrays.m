PublicFunction[NeutralGraphics3D]

NeutralGraphics3D[prims_, opts___] := Graphics3D[
  {Opacity[0.5], EdgeForm @ AbsoluteThickness[3], FaceForm @ GrayLevel[0.9], EdgeForm @ GrayLevel[0.5], prims},
  opts, Boxed -> False, Lighting -> AmbientLight[White], ImageSize -> 300,
  ViewProjection->"Orthographic"
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

