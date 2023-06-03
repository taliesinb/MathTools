PublicFunction[NeutralGraphics3D]

NeutralGraphics3D[prims_, opts___] := Graphics3D[
  {EdgeForm @ AbsoluteThickness[3], FaceForm @ GrayLevel[0.9], EdgeForm @ GrayLevel[0.5], stripG3D @ prims},
  opts, Boxed -> False, Lighting -> {DirectionalLight[GrayLevel[0.6],{10,-10,10}], AmbientLight[White]}, ImageSize -> 300,
  ViewProjection -> "Orthographic",
  ViewPoint -> {-2, -1.5, 2.5},
  ViewVertical -> {0, 0, 1},
  BaseStyle -> {RenderingOptions -> {"3DRenderingEngine" -> "OpenGL", "3DRenderingMethod" -> "BSPTree"}},
  Method -> {"ShrinkWrap" -> True}
]
stripG3D = Case[
  l_List := Map[%, l];
  Graphics3D[g_, ___] := g;
  e_ := e;
];

(**************************************************************************************************)

PublicForm[PixelForm]

DefineStandardTraditionalForm[{
  PixelForm[{r_, g_, b_}] :> StyleBox["\[FilledSquare]", FontColor -> RGBColor[r, g, b]],
  PixelForm[g_ ? NumberQ] :> StyleBox["\[FilledSquare]", FontColor -> GrayLevel[g]]
}];

(**************************************************************************************************)

PublicForm[FadedRationalForm]

makeFade[r_] := GrayLevel[(1 - r)/1.5];

DefineStandardTraditionalForm[{
  FadedRationalForm[0]   :> StyleBox["0", FontColor -> makeFade[0]],
  FadedRationalForm[1/2] :> StyleBox["½", FontColor -> makeFade[1/2]],
  FadedRationalForm[1]   :> StyleBox["1", FontColor -> makeFade[1]],
  FadedRationalForm[n_]  :> IntegerString[n],
  FadedRationalForm[3/4] :> StyleBox["¾", FontColor -> makeFade[3/4]],
  FadedRationalForm[1/4] :> StyleBox["¼", FontColor -> makeFade[1/4]],
  FadedRationalForm[1/3] :> StyleBox["⅓", FontColor -> makeFade[1/3]],
  FadedRationalForm[2/3] :> StyleBox["⅔", FontColor -> makeFade[2/3]]
}];

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
    True,               rules = rules /. indexedColor[i_] :> OkHue[0.6 * i / $count]
  ];
  rules
]

(**************************************************************************************************)

$defaultItemFunction = Cube[#, 1-$cubeGap]&;

makeCube[dims_, pos_] := Scope[
  coords = PadRight[pos-.5, 3, 0] - dims/2;
  style = If[MatchQ[$itemStyleFunction, None|Automatic], Automatic, $itemStyleFunction @ pos];
  Annotation[applyStyle[$itemFunction[coords, pos], style], AnnotatedCoordinate[coords, pos]]
];

applyStyle[e_, Automatic] := e;
applyStyle[e_, None] := {};
applyStyle[e_, Transparent] := Style[e, FaceEdgeForm[Transparent]];
applyStyle[e_, o:Opacity[_, _]] := Style[e, FaceEdgeForm[o]];
applyStyle[e_, c_ ? ColorQ] := Style[e, FaceEdgeForm[c]];
applyStyle[e_, s_] := Style[e, Seq @@ ToList[s]];

(**************************************************************************************************)

resolveFlip[opts___] :=
  resolveFlip1 @@ OptionValue[ColoredCubeArray, {opts}, {FlipAxes, FlipX, FlipY, FlipZ}];

resolveFlip1[spec_String, args__] :=
  resolveFlip1[Characters @ ToUpperCase @ spec, args];

resolveFlip1[spec_List, flipx_, flipy_, flipz_] :=
  If[#, -1, 1]& /@ MapThread[
    If[ContainsQ[spec, #1], !#2, #2]&,
    {{1|"X", 2|"Y", 3|"Z"}, {flipx, flipy, flipz}}
  ];

resolveFlip1[{} | None, flipx_, flipy_, flipz_] :=
  If[#, -1, 1]& /@ {flipx, flipy, flipz}

(**************************************************************************************************)

multXYZ[{1,1,1}][e_] := e;
multXYZ[xyz_][e_] := ScalePrimitives[e, xyz];

(**************************************************************************************************)

attachLabelAxes[dims_, None|False, _] := Identity;

attachLabelAxes[dims_, True, opts_] := attachLabelAxes[dims, Automatic, opts];

attachLabelAxes[dims_, Placed[spec_, pos_String], opts_] := attachLabelAxes[dims, spec, Prepend[opts, LabelPosition -> pos]];

attachLabelAxes[dims_, Automatic, opts_] :=
  attachLabelAxes[dims, If[# > 1, Automatic, None]& /@ dims, opts];

attachLabelAxes[dims_, {lx_, ly_, lz_}, opts_][e_] := Scope[
  SetAutomatic[lx, 1]; SetAutomatic[ly, 2]; SetAutomatic[lz, 3];
  axes = CubeAxes[-dims/2, dims, {lx, ly, lz}, Seq @@ opts];
  {e, axes}
]

(**************************************************************************************************)

PublicFunction[CubeArray, CubeGrid]

PublicOption[CubeGap, CubeStyle, FlipAxes, LabelAxes, AxesOptions, CubeOpacity, SpanHighlights, ItemStyleFunction, FlipTicks]

Options[CubeGrid] = Options[CubeArray] = {
  CubeGap -> 0.2, CubeStyle -> Automatic, FlipAxes -> None, FlipX -> False, FlipY -> False, FlipZ -> False,
  LabelAxes -> False, AxesOptions -> {}, CubeOpacity -> .3, SpanGap -> 0.2, SpanHighlights -> {},
  HighlightStyle -> {}, MeshStyle -> None, ItemFunction -> Automatic, ItemStyleFunction -> None,
  TicksStyle -> Automatic, TickSpacing -> 0.1, Ticks -> None, FlipTicks -> {}, FrameStyle -> None,
  PlotLabel -> None
};

CubeGrid[args___] := CubeArray[args, CubeGap -> 0, CubeStyle -> None, MeshStyle -> Automatic];

$defaultCubeStyle := Directive[FaceForm[GrayLevel[0.9]], EdgeForm[{AbsoluteThickness[1], GrayLevel[0.5]}]];

$defaultMeshStyle = Directive[AbsoluteThickness[3], GrayLevel[0.5,0.1]];

$defaultTicksStyle = {FontSize -> 18, FontColor -> $Gray, FontFamily -> "Avenir"};

CubeArray[dims_List, opts:OptionsPattern[]] := Scope[
  
  UnpackOptions[
    $cubeGap, cubeStyle,
    labelAxes, axesOptions, meshStyle,
    cubeOpacity,
    spanGap, spanHighlights, highlightStyle,
    $itemFunction, $itemStyleFunction,
    frameStyle,
    plotLabel
  ];
  
  SetAutomatic[$itemFunction, $defaultItemFunction];
  SetAutomatic[cubeStyle, $defaultCubeStyle];
  SetAutomatic[meshStyle, $defaultMeshStyle];

  attachPlotLabel[dims, plotLabel] @ 
  ReplaceAll[Annotation[a_, _] :> a] @
  attachLabelAxes[dims, labelAxes, axesOptions] @
  attachSpanHighlights[dims, spanHighlights, False] @
  attachTicks[dims, opts] @
  multXYZ[resolveFlip[opts]] @
  attachMesh[dims, meshStyle, frameStyle] @
  If[cubeStyle === $itemStyleFunction === None && $itemFunction === $defaultItemFunction, {},
    Style[
      Array[makeCube[dims, {##}]&, dims],
      FaceEdgeForm[cubeStyle],
      Opacity[cubeOpacity]
    ]
  ]
];

$defaultLabelStyle = {FontSize -> 30, FontColor -> $Gray, FontFamily -> "Avenir"};

attachPlotLabel[_, None][e_] := e;
attachPlotLabel[dims_, label_][e_] := {e, PlaneInset[label, -dims/2 - {2, 1.5, 0}/10, "Screen", {0, 1.1}, BaseStyle -> $defaultLabelStyle]};

attachMesh[dims_, None, None][e_] := e;
attachMesh[dims_, style_, None][e_] := {Style[MeshLines3D[-dims/2, dims, FrameStyle -> None, MeshStyle -> style], AbsoluteThickness[1]], e};

attachMesh[dims_, style_, fstyle_][e_] := {Style[MeshLines3D[-dims/2, dims, FrameStyle -> fstyle, MeshStyle -> style], AbsoluteThickness[1]], e};

attachTicks[dims_, opts:OptionsPattern[CubeArray]][e_] := Scope[
  UnpackOptions[ticksStyle, tickSpacing, ticks, flipTicks];
  SetAutomatic[ticksStyle, $defaultTicksStyle];
  If[ticks === None, Return @ e];
  center = -dims/2;
  isFlipped = resolveFlip[opts];
  tickItems = Map[makeTickItems, ToList @ ticks];
  cg = $cubeGap / 2; sp = tickSpacing;
  {$maxx, $maxy, $maxz} = dims - cg;
  {$minx, $miny, $minz} = {0, 0, 0} + cg;
  {e, tickItems}
,
  makeTickItems[list_List] := Map[makeTickItems, list],
  makeTickItems[Style[p_, rest___]] := Block[{ticksStyle = ToList[rest, ticksStyle]}, makeTickItems @ p],
  makeTickItems[None] := {},
  makeTickItems[sym_Symbol] := Scope[
    {axis, coordFn, {vx, vy}, {offx, offy}} = toAxisIterator[sym];
    max = Part[dims, axis]; flip = MemberQ[flipTicks, axis];
    PlaneInset[If[flip, max + 1 - #, #], center + coordFn[#], {vx, vy}, {offx, offy}, BaseStyle -> ticksStyle]& /@ Range[max]
  ],
  makeTickItems[o_] := Print["Unknown tick: ", o]
];

(* these refer to which of the outermost edges we should label *)
toAxisIterator = Case[
  BottomLeft    := {2, {$minx - sp, #-.5, $minz}&, {{0, -1, 0}, {1, 0, 0}}, {0, 1}};
  TopRight      := {2, {$maxx + sp, #-.5, $maxz}&, {{0, -1, 0}, {1, 0, 0}}, {0, -1}};
  BottomRight   := {1, {#-.5, $miny - sp, $minz}&, {{1, 0, 0}, {0, 1, 0}}, {0, 1}};
  TopLeft       := {1, {#-.5, $maxy + sp, $maxz}&, {{1, 0, 0}, {0, 1, 0}}, {0, -1}};
  (* This should be like this but the corresponding CubeAxes is oriented differently. So we should chuck out all that code
  and switch to this convention. *)
  (* Right         := {3, {$maxx + sp, $miny, #-.5}&, {{1, 0, 0}, {0, 0, 1}}, {-1, 0}}; *)
  Right         := {3, {$maxx, $miny - sp, #-.5}&, {{0, -1, 0}, {0, 0, 1}}, {-1, 0}};
  Left          := {3, {$minx, $maxy + sp, #-.5}&, {{0, -1, 0}, {0, 0, 1}}, {1, 0}};
];

  (* X axis *)
  
(**************************************************************************************************)

PublicFunction[InsetCubeGrid]

PublicOption[Displacement, TextOrientation]

Options[InsetCubeGrid] = JoinOptions[
  TextOrientation -> "Screen",
  FontSize -> Automatic, FontFamily -> Automatic, FontColor -> Automatic, FontWeight -> Automatic,
  Displacement -> {0, 0, 0}, ViewVector -> {-2, -1.5, 2.5},
  CubeGrid
];

InsetCubeGrid[arr_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[textOrientation, displacement, viewVector];
  insetOpts = Sequence[FlipX -> False, FlipY -> False, FilterOptions[PlaneInset, opts]];
  displacement //= Replace["Screen" -> -viewVector/4];
  CubeArray[
    Dimensions @ arr,
    ItemFunction -> Function[{
      $defaultItemFunction[#1],
      Style[PlaneInset[Extract[arr, #2], #1 + displacement, textOrientation, insetOpts], Opacity[1]]
    }],
    FilterOptions @ opts
  ]
]

(**************************************************************************************************)

PublicFunction[ColoredCubeArray, ColoredCubeGrid]

Options[ColoredCubeGrid] = Options[ColoredCubeArray] = JoinOptions[
  ColorRules -> None,
  CubeArray
];

ColoredCubeGrid[args___] := ColoredCubeArray[args, CubeGap -> 0, CubeStyle -> None, CubeOpacity -> 1, MeshStyle -> Automatic];

ColoredCubeArray[array_List /; ArrayQ[array, 3], opts:OptionsPattern[]] := Scope[
  ColoredCubeArray[Extract[array, #]&, Dimensions @ array, opts]
];
 
ColoredCubeArray[cfunc_, dims:{__Integer}, opts:OptionsPattern[]] := Scope[

  UnpackOptions[colorRules];

  If[ListQ[colorRules], cfunc = cfunc /* Replace[Append[colorRules, _ -> None]]];

  CubeArray[dims, ItemStyleFunction -> cfunc, FilterOptions @ opts]

];

  
PublicFunction[ColoredCubeArray]

ColoredCubeArray[spec:(_List | _Rule), dims_List, opts:OptionsPattern[]] :=
  ColoredCubeArray[
    Replace[Append[_ -> None] @ highlightSpecToRules[dims, spec]],
    dims,
    opts
  ];

(**************************************************************************************************)

PublicFunction[SpannedCubeArray]

PublicOption[SpanGap, HideHighlighted]

Options[SpannedCubeArray] = JoinOptions[
  CubeArray,
  HideHighlighted -> True
];

SpannedCubeArray[spec:(_List | _Rule), dims_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[spanGap, hideHighlighted];
  attachSpanHighlights[dims, spec, hideHighlighted] @ CubeArray[dims, FilterOptions @ opts]
];

highlightStyle = {};
attachSpanHighlights[dims_, {} | None, _][e_] := e;
attachSpanHighlights[dims_, spans_, hide_][array_] := Scope[
  rules = highlightSpecToRules[dims, spans];
  spans = makeSpanningCuboid @@@ rules;
  pattern = Alternatives @@ Keys[rules];
  If[hide, array = array /. Annotation[_, AnnotatedCoordinate[_, pattern]] -> {}];
  {array, spans}
,
  makeSpanningCuboid[pattern_, color_] := Scope[
    matches = DeepCases[array, AnnotatedCoordinate[coord_, pattern] :> coord];
    If[matches === {}, Print["No matches for ", pattern]; Return[{}]];
    bounds = CoordinateBoundingBox[matches, 0.5 - spanGap/2];
    Style[Cuboid @@ bounds, FaceEdgeForm @ color, Seq @@ ToList[highlightStyle]]
  ]
];

(**************************************************************************************************)

PublicFunction[CubeAxes]

PublicOption[FlagOrientation, CubeOffset, HideTrivialAxes]

PublicVariable[$CubeBaseStyle]

$CubeBaseStyle = {FontWeight -> Bold, FontFamily -> "Avenir", FontSize -> 24};

CubeAxes::badlabelpos = "Unrecognized LabelPosition ``.";

Options[CubeAxes] = {
  LabelPosition -> "Near",
  InsetScale -> 1/144, BaseStyle -> Automatic,
  FontSize -> Automatic, IncludeArrow -> False,
  CubeOffset -> 0.1, HideTrivialAxes -> True,
  Spacing -> 0
};

CubeAxes[origin_, size_, {lx_, ly_, lz_}, OptionsPattern[]] := Scope[
  UnpackOptions[labelPosition, insetScale, baseStyle, fontSize, includeArrow, cubeOffset, hideTrivialAxes, spacing];
  SetAutomatic[baseStyle, $CubeBaseStyle];
  If[FreeQ[baseStyle, FontSize], SetAutomatic[fontSize, 24]];
  If[NumberQ[fontSize], baseStyle //= ReplaceOptions[FontSize -> fontSize]];
  If[MatchQ[labelPosition, Placed[_String, _]],
    labelScale = Last[labelPosition]; labelPosition //= First,
    labelScale = 0
  ];
  spec = Lookup[$cubeAxesOrient, labelPosition, ReturnFailed["badlabelpos", orient]];
  spec = MapThread[#1 /. {$s -> #2, $i -> (1 - #2)}&, {spec, labelScale * {1, 1, 1}}];
  MapThread[
    If[hideTrivialAxes && #4 === 1, Nothing,
    CubeEdgeText[#1, origin, size, Seq @@ #2, InsetScale -> insetScale, BaseStyle -> baseStyle, IncludeArrow -> #3, CubeOffset -> cubeOffset, Spacing -> #5]]&,
    {{lx, ly, lz}, spec, includeArrow * {1,1,1}, size, spacing * {1,1,1}}
  ]
];

$cubeAxesOrient = <|
  "XZY"     -> {{{3, 1, 2}, {0,$s,0}}, {{1, 2, 3}, {1,$s,1}}, {{1, 3, 2}, {1,$s,0}}},
  "YZX"     -> {{{3, 1, 2}, {1,$s,1}}, {{3, 2, 1}, {0,$s,0}}, {{1, 3, 2}, {0,$s,1}}},
  "YXZ"     -> {{{3, 1, 2}, {0,$s,0}}, {{3, 2, 1}, {0,$s,0}}, {{1, 3, 2}, {1,$s,0}}},
  "ZXY"     -> {{{3, 1, 2}, {1,$s,1}}, {{3, 2, 1}, {1,$i,1}}, {{1, 3, 2}, {0,$s,1}}},
  "XYZ"     -> {{{3, 1, 2}, {1,$s,1}}, {{3, 2, 1}, {1,$i,1}}, {{1, 3, 2}, {1,$i,0}}},
  "ZYX"     -> {{{3, 1, 2}, {1,$i,1}}, {{3, 2, 1}, {1,$i,1}}, {{1, 3, 2}, {1,$s,0}}},
  "TopLeft" -> {{{3, 1, 2}, {1,$s,1}}, {{3, 2, 1}, {0,$i,0}}, {{1, 3, 2}, {0,$i,1}}},
  "Near"    -> {{{3, 1, 2}, {0,$s,0}}, {{3, 2, 1}, {0,$s,0}}, {{1, 3, 2}, {0,$s,{Above, 0}}}},
  "Far"     -> {{{3, 1, 2}, {1,$i,1}}, {{3, 2, 1}, {1,$i,1}}, {{1, 3, 2}, {1,$i,{Below, 1.02}}}},
  "Foo" -> {{{3, 1, 2}, {1,$i,1}}, {{3, 2, 1}, {1,$i,1}}, {{1, 3, 2}, {1, $i, 0}}}
|>;

$CubeAxesStyle = FontWeight -> Bold;

(**************************************************************************************************)

PublicFunction[CubeEdgeText]

PublicOption[IncludeArrow]

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

Options[CubeEdgeText] = {
  FlipX -> Automatic, FlipY -> False, InsetScale -> 1/144, BaseStyle -> {}, IncludeArrow -> False, CubeOffset -> 1/10,
  Spacing -> 0
};

CubeEdgeText[None, ___] := Nothing;

CubeEdgeText[text_, origin_, dims_, faceName_String, pos_Symbol] :=
  CubeEdgeText[text, origin, dims, Seq @@ faceNameToIndex[{faceName, pos}]];

CubeEdgeText[text_, origin_, dims_, indices_, positions_:{0,0,0}, opts:OptionsPattern[]] := Scope[
  UnpackOptions[cubeOffset, spacing];
  coFac = N[cubeOffset / Norm[dims]];
  origin2 = origin - coFac * dims; dims2 = dims * (1 + 2*coFac);
  cubeText0[text, origin2, DiagonalMatrix[dims2], indices, positions, opts]
];
  
cubeText0[text_, origin_, axisVectors:{_, _, _}, {i_, j_, k_}, {s_, t_, p_}, opts___] :=
  cubeText1[text, origin + Part[axisVectors, i] * s, Part[axisVectors, {j, k}], {t, p}, opts]
  
cubeText1[text_, origin_, {dx_, dy_}, {fx_, fy_}, opts___] := Scope[
  arrow = Lookup[{opts}, IncludeArrow, False];
  arrow = Switch[arrow,
    False, 0,
    True | Automatic, Sign[fx],
    All, All,
    _, Sign @ arrow
  ];
  text = Switch[arrow, 0, text, -1, Row[{$larr, text}], 1, Row[{text, $rarr}], All,  Row[{$larr, text, $rarr}]];
  PlaneInset[text, origin + dx * toRat[fx] + dy * toRat[fy] + dy * spacing * Sign[toRat[fy] - 0.5], {dx, dy}, {1, -1} * Map[unit2biunit, {fx, fy}], FilterOptions @ opts]
];
  
$larr = Style["\[LeftArrow]\[ThinSpace]", $Gray];
$rarr = Style["\[ThinSpace]\[RightArrow]", $Gray];

toRat[{Above|Below, x_}] := x;
toRat[x_] := x;

unit2biunit[{Above, _}] := 1;
unit2biunit[{Below, _}] := -1;
unit2biunit[x_] := 2x - 1;

