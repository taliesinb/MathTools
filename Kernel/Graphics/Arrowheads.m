PublicFunction[ArrowheadData]

ArrowheadData[name_, style_:$Gray] :=
  makeArrowheadShape[name, style];

(**************************************************************************************************)

PrivateFunction[makeArrowheadShape]

makeArrowheadShape[None, _] := None;

makeArrowheadShape["Sphere", style_] :=
  makeArrowheadGraphic3D[$arrowheads3D @ name, Color3D @ style];

makeArrowheadShape[name_Str, style_] /; StringStartsQ[name, {"Line", "DoubleLine", "CrossLine"}] :=
  makeArrowheadGraphic2D[
    $arrowheads2D @ name,
    Directive[style, $edgeThickness]
  ];

ExtendedGraphPlot::arrow3din2d = "Cannot use shape `` in a 2D graph."

makeArrowheadShape[name_Str, style_] := Which[
  KeyExistsQ[$arrowheads2D, name],
    makeArrowheadGraphic2D[$arrowheads2D @ name, style],
  KeyExistsQ[$arrowheads3D, name],
    If[!$GraphIs3D, failPlot["arrow3din2d", name]];
    makeArrowheadGraphic3D[$arrowheads3D @ name, If[ColorQ[style], Color3D @ style, style]],
  True,
    badArrowheadShape[name]
];

makeArrowheadShape[Graphics[elems_, opts___], style_] :=
  Graphics[{Opacity[1], style, elems}, opts, AspectRatio -> 1, PlotRangeClipping -> False];

makeArrowheadShape[Placed[img_Image, pos_], style_] :=
  imageToGraphics[img, pos];

makeArrowheadShape[img_Image, style_] :=
  imageToGraphics[img, {0, 1}];

makeArrowheadShape[spec_, _] :=
  badArrowheadShape[spec];

badArrowheadShape[spec_] :=
  failPlot["badarrowhead", spec,
    commaString @ $namedArrowheads];

imageToGraphics[img_, pos_] := ImageToGraphics[ImageResize[img, 50], pos, 50];

ExtendedGraphPlot::badarrowhead = "ArrowheadShape -> `` should be None, Automatic, Graphics[..], an Image, or one of ``."

(**************************************************************************************************)

makeArrowheadGraphic2D[primitives_, style_, opts___] :=
  Graphics[
    {Opacity[1], EdgeForm @ If[$arrowBorderStyle =!= None,
      SetColorOpacity[$arrowBorderStyle @ style, 1], None],
      style, primitives},
    AspectRatio -> Automatic,
    PlotRangeClipping -> False,
    opts
  ];

makeArrowheadGraphic3D[primitives_, style_, opts___] :=
  Graphics3D[
    {Opacity[1], EdgeForm @ None, FaceForm @ style, primitives},
    opts
  ];

$nudge = 1*^-2;

$arrowheads2D = Assoc[
  "Invisible" -> {},
  "Line" ->
    Line @ ToPacked @ {{-0.2, -0.3}, {0.1, 0.}, {-0.2, 0.3}},
  "LineDoubleOut" ->
    Line @ ToPacked @ {
      {{+0.1, -0.3}, {+0.4, 0.}, {+0.1, 0.3}},
      {{-0.1, -0.3}, {-0.4, 0.}, {-0.1, 0.3}}
    },
  "LineDoubleOutClose" ->
    Line @ ToPacked @ {
      {{0., -0.3}, {+0.3, 0.}, {0., 0.3}},
      {{0., -0.3}, {-0.3, 0.}, {0., 0.3}}
    },
  "LineDoubleIn" ->
    Line @ ToPacked @ {
      {{+0.4, -0.3}, {+0.1, 0.}, {+0.4, 0.3}},
      {{-0.4, -0.3}, {-0.1, 0.}, {-0.4, 0.3}}
    },
  "LineDoubleInClose" ->
    Line @ ToPacked @ {
      {{+0.3, -0.3}, {0., 0.}, {+0.3, 0.3}},
      {{-0.3, -0.3}, {0., 0.}, {-0.3, 0.3}}
    },
  "DoubleLine" ->
    Line @ ToPacked @ {
      {{-0.25, -0.3}, {+0.05, 0.}, {-0.25, 0.3}},
      {{-0.05, -0.3}, {+0.25, 0.}, {-0.05, 0.3}}
    },
  "EqualLine" ->
    Line @ ToPacked @ (0.25 * {{-0.5, -0.87}, {1.0, 0.}, {-0.5, 0.87}}),
  "EqualTriangle" ->
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {0.25 * {{-0.5, -0.87}, {1.0, 0.}, {-0.5, 0.87}}}
    ],
  "Triangle" ->
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.1, -0.3}, {-0.1, 0.3}, {0.2, 0.}}}
    ],
  "TriangleDoubleOut" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.1, -0.3}, {-0.1, 0.3}, {-0.4, 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.1, -0.3}, {+0.1, 0.3}, {0.4, 0.}}}
    ]},
  "TriangleDoubleOutClose" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0., -0.3}, {0., 0.3}, {-0.3, 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0., -0.3}, {0., 0.3}, {0.3, 0.}}}
    ]},
  "TriangleDoubleIn" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.4, -0.3}, {-0.4, 0.3}, {-0.1, 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.4, -0.3}, {+0.4, 0.3}, {0.1, 0.}}}
    ]},
  "TriangleDoubleInClose" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.3, -0.3}, {-0.3, 0.3}, {0., 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0.3, -0.3}, {0.3, 0.3}, {0., 0.}}}
    ]},

  "HalfTriangle" ->
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.15, -0.4}, {-0.15, 0.}, {0.25, 0.}}}
    ],
  "HalfTriangleDoubleOut" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.1, -0.4}, {-0.1, 0.}, {-0.5, 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.1, -0.4}, {+0.1, 0.}, {0.5, 0.}}}
    ]},
  "HalfTriangleDoubleOutClose" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0., -0.4}, {0., 0.}, {-0.4, 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0., -0.4}, {0., 0.}, {+0.4, 0.}}}
    ]},
  "HalfTriangleDoubleIn" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.5, -0.4}, {-0.5, 0.}, {-0.1, 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.5, -0.4}, {+0.5, 0.}, {+0.1, 0.}}}
    ]},
  "HalfTriangleDoubleInClose" -> {
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.4, -0.4}, {-0.4, 0.}, {0., 0.}}}
    ],
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.4, -0.4}, {+0.4, 0.}, {0., 0.}}}
    ]},

  "OtherHalfTriangle" ->
    FilledCurve[
      {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.15, 0.4}, {-0.15, 0.}, {0.25, 0.}}}
    ],

  "FlatArrow" ->
    Polygon[
      ToPacked @ {{{-0.3166, -0.3333}, {-0.3166, 0.3333}, {0.25, 0.}}}
    ],

  "NarrowArrow" ->
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.3166, -0.25}, {-0.1833, 0.}, {-0.3166, 0.25}, {0.25, 0.}}}
    ],
  "NarrowArrowDoubleIn" -> {
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.45, -0.25}, {-0.05, 0.}, {-0.45, 0.25}, {-0.35, 0.}}}
    ],
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.45, -0.25}, {+0.05, 0.}, {+0.45, 0.25}, {+0.35, 0.}}}
    ]},

  "Arrow" ->
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.3166, -0.3333}, {-0.1833, 0.}, {-0.3166, 0.3333}, {0.25, 0.}}}
    ],
  "ArrowDoubleOut" -> {
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.05, -0.33}, {-0.45, 0.}, {-0.05, 0.33}, {-0.15, 0.}}}
    ],
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0.05, -0.33}, {0.45, 0.}, {0.05, 0.33}, {0.15, 0.}}}
    ]},
  "ArrowDoubleOutClose" -> {
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0.0, -0.33}, {-0.4, 0.}, {0.0, 0.33}, {-0.1, 0.}}}
    ],
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{0.0, -0.33}, {0.4, 0.}, {0.0, 0.33}, {0.1, 0.}}}
    ]},
  "ArrowDoubleIn" -> {
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.45, -0.33}, {-0.05, 0.}, {-0.45, 0.33}, {-0.35, 0.}}}
    ],
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.45, -0.33}, {+0.05, 0.}, {+0.45, 0.33}, {+0.35, 0.}}}
    ]},
  "ArrowDoubleInClose" -> {
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.4, -0.33}, {0., 0.}, {-0.4, 0.33}, {-0.30, 0.}}}
    ],
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{+0.4, -0.33}, {0, 0.}, {+0.4, 0.33}, {+0.3, 0.}}}
    ]},
  "Disk" ->
    Disk[ToPacked @ {0., 0.}, .2],
  "Square" ->
    ToPacked /@ Rectangle[-0.2 * {1,1}, 0.2 * {1,1}],
  "Diamond" ->
    FilledCurve[
      {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
      ToPacked @ {{{-0.45, 0.}, {0., -0.25}, {0.45, 0.}, {0., 0.25}}}
    ],

  "CrossLine" ->
    Line @ ToPacked @ {{0., -0.333}, {0., 0.333}},
  "CrossBar" ->
    ToPacked /@ Rectangle[{-0.05,-0.2}, {0.05,0.2}]
];

$coneRadius = 0.15;

$arrowheads3D = Assoc[
  "Cone" ->
    Cone[ToPacked @ {{-0.2, 0., 0.}, {0.2, 0., 0.}}, $coneRadius],
  "Tube" ->
    Tube[ToPacked @ {{-0.2, 0., 0.}, {0.2, 0., 0.}}, $coneRadius/2],
  "ConeDoubleOut" ->
    Cone[ToPacked @ {{{0.1, 0., 0.}, {0.5, 0., 0.}}, {{-0.1, 0., 0.}, {-0.5, 0., 0.}}}, {$coneRadius, $coneRadius}],
  "ConeDoubleOutClose" ->
    Cone[ToPacked @ {{{0., 0., 0.}, {0.4, 0., 0.}}, {{0., 0., 0.}, {-0.4, 0., 0.}}}, {$coneRadius, $coneRadius}],
  "ConeDoubleIn" ->
    Cone[ToPacked @ {{{0.5, 0., 0.}, {0.1, 0., 0.}}, {{-0.5, 0., 0.}, {-0.1, 0., 0.}}}, {$coneRadius, $coneRadius}],
  "Sphere" ->
    Sphere[ToPacked @ {0., 0., 0.}, .2]
];

$namedArrowheads = Union[
  Discard[Keys @ $arrowheads2D, StringContainsQ["DoubleIn" | "DoubleOut"]],
  Keys @ $arrowheads3D, {"Cardinal"}
];

(**************************************************************************************************)

PublicFunction[ArrowheadLegend]

Options[ArrowheadLegend] = {
  ArrowheadShape -> "Arrow",
  Orientation -> Vertical,
  MaxItems -> 10
}

PrivateFunction[to2DArrowheadShape]

to2DArrowheadShape = Case[
  Automatic   := "Arrow";
  "Cone"      := "Arrow";
  "Sphere"    := "Disk";
  a_          := a;
];

ArrowheadLegend[assoc_Assoc, OptionsPattern[]] := Scope[
  UnpackOptions[arrowheadShape, orientation, maxItems];
  If[arrowheadShape === "Cardinal", Return[""]];
  isHorizontal = orientation === Horizontal;
  shapeIndex = If[AssocQ[arrowheadShape], arrowheadShape, <|All -> arrowheadShape|>];
  rows = KeyValueMap[
    {name, color} |-> (
      arrowShape = to2DArrowheadShape @ Lookup[shapeIndex, name, Lookup[shapeIndex, All, "Arrow"]];
      If[MissingQ[arrowShape] || arrowShape === None, Nothing,
        graphic = makeLegendArrowheadGraphic[color, arrowShape, isHorizontal];
        {graphic, name}
      ]),
    assoc
  ];
  If[Len[rows] > maxItems, rows = Append[Take[rows, maxItems], {"", "\[VerticalEllipsis]"}]];
  If[isHorizontal, horizontalALFrame, verticalALFrame] @ rows
]

verticalALFrame[rows_] :=
  Framed[
    Grid[rows, BaseStyle -> {FontFamily -> $CardinalFont, FontSize -> 12}, Spacings -> {.4, 0.5}],
    FrameMargins -> {{0, 0}, {5, 5}},
    FrameStyle -> None
  ]

horizontalALFrame[rows_] :=
  Framed[
    Grid[Transpose @ rows, BaseStyle -> {FontFamily -> $CardinalFont, FontSize -> 12}, Spacings -> {.4, 0.5}],
    FrameMargins -> {{5, 5}, {0, 0}},
    FrameStyle -> None
  ]

(**************************************************************************************************)

PrivateFunction[makeLegendArrowheadGraphic]

makeLegendArrowheadGraphic[color_, shape_, isHorizontal_:False] := Scope[
  isLine = StringContainsQ[shape, "Line"];
  prims = $arrowheads2D @ shape;
  makeArrowheadGraphic2D[
    {If[isLine, AbsoluteThickness[2.0], Nothing], If[!isHorizontal, Rotate[prims, Pi/2], prims]},
    If[isLine, Id, FaceForm] @ color,
    BaselinePosition -> Scaled[0.1], ImageSize -> {11, 11}, AspectRatio -> Automatic
  ]
];

(**************************************************************************************************)

PrivateFunction[makeHighlightArrowheadShape]

makeHighlightArrowheadShape[color_, scaling_, False] :=
  makeArrowheadGraphic2D[
    $arrowheads2D["Line"] /. r_List :> (r * scaling),
    toDirective @ {color, JoinForm @ "Miter", CapForm @ "Round"}
  ];

makeHighlightArrowheadShape[color_, scaling_, True] :=
  makeArrowheadGraphic3D[
    Cone[ToPacked @ ({{-0.2, 0., 0.}, {0.2, 0., 0.}} * 1.75 * scaling), $coneRadius * 1.75 * scaling],
    Color3D @ color
  ];

(**************************************************************************************************)

PrivateFunction[TransformArrowheads]

$arrowheadTransforms = <|
  "Reverse" -> reverseArrowhead,
  "OverBar" -> addInversionBar[True],
  "UnderBar" -> addInversionBar[False],
  "Identity" -> Id
|>;

TransformArrowheads[primitives_, transform_Str] := Scope[
  func = Lookup[$arrowheadTransforms, transform];
  ReplaceAll[primitives,
    g:{_, _, (Graphics|Graphics3D)[Except[{}], ___]} :> RuleCondition @ func @ g
  ]
];

TransformArrowheads[transform_][primitives_] :=
  TransformArrowheads[primitives, transform];

reverseArrowhead[{size_, pos_, graphics_}] :=
  {-size, pos, graphics};

addInversionBar[isOver_][{size_, pos_, graphics:Graphics[primitives_, opts___]}] := Scope[
  {{xl, xh}, {yl, yh}} = GraphicsPlotRange @ graphics;
  yb = If[isOver, yh * 1.4, yl * 1.4];
  graphics = Graphics[{primitives, {Opacity[1], Black, AbsoluteThickness[0.5], Line @ {{xl, yb}, {xh, yb}}}}, opts];
  {size, pos, graphics}
];

