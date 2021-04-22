Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

PackageExport["ArrowheadLegend"]

$arrowheadPoints = With[{adx = 4/3, offx = 0}, {{-adx + offx, -1}, {-adx + offx, 1}, {adx + offx, 0}}];
$arrowheadSize = 12 * {1, 1};

ArrowheadLegend[assoc_Association] := Scope[
  $arrowheadSize = 10 * {1, 1};
  rows = KeyValueMap[{name, color} |-> {" ",
    Append[makeBaseArrowhead[color] /. p_FilledCurve :> Rotate[p, Pi/2], BaselinePosition -> Scaled[0.1]],
    name
  }, assoc];
  Grid[rows, BaseStyle -> {FontFamily -> "Avenir"}, Spacings -> {.5, 0.5}]
]

(**************************************************************************************************)

PackageExport["ColoredArrowhead"]

ColoredArrowhead[color_, sz_:12] := Scope[
  $arrowheadSize = sz * {1, 1};
  Append[
    makeBaseArrowhead[color] /. p_Polygon :> Rotate[p, Pi/2],
    BaselinePosition -> Scaled[0.1]
  ]
];

(**************************************************************************************************)

PackageScope["$baseShortArrowheadGraphics"]
PackageScope["$baseLongArrowheadGraphics"]

offsetX[dx_][list_] := MapAt[# + dx&, list, {All, All, 1}];

$baseShortArrowhead = FilledCurve[
  {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
  offsetX[0.35] @ {{
    {-0.6666528591843921`, -0.3333333333333333`},
    {-0.533327810340424`, 6.903741136987662`*^-6},
    {-0.6666528591843921`, 0.3333333333333333`},
    {0.`, 6.903741136987662`*^-6}
  }}
];

$baseLongArrowhead = FilledCurve[
  {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}},
  offsetX[0.55] @ {{
    {-1.`,-0.31249915682968066`},
    {-0.8749949409780775`,-8.881784197001252`*^-16},
    {-1.`,0.3124991568296789`}, {0.`,-8.881784197001252`*^-16}
  }}
];

makeRawArrowhead[g_, offset_] := Scope[
  {{x1, x2}, {y1, y2}} = PlotRange @ Graphics @ g;
  Graphics[g, PlotRange -> {{x1, x2}, {y1, y2}}, PlotRangeClipping -> False]
]

$baseShortArrowheadGraphics = makeRawArrowhead[$baseShortArrowhead, 0.35];
$baseLongArrowheadGraphics = makeRawArrowhead[$baseLongArrowhead, 0.55];

(**************************************************************************************************)

PackageScope["makeBaseArrowhead"]

makeBaseArrowhead[color_] :=
  Graphics[
    {Opacity[1.0], FaceForm[color], EdgeForm[None], $baseShortArrowhead},
    ImageSize -> $arrowheadSize, AspectRatio -> 1,
    PlotRangeClipping -> False
  ];
