Package["GraphTools`"]

PackageImport["GeneralUtilities`"]


(**************************************************************************************************)

PackageExport["ArrowheadLegend"]

ArrowheadLegend[assoc_Association] := Scope[
  rows = KeyValueMap[{name, color} |-> {" ", ColoredArrowhead[color, 11], name}, assoc];
  Grid[rows, BaseStyle -> $LegendLabelStyle, Spacings -> {.4, 0.5}]
]

(**************************************************************************************************)

PackageExport["ColoredArrowhead"]

ColoredArrowhead[color_, sz_:12] := Scope[
  ReplaceOptions[
    makeBaseArrowhead[color] /. p_FilledCurve :> Rotate[p, Pi/2],
    {BaselinePosition -> Scaled[-0.0], ImageSize -> sz}
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
    {(* 0.` *) -0.1, 6.903741136987662`*^-6}
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
    AspectRatio -> 1,
    PlotRangeClipping -> False
  ];
