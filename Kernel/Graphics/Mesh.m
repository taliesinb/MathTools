PublicFunction[MeshLines]

PublicOption[ZVector]

Options[MeshLines] = {
  MeshStyle -> $Gray,
  FrameStyle -> Automatic,
  Background -> Automatic,
  ItemSize -> 1,
  Dividers -> True,
  ZVector -> {1/3,1/3}
};

MeshLines[{xmin_, ymin_}, {ncols_, nrows_}, OptionsPattern[]] := CatchMessage @ Scope[
  UnpackOptions[itemSize, dividers, meshStyle, frameStyle, background];
  UnpackTuple[itemSize, cellw, cellh];
  UnpackTuple[dividers, framew, frameh];
  {xmax, ymax} = {xmin, ymin} + {ncols, nrows} * {cellw, cellh};
  SetAutomatic[frameStyle, FirstCase[meshStyle, c_ ? ColorQ :> Darker[c, .2], $DarkGray, {0, Infinity}]];
  {
    If[meshStyle === None, Nothing,
      {
        HorizontalLine[{xmin, xmax}, Range[ymin + cellh, ymax - cellh, cellh]],
        VerticalLine[Range[xmin + cellw, xmax - cellw, cellw], {ymin, ymax}]
      } ~Style~ toSeq[meshStyle]
    ],
    If[frameStyle === None, Nothing,
      Style[Rectangle[{xmin, ymin}, {xmax, ymax}], EdgeForm[frameStyle], FaceForm[background]]
    ]
  }
];

(**************************************************************************************************)

MeshLines[pos:{_, _, _}, num:{_, _, _}, opts:OptionsPattern[]] := CatchMessage @ Scope[
  
  UnpackOptions[zVector];

  primitives = makeMesh3D[pos, num, opts];

  GraphicsTransformCoordinates[zVector, primitives]
];

(**************************************************************************************************)

makeMesh3D[{minX_, minY_, minZ_}, {numX_, numY_, numZ_}, OptionsPattern[MeshLines]] := Scope[
  UnpackOptions[itemSize, meshStyle, frameStyle, background];
  
  UnpackTuple[itemSize, sizeX, sizeY, sizeZ];
  UnpackTuple[meshStyle, meshX, meshY, meshZ];
  UnpackTuple[frameStyle, frameX, frameY, frameZ];

  SetAutomatic[frameX, meshToFrameStyle @ meshX];
  SetAutomatic[frameY, meshToFrameStyle @ meshY];
  SetAutomatic[frameZ, meshToFrameStyle @ meshZ];
  SetAutomatic[background, GrayLevel[1, 0.9]];

  {maxX, maxY, maxZ} = {minX, minY, minZ} + {numX, numY, numZ} * {sizeX, sizeY, sizeZ};

  Xseg = CoordinateSegment[minX, maxX];
  Yseg = CoordinateSegment[minY, maxY];
  Zseg = CoordinateSegment[minZ, maxZ];
  Xran = Range[minX + sizeX, maxX - sizeX, sizeX];
  Yran = Range[minY + sizeY, maxY - sizeY, sizeY];
  Zran = Range[minZ + sizeZ, maxZ - sizeZ, sizeZ];

  List[
    styleFrame[frameX, CoordinateComplex[{Xran, Yseg, Zseg}]],
    styleFrame[frameY, CoordinateComplex[{Xseg, Yran, Zseg}]],
    styleFrame[frameZ, CoordinateComplex[{Xseg, Yseg, Zran}]],

    styleMesh[meshX, CoordinateComplex[{Xseg, Yran, Zran}]],
    styleMesh[meshY, CoordinateComplex[{Xran, Yseg, Zran}]],
    styleMesh[meshZ, CoordinateComplex[{Xran, Yran, Zseg}]]
  ]
];

SetHoldRest[styleFrame];
styleFrame[None, a_] := Nothing;
styleFrame[s_, a_] := Style[a, EdgeForm @ s, FaceForm @ background];

SetHoldRest[styleMesh];
styleMesh[None, a_] := Nothing;
styleMesh[{s___}, a_] := Style[a, s];
styleMesh[s_, a_] := Style[a, s];

meshToFrameStyle[meshStyle_] := FirstCase[meshStyle, c_ ? ColorQ :> Darker[c, .2], $DarkGray, {0, Infinity}];

(**************************************************************************************************)

PublicFunction[MeshGrid]
PublicOption[HighlightItems, HighlightOffset, ZDimension]

Options[MeshGrid] = JoinOptions[
  ItemFunction -> Identity,
  ItemSize -> 1,
  ItemStyle -> None,
  HighlightItems -> None,
  HighlightOffset -> 0,
  AxesLabel -> None,
  LabelStyle -> Automatic,
  ZDimension -> None,
  MeshLines
];

MeshGrid[array_ ? MatrixQ, opts:OptionsPattern[]] :=
  MeshGrid[{0, 0}, array, opts];

$axesLabelStyle = {FontSize -> 14, FontColor -> Black, FontFamily -> "Avenir"};

MeshGrid[{xmin_, ymin_}, array_ ? MatrixQ, opts:OptionsPattern[]] := Scope[
  {nrows, ncols} = Take[Dimensions @ array, 2];
  UnpackOptions[itemSize, itemFunction, itemStyle, highlightItems, highlightOffset, axesLabel, labelStyle];
  meshItems = MeshLines[{xmin, ymin}, {ncols, nrows}, itemSize, FilterOptions @ opts];
  If[itemFunction =!= Identity, array = Map[itemFunction, array, {2}]];
  {cellw, cellh} = {1, 1} * itemSize;
  xcoords = xmin + cellw * (Range[ncols] - 0.5);
  ycoords = ymin + cellh * (Reverse[Range[nrows]] - 0.5);
  arrayItems = MapIndexed[
    {a, {i, j}} |-> Inset[a, {Part[xcoords, j], Part[ycoords, i]}],
    array, {2}
  ];
  If[itemStyle =!= None, arrayItems = Style[arrayItems, toSeq @ itemStyle]];
  axesItems = If[axesLabel === None, Nothing,
    If[!MatchQ[axesLabel, {_, _}], ReturnFailed[]];
    SetAutomatic[labelStyle, $axesLabelStyle];
    {rowLabel, colLabel} = axesLabel;
    origin = {0, nrows} * 2;
    {
      If[rowLabel === None, Nothing, Text[rowLabel, origin, {1, -1.1}, {0, 1}, BaseStyle -> labelStyle]],
      If[colLabel === None, Nothing, Text[colLabel, origin, {-1, -1.1}, {1, 0}, BaseStyle -> labelStyle]]
    }
  ];
  If[highlightItems === None,
    highlightItems = Nothing;
  ,
    hsize = 0.5 + highlightOffset; hsizex = First[hsize, hsize]; hsizey = Last[hsize, hsize];
    xfunc = Part[xcoords, #] + {-1, 1}*hsizex*cellw&;
    yfunc = Part[ycoords, #] + {-1, 1}*hsizey*cellh&;
    highlightItems = Style[Map[toHighlightItem, ToList @ highlightItems], Opacity[0.3, $Red]];
  ];
  {meshItems, arrayItems, highlightItems, axesItems}
];

toHighlightItem = Case[
  All := % @ {All, All};
  "Rows" := Splice @ Table[% @ {r, All}, {r, nrows}];
  "Columns" := Splice @ Table[% @ {All, c}, {c, ncols}];
  "Cells" := Splice @ Catenate @ Table[% @ {r, c}, {r, nrows}, {c, ncols}];
  {i_, j_} := Scope[
    {xmin, xmax} = xfunc /@ fromSpan[ncols, j]; xmin //= Min; xmax //= Max;
    {ymin, ymax} = yfunc /@ fromSpan[nrows, i]; ymin //= Max; ymax //= Min;
    Rectangle[{xmin, ymin}, {xmax, ymax}, RoundingRadius -> Mean[{cellw, cellh}]/5]
  ];
];

fromSpan[n_, i_Integer]              := {1,1} * fromInt[n, i];
fromSpan[n_, All]                    := {1, n};
fromSpan[n_, i_ ;; All]              := Append[fromSpan[n, i], n];
fromSpan[n_, All ;; i]               := Prepend[fromSpan[n, i], 1];
fromSpan[n_, i_Integer ;; j_Integer] := {fromInt[n, i], fromInt[n, j]};

fromInt[n_, i_] := Which[i > n, n, i == 0, 1, i < 0, i + n + 1, True, i];