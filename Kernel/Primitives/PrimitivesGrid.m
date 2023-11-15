PublicGraphicsPrimitive[PrimitivesGrid, PrimitivesRow, PrimitivesColumn]

DeclareGraphicsPrimitive[PrimitivesGrid,   "Primitives,Vector?", primitivesGRCBoxes]
DeclareGraphicsPrimitive[PrimitivesRow,    "Primitives,Vector?", primitivesGRCBoxes];
DeclareGraphicsPrimitive[PrimitivesColumn, "Primitives,Vector?", primitivesGRCBoxes];

Options[PrimitivesGrid] = {
  Alignment -> Center,
  RowsEqual -> False,
  RowAlignments -> Automatic,
  ColumnsEqual -> False,
  ColumnAlignments -> Automatic,
  AlignmentPoint -> Center,
  Spacings -> 0,
  RowSpacings -> Automatic,
  ColumnSpacings -> Automatic,
  Dividers -> None,
  Frame -> False,
  FrameStyle -> GrayLevel[0.5],
  FrameMargins -> Automatic
};

Options[PrimitivesRow] = DropOptions[{ColumnAlignments, RowsEqual, RowSpacings}] @ Options[PrimitivesGrid];
Options[PrimitivesColumn] = DropOptions[{RowAlignments, ColumnsEqual, ColumnSpacings}] @ Options[PrimitivesGrid];

primitivesGRCBoxes = Case[
  PrimitivesRow[items_List, pos:$CoordP:{0,0}, opts___Rule]    := RawPrimitivesRow[List @ Map[toPGitem, items], pos, opts];
  PrimitivesColumn[items_List, pos:$CoordP:{0,0}, opts___Rule] := RawPrimitivesColumn[List /@ Map[toPGitem, items], pos, opts];
  PrimitivesGrid[items_List, pos:$CoordP:{0,0}, opts___Rule]   := RawPrimitivesGrid[toPGrow /@ items, pos, opts];
];

PrimitivesGrid::badRow = "Row element `` should be a list."
toPGrow = Case[
  e_List := Map[toPGitem, e];
  other_ := (Message[PrimitivesGrid::badRow, MsgExpr @ other]; Nothing);
]

toPGitem = Case[
  Null      := Nothing;
  other_    := ToGraphicsBoxes @ other;
]

(**************************************************************************************************)

PrivateTypesettingBoxFunction[RawPrimitivesGrid, RawPrimitivesRow, RawPrimitivesColumn]

RawPrimitivesRow[itemBoxes_List, pos_List, opts___Rule] :=
  RawPrimitivesGrid[
    itemBoxes, pos,
    SeqRenameOptions[{Alignment -> RowAlignments, Spacings -> ColumnSpacings}] @ opts
  ];

RawPrimitivesColumn[itemBoxes_List, pos_List, opts___Rule] :=
  RawPrimitivesGrid[
    itemBoxes, pos,
    SeqRenameOptions[{Alignment -> ColumnAlignments, Spacings -> RowSpacings}] @ opts
  ];

RawPrimitivesGrid[itemBoxes_, pos_, opts___Rule] := Scope[

  UnpackOptionsAs[PrimitivesGrid, {opts},
    alignment, rowAlignments, columnAlignments, alignmentPoint,
    spacings, rowSpacings, columnSpacings, dividers, frame, frameStyle, frameMargins,
    rowsEqual, columnsEqual
  ];

  numRows = Len @ itemBoxes;
  numCols = Max[Len /@ itemBoxes];
  itemBoxes //= Map[PadRight[#, numCols, {{}}]&];

  itemBounds = MatrixMap[PrimitiveBoxesBounds, itemBoxes];

  {halign, valign} = ParseAlignment[alignment, PrimitivesRow, Alignment];
  colAlign = Map[parseHalign, ParseCyclicSpec[columnAlignments, numCols] /. Automatic -> halign];
  rowAlign = Map[parseValign, ParseCyclicSpec[rowAlignments, numRows] /. Automatic -> valign];

  origSpacings = spacings; spacings *= {1, 1}; {hspacing, vspacing} = spacings;
  colSpace = Append[0] @ ParseCyclicSpec[columnSpacings, numCols-1] /. Automatic -> hspacing;
  rowSpace = Append[0] @ ParseCyclicSpec[rowSpacings, numRows-1] /. Automatic -> vspacing;

  itemSizes = MatrixMap[BoundsToSize, itemBounds];
  itemWides = Part[itemSizes, All, All, 1];
  itemHighs = Part[itemSizes, All, All, 2];

  rowHighs = Max /@ itemHighs;
  colWides = Max /@ Transpose @ itemWides;

  If[rowsEqual,    rowHighs //= repeatMax];
  If[columnsEqual, colWides //= repeatMax];

  totalHigh = Total[rowHighs] + Total[rowSpace];
  totalWide = Total[colWides] + Total[colSpace];
  totalSize = {totalWide, totalHigh};

  hasFrame = frame =!= False || dividers === All;
  SetAutomatic[frameMargins, If[!hasFrame, 0, SelectFirst[{origSpacings, rowSpacings, columnSpacings}, Positive, 0]/2]];
  fm = frameMargins;
  If[hasFrame, {totalSize, totalWide, totalHigh} += 2 * fm];

  galign = ParseAlignment[alignmentPoint, PrimitivesGrid, AlignmentPoint];
  {gx, gy} = pos - galign * totalSize;

  (* TODO: what would it mean to have Axis alignment, which would mean the origin ? *)
  items = Repeat[Null, {numRows, numCols}];
  dy = gy + totalHigh - fm;
  Do[
    dx = gx + fm;
    th = Part[rowHighs, r];
    va = Part[rowAlign, r];
    rs = Part[rowSpace, r];
    Do[
      {{x1, x2}, {y1, y2}} = Part[itemBounds, r, c];
      {w, h} = Part[itemSizes, r, c];
      tw = Part[colWides, c];
      ha = Part[colAlign, c];
      ox = dx - (x1 - Lerp[0, tw - w, ha]);
      oy = dy - (y2 + Lerp[0, th - h, va]);
      Part[items, r, c] = Construct[
        GeometricTransformationBox,
        Part[itemBoxes, r, c],
        ToPackedReal @ {ox, oy}
      ];
      dx += tw + Part[colSpace, c];
    ,
      {c, numCols}
    ];
    dy -= th + Part[rowSpace, r];
  ,
    {r, numRows}
  ];

  prims = items;

  If[hasFrame,
    {bl, tr} = {{0, 0}, totalSize} + Threaded[{gx, gy}];
    framePrim = StyleBox[Construct[RectangleBox, bl, tr], FaceForm @ None, EdgeForm @ frameStyle];
    prims = {prims, framePrim};
  ];

  SetNone[dividers, False];
  If[dividers =!= False,
    xs = Prepend[Accumulate[colWides + colSpace] - colSpace/2, 0] + gx + fm;
    ys = -Prepend[Accumulate[rowHighs + rowSpace] - rowSpace/2, 0] + gy + totalHigh - fm;
    lr = {0, totalWide} + gx;
    bt = {0, totalHigh} + gy;
    {xs, ys} = Map[Take[#, {2, -2}]&, {xs, ys}];
    gridPrims = StyleBox[{ToHorizontalLine[lr, ys], ToVerticalLine[xs, bt]}, frameStyle];
    prims = {gridPrims /. Line -> LineBox, prims};
  ];

  prims
];

(* TODO: Allow overriding the other side, e.g. normally we have ColumnAlignments -> {Left, Right} but
we could parse {BottomLeft, Right} to {$override[0, 0], 1}, the $override would be picked up
and would override the default va that came from that row *)

PrimitivesGrid::badDirAlign = "Invalid specification `` given as part of ``."
parseHalign = Case[
  Left := 0; Center := 0.5; Right := 1;
  r:$NumberP := r;
  e_ := (Message[PrimitivesGrid::badDirAlign, MsgExpr @ e, ColumnAlignments]; 0.5)
];
parseValign = Case[
  Bottom  := 1; Center := 0.5; Top := 0;
  r:$NumberP := r;
  e_ := (Message[PrimitivesGrid::badDirAlign, MsgExpr @ e, RowAlignments]; 0.5)
];

repeatMax[n_] := Repeat[Max @ n, Len @ n];

