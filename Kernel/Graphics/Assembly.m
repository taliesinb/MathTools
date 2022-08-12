PublicHead[SRow, SCol, SRows, SCols, SGrid, Padded]

PublicFunction[AssembleGraphics]

PublicOption[HorizontalAlignment, VerticalAlignment, HorizontalSpacing, VerticalSpacing, Spacing, ImageScale]

Options[AssembleGraphics] = JoinOptions[
  HorizontalAlignment -> Center,
  VerticalAlignment -> Center,
  HorizontalSpacing -> 1,
  VerticalSpacing -> 1,
  Alignment -> None,
  Spacing -> None,
  ImageScale -> None,
  BaseStyle -> {FontSize -> 30, FontFamily -> "Source Code Pro"},
  Graphics
];

AssembleGraphics[g_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[$horizontalAlignment, $verticalAlignment, $horizontalSpacing, $verticalSpacing, alignment, spacing, $imageScale, $baseStyle, imagePadding];
  If[alignment =!= None, Switch[alignment,
    Left | Right, $horizontalAlignment = alignment,
    Top | Bottom, $verticalAlignment = alignment,
    Center, $horizontalAlignment = $verticalAlignment = Center,
    {_, _}, {$horizontalAlignment, $verticalAlignment} = alignment
  ]];
  If[spacing =!= None, Switch[spacing,
    _ ? NumericQ | Scaled[_ ? NumericQ], $horizontalSpacing = $verticalSpacing = spacing,
    {_ ? NumericQ, _ ? NumericQ}, {$horizontalSpacing, $verticalSpacing} = spacing
  ]];

  $pointSizeScaleFactor = None;
  Label[Recompute];
  $requiredScaleFactor = False;
  prims = assemble @ embedSizes @ g;
  If[!MatchQ[prims, _Sized], ReturnFailed[]];

  {xmax, ymax} = Last @ prims;
  prims //= First;
  prims //= SimplifyTranslate;
  
  graphics = Graphics[prims, BaseStyle -> $baseStyle, PlotRange -> {{0, xmax}, {0, ymax}}, FilterOptions @ opts];
  If[$imageScale =!= None, graphics = SetGraphicsScale[graphics, $imageScale, imagePadding]];
  
  If[$requiredScaleFactor,
    {{xmin, xmax}, {ymin, ymax}} = GraphicsPlotRange[graphics];
    imageWidth = First @ LookupImageSize @ graphics;
    $pointSizeScaleFactor = (xmax - xmin) / imageWidth;
    Goto[Recompute];
  ];
  
  graphics
];

(**************************************************************************************************)

embedSizes = Case[
  s:(_SRow | _SCol)          := Map[%, s];
  s:(_SRows | _SCols)        := Map[embedSizesList, s];
  SGrid[array_List, rest___] := SGrid[Map[embedSizesList, array], rest];
  r_Rule                     := r; (* these are options to SRow, SCol, etc *)
  e_                         := wrapGraphics[e];
];

embedSizesList = Case[
  list_List := Map[embedSizes, list];
];

$centerOSpecP = Center | Automatic | {Center, Center} | Scaled[{.5, .5}] | {Scaled[.5], Scaled[.5]};

wrapGraphics[i:Inset[_, _, $centerOSpecP, {w_ ? NumericQ, h_ ? NumericQ}]] :=
  Sized[Translate[i, -{w,h}/2], {w, h}];

wrapGraphics[s_String] := wrapGraphics @ Text[s, {0, 0}];

wrapGraphics[Inset[s_String, pos_]] := wrapGraphics @ Text[s, pos];

wrapGraphics[Style[e:(_Text | _Inset | _String), style___]] := Block[{$baseStyle = ToList[style, $baseStyle]}, wrapGraphics[e]];

wrapGraphics[Spacer[w_ ? NumericQ]] := Sized[{}, {w, 0.0001}];
wrapGraphics[Spacer[{w_ ? NumericQ, h_ ? NumericQ}]] := Sized[{}, {w, h}];

wrapGraphics[Padded[e_, padding_]] := Scope[
  {g, {w, h}} = List @@ wrapGraphics[e];
  {{l, r}, {b, t}} = StandardizePadding[padding];
  Sized[Translate[g, {l, b}], {w + l + r, h + b + t}]
];

wrapGraphics[t:(_Text | _Inset)] := Scope[
  If[$pointSizeScaleFactor === None,
    $requiredScaleFactor ^= True;
    Return @ Sized[t, {1, 1}];
  ];
  pointSize = cachedBoundingBox[t];
  coordSize = pointSize * $pointSizeScaleFactor;
  Sized[Translate[t, coordSize/2], coordSize]
];

Clear[QuiverGeometryLoader`$BoundingBoxCache];
If[!AssociationQ[QuiverGeometryLoader`$BoundingBoxCache],
QuiverGeometryLoader`$BoundingBoxCache = UAssociation[];
];

cachedBoundingBox[Text[t_, ___, BaseStyle -> baseStyle_, ___]] :=
  cachedBoundingBox[Style[t, BaseStyle -> baseStyle]];

cachedBoundingBox[Text[t_, ___]] :=
  cachedBoundingBox[t];

cachedBoundingBox[Inset[t_, ___]] :=
  cachedBoundingBox[t];

cachedBoundingBox[t_] := CacheTo[
  QuiverGeometryLoader`$BoundingBoxCache,
  Hash[{t, $baseStyle}],
  Take[Rasterize[Style[t, Seq @@ ToList[$baseStyle]], "BoundingBox"], 2]
];

wrapGraphics[g_] := Scope[
  {{xmin, xmax}, {ymin, ymax}} = GraphicsPlotRange[g];
  xsize = xmax - xmin;
  ysize = ymax - ymin;
  Sized[Translate[g, -{xmin, ymin}], {xsize, ysize}]
];

assemble = Case[
  SRow[args___, opts___Rule] := assembleSRow[
    assemble /@ {args},
    Lookup[{opts}, {Alignment, Spacing}, Inherited]
  ];
  SCol[args___, opts___Rule] := assembleSCol[
    assemble /@ {args},
    Lookup[{opts}, {Alignment, Spacing}, Inherited]
  ];
  SGrid[array_List, opts___Rule] := assembleSGrid[
    Map[assemble, array, {2}],
    Lookup[{opts}, {HorizontalAlignment, VerticalAlignment, HorizontalSpacing, VerticalSpacing}, Inherited]
  ];
  e_ := e;
];

assembleSRow[list_List, {valign_, hspacing_}] := Scope[
  sizes = Part[list, All, 2];
  SetInherited[valign, $verticalAlignment];
  SetInherited[hspacing, $horizontalSpacing];
  SetScaledFactor[hspacing, Mean @ FirstColumn @ sizes];
  maxHeight = Max @ LastColumn @ sizes;
  yfunc = toAlignFunc[maxHeight, valign];
  list2 = VectorApply[
    x = 0; {g, {w, h}} |-> SeqFirst[Translate[g, {x, yfunc[h]}], x += w + hspacing],
    list
  ];
  Sized[list2, {x - hspacing, maxHeight}]
];

assembleSCol[list_List, {halign_, vspacing_}] := Scope[
  list //= Reverse;
  sizes = Part[list, All, 2];
  SetInherited[halign, $horizontalAlignment];
  SetInherited[vspacing, $verticalSpacing];
  SetScaledFactor[vspacing, Mean @ LastColumn @ sizes];
  maxWidth = Max @ FirstColumn @ sizes;
  maxHeight = Max @ LastColumn @ sizes;
  xfunc = toAlignFunc[maxWidth, halign];
  list2 = VectorApply[
    y = 0; {g, {w, h}} |-> SeqFirst[Translate[g, {xfunc[w], y}], y += h + vspacing],
    list
  ];
  Sized[list2, {maxWidth, y - vspacing}]
];

toAlignFunc[max_, align_] :=
  Switch[align,
    Bottom|Left,          0&,
    Center,               (max - #) / 2&,
    Scaled[_ ? NumericQ], (max - #) * N[First @ align]&,
    Top|Right,            (max - #)&
  ];

assembleSGrid[array_List, {halign_, valign_, hspacing_, vspacing_}] := Scope[
  SetInherited[halign, $horizontalAlignment];
  SetInherited[valign, $verticalAlignment];
  SetInherited[hspacing, $horizontalSpacing];
  SetInherited[vspacing, $verticalSpacing];
  array //= Reverse;
  sizes = Part[array, All, All, 2];
  widths = Part[sizes, All, All, 1];
  heights = Part[sizes, All, All, 2];
  SetScaledFactor[hspacing, Mean @ Catenate @ widths];
  SetScaledFactor[vspacing, Mean @ Catenate @ heights];
  maxWidths = Max /@ Transpose[widths]; (* max width of each column *)
  maxHeights = Max /@ heights; (* max height of each row *)
  xfuncs = toAlignFunc[#, halign]& /@ maxWidths;
  yfuncs = toAlignFunc[#, valign]& /@ maxHeights;
  list2 = MapIndex1[
    y = 0;
    {row, rowInd} |-> (
      x = 0;
      res = MapIndex1[
        {cell, colInd} |-> (
          {g, {w, h}} = List @@ cell;
          cellX = x + Part[xfuncs, colInd][w]; x += Part[maxWidths, colInd] + hspacing;
          cellY = y + Part[yfuncs, rowInd][h];
          Translate[g, {cellX, cellY}]
        ),
        row
      ];
      y += Part[maxHeights, rowInd] + vspacing;
      res
    ),
    array
  ];
  Sized[list2, {x - hspacing, y - vspacing}]
];

