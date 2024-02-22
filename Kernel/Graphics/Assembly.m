PublicHead[SRow, SCol, SColR, SRowR, SRows, SCols, SGrid, Padded]

SColR[args___, opts___Rule] := SCol[Seq @@ Rev[{args}], opts];
SRowR[args___, opts___Rule] := SRow[Seq @@ Rev[{args}], opts];

PublicFunction[AssembleGraphics]

PublicOption[HorizontalAlignment, VerticalAlignment, HorizontalSpacing, VerticalSpacing, Spacing]

Options[AssembleGraphics] = JoinOptions[
  HorizontalAlignment -> Center,
  VerticalAlignment -> Center,
  HorizontalSpacing -> 1,
  VerticalSpacing -> 1,
  Alignment -> None,
  Spacing -> None,
  BaseStyle -> {FontSize -> 22, FontFamily -> "Source Code Pro"},
  Graphics
];

AssembleGraphics[g_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[$horizontalAlignment, $verticalAlignment, $horizontalSpacing, $verticalSpacing, alignment, spacing, $baseStyle, imagePadding];
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
  $spacing = {$horizontalSpacing, $verticalSpacing};

  $pointSizeScaleFactor = None;
  Label[Recompute];
  $requiredScaleFactor = False;
  prims = assemble @ g;
  If[!MatchQ[prims, _Sized], ReturnFailed[]];

  {xmax, ymax} = L @ prims;
  prims //= F;
  prims //= SimplifyTranslate;
  
  graphics = Graphics[prims, BaseStyle -> $baseStyle, PlotRange -> {{0, xmax}, {0, ymax}}, FilterOptions @ opts];
  
  If[$requiredScaleFactor,
    {{xmin, xmax}, {ymin, ymax}} = GraphicsPlotRange[graphics];
    imageWidth = F @ LookupImageSize @ graphics;
    $pointSizeScaleFactor = (xmax - xmin) / imageWidth;
    Goto[Recompute];
  ];
  
  graphics
];

(**************************************************************************************************)

(* TODO: collapse embedSizes /* assemble into just one step. its purely recursive anyway *)

$centerOSpecP = Center | Auto | {Center, Center} | Scaled[{.5, .5}] | {Scaled[.5], Scaled[.5]};

$labelStyle = {FontFamily -> "Avenir", FontSize -> 20};

assemblePrimitives = Case[

  Graphics[p_, ___] := % @ p;

  i:Inset[_, _, $centerOSpecP, {w_ ? NumericQ, h_ ? NumericQ}] := Sized[Translate[i, -{w,h}/2], {w, h}];

  s_ ? NumberQ := % @ TextString @ s;

  s_Str := % @ Text[s, {0, 0}, BaseStyle -> $labelStyle];

  Inset[s_Str, pos_] := % @ Text[s, pos, BaseStyle -> $LabelStyle];

  Style[e:(_Text | _Inset | _Str), style___] := Block[{$baseStyle = ToList[style, $baseStyle]}, %[e]];

  Spacer[w_ ? NumericQ] := Sized[{}, {w, 0.0001}];

  Spacer[{w_ ? NumericQ, h_ ? NumericQ}] := Sized[{}, {w, h}];

  t:(_Text | _Inset) := Scope[
    If[$pointSizeScaleFactor === None,
      $requiredScaleFactor ^= True;
      Return @ Sized[t, {1, 1}];
    ];
    pointSize = cachedBoundingBox[t];
    coordSize = pointSize * $pointSizeScaleFactor;
    Sized[Translate[t, coordSize/2], coordSize]
  ];

  g_ := Scope[
    {{xmin, xmax}, {ymin, ymax}} = GraphicsPlotRange[g];
    xsize = xmax - xmin;
    ysize = ymax - ymin;
    Sized[Translate[g, -{xmin, ymin}], {xsize, ysize}]
  ];
];

(* TODO: use Rasterization utilities for this instead! *)
CacheVariable[$BoundingBoxCache]

cachedBoundingBox[Text[t_, ___, BaseStyle -> baseStyle_, ___]] :=
  cachedBoundingBox[Style[t, BaseStyle -> baseStyle]];

cachedBoundingBox[Text[t_, ___]] :=
  cachedBoundingBox[t];

cachedBoundingBox[Inset[t_, ___]] :=
  cachedBoundingBox[t];

cachedBoundingBox[t_] := CachedInto[
  $BoundingBoxCache,
  Hash[{t, $baseStyle}],
  Take[Rasterize[Style[t, Seq @@ ToList[$baseStyle]], "BoundingBox"], 2]
];

assemble = Case[

  (SRow|XStack)[args___, opts___Rule] := assembleSRow[
    assemble /@ {args},
    Lookup[{opts}, {Alignment, Spacing}, Inherited]
  ];

  XYStack[args___, opts___Rule] := assembleXY[
    assemble /@ {args}, False,
    Lookup[{opts}, {Alignment, Spacing}, Inherited]
  ];

  (SCol|YStack)[args___, opts___Rule] := assembleSCol[
    assemble /@ {args},
    Lookup[{opts}, {Alignment, Spacing}, Inherited]
  ];

  SGrid[array_List, opts___Rule] := assembleSGrid[
    Map[assemble, array, {2}],
    Lookup[{opts}, {HorizontalAlignment, VerticalAlignment, HorizontalSpacing, VerticalSpacing}, Inherited]
  ];

  Style[a___, rule:((FontSize|FontFamily|FontColor|FontWeight) -> _), b___] := Block[
    {$labelStyle = ReplaceOptions[$labelStyle, rule]},
    % @ Style[a, b]
  ];

  Style[a_] := % @ a;

  Translate[g_, pos_] := Scope[
    {g1, sz} = List @@ assemble[g];
    Sized[Translate[g1, pos], sz]
  ];

  Overlay[{g1_, g2_}, opts___] := Scope[
    {g1, s1} = List @@ assemble[g1];
    {g2, s2} = List @@ assemble[g2];
    a = Lookup[{opts}, Alignment, Center];
    If[a === None, g2 //= F; t = s1/2, t = -alignAgainst[s1, s2, a]];
    Sized[{g1, Translate[g2, t]}, s1]
  ];

  Labeled[a_, b_, side:$SidePattern:Bottom, opt___Rule] := Scope[
    {a, s1} = List @@ assemble[a];
    {b, s2} = List @@ assemble[b];
    sp = Lookup[{opt}, Spacing, 0];
    add = $SideToCoords[side] * s2 + $SideToCoords[side] * sp;
    Sized[{a, Translate[b, -alignAgainst[s1, s2, side] + add]}, s1]
  ];

  Padded[e_, padding_] := Scope[
    {g, {w, h}} = List @@ assemble[e];
    {{l, r}, {b, t}} = StandardizePadding[padding];
    Sized[Translate[g, {l, b}], {w + l + r, h + b + t}]
  ];

  Rotate[e_, a:(Pi/2|-Pi/2)] := Scope[
    {g, {w, h}} = List @@ assemble[e];
    c = {w, h} / 2;
    Sized[Rotate[g, a, c], {h, w}]
  ];

  primitives_ := assemblePrimitives @ primitives;
];

assembleSRow[list_List, {valign_, hspacing_}] := Scope[
  sizes = Col2[list];
  SetInherited[valign, $verticalAlignment];
  SetInherited[hspacing, $horizontalSpacing];
  SetScaledFactor[hspacing, Mean @ Col1 @ sizes];
  maxHeight = Max @ Col2 @ sizes;
  yfunc = toAlignFunc[maxHeight, valign];
  list2 = VectorApply[
    x = 0; {g, {w, h}} |-> SeqFirst[Translate[g, {x, yfunc[h]}], x += w + hspacing],
    list
  ];
  Sized[list2, {x - hspacing, maxHeight}]
];

assembleSCol[list_List, {halign_, vspacing_}] := Scope[
  list //= Rev;
  sizes = Col2[list];
  SetInherited[halign, $horizontalAlignment];
  SetInherited[vspacing, $verticalSpacing];
  SetScaledFactor[vspacing, Mean @ Col2 @ sizes];
  maxWidth = Max @ Col1 @ sizes;
  maxHeight = Max @ Col2 @ sizes;
  xfunc = toAlignFunc[maxWidth, halign];
  list2 = VectorApply[
    y = 0; {g, {w, h}} |-> SeqFirst[Translate[g, {xfunc[w], y}], y += h + vspacing],
    list
  ];
  Sized[list2, {maxWidth, y - vspacing}]
];

$toAlignPair = <|
  Left -> {Left, Center}, Right -> {Right, Center}, Top -> {Center, Top}, Bottom -> {Center, Bottom},
  Center -> {Center, Center}, TopLeft -> {Left, Top}, BottomLeft -> {Left, Bottom}, TopRight -> {Right, Top}, BottomRight -> {Right, Bottom}
|>;

(* TODO: invert terms for all corners / sides of a cube! *)
alignAgainst[a:{_, _}, b_, s_Symbol] :=
  alignAgainst[a, b, $toAlignPair @ s];

alignAgainst[{x_, y_}, {mx_, my_}, {ax_, ay_}] :=
  MapThread[toAlignFunc[#2, #3][#1]&, {{x,y}, {mx,my}, {ax,ay}}];

alignAgainst[{x_, y_, z_}, {mx_, my_, mz_}, {ax_, ay_, az_}] :=
  MapThread[toAlignFunc[#2, #3][#1]&, {{x,y,z}, {mx,my,mz}, {ax,ay,az}}];

toAlignFunc[max_, align_] :=
  Switch[align,
    Bottom|Left,          0&,
    Center,               (max - #) / 2&,
    Scaled[_ ? NumericQ], (max - #) * N[F @ align]&,
    Top|Right,            (max - #)&
  ];

assembleSGrid[array_List, {halign_, valign_, hspacing_, vspacing_}] := Scope[
  SetInherited[halign, $horizontalAlignment];
  SetInherited[valign, $verticalAlignment];
  SetInherited[hspacing, $horizontalSpacing];
  SetInherited[vspacing, $verticalSpacing];
  array //= Rev;
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

(**************************************************************************************************)

PublicFunction[AssembleGraphics3D]

PublicHead[XStack, XStackR, YStack, YStackR, XYStack, XYStackR, ZStack, ZStackR, CubeStack]

XStackR[args___, opts___Rule] := XStack[Seq @@ Rev[{args}], opts];
YStackR[args___, opts___Rule] := YStack[Seq @@ Rev[{args}], opts];
ZStackR[args___, opts___Rule] := ZStack[Seq @@ Rev[{args}], opts];
XYStackR[args___, opts___Rule] := XYStack[Seq @@ Rev[{args}], opts];


PublicOption[XSpacing, YSpacing, ZSpacing]

Options[AssembleGraphics3D] = JoinOptions[
  XSpacing -> 1, YSpacing -> 1, ZSpacing -> 1, Spacing -> None,
  Alignment -> Center,
  BaseStyle -> {FontSize -> 30, FontFamily -> "Source Code Pro"},
  Graphics3D
];

AssembleGraphics3D[g_, opts:OptionsPattern[]] := Scope[
  
  UnpackOptions[$xSpacing, $ySpacing, $zSpacing, spacing, $baseStyle, $alignment];

  If[!ListQ[$alignment], $alignment *= {1, 1, 1}];

  If[spacing =!= None, UnpackTuple[spacing, $xSpacing, $ySpacing, $zSpacing]];
  $spacing = {$xSpacing, $ySpacing, $zSpacing};

  prims = assemble3D @ embedSizes3D @ g;
  If[!MatchQ[prims, _Sized], Print[prims]; ReturnFailed[]];

  {xmax, ymax, zmax} = L @ prims;
  prims //= F;
  prims //= SimplifyTranslate;
  
  Graphics3D[prims, BaseStyle -> $baseStyle, PlotRange -> {{0, xmax}, {0, ymax}, {0, zmax}}, FilterOptions @ opts]

];
  
(**************************************************************************************************)

embedSizes3D = Case[
  s:(_XStack | _YStack | _ZStack | _XYStack) := Map[%, s];
  CubeStack[c_, spec:{___Rule}, args___] := CubeStack[% @ c, MapColumn[%, 2, spec], args];
  r_Rule                                 := r;
  Framed[g_]                             := Framed[embedSizes3D @ g];
  Labeled[g_, r___]                      := Labeled[embedSizes3D @ g, r];
  Overlay[list_List, opts___]            := Overlay[embedSizes3D /@ list, opts];
  e_                                     := wrapGraphics3D[e];
];

wrapGraphics3D[Graphics[p_, ___]] := wrapGraphics[p];

wrapGraphics3D[Overlay[{g1_, g2_}]] := Scope[
  {g1, s} = List @@ wrapGraphics3D[g1];
  {g2, s2} = List @@ wrapGraphics3D[g2];
  Sized[{g1, g2}, s]
];

wrapGraphics3D[Spacer[w_ ? NumericQ]] := Sized[{}, {w, 0.0001, 0.0001}];
wrapGraphics3D[Spacer[c:$Coord3P]] := Sized[{}, c];

wrapGraphics3D[Padded[e_, padding_]] := Scope[
  {g, {w, h, d}} = List @@ wrapGraphics3D[e];
  {{l, r}, {b, t}, {u, o}} = StandardizePadding3D[padding];
  Sized[Translate[g, {l, b, u}], {w + l + r, h + b + t, d + u + o}]
];

wrapGraphics3D[g_] := Scope[
  {{xmin, xmax}, {ymin, ymax}, {zmin, zmax}} = plotRange3D @ g;
  xsize = xmax - xmin;
  ysize = ymax - ymin;
  zsize = zmax - zmin;
  Sized[Translate[g, -{xmin, ymin, zmin}], {xsize, ysize, zsize}]
];

plotRange3D = Case[
  Cuboid[min_, max_] := Trans[min, max];
  other_ := GraphicsPlotRange @ Graphics3D @ other;
];

assemble3D = Case[
  Framed[g_] := Scope[
    {res, size} = List @@ assemble3D[g];
    Sized[{Style[Cuboid[{0,0,0}, size], EdgeForm[{Black, AbsoluteThickness[2], Dotted}], FaceForm[None]], res}, size]
  ];
  XStack[args___, opts___Rule] := assembleI[assemble3D /@ {args}, 1, Lookup[{opts}, {Alignment, Spacing}, Inherited]];
  YStack[args___, opts___Rule] := assembleI[assemble3D /@ {args}, 2, Lookup[{opts}, {Alignment, Spacing}, Inherited]];
  ZStack[args___, opts___Rule] := assembleI[assemble3D /@ {args}, 3, Lookup[{opts}, {Alignment, Spacing}, Inherited]];
  XYStack[args___, opts___Rule] := assembleXY[assemble3D /@ {args}, True, Lookup[{opts}, {Alignment, Spacing}, Inherited]];
  Labeled[g_, a_, Below] := Scope[
    {g2, sz} = List @@ assemble3D[g];
    Sized[{g2, Text[a, {0, 0, 0}, {0, -1}, BaseStyle -> $labelStyle]}, sz]
  ];
  Overlay[{a_, b_}, opts___] := assembleOverlay[assemble3D @ a, assemble3D @ b, opts];
  CubeStack[center_, specs:{___Rule}, opts___Rule] := assembleCube[
    assemble3D @ center,
    MapColumn[assemble3D, 2, List @@@ specs],
    Lookup[{opts}, {Alignment, XSpacing, YSpacing, ZSpacing}, Inherited]
  ];
  e_ := e;
];

assembleOverlay[g1_, g2_, opts___] := Scope[
  {g1, s1} = List @@ assemble3D[g1];
  {g2, s2} = List @@ assemble3D[g2];
  a = Lookup[{opts}, Alignment, None];
  If[a === None, g2 //= F; t = s1/2, t = -alignAgainst[s1, s2, a]];
  Sized[{g1, Translate[g2, t]}, s1]
];

assembleXY[list_List, is3D_, {align_, spacing_}] := Scope[
  sizes = Col2[list];
  dxy = Normalize @ If[is3D, {-1.5, 2, 0}, {1, 1}];
  SetInherited[spacing, Mean @ $spacing];
  list2 = VectorApply[
    offset = 0; {g, size} |-> SeqFirst[
      norm = Norm[Take[size, 2]] + spacing/2;
      offset += norm/2;
      Translate[g, -size/2 + offset * dxy],
      offset += norm/2;
    ],
    list
  ];
  t1 = list2[[1, 2]];
  Sized[Translate[list2, -t1], dxy * offset]
];

assembleI[list_List, i_, {align_, spacing_}] := Scope[
  sizes = Col2[list];
  SetInherited[align, Part[$alignment, i]];
  SetInherited[spacing, Part[$spacing, i]];
  SetScaledFactor[spacing, Mean @ Part[sizes, All, i]];
  maxCoords = Map[Max, Transpose @ sizes];
  afuncs = toAlignFunc[#, align]& /@ maxCoords;
  offset = 0;
  list2 = VectorApply[
    offset = 0; {g, size} |-> SeqFirst[
      Translate[g, RepPart[MapThread[Construct, {afuncs, size}], i -> offset]],
      offset += Part[size, i] + spacing
    ],
    list
  ];
  size = RepPart[maxCoords, i -> (offset - spacing)];
  Sized[list2, size]
];

assembleCube[Sized[g_, size_], specs_, {align_, xSpacing_, ySpacing_, zSpacing_}] := Scope[

  posl = posr = posb = post = posu = poso = size/2;
  posl[[1]] = 0;         posb[[2]] = 0;         posu[[3]] = 0;
  posr[[1]] = size[[1]]; post[[2]] = size[[2]]; poso[[3]] = size[[3]];

  InheritedBlock[{$xSpacing, $ySpacing, $zSpacing},
    If[xSpacing =!= Inherited, $xSpacing ^= xSpacing];
    If[ySpacing =!= Inherited, $ySpacing ^= ySpacing];
    If[zSpacing =!= Inherited, $zSpacing ^= zSpacing];
    res = Flatten @ {g, procSpec @@@ specs}
  ];

  min = VectorMin[posl, posb, posu];
  max = VectorMax[posr, post, poso];
  Sized[Translate[res, -min], max - min]
,
  procSpec[Left,   Sized[h_, s:{dx_, dy_, dz_}]] := P2 @ {posl[[1]] -= dx/2 + $xSpacing, Translate[h, -s/2 + posl], posl[[1]] -= dx/2},
  procSpec[Right,  Sized[h_, s:{dx_, dy_, dz_}]] := P2 @ {posr[[1]] += dx/2 + $xSpacing, Translate[h, -s/2 + posr], posr[[1]] += dx/2},
  procSpec[Bottom, Sized[h_, s:{dx_, dy_, dz_}]] := P2 @ {posb[[2]] -= dy/2 + $ySpacing, Translate[h, -s/2 + posb], posb[[2]] -= dy/2},
  procSpec[Top,    Sized[h_, s:{dx_, dy_, dz_}]] := P2 @ {post[[2]] += dy/2 + $ySpacing, Translate[h, -s/2 + post], post[[2]] += dy/2},
  procSpec[Under,  Sized[h_, s:{dx_, dy_, dz_}]] := P2 @ {posu[[3]] -= dz/2 + $zSpacing, Translate[h, -s/2 + posu], posu[[3]] -= dz/2},
  procSpec[Over,   Sized[h_, s:{dx_, dy_, dz_}]] := P2 @ {poso[[3]] += dz/2 + $zSpacing, Translate[h, -s/2 + poso], poso[[3]] += dz/2}
];