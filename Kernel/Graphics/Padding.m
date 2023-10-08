PrivateFunction[ImageSizePad]

SetUsage @ "
ImageSizePad[size$, padding$] expands the image size size$ by padding it according to padding$.
* padding$ can be any of the specifications supported by %ImagePadding.
"

ImageSizePad::badpadding = "`` is not a valid padding spec.";

ImageSizePad[imageSize_, paddingSpec_] := Scope[
  If[!MatchQ[imageSize, {_ ? NumericQ, _ ? NumericQ}], ReturnFailed[]];
  {pw, ph} = padding = processPadding[ImageSizePad, paddingSpec];
  {w, h} = imageSize;
  {w + Total[pw], h + Total[ph]}
];

processPadding[head_, paddingSpec_] := Scope[
  padding = Ceiling @ StandardizePadding @ paddingSpec;
  If[RealMatrixQ[padding] && Dimensions[padding] === {2, 2},
    padding,
    ReturnFailed[MessageName[head, "badpadding"], paddingSpec]
  ]
];

(**************************************************************************************************)

PrivateFunction[FindAutomaticPadding]

FindAutomaticPadding[g_Graphics, scale_] := Scope[
  oldBounds = GraphicsPlotRange[g];
  $scale = scale; $aps = 1;
  $pbag = Bag[];
  $fontSize = $fontFamily = $fontWeight = Inherited;
  autoPad @ ReplaceAll[NCache[_, n_] :> n] @ First @ ToBoxes @ g;
  StuffBag[$pbag, Transpose @ oldBounds, 1];
  points = ResolveOffsets[BagPart[$pbag, All], $scale];
  newBounds = CoordinateBounds @ points;
  SowGraphics[g];
  SowGraphics[1, BoundingRectangle @ oldBounds];
  SowGraphics[2, BoundingRectangle @ newBounds];
  Abs[newBounds - oldBounds]
];

$interP = TagBox|Annotation|InterpretationBox;

(* TODO: support Offset, and allow for EdgeThickness, properly handle FontSize and FontFamily *)
autoPad = Case[
  Translate[g_, p_]              := Block[{$t = $t + p}, % @ g];
  l_List                         := InheritedBlock[{$aps, $fontSize, $fontFamily, $fontWeight}, Scan[%, l]];
  Directive[e_]                  := % @ e;
  Directive[{e___}]              := Scan[%, e];
  FontSize -> fs_                := $fontSize = fs;
  FontFamily -> ff_              := $fontFamily = ff;
  FontWeight -> fw_              := $fontWeight = fw;
  AbsolutePointSize[ps_]         := ($aps = ps);
  PointBox[p:$Coord2P]           := padPoly[p, (1+$aps)/$scale/2 * {{1,1}, {1,-1}, {-1,-1}, {-1,1}}];
  PointBox[l_List]               := Scan[%[Point[#]]&, p];
  StyleBox[b_, s___]             := % @ {s, b};
  $interP[b_, ___]               := % @ b;
  e_InsetBox                     := autoPadInset[e];
  other_                         := other;
];

$autoFS := {FontSize -> $fontSize, FontFamily -> $fontFamily, FontWeight -> $fontWeight};

FindAutomaticPadding::badisize = "Bad ImageSize -> ``."
FindAutomaticPadding::unrecog = "Unrecognized ``."

autoPadInset = Case[

  InsetBox[FormBox[txt_, TraditionalForm], pos_, offset:Except[_Rule]:ImageScaled[{0.5,0.5}], sz_:Automatic, dir:Except[_Rule]:{1,0}, opts___Rule] :=
    padText @ Text[RawBoxes @ txt, pos, Replace[offset, ImageScaled[s_] :> (s - 0.5) * 2], dir, opts];

  i:InsetBox[g_GraphicsBox, pos_, origin_:{0,0}, Automatic, dir_:{1, 0}, ___] := Scope[
    isize = LookupOption[g, ImageSize, None];
    Switch[isize,
      $NumberP, w = isize,
      $Coord2P, {w, h} = isize,
      _, Message[FindAutomaticPadding::badisize, MsgExpr @ isize]; Return[];
    ];
    {{x1, x2}, {y1, y2}} = plotRange = GraphicsPlotRange @ g;
    {pw, ph} = {x2 - x1, y2 - y1};
    aspectRatio = (y2 - y1) / (x2 - x1);
    {ox, oy} = Replace[origin, ImageScaled[{a_, b_}] :> {Lerp[x1, x2, a], Lerp[y1, y2, b]}];
    SetAutomatic[h, w * aspectRatio];
    SetAutomatic[w, h / aspectRatio];
    scale = (w / pw) / $scale;
    dirx = Normalize[dir]; diry = VectorRotate90[dirx];
    {l, r} = {x2 - ox, x1 - ox} * scale; l *= dirx; r *= dirx;
    {b, t} = {y2 - oy, y1 - oy} * scale; b *= diry; t *= diry;
    padPoly[pos, {l + b, l + t, r + t, r + b}];
  ];

  other_ := (Message[FindAutomaticPadding::unrecog, MsgExpr @ other];);
];

padText[Text[txt_, pos_, offset:Except[_Rule]:{0,0}, dirx:Except[_Rule]:{1,0}, opts___Rule]] := Scope[
  pos = ResolveOffsets[pos, $scale];
  If[FreeQ[{opts}, BaseStyle], opts = Sequence[opts, BaseStyle -> $autoFS]];
  {w, h} = TextRasterSize @ Text[txt, pos, opts] + 1;
  dirx = Normalize[dirx] / $scale;
  diry = VectorRotate90[dirx];
  dirx = dirx * w/2;
  diry = diry * h/2;
  off = Mean[{dirx, diry} * offset] * -2;
  padPoly[off + pos, {dirx -diry, -dirx +diry, dirx -diry, dirx + diry}];
];

padPoly[p_, pnts_] := padPoly[Threaded[p] + pnts];
padPoly[p_] := StuffBag[$pbag, SowGraphics[Polygon @ p]; p, 1];

padPoint[p_? MatrixQ] := StuffBag[$pbag, EchoDebugGraphics @ p, 1];
padPoint[p_] := StuffBag[$pbag, EchoDebugGraphics @ p];

(**************************************************************************************************)

PrivateFunction[StandardizePadding]

SetUsage @ "
StandardizePadding[spec$] standardizes a padding specification spec$.
* StandardizePadding returns {{l$, r$}, {b$, t$}}.
* StandardizePadding accepts the following forms:
| None | no padding |
| n$ | pad by n$ on all sides |
| {h$, v$} | pad by h$ horizontally and v$ vertically |
| {{l$, r$}, {b$, t$}} | explicit padding |
| {Left -> l$, $$} | per-side padding |
* Sides can be Horizontal or Vertical to indicate both sides.
"

StandardizePadding = Case[
  All                              := All;
  None                             := {{0, 0}, {0, 0}};
  p:$NumberP                       := N @ {{p, p}, {p, p}};
  c:$Coord2P                       := N @ c;
  s:{$Coord2P, $Coord2P}           := N @ s;

  rule_Rule                        := % @ {rule};
  rules:{Rule[sideP, $NumberP]...} := N @ LookupSide[rules, {{Left, Right}, {Bottom, Top}}];

  _                                := $Failed;
,
  {sideP -> $SidePattern|Horizontal|Vertical|All}
];

(**************************************************************************************************)

PrivateFunction[StandardizePadding3D]

SystemSymbol[Under, Over]

Under::usage = "Under is like Bottom but for the Z coordinate.";
Over::usage = "Over is like Top but for the Z coordinate.";

SetUsage @ "
StandardizePadding3D[spec$] standardizes a 3D padding specification spec$.
* StandardizePadding3D returns {{l$, r$}, {b$, t$}, {u$, o$}}.
* StandardizePadding3D accepts the following forms:
| None | no padding |
| n$ | pad by n$ on all sides |
| {x$, y$, z$} | pad on both sides on the x$, $y and $z axes |
| {{l$, r$}, {b$, t$}, {$u, o$}} | explicit padding |
| {Left -> l$, $$} | per-side padding |
"

StandardizePadding3D = Case[
  All                              := All;
  None                             := {{0, 0}, {0, 0}, {0, 0}};
  p:$NumberP                       := N @ {{p, p}, {p, p}, {p, p}};
  c:$Coord3P                       := N @ c;
  s:{$Coord2P, $Coord2P, $Coord2P} := N @ s;

  rule_Rule                        := % @ {rule};
  rules:{Rule[sideP, $NumberP]...} := N @ LookupSide[rules, {{Left, Right}, {Bottom, Top}, {Under, Over}}];

  _                                := $Failed;
,
  {sideP -> $SidePattern|Horizontal|Vertical|Under|Over|All}
];

(**************************************************************************************************)

PrivateFunction[LookupSide]

LookupSide[rules_, sides_List] :=
  Map[LookupSide[rules, #]&, sides];

LookupSide[rules_, side_] :=
  Lookup[rules, side, Lookup[rules, toSideClass @ side, Lookup[rules, All, 0]]];

LookupSide[rules_, side_] :=
  Lookup[rules, side,
  Lookup[rules, toSideClass @ side,
  Lookup[rules, toMultiClassC @ side,
  Lookup[rules, toMultiClassA @ side,
  Lookup[rules, All, 0]]]]];

toMultiClassC = <|Bottom -> BottomLeft, Left -> TopLeft, Top -> TopRight, Right -> BottomRight|>;
toMultiClassA = <|Bottom -> BottomRight, Left -> BottomLeft, Top -> TopLeft, Right -> TopRight|>;

LookupSide[rules_][side_] :=
  LookupSide[rules, side];

toSideClass = Case[
  Left|Right := Horizontal;
  Bottom|Top := Vertical;
  other_     := Null;
]
