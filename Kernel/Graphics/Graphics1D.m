PublicTypesettingForm[Graphics1D]


PublicOption[JitterSize]

Options[Graphics1D] = JoinOptions[
  ImageSize     -> 600,
  Ticks         -> {True, False},
  JitterSize    -> 5,
  FixedGraphics
];

Graphics1D[g_, opts___Rule] := Scope[

  UnpackOptionsAs[Graphics1D, {opts}, imageSize, ticks, jitterSize, imagePadding];

  width = F[imageSize, imageSize];

  $xBounds = None;
  $occAssoc = UAssoc[]; $jitter = 1;
  primitives = proc1D @ g;
  xsize = If[$xBounds === None, 1, Max[Dist @@ $xBounds, .001]];

  $occAssoc = UAssoc[]; $jitter = N[jitterSize * xsize / width];
  primitives = proc1D @ g;

  scale = Ceiling @ N[width / xsize];

  FixedGraphics[
    primitives,
    GraphicsScale -> scale,
    Ticks -> ticks,
    FilterOptions @ opts
  ]
];

boundX[x_] := ($xBounds = MinMax[DeleteNone @ {$xBounds, x}]; x);
chooseY[x_] := Mod[KeyAddTo[$occAssoc, Round[x, .0001], 1], 4] * $jitter;

(**************************************************************************************************)

to2D = Case[
  x:$NumberP := {boundX @ x, chooseY @ x};
  l_List     := Map[%, l];
];

to2DM = Case[
  xs:{$NumberP..} := Module[{ys = Map[chooseY, xs]}, Thread @ List[boundX @ xs, Mean[ys]]];
  xs:{__List}     := Map[%, xs]
];

(**************************************************************************************************)

proc1D = Case[
  (h:$vec)[p_, a___]         := h[to2D @ p, a];
  (h:$mat)[p_, a___]         := h[to2DM @ p, a];
  (h:$opvec)[f_, v_, a___]   := h[f, to2D @ v, a];
  (h:$primvec)[p_, v_, a___] := h[% @ p, to2D @ v, a];
  (h:$prim)[p_, a___]        := h[% @ p, a];
  list_List                  := Map[%, list];
  other_                     := other;
,
  {$vec     -> Alt[Point, Circle, Disk],
   $mat     -> Alt[Line, Arrow, ExtendedArrow, DebugPoints],
   $opvec   -> Alt[Inset, Text],
   $primvec -> Alt[Translate],
   $prim    -> Alt[Annotation, Button, EventHandler, Mouseover, NiceTooltip, Tooltip, PopupWindow, StatusArea, Style]}
];

