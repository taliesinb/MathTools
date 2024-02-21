PublicFunction[TextToPolygon]

CacheVariable[$TextToPolygonCache]

SetUsage @ "
TextToPolygon['text$'] turns text into a polygon, returning a tuple of {primitives$, bounds$, shift$}.
* the 'OperatorSubstitution' option is used to ensure the font applies to characters like '['.
* the result is cached.
* the polygon is shifted so that the baseline is exactly the X axis.
* certain Unicode characters have special polygons associated with them, for example BLACK RIGHTWARDS ARROW.
* font style options like %FontSize, %FontFamily etc. can be given as options.
"

Options[TextToPolygon] = {
  FontSize -> 20,
  FontFamily :> $MathFont,
  FontWeight -> Plain,
  FontSlant -> Plain
}
TextToPolygon[text_Str, OptionsPattern[]] := Scope[
  style = OptionValue[{FontSize, FontFamily, FontWeight, FontSlant}];
  CachedInto[$TextToPolygonCache, {text, style}, iTextToPolygon[text, style]]
];

iTextToPolygon[str_, {fontSize_, fontFamily_, fontWeight_, fontSlant_}] := Scope[

  text = Text @ Style[str <> ".",
    FontSize -> fontSize,
    FontFamily -> fontFamily,
    FontWeight -> fontWeight,
    FontSlant -> fontSlant,
    PrivateFontOptions -> {"OperatorSubstitution" -> False}
  ];

  polygons = RegionPolygon @ BoundaryDiscretizeGraphics[text, _Text, MaxCellMeasure -> 0.05];

  (* was a whitespace string? *)
  If[MatchQ[polygons, _Polygon | {_Polygon}], Return @ {{}, {{0, 0}, {0, 0}}, 0}];

  (* extract the final dot by its x position *)
  dotIndex = MaximumIndexBy[polygons, DeepFirstCase[#, $NumberP]&];
  dot = Part[polygons, dotIndex];
  polygons = Delete[polygons, dotIndex];
  {lr, {b, t}} = CoordinateBounds @ P1 @ dot;

  $alignTrans = Threaded[{0, -b}];
  polygons //= alignPolygons;

  (* hacky but cheap *)
  bounds = PrimitiveBoxesBounds[polygons /. {Polygon -> PolygonBox, Line -> LineBox, Disk -> DiskBox, Circle -> CircleBox}];

  If[Len[polygons] === 1, polygons //= P1];

  List[polygons, bounds, 2]
];

alignPolygons = Case[
  m_ ? MatrixQ := ToPackedReal[$alignTrans + m];
  list_List := % /@ list;
  Polygon[m_List, o___] := Polygon[% @ m, o];
  Polygon[r_Rule, o___] := Polygon[% /@ r, o];
]

$unicodeToNamedIcon = Assoc[
  "➡︎" -> {"BoldRightArrow", .75, 1},
  "⬅︎" -> {"BoldLeftArrow",  .75, 1},
  "⬆︎" -> {"BoldUpArrow",    .75, 3},
  "⬇︎" -> {"BoldDownArrow",  .75, 3}
];

iTextToPolygon[letter:(Alternatives @@ Keys[$unicodeToNamedIcon]), {fontSize_, _, _, _}] := Scope[
  {letter, scale, bshift} = $unicodeToNamedIcon @ letter;
  {prims, boxes, boxes3D, bounds, solid} = NamedIconData[letter];
  h = scale * fontSize/4 + (bshift * fontSize/20); s = scale * fontSize/2;
  prims = TranslatePrimitives[ScalePrimitives[prims, s], {0, h}];
  bounds = Take[bounds, 2]*s + {{0,0},{1,1} * h};
  bounds[[2, 1]] //= MinOperator[0];
  {prims, bounds, 5}
];

(**************************************************************************************************)

PrivateFunction[CenterTextVertical]

CenterTextVertical[txt:Text[label_, pos_, {0|0., 0|0.}, args___]] := Scope[
  centroid = TextCentroid @ txt;
  Text[label, pos, (centroid * 2 - 1), args]
];

CenterTextVertical[text_] := text;

(**************************************************************************************************)

CacheVariable[$TextCentroidCache]

PublicFunction[TextCentroid]

TextCentroid[text_Text] := Scope @ CachedInto[
  $TextCentroidCache, Hash @ text,
  img = Binarize[MakeTextImage[text], .9];
  {w, h} = ImageDimensions @ img;
  xys = N @ PixelValuePositions[img, 0];
  If[xys === {}, {.5, .5},
    {xs, ys} = Transpose @ xys;
    xs = Clip[xs, Quantile[xs, {.3, .7}]];
    ys = Clip[ys, Quantile[ys, {.15, .85}]];
    x = Mean @ xs; y = Mean @ MinMax @ ys;
    x = Rescale[x, {1, w}];
    y = Rescale[y, {1, h}];
    If[Abs[x - .5] < .15, x = .5];
    If[Abs[y - .5] < .05, y = Avg[y, .5]];
    {x, y}
  ]
]

(**************************************************************************************************)

PrivateFunction[ApplyScriptScaling]

SetUsage @ "
ApplyScriptScaling[form$] attempts to adjust fixed-font-size forms when they occur in script position.
* ApplyScriptScaling affects %GradientSymbol and %TextIcon.
* this is achieved by consulting an internal registry that knows which forms apply script position to their arguments.
"

$formScriptingFormP := $formScriptingFormP =
  Apply[Alternatives, Blank /@ Keys[$formScriptingArgumentPositions]];

ApplyScriptScaling[expr_] := ReplaceAll[
  expr,
  sub:$formScriptingFormP :> RuleCondition @ rescriptSubExpr[sub]
]

rescriptSubExpr[sub_] := MapIndices[rescript, $formScriptingArgumentPositions @ H @ sub, sub];

$scalingHeadP = (GradientSymbol | TextIcon);
rescript[e_] := ReplaceAll[e,
  (h:$scalingHeadP)[l___, FontSize -> fs_, r___] :> h[l, FontSize -> Round[fs * 0.66], r]
];
