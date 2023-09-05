PublicSymbol[CommutativeDiagram]

SetUsage @ "
CommutativeDiagram[objects$, arrows$] evaluates to a graphics object that contains a commutative diagram.
* objects is a grid of objects (which are their own labels) or label$ -> object$ pairs.
* arrows is a list of arrows, given as:
| src$ -> dst$ | an unlabeled arrow |
| {rule$, label$} | a labeled arrow |
| {rule$, label$, type$} | a labeled arrow of type type$ |
| %MorphismArrow[$$] | a fully specified arrow |
* the type$ given above include 'Iso', 'Epi', 'Mono', 'MapsTo', 'DoubleArrow', 'Equality', 'Line'.
"

Options[CommutativeDiagram] = {
  Transposed -> False,
  GraphicsScale -> None,
  Alignment -> Center
}

declareGraphicsFormatting[cd:CommutativeDiagram[_List, _List, ___Rule] :> cdToBoxes[cd], Graphics];

cdToBoxes[cd_] := ToGraphicsBoxes @ cdToPrimitives[cd];

Format[cd:CommutativeDiagram[a1_List, a2_List, opts___Rule], StandardForm] :=
  ScaleGraphics[
    cdToPrimitives[CommutativeDiagram[a1, a2, opts, ArrowPathSetback -> 0.15]],
    GraphicsScale -> Lookup[{opts}, GraphicsScale, 60],
    ImagePadding -> Lookup[{opts}, ImagePadding, 10],
    AdjustFontSize -> False
  ];

cdToPrimitives[CommutativeDiagram[grid_List, arrows_List, opts___Rule]] := Scope[
  UnpackOptionsAs[CommutativeDiagram, {FilterOptions[CommutativeDiagram, opts]}, transposed, alignment];
  $objectLabels = {};
  gridSpec = parseGridSpec @ grid;
  arrowPrimitives = parseMorphism /@ arrows;
  {
    LabelFontSize -> 18, FontSize -> 20, FontFamily -> "KaTeX_Main", opts,
    GridComplex[gridSpec, {$objectLabels, arrowPrimitives}, Transposed -> transposed]
  }
]

parseGridSpec = Case[
  rules:{__Rule} := MapColumn[parseObject, 2, rules];
  grid:{__}      := Map[parseObject, ToList @ #]& /@ grid;
];

parseObject = Case[
  obj_             := % @ Rule[obj, obj];
  Rule[lbl_, None] := lbl;
  Rule[lbl_, obj_] := (AppendTo[$objectLabels, fmtLabel[lbl, obj]]; lbl);
]

fmtLabel[lbl_, obj_] := Text[obj, lbl, Lookup[$sideToLabelOffset, alignment]];
fmtLabel[lbl_, c_Customized] := customizedBlock[c, {Alignment} :> {alignment}, fmtLabel[lbl, #]&];

parseMorphism = Case[
  opt:(_Symbol -> _)                          := opt;
  r_Rule                                      := % @ {r};
  de_DirectedEdge                             := % @ {de};
  de_DirectedEdge -> rhs_                     := % @ {de, rhs};
  {Rule[s_, t_] | DirectedEdge[s_, t_], lbl_:None, type_:Automatic} :=
    MorphismArrow[{s, t}, Switch[lbl, None, {}, _List, lbl, _, {{0.5, Above} -> lbl}], type];
  other_                                      := other;
]


PublicFunction[CommutativeSquare]

CommutativeSquare[{nw_, ne_, se_, sw_}, {n_, e_, s_, w_}, opts___Rule] :=
  CommutativeDiagram[
    {{1, 1} -> "NW" -> nw, {2, 1} -> "NE" -> ne, {1, 2} -> "SW" -> sw, {2, 2} -> "SE" -> se},
    {MorphismArrow[{"NW", "NE"}, n, LabelPosition -> Above],
     MorphismArrow[{"NE", "SE"}, e, LabelPosition -> Right],
     MorphismArrow[{"NW", "SW"}, w, LabelPosition -> Left],
     MorphismArrow[{"SW", "SE"}, s, LabelPosition -> Bottom]},
    opts, GraphicsScale -> 125
  ];

