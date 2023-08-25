PublicSymbol[CommutativeDiagram]

SetUsage @ "
CommutativeDiagram[objects$, arrows$] evaluates to a graphics object that contains a commutative diagram.
* objects is a grid of objects (which are their own labels) or label$ -> object$ pairs.
* arrows is a list of arrows, given as:
| src$ -> dst$ | an unlabeled arrow |
| {rule$, label$} | a labeled arrow |
| {rule$, label$, type$} | a labeled arrow of type type$ |
| %MorphismArrow[$$] | a fully specified arrow |
* the type$ given above can be one of 'Iso', 'Epi', 'Mono', 'Hook', 'MapsTo'.
"

declareGraphicsFormatting[cd:CommutativeDiagram[_List, _List, ___] :> cdToBoxes[cd], Graphics];

cdToBoxes[cd_] := ToGraphicsBoxes @ cdToPrimitives[cd];

Format[cd:CommutativeDiagram[_List, _List, ___], StandardForm] := Graphics[{ArrowPathSetback -> 0.15, cdToPrimitives[cd]}];

cdToPrimitives[CommutativeDiagram[grid_List, arrows_List, scale_:Automatic, opts___Rule]] := Scope[
  $objectLabels = {};
  gridSpec = parseGridSpec @ grid;
  arrowPrimitives = parseMorphism /@ arrows;
  {
    FontSize -> 20, FontFamily -> "KaTeX_Main", opts,
    GridComplex[gridSpec, {$objectLabels, arrowPrimitives}, scale]
  }
]

parseGridSpec = Case[
  rules:{__Rule} := MapColumn[parseObject, 2, rules];
  grid:{__}      := Map[parseObject, ToList @ #]& /@ grid;
];

parseObject = Case[
  obj_             := % @ Rule[obj, obj];
  Rule[lbl_, None] := lbl;
  Rule[lbl_, obj_] := (AppendTo[$objectLabels, Text[obj, lbl]]; lbl);
]

parseMorphism = Case[
  opt:(_Symbol -> _)                          := opt;
  r_Rule                                      := % @ {r};
  {Rule[s_, t_], lbl_:None, type_:Automatic}  := MorphismArrow[{s, t}, Switch[lbl, None, {}, _List, lbl, _, {{0.5, Above} -> lbl}], type];
  other_                                      := other;
]