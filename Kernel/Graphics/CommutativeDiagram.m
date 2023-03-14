PublicSymbol[CommutativeDiagram]

CommutativeDiagram[grid_List, arrows_List] := Scope[
  $objectLabels = {};
  gridSpec = Map[parseObject, ToList @ #]& /@ grid;
  arrowPrimitives = parseMorphism /@ arrows;
  Graphics @ {
    ArrowPathSetback -> 0.15, FontSize -> 20, FontFamily -> "KaTeX_Main",
    GridComplex[gridSpec, {$objectLabels, arrowPrimitives}]
  }
]

parseObject = Case[
  obj_             := % @ Rule[obj, obj];
  Rule[lbl_, None] := lbl;
  Rule[lbl_, obj_] := (AppendTo[$objectLabels, Text[obj, lbl]]; lbl);
]

parseMorphism = Case[
  r_Rule                                      := % @ {r};
  ma_MorphismArrow                            := ma;
  {Rule[s_, t_], lbl_:None, type_:Automatic}  := MorphismArrow[{s, t}, Switch[lbl, None, {}, _List, lbl, _, {{0.5, Above} -> lbl}], type];
]