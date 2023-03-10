PublicHead[GridComplex, GridOffset]

Typeset`MakeBoxes[gc:GridComplex[_List, _, _:Automatic], form:StandardForm | TraditionalForm, type:Graphics|Graphics3D] :=
  gridComplexBoxes[gc, form, type];

gridComplexBoxes[gc_GridComplex, form_, type_] := Scope[
  primitives = gridComplexPrimitives @ gc;
  Construct[Typeset`MakeBoxes, primitives, form, type]
];

(**************************************************************************************************)

readGrid[rows_List] := Scope[
  $gridCoords = <||>;
  MapIndex1[readGridRow, rows];
  KeyDropFrom[$gridCoords, {None, Null}];
  {$gridCoords, Max /@ Transpose @ Values @ $gridCoords}
];

readGridRow[row_List, r_] := MapIndex1[readGridElem[r], row];
readGridRow[e_, r_] := $gridCoords[e] = {r, 1};
readGridElem[r_][e_, c_] := $gridCoords[e] = {r, c};

(**************************************************************************************************)

PublicFunction[ExpandGridComplex]

ExpandGridComplex[g_] := ReplaceAll[g, gc:GridComplex[_List, _, _:Automatic] :> RuleCondition @ gridComplexPrimitives @ gc];

(**************************************************************************************************)

PrivateFunction[gridComplexPrimitives]

gridComplexPrimitives[GridComplex[grid_List, primitives_, scale_:1]] := Scope[
  {coords, size} = readGrid @ grid;
  {r, c} = size;
  SetAutomatic[scale, 1];
  rules = Dispatch @ Normal @ VectorApply[{#2 - 1, r - #1} * scale&, coords];
  border = Invisible @ Point @ {{0, 0}-scale/5, ({c, r} - 1 + 1/5) * scale};
  replacement = # /. rules /. GridOffset[d_, p_] :> p + scale * d&;
  {
    recurseGraphicsCoordinates[replacement, primitives],
    border
  }
];