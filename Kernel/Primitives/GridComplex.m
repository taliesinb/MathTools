PublicGraphicsPrimitive[GridComplex]

PublicHead[GridOffset]
PublicOption[GridScale]

SetUsage @ "
GridComplex[labels$, primitives$] represents a grid of graphics primitives where labels$ is a grid of \
abstract labels that are interpreted in primitives$ as referring to 2D coordinates.
GridComplex[{coord$1 -> label$1, coord$2 -> label$2, $$}, $$] manually specifies coordinates of labels.
* the option %GridScale sets the size of individual cells.
"

Options[GridComplex] = {
  GridScale -> 1
}

DeclareGraphicsPrimitive[GridComplex, "Rules,Primitives", gridComplexBoxes];

gridComplexBoxes[gc:GridComplex[_List, _, ___Rule]] :=
  Construct[Typeset`MakeBoxes, gridComplexPrimitives @ gc, StandardForm, Graphics];

GridComplex /: Normal[gc_GridComplex] := gridComplexPrimitives[gc];

(**************************************************************************************************)

gridComplexPrimitives[GridComplex[grid_List, primitives_, opts:OptionsPattern[]]] := Scope[
  UnpackOptionsAs[GridComplex, {opts}, gridScale];
  {coords, size} = readGrid @ grid;
  {r, c} = size;
  SetAutomatic[gridScale, 1];
  rules = Normal @ VectorApply[{#2 - 1, r - #1} * gridScale&, coords];
  border = Invisible @ Point @ {{0, 0}-gridScale/5, ({c, r} - 1 + 1/5) * gridScale};
  {
    ReplacePrimitiveCoordinates[primitives, rules] /. GridOffset[d_, p_] :> p + gridScale * d,
    border
  }
];

(**************************************************************************************************)

readGrid[rules:{({_, _} -> _)..}] := Scope[
  $gridCoords = Assoc @ Rev[rules, {2}];
  {$gridCoords, Max /@ Transpose @ Values @ $gridCoords}
]

readGrid[rows_List] := Scope[
  $gridCoords = <||>;
  MapIndex1[readGridRow, rows];
  KDropFrom[$gridCoords, {None, Null}];
  {$gridCoords, Max /@ Transpose @ Values @ $gridCoords}
];

readGridRow[row_List, r_] := MapIndex1[readGridElem[r], row];
readGridRow[e_, r_] := $gridCoords[e] = {r, 1};
readGridElem[r_][e_, c_] := $gridCoords[e] = {r, c};
