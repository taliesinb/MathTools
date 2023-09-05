PublicHead[GridComplex, GridOffset]
PublicOption[GridScale]

SetUsage @ "
GridComplex[labels$, primitives$] represents a grid of graphics primitives where labels$ is a grid of \
abstract labels that are interpreted in primitives$ as referring to 2D coordinates.
GridComplex[{coord$1 -> label$1, coord$2 -> label$2, $$}, $$] manually specifies coordinates of labels.
* GridComplex takes the following options:
| GridScale | the size of individual cells |
| Transposed | whether coordinates correspond to {x$,y$} or {y$,x$} |
"

Options[GridComplex] = {
  Transposed -> True,
  GridScale -> 1
}

declareCustomGraphicsHead[GridComplex];

Typeset`MakeBoxes[gc:GridComplex[_List, _, opts___Rule], form:StandardForm | TraditionalForm, type:Graphics|Graphics3D] :=
  gridComplexBoxes[gc, form, type];

gridComplexBoxes[gc_GridComplex, form_, type_] := Scope[
  primitives = gridComplexPrimitives @ gc;
  Construct[Typeset`MakeBoxes, primitives, form, type]
];

(**************************************************************************************************)

readGrid[rules:{({_, _} -> _)..}] := Scope[
  $gridCoords = Association @ Reverse[rules, {2}];
  {$gridCoords, Max /@ Transpose @ Values @ $gridCoords}
]

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

ExpandGridComplex[g_] := ReplaceAll[g, gc:GridComplex[_List, _, ___Rule] :> RuleCondition @ gridComplexPrimitives @ gc];

(**************************************************************************************************)

PrivateFunction[gridComplexPrimitives]

gridComplexPrimitives[GridComplex[grid_List, primitives_, opts:OptionsPattern[]]] := Scope[
  UnpackOptionsAs[GridComplex, {opts}, gridScale, transposed];
  {coords, size} = readGrid @ grid;
  {r, c} = size;
  SetAutomatic[gridScale, 1];
  If[!transposed, coords = Reverse[coords, 2]; {r,c} = {c,r}];
  rules = Dispatch @ Normal @ VectorApply[{#2 - 1, r - #1} * gridScale&, coords];
  border = Invisible @ Point @ {{0, 0}-gridScale/5, ({c, r} - 1 + 1/5) * gridScale};
  replacement = # /. rules /. GridOffset[d_, p_] :> p + gridScale * d&;
  {
    recurseGraphicsCoordinates[replacement, primitives],
    border
  }
];