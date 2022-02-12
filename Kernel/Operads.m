(**************************************************************************************************)

PackageExport["Polynomial"]

SetAttributes[Polynomial, HoldRest];

declareBoxFormatting[
  poly:Polynomial[_List, _] :> makePolynomialBoxes @ poly
];

makePolynomialBoxes[Polynomial[vars_List, body_]] := Scope[
  vars = # -> SymbolForm[SymbolName[#]]& /@ vars;
  ToBoxes[HoldForm[body] /. vars /. {Times -> ImplicitTimesForm, Plus -> PlusForm}]
];

(**************************************************************************************************)

PackageExport["PolynomialGraph"]

Options[PolynomialGraph] = {
  ItemFunction -> Automatic
};

PolynomialGraph[expr_, opts:OptionsPattern[]] := Scope[
  UnpackOptions[itemFunction];
  SetAutomatic[itemFunction, polyItemFunction];
  vars = Association[];
  $varCount = 1; $edges = {}; $vlabels = {};
  toPolyGraph[expr];
  vertexRange = Range[1, $varCount-1];
  ExtendedGraph[
    vertexRange,
    Flatten @ $edges,
    VertexAnnotations -> <|"Data" -> Lookup[$vlabels, vertexRange, None]|>,
    GraphTheme -> "PolynomialGraph",
    FilterOptions @ opts
  ]
];

addPoly[p:Polynomial[vars_, body_], inputIndices_] := Scope[
  bodyIndex = $varCount++;
  AppendTo[$vlabels, bodyIndex -> itemFunction[p]];
  AppendTo[$edges, MapThread[DirectedEdge[bodyIndex, #1, #2]&, {inputIndices, vars}]];
  bodyIndex
];

toPolyGraph = Case[
  None := ($varCount++);
  poly:Polynomial[vars_, _] := addPoly[poly, Table[$varCount++, Length @ vars]];
  (poly_Polynomial)[inputs___] := addPoly[poly, Map[%, {inputs}]];
  body_ := (AppendTo[$vlabels, $varCount -> Form[body]]; $varCount++);
];

polyItemFunction[Polynomial[vars_, body_]] :=
  QuiverProductPolyForm @ ReplaceAll[HoldForm[body], Flatten[toVarRules /@ vars]];

toVarRules[var_] := var -> Style[var, CardinalColor[var]];

(**************************************************************************************************)

$GraphThemeData["PolynomialGraph"] := $PolynomialGraphThemeData;

$PolynomialGraphThemeData = {
  VertexLayout -> TreeVertexLayout[Orientation -> Top, Balanced -> True],
  EdgeColorFunction -> "Cardinal",
  ImagePadding -> {Vertical -> 15, Horizontal -> 30},
  ImageSize -> "ShortestEdge" -> 50,
  BaselinePosition -> Top,
  VertexShapeFunction -> PolynomialGraphVertexShapeFunction,
  ArrowheadShape -> None
};

PackageExport["PolynomialGraphVertexShapeFunction"]

PolynomialGraphVertexShapeFunction[assoc_] := Scope[
  UnpackAssociation[assoc, coordinates, annotations];
  data = annotations["Data"];
  If[data === None, Return @ Disk[coordinates, .1]];
  Text[Framed[Style[annotations["Data"], 12], Background -> White, FrameStyle -> $LightGray], coordinates]
];

(**************************************************************************************************)

PackageExport["ArrowPolynomialGraph"]

ArrowPolynomialGraph[expr_, opts:OptionsPattern[]] :=
  PolynomialGraph[expr, opts, ItemFunction -> arrowPolyItemFunction];

arrowPolyItemFunction[Polynomial[vars_, body_]] :=
  QuiverProductPolyForm @ ReplaceAll[HoldForm[body], Flatten[toArrowVarRules /@ vars]];

toArrowVarRules[var_] := {
  HoldPattern[Power[var, 1]] -> Style[ForwardFactorSymbol, CardinalColor[var]],
  HoldPattern[Power[var, 0]] -> Style[NeutralFactorSymbol, CardinalColor[var]],
  HoldPattern[Power[var, -1]] -> Style[BackwardFactorSymbol, CardinalColor[var]],
  var -> Style[ForwardFactorSymbol, CardinalColor[var]]
};

