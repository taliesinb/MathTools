PublicFunction[FunctionTreeGraph]

FunctionTreeGraph[e_, rules:OptionsPattern[]] :=
  makeTreeGraph[e, scanFunction, "ExpressionTreeGraph", rules];

scanFunction = Case[
  HoldPattern[Function[vars_List, body_]] := Scope[$slotSymbols = vars; scanToTree[{}, body]];
  HoldPattern[Function[var_Symbol, body_]] := % @ Function[{var}, body];
  HoldPattern[Function[body_]] := scanToTree[{}, body];
  _ := $Failed;
];

(**************************************************************************************************)

PublicFunction[ExpressionTreeGraph]

ExpressionTreeGraph[e_, rules:OptionsPattern[]] :=
  makeTreeGraph[e, scanExpression, "ExpressionTreeGraph", rules];

scanExpression[e_] := scanToTree[{}, e];

(**************************************************************************************************)

makeTreeGraph[e_, scanFn_, theme_, rules___] := Scope[
  $type = $degree = $data = $leafData = Association[];
  $edges = Internal`Bag[]; $verts = Internal`Bag[]; $slotSymbols = {};
  scanFn @ e;
  verts = Internal`BagPart[$verts, All];
  edges = Internal`BagPart[$edges, All];
  ExtendedGraph[
    verts, edges,
    FilterOptions[rules],
    VertexAnnotations -> Map[
      Lookup[#, verts, None]&,
      <|"Type" -> $type, "Degree" -> $degree, "Data" -> $data, "LeafData" -> $leafData|>
    ],
    GraphTheme -> theme
  ]
];

(**************************************************************************************************)

SetAttributes[{scanToTree}, HoldRest];

scanToTree[pos_, HoldForm[e_]] := scanToTree[pos, e];

scanToTree[pos_, e:(head_Symbol[___])] /; head =!= Form := Scope[
  n = Length[Unevaluated @ e];
  Internal`StuffBag[$verts, pos];
  $degree[pos] ^= n;
  $data[pos] ^= head;
  $leafData[pos] ^= Null;
  $type[pos] ^= "Node";
  Do[
    scanToTree[
      subPos = Append[pos, i],
      Evaluate @ Extract[Unevaluated @ e, i, HoldForm]
    ];
    Internal`StuffBag[$edges, subPos -> pos];
  ,
    {i, n}
  ];
];

scanToTree[pos_, Slot[n_Integer | n_String]] := Scope[
  Internal`StuffBag[$verts, pos];
  $degree[pos] ^= 0;
  $data[pos] ^= n;
  $type[pos] = "Slot";
];

scanToTree[pos_, e_] := Scope[
  If[Head[e] === Form, e //= First];
  Internal`StuffBag[$verts, pos];
  $degree[pos] ^= 0;
  $data[pos] ^= e;
  $leafData[pos] ^= e;
  $type[pos] = "Leaf";
];

$slotSymbols = {};
scanToTree[pos_, e_Symbol /; MemberQ[$slotSymbols, e]] := Scope[
  Internal`StuffBag[$verts, pos];
  $degree[pos] ^= 0;
  $data[pos] ^= e;
  $type[pos] = "Slot";
];

$GraphThemeData["ExpressionTreeGraph"] := $ExpressionTreeGraphThemeData;

$ExpressionTreeGraphThemeData = {
  VertexLayout -> TreeVertexLayout[
    Orientation -> Top, Balanced -> False, RootVertex -> {}, RootOrientation -> "Sink", BendStyle -> "HalfCenter",
    PreserveBranchOrder -> True],
  ImagePadding -> {Vertical -> 15, Horizontal -> 30},
  EdgeStyle -> $LightGray, (* makes HalfCenter bending look good *)
  ImageSize -> "ShortestEdge" -> 50,
  BaselinePosition -> Top,
  VertexShapeFunction -> ExpressionTreeGraphVertexShape,
  ArrowheadShape -> None
};

PublicFunction[ExpressionTreeGraphVertexShape]

ExpressionTreeGraphVertexShape[assoc_] := Scope[
  UnpackAssociation[assoc, coordinates, annotations];
  UnpackAssociation[annotations, data, type];
  color = Switch[type,
    "Leaf", White,
    "Slot", $LightPink,
    "Node", Part[$LightColorPalette, Mod[Hash @ data, 6, 1]]
  ];
  Text[Framed[Style[annotations["Data"], 12, FontColor -> Black], Background -> color, FrameStyle -> Darker[color]], coordinates]
];

$GraphThemeData["ExpressionTreeGraph"] := $ExpressionTreeGraphThemeData;

(**************************************************************************************************)

PublicFunction[NestedListGraph]

NestedListGraph[e_, rules:OptionsPattern[]] := Scope[
  makeTreeGraph[e, scanExpression, "NestedListGraph", rules]
];

$GraphThemeData["NestedListGraph"] := $NestedListGraphThemeData;

$NestedListGraphThemeData = {
  VertexLayout -> TreeVertexLayout[
    Orientation -> Top, Balanced -> False, RootVertex -> {}, RootOrientation -> "Sink", BendStyle -> "Top",
    StretchFactor -> 0.75, BendRadius -> 1,
    PreserveBranchOrder -> True],
  EdgeStyle -> $LightGray, (* makes HalfCenter bending look good *)
  ImageSize -> "ShortestEdge" -> 20,
  VertexSize -> 5,
  BaselinePosition -> Top,
  VertexColorFunction -> "LeafData",
  ArrowheadShape -> None
};

(**************************************************************************************************)

PublicHead[Polynomial]

SetAttributes[Polynomial, HoldRest];

declareBoxFormatting[
  poly:Polynomial[_List, _] :> makePolynomialBoxes @ poly
];

makePolynomialBoxes[Polynomial[vars_List, body_]] := Scope[
  vars = # -> SymbolForm[SymbolName[#]]& /@ vars;
  ToBoxes[HoldForm[body] /. vars /. {Times -> ImplicitTimesForm, Plus -> PlusForm}]
];

(**************************************************************************************************)

PublicFunction[PolynomialGraph]

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

PublicFunction[PolynomialGraphVertexShapeFunction]

PolynomialGraphVertexShapeFunction[assoc_] := Scope[
  UnpackAssociation[assoc, coordinates, annotations];
  data = annotations["Data"];
  If[data === None, Return @ Disk[coordinates, .1]];
  Text[Framed[Style[annotations["Data"], 12], Background -> White, FrameStyle -> $LightGray], coordinates]
];

(**************************************************************************************************)

PublicFunction[ArrowPolynomialGraph]

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

(**************************************************************************************************)

PublicHead[Hyperedge]

(**************************************************************************************************)

PublicFunction[HyperedgeIncidenceGraph]

SetUsage @ "
HyperedgeIncidenceGraph[<|name$1 -> expr$1, $$|>] constructs a bipartite graph, consisting of two vertex types:
* hyperedge vertices reprepresent expressions and sub-expressions of the expr$i.
* atomic vertices represents atomic values in the expressions.
* edges h$ -> a$ are labeled with the part p$ of h$ that has value a$.
* if any of the expr$i contain name$i, there will be edges of the form h$ -> h$'.
"

HyperedgeIncidenceGraph::notassoc = "First argument should be an association from names to hyperedges."

HyperedgeIncidenceGraph[expr_, rules:OptionsPattern[]] := Scope[
  If[!AssociationQ[expr], ReturnFailed["notassoc"]];
  keys = Keys[expr];
  $keysP = Apply[Alternatives, Verbatim /@ keys];
  
  $type = $content = Association[];
  CollectTo[{$hyperedges, $vertices},
    KeyValueScan[scanHyperedge, expr /. p:$keysP :> Hyperedge[p]]
  ];
  
  $vertices //= DeleteDuplicates;

  ExtendedGraph[
    $vertices,
    $hyperedges,
    FilterOptions[rules],
    VertexAnnotations -> Map[
      Lookup[#, $vertices, None]&,
      <|"Content" -> $content, "Type" -> $type|>
    ],
    GraphTheme -> "HyperedgeIndicenceGraph"
  ]
];

$GraphThemeData["HyperedgeIndicenceGraph"] := $HyperedgeIncidenceGraphThemeData;

$HyperedgeIncidenceGraphThemeData = {
  VertexLayout -> TreeVertexLayout[Orientation -> Top, Balanced -> True],
  ImagePadding -> {Vertical -> 15, Horizontal -> 30},
  ImageSize -> "ShortestEdge" -> 50, EdgeLabels -> "Cardinal", EdgeLabelStyle -> {LabelPosition -> Right, Background -> None},
  BaselinePosition -> Top,
  VertexColorFunction -> IncidenceVertexColorFunction,
  ArrowheadShape -> "Line", ArrowheadSize -> 15, ArrowheadPosition -> 0.6,
  CardinalColors -> $Gray,
  EdgeSetback -> .1
};

scanHyperedge[path_, value_] := Scope[
  p = Hyperedge[path];
  $content[p] = value;
  res = toHyperedgePart[p, value];
  If[res =!= p, Internal`StuffBag[$hyperedges, DirectedEdge[p, res, None]]];
];

toHyperedgePart[path_, ref_Hyperedge] :=
  ref;

toHyperedgePart[path_, value:(_List | _Association)] := (
  PartValueMap[addPathEdge[path], value];
  addHyperedgeVertex[path, value]
);

toHyperedgePart[path_, atom_] :=
  addAtomVertex[atom];

addHyperedgeVertex[path_, value_] := (
  Internal`StuffBag[$vertices, path];
  $type[path] = SymbolName @ Head @ value;
  $content[path] = value;
  path
)

addAtomVertex[atom_] := (
  Internal`StuffBag[$vertices, atom];
  $type[atom] = "Atom";
  $content[atom] = atom;
  atom
);

addPathEdge[path_][subPart_, subValue_] :=
  Internal`StuffBag[
    $hyperedges,
    DirectedEdge[path, toHyperedgePart[Append[path, subPart], subValue], subPart]
  ];


PublicFunction[IncidenceVertexColorFunction]

IncidenceVertexColorFunction = Case[
  _Hyperedge := White;
  _ := Black;
];


PublicFunction[IncidenceGraphVertexShapeFunction]

IncidenceGraphVertexShapeFunction[assoc_] := Scope[
  UnpackAssociation[assoc, coordinates, annotations];
  data = annotations["Type"];
  If[data === None, Return @ Disk[coordinates, .1]];
  Text[Framed[Style[annotations["Data"], 12], Background -> White, FrameStyle -> $LightGray], coordinates]
];
