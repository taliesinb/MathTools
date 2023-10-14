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
  $edges = Bag[]; $verts = Bag[]; $slotSymbols = {};
  scanFn @ e;
  verts = BagPart[$verts, All];
  edges = BagPart[$edges, All];
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

scanToTree[pos_, e:(head_Symbol[___])] /; !MatchQ[head, Form | RGBColor | GrayLevel] := Scope[
  n = Length[Unevaluated @ e];
  StuffBag[$verts, pos];
  $degree[pos] ^= n;
  $data[pos] ^= head;
  $leafData[pos] ^= Null;
  $type[pos] ^= "Node";
  Do[
    scanToTree[
      subPos = Append[pos, i],
      Evaluate @ Extract[Unevaluated @ e, i, HoldForm]
    ];
    StuffBag[$edges, subPos -> pos];
  ,
    {i, n}
  ];
];

scanToTree[pos_, Slot[n_Integer | n_String]] := Scope[
  StuffBag[$verts, pos];
  $degree[pos] ^= 0;
  $data[pos] ^= n;
  $type[pos] ^= "Slot";
];

scanToTree[pos_, e_] := Scope[
  If[Head[e] === Form, e //= First];
  StuffBag[$verts, pos];
  $degree[pos] ^= 0;
  $data[pos] ^= e;
  $leafData[pos] ^= e;
  $type[pos] = "Leaf";
];

$slotSymbols = {};
scanToTree[pos_, e_Symbol /; MemberQ[$slotSymbols, e]] := Scope[
  StuffBag[$verts, pos];
  $degree[pos] ^= 0;
  $data[pos] ^= e;
  $type[pos] ^= "Slot";
];

DefineGraphTheme["ExpressionTreeGraph",
  VertexLayout -> OrderedTreeVertexLayout[
    RootVertex -> {}, RootOrientation -> "Sink", FanoutStyle -> Center,
    BendRadius -> 1
  ],
  ImagePadding -> {Vertical -> 15, Horizontal -> 30},
  EdgeStyle -> $LightGray, (* makes HalfCenter bending look good *)
  ImageSize -> "ShortestEdge" -> 50,
  BaselinePosition -> Top,
  EdgeOpacity -> None,
  VertexShapeFunction -> ExpressionTreeGraphVertexShape,
  ArrowheadShape -> None
];

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

(**************************************************************************************************)

PublicFunction[NestedListGraph]

NestedListGraph[e_, rules:OptionsPattern[]] := Scope[
  makeTreeGraph[e, scanExpression, "NestedListGraph", rules]
];

DefineGraphTheme["NestedListGraph",
  VertexLayout -> OrderedTreeVertexLayout[
    RootVertex -> {}, RootOrientation -> "Sink", FanoutStyle -> Center -> 0.33,
    BendRadius -> 1
  ],
  EdgeStyle -> $LightGray, (* makes HalfCenter bending look good *)
  VertexSize -> 5,
  GraphicsScale -> 15,
  BaselinePosition -> Top,
  VertexColorFunction -> "LeafData",
  EdgeOpacity -> None,
  ArrowheadShape -> None
];

(**************************************************************************************************)

PublicFunction[RainbowTree]

RainbowTree[e_, rules:OptionsPattern[]] := Scope[
  makeTreeGraph[e, scanExpression, "RainbowTree", rules]
];

DefineGraphTheme["RainbowTree" -> "NestedListGraph",
  VertexColorFunction -> "LeafData" -> ToRainbowColor
];

(**************************************************************************************************)

PublicObject[Polynomial]

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

DefineGraphTheme["PolynomialGraph",
  VertexLayout -> TreeVertexLayout[Orientation -> Top, Balanced -> True],
  EdgeColorFunction -> "Cardinal",
  ImagePadding -> {Vertical -> 15, Horizontal -> 30},
  ImageSize -> "ShortestEdge" -> 50,
  BaselinePosition -> Top,
  VertexShapeFunction -> PolynomialGraphVertexShapeFunction,
  ArrowheadShape -> None
];

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

DefineGraphTheme["HyperedgeIndicenceGraph",
  VertexLayout -> TreeVertexLayout[Orientation -> Top, Balanced -> True],
  ImagePadding -> {Vertical -> 15, Horizontal -> 30},
  ImageSize -> "ShortestEdge" -> 50, EdgeLabels -> "Cardinal", EdgeLabelStyle -> {LabelPosition -> Right, Background -> None},
  BaselinePosition -> Top,
  VertexColorFunction -> IncidenceVertexColorFunction,
  ArrowheadShape -> "Line", ArrowheadSize -> 15, ArrowheadPosition -> 0.6,
  CardinalColors -> $Gray,
  EdgeSetback -> .1
];

scanHyperedge[path_, value_] := Scope[
  p = Hyperedge[path];
  $content[p] = value;
  res = toHyperedgePart[p, value];
  If[res =!= p, StuffBag[$hyperedges, DirectedEdge[p, res, None]]];
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
  StuffBag[$vertices, path];
  $type[path] = SymbolName @ Head @ value;
  $content[path] = value;
  path
)

addAtomVertex[atom_] := (
  StuffBag[$vertices, atom];
  $type[atom] = "Atom";
  $content[atom] = atom;
  atom
);

addPathEdge[path_][subPart_, subValue_] :=
  StuffBag[
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
