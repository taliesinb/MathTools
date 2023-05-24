PublicHead[NodeComplex, NodeColumn, NodeRow, NodeGrid, NodeBox]

PublicOption[NodeLabel, NodePorts, PortSpacing]

PrivateHead[$node, $var]

SetUsage @ "
NodeComplex[$$]
"
Typeset`MakeBoxes[gc:NodeComplex[___], form:StandardForm | TraditionalForm, type:Graphics] :=
  nodeComplexBoxes[gc];

Options[NodeComplex] = {
  Prolog -> None,
  Epilog -> None,
  Background -> None
};

nodeComplexBoxes[NodeComplex[nodes_, opts___Rule]] := Scope @ CatchMessage[
  $path = $var[]; $eqs = {};
  UnpackOptionsAs[NodeComplex, {opts}, epilog, prolog, background];
  boxes = attachProEpi[prolog, epilog] @ processNode[nodes];
  addEqns[{subPath["T"] -> 0, subPath["L"] -> 0}];
  eqs = Flatten @ $eqs;
  vars = DeepUniqueCases[eqs, _$var];
  solutions = SolveCyclicEquations[eqs, EquationVariables -> vars];
  boxes = boxes /. d_$delayed :> RuleCondition @ processDelayedSection[d];
  boxes /. solutions
];

(**************************************************************************************************)

PublicHead[NodeCenter, NodeSide, NodePort]

processDelayedSection[$delayed[primitives_, path_$var]] :=
  ToGraphicsBoxes @
  ReplaceAll[v_$var :> RuleCondition @ Join[path, v]] @
  ReplaceAll[(n:(_NodeCenter|_NodeSide|_NodePort)) :> RuleCondition @ procNodeCoordinate @ n] @
  primitives;

procNodeCoordinate = Case[
  NodeCenter[a___] := {$var[a, "LR"], $var[a, "TB"]};
  NodeSide[a___, side:Left|Right|Bottom|Top|Center|TopLeft|TopRight|BottomLeft|BottomRight] := Switch[
    side,
    Center,      {$var[a, "LR"], $var[a, "TB"]},
    Top,         {$var[a, "LR"], $var[a, "T"]},
    Bottom,      {$var[a, "LR"], $var[a, "B"]},
    Left,        {$var[a, "L"],  $var[a, "TB"]},
    Right,       {$var[a, "R"],  $var[a, "TB"]},
    TopLeft,     {$var[a, "L"],  $var[a, "T"]},
    TopRight,    {$var[a, "R"],  $var[a, "T"]},
    BottomLeft,  {$var[a, "L"],  $var[a, "B"]},
    BottomRight, {$var[a, "R"],  $var[a, "B"]}
  ];
  NodePort[a___, side:Left|Right|Bottom|Top, p_] := $var[a, StringTake[SymbolName[side], 1], p];
  NodePort[a___, p_] := $var[a, "Port", p];
];

(**************************************************************************************************)

PublicHead[NodeColumn, NodeRow, NodeFrame, NodeEpilog, NodeProlog, NodeBox, NodeDisk]

processNode = Case[
  NodeColumn[nodes_List, opts___Rule] := columnLayout[nodes, {opts}];
  NodeRow[nodes_List, opts___Rule]    := rowLayout[nodes, {opts}];
  NodeFrame[interior_]                := nodeFrame[interior];

  NodeEpilog[node_, epilog_]          := nodeEpilog[node, epilog];
  NodeProlog[node_, prolog_]          := nodeProlog[node, prolog];

  NodeBox[args___]                    := nodeBox[NodeBox, args];
  NodeDisk[args___]                   := nodeBox[NodeDisk, args];

  Sequence[node_, part_] := Block[
    {$path = Append[$path, part]},
    processNode @ node
  ];
];

addEqns[eqns_List, rhs_] := addEqns @ Map[# -> rhs&, eqns];
addEqns[lhs_, rhs_] := addEqns[lhs -> rhs];
addEqns[eqns_] := AppendTo[$eqs, eqns];

subPath[] := $path;
subPath[e_, f_List] := Map[subPath[e, #]&, f];
subPath[e_List, f_] := Map[subPath[#, f]&, e];
subPath[e_, f_] := Append[Append[$path, e], f];
subPath[e_] := Append[$path, e];

(**************************************************************************************************)

nodeFrame[node_] :=
  {
    StyleBox[
      makeRect[subPath[{"L", "B"}], subPath[{"R", "T"}]],
      FaceForm[None], EdgeForm[$Red]
    ],
    processNode[node]
  };

(**************************************************************************************************)

nodeEpilog[node_, epilog_] := {processNode[node], $delayed[epilog, $path]};
nodeProlog[node_, prolog_] := {$delayed[prolog, $path], processNode[node]};

(**************************************************************************************************)

columnLayout[nodes_List, opts_] :=
  hvNodeLayout[NodeColumn, nodes, opts, {Left, Right}, {"T", "B", "H", "L", "R", "W"}, -1];

(**************************************************************************************************)

rowLayout[nodes_List, opts_] :=
  hvNodeLayout[NodeRow, nodes, opts, {Top, Bottom}, {"L", "R", "W", "T", "B", "H"}, 1];

(**************************************************************************************************)

Options[NodeRow] = Options[NodeColumn] = {
  Alignment -> Center,
  FrameMargins -> 0,
  Spacings -> 1,
  Epilog -> None,
  Prolog -> None
};

hvNodeLayout[head_, nodes_List, opts_, {startSym_, endSym_}, {mainStart_, mainEnd_, mainDim_, otherStart_, otherEnd_, otherDim_}, mult_] := Scope[
  UnpackOptionsAs[head, opts, alignment, frameMargins, spacings, epilog, prolog];
  SetAutomatic[alignment, 0];
  SetAutomatic[frameMargins, 0];
  SetAutomatic[spacings, 1];
  n = Length @ nodes;
  nodes = MapIndex1[processNode, nodes];
  range = Range[n];
  Switch[alignment,
    startSym | -1,
      addEqns[subPath[range, otherStart], subPath[otherStart] + mult * frameMargins],
    endSym | 1,
      addEqns[subPath[otherEnd], subPath[range, otherEnd] + mult * frameMargins],
    Center | 0,
      otherMiddle = StringJoin[otherStart, otherEnd];
      addEqns[subPath[range, otherMiddle], subPath[otherMiddle]]
  ];
  addEqns @ {subPath[1, mainStart] -> subPath[mainStart], subPath[n, mainEnd] -> subPath[mainEnd]};
  addEqns @ Map[subPath[#+1, mainStart] -> subPath[#, mainEnd] + mult * spacings&, Most @ range];
  addFrameEqns[
    Max @ subPath[range, otherDim] + 2 * frameMargins,
    Total @ subPath[range, mainDim] + (spacings * (n-1))
  ];
  attachProEpi[prolog, epilog] @ nodes
];

(**************************************************************************************************)

addFrameEqns[w_, h_] := addEqns @ {
  subPath["W"] -> w,
  subPath["H"] -> h,
    subPath["B"] -> subPath["T"] - subPath["H"],
    subPath["R"] -> subPath["L"] + subPath["W"],
    subPath["L"] -> subPath["LR"] - subPath["W"]/2,
    subPath["T"] -> subPath["TB"] + subPath["H"]/2,
    subPath["LR"] -> (subPath["L"] + subPath["R"])/2,
    subPath["TB"] -> (subPath["B"] + subPath["T"])/2
}

PublicOption[NodeLabelStyle]

Options[NodeBox] = Options[NodeDisk] = {
  Background -> None,
  NodePorts -> None,
  NodeLabel -> None,
  FrameMargins -> 0.1,
  NodeLabelStyle -> {FontSize -> 15, FontFamily -> "Fira Code"},
  PortSpacing -> 0.5,
  PortSize -> 10,
  PortShape -> "Point",
  Epilog -> None,
  Prolog -> None
}

NodeComplex::badinterior = "Interior of node `` did not construct."

nodeBox[head_, spec_, opts___Rule] := Scope[
  UnpackOptionsAs[head, {opts},
    background, nodePorts, nodeLabel, portSpacing, portSize, portShape, nodeLabelStyle,
    frameMargins, epilog, prolog
  ];
  {{w, h}, interior} = toNodeBoxInterior @ spec;
  portPrimitives = If[head === NodeBox, processNodeBoxPorts, processNodeDiskPorts] @ nodePorts;
  portPrimitives //= Flatten;
  portPrimitives = If[portPrimitives === {}, Nothing, StyleBox[portPrimitives, AbsolutePointSize @ portSize]];
  addFrameEqns[w, h];
  boxPrimitives = Switch[head,
    NodeBox,  makeRect[subPath /@ {"L", "B"}, subPath /@ {"R", "T"}],
    NodeDisk,
      addEqns[subPath["Radius"], Max[w, h]/2];
      makeDisk[currentCenter[], subPath["Radius"]]
  ];
  boxPrimitives = StyleBox[boxPrimitives, FaceForm @ background, EdgeForm @ $Gray];
  labelPrimitives = If[nodeLabel === None, Nothing,
    makeText[StyleOperator[nodeLabelStyle] @ nodeLabel, currentCenter[]]];
  interiorPrimitives = If[interior === None, Nothing, interior];
  attachProEpi[prolog, epilog] @ List[
    boxPrimitives, portPrimitives, interiorPrimitives, labelPrimitives
  ]
];

attachProEpi[None, None][prims_] := prims;
attachProEpi[pro_, epi_][prims_] := {makeDelayed @ pro, prims, makeDelayed @ epi};

makeDelayed[None | {}] := Nothing;
makeDelayed[prims_] := $delayed[prims, $path]

(**************************************************************************************************)

makePortOffsets[n_] := Scope[
  offsets = Range[n] * portSpacing;
  offsets - Mean[offsets]
];

NodeComplex::badportspec = "`` is not a valid port specification."
procPortSpec = Case[
  n_Integer  := Range @ n;
  s_String   := Characters @ s;
  l_List     := l;
  spec_      := ThrowMessage["badportspec", spec];
];

processNodeBoxPorts = Case[
  (side:Left|Right|Top|Bottom) -> spec_ := Scope[
    ports = procPortSpec @ spec;
    n = Length @ ports;
    offsets = makePortOffsets @ n;
    portCoords = Switch[side,
      Center, ConstantArray[{subPath["L"], subPath["TB"]}, n],
      Left,   Threaded[{subPath["L"], subPath["TB"]}] - Thread[{0, offsets}],
      Right,  Threaded[{subPath["R"], subPath["TB"]}] - Thread[{0, offsets}],
      Bottom, Threaded[{subPath["LR"], subPath["B"]}] + Thread[{offsets, 0}],
      Top,    Threaded[{subPath["LR"], subPath["T"]}] + Thread[{offsets, 0}]
    ];
    sideStr = StringTake[SymbolName @ side, 1];
    addEqns @ RuleThread[subPath[sideStr, ports], portCoords];
    addEqns @ RuleThread[subPath["Port", ports], portCoords];
    makePoint @ portCoords
  ];
  list_List := Map[%, list];
  None      := {};
  spec_     := ThrowMessage["badportspec", spec];
];

$sideToAngle = <|
  Left  -> 0.5, Right -> 0, Top -> 0.25, Bottom -> -0.25,
  TopLeft -> 0.375, TopRight -> 0.125, BottomLeft -> -0.375, BottomRight -> -0.125
|>

processNodeDiskPorts = Case[
  (side:Left|Right|Top|Bottom|TopLeft|TopRight|BottomLeft|BottomRight) -> spec_ := Scope[
    initAng = Lookup[$sideToAngle, side];
    ports = procPortSpec @ spec;
    makeCirclePorts[ports, initAng, makePortOffsets @ Length @ ports]
  ];
  spec:(_String | _Integer) := Scope[
    ports = procPortSpec @ spec;
    n = Length @ ports;
    makeCirclePorts[ports, 0, Range[0, n-1] / n]
  ];
  list_List := Map[%, list];
  None      := {};
  spec_     := ThrowMessage["badportspec", spec];
];

makeCirclePorts[ports_, initAngle_, angles_] := Scope[
  portCoords = Threaded[currentCenter[]] + AnglePair[initAngle + angles] * subPath["Radius"];
  addEqns @ RuleThread[subPath["Port", ports], portCoords];
  makePoint @ portCoords
]

currentCenter[] := subPath /@ {"LR", "TB"};

(**************************************************************************************************)

makeText[label_, coords_] := ToGraphicsBoxes @ Text[label, coords];
makeRect[coords__] := RectangleBox[coords];
makeDisk[coords__] := DiskBox[coords]
makePoint[coords_] := PointBox[coords];

flipY[list_] := Threaded[{1, -1}] * list;

(**************************************************************************************************)

toNodeBoxInterior = Case[
  pos:$Coord2P := {pos, None};
  sz:$NumberP := {{sz, sz}, None};
  other_       := Scope[
    size = subPath["Interior", {"W", "H"}] + 2 * frameMargins;
    interior = processNode[other, "Interior"];
    addEqns @ {
      subPath["Interior", "L"] -> subPath["L"] + frameMargins,
      subPath["Interior", "T"] -> subPath["T"] - frameMargins,
      subPath["R"] -> subPath["Interior", "R"] + frameMargins,
      subPath["B"] -> subPath["Interior", "B"] - frameMargins
    };
    {size, interior}
  ];
];
