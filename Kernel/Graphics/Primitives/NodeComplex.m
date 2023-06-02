PublicHead[NodeComplex, NodeColumn, NodeRow, NodeGrid, NodeBox, AbsoluteOffset]

PublicOption[NodeLabel, NodePorts, NodeAlias, PrologStyle, EpilogStyle]

PrivateHead[$node, $var, $tagvar, $relvar]

SetUsage @ "
NodeComplex[$$]
"

Typeset`MakeBoxes[gc:NodeComplex[___], form:StandardForm | TraditionalForm, type:Graphics] :=
  nodeComplexBoxes[gc];

declareCustomGraphicsHead[NodeComplex];

Options[NodeComplex] = {
  Prolog -> None,
  Epilog -> None,
  EpilogStyle -> None,
  PrologStyle -> None,
  Background -> None
};

NodeComplex::available = "Available vars shown below.";
NodeComplex::unresolved = "Unresolved variable `` that compiles to ``.";

$nodeCoordinateP = _NodePort | _NodeCenter | _NodeCorner | _NodeSide;

nodeComplexBoxes[NodeComplex[nodes_, opts___Rule]] := Scope @ CatchMessage[NodeComplex,
  $path = $var[]; $eqs = {}; $aliases = <||>; $isAutomaticSize = <||>; $portPositions = <||>;
  UnpackOptionsAs[NodeComplex, {opts}, epilog, prolog, background, epilogStyle, prologStyle];
  $epilogFn = StyleBoxOperator @ epilogStyle;
  $prologFn = StyleBoxOperator @ prologStyle;
  boxes = attachProEpi[prolog, epilog] @ processNode[nodes];
  addEqns[{subPath[$T] -> 0, subPath[$L] -> 0}];
  If[Length[$portPositions] > 0,
    addEqns @ Normal @ Map[NoLinearExpand[Mean[#]]&, $portPositions];
  ];
  eqs = Flatten @ $eqs;
  (* these heads will only make it into equations currently if a PortPositions spec includes a NodePort *)
  eqs = eqs /. np:$nodeCoordinateP :> RuleCondition[procNodeCoordinate[np] /. rv_$relvar -> toAbsVar[rv, $var[]]];
  vars = DeepUniqueCases[eqs, _$var];
  solutions = SolveCyclicEquations[eqs, EquationVariables -> vars, ExpandLinearEquations -> True];
  boxes = evaluateDelayed[boxes];
  result = boxes /. solutions;
  If[ContainsQ[result, _$var],
    result = result /. $tagvar[spec_, v_$var] :> RuleCondition[Message[NodeComplex::unresolved, spec, v]; {0, 0}];
    Message[NodeComplex::available];
    Print @ Row[Sort @ Keys @ solutions, ", "];
  ];
  result = result /. ListPart -> Part /. $tagvar[_, v_] :> v /. AbsoluteOffset[off_][pos_] :> RuleCondition[Threaded[off] + pos];
  result = result /. $delayed[e_] :> RuleCondition[ToGraphicsBoxes @ e];
  result
];

DefineStandardTraditionalForm[
  $var[args___] :> ToBoxes @ Style[Row @ Riffle[{args}, ":"], FontFamily -> "Fira Code", FontColor -> $Blue]
]

(**************************************************************************************************)

PublicHead[NodeCenter, NodeSide, NodePort, NodeInPort, NodeOutPort, NodeCorner]
PublicHead[LeftRight, TopBottom]

evaluateDelayed[e_] := ReplaceAll[e, d:(_$delayed | _$semidelayed) :> RuleCondition @ processDelayed[d]];

normalizeNodeInOutPorts[e_] := ReplaceAll[e, {
  NodeInPort[args___, n_Integer] :> NodePort[args, Top, n], NodeInPort[args___] :> NodePort[args, Top],
  NodeOutPort[args___, n_Integer] :> NodePort[args, Bottom, n], NodeOutPort[args___] :> NodePort[args, Bottom]
}];

convertNodeSymbolsToVars[e_] := ReplaceAll[e, n:$nodeCoordinateP :> RuleCondition @ $tagvar[n, procNodeCoordinate @ n]];
resolveVars[path_, e_] := ReplaceAll[ReplaceAll[e, v_$relvar :> RuleCondition @ toAbsVar[v, path]], solutions];

processDelayed[$semidelayed[boxes_]] := resolveVars[$var[], convertNodeSymbolsToVars @ normalizeNodeInOutPorts @ boxes];
processDelayed[$delayed[primitives_, path_$var]] := $delayed @ evaluateDelayed @ resolveVars[path, convertNodeSymbolsToVars @ normalizeNodeInOutPorts @ primitives];

$LR = LeftRight;
$TB = TopBottom;
$T = Top;
$B = Bottom;
$L = Left;
$R = Right;
$H = "H";
$W = "W";

$sideToCoords = <|
  Center      -> {$LR, $TB},
  Top         -> {$LR, $T},
  Bottom      -> {$LR, $B},
  Left        -> {$L,  $TB},
  Right       -> {$R,  $TB},
  TopLeft     -> {$L,  $T},
  TopRight    -> {$R,  $T},
  BottomLeft  -> {$L,  $B},
  BottomRight -> {$R,  $B}
|>;

procNodeCoordinate = Case[
  NodeCorner[x_, y_] := {ListPart[% @ x, 1], ListPart[% @ y, 2]};
  NodeCenter[a___] := {$relvar[a, $LR], $relvar[a, $TB]};
  NodeSide[a___, side_Symbol] := $relvar[a, #]& /@ $sideToCoords[side];
  NodePort[a___, side:Left|Right|Bottom|Top] := $relvar[a, side, 1];
  NodePort[a___, side:Left|Right|Bottom|Top, p_] := $relvar[a, side, p];
  NodePort[a___, p_Integer] := $relvar[a, "PortNumber", p];
];

toAbsVar[$relvar[a_String /; KeyExistsQ[$aliases, a], rest___], _] :=
  $var[Sequence @@ $aliases[a], rest];

toAbsVar[v_$relvar, b_$var] := Join[b, $var @@ v];

(**************************************************************************************************)

PublicHead[NodeColumn, NodeRow, NodeGrid, NodeFrame, NodeEpilog, NodeProlog, NodeBox, NodeDisk, PortSkeleton]

General::badNodeSpec = "Invalid node expression: ``.";
processNode = Case[
  NodeColumn[nodes_List, opts___Rule] := columnLayout[nodes, {opts}];
  NodeRow[nodes_List, opts___Rule]    := rowLayout[nodes, {opts}];
  NodeGrid[spec_List, opts___Rule]    := gridLayout[spec, {opts}];
  NodeFrame[interior_]                := nodeFrame[interior];

  NodeEpilog[node_, epilog_]          := nodeEpilog[node, epilog];
  NodeProlog[node_, prolog_]          := nodeProlog[node, prolog];

  PortSkeleton[args___]               := portSkeletonBox[args];

  NodeBox[args___]                    := nodeBox[NodeBox, args];
  NodeDisk[args___]                   := nodeBox[NodeDisk, args];

  Spacer[{w_, h_}]                    := emptyBox[w, h];
  Padded[node_, spec_]                := paddedNode[node, spec];

  node_                               := ThrowMessage["badNodeSpec", MsgExpr @ node];

  Sequence[node_, part_] := Block[
    {$path = Append[$path, part]},
    processNode @ node
  ];
];

addEqns[eqns_List, rhs_] := addEqns @ Map[# -> rhs&, eqns];
addEqns[lhs_, rhs_] := addEqns[lhs -> rhs];
addEqns[eqns_] := (AppendTo[$eqs, eqns]; eqns);

createAlias := Case[
  None       := Null;
  i_Integer  := % @ TextString[i];
  str_String := AssociateTo[$aliases, str -> $path];
];

subPath[] := $path;
subPath[e_, f_List] := Map[subPath[e, #]&, f];
subPath[e_List, f_] := Map[subPath[#, f]&, e];
subPath[e_, f_] := Append[Append[$path, e], f];
subPath[e_, f_, g_] := Append[Append[Append[$path, e], f], g];
subPath[e_] := Append[$path, e];

(**************************************************************************************************)

paddedNode[node_, spec_] := Scope[
  {{l, r}, {b, t}} = StandardizePadding @ spec;

];

emptyBox[w_, h_] := (addFrameEqns[w, h]; {})

(**************************************************************************************************)

nodeFrame[node_] :=
  {
    StyleBox[
      makeRect[subPath[{$L, $B}], subPath[{$R, $T}], None, $Gray, 1],
      FaceForm[None], EdgeForm[$Red]
    ],
    processNode[node]
  };

(**************************************************************************************************)

nodeEpilog[node_, epilog_] := {processNode[node], $delayed[epilog, $path]};
nodeProlog[node_, prolog_] := {$delayed[prolog, $path], processNode[node]};

(**************************************************************************************************)

columnLayout[nodes_List, opts_] :=
  hvNodeLayout[NodeColumn, nodes, opts, {Left, Right}, {$T, $B, $H, $L, $R, $W}, -1];

(**************************************************************************************************)

rowLayout[nodes_List, opts_] :=
  hvNodeLayout[NodeRow, nodes, opts, {Top, Bottom}, {$L, $R, $W, $T, $B, $H}, 1];

(**************************************************************************************************)

Options[NodeRow] = Options[NodeColumn] = {
  NodeAlias -> None,
  Alignment -> Center,
  FrameMargins -> 0,
  Spacings -> .5,
  Epilog -> None,
  Prolog -> None
};

NodeComplex::badalign = "Alignment -> `` not valid for ``."

hvNodeLayout[head_, nodes_List, opts_, {startSym_, endSym_}, {mainStart_, mainEnd_, mainDim_, otherStart_, otherEnd_, otherDim_}, mult_] := Scope[
  UnpackOptionsAs[head, opts, alignment, frameMargins, spacings, epilog, prolog, nodeAlias];
  createAlias[nodeAlias];
  SetAutomatic[alignment, 0];
  SetAutomatic[frameMargins, 0];
  {lrmargin, btmargin} = StandardizePadding[frameMargins];
  tbmargin = Reverse @ btmargin;
  {{smargin, emargin}, {osmargin, oemargin}} = If[head === NodeRow, {lrmargin, tbmargin}, {tbmargin, lrmargin}];
  SetAutomatic[spacings, 1];
  n = Length @ nodes;
  nodes = MapIndex1[processNode, nodes];
  range = Range[n];
  multOther = mult * -1;
  Switch[alignment,
    startSym | -1,
      addEqns[subPath[range, otherStart], subPath[otherStart] + multOther * osmargin],
    endSym | 1,
      addEqns[subPath[range, otherEnd], subPath[otherEnd] - multOther * oendmargin],
    Center | 0,
      otherMiddle = If[otherStart === $L, $LR, $TB];
      addEqns[subPath[range, otherMiddle], subPath[otherMiddle]],
    _,
      ThrowMessage["badalign", alignment, head];
  ];
  addEqns @ {
    subPath[1, mainStart] -> subPath[mainStart] + mult * smargin,
    subPath[mainEnd]      -> subPath[n, mainEnd] + mult * emargin
  };
  addEqns @ Map[subPath[#+1, mainStart] -> subPath[#, mainEnd] + mult * spacings&, Most @ range];
  mainSize = Total[subPath[range, mainDim]] + (smargin + emargin) + (spacings * (n-1));
  otherSize = Max @ subPath[range, otherDim] + (osmargin + oemargin);
  If[head === NodeRow, addFrameEqns[mainSize, otherSize], addFrameEqns[otherSize, mainSize]];
  attachProEpi[prolog, epilog] @ nodes
];

(**************************************************************************************************)

NodeGrid::badNodeGridSpec = "Spec `` should be a dense matrix of items or a list of rules mapping position to item.";

Options[NodeGrid] = {
  RowAlignments -> Top,
  ColumnAlignments -> Left,
  RowSpacings -> .5,
  ColumnSpacings -> .5,
  FrameMargins -> 0
}

gridLayout[spec_List, opts_List] := Scope[
  UnpackOptionsAs[NodeGrid, {opts}, rowAlignments, columnAlignments, rowSpacings, columnSpacings, frameMargins];
  $halign = columnAlignments; $valign = rowAlignments;
  $hspacing = columnSpacings; $vspacing = rowSpacings;
  CatchMessage[NodeGrid, parseGridItems[spec]]
];

NodeGrid::unspecsize = "Cannot choose a `` for at least one ``.";

parseGridItems = Case[
  rules:{__Rule}  := Scope[
    $singletonsR = $singletonsC = <||>;
    {maxR, maxC} = Map[Max, Transpose @ MatrixMap[parseSpan, Keys @ rules]];

    $hspacing ^= expandRepSpec[maxC-1] @ $hspacing;
    $vspacing ^= expandRepSpec[maxR-1] @ $vspacing;
    rangeR = Range @ maxR;
    rangeC = Range @ maxC;

    (* vars for row heights, column widths *)
    varsH = subPath["Rows", #, $H]& /@ rangeR;
    varsW = subPath["Cols", #, $W]& /@ rangeC;

    items = Map[$i = 1; parseGridRuleItem, rules];

    (* obtain RHS for row heights, column widths, removing widths and heights that are inherited from the row/column itself *)
    allSizesC = Lookup[$singletonsC, rangeC, ThrowMessage["unspecsize", "width", "column"]];
    allSizesR = Lookup[$singletonsR, rangeR, ThrowMessage["unspecsize", "height", "row"]];
    sizesC = maxRCSize /@ allSizesC;
    sizesR = maxRCSize /@ allSizesR;

    (* equate these vars to these maxes *)
    addEqns @ RuleThread[varsH, sizesR];
    addEqns @ RuleThread[varsW, sizesC];

    (* RHS for total widths, heights *)
    {{marginL, marginR}, {marginB, marginT}} = StandardizePadding[frameMargins];
    height = Total[ListRiffle[varsH, $vspacing]] + marginB + marginT;
    width = Total[ListRiffle[varsW, $hspacing]] + marginL + marginR;

    (* frame equations *)
    addFrameEqns[width, height];

    (* equate left and right of columns based on width vars *)
    addEqns @ MapIndex1[subPath["Cols", #2, $R] -> subPath["Cols", #2, $L] + #1&, varsW];

    (* equate top and bottom of rows based on height vars *)
    addEqns @ MapIndex1[subPath["Rows", #2, $B] -> subPath["Rows", #2, $T] - #1&, varsH];

    (* join outer dividers to frame *)
    addEqns @ {
      subPath[$T] -> subPath["Rows", 1, $T]    + marginT,
      subPath[$L] -> subPath["Cols", 1, $L]    - marginL,
      subPath[$B] -> subPath["Rows", maxR, $B] - marginB,
      subPath[$R] -> subPath["Cols", maxC, $R] + marginR
    };

    (* gaps between rows and columns *)
    addEqns @ ApplyWindowed[subPath["Cols", #1, $R] -> subPath["Cols", #2, $L] - Part[$hspacing, #1]&, rangeC];
    addEqns @ ApplyWindowed[subPath["Rows", #1, $B] -> subPath["Rows", #2, $T] + Part[$vspacing, #1]&, rangeR];

    items
  ];
  grid:{__List}  := % @ Catenate @ MapIndexed[#2 -> #1&, grid, {2}];
  other_         := ThrowMessage["badNodeGridSpec", MsgExpr @ other];
];

maxRCSize[list_] := Max @ Replace[Discard[list, Lookup[$isAutomaticSize, #, False]&], {} -> {1}];

NodeGrid::baditemalign = "Item Alignment -> `` is invalid."

parseGridRuleItem = Case[
  Rule[rc:{_, _}, Item[item_, Alignment -> align_]] := Scope[
    Switch[align,
      Center,      $halign = Center; $valign = Center,
      Left|Right,  $halign = align;  $valign = Center,
      Top|Bottom,  $halign = Center; $valign = align,
      TopLeft,     $halign = Left;   $valign = Top,
      TopRight,    $halign = Right;  $valign = Top,
      BottomLeft,  $halign = Left;   $valign = Bottom,
      BottomRight, $halign = Right;  $valign = Bottom,
      {Left|Right|Center, Top|Bottom|Center}, {$halign, $valign} = align,
      _,           ThrowMessage["baditemalign", align];
    ];
    % @ Rule[rc, item]
  ];
  Rule[rc:{_, _}, item_] := Scope[
    {{rs, re}, {cs, ce}} = Map[parseSpan, rc];
    l = subPath["Cols", cs, $L]; r = subPath["Cols", ce, $R];
    t = subPath["Rows", rs, $T]; b = subPath["Rows", re, $B];
    addEqns @ {
      Switch[$halign,
        Left,   subPath[$i, $L] -> l,
        Right,  subPath[$i, $R] -> r,
        Center, subPath[$i, $LR] -> (l + r)/2
      ],
      Switch[$valign,
        Top,    subPath[$i, $T] -> t,
        Bottom, subPath[$i, $B] -> b,
        Center, subPath[$i, $TB] -> (t + b)/2
      ]
    };
    If[rs == re, KeyAppendTo[$singletonsR, rs, subPath[$i, $H]]];
    If[cs == ce, KeyAppendTo[$singletonsC, cs, subPath[$i, $W]]];
    $defaultNodeSize := {totalSpanningWidth @ Range[cs, ce], totalSpanningHeight @ Range[rs, re]};
    node = processNode[item, $i++]
  ];
  _        := ThrowMessage["badNodeGridSpec"];
]

totalSpanningWidth[indices_]  := Plus[Total @ Part[varsW, indices], Total @ Part[$hspacing, Most @ indices]];
totalSpanningHeight[indices_] := Plus[Total @ Part[varsH, indices], Total @ Part[$vspacing, Most @ indices]];

NodeGrid::badspan = "Bad item position ``.";
parseSpan = Case[
  i_Integer                  := {i, i};
  Span[i_Integer, j_Integer] := {i, j};
  other_                     := ThrowMessage["badspan", other];
]

getMax = Case[
  i_Integer    := i;
  _;;i_Integer := i;
  other_       := ThrowMessage["badspan", other];
]


(**************************************************************************************************)

Options[PortSkeleton] = {
  PortSpacing -> 0.2,
  PortSize -> 0.05,
  PortShape -> "Disk",
  PortEdgeColor -> Automatic,
  PortEdgeThickness -> 1,
  PortFaceColor -> Black,
  HiddenPorts -> None,
  NodeAlias -> None
}

portSkeletonBox[w_, nodePorts_, opts___Rule] := Scope[
  UnpackOptionsAs[PortSkeleton, {opts},
    portSpacing, portSize, portShape, portEdgeColor, portFaceColor, portEdgeThickness, hiddenPorts, nodeAlias
  ];
  createAlias[nodeAlias];
  $po = 0;
  portPrimitives = Flatten @ processNodeBoxPorts[Bottom -> nodePorts];
  addFrameEqns[w, 0];
  portPrimitives
];

(**************************************************************************************************)

addFrameEqns[w_, h_] := addEqns @ {
  subPath[$W]  -> w,
  subPath[$H]  -> h,
  subPath[$B]  -> subPath[$T]  - subPath[$H],
  subPath[$R]  -> subPath[$L]  + subPath[$W],
  subPath[$LR] -> subPath[$L] + subPath[$W]/2,
  subPath[$TB] -> subPath[$B] + subPath[$H]/2
}

PublicOption[NodePorts, NodeLabelStyle, NodeLabelColor, FrameColor, FrameThickness, FrameLabelSpacing, FrameLabelStyle]
PublicOption[PortSpacing, PortPositions, PortSize, PortShape, PortEdgeColor, PortFaceColor, PortEdgeThickness, HiddenPorts, PortPositions]

Options[NodeDisk] = {
  FrameColor -> $Gray,
  FrameThickness -> 1,
  FrameLabel -> None,
  FrameLabelSpacing -> 0.1,
  FrameLabelStyle -> {FontSize -> 16, FontFamily -> "Fira Code", FontWeight -> "Bold"},
  Background -> None,
  NodePorts -> None,
  NodeAlias -> None,
  NodeLabel -> None,
  NodeLabelColor -> None,
  FrameMargins -> 0.1,
  NodeLabelStyle -> {FontSize -> 16, FontFamily -> "Fira Code", FontWeight -> "Bold"},
  PortSpacing -> 0.2,
  PortSize -> 0.05,
  PortShape -> "Disk",
  PortEdgeColor -> Automatic,
  PortEdgeThickness -> 1,
  PortFaceColor -> Black,
  HiddenPorts -> None,
  Epilog -> None,
  Prolog -> None,
  PortPositions -> Automatic
}

Options[NodeBox] = JoinOptions[
  NodeDisk,
  RoundingRadius -> None
]

nodeBox[head_, spec_, opts___Rule] := Scope @ CatchMessage[head,
  UnpackOptionsAs[head, {opts},
    background, nodePorts, nodeLabel, nodeLabelStyle, nodeLabelColor,
    frameMargins, epilog, prolog, nodeAlias,
    portSpacing, portSize, portShape, portEdgeColor, portFaceColor, portEdgeThickness,
    frameColor, background, frameThickness, hiddenPorts, portPositions,
    frameLabel, frameLabelSpacing, frameLabelStyle
  ];
  createAlias[nodeAlias];
  {{lmargin, rmargin}, {bmargin, tmargin}} = StandardizePadding[frameMargins];
  {{w, h}, interior} = toNodeBoxInterior @ spec;
  $po = 0;
  portPrimitives = If[head === NodeBox, processNodeBoxPorts, processNodeDiskPorts] @ nodePorts;
  portPrimitives //= Flatten;
  If[portPrimitives === {}, portPrimitives = Nothing];
  addFrameEqns[w, h];
  boxPrimitives = Switch[head,
    NodeBox,
      UnpackOptionsAs[NodeBox, {opts}, roundingRadius];
      makeRoundedRect[subPath /@ {$L, $B}, subPath /@ {$R, $T}, roundingRadius, background, frameColor, frameThickness]
    ,
    NodeDisk,
      addEqns[subPath["Radius"], Max[w, h]/2];
      makeDisk[currentCenter[], subPath["Radius"], background, frameColor, frameThickness]
  ];
  labelPrimitives = If[nodeLabel === None, Nothing,
    makeText[StyleOperator[ToRainbowColor @ nodeLabelColor] @ StyleOperator[nodeLabelStyle] @ nodeLabel, currentCenter[]]];
  frameLabelPrimitives = makeFrameLabel[frameLabel];
  interiorPrimitives = If[interior === None, Nothing, interior];
  attachProEpi[prolog, epilog] @ List[
    boxPrimitives, portPrimitives, interiorPrimitives, labelPrimitives, frameLabelPrimitives
  ]
];

attachProEpi[None, None][prims_] := prims;
attachProEpi[pro_, epi_][prims_] := {$prologFn @ makeDelayed @ pro, prims, $epilogFn @ makeDelayed @ epi};

makeDelayed[None | {}] := Nothing;
makeDelayed[prims_] := $delayed[prims, $path]

(**************************************************************************************************)

$sideToLabelOffset = <|
        Left -> { 1,  0},       Right -> {-1,  0},
     TopLeft -> { 1, -1},    TopRight -> {-1, -1},
  BottomLeft -> { 1,  1}, BottomRight -> {-1,  1},
      Bottom -> { 0,  1},         Top -> { 0, -1}
|>;

General::badNodeFrameLabel = "`` is not a valid spec for FrameLabel.";
makeFrameLabel = Case[
  None | {}  := Nothing;
  other_     := %[Top -> other];
  list_List  := Map[%, list];
  side:$SidePattern -> label_ := Scope[
    coords = Lookup[$sideToCoords, side, Failure["badNodeFrameLabel", side -> label]];
    coords = $var /@ coords;
    offset = Lookup[$sideToLabelOffset, side] * (1 + frameLabelSpacing);
    $delayed[Text[Style[label, frameLabelStyle], coords, offset], $path]
  ];
]

(**************************************************************************************************)

makePortOffsets[n_] := Scope[
  offsets = Range[n] * portSpacing;
  offsets - Mean[offsets]
];

General::badNodePortSpec = "`` is not a valid port specification."
procPortSpec = Case[
  Rule[_, spec_] := % @ spec;
  n_Integer      := Range @ n;
  s_String       := Characters @ s;
  l_List         := l;
  l:{___Integer} := Set[portFaceColor, l];
  spec_          := ThrowMessage["badNodePortSpec", spec];
];

SetUsage @ "
PortPositions is an option for %NodeBox that specifies where ports will be placed on relevant side.
* It can take the following settings:
| Automatic | centered, spaced according to %PortSpacing (default) |
| %AbsoluteOffset[off] | offset the values produced by Automatic |
| Inherited | use average position of same numbered/colored ports elsewhere in circuit |
| {spec$1, spec$2, $$} | per-port settings |
* Each port setting can be one of the following:
| Automatic | previous setting plus %PortSpacing |
| Inherited | average position of this-numbered port |
| %AbsoluteOffset[$$] | offset the inherited position |
| %AbsoluteOffset[$$][pos$] | offset another specification |
| {port$1, port$2, $$} | average position of specified ports |
| offset$ | a specific position relative to the center position |
"

NodeBox::badPortPositionLen = "PortPositions was specified with `` values, but `` are present."
NodeBox::badPortPositions = "PortPositions was not a recognized setting.";

processNodeBoxPorts = Case[
  lhs_ -> Style[rhs_, PortSpacing -> s_, rest___]       := Scope[portSpacing = s;       %[lhs -> Style[rhs, rest]]];
  lhs_ -> Style[rhs_, PortShape -> s_, rest___]         := Scope[portShape = s;         %[lhs -> Style[rhs, rest]]];
  lhs_ -> Style[rhs_, PortSize -> s_, rest___]          := Scope[portSize = s;          %[lhs -> Style[rhs, rest]]];
  lhs_ -> Style[rhs_, PortFaceColor -> s_, rest___]     := Scope[portFaceColor = s;     %[lhs -> Style[rhs, rest]]];
  lhs_ -> Style[rhs_, PortEdgeColor -> s_, rest___]     := Scope[portEdgeColor = s;     %[lhs -> Style[rhs, rest]]];
  lhs_ -> Style[rhs_, PortEdgeThickness -> s_, rest___] := Scope[portEdgeThickness = s; %[lhs -> Style[rhs, rest]]];
  lhs_ -> Style[rhs_, HiddenPorts -> s_, rest___]       := Scope[hiddenPorts = s;       %[lhs -> Style[rhs, rest]]];
  lhs_ -> Style[rhs_] := %[lhs -> rhs];
  _ -> {} := {};
  (side:Left|Right|Top|Bottom) -> spec_ := Scope @ Block[{portFaceColor = portFaceColor},
    ports = procPortSpec @ spec;
    n = Length @ ports;
    offsets = If[RuleQ[spec], Keys @ spec, makePortOffsets @ n];
    portCoords = Switch[side,
      Center, ConstantArray[{subPath[$L], subPath[$TB]}, n],
      Left,   Threaded[{subPath[$L], center = subPath[$TB]}] - Thread[{0, offsets}],
      Right,  Threaded[{subPath[$R], center = subPath[$TB]}] - Thread[{0, offsets}],
      Bottom, Threaded[{center = subPath[$LR], subPath[$B]}] + Thread[{offsets, 0}],
      Top,    Threaded[{center = subPath[$LR], subPath[$T]}] + Thread[{offsets, 0}]
    ];
    isVert = MatchQ[side, Top|Bottom]; portXY = If[isVert, 1, 2];
    globalPortOrdinates = $var["Ports", side, #]& /@ ports;
    portPosSpec = If[RuleListQ @ portPositions, Lookup[portPositions, side, Automatic], portPositions];
    portCoordOverrides = Switch[
      portPosSpec,
      Automatic | None,  Null,
      Inherited,         globalPortOrdinates,
      AbsoluteOffset[_], Part[portCoords, All, portXY] += First[portPosSpec]; Null,
      _List,             toSemidelayedPortPositions @ portPosSpec,
      _,                 Message[NodeBox::badPortPositions]; Null
    ];
    If[ListQ[portCoordOverrides] && LengthEqualOrMessage[NodeBox::badPortPositionLen, portCoordOverrides, portCoords],
      (* fill in the relevant ordinate of ports from provided port positions *)
      Part[portCoords, All, If[isVert, 1, 2]] = portCoordOverrides;
    ,
      (* otherwise, store the relevant ordinate so it can be meaned later *)
      ScanThread[
        KeyAppendTo[$portPositions, #1, Part[#2, portXY]]&,
        {globalPortOrdinates, portCoords}
      ];
    ];
    addEqns @ RuleThread[subPath[side, ports], portCoords];
    absPorts = Range[n] + $po; $po += n;
    addEqns @ RuleThread[subPath["PortNumber", absPorts], subPath[side, ports]];
    centerVec = Switch[side, Center, {0, 0}, Left, {1, 0}, Right, {-1, 0}, Top, {0, -1}, Bottom, {0, 1}];
    makePorts[portCoords, ConstantArray[centerVec, n], absPorts]
  ];
  list_List := Map[%, list];
  None      := {};
  spec_     := ThrowMessage["badNodePortSpec", spec];
];

toSemidelayedPortPositions[spec_List] := Scope[
  spec = normalizeNodeInOutPorts[spec] /. np:$nodeCoordinateP :> ListPart[np, portXY];
  MapIndex1[$last = 0; $semidelayed[$i = #2; $last = toSemidelayedPortPos[#1]]&, spec]
];

toSemidelayedPortPos = Case[
  Automatic              := $last + portSpacing;
  Inherited              := Part[globalPortOrdinates, $i];
  spec:{__ListPart}      := Mean[spec];
  a_AbsoluteOffset       := % @ a[Inherited];
  AbsoluteOffset[p_][e_] := %[e] + p;
  n:$NumberP             := n + center;
  other_                 := other;
];

$sideToAngle = <|
  Left  -> 0.5, Right -> 0, Top -> 0.25, Bottom -> -0.25,
  TopLeft -> 0.375, TopRight -> 0.125, BottomLeft -> -0.375, BottomRight -> -0.125
|>

processNodeDiskPorts = Case[
  _ -> {} := None;
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
  spec_     := ThrowMessage["badNodePortSpec", spec];
];

makeCirclePorts[ports_, initAngle_, angles_] := Scope[
  angles = AnglePair[initAngle + angles];
  portCoords = Threaded[currentCenter[]] + angles * subPath["Radius"];
  addEqns @ RuleThread[subPath["Port", ports], portCoords];
  makePorts[portCoords, -angles, ports]
]

currentCenter[] := subPath /@ {$LR, $TB};

General::badNodePortShape = "`` is not a valid PortShape."

hidePorts[list_] := If[Length[hiddenPorts] == 0, list, Delete[list, List /@ hiddenPorts]];

makePorts[coords_, dirs_, absPorts_] := If[Length[hiddenPorts] == 0,
  makePorts1[coords, dirs],
  With[{ind = SelectIndices[absPorts, !MemberQ[hiddenPorts, #]&]},
    makePorts1[Part[coords, ind], Part[dirs, ind]]
  ]
];

makePorts1[coords_, innerDirs_] /; ListQ[portFaceColor] := Table[
  Block[{portFaceColor = Part[portFaceColor, i]}, makePorts1[Part[coords, {i}], Part[innerDirs, {i}]]],
  {i, Length @ coords}
];

makePorts1[coords_, innerDirs_] := Switch[
  portShape,
  "Point",        makePoint[coords, portSize, ToRainbowColor @ portFaceColor],
  "Disk",         makeDisk[coords, portSize, ToRainbowColor @ portFaceColor, ToRainbowColor @ portEdgeColor, portEdgeThickness],
  "OuterDisk",    makeHalfDisks[coords, ArcTan2 @@@ (-innerDirs), portSize, ToRainbowColor @ portFaceColor, ToRainbowColor @ portEdgeColor, portEdgeThickness],
  "InnerDisk",    makeHalfDisks[coords, ArcTan2 @@@ innerDirs, portSize, ToRainbowColor @ portFaceColor, ToRainbowColor @ portEdgeColor, portEdgeThickness],
  "Square",       makeSquare[coords, portSize, ToRainbowColor @ portFaceColor, ToRainbowColor @ portEdgeColor, portEdgeThickness],
  "InnerDiamond", makeHalfDiamond[coords, innerDirs * portSize, ToRainbowColor @ portFaceColor, ToRainbowColor @ portEdgeColor, portEdgeThickness],
  "OuterDiamond", makeHalfDiamond[coords, -innerDirs * portSize, ToRainbowColor @ portFaceColor, ToRainbowColor @ portEdgeColor, portEdgeThickness],
  "InnerSquare",  makeHalfSquare[coords, innerDirs * portSize, ToRainbowColor @ portFaceColor, ToRainbowColor @ portEdgeColor, portEdgeThickness],
  "OuterSquare",  makeHalfSquare[coords, -innerDirs * portSize, ToRainbowColor @ portFaceColor, ToRainbowColor @ portEdgeColor, portEdgeThickness],
  "Lozenge",      makeLozenge[Part[coords, {-1, 1}], portSize, ToRainbowColor @ portFaceColor, ToRainbowColor @ portEdgeColor, portEdgeThickness],
  None,           Null,
  _,              ThrowMessage["badportspec", portShape];
];

(**************************************************************************************************)

makeText[label_, coords_] := ToGraphicsBoxes @ Text[label, coords];

mapMatrix[f_, coords_, args___] := If[MatrixQ[coords], Map[f[#, args]&, coords], f[coords, args]];

makeRoundedRect[c1_, c2_, r_, face_, edge_, thickness_] := StyleBox[RectangleBox[c1, c2, RoundingRadius -> r], FaceEdgeForm[face, edge, thickness]];
makeRoundedRect[c1_, c2_, None|0|0., rest___] := makeRect[c1, c2, rest];

makeRect[c1_, c2_, face_, edge_, thickness_] := StyleBox[RectangleBox[c1, c2], FaceEdgeForm[face, edge, thickness]];

squareBox[p_, r_] := Construct[RectangleBox, p - r, p + r];

makeSquare[coords_, r_, face_, edge_, thickness_] := StyleBox[mapMatrix[squareBox, coords, r], FaceEdgeForm[face, edge, thickness]];

partialCircleBox[pos_, r_, ang_] := Construct[LineBox, Threaded[pos] + DiscretizeCurve[Circle[{0,0}, r, ang]]];
partialDiskBox[pos_, r_, ang_] := Construct[PolygonBox, Threaded[pos] + DiscretizeCurve[Circle[{0,0}, r, ang]]];

makeHalfDiskAngs[angs_] := {# - Pi/2, # + Pi/2}& /@ angs;
makeHalfDisks[coords_, ang_, r_, None, edge_, thickness_] := StyleBox[MapThread[partialCircleBox[#1, r, #2]&, {coords, makeHalfDiskAngs @ ang}], edge, AbsoluteThickness @ thickness];
makeHalfDisks[coords_, ang_, r_, face_, edge_, thickness_] := StyleBox[MapThread[partialDiskBox[#1, r, #2]&, {coords, makeHalfDiskAngs @ ang}], FaceEdgeForm[face, edge, thickness]];

makeHalfDiamond[coords_, dirs_, face_, edge_, thickness_] :=
  StyleBox[
    MapThread[
      Construct[PolygonBox, Threaded[#1] + {VectorRotate90 @ #2, #2, VectorRotate90CW @ #2}]&,
      {coords, dirs}
    ],
    FaceEdgeForm[face, edge, thickness]
  ];

makeHalfSquare[coords_, dirs_, face_, edge_, thickness_] :=
  StyleBox[
    MapThread[
      Construct[PolygonBox, Threaded[#1] + {VectorRotate90[#2], VectorRotate90[#2] + #2, VectorRotate90CW[#2] + #2, VectorRotate90CW[#2]}]&,
      {coords, dirs}
    ],
    FaceEdgeForm[face, edge, thickness]
  ];

makeDisk[coords_, r_, None, edge_, thickness_] := StyleBox[mapMatrix[CircleBox, coords, r], EdgeForm[{edge, AbsoluteThickness @ thickness}]];
makeDisk[coords_, r_, face_, edge_, thickness_] := StyleBox[mapMatrix[DiskBox, coords, r], FaceEdgeForm[face, edge, thickness]];

makePoint[coords_, r_, face_] := StyleBox[PointBox[coords], PointSize[2 * r / $var[$W]], face];
makeLozenge[coords_, r_, face_, edge_, thickness_] := StyleBox[ToGraphicsBoxes @ StadiumShape[coords, r], FaceEdgeForm[face, edge, thickness]];

(**************************************************************************************************)

$defaultNodeSize = {1, 1};
$boxSizeP = $NumberP | Automatic | Inherited;

toNodeBoxInterior = Case[
  sz:$boxSizeP               := % @ {sz, sz};
  {w:$boxSizeP, h:$boxSizeP} := {
    {ReplaceAutomatic[w, $isAutomaticSize[subPath[$W]] = True; First @ $defaultNodeSize],
     ReplaceAutomatic[h, $isAutomaticSize[subPath[$H]] = True; Last @ $defaultNodeSize]},
    None
  };
  other_                     := Scope[
    size = subPath[All, {$W, $H}] + {lmargin + rmargin, bmargin + tmargin};
    interior = processNode[other, All];
    addEqns @ {
      subPath[All, $L] -> subPath[$L] + lmargin,
      subPath[All, $T] -> subPath[$T] - tmargin,
      subPath[$R] -> subPath[All, $R] + rmargin,
      subPath[$B] -> subPath[All, $B] - bmargin
    };
    {size, interior}
  ];
];
