$LR = LeftRight;
$TB = TopBottom;
$T = Top;
$B = Bottom;
$L = Left;
$R = Right;
$H = "H";
$W = "W";

(**************************************************************************************************)

PublicGraphicsPrimitive[NodeComplex]

PublicHead[NodeColumn, NodeRow, NodeGrid, NodeBox, AbsoluteOffset, ManualNodeLayout]

PublicOption[NodeLabel, NodePorts, NodeAlias, PrologStyle, EpilogStyle, NodePalette]

SetUsage @ "
NodeComplex[$$]
"

Options[NodeComplex] = {
  Prolog -> None,
  Epilog -> None,
  EpilogStyle -> None,
  PrologStyle -> None,
  Background -> None,
  NodePalette -> None,
  GraphicsScale -> 40
};

DeclareGraphicsPrimitive[NodeComplex, "Opaque", nodeComplexBoxes];

(**************************************************************************************************)

nodeComplexBoxes[NodeComplex[nodes_, opts___Rule]] := Scope @ CatchMessage[NodeComplex,
  $path = $var["Root"]; $eqs = {}; $nodeAliases = <||>; $varAliases = {}; $isAutomaticSize = <||>;
  $uniqueMeanInd = 0; $portPositionDefaults = {}; $meanConnectionEqs = {}; $meanEqCycles = Null;
  UnpackOptionsAs[NodeComplex, {opts}, epilog, prolog, background, epilogStyle, prologStyle, nodePalette, graphicsScale];
withNodePalette[nodePalette,
  $epilogFn = StyleBoxOperator @ epilogStyle;
  $prologFn = StyleBoxOperator @ prologStyle;
  boxes = attachProEpi[prolog, epilog] @ processNode[nodes];
  addEqns[{subPath[$T] -> 0, subPath[$L] -> 0}];
  $portPositionDefaults //= Flatten;
  $varAliases //= Flatten;
  $nodeAliases //= computeNodeAliases;
  eqs = Flatten @ $eqs;
  (* these heads will only make it into equations currently if a PortPositions spec includes a NodePort *)
  {eqs, boxes, $portPositionDefaults} //= resolveNodeCoordinatesAndAliases;
  JoinTo[eqs, meanConnectionEquations[eqs, boxes]];
  vars = DeepUniqueCases[eqs, _$var];
  solutions = SolveCyclicEquations[eqs, EquationVariables -> vars, ExpandLinearEquations -> True];
  result = boxes /. solutions;
  If[ContainsQ[result, _$var],
    result //= RepAll[$fromNC[v_$var, nc_] :> RuleEval[complainUnresolved[result, v, nc]; {0, 0}]];
(*     Message[NodeComplex::available];
    Print @ Row[Sort @ Keys @ solutions, ", "];
 *)
  ];
  (* this forces the solved coordinates to be evaluated *)
  result = MapPrimitiveBoxCoordinates[N, result];
  result //= ReplaceAllOperator[
    $fromNC[a_, _]                 :> a,
    AbsoluteOffset[off_][pos_List] :> RuleEval[Threaded[off] + pos],
    ListPart                       -> Part,
    $delayedBoxes[e_]              :> RuleEval[ToGraphicsBoxes @ e]
  ];
  result //= ReplaceAllOperator[AbsoluteOffset[off_][pos_] :> RuleEval[pos]];
  result
]];

DefineStandardTraditionalForm[
  $var[args___] :> ToBoxes @ Style[Row @ Riffle[{args}, ":"], FontFamily -> "Fira Code", FontColor -> $Blue]
]

(**************************************************************************************************)

NodeComplex::unresolvedVar = "Variable `` in `` that compiles to `` was not present in solution. Equations defining it are printed below, followed by those mentioning it.";
NodeComplex::matchingVars = "Vars similar to `` are ``.";
NodeComplex::matchingVars = "Vars similar to `` are ``.";
NodeComplex::meanConnectionEquations = "Mean connection equations printed below, followed by any detected cycles.";

complainUnresolved[result_, var_, nodeCoord_] := Scope[
  context = Extract[result, SafeDrop[F @ Position[result, var], -3]] /. ($fromNC[_, n_] :> n) /. (var|nodeCoord -> Style["XXX", Black, Bold]);
  Message[NodeComplex::unresolvedVar, nodeCoord, context, var];
  printEqns @ Select[eqs, F[#] === var&];
  printEqns @ Select[eqs, ContainsQ[L @ #, Verbatim @ var]&];
  If[ListQ[$meanConnectionEqs],
    Message[NodeComplex::meanConnectionEquations];
    printEqns @ $meanConnectionEqs;
    $meanConnectionEqs ^= Null;
  ];
  pattern = var;
  If[MatchQ[L @ var, _Str[_]], Part[pattern, -1, 1] = _];
  Do[
    matchingVars = Cases[vars, App[___] @ pattern];
    If[matchingVars =!= {}, Message[NodeComplex::matchingVars, var, matchingVars]; Break[]];
    pattern //= Most;
  ,
    {n, Len[var] - 1}
  ];
];

(**************************************************************************************************)

meanConnectionEquations[eqns_, boxes_] := Scope[
  meanVars = DeepUniqueCases[{eqns, boxes}, $var[___, "InMean" | "OutMean"]];
  If[meanVars === {}, Return[{}]];
  iConns = oConns = <||>;
  boxes2 = boxes /. $fromNC[v_, _] :> v;
  boxes2 = boxes2 /. CircuitCurve[FanOut[a_, b_], ___] :> RuleEval @ Map[CircuitCurve[{a, #}]&, b];
  cEdges = Flatten @ Map[
    patt |-> DeepCases[boxes2, patt],
    {
      CircuitCurve[{a_$var, b_$var}, ___] :> (
        KAppTo[oConns, a, b];
        KAppTo[iConns, b, a];
        DirectedEdge[a, b]
      ),
      CircuitCurve[{a_$var, b_ /; ContainsQ[b, $var]}, ___] :> (
        KAppTo[oConns, a, b];
        DirectedEdge[a, #] & /@ DeepCases[b, _$var]
      ),
      CircuitCurve[{a_ /; ContainsQ[a, $var], b_$var}, ___] :> (
        KAppTo[iConns, b, a];
        DirectedEdge[#, b] & /@ DeepCases[a, _$var]
      )
  }];
  meanEqs = Map[toMeanVarRule, meanVars];
  varP = Alt @@ vars;
  meanEqDepGraph = Graph @ Flatten @ Cases[meanEqs, Rule[a_, b_] :> Thread[Most[a] -> DeepCases[b, _$var]]];
  $meanEqCycles ^= meanEqCycles = FindCycle[meanEqDepGraph, 2, All];
  If[meanEqCycles =!= {},
    (* this occurs when two ports involve eachother in their means.
    here the downstream port gets its default position to break the cycle *)
    varTopOrderSort = AssociationRange @ TopologicalSort @ Graph @ cEdges;
    meanEqCycleBreakVars = chooseCycleVarToBreak /@ meanEqCycles;
    meanEqs //= Map[filterMeanEqToRemoveCycleVar];

  ];
  $meanConnectionEqs ^= meanEqs
];

filterMeanEqToRemoveCycleVar[var_ -> rhs_] :=
  If[ElementQ[Most @ var, meanEqCycleBreakVars],
    var -> Lookup[$portPositionDefaults, Most @ var],
    var -> rhs
  ];

noExpand[v:(_$var | _List)] := v;
noExpand[e_] := NoLinearExpand[e];

toMeanVarRule[var_] := var -> meanVarRHS[L @ var, Most @ var];
meanVarRHS[type_, var_] := noExpand @ Mean @ Lookup[
  If[type === "InMean", iConns, oConns],
  var, List @ Lookup[$portPositionDefaults, var, {0,0}]
];

chooseCycleVarToBreak[cycle_List] := MaximumBy[Col1[cycle], varTopOrderSort];

(**************************************************************************************************)

PublicHead[NodeCenter, NodeSide, NodePort, NodeInPort, NodeOutPort, NodeCorner, PortIndex]

resolveNodeCoordinatesAndAliases[expr_] := Scope[
  expr //= expandNodeCoordinates;
  expr //= evaluateScopes;
  expr //= applyAliases;
  expandVars[$var[], expr]
];

applyAliases[e_] := ReplaceAllOperator[$nodeAliases, $varAliases /. $nodeAliases] @ e;

computeNodeAliases[aliasAssoc_] := ReverseSort @ KVMap[toAliasRule, aliasAssoc];
toAliasRule[name_Str, var_$var] := App[var, l___] :> $var[name, l];

$nodeCoordinateP = _NodePort | _NodeInPort | _NodeOutPort | _NodeCenter | _NodeCorner | _NodeSide;
expandNodeCoordinates[e_] := RepAll[e, np:$nodeCoordinateP :> RuleEval[$fromNC[procNodeCoordinate @ np, np]]];

procNodeCoordinate = Case[
  NodeInPort[p_:Null, port_]      := makeVar[p, $PortIn[port]];
  NodeOutPort[p_:Null, port_]     := makeVar[p, $PortOut[port]];
  NodeCorner[x_, y_]              := {ListPart[% @ x, 1], ListPart[% @ y, 2]};
  NodeCenter[p_:Null]             := {makeVar[p, $LR], makeVar[p, $TB]};
  NodeSide[p_:Null, side_Symbol]  := makeVar[p, #]& /@ $sideToCoordVars[side];
  NodePort[p_:Null, port_]        := makeVar[p, $Port[port]];
];

makeVar[Null, p_] := $relvar[p];
makeVar[s_, p_] := $var[s, p];

$sideToCoordVars = <|
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

(**************************************************************************************************)

evaluateScopes[expr_] := RepAll[expr, d_$scoped :> RuleEval @ evaluateScope[d]];

evaluateScope[$scoped[path_$var, expr_]] := expandVars[path, evaluateScopes @ expr];

expandVars[path_, e_] := RepAll[e, v_$relvar :> RuleEval @ toAbsVar[v, path]];

toAbsVar[v_$relvar, b_$var] := Join[b, $var @@ v];

toVar[".", args_] := $relvar[args];
toVar[args_] := $var[args];

(**************************************************************************************************)

PublicHead[NodeColumn, NodeRow, NodeGrid, NodeFrame, NodeBox, NodeDisk, PortSkeleton, NodePort]

General::badNodeSpec = "Invalid node expression: ``.";

processNode = Case[

  ManualNodeLayout[nodes_List, size_List, opts___Rule] := manualNodeLayout[nodes, size, opts];

  NodeColumn[nodes_List, opts___Rule] := columnLayout[nodes, {opts}];
  NodeRow[nodes_List, opts___Rule]    := rowLayout[nodes, {opts}];
  NodeGrid[spec_List, opts___Rule]    := gridLayout[spec, {opts}];
  NodeFrame[interior_]                := nodeFrame[interior];

  PortSkeleton[args___]               := portSkeletonBox[args];

  NodeBox[args___]                    := nodeBox[NodeBox, args];
  NodeDisk[args___]                   := nodeBox[NodeDisk, args];

  Spacer[{w_, h_}]                    := emptyBox[w, h];
  Padded[node_, spec_]                := paddedNode[node, spec];

  Pane[text_, size_, opts___Rule]     := paneNode[text, size, opts];
  p_Point                             := (emptyBox[0, 0]; makeDelayed @ Translate[p, currentCenter[]]);

  node_                               := Msg::badNodeSpec[node];

  Sequence[node_, part_] := Block[
    {$path = App[$path, part]},
    processNode @ node
  ];
];

addEqns[eqns_List, rhs_] := addEqns @ Map[# -> rhs&, eqns];
addEqns[lhs_, rhs_] := addEqns[lhs -> rhs];
addEqns[eqns_] := (AppTo[$eqs, eqns]; eqns);

createNodeAlias := Case[
  None       := Null;
  i_Int  := % @ TextString[i];
  str_Str := AssociateTo[$nodeAliases, str -> $path];
];

subPath[] := $path;
subPath[e_, f_List] := Map[subPath[e, #]&, f];
subPath[e_List, f_] := Map[subPath[#, f]&, e];
subPath[e_, f_] := App[App[$path, e], f];
subPath[e_, f_, g_] := App[App[App[$path, e], f], g];
subPath[e_] := App[$path, e];

(**************************************************************************************************)

SetHoldRest[withNodePalette];
withNodePalette[None | Auto | {}, body_] := body;
withNodePalette[palette_, body_] := InheritedBlock[
  {$nodePalette},
  AssociateTo[$nodePalette, Map[ToRainbowColor, Assoc @ palette]];
  body
];

$nodePalette = <||>;
toRainbowColor[i_] := Lookup[$nodePalette, i, ToRainbowColor @ i];
toRainbowColor[p_, i_] := Lookup[$nodePalette, p, Lookup[$nodePalette, i, ToRainbowColor[p, i]]];

applyNodePalette[e_] := e /. $portColor[p_] :> RuleEval[Lookup[$nodePalette, p]];

(**************************************************************************************************)

paddedNode[interior_, spec_] := Scope[
  {{lmargin, rmargin}, {bmargin, tmargin}} = StandardizePadding[spec];
  {{$w, $h}, interior} = toNodeBoxInterior @ interior;
  addFrameEqns[$w, $h];
  interior
];

emptyBox[w_, h_] := (addFrameEqns[w, h]; {})

(**************************************************************************************************)

Options[paneNode] = {
  Background -> None, FrameColor -> None, FrameThickness -> 1, RoundingRadius -> None, Alignment -> Center
};

paneNode[text_, {w_, h_}, OptionsPattern[]] := Scope[
  UnpackOptions[frameColor, background, frameThickness, roundingRadius, alignment];
  addFrameEqns[w, h];
  fet = {toRainbowColor @ background, toRainbowColor @ frameColor, frameThickness};
  boxPrimitives = makeRoundedRect[subPath /@ {$L, $B}, subPath /@ {$R, $T}, roundingRadius, fet];
  align = {1.2, 1} * -Lookup[$SideToCoords, alignment];
  labelPos = subPath /@ Lookup[$sideToCoordVars, alignment];
  List[
    boxPrimitives,
    makeDelayed @ {
      Style[Point[labelPos], Opacity[0]],
      Text[text, labelPos, align, BaseStyle -> ToList[FontWeight -> "Regular", $NodeComplexLabelStyle]]
    }
  ]
];

(**************************************************************************************************)

nodeFrame[node_] :=
  {
    makeRect[subPath /@ {$L, $B}, subPath /@ {$R, $T}, {None, $Red, 1}],
    processNode[node]
  };

(**************************************************************************************************)

columnLayout[l___, NodePalette -> p_, r___] := withNodePalette[p, columnLayout[l, r]];
columnLayout[nodes_List, opts_] :=
  hvNodeLayout[NodeColumn, nodes, opts, {Left, Right}, {$T, $B, $H, $L, $R, $W}, -1];

(**************************************************************************************************)

rowLayout[l___, NodePalette -> p_, r___] := withNodePalette[p, rowLayout[l, r]];
rowLayout[nodes_List, opts_] :=
  hvNodeLayout[NodeRow, nodes, opts, {Top, Bottom}, {$L, $R, $W, $T, $B, $H}, 1];

(**************************************************************************************************)

Options[NodeRow] = Options[NodeColumn] = {
  NodeAlias -> None,
  Alignment -> Center,
  FrameMargins -> 0,
  Spacings -> .5,
  Epilog -> None,
  Prolog -> None,
  NodePalette -> None
};

NodeComplex::badalign = "Alignment -> `` not valid for ``."

hvNodeLayout[head_, nodes_List, opts_, {startSym_, endSym_}, {mainStart_, mainEnd_, mainDim_, otherStart_, otherEnd_, otherDim_}, mult_] := Scope[
  UnpackOptionsAs[head, opts, alignment, frameMargins, spacings, epilog, prolog, nodeAlias, nodePalette];
withNodePalette[nodePalette,
  createNodeAlias[nodeAlias];
  SetAuto[alignment, 0];
  SetAuto[frameMargins, 0];
  {lrmargin, btmargin} = StandardizePadding[frameMargins];
  tbmargin = Rev @ btmargin;
  {{smargin, emargin}, {osmargin, oemargin}} = If[head === NodeRow, {lrmargin, tbmargin}, {tbmargin, lrmargin}];
  SetAuto[spacings, 1];
  n = Len @ nodes;
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
      Msg::badalign[alignment, head];
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
]];

(**************************************************************************************************)

Options[ManualNodeLayout] = {
  Alignment -> Center,
  Epilog -> None,
  Prolog -> None,
  NodeAlias -> None
};

manualNodeLayout[nodes_List, {w_, h_}, OptionsPattern[ManualNodeLayout]] := Scope[
  UnpackOptions[alignment, epilog, prolog, nodeAlias];
  createNodeAlias[nodeAlias];
  {$halign, $valign} = ToAlignmentPair @ alignment;
  addFrameEqns[w, h];
  nodes = MapIndex1[processManualNode, nodes];
  attachProEpi[prolog, epilog] @ nodes
]

processManualNode[pos_ -> Item[node_, Alignment -> align_], i_] := Scope[
  {$halign, $valign} = ToAlignmentPair @ align;
  processManualNode[pos -> node, i]
];

processManualNode[{x_, y_} -> node_, i_] := (
  addEqns @ {
    subPath[i, Switch[$valign, Top,  $T, Center, $TB, Bottom, $B]] -> subPath[$T] - y,
    subPath[i, Switch[$halign, Left, $L, Center, $LR,  Right, $R]] -> subPath[$L] + x
  };
  processNode[node, i]
);

(**************************************************************************************************)

NodeGrid::badNodeGridSpec = "Spec `` should be a dense matrix of items or a list of rules mapping position to item.";

Options[NodeGrid] = {
  RowAlignments -> Center,
  ColumnAlignments -> Center,
  RowSpacings -> .5,
  ColumnSpacings -> .5,
  FrameMargins -> 0,
  Epilog -> None,
  Prolog -> None,
  NodePalette -> None
}

gridLayout[spec_List, {l___, NodePalette -> p_, r___}] :=
  withNodePalette[p, gridLayout[spec, {l, r}]];

gridLayout[spec_List, opts_List] := Scope[
  UnpackOptionsAs[NodeGrid, {opts},
    rowAlignments, columnAlignments, rowSpacings, columnSpacings, frameMargins,
    prolog, epilog
  ];
  $halign = columnAlignments; $valign = rowAlignments;
  $hspacing = columnSpacings; $vspacing = rowSpacings;
  items = CatchMessage[NodeGrid, parseGridItems[spec]];
  attachProEpi[prolog, epilog] @ items
];

NodeGrid::unspecsize = "Cannot choose a `` for at least one ``.";

parseGridItems = Case[
  rules:{__Rule}  := Scope[
    $singletonsR = $singletonsC = <||>;
    {maxR, maxC} = Map[Max, Transpose @ MatrixMap[parseSpan, Keys @ rules]];

    $hspacing ^= ParseCyclicSpec[maxC-1] @ $hspacing;
    $vspacing ^= ParseCyclicSpec[maxR-1] @ $vspacing;
    rangeR = Range @ maxR;
    rangeC = Range @ maxC;

    (* vars for row heights, column widths *)
    varsH = subPath["Rows", #, $H]& /@ rangeR;
    varsW = subPath["Cols", #, $W]& /@ rangeC;

    items = Map[$i = 1; parseGridRuleItem, rules];

    (* obtain RHS for row heights, column widths, removing widths and heights that are inherited from the row/column itself *)
    allSizesC = Lookup[$singletonsC, rangeC, Msg::unspecsize["width", "column"]];
    allSizesR = Lookup[$singletonsR, rangeR, Msg::unspecsize["height", "row"]];
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
  other_         := Msg::badNodeGridSpec[other];
];

maxRCSize[list_] := Max @ Rep[Discard[list, Lookup[$isAutomaticSize, #, False]&], {} -> {1}];

NodeGrid::baditemalign = "Item Alignment -> `` is invalid."

parseGridRuleItem = Case[
  Rule[rc:{_, _}, Item[item_, Alignment -> align_]] := Scope[
    pair = ToAlignmentPair[align];
    If[FailureQ[pair], Msg::baditemalign[align]];
    {$halign, $valign} = pair;
    % @ Rule[rc, item]
  ];
(*   Rule[pos_, Translate[item_, delta_]] :=
    Translate[%[pos -> item],
 *)  Rule[rc:{_, _}, item_] := Scope[
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
  i_Int              := {i, i};
  Span[i_Int, j_Int] := {i, j};
  other_             := Msg::badspan[other];
]

getMax = Case[
  i_Int    := i;
  _;;i_Int := i;
  other_   := Msg::badspan[other];
]


(**************************************************************************************************)

Options[PortSkeleton] = {
  PortSpacing       -> 0.2,
  PortSize          -> 0.05,
  PortShape         -> "Disk",
  PortEdgeColor     -> Auto,
  PortColor         -> Black,
  PortEdgeThickness -> 1,
  PortPositions     -> Auto,
  NodeAlias         -> None
}

portSkeletonBox[w_, nodePorts_, opts___Rule] := Scope[
  UnpackOptionsAs[PortSkeleton, {opts}, nodeAlias];
  createNodeAlias[nodeAlias];
  $w = w;
  $isNodeDisk = False;
  {$portData, $defaultPortData} = OptionValueAssociations[PortSkeleton, replaceIO @ {opts}, $portOptionKeys];
  portPrimitives = Flatten @ processNodeBoxPorts[Bottom -> nodePorts];
  addFrameEqns[$w, 0];
  portPrimitives
];

portSkeletonBox[str_Str, opts___Rule] :=
  portSkeletonBox[.5, 1, PortStyleData[str], opts];

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
PublicOption[PortSpacing, PortPositions, PortSize, PortShape, PortEdgeColor, PortColor, PortEdgeThickness, HiddenPorts, PortPositions, PortLabelStyle, NodeLabelOffset]
PublicVariable[$NodeComplexLabelStyle, $PortLabelStyle]

(* TODO: split the styles up into separate options *)
SetInitialValue[$NodeComplexLabelStyle, {FontSize  -> 16, FontWeight -> "Bold", FontFamily -> "Fira Code", PrivateFontOptions -> {"OperatorSubstitution" -> False}, BaseStyle -> "PreformattedCode"}];
SetInitialValue[$PortLabelStyle, {FontSize -> 11, FontFamily -> "Fira Code", PrivateFontOptions -> {"OperatorSubstitution" -> False}, BaseStyle -> "PreformattedCode"}];

Options[NodeDisk] = Options[NodeBox] = {
  Background        -> None,
  Epilog            -> None,
  FrameColor        -> $Gray,
  FrameLabel        -> None,
  FrameLabelSpacing -> 0.1,
  FrameLabelStyle   :> $NodeComplexLabelStyle,
  FrameMargins      -> 0.1,
  FrameThickness    -> 1,
  FrameDashing      -> None,
  NodeAlias         -> None,
  NodeLabel         -> None,
  NodeLabelColor    -> None,
  NodeLabelStyle    :> $NodeComplexLabelStyle,
  NodeLabelOffset   -> 0,
  NodePalette       -> None,
  NodePorts         -> None,
  PortLabelStyle    :> $PortLabelStyle,
  PortEdgeColor     -> Auto,
  PortEdgeThickness -> 1,
  PortColor         -> Black,
  PortPositions     -> Auto,
  PortShape         -> "Disk",
  PortSize          -> 0.05,
  PortSpacing       -> Auto,
  Prolog            -> None,
  RoundingRadius    -> None
}

SetUsage @ "
NodeBox[interior$] represents a rectangular node in a %NodeComplex.
* interior$ can be Automatic, w$, {w$, h$}, or further boxes.
* The following options are supported:
| %Background | color of background of box |
| %Epilog | graphics primitives drawn after the box |
| %Prolog | graphics primitives drawn before the box |
| %FrameColor | color of frame of box |
| %FrameLabel | label on outside of frame |
| %FrameLabelSpacing | distance from frame to label |
| %FrameLabelStyle | style of frame label |
| %FrameThickness | thickness of frame of box |
| %FrameMargins | space between frame and interior |
| %NodeAlias | global name for this node |
| %NodeLabel | label shown in interior of node |
| %NodeLabelColor | color of interior node label |
| %NodeLabelStyle | other styles |
| %NodePorts | ports to attach to node |
* For %NodePorts, the following settings are supported:
| side$ -> n$ | create n$ ports on the given side |
| side$ -> {p$1, p$2, $$} | create ports with the given names on the given side |
| {rule$1, rule$2, $$} | create ports on multiple sides |
* The following options control how ports appear:
| %PortEdgeColor | color of edge of shape of port |
| %PortEdgeThickness | thickness of edge of shape of port |
| %PortColor | color of shape of port |
| %PortPositions | positions of ports |
| %PortShape | shape of port |
| %PortSize | size of port |
| %PortSpacing | automatic gap between ports |
* Any of the above options can be a single value, or a list of rules mapping sides to values.
* Additionally, a list indicates that there is a distinct value for each port.
* A list of rules of the form {port$1 -> val$1, $$, All -> default$} can also be provided.
"

(* all these options have a uniform inheritance mechanism:
when passed as function options to NodeBox, they can be a single value, or per-side rules: {side -> spec, ...}
they can also be customized per individual port by wrapping the port spec in Style[..., opt -> ...]
moreover, if they are a list of values, they will be matched up with the relevant ports.
*)

$portOptionKeys = {"PortSpacing", "PortSize", "PortShape", "PortEdgeColor", "PortColor", "PortEdgeThickness", "PortPositions"};

replaceIO = RepAll[{In -> Top, Out -> Bottom}];

DefineSimpleMacro[OptionValueAssociations,
  OptionValueAssociations[head_, opts_, keys_] :> {
    AssocThread[keys, OptionValue[head, opts, keys]],
    AssocThread[keys, OptionValue[head, keys]]
  }
];

nodeBox[l___, NodePalette -> p_, r___] := withNodePalette[p, nodeBox[l, r]];
nodeBox[head_, spec_, opts___Rule] := Scope @ CatchMessage[head,
  UnpackOptionsAs[head, {opts},
    background, nodePorts, nodeLabel, nodeLabelStyle, nodeLabelColor, nodeLabelOffset,
    frameMargins, frameDashing, epilog, prolog, nodeAlias,
    portLabelStyle,
    (* portSpacing, portSize, portShape, portEdgeColor, portColor, portEdgeThickness, portPositions, *)
    frameColor, background, frameThickness, roundingRadius,
    frameLabel, frameLabelSpacing, frameLabelStyle
  ];

  $isNodeDisk = head === NodeDisk;
  {$portData, $defaultPortData} = OptionValueAssociations[head, replaceIO @ {opts}, $portOptionKeys];
  createNodeAlias[nodeAlias];

  {{lmargin, rmargin}, {bmargin, tmargin}} = StandardizePadding[frameMargins];
  {{$w, $h}, interior} = toNodeBoxInterior @ spec;

  portPrimitives = If[$isNodeDisk, processNodeDiskPorts, processNodeBoxPorts] @ replaceIO @ nodePorts;
  portPrimitives //= Flatten;
  If[portPrimitives === {}, portPrimitives = Nothing];

  addFrameEqns[$w, $h];

  fet = {toRainbowColor @ background, toRainbowColor @ frameColor, frameThickness};
  boxPrimitives = If[$isNodeDisk,
    addEqns[subPath["Radius"], Max[$w, $h]/2];
    makeDisk[currentCenter[], subPath["Radius"], fet]
  ,
    makeRoundedRect[subPath /@ {$L, $B}, subPath /@ {$R, $T}, roundingRadius, fet]
  ];

  If[frameDashing === True,
    boxPrimitives = boxPrimitives /. EdgeForm[e_] :> EdgeForm[ToList[e, Dashed]]];

  labelPrimitives = If[nodeLabel === None, Nothing,
    ToGraphicsBoxes @ Text[
      StyleOperator[toRainbowColor @ nodeLabelColor] @ StyleOperator[nodeLabelStyle] @ nodeLabel,
      If[nodeLabelOffset =!= 0, AbsoluteOffset[nodeLabelOffset / graphicsScale], Id] @ currentCenter[]
    ]];

  frameLabelPrimitives = makeFrameLabel[frameLabel];

  interiorPrimitives = If[interior === None, Nothing, interior];

  List[
    boxPrimitives,
    $prologFn @ makeDelayed @ prolog,
    StyleBox[portPrimitives, ZOrder -> 1],
    interiorPrimitives,
    labelPrimitives,
    frameLabelPrimitives,
    $epilogFn @ makeDelayed @ epilog
  ]
];

attachProEpi[None, None][prims_] := prims;
attachProEpi[pro_, epi_][prims_] := {$prologFn @ makeDelayed @ pro, prims, $epilogFn @ makeDelayed @ epi};

makeDelayed[None | {}] := Nothing;
makeDelayed[prims_] := $delayedBoxes[$scoped[$path, applyNodePalette @ prims]];

(**************************************************************************************************)

General::badNodeFrameLabel = "`` is not a valid spec for FrameLabel.";
makeFrameLabel = Case[
  None | {}  := Nothing;
  other_     := %[Top -> other];
  list_List  := Map[%, list];
  side:$SidePattern -> label_ := Scope[
    coords = Lookup[$sideToCoordVars, side, Failure["badNodeFrameLabel", side -> label]];
    coords = subPath /@ coords;
    makeLabel[label, coords, side, frameLabelSpacing, frameLabelStyle]
  ];
]

(**************************************************************************************************)

makePortOffsets[n_, portSpacing_] := Scope[
  offsets = Range[n] * portSpacing;
  offsets - Mean[offsets]
];

General::badNodePortSpec = "`` is not a valid port specification."
procPortSpec = Case[
  n_Int  := Range @ n;
  s_Str  := {s};
  l_List := l;
  spec_  := Msg::badNodePortSpec[spec];
];

SetUsage @ "
NodePort is an option for %NodeBox and %NodeDisk that specifies the ports to be created on the node.
";

SetUsage @ "
PortPositions is an option for %NodeBox and %NodeDisk that specifies where ports will be placed on relevant side.
* It can take the following settings:
| {side$1 -> spec$1, $$} | specs per side |
| Automatic | centered, spaced according to %PortSpacing (default) |
| %AbsoluteOffset[off] | offset the values produced by Automatic |
| 'MatchOut' | use average position of downstream nodes connected by CircuitCurves |
| 'MatchIn' | use average position of upstream nodes connected by CircuitCurves |
| 'MatchInterior' | use 'MatchOut' for Top and 'MatchIn' for Bottom |
| 'MatchExterior' | use 'MatchIn' for Top and 'MatchOut' for Bottom |
| {spec$1, spec$2, $$} | per-port settings |
* Each port setting can be one of the following:
| Automatic | previous setting plus %PortSpacing |
| 'MatchOut' | use average position of downstream nodes connected by CircuitCurves |
| 'MatchIn' | use average position of upstream nodes connected by CircuitCurves |
| %AbsoluteOffset[$$] | offset the inherited position |
| %AbsoluteOffset[$$][pos$] | offset another specification |
| {port$1, port$2, $$} | average position of specified ports |
| offset$ | a specific position relative to the center position |
"

NodeBox::badPortPositionLen = "PortPositions was specified with `` values, but `` are present. Did you specify a side?"
NodeBox::badPortPositions = "PortPositions -> `` was not a recognized setting.";

$sideP = Left | Right | Bottom | Top | All;

$PortIn = "IPort";
$PortOut = "OPort";
$PortLeft = "LPort";
$PortRight = "RPort";
$Port = "Port";

sideToPortKind = Case[
  Left         := $PortLeft;
  Right        := $PortRight;
  Top | In     := $PortIn;
  Bottom | Out := $PortOut;
]

sideSpecializedPortData[side_] := MapThread[
  If[MatchQ[#1, {($sideP -> _)..} | ($sideP -> _)], Lookup[#1, side, Lookup[#, All, #2]], #1]&,
  {$portData, $defaultPortData}
];

portDataRules[rules___] := SymbolName[#1] -> #2& @@@ {rules};

processNodeBoxPorts = Case[
  None                                  := {};
  _ -> {}                               := {};
  list_List                             := Map[%, list];
  spec_                                 := Msg::badNodePortSpec[spec];
  side_ -> Style[spec_, rules___Rule]   := InheritedBlock[{$portData},
    AssociateTo[$portData, portDataRules[rules]];
    %[side -> spec]
  ];
  (side:Left|Right|Top|Bottom) -> spec_ := Scope[
    ports = procPortSpec @ spec;
    portPaths = makePortPaths[sideToPortKind @ side, ports];
    n = Len @ ports;
    $sidePortData = sideSpecializedPortData[side];
    portSpacing = SubAuto[$sidePortData["PortSpacing"], $w / (n + .333)];
    offsets = makePortOffsets[n, portSpacing];
    portCoords = Switch[side,
      Center, Repeat[{subPath[$L], subPath[$TB]}, n],
      Left,   Threaded[{subPath[$L], center = subPath[$TB]}] - Thread[{0, offsets}],
      Right,  Threaded[{subPath[$R], center = subPath[$TB]}] - Thread[{0, offsets}],
      Bottom, Threaded[{center = subPath[$LR], subPath[$B]}] + Thread[{offsets, 0}],
      Top,    Threaded[{center = subPath[$LR], subPath[$T]}] + Thread[{offsets, 0}]
    ];
    isVert = MatchQ[side, Top|Bottom]; portXY = If[isVert, 1, 2];
    portPosSpec = Rep[$sidePortData @ "PortPositions", {
      "MatchInterior" :> If[side === Top, "MatchOut", "MatchIn"],
      "MatchExterior" :> If[side === Top, "MatchIn", "MatchOut"]
    }];
    portCoordOverrides = Switch[
      portPosSpec,
      Auto | None,       Null,
      "MatchIn" | "MatchOut", toPortPositions @ Repeat[portPosSpec, n],
      AbsoluteOffset[_],      Part[portCoords, All, portXY] += F[portPosSpec]; Null,
      _List,                  toPortPositions @ portPosSpec,
      _,                      Message[NodeBox::badPortPositions, portPosSpec]; Null
    ];
    If[ListQ[portCoordOverrides],
      SameLenMsg::badPortPositionLen[portCoordOverrides, portCoords];
      (* fill in the relevant ordinate of ports from provided port positions *)
      $portPositionDefaults ^= {$portPositionDefaults, RuleThread[portPaths, portCoords]};
      Part[portCoords, All, If[isVert, 1, 2]] = portCoordOverrides;
    ];
    centerVec = Switch[side, Center, {0, 0}, Left, {1, 0}, Right, {-1, 0}, Top, {0, -1}, Bottom, {0, 1}];
    makePorts @ <|"paths" -> portPaths, "coords" -> portCoords, "dirs" -> Repeat[centerVec, n], "ports" -> ports, $sidePortData|>
  ];
];

makePortPaths[kind_, ports_List] := Scope[
  ports = ports /. {Invisible[p_] :> p, Style[p_, ___] :> p, Placed[p_, ___] :> p};
  portPaths = subPath[kind[#]]& /@ ports;
  i = 1; indexedPortPaths = subPath[kind[PortIndex[i++]]]& /@ ports;
  $varAliases ^= {$varAliases,
    If[kind === $Port, {},
      namedPortPaths = subPath[$Port[#]]& /@ ports;
      RuleThread[namedPortPaths, portPaths]
    ],
    RuleThread[indexedPortPaths, portPaths]
  };
  portPaths
]

(**************************************************************************************************)

toPortPositions[spec_List] := Scope[
  spec = spec /. np:$nodeCoordinateP :> ListPart[np, portXY];
  MapIndex1[$last = 0; ($i = #2; $last = toSinglePortPosition[#1])&, spec]
];

toSinglePortPosition = Case[
  Auto              := $last + portSpacing;
  "MatchIn"              := ListPart[App["InMean"] @  Part[portPaths, $i], portXY];
  "MatchOut"             := ListPart[App["OutMean"] @ Part[portPaths, $i], portXY];
  spec:{__ListPart}      := Mean[spec];
  a_AbsoluteOffset       := % @ a[Inherited];
  AbsoluteOffset[p_][e_] := %[e] + p;
  n:$NumberP             := n + center;
  other_                 := other;
];

(**************************************************************************************************)

$sideToAngle = <|
  Left  -> -0.25, Right -> 0.25, Top -> 0, Bottom -> 0.5,
  TopLeft -> -0.125, TopRight -> 0.125, BottomLeft -> -0.375, BottomRight -> 0.375
|>

processNodeDiskPorts = Case[
  _ -> {} := None;
  (* left, top, right input ports, bottom output port
  used by arithmetic nodes but NodeSide is better in practice.
  still, might be useful later. *)
  "Compass" := Scope[
    dirs = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
    portCoords = Threaded[currentCenter[]] + dirs * subPath["Radius"];
    portPaths = Join[makePortPaths[$PortIn, {1,2,3}], makePortPaths[$PortOut, {1}]];
    makePorts @ <|"paths" -> portPaths, "coords" -> portCoords, "dirs" -> -dirs, "ports" -> {1,2,3,1}, $portData|>
  ];
  spec:{(($SidePattern | $NumberP) -> _)..} := Scope[
    {angles, ports} = KeysValues @ spec;
    angles = angles /. $sideToAngle;
    makeCirclePorts[ports, angles]
  ];
  (side:$SidePattern -> spec_List) := Scope[
    initAng = Lookup[$sideToAngle, side];
    n = Len[spec];
    angles = initAng + Range[0,n-1] * SubAuto[$portData["PortSpacing"], 1 / n];
    % @ RuleThread[N @ angles, spec]
  ];
  n_Int        := %[Top -> Range[n]];
  list:{__Int} := %[Top -> list];
  None         := {};
  spec_        := Msg::badNodePortSpec[spec];
];

makeCirclePorts[ports_, angles_] := Scope[
  dirs = AnglePair[-(angles - 0.25)];
  portCoords = Threaded[currentCenter[]] + dirs * subPath["Radius"];
  portPaths = makePortPaths[$Port, ports];
  makePorts @ <|"paths" -> portPaths, "coords" -> portCoords, "dirs" -> -dirs, "ports" -> ports, $portData|>
]

currentCenter[] := subPath /@ {$LR, $TB};

(**************************************************************************************************)

General::badNodePortShape = "PortShape -> `` is not one of ``."

procPortListSpec[spec_, n_, prev_] := Repeat[spec, n];
procPortListSpec[spec_List, n_, prev_] := PadRight[spec, n, prev];
procPortListSpec[rules:{__Rule}, n_, prev_] := VectorReplace[Range[n], App[_ -> prev] @ rules];
procPortListSpec[{rules__Rule, All -> def_}, n_, _] := procPortListSpec[{rules}, n, def];

makePorts[data_Assoc] := Scope[
  n = Len @ data["ports"];
  AssociationMapThread[
    Fn[
      Associate[#, "PortShape" -> fixShape[#PortShape, #dirs]]
    ] /*
    Fn[
      Associate[#, "frameOffset" -> (frameThickness/2 * shapeOffset[#PortShape] * #dirs / graphicsScale)]
    ] /*
    Fn[
      addEqns[#paths, #coords + 2 * #frameOffset];
      makeSinglePort[#ports, #]
    ],
    MapThread[
      procPortListSpec[#1, n, #2]&,
      KDrop[KUnion[{data, $defaultPortData}], {"PortPositions", "PortSpacing"}]
    ]
  ]
];

fixShape[shape_Str, dirs_] /; SStartsQ[shape, "DownHalf"] :=
  If[P2[dirs] > 0, "Outer", "Inner"] <> SDrop[shape, 8];
fixShape[shape_, _] := shape;

$shapeP = _Str | _Labeled | _Placed;
makeSinglePort = Case[
  Sequence[_Invisible, _]                        := {};
  Sequence[Style[p_, s:$shapeP, opts___], data_] := %[Style[p, PortShape -> s, opts], data];
  Sequence[Style[p_, n_Int], data_]              := %[Style[p, PortColor -> n, opts], data];
  Sequence[Style[p_, opts___Rule], data_]        := $makeSinglePortFn @ Assoc[data, portDataRules @ opts];
  Sequence[_, data_]                             := $makeSinglePortFn @ data;
];

$makeSinglePortFn = Fn[
  shapeToFn[#PortShape] @
  App[#, {
    "FET" -> toFET[#ports, #PortColor, #PortEdgeColor, #PortEdgeThickness],
    "coords" -> #coords + #frameOffset
  }]
];

toFET[Style[p_, c:($ColorPattern | _Int)], f_, e_, t_] := toFET[p, c, e, t];
toFET[p_, f_, e_, t_] /; KeyQ[$nodePalette, p]   := {toRainbowColor @ $nodePalette @ p, toRainbowColor @ e, t};
toFET[i_Int, f_, e_, t_]                               := {toRainbowColor[f, i], toRainbowColor[e, i], t};
toFET[_, f_, e_, t_]                                   := {toRainbowColor @ f, toRainbowColor @ e, t};

shapeOffset = Case[
  _                   := 0;
  s_Str               := Which[
    SStartsQ[s, "Inner"], 1,
    SStartsQ[s, "Outer"], -1,
    True, 0
  ];
  Labeled[_, _, pos_] := % @ Placed[Null, pos];
  Placed[_, Below|BottomLeft|Bottom|BottomRight] := 1;
  Placed[_, Above|TopLeft|Top|TopRight]          := -1;
]

$needsRounding := $isNodeDisk || TrueQ[roundingRadius >= $w/2];

shapeToFn := Case[
  "InnerDisk" /; TrueQ[$needsRounding]      := Fn[makeHalfDisk[#coords, -#dirs, {#PortSize, $w/2}, #FET]];
  "OuterDisk" /; TrueQ[$needsRounding]      := Fn[makeHalfDisk[#coords,  #dirs, {#PortSize, $w/2}, #FET]];
  shape_                                    := Lookup[$shapeToFn, shape, Msg::badNodePortShape[shape, Keys @ $shapeToFn]];
  Placed[label_, pos_Symbol]                := Fn[makeLabel[label, #coords - #frameOffset/2, pos, 0.2, portLabelStyle]];
  Labeled[shape_, label_, pos_]             := ApplyThrough[{% @ shape, % @ Placed[label, pos]}]
];

$shapeToFn = <|
  "Point"        -> Fn[makePoint[#coords, #PortSize, #FET]],
  "Disk"         -> Fn[makeDisk[#coords, #PortSize, #FET]],
  "OuterDisk"    -> Fn[makeHalfDisk[#coords, -#dirs, #PortSize, #FET]],
  "InnerDisk"    -> Fn[makeHalfDisk[#coords, #dirs, #PortSize, #FET]],
  "Square"       -> Fn[makeSquare[#coords, #PortSize, #FET]],
  "OuterDiamond" -> Fn[makeHalfDiamond[#coords, -#dirs * #PortSize, #FET]],
  "InnerDiamond" -> Fn[makeHalfDiamond[#coords, #dirs * #PortSize, #FET]],
  "OuterSquare"  -> Fn[makeHalfSquare[#coords, -#dirs * #PortSize, #FET]],
  "InnerSquare"  -> Fn[makeHalfSquare[#coords, #dirs * #PortSize, #FET]],
  "OuterTrapezoid" -> Fn[makeHalfTrap[#coords, -#dirs * #PortSize, #FET]],
  "InnerTrapezoid" -> Fn[makeHalfTrap[#coords, #dirs * #PortSize, #FET]],
  None           -> ({}&)
|>;

(**************************************************************************************************)

edgeForm[prim_, e_, t_] := StyleBox[prim, e, AbsoluteThickness @ t, CapForm[None]];

toEdgeCol[face_][Auto] := Darker[face, .2];
toEdgeCol[face_][edge_] := edge;

applyStyle[fet_][obj_] := applyStyle[obj, fet];
applyStyle[PolygonBox[coords_], {None,  edge_, thickness_}] := edgeForm[Construct[LineBox, closePath @ coords], edge, thickness];
applyStyle[DiskBox[args__],     {None,  edge_, thickness_}] := edgeForm[CircleBox[args], edge, thickness];
applyStyle[halfThing[f_, e_], fet_] := applyStyle[f, fet];
(*
(* this does avoids edging on the inner side of ports *)
applyStyle[halfThing[f_, e_],   {face_, None,  thickness_}] := applyStyle[f, {face, None, thickness}];
applyStyle[halfThing[f_, e_],   {None,  edge_, thickness_}] := edgeForm[e, edge, thickness];
applyStyle[halfThing[f_, e_],   {face_, edge_, thickness_}] := {StyleBox[f, FaceEdgeForm[face, None]], edgeForm[e, toEdgeCol[face] @ edge, thickness]};
*)
applyStyle[other_,              {face_, edge_, thickness_}] := StyleBox[other, FaceEdgeForm[face, edge, thickness]];

closePath[coords_] := If[F[coords] === L[coords], coords, App[F @ coords] @ coords];

(**************************************************************************************************)

makeHalfPolygon[points_] := halfThing[makePolygon @ points, Construct[LineBox, points]];
makePolygon[points_] := Construct[PolygonBox, points];

makeRoundedRect[c1_, c2_, r_, fet_] := makeRect[c1, c2, fet, RoundingRadius -> r];
makeRoundedRect[c1_, c2_, None|0|0., fet_] := makeRect[c1, c2, fet];

makeRect[c1_, c2_, fet_, opts___Rule] := applyStyle[fet] @ RectangleBox[c1, c2, opts];
makeSquare[p_, r_, fet_] := makeRect[p - r, p + r, fet];

arcPoints[ang_] := arcPoints[ang] = DiscretizeCurve[Circle[{0,0}, 1, {-ang, ang}]];
SetCached[$halfCirclePoints, arcPoints[Pi/2]];

makeHalfDisk[pos_, dir_, r_, fet_] := applyStyle[fet] @
  makeHalfPolygon @ ScaleRotateTranslateVector[r, PairAngle @ dir, pos, $halfCirclePoints];

makeHalfDisk[pos_, dir_, {r1_, r2_}, fet_] := Scope[
  dtheta1 = r1 / r2;
  dtheta2 = Pi/2 * (1 - dtheta1/2.5);
  outerArc = ScaleRotateTranslateVector[r2, dir, pos - dir * r2, arcPoints[dtheta1]];
  innerArc = ScaleRotateTranslateVector[r1, -dir, pos, arcPoints[dtheta2]];
  applyStyle[fet] @ halfThing[
    makePolygon @ Join[outerArc, innerArc],
    Construct[LineBox, innerArc]
  ]
];

makeHalfDiamond[pos_, {x_, y_}, fet_] := applyStyle[fet] @ makeHalfPolygon @ TranslateVector[pos] @ {{-y, x}, {x, y}, {y, -x}};
makeHalfSquare[pos_, {x_, y_}, fet_] := applyStyle[fet] @ makeHalfPolygon @ TranslateVector[pos] @ {{-y, x}, {x - y, x + y}, {x + y, -x + y}, {y, -x}};
makeHalfTrap[pos_, {x_, y_}, fet_]  := applyStyle[fet] @ makeHalfPolygon @ TranslateVector[pos] @ {(*NE*){-y, x}, (*SE*){x - y*0.6, x + y}, (*SW*){x + y*0.6, -x + y}, (*NW*){y, -x}};

makeDisk[pos_, r_, fet_] := applyStyle[fet] @ DiskBox[pos, r];
makePoint[pos_, r_, fet_] := StyleBox[PointBox[pos], PointSize[2 * r / $var[$W]], F @ fet];

(**************************************************************************************************)


(**************************************************************************************************)

General::badLabelPosition = "Bad label position ``."
makeLabel[label_, coords_, side_, spacing_, style_] := Scope[
  offset = -Lookup[$SideToCoords, side, Msg::badLabelPosition[side]] * (1 + spacing);
  makeDelayed @ Text[label, coords, offset, BaseStyle -> style]
]

(**************************************************************************************************)

$defaultNodeSize = {1, 1};
$boxSizeP = $NumberP | Auto | Inherited;
$intOrSpanP = _Int | _Span;

toNodeBoxInterior = Case[
  sz:$boxSizeP               := % @ {sz, sz};
  {w:$boxSizeP, h:$boxSizeP} := {
    {SubAuto[w, $isAutomaticSize[subPath[$W]] = True; F @ $defaultNodeSize],
     SubAuto[h, $isAutomaticSize[subPath[$H]] = True; L @ $defaultNodeSize]},
    None
  };
  rules:{({$intOrSpanP, $intOrSpanP} -> _)..} := {
    {subPath[$W], subPath[$H]},
    gridLayout[rules, {FrameMargins -> .75}]
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