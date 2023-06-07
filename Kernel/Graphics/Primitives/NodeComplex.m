$LR = LeftRight;
$TB = TopBottom;
$T = Top;
$B = Bottom;
$L = Left;
$R = Right;
$H = "H";
$W = "W";

(**************************************************************************************************)

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

nodeComplexBoxes[NodeComplex[nodes_, opts___Rule]] := Scope @ CatchMessage[NodeComplex,
  $path = $var["Root"]; $eqs = {}; $nodeAliases = <||>; $varAliases = {}; $isAutomaticSize = <||>; $uniqueMeanInd = 0; $portPositionDefaults = {}; $meanConnectionEqs = {};
  UnpackOptionsAs[NodeComplex, {opts}, epilog, prolog, background, epilogStyle, prologStyle];
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
    result //= ReplaceAll[$fromNC[v_$var, nc_] :> RuleCondition[complainUnresolved[result, v, nc]; {0, 0}]];
(*     Message[NodeComplex::available];
    Print @ Row[Sort @ Keys @ solutions, ", "];
 *)
  ];
  result //= ReplaceAllOperator[
    $fromNC[a_, _]             :> a,
    ListPart                   -> Part,
    AbsoluteOffset[off_][pos_] :> RuleCondition[Threaded[off] + pos],
    $delayedBoxes[e_]          :> RuleCondition[ToGraphicsBoxes @ e]
  ];
  result
];

DefineStandardTraditionalForm[
  $var[args___] :> ToBoxes @ Style[Row @ Riffle[{args}, ":"], FontFamily -> "Fira Code", FontColor -> $Blue]
]

(**************************************************************************************************)

NodeComplex::unresolvedVar = "Variable `` in `` that compiles to `` was not present in solution. Equations defining it are printed below, followed by those mentioning it.";
NodeComplex::matchingVars = "Vars similar to `` are ``.";
NodeComplex::matchingVars = "Vars similar to `` are ``.";
NodeComplex::meanConnectionEquations = "Mean connection equations printed below.";

complainUnresolved[result_, var_, nodeCoord_] := Scope[
  context = Extract[result, SafeDrop[First @ Position[result, var], -3]] /. ($fromNC[_, n_] :> n) /. (var|nodeCoord -> Style["XXX", Black, Bold]);
  Message[NodeComplex::unresolvedVar, nodeCoord, context, var];
  printEqns @ Select[eqs, First[#] === var&];
  printEqns @ Select[eqs, ContainsQ[Last @ #, Verbatim @ var]&];
  If[ListQ[$meanConnectionEqs],
    Message[NodeComplex::meanConnectionEquations];
    printEqns @ $meanConnectionEqs;
    $meanConnectionEqs ^= Null;
  ];
  pattern = var;
  If[MatchQ[Last @ var, _String[_]], Part[pattern, -1, 1] = _];
  Do[
    matchingVars = Cases[vars, Append[___] @ pattern];
    If[matchingVars =!= {}, Message[NodeComplex::matchingVars, var, matchingVars]; Break[]];
    pattern //= Most;
  ,
    {n, Length[var] - 1}
  ];
];

(**************************************************************************************************)

meanConnectionEquations[eqns_, boxes_] := Scope[
  meanVars = DeepUniqueCases[{eqns, boxes}, $var[___, "InMean" | "OutMean"]];
  If[meanVars === {}, Return[{}]];
  iConns = oConns = <||>;
  DeepCases[
    boxes /. $fromNC[v_, _] :> v,
    CircuitCurve[{a_$var, b_$var}, ___] :> (
      KeyAppendTo[oConns, a, b];
      KeyAppendTo[iConns, b, a];
    )
  ];
  meanEqs = Map[# -> meanVarRHS[Last @ #, Most @ #]&, meanVars];
  varP = Alternatives @@ vars;
  meanEqDepGraph = Graph @ Flatten @ Cases[meanEqs, Rule[a_, b_] :> Thread[Most[a] -> DeepCases[b, _$var]]];
  meanEqCycles = FindCycle[meanEqDepGraph, 2, All];
  (* this occurs when two ports involve eachother in their means. here the downstream port gets its default position to break the cycle *)
  Scan[
    cycle |-> AppendTo[meanEqs, depVar = Part[cycle, 1, 1]; depVar -> Lookup[$portPositionDefaults, depVar]],
    meanEqCycles
  ];
  $meanConnectionEqs ^= meanEqs
];

meanVarRHS[type_, var_] := Mean @ Lookup[
  If[type === "InMean", iConns, oConns],
  var, List @ Lookup[$portPositionDefaults, var, {0,0}]
];

(**************************************************************************************************)

PublicHead[NodeCenter, NodeSide, NodePort, NodeInPort, NodeOutPort, NodeCorner, PortIndex]
PublicHead[LeftRight, TopBottom]

resolveNodeCoordinatesAndAliases[expr_] := Scope[
  expr //= expandNodeCoordinates;
  expr //= evaluateScopes;
  expr //= applyAliases;
  expandVars[$var[], expr]
];

applyAliases[e_] := ReplaceAllOperator[$nodeAliases, $varAliases /. $nodeAliases] @ e;

computeNodeAliases[aliasAssoc_] := ReverseSort @ KeyValueMap[toAliasRule, aliasAssoc];
toAliasRule[name_String, var_$var] := Append[var, l___] :> $var[name, l];

$nodeCoordinateP = _NodePort | _NodeInPort | _NodeOutPort | _NodeCenter | _NodeCorner | _NodeSide;
expandNodeCoordinates[e_] := ReplaceAll[e, np:$nodeCoordinateP :> RuleCondition[$fromNC[procNodeCoordinate @ np, np]]];

procNodeCoordinate = Case[
  NodeInPort[p_:Null, port_]      := makeVar[p, $PortIn[port]];
  NodeOutPort[p_:Null, port_]     := makeVar[p, $PortOut[port]];
  NodeCorner[x_, y_]              := {ListPart[% @ x, 1], ListPart[% @ y, 2]};
  NodeCenter[p_:Null]             := {makeVar[p, $LR], makeVar[p, $TB]};
  NodeSide[p_:Null, side_Symbol]  := makeVar[p, #]& /@ $sideToCoords[side];
  NodePort[p_:Null, port_]        := makeVar[p, $Port[port]];
];

makeVar[Null, p_] := $relvar[p];
makeVar[s_, p_] := $var[s, p];

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

(**************************************************************************************************)

evaluateScopes[expr_] := ReplaceAll[expr, d_$scoped :> RuleCondition @ evaluateScope[d]];

evaluateScope[$scoped[path_$var, expr_]] := expandVars[path, evaluateScopes @ expr];

expandVars[path_, e_] := ReplaceAll[e, v_$relvar :> RuleCondition @ toAbsVar[v, path]];

toAbsVar[v_$relvar, b_$var] := Join[b, $var @@ v];

toVar[".", args_] := $relvar[args];
toVar[args_] := $var[args];

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

createNodeAlias := Case[
  None       := Null;
  i_Integer  := % @ TextString[i];
  str_String := AssociateTo[$nodeAliases, str -> $path];
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

nodeEpilog[node_, epilog_] := {processNode @ node, makeDelayed @ epilog};
nodeProlog[node_, prolog_] := {makeDelayed @ prolog, processNode @ node};

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
  createNodeAlias[nodeAlias];
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
  PortSpacing       -> 0.2,
  PortSize          -> 0.05,
  PortShape         -> "Disk",
  PortEdgeColor     -> Automatic,
  PortColor         -> Black,
  PortEdgeThickness -> 1,
  PortPositions     -> Automatic,
  NodeAlias         -> None
}

portSkeletonBox[w_, nodePorts_, opts___Rule] := Scope[
  UnpackOptionsAs[PortSkeleton, {opts}, nodeAlias];
  createNodeAlias[nodeAlias];
  $w = w;
  {$portData, $defaultPortData} = OptionValueAssociations[PortSkeleton, replaceIO @ {opts}, $portOptionKeys];
  portPrimitives = Flatten @ processNodeBoxPorts[Bottom -> nodePorts];
  addFrameEqns[$w, 0];
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
PublicOption[PortSpacing, PortPositions, PortSize, PortShape, PortEdgeColor, PortColor, PortEdgeThickness, HiddenPorts, PortPositions]
PublicVariable[$NodeComplexLabelStyle, $PortLabelStyle]

SetInitialValue[$NodeComplexLabelStyle, {FontSize  -> 16, FontFamily -> "Fira Code", FontWeight -> "Bold"}];
SetInitialValue[$PortLabelStyle, {FontSize -> 11, FontFamily -> "Fira Code"}];

Options[NodeDisk] = {
  Background        -> None,
  Epilog            -> None,
  FrameColor        -> $Gray,
  FrameLabel        -> None,
  FrameLabelSpacing -> 0.1,
  FrameLabelStyle   :> $NodeComplexLabelStyle,
  FrameMargins      -> 0.1,
  FrameThickness    -> 1,
  NodeAlias         -> None,
  NodeLabel         -> None,
  NodeLabelColor    -> None,
  NodeLabelStyle    :> $NodeComplexLabelStyle,
  NodePorts         -> None,
  PortLabelStyle    :> $PortLabelStyle,
  PortEdgeColor     -> Automatic,
  PortEdgeThickness -> 1,
  PortColor         -> Black,
  PortPositions     -> Automatic,
  PortShape         -> "Disk",
  PortSize          -> 0.05,
  PortSpacing       -> Automatic,
  Prolog            -> None
}

Options[NodeBox] = JoinOptions[
  NodeDisk,
  RoundingRadius    -> None
]

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

replaceIO = ReplaceAll[{In -> Top, Out -> Bottom}];

DefineLiteralMacro[OptionValueAssociations,
  OptionValueAssociations[head_, opts_, keys_] := {
    AssociationThread[keys, OptionValue[head, opts, keys]],
    AssociationThread[keys, OptionValue[head, keys]]
  }
];

nodeBox[head_, spec_, opts___Rule] := Scope @ CatchMessage[head,
  UnpackOptionsAs[head, {opts},
    background, nodePorts, nodeLabel, nodeLabelStyle, nodeLabelColor,
    frameMargins, epilog, prolog, nodeAlias,
    portLabelStyle,
    (* portSpacing, portSize, portShape, portEdgeColor, portColor, portEdgeThickness, portPositions, *)
    frameColor, background, frameThickness,
    frameLabel, frameLabelSpacing, frameLabelStyle
  ];

  {$portData, $defaultPortData} = OptionValueAssociations[head, replaceIO @ {opts}, $portOptionKeys];
  createNodeAlias[nodeAlias];

  {{lmargin, rmargin}, {bmargin, tmargin}} = StandardizePadding[frameMargins];
  {{$w, $h}, interior} = toNodeBoxInterior @ spec;

  portPrimitives = If[head === NodeBox, processNodeBoxPorts, processNodeDiskPorts] @ replaceIO @ nodePorts;
  portPrimitives //= Flatten;
  If[portPrimitives === {}, portPrimitives = Nothing];

  addFrameEqns[$w, $h];

  fet = {background, frameColor, frameThickness};
  boxPrimitives = Switch[head,
    NodeBox,
      UnpackOptionsAs[NodeBox, {opts}, roundingRadius];
      makeRoundedRect[subPath /@ {$L, $B}, subPath /@ {$R, $T}, roundingRadius, fet]
    ,
    NodeDisk,
      addEqns[subPath["Radius"], Max[$w, $h]/2];
      makeDisk[currentCenter[], subPath["Radius"], fet]
  ];

  labelPrimitives = If[nodeLabel === None, Nothing,
    ToGraphicsBoxes @ Text[
      StyleOperator[ToRainbowColor @ nodeLabelColor] @ StyleOperator[nodeLabelStyle] @ nodeLabel,
      currentCenter[]
    ]];

  frameLabelPrimitives = makeFrameLabel[frameLabel];

  interiorPrimitives = If[interior === None, Nothing, interior];

  List[
    boxPrimitives,
    $prologFn @ makeDelayed @ prolog,
    portPrimitives,
    interiorPrimitives,
    labelPrimitives,
    frameLabelPrimitives,
    $epilogFn @ makeDelayed @ epilog
  ]
];

attachProEpi[None, None][prims_] := prims;
attachProEpi[pro_, epi_][prims_] := {$prologFn @ makeDelayed @ pro, prims, $epilogFn @ makeDelayed @ epi};

makeDelayed[None | {}] := Nothing;
makeDelayed[prims_] := $delayedBoxes[$scoped[$path, prims]];

(**************************************************************************************************)

General::badNodeFrameLabel = "`` is not a valid spec for FrameLabel.";
makeFrameLabel = Case[
  None | {}  := Nothing;
  other_     := %[Top -> other];
  list_List  := Map[%, list];
  side:$SidePattern -> label_ := Scope[
    coords = Lookup[$sideToCoords, side, Failure["badNodeFrameLabel", side -> label]];
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
  n_Integer           := Range @ n;
  s_String            := {s};
  l_List              := l;
  spec_               := ThrowMessage["badNodePortSpec", spec];
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
  spec_                                 := ThrowMessage["badNodePortSpec", spec];
  side_ -> Style[spec_, rules___Rule]   := Internal`InheritedBlock[{$portData},
    AssociateTo[$portData, portDataRules[rules]];
    %[side -> spec]
  ];
  (side:Left|Right|Top|Bottom) -> spec_ := Scope[
    ports = procPortSpec @ spec;
    portPaths = makePortPaths[sideToPortKind @ side, ports];
    n = Length @ ports;
    $sidePortData = sideSpecializedPortData[side];
    portSpacing = ReplaceAutomatic[$sidePortData["PortSpacing"], $w / (n + .333)];
    offsets = makePortOffsets[n, portSpacing];
    portCoords = Switch[side,
      Center, ConstantArray[{subPath[$L], subPath[$TB]}, n],
      Left,   Threaded[{subPath[$L], center = subPath[$TB]}] - Thread[{0, offsets}],
      Right,  Threaded[{subPath[$R], center = subPath[$TB]}] - Thread[{0, offsets}],
      Bottom, Threaded[{center = subPath[$LR], subPath[$B]}] + Thread[{offsets, 0}],
      Top,    Threaded[{center = subPath[$LR], subPath[$T]}] + Thread[{offsets, 0}]
    ];
    isVert = MatchQ[side, Top|Bottom]; portXY = If[isVert, 1, 2];
    portPosSpec = Replace[$sidePortData @ "PortPositions", {
      "MatchInterior" :> If[side === Top, "MatchOut", "MatchIn"],
      "MatchExterior" :> If[side === Top, "MatchIn", "MatchOut"]
    }];
    portCoordOverrides = Switch[
      portPosSpec,
      Automatic | None,       Null,
      "MatchIn" | "MatchOut", toPortPositions @ ConstantArray[portPosSpec, n],
      AbsoluteOffset[_],      Part[portCoords, All, portXY] += First[portPosSpec]; Null,
      _List,                  toPortPositions @ portPosSpec,
      _,                      Message[NodeBox::badPortPositions, portPositions]; Null
    ];
    If[ListQ[portCoordOverrides] && LengthEqualOrMessage[NodeBox::badPortPositionLen, portCoordOverrides, portCoords],
      (* fill in the relevant ordinate of ports from provided port positions *)
      $portPositionDefaults ^= {$portPositionDefaults, RuleThread[portPaths, portCoords]};
      Part[portCoords, All, If[isVert, 1, 2]] = portCoordOverrides;
    ];
    addEqns @ RuleThread[portPaths, portCoords];
    centerVec = Switch[side, Center, {0, 0}, Left, {1, 0}, Right, {-1, 0}, Top, {0, -1}, Bottom, {0, 1}];
    makePorts @ <|"coords" -> portCoords, "dirs" -> ConstantArray[centerVec, n], "ports" -> ports, $sidePortData|>
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
  Automatic              := $last + portSpacing;
  "MatchIn"              := ListPart[Append["InMean"] @  Part[portPaths, $i], portXY];
  "MatchOut"             := ListPart[Append["OutMean"] @ Part[portPaths, $i], portXY];
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
  spec:{(($SidePattern | $NumberP) -> _)..} := Scope[
    {angles, ports} = KeysValues @ spec;
    angles = angles /. $sideToAngle;
    makeCirclePorts[ports, angles]
  ];
  (side:$SidePattern -> spec_List) := Scope[
    initAng = Lookup[$sideToAngle, side];
    n = Length[spec];
    angles = initAng + Range[0,n-1] * ReplaceAutomatic[$portData["PortSpacing"], 1 / n];
    % @ RuleThread[N @ angles, spec]
  ];
  n_Integer        := %[Top -> Range[n]];
  list:{__Integer} := %[Top -> list];
  None             := {};
  spec_            := ThrowMessage["badNodePortSpec", spec];
];

makeCirclePorts[ports_, angles_] := Scope[
  dirs = AnglePair[-(angles - 0.25)];
  portCoords = Threaded[currentCenter[]] + dirs * subPath["Radius"];
  portPaths = makePortPaths[$Port, ports];
  addEqns @ RuleThread[portPaths, portCoords];
  makePorts @ <|"coords" -> portCoords, "dirs" -> -dirs, "ports" -> ports, $portData|>
]

currentCenter[] := subPath /@ {$LR, $TB};

(**************************************************************************************************)

General::badNodePortShape = "PortShape -> `` is not one of ``."

procPortListSpec[spec_, n_, prev_] := ConstantArray[spec, n];
procPortListSpec[spec_List, n_, prev_] := PadRight[spec, n, prev];
procPortListSpec[rules:{__Rule}, n_, prev_] := VectorReplace[Range[n], Append[_ -> prev] @ rules];
procPortListSpec[{rules__Rule, All -> def_}, n_, _] := procPortListSpec[{rules}, n, def];

makePorts[data_Association] := Scope[
  n = Length @ data["ports"];
  AssociationMapThread[
    makeSinglePort[#ports, #]&,
    MapThread[
      procPortListSpec[#1, n, #2]&,
      KeyDrop[KeyUnion[{data, $defaultPortData}], {"PortPositions", "PortSpacing"}]
    ]
  ]
];

$shapeP = _String | _Labeled | _Placed;
makeSinglePort = Case[
  Sequence[_Invisible, _]                        := {};
  Sequence[Style[p_, s:$shapeP, opts___], data_] := %[Style[p, PortShape -> s, opts], data];
  Sequence[Style[p_, opts___], data_]            := $makeSinglePortFn @ Association[data, portDataRules @ opts];
  Sequence[_, data_]                             := $makeSinglePortFn @ data;
];

$makeSinglePortFn = Function[
  applyGraphicsOffset[shapeOffset[#PortShape] * #dirs] @ shapeToFn[#PortShape] @
  Append[#, "FET" -> toFET[#ports, #PortColor, #PortEdgeColor, #PortEdgeThickness]]
];

applyGraphicsOffset[{0|0., 0|0.}][e_] := e;
applyGraphicsOffset[offset_][e_] := Construct[GeometricTransformationBox, e, AbsoluteThicknessToDimension[#, 40]& /@ offset];

toFET[Style[p_, c:($ColorPattern | _Integer)], f_, e_, t_] := toFET[p, c, e, t];
toFET[i_Integer, f_, e_, t_]                               := {ToRainbowColor[f, i], ToRainbowColor[e, i], t};
toFET[_, f_, e_, t_]                                       := {ToRainbowColor @ f, ToRainbowColor @ e, t};

shapeOffset = Case[
  _                   := 0;
  s_String            := Which[
    StringStartsQ[s, "Inner"], 1,
    StringStartsQ[s, "Outer"], -1,
    True, 0
  ];
  Labeled[_, _, pos_] := % @ Placed[Null, pos];
  Placed[_, Below|BottomLeft|Bottom|BottomRight]    := 1;
  Placed[_, Above|TopLeft|Top|TopRight]             := -1;
]

shapeToFn := Case[
  shape_ := Lookup[$shapeToFn, shape, ThrowMessage["badNodePortShape", shape, Keys @ $shapeToFn]];
  Placed[label_, pos_Symbol] := Function[
    makeLabel[label, #coords, pos, 0.2, portLabelStyle]
  ];
  Labeled[shape_, label_, pos_] := ApplyThrough[{% @ shape, % @ Placed[label, pos]}]
];

$shapeToFn = <|
  "Point"        -> Function[makePoint[#coords, #PortSize, #FET]],
  "Disk"         -> Function[makeDisk[#coords, #PortSize, #FET]],
  "OuterDisk"    -> Function[makeHalfDisk[#coords, -#dirs, #PortSize, #FET]],
  "InnerDisk"    -> Function[makeHalfDisk[#coords, #dirs, #PortSize, #FET]],
  "Square"       -> Function[makeSquare[#coords, #PortSize, #FET]],
  "OuterDiamond" -> Function[makeHalfDiamond[#coords, -#dirs * #PortSize, #FET]],
  "InnerDiamond" -> Function[makeHalfDiamond[#coords, #dirs * #PortSize, #FET]],
  "OuterSquare"  -> Function[makeHalfSquare[#coords, -#dirs * #PortSize, #FET]],
  "InnerSquare"  -> Function[makeHalfSquare[#coords, #dirs * #PortSize, #FET]],
  None           -> ({}&)
|>;

(**************************************************************************************************)

applyStyle[PolygonBox[coords_], {None,  edge_, thickness_}] := StyleBox[LineBox[coords], edge, AbsoluteThickness @ thickness];
applyStyle[DiskBox[args__],     {None,  edge_, thickness_}] := StyleBox[CircleBox[args], edge, AbsoluteThickness @ thickness];
applyStyle[other_,              {face_, edge_, thickness_}] := StyleBox[other, FaceEdgeForm[face, edge, thickness]];

(**************************************************************************************************)

makeRoundedRect[c1_, c2_, r_, fet_] := makeRect[c1, c2, fet, RoundingRadius -> r];
makeRoundedRect[c1_, c2_, None|0|0., fet_] := makeRect[c1, c2, fet];

makeRect[c1_, c2_, fet_, opts___Rule] := applyStyle[RectangleBox[c1, c2, opts], fet];
makeSquare[p_, r_, fet_] := makeRect[p - r, p + r, fet];

halfCirclePoints[ang_] := halfCirclePoints[ang] = DiscretizeCurve[Circle[{0,0}, 1, {ang - Pi/2, ang + Pi/2}]];
makeHalfDisk[pos_, dir_, r_, fet_] := applyStyle[Construct[PolygonBox, Threaded[pos] + r * halfCirclePoints[ArcTan2 @@ dir]], fet];

makeHalfDiamond[pos_, {x_, y_}, fet_] := applyStyle[Construct[PolygonBox, Threaded[pos] + {{-y, x}, {x, y}, {y, -x}}], fet];
makeHalfSquare[pos_, {x_, y_}, fet_]  := applyStyle[Construct[PolygonBox, Threaded[pos] + {{-y, x}, {x - y, x + y}, {x + y, -x + y}, {y, -x}}], fet];

makeDisk[pos_, r_, fet_] := applyStyle[DiskBox[pos, r], fet];
makePoint[pos_, r_, fet_] := StyleBox[PointBox[pos], PointSize[2 * r / $var[$W]], First @ fet];

(**************************************************************************************************)

General::badLabelPosition = "Bad label position ``."
makeLabel[label_, coords_, side_, spacing_, style_] := Scope[
  offset = Lookup[$sideToLabelOffset, side, ThrowMessage["badLabelPosition", side]] * (1 + spacing);
  makeDelayed @ Text[label, coords, offset, BaseStyle -> style]
]

$sideToLabelOffset = <|
        Left -> { 1,  0},       Right -> {-1,  0},
     TopLeft -> { 1, -1},    TopRight -> {-1, -1},
  BottomLeft -> { 1,  1}, BottomRight -> {-1,  1},
      Bottom -> { 0,  1},         Top -> { 0, -1},
       Below -> { 0,  1},       Above -> { 0, -1},
      Center -> { 0,  0}
|>;

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
