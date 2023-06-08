PublicVariable[$KeyWireStyle, $ValueWireStyle, $BubbleWireStyle, $BundleWireStyle, $RainbowBundleWireStyle]

$KeyWireStyle = $Purple;
$ValueWireStyle = $Gray;
$BubbleWireStyle = $DarkBlue;
$RainbowBundleWireStyle = $LightGray;
$BundleWireStyle = $Purple;

(**************************************************************************************************)

$defaultNodeStyle = Sequence[
  PortSpacing -> Automatic,
  Background -> White,
  PortSize -> 0.16,
  FrameThickness -> 2,
  FrameMargins -> 0.75,
  FrameColor -> $LightGray
];

$epipro = Epilog;

(**************************************************************************************************)

toTopBottom[Rule[i_, o_]] := {Top -> i, Bottom -> o};
toTopBottom[i_]           := {Top -> i, Bottom -> 1};

(**************************************************************************************************)

PublicFunction[ClassicalRelationNode]

$relationNodeStyle = Sequence[
  PortShape -> "Disk",
  PortSize -> 0.1,
  PortColor -> $KeyWireStyle,
  FrameThickness -> 3,
  $defaultNodeStyle
]

ClassicalRelationNode[name_, n_, r_, opts___Rule] :=
  NodeDisk[r, opts, NodeLabel -> name, NodeAlias -> name, NodePorts -> n, $relationNodeStyle];

(**************************************************************************************************)

$arrayNodeStyle = Sequence[
  PortShape -> {In -> "InnerDisk", Out -> "OuterDiamond"},
  $defaultNodeStyle
];

PublicFunction[ClassicalArrayNode]

$classicalArrayNodeStyle = Sequence[
  PortColor -> {In -> $KeyWireStyle, Out -> $ValueWireStyle},
  $arrayNodeStyle
]

ClassicalArrayNode[name_, n_, interior:Except[_Rule]:Automatic, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name, NodePorts -> toTopBottom[n], $classicalArrayNodeStyle];

(**************************************************************************************************)

PublicFunction[MultisetNode]

$multisetNodeStyle = Sequence[
  RoundingRadius -> 0.2,
  FrameThickness -> 3,
  PortShape -> None,
  Background -> GrayLevel[.97],
  PortPositions -> "MatchInterior",
  $classicalArrayNodeStyle
];

MultisetNode[name_, n_, interior_, opts___Rule] := MultisetNode[name, n, interior, {}, opts];
MultisetNode[name_, n_, interior_, epilog_List, opts___Rule] :=
  NodeBox[interior, opts, NodeAlias -> name, NodePorts -> toTopBottom[n], $epipro -> applyMultisetFixups[epilog], $multisetNodeStyle];

applyMultisetFixups[e_] := e /. {
  (* since multiset nodes have no port, we must have them reach the frame *)
  CircuitCurve[p:{NodeInPort[_], _}, opts___] :> CircuitCurve[p, SetbackDistance -> 0]
};

(**************************************************************************************************)

PublicFunction[AggregationNode, RainbowAggregationNode]

AggregationNode[name_, n_ -> agg_, rest___] := MultisetNode[
  name, n, rest, PortShape -> {In -> "Disk", Out -> Labeled["OuterDiamond", Style[agg, FontSize -> 14], TopRight]}
];

RainbowAggregationNode[args___] := AggregationNode[args, PortColor -> {In -> "Medium", Out -> $ValueWireStyle}];

(**************************************************************************************************)

PublicFunction[RainbowArrayNode]

$rainbowArrayNodeStyle = Sequence[
  PortColor -> {In -> "Medium", Out -> $ValueWireStyle},
  $arrayNodeStyle
];

RainbowArrayNode[name_, cols_, interior:Except[_Rule]:Automatic, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name, NodePorts -> toTopBottom[cols], $rainbowArrayNodeStyle];

PublicFunction[RainbowTensorNode]

$rainbowTensorNodeStyle = Sequence[
  PortShape -> "InnerDisk",
  PortColor -> "Medium",
  FrameThickness -> 3,
  $defaultNodeStyle
];

RainbowTensorNode[name_, n_, r_, opts___Rule] :=
  NodeDisk[r, opts, NodeLabel -> name, NodeAlias -> name, NodePorts -> n, $rainbowTensorNodeStyle];

(**************************************************************************************************)

$derivedStyle = Sequence[
  FrameColor -> $DarkGray,
  FrameThickness -> 2,
  PortPositions -> "MatchInterior",
  PortSpacing -> .8
];

PublicFunction[DerivedArrayNode]

$derivedArrayNodeStyle = Sequence[$derivedStyle, $classicalArrayNodeStyle];

DerivedArrayNode[n_, interior_, epilog_List, opts___Rule] := DerivedArrayNode[n, interior, $epipro -> epilog, opts];
DerivedArrayNode[n_, interior_, opts___Rule] := NodeBox[interior, opts, NodeAlias -> "G", NodePorts -> toTopBottom[n], $derivedArrayNodeStyle];

PublicFunction[DerivedRainbowArrayNode]

$derivedRainbowArrayNodeStyle = Sequence[$derivedStyle, $rainbowArrayNodeStyle];

DerivedRainbowArrayNode[n_, interior_, epilog_List, opts___Rule] := DerivedRainbowArrayNode[n, interior, $epipro -> epilog, opts];
DerivedRainbowArrayNode[n_, interior_, opts___Rule] := NodeBox[interior, opts, NodeAlias -> "G", NodePorts -> toTopBottom[n], $derivedRainbowArrayNodeStyle];

PublicFunction[DerivedRainbowTensorNode]

DerivedRainbowTensorNode[args___] := DerivedRainbowArrayNode[args, RoundingRadius -> .5, PortPositions -> {Top -> "MatchOut"}, FrameThickness -> 3];

(**************************************************************************************************)

$bubbleNodeStyle = Sequence[
  RoundingRadius -> .5,
  FrameThickness -> 3,
  FrameColor -> $BubbleWireStyle,
  Background -> RGBColor[.96, .98, 1]
]

PublicFunction[BubbleValueNode]

$bubbleValueNodeStyle = Sequence[
  $bubbleNodeStyle,
  $classicalArrayNodeStyle
]

BubbleValueNode[name_, n_, interior:Except[_Rule]:Automatic, opts___Rule] :=
  NodeBox[interior, opts, NodeAlias -> name, NodePorts -> toTopBottom[n], $bubbleValueNodeStyle];

PublicFunction[RainbowBubbleValueNode]

$rainbowBubbleValueNodeStyle = Sequence[
  $bubbleNodeStyle,
  $rainbowArrayNodeStyle
]

RainbowBubbleValueNode[name_, n_, interior:Except[_Rule]:Automatic, opts___Rule] :=
  NodeBox[interior, opts, NodeAlias -> name, NodePorts -> toTopBottom[n], $rainbowBubbleValueNodeStyle];

(**************************************************************************************************)

PublicFunction[KeyPortSkeleton]

$keyPortSkeletonStyle = Sequence[
  PortShape -> "Disk",
  PortColor -> $KeyWireStyle,
  PortPositions -> {Out -> "MatchOut"},
  $defaultNodeStyle
]

KeyPortSkeleton[args___] := PortSkeleton[args, FilterOptions @ $keyPortSkeletonStyle];

PublicFunction[RainbowKeyPortSkeleton]

RainbowKeyPortSkeleton[args___] := PortSkeleton[args, PortColor -> "Medium", FilterOptions @ $keyPortSkeletonStyle];

(**************************************************************************************************)

PublicFunction[KeyWire, KeyWireBundle, ValueWire, BubbleWire, RainbowWire, RainbowWireBundle]

toWirePort = Case[
  i_Integer        := $in[i];
  key_String       := %[key -> PortIndex[1]];
  key_ -> i_       := $out[key, i];
  vals_            := Map[%, vals];
  other_           := other;
];

toWirePort[vals_$] := Map[toWirePort, vals];

toWireSrcPort[e_] := toWirePort[e] /. {$in -> NodeInPort,  $out -> NodeOutPort};
toWireDstPort[e_] := toWirePort[e] /. {$in -> NodeOutPort, $out -> NodeInPort};

toCircuitCurve[e_] := ReplaceAll[e, {
    curve[a_, b_, None] :> CircuitCurve[{a, b}],
    curve[a_, b_, Automatic] :> Style[CircuitCurve[{a, b}], ToRainbowColor @ getPortColors[a, b]],
    curve[a_, b_, s_]   :> Style[CircuitCurve[{a, b}], ToRainbowColor @ s]
  }];

wireCurve[args___] :=
  wireCurve1 @@ ReplaceAll[{args}, {
    $[s_String] :> Apply[$, Characters @ s],
    $[i_Integer] :> Apply[$, Range @ i]
  }];

wireCurve1[a_, b_, s_:None] := Scope[
  c = curve[toWireSrcPort @ a, toWireDstPort @ b, s];
  If[ContainsQ[c, _$],
    i = 1; args = {};
    cFn = Construct[Function, c] /. vals_$ :> RuleCondition[
      AppendTo[args, List @@ vals];
      Slot[i++]
    ];
    c =  MapThread[cFn, args]
  ];
  applyWireFixups @ toCircuitCurve @ c
];

applyWireFixups[e_] := e /. {
  (* this ensures connections between non-input and output curves reach the inward-facing ports *)
  CircuitCurve[p:{_NodePort, _NodePort}, opts___] :> CircuitCurve[p, SetbackDistance -> 0, opts]
}

RainbowWire::noAutoColor = "Cannot choose an automatic color for wire with ports `` and ``."
getPortColors[(NodeInPort|NodeOutPort|NodePort)[___, i_Integer], _] := i;
getPortColors[_, (NodeInPort|NodeOutPort|NodePort)[___, i_Integer]] := i;
getPortColors[a_, b_] := (Message[RainbowWire::noAutoColor, a, b]; $Failed);

PublicFunction[ApplyWireOptions]

ApplyWireOptions[style___][e_] := e /. CircuitCurve[c_, opts___] :> CircuitCurve[c, style, opts];
withWireBundleStyle = ApplyWireOptions[SetbackDistance -> {{0.2, 0.05}, {0.1, 0.06}}, LineThickness -> 12];

RainbowWire[port1_, port2_, color_:Automatic]  := wireCurve[port1, port2, color];
RainbowWireBundle[port1_, port2_] := Style[wireCurve[port1, port2], $RainbowBundleWireStyle] // withWireBundleStyle;

KeyWire[port1_, port2_]           := Style[wireCurve[port1, port2], $KeyWireStyle];
ValueWire[port1_, port2_]         := Style[wireCurve[port1, port2], $ValueWireStyle];
BubbleWire[port1_, port2_]        := Style[wireCurve[port1, port2], $BubbleWireStyle] // ApplyWireOptions[SplitPosition -> 1, BendStyle -> "Arc"];
(* KeyWireBundle[port1_, port2_, off_:{1, 0}] := Style[Translate[wireCurve[port1, port2], Outer[Times, {-1, 0, 1}*0.1, off]], $KeyWireStyle, AbsoluteThickness[2]]; *)

KeyWireBundle[port1_, port2_] := Style[wireCurve[port1, port2], $BundleWireStyle] // withWireBundleStyle;

(**************************************************************************************************)

$functionNodeStyle = Sequence[
  PortPositions -> {In -> "MatchIn"},
  $defaultNodeStyle
];

PublicFunction[KeyFunctionNode]

$keyFunctionNodeStyle = Sequence[
  PortShape -> {In -> "InnerDisk", Out -> "OuterDisk"},
  PortColor -> $KeyWireStyle,
  $functionNodeStyle
]

KeyFunctionNode[name_, n_, interior:Except[_Rule]:Automatic, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name, NodePorts -> toTopBottom[n], $keyFunctionNodeStyle];


PublicFunction[ScalarFunctionNode]

$scalarFunctionNodeStyle = Sequence[
  PortShape -> {In -> "InnerDiamond", Out -> "OuterDiamond"},
  PortColor -> $ValueWireStyle,
  $functionNodeStyle
]

ScalarFunctionNode[name_, n_, interior:Except[_Rule]:Automatic, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name, NodePorts -> toTopBottom[n], $scalarFunctionNodeStyle];

PublicFunction[RainbowScalarFunctionNode]

$rainbowFunctionNodeStyle = Sequence[
  PortShape -> {In -> "InnerDiamond", Out -> "OuterDiamond"},
  PortColor -> {In -> "Medium", Out -> $ValueWireStyle},
  $functionNodeStyle
]

RainbowScalarFunctionNode[name_, n_, interior:Except[_Rule]:Automatic, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name, NodePorts -> toTopBottom[n], $rainbowFunctionNodeStyle];

(**************************************************************************************************)

PublicFunction[SumNode]

SumNode[] := ScalarFunctionNode["sum", 1, {1.5, 1}]

(**************************************************************************************************)

PublicFunction[ArrayCircuitGraphics]

ArrayCircuitGraphics[nodes_, opts___Rule] := ScaleGraphics[
  NodeComplex[nodes, PrologStyle -> {AbsoluteThickness[3]}, EpilogStyle -> {AbsoluteThickness[3]}],
  ImagePadding -> StandardizePadding @ Lookup[{opts}, ImagePadding, {All -> 1, Bottom -> 6}],
  GraphicsScale -> 40, BaseStyle -> {FontFamily -> "Fira Code"}
];