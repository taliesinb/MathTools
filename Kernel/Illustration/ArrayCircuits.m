PublicVariable[$KeyWireStyle, $ValueWireStyle, $RainbowValueWireStyle, $BubbleWireStyle, $ClassicalArrayNodeStyle, $ScalarFunctionNodeStyle]

$KeyWireStyle = $Purple;
$ValueWireStyle = $Orange;
$RainbowValueWireStyle = $Gray;
$BubbleWireStyle = $Blue;

$defaultNodeStyle = Sequence[
  PortSpacing -> .75,
  PortSize -> 0.16,
  FrameThickness -> 3,
  FrameMargins -> 0.75
];

$valuePortStyle = Sequence[
  PortFaceColor -> $ValueWireStyle,
  PortEdgeColor -> Automatic
];

$valueInPortStyle = Sequence[$valuePortStyle, PortShape -> "InnerDiamond"];
$valueOutPortStyle = Sequence[$valuePortStyle, PortShape -> "OuterDiamond"];

$rainbowValuePortStyle = Sequence[
  PortFaceColor -> $RainbowValueWireStyle,
  PortEdgeColor -> Automatic
];

$rainbowValueInPortStyle = Sequence[$rainbowValuePortStyle, PortShape -> "InnerDiamond"];
$rainbowValueOutPortStyle = Sequence[$rainbowValuePortStyle, PortShape -> "OuterDiamond"];

$keyPortStyle = Sequence[
  PortShape -> "Disk",
  PortFaceColor -> $KeyWireStyle,
  PortEdgeColor -> Automatic
];

$keyInPortStyle = Sequence[$keyPortStyle, PortShape -> "InnerDisk"];
$keyOutPortStyle = Sequence[$keyPortStyle, PortShape -> "OuterDisk"];

$bubblePortStyle = Sequence[
  PortShape -> "Disk",
  PortFaceColor -> $BubbleWireStyle,
  PortEdgeColor -> Automatic
];

$bubbleInPortStyle = Sequence[$bubblePortStyle, PortShape -> "InnerDiamond"];
$bubbleOutPortStyle = Sequence[$bubblePortStyle, PortShape -> "OuterDiamond"];

(**************************************************************************************************)

PublicFunction[KeyInPorts, KeyOutPorts, ValueInPorts, ValueOutPorts, BubbleInPorts, BubbleOutPorts]

KeyInPorts[n_, opts___] := Top -> Style[n, $keyInPortStyle];
KeyOutPorts[n_] := Bottom -> Style[n, $keyOutPortStyle];

ValueInPorts[n_] := Top -> Style[n, $valueInPortStyle];
ValueOutPorts[n_] := Bottom -> Style[n, $valueOutPortStyle];

BubbleInPorts[n_] := Top -> Style[n, $bubbleInPortStyle];
BubbleOutPorts[n_] := Bottom -> Style[n, $bubbleOutPortStyle];

(**************************************************************************************************)

PublicFunction[DerivedArrayNode, DerivedRainbowArrayNode]

DerivedArrayNode[n_, interior_, epilog_List, opts___Rule] :=
  DerivedArrayNode[n, interior, Epilog -> epilog, opts];

DerivedArrayNode[n_, interior_, opts___Rule] :=
  NodeBox[interior, opts, NodeAlias -> "G",
    NodePorts -> {
      Top -> Style[n, $keyInPortStyle],
      Bottom -> Style[1, $valueOutPortStyle]
    },
    $defaultNodeStyle, FrameColor -> $DarkGray
  ];

DerivedRainbowArrayNode[n_, interior_, epilog_List, opts___Rule] :=
  DerivedRainbowArrayNode[n, interior, Epilog -> epilog, opts];

DerivedRainbowArrayNode[n_, interior_, opts___Rule] :=
  NodeBox[interior, opts, NodeAlias -> "G",
    NodePorts -> {
      Top -> Style[n, PortShape -> "InnerDisk"],
      Bottom -> Style[1, $rainbowValueOutPortStyle]
    },
    $defaultNodeStyle, FrameColor -> $DarkGray
  ];

(**************************************************************************************************)

PublicFunction[ClassicalArrayNode, RainbowArrayNode]

$ClassicalArrayNodeStyle = $LightGray;

ClassicalArrayNode[name_, n_, opts___Rule] := ClassicalArrayNode[name, n, Automatic, opts];
ClassicalArrayNode[name_, n_, interior_, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name,
    NodePorts -> {
      Top -> Style[n, $keyInPortStyle],
      Bottom -> Style[1, $valueOutPortStyle]
    },
    $defaultNodeStyle, FrameColor -> $ClassicalArrayNodeStyle
  ];

RainbowArrayNode[name_, cols_List, opts___Rule] := RainbowArrayNode[name, cols, Automatic, opts];
RainbowArrayNode[name_, cols_List, interior_, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name,
    NodePorts -> {
      Top -> Style[cols, PortShape -> "InnerDisk"],
      Bottom -> Style[1, $rainbowValueOutPortStyle]
    },
    $defaultNodeStyle, FrameColor -> $ClassicalArrayNodeStyle
  ];

(**************************************************************************************************)

PublicFunction[BubbleValueNode]

BubbleValueNode[name_, n_, opts___Rule] := BubbleValueNode[name, n, Automatic, opts];
BubbleValueNode[name_, n_, interior_, opts___Rule] :=
  NodeBox[interior, opts, NodeAlias -> name,
    RoundingRadius -> .5,
    NodePorts -> {
      Top -> Style[n, $keyInPortStyle],
      Bottom -> Style[1, $valueOutPortStyle]
    },
    $defaultNodeStyle,
    FrameColor -> $BubbleWireStyle
  ];


(**************************************************************************************************)

PublicFunction[KeyPortSkeleton]

KeyPortSkeleton[args___] := PortSkeleton[args, $keyPortStyle,
  FilterOptions @ $defaultNodeStyle
];

(**************************************************************************************************)

PublicFunction[CircuitKeyWire, CircuitValueWire, CircuitBubbleWire, RainbowKeyWire, RainbowValueWire]

toWireInPort = Case[
  key_String       := %[key -> 1];
  key_String -> i_ := NodeOutPort[key, i];
  i_Integer        := NodeInPort["G", i];
  other_           := other;
];

toWireOutPort = Case[
  key_String       := %[key -> 1];
  key_String -> i_ := NodeInPort[key, i];
  i_Integer        := NodeOutPort["G", i];
  other_           := other;
];

wireCurve[a_, b_] := SetbackCurve[SmoothedCurve @ CircuitCurve[{toWireInPort @ a, toWireOutPort @ b}], 0.05];

RainbowKeyWire[port1_, port2_, color_]   := Style[wireCurve[port1, port2], ToRainbowColor @ color];
RainbowValueWire[port1_, port2_]         := Style[wireCurve[port1, port2], $RainbowValueWireStyle];
RainbowValueWire[port1_, port2_, color_] := Style[wireCurve[port1, port2], ToRainbowColor @ color];
CircuitKeyWire[port1_, port2_]           := Style[wireCurve[port1, port2], $KeyWireStyle];
CircuitValueWire[port1_, port2_]         := Style[wireCurve[port1, port2], $ValueWireStyle];
CircuitBubbleWire[port1_, port2_]        := Style[wireCurve[port1, port2], $BubbleWireStyle];

(**************************************************************************************************)

PublicFunction[KeyFunctionNode]

KeyFunctionNode[name_, n_, interior_, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name,
  NodePorts -> {
    Top -> Style[n, $keyInPortStyle],
    Bottom -> Style[1, $keyOutPortStyle]
  },
  $defaultNodeStyle, FrameColor -> $ScalarFunctionNodeStyle
];

(**************************************************************************************************)

PublicFunction[ScalarFunctionNode, RainbowScalarFunctionNode]

$ScalarFunctionNodeStyle = $LightGray;

ScalarFunctionNode[name_, n_, opts___Rule] := ScalarFunctionNode[name, n, Automatic, opts];
ScalarFunctionNode[name_, n_, interior_, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name,
  NodePorts -> {
    Top -> Style[n, $valueInPortStyle],
    Bottom -> Style[1, $valueOutPortStyle]
  },
  $defaultNodeStyle, FrameColor -> $ScalarFunctionNodeStyle
];

RainbowScalarFunctionNode[name_, n_, opts___Rule] := RainbowScalarFunctionNode[name, n, Automatic, opts];
RainbowScalarFunctionNode[name_, n_, interior_, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name,
  NodePorts -> {
    Top -> Style[n, $rainbowValueInPortStyle],
    Bottom -> Style[1, $rainbowValueOutPortStyle]
  },
  $defaultNodeStyle, FrameColor -> $ScalarFunctionNodeStyle
];


(**************************************************************************************************)

PublicFunction[ArrayCircuitGraphics]

ArrayCircuitGraphics[nodes_] := ScaleGraphics[
  NodeComplex[nodes, PrologStyle -> {AbsoluteThickness[3]}, EpilogStyle -> {AbsoluteThickness[3]}],
  ImagePadding -> 5,
  GraphicsScale -> 40
];