PublicVariable[$KeyWireStyle, $ValueWireStyle, $BubbleWireStyle, $BundleWireStyle, $RainbowBundleWireStyle, $BubbleBackground]

$KeyWireStyle = $Purple;
$ValueWireStyle = $Gray;
$BubbleWireStyle = $DarkBlue;
$RainbowBundleWireStyle = $LightGray;
$BundleWireStyle = $Purple;
$BubbleBackground = RGBColor[.96, .98, 1];

(**************************************************************************************************)

PublicFunction[PortStyleData]

(**************************************************************************************************)

PortStyleData["Default"] = Sequence[
  PortSize -> 0.16
];

$defaultNodeStyle = Sequence[
  PortStyleData["Default"],
  PortSpacing -> Automatic,
  Background -> White,
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

PortStyleData["Relation"] = Sequence[
  PortShape -> "Disk",
  PortColor -> $KeyWireStyle,
  PortSize -> 0.1
];

$relationNodeStyle = Sequence[
  PortStyleData["Relation"],
  FrameThickness -> 3,
  $defaultNodeStyle
]

ClassicalRelationNode[name_, n_, r_, opts___Rule] :=
  NodeDisk[r, opts, NodeLabel -> name, NodeAlias -> name, NodePorts -> n, $relationNodeStyle];

(**************************************************************************************************)

PortStyleData["Key"] = Sequence[
  PortShape -> "DownHalfDisk",
  PortColor -> $KeyWireStyle,
  PortStyleData["Default"]
]

PortStyleData["Value"] = Sequence[
  PortShape -> "DownHalfDiamond",
  PortColor -> $ValueWireStyle,
  PortStyleData["Default"]
]

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

PortStyleData["Rainbow"] = Sequence[
  PortShape -> "DownHalfDisk",
  PortColor -> "Medium",
  PortStyleData["Default"]
]

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

PortStyleData["RainbowTensor"] = Sequence[
  PortShape -> "InnerDisk",
  PortColor -> "Medium",
  PortSize -> 0.15
]

$rainbowTensorNodeStyle = Sequence[
  PortStyleData["RainbowTensor"],
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
  PortSpacing -> .8,
  FrameLabelStyle -> {FontSize  -> 15, BaseStyle -> "PreformattedCode", SingleLetterItalics -> True},
  FrameLabelSpacing -> 0.4
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

PublicFunction[DerivedBubbleFunctionNode]

DerivedBubbleFunctionNode[args___] := DerivedArrayNode[args, PortColor -> $BubbleWireStyle, PortShape -> {In -> "InnerDiamond", Out -> "OuterDiamond"}];

PublicFunction[DerivedRainbowBubbleFunctionNode]

DerivedRainbowBubbleFunctionNode[args___] := DerivedRainbowArrayNode[args, PortShape -> {In -> "InnerDiamond", Out -> "OuterDiamond"}];

(**************************************************************************************************)

$bubbleNodeStyle = Sequence[
  RoundingRadius -> .5,
  FrameThickness -> 3,
  FrameColor -> $BubbleWireStyle,
  Background -> $BubbleBackground
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

PublicFunction[RainbowBubbleTensorNode]

$rainbowBubbleTensorNodeStyle = Sequence[
  $bubbleNodeStyle,
  $rainbowTensorNodeStyle
]

RainbowBubbleTensorNode[name_, n_, r_, opts___Rule] :=
  NodeDisk[r, opts, NodeLabel -> name, NodeAlias -> name, NodePorts -> n, FilterOptions @ $rainbowBubbleTensorNodeStyle];

(**************************************************************************************************)

PublicFunction[KeyPortSkeleton]

$keyPortSkeletonStyle = Sequence[
  PortStyle["Key"],
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
  vals_$           := Map[%, vals];
  other_           := other;
];

toWireSrcPort[e_] := toWirePort[e] /. {$in -> NodeInPort,  $out -> NodeOutPort};
toWireDstPort[e_] := toWirePort[e] /. {$in -> NodeOutPort, $out -> NodeInPort};

toCircuitCurve[e_] := ReplaceAll[e, {
    curve[a_, b_, None] :> CircuitCurve[{a, b}],
    curve[a_, b_, Automatic] :> Style[CircuitCurve[{a, b}], getPortColors[a, b]],
    curve[a_, b_, s_]   :> Style[CircuitCurve[{a, b}], s]
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
  CircuitCurve[p:{NodePort[_, _], NodePort[_, _]}, opts___] :> CircuitCurve[p, SetbackDistance -> 0, opts],
  CircuitCurve[p:{_, NodeSide[_, _]}, opts___] :> CircuitCurve[p, SetbackDistance -> 0, opts],
  CircuitCurve[p:{NodeSide[_, _], _}, opts___] :> CircuitCurve[p, SetbackDistance -> 0, opts]
}

PrivateHead[$portColor]

$portColor[i_Integer] := ToRainbowColor @ i;
$portColor[Automatic] := Automatic;

RainbowWire::noAutoColor = "Cannot choose an automatic color for wire with ports `` and ``."
getPortColors[(NodeInPort|NodeOutPort|NodePort)[___, p_], _] := $portColor[p];
getPortColors[_, (NodeInPort|NodeOutPort|NodePort)[___, p_]] := $portColor[p];
getPortColors[a_, b_] := (Message[RainbowWire::noAutoColor, a, b]; $Failed);

PublicFunction[ApplyWireOptions]

ApplyWireOptions[style___][e_] := e /. CircuitCurve[c_, opts___] :> CircuitCurve[c, style, opts];
withWireBundleStyle = ApplyWireOptions[SetbackDistance -> {{0.2, 0.05}, {0.1, 0.06}}, LineThickness -> 12];

RainbowWire[port1_, port2_, color_:Automatic]  := wireCurve[port1, port2, $portColor @ color];
RainbowWireBundle[port1_, port2_] := Style[wireCurve[port1, port2], $RainbowBundleWireStyle] // withWireBundleStyle;
RainbowWireBundle[port1_, port2_, color_] := wireCurve[port1, port2, $portColor @ color] // withWireBundleStyle;

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

PublicFunction[BubbleFunctionNode]

BubbleFunctionNode[args__] :=
  ScalarFunctionNode[args, PortColor -> $BubbleWireStyle];

(**************************************************************************************************)

PublicFunction[SumNode]

SumNode[] := ScalarFunctionNode["sum", 1, {1.5, 1}]

(**************************************************************************************************)

PublicFunction[ArrayCircuitGraphics]

ArrayCircuitGraphics[nodes_, opts___Rule] := Scope[
  padding = Lookup[{opts}, ImagePadding, Automatic];
  frameLabel = LookupOption[nodes, FrameLabel, None];
  SetAutomatic[padding, 1];
  padding //= StandardizePadding;
  Part[padding, 2, 1] //= Max[#, 6]&;
  If[frameLabel =!= None,
    Part[padding, 2, 2] //= Max[#, 18]&;
    If[StringQ[frameLabel] && StringLength[frameLabel] > 10,
      Part[padding, 1, 1] //= Max[#, 10]&;
      Part[padding, 1, 2] //= Max[#, 10]&;
    ];
  ];
  complex = NodeComplex[nodes,
    PrologStyle -> {AbsoluteThickness[3]}, EpilogStyle -> {AbsoluteThickness[3]},
    NodePalette -> Lookup[{opts}, NodePalette, None]
  ];
  ScaleGraphics[
    complex,
    ImagePadding -> padding,
    GraphicsScale -> 40, BaseStyle -> {FontFamily -> "Fira Code"}
  ]
];

(**************************************************************************************************)

PublicFunction[WireLegend]

Options[WireLegend] = {
  FontSize -> 20,
  RowSpacings -> 1,
  ColumnSpacings -> 1
};

WireLegend[rules:{__Rule}, OptionsPattern[]] := Scope[
  UnpackOptions[fontSize, rowSpacings, columnSpacings];
  Grid[
    toWireLegendItem /@ rules,
    Spacings -> {rowSpacings, columnSpacings},
    BaseStyle -> {FontFamily -> "Fira Code", FontSize -> fontSize}
  ]
];

toWireLegendItem = Case[
  style_ -> label_ := {
    wireIconGraphics[toWireStyle[style]],
    label
  };
]

toWireStyle = Case[
  i_Integer := ToRainbowColor[i];
]

wireIconGraphics[style_] := ScaleGraphics[
  {AbsoluteThickness[3], style, CircuitCurve[{{0,0}, {0, .5}}]},
  Axes -> None, ImagePadding -> {Horizontal -> 2},
  GraphicsScale -> 40,
  BaselinePosition -> Scaled[0.15]
];

(* (**************************************************************************************************)

PublicFunction[DerivedArraySignatureForm]

DefineStandardTraditionalForm[
  DerivedArraySignatureForm[name_, ins_, out_] :> TBox[
    name,
    derivedArrayInBox[ins],
    shapeBox[out],
    "derivedArraySignatureForm"
  ]
];

DefineTemplateBox[DerivedArraySignatureForm, "derivedArraySignatureForm", RBox[#1, ":", #2, "\[Rule]", #3], None];

derivedArrayInBox[list_] := ParenthesesBox[CommaRowBox @@ Map[shapeFieldBox, ToList @ list]];

shapeBox[item_ | {item_}] := TBox[item, "arrayShapeForm"];
shapeBox[item_List] := TBox[RowBox @ list, "arrayShapeForm"];
DefineTemplateBox[DerivedArraySignatureForm, "arrayShapeForm", AngleBracketBox @ #1, None];

shapeFieldBox[shape_] := shapeBox @ shape;
shapeFieldBox[name_ -> shape_] := TBox[name, shapeBox @ shape, "shapeFieldForm"];
DefineTemplateBox[DerivedArraySignatureForm, "shapeFieldForm", RBox[BoldBox[#1], ":", #2], None];
 *)
(**************************************************************************************************)

PublicFunction[DerivedArraySignatureForm]

$colon = StyleBox[" : ", FontWeight -> "Regular"];

shapeLabelBox[a_, b_] := SuperscriptBox[a, b];
shapeLabelBox[a_, b_, s_] := RowBox[{SubscriptBox["", s], SuperscriptBox[a, b]}];
shapeLabelBox[a_, b_, s_] := SubsuperscriptBox[a, s, b];

a_DerivedArraySignatureForm[graphics_] :=
  ArrayCiruitLabeled[graphics, a];

DefineStandardTraditionalForm[
  DerivedArraySignatureForm[name_, ins_, out_] :> TBox[
    StyleBox[name, FontWeight -> "Regular"],
    derivedArrayInBox[ins],
    shapeBox[out],
    "derivedArraySignatureForm"
  ]
];

DefineTemplateBox[DerivedArraySignatureForm, "derivedArraySignatureForm", RBox[shapeLabelBox[#1, #3], $colon, #2], None];
$notebookDisplayFunctionBases["derivedArraySignatureForm"] = "StringBlockForm";

DefineStandardTraditionalForm[
  DerivedArraySignatureForm[name_, ins_, out_, scalar_] :> TBox[
    StyleBox[name, FontWeight -> "Regular"],
    derivedArrayInBox[ins],
    shapeBox[out],
    ToBoxes @ scalar,
    "derivedArraySignatureWithScalarForm"
  ]
];

DefineTemplateBox[DerivedArraySignatureForm, "derivedArraySignatureWithScalarForm", RBox[shapeLabelBox[#1, #3, #4], $colon, #2], None];
$notebookDisplayFunctionBases["derivedArraySignatureWithScalarForm"] = "StringBlockForm";

DefineStandardTraditionalForm[
  DerivedArraySignatureForm[name_, {}, out_, scalar_] :> TBox[
    StyleBox[name, FontWeight -> "Regular"],
    shapeBox[out],
    ToBoxes @ scalar,
    "derivedArraySignatureWithScalarFormNoParams"
  ]
];

DefineTemplateBox[DerivedArraySignatureForm, "derivedArraySignatureWithScalarFormNoParams", shapeLabelBox[#1, #2, #3], None];
$notebookDisplayFunctionBases["derivedArraySignatureWithScalarFormNoParams"] = "StringBlockForm";

derivedArrayInBox[list_] := ParenthesesBox[CommaRowBox @@ Map[shapeFieldBox, ToList @ list]];

shapeBox[list_List] := TBox[RowBox @ Map[dimBox, list], "arrayShapeForm"];
shapeBox[item_ | {item_}] := TBox[dimBox @ item, "arrayShapeForm"];

DefineTemplateBox[DerivedArraySignatureForm, "arrayShapeForm", #1, None];
$notebookDisplayFunctionBases["arrayShapeForm"] = "StringBlockForm";

dimBox[a_ArrayShapeForm] := ToBoxes @ a;
dimBox[i_Integer] := StyleBox["\[FilledSquare]", ToRainbowColor @ i, FontFamily -> "Fira Code"];
dimBox[s_ -> i_] := StyleBox["\[FilledSquare]", ToRainbowColor @ i, FontFamily -> "Fira Code"];
dimBox[(All | "\[Ellipsis]") -> i_] := StyleBox["\[FilledSquare]", ToRainbowColor @ i, FontFamily -> "Fira Code"];
dimBox[All | "\[Ellipsis]"] := StyleBox["\[FilledSquare]", $LightGray, FontFamily -> "Fira Code"];
dimBox[s_] := s;

shapeFieldBox[shape_] := shapeBox @ shape;
shapeFieldBox[name_ -> None] := TBox[name, "shapeFieldNameForm"];
shapeFieldBox[name_ -> shape_] := TBox[TBox[name, "shapeFieldNameForm"], shapeBox @ shape, "shapeFieldForm"];
DefineTemplateBox[DerivedArraySignatureForm, "shapeFieldForm", shapeLabelBox[#1, #2], None];
DefineTemplateBox[DerivedArraySignatureForm, "shapeFieldNameForm", StyleBox[#, Bold, FontFamily -> "Fira Code", SingleLetterItalics -> False], None];
$notebookDisplayFunctionBases["shapeFieldForm"] = "StringBlockForm";
$notebookDisplayFunctionBases["shapeFieldNameForm"] = "StringBlockForm";

(**************************************************************************************************)

PublicFunction[ArrayShapeForm]

DefineStandardTraditionalForm[
  ArrayShapeForm[shape_] :> shapeFieldBox[shape]
];

PublicFunction[ArrayCiruitLabeled]

ArrayCiruitLabeled[graphics_, label_] :=
  Grid[{
    List @ label,
    List @ graphics
  }, BaselinePosition -> {2, 1}, Alignment -> {Center, Baseline}];

