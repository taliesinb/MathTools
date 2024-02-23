PublicVariable[$KeyWireColor, $ValueWireColor, $BubbleWireColor, $BubblePortColor, $BundleWireColor, $RainbowBundleWireColor, $BubbleBackground]

$KeyWireColor = $Purple;
$ValueWireColor = $Gray;
$BubbleWireColor = GrayLevel[0.3];
$BubblePortColor = GrayLevel[0.3];
$BubblePortShape = "DownHalfDiamond"
$RainbowBundleWireColor = $LightGray;
$BundleWireColor = $Purple;
$BubbleBackground = GrayLevel[0.97];

(**************************************************************************************************)

PublicFunction[PortStyleData]

(**************************************************************************************************)

PortStyleData["Default"] = Sequence[
  PortSize -> 0.16
];

$defaultNodeStyle = Sequence[
  PortStyleData["Default"],
  PortSpacing -> Auto,
  Background -> White,
  FrameThickness -> 2,
  FrameMargins -> 0.75,
  FrameColor -> $Gray
];

$epipro = Epilog;

(**************************************************************************************************)

toTopBottom[list:{__Rule}] := list;
toTopBottom[Rule[i_, o_]]  := {Top -> i, Bottom -> o};
toTopBottom[i_]            := {Top -> i, Bottom -> 1};

(**************************************************************************************************)

PublicFunction[ClassicalRelationNode]

PortStyleData["Relation"] = Sequence[
  PortShape -> "Disk",
  PortColor -> $KeyWireColor,
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
  PortColor -> $KeyWireColor,
  PortStyleData["Default"]
]

PortStyleData["Value"] = Sequence[
  PortShape -> "DownHalfDiamond",
  PortColor -> $ValueWireColor,
  PortStyleData["Default"]
]

$arrayNodeStyle = Sequence[
  FrameThickness -> 3,
  NodeLabelOffset -> {0, -2},
  PortShape -> {All -> "InnerDisk", Out -> "OuterDiamond"},
  $defaultNodeStyle
];

PublicFunction[ClassicalArrayNode]

$classicalArrayNodeStyle = Sequence[
  PortColor -> {All -> $KeyWireColor, Out -> $ValueWireColor},
  $arrayNodeStyle
]

ClassicalArrayNode[name_, n_, interior:Except[_Rule]:Auto, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name, NodePorts -> toTopBottom[n], $classicalArrayNodeStyle];

(**************************************************************************************************)

PublicFunction[MultisetNode]

$multisetNodeStyle = Sequence[
  RoundingRadius -> 0.2,
  FrameThickness -> 3,
  PortShape -> None,
  FrameColor -> $DarkPurple,
  FrameDashing -> False,
  PortPositions -> "MatchInterior",
  $classicalArrayNodeStyle
];

MultisetNode[name_, n_, interior_, opts___Rule] := MultisetNode[name, n, interior, {}, opts];
MultisetNode[name_, n_, interior_, epilog_List, opts___Rule] :=
  NodeBox[interior, opts, NodeAlias -> name, NodePorts -> toTopBottom[n], $epipro -> epilog, $multisetNodeStyle];

(**************************************************************************************************)

PublicFunction[PartSumNode]



(**************************************************************************************************)

PublicFunction[AggregationNode, RainbowAggregationNode]

PortStyleData["Rainbow"] = Sequence[
  PortShape -> "DownHalfDisk",
  PortColor -> "Medium",
  PortStyleData["Default"]
]

AggregationNode[name_, n_ -> agg_, rest___] := MultisetNode[
  name, n, rest, PortShape -> {All -> "Disk", Out -> Labeled["OuterDiamond", Style[Padded[agg, Left -> .2], FontSize -> 14, Bold, FontFamily -> "Fira Code"], TopRight]}
];

RainbowAggregationNode[args___] := AggregationNode[args, PortColor -> {All -> "Medium", Out -> $ValueWireColor}];

(**************************************************************************************************)

PublicFunction[AggregationLabelProlog]

AggregationLabelProlog[label_] :=
  Prolog -> Text[label, AbsoluteOffset[{.15, .03}] @ NodeSide[Bottom], {-1,-1}, BaseStyle -> {FontSize -> 14, Bold, FontFamily -> "Fira Code"}];

(**************************************************************************************************)

PublicFunction[RainbowArrayNode]

$rainbowArrayNodeStyle = Sequence[
  PortColor -> {All -> "Medium", Out -> $ValueWireColor},
  $arrayNodeStyle
];

RainbowArrayNode[name_, cols_, interior:Except[_Rule]:Auto, opts___Rule] :=
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
  FrameColor -> $Gray,
  FrameThickness -> 3,
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

PublicVariable[$HigherOrderFunctionOptions]
$HigherOrderFunctionOptions = Sequence[FrameColor -> $DarkGray, FrameThickness -> 4, RoundingRadius -> .1];

DerivedBubbleFunctionNode[args___] := DerivedArrayNode[args, PortColor -> $BubblePortColor, PortShape -> $BubblePortShape, $HigherOrderFunctionOptions];

PublicFunction[DerivedBubbleOutFunctionNode]

DerivedBubbleOutFunctionNode[args___] := DerivedArrayNode[args, PortColor -> {In -> $KeyWireColor, Out -> $BubblePortColor}, PortShape -> {In -> "DownHalfDisk", Out -> $BubblePortShape}];

PublicFunction[DerivedRainbowBubbleFunctionNode]

DerivedRainbowBubbleFunctionNode[args___] := DerivedRainbowArrayNode[args, PortShape -> $BubblePortShape, $HigherOrderFunctionOptions];

(**************************************************************************************************)

PublicFunction[RainbowAliasLabel]

RainbowAliasLabelProlog[i_Int -> name_Str] :=
  Prolog -> Text[name, NodeSide[BottomRight], {1.3,-1.3}, BaseStyle -> {$portColor @ i, Bold}];

(**************************************************************************************************)

PublicFunction[RainbowAliasedFunction]

RainbowAliasedFunction[i_Int -> name_Str, args___] :=
  DerivedRainbowArrayNode[
    args,
    RainbowAliasLabelProlog[i -> name],
    NodeAlias -> name
  ];

(**************************************************************************************************)

$bubbleNodeStyle = Sequence[
  RoundingRadius -> .5,
  FrameThickness -> 4,
  FrameColor -> $BubbleWireColor,
  Background -> $BubbleBackground
]

PublicFunction[BubbleValueNode]

$bubbleValueNodeStyle = Sequence[
  $bubbleNodeStyle,
  $classicalArrayNodeStyle
]

BubbleValueNode[name_, n_, interior:Except[_Rule]:Auto, opts___Rule] :=
  NodeBox[interior, opts, NodeAlias -> name, NodePorts -> toTopBottom[n], $bubbleValueNodeStyle];

BubbleValueNode[name_, n_, interior:Except[_Rule]:Auto, epilog:Except[_Rule], opts___Rule] :=
  BubbleValueNode[name, n, interior, Epilog -> epilog, opts];

(**************************************************************************************************)

PublicFunction[RainbowBubbleValueNode]

$rainbowBubbleValueNodeStyle = Sequence[
  $bubbleNodeStyle,
  $rainbowArrayNodeStyle
]

RainbowBubbleValueNode[name_, n_, interior:Except[_Rule]:Auto, opts___Rule] :=
  NodeBox[interior, opts, NodeAlias -> name, NodePorts -> toTopBottom[n], $rainbowBubbleValueNodeStyle];

RainbowBubbleValueNode[name_, n_, interior:Except[_Rule]:Auto, epilog:Except[_Rule], opts___Rule] :=
  RainbowBubbleValueNode[name, n, interior, Epilog -> epilog, opts];

(**************************************************************************************************)

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
  PortShape -> "Disk",
  PortStyleData["Key"],
  PortPositions -> {Out -> "MatchOut"},
  $defaultNodeStyle
]

KeyPortSkeleton[args___] := PortSkeleton[args, FilterOptions @ $keyPortSkeletonStyle];

(**************************************************************************************************)

PublicFunction[RainbowKeyPortSkeleton]

RainbowKeyPortSkeleton[args___] := PortSkeleton[args, PortColor -> "Medium", FilterOptions @ $keyPortSkeletonStyle];

(**************************************************************************************************)

toWirePort = Case[
  i_Int      := $in[i];
  key_Str    := %[key -> PortIndex[1]];
  key_ -> i_ := $out[key, i];
  vals_$     := Map[%, vals];
  list_List  := Map[%, list];
  other_     := other;
];

toWireSrcPort[e_] := toWirePort[e] /. {$in -> NodeInPort,  $out -> NodeOutPort};
toWireDstPort[e_] := toWirePort[e] /. {$in -> NodeOutPort, $out -> NodeInPort};

toCircuitCurve[e_] := Fold[RepAll, e, {
  curve[a_, b_, l___, LineColor -> Auto, r___] :> curve[a, b, l, LineColor -> getPortColors[a, b], r],
  curve[a_, b:Except[$Coord2P, _List], opts___Rule] :> CircuitCurve[FanOut[a, b], opts],
  curve[a_, b:Except[_Rule], opts___Rule]           :> CircuitCurve[{a, b}, opts],
  c_curve :> Print[c]
}];

wireCurve[args___] :=
  wireCurve1 @@ RepAll[{args}, {
    $[s_Str] :> Apply[$, Chars @ s],
    $[i_Int] :> Apply[$, Range @ i]
  }];

wireCurve1[a:$Coord2P, b:$Coord2P, opts___Rule] := toCircuitCurve @ curve[a, b, opts];

wireCurve1[a_, b_, opts___] := Scope[
  c = curve[toWireSrcPort @ a, toWireDstPort @ b, opts];
  If[ContainsQ[c, _$],
    i = 1; args = {};
    cFn = Construct[Fn, c] /. vals_$ :> RuleEval[
      AppTo[args, List @@ vals];
      Slot[i++]
    ];
    c = MapThread[cFn, args]
  ];
  toCircuitCurve @ c
];

(**************************************************************************************************)

PrivateHead[$portColor]

$portColor[i_Int] := ToRainbowColor @ i;
$portColor[Auto] := Auto;

getPortColors[(NodeInPort|NodeOutPort|NodePort)[___, p_], _] := $portColor[p];
getPortColors[_, (NodeInPort|NodeOutPort|NodePort)[___, p_]] := $portColor[p];
getPortColors[a_, b_] := None;

(**************************************************************************************************)

$curveP = _Line | _NeatCurve | _CompassCurve | _SetbackCurve | _RollingCurve | _SnakeCurve;

PublicFunction[ApplyWireOptions]

ApplyWireOptions[style___][e_] := e /. CircuitCurve[c_, opts___] :> CircuitCurve[c, style, opts];

PublicFunction[RainbowWire]

$rainbowWireOpts = Sequence[LineThickness -> 2, LineEdging -> True];
RainbowWire[c:$curveP, opts___Rule] := CircuitCurve[c, opts, $rainbowWireOpts];
RainbowWire[port1_, port2_, opts___Rule] := RainbowWire[port1, port2, Auto, opts];
RainbowWire[port1_, port2_, color:($ColorPattern|Auto|_Int|_String), opts___Rule]  := wireCurve[port1, port2, LineColor -> $portColor[color], opts, $rainbowWireOpts];

PublicFunction[RainbowWireBundle]

$bundleOpts = Sequence[LineThickness -> 11, LineEdging -> True];
RainbowWireBundle[c:$curveP, opts___Rule] := CircuitCurve[c, opts, $bundleOpts];
RainbowWireBundle[port1_, port2_, opts___Rule] := wireCurve[port1, port2, LineColor -> $RainbowBundleWireColor, opts, $bundleOpts];
RainbowWireBundle[port1_, port2_, color_, opts___Rule] := wireCurve[port1, port2, LineColor -> $portColor[color], opts, $bundleOpts];

PublicFunction[KeyWire]

$wireOpts = Sequence[LineThickness -> 4];
$keyWireOpts = Sequence[LineColor -> $KeyWireColor, $wireOpts];
KeyWire[c:$curveP, opts___Rule] := CircuitCurve[c, opts, $wireOpts];
KeyWire[port1_, port2_, opts___Rule]   := wireCurve[port1, port2, opts, $keyWireOpts];

PublicFunction[ValueWire]

$valueWireOpts = Sequence[LineColor -> $ValueWireColor, $wireOpts];
ValueWire[c:$curveP, opts___Rule] := CircuitCurve[c, opts, $valueWireOpts];
ValueWire[port1_, port2_, opts___Rule] := wireCurve[port1, port2, opts, $valueWireOpts];

PublicFunction[BubbleWire, OffsetBubbleWire]

$bubbleWireOpts = Sequence[LineColor -> $BubbleWireColor, LineThickness -> 5];
BubbleWire[c:$curveP, opts___Rule] := CircuitCurve[c, opts, $bubbleWireOpts]
BubbleWire[port1_, port2_, opts___Rule] := wireCurve[port1, port2, opts, $bubbleWireOpts];
BubbleWire[port1_, port2_, cols_List, opts___Rule] := wireCurve[port1, port2, opts, WireTypeSlug -> cols, $bubbleWireOpts];
OffsetBubbleWire[n_, box1_, port2_, rest___] :=
  BubbleWire[SnakeCurve[{AbsoluteOffset[{n, 0}] @ NodeSide[box1, Bottom], port2}, BendStyle -> "Smooth", SplitPosition -> "Middle"], rest];

PublicFunction[KeyWireBundle]

$keyWireBundleOpts = Sequence[LineColor -> $BundleWireColor, $bundleOpts];
KeyWireBundle[c:$curveP, opts___Rule] := CircuitCurve[c, opts, $keyWireBundleOpts];
KeyWireBundle[port1_, port2_, opts___Rule] := wireCurve[port1, port2, opts, $keyWireBundleOpts];

(**************************************************************************************************)

$functionNodeStyle = Sequence[
  FrameThickness -> 3,
  NodeLabelOffset -> {0, -2},
  PortPositions -> {In -> "MatchIn"},
  $defaultNodeStyle
];

PublicFunction[KeyFunctionNode]

$keyFunctionNodeStyle = Sequence[
  PortShape -> {All -> "InnerDisk", Out -> "OuterDisk"},
  PortColor -> $KeyWireColor,
  $functionNodeStyle
]

KeyFunctionNode[name_, n_, interior:Except[_Rule]:Auto, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name, NodePorts -> toTopBottom[n], $keyFunctionNodeStyle];

(**************************************************************************************************)

PublicFunction[ScalarFunctionNode]

$scalarFunctionNodeStyle = Sequence[
  PortShape -> {All -> "InnerDiamond", Out -> "OuterDiamond", Left -> "InnerDiamond", Right -> "InnerDiamond"},
  PortColor -> $ValueWireColor,
  $functionNodeStyle
]

ScalarFunctionNode[name_, n_, interior:Except[_Rule]:Auto, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name, NodePorts -> toTopBottom[n], $scalarFunctionNodeStyle];

(**************************************************************************************************)

PublicFunction[RainbowScalarFunctionNode]

$rainbowFunctionNodeStyle = Sequence[
  PortShape -> {All -> "InnerDiamond", Out -> "OuterDiamond"},
  PortColor -> {All -> "Medium", Out -> $ValueWireColor},
  $functionNodeStyle
]

RainbowScalarFunctionNode[name_, n_, interior:Except[_Rule]:Auto, opts___Rule] :=
  NodeBox[interior, opts, NodeLabel -> name, NodeAlias -> name, NodePorts -> toTopBottom[n], $rainbowFunctionNodeStyle];

(**************************************************************************************************)

PublicFunction[BubbleFunctionNode]

BubbleFunctionNode[args__] :=
  ScalarFunctionNode[args, PortColor -> $BubblePortColor, PortShape -> $BubblePortShape, $HigherOrderFunctionOptions];

(**************************************************************************************************)

PublicFunction[SumNode]

SumNode[] := ScalarFunctionNode["sum", 1, {1.5, 1}]

(**************************************************************************************************)

arithmeticNode[label_, col_] := NodeDisk[
  .6,
  NodeLabel -> Padded[Style[label, PrivateFontOptions -> {"OperatorSubstitution" -> True}], {.08, 0}],
  NodeAlias -> label,
  NodePorts -> "Compass", PortShape -> None,
  Background -> White,
  FrameThickness -> 3,
  FrameColor -> col
];

PublicFunction[ScalarTimesNode]
PublicFunction[ArrayTimesNode]

ScalarTimesNode[] := arithmeticNode["*", $Gray];
ArrayTimesNode[] := arithmeticNode["*", $BubbleWireColor];

PublicFunction[ScalarPlusNode]
PublicFunction[ArrayPlusNode]

ScalarPlusNode[] := arithmeticNode["+", $Gray];
ArrayPlusNode[] := arithmeticNode["+", $BubbleWireColor];

PublicFunction[ScalarDivideNode]
PublicFunction[ArrayDivideNode]

ScalarDivideNode[] := arithmeticNode["/", $Gray];
ArrayDivideNode[] := arithmeticNode["/", $BubbleWireColor];

(**************************************************************************************************)

PublicFunction[ArrayCircuitGraphics]

ArrayCircuitGraphics[nodes_, opts___Rule] := Scope[
  padding = Lookup[{opts}, ImagePadding, Auto];
  frameLabel = LookupOption[nodes, FrameLabel, None];
  SetAuto[padding, 2];
  padding //= StandardizePadding;
  Part[padding, 2, 1] //= Max[#, 6]&;
  If[frameLabel =!= None,
    Part[padding, 2, 2] //= Max[#, 18]&;
    If[StrQ[frameLabel] && SLen[frameLabel] > 10,
      Part[padding, 1, 1] //= Max[#, 10]&;
      Part[padding, 1, 2] //= Max[#, 10]&;
    ];
  ];
  complex = NodeComplex[nodes,
    NodePalette -> Lookup[{opts}, NodePalette, None]
  ];
  FixedGraphics[
    {FontFamily -> "Fira Code", complex},
    ImagePadding -> padding,
    GraphicsScale -> 40
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
  i_Int := ToRainbowColor[i];
]

wireIconGraphics[style_] := FixedGraphics[
  {AbsoluteThickness[3], style, CircuitCurve[{{0,0}, {0, .5}}]},
  ImagePadding -> {Horizontal -> 2},
  GraphicsScale -> 40,
  BaselinePosition -> Scaled[0.15]
];

(**************************************************************************************************)

PublicFunction[DerivedArraySignatureForm]

$colon = StyleBox["\[ThinSpace]:\[ThinSpace]", FontWeight -> "Regular"];

shapeLabelBox[a_, b_] := SuperscriptBox[a, b];
shapeLabelBox[a_, b_, s_] := RowBox[{SubscriptBox["", s], SuperscriptBox[a, b]}];
shapeLabelBox[a_, b_, s_] := SubsuperscriptBox[a, s, b];

a_DerivedArraySignatureForm[graphics_] :=
  ArrayCiruitLabeled[graphics, a];

largeBox[b_] := LargerBox @ LargerBox @ b;

DefineStandardTraditionalForm[
  DerivedArraySignatureForm[name_] :> TBox[
    largeBox @ BoldBox @ MakeMathBoxes @ name,
    "derivedArraySignatureFormNoArgs"
  ]
];

DefineTemplateBox[DerivedArraySignatureForm, "derivedArraySignatureFormNoArgs", #1, None];
$notebookDisplayFunctionBases["derivedArraySignatureFormNoArgs"] = "StringBlockForm";

DefineStandardTraditionalForm[
  DerivedArraySignatureForm[name_, ins_, out_] :> TBox[
    largeBox @ BoldBox @ MakeMathBoxes @ name,
    derivedArrayInBox[ins],
    shapeBox[out],
    "derivedArraySignatureForm"
  ]
];

DefineTemplateBox[DerivedArraySignatureForm, "derivedArraySignatureForm", RBox[shapeLabelBox[#1, #3], $colon, #2], None];
$notebookDisplayFunctionBases["derivedArraySignatureForm"] = "StringBlockForm";

DefineStandardTraditionalForm[
  DerivedArraySignatureForm[name_, ins_, out_, scalar_] :> TBox[
    largeBox @ BoldBox @ MakeMathBoxes @ name,
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
    largeBox @ BoldBox @ MakeMathBoxes @ name,
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
dimBox[i_Int] := StyleBox["\[FilledSquare]", ToRainbowColor @ i, FontFamily -> "Fira Code"];
dimBox[s_ -> i_] := StyleBox["\[FilledSquare]", ToRainbowColor @ i, FontFamily -> "Fira Code"];
dimBox[(All | "\[Ellipsis]") -> i_] := StyleBox["\[FilledSquare]", ToRainbowColor @ i, FontFamily -> "Fira Code"];
dimBox[All | "\[Ellipsis]"] := StyleBox["\[FilledSquare]", $LightGray, FontFamily -> "Fira Code"];
dimBox[s_] := s;

shapeFieldBox[str_String] := MakeMathBoxes @ str;
shapeFieldBox[shape_] := shapeBox @ shape;
shapeFieldBox[name_ -> None] := TBox[MakeMathBoxes @ name, "shapeFieldNameForm"];
shapeFieldBox[name_ -> shape_] := TBox[TBox[MakeMathBoxes @ name, "shapeFieldNameForm"], shapeBox @ shape, "shapeFieldForm"];
DefineTemplateBox[DerivedArraySignatureForm, "shapeFieldForm", shapeLabelBox[#1, #2], None];
DefineTemplateBox[DerivedArraySignatureForm, "shapeFieldNameForm", StyleBox[#, Bold, FontFamily -> "Fira Code", SingleLetterItalics -> False], None];
$notebookDisplayFunctionBases["shapeFieldForm"] = "StringBlockForm";
$notebookDisplayFunctionBases["shapeFieldNameForm"] = "StringBlockForm";

(**************************************************************************************************)

PublicFunction[ArrayShapeForm]

DefineStandardTraditionalForm[
  ArrayShapeForm[shape_] :> shapeFieldBox[shape]
];

PublicTypesettingForm[ArrayCiruitLabeled]

ArrayCiruitLabeled[label_][graphics_] := ArrayCiruitLabeled[graphics, label];

DefineStandardTraditionalForm[
  ArrayCiruitLabeled[graphics_, label_] :>
  ToBoxes @ Grid[{
    List @ label,
    List @ graphics
  }, BaselinePosition -> {2, 1}, Alignment -> {Center, Baseline}]
];
