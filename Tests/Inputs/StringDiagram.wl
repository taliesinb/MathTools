(* ::Package:: *)

(* ::Section:: *)
(*Basic*)


LoadShortcuts["Categories"]


(* ::Section:: *)
(*Centering of text within disks*)


toPos[i_] := {Mod[i, 5,1], Ceiling[i/5]-.5}*6 - 18;
plotCharGrid[opts___] := StringDiagram[MapIndex1[toPos[#2] -> #1&,
  Append["[ "] @ Characters["\[Gamma]\[Eta]\[Delta]\[Epsilon]\[Alpha]\[LongRightArrow]\[RightArrow]\[UpDownArrow]\[LeftArrow]\[LongLeftArrow]_*.^\[FilledSquare]\[FilledCircle]\[EmptyCircle]\[CircleDot]\[CirclePlus]\[CircleTimes]01234() ["]],{}, opts, DiagramSize -> {16,20}];
$sizes = {1, 1.25, 1.5};
ZipMap[
  TestRaster @ plotCharGrid[GraphicsScale -> #1, NodeSize -> #2, FontSize -> #3]&,
  5 * $sizes, 25 * $sizes, 20 * $sizes
]


(* ::Section:: *)
(*Functorial string diagram*)


TestRaster @ FunctorialStringDiagram[
  {Center -> $NTeta},
  {},
  {RF @ $Oc, 0.5 -> RBF @ $Af, BF @ $Od}
]


(* ::Section:: *)
(*Adjunctions*)


(* ::Subsection:: *)
(*ok so burrow modifier ruins *)


$colorRules = {
  $Oc -> $DarkBlue, $Od -> $DarkRed, $ff -> $DarkRed, $fg -> $DarkBlue,
  $CC -> 1, $CD -> 2, $CE -> 3,
  $FunL -> {1, 2}, $FunR -> {2, 1},   $NTeta -> $Pink,  $NTeps -> $Pink,
  $FunL1 -> {1, 2}, $FunR1 -> {2, 1}, $NTeps1 -> $Pink, $NTeta1 -> $Pink,
  $FunL2 -> {2, 3}, $FunR2 -> {3, 2}, $NTeps2 -> $Teal, $NTeta2 -> $Teal,
  $FunL3 -> {1, 3}, $FunR3 -> {3, 1}, $NTeps3 -> $Orange, $NTeta3 -> $Orange
};
$adjOpts := Sequence[
  RegionFilling -> "Labeled", ColorRules -> $colorRules,
  WireThickness -> 2, NodeEdgeThickness -> 2, NodeLabelFontSize -> 18, NodeSize -> 21, NodeShape -> "Box",
  TextModifiers -> {Subscript[z_, _] :> z}
];
MonadPanelDiagram[c_, d_, l_, r_, eta_, eps_] := SpacedRow[
  StringDiagram[{}, {Bottom \[UndirectedEdge] Top -> l}, {Left -> c, Right -> d}, DiagramSize -> {9, 12}, $adjOpts],
  StringDiagram[{}, {Bottom \[UndirectedEdge] Top -> r}, {Left -> d, Right -> c}, DiagramSize -> {9, 12}, $adjOpts],
  StringDiagram[{{0,0} -> eta}, {1 \[UndirectedEdge] {Top,-6} -> r, 1 \[UndirectedEdge] {Top,6} -> l}, {Bottom -> d, Top -> c}, $adjOpts],
  StringDiagram[{{0,0} -> eps}, {1 \[UndirectedEdge] {Bottom,-6} -> l, 1 \[UndirectedEdge] {Bottom,6} -> r}, {Bottom -> d, Top -> c}, $adjOpts]
];
TestRaster /@ First @ MonadPanelDiagram[$CC, $CD, $FunL, $FunR, $NTeta, $NTeps]


(* ::Subsection:: *)
(*TODO: introduce violin curve!*)


TestRaster @ StringDiagram[{
  {-4,5} -> $NTeps, {4,-5} -> $NTeta},
  {1 \[UndirectedEdge] {Bottom,-8} -> $FunL, Customized[1 \[UndirectedEdge] 2 -> $FunR, LabelPosition -> Left], 2 \[UndirectedEdge] {Top,8} -> $FunL},
  {{-11,-11} -> Red, {11,11} -> Blue}, $adjOpts]
TestRaster @ StringDiagram[{}, {Bottom \[UndirectedEdge] Top -> $FunL}, {{-6,0} -> $CC, {6,0} -> $CD}, $adjOpts]

TestRaster @ StringDiagram[
  {{4,5} -> $NTeps, {-4,-5} -> $NTeta},
  {1 \[UndirectedEdge] {Bottom,8} -> $FunR, Customized[1 \[UndirectedEdge] 2 -> $FunL,  SplitPosition -> "Middle", "SplitOrientation" -> Vertical], 2 \[UndirectedEdge] {Top,-8} -> $FunR},
  {{11,-11} -> Red, {-11,11} -> Blue}, $adjOpts]
TestRaster @ StringDiagram[{}, {Bottom \[UndirectedEdge] Top -> $FunR}, {{6,0} -> $CC, {-6, 0} -> $CD}, $adjOpts]


RQG;TestRaster @ FunctorialStringDiagram[
  {},
  {Customized[{Bottom,0} \[UndirectedEdge] 1 -> $FunL, SplitPosition -> "End"]},
  {$Oc, 0.5 -> $ff, $Od},
  {BottomRight -> $CD, BottomLeft -> $CC},
  $adjOpts
]

TestRaster @ FunctorialStringDiagram[
  {{0, 7} -> $NTeps},
  {Customized[Bottom \[UndirectedEdge] 1 -> $FunL, SplitPosition -> "End"],
   Customized[1 \[UndirectedEdge] "g" -> Customized[$FunR, LabelPosition -> Left], SplitPosition -> "End"]
   },
  {$Oc, 0.5 -> $fg, $Od},
  {BottomRight -> $CD, BottomLeft -> $CC},
  $adjOpts
]


TestRaster @ FunctorialStringDiagram[
  {},
  {Customized[{Top, 0} \[UndirectedEdge] "g" -> $FunR, SplitPosition -> "End"]},
  {$Oc, 0.5 -> $fg, $Od},
  {TopLeft -> $CD, TopRight -> $CC},
  $adjOpts
]
TestRaster @ FunctorialStringDiagram[
  {{0, -7} -> $NTeps},
  {Customized[{Top,0} \[UndirectedEdge] 1 -> $FunR, SplitPosition -> "End"],
   Customized[1 \[UndirectedEdge] 2 -> $FunL, SplitPosition -> "End", LabelPosition -> Right]
   },
  {$Oc, 0.5 -> $ff, $Od},
  {TopLeft -> $CD, TopRight -> $CC},
  $adjOpts
]


TripleFunctorDiagram[c_, d_, e_, l1_, l2_] :=
  StringDiagram[{}, {{Bottom,3} \[UndirectedEdge] Top -> l2, {Bottom,-3} \[UndirectedEdge] Top -> l1}, {Left -> c, Center -> d, Right -> e}, DiagramSize -> {9, 12}, $adjOpts];

NestedMuDiagram[c_, d_, e_, l1_, l2_, r1_, r2_, eta1_, eta2_, opts___] :=
  StringDiagram[{{0,4.5} -> eta1, {0, -4.5} -> eta2}, {2 \[UndirectedEdge] {Top,-8} -> r2, 1 \[UndirectedEdge] {Top,-4} -> r1, 1 \[UndirectedEdge] {Top,4} -> l1, 2 \[UndirectedEdge] {Top,8} -> l2}, {{0, -9} -> e, Center -> d, {0, 9} -> c}, $adjOpts, opts];

ComposedMonadPanelDiagram[c_, d_, e_, l1_, l2_, r1_, r2_, eta1_, eta2_, eps1_, eps2_] := SpacedRow[
  TripleFunctorDiagram[c, d, e, l1, l2],
  TripleFunctorDiagram[e, d, c, r2, r1],
  NestedMuDiagram[c, d, e, l1, l2, r1, r2, eta1, eta2],
  NestedMuDiagram[c, d, e, r2, r1, l2, l1, eps2, eps1, FlipY -> True]
];
TestRaster /@ First @ MonadPanelDiagram[$CC, $CD, $FunL1, $FunR1, $NTeta1, $NTeps1]
TestRaster /@ First @ MonadPanelDiagram[$CD, $CE, $FunL2, $FunR2, $NTeta2, $NTeps2]
TestRaster /@ First @ ComposedMonadPanelDiagram[$CC, $CD, $CE, $FunL1, $FunL2, $FunR1, $FunR2, $NTeta1, $NTeta2, $NTeps1, $NTeps2]
TestRaster /@ First @ MonadPanelDiagram[$CC, $CE, $FunL3, $FunR3, $NTeta3, $NTeps3]


(* ::Section:: *)
(* Monad and comonad diagrams*)


$monAdjColorRules = {
  $Oc -> $DarkBlue, $Od -> $DarkRed, $ff -> $DarkRed, $fg -> $DarkBlue,
  $CC -> 1, $CD -> 2, $CE -> 3,
  $FunT1 -> $DarkRed, $FunT2 -> $DarkBlue, $FunT3 -> 3,
  $NTmu -> $DarkBlue, $NTeta -> $DarkBlue,
  $NTdelta -> $DarkRed, $NTeps -> $DarkRed,
  $FunL -> {1, 2}, $FunR -> {2, 1},
  $FunL1 -> {1, 2}, $FunR1 -> {2, 1}, $NTeps1 -> $Pink, $NTeta1 -> $Pink,
  $FunL2 -> {2, 3}, $FunR2 -> {3, 2}, $NTeps2 -> $Teal, $NTeta2 -> $Teal
};
$monAdjOpts := Sequence[
  RegionFilling -> "Unlabeled", ColorRules -> $monAdjColorRules,
  WireThickness -> 2, NodeEdgeThickness -> 2, NodeLabelFontSize -> 18, NodeSize -> 22, NodeShape -> "Box",
  TextModifiers -> {Subscript[z_, _] :> z}
];
MonadMuDiagram[c_, t_, mu_, opts___] := StringDiagram[{{0,0} -> mu}, {1 \[UndirectedEdge] {Bottom,6} -> t, 1 \[UndirectedEdge] {Bottom,-6} -> t, 1 \[UndirectedEdge] Top -> t}, Background -> c, opts, $monAdjOpts];
MonadEtaDiagram[c_, t_, eta_, opts___] := StringDiagram[{{0,0} -> eta}, {1 \[UndirectedEdge] Top -> t}, Background -> c, opts, $monAdjOpts];
MonadDeltaDiagram[c_, t_, delta_] := MonadMuDiagram[c, t, delta, FlipY -> True];
MonadEpsDiagram[c_, t_, eps_] := MonadEtaDiagram[c, t, eps, FlipY -> True];
TestRaster @ MonadMuDiagram[$CD, $FunT2, $NTmu]
TestRaster @ MonadEtaDiagram[$CD, $FunT2, $NTeta]
TestRaster @ MonadDeltaDiagram[$CC, $FunT1, $NTdelta]
TestRaster @ MonadEpsDiagram[$CC, $FunT1, $NTeps]


(* ::Subsubsection:: *)
(*Monad from adjunction*)


$FunRL = CompositionForm[$FunR, $FunL];
AdjMonadMuDiagram[c_, d_, l_, r_, eps_, opts___] := StringDiagram[
  {{0,-3} -> eps},
  {1 \[UndirectedEdge] {Bottom, -4} -> l, 1 \[UndirectedEdge] {Bottom, 4} -> r, 
  Customized[{Bottom, 8} \[UndirectedEdge] {Top, 2} -> l, SegmentPosition -> 0.55],
   Customized[{Bottom, -8} \[UndirectedEdge] {Top, -2} -> r, SegmentPosition -> 0.55]},
  {Bottom -> d, Center -> c, Right -> d, Left -> d},
  opts, $monAdjOpts
];
AdjMonadEtaDiagram[c_, d_, l_, r_, eta_, opts___] := StringDiagram[
  {{0,0} -> eta},
  {1 \[UndirectedEdge] {Top, -4} -> r, 1 \[UndirectedEdge] {Top, 4} -> l},
  {Bottom -> d, Top -> c},
  opts, $monAdjOpts
];
TestRaster @ MonadMuDiagram[$CD, $FunRL, $NTmu]
TestRaster @ AdjMonadMuDiagram[$CC, $CD, $FunL, $FunR, $NTeps1]
TestRaster @ MonadEtaDiagram[$CD, $FunRL, $NTeta]
TestRaster @ AdjMonadEtaDiagram[$CC, $CD, $FunL, $FunR, $NTeta1]


$FunLR = CompositionForm[$FunL, $FunR];
AdjMonadDeltaDiagram[c_, d_, l_, r_, eta_] := AdjMonadMuDiagram[d, c, r, l, eta, FlipY -> True];
AdjMonadEpsDiagram[c_, d_, l_, r_, eps_] := AdjMonadEtaDiagram[d, c, r, l, eps, FlipY -> True];
TestRaster @ MonadDeltaDiagram[$CC, $FunLR, $NTdelta]
TestRaster @ AdjMonadDeltaDiagram[$CC, $CD, $FunL, $FunR, $NTeta1]
TestRaster @ MonadEpsDiagram[$CC, $FunLR, $NTeps]
TestRaster @ AdjMonadEpsDiagram[$CC, $CD, $FunL, $FunR, $NTeps1]


(* ::Section:: *)
(*Naturality diagrams*)


$obx = DarkRedForm[$Ox]; $oby = DarkBlueForm[$Oy];
$fnf = DarkPinkForm[$Af];
TestRaster @ FunctorialStringDiagram[
  {{-2,#1} -> $NTeta},
  {Bottom \[UndirectedEdge] 1 -> $FunF, 1 \[UndirectedEdge] Top -> $FunG},
  {$Ox, 0.5 -> $Af, $Oy},
  DiagramSize -> {6, 12},
  ColorRules -> {$Ox -> RF, $Af -> RBF, $Oy -> BF}, $adjOpts
]& /@ {-5, 5}


TestRaster @ StringDiagram[
  {{-4, #1} -> $NTalpha, {4, #2} -> $NTbeta},
  {Bottom \[UndirectedEdge] 1, 1 \[UndirectedEdge] Top, Bottom \[UndirectedEdge] 2, 2 \[UndirectedEdge] Top}, {},
  DiagramSize -> {6, 12}, ImagePadding -> 10, $adjOpts
]& @@@ {{-4, 4}, {0, 0}, {4, -4}}

