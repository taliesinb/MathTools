PublicOption[VertexLayout, AdditionalVertexLayoutOptions, VertexOverlapResolution, GraphOrigin]

PublicOption[EdgeLayout]

PublicOption[GraphMetric]

PublicOption[ArrowheadShape, ArrowheadSize, ArrowheadStyle, ArrowheadPosition, TwoWayStyle]

PublicOption[VertexColorFunction, EdgeColorFunction, VertexColorRules, EdgeColorRules, RegionColorRules]

PublicOption[VertexTooltips, VertexClickFunction, EdgeTooltips, VertexOpacity]

PublicOption[CardinalColors, CardinalColorRules, CardinalColorFunction]

PublicOption[VertexLabelPosition, VertexLabelSpacing, VertexLabelBaseStyle, VertexLabelOrientation]

PublicOption[VertexLabelRules, VertexLabelFunction]

PublicOption[EdgeLabelRules, EdgeLabelFunction]

PublicOption[VertexFontSize, VertexBackground]

PublicOption[EdgeLabelPosition, EdgeLabelSpacing, EdgeLabelBaseStyle, EdgeLabelOrientation]

PublicOption[CollapseMultiedges]

PublicOption[EdgeLength]

PublicOption[FrameFade]

PublicOption[VisibleCardinals, ViewRegion, ViewOptions, ViewRotation, LayoutDimension, AdditionalImagePadding, ExtendImagePadding]

PublicOption[VertexCoordinateFunction, CoordinateTransformFunction, CoordinateRotation]

PublicOption[LabelCardinals, AspectRatioClipping, PrologFunction, UseAbsoluteSizes, SelfLoopRadius, MultiEdgeDistance, PackingSpacing]

PublicOption[HighlightStyle, HighlightColor, HighlightOpacity]

PublicOption[PeripheralVertices]

PublicOption[GraphPlottingFunction]

SetUsage @ "
GraphPlottingFunction is an extended option to Graph that specifies a custom function to apply to \
the graph to produce a graphical representation.
* Various global variables are temporarily set during the application that allow properties \
of the graph to be accessed. See GraphPlotScope for more info.
* None indicates the ordinary graph plotting codepath should be used.
* Automatic indicates that the default extended plotting codepath should be used.
"

(**************************************************************************************************)

PrivateVariable[$extendedGraphOptionsRules]

$extendedGraphOptionsRules = {
  AdditionalImagePadding              -> None,
  ArrowheadPosition                   -> Auto,
  ArrowheadShape                      -> Auto,
  ArrowheadSize                       -> 15,
  ArrowheadStyle                      -> Auto,
  ArrowheadOpacity                    -> None,
  AspectRatioClipping                 -> True,
  CardinalColorFunction               -> None,
  CardinalColorRules                  -> None,
  CardinalColors                      -> Auto,
  Cardinals                           -> Auto,
  CollapseMultiedges                  -> False,
  CoordinateRotation                  -> None,
  CoordinateTransformFunction         -> None,
  CustomGraphAnnotation[_Str]      -> None,
  EdgeAnnotations                     -> None,
  EdgeColorFunction                   -> None,
  EdgeColorRules                      -> None,
  EdgeLabelBaseStyle                  -> None,
  EdgeLabelFunction                   -> None,
  EdgeLabelOrientation                -> Auto,
  EdgeLabelPosition                   -> Auto,
  EdgeLabelRules                      -> None,
  EdgeLabelSpacing                    -> 0,
  EdgeSetback                         -> Auto,
  EdgeThickness                       -> Auto,
  EdgeTooltips                        -> None,
  EdgeLength                          -> None,
  EdgeLayout                          -> Auto,
  EdgeOpacity                         -> 0.18,
  EpilogFunction                      -> None,
  ExtendImagePadding                  -> True,
  FrameFade                           -> None,
  GraphLegend                         -> None,
  GraphMetric                         -> Auto,
  GraphOrigin                         -> None,
  GraphPlottingFunction               -> None,
  GraphRegionHighlight                -> None,
  GraphicsScale                       -> None,
  GraphTheme                          -> None,
  HighlightColor                      -> Auto,
  HighlightOpacity                    -> Auto,
  HighlightStyle                      -> Auto,
  LabelCardinals                      -> False,
  LayoutDimension                     -> Auto,
  MultiEdgeDistance                   -> Auto,
  PackingSpacing                      -> Auto,
  PeripheralVertices                  -> None,
  PrologFunction                      -> None,
  RegionColorRules                    -> None,
  SelfLoopRadius                      -> Auto,
  TwoWayStyle                         -> Auto,
  UseAbsoluteSizes                    -> Auto,
  VertexAnnotations                   -> None,
  VertexBackground                    -> White,
  VertexClickFunction                 -> None,
  VertexColorFunction                 -> None,
  VertexColorRules                    -> None,
  VertexColors                        -> None,
  VertexCoordinateFunction            -> None,
  VertexCoordinateRules               -> None,
  VertexFontSize                      -> None,
  VertexLabelBaseStyle                -> None,
  VertexLabelFunction                 -> None,
  VertexLabelOrientation              -> Auto,
  VertexLabelPosition                 -> Auto,
  VertexLabelRules                    -> None,
  VertexLabelSpacing                  -> 0,
  VertexLayout                        -> Auto,
  AdditionalVertexLayoutOptions       -> {},
  VertexOpacity                       -> 1,
  VertexOverlapResolution             -> None,
  VertexTooltips                      -> None,
  ViewOptions                         -> Auto,
  ViewRegion                          -> All,
  VisibleCardinals                    -> All
};

PrivateVariable[$extendedGraphOptionSymbols]

$extendedGraphOptionSymbols = Keys @ $extendedGraphOptionsRules;

$extendedGraphOptionSymbolPattern = Alt @@ $extendedGraphOptionSymbols;

$extendedGraphOptionRulePattern = Rule[$extendedGraphOptionSymbolPattern, _];

(**************************************************************************************************)

$notIntercepted = True;

(* IfSyntaxInfo[SyntaxInformation[Graph]]; *)
Graph; Options[Graph];

$ignoredGraphOptionsSymbols = Alt[
  AlignmentPoint, AnnotationRules, AspectRatio, Axes, AxesLabel,
  AxesOrigin, AxesStyle, Background, BaseStyle, ContentSelectable,
  DirectedEdges, EdgeCapacity, EdgeCost, EdgeWeight, Editable,
  FormatType, FrameTicks, FrameTicksStyle,
  GraphRoot, GraphStyle, GridLines, GridLinesStyle,
  ImageMargins, PerformanceGoal, PlotRegion, PlotTheme,
  Properties, RotateLabel, Ticks, TicksStyle, VertexCapacity,
  VertexShape, VertexWeight
];

$fullGraphOptions = Decases[Sort @ JoinOptions[Graph, $extendedGraphOptionsRules], $ignoredGraphOptionsSymbols -> _];

(**************************************************************************************************)

PublicVariable[$ExtendedGraphOptions, $ExtendedGraphOptionSymbols]

$ExtendedGraphOptions = Cases[$fullGraphOptions, HoldP[_Symbol -> _]];
$ExtendedGraphOptionSymbols = Keys @ $ExtendedGraphOptions;
$extendedGraphSymbolNames = Map[SymbolName, $ExtendedGraphOptionSymbols];

Options[ExtendedGraph] = $ExtendedGraphOptions;

(**************************************************************************************************)

Unprotect[Graph];
(* IfSyntaxInfo[
  SyntaxInformation[Graph] = ReplaceOptions[SyntaxInformation[Graph], "OptionNames" -> $extendedGraphSymbolNames];
];
 *)
HoldP[g:Graph[___]] /; MemberQ[Uneval @ g, $extendedGraphOptionRulePattern] && $notIntercepted :=
  Block[{$notIntercepted = False}, interceptedGraphConstructor[g]];
Protect[Graph];

$extendedGraphOptionSymbols2 = App[$extendedGraphOptionSymbols, AnnotationRules];

splitUserGraphOptions[options___Rule] := Scope[
  options = {options};
  extOptions = DedupBy[TakeOptions[options, $extendedGraphOptionSymbols], F];
  options = Map[optionFixup] @ DropOptions[options, $extendedGraphOptionSymbols2];
  {options, checkGraphAnnotations @ extOptions}
];

(**************************************************************************************************)

PrivateFunction[interceptedGraphConstructor, toPlainGraphConstructorOptions]

SetHoldAllComplete[interceptedGraphConstructor];

interceptedGraphConstructor[Graph[Shortest[args__], options__Rule]] := Scope[
  {newOptions, extOptions} = splitUserGraphOptions[options];
  result = Graph[args, Sequence @@ newOptions];
  (* todo: forgoe Annotate and just do the combination ourselves *)
  If[!GraphQ[result], result = makeNewGraph[args, newOptions]];
  If[!GraphQ[result], ReturnFailed[]];
  Annotate[result, extOptions]
];

(* since this is for fresh graphs, which don't have to worry about merging with existing AnnotationRules *)
toPlainGraphConstructorOptions[] :=
  {AnnotationRules -> {"GraphProperties" -> {GraphPlottingFunction -> ExtendedGraphPlottingFunction}}};

toPlainGraphConstructorOptions[options__Rule] := Scope[
  {plainOptions, extOptions} = splitUserGraphOptions[options];
  AppTo[extOptions, GraphPlottingFunction -> ExtendedGraphPlottingFunction];
  App[plainOptions, AnnotationRules -> {"GraphProperties" -> extOptions}]
];

(**************************************************************************************************)

PrivateFunction[makeNewGraph]

makeNewGraph[graph_Graph ? GraphQ, newOptions_List] :=
  Graph[VertexList @ graph, EdgeList @ graph, Sequence @@ newOptions, Sequence @@ Options @ graph];

makeNewGraph[___] := $Failed;

(* these compensate for a weird extra level of list that Graph adds *)
optionFixup = Case[
  Rule[GraphLayout, {"Dimension" -> d_}]          := Rule[LayoutDimension, d];
  Rule[GraphHighlight, r_]                        := Rule[GraphRegionHighlight, r];
  Rule[VertexSize, r:{__Rule}]                    := Rule[VertexSize, Assoc @ r];
  Rule[sym:(VertexLabels | EdgeLabels), l_List | l_Rule] := Rule[sym, If[MatchQ[l, {_Hold | _Assoc}], F @ l, Hold @ l]];
  Rule[sym:(EdgeStyle|VertexStyle), val_]         := Rule[sym, toDirective[val]];
  Rule[VertexShapeFunction, assoc_Assoc]          := Rule[VertexShapeFunction, toShape /@ assoc];
  Rule[VertexShapeFunction, rule_Rule]            := Rule[VertexShapeFunction, Hold[rule]];
  Rule[sym:(HighlightStyle|VertexLabelStyle|EdgeLabelStyle), elem_] := Rule[sym, toDirective[elem]];
  other_                                          := other;
];

(* TODO: compute sizes here so that graph layout knows about them *)
toShape[g_Graph] := ExtendedGraphPlot @ g;
toShape[other_] := other;

interceptedGraphConstructor[e_] := e;

(**************************************************************************************************)

PublicFunction[ExtendedGraphQ]

ExtendedGraphQ[g_Graph ? GraphQ] :=
  Count[AnnotationValue[g, $extendedGraphOptionSymbols], $Failed] =!= Len[$extendedGraphOptionSymbols];

ExtendedGraphQ[_] := False;

(**************************************************************************************************)

PrivateVariable[$GraphThemeData]

$fontThemeOpts = {VertexLabelBaseStyle -> $MathLabelStyle, EdgeLabelBaseStyle -> $CardinalLabelStyle};

$GraphThemeData = <|
  None -> {},
  "Fonts" :> $fontThemeOpts
|>;

(**************************************************************************************************)

PrivateSpecialFunction[DefineGraphTheme]

(* TODO: eventually have graph themes be able to chain other graph themes dynamically *)
DefineGraphTheme[name_Str -> parent_Str, opts___Rule] :=
  $GraphThemeData[name] = Join[{opts}, $GraphThemeData[parent]];

DefineGraphTheme[name_Str, opts___Rule] :=
  $GraphThemeData[name] = List[opts];

_DefineGraphTheme := $Unreachable;

