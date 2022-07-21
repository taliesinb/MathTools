PublicOption[VertexLayout, VertexOverlapResolution, GraphOrigin]

PublicOption[GraphMetric]

PublicOption[ArrowheadShape, ArrowheadSize, ArrowheadStyle, ArrowheadPosition, TwoWayStyle]

PublicOption[VertexColorFunction, EdgeColorFunction, VertexColorRules, EdgeColorRules, RegionColorRules]

PublicOption[VertexTooltips, VertexClickFunction, EdgeTooltips]

PublicOption[CardinalColors, CardinalColorRules, CardinalColorFunction]

PublicOption[VertexLabelPosition, VertexLabelSpacing, VertexLabelBaseStyle, VertexLabelOrientation]

PublicOption[VertexFontSize, VertexBackground]

PublicOption[EdgeLabelPosition, EdgeLabelSpacing, EdgeLabelBaseStyle, EdgeLabelOrientation]

PublicOption[CollapseMultiedges]

PublicOption[EdgeLength]

PublicOption[FrameFade]

PublicOption[VisibleCardinals, ViewRegion, ViewOptions, ViewRotation, LayoutDimension, AdditionalImagePadding, ExtendImagePadding]

PublicOption[CoordinateTransformFunction, CoordinateRotation]

PublicOption[LabelCardinals, AspectRatioClipping, PrologFunction, UseAbsoluteSizes, SelfLoopRadius, MultiEdgeDistance, PackingSpacing]

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
  ArrowheadPosition                   -> Automatic,
  ArrowheadShape                      -> Automatic,
  ArrowheadSize                       -> 15,
  ArrowheadStyle                      -> Automatic,
  AspectRatioClipping                 -> True,
  CardinalColorFunction               -> None,
  CardinalColorRules                  -> None,
  CardinalColors                      -> Automatic,
  Cardinals                           -> Automatic,
  CollapseMultiedges                  -> False,
  CoordinateRotation                  -> None,
  CoordinateTransformFunction         -> None,
  CustomGraphAnnotation[_String]      -> None,
  EdgeAnnotations                     -> None,
  EdgeColorFunction                   -> None,
  EdgeColorRules                      -> None,
  EdgeLabelBaseStyle                  -> None,
  EdgeLabelPosition                   -> Automatic,
  EdgeLabelOrientation                -> Automatic,
  EdgeLabelSpacing                    -> 0,
  EdgeSetback                         -> Automatic,
  EdgeThickness                       -> Automatic,
  EdgeTooltips                        -> None,
  EdgeLength                          -> None,
  EpilogFunction                      -> None,
  ExtendImagePadding                  -> True,
  FrameFade                           -> None,
  GraphLegend                         -> None,
  GraphMetric                         -> Automatic,
  GraphOrigin                         -> None,
  GraphPlottingFunction               -> None,
  GraphRegionHighlight                -> None,
  GraphTheme                          -> None,
  LabelCardinals                      -> False,
  LayoutDimension                     -> Automatic,
  MultiEdgeDistance                   -> Automatic,
  PackingSpacing                      -> Automatic,
  PeripheralVertices                  -> None,
  PrologFunction                      -> None,
  RegionColorRules                    -> None,
  SelfLoopRadius                      -> Automatic,
  TwoWayStyle                         -> Automatic,
  UseAbsoluteSizes                    -> Automatic,
  VertexAnnotations                   -> None,
  VertexBackground                    -> White,
  VertexClickFunction                 -> None,
  VertexColorFunction                 -> None,
  VertexColorRules                    -> None,
  VertexCoordinateFunction            -> None,
  VertexCoordinateRules               -> None,
  VertexFontSize                      -> None,
  VertexLabelBaseStyle                -> None,
  VertexLabelPosition                 -> Automatic,
  VertexLabelSpacing                  -> 0,
  VertexLabelOrientation              -> Automatic,
  VertexLayout                        -> Automatic,
  VertexOverlapResolution             -> None,
  VertexTooltips                      -> None,
  ViewOptions                         -> Automatic,
  ViewRegion                          -> All,
  VisibleCardinals                    -> All
};

PrivateVariable[$extendedGraphOptionSymbols]

$extendedGraphOptionSymbols = Keys @ $extendedGraphOptionsRules;

$extendedGraphOptionSymbolPattern = Alternatives @@ $extendedGraphOptionSymbols;

$extendedGraphOptionRulePattern = Rule[$extendedGraphOptionSymbolPattern, _];

(**************************************************************************************************)

$notIntercepted = True;

Graph;
SyntaxInformation[Graph];
Options[Graph];

$ignoredGraphOptionsSymbols = Alternatives[
  AlignmentPoint, AnnotationRules, AspectRatio, Axes, AxesLabel,
  AxesOrigin, AxesStyle, Background, BaseStyle, ContentSelectable,
  DirectedEdges, EdgeCapacity, EdgeCost, EdgeWeight, Editable,
  FormatType, FrameTicks, FrameTicksStyle,
  GraphHighlight, GraphRoot, GraphStyle, GridLines, GridLinesStyle,
  ImageMargins, PerformanceGoal, PlotRegion, PlotTheme,
  Properties, RotateLabel, Ticks, TicksStyle, VertexCapacity,
  VertexShape, VertexWeight
];

$fullGraphOptions = DeleteCases[Sort @ JoinOptions[Graph, $extendedGraphOptionsRules], $ignoredGraphOptionsSymbols -> _];

(**************************************************************************************************)

PublicVariable[$ExtendedGraphOptions, $ExtendedGraphOptionSymbols]

$ExtendedGraphOptions = Cases[$fullGraphOptions, HoldPattern[_Symbol -> _]];
$ExtendedGraphOptionSymbols = Keys @ $ExtendedGraphOptions;
$extendedGraphSymbolNames = Map[SymbolName, $ExtendedGraphOptionSymbols];

Options[ExtendedGraph] = $ExtendedGraphOptions;

(**************************************************************************************************)

Unprotect[Graph];
SyntaxInformation[Graph] = ReplaceOptions[SyntaxInformation[Graph], "OptionNames" -> $extendedGraphSymbolNames];
HoldPattern[g:Graph[___]] /; MemberQ[Unevaluated @ g, $extendedGraphOptionRulePattern] && $notIntercepted :=
  Block[{$notIntercepted = False}, interceptedGraphConstructor[g]];
Protect[Graph];

$extendedGraphOptionSymbols2 = Append[$extendedGraphOptionSymbols, AnnotationRules];

splitUserGraphOptions[options___Rule] := Scope[
  options = {options};
  extOptions = DeleteDuplicatesBy[TakeOptions[options, $extendedGraphOptionSymbols], First];
  options = Map[optionFixup] @ DeleteOptions[options, $extendedGraphOptionSymbols2];
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
  AppendTo[extOptions, GraphPlottingFunction -> ExtendedGraphPlottingFunction];
  Append[plainOptions, AnnotationRules -> {"GraphProperties" -> extOptions}]
];

(**************************************************************************************************)

PrivateFunction[makeNewGraph]

makeNewGraph[graph_Graph ? GraphQ, newOptions_List] :=
  Graph[VertexList @ graph, EdgeList @ graph, Sequence @@ newOptions, Sequence @@ Options @ graph];

makeNewGraph[___] := $Failed;

(* these compensate for a weird extra level of list that Graph adds *)
optionFixup = Case[
  Rule[GraphLayout, {"Dimension" -> d_}]          := Rule[LayoutDimension, d];
  Rule[VertexSize, r:{__Rule}]                    := Rule[VertexSize, Association @ r];
  Rule[sym:(VertexLabels | EdgeLabels), l_List | l_Rule] := Rule[sym, If[MatchQ[l, {_Hold | _Association}], First @ l, Hold @ l]];
  Rule[sym:(EdgeStyle|VertexStyle), val_]         := Rule[sym, toDirective[val]];
  Rule[VertexShapeFunction, assoc_Association]    := Rule[VertexShapeFunction, toShape /@ assoc];
  Rule[VertexShapeFunction, rule_Rule]            := Rule[VertexShapeFunction, Hold[rule]];
  Rule[sym:(GraphHighlightStyle|VertexLabelStyle|EdgeLabelStyle), elem_] := Rule[sym, toDirective[elem]];
  other_                                          := other;
];

(* TODO: compute sizes here so that graph layout knows about them *)
toShape[g_Graph] := ExtendedGraphPlot @ g;
toShape[other_] := other;

interceptedGraphConstructor[e_] := e;

(**************************************************************************************************)

PublicFunction[ExtendedGraphQ]

ExtendedGraphQ[g_Graph ? GraphQ] :=
  Count[AnnotationValue[g, $extendedGraphOptionSymbols], $Failed] =!= Length[$extendedGraphOptionSymbols];

ExtendedGraphQ[_] := False;

(**************************************************************************************************)

PrivateVariable[$GraphThemeData]

$fontThemeOpts = {VertexLabelBaseStyle -> $MathLabelStyle, EdgeLabelBaseStyle -> $CardinalLabelStyle};

$GraphThemeData = <|
  None -> {},
  "Fonts" :> $fontThemeOpts
|>;

(**************************************************************************************************)

PublicFunction[DefineGraphTheme]

(* TODO: eventually have graph themes be able to chain other graph themes dynamically *)
DefineGraphTheme[name_String -> parent_String, opts___Rule] :=
  $GraphThemeData[name] = Join[{opts}, $GraphThemeData[parent]];

DefineGraphTheme[name_String, opts___Rule] :=
  $GraphThemeData[name] = List[opts];

_DefineGraphTheme := $Unreachable;

