PackageExport["Signed"]

SetUsage @ "
Signed is an option to various graph utility functions.
"


(**************************************************************************************************)

PackageScope["$extendedGraphOptionsRules"]

$extendedGraphOptionsRules = {
  AdditionalImagePadding              -> None,
  ArrowheadPosition                   -> Automatic,
  ArrowheadShape                      -> Automatic,
  ArrowheadSize                       -> Automatic,
  ArrowheadStyle                      -> Automatic,
  AspectRatioClipping                 -> True,
  CardinalColorFunction               -> None,
  CardinalColorRules                  -> None,
  CardinalColors                      -> Automatic,
  Cardinals                           -> Automatic,
  CoordinateRotation                  -> None,
  CoordinateTransformFunction         -> None,
  CustomGraphAnnotation[_String]      -> None,
  EdgeAnnotations                     -> None,
  EdgeColorFunction                   -> None,
  EdgeColorRules                      -> None,
  EdgeLabelBaseStyle                  -> None,
  EdgeLabelPosition                   -> Top,
  EdgeLabelSpacing                    -> 0,
  EdgeSetback                         -> Automatic,
  EdgeThickness                       -> Automatic,
  EdgeTooltips                        -> None,
  EdgeLength                          -> None,
  EpilogFunction                      -> None,
  ExtendedGraphLayout                 -> Automatic,
  ExtendImagePadding                  -> True,
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
  VertexLabelPosition                 -> Top,
  VertexLabelSpacing                  -> 0,
  VertexLayout                        -> None,
  VertexOverlapResolution             -> None,
  VertexTooltips                      -> None,
  ViewOptions                         -> Automatic,
  ViewRegion                          -> All,
  VisibleCardinals                    -> All
};

PackageScope["$extendedGraphOptionSymbols"]

$extendedGraphOptionSymbols = Keys @ $extendedGraphOptionsRules;

$extendedGraphOptionSymbolPattern = Alternatives @@ $extendedGraphOptionSymbols;

$extendedGraphOptionRulePattern = Rule[$extendedGraphOptionSymbolPattern, _];

(**************************************************************************************************)

$notIntercepted = True;

Graph;
SyntaxInformation[Graph];
Options[Graph];

$fullGraphOptions = Sort @ JoinOptions[Graph, $extendedGraphOptionsRules];
$extendedGraphSymbolNames = Map[SymbolName, Select[SymbolQ] @ Keys @ $fullGraphOptions];

(**************************************************************************************************)

PackageExport["$ExtendedGraphOptions"]

$ExtendedGraphOptions = Cases[$fullGraphOptions, HoldPattern[_Symbol -> _]];

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
  (* so the kernel will randomly mess with and rewrite GraphLayout, and hence ExtendedGraphLayout lets us avoid this,
  and override it. i used to rewrite GraphLayout to *become* ExtendedGraphLayout so users did not have to understand
  this, but the kernel would then take over the user stuff sometimes when graphs were reconstructed from existing graphs,
  so disabled this here *)
(*   If[!MemberQ[options, ExtendedGraphLayout -> _] && MemberQ[options, GraphLayout -> Except[{"Dimension" -> _}]],
    options = Replace[options, Rule[GraphLayout, l_] :> Rule[ExtendedGraphLayout, l], {1}]];
 *)
  extOptions = DeleteDuplicatesBy[TakeOptions[options, $extendedGraphOptionSymbols], First];
  options = Map[optionFixup] @ DeleteOptions[options, $extendedGraphOptionSymbols2];
  {options, checkGraphAnnotations @ extOptions}
];

(**************************************************************************************************)

PackageScope["interceptedGraphConstructor"]

SetHoldAllComplete[interceptedGraphConstructor];

interceptedGraphConstructor[Graph[Shortest[args__], options__Rule]] := Scope[
  {newOptions, extOptions} = splitUserGraphOptions[options];
  result = Graph[args, Sequence @@ newOptions];
  (* todo: forgoe Annotate and just do the combination ourselves *)
  If[!GraphQ[result], result = makeNewGraph[args, newOptions]];
  If[!GraphQ[result], ReturnFailed[]];
  Annotate[result, extOptions]
];

(**************************************************************************************************)

PackageScope["makeNewGraph"]

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

PackageScope["$simpleGraphOptions"]
PackageScope["$simpleGraphOptionRules"]

$simpleGraphOptionRules = JoinOptions[
  EdgeLabels -> None, GraphLayout -> Automatic, ImagePadding -> None,
  ImageSize -> Automatic, VertexCoordinates -> Automatic,
  VertexLabels -> None, VertexSize -> Automatic,
  VertexStyle -> Automatic, EdgeStyle -> Automatic,
  VertexShapeFunction -> Automatic, EdgeShapeFunction -> Automatic, PlotLabel -> None,
  GraphHighlightStyle -> Automatic, VertexLabelStyle -> Automatic, EdgeLabelStyle -> Automatic,
  Epilog -> {}, Prolog -> {}, Frame -> None, FrameStyle -> Automatic, BaselinePosition -> Automatic,
  FrameLabel -> None, PlotRange -> Automatic,
  DeleteCases[$extendedGraphOptionsRules, ExtendedGraphPlottingFunction -> _]
]

$simpleGraphOptions = Keys @ $simpleGraphOptionRules;

(**************************************************************************************************)

PackageExport["ExtendedGraphQ"]

ExtendedGraphQ[g_Graph ? GraphQ] :=
  Count[AnnotationValue[g, $extendedGraphOptionSymbols], $Failed] =!= Length[$extendedGraphOptionSymbols];

ExtendedGraphQ[_] := False;

(**************************************************************************************************)

PackageExport["$GraphThemeData"]

$fontThemeOpts = {VertexLabelBaseStyle -> $MathLabelStyle, EdgeLabelBaseStyle -> $CardinalLabelStyle};

$GraphThemeData = <|
  None -> {},
  "Fonts" :> $fontThemeOpts
|>;
