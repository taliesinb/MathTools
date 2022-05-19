PackageExport["Signed"]

SetUsage @ "
Signed is an option to various graph utility functions.
"


(**************************************************************************************************)

PackageScope["$extendedGraphOptionsRules"]

$extendedGraphOptionsRules = {
  GraphPlottingFunction               -> None,
  GraphRegionHighlight                -> None,
  GraphLegend                         -> None,
  ArrowheadSize                       -> Automatic,
  ArrowheadStyle                      -> Automatic,
  ArrowheadShape                      -> Automatic,
  ArrowheadPosition                   -> Automatic,
  EdgeSetback                         -> Automatic,
  VertexColorFunction                 -> None,
  EdgeColorFunction                   -> None,
  VertexAnnotations                   -> None,
  EdgeAnnotations                     -> None,
  LayoutDimension                     -> Automatic,
  ExtendedGraphLayout                 -> Automatic,
  GraphMetric                         -> Automatic,
  GraphOrigin                         -> None,
  Cardinals                           -> Automatic,
  CardinalColors                      -> Automatic,
  CardinalColorRules                  -> None,
  CardinalColorFunction               -> None,
  VisibleCardinals                    -> All,
  ViewOptions                         -> Automatic,
  LabelCardinals                      -> False,
  CoordinateTransformFunction         -> None,
  ViewRegion                          -> All,
  AdditionalImagePadding              -> None,
  ExtendImagePadding                  -> True,
  AspectRatioClipping                 -> True,
  EdgeThickness                       -> Automatic,
  VertexLayout                        -> None,
  VertexOverlapResolution             -> None,
  VertexCoordinateRules               -> None,
  VertexCoordinateFunction            -> None,
  VertexColorRules                    -> None,
  VertexTooltips                      -> None,
  VertexClickFunction                 -> None,
  EdgeTooltips                        -> None,
  EdgeColorRules                      -> None,
  RegionColorRules                    -> None,
  PrologFunction                      -> None,
  EpilogFunction                      -> None,
  UseAbsoluteSizes                    -> Automatic,
  SelfLoopRadius                      -> Automatic,
  MultiEdgeDistance                   -> Automatic,
  PackingSpacing                      -> Automatic,
  CustomGraphAnnotation[_String]      -> None,
  VertexLabelPosition                 -> Top,
  EdgeLabelPosition                   -> Top,
  VertexLabelSpacing                  -> 0,
  EdgeLabelSpacing                    -> 0,
  VertexLabelBaseStyle                -> None,
  EdgeLabelBaseStyle                  -> None,
  GraphTheme                          -> None,
  VertexFontSize                      -> None,
  VertexBackground                    -> White,
  PeripheralVertices                  -> None,
  CoordinateRotation                  -> None
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
  Rest @ $extendedGraphOptionsRules
]

$simpleGraphOptions = Keys @ $simpleGraphOptionRules;

(**************************************************************************************************)

PackageExport["ExtendedGraphQ"]

ExtendedGraphQ[g_Graph ? GraphQ] :=
  Count[AnnotationValue[g, $extendedGraphOptionSymbols], $Failed] =!= Length[$extendedGraphOptionSymbols];

ExtendedGraphQ[_] := False;

