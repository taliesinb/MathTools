$arrowheadSizePattern = Alternatives[
  _ ? NumericQ,
  $SymbolicSizePattern,
  Scaled[(_ ? NumericQ) | $SymbolicSizePattern],
  PointSize[_ ? NumericQ],
  _Association,
  Automatic | None
];

$vertexAnnotationsPattern = Alternatives[
  Association[RepeatedNull[_String -> _List]],
  None
];

$edgeAnnotationsPattern = Alternatives[
  Association[RepeatedNull[_String -> _Association]],
  None
];
$layoutDimensionPattern = Alternatives[
  Automatic, None, 2, 3
];

$graphMetricPattern = Alternatives[
  Automatic, "Euclidean", "Chessboard", _QuadraticFormObject, _List, _Integer, _ ? System`Private`MightEvaluateWhenAppliedQ
];

$viewOptionKeysPattern = Alternatives[
  ViewPoint, ViewCenter, ViewVertical, ViewVector, ViewRotation, ViewMatrix, ViewProjection, ViewAngle, "ShrinkWrap", SphericalRegion
];

$viewOptionsRulePattern = Automatic | {RepeatedNull[$viewOptionKeysPattern -> _]};

$extendedGraphOptionPatterns = <|
  ArrowheadSize -> $arrowheadSizePattern,
  VertexAnnotations -> $vertexAnnotationsPattern,
  EdgeAnnotations -> $edgeAnnotationsPattern,
  LayoutDimension -> $layoutDimensionPattern,
  GraphMetric -> $graphMetricPattern,
  ViewOptions -> $viewOptionsRulePattern
|>;

(**************************************************************************************************)

PackageScope["checkGraphAnnotations"]

checkGraphAnnotations[rules_List] := Map[checkGraphAnnotationRule, rules];

General::badextopt = "The extended option `` -> `` is invalid and will be ignored."

checkGraphAnnotationRule[key_ -> value_] /; And[
  KeyExistsQ[$extendedGraphOptionPatterns, key],
  !MatchQ[value, $extendedGraphOptionPatterns @ key]] := (
    Message[Graph::badextopt, key, value];
    Nothing
  );

checkGraphAnnotationRule[rule_] := rule;

(**************************************************************************************************)

PackageExport["AttachGraphOptions"]

AttachGraphOptions[graph_Graph ? GraphQ, opts___] := Scope[
  result = Graph[graph, opts];
  If[GraphQ[result], result, makeNewGraph[graph, {opts}]]
];

(**************************************************************************************************)

PackageExport["LookupExtendedOption"]

LookupExtendedOption[graph_, keys_List] :=
  MapThread[
    If[#1 === $Failed, #2, #1]&,
    {AnnotationValue[graph, keys], Lookup[$extendedGraphOptionsRules, keys]}
  ];

LookupExtendedOption[graph_, key_Symbol | key_CustomGraphAnnotation] :=
  LookupAnnotation[graph, key, Lookup[$extendedGraphOptionsRules, key]];

(**************************************************************************************************)

PackageScope["ExtendedGraphAnnotations"]

ExtendedGraphAnnotations[graph_] :=
  Normal @ DeleteCases[$Failed] @ AssociationThread[
    $extendedGraphOptionSymbols,
    AnnotationValue[graph, $extendedGraphOptionSymbols]
  ];