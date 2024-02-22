$arrowheadSizePattern = Alt[
  _ ? NumericQ,
  $SymbolicSizePattern,
  Scaled[(_ ? NumericQ) | $SymbolicSizePattern],
  PointSize[_ ? NumericQ],
  _Assoc,
  Auto | None
];

$vertexAnnotationsPattern = Alt[
  Assoc[RepeatedNull[_Str -> _List]],
  None
];

$edgeAnnotationsPattern = Alt[
  Assoc[RepeatedNull[_Str -> _Assoc]],
  None
];
$layoutDimensionPattern = Alt[
  Auto, None, 1, 2, 3
];

$graphMetricPattern = Alt[
  Auto, "Euclidean", "Chessboard", _QuadraticFormObject, _List, _Int, _ ? MightEvaluateWhenAppliedQ
];

$viewOptionKeysPattern = Alt[
  ViewPoint, ViewCenter, ViewVertical, ViewVector, ViewRotation, ViewMatrix, ViewProjection, ViewAngle, "ShrinkWrap", SphericalRegion
];

$viewOptionsRulePattern = Auto | {RepeatedNull[$viewOptionKeysPattern -> _]};

$extendedGraphOptionPatterns = <|
  ArrowheadSize -> $arrowheadSizePattern,
  VertexAnnotations -> $vertexAnnotationsPattern,
  EdgeAnnotations -> $edgeAnnotationsPattern,
  LayoutDimension -> $layoutDimensionPattern,
  GraphMetric -> $graphMetricPattern,
  ViewOptions -> $viewOptionsRulePattern
|>;

(**************************************************************************************************)

PrivateFunction[checkGraphAnnotations]

checkGraphAnnotations[rules_List] := Map[checkGraphAnnotationRule, rules];

General::badextopt = "The extended option `` -> `` is invalid and will be ignored."

checkGraphAnnotationRule[key_ -> value_] /; And[
  KeyQ[$extendedGraphOptionPatterns, key],
  !MatchQ[value, $extendedGraphOptionPatterns @ key]] := (
    Message[Graph::badextopt, key, value];
    Nothing
  );

checkGraphAnnotationRule[rule_] := rule;

(**************************************************************************************************)

PrivateFunction[AttachGraphOptions]

AttachGraphOptions[graph_Graph ? GraphQ, opts___] := Scope[
  result = Graph[graph, opts];
  If[GraphQ[result], result, makeNewGraph[graph, {opts}]]
];

(**************************************************************************************************)

PrivateFunction[LookupExtendedOption]

LookupExtendedOption[graph_, keys_List] :=
  MapThread[
    If[#1 === $Failed, #2, #1]&,
    {AnnotationValue[graph, keys], Lookup[$extendedGraphOptionsRules, keys]}
  ];

LookupExtendedOption[graph_, key_Symbol | key_CustomGraphAnnotation] :=
  LookupAnnotation[graph, key, Lookup[$extendedGraphOptionsRules, key]];

LookupExtendedOption[key_][graph_] := LookupExtendedOption[graph, key];

(**************************************************************************************************)

PrivateFunction[ExtendedGraphAnnotations]

ExtendedGraphAnnotations[graph_] :=
  Normal @ Decases[$Failed] @ AssocThread[
    $extendedGraphOptionSymbols,
    AnnotationValue[graph, $extendedGraphOptionSymbols]
  ];

(**************************************************************************************************)

PrivateFunction[ExtractExtendedGraphOptions]

ExtractExtendedGraphOptions[graph_Graph] := Scope[
  opts = Options @ graph;
  annoRules = Lookup[opts, AnnotationRules, {}];
  graphProps = Lookup[annoRules, "GraphProperties", {}];
  Join[DropOptions[opts, AnnotationRules], graphProps]
]