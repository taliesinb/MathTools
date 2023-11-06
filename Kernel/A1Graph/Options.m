$arrowheadSizePattern = Alternatives[
  _ ? NumericQ,
  $SymbolicSizePattern,
  Scaled[(_ ? NumericQ) | $SymbolicSizePattern],
  PointSize[_ ? NumericQ],
  _Assoc,
  Automatic | None
];

$vertexAnnotationsPattern = Alternatives[
  Assoc[RepeatedNull[_Str -> _List]],
  None
];

$edgeAnnotationsPattern = Alternatives[
  Assoc[RepeatedNull[_Str -> _Assoc]],
  None
];
$layoutDimensionPattern = Alternatives[
  Automatic, None, 1, 2, 3
];

$graphMetricPattern = Alternatives[
  Automatic, "Euclidean", "Chessboard", _QuadraticFormObject, _List, _Int, _ ? MightEvaluateWhenAppliedQ
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

PrivateFunction[checkGraphAnnotations]

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
  Normal @ DeleteCases[$Failed] @ AssociationThread[
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