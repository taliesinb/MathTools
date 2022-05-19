(**************************************************************************************************)

PackageExport["CircularStringRewritingSystem"]

CircularStringRewritingSystem[rules_] := Scope[
  constructRewritingSystsem["CircularString", rules]
]

declareRewritingSystemDispatch["CircularString", circularStringRewritingSystemProperty]

circularStringRewritingSystemProperty[data_, "CayleyFunction", opts___Rule] := Scope[
  UnpackAssociation[data, rules];
  UnpackStringOptions[{opts}, True, labeled];
  If[labeled,
    CircularStringLabeledReplaceList[rules],
    CircularStringReplaceListOperator[rules]
  ]
];

(**************************************************************************************************)

PackageExport["CircularStringReplaceListOperator"]

CircularStringReplaceListOperator[rules_][str_] :=
  Part[CircularStringLabeledReplaceList[str, rules], All, 1];

(**************************************************************************************************)

PackageExport["CircularStringLabeledReplaceList"]

CircularStringLabeledReplaceList[str_, {rule_}] :=
  CircularStringLabeledReplaceList[str, rule, None];

CircularStringLabeledReplaceList[str_, rules_List] :=
  Catenate @ MapIndexed[
    CircularStringLabeledReplaceList[str, #1, First @ #2]&,
    rules
  ];

CircularStringLabeledReplaceList[str_String, rule_, matchIndex_:None] := Scope[
  str2 = StringJoin[str, str]; len = StringLength @ str;
  spans = StringPosition[str2, First @ rule];
  spans = {#, Mod[#, len, 1]}& /@ spans;
  spans = DeleteDuplicatesBy[spans, Last];
  VectorApply[
    {span, modSpan} |-> Labeled[
      circularStringReplacePart[str, ochunk = StringReplace[ichunk = StringTake[str2, span], rule], modSpan],
      RewriteForm[
        StringRegionalStateForm[ichunk, modSpan],
        StringRegionalStateForm[ochunk, modSpan],
        matchIndex
      ]
    ],
    spans
  ]
];

CircularStringLabeledReplaceList[rule_][str_] := CircularStringLabeledReplaceList[str, rule];

circularStringReplacePart[str_, new_, {i_, j_}] /; j < i :=
  StringReplacePart[
    StringReplacePart[str, StringTake[new, 1 + len - i], {i, len}],
    StringTake[new, -j],
    {1, j}
  ];

circularStringReplacePart[str_, new_, span_] := StringReplacePart[str, new, span];
