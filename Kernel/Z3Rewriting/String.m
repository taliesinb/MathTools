(**************************************************************************************************)

PackageExport["StringPlot"]

charToColor = Replace[{
  "r" -> $Red, "g" -> $Green, "b" -> $Blue,
  "R" -> $Teal, "G" -> $Pink, "B" -> $Orange,
  "a" -> $Red, "b" -> $Blue, "c" -> $Green, "d" -> $Pink, "e" -> $Orange, "f" -> $Teal,
  "A" -> $Teal, "B" -> $Orange, "C" -> $Pink, "D" -> $Green, "E" -> $Blue, "F" -> $Red,
  "x" -> $Teal, "y" -> $Orange, "z" -> $Pink,
  "0" -> White, "1" -> GrayLevel[0.3], "3" -> $Red, "4" -> $Blue, "5" -> $Green, "6" -> $Orange,
  "\[EmptySquare]" -> White, "\[FilledSquare]" -> GrayLevel[0.3],
  " " -> White, "." -> GrayLevel[0.6],
  _ -> Pink
}];

StringPlot[s_String, sz_:6] :=
  FadedMeshImage[List @ ToRGB @ Map[charToColor, Characters @ s], sz]

(**************************************************************************************************)

PackageExport["StringRewritingSystem"]

StringRewritingSystem[rules_] := Scope[
  constructRewritingSystsem["String", rules]
]

declareRewritingSystemDispatch["String", stringRewritingSystemProperty]

stringRewritingSystemProperty[data_, "CayleyFunction", opts___Rule] := Scope[
  UnpackAssociation[data, rules];
  UnpackStringOptions[{opts}, True, labeled];
  If[labeled,
    StringLabeledReplaceList[rules],
    StringReplaceListOperator[rules]
  ]
];

(**************************************************************************************************)

PackageExport["StringReplaceListOperator"]

StringReplaceListOperator[rules_][str_] :=
  StringReplaceList[str, rules];

(**************************************************************************************************)

PackageExport["StringLabeledReplaceList"]

StringLabeledReplaceList[str_, {rule_}] :=
  StringLabeledReplaceList[str, rule, None];

StringLabeledReplaceList[str_, rules_List] :=
  Catenate @ MapIndexed[
    StringLabeledReplaceList[str, #1, First @ #2]&,
    rules
  ];

StringLabeledReplaceList[str_String, rule_, matchIndex_:None] := Scope[
  Map[
    span |-> Labeled[
      StringReplacePart[str, ochunk = StringReplace[ichunk = StringTake[str, span], rule], span],
      RewriteForm[
        StringRegionalStateForm[ichunk, span],
        StringRegionalStateForm[ochunk, span],
        matchIndex
      ]
    ],
    StringPosition[str, First @ rule]
  ]
];

StringLabeledReplaceList[rule_][str_] := StringLabeledReplaceList[str, rule];
  
stringTokens[str_String, All] :=
  stringTokens[str, {1, StringLength @ str}];

stringTokens[str_String, span:{i_, j_}] :=
  Transpose[{
    Characters @ StringTake[str, span],
    Range[i, j]
  }];
