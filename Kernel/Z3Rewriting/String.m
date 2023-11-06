(**************************************************************************************************)

PublicFunction[StringPlot]

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

StringPlot[s_Str, sz_:6] :=
  FadedMeshImage[List @ ToRGB @ Map[charToColor, Characters @ s], sz]

(**************************************************************************************************)

PublicFunction[StringRewritingSystem]

StringRewritingSystem[rules_] := Scope[
  constructRewritingSystem["String", rules]
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

PublicFunction[StringReplaceListOperator]

StringReplaceListOperator[rules_][str_] :=
  StringReplaceList[str, rules];

(**************************************************************************************************)

PublicFunction[StringLabeledReplaceList]

StringLabeledReplaceList[str_, {rule_}] :=
  StringLabeledReplaceList[str, rule, None];

StringLabeledReplaceList[str_, rules_List] :=
  Catenate @ MapIndexed[
    StringLabeledReplaceList[str, #1, P1 @ #2]&,
    rules
  ];

StringLabeledReplaceList[str_Str, rule_, matchIndex_:None] := Scope[
  Map[
    span |-> Labeled[
      StringReplacePart[str, ochunk = StringReplace[ichunk = StringTake[str, span], rule], span],
      RewriteForm[
        StringRegionalStateForm[ichunk, span],
        If[StringLength[ochunk] == StringLength[ichunk], StringRegionalStateForm[ochunk, span], ochunk],
        matchIndex
      ]
    ],
    StringPosition[str, P1 @ rule]
  ]
];

StringLabeledReplaceList[rule_][str_] := StringLabeledReplaceList[str, rule];
  
stringTokens[str_Str, All] :=
  stringTokens[str, {1, StringLength @ str}];

stringTokens[str_Str, span:{i_, j_}] :=
  Trans[
    Characters @ StringTake[str, span],
    Range[i, j]
  ];
