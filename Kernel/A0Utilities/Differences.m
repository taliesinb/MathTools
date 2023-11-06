PublicObject[ExpressionDifference]

PrivateHead[$HeadCrumb, $ArgsCrumb, $KeysCrumb, $ArrayCrumb]

DefineStandardTraditionalForm[le:ExpressionDifference[{___Int}, _List, _] :> lepBoxes[le]]

lepBoxes[ed:ExpressionDifference[pos_List, crumbs_List, diff_]] :=
  GridBox[
    List @ Map[ToBoxes, Append[crumbs, diff]],
    FrameStyle -> $LightGray,
    Dividers -> All,
    ColumnSpacings -> 2,
    GridBoxDividers -> {"Columns" -> {False, {True}, False}, "Rows" -> {{}}},
    FrameMargins -> {{5, 5}, {3, 10}}
  ];

Format[ExpressionDifference[pos_List, crumbs_List, diff_], OutputForm] := expressionDifferenceOutputForm[pos, crumbs, diff];

expressionDifferenceOutputForm[pos_, crumbs_, diff_] :=
  Row[Join[Riffle[crumbs, " | "], {" --> ", diff}]] /. HoldComplete[h_] :> HoldForm[h];

DefineStandardTraditionalForm[{
  $HeadCrumb          :> crumbSubBox["?", "head"],
  $ArgsCrumb[h_]      :> crumbSubBox[h, "args"],
  $ArgsCrumb[h_, n_]  :> crumbSubBox[h, n],
  $ArrayCrumb[]       :> StyleBox["\[DottedSquare]", Bold],
  $ArrayCrumb[n_]     :> crumbSubBox["\[DottedSquare]", n],
  $KeysCrumb[h_]      :> crumbSubBox[h, "keys"],
  $KeysCrumb[h_, k_]  :> crumbSubBox[h, k]
}];

Format[$HeadCrumb, OutputForm] := Underscript["?", "head"];
Format[$ArgsCrumb[h_], OutputForm] := Underscript[h, "args"];
Format[$ArgsCrumb[h_, n_], OutputForm] := Underscript[h, n]
Format[$ArrayCrumb[], OutputForm] := "-array-"
Format[$ArrayCrumb[n_], OutputForm] := Underscript["-array-", n];
Format[$KeysCrumb[h_], OutputForm] := Underscript[head, "keys"];
Format[$KeysCrumb[h_, k_], OutputForm] := Underscript[h, k];

partRow[h_, n_] := Row[{h, "[[", n, "]]"}];

crumbSubBox[head_, sub_] := UnderscriptBox[
  StyleBox[crumbHeadStr @ head, Bold],
  StyleBox[crumbLabelString @ sub, Italic]
]

crumbHeadStr = Case[
  s_Str                                 := s;
  HoldComplete[List]                    := "{\[CenterEllipsis]}";
  HoldComplete[Assoc]                   := "\[LeftAssociation]\[CenterEllipsis]\[RightAssociation]";
  HoldComplete[head_Symbol ? HoldAtomQ] := codeBox @ HoldSymbolName @ head;
  HoldComplete[head_[___]]              := RBox[% @ HoldComplete @ head, "[\[CenterEllipsis]]"];
  HoldComplete[s_Str]                   := codeBox @ StringJoin["\"", s, "\""];
  hc_HoldComplete                       := codeBox @ hc;
];

crumbLabelString = Case[
  i_Int          := IntegerString @ i;
  s_Str          := StyleBox[s, $Gray, FontFamily -> "Palantir"];
  h_HoldComplete := codeBox @ h;
]

(**************************************************************************************************)

PrivateHead[$ValueDiff, $WeakValueDiff, $ArrayValueDiff, $LengthDiff, $DepthDiff, $DimsDiff, $KeysAdded, $KeysRemoved, $KeysReordered, $KeysChanged, $ArgsAdded, $ArgsRemoved]

DefineStandardTraditionalForm[{
  $ValueDiff[e1_, e2_]       :> diffBox @ notEqualBox[codeBox @ e1, codeBox @ e2],
  $WeakValueDiff[e1_, e2_]   :> diffBox @ notSameBox[codeBox @ e1, codeBox @ e2],
  $ArrayValueDiff[e1_, e2_]  :> diffBox @ notSameBox[minMaxBox @ e1, minMaxBox @ e2],
  $LengthDiff[n1_, n2_]      :> diffBox @ changeBox["len", intBox @ n1, intBox @ n2],
  $DepthDiff[d1_, d2_]       :> diffBox @ changeBox["depth", intBox @ d1, intBox @ d2],
  $DimsDiff[d1_, d2_]        :> diffBox @ changeBox["dims", dimsBox @ d1, dimsBox @ d2],
  $KeysAdded[k1_, k2_]       :> diffBox @ plusBox[keysBox @ Complement[k2, k1]],
  $KeysRemoved[k1_, k2_]     :> diffBox @ minusBox[keysBox @ Complement[k1, k2]],
  $KeysReordered[k1_, k2_]   :> diffBox @ notSameBox[keysBox @ k1, keysBox @ k2],
  $KeysChanged[k1_, k2_]     :> diffBox @ notSameBox[keysBox @ k1, keysBox @ k2],
  $ArgsAdded[n1_, n2_]       :> diffBox @ changeBox["len", intBox @ n1, intBox @ n2],
  $ArgsRemoved[n1_, n2_]     :> diffBox @ changeBox["len", intBox @ n1, intBox @ n2]
}];

diffBox[e_] := e;

$changeArrow = changeStyle @ "\[RightArrow]";

notEqualBox[a_, b_]             := compBox["\[NotEqual]", a, b];
notSameBox[a_, b_]              := compBox["\[NotCongruent]", a, b];
changeStyle[e_]                 := OrangeBox @ BoldBox @ e;
propBox[e_]                     := TypewriterBox @ StyleBox[e, $Gray];
minMaxBox[arr_]                 := GridBox[{{PinkBox @ RealDigitsString[Max @ arr, 2]}, {TealBox @ RealDigitsString[Min @ arr, 2]}}, RowSpacings -> 0];

compBox[cmp_, a_, b_]           := RBox[a, changeStyle @ cmp, b];
changeBox[prop_, a_, b_]        := RBox[propBox @ prop, ":", notSameBox[a, b]];
plusBox[a_]                     := RBox[GreenBox @ BoldBox @ "+", " ", a];
minusBox[a_]                    := RBox[RedBox @ BoldBox @ "-", " ", a];

codeBox[a_]                     := StyleBox[a, "Code", Background -> None, FontColor -> Black];
codeBox[HoldComplete[e_]]       := codeBox @ ToPrettifiedString[InternalHoldForm[e], MaxLength -> 20, MaxDepth -> 2, CompactRealNumbers -> 4];

keysBox[list_List]              := RowBox @ Riffle[Map[keyBox, list], ","];
keyBox[HoldComplete[s_Symbol]]  := ItalicBox @ HoldSymbolName @ s;
keyBox[h_]                      := codeBox[h];
intBox[i_Int]                   := IntegerString[i];
dimsBox[dims_List]              := RowBox @ Riffle[Map[intBox, dims], "\[Times]"];

(**************************************************************************************************)

PublicFunction[FindExpressionDifferences]

Options[FindExpressionDifferences] = {
  MaxItems -> 3
};

FindExpressionDifferences[a_, b_, OptionsPattern[]] := Scope[
  UnpackOptions[maxItems];
  $diffs = Bag[]; $count = 0; $crumbs = {}; $pos = {};
  Catch[
    diffExpr[HoldComplete[a], HoldComplete[b]],
    $fedTag
  ];
  BagPart[$diffs, All]
];

emitDiff[tag_] := If[$count++ < maxItems,
  StuffBag[$diffs, ExpressionDifference[$pos, $crumbs, tag]]; True,
  Throw[Null, $fedTag];
];

emitAtomDiff[a_, b_] /; a === b := False;
emitAtomDiff[a_, b_] := emitDiff @ If[TrueQ[a == b], $WeakValueDiff[a, b], $ValueDiff[a, b]];

_emitDiff := BadArguments[];

(**************************************************************************************************)

holdPart[e_HoldComplete, p_] := Extract[e, p, HoldComplete];
holdPart[e_HoldComplete, p__] := Extract[e, {p}, HoldComplete];

_holdPart := BadArguments[];

holdKeys[HoldComplete[]] := {};
holdKeys[HoldComplete[args__]] := Keys[Unevaluated[{args}], HoldComplete];
_holdKeys := BadArguments[];

(**************************************************************************************************)

SetHoldRest[withCrumb, withPos, withPosCrumb, withSubCrumb];

withCrumb[c_, body_] := Block[{$crumbs = Append[$crumbs, c]}, body];
withPos[p_, body_] := Block[{$pos = Append[$pos, p]}, body];
withPosCrumb[p_, c_, body_] := Block[{$pos = Append[$pos, p], $crumbs = Append[$crumbs, c]}, body];
withSubCrumb[p_, c_, body_] := Block[{$pos = Append[$pos, p], $crumbs = Insert[$crumbs, c, {-1, -1}]}, body];

(**************************************************************************************************)

diffExpr[a_HoldComplete, b_HoldComplete] /; a === b := False;

diffExpr[a:HoldComplete[_Assoc ? HoldAtomQ], b:HoldComplete[_Assoc ? HoldAtomQ]] :=
  diffAssoc[a, b];

diffExpr[a:HoldComplete[_ ? HoldAtomQ], b:HoldComplete[_ ? HoldAtomQ]] :=
  emitAtomDiff[a, b];

diffExpr[a:HoldComplete[_List ? HoldPackedArrayQ], b:HoldComplete[_List ? HoldPackedArrayQ]] :=
  diffArray[a, b];

diffExpr[
  HoldComplete[(h1_[Shortest[args1___], opts1:((_Symbol|_Str) -> _)...]) ? HoldEntryQ],
  HoldComplete[(h2_[Shortest[args2___], opts2:((_Symbol|_Str) -> _)...]) ? HoldEntryQ]] := With[
    {head = HoldComplete @ h1},
    Or[
      withPosCrumb[0, $HeadCrumb,  diffExpr[HoldComplete @ h1, HoldComplete @ h2]],
      withCrumb[$ArgsCrumb @ head, diffArgs[HoldComplete @ args1, HoldComplete @ args2]],
      withCrumb[$KeysCrumb @ head, diffKeys[HoldSeqLength @ args1, HoldComplete @ opts1, HoldComplete @ opts2]]
    ]
  ];

diffExpr[a_HoldComplete, b_HoldComplete] :=
  emitAtomDiff[a, b];

_diffExpr := BadArguments[];

(**************************************************************************************************)

diffArray[a_HoldComplete, b_HoldComplete] /; a === b := False;

(* these are packed and so we don't care about evaluation *)
diffArray[HoldComplete[a_List], HoldComplete[b_List]] := Scope[
  dims1 = Dimensions @ a; depth1 = Len @ dims1;
  dims2 = Dimensions @ b; depth2 = Len @ dims2;
  withCrumb[$ArrayCrumb[],
    Which[
      depth1 =!= depth2,
        emitDiff @ $DepthDiff[depth1, depth2],
      dims1 =!= dims2,
        emitDiff @ $DimsDiff[dims1, dims2],
      True,
        counts = Counts @ Flatten @ MapThread[SameQ, {a, b}, depth1];
        If[counts[False] >= 3 || counts[True] == 0,
          (* don't try find differing entries if there are a lot or no entries are equal! *)
          emitDiff @ $ArrayValueDiff[a, b],
          Catch @ MapIndexed[diffScalar, TupleArray @ {a, b}, depth1]
        ];
    ]
  ];
  True
];

_diffArray := BadArguments[];

diffScalar[{a_, b_}, pos_] /; a === b := Null;
diffScalar[{a_, b_}, pos_] := withSubCrumb[
  Splice @ pos, pos,
  emitAtomDiff[HoldComplete @ a, HoldComplete @ b]
];

(**************************************************************************************************)

diffArgs[args1_HoldComplete, args2_HoldComplete] /; args1 === args2 := False;

diffArgs[args1_HoldComplete, args2_HoldComplete] := Scope[
  len1 = Len @ args1;
  len2 = Len @ args2;
  Which[

    len1 <= len2 && (args1 === Take[args2, len1]),
      emitDiff @ $ArgsAdded[len1, len2],

    len2 <= len1 && (args2 === Take[args1, len2]),
      emitDiff @ $ArgsRemoved[len1, len2],

    len1 =!= len2,
      emitDiff @ $LengthDiff[len1, len2],

    True,
      Do[
        withSubCrumb[i, i, diffExpr[holdPart[args1, i], holdPart[args2, i]]],
        {i, 1, len1}
      ]
  ];
  True
];

_diffArgs := BadArguments[];

(**************************************************************************************************)

(* these are valid associations and so don't need to be held *)

diffAssoc[a_HoldComplete, b_HoldComplete] /; a === b := False;

diffAssoc[HoldComplete[a_Assoc], HoldComplete[b_Assoc]] :=
  withCrumb[
    $KeysCrumb @ HoldComplete @ Assoc,
    diffKeys[1, assocToEntries @ a, assocToEntries @ b]
  ];

assocToEntries[assoc_Assoc] := Scope[
  pairs = HoldComplete @@ KeyValueMap[HoldComplete, assoc];
  VectorReplace[pairs, HoldComplete[k_, v_] :> (k -> v)]
];

_diffAssoc := BadArguments[];

(**************************************************************************************************)

diffKeys[startIndex_, entries1_HoldComplete, entries2_HoldComplete] := Scope[

  len1 = Len @ entries1; keys1 = holdKeys @ entries1;
  len2 = Len @ entries2; keys2 = holdKeys @ entries2;

  Which[

    len1 < len2 && SubsetQ[keys2, keys1],
      emitDiff @ $KeysAdded[keys1, keys2],

    len2 < len1 && SubsetQ[keys1, keys2],
      emitDiff @ $KeysRemoved[keys1, keys2],

    keys1 =!= keys2,
      If[Sort[keys1] === Sort[keys2],
        emitDiff @ $KeysReordered[keys1, keys2],
        emitDiff @ $KeysChanged[keys1, keys2]
      ],

    True,
      Null
  ];

  ScanIndex1[{key, i1} |->
    If[IntegerQ[i2 = IndexOf[keys2, key, None]],
      withSubCrumb[startIndex + i1 - 1, key,
        diffExpr[holdPart[entries1, i1, 2], holdPart[entries2, i2, 2]]
      ]
    ],
    keys1
  ];

  True
];

