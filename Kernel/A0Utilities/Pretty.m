PublicFunction[ToPrettifiedString]

PublicOption[MaxIndent, MaxDepth, MaxLength, CompactingWidth, InlineHeads, FullSymbolContext]

(* not the same as GeneralUtilities`ToPrettyString *)

Options[ToPrettifiedString] = {
  MaxIndent -> 5,
  MaxDepth -> Infinity,
  MaxLength -> Infinity,
  CompactingWidth -> 48,
  InlineHeads -> {Quantity, Entity, Interval},
  FullSymbolContext -> True
}

ToPrettifiedString[e_, OptionsPattern[]] := Scope[
  {$maxIndent, $maxWidth, $maxDepth, $maxLength, $inlineHeads, $fullSymbolContext} = OptionValue[{MaxIndent, CompactingWidth, MaxDepth, MaxLength, InlineHeads, FullSymbolContext}];
  $ContextPath = {"System`", "QuiverGeometry`", "GeneralUtilities`"};
  If[!$fullSymbolContext, $ContextPath = Join[$ContextPath, getAllSymbolContexts @ HoldComplete @ e]];
  $depth = 0;
  Block[{FilterOptions}, pretty0[e]]
]

$fatHeadP = (_NumericArray | _SparseArray | _Image | _Video | _AnimatedImage | _Graph) ? Developer`HoldAtomQ;

getAllSymbolContexts[e_] := DeepUniqueCases[e, s_Symbol ? Developer`HoldAtomQ :> Context[Unevaluated @ s]];

(**************************************************************************************************)

SetHoldAllComplete[pretty0, pretty1, pretty1wrap, pretty2, prettyRule, prettyRuleDelayed, prettyInfix, prettyCompressed, prettyDeep, prettyLong, prettyHead, symbolString];

pretty0[e_] /; TrueQ[$depth > $maxDepth] := $ellipsisString;

pretty0[e_] /; TrueQ[$depth == $maxDepth] := prettyDeep[e];

pretty0[e:((s_Symbol)[___])] /; MemberQ[$inlineHeads, HoldPattern @ s] := pretty2[e];

pretty0[e_] := Block[{$depth = $depth + 1},

  If[!wideQ[e] && shortQ[str = pretty2[e]], Return @ str];

  If[longQ[e], Return @ prettyLong[e]];

  pretty1[e]
];

(**************************************************************************************************)

SetHoldAllComplete[prettyDeep];

prettyDeep = Case[
  (h_Symbol ? HAQ)[]                      := StringJoin[symbolString @ h, "[]"];
  r:Rule[_ ? smallQ, _ ? smallQ]          := pretty2[r];
  r:RuleDelayed[_ ? smallQ, _ ? smallQ]   := pretty2[r];
  a_ ? smallQ                             := pretty2[a];
  e_                                      := prettyLong[e];
,
  {HAQ -> Developer`HoldAtomQ, $fatHeadP}
];

(**************************************************************************************************)

$ellipsisString = "\[Ellipsis]";

SetHoldAllComplete[prettyLong];

prettyLong = Case[
  str_String              := Scope[
    If[StringLength[str] < $maxLength, Return @ pretty2 @ str];
    prefix = If[$maxLength > 5, StringTake[ToString[StringTake[str, $maxLength - 5], InputForm], {2, -2}], ""];
    StringJoin["\"", prefix, $ellipsisString, "\""]
  ];
_List                 := StringJoin["{", $ellipsisString, "}"];
  _Association ? HAQ    := StringJoin["<|", $ellipsisString, "|>"];
  (h_Symbol ? HAQ)[___] := StringJoin[symbolString @ h, "[", $ellipsisString, "]"];
  e:$fatHeadP           := StringJoin[prettyHead @ e, "[", $ellipsisString, "]"];
  _                     := $ellipsisString;
,
  {HAQ -> Developer`HoldAtomQ, $fatHeadP}
]

SetHoldAllComplete[symbolString, prettyHead];

symbolString[s_Symbol ? Developer`HoldAtomQ] := If[$fullSymbolContext, ToString[Unevaluated @ s, InputForm], SymbolName[Unevaluated @ s]];
symbolString[_] := $ellipsisString;

prettyHead[h_[___]] := symbolString[h];
prettyHead[_] := $ellipsisString;

(**************************************************************************************************)

SetHoldAllComplete[smallQ, wideQ, longQ];

smallQ = Case[
  _Symbol ? HAQ   := True;
  s_String ? HAQ  := StringLength[s] < 12 || StringMatchQ[s, LetterCharacter..];
  _Integer ? HAQ  := True;
  _               := False;
,
  {HAQ -> Developer`HoldAtomQ}
];

wideQ[_Sequence] := False;
wideQ[e_] := (2*LeafCount[Unevaluated @ e] > $maxWidth) || (2*ByteCount[Unevaluated @ e]/48) > $maxWidth;

longQ[e_String ? Developer`HoldAtomQ] := StringLength[Unevaluated @ e] > $maxLength;
longQ[e_] := Length[Unevaluated @ e] > $maxLength;

shortQ[s_] := StringLength[s] <= $maxWidth;

(**************************************************************************************************)

SetHoldAllComplete[pretty1, pretty1wrap];

pretty1wrap[e_ ? longQ] := prettyLong[e];
pretty1wrap[e_ ? Developer`HoldAtomQ] := pretty2[e];
pretty1wrap[e_] := "(" <> pretty1[e] <> ")";

pretty1 = Case[
  Verbatim[Rule][a_, b_]         := prettyRule[a, b];
  Verbatim[RuleDelayed][a_, b_]  := prettyRuleDelayed[" :> ", a, b];
  Verbatim[Plus][args__]         := prettyInfix[" + ", args];
  Verbatim[Times][args__]        := prettyInfix[" * ", args];
  Verbatim[Minus][arg_]          := "-" <> pretty1wrap[arg];
  Verbatim[Times][-1, arg_]      := "-" <> pretty1wrap[arg];
  list_List /; Developer`PackedArrayQ[Unevaluated @ list] && LeafCount[Unevaluated @ list] > 256 := prettyCompressed[list];
  list_List                      := indentedBlock["{", MapUnevaluated[pretty0, list], "}"];
  assoc_Association /; AssociationQ[Unevaluated[assoc]]
                                 := indentedBlock["<|", KeyValueMap[prettyRule, Unevaluated @ assoc], "|>"];
  e:$fatHeadP                    := pretty2[e];
  e:(_Symbol[])                  := pretty2[e];
  head_Symbol[args___]           := indentedBlock[pretty2[head] <> "[", MapUnevaluated[pretty0, {args}], "]"];
  head_[args___]                 := indentedBlock[pretty1[head] <> "[", MapUnevaluated[pretty0, {args}], "]"];
  atom_ ? Developer`HoldAtomQ    := pretty2[atom];
,
  {HAQ -> ? Developer`HoldAtomQ, $fatHeadP}
];

makeTab[n_] := makeTab[n] = StringRepeat["  ", n];

(**************************************************************************************************)

indentedBlock[begin_, {line_String}, end_] := StringJoin[begin, line, end];
indentedBlock[begin_, list_List, end_] := With[
  {t1 = makeTab[$depth], t2 = makeTab[$depth - 1]},

  compact = StringJoin[
    begin,
    Map[{#, ", "}&, Most @ list],
    {Last @ list},
    end
  ];

  If[shortQ[compact] || $depth > $maxIndent, Return @ compact];

  StringJoin[
    begin, "\n",
    Map[{t1, #, ",\n"}&, Most @ list],
    {t1, Last @ list, "\n"},
    t2, end
  ]
];

(**************************************************************************************************)

SetHoldAllComplete[prettyInfix, prettyRule, prettyRuleDelayed];

prettyInfix[s_, a_] := pretty1wrap[a];
prettyInfix[s_, a_, b_] := pretty1wrap[a] <> s <> pretty1wrap[b];
prettyInfix[s_, a_, b_, c_] := pretty1wrap[a] <> s <> pretty1wrap[b] <> s <> pretty1wrap[c];
prettyInfix[s_, a_, b_, c_, d__] := pretty1wrap[a] <> s <> pretty1wrap[b] <> s <> prettyInfix[s, c, d];

prettyRule[a_, b_] := pretty0[a] <> " -> " <> pretty0[b];
prettyRuleDelayed[a_, b_] := pretty0[a] <> " :> " <> pretty0[b];

(**************************************************************************************************)

pretty2 = Case[
  e:$fatHeadP := prettyCompressed[e];
  e_ := ToString[Unevaluated @ e, InputForm]
,
  {$fatHeadP}
];

(**************************************************************************************************)

prettyCompressed[e_] := Scope[
  head = Head[Unevaluated @ e];
  headName = If[Head[head] === Symbol, SymbolName[head] <> ";", ""];
  str = Compress[Unevaluated @ e];
  len = StringLength[str];
  StringJoin @ Which[
    len <= 2 * $maxWidth,
      {"CompressedData[", headName, "\"", str, "\"]"},
    len <= 180,
      {"CompressedData[", headName, "\"\n", makeTab[$depth], str, "\n", makeTab[$depth-1], "\"]"},
    True,
      chunks =  StringPartition[str, UpTo[120]];
      t1 = makeTab[Min[$depth, 4]]; t2 = makeTab[$depth - 1];
      {"CompressedData[", headName, "\"\n", Map[{t1, #, "\n"}&, chunks], t2, "\"]"}
  ]
];