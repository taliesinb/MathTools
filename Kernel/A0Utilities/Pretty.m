$prettyFormOpts = Sequence[
  MaxIndent -> 10, CompactingWidth -> 150, TabSize -> None, FullSymbolContext -> False
];

(* this overrides GU`PrettyForm *)
PrettyForm /: MakeBoxes[PrettyForm[expr_], StandardForm] :=
  ToPrettifiedString[InternalHoldForm @ expr, $prettyFormOpts];

PrettyForm /: MakeBoxes[PrettyForm[expr_, opts__], StandardForm] :=
  ToPrettifiedString[InternalHoldForm @ expr, opts, $prettyFormOpts];

HoldPrettyForm /: MakeBoxes[HoldPrettyForm[expr_, opts__], StandardForm] :=
  ToPrettifiedString[InternalHoldForm @ expr, opts, $prettyFormOpts];

HoldPrettyForm /: MakeBoxes[HoldPrettyForm[expr_], StandardForm] :=
  ToPrettifiedString[InternalHoldForm @ expr, $prettyFormOpts];

(**************************************************************************************************)

PublicOption[ColorSymbolContext]

PublicHead[InternalHoldForm]

SetHoldAllComplete[InternalHoldForm]

PublicTypesettingForm[CompactPrettyForm, CompactPrettyFullForm]

$compactOpts = Sequence[
  MaxIndent -> 10, FullSymbolContext -> False, ColorSymbolContext -> $Notebooks,
  CompactingWidth -> 150, InlineColors -> $Notebooks,
  CompactRealNumbers -> True, TabSize -> None
];

cpfString[expr_, ela_, opts___] := ToPrettifiedString[
  InternalHoldForm @ expr, opts,
  $compactOpts, ElideLargeArrays -> ela
];

Format[CompactPrettyForm[expr_, opts___Rule], OutputForm] := cpfString[expr, True, opts];
CompactPrettyForm /: MakeBoxes[CompactPrettyForm[expr_, opts___Rule], StandardForm] := cpfString[expr, True, opts];

Format[CompactPrettyFullForm[expr_, opts___Rule], OutputForm] := cpfString[expr, False, opts];
CompactPrettyFullForm /: MakeBoxes[CompactPrettyFullForm[expr_, opts___Rule], StandardForm] := cpfString[expr, False, opts];

(**************************************************************************************************)

PublicFunction[ToPrettifiedString]

PublicOption[MaxIndent, MaxDepth, MaxLength, MaxStringLength, TabSize, CompactingWidth, InlineHeads, FullSymbolContext, CompressLargeSubexpressions, ElideLargeArrays, ElideAtomicHeads, InlineColors, CompactRealNumbers]

(* not the same as GeneralUtilities`ToPrettyString *)

Options[ToPrettifiedString] = {
  MaxIndent -> 5,
  MaxDepth -> Inf,
  MaxLength -> Inf,
  MaxStringLength -> 32,
  CompactingWidth -> 48,
  InlineHeads -> {"Quantity", "Entity", "Interval"},
  TabSize -> 2,
  FullSymbolContext -> True,
  ColorSymbolContext -> False,
  CompressLargeSubexpressions -> True,
  ElideLargeArrays -> False,
  ElideAtomicHeads -> False,
  InlineColors -> False,
  CompactRealNumbers -> False
}

(* for debugging *)
$depth = 0;
$shortenDepth = 0;
$prettyCompression = True;
$maxIndent = 8;
$maxDepth = 8;
$maxLength = 128;
$maxStringLength = 64;
$inlineHeads = {};
$fullSymbolContext = True;
$elideLargeArrays = False;
$elideAtomicHeads = False;
$inlineColors = False;
$compactRealNumbers = False;
$colorSymbolContext = False;
$tabSize = 2;
$compactRealLength = 2;

ToPrettifiedString[e_, OptionsPattern[]] := Scope[
  {$maxIndent, $maxWidth, $maxDepth, $maxLength, $maxStringLength, $tabSize, $inlineHeads, $fullSymbolContext, $colorSymbolContext, $prettyCompression,        $elideLargeArrays, $elideAtomicHeads, $inlineColors, $compactRealNumbers} = OptionValue[
  {MaxIndent, CompactingWidth, MaxDepth, MaxLength, MaxStringLength, TabSize, InlineHeads, FullSymbolContext, ColorSymbolContext, CompressLargeSubexpressions, ElideLargeArrays,  ElideAtomicHeads, InlineColors, CompactRealNumbers}];
  $ContextPath = {"System`", "QuiverGeometry`", "GeneralUtilities`"};
  $compactRealLength = If[IntQ[$compactRealNumbers], $compactRealNumbers, 2];
  $compactRealNumbers = !FalseQ[$compactRealNumbers];
  If[!$fullSymbolContext, $ContextPath = Join[$ContextPath, getAllSymbolContexts @ HoldC @ e]];
  $depth = $shortenDepth = 0;
  Block[{FilterOptions}, pretty0[e]]
]

$fatHeadP = HoldP[_ByteArray | _NumericArray | _SparseArray | _Image | _Video | _AnimatedImage] ? HoldAtomQ;

getAllSymbolContexts[e_] := DeepUniqueCases[e, s_Symbol ? HoldAtomQ :> Context[Uneval @ s]];

(**************************************************************************************************)

SetHoldAllComplete[pretty0, pretty1, pretty1wrap, pretty2, prettyRule, prettyRuleDelayed, prettyInfix, prettyCompressed, prettyDeep, prettyLong, prettyHead, symbolString];

pretty0[InternalHoldForm[a__]] := pretty0[Sequence[a]];

pretty0[InternalHoldForm[e_]] := pretty0[e];

pretty0[InternalHoldForm[]] := "Sequence[]";

pretty0[e_] /; TrueQ[$depth > $maxDepth] := $ellipsisString;

pretty0[e_] /; TrueQ[$depth == $maxDepth] := prettyDeep[e];

pretty0[e:((s_Symbol)[___])] /; MemberQ[$inlineHeads, HoldSymbolName @ s] := pretty2[e];

pretty0[r_Real ? HoldAtomQ]  := realString[r];

pretty0[e_] := Block[{$depth = $depth + 1},

  (* TODO: I think this was done out of laziness but doens't handle certain things that we want custom formatting for *)
  If[!wideQ[e] && !$elideLargeArrays && FreeQ[Uneval @ e, _DirectedEdge|_UndirectedEdge|_Graph|_Image|_NumericArray] && shortQ[str = pretty2[e]],
    Return @ str];

  If[longQ[e], Return @ prettyLong[e]];

  pretty1[e]
];

(**************************************************************************************************)

realString[r_] := If[TrueQ[$compactRealNumbers], RealDigitsString[r, $compactRealLength], ToString[r]];

(**************************************************************************************************)

SetHoldAllComplete[prettyDeep];

prettyDeep = Case[
  (h_Symbol ? HAQ)[]                      := SJoin[symbolString @ h, "[]"];
  r:Rule[_ ? smallQ, _ ? smallQ]          := pretty2[r];
  r:RuleDelayed[_ ? smallQ, _ ? smallQ]   := pretty2[r];
  a_ ? smallQ                             := pretty2[a];
  e_                                      := prettyLong[e];
,
  {HAQ -> HoldAtomQ}
];

(**************************************************************************************************)

$ellipsisString = "\[Ellipsis]";

SetHoldAllComplete[prettyLong, fatHeadString];

prettyLong = Case[
  str_Str               := Scope[
    If[SLen[str] < $maxStringLength, Return @ pretty2 @ str];
    prefix = If[$maxStringLength > 5, STake[ToString[STake[str, $maxStringLength - 5], InputForm], {2, -2}], ""];
    SJoin["\"", prefix, $ellipsisString, "\""]
  ];
  _List                 := SJoin["{", $ellipsisString, "}"];
  _Assoc ? HAQ          := SJoin["<|", $ellipsisString, "|>"];
  (h_Symbol ? HAQ)[___] := SJoin[symbolString @ h, "[", $ellipsisString, "]"];
  e:$fatHeadP           := fatHeadString[e];
  g_Graph ? HAQ         := SJoin["Graph[«", IntStr @ VertexCount @ g, "», «", IntStr @ EdgeCount @ g, "», ", $ellipsisString, "]"];
  _                     := $ellipsisString;
,
  {HAQ -> HoldAtomQ, $fatHeadP}
]

fatHeadString[e_] := SJoin[prettyHead @ e, "[", $ellipsisString, "]"];

SetHoldAllComplete[symbolString, prettyHead];

symbolString[s_Symbol ? HoldAtomQ] :=
  If[$colorSymbolContext, colorByContext[Context[s]], Id] @
  If[$fullSymbolContext, ToString[Uneval @ s, InputForm], HoldSymbolName @ s];

symbolString[_] := "?";

wrapColor[col_, con_][str_] := SJoin[
  "\!\(\*TooltipBox[StyleBox[", str, ",", ToString @ col, "],\"", con, "\"]",
  "\)"
];

colorByContext["System`" | "Internal`" | "GeneralUtilities`" | "Developer`" | "QuiverGeometry`"] := Id;
colorByContext["Global`"] := wrapColor[$Teal, "Global`"];
colorByContext["QuiverGeometry`Private`"] := wrapColor[$Orange, "QuiverGeometry`Private`"];
colorByContext[con_] := wrapColor[$Red, con];


symbolString[_] := $ellipsisString;

prettyHead[h_[___]] := symbolString[h];
prettyHead[_Graph] := "Graph";
prettyHead[_] := $ellipsisString;

(**************************************************************************************************)

SetHoldAllComplete[smallQ, wideQ, longQ];

smallQ = Case[
  _Symbol ? HAQ := True;
  s_Str ? HAQ   := SLen[s] < 12 || SMatchQ[s, LetterCharacter..];
  _Int ? HAQ    := True;
  _Real ? HAQ   := $compactRealNumbers;
  _             := False;
,
  {HAQ -> HoldAtomQ}
];

wideQ[_Sequence] := False;
wideQ[e_] := (2*LeafCount[Uneval @ e] > $maxWidth) || (2*ByteCount[Uneval @ e]/48) > $maxWidth;

longQ[e_Str ? HoldAtomQ] := SLen[Uneval @ e] > $maxStringLength;
longQ[a_Assoc ? HoldAtomQ] := Len[Uneval @ a] > $maxLength;
longQ[(_?HoldAtomQ)[Shortest[a___], ___Rule]] := Len[Uneval @ {a}] > $maxLength;
longQ[e_] := Len[Uneval @ e] > $maxLength;

shortQ[s_] := shortStringQ[s] || shortStringQ[SDelete[s, "\!\(\*StyleBox[" ~~ Shortest[__] ~~ "Rule[StripOnInput, False]]\)"]];

shortStringQ[s_] := tabStringLength[s] <= ($maxWidth - ($depth + $shortenDepth) * Rep[$tabSize, None -> 4]);
tabStringLength[s_] := SLen[SRep[s, "\t" -> "    "]];

(**************************************************************************************************)

SetHoldAllComplete[pretty1, pretty1wrap, indentArgs];

pretty1wrap[e_ ? longQ] := prettyLong[e];
pretty1wrap[e_ ? HoldAtomQ] := pretty2[e];
pretty1wrap[e:(_Blank | _BlankSequence | _BlankNullSequence)] := pretty1[e];
pretty1wrap[e_List | e_Assoc] := pretty1[e];
pretty1wrap[e_] := "(" <> pretty1[e] <> ")";

pretty1 = Case[
  Verbatim[Rule][a_, b_]              := prettyRule[a, b];
  Verbatim[RuleDelayed][a_, b_]       := prettyRuleDelayed[a, b];
  Verbatim[Set][args__]               := prettyInfix[" = ", args];
  Verbatim[SetDelayed][args__]        := prettyInfix[" := ", args];
  Verbatim[Plus][args__]              := prettyInfix[" + ", args];
  Verbatim[Times][args__]             := prettyInfix[" * ", args];
  Verbatim[PatternTest][a_, b_ ? HSQ] := prettyInfix[" ? ", a, b];
  Verbatim[Pattern][s_ ? HSQ, b_]     := prettyInfix[":", s, b];
  Verbatim[Blank][]                     := "_";
  Verbatim[Blank][s_ ? HSQ]             := "_" <> symbolString[s];
  Verbatim[BlankSequence][]             := "__";
  Verbatim[BlankSequence][s_ ? HSQ]     := "__" <> symbolString[s];
  Verbatim[BlankNullSequence][]         := "___";
  Verbatim[BlankNullSequence][s_ ? HSQ] := "___" <> symbolString[s];
  Verbatim[Pattern][s_ ? HSQ, p:(_Blank | _BlankSequence | _BlankNullSequence)] := symbolString[s] <> pretty1[p];
  Verbatim[Alt][a__]         := prettyInfix[" | ", a];
  Verbatim[Minus][arg_]               := "-" <> pretty1wrap[arg];
  Verbatim[Times][-1, arg_]           := "-" <> pretty1wrap[arg];
  Verbatim[DirectedEdge][a1_, a2_]    := prettyInfix[" => ", a1, a2];
  Verbatim[UndirectedEdge][a1_, a2_]  := prettyInfix[" <=> ", a1, a2];
  col:(_RGBColor | _GrayLevel) /; TrueQ[$inlineColors] && ColorQ[Uneval @ col] := prettyInlineColor[col];

  list_List /; TrueQ[$elideLargeArrays] && HoldNumericArrayQ[list] && beefyNumericArrayQ[list] := prettyElidedList[list];
  list_List /; TrueQ[$prettyCompression] && HoldPackedArrayQ[list] && holdLeafCount[list] > 128 := prettyCompressed[list];
  list_List                      := indentedBlock["{", indentArgs @ list, "}"];
  assoc_Assoc /; AssocQ[Uneval[assoc]]
                                 := indentedBlock["<|", KVMap[prettyRule, Uneval @ assoc], "|>"];
  Assoc[args___]                 := indentedBlock["<|", indentArgs @ {args}, "|>"];
  e:$fatHeadP                    := If[$elideAtomicHeads, fatHeadString[e], pretty2[e]];
  e:(_Symbol[])                  := pretty2[e];
  g_Graph ? HAQ                  := If[$elideAtomicHeads, fatHeadString[g], prettyGraph[g]];
  head_Symbol[args___]           := indentedBlock[pretty2[head] <> "[", indentArgs @ {args}, "]"];
  head_[args___]                 := indentedBlock[pretty1wrap[head] <> "[", indentArgs @ {args}, "]"];
  atom_ ? HAQ                    := pretty2[atom];
,
  {$fatHeadP, HAQ -> HoldAtomQ, HSQ -> HoldSymbolQ}
];

indentArgs[list_] := Block[{$shortenDepth = $shortenDepth + 1}, MapUnevaluated[pretty0, list]];

makeTab[n_] := If[IntQ[$tabSize], makeSpaceTab[n * $tabSize], makeTabTab[n]];

makeTabTab[n_] := makeTabTab[n] = SRepeat["\t", n];
makeSpaceTab[n_] := makeSpaceTab[n] = SRepeat[" ", n];

(**************************************************************************************************)

$literalPatternVariables = Hold[
  $ColorPattern, $OpacityPattern,
  $NumberP,
  $CoordP, $Coord2P, $Coord3P, $CoordPairP, $Coord2PairP, $Coord3PairP, $CoordMat2P, $CoordMat3P, $CoordMatP, $CoordMaybeMatP, $CoordMatsP, $CoordMaybeMatsP,
  $SidePattern, $SizePattern,
  $RulePattern, $RuleListPattern,
  $Blue, $Red, $Green, $Pink, $Teal, $Yellow, $Orange, $Purple, $Gray, $White, $Black,
  $DarkRed, $DarkBlue, $DarkGreen, $DarkOrange, $DarkPurple, $DarkTeal, $DarkGray, $DarkPink, $DarkYellow, $DarkWhite, $DarkBlack,
  $LightRed, $LightBlue, $LightGreen, $LightOrange, $LightPurple, $LightTeal, $LightGray, $LightPink, $LightYellow, $LightWhite, $LightBlack
];

Scan[
  Fn[s, With[{v = s},
    pretty1wrap[Verbatim[v]] := symbolString[s];
    pretty1[Verbatim[v]] := symbolString[s];
  ], HoldAll],
  $literalPatternVariables
];

(**************************************************************************************************)

prettyInlineColor[color_] := ToString[Style["\[FilledSquare]", color], StandardForm];

(**************************************************************************************************)

SetHoldAllComplete[holdLeafCount, beefyNumericArrayQ];

holdLeafCount[e_] := LeafCount[Uneval @ e];
beefyNumericArrayQ[list_] := holdLeafCount[list] >= If[ArrayQ[Uneval @ list, _, IntQ], 8, 4];
prettyElidedList[list_] := With[
  {dims = Dims @ Uneval @ list},
  SJoin @ {"\[LeftAngleBracket]", dimsString @ dims, "\[RightAngleBracket]"}
];

dimsString[dims_] := Riffle[IntStr /@ dims, ","];

(**************************************************************************************************)

indentedBlock[begin_, {}, end_] := begin <> end;

indentedBlock[begin_ ? (SEndsQ["["]), {line_Str} /; SLen[line] > 8, "]"] :=
  SJoin[SDrop[begin, -1] <> " @ ", deIndent @ line];

(* indentedBlock["{", {line_Str} /; StringLength[line] > 8, "}"] :=
  StringJoin["List @ ", deIndent @ line];
 *)
indentedBlock[begin_, {line_Str}, end_] :=
  SJoin[begin, deIndent @ line, end];

deIndent[line_Str] := SRep[line, "\n\t" -> "\n"];

indentedBlock[begin_, list_List, end_] := With[
  {t1 = makeTab[$depth], t2 = makeTab[$depth - 1]},

  compact = SJoin[
    begin,
    Map[{#, ", "}&, Most @ list],
    {L @ list},
    end
  ];

  If[shortQ[compact] || $depth > $maxIndent, Return @ compact];

  SJoin[
    begin, "\n",
    Map[{t1, #, ",\n"}&, Most @ list],
    {t1, L @ list, "\n"},
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

prettyGraph[g_Graph] := With[{v = VertexList[g], e = EdgeList[g], o = Options[g]},
  indentedBlock["Graph[", Join[{pretty0 @ v, pretty0 @ e}, MapUnevaluated[pretty0, o]], "]"]
];

(**************************************************************************************************)

$literalPatternVariableReplacements := $literalPatternVariableReplacements =
  Cases[$literalPatternVariables, var_ :> With[{val = var},
    RuleDelayed[Verbatim[val], var]
  ]
];

pretty2 = Case[
  e:$fatHeadP /; TrueQ[$prettyCompression] := prettyCompressed[e];
  e_Symbol ? HAQ := symbolString[e];
  e_Real ? HAQ   := realString[e];
  _DataFrame     := "DataFrame[\[Ellipsis]]";
  e_             := chunkToString[e];
,
  {$fatHeadP}
];

SetHoldAllComplete[chunkToString];
chunkToString[e_] := With[{h = HoldC[e]},
  Rep[
    h /. $literalPatternVariableReplacements,
    HoldC[z_] :> compactReals @ ToString[Uneval @ z, InputForm]
  ]
];

(* this is super hacky but not sure how else to easily clip numbers once ToString is done *)
compactReals[str_] := If[$compactRealNumbers && SFreeQ[str, "\""],
  SRep[str, {
      l:"0".. ~~ "." ~~ Longest[m:"0"..] ~~ r:DigitCharacter.. :> SJoin[l, ".", m, STake[r, UpTo @ $compactRealLength]],
      l:DigitCharacter.. ~~ "." ~~ r:Repeated[DigitCharacter, {$compactRealLength + 1, Inf}] :> SJoin[l, ".", STake[r, $compactRealLength]]
  }],
  str
]

(**************************************************************************************************)

prettyCompressed[e_] := Scope[
  head = H[Uneval @ e];
  headName = If[H[head] === Symbol, SymbolName[head] <> ";", ""];
  str = Compress[Uneval @ e];
  len = SLen[str];
  SJoin @ Which[
    len <= 2 * $maxWidth,
      {"CompressedData[", headName, "\"", str, "\"]"},
    len <= 180,
      {"CompressedData[", headName, "\"\n", makeTab[$depth], str, "\n", makeTab[$depth-1], "\"]"},
    True,
      chunks =  SPartition[str, UpTo[120]];
      t1 = makeTab[Min[$depth, 4]]; t2 = makeTab[$depth - 1];
      {"CompressedData[", headName, "\"\n", Map[{t1, #, "\n"}&, chunks], t2, "\"]"}
  ]
];