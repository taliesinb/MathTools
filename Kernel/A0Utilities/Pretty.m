(* this overrides GU`PrettyForm *)
PrettyForm /: MakeBoxes[PrettyForm[expr_], StandardForm] :=
  ToPrettifiedString[Unevaluated @ expr, MaxIndent -> 10, CompactingWidth -> 150, TabSize -> None];

HoldPrettyForm /: MakeBoxes[HoldPrettyForm[expr_], StandardForm] :=
  ToPrettifiedString[Unevaluated @ expr, MaxIndent -> 10, CompactingWidth -> 150, TabSize -> None];

(**************************************************************************************************)

PublicOption[ColorSymbolContext]

PublicTypesettingForm[CompactPrettyForm]

CompactPrettyForm /: MakeBoxes[CompactPrettyForm[expr_], StandardForm] :=
  ToPrettifiedString[
    Unevaluated @ expr,
    MaxIndent -> 10, FullSymbolContext -> False, ColorSymbolContext -> True,
    CompactingWidth -> 150, ElideLargeArrays -> True, InlineColors -> True,
    CompactRealNumbers -> True, TabSize -> None
  ];

(**************************************************************************************************)

PublicFunction[ToPrettifiedString]

PublicOption[MaxIndent, MaxDepth, MaxLength, TabSize, CompactingWidth, InlineHeads, FullSymbolContext, CompressLargeSubexpressions, ElideLargeArrays, InlineColors, CompactRealNumbers]

(* not the same as GeneralUtilities`ToPrettyString *)

Options[ToPrettifiedString] = {
  MaxIndent -> 5,
  MaxDepth -> Infinity,
  MaxLength -> Infinity,
  CompactingWidth -> 48,
  InlineHeads -> {Quantity, Entity, Interval},
  TabSize -> 2,
  FullSymbolContext -> True,
  ColorSymbolContext -> False,
  CompressLargeSubexpressions -> True,
  ElideLargeArrays -> False,
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
$inlineHeads = {};
$fullSymbolContext = True;
$elideLargeArrays = False;
$inlineColors = False;
$compactRealNumbers = False;
$colorSymbolContext = False;
$tabSize = 2;

ToPrettifiedString[e_, OptionsPattern[]] := Scope[
  {$maxIndent, $maxWidth, $maxDepth, $maxLength, $tabSize, $inlineHeads, $fullSymbolContext, $colorSymbolContext, $prettyCompression, $elideLargeArrays, $inlineColors, $compactRealNumbers} = OptionValue[
  {MaxIndent, CompactingWidth, MaxDepth, MaxLength, TabSize, InlineHeads, FullSymbolContext, ColorSymbolContext, CompressLargeSubexpressions, ElideLargeArrays, InlineColors, CompactRealNumbers}];
  $ContextPath = {"System`", "QuiverGeometry`", "GeneralUtilities`"};
  If[!$fullSymbolContext, $ContextPath = Join[$ContextPath, getAllSymbolContexts @ HoldComplete @ e]];
  $depth = $shortenDepth = 0;
  Block[{FilterOptions}, pretty0[e]]
]


$fatHeadP = (_NumericArray | _SparseArray | _Image | _Video | _AnimatedImage) ? HoldAtomQ;

getAllSymbolContexts[e_] := DeepUniqueCases[e, s_Symbol ? HoldAtomQ :> Context[Unevaluated @ s]];

(**************************************************************************************************)

SetHoldAllComplete[pretty0, pretty1, pretty1wrap, pretty2, prettyRule, prettyRuleDelayed, prettyInfix, prettyCompressed, prettyDeep, prettyLong, prettyHead, symbolString];

pretty0[e_] /; TrueQ[$depth > $maxDepth] := $ellipsisString;

pretty0[e_] /; TrueQ[$depth == $maxDepth] := prettyDeep[e];

pretty0[e:((s_Symbol)[___])] /; MemberQ[$inlineHeads, HoldPattern @ s] := pretty2[e];

pretty0[r_Real ? HoldAtomQ] /; TrueQ[$compactRealNumbers] := RealDigitsString[r, 2]

pretty0[e_] := Block[{$depth = $depth + 1},

  (* TODO: I think this was done out of laziness but doens't handle certain things that we want custom formatting for *)
  If[!wideQ[e] && !$elideLargeArrays && FreeQ[Unevaluated @ e, _DirectedEdge|_UndirectedEdge|_Graph] && shortQ[str = pretty2[e]],
    Return @ str];

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
  {HAQ -> HoldAtomQ, $fatHeadP}
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
  g_Graph ? HAQ         := StringJoin["Graph[«", IntegerString @ VertexCount @ g, "», «", IntegerString @ EdgeCount @ g, "», ", $ellipsisString, "]"];
  _                     := $ellipsisString;
,
  {HAQ -> HoldAtomQ, $fatHeadP}
]

SetHoldAllComplete[symbolString, prettyHead];

symbolString[s_Symbol ? HoldAtomQ] :=
  If[$colorSymbolContext, colorByContext[Context[s]], Identity] @
  If[$fullSymbolContext, ToString[Unevaluated @ s, InputForm], SymbolName[Unevaluated @ s]];

symbolString[_] := "?";

wrapColor[col_, con_][str_] := StringJoin[
  "\!\(\*TooltipBox[StyleBox[", str, ",", ToString @ col, "],\"", con, "\"]",
  "\)"
];

colorByContext["System`" | "Internal`" | "GeneralUtilities`" | "Developer`" | "QuiverGeometry`"] := Identity;
colorByContext["Global`"] := wrapColor[$Teal, "Global`"];
colorByContext["QuiverGeometry`Private`"] := wrapColor[$Orange, "QuiverGeometry`Private`"];
colorByContext[con_] := wrapColor[$Red, con];


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
  {HAQ -> HoldAtomQ}
];

wideQ[_Sequence] := False;
wideQ[e_] := (2*LeafCount[Unevaluated @ e] > $maxWidth) || (2*ByteCount[Unevaluated @ e]/48) > $maxWidth;

longQ[e_String ? HoldAtomQ] := StringLength[Unevaluated @ e] > $maxLength;
longQ[(_?HoldAtomQ)[Shortest[a___], ___Rule]] := Length[Unevaluated @ e] > $maxLength;
longQ[e_] := Length[Unevaluated @ e] > $maxLength;

shortQ[s_] := shortStringQ[s] || shortStringQ[StringDelete[s, "\!\(\*StyleBox[" ~~ Shortest[__] ~~ "Rule[StripOnInput, False]]\)"]];

shortStringQ[s_] := tabStringLength[s] <= ($maxWidth - ($depth + $shortenDepth) * Replace[$tabSize, None -> 4]);
tabStringLength[s_] := StringLength[StringReplace[s, "\t" -> "    "]];

(**************************************************************************************************)

SetHoldAllComplete[pretty1, pretty1wrap, indentArgs];

pretty1wrap[e_ ? longQ] := prettyLong[e];
pretty1wrap[e_ ? HoldAtomQ] := pretty2[e];
pretty1wrap[e:(_Blank | _BlankSequence | _BlankNullSequence)] := pretty1[e];
pretty1wrap[e_List | e_Association] := pretty1[e];
pretty1wrap[e_] := "(" <> pretty1[e] <> ")";

pretty1 = Case[
  Verbatim[Rule][a_, b_]              := prettyRule[a, b];
  Verbatim[RuleDelayed][a_, b_]       := prettyRuleDelayed[a, b];
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
  Verbatim[Alternatives][a__]         := prettyInfix[" | ", a];
  Verbatim[Minus][arg_]               := "-" <> pretty1wrap[arg];
  Verbatim[Times][-1, arg_]           := "-" <> pretty1wrap[arg];
  Verbatim[DirectedEdge][a1_, a2_]    := prettyInfix[" => ", a1, a2];
  Verbatim[UndirectedEdge][a1_, a2_]  := prettyInfix[" <=> ", a1, a2];
  col:(_RGBColor | _GrayLevel) /; TrueQ[$inlineColors] && ColorQ[Unevaluated @ col] := prettyInlineColor[col];
  list_List /; TrueQ[$elideLargeArrays] && HoldNumericArrayQ[list] && beefyNumericArrayQ[list] := prettyElidedList[list];
  list_List /; TrueQ[$prettyCompression] && HoldPackedArrayQ[list] && holdLeafCount[list] > 128 := prettyCompressed[list];
  list_List                      := indentedBlock["{", indentArgs @ list, "}"];
  assoc_Association /; AssociationQ[Unevaluated[assoc]]
                                 := indentedBlock["<|", KeyValueMap[prettyRule, Unevaluated @ assoc], "|>"];
  e:$fatHeadP                    := pretty2[e];
  e:(_Symbol[])                  := pretty2[e];
  g_Graph ? HAQ                  := prettyGraph[g];
  head_Symbol[args___]           := indentedBlock[pretty2[head] <> "[", indentArgs @ {args}, "]"];
  head_[args___]                 := indentedBlock[pretty1wrap[head] <> "[", indentArgs @ {args}, "]"];
  atom_ ? HAQ                    := pretty2[atom];
,
  {$fatHeadP, HAQ -> HoldAtomQ, HSQ -> HoldSymbolQ}
];

indentArgs[list_] := Block[{$shortenDepth = $shortenDepth + 1}, MapUnevaluated[pretty0, list]];

makeTab[n_] := If[IntegerQ[$tabSize], makeSpaceTab[n * $tabSize], makeTabTab[n]];

makeTabTab[n_] := makeTabTab[n] = StringRepeat["\t", n];
makeSpaceTab[n_] := makeSpaceTab[n] = StringRepeat[" ", n];

(**************************************************************************************************)

$literalPatternVariables = Hold[
  $ColorPattern, $OpacityPattern,
  $NumberP,
  $CoordP, $Coord2P, $Coord3P, $CoordPairP, $Coord2PairP, $Coord3PairP, $CoordMat2P, $CoordMat3P, $CoordMatP, $CoordMaybeMatP, $CoordMatsP, $CoordMaybeMatsP,
  $SidePattern, $SizePattern,
  $RulePattern, $RuleListPattern
];

Scan[
  Function[s, With[{v = s},
    pretty1wrap[Verbatim[v]] := symbolString[s];
    pretty1[Verbatim[v]] := symbolString[s];
  ], HoldAll],
  $literalPatternVariables
];

(**************************************************************************************************)

prettyInlineColor[color_] := ToString[Style["\[FilledSquare]", color], StandardForm];

(**************************************************************************************************)

SetHoldAllComplete[holdLeafCount, beefyNumericArrayQ];

holdLeafCount[e_] := LeafCount[Unevaluated @ e];
beefyNumericArrayQ[list_] := holdLeafCount[list] >= If[ArrayQ[Unevaluated @ list, _, IntegerQ], 8, 4];
prettyElidedList[list_] := With[
  {dims = Dimensions @ Unevaluated @ list},
  StringJoin @ {"\[LeftAngleBracket]", Riffle[IntegerString /@ dims, ","], "\[RightAngleBracket]"}
];

(**************************************************************************************************)

indentedBlock[begin_, {}, end_] := begin <> end;

indentedBlock[begin_ ? (StringEndsQ["["]), {line_String} /; StringLength[line] > 8, "]"] :=
  StringJoin[StringDrop[begin, -1] <> " @ ", deIndent @ line];

indentedBlock["{", {line_String} /; StringLength[line] > 8, "}"] :=
  StringJoin["List @ ", deIndent @ line];

indentedBlock[begin_, {line_String}, end_] :=
  StringJoin[begin, deIndent @ line, end];

deIndent[line_String] := StringReplace[line, "\n\t" -> "\n"];

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

prettyGraph[g_Graph] := With[{v = VertexList[g], e = EdgeList[g], o = Options[g]},
  indentedBlock["Graph[", Join[{pretty0 @ v, pretty0 @ e}, MapUnevaluated[pretty0, o]], "]"]
];

(**************************************************************************************************)

pretty2 = Case[
  e:$fatHeadP /; TrueQ[$prettyCompression] := prettyCompressed[e];
  e_Symbol := symbolString[e];
  e_ := ToString[Unevaluated @ e, InputForm];
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