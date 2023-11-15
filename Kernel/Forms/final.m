(**************************************************************************************************)

PublicTypesettingForm[SymbolForm]

DefineTaggedForm[SymbolForm]

(**************************************************************************************************)

(* TODO: Put these in SymbolTranslation.m *)
$binaryRelationMapping = <|
  Equal              -> "=",
  Plus               -> "+",
  Unequal            -> "≠",
  Subset             -> "⊂",
  SubsetEqual        -> "⊆",
  Superset           -> "⊃",
  SupersetEqual      -> "⊇",
  NotSubset          -> "⊄",
  NotSubsetEqual     -> "⊈",
  NotSuperset        -> "⊅",
  NotSupersetEqual   -> "⊉",
  TildeEqual         -> "≃",
  TildeFullEqual     -> "≅",
  TildeTilde         -> "≈",
  LessEqual          -> "≤",
  Less               -> "<",
  GreaterEqual       -> "≥",
  Greater            -> ">"
|>;

(**************************************************************************************************)

PublicTypesettingForm[Form]

DefineStandardTraditionalForm[
  Form[e_] :> TagBox[MakeQGBoxes @ e, "QG"]
]

(**************************************************************************************************)

PublicFunction[MakeQGBoxes, MakeQGBoxSequence]

SetHoldAllComplete[MakeQGBoxes, MakeQGBoxSequence];

MakeQGBoxSequence[] := Sequence[];
MakeQGBoxSequence[e_] := MakeQGBoxes[e];
MakeQGBoxSequence[e___] := Sequence @@ Map[MakeQGBoxes, {e}];

$binaryRelationHeads = Alternatives @@ Keys[$binaryRelationMapping];

$domainsP = Alternatives[Integers, Reals, Rationals, Complexes, Naturals, PositiveNaturals, PositiveReals, UnitInterval, Primes];

(* this is the general dispatch mechanism for a form of unknown type *)
MakeQGBoxes = Case[
  None | Null               := "";
  d:domainsP                := MakeBoxes @ d;
  e:symP                    := symbolBoxes @ e;
  e_Subtract                := algebraBoxes[e, "SubtractForm"];
  Plus[a_, Times[-1, b_]]   := MakeBoxes @ SubtractForm[a, b];
  Plus[a_, Times[n_Int ? Negative, b_]] := With[{n2 = Abs @ n}, MakeBoxes @ SubtractForm[a, ImplicitTimesForm[n2, b]]];
  Times[n_Int, e_]          := MakeBoxes @ ImplicitTimesForm[n, e];
  e_Times                   := algebraBoxes[e, "ImplicitTimesForm"];
  e_Plus                    := algebraBoxes[e, "PlusForm"];
  Equal[a_, b_]             := MakeBoxes @ EqualForm[a, b];
  Unequal[a_, b_]           := MakeBoxes @ UnequalForm[a, b];
  h:binHeads                := Lookup[$binaryRelationMapping, h];
  (h:binHeads)[args__]      := With[{str = Lookup[$binaryRelationMapping, h]}, MakeBoxes @ BinaryRelationForm[str][args]];
  (i_InverseForm)[args__]   := MakeBoxes @ AppliedForm[i, args];
  Minus[e_]                 := makeTemplateBox[e, "MinusForm"];
  Power[e_, -1]             := makeTemplateBox[e, "InverseForm"];
  Times[-1, e_]             := makeTemplateBox[e, "MinusForm"];
  Inverted[e_]              := makeTemplateBox[e, "InvertedForm"];
  DirectedEdge[args__]      := MakeBoxes @ DirectedEdgeForm[args];
  UndirectedEdge[args__]    := MakeBoxes @ UndirectedEdgeForm[args];
  Labeled[a_, l_]           := MakeBoxes @ ParenthesesLabeledForm[a, l];
  Text[t_]                  := MakeBoxes @ MathTextForm[t];
  Row[{r__}]                := MakeBoxes @ RowForm[r];
  Modulo[n_]                := MakeBoxes @ ModuloForm[n];
  Invisible[n_]             := TemplateBox[List @ MakeBoxes @ n, "InvisibleForm"];
  {a_, b__}                 := MakeBoxes @ TupleForm[a, b];
  Composition[a___]         := MakeBoxes @ FunctionCompositionForm[a];
  RightComposition[a___]    := MakeBoxes @ RightFunctionCompositionForm[a];
  s_Str                     := s;
  other_                    := MakeBoxes @ other,
  {lsymsP -> $literalSymbolsP, symP -> Rest[$rawSymbolP], binHeads -> $binaryRelationHeads, domainsP -> $domainsP}
];

$TemplateKatexFunction["InvisibleForm"] := "phantom";

algebraBoxes[_[args__], tag_] := makeTemplateBox[args, tag];

(**************************************************************************************************)

PrivateFunction[rawSymbolBoxes]

SetHoldAllComplete[rawSymbolBoxes, toSymbolName];

rawSymbolBoxes = Case[
  l:lsymsP                    := MakeBoxes @ l;
  (c:colorP)[e_]              := TemplateBox[List @ % @ e, SymbolName @ c];
  s_Symbol                    := toSymbolName[s];
  str_Str                     := str;
  i_Int                       := TextString @ i;
  s_SymbolForm                := MakeBoxes @ s;
  PrimedForm[x_]              := TemplateBox[List @ % @ x, "PrimedForm"];
  Subscript[a_, b_]           := SubscriptBox[MakeQGBoxes @ a, MakeQGBoxes @ b];
  Subscript[a_, b_, c_]       := SubscriptBox[MakeQGBoxes @ a, RBox[MakeQGBoxes @ b, ",", MakeQGBoxes @ c]];
  Subscript[a_, b_, c_, d_]   := SubscriptBox[MakeQGBoxes @ a, RBox[MakeQGBoxes @ b, ",", MakeQGBoxes @ c, ",", MakeQGBoxes @ d]];
  Superscript[a_, b_]         := SuperscriptBox[MakeQGBoxes @ a, MakeQGBoxes @ b];
  Superscript[a_, b_, c_]     := SuperscriptBox[MakeQGBoxes @ a, RBox[MakeQGBoxes @ b, ",", MakeQGBoxes @ c]];
  Superscript[a_, b_, c_, d_] := SuperscriptBox[MakeQGBoxes @ a, RBox[MakeQGBoxes @ b, ",", MakeQGBoxes @ c, ",", MakeQGBoxes @ d]];
  Subsuperscript[a_, b_, c_]  := SubsuperscriptBox[% @ a, MakeQGBoxes @ b, MakeQGBoxes @ c];
  m_WhiteCircleModifierForm   := MakeBoxes @ m;
  m_BlackCircleModifierForm   := MakeBoxes @ m;
,
  {lsymsP -> $literalSymbolsP, colorP -> $colorFormP}
]

(* todo: support formal symbols, rewriting them as necessary *)

toSymbolName[e_] := SymbolName[Unevaluated @ e];

(**************************************************************************************************)

PrivateFunction[makeNaryHintedTemplateBox, makeHintedTemplateBox, toHintedSymbol]

SetHoldAllComplete[makeNaryHintedTemplateBox, makeHintedTemplateBox, toHintedSymbol];

makeNaryHintedTemplateBox[None, args___, tag_] :=
  makeTemplateBox[args, tag];

makeNaryHintedTemplateBox[hint_, args___, tag_] :=
  TemplateBox[
    MapUnevaluated[toHintedSymbol[# -> hint]&, {args}],
    tag
  ];

makeHintedTemplateBox[args___, tag_] :=
  TemplateBox[
    MapUnevaluated[toHintedSymbol, {args}],
    tag
  ];

toHintedSymbol = Case[
  Rule[None, _] :=
    "";
  Rule[Form[e_], _] :=
    MakeQGBoxes @ e;
  Rule[e_, None] :=
    e;
  Rule[e_, Automatic] :=
    MakeQGBoxes @ e;
  Rule[e_, m_maybeParen] :=
    m @ e;
  Rule[l:literalP, _] :=
    MakeBoxes @ l;
  Rule[BlankSymbol, _] :=
    MakeBoxes @ BlankSymbol;
  Rule[(c:colorsP)[arg_], hint_] :=
    TemplateBox[List @ toHintedSymbol[arg -> hint], SymbolName @ c];
  Rule[arg:symsP, hint_] :=
    MakeBoxes @ hint @ arg;
  Rule[arg_, _] :=
    MakeQGBoxes @ arg;
  arg_ :=
    MakeQGBoxes @ arg,
  {symsP -> $rawSymbolP, colorsP -> $colorFormP, literalP -> $literalSymbolsP}
]

(**************************************************************************************************)

PrivateFunction[maybeParen]

SetHoldAllComplete[maybeParen, maybeParenBoxes];

maybeParen[h_][b_] := Block[{$eh = h, $ehc = If[H[h] === Alternatives, P1 @ h, h]}, maybeParenBoxes @  b];

maybeParenBoxes = Case[
  NoParenthesesForm[e_]                 := MakeBoxes @ e;
  l:lsymsP                              := MakeBoxes @ l;
  s:symsP                               := With[{head = $ehc}, MakeBoxes @ head @ s];
  e:_InverseForm | _GroupInverseForm | _GroupoidInverseForm | _AppliedForm := MakeBoxes @ e;
  (e:(head_[___])) /; MatchQ[head, $eh] := MakeBoxes @ e;
  e:(colsP[_])                          := MakeBoxes @ e;
  other_                                := MakeBoxes @ ParenthesesForm @ other,
  {symsP -> $rawSymbolP, colsP -> $colorFormP, lsymsP -> $literalSymbolsP}
];