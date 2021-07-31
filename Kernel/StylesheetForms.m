(**************************************************************************************************)

PackageExport["Naturals"]

SetUsage @ "Naturals represents the natural numbers."

MakeBoxes[Naturals, StandardForm] := TemplateBox[{}, "Naturals"];
MakeBoxes[Naturals, TraditionalForm] := TemplateBox[{}, "Naturals"];

(********************************************)

PackageExport["RawSymbolForm"]

declareBoxFormatting[
  RawSymbolForm[p_] :>
    rawSymbolBoxes @ p
];

(********************************************)

rBox[e_] := TemplateBox[{e}, "RedForm"];
gBox[e_] := TemplateBox[{e}, "GreenForm"];
bBox[e_] := TemplateBox[{e}, "BlueForm"];
negBox[e_] := TemplateBox[{e}, "NegatedForm"];

(********************************************)

$rawSymbolP = _Symbol | _String | _Subscript | _Superscript | _Subsuperscript;

(**************************************************************************************************)

SetHoldAllComplete[rawSymbolBoxes, toSymbolName];

rawSymbolBoxes = Case[
  RedForm[e_]                 := TemplateBox[List @ % @ e, "RedForm"];
  GreenForm[e_]               := TemplateBox[List @ % @ e, "GreenForm"];
  BlueForm[e_]                := TemplateBox[List @ % @ e, "BlueForm"];
  s_Symbol                    := toSymbolName[s];
  str_String                  := str;
  i_Integer                   := TextString @ i;
  Subscript[a_, b_]           := SubscriptBox[% @ a, % @ b];
  Superscript[a_, b_]         := SuperscriptBox[% @ a, % @ b];
  Subsuperscript[a_, b_, c_]  := SubsuperscriptBox[% @ a, % @ b, % @ c];
]

(* todo: support formal symbols, rewriting them as necessary *)

toSymbolName[e_] := SymbolName[Unevaluated @ e];

(********************************************)

PackageExport["SymbolForm"]

declareBoxFormatting[
  SymbolForm[p_] :>
    TemplateBox[List @ symbolBoxes @ p, "SymbolForm"]
];

(**************************************************************************************************)

$symbolFormsP = Alternatives[
  _SymbolForm,
  _PathQuotientSymbol, _PathGroupoidSymbol, _GroupoidSymbol,
  _PathQuiverSymbol, _QuiverSymbol, _PathSymbol, 
  _WordForm,
  _VertexSymbol,
  _PathMapSymbol, _MapSymbol,
  EllipsisSymbol, BlankSymbol
];

(********************************************)

symbolBoxes = Case[
  s:($symbolFormsP)   := MakeBoxes @ s;
  NegatedForm[n_]     := % @ Negated @ n;
  Negated[n_]         := NegatedBoxForm @ RawBoxes @ % @ n;
  other_              := rawSymbolBoxes @ other;
];

(**************************************************************************************************)

SetHoldAllComplete[makeStandardBoxTemplate];
makeStandardBoxTemplate[args___, tag_] :=
  TemplateBox[
    Map[makeQGBoxes, Unevaluated @ {args}],
    tag
  ];

SetHoldAllComplete[makeTypedTemplateBox, toTypedSymbol];
makeTypedTemplateBox[args___, tag_] :=
  TemplateBox[
    Map[toTypedSymbol, Unevaluated @ {args}],
    tag
  ];

toTypedSymbol = Case[
  Rule[BlankSymbol, _] :=
    MakeBoxes @ BlankSymbol;
  Rule[(c:RedForm|GreenForm|BlueForm)[arg_], type_] :=
    TemplateBox[List @ toTypedSymbol[arg -> type], SymbolName @ c];
  Rule[arg:((type_)[___]), type_] :=
    MakeBoxes @ arg;
  Rule[arg_, type_] := 
    MakeBoxes @ type @ arg;
  arg_ :=
    makeQGBoxes @ arg;
]

(**************************************************************************************************)

SetHoldAllComplete[makeHintedTemplateBox, toHintedSymbol];
makeHintedTemplateBox[args___, tag_] :=
  TemplateBox[
    Map[toHintedSymbol, Unevaluated @ {args}],
    tag
  ];

toHintedSymbol = Case[
  Rule[BlankSymbol, _] :=
    MakeBoxes @ BlankSymbol;
  Rule[(c:RedForm|GreenForm|BlueForm)[arg_], hint_] :=
    TemplateBox[List @ toHintedSymbol[arg -> hint], SymbolName @ c];
  Rule[arg:symsP, hint_] :=
    MakeBoxes @ hint @ arg;
  Rule[arg_, _] := 
    makeQGBoxes @ arg;
  arg_ :=
    makeQGBoxes @ arg,
  {symsP -> $rawSymbolP}
]

(**************************************************************************************************)

PackageExport["BlankSymbol"]

declareBoxFormatting[
  BlankSymbol :> TemplateBox[{}, "BlankSymbol"]
]

(**************************************************************************************************)

PackageExport["PathQuotientSymbol"]

SetUsage @ "
PathQuotientSymbol[q$, mu$] represents the path quiver on quiver q$.
"

declareBoxFormatting[
  PathQuotientSymbol[q_, u_] :> makeTypedTemplateBox[q -> QuiverSymbol, u -> PathMapSymbol, "PathQuotientSymbolForm"]
];

(**************************************************************************************************)

PackageExport["ElementOfForm"]
PackageExport["VertexOfForm"]
PackageExport["EdgeOfForm"]
PackageExport["PathOfForm"]

declareBoxFormatting[
  ElementOfForm[a_, b_] :> makeStandardBoxTemplate[a, b, "ElementOfForm"],
  VertexOfForm[a_, b_] :> makeHintedTemplateBox[a -> VertexSymbol, b -> QuiverSymbol, "VertexOfForm"],
  EdgeOfForm[a_, b_] :> makeHintedTemplateBox[a -> EdgeSymbol, b -> QuiverSymbol, "EdgeOfForm"],
  PathOfForm[a_, b_] :> makeHintedTemplateBox[a -> PathSymbol, b -> QuiverSymbol, "PathOfForm"]
];

(**************************************************************************************************)

(* PackageExport["SignedForm"]

SignedForm[s_String] := FunctionSymbol @ SignedForm @ s;
(* SignedForm[(h:FunctionSymbol|PathMapSymbol)[arg_]] := h[SignedForm @ arg]; *)

declareBoxFormatting[
  SignedForm[(h:FunctionSymbol|PathMapSymbol)[arg_]] :>
    MakeBoxes[head @ SignedForm @ arg],

  SignedForm[e_] :>
    makeStandardBoxTemplate[e, "SignedForm"]
]; *)

(**************************************************************************************************)

PackageExport["FunctionSymbol"]

$namedFunctions = {
  VertexListFunction,
  EdgeListFunction,
  CardinalListFunction,
  SignedLengthFunction,
  LengthFunction,
  WordFunction,
  PathListFunction
};

$functionFormP = Alternatives @@ Join[
  {_FunctionSymbol, _PathMapSymbol},
  $namedFunctions
];

f_FunctionSymbol[args__] := AppliedForm[f, args];

SetHoldAllComplete[symOrStringMatchingQ, isMuQ];

symOrStringMatchingQ[s_String, patt_] := StringMatchQ[s, patt];
symOrStringMatchingQ[s_Symbol, patt_] := StringMatchQ[SymbolName[Unevaluated @ s], patt];
symOrStringMatchingQ[_, _] := False;

isMuQ[s_] := symOrStringMatchingQ[s, "\[Mu]" | "mu"];

declareBoxFormatting[

  FunctionSymbol[f:$functionFormP] :>
    MakeBoxes @ f,

  FunctionSymbol[f_ ? isMuQ] :> 
    MakeBoxes @ PathMapSymbol["\[Mu]"],

  FunctionSymbol[f_] :>
    makeStandardBoxTemplate[f, "FunctionSymbolForm"]
];

(**************************************************************************************************)

PackageExport["AppliedForm"]

declareBoxFormatting[
  AppliedForm[f_, args__] :> makeTypedTemplateBox[f -> FunctionSymbol, args, "AppliedForm"]
];

(**************************************************************************************************)

PackageExport["PathMapSymbol"]

f_PathMapSymbol[args__] := AppliedForm[f, args];

PathMapSymbol[] := PathMapSymbol["\[Mu]"];

declareBoxFormatting[
  PathMapSymbol[mu_ ? isMuQ] :> 
    makeStandardBoxTemplate["\[Mu]", "PathMapSymbolForm"],

  PathMapSymbol[mu_] :>
    makeStandardBoxTemplate[mu, "PathMapSymbolForm"]
]

(**************************************************************************************************)

PackageExport["FunctionSignatureForm"]

declareBoxFormatting[
  FunctionSignatureForm[f_, a_, b_] :>
    makeTypedTemplateBox[f -> FunctionSymbol, a, b, "FunctionSignatureForm"]
]

(**************************************************************************************************)

PackageExport["PathMapSignatureForm"]

PathMapSignatureForm[mu_String, q_String, v_String] := 
  FunctionSignatureForm[
    PathMapSymbol @ mu,
    PathQuiverSymbol @ q,
    GroupoidSymbol @ v
  ];

declareBoxFormatting[
  PathMapSignatureForm[mu_, q_, v_] :>
    makeTypedTemplateBox[mu -> PathMapSymbol, q -> PathQuiverSymbol, v -> GroupoidSymbol, "FunctionSignatureForm"]
]

(**************************************************************************************************)

PackageExport["GroupoidSymbol"]

$groupoidAliases = <|
  "N" -> "Naturals",
  "C" -> "Complexes",
  "R" -> "Reals",
  "Z" -> "Integers",
  "Q" -> "Rationals"
|>

declareBoxFormatting[
  GroupoidSymbol[s_String /; KeyExistsQ[$groupoidAliases, s]] :>
    TemplateBox[List @ TemplateBox[{}, Lookup[$groupoidAliases, s]], "GroupoidSymbolForm"],

  GroupoidSymbol["\[Gamma]"] :>
    MakeBoxes @ PathGroupoidSymbol["Q"],
  
  GroupoidSymbol[n_] :>
    TemplateBox[List @ rawSymbolBoxes @ n, "GroupoidSymbolForm"]

]

(**************************************************************************************************)

PackageExport["GroupSymbol"]

GroupSymbol[] := GroupSymbol["G"];

declareBoxFormatting[

  GroupSymbol[s_String /; KeyExistsQ[$groupoidAliases, s]] :>
    TemplateBox[List @ TemplateBox[{}, Lookup[$groupoidAliases, s]], "GroupSymbolForm"],

  GroupSymbol[n_] :>
    TemplateBox[List @ rawSymbolBoxes @ n, "GroupSymbolForm"]

]

(**************************************************************************************************)

PackageExport["WordGroupSymbol"]

declareBoxFormatting[
  
  WordGroupSymbol[] :>
    TemplateBox[{}, "WordGroupSymbolForm"],

  WordGroupSymbol[s_] :>
    makeTypedTemplateBox[s -> QuiverSymbol, "WordGroupSymbolForm"]

];

(**************************************************************************************************)

PackageExport["GroupElementSymbol"]

GroupElementSymbol[] := GroupElementSymbol["g"];

declareBoxFormatting[

  GroupElementSymbol[n_] :>
    TemplateBox[List @ rawSymbolBoxes @ n, "GroupElementSymbolForm"]

]

(**************************************************************************************************)

PackageExport["GroupMultiplicationForm"]

declareBoxFormatting[
  GroupMultiplicationForm[args___] :>
    makeStandardBoxTemplate[args, "GroupMultiplicationForm"]
];

(**************************************************************************************************)

PackageExport["GroupoidMultiplicationForm"]

declareBoxFormatting[
  GroupoidMultiplicationForm[args___] :>
    makeStandardBoxTemplate[args, "GroupoidMultiplicationForm"]
];

(**************************************************************************************************)

PackageExport["PathGroupoidSymbol"]

PathGroupoidSymbol[] := PathGroupoidSymbol["Q"];

declareBoxFormatting[
  PathGroupoidSymbol[q_] :>
    makeTypedTemplateBox[q -> QuiverSymbol, "PathGroupoidSymbolForm"]
]

(**************************************************************************************************)

PackageExport["PathQuiverSymbol"]

SetUsage @ "
PathQuiverSymbol[q$] represents the path quiver on quiver q$.
"

PathQuiverSymbol[] := PathQuiverSymbol["Q"];

declareBoxFormatting[  
  HoldPattern[PathQuiverSymbol[q_]] :>
    makeTypedTemplateBox[q -> QuiverSymbol, "PathQuiverSymbolForm"]
];

(**************************************************************************************************)

PackageExport["ForwardPathQuiverSymbol"]

SetUsage @ "
ForwardPathQuiverSymbol[q$] represents the path quiver on quiver q$.
"

ForwardPathQuiverSymbol[] := ForwardPathQuiverSymbol["Q", "v"];
ForwardPathQuiverSymbol[q_] := ForwardPathQuiverSymbol[q, "v"];

declareBoxFormatting[
  HoldPattern[ForwardPathQuiverSymbol[q_, v_]] :>
    makeTypedTemplateBox[q -> QuiverSymbol, v -> VertexSymbol, "ForwardPathQuiverSymbolForm"]
];

(**************************************************************************************************)

PackageExport["BackwardPathQuiverSymbol"]

SetUsage @ "
BackwardPathQuiverSymbol[q$] represents the path quiver on quiver q$.
"

BackwardPathQuiverSymbol[] := BackwardPathQuiverSymbol["Q", "v"];
BackwardPathQuiverSymbol[q_] := BackwardPathQuiverSymbol[q, "v"];

declareBoxFormatting[
  HoldPattern[BackwardPathQuiverSymbol[q_, v_]] :>
    makeTypedTemplateBox[q -> QuiverSymbol, v -> VertexSymbol, "BackwardPathQuiverSymbolForm"]
];

(********************************************)

PackageExport["QuiverSymbol"]

QuiverSymbol[] := QuiverSymbol["Q"];

declareBoxFormatting[
  QuiverSymbol[q_] :>
    makeTypedTemplateBox[q -> RawSymbolForm, "QuiverSymbolForm"]
]

(********************************************)

PackageExport["PathSymbol"]

PathSymbol[] := PathSymbol["P"];

declareBoxFormatting[
  PathSymbol[e_] :>
    makeStandardBoxTemplate[e, "PathSymbolForm"]
]

(**************************************************************************************************)

PackageExport["EdgeSymbol"]

declareBoxFormatting[
  EdgeSymbol[a_] :>
    TemplateBox[List @ rawSymbolBoxes @ a, "EdgeSymbolForm"]
]

(**************************************************************************************************)

PackageExport["VertexSymbol"]

declareBoxFormatting[
  VertexSymbol[a_] :>
    TemplateBox[List @ rawSymbolBoxes @ a, "VertexSymbolForm"]
]

(**************************************************************************************************)

PackageExport["CardinalSymbol"]

declareBoxFormatting[
  CardinalSymbol[a_] :>
    TemplateBox[List @ rawSymbolBoxes @ a, "CardinalSymbolForm"]
]

(**************************************************************************************************)

SetHoldAllComplete[makeQGBoxes];

(* this is the general dispatch mechanism for a form of unknown type *)
makeQGBoxes = Case[
  s:namedFnP                := MakeBoxes @ s;
  e:symP                    := symbolBoxes @ e;
  e_Times                   := algebraBoxes[e, "TimesForm"];
  e_Plus                    := algebraBoxes[e, "PlusForm"];
  Minus[e_]                 := makeStandardBoxTemplate[e, "MinusForm"];
  Power[e_, -1]             := makeStandardBoxTemplate[e, "InverseForm"];
  Times[-1, e_]             := makeStandardBoxTemplate[e, "MinusForm"];
  Negated[e_]               := makeStandardBoxTemplate[e, "NegatedForm"];
  DirectedEdge[args__]      := MakeBoxes @ DirectedEdgeForm[args];
  UndirectedEdge[args__]    := MakeBoxes @ UndirectedEdgeForm[args];
  other_                    := MakeBoxes @ other,
  {symP -> $rawSymbolP, namedFnP -> Alternatives @@ $namedFunctions}
];

algebraBoxes[_[args__], tag_] := makeStandardBoxTemplate[args, tag];

(**************************************************************************************************)

PackageExport["DirectedEdgeForm"]

declareBoxFormatting[
  DirectedEdgeForm[a_, b_] :> 
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, "DirectedEdgeForm"],
  DirectedEdgeForm[a_, b_, c_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, c -> CardinalSymbol, "TaggedDirectedEdgeForm"]
]

(**************************************************************************************************)

PackageExport["UndirectedEdgeForm"]

declareBoxFormatting[
  UndirectedEdgeForm[a_, b_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, "UndirectedEdgeForm"],
  UndirectedEdgeForm[a_, b_, c_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, c -> CardinalSymbol, "TaggedUndirectedEdgeForm"]
]

(**************************************************************************************************)

PackageExport["SumForm"]

declareBoxFormatting[
  SumForm[args__] :>
    makeStandardBoxTemplate[args, "SumForm"]
]

(**************************************************************************************************)

PackageExport["ProductForm"]

declareBoxFormatting[
  SumForm[args__] :>
    makeStandardBoxTemplate[args, "ProductForm"]
]

(**************************************************************************************************)

PackageExport["PlusForm"]

declareBoxFormatting[
  PlusForm[args__] :>
    makeStandardBoxTemplate[args, "PlusForm"]
]

(**************************************************************************************************)

PackageExport["TimesForm"]

declareBoxFormatting[
  TimesForm[args__] :>
    makeStandardBoxTemplate[args, "TimesForm"]
]

(**************************************************************************************************)

PackageExport["IsomorphicForm"]

declareBoxFormatting[
  IsomorphicForm[args__] :>
    makeStandardBoxTemplate[args, "IsomorphicForm"]
]

(**************************************************************************************************)

PackageExport["EqualForm"]

declareBoxFormatting[
  EqualForm[args__] :>
    makeStandardBoxTemplate[args, "EqualForm"]
]

(**************************************************************************************************)

PackageExport["NotEqualForm"]

declareBoxFormatting[
  NotEqualForm[args__] :>
    makeStandardBoxTemplate[args, "NotEqualForm"]
]

(**************************************************************************************************)

PackageExport["ImplicitTimesForm"]

declareBoxFormatting[
  ImplicitTimesForm[args__] :>
    makeStandardBoxTemplate[args, "ImplicitTimesForm"]
]

(**************************************************************************************************)

PackageExport["InverseForm"]

declareBoxFormatting[
  InverseForm[e_] :>
    makeStandardBoxTemplate[e, "InverseForm"]
];

(**************************************************************************************************)

PackageExport["RedForm"]

declareBoxFormatting[
  RedForm[e_] :>
    TemplateBox[List @ makeQGBoxes @ e, "RedForm"]
];

(**************************************************************************************************)

PackageExport["GreenForm"]

declareBoxFormatting[
  GreenForm[e_] :>
    TemplateBox[List @ makeQGBoxes @ e, "GreenForm"]
];

(**************************************************************************************************)

PackageExport["BlueForm"]

declareBoxFormatting[
  BlueForm[e_] :>
    TemplateBox[List @ makeQGBoxes @ e, "BlueForm"]
];

(**************************************************************************************************)

PackageExport["NegatedBoxForm"]

SetHoldFirst[NegatedBoxForm];
NegatedBoxForm[e_] := makeStandardBoxTemplate[e, "NegatedForm"]

(********************************************)

PackageExport["WordVectorForm"]

WordVectorForm[p_String, args___] := WordVectorForm[ToPathWord @ p, args]

declareBoxFormatting[
  WordVectorForm[p_, dir_:"Forward"] :>
    TemplateBox[{wordBoxes @ p, dir}, "WordVectorForm"]
];

(********************************************)

makePathBoxTemplate[args__, tag_] :=
  TemplateBox[
    Map[maybeParen[PathSymbol], Unevaluated @ {args}],
    tag
  ];

(********************************************)

PackageExport["$PathComposeSymbol"]

$PathComposeSymbol = "\[Proportion]";

PackageExport["PathComposeForm"]

declareBoxFormatting[
  PathComposeForm[a_, b__] :>
    makePathBoxTemplate[a, b, "PathComposeForm"]
];

(********************************************)

PackageExport["PathTranslateForm"]

declareBoxFormatting[
  PathTranslateForm[a_, b__] :>
    makePathBoxTemplate[a, b, "PathTranslateForm"]
];

(********************************************)

PackageExport["PathBackwardTranslateForm"]

declareBoxFormatting[
  PathBackwardTranslateForm[a_, b__] :>
    makePathBoxTemplate[a, b, "PathBackwardTranslateForm"]
];

(********************************************)

PackageExport["PathHeadVectorForm"]

declareBoxFormatting[
  PathHeadVectorForm[a_] :>
    makePathBoxTemplate[a, "PathHeadVectorForm"]
];

(********************************************)

PackageExport["PathTailVectorForm"]

declareBoxFormatting[
  PathTailVectorForm[a_] :>
    makePathBoxTemplate[a, "PathTailVectorForm"]
];

(********************************************)

PackageExport["PathReverseForm"]

declareBoxFormatting[
  PathReverseForm[a_] :>
    makePathBoxTemplate[a, "PathReverseForm"]
];

(********************************************)

PackageExport["PathForwardDifferenceForm"]

PathForwardDifferenceForm[w_String] :=
  PathForwardDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathForwardDifferenceForm[w_] :>
    MakeBoxes @ PathDifferenceForm[w, "Forward"]
];

(********************************************)

PackageExport["PathBackwardDifferenceForm"]

PathBackwardDifferenceForm[w_String] :=
  PathBackwardDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathBackwardDifferenceForm[w_] :>
    MakeBoxes @ PathDifferenceForm[w, "Backward"]
];

(********************************************)

PackageExport["PathCentralDifferenceForm"]

PathCentralDifferenceForm[w_String] :=
  PathCentralDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathCentralDifferenceForm[w_] :>
    MakeBoxes @ PathDifferenceForm[w, "Central"]
];

(********************************************)

PackageExport["PathDifferenceForm"]

PathDifferenceForm[w_String] :=
  PathDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathDifferenceForm[w_, dir_] :>
    TemplateBox[{pathOrWordBoxes @ w, dir}, "PathDifferenceForm"]
];

toPathWordOrSymbol = Case[
  s:("P" | "Q" | "R" | "T") := PathSymbol @ s;
  w_                        := ToPathWord[w];
];

pathOrWordBoxes = Case[
  s:("P" | "Q" | "R" | "T") := MakeBoxes @ PathSymbol @ s;
  s:syms        := MakeBoxes @ PathSymbol @ s;
  s_List        := wordBoxes @ s;
  p:(_PathSymbol | WordForm) := MakeBoxes @ p,
  {syms -> $rawSymbolP}
]

(********************************************)

SetHoldAllComplete[maybeParen, maybeParenBoxes];

maybeParen[h_][b_] := Block[{$eh = h}, maybeParenBoxes @  b];

maybeParenBoxes = Case[
  s:syms                                := With[{head = $eh}, MakeBoxes @ head @ s];
  e_InverseForm                         := MakeBoxes @ e;
  (e:(head_[___])) /; MatchQ[head, $eh] := MakeBoxes @ e;
  other_                                := MakeBoxes @ ParenthesesForm @ other,
  {syms -> $rawSymbolP}
];

(********************************************)

PackageExport["ListForm"]

declareBoxFormatting[
  ListForm[args__] :>
    makeStandardBoxTemplate[args, "ListForm"]
]


(********************************************)

PackageExport["ParenthesesForm"]

declareBoxFormatting[
  ParenthesesForm[args__] :>
    makeStandardBoxTemplate[args, "ParenthesesForm"]
]

(********************************************)

PackageExport["TupleForm"]

declareBoxFormatting[
  TupleForm[args__] :>
    makeStandardBoxTemplate[args, "TupleForm"]
]

(********************************************)

PackageExport["ConcatenationForm"]

declareBoxFormatting[
  ConcatenationForm[args__] :>
    makeStandardBoxTemplate[args, "ConcatenationForm"]
]

(********************************************)

PackageExport["ParenPathWordForm"]

ParenPathWordForm[args__] := ParenthesesForm @ PathWordForm @ args;

(********************************************)

PackageExport["RewriteForm"]

declareBoxFormatting[
  RewriteForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "RewriteForm"]
];

(**************************************************************************************************)

PackageExport["EllipsisSymbol"]

declareBoxFormatting[
  EllipsisSymbol :> TemplateBox[{}, "EllipsisSymbol"]
]

(**************************************************************************************************)

PackageExport["VertexListFunction"]
PackageExport["EdgeListFunction"]
PackageExport["PathListFunction"]
PackageExport["CardinalListFunction"]
PackageExport["SignedLengthFunction"]
PackageExport["LengthFunction"]
PackageExport["WordFunction"]

declareFunctionFormatting[sym_] := With[
  {name = StringDelete[SymbolName[sym], "Function"]},
  declareBoxFormatting[
    sym :> TemplateBox[{name}, "NamedFunctionSymbolForm"],
    sym[args___] :> makeTypedTemplateBox[sym, args, "AppliedForm"]
  ]
];

Scan[declareFunctionFormatting, $namedFunctions];

(********************************************)

PackageExport["RowForm"]

declareBoxFormatting[
  RowForm[args__] :>
    makeStandardBoxTemplate[args, "RowForm"]
];

(********************************************)

PackageExport["IdentityElementForm"]

declareBoxFormatting[
  IdentityElementForm[args___] :>
    makeStandardBoxTemplate[args, "IdentityElementForm"]
];


(********************************************)

PackageExport["PathWordForm"]

PathWordForm[a_, b_String, c_] := PathWordForm[a, ToPathWord @ b, c];

declareBoxFormatting[
  PathWordForm[t_, w_, h_] :>
    makeTypedTemplateBox[t -> VertexSymbol, w -> WordForm, h -> VertexSymbol, "PathWordForm"]
];

(********************************************)

PackageExport["$AutoColorCardinals"]

$AutoColorCardinals = True;

(********************************************)

PackageExport["WordForm"]

WordForm[s_String] := WordForm @ ToPathWord @ s;

declareBoxFormatting[
  WordForm[e_] :> wordBoxes[e]
];

SetHoldAllComplete[wordBoxes];
wordBoxes = Case[
  {}|""                               := TemplateBox[{}, "EmptyWordForm"];
  word_String                         := % @ ToPathWord @ word;
  (Times|ConcatenationForm)[a_, b_]   := TemplateBox[% /@ {a, b}, "ConcatenationForm"];
  list_List                           := TemplateBox[tryColorCardinals @ cardinalBoxes @ list, "WordForm"];
  Negated[s_]                         := negBox[% @ s];
  s:syms                              := TemplateBox[List @ rawSymbolBoxes @ s, "WordSymbolForm"],
  {syms -> $rawSymbolP}
];

cardinalBoxes[list_List] := Map[cardinalBox, list];

cardSymBox[e_] := TemplateBox[{e}, "CardinalSymbolForm"];

toNegCard[TemplateBox[{e_}, "CardinalSymbolForm"]] := 
  TemplateBox[{e}, "NegatedCardinalSymbolForm"];

cardinalBox = Case[
  s_String    := cardSymBox @ s;
  i_Integer   := cardSymBox @ TextString @ i;
  s:syms      := cardSymBox @ rawSymbolBoxes @ s;
  Negated[s_] := toNegCard @ % @ s,
  {syms -> $rawSymbolP}
]

(* only colors if it is all-or-nothing *)
tryColorCardinals[list_] /; $AutoColorCardinals := 
  If[SubsetQ[{"r", "g", "b"}, list /. TemplateBox[{a_}, _] :> a],
    list /. $colorCardinalRules,
    list
  ];

tryColorCardinals[list_] := list;

$colorCardinalRules = {
  b:TemplateBox[{"r"}, "CardinalSymbolForm" | "NegatedCardinalSymbolForm"] -> rBox @ b,
  b:TemplateBox[{"g"}, "CardinalSymbolForm" | "NegatedCardinalSymbolForm"] -> gBox @ b,
  b:TemplateBox[{"b"}, "CardinalSymbolForm" | "NegatedCardinalSymbolForm"] -> bBox @ b
};


(********************************************)

PackageExport["PlainWordForm"]

PlainWordForm[s_String] := PlainWordForm @ ToPathWord @ s;

declareBoxFormatting[
  PlainWordForm[e_] :> Block[{$AutoColorCardinals = False}, wordBoxes[e]]
];


