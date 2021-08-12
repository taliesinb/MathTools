(**************************************************************************************************)

PackageExport["Naturals"]

SetUsage @ "Naturals represents the natural numbers."

MakeBoxes[Naturals, StandardForm] := TemplateBox[{}, "Naturals"];
MakeBoxes[Naturals, TraditionalForm] := TemplateBox[{}, "Naturals"];

applyRiffled[f_, op_][args___] := f[riffled[op][args]];

(********************************************)

riffled[op_][a_] := a;
riffled[op_][a_, b_] := {a, op, b}
riffled[op_][a_, b_, c__] := Riffle[{a, b, c}, op];

(********************************************)

katexNary[op_][a_] := op[a];
katexNary[op_][a_, b_] := op[a][b];
katexNary[op_][a_, b_, c_] := op[a][b][c];
katexNary[op_][a_, b_, c_, d_] := op[a][b][c][d];
katexNary[op_][a_, b_, c_, d_, e_] := op[a][b][c][d][e];

(********************************************)

(* katexComma[op_][a_] := op[a];
katexComma[op_][a_, b_] := op[a, " , ", b]
katexComma[op_][a_, b_, c__] := op @@ Riffle[{a, b, c}, " , "];

 *)(********************************************)

PackageExport["RawSymbolForm"]

declareBoxFormatting[
  RawSymbolForm[p_] :>
    rawSymbolBoxes @ p
];

(**************************************************************************************************)

declareSymbolForm::badsym = "Name of symbol `` should end in SymbolForm."
declareSymbolForm[head_Symbol] := Scope[
  name = SymbolName @ head;
  Which[
    StringEndsQ[name, "SymbolForm"], Null,
    StringEndsQ[name, "Symbol"], name = name <> "Form", 
    True, ReturnFailed["badsym", head]
  ];
  With[{name2 = name},
    declareBoxFormatting[head[s_] :> makeTypedTemplateBox[s -> RawSymbolForm, name2]];
  ];
  $TemplateKatexFunction[name] = LowerCaseFirst @ StringDrop[name, -10];
];

(********************************************)

rBox[e_] := TemplateBox[{e}, "RedForm"];
gBox[e_] := TemplateBox[{e}, "GreenForm"];
bBox[e_] := TemplateBox[{e}, "BlueForm"];
negBox[e_] := TemplateBox[{e}, "NegatedForm"];

(********************************************)

$rawSymbolP = _Symbol | _String | _Subscript | _Superscript | _Subsuperscript | EllipsisSymbol;

(**************************************************************************************************)

SetHoldAllComplete[rawSymbolBoxes, toSymbolName];

rawSymbolBoxes = Case[
  l:lsymsP                    := MakeBoxes @ l;
  RedForm[e_]                 := TemplateBox[List @ % @ e, "RedForm"];
  GreenForm[e_]               := TemplateBox[List @ % @ e, "GreenForm"];
  BlueForm[e_]                := TemplateBox[List @ % @ e, "BlueForm"];
  RedGreenForm[e_]            := TemplateBox[List @ % @ e, "RedGreenForm"];
  RedBlueForm[e_]             := TemplateBox[List @ % @ e, "RedBlueForm"];
  GreenBlueForm[e_]           := TemplateBox[List @ % @ e, "GreenBlueForm"];
  s_Symbol                    := toSymbolName[s];
  str_String                  := str;
  i_Integer                   := TextString @ i;
  Subscript[a_, b_]           := SubscriptBox[makeQGBoxes @ a, makeQGBoxes @ b];
  Subscript[a_, b_, c_]       := SubscriptBox[makeQGBoxes @ a, RowBox[{makeQGBoxes @ b, ",", makeQGBoxes @ c}]];
  Superscript[a_, b_]         := SuperscriptBox[makeQGBoxes @ a, makeQGBoxes @ b];
  Subsuperscript[a_, b_, c_]  := SubsuperscriptBox[% @ a, makeQGBoxes @ b, makeQGBoxes @ c],
  {lsymsP -> $literalSymbolsP}
]

(* todo: support formal symbols, rewriting them as necessary *)

toSymbolName[e_] := SymbolName[Unevaluated @ e];

(********************************************)

PackageExport["SymbolForm"]

declareBoxFormatting[
  SymbolForm[p_] :>
    TemplateBox[List @ symbolBoxes @ p, "SymbolForm"]
];

$TemplateKatexFunction["SymbolForm"] = "sym";

(********************************************)

PackageExport["PlainTextForm"]

declareBoxFormatting[
  PlainTextForm[p_] :>
    TemplateBox[List @ MakeBoxes @ p, "PlainTextForm"]
];

$TemplateKatexFunction["PlainTextForm"] = "textrm";

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
  f_PlainTextForm     := MakeBoxes @ f;
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

$colorFormP = Alternatives[
  RedForm, BlueForm, GreenForm,
  RedBlueForm, GreenBlueForm, RedGreenForm, 
  DarkGrayForm, MediumGrayForm, LightGrayForm
];

$literalSymbolsP = EllipsisSymbol | EmptyTokenSymbol | FilledTokenSymbol;

toTypedSymbol = Case[
  Rule[e_, None] := e;
  Rule[BlankSymbol, _] :=
    MakeBoxes @ BlankSymbol;
  Rule[(c:colorsP)[arg_], type_] :=
    TemplateBox[List @ toTypedSymbol[arg -> type], SymbolName @ c];
  Rule[arg:((type_)[___]), type_] :=
    MakeBoxes @ arg;
  Rule[arg_, type_] := 
    MakeBoxes @ type @ arg;
  arg_ :=
    makeQGBoxes @ arg,
  {colorsP -> $colorFormP}
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
  Rule[(c:colorsP)[arg_], hint_] :=
    TemplateBox[List @ toHintedSymbol[arg -> hint], SymbolName @ c];
  Rule[arg:symsP, hint_] :=
    MakeBoxes @ hint @ arg;
  Rule[arg_, _] := 
    makeQGBoxes @ arg;
  arg_ :=
    makeQGBoxes @ arg,
  {symsP -> $rawSymbolP, colorsP -> $colorFormP}
]

(**************************************************************************************************)

PackageExport["BlankSymbol"]

declareBoxFormatting[
  BlankSymbol :> TemplateBox[{}, "BlankSymbol"]
]

$TemplateKatexFunction["BlankSymbol"] = Function["\\blank"];

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

$TemplateKatexFunction["ElementOfForm"] = "elemOf";
$TemplateKatexFunction["VertexOfForm"] = "vertOf";
$TemplateKatexFunction["EdgeOfForm"] = "edgeOf";
$TemplateKatexFunction["PathOfForm"] = "pathOf";

(**************************************************************************************************)

PackageExport["EdgesSymbol"]
PackageExport["VerticesSymbol"]

declareBoxFormatting[
  EdgesSymbol[q_] :> makeHintedTemplateBox[q -> QuiverSymbol, "EdgesSymbolForm"],
  VerticesSymbol[q_] :> makeHintedTemplateBox[q -> QuiverSymbol, "VerticesSymbolForm"]
]

$TemplateKatexFunction["EdgesSymbolForm"] = "edges";
$TemplateKatexFunction["VerticesSymbolForm"] = "vertices";

(**************************************************************************************************)

PackageExport["VertexFieldSymbol"]
PackageExport["EdgeFieldSymbol"]

declareSymbolForm[VertexFieldSymbol];
declareSymbolForm[EdgeFieldSymbol];

PackageExport["PathVectorSpaceSymbol"]
PackageExport["PathVectorSymbol"]
PackageExport["PathWeightSymbol"]

declareSymbolForm[PathVectorSpaceSymbol];
declareSymbolForm[PathVectorSymbol];
declareSymbolForm[PathWeightSymbol];

(**************************************************************************************************)

PackageExport["BasisPathVectorSymbol"]
PackageExport["BasisPathWeightSymbol"]

declareBoxFormatting[
  BasisPathVectorSymbol[sub_] :> makeStandardBoxTemplate[sub, "BasisPathVectorSymbolForm"],
  BasisPathWeightSymbol[sub_] :> makeStandardBoxTemplate[sub, "BasisPathWeightSymbolForm"]
];

$TemplateKatexFunction["BasisPathVectorSymbolForm"] = "basisPath";
$TemplateKatexFunction["BasisPathWeightSymbolForm"] = "basisPathWeight";

(**************************************************************************************************)

PackageExport["BaseFieldSymbol"]

BaseFieldSymbol[] := BaseFieldSymbol["K"];

declareBoxFormatting[
  BaseFieldSymbol[s_] :> makeStandardBoxTemplate[s, "BaseFieldSymbolForm"]
];

$TemplateKatexFunction["BaseFieldSymbolForm"] = "baseField";

(**************************************************************************************************)

PackageExport["FiniteFieldSymbol"]

declareBoxFormatting[
  FiniteFieldSymbol[n_] :> makeStandardBoxTemplate[n, "FiniteField"]
];

$TemplateKatexFunction["FiniteField"] = "finiteField";

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
  PathListFunction,
  HeadVertexFunction, TailVertexFunction,
  AutomorphismsFunction,
  BasisFunction,
  SupportFunction
};

$functionHeads = {
  FunctionSymbol, PathMapSymbol, 
  GroupoidFunctionSymbol, GroupFunctionSymbol,
  PathHomomorphismSymbol, GraphHomomorphismSymbol,
  InverseForm
}

setupGrabbingRule[sym_] := (
  sym /: Subscript[sym[inner_], rest__] := sym[Subscript[inner, rest]];
  sym /: Superscript[sym[inner_], rest__] := sym[Superscript[inner, rest]];
  sym /: Subsuperscript[sym[inner_], rest__] := sym[Subsuperscript[inner, rest]];
  f_sym[args__] := AppliedForm[f, args];
);

Scan[setupGrabbingRule, $functionHeads];


$functionFormP = Alternatives @@ Join[
  Blank /@ $functionHeads,
  $namedFunctions
];

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

$TemplateKatexFunction["FunctionSymbolForm"] = "function";


(**************************************************************************************************)

PackageExport["AppliedForm"]

declareBoxFormatting[
  AppliedForm[f_, args__] :> makeStandardBoxTemplate[f -> FunctionSymbol, args, "AppliedForm"]
];

$TemplateKatexFunction["AppliedForm"] = appliedKatex;

appliedKatex[f_, args___] := {f, "(", Riffle[{args}, ","], ")"};

(**************************************************************************************************)

PackageExport["OperatorAppliedForm"]

declareBoxFormatting[
  OperatorAppliedForm[f_, g_] :> makeStandardBoxTemplate[f, g, "OperatorAppliedForm"]
];

$TemplateKatexFunction["OperatorAppliedForm"] = operatorAppliedKatex;

operatorAppliedKatex[f_, g_] := {f, "\,", g};

(**************************************************************************************************)

PackageExport["PathMapSymbol"]

PathMapSymbol[] := PathMapSymbol["\[Mu]"];

declareBoxFormatting[
  PathMapSymbol[mu_ ? isMuQ] :> 
    makeStandardBoxTemplate["\[Mu]", "PathMapSymbolForm"],

  PathMapSymbol[mu_] :>
    makeStandardBoxTemplate[mu, "PathMapSymbolForm"]
]

$TemplateKatexFunction["PathMapSymbolForm"] = "pathMap";

(**************************************************************************************************)

PackageExport["FunctionSignatureForm"]

FunctionSignatureForm[f_, a_List, b_] :=
  FunctionSignatureForm[f, TupleForm @@ a, b];

FunctionSignatureForm[f_, a_, b_List] :=
  FunctionSignatureForm[f, a, TupleForm @@ b];

declareBoxFormatting[
  FunctionSignatureForm[f_, a_, b_] :>
    makeTypedTemplateBox[f -> FunctionSymbol, a, b, "FunctionSignatureForm"]
]

$TemplateKatexFunction["FunctionSignatureForm"] = "functionSignature";

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

$TemplateKatexFunction["GroupoidSymbolForm"] = "groupoid";

(**************************************************************************************************)

PackageExport["GraphHomomorphismSymbol"]

GraphHomomorphismSymbol[] := GraphHomomorphismSymbol["\[Pi]"]

declareBoxFormatting[
  
  GraphHomomorphismSymbol[f_] :>
    TemplateBox[List @ rawSymbolBoxes @ f, "GraphHomomorphismSymbolForm"]

]

$TemplateKatexFunction["GraphHomomorphismSymbolForm"] = "graphHomomorphism";

(**************************************************************************************************)

PackageExport["PathHomomorphismSymbol"]

PathHomomorphismSymbol[] := PathHomomorphismSymbol["\[Rho]"]

declareSymbolForm[PathHomomorphismSymbol];

(**************************************************************************************************)

PackageExport["GroupoidFunctionSymbol"]

GroupoidFunctionSymbol[] := GroupoidFunctionSymbol["\[Mu]"]

declareSymbolForm[GroupoidFunctionSymbol];

(**************************************************************************************************)

PackageExport["GroupFunctionSymbol"]

GroupFunctionSymbol[] := GroupoidFunctionSymbol["\[Pi]"]

declareSymbolForm[GroupFunctionSymbol];

(**************************************************************************************************)

PackageExport["GroupSymbol"]

GroupSymbol[] := GroupSymbol["G"];

declareBoxFormatting[

  GroupSymbol[s_String /; KeyExistsQ[$groupoidAliases, s]] :>
    TemplateBox[List @ TemplateBox[{}, Lookup[$groupoidAliases, s]], "GroupSymbolForm"],

  GroupSymbol[n_] :>
    TemplateBox[List @ rawSymbolBoxes @ n, "GroupSymbolForm"]

]

$TemplateKatexFunction["GroupSymbolForm"] = "group";

(**************************************************************************************************)

PackageExport["GeneralLinearGroup"]

declareBoxFormatting[
  GeneralLinearGroup[n_] :>
    TemplateBox[{rawSymbolBoxes @ n}, "GeneralLinearGroupForm"]
];

$TemplateKatexFunction["GeneralLinearGroupForm"] = "gl";

(**************************************************************************************************)

PackageExport["WordGroupSymbol"]

declareBoxFormatting[
  
  WordGroupSymbol[] :>
    TemplateBox[{}, "WordGroupSymbolForm"],

  WordGroupSymbol[s_] :>
    makeTypedTemplateBox[s -> QuiverSymbol, "WordGroupSymbolForm"]

];

$TemplateKatexFunction["WordGroupSymbolForm"] = "wordGroup";

(**************************************************************************************************)

PackageExport["GroupElementSymbol"]

GroupElementSymbol[] := GroupElementSymbol["g"];

declareSymbolForm[GroupElementSymbol];

(**************************************************************************************************)

PackageExport["GroupoidElementSymbol"]

GroupoidElementSymbol[] := GroupoidElementSymbol["g"];

declareSymbolForm[GroupoidElementSymbol];

(**************************************************************************************************)

PackageExport["GroupInverseForm"]
PackageExport["GroupoidInverseForm"]

declareBoxFormatting[
  GroupInverseForm[g_] :>
    TemplateBox[List @ maybeParen[GroupElementSymbol] @ g, "GroupInverseForm"],

  GroupoidInverseForm[g_] :>
    TemplateBox[List @ maybeParen[GroupoidElementSymbol] @ g, "GroupoidInverseForm"]
];

$TemplateKatexFunction["GroupInverseForm"] = "groupInv";
$TemplateKatexFunction["GroupoidInverseForm"] = "groupoidInv";

(**************************************************************************************************)

PackageExport["MatrixDotForm"]

declareBoxFormatting[
  MatrixDotForm[args__] :>
    TemplateBox[
      Map[maybeParen[MatrixSymbol], Unevaluated @ {args}],
      "MatrixDotForm"
    ],

  MatrixDotForm[] :>
    makeStandardBoxTemplate[args, "MatrixDotSymbol"]
];

$TemplateKatexFunction["MatrixDotForm"] = riffled[" \\cdot "];
$TemplateKatexFunction["MatrixDotSymbol"] = " \\cdot "&;


(**************************************************************************************************)

PackageExport["GroupMultiplicationForm"]

declareBoxFormatting[
  GroupMultiplicationForm[args__] :>
    TemplateBox[
      Map[maybeParen[GroupElementSymbol], Unevaluated @ {args}],
      "GroupMultiplicationForm"
    ],

  GroupMultiplicationForm[] :>
    makeStandardBoxTemplate[args, "GroupMultiplicationSymbol"]
];

$TemplateKatexFunction["GroupMultiplicationForm"] = riffled[" \\Gmult "];
$TemplateKatexFunction["GroupMultiplicationSymbol"] = " \\Gmult "&;

(**************************************************************************************************)

PackageExport["GroupoidMultiplicationForm"]

declareBoxFormatting[
  GroupoidMultiplicationForm[args__] :>
    TemplateBox[
      Map[maybeParen[GroupoidElementSymbol], Unevaluated @ {args}],
      "GroupoidMultiplicationForm"
    ],

  GroupoidMultiplicationForm[] :>
    makeStandardBoxTemplate[args, "GroupoidMultiplicationSymbol"]
];

$TemplateKatexFunction["GroupoidMultiplicationForm"] = riffled[" \\gmult "];
$TemplateKatexFunction["GroupoidMultiplicationSymbol"] = " \\gmult "&;

(**************************************************************************************************)

PackageExport["PathGroupoidSymbol"]

PathGroupoidSymbol[] := PathGroupoidSymbol["Q"];

declareBoxFormatting[
  PathGroupoidSymbol[q_] :>
    makeTypedTemplateBox[q -> QuiverSymbol, "PathGroupoidSymbolForm"]
]

$TemplateKatexFunction["PathGroupoidSymbolForm"] = "pathGroupoid";

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

$TemplateKatexFunction["PathQuiverSymbolForm"] = "pathQuiver";

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

$TemplateKatexFunction["ForwardPathQuiverSymbolForm"] = "forwardPathQuiver";


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

$TemplateKatexFunction["BackwardPathQuiverSymbolForm"] = "backwardPathQuiver";


(********************************************)

PackageExport["QuiverSymbol"]

QuiverSymbol[] := QuiverSymbol["Q"];

declareSymbolForm[QuiverSymbol];

(********************************************)

PackageExport["CoversForm"]

declareBoxFormatting[
  CoversForm[g_, h_] :>
    makeHintedTemplateBox[g -> GraphSymbol, h -> GraphSymbol, "CoversForm"],
  CoversForm[g_, h_, i_] :>
    makeHintedTemplateBox[g -> GraphSymbol, h -> GraphSymbol, i -> GraphSymbol, "CoversForm"],
  CoversForm[] :> 
    TemplateBox[{}, "CoversSymbol"]
]

$TemplateKatexFunction["CoversForm"] = riffled["\\covers"];
$TemplateKatexFunction["CoversSymbol"] = "\\covers";

(********************************************)

PackageExport["IndexedCoveringForm"]

declareBoxFormatting[
  IndexedCoveringForm[pi_, g_, h_] :>
    makeHintedTemplateBox[pi -> GraphHomomorphismSymbol, g -> GraphSymbol, h -> GraphSymbol, "IndexedCoveringForm"]
];

$TemplateKatexFunction["IndexedCoveringForm"] = "covering";


(********************************************)

PackageExport["GraphSymbol"]

GraphSymbol[] := GraphSymbol["G"];

declareSymbolForm[GraphSymbol];

(********************************************)

PackageExport["MatrixSymbol"]

MatrixSymbol[] := MatrixSymbol["M"];

declareSymbolForm[MatrixSymbol];

(**************************************************************************************************)

PackageExport["NullPath"]
PackageExport["NullElement"]

declareBoxFormatting[
  NullPath :> TemplateBox[{}, "NullPathSymbol"],
  NullElement :> TemplateBox[{}, "NullPathSymbol"]
];

$TemplateKatexFunction["NullPathSymbol"] = "nullpath";

(********************************************)

PackageExport["PathSymbol"]

PathSymbol[] := PathSymbol["P"];

declareBoxFormatting[
  PathSymbol[e_] :>
    makeStandardBoxTemplate[e, "PathSymbolForm"]
]

$TemplateKatexFunction["PathSymbolForm"] = "path";

(**************************************************************************************************)

PackageExport["EdgeSymbol"]

declareSymbolForm[EdgeSymbol];

(**************************************************************************************************)

PackageExport["VertexSymbol"]

declareSymbolForm[VertexSymbol];

$TemplateKatexFunction["VertexSymbolForm"] = "vert";

(**************************************************************************************************)

PackageExport["CardinalSymbol"]

declareSymbolForm[CardinalSymbol];

$TemplateKatexFunction["CardinalSymbolForm"] = "card";
$TemplateKatexFunction["NegatedCardinalSymbolForm"] = "ncard";

(**************************************************************************************************)

PackageExport["Form"]

declareBoxFormatting[
  Form[e_] :> makeQGBoxes @ e
]

SetHoldAllComplete[makeQGBoxes];

$binaryRelationMapping = <|
  Equal              -> "=",
  Unequal            -> "\[NotEqual]",
  Subset             -> "\[Subset]",
  SubsetEqual        -> "\[SubsetEqual]",
  Superset           -> "\[Superset]",
  SupersetEqual      -> "\[SupersetEqual]",
  NotSubset          -> "\[NotSubset]",
  NotSubsetEqual     -> "\[NotSubsetEqual]",
  NotSuperset        -> "\[NotSuperset]",
  NotSupersetEqual   -> "\[NotSupersetEqual]",
  TildeEqual         -> "\[TildeEqual]",
  TildeFullEqual     -> "\[TildeFullEqual]",
  TildeTilde         -> "\[TildeTilde]"
|>;

$binaryRelationHeads = Alternatives @@ Keys[$binaryRelationMapping];

(* this is the general dispatch mechanism for a form of unknown type *)
makeQGBoxes = Case[
  l:lsymsP                  := MakeBoxes @ l;
  s:namedFnP                := MakeBoxes @ s;
  e:symP                    := symbolBoxes @ e;
  e_Subtract                := algebraBoxes[e, "SubtractForm"];
  e_Times                   := algebraBoxes[e, "TimesForm"];
  e_Plus                    := algebraBoxes[e, "PlusForm"];
  Equal[a_, b_]             := MakeBoxes @ EqualForm[a, b];
  Unequal[a_, b_]           := MakeBoxes @ UnequalForm[a, b];
  (h:binHeads)[args__]      := With[{str = Lookup[$binaryRelationMapping, h]}, MakeBoxes @ BinaryRelationForm[str][args]];
  (i_InverseForm)[args__]   := MakeBoxes @ AppliedForm[i, args];
  Minus[e_]                 := makeStandardBoxTemplate[e, "MinusForm"];
  Power[e_, -1]             := makeStandardBoxTemplate[e, "InverseForm"];
  Times[-1, e_]             := makeStandardBoxTemplate[e, "MinusForm"];
  Negated[e_]               := makeStandardBoxTemplate[e, "NegatedForm"];
  DirectedEdge[args__]      := MakeBoxes @ DirectedEdgeForm[args];
  UndirectedEdge[args__]    := MakeBoxes @ UndirectedEdgeForm[args];
  other_                    := MakeBoxes @ other,
  {lsymsP -> $literalSymbolsP, symP -> $rawSymbolP, namedFnP -> Alternatives @@ $namedFunctions, 
    binHeads -> $binaryRelationHeads}
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

$TemplateKatexFunction["DirectedEdgeForm"] = "de";
$TemplateKatexFunction["TaggedDirectedEdgeForm"] = "tde";

(**************************************************************************************************)

PackageExport["UndirectedEdgeForm"]

declareBoxFormatting[
  UndirectedEdgeForm[a_, b_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, "UndirectedEdgeForm"],
  UndirectedEdgeForm[a_, b_, c_] :>
    makeHintedTemplateBox[a -> VertexSymbol, b -> VertexSymbol, c -> CardinalSymbol, "TaggedUndirectedEdgeForm"]
]

$TemplateKatexFunction["UndirectedEdgeForm"] = "de";
$TemplateKatexFunction["TaggedUndirectedEdgeForm"] = "tue";


(**************************************************************************************************)

PackageExport["SumForm"]

declareBoxFormatting[
  SumForm[args__] :>
    makeStandardBoxTemplate[args, "SumForm"]
]

$TemplateKatexFunction["SumForm"] = "indexSum";

(**************************************************************************************************)

PackageExport["ProductForm"]

declareBoxFormatting[
  SumForm[args__] :>
    makeStandardBoxTemplate[args, "ProductForm"]
]

$TemplateKatexFunction["ProductForm"] = "indexProd";

(**************************************************************************************************)

PackageExport["PlusForm"]

declareBoxFormatting[
  PlusForm[args__] :>
    makeStandardBoxTemplate[args, "PlusForm"],
  PlusForm[] :> "+"
]

$TemplateKatexFunction["PlusForm"] = riffled[" + "];

(**************************************************************************************************)

PackageExport["SubtractForm"]

declareBoxFormatting[
  SubtractForm[args__] :>
    makeStandardBoxTemplate[args, "SubtractForm"],
  SubtractForm[] :> "-"
]

$TemplateKatexFunction["SubtractForm"] = riffled[" - "];

(**************************************************************************************************)

PackageExport["MinusForm"]

declareBoxFormatting[
  MinusForm[args__] :>
    makeStandardBoxTemplate[args, "MinusForm"],
  MinusForm[] :> "-"
]

$TemplateKatexFunction["MinusForm"] = " - "&;

(**************************************************************************************************)

PackageExport["TimesForm"]

declareBoxFormatting[
  TimesForm[args__] :>
    makeStandardBoxTemplate[args, "TimesForm"]
]

$TemplateKatexFunction["TimesForm"] = riffled[" \\cross "];

(**************************************************************************************************)

PackageExport["ApproxEqualForm"]

declareBoxFormatting[
  ApproxEqualForm[args__] :>
    makeStandardBoxTemplate[args, "ApproxEqualForm"]
]

$TemplateKatexFunction["ApproxEqualForm"] = riffled[" \\approx "];

(**************************************************************************************************)

PackageExport["IsomorphicForm"]

declareBoxFormatting[
  IsomorphicForm[args__] :>
    makeStandardBoxTemplate[args, "IsomorphicForm"]
]

$TemplateKatexFunction["IsomorphicForm"] = riffled[" \\iso "];

(**************************************************************************************************)

PackageExport["HomotopicForm"]

declareBoxFormatting[
  HomotopicForm[args__] :>
    makeStandardBoxTemplate[args, "HomotopicForm"],

  HomotopicForm[] :> TemplateBox[{}, "HomotopicSymbol"]

]

$TemplateKatexFunction["HomotopicForm"] = riffled[" \\homotopic "];
$TemplateKatexFunction["HomotopicSymbol"] = " \\homotopic{}{}"

(**************************************************************************************************)

PackageExport["EqualForm"]

declareBoxFormatting[
  EqualForm[args__] :>
    makeStandardBoxTemplate[args, "EqualForm"]
]

$TemplateKatexFunction["EqualForm"] = riffled[" = "]

(**************************************************************************************************)

PackageExport["EqualGridForm"]

declareBoxFormatting[
  EqualGridForm[args__List] :>
    TemplateBox[
      {GridBox @ Map[toEqualRowBoxes, Unevaluated @ {args}]},
      "EqualGridForm"
    ]
]

SetHoldAllComplete[toEqualRowBoxes]
toEqualRowBoxes[list_List] := Scope[
  res = Map[makeQGBoxes, Unevaluated @ list];
  Append[MapStaggered[addEqual, res], Last @ res]
]

addEqual["", _] := Splice[{"", ""}];
addEqual[x_, ""] := Splice[{x, ""}];
addEqual[x_, y_] := Splice[{x, "="}];

$TemplateKatexFunction["EqualGridForm"] = katexEqualGrid;

katexEqualGrid[GridBox[rows_, ___]] := {
  "\\begin{equation*} \\begin{split} ",
  Replace[katexEqualGridRow /@ rows, "=" -> "&=", {2}],
  "\\end{split} \\end{equation*}"
}

katexEqualGridRow[row_List] := Append[Riffle[row, " "], "\\\\"];

(**************************************************************************************************)

PackageExport["Form"]

declareBoxFormatting[
  BinaryRelationForm[relation_String][args__] :>
    makeTypedTemplateBox[relation -> None, args, "BinaryRelationForm"]
]

$TemplateKatexFunction["BinaryRelationForm"] = Function[riffled[#1][##2]];

(**************************************************************************************************)

PackageExport["NotEqualForm"]

declareBoxFormatting[
  NotEqualForm[args__] :>
    makeStandardBoxTemplate[args, "NotEqualForm"]
]

$TemplateKatexFunction["NotEqualForm"] = riffled[" \\neq "]

(**************************************************************************************************)

PackageExport["ImplicitTimesForm"]

declareBoxFormatting[
  ImplicitTimesForm[args__] :>
    makeStandardBoxTemplate[args, "ImplicitTimesForm"]
]

$TemplateKatexFunction["ImplicitTimesForm"] = riffled["\,"]


(**************************************************************************************************)

PackageExport["InverseForm"]

declareBoxFormatting[
  InverseForm[e_] :>
    makeStandardBoxTemplate[e, "InverseForm"]
];

$TemplateKatexFunction["InverseForm"] = "inv";

(**************************************************************************************************)

PackageExport["RedForm"]

declareBoxFormatting[
  RedForm[e_] :>
    TemplateBox[List @ makeQGBoxes @ e, "RedForm"]
];

$TemplateKatexFunction["RedForm"] = "rform";

(**************************************************************************************************)

PackageExport["GreenForm"]

declareBoxFormatting[
  GreenForm[e_] :>
    TemplateBox[List @ makeQGBoxes @ e, "GreenForm"]
];

$TemplateKatexFunction["GreenForm"] = "gform";

(**************************************************************************************************)

PackageExport["BlueForm"]

declareBoxFormatting[
  BlueForm[e_] :>
    TemplateBox[List @ makeQGBoxes @ e, "BlueForm"]
];

$TemplateKatexFunction["BlueForm"] = "bform";

(**************************************************************************************************)

PackageExport["RedBlueForm"]

declareBoxFormatting[
  RedBlueForm[e_] :>
    TemplateBox[List @ makeQGBoxes @ e, "RedBlueForm"]
];

$TemplateKatexFunction["RedBlueForm"] = "rbform";

(**************************************************************************************************)

PackageExport["GreenBlueForm"]

declareBoxFormatting[
  GreenBlueForm[e_] :>
    TemplateBox[List @ makeQGBoxes @ e, "GreenBlueForm"]
];

$TemplateKatexFunction["GreenBlueForm"] = "bgform";

(**************************************************************************************************)

PackageExport["RedGreenForm"]

declareBoxFormatting[
  RedGreenForm[e_] :>
    TemplateBox[List @ makeQGBoxes @ e, "RedGreenForm"]
];

$TemplateKatexFunction["RedGreenForm"] = "rgform";

(**************************************************************************************************)

PackageExport["DarkGrayForm"]

declareBoxFormatting[
  DarkGrayForm[e_] :>
    TemplateBox[List @ makeQGBoxes @ e, "DarkGrayForm"]
];

$TemplateKatexFunction["DarkGrayForm"] = "waform";

(**************************************************************************************************)

PackageExport["MediumGrayForm"]

declareBoxFormatting[
  MediumGrayForm[e_] :>
    TemplateBox[List @ makeQGBoxes @ e, "MediumGrayForm"]
];

$TemplateKatexFunction["MediumGrayForm"] = "wbform";

(**************************************************************************************************)

PackageExport["LightGrayForm"]

declareBoxFormatting[
  LightGrayForm[e_] :>
    TemplateBox[List @ makeQGBoxes @ e, "LightGrayForm"]
];

$TemplateKatexFunction["LightGrayForm"] = "wcform";

(**************************************************************************************************)

PackageExport["FilledTokenSymbol"]

declareBoxFormatting[
  FilledTokenSymbol :> TemplateBox[{}, "FilledTokenSymbol"]
];

$TemplateKatexFunction["FilledTokenSymbol"] = "\\filledToken"&;

(**************************************************************************************************)

PackageExport["EmptyTokenSymbol"]

declareBoxFormatting[
  EmptyTokenSymbol :> TemplateBox[{}, "EmptyTokenSymbol"]
];

$TemplateKatexFunction["EmptyTokenSymbol"] = "\\emptyToken"&;

(**************************************************************************************************)

PackageExport["NegatedBoxForm"]

SetHoldFirst[NegatedBoxForm];
NegatedBoxForm[e_] := makeStandardBoxTemplate[e, "NegatedForm"]

$TemplateKatexFunction["NegatedForm"] = "negated";

(********************************************)

PackageExport["WordVectorForm"]

WordVectorForm[p_String, args___] := WordVectorForm[ToPathWord @ p, args]

declareBoxFormatting[
  WordVectorForm[p_, dir_:"Forward"] :>
    TemplateBox[{wordBoxes @ p, dir}, "WordVectorForm"]
];

(********************************************)

makePathBoxTemplate[left_, rest___, tag_] :=
  TemplateBox[
    Join[
      List @ maybeParen[PathSymbol|CardinalSymbol] @ left,
      Map[
        maybeParen[PathSymbol|EdgeFieldSymbol|VertexFieldSymbol|PathTranslateForm|PathBackwardTranslateForm], 
        Unevaluated @ {rest}
      ]
    ],
    tag
  ];

(********************************************)

PackageExport["$PathComposeSymbol"]

$PathComposeSymbol = "\[Proportion]";

PackageExport["PathComposeForm"]

declareBoxFormatting[
  PathComposeForm[a_, b__] :>
    makePathBoxTemplate[a, b, "PathComposeForm"],

  PathComposeForm[] :>
    TemplateBox[{}, "PathComposeSymbol"]
];

$TemplateKatexFunction["PathComposeForm"] = "pathCompose";
$TemplateKatexFunction["PathComposeSymbol"] = "pathComposeSymbol";

(********************************************)

PackageExport["PathTranslateForm"]
PackageExport["PathLeftTranslateForm"]

declareBoxFormatting[
  PathTranslateForm[a_, b__] :>
    makePathBoxTemplate[a, b, "PathTranslateForm"],
  PathTranslateForm[a_] :> 
    makePathBoxTemplate[a, "PathAnonymousTranslateForm"],
  PathLeftTranslateForm[a_] :>
    makePathBoxTemplate[a, "PathLeftTranslateForm"]
];

$TemplateKatexFunction["PathTranslateForm"] = "pathTranslate";
$TemplateKatexFunction["PathAnonymousTranslateForm"] = "pathTranslate{}";
$TemplateKatexFunction["PathLeftTranslateForm"] = "pathLeftTranslate";

(********************************************)

PackageExport["PathBackwardTranslateForm"]
PackageExport["PathLeftBackwardTranslateForm"]

declareBoxFormatting[
  PathBackwardTranslateForm[a_, b__] :>
    makePathBoxTemplate[a, b, "PathBackwardTranslateForm"],
  PathBackwardTranslateForm[a_] :>
    makePathBoxTemplate[a, "PathAnonymousBackwardTranslateForm"],
  PathLeftBackwardTranslateForm[a_] :>
    makePathBoxTemplate[a, "PathLeftBackwardTranslateForm"]
];

$TemplateKatexFunction["PathBackwardTranslateForm"] = "pathBackwardTranslate";
$TemplateKatexFunction["PathAnonymousBackwardTranslateForm"] = "pathBackwardTranslate{}";
$TemplateKatexFunction["PathLeftBackwardTranslateForm"] = "pathLeftBackwardTranslate";

(********************************************)

PackageExport["PathHeadVectorForm"]

declareBoxFormatting[
  PathHeadVectorForm[a_] :>
    makePathBoxTemplate[a, "PathHeadVectorForm"]
];

$TemplateKatexFunction["PathHeadVectorForm"] = "pathHeadVector";

(********************************************)

PackageExport["PathTailVectorForm"]

declareBoxFormatting[
  PathTailVectorForm[a_] :>
    makePathBoxTemplate[a, "PathTailVectorForm"]
];

$TemplateKatexFunction["PathTailVectorForm"] = "pathTailVector";

(********************************************)

PackageExport["PathReverseForm"]

declareBoxFormatting[
  PathReverseForm[a_] :>
    makePathBoxTemplate[a, "PathReverseForm"]
];

$TemplateKatexFunction["PathReverseForm"] = "pathReverse";

(********************************************)

PackageExport["PathForwardDifferenceForm"]

PathForwardDifferenceForm[w_String] :=
  PathForwardDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathForwardDifferenceForm[w__] :>
    TemplateBox[Map[pathOrWordBoxes, {w}], "PathForwardDifferenceForm"],
  PathForwardDifferenceForm[] :>
    TemplateBox[{}, "PathForwardDifferenceSymbol"],
  (op_PathForwardDifferenceForm)[arg_] :>
    MakeBoxes @ OperatorAppliedForm[op, arg]
];

$TemplateKatexFunction["PathForwardDifferenceForm"] = applyRiffled["pathForwardDifference",","];
$TemplateKatexFunction["PathForwardDifferenceSymbol"] = "forwardDifference";

(********************************************)

PackageExport["PathBackwardDifferenceForm"]

PathBackwardDifferenceForm[w_String] :=
  PathBackwardDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathBackwardDifferenceForm[w__] :>
    TemplateBox[Map[pathOrWordBoxes, {w}], "PathBackwardDifferenceForm"],
  PathBackwardDifferenceForm[] :>
    TemplateBox[{}, "PathBackwardDifferenceSymbol"],
  (op_PathBackwardDifferenceForm)[arg_] :>
    MakeBoxes @ OperatorAppliedForm[op, arg]
];

$TemplateKatexFunction["PathBackwardDifferenceForm"] = applyRiffled["pathBackwardDifference",","];
$TemplateKatexFunction["PathBackwardDifferenceSymbol"] = "backwardDifference";

(********************************************)

PackageExport["PathCentralDifferenceForm"]

PathCentralDifferenceForm[w_String] :=
  PathCentralDifferenceForm @ toPathWordOrSymbol @ w;

declareBoxFormatting[
  PathCentralDifferenceForm[w__] :>
    TemplateBox[Map[pathOrWordBoxes, {w}], "PathCentralDifferenceForm"],
  PathCentralDifferenceForm[] :>
    TemplateBox[{}, "PathCentralDifferenceSymbol"],
  (op_PathCentralDifferenceForm)[arg_] :>
    MakeBoxes @ OperatorAppliedForm[op, arg]    
];

$TemplateKatexFunction["PathCentralDifferenceForm"] = applyRiffled["pathCentralDifference",","];
$TemplateKatexFunction["PathCentralDifferenceSymbol"] = "centralDifference";

(********************************************)

toPathWordOrSymbol = Case[
  s:("P" | "Q" | "R" | "T")           := PathSymbol @ s;
  c:"x" | "y" | "z" | "a" | "b" | "c" := CardinalSymbol @ c;
  w_                                  := ToPathWord[w];
];

pathOrWordBoxes = Case[
  s:("P" | "Q" | "R" | "T") := MakeBoxes @ PathSymbol @ s;
  s:syms        := MakeBoxes @ PathSymbol @ s;
  s_List        := wordBoxes @ s;
  p:(_PathSymbol | _VertexFieldSymbol | _EdgeFieldSymbol | _WordForm | _CardinalSymbol) := MakeBoxes @ p,
  {syms -> $rawSymbolP}
]

(********************************************)

SetHoldAllComplete[maybeParen, maybeParenBoxes];

maybeParen[h_][b_] := Block[{$eh = h, $ehc = If[Head[h] === Alternatives, First @ h, h]}, maybeParenBoxes @  b];

maybeParenBoxes = Case[
  s:syms                                := With[{head = $ehc}, MakeBoxes @ head @ s];
  e:_InverseForm | _GroupInverseForm | _GroupoidInverseForm | _AppliedForm := MakeBoxes @ e;
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

$TemplateKatexFunction["ListForm"] = applyRiffled["list", ","];

(********************************************)

PackageExport["ParenthesesForm"]

declareBoxFormatting[
  ParenthesesForm[args__] :>
    makeStandardBoxTemplate[args, "ParenthesesForm"]
]

$TemplateKatexFunction["ParenthesesForm"] = applyRiffled["paren", ","];

(********************************************)

PackageExport["TupleForm"]

declareBoxFormatting[
  TupleForm[args__] :>
    makeStandardBoxTemplate[args, "TupleForm"]
]

$TemplateKatexFunction["TupleForm"] = applyRiffled["tuple", ","];

(********************************************)

PackageExport["SmallMatrixForm"]

declareBoxFormatting[
  SmallMatrixForm[array_] :>
    TemplateBox[Map[smallMatrixRowBoxes, Unevaluated @ array], "SmallMatrixForm"]
];

SetHoldAllComplete[smallMatrixRowBoxes]

smallMatrixRowBoxes[row_List] :=
  TemplateBox[Map[makeQGBoxes, Unevaluated @ row], "SmallMatrixRowForm"];

$TemplateKatexFunction["SmallMatrixForm"] = smallMatrixKatex;
$TemplateKatexFunction["SmallMatrixRowForm"] = smallMatrixRowKatex;

smallMatrixKatex[rows___] := {"{\\begin{smallmatrix}", Riffle[{rows}, "\\\\"], "\\end{smallmatrix}}"};
smallMatrixRowKatex[cols___] := Riffle[{cols}, "&"];

(********************************************)

PackageExport["ConcatenationForm"]

declareBoxFormatting[
  ConcatenationForm[args__] :>
    makeStandardBoxTemplate[args, "ConcatenationForm"]
]

$TemplateKatexFunction["ConcatenationForm"] = applyRiffled["concat", " "];

(********************************************)

PackageExport["CommaRowForm"]

declareBoxFormatting[
  CommaRowForm[args__] :>
    makeStandardBoxTemplate[args, "CommaRowForm"]
]

$TemplateKatexFunction["CommaRowForm"] = riffled[","];

(********************************************)

PackageExport["RewriteForm"]

declareBoxFormatting[
  RewriteForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "RewriteForm"]
];

$TemplateKatexFunction["RewriteForm"] = "rewrite";

(********************************************)

PackageExport["AndForm"]

declareBoxFormatting[
  AndForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "AndForm"]
];

$TemplateKatexFunction["AndForm"] = riffled["\\land"];

(********************************************)

PackageExport["OrForm"]

declareBoxFormatting[
  OrForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "OrForm"]
];

$TemplateKatexFunction["OrForm"] = riffled["\\lor"];

(********************************************)

PackageExport["NotForm"]

declareBoxFormatting[
  NotForm[a_] :>
    TemplateBox[List @ maybeParen[SymbolForm] @ a, "NotForm"]
];

$TemplateKatexFunction["NotForm"] = riffled["\\lnot"];

(********************************************)

PackageExport["ImpliesForm"]

declareBoxFormatting[
  ImpliesForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "ImpliesForm"]
];

$TemplateKatexFunction["ImpliesForm"] = riffled["\\implies"];

(********************************************)

PackageExport["EquivalentForm"]

declareBoxFormatting[
  EquivalentForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "EquivalentForm"]
];

$TemplateKatexFunction["EquivalentForm"] = riffled["\\iff"];

(********************************************)

PackageExport["MapsToForm"]

declareBoxFormatting[
  MapsToForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "MapsToForm"]
];

$TemplateKatexFunction["MapsToForm"] = "mto";

(**************************************************************************************************)

PackageExport["EllipsisSymbol"]

declareBoxFormatting[
  EllipsisSymbol :> TemplateBox[{}, "EllipsisSymbol"]
]

$TemplateKatexFunction["EllipsisSymbol"] = "\\dots"&;

(**************************************************************************************************)

PackageExport["PartialDifferentialOfForm"]

declareBoxFormatting[
  PartialDifferentialOfForm[x_] :> 
    makeStandardBoxTemplate[x, "PartialDifferentialOfForm"],
  PartialDifferentialOfForm[] :>
    TemplateBox[{}, "PartialDifferentialSymbol"]
];

$TemplateKatexFunction["PartialDifferentialOfForm"] = "partialdof";
$TemplateKatexFunction["PartialDifferentialSymbol"] = "\\partial"&;

(**************************************************************************************************)

PackageExport["VertexListFunction"]
PackageExport["EdgeListFunction"]
PackageExport["PathListFunction"]
PackageExport["CardinalListFunction"]
PackageExport["SignedLengthFunction"]
PackageExport["LengthFunction"]
PackageExport["HeadVertexFunction"]
PackageExport["TailVertexFunction"]
PackageExport["WordFunction"]
PackageExport["AutomorphismsFunction"]
PackageExport["BasisFunction"]
PackageExport["SupportFunction"]

declareFunctionFormatting[sym_] := With[
  {name = StringDelete[SymbolName[sym], "Function"]},
  declareBoxFormatting[
    sym :> TemplateBox[{name}, "NamedFunctionSymbolForm"],
    sym[args___] :> makeTypedTemplateBox[sym, args, "AppliedForm"]
  ]
];

Scan[declareFunctionFormatting, $namedFunctions];

$TemplateKatexFunction["NamedFunctionSymbolForm"] = namedFuncKatex;

namedFuncKatex["Word"] := "\\wordOf"; (* because 'word' already menas wordForm *)
namedFuncKatex[s_String] := namedFuncKatex[s] = StringJoin["\\", LowerCaseFirst @ s];

(**************************************************************************************************)

PackageExport["QuotientForm"]

declareBoxFormatting[
  QuotientForm[a_, b_] :>
    makeStandardBoxTemplate[a, b, "QuotientForm"]
];

$TemplateKatexFunction["QuotientForm"] = "quotient";

(********************************************)

PackageExport["RowForm"]
PackageExport["TermRowForm"]

declareBoxFormatting[
  RowForm[args__] :>
    makeStandardBoxTemplate[args, "RowForm"],
  TermRowForm[args__] :>
    makeStandardBoxTemplate[args, "TermRowForm"]
];

$TemplateKatexFunction["RowForm"] = riffled[" "];
$TemplateKatexFunction["TermRowForm"] = riffled[" "];

(********************************************)

PackageExport["IdentityElementForm"]

declareBoxFormatting[
  IdentityElementForm[args___] :>
    makeStandardBoxTemplate[args, "IdentityElementForm"]
];

$TemplateKatexFunction["IdentityElementForm"] = "idElem";

(********************************************)

PackageExport["ParenPathWordForm"]

ParenPathWordForm[args__] := ParenthesesForm @ PathWordForm @ args;

(********************************************)

PackageExport["ParenEmptyPathWordForm"]

ParenEmptyPathWordForm[v_] := ParenthesesForm @ EmptyPathWordForm @ v;

(********************************************)

PackageExport["EmptyPathWordForm"]

declareBoxFormatting[
  EmptyPathWordForm[v_] :>
    MakeBoxes @ PathWordForm[v, {}, v]
];

(********************************************)

PackageExport["PathWordForm"]

PathWordForm[a_, b_String, c_] := PathWordForm[a, ToPathWord @ b, c];

declareBoxFormatting[
  PathWordForm[t_, w_, h_] :>
    makeTypedTemplateBox[t -> VertexSymbol, w -> WordForm, h -> VertexSymbol, "PathWordForm"]
];

$TemplateKatexFunction["PathWordForm"] = "pathWord";

(********************************************)

PackageExport["$AutoColorCardinals"]

$AutoColorCardinals = True;

(********************************************)

PackageExport["WordForm"]

WordForm[s_String] := WordForm @ ToPathWord @ s;

declareBoxFormatting[
  WordForm[e_] :> wordBoxes[e]
];

$TemplateKatexFunction["EmptyWordForm"] := "emptyWord"
$TemplateKatexFunction["WordForm"] = "word";
$TemplateKatexFunction["WordSymbolForm"] = "wordSymbol";

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

$cardFormP = "CardinalSymbolForm" | "NegatedCardinalSymbolForm";

$colorCardinalRules = {
  b:TemplateBox[{"r"}, $cardFormP] -> rBox @ b,
  b:TemplateBox[{"g"}, $cardFormP] -> gBox @ b,
  b:TemplateBox[{"b"}, $cardFormP] -> bBox @ b
};


(********************************************)

PackageExport["PlainWordForm"]

PlainWordForm[s_String] := PlainWordForm @ ToPathWord @ s;

declareBoxFormatting[
  PlainWordForm[e_] :> Block[{$AutoColorCardinals = False}, wordBoxes[e]]
];


